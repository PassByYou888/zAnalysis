{ ****************************************************************************** }
{ * h264Encoder.pas        by qq600585                                         * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit h264Encoder;

{$I zDefine.inc}

interface

uses
  sysutils, h264Stdint, h264Common, h264Util, h264Parameters, h264Frame, h264stream, h264Stats, h264loopfilter,
  h264intra_pred, h264motion_comp, h264motion_est, h264ratecontrol, h264image, h264mb_encoder, CoreClasses;

type
  TFevh264Encoder = class
  private
    h264s: TH264Stream;
    mb_enc: TMacroblockEncoder;
    fenc: TFrame; // currently encoded frame
    stats: TStreamStats;
    frame_num: int32_t;

    width, height: int32_t;
    key_interval: int32_t; // IDR interval
    last_keyframe_num: int32_t;
    num_ref_frames: int32_t;
    mb_width, mb_height, mb_count: int32_t;

    // encoder configuration
    _param: TEncodingParameters;
    stats_file: textfile;

    // classes
    frames: TFrameManager;
    rc: TRatecontrol;
    mc: TMotionCompensation;
    me: TMotionEstimator;

    procedure SetISlice;
    procedure SetPSlice;
    function TryEncodeFrame(const img: TPlanarImage): boolean;
    function SceneCut(const mbrow: int32_t): boolean;
    procedure GetFrameSSD;
    procedure UpdateStats;
  public
    {
      Create encoder with desired parameters.
      Param instance is bound to encoder and shouldn't be modified until the encoder is freed
    }
    constructor Create(var param: TEncodingParameters);
    destructor destroy; override;
    procedure EncodeFrame(const img: TPlanarImage; buffer: uint8_p; out stream_size: uint32_t);
    procedure GetLastFrameSSD(out ssd: array of int64_t);
    procedure GetLastFrame(out last_frame: TFrame);
  end;

implementation


constructor TFevh264Encoder.Create(var param: TEncodingParameters);
begin
  inherited Create;

  _param := param;

  // check&set params
  width := param.FrameWidth;
  height := param.FrameHeight;
  num_ref_frames := param.NumReferenceFrames;
  key_interval := param.KeyFrameInterval;

  frame_num := 0;
  last_keyframe_num := 0;
  mb_width := width div 16;
  mb_height := height div 16;
  if (width and $F) > 0 then
      inc(mb_width);
  if (height and $F) > 0 then
      inc(mb_height);
  mb_count := mb_width * mb_height;

  // stream settings
  h264s := TH264Stream.Create(width, height, mb_width, mb_height);
  h264s.QP := param.QParam;
  h264s.ChromaQPOffset := param.ChromaQParamOffset;
  h264s.KeyInterval := key_interval;
  h264s.NumRefFrames := num_ref_frames;
  if not param.LoopFilterEnabled then
      h264s.DisableLoopFilter;

  // allocate frames
  frames := TFrameManager.Create(num_ref_frames, mb_width, mb_height);

  // inter pred
  mc := TMotionCompensation.Create;
  me := TMotionEstimator.Create(width, height, mb_width, mb_height, mc, h264s.GetInterPredCostEvaluator);
  me.subme := param.SubpixelMELevel;

  // ratecontrol
  rc := TRatecontrol.Create;
  if param.ABRRateControlEnabled then
      rc.Set2pass(param.Bitrate, param.FrameCount, param.FrameRate)
  else
      rc.SetConstQP(param.QParam);

  // mb encoder
  case param.AnalysisLevel of
    0: mb_enc := TMBEncoderNoAnalyse.Create;
    1: mb_enc := TMBEncoderQuickAnalyse.Create;
    2: mb_enc := TMBEncoderQuickAnalyseSATD.Create;
    else mb_enc := TMBEncoderRateAnalyse.Create;
  end;
  mb_enc.num_ref_frames := num_ref_frames;
  mb_enc.chroma_coding := true;
  mb_enc.mc := mc;
  mb_enc.me := me;
  mb_enc.h264s := h264s;
  mb_enc.ChromaQPOffset := param.ChromaQParamOffset;
  mb_enc.chroma_coding := not param.IgnoreChroma;
  mb_enc.loopfilter := param.LoopFilterEnabled;

  // stats
  stats := TStreamStats.Create;
  h264s.SEIString := param.ToPascalString;
end;

destructor TFevh264Encoder.destroy;
begin
  rc.Free;
  frames.Free;
  me.Free;
  mc.Free;
  h264s.Free;
  mb_enc.Free;
  stats.Free;
  inherited destroy;
end;

procedure TFevh264Encoder.EncodeFrame(const img: TPlanarImage; buffer: uint8_p; out stream_size: uint32_t);
begin
  frames.GetFree(fenc);
  frame_img2frame_copy(fenc, img);
  fenc.num := frame_num;

  // set frame params
  if (frame_num = 0) or (frame_num - last_keyframe_num >= key_interval) then
      SetISlice
  else
      SetPSlice;

  // encode frame (or reencode P as I)
  if TryEncodeFrame(img) = false then
    begin
      SetISlice;
      TryEncodeFrame(img);
    end;

  // prepare reference frame for ME
  frame_paint_edges(fenc);
  if _param.SubpixelMELevel > 0 then
      frame_hpel_interpolate(fenc);

  // convert bitstream to bytestream of NAL units
  h264s.GetSliceBitstream(buffer, stream_size);

  // stats
  rc.Update(frame_num, stream_size * 8, fenc);
  fenc.stats.size_bytes := stream_size;
  UpdateStats;

  // advance
  frames.InsertRef(fenc);
  inc(frame_num);
end;

procedure TFevh264Encoder.SetISlice;
begin
  fenc.ftype := SLICE_I;
  last_keyframe_num := frame_num;
end;

procedure TFevh264Encoder.SetPSlice;
begin
  fenc.num_ref_frames := min(num_ref_frames, frame_num - last_keyframe_num);
  fenc.ftype := SLICE_P;
  frames.SetRefs(fenc, frame_num, fenc.num_ref_frames);
  me.NumReferences := fenc.num_ref_frames;
end;

function TFevh264Encoder.TryEncodeFrame(const img: TPlanarImage): boolean;
var
  x, y: int32_t;
  deblocker: IDeblocker;
  loopfilter: boolean;
begin
  result := true;

  // init slice bitstream
  if img.QParam <> QPARAM_AUTO then
      fenc.QP := img.QParam
  else
      fenc.QP := rc.GetQP(frame_num, fenc.ftype);
  h264s.InitSlice(fenc.ftype, fenc.QP, fenc.num_ref_frames, fenc.bs_buf);

  // frame encoding setup
  fenc.stats.Clear;
  mb_enc.SetFrame(fenc);
  loopfilter := _param.LoopFilterEnabled;
  if loopfilter then
      deblocker := GetNewDeblocker(fenc, not(_param.AdaptiveQuant), _param.FilterThreadEnabled);

  // encode rows
  for y := 0 to (mb_height - 1) do
    begin
      for x := 0 to (mb_width - 1) do
          mb_enc.Encode(x, y);

      if SceneCut(y) then
        begin
          result := false;
          h264s.AbortSlice;
          break;
        end;

      if loopfilter then
          deblocker.MBRowFinished;
    end;

  // finish frame processing. If we don't do any deblocking, SSD is already calculated at the last stage of macroblock encoding
  if loopfilter then
    begin
      deblocker.FrameFinished;
      deblocker.Free;
      if result then
          GetFrameSSD;
    end;
end;

function TFevh264Encoder.SceneCut(const mbrow: int32_t): boolean;
begin
  result := false;
  if (fenc.ftype = SLICE_P) and (mbrow > mb_height div 2) then
    begin
      if (2 * fenc.stats.mb_i4_count > mb_count)
        or (4 * int32_t(fenc.stats.mb_i16_count) > 3 * mb_count)
        or (8 * int32_t(fenc.stats.mb_i4_count + fenc.stats.mb_i16_count) > 7 * mb_count)
      then
          result := true;
    end;
end;

procedure TFevh264Encoder.GetLastFrameSSD(out ssd: array of int64_t);
begin
  case Length(ssd) of
    0:
      ;
    1 .. 2:
      ssd[0] := fenc.stats.ssd[0];
    else
      begin
        ssd[0] := fenc.stats.ssd[0];
        ssd[1] := fenc.stats.ssd[1];
        ssd[2] := fenc.stats.ssd[2];
      end;
  end;
end;

procedure TFevh264Encoder.GetLastFrame(out last_frame: TFrame);
begin
  last_frame := fenc;
end;

// update stream stats with current frame's stats
procedure TFevh264Encoder.UpdateStats;
begin
  if fenc.ftype = SLICE_I then
      inc(stats.i_count)
  else
      inc(stats.p_count);
  stats.Add(fenc.stats);
end;

procedure TFevh264Encoder.GetFrameSSD;
var
  x, y: int32_t;
  mb: PMacroblock;
begin
  for y := 0 to (mb_height - 1) do
    begin
      for x := 0 to (mb_width - 1) do
        begin
          mb := @fenc.mbs[y * mb_width + x];
          dsp.pixel_load_16x16(mb^.pixels, mb^.pfdec, fenc.stride);
          dsp.pixel_load_8x8(mb^.pixels_c[0], mb^.pfdec_c[0], fenc.stride_c);
          dsp.pixel_load_8x8(mb^.pixels_c[1], mb^.pfdec_c[1], fenc.stride_c);

          inc(fenc.stats.ssd[0], dsp.ssd_16x16(mb^.pixels, mb^.pfenc, fenc.stride));
          inc(fenc.stats.ssd[1], dsp.ssd_8x8(mb^.pixels_c[0], mb^.pfenc_c[0], fenc.stride_c));
          inc(fenc.stats.ssd[2], dsp.ssd_8x8(mb^.pixels_c[1], mb^.pfenc_c[1], fenc.stride_c));
        end;
    end;
end;

(* ******************************************************************************
  ****************************************************************************** *)

initialization

frame_init;
intra_pred_init;
dsp := TDsp.Create;

finalization

disposeObject(dsp);

end.
