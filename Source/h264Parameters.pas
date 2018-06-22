{ ****************************************************************************** }
{ * h264Parameters.pas        by qq600585                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit h264Parameters;

{$I zDefine.inc}

interface

uses
  Classes, SysUtils, h264Stdint, h264util, PascalStrings;

const
  MIN_QP               = 0;
  MAX_QP               = 51;
  MAX_CHROMA_QP_OFFSET = 12;
  MAX_REFERENCE_FRAMES = 16;

type
  { TEncodingParameters }
  // encoder configuration parameters
  TEncodingParameters = class
  private
    width, height: uint16_t;  // input dimensions
    frames: uint32_t;         // frame count
    fps: single;              // fps
    qp: uint8_t;              // quantization parameter
    chroma_qp_offset: int8_t; // chroma qp offset
    subme: uint8_t;           // subpixel ME refinement
    { 0 - none (fpel only)
      1 - hpel
      2 - qpel
      3 - qpel SATD
    }
    analyse: uint8_t; // mb type decision quality
    { 0 - none
      1 - heuristics - SAD
      2 - heuristics - SATD
      3 - bitcost
    }
    ref: uint8_t;           // reference frame count
    key_interval: uint16_t; // maximum keyframe interval
    loopfilter: boolean;    // deblocking
    filter_thread: boolean; // deblocking in separate thread
    aq: boolean;            // mb-level adaptive quantization
    luma_only: boolean;     // ignore chroma

    rc: record
      enabled: boolean;  // enable avg. bitrate ratecontrol
      bitrate: uint32_t; // desired bitrate in kbps
    end;

    procedure SetAnalysisLevel(const AValue: uint8_t);
    procedure SetChromaQParamOffset(const AValue: int8_t);
    procedure SetFilterThreadEnabled(AValue: boolean);
    procedure SetKeyFrameInterval(const AValue: uint16_t);
    procedure SetNumReferenceFrames(const AValue: uint8_t);
    procedure SetQParam(const AValue: uint8_t);
    procedure SetSubpixelMELevel(const AValue: uint8_t);
    procedure ValidateQParams;
    procedure ValidateSubME;
  public
    property FrameWidth: uint16_t read width;
    property FrameHeight: uint16_t read height;
    property FrameRate: single read fps;

    property ABRRateControlEnabled: boolean read rc.enabled;
    property FrameCount: uint32_t read frames write frames;
    property bitrate: uint32_t read rc.bitrate;

    property QParam: uint8_t read qp write SetQParam;
    property ChromaQParamOffset: int8_t read chroma_qp_offset write SetChromaQParamOffset;
    property KeyFrameInterval: uint16_t read key_interval write SetKeyFrameInterval;

    property LoopFilterEnabled: boolean read loopfilter write loopfilter;
    property FilterThreadEnabled: boolean read filter_thread write SetFilterThreadEnabled;

    property AnalysisLevel: uint8_t read analyse write SetAnalysisLevel; // mb type decision quality
    { 0 - none
      1 - heuristics - SAD
      2 - heuristics - SATD
      3 - bitcost
    }

    property SubpixelMELevel: uint8_t read subme write SetSubpixelMELevel; // subpixel ME refinement
    { 0 - none (fpel only)
      1 - hpel
      2 - qpel
      3 - qpel SATD
    }

    property NumReferenceFrames: uint8_t read ref write SetNumReferenceFrames;

    property AdaptiveQuant: boolean read aq write aq;

    property IgnoreChroma: boolean read luma_only write luma_only;

    constructor Create; overload;
    constructor Create(const width_, height_: uint16_t; const fps_: double); overload;
    procedure SetABRRateControl(const bitrate_: uint32_t);
    procedure SetStreamParams(const width_, height_, frame_count: int32_t; const fps_: single);
    function ToPascalString: TPascalString; overload;
  end;

implementation

{ TEncodingParameters }

procedure TEncodingParameters.SetAnalysisLevel(const AValue: uint8_t);
begin
  analyse := clip3(0, AValue, 3);
end;

procedure TEncodingParameters.SetChromaQParamOffset(const AValue: int8_t);
begin
  chroma_qp_offset := clip3(-MAX_CHROMA_QP_OFFSET, AValue, MAX_CHROMA_QP_OFFSET);
  ValidateQParams;
end;

procedure TEncodingParameters.SetFilterThreadEnabled(AValue: boolean);
begin
  if filter_thread = AValue then
      Exit;
  filter_thread := AValue;
  if AValue and not loopfilter then
      LoopFilterEnabled := true;
end;

procedure TEncodingParameters.SetKeyFrameInterval(const AValue: uint16_t);
begin
  key_interval := AValue;
  if key_interval = 0 then
      key_interval := 1;
end;

procedure TEncodingParameters.SetNumReferenceFrames(const AValue: uint8_t);
begin
  ref := clip3(1, AValue, MAX_REFERENCE_FRAMES);
  ValidateSubME;
end;

procedure TEncodingParameters.SetQParam(const AValue: uint8_t);
begin
  qp := clip3(MIN_QP, AValue, MAX_QP);
  ValidateQParams;
end;

procedure TEncodingParameters.SetSubpixelMELevel(const AValue: uint8_t);
begin
  subme := clip3(0, AValue, 4);
  ValidateSubME;
end;

procedure TEncodingParameters.ValidateQParams;
begin
  if qp + chroma_qp_offset > MAX_QP then
      chroma_qp_offset := MAX_QP - qp;
  if qp + chroma_qp_offset < MIN_QP then
      chroma_qp_offset := MIN_QP - qp;
end;

procedure TEncodingParameters.ValidateSubME;
begin
  if (ref > 1) and (subme < 2) then
      subme := 2;
end;

constructor TEncodingParameters.Create;
begin
  Create(320, 240, 25.0);
end;

constructor TEncodingParameters.Create(const width_, height_: uint16_t; const fps_: double);
begin
  inherited Create;
  width := width_;
  height := height_;
  fps := fps_;

  qp := 21;
  chroma_qp_offset := 0;
  key_interval := 300;
  subme := 3;
  analyse := 2;
  ref := 1;
  rc.enabled := false;
  aq := false;
  loopfilter := false;
  filter_thread := false;
  luma_only := false;
  frames := 0;
end;

procedure TEncodingParameters.SetABRRateControl(const bitrate_: uint32_t);
begin
  rc.enabled := true;
  rc.bitrate := bitrate_;
end;

procedure TEncodingParameters.SetStreamParams(const width_, height_, frame_count: int32_t; const fps_: single);
begin
  width := width_;
  height := height_;
  frames := frame_count;
  fps := fps_;
end;

function TEncodingParameters.ToPascalString: TPascalString;
  function b2s(const b: boolean): SystemString;
  begin
    if b then
        result := '1'
    else
        result := '0'
  end;

begin
  result.Text := PFormat('%dx%d keyint:%d qp:%d subme:%d analyse:%d ref:%d aq:%s '
    + 'chroma_qp_offset:%d loopfilter:%s (threaded:%s)',
    [width, height, key_interval, qp, subme, analyse, ref, b2s(aq),
    chroma_qp_offset, b2s(loopfilter), b2s(filter_thread)]);
end;

end.
