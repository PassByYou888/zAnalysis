{ ****************************************************************************** }
{ * h264Stats.pas        by qq600585                                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264Stats;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses h264Stdint, CoreClasses;

type
  TFrameStats = class
  public
    pred: array [0 .. 8] of Int64_t;
    pred16: array [0 .. 3] of Int64_t;
    pred_8x8_chroma: array [0 .. 3] of Int64_t;
    ref: array [0 .. 15] of Int64_t;
    ptex_bits, itex_bits, mb_skip_count, mb_i4_count, mb_i16_count, mb_p_count: Int64_t;
    size_bytes: Int64_t;
    ssd: array [0 .. 2] of Int64_t;

    constructor Create;
    procedure Clear; virtual;
    procedure Add(A: TFrameStats);
  end;

  { TStreamStats }

  TStreamStats = class(TFrameStats)
  public
    i_count, p_count: Int64_t;
    procedure Clear; override;
  end;

  (* ******************************************************************************
    ****************************************************************************** *)
implementation

  { TStreamStats }

procedure TStreamStats.Clear;
begin
  inherited Clear;
  i_count := 0;
  p_count := 0;
end;

{ TFrameStats }

constructor TFrameStats.Create;
begin
  Clear;
end;

procedure TFrameStats.Clear;
begin
  FillPtrByte(@pred, SizeOf(pred), 0);
  FillPtrByte(@pred16, SizeOf(pred16), 0);
  FillPtrByte(@pred_8x8_chroma, SizeOf(pred_8x8_chroma), 0);
  FillPtrByte(@ref, SizeOf(ref), 0);
  FillPtrByte(@ssd, SizeOf(ssd), 0);
  ptex_bits := 0;
  itex_bits := 0;
  mb_skip_count := 0;
  mb_i4_count := 0;
  mb_i16_count := 0;
  mb_p_count := 0;
  size_bytes := 0;
end;

procedure TFrameStats.Add(A: TFrameStats);
var
  i: int32_t;
begin
  Inc(itex_bits, A.itex_bits);
  Inc(ptex_bits, A.ptex_bits);
  Inc(mb_i4_count, A.mb_i4_count);
  Inc(mb_i16_count, A.mb_i16_count);
  Inc(mb_p_count, A.mb_p_count);
  Inc(mb_skip_count, A.mb_skip_count);
  Inc(size_bytes, A.size_bytes);
  for i := 0 to 8 do
      Inc(pred[i], A.pred[i]);
  for i := 0 to 3 do
      Inc(pred16[i], A.pred16[i]);
  for i := 0 to 3 do
      Inc(pred_8x8_chroma[i], A.pred_8x8_chroma[i]);
  for i := 0 to 15 do
      Inc(ref[i], A.ref[i]);
  for i := 0 to 2 do
      Inc(ssd[i], A.ssd[i]);
end;

end.  
