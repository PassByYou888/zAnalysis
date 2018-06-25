{ ****************************************************************************** }
{ * h264Intra_pred.pas        by qq600585                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264Intra_pred;

{$I zDefine.inc}

interface

uses
  h264stdint, h264common, h264util, h264pixel, CoreClasses;

type
  TPredict4x4Func   = procedure(src, dst: uint8_p; stride: int32_t);
  TPredict16x16Func = procedure(src, dst: uint8_p);

  TIntraPredictor = class
  private
    mbcmp_16x16, mbcmp_8x8, mbcmp_4x4: mbcmp_func_t;
    pred4_cache: array [0 .. 8] of uint8_p; // cache for 4x4 predicted pixels
  public
    // luma
    pixels, prediction: uint8_p;
    frame_stride: int32_t;
    // chroma
    pixels_c, prediction_c: array [0 .. 1] of uint8_p;
    stride_c: int32_t;
    pixel_cache: uint8_p;
    mb_width: int32_t;

    constructor Create;
    destructor Destroy; override;
    procedure UseSATDCompare;
    procedure Predict_4x4(mode: int32_t; ref: uint8_p; mbx, mby, n: int32_t);
    procedure Predict_8x8_cr(mode: int32_t; refU, refV: uint8_p; mbx, mby: int32_t);
    procedure Predict_16x16(mode: int32_t; mbx, mby: int32_t);

    // Get best mode for i4x4 prediction. Also stores the predicted pixels
    function Analyse_4x4(const ref: uint8_p; const mbx, mby, n: int32_t): int32_t;

    procedure Analyse_8x8_cr(refU, refV: uint8_p; mbx, mby: int32_t; out mode: int32_t);
    procedure Analyse_16x16(mbx, mby: int32_t; out mode: int32_t; out score: int32_t);
  end;

var
  predict_top16, predict_left16, predict_plane16: TPredict16x16Func;

procedure intra_pred_init;

implementation

uses DoStatusIO;

const
  I4x4CACHE_STRIDE = 16;

  (* top *)
procedure predict_top4(src, dst: uint8_p; sstride: int32_t);
var
  p: int32_t;
  i: int32_t;
begin
  dec(src, sstride);
  p := int32_p(src)^;
  for i := 0 to 3 do
    begin
      int32_p(dst)^ := p;
      inc(dst, I4x4CACHE_STRIDE);
    end;
end;

(* left *)
procedure predict_left4(src, dst: uint8_p; sstride: int32_t);
var
  i, p: int32_t;
begin
  dec(src);
  for i := 0 to 3 do
    begin
      p := (src^ shl 8) or src^;
      int32_p(dst)^ := (p shl 16) or p;
      inc(src, sstride);
      inc(dst, I4x4CACHE_STRIDE);
    end;
end;

(* dc *)
procedure predict_dc4(src, dst: uint8_p; sstride: int32_t; mbx, mby, n: uint16_t);
var
  has_top, has_left: boolean;
  dc, i, shift: int32_t;
begin
  has_top := (mby > 0) or not(n in [0, 1, 4, 5]);
  has_left := (mbx > 0) or not(n in [0, 2, 8, 10]);
  dc := 0;
  shift := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(dc, src[i - sstride]);
      shift := 2;
    end;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(dc, src[-1 + i * sstride]);
      inc(shift, 2);
    end;

  if shift = 4 then
      dc := (dc + 4) shr 3
  else if shift = 2 then
      dc := (dc + 2) shr 2
  else
      dc := 128;
  dc := dc or (dc shl 8) or (dc shl 16) or (dc shl 24); // spread

  for i := 0 to 3 do
    begin
      int32_p(dst)^ := dc;
      inc(dst, I4x4CACHE_STRIDE);
    end;
end;

{ 8.3.1.2.4  Specification of Intra_4x4_Diagonal_Down_Left prediction mode
  If x is equal to 3 and y is equal to 3,
  pred4x4L[x, y] = ( p[6, -1] + 3 * p[7, -1] + 2 ) >> 2
  Otherwise (x is not equal to 3 or y is not equal to 3),
  pred4x4L[x, y] = ( p[x + y, -1] + 2 * p[x + y + 1, -1] + p[x + y + 2, -1] + 2 ) >> 2
}
procedure predict_ddl4(src, dst: uint8_p; sstride: int32_t);
var
  x, y: int32_t;
begin
  src := src - sstride;
  for y := 0 to 3 do
    for x := 0 to 3 do
        dst[y * I4x4CACHE_STRIDE + x] := (src[x + y]
        + src[x + y + 1] * 2
        + src[x + y + 2] + 2) shr 2;
  dst[3 * I4x4CACHE_STRIDE + 3] := (src[6] + 3 * src[7] + 2) shr 2
end;

{ 8.3.1.2.5  Specification of Intra_4x4_Diagonal_Down_Right prediction mode
  If x is greater than y,
  pred4x4L[x, y] = ( p[x - y - 2, -1]   + 2 * p[x - y - 1, -1]  + p[x - y, -1] + 2 ) >> 2
  Otherwise if x is less than y,
  pred4x4L[x, y] = ( p[-1, y - x - 2]   + 2 * p[-1, y - x - 1]  + p[-1, y - x] + 2 ) >> 2
  Otherwise (x is equal to y),
  pred4x4L[x, y] = ( p[0, -1] + 2 * p[-1, -1] + p[-1, 0] + 2 ) >> 2
}
procedure predict_ddr4(src, dst: uint8_p; sstride: int32_t);
var
  x, y: int32_t;
begin
  for y := 0 to 3 do
    for x := 0 to 3 do
      if x > y then
          dst[y * I4x4CACHE_STRIDE + x] := (src[x - y - 2 - sstride]
          + src[x - y - 1 - sstride] * 2
          + src[x - y - sstride] + 2) shr 2
      else if x < y then
          dst[y * I4x4CACHE_STRIDE + x] := (src[-1 + (y - x - 2) * sstride]
          + src[-1 + (y - x - 1) * sstride] * 2
          + src[-1 + (y - x) * sstride] + 2) shr 2
      else { x = y }
          dst[y * I4x4CACHE_STRIDE + x] := (src[-sstride]
          + src[-1 - sstride] * 2
          + src[-1] + 2) shr 2
end;

(* vertical right *)
procedure predict_vr4(src, dst: uint8_p; stride: int32_t);
var
  z, x, y, i: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      begin
        z := 2 * x - y;
        if z >= 0 then
          begin
            i := x - (y div 2) - stride;
            if (z and 1) = 0 then
                dst[x + y * I4x4CACHE_STRIDE] := (src[i - 1]
                + src[i] + 1) div 2
            else
                dst[x + y * I4x4CACHE_STRIDE] := (src[i - 2]
                + src[i - 1] * 2
                + src[i] + 2) div 4
          end
        else if z = -1 then
            dst[x + y * I4x4CACHE_STRIDE] := (src[-1] + src[-1 - stride] * 2 + src[-stride] + 2) div 4
        else
            dst[x + y * I4x4CACHE_STRIDE] := (src[-1 + (y - x - 1) * stride]
            + src[-1 + (y - x - 2) * stride] * 2
            + src[-1 + (y - x - 3) * stride] + 2) div 4;
      end;
end;

(* vertical left *)
procedure predict_vl4(src, dst: uint8_p; stride: int32_t);
var
  x, y: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      if (y and 1) = 1 then
          dst[x + y * I4x4CACHE_STRIDE] := (src[x + (y div 2) - stride]
          + src[x + (y div 2) + 1 - stride] * 2
          + src[x + (y div 2) + 2 - stride] + 2) div 4
        // P[x,y] = (S[x+(y/2),-1] + 2 * S[x+(y/2)+1,-1] + S[x+(y/2)+2,-1] + 2) / 4
      else
          dst[x + y * I4x4CACHE_STRIDE] := (src[x + (y div 2) - stride]
          + src[x + (y div 2) + 1 - stride] + 1) div 2;
  // P[x,y] = (S[x+(y/2),-1] + S[x+(y/2)+1,-1] + 1) / 2
end;

(* horiz down *)
procedure predict_hd4(src, dst: uint8_p; stride: int32_t);
var
  z, x, y, i: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      begin
        z := 2 * y - x;
        if z >= 0 then
          begin
            i := y - (x div 2);
            if (z and 1) = 0 then
                dst[x + y * I4x4CACHE_STRIDE] := (src[-1 + (i - 1) * stride]
                + src[-1 + i * stride] + 1) div 2
              // P[x,y] = (S[-1,y-(x/2)-1] + S[-1,y-(x/2)] + 1) / 2
            else
                dst[x + y * I4x4CACHE_STRIDE] := (src[-1 + (i - 2) * stride]
                + src[-1 + (i - 1) * stride] * 2
                + src[-1 + i * stride] + 2) div 4
              // P[x,y] = (S[-1,y-(x/2)-2] + 2 * S[-1,y-(x/2)-1] + S[-1,y-(x/2)] + 2) / 4
          end
        else if z = -1 then
            dst[x + y * I4x4CACHE_STRIDE] := (src[-1] + src[-1 - stride] * 2 + src[-stride] + 2) div 4
          // P[x,y] = (S[-1,0] + 2 * S[-1,-1] + S[0,-1] + 2) / 4
        else
          begin
            i := x - y - 1 - stride;
            dst[x + y * I4x4CACHE_STRIDE] := (src[i]
              + src[i - 1] * 2
              + src[i - 2] + 2) div 4;
          end;
        // P[x,y] = (S[x-y-1,-1] + 2 * S[x-y-2,-1] + S[x-y-3,-1] + 2) / 4
      end;
end;

{ 8.3.1.2.9 Specification of Intra_4x4_Horizontal_Up prediction mode
  zHU be set equal to x + 2 * y.
  - If zHU is equal to 0, 2, or 4
  pred4x4L[ x, y ] = ( p[ -1, y + ( x >> 1 ) ] + p[ -1, y + ( x >> 1 ) + 1 ] + 1 ) >> 1
  - Otherwise, if zHU is equal to 1 or 3
  pred4x4L[ x, y ] = ( p[ -1, y + ( x >> 1 ) ] + 2 * p[ -1, y + ( x >> 1 ) + 1 ] + p[ -1, y + ( x >> 1 ) + 2 ] + 2 ) >> 2
  - Otherwise, if zHU is equal to 5,
  pred4x4L[ x, y ] = ( p[ -1, 2 ] + 3 * p[ -1, 3 ] + 2 ) >> 2
  - Otherwise (zHU is greater than 5),
  pred4x4L[ x, y ] = p[ -1, 3 ]
}
procedure predict_hu4(src, dst: uint8_p; stride: int32_t);
var
  z, x, y, i: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      begin
        z := x + 2 * y;
        if (z >= 0) and (z < 5) then
          begin
            i := y + (x div 2);
            if (z and 1) = 0 then
                dst[x + y * I4x4CACHE_STRIDE] := (src[-1 + i * stride]
                + src[-1 + (i + 1) * stride] + 1) shr 1
            else
                dst[x + y * I4x4CACHE_STRIDE] := (src[-1 + i * stride]
                + src[-1 + (i + 1) * stride] * 2
                + src[-1 + (i + 2) * stride] + 2) shr 2
          end
        else if z = 5 then
            dst[x + y * I4x4CACHE_STRIDE] := (src[-1 + 2 * stride]
            + src[-1 + 3 * stride] * 3 + 2) shr 2
        else
            dst[x + y * I4x4CACHE_STRIDE] := src[-1 + 3 * stride];
      end;
end;

(* ******************************************************************************
  8.3.2 Intra_16x16 prediction process for luma samples
  ****************************************************************************** *)
procedure predict_top16_pas(src, dst: uint8_p);
var
  p1, p2: int64_t;
  i: int32_t;
begin
  p1 := int64_p(src + 1)^;
  p2 := int64_p(src + 9)^;
  for i := 0 to 7 do
    begin
      int64_p(dst)^ := p1;
      int64_p(dst + 8)^ := p2;
      int64_p(dst + 16)^ := p1;
      int64_p(dst + 24)^ := p2;
      inc(dst, 32);
    end;
end;

procedure predict_left16_pas(src, dst: uint8_p);
var
  i: int32_t;
  v: int64_t;
begin
  inc(src, 18);
  for i := 0 to 15 do
    begin
      v := (src^ shl 24) or (src^ shl 16) or (src^ shl 8) or src^;
      v := v or (v shl 32);
      int64_p(dst)^ := v;
      int64_p(dst + 8)^ := v;
      inc(src);
      inc(dst, 16);
    end;
end;

procedure predict_dc16(src, dst: uint8_p; const mbx, mby: uint16_t);
var
  dc, i, avail: int32_t;
begin
  dc := 0;
  avail := 0;
  if mby > 0 then
    begin
      for i := 1 to 16 do
          inc(dc, src[i]);
      inc(avail);
    end;
  if mbx > 0 then
    begin
      for i := 18 to 33 do
          inc(dc, src[i]);
      inc(avail);
    end;

  if avail = 2 then
      dc := (dc + 16) shr 5
  else if avail = 1 then
      dc := (dc + 8) shr 4
  else
      dc := 128;

  FillPtrByte(dst, 256, uint8_t(dc));
end;

procedure predict_plane16_pas(src, dst: uint8_p);
var
  x, y: int32_t;
  a, b, c, d, h, v, i: int32_t;

begin
  h := 0;
  v := 0;
  inc(src);

  for i := 0 to 7 do
    begin
      inc(h, (i + 1) * (src[8 + i] - src[6 - i]));
      inc(v, (i + 1) * (src[17 + 8 + i] - src[17 + 6 - i]));
    end;

  a := 16 * (src[15 + 17] + src[15]) + 16;
  b := Sar16(SmallInt(5 * h + 32), 6);
  c := Sar16(SmallInt(5 * v + 32), 6);

  for y := 0 to 15 do
    begin
      d := a + c * (y - 7);
      for x := 0 to 15 do
        begin
          i := b * (x - 7) + d;
          if i < 0 then
              dst[x] := 0
          else
            begin
              i := i shr 5;
              if i > 255 then
                  i := 255;
              dst[x] := uint8_t(i);
            end;
        end;
      inc(dst);
    end;
end;

(* ******************************************************************************
  8.3.3 Intra prediction process for chroma samples
  ****************************************************************************** *)
procedure predict_dc8(src, dst: uint8_p; sstride: int32_t; mbx, mby: uint16_t);
var
  has_top, has_left: boolean;
  i, k: int32_t;
  dc, shift: int32_t;
  dcf: array [0 .. 3] of uint8_t;

begin
  has_top := mby > 0;
  has_left := mbx > 0;

  // 0
  dc := 0;
  shift := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(dc, src[i - sstride]);
      shift := 2;
    end;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(dc, src[-1 + i * sstride]);
      inc(shift, 2);
    end;
  if shift = 4 then
      dc := (dc + 4) shr 3
  else if shift = 2 then
      dc := (dc + 2) shr 2
  else
      dc := 128;
  dcf[0] := dc;

  // 1
  dc := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(dc, src[4 + i - sstride]);
      dc := (dc + 2) shr 2;
    end
  else if has_left then
    begin
      for i := 0 to 3 do
          inc(dc, src[-1 + i * sstride]);
      dc := (dc + 2) shr 2;
    end
  else
      dc := 128;
  dcf[1] := dc;

  // 2
  dc := 0;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(dc, src[-1 + (i + 4) * sstride]);
      dc := (dc + 2) shr 2;
    end
  else if has_top then
    begin
      for i := 0 to 3 do
          inc(dc, src[i - sstride]);
      dc := (dc + 2) shr 2;
    end
  else
      dc := 128;
  dcf[2] := dc;

  // 3
  dc := 0;
  shift := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(dc, src[4 + i - sstride]);
      shift := 2;
    end;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(dc, src[-1 + (4 + i) * sstride]);
      inc(shift, 2);
    end;
  if shift = 4 then
      dc := (dc + 4) shr 3
  else if shift = 2 then
      dc := (dc + 2) shr 2
  else
      dc := 128;
  dcf[3] := dc;

  // write
  for i := 0 to 3 do
    begin
      for k := 0 to 3 do
          dst[k] := dcf[0];
      for k := 4 to 7 do
          dst[k] := dcf[1];
      inc(dst, 16);
    end;

  for i := 0 to 3 do
    begin
      for k := 0 to 3 do
          dst[k] := dcf[2];
      for k := 4 to 7 do
          dst[k] := dcf[3];
      inc(dst, 16);
    end;
end;

procedure predict_top8(src, dst: uint8_p; sstride: int32_t);
var
  p: int64_t;
  i: int32_t;
begin
  dec(src, sstride);
  p := int64_p(src)^;
  for i := 0 to 7 do
    begin
      int64_p(dst)^ := p;
      inc(dst, 16);
    end;
end;

procedure predict_left8(src, dst: uint8_p; sstride: int32_t);
var
  i, j: int32_t;
begin
  dec(src);
  for i := 0 to 7 do
    begin
      for j := 0 to 7 do
          dst[j] := src^;
      inc(src, sstride);
      inc(dst, 16);
    end;
end;

procedure predict_plane8(src, dst: uint8_p; stride: int32_t);
var
  x, y: int32_t;
  a, b, c, d, h, v, i: int32_t;

begin
  h := 0;
  v := 0;

  for x := 0 to 3 do
      inc(h, (x + 1) * (src[-stride + 4 + x] - src[-stride + 2 - x]));
  for y := 0 to 3 do
      inc(v, (y + 1) * (src[(4 + y) * stride - 1] - src[(2 - y) * stride - 1]));

  a := 16 * (src[7 * stride - 1] + src[-stride + 7]) + 16;
  b := Sar16(17 * h + 16, 5);
  c := Sar16(17 * v + 16, 5);

  for y := 0 to 7 do
    begin
      d := a + c * (y - 3);
      for x := 0 to 7 do
        begin
          i := b * (x - 3) + d;
          if i < 0 then
              dst[x] := 0
          else
            begin
              i := i shr 5;
              if i > 255 then
                  i := 255;
              dst[x] := uint8_t(i);
            end;
        end;
      inc(dst, 16);
    end;
end;

const
  Predict4x4Funcs: array [INTRA_PRED_TOP .. INTRA_PRED_HU] of TPredict4x4Func = (
{$IFDEF FPC}@{$ENDIF FPC}predict_top4,
{$IFDEF FPC}@{$ENDIF FPC}predict_left4,
    nil, // INTRA_PRED_DC is different
{$IFDEF FPC}@{$ENDIF FPC}predict_ddl4,
{$IFDEF FPC}@{$ENDIF FPC}predict_ddr4,
{$IFDEF FPC}@{$ENDIF FPC}predict_vr4,
{$IFDEF FPC}@{$ENDIF FPC}predict_hd4,
{$IFDEF FPC}@{$ENDIF FPC}predict_vl4,
{$IFDEF FPC}@{$ENDIF FPC}predict_hu4
    );

  { TIntraPredictor }

constructor TIntraPredictor.Create;
var
  i: int32_t;
begin
  inherited Create;
  mbcmp_16x16 := dsp.sad_16x16;
  mbcmp_8x8 := dsp.sad_8x8;
  mbcmp_4x4 := dsp.sad_4x4;
  pred4_cache[0] := fev_malloc(9 * 4 * I4x4CACHE_STRIDE);
  for i := 1 to 8 do
      pred4_cache[i] := pred4_cache[i - 1] + 4 * I4x4CACHE_STRIDE;
end;

destructor TIntraPredictor.Destroy;
begin
  fev_free(pred4_cache[0]);
  inherited Destroy;
end;

procedure TIntraPredictor.UseSATDCompare;
begin
  mbcmp_16x16 := dsp.satd_16x16;
  mbcmp_8x8 := dsp.satd_8x8;
  mbcmp_4x4 := dsp.satd_4x4;
end;

procedure TIntraPredictor.Predict_4x4(mode: int32_t; ref: uint8_p; mbx, mby, n: int32_t);
begin
  if mode = INTRA_PRED_DC then
      predict_dc4(ref, prediction + block_offset4[n], frame_stride, mbx, mby, n)
  else
      Predict4x4Funcs[mode](ref, prediction + block_offset4[n], frame_stride);
end;

procedure TIntraPredictor.Predict_8x8_cr(mode: int32_t; refU, refV: uint8_p; mbx, mby: int32_t);
begin
  case mode of
    INTRA_PRED_CHROMA_DC:
      begin
        predict_dc8(refU, prediction_c[0], stride_c, mbx, mby);
        predict_dc8(refV, prediction_c[1], stride_c, mbx, mby);
      end;
    INTRA_PRED_CHROMA_TOP:
      begin
        predict_top8(refU, prediction_c[0], stride_c);
        predict_top8(refV, prediction_c[1], stride_c);
      end;
    INTRA_PRED_CHROMA_LEFT:
      begin
        predict_left8(refU, prediction_c[0], stride_c);
        predict_left8(refV, prediction_c[1], stride_c);
      end;
    INTRA_PRED_CHROMA_PLANE:
      begin
        predict_plane8(refU, prediction_c[0], stride_c);
        predict_plane8(refV, prediction_c[1], stride_c);
      end
    else
      DoStatus('mb_intra_pred_chroma error: unknown predict mode');
  end;
end;

procedure TIntraPredictor.Predict_16x16(mode: int32_t; mbx, mby: int32_t);
begin
  case mode of
    INTRA_PRED_DC: predict_dc16(pixel_cache, prediction, mbx, mby);
    INTRA_PRED_TOP: predict_top16(pixel_cache, prediction);
    INTRA_PRED_LEFT: predict_left16(pixel_cache, prediction);
    INTRA_PRED_PLANE: predict_plane16(pixel_cache, prediction);
    else
      DoStatus('mb_intra_pred_16 error: unknown predict mode');
  end;
end;

function TIntraPredictor.Analyse_4x4(const ref: uint8_p; const mbx, mby, n: int32_t): int32_t;
const
  TopMask        = 65484; { !(n in [0, 1, 4, 5]) }
  LeftMask       = 64250; { !(n in [0, 2, 8, 10]) }
  TopLeftMask    = 64200; { n in [3, 6, 7, 9, 11, 12, 13, 14, 15] }
  InsideTTRMask  = 22340; { top/topright,      n in [2, 6, 8, 9, 10, 12, 14] }
  InsideLTTRMask = 21056; { left/top/topright, n in [6, 9, 12, 14] }
  OutsideTTRMask = 22391; { top/topright,      !(n in [3, 7, 11, 13, 15]) }
var
  pix: uint8_p;
  modes, mode: int32_t;
  score, min_score: int32_t;
  mask: int32_t;
  has_top, has_left, has_tl, has_inside_ttr, has_inside_lttr, has_outside_ttr: boolean;
begin
  pix := pixels + block_offset4[n];

  // always run dc
  predict_dc4(ref, pred4_cache[INTRA_PRED_DC], frame_stride, mbx, mby, n);
  min_score := mbcmp_4x4(pix, pred4_cache[INTRA_PRED_DC], I4x4CACHE_STRIDE);
  result := INTRA_PRED_DC;
  modes := 0;

  // rules based on the 4x4 block position inside 16x16 macroblock
  mask := 1 shl n;
  has_top := (TopMask and mask) > 0;
  has_left := (LeftMask and mask) > 0;
  has_tl := (TopLeftMask and mask) > 0;
  has_inside_ttr := (InsideTTRMask and mask) > 0;
  has_inside_lttr := (InsideLTTRMask and mask) > 0;
  has_outside_ttr := (OutsideTTRMask and mask) > 0;

  // enable modes that need:
  // top pixels
  if (mby > 0) or has_top then
      modes := modes or (1 shl INTRA_PRED_TOP);
  // left pixels
  if (mbx > 0) or has_left then
      modes := modes or (1 shl INTRA_PRED_LEFT) or (1 shl INTRA_PRED_HU);
  // top & left pixels
  if ((mbx > 0) and (mby > 0)) or has_tl then
      modes := modes or (1 shl INTRA_PRED_DDR) or (1 shl INTRA_PRED_VR) or (1 shl INTRA_PRED_HD);
  // top & top-right pixels
  if ((mby > 0) and (mbx < mb_width - 1) and has_outside_ttr) or has_inside_ttr then
      modes := modes or (1 shl INTRA_PRED_DDL);
  // left, top & top-right pixels
  if ((mby > 0) and (mbx > 0) and (mbx < mb_width - 1) and has_outside_ttr) or has_inside_lttr then
      modes := modes or (1 shl INTRA_PRED_VL);

  // run all enabled modes
  for mode := 0 to 8 do
    begin
      if ((1 shl mode) and modes) > 0 then
        begin
          Predict4x4Funcs[mode](ref, pred4_cache[mode], frame_stride);
          score := mbcmp_4x4(pix, pred4_cache[mode], I4x4CACHE_STRIDE);
          if score < min_score then
            begin
              min_score := score;
              result := mode;
            end;
        end;
    end;

  // restore best mode's prediction from cache
  pixel_load_4x4(prediction + block_offset4[n], pred4_cache[result], I4x4CACHE_STRIDE);
end;

procedure TIntraPredictor.Analyse_8x8_cr(refU, refV: uint8_p; mbx, mby: int32_t; out mode: int32_t);
var
  mscore, cscore: int32_t;
  cmp: mbcmp_func_t;

  procedure ipmode(m: uint8_t);
  begin
    cscore := cmp(pixels_c[0], prediction_c[0], 16);
    inc(cscore, cmp(pixels_c[1], prediction_c[1], 16));
    if cscore < mscore then
      begin
        mode := m;
        mscore := cscore;
      end;
  end;

begin
  mscore := MaxInt;
  cmp := mbcmp_8x8;

  // dc
  predict_dc8(refU, prediction_c[0], stride_c, mbx, mby);
  predict_dc8(refV, prediction_c[1], stride_c, mbx, mby);
  ipmode(INTRA_PRED_CHROMA_DC);

  // top - vertical
  if (mby > 0) then
    begin
      predict_top8(refU, prediction_c[0], stride_c);
      predict_top8(refV, prediction_c[1], stride_c);
      ipmode(INTRA_PRED_CHROMA_TOP);
    end;

  // left - horizontal
  if (mbx > 0) then
    begin
      predict_left8(refU, prediction_c[0], stride_c);
      predict_left8(refV, prediction_c[1], stride_c);
      ipmode(INTRA_PRED_CHROMA_LEFT);
    end;

  // plane
  if (mbx > 0) and (mby > 0) then
    begin
      predict_plane8(refU, prediction_c[0], stride_c);
      predict_plane8(refV, prediction_c[1], stride_c);
      ipmode(INTRA_PRED_CHROMA_PLANE);
    end;

  // restore best mode
  Predict_8x8_cr(mode, refU, refV, mbx, mby);
end;

procedure TIntraPredictor.Analyse_16x16(mbx, mby: int32_t; out mode: int32_t; out score: int32_t);
var
  mscore, cscore: int32_t;
  cmp: mbcmp_func_t;

  procedure ipmode(m: uint8_t);
  begin
    cscore := cmp(pixels, prediction, 16);
    if cscore < mscore then
      begin
        mode := m;
        mscore := cscore;
      end;
  end;

begin
  mscore := MaxInt;
  cmp := mbcmp_16x16;

  // dc
  predict_dc16(pixel_cache, prediction, mbx, mby);
  ipmode(INTRA_PRED_DC);

  // vertical
  if (mby > 0) then
    begin
      predict_top16(pixel_cache, prediction);
      ipmode(INTRA_PRED_TOP);
    end;

  // horizontal
  if (mbx > 0) then
    begin
      predict_left16(pixel_cache, prediction);
      ipmode(INTRA_PRED_LEFT);
    end;

  // plane
  if (mbx > 0) and (mby > 0) then
    begin
      predict_plane16(pixel_cache, prediction);
      ipmode(INTRA_PRED_PLANE);
    end;

  score := mscore;
end;

(* ******************************************************************************
*)

procedure intra_pred_init;
begin
  predict_top16 := @predict_top16_pas;
  predict_left16 := @predict_left16_pas;
  predict_plane16 := @predict_plane16_pas;
end;

end.
