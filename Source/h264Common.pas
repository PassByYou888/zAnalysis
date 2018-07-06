{ ****************************************************************************** }
{ * h264Common.pas        by qq600585                                          * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264Common;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Stdint, h264Stats, h264_FPCGenericStructlist, CoreClasses, MemoryRaster;

const
  SLICE_P = 5;
  SLICE_I = 7;

  MB_I_4x4   = 0;
  MB_I_16x16 = 1;
  MB_P_16x16 = 2;
  MB_P_SKIP  = 3;
  MB_I_PCM   = 4;

  INTRA_PRED_TOP   = 0;
  INTRA_PRED_LEFT  = 1;
  INTRA_PRED_DC    = 2;
  INTRA_PRED_PLANE = 3; // I16x16
  INTRA_PRED_DDL   = 3; // I4x4
  INTRA_PRED_DDR   = 4;
  INTRA_PRED_VR    = 5;
  INTRA_PRED_HD    = 6;
  INTRA_PRED_VL    = 7;
  INTRA_PRED_HU    = 8;
  INTRA_PRED_NA    = 255;

  INTRA_PRED_CHROMA_DC    = 0;
  INTRA_PRED_CHROMA_LEFT  = 1;
  INTRA_PRED_CHROMA_TOP   = 2;
  INTRA_PRED_CHROMA_PLANE = 3;

  NZ_COEF_CNT_NA = 255;

  EG_MAX_ABS  = 2047; // = 2^12 / 2 - 1 (abs.maximum for exp-golomb encoding)
  MB_SKIP_MAX = EG_MAX_ABS * 2;

  { ordering of 8x8 luma blocks
    1 | 2
    --+--
    3 | 4

    ordering of 4x4 luma blocks
    0 |  1 |  4 |  5
    ---+----+----+---
    2 |  3 |  6 |  7
    ---+----+----+---
    8 |  9 | 12 | 13
    ---+----+----+---
    10 | 11 | 14 | 15
  }
  block_offset4: array [0 .. 15] of uint8_t = (
    0, 4, 64, 68,
    8, 12, 72, 76,
    128, 132, 192, 196,
    136, 140, 200, 204
    );

  { ordering of 4x4 chroma blocks
    c0       c1
    0 | 1 |  | 0 | 1
    ---+--|  |-- +--
    2 | 3 |  | 2 | 3
  }
  block_offset_chroma: array [0 .. 3] of uint8_t = (
    0, 4,
    64, 68
    );

  block_dc_order: array [0 .. 15] of uint8_t = (0, 1, 4, 5, 2, 3, 6, 7, 8, 9, 12, 13, 10, 11, 14, 15);

function is_intra(const M: int32_t): Boolean; inline;
function is_inter(const M: int32_t): Boolean; inline;

type
  // motion vector
  TMotionvec = packed record
    X, Y: int16_t;

{$IFNDEF FPC}
    // operator overloads
    class operator Equal(const A, b: TMotionvec): Boolean;
    class operator Add(const A, b: TMotionvec): TMotionvec;
    class operator Subtract(const A, b: TMotionvec): TMotionvec;
    class operator Multiply(const A: TMotionvec; multiplier: int32_t): TMotionvec;
    class operator Divide(const A: TMotionvec; Divisor: int32_t): TMotionvec;
{$ENDIF FPC}
  end;

  PMotionvec = ^TMotionvec;

{$IFDEF FPC}
  TMotionVectorList = specialize TGenericStructList<TMotionvec>;
{$ELSE FPC}
  TMotionVectorList = TGenericsList<TMotionvec>;
{$ENDIF FPC}

{$IFDEF FPC}
operator = (const A, b: TMotionvec): Boolean;
operator / (const A: TMotionvec; const Divisor: int32_t): TMotionvec;
operator * (const A: TMotionvec; const multiplier: int32_t): TMotionvec;
operator + (const A, b: TMotionvec): TMotionvec;
operator - (const A, b: TMotionvec): TMotionvec;
{$ENDIF FPC}

function XYToMVec(const X: int32_t; const Y: int32_t): TMotionvec; inline;

const
  ZERO_MV: TMotionvec = (X: 0; Y: 0);

type
  PFrame = ^TFrame;

  // residual block
  TBlock = packed record
    t0, t1, t1_signs: uint8_t;
    ncoef, nlevel: uint8_t;
    run_before: array [0 .. 15] of uint8_t;
    level: array [0 .. 15] of int16_t;
  end;

  // boundary strength
  TBSarray = array [0 .. 3, 0 .. 3] of uint8_t;

  // macroblock
  PMacroblock = ^TMacroblock;

  TMacroblock = packed record
    X, Y: int32_t; // position
    mbtype: int32_t;
    qp, qpc: uint8_t;
    chroma_qp_offset: int8_t;

    i4_pred_mode: array [0 .. 23] of uint8_t;
    { intra prediction mode for luma 4x4 blocks
      0..15  - blocks from current mb
      16..19 - top mb bottom row
      20..23 - left mb right column
    }
    i16_pred_mode: int32_t;    // intra 16x16 pred mode
    chroma_pred_mode: int32_t; // chroma intra pred mode

    mvp, mv_skip, mv: TMotionvec; // mvs: predicted, skip, coded
    fref: PFrame;                 // reference frame selected for inter prediction
    ref: int32_t;                  // reference frame L0 index
    cbp: int32_t;                  // cpb bitmask: 0..3 luma, 4..5 chroma u/v

    // luma
    pfenc, pfdec, pfpred: uint8_p;
    pixels: uint8_p;     // original pixels
    pred: uint8_p;       // predicted pixels
    mcomp: uint8_p;      // motion-compensated pixels (maps to pred!)
    pixels_dec: uint8_p; // decoded pixels

    // chroma
    pfenc_c, pfdec_c, pfpred_c: array [0 .. 1] of uint8_p;
    pixels_c, pred_c, mcomp_c, pixels_dec_c: array [0 .. 1] of uint8_p;

    // coef arrays
    dct: array [0 .. 24] of int16_p; // 0-15 - luma, 16-23 chroma, 24 - luma DC
    chroma_dc: array [0 .. 1, 0 .. 3] of int16_t;
    Block: array [0 .. 26] of TBlock; // 0-24 as in dct, 25/26 chroma_dc u/v

    // cache for speeding up the prediction process
    intra_pixel_cache: array [0 .. 33] of uint8_t;
    { 0,17 - top left pixel
      1..16 - pixels from top row
      18..33 - pixels from left column
    }

    // non-zero coef count of surrounding blocks for I4x4/I16x16/chroma ac blocks
    nz_coef_cnt: array [0 .. 23] of uint8_t;
    nz_coef_cnt_chroma_ac: array [0 .. 1, 0 .. 7] of uint8_t;
    nz_coef_cnt_dc: uint8_t;

    // me
    L0_mvp: array [0 .. 15] of TMotionvec; // predicted mv for L0 refs
    score_skip, score_skip_uv: int32_t;
    residual_bits: int32_t;

    // loopfilter
    mba, mbb: PMacroblock;
    bS_vertical, bS_horizontal: TBSarray;

    // analysis
    bitcost: int32_t;
  end;

  // frame
  TFrame = packed record
    ftype: int32_t; // slice type

    qp: int32_t;             // fixed quant parameter
    Num: int32_t;            // frame number
    mbs: PMacroblock;       // frame macroblocks
    num_ref_frames: int32_t; // L0 reference picture count

    // img data
    w, h: int32_t;                                  // width, height
    w_cr, h_cr: int32_t;                            // chroma w&h
    pw, ph: int32_t;                                // padded w&h
    mbw, mbh: int32_t;                              // macroblock width, height
    mem: array [0 .. 5] of uint8_p;                 // allocated memory
    plane: array [0 .. 2] of uint8_p;               // image planes
    luma_mc: array [0 .. 3] of uint8_p;             // luma planes for hpel interpolated samples (none, h, v, h+v)
    luma_mc_qpel: array [0 .. 7] of uint8_p;        // plane pointers for qpel mc
    plane_dec: array [0 .. 2] of uint8_p;           // decoded image planes
    stride, stride_c: int32_t;                      // luma stride, chroma stride
    frame_mem_offset, frame_mem_offset_cr: int32_t; // padding to image offset in bytes
    blk_offset: array [0 .. 15] of int32_t;         // 4x4 block offsets
    blk_chroma_offset: array [0 .. 3] of int32_t;   // 4x4 chroma block offsets
    filter_hv_temp: int16_p;                        // temp storage for fir filter
    refs: array [0 .. 15] of PFrame;               // L0 reference list

    // mb-adaptive quant data
    aq_table: uint8_p; // qp table
    qp_avg: Single;    // average quant

    // bitstream buffer
    bs_buf: uint8_p;

    // stats
    stats: TFrameStats;
    estimated_framebits: int32_t;
    qp_adj: int32_t;
  end;

  IInterPredCostEvaluator = class
    procedure SetQP(qp: int32_t); virtual; abstract;
    procedure SetMVPredAndRefIdx(const mvp: TMotionvec; const idx: int32_t); virtual; abstract;
    function bitcost(const mv: TMotionvec): int32_t; virtual; abstract;
  end;

procedure YV12ToRaster(const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h, stride, stride_cr: int32_t; const dest: TMemoryRaster; const forceITU_BT_709, lumaFull: Boolean); overload;
procedure YV12ToRaster(const sour: PFrame; const dest: TMemoryRaster); overload;
procedure RasterToYV12(const sour: TMemoryRaster; const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h: int32_t); overload;

var
  lookup_table_CCIR_601_1: array [0 .. 3] of int32_p;
  lookup_table_ITU_BT_709: array [0 .. 3] of int32_p;

implementation

{$IFNDEF FPC}


class operator TMotionvec.Equal(const A, b: TMotionvec): Boolean;
begin
  Result := int32_t(A) = int32_t(b);
end;

class operator TMotionvec.Add(const A, b: TMotionvec): TMotionvec;
begin
  Result.X := A.X + b.X;
  Result.Y := A.Y + b.Y;
end;

class operator TMotionvec.Subtract(const A, b: TMotionvec): TMotionvec;
begin
  Result.X := A.X - b.X;
  Result.Y := A.Y - b.Y;
end;

class operator TMotionvec.Multiply(const A: TMotionvec; multiplier: int32_t): TMotionvec;
begin
  Result.X := A.X * multiplier;
  Result.Y := A.Y * multiplier;
end;

class operator TMotionvec.Divide(const A: TMotionvec; Divisor: int32_t): TMotionvec;
begin
  Result.X := A.X div Divisor;
  Result.Y := A.Y div Divisor;
end;

{$ELSE}


operator = (const A, b: TMotionvec): Boolean; inline;
begin
  Result := int32_t(A) = int32_t(b);
end;

operator / (const A: TMotionvec; const Divisor: int32_t): TMotionvec;
begin
  Result.X := A.X div Divisor;
  Result.Y := A.Y div Divisor;
end;

operator * (const A: TMotionvec; const multiplier: int32_t): TMotionvec;
begin
  Result.X := A.X * multiplier;
  Result.Y := A.Y * multiplier;
end;

operator + (const A, b: TMotionvec): TMotionvec;
begin
  Result.X := A.X + b.X;
  Result.Y := A.Y + b.Y;
end;

operator - (const A, b: TMotionvec): TMotionvec;
begin
  Result.X := A.X - b.X;
  Result.Y := A.Y - b.Y;
end;
{$ENDIF FPC}


function XYToMVec(const X: int32_t; const Y: int32_t): TMotionvec;
begin
  Result.X := X;
  Result.Y := Y;
end;

function is_intra(const M: int32_t): Boolean; inline;
begin
  Result := M in [MB_I_4x4, MB_I_16x16, MB_I_PCM];
end;

function is_inter(const M: int32_t): Boolean; inline;
begin
  Result := M in [MB_P_16x16, MB_P_SKIP];
end;

procedure YV12ToRaster(const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h, stride, stride_cr: int32_t; const dest: TMemoryRaster; const forceITU_BT_709, lumaFull: Boolean);
// conversion works on 2x2 pixels at once, since they share chroma info
var
  Y, X: int32_t;
  p, pu, PV, T: uint8_p;   // source plane ptrs
  d: int32_t;              // dest index for topleft pixel
  r0, r1, r2, r4: int32_t; // scaled yuv values for rgb calculation
  t0, t1, t2, t3: int32_p; // lookup table ptrs
  row1, row2: PRasterColorEntry;

  function Clip(C: int32_t): uint8_t; inline;
  begin
    Result := uint8_t(C);
    if C > 255 then
        Result := 255
    else if C < 0 then
        Result := 0;
  end;

begin
  dest.SetSize(w, h);

  if forceITU_BT_709 then
    begin
      t0 := lookup_table_ITU_BT_709[0];
      t1 := lookup_table_ITU_BT_709[1];
      t2 := lookup_table_ITU_BT_709[2];
      t3 := lookup_table_ITU_BT_709[3];
    end
  else
    begin
      t0 := lookup_table_CCIR_601_1[0];
      t1 := lookup_table_CCIR_601_1[1];
      t2 := lookup_table_CCIR_601_1[2];
      t3 := lookup_table_CCIR_601_1[3];
    end;

  p := luma_ptr;
  pu := u_ptr;
  PV := v_ptr;

  for Y := 0 to dest.height shr 1 - 1 do
    begin

      row1 := PRasterColorEntry(dest.ScanLine[Y * 2]);
      row2 := PRasterColorEntry(dest.ScanLine[Y * 2 + 1]);

      for X := 0 to dest.width shr 1 - 1 do
        begin
          // row start relative index
          d := X * 2;
          r0 := t0[(PV + X)^]; // chroma
          r1 := t1[(pu + X)^] + t2[(PV + X)^];
          r2 := t3[(pu + X)^];
          T := p + d; // upper left luma

          // upper left/right luma
          r4 := T^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row1[d].R := Clip(r4 + r0);
          row1[d].g := Clip(r4 + r1);
          row1[d].b := Clip(r4 + r2);
          row1[d].A := 255;

          r4 := (T + 1)^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row1[d + 1].R := Clip(r4 + r0);
          row1[d + 1].g := Clip(r4 + r1);
          row1[d + 1].b := Clip(r4 + r2);
          row1[d + 1].A := 255;

          // lower left/right luma
          r4 := (T + stride)^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row2[d].R := Clip(r4 + r0);
          row2[d].g := Clip(r4 + r1);
          row2[d].b := Clip(r4 + r2);
          row2[d].A := 255;

          r4 := (T + 1 + stride)^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row2[d + 1].R := Clip(r4 + r0);
          row2[d + 1].g := Clip(r4 + r1);
          row2[d + 1].b := Clip(r4 + r2);
          row2[d + 1].A := 255;
        end;

      Inc(p, stride * 2);
      Inc(pu, stride_cr);
      Inc(PV, stride_cr);
    end;
end;

procedure YV12ToRaster(const sour: PFrame; const dest: TMemoryRaster);
begin
  YV12ToRaster(sour^.plane[0], sour^.plane[1], sour^.plane[2], sour^.w, sour^.h, sour^.stride, sour^.stride_c, dest, False, False);
end;

procedure RasterToYV12(const sour: TMemoryRaster; const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h: int32_t);
  function Clip(C: int32_t): uint8_t; inline;
  begin
    Result := uint8_t(C);
    if C > 255 then
        Result := 255
    else
      if C < 0 then
        Result := 0;
  end;

var
  nm: TMemoryRaster;
  i, J: int32_t;
  C: TRasterColorEntry;
  Y, u, v, uu, VV, cv, NV, CU, Nu: uint8_p;
  v01, v02, v11, v12, u01, u02, u11, u12: uint8_t;
begin
  if (sour.width <> w) or (sour.height <> h) then
    begin
      nm := TMemoryRaster.Create;
      nm.ZoomFrom(sour, w, h);
    end
  else
      nm := sour;

  Y := luma_ptr;
  uu := GetMemory(w * h * 2);
  u := uu;
  VV := @(uu[w * h]);
  v := VV;

  for J := 0 to h - 1 do
    for i := 0 to w - 1 do
      begin
        C.RGBA := nm.Pixel[i, J];
        Y^ := Clip(Trunc(0.256788 * C.R + 0.504129 * C.g + 0.097906 * C.b + 16));
        Inc(Y);
        u^ := Clip(Trunc(-0.148223 * C.R - 0.290993 * C.g + 0.439216 * C.b + 128));
        Inc(u);
        v^ := Clip(Trunc(0.439216 * C.R - 0.367788 * C.g - 0.071427 * C.b + 128));
        Inc(v);
      end;

  u := u_ptr;
  v := v_ptr;
  J := 0;
  while J < h do
    begin
      cv := VV + J * w;
      NV := VV + (J + 1) * w;
      CU := uu + J * w;
      Nu := uu + (J + 1) * w;

      i := 0;
      while i < w do
        begin
          v01 := (cv + i)^;
          v02 := (cv + i + 1)^;
          v11 := (NV + i)^;
          v12 := (NV + i + 1)^;
          v^ := (v01 + v02 + v11 + v12) div 4;

          u01 := (CU + i)^;
          u02 := (CU + i + 1)^;
          u11 := (Nu + i)^;
          u12 := (Nu + i + 1)^;
          u^ := (u01 + u02 + u11 + u12) div 4;

          Inc(v);
          Inc(u);
          Inc(i, 2);
        end;
      Inc(J, 2);
    end;

  FreeMemory(uu);

  if nm <> sour then
      DisposeObject(nm);
end;

procedure BuildLut;
const
  UV_CSPC_CCIR_601_1: array [0 .. 3] of Real = (1.4020, -0.3441, -0.7141, 1.7720); // CCIR 601-1
  UV_CSPC_ITU_BT_709: array [0 .. 3] of Real = (1.5701, -0.1870, -0.4664, 1.8556); // ITU.BT-709
var
  C, J: int32_t;
  v: Single;
begin
  for C := 0 to 3 do
    begin
      lookup_table_CCIR_601_1[C] := GetMemory(256 * 4);
      lookup_table_ITU_BT_709[C] := GetMemory(256 * 4);
    end;

  for C := 0 to 255 do
    begin
      v := C - 128;
      for J := 0 to 3 do
        begin
          lookup_table_CCIR_601_1[J, C] := Round(v * UV_CSPC_CCIR_601_1[J]);
          lookup_table_ITU_BT_709[J, C] := Round(v * UV_CSPC_ITU_BT_709[J]);
        end;
    end;
end;

procedure FreeLut;
var
  C: int32_t;
begin
  for C := 0 to 3 do
    begin
      FreeMemory(lookup_table_CCIR_601_1[C]);
      lookup_table_CCIR_601_1[C] := nil;

      FreeMemory(lookup_table_ITU_BT_709[C]);
      lookup_table_ITU_BT_709[C] := nil;
    end;
end;

initialization

BuildLut;

finalization

FreeLut;

end.  
