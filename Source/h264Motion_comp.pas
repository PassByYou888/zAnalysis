{ ****************************************************************************** }
{ * h264Motion_comp.pas        by qq600585                                     * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264Motion_comp;

{$INCLUDE zDefine.inc}

interface

uses
  h264Common, h264Util, h264Frame, h264Stdint, CoreClasses;

type
  TMotionCompensation = class
  public
    procedure Compensate(const fref: PFrame; mv: TMotionvec; mbx, mby: int32_t; Dst: uint8_p);
    procedure CompensateQPelXY(const fref: PFrame; QX, qy: int32_t; Dst: uint8_p);
    procedure CompensateChroma(const fref: PFrame; mv: TMotionvec; mbx, mby: int32_t; dstU, dstV: uint8_p);
    procedure CompensateChromaQpelXY(const fref: PFrame; QX, qy: int32_t; dstU, dstV: uint8_p);
  end;

var
  mc_chroma_8x8: mc_chroma_func_t;

procedure motion_compensate_init;

implementation

procedure TMotionCompensation.Compensate(const fref: PFrame; mv: TMotionvec; mbx, mby: int32_t; Dst: uint8_p);
var
  X, Y,
    fx, fy: int32_t; // fullpel position
  stride: int32_t;
  J: uint32_t;
begin
  X := mbx * 64 + mv.X;
  Y := mby * 64 + mv.Y;
  // qpel or hpel / fullpel
  if (X and 1 + Y and 1) > 0 then
      CompensateQPelXY(fref, X, Y, Dst)
  else
    begin
      stride := fref^.stride;
      fx := (X + FRAME_PADDING_W * 4) shr 2;
      fy := (Y + FRAME_PADDING_W * 4) shr 2;
      J := (Y and 2) or (X and 2 shr 1);
      DSP.pixel_loadu_16x16(Dst, fref^.luma_mc[J] - fref^.frame_mem_offset + fy * stride + fx, stride);
    end;
end;

procedure TMotionCompensation.CompensateQPelXY(const fref: PFrame; QX, qy: int32_t; Dst: uint8_p);
const
  {
    plane 1/2 idx
    0..3 - fpelx/y -> fpel/h/v/hv
    4, 5 - fpelx+1/y  fpel/v
    6, 7 - fpelx/y+1  fpel/h
    index: delta y, delta x
  }
  qpel_plane_idx: array [0 .. 3, 0 .. 3, 0 .. 1] of uint8_t = (
    ((0, 0), (0, 1), (1, 1), (1, 4)),
    ((0, 2), (1, 2), (1, 3), (1, 5)),
    ((2, 2), (2, 3), (3, 3), (3, 5)),
    ((2, 6), (2, 7), (3, 7), (5, 7))
    );
var
  stride: int32_t;
  fx, fy: int32_t; // fullpel
  dx, dy: int8_t;  // delta: qpelx/y - fpelx/y * 4
  p1, p2: uint8_p;
  i: int32_t;

begin
  stride := fref^.stride;
  Inc(QX, FRAME_PADDING_W * 4);
  Inc(qy, FRAME_PADDING_W * 4);
  fx := QX shr 2;
  fy := qy shr 2;
  dx := QX and 3;
  dy := qy and 3;
  i := fy * stride + fx - fref^.frame_mem_offset;
  p1 := fref^.luma_mc_qpel[qpel_plane_idx[dy, dx, 0]];
  p2 := fref^.luma_mc_qpel[qpel_plane_idx[dy, dx, 1]];
  if p1 = p2 then
      DSP.pixel_loadu_16x16(Dst, p1 + i, stride)
  else
      DSP.pixel_avg_16x16(p1 + i, p2 + i, Dst, stride);
end;

procedure TMotionCompensation.CompensateChroma(const fref: PFrame; mv: TMotionvec; mbx, mby: int32_t; dstU, dstV: uint8_p);
var
  X, Y,
    fx, fy: int32_t; // chroma fullpel
  dx, dy: int32_t;
  coef: array [0 .. 3] of uint8_t;
  i, stride: int32_t;
begin
  X := mbx * 64 + mv.X; // qpel position
  Y := mby * 64 + mv.Y;
  CompensateChromaQpelXY(fref, X, Y, dstU, dstV);
end;

procedure TMotionCompensation.CompensateChromaQpelXY(const fref: PFrame; QX, qy: int32_t; dstU, dstV: uint8_p);
var
  fx, fy: int32_t; // chroma fullpel
  dx, dy: int32_t;
  coef: array [0 .. 3] of uint8_t;
  i, stride: int32_t;
begin
  stride := fref^.stride_c;
  Inc(QX, FRAME_PADDING_W * 4); // qpel position
  Inc(qy, FRAME_PADDING_W * 4);
  fx := SAR32(QX, 3);
  fy := SAR32(qy, 3);
  dx := QX and 7;
  dy := qy and 7;

  coef[0] := (8 - dx) * (8 - dy);
  coef[1] := dx * (8 - dy);
  coef[2] := (8 - dx) * dy;
  coef[3] := dx * dy;
  i := fy * stride + fx - fref^.frame_mem_offset_cr;

  DSP.mc_chroma_8x8(fref^.plane_dec[1] + i, dstU, stride, @coef);
  DSP.mc_chroma_8x8(fref^.plane_dec[2] + i, dstV, stride, @coef);
end;

(* ******************************************************************************
  motion_compensate_chroma
*)
procedure mc_chroma_8x8_pas(Src, Dst: uint8_p; const stride: int32_t; coef: uint8_p);
var
  i, J: int32_t;
begin
  for J := 0 to 7 do
    begin
      for i := 0 to 7 do
          Dst[i] := (coef[0] * Src[i] + coef[1] * Src[i + 1] + coef[2] * Src[i + stride] + coef[3] * Src[i + stride + 1] + 32) shr 6;
      Inc(Dst, 16);
      Inc(Src, stride);
    end;
end;

(* ******************************************************************************
  motion_compensate_init
*)
procedure motion_compensate_init;
begin
  mc_chroma_8x8 := @mc_chroma_8x8_pas;
end;

end.  
