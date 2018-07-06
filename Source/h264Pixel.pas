{ ****************************************************************************** }
{ * h264Pixel.pas        by qq600585                                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264Pixel;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Stdint, h264Util, CoreClasses;

procedure pixel_load_4x4(dest, Src: uint8_p; stride: int32_t);
procedure pixel_save_4x4(Src, dest: uint8_p; stride: int32_t);
procedure pixel_init;

var
  sad_16x16, sad_8x8, sad_4x4, ssd_16x16, ssd_8x8, satd_4x4, satd_8x8, satd_16x16: mbcmp_func_t;
  var_16x16: mbstat_func_t;
  pixel_load_16x16, pixel_loadu_16x16, pixel_load_8x8, pixel_save_16x16, pixel_save_8x8: pixmove_func_t;
  pixel_add_4x4, pixel_sub_4x4: pixoper_func_t;
  pixel_avg_16x16: pixavg_func_t;

  (* ******************************************************************************
    ****************************************************************************** *)
implementation

function sad_16x16_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  X, Y: int32_t;
begin
  Result := 0;
  for Y := 0 to 15 do
    begin
      for X := 0 to 15 do
          Inc(Result, Abs(pix1[X] - pix2[X]));
      Inc(pix1, 16);
      Inc(pix2, stride);
    end;
end;

function sad_8x8_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  X, Y: int32_t;
begin
  Result := 0;
  for Y := 0 to 7 do
    begin
      for X := 0 to 7 do
          Inc(Result, Abs(pix1[X] - pix2[X]));
      Inc(pix1, 16);
      Inc(pix2, stride);
    end;
end;

function sad_4x4_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  X, Y: int32_t;
begin
  Result := 0;
  for Y := 0 to 3 do
    begin
      for X := 0 to 3 do
          Inc(Result, Abs(pix1[X] - pix2[X]));
      Inc(pix1, 16);
      Inc(pix2, stride);
    end;
end;

(* ******************************************************************************
  SSD
*)
function ssd_16x16_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  X, Y: int32_t;
begin
  Result := 0;
  for Y := 0 to 15 do
    begin
      for X := 0 to 15 do
          Inc(Result, (pix1[X] - pix2[X]) * (pix1[X] - pix2[X]));
      Inc(pix1, 16);
      Inc(pix2, stride);
    end;
end;

function ssd_8x8_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  X, Y: int32_t;
begin
  Result := 0;
  for Y := 0 to 7 do
    begin
      for X := 0 to 7 do
          Inc(Result, (pix1[X] - pix2[X]) * (pix1[X] - pix2[X]));
      Inc(pix1, 16);
      Inc(pix2, stride);
    end;
end;

(* ******************************************************************************
  variance 16x16
*)
function var_16x16_pas(pix: uint8_p): UInt32;
var
  X, Y: int32_t;
  s: uint16_t;  // sum
  SS: uint32_t; // sum squared
begin
  s := 0;
  SS := 0;
  for Y := 0 to 15 do
    begin
      for X := 0 to 15 do
        begin
          Inc(s, pix[X]);
          Inc(SS, pix[X] * pix[X]);
        end;
      Inc(pix, 16);
    end;
  Result := SS - (s * s div 256);
end;

(* ******************************************************************************
  SATD
*)
function satd_4x4_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
type
  matrix_t = array [0 .. 3, 0 .. 3] of int16_t;
var
  A, T: matrix_t;
  E, F, g, h: array [0 .. 3] of int16_t;
  i, J: int32_t;
begin
  for i := 0 to 3 do
    begin
      for J := 0 to 3 do
          A[i][J] := pix1[J] - pix2[J];
      Inc(pix1, 16);
      Inc(pix2, stride);
    end;

  for i := 0 to 3 do
    begin
      E[i] := A[0][i] + A[2][i];
      F[i] := A[0][i] - A[2][i];
      g[i] := A[1][i] + A[3][i];
      h[i] := A[1][i] - A[3][i];

      T[i][0] := E[i] + g[i];
      T[i][1] := E[i] - g[i];
      T[i][2] := F[i] + h[i];
      T[i][3] := F[i] - h[i];
    end;

  for i := 0 to 3 do
    begin
      E[i] := T[0][i] + T[2][i];
      F[i] := T[0][i] - T[2][i];
      g[i] := T[1][i] + T[3][i];
      h[i] := T[1][i] - T[3][i];
    end;
  for i := 0 to 3 do
    begin
      T[i][0] := E[i] + g[i];
      T[i][1] := E[i] - g[i];
      T[i][2] := F[i] + h[i];
      T[i][3] := F[i] - h[i];
    end;

  Result := 0;
  for i := 0 to 15 do
      Inc(Result, Abs(int16_p(@T)[i]));
end;

function satd_8x8_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  i: int32_t;
begin
  Result := 0;
  for i := 0 to 1 do
    begin
      Inc(Result, satd_4x4_pas(pix1, pix2, stride));
      Inc(Result, satd_4x4_pas(pix1 + 4, pix2 + 4, stride));
      Inc(pix1, 4 * 16);
      Inc(pix2, 4 * stride);
    end
end;

function satd_16x16_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  i: int32_t;
begin
  Result := 0;
  for i := 0 to 3 do
    begin
      Inc(Result, satd_4x4_pas(pix1, pix2, stride));
      Inc(Result, satd_4x4_pas(pix1 + 4, pix2 + 4, stride));
      Inc(Result, satd_4x4_pas(pix1 + 8, pix2 + 8, stride));
      Inc(Result, satd_4x4_pas(pix1 + 12, pix2 + 12, stride));
      Inc(pix1, 4 * 16);
      Inc(pix2, 4 * stride);
    end
end;

(* ******************************************************************************
  pixel_sub_8x8_pas
  subtract two 8x8 blocks, return uint16_t-sized results
*)
procedure pixel_sub_4x4_pas(pix1, pix2: uint8_p; Diff: int16_p);
var
  X, Y: int32_t;
begin
  for Y := 0 to 3 do
    begin
      for X := 0 to 3 do
          Diff[X] := pix1[X] - pix2[X];
      Inc(pix1, 16);
      Inc(pix2, 16);
      Inc(Diff, 4);
    end;
end;

(* ******************************************************************************
  pixel_add_8x8_pas
  addition of two 8x8 blocks, return clipped uint8_t-sized results
*)
procedure pixel_add_4x4_pas(pix1, pix2: uint8_p; Diff: int16_p);

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
  Y, X: int32_t;
begin
  for Y := 0 to 3 do
    begin
      for X := 0 to 3 do
          pix1[X] := Clip(Diff[X] + pix2[X]);
      Inc(pix1, 16);
      Inc(pix2, 16);
      Inc(Diff, 4);
    end;
end;

(* ******************************************************************************
  pixel_load_16x16
  load 16x16 pixel block from frame
*)
procedure pixel_load_16x16_pas(dest, Src: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 15 do
    begin
      CopyPtr(Src, dest, 16);
      Inc(Src, stride);
      Inc(dest, 16);
    end;
end;

procedure pixel_load_8x8_pas(dest, Src: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 7 do
    begin
      uint64_p(dest)^ := uint64_p(Src)^;
      Inc(Src, stride);
      Inc(dest, 16);
    end;
end;

procedure pixel_load_4x4(dest, Src: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 3 do
    begin
      uint32_p(dest)^ := uint32_p(Src)^;
      Inc(Src, stride);
      Inc(dest, 16);
    end;
end;

(* ******************************************************************************
  pixel_save_16x16
  save 16x16 pixel block to frame
*)
procedure pixel_save_16x16_pas(Src, dest: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 15 do
    begin
      CopyPtr(Src, dest, 16);
      Inc(dest, stride);
      Inc(Src, 16);
    end;
end;

procedure pixel_save_8x8_pas(Src, dest: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 7 do
    begin
      uint64_p(dest)^ := uint64_p(Src)^;
      Inc(dest, stride);
      Inc(Src, 16);
    end;
end;

procedure pixel_save_4x4(Src, dest: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 3 do
    begin
      uint32_p(dest)^ := uint32_p(Src)^;
      Inc(dest, stride);
      Inc(Src, 16);
    end;
end;

(* ******************************************************************************
  pixel_avg_16x16
  average of 2 pixel arrays
*)
procedure pixel_avg_16x16_pas(src1, src2, dest: uint8_p; stride: int32_t);
var
  X, Y: int32_t;
begin
  for Y := 0 to 15 do
    begin
      for X := 0 to 15 do
          dest[X] := (src1[X] + src2[X] + 1) shr 1;
      Inc(src1, stride);
      Inc(src2, stride);
      Inc(dest, 16);
    end;
end;

procedure pixel_init;
begin
  sad_16x16 := @sad_16x16_pas;
  sad_8x8 := @sad_8x8_pas;
  sad_4x4 := @sad_4x4_pas;

  ssd_16x16 := @ssd_16x16_pas;
  ssd_8x8 := @ssd_8x8_pas;
  var_16x16 := @var_16x16_pas;

  satd_4x4 := @satd_4x4_pas;
  satd_8x8 := @satd_8x8_pas;
  satd_16x16 := @satd_16x16_pas;

  pixel_load_16x16 := @pixel_load_16x16_pas;
  pixel_loadu_16x16 := @pixel_load_16x16_pas;
  pixel_load_8x8 := @pixel_load_8x8_pas;
  pixel_save_16x16 := @pixel_save_16x16_pas;
  pixel_save_8x8 := @pixel_save_8x8_pas;
  pixel_add_4x4 := @pixel_add_4x4_pas;
  pixel_sub_4x4 := @pixel_sub_4x4_pas;
  pixel_avg_16x16 := @pixel_avg_16x16_pas;
end;

end.  
