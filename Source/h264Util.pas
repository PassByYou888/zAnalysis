{ ****************************************************************************** }
{ * h264Util.pas        by qq600585                                            * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit h264Util;

{$I zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Stdint, math, CoreClasses;

function fev_malloc(size: uint32_t): pointer;
procedure fev_free(ptr: pointer);

function min(const a, b: int32_t): int32_t; inline;
function max(const a, b: int32_t): int32_t; inline;
function clip3(const a, b, c: int32_t): int32_t; inline; // lower bound, value, upper bound
function median(const x, y, z: int32_t): int16_t;
function num2log2(n: int32_t): uint8_t;
procedure swap_ptr(var a, b: pointer); overload;
procedure swap_ptr(var a, b: uint8_p); overload;

type
  mbcmp_func_t     = function(pix1, pix2: uint8_p; stride: int32_t): int32_t;
  mbstat_func_t    = function(pix: uint8_p): uint32;
  pixmove_func_t   = procedure(pix1, pix2: uint8_p; stride: int32_t);
  pixoper_func_t   = procedure(pix1, pix2: uint8_p; diff: int16_p);
  pixavg_func_t    = procedure(src1, src2, dest: uint8_p; stride: int32_t);
  mc_chroma_func_t = procedure(src, dst: uint8_p; const stride: int32_t; coef: uint8_p);

  { TDsp }
  TDsp = class
  public
    sad_16x16, sad_8x8, sad_4x4, ssd_16x16, ssd_8x8, satd_4x4, satd_8x8, satd_16x16: mbcmp_func_t;
    var_16x16: mbstat_func_t;
    pixel_load_16x16, pixel_load_8x8, pixel_save_16x16, pixel_save_8x8: pixmove_func_t;
    pixel_add_4x4, pixel_sub_4x4: pixoper_func_t;
    pixel_avg_16x16: pixavg_func_t;
    pixel_loadu_16x16: pixmove_func_t;
    mc_chroma_8x8: mc_chroma_func_t;
    constructor Create;
  end;

var
  dsp: TDsp;

implementation

uses
  h264Pixel, h264Motion_comp;

(* ******************************************************************************
  evx_malloc, evx_mfree
  memory allocation with address aligned to 16-uint8_t boundaries
  ****************************************************************************** *)
function fev_malloc(size: uint32_t): pointer;
const
  ALIGNMENT = 64;
var
  ptr: pointer;
begin
  ptr := getMemory(size + ALIGNMENT);
  result := MemoryAlign(ptr, ALIGNMENT);
  if result = ptr then
      inc(uint8_p(result), ALIGNMENT);
  (uint8_p(result) - 1)^ := NativeUInt(result) - NativeUInt(ptr);
end;

procedure fev_free(ptr: pointer);
begin
  if ptr = nil then
      exit;
  dec(uint8_p(ptr), uint8_p(NativeUInt(ptr) - 1)^);
  freemem(ptr);
  ptr := nil;
end;

function min(const a, b: int32_t): int32_t;
begin
  if a < b then
      result := a
  else
      result := b;
end;

function max(const a, b: int32_t): int32_t;
begin
  if a >= b then
      result := a
  else
      result := b;
end;

function clip3(const a, b, c: int32_t): int32_t;
begin
  if b < a then
      result := a
  else if b > c then
      result := c
  else
      result := b;
end;

function median(const x, y, z: int32_t): int16_t;
begin
  result := x + y + z - min(x, min(y, z)) - max(x, max(y, z));
end;

function num2log2(n: int32_t): uint8_t;
begin
  result := ceil(log2(n));
end;

procedure swap_ptr(var a, b: pointer);
var
  t: pointer;
begin
  t := a;
  a := b;
  b := t;
end;

procedure swap_ptr(var a, b: uint8_p);
var
  t: uint8_p;
begin
  t := a;
  a := b;
  b := t;
end;

{ TDsp }

constructor TDsp.Create;
begin
  pixel_init;
  motion_compensate_init;

  sad_16x16 := h264Pixel.sad_16x16;
  sad_8x8 := h264Pixel.sad_8x8;
  sad_4x4 := h264Pixel.sad_4x4;
  satd_16x16 := h264Pixel.satd_16x16;
  satd_8x8 := h264Pixel.satd_8x8;
  satd_4x4 := h264Pixel.satd_4x4;
  ssd_16x16 := h264Pixel.ssd_16x16;
  ssd_8x8 := h264Pixel.ssd_8x8;
  var_16x16 := h264Pixel.var_16x16;

  pixel_loadu_16x16 := h264Pixel.pixel_loadu_16x16;
  pixel_load_16x16 := h264Pixel.pixel_load_16x16;
  pixel_load_8x8 := h264Pixel.pixel_load_8x8;
  pixel_save_16x16 := h264Pixel.pixel_save_16x16;
  pixel_save_8x8 := h264Pixel.pixel_save_8x8;
  pixel_add_4x4 := h264Pixel.pixel_add_4x4;
  pixel_sub_4x4 := h264Pixel.pixel_sub_4x4;
  pixel_avg_16x16 := h264Pixel.pixel_avg_16x16;

  mc_chroma_8x8 := h264Motion_comp.mc_chroma_8x8;
end;

end.
