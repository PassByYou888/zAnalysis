(*
Copyright (c) 1998-2014 by Carlotta Calandra. All rights reserved.
Copyright (c) 2011-2014 by Xequte software.

This software comes without express or implied warranty.
In no case shall the author be liable for any damage or unwanted behavior of any
computer hardware and/or software.

Author grants you the right to include the component
in your application, whether COMMERCIAL, SHAREWARE, or FREEWARE.

ImageEn, IEvolution and ImageEn ActiveX may not be included in any
commercial, shareware or freeware libraries or components.

www.ImageEn.com
*)

(*
File version 1005
*)

unit pngfilt;

{$R-}
{$Q-}

{$I ie.inc}


{$IFDEF IEINCLUDEPNG}


interface

uses Windows, Graphics, classes, sysutils, ImageEnIO, hyiedefs, hyieutils;



procedure ReadPNGStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; Preview: boolean);
function IsPNGStream(Stream: TStream): boolean;





implementation




uses math, ImageEnView, ieview, ievision, ImageEnProc, iesettings,
     {$ifdef IEUSEVCLZLIB}zlib{$else}iezlib{$endif};

const

  IEPNG_COLOR_MASK_PALETTE    = 1;
  IEPNG_COLOR_MASK_COLOR      = 2;
  IEPNG_COLOR_MASK_ALPHA      = 4;

  IEPNG_COLOR_TYPE_GRAY       = 0;
  IEPNG_COLOR_TYPE_PALETTE    = IEPNG_COLOR_MASK_COLOR or IEPNG_COLOR_MASK_PALETTE;
  IEPNG_COLOR_TYPE_RGB        = IEPNG_COLOR_MASK_COLOR;
  IEPNG_COLOR_TYPE_RGB_ALPHA  = IEPNG_COLOR_MASK_COLOR or IEPNG_COLOR_MASK_ALPHA;
  IEPNG_COLOR_TYPE_GRAY_ALPHA = IEPNG_COLOR_MASK_ALPHA;

  IEPNG_INTERLACE_NONE        = 0;
  IEPNG_INTERLACE_ADAM7       = 1;

type
  EIEPNGException = class(Exception);


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// static png library wrappers


{$ifndef IEUSEDLLPNGLIB}

{$Z4}

type
  png_uint_32   = Cardinal;
  png_uint_32p  = ^png_uint_32;
  png_int_32    = integer;
  png_uint_16   = Word;
  png_int_16    = Smallint;
  png_byte      = Byte;
  png_size_t    = png_uint_32;
  png_charpp    = ^png_charp;
  png_charp     = PAnsiChar;
  float         = single;
  int           = integer;
  png_bytepp    = ^png_bytep;
  png_bytep     = ^png_byte;
  png_uint_16p  = ^png_uint_16;
  png_uint_16pp = ^png_uint_16p;
  png_voidp     = pointer;
  time_t        = Longint;
  png_doublep   = ^png_double;
  png_double    = double;

  user_error_ptr = Pointer;
  png_error_ptrp = ^png_error_ptr;
  png_rw_ptrp    = ^png_rw_ptr;
  png_flush_ptrp = ^png_flush_ptr;
  png_progressive_info_ptrp = ^png_progressive_info_ptr;
  png_progressive_end_ptrp  = ^png_progressive_end_ptr;
  png_progressive_row_ptrp  = ^png_progressive_row_ptr;

  png_error_ptr            = procedure(png_ptr: Pointer; msg: Pointer); cdecl;
  png_rw_ptr               = procedure(png_ptr: Pointer; data: Pointer; length: png_size_t); cdecl;
  png_flush_ptr            = procedure(png_ptr: Pointer); cdecl;
  png_progressive_info_ptr = procedure(png_ptr: Pointer; info_ptr: Pointer); cdecl;
  png_progressive_end_ptr  = procedure(png_ptr: Pointer; info_ptr: Pointer); cdecl;
  png_progressive_row_ptr  = procedure(png_ptr: Pointer; data: Pointer; length: png_uint_32; count: int); cdecl;
  png_read_status_ptr      = procedure(png_ptr: Pointer; row_number: png_uint_32; pass: int); cdecl;
  png_write_status_ptr     = procedure(png_ptr: Pointer; row_number: png_uint_32; pass: int); cdecl;
  png_user_transform_ptr   = procedure(png_ptr: Pointer; row_info: Pointer; data: png_bytep); cdecl;

  png_colorpp = ^png_colorp;
  png_colorp  = ^png_color;
  png_color = packed record
    red, green, blue: png_byte;
  end;

  png_color_16pp = ^png_color_16p;
  png_color_16p  = ^png_color_16;
  png_color_16 = packed record
    index: png_byte; //used for palette files
    red, green, blue: png_uint_16; //for use in red green blue files
    gray: png_uint_16; //for use in grayscale files
  end;

  png_color_8pp = ^png_color_8p;
  png_color_8p  = ^png_color_8;
  png_color_8 = packed record
    red, green, blue: png_byte; //for use in red green blue files
    gray: png_byte; //for use in grayscale files
    alpha: png_byte; //for alpha channel files
  end;

  png_textpp = ^png_textp;
  png_textp  = ^tpng_text;
  tpng_text = packed record
    compression: int; //compression value
    key: png_charp; //keyword, 1-79 character description of "text"
    text: png_charp; //comment, may be empty ("")
    text_length: png_size_t; //length of text field
  end;

  png_timepp = ^png_timep;
  png_timep = ^tpng_time;
  tpng_time = packed record
    year: png_uint_16; //yyyy
    month: png_byte; //1..12
    day: png_byte; //1..31
    hour: png_byte; //0..23
    minute: png_byte; //0..59
    second: png_byte; //0..60 (leap seconds)
  end;

  png_infopp = ^png_infop;
  png_infop = Pointer;

  png_row_infopp = ^png_row_infop;
  png_row_infop = ^png_row_info;
  png_row_info = packed record
    width: png_uint_32; //width of row
    rowbytes: png_size_t; //number of bytes in row
    color_type: png_byte; //color type of row
    bit_depth: png_byte; //bit depth of row
    channels: png_byte; //number of channels (1, 2, 3, or 4)
    pixel_depth: png_byte; //bits per pixel (depth * channels)
  end;

  png_structpp = ^png_structp;
  png_structp = Pointer;

  // Supported compression types for text in PNG files (tEXt, and zTXt).
  // The values of the PNG_TEXT_COMPRESSION_ defines should NOT be changed.

const
  PNG_TEXT_COMPRESSION_NONE_WR = -3;
  PNG_TEXT_COMPRESSION_zTXt_WR = -2;
  PNG_TEXT_COMPRESSION_NONE = -1;
  PNG_TEXT_COMPRESSION_zTXt = 0;

  // This is for compression type. PNG 1.0 only defines the single type.

  PNG_COMPRESSION_TYPE_BASE = 0; // Deflate method 8, 32K window
  PNG_COMPRESSION_TYPE_DEFAULT = PNG_COMPRESSION_TYPE_BASE;

  // This is for filter type. PNG 1.0 only defines the single type.

  PNG_FILTER_TYPE_BASE = 0; // Single row per-byte filtering
  PNG_FILTER_TYPE_DEFAULT = PNG_FILTER_TYPE_BASE;


  // These are for the oFFs chunk.  These values should NOT be changed.

  PNG_OFFSET_PIXEL = 0; // Offset in pixels
  PNG_OFFSET_MICROMETER = 1; // Offset in micrometers (1/10^6 meter)

  // These are for the pCAL chunk.  These values should NOT be changed.

  PNG_EQUATION_LINEAR = 0; // Linear transformation
  PNG_EQUATION_BASE_E = 1; // Exponential base e transform
  PNG_EQUATION_ARBITRARY = 2; // Arbitrary base exponential transform
  PNG_EQUATION_HYPERBOLIC = 3; // Hyperbolic sine transformation

  // These are for the pHYs chunk.  These values should NOT be changed.

  PNG_RESOLUTION_UNKNOWN = 0; // pixels/unknown unit (aspect ratio)
  PNG_RESOLUTION_METER = 1; // pixels/meter

  // These are for the sRGB chunk.  These values should NOT be changed.
  PNG_sRGB_INTENT_SATURATION = 0;
  PNG_sRGB_INTENT_PERCEPTUAL = 1;
  PNG_sRGB_INTENT_ABSOLUTE = 2;
  PNG_sRGB_INTENT_RELATIVE = 3;

  // Handle alpha and tRNS by replacing with a background color.
  PNG_BACKGROUND_GAMMA_UNKNOWN = 0;
  PNG_BACKGROUND_GAMMA_SCREEN = 1;
  PNG_BACKGROUND_GAMMA_FILE = 2;
  PNG_BACKGROUND_GAMMA_UNIQUE = 3;

  // Values for png_set_crc_action() to say how to handle CRC errors in
  // ancillary and critical chunks, and whether to use the data contained
  // therein.  Note that it is impossible to "discard" data in a critical
  // chunk.  For versions prior to 0.90, the action was always error/quit,
  // whereas in version 0.90 and later, the action for CRC errors in ancillary
  // chunks is warn/discard.  These values should NOT be changed.

  //      value                   action: critical     action: ancillary

  PNG_CRC_DEFAULT = 0; // error/quit          warn/discard data
  PNG_CRC_ERROR_QUIT = 1; // error/quit          error/quit
  PNG_CRC_WARN_DISCARD = 2; // (INVALID)           warn/discard data
  PNG_CRC_WARN_USE = 3; // warn/use data       warn/use data
  PNG_CRC_QUIET_USE = 4; // quiet/use data      quiet/use data
  PNG_CRC_NO_CHANGE = 5; // use current value   use current value

  // Flags for png_set_filter() to say which filters to use.  The flags
  // are chosen so that they don't conflict with real filter types
  // below, in case they are supplied instead of the #defined constants.
  // These values should NOT be changed.

  PNG_NO_FILTERS = $00;
  PNG_FILTER_NONE = $08;
  PNG_FILTER_SUB = $10;
  PNG_FILTER_UP = $20;
  PNG_FILTER_AVG = $40;
  PNG_FILTER_PAETH = $80;
  PNG_ALL_FILTERS = PNG_FILTER_NONE or PNG_FILTER_SUB or PNG_FILTER_UP or PNG_FILTER_AVG or PNG_FILTER_PAETH;

  // Filter values (not flags) - used in pngwrite.c, pngwutil.c for now.
  // These defines should NOT be changed.

  PNG_FILTER_VALUE_NONE = 0;
  PNG_FILTER_VALUE_SUB = 1;
  PNG_FILTER_VALUE_UP = 2;
  PNG_FILTER_VALUE_AVG = 3;
  PNG_FILTER_VALUE_PAETH = 4;

  // Heuristic used for row filter selection.  These defines should NOT be
  // changed.

  PNG_FILTER_HEURISTIC_DEFAULT = 0; // Currently "UNWEIGHTED"
  PNG_FILTER_HEURISTIC_UNWEIGHTED = 1; // Used by libpng < 0.95
  PNG_FILTER_HEURISTIC_WEIGHTED = 2; // Experimental feature
  PNG_FILTER_HEURISTIC_LAST = 3; // Not a valid value



{$R-}

var
  __turboFloat: LongBool = False;


function memcmp(buf1, buf2: pbyte; count: integer): integer;  cdecl;
begin
  if count = 0 then
    result := 0
  else
  begin
    while true do
    begin
      dec(count);
      if (count=0) or (buf1^<>buf2^) then
        break;
      inc(buf1);
      inc(buf2);
    end;
    result := buf1^ - buf2^;
  end;
end;

function strncpy(dest, src: PAnsiChar; maxlen: integer): PAnsiChar; cdecl;
begin
  result := IEStrMove(dest, src, maxlen);
end;

function strcpy(dest, src: PAnsiChar): PAnsiChar; cdecl;
begin
  result := IEStrCopy(dest, src);
end;

function fabs(v: double): double; cdecl;
begin
  result := abs(v);
end;

function pow(Base, Exponent: double): double; cdecl;
begin
  result := Power(Base, Exponent);
end;

function strtod(s: PAnsiChar; var vp: PAnsiChar): double; cdecl;
begin
  vp := @s[IEStrLen(s) - 1]; // !!
  result := IEStrToFloatDefA(s, 0);
end;

function malloc(size: Integer): Pointer; cdecl;
begin
  result := allocmem(size);
end;

procedure free(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

function memset(P: Pointer; B: Byte; count: Integer): pointer; cdecl;
begin
  FillChar(P^, count, B);
  result := P;
end;

function memcpy(dest, source: Pointer; count: Integer): pointer; cdecl;
begin
  Move(source^, dest^, count);
  result := dest;
end;

function _ftol: integer; cdecl;
var
  f: double;
begin
  asm
   lea    eax, f             //  BC++ passes floats on the FPU stack
   fstp  qword ptr [eax]     //  Delphi passes floats on the CPU stack
  end;
  if f > 2147483647.0 then
    f := 2147483647.0;
  if f < -2147483648.0 then
    f := 2147483648.0;
  result := integer(Trunc(f));
end;

procedure _assert(__cond: PAnsiChar; __file: PAnsiChar; __line: integer); cdecl;
begin
end;

function memmove(dest, source: Pointer; count: Integer): pointer; cdecl;
begin
  Move(source^, dest^, count);
  result := dest;
end;

function strlen(str: PAnsiChar): integer; cdecl;
begin
  result := IEStrLen(str);
end;

function realloc(block: pointer; size: integer): pointer; cdecl;
begin
  reallocmem(block, size);
  result := block;
end;

function fscanf(f: pointer; format: PAnsiChar): integer; cdecl;
begin
  result := 0;
end;


{$L pngread.obj}
{$L pngset.obj}
{$L pngtrans.obj}
{$L pngrtran.obj}
{$L pngrio.obj}
{$L pngmem.obj}
{$L pngerror.obj}
{$L pngrutil.obj}
{$L pngget.obj}
{$L png.obj}

function png_create_read_struct(user_png_ver: png_charp; error_ptr: user_error_ptr; error_fn: png_error_ptr; warn_fn: png_error_ptr): png_structp; cdecl; external;
procedure png_chunk_warning(png_ptr: png_structp; const mess: png_charp); cdecl; external;
procedure png_chunk_error(png_ptr: png_structp; const mess: png_charp); cdecl; external;
procedure png_set_IHDR(png_ptr: png_structp; info_ptr: png_infop; width, height: png_uint_32; bit_depth, color_type, interlace_type, compression_type, filter_type: int); cdecl; external;
procedure png_set_PLTE(png_ptr: png_structp; info_ptr: png_infop; palette: png_colorp; num_palette: int); cdecl; external;
procedure png_set_gAMA(png_ptr: png_structp; info_ptr: png_infop; file_gamma: double); cdecl; external;
procedure png_set_sBIT(png_ptr: png_structp; info_ptr: png_infop; sig_bits: png_color_8p); cdecl; external;
procedure png_set_cHRM(png_ptr: png_structp; info_ptr: png_infop; white_x, white_y, red_x, red_y, green_x, green_y, blue_x, blue_y: double); cdecl; external;
procedure png_set_sRGB_gAMA_and_cHRM(png_ptr: png_structp; info_ptr: png_infop; intent: int); cdecl; external;
procedure png_set_tRNS(png_ptr: png_structp; info_ptr: png_infop; trans: png_bytep; num_trans: int; trans_values: png_color_16p); cdecl; external;
function png_get_tRNS(png_ptr: png_structp; info_ptr: png_infop; trans: png_bytepp; num_trans: pinteger; trans_values: png_color_16pp): png_uint_32; cdecl; external;
procedure png_set_bKGD(png_ptr: png_structp; info_ptr: png_infop; background: png_color_16p); cdecl; external;
procedure png_set_hIST(png_ptr: png_structp; info_ptr: png_infop; hist: png_uint_16p); cdecl; external;
procedure png_set_pHYs(png_ptr: png_structp; info_ptr: png_infop; res_x, res_y: png_uint_32; unit_type: int); cdecl; external;
procedure png_set_oFFs(png_ptr: png_structp; info_ptr: png_infop; offset_x, offset_y: png_uint_32; unit_type: int); cdecl; external;
procedure png_set_pCAL(png_ptr: png_structp; info_ptr: png_infop; purpose: png_charp; X0, X1: png_int_32; typ, nparams: int; units: png_charp; params: png_charpp); cdecl; external;
procedure png_set_tIME(png_ptr: png_structp; info_ptr: png_infop; mod_time: png_timep); cdecl; external;
procedure png_set_text(png_ptr: png_structp; info_ptr: png_infop; text_ptr: png_textp; num_text: int); cdecl; external;
function png_get_text(png_ptr: png_structp; info_ptr: png_infop; text_ptr: png_textpp; num_text: pinteger): png_uint_32; cdecl; external;
function png_create_info_struct(png_ptr: png_structp): png_infop; cdecl; external;
procedure png_destroy_read_struct(png_ptr_ptr: png_structpp; info_ptr_ptr, end_info_ptr_ptr: png_infopp); cdecl; external;
procedure png_set_read_fn(png_ptr: png_structp; io_ptr: png_voidp; read_data_fn: png_rw_ptr); cdecl; external;
procedure png_read_info(png_ptr: png_structp; info_ptr: png_infop); cdecl; external;
function png_get_IHDR(png_ptr: png_structp; info_ptr: png_infop; var width, height: png_uint_32; var bit_depth, color_type, interlace_type, compression_type, filter_type: int): png_uint_32; cdecl; external;
procedure png_set_expand(png_ptr: png_structp); cdecl; external;
procedure png_set_bgr(png_ptr: png_structp); cdecl; external;
procedure png_set_swap(png_ptr: png_structp); cdecl; external;
procedure png_set_strip_16(png_ptr: png_structp); cdecl; external;
procedure png_set_packing(png_ptr: png_structp); cdecl; external;
procedure png_set_gray_to_rgb(png_ptr: png_structp); cdecl; external;
procedure png_read_update_info(png_ptr: png_structp; info_ptr: png_infop); cdecl; external;
function png_set_interlace_handling(png_ptr: png_structp): int; cdecl; external;
procedure png_read_rows(png_ptr: png_structp; row, display_row: png_bytepp; num_rows: png_uint_32); cdecl; external;
procedure png_read_end(png_ptr: png_structp; info_ptr: png_infop); cdecl; external;
function png_get_io_ptr(png_ptr: png_structp): png_voidp; cdecl; external;
function png_get_rowbytes(png_ptr: png_structp; info_ptr: png_infop): png_uint_32; cdecl; external;
function png_get_bKGD(png_ptr: png_structp; info_ptr: png_infop; var background: png_color_16p): png_uint_32; cdecl; external;
procedure png_set_background(png_ptr: png_structp; background_color: png_color_16p; background_gamma_code, need_expand: int; background_gamma: double); cdecl; external;
function png_get_x_pixels_per_meter(png_ptr: png_structp; info_ptr: png_infop): png_uint_32; cdecl; external;
function png_get_y_pixels_per_meter(png_ptr: png_structp; info_ptr: png_infop): png_uint_32; cdecl; external;
function png_get_interlace_type(png_ptr: png_structp; info_ptr: png_infop): png_byte; cdecl; external;
procedure png_set_gamma(png_ptr: png_structp; screen_gamma, default_file_gamma: double); cdecl; external;
function png_get_gAMA(png_ptr: png_structp; info_ptr: png_infop; var file_gamma: double): png_uint_32; cdecl; external;
function png_get_PLTE(png_ptr: png_structp; info_ptr: png_infop; var palette: png_colorp; var num_palette: int): png_uint_32; cdecl; external;
function png_sig_cmp(sig: png_bytep; start, num_to_check: png_size_t): int; cdecl; external;
function png_get_channels(png_ptr: png_structp; info_ptr: png_infop): png_byte; cdecl; external;
procedure png_set_tRNS_to_alpha(png_ptr: png_structp); cdecl; external;
function png_get_error_ptr(png_ptr: png_structp): png_voidp; cdecl; external;

procedure PNG_MEMSET_CHECK; external;
procedure PNG_DO_STRIP_FILLER; external;
procedure PNG_DO_INVERT; external;
procedure PNG_DO_BGR; external;
procedure PNG_DO_PACKSWAP; external;
procedure PNG_DO_SWAP; external;
procedure PNG_INIT_READ_TRANSFORMATIONS; external;
procedure PNG_SET_GAMA_FIXED; external;
procedure PNG_SET_CHRM_FIXED; external;
procedure PNG_SET_ICCP; external;
procedure PNG_SET_SPLT; external;
procedure PNG_SET_SCAL; external;
procedure PNG_SET_UNKNOWN_CHUNKS; external;
procedure png_set_text_2; external;



////////////////////////////////////////////////////////////////////////////////////

type

IEPNG_Decompressor_t = record
  pngPtr:  png_structp;
  infoPtr: png_infop;
end;

IEPNG_Decompressor = ^IEPNG_Decompressor_t;
IEPNG_Color        = png_colorp;
IEPNG_Text         = png_textp;


procedure IEPNG_Decomp_destroy(var decomp: IEPNG_Decompressor);
begin
  if decomp <> nil then
  begin
    if decomp^.pngPtr <> nil then
      png_destroy_read_struct(@decomp^.pngPtr, @decomp^.infoPtr, nil);  // png_destroy_read_struct accepts decomp^.infoPtr=nil
    FreeMem(decomp);
    decomp := nil;
  end;
end;

function IEPNG_Decomp_create(errorPtr: pointer; errorFunc: pointer; warnFunc: pointer): IEPNG_Decompressor;
begin
  result := AllocMem(sizeof(IEPNG_Decompressor_t)); // zero filled
  result^.pngPtr := png_create_read_struct('1.2.14', errorPtr, errorFunc, warnFunc);
  if result^.pngPtr <> nil then
    result^.infoPtr := png_create_info_struct(result^.pngPtr);
  if (result^.pngPtr = nil) or (result^.infoPtr = nil) then
    IEPNG_Decomp_destroy(result);  // this also sets result=nil
end;

procedure IEPNG_Decomp_setReadFunction(decomp: IEPNG_Decompressor; ioPtr: TStream; readDataFunction: pointer);
begin
  png_set_read_fn(decomp^.pngPtr, ioPtr, readDataFunction);
end;

procedure IEPNG_Decomp_readInfo(decomp: IEPNG_Decompressor);
begin
  png_read_info(decomp^.pngPtr, decomp^.infoPtr);
end;

function IEPNG_Decomp_getIHDR(decomp: IEPNG_Decompressor; var width: dword; var height: dword; var bitDepth: integer; var colorType: integer; var interlaceType: integer; var compressionType: integer; var filterType: integer): dword;
begin
  result := png_get_IHDR(decomp^.pngPtr, decomp^.infoPtr, width, height, bitDepth, colorType, interlaceType, compressionType, filterType);
end;

function IEPNG_Decomp_getText(decomp: IEPNG_Decompressor; var textPtr: IEPNG_Text): dword;
begin
  result := png_get_text(decomp^.pngPtr, decomp^.infoPtr, @textPtr, nil);
end;

function IEPNG_Decomp_getBackground(decomp: IEPNG_Decompressor; defaultValue: TRGB): TRGB;
var
  b: png_color_16p;
begin
  b := nil;
  png_get_bKGD(decomp^.pngPtr, decomp^.infoPtr, b);
  if b <> nil then
    result := CreateRGB(b^.red shr 8, b^.green shr 8, b^.blue shr 8)
  else
    result := defaultValue;
end;

procedure IEPNG_Decomp_setExpand(decomp: IEPNG_Decompressor);
begin
  png_set_expand(decomp^.pngPtr);
end;

procedure IEPNG_Decomp_setStrip16(decomp: IEPNG_Decompressor);
begin
  png_set_strip_16(decomp^.pngPtr);
end;

procedure IEPNG_Decomp_setPacking(decomp: IEPNG_Decompressor);
begin
  png_set_packing(decomp^.pngPtr);
end;

procedure IEPNG_Decomp_setGrayToRGB(decomp: IEPNG_Decompressor);
begin
  png_set_gray_to_rgb(decomp^.pngPtr);
end;

procedure IEPNG_Decomp_setBGR(decomp: IEPNG_Decompressor);
begin
  png_set_bgr(decomp^.pngPtr);
end;

procedure IEPNG_Decomp_setSwap(decomp: IEPNG_Decompressor);
begin
  png_set_swap(decomp^.pngPtr);
end;

procedure IEPNG_Decomp_setTRNStoAlpha(decomp: IEPNG_Decompressor);
begin
  png_set_tRNS_to_alpha(decomp^.pngPtr);
end;

function IEPNG_Decomp_setInterlaceHandling(decomp: IEPNG_Decompressor): integer;
begin
  result := png_set_interlace_handling(decomp^.pngPtr);
end;

procedure IEPNG_Decomp_readUpdateInfo(decomp: IEPNG_Decompressor);
begin
  png_read_update_info(decomp^.pngPtr, decomp^.infoPtr);
end;

function IEPNG_Decomp_getXPixelsPerMeter(decomp: IEPNG_Decompressor): dword;
begin
  result := png_get_x_pixels_per_meter(decomp^.pngPtr, decomp^.infoPtr);
end;

function IEPNG_Decomp_getYPixelsPerMeter(decomp: IEPNG_Decompressor): dword;
begin
  result := png_get_y_pixels_per_meter(decomp^.pngPtr, decomp^.infoPtr);
end;

function IEPNG_Decomp_getPalette(decomp: IEPNG_Decompressor; var palette: IEPNG_Color; var numPalette: integer): dword;
begin
  result := png_get_PLTE(decomp^.pngPtr, decomp^.infoPtr, palette, numPalette);
end;

function IEPNG_Decomp_getInterlaceType(decomp: IEPNG_Decompressor): byte;
begin
  result := png_get_interlace_type(decomp^.pngPtr, decomp^.infoPtr);
end;

function IEPNG_Decomp_getChannels(decomp: IEPNG_Decompressor): byte;
begin
  result := png_get_channels(decomp^.pngPtr, decomp^.infoPtr);
end;

procedure IEPNG_Decomp_readRows(decomp: IEPNG_Decompressor; row: pointer; displayRow: pointer; numRows: dword);
begin
  png_read_rows(decomp^.pngPtr, row, displayRow, numRows);
end;

procedure IEPNG_Decomp_readEnd(decomp: IEPNG_Decompressor);
begin
  png_read_end(decomp^.pngPtr, decomp^.infoPtr);
end;

function IEPNG_Decomp_getTRNS(decomp: IEPNG_Decompressor; trans: pointer; var numTrans: integer): dword;
begin
  result := png_get_tRNS(decomp^.pngPtr, decomp^.infoPtr, trans, @numTrans, nil);
end;

function IEPNG_sigCmp(sig: pointer; start: dword; numToCheck: dword): integer;
begin
  result := png_sig_cmp(sig, start, numToCheck);
end;

function IEPNG_getTextKey(textPtr: IEPNG_Text): PAnsiChar;
begin
  result := textPtr^.key;
end;

function IEPNG_getTextText(textPtr: IEPNG_Text): PAnsiChar;
begin
  result := textPtr^.text;
end;

function IEPNG_getTextNext(textPtr: IEPNG_Text): IEPNG_Text;
begin
  inc(textPtr);
  result := textPtr;
end;

function IEPNG_getIOPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor or IEPNG_Compressor
begin
  result := png_get_io_ptr(pngPtr);
end;

function IEPNG_getErrorPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor or IEPNG_Compressor
begin
  result := png_get_error_ptr(pngPtr);
end;



{$endif}  // not IEUSEDLLPNGLIB

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////






///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// dynamic (DLL) png library wrappers


{$ifdef IEUSEDLLPNGLIB}



type

IEPNG_Decompressor = TIELibPNGDecompressor;
IEPNG_Text         = TIELibPNGText;

IEPNG_Color_t = packed record
  red:   byte;
  green: byte;
  blue:  byte;
end;

IEPNG_Color = ^IEPNG_Color_t;



procedure IEPNG_Decomp_destroy(var decomp: IEPNG_Decompressor);
begin
  decomp := nil;
end;

function IEPNG_Decomp_create(errorPtr: pointer; errorFunc: pointer; warnFunc: pointer): IEPNG_Decompressor;
begin
  if IELibAvailable() then
  begin
    result := IELib.createPNGDecompressor(errorPtr, errorFunc, warnFunc);
    if not result.isValid() then
      result := nil;
  end
  else
    raise EIEPNGException.Create(IERS_IEVISIONNOTFOUND);
end;

procedure IEPNG_Decomp_setReadFunction(decomp: IEPNG_Decompressor; ioPtr: TStream; readDataFunction: pointer);
begin
  decomp.setReadFunction(ioPtr, readDataFunction);
end;

procedure IEPNG_Decomp_readInfo(decomp: IEPNG_Decompressor);
begin
  decomp.readInfo();
end;

function IEPNG_Decomp_getIHDR(decomp: IEPNG_Decompressor; var width: dword; var height: dword; var bitDepth: integer; var colorType: integer; var interlaceType: integer; var compressionType: integer; var filterType: integer): dword;
begin
  result := decomp.getIHDR(width, height, bitDepth, colorType, interlaceType, compressionType, filterType);
end;

function IEPNG_Decomp_getText(decomp: IEPNG_Decompressor; var textPtr: IEPNG_Text): dword;
begin
  result := decomp.getText(textPtr);
end;

function IEPNG_Decomp_getBackground(decomp: IEPNG_Decompressor; defaultValue: TRGB): TRGB;
begin
  result := IEVisionBGR8ToTRGB( decomp.getBackground(IETRGBToVisionBGR8(defaultValue)) );
end;

procedure IEPNG_Decomp_setExpand(decomp: IEPNG_Decompressor);
begin
  decomp.setExpand();
end;

procedure IEPNG_Decomp_setStrip16(decomp: IEPNG_Decompressor);
begin
  decomp.setStrip16();
end;

procedure IEPNG_Decomp_setPacking(decomp: IEPNG_Decompressor);
begin
  decomp.setPacking();
end;

procedure IEPNG_Decomp_setGrayToRGB(decomp: IEPNG_Decompressor);
begin
  decomp.setGrayToRGB();
end;

procedure IEPNG_Decomp_setBGR(decomp: IEPNG_Decompressor);
begin
  decomp.setBGR();
end;

procedure IEPNG_Decomp_setSwap(decomp: IEPNG_Decompressor);
begin
  decomp.setSwap();
end;

procedure IEPNG_Decomp_setTRNStoAlpha(decomp: IEPNG_Decompressor);
begin
  decomp.setTRNStoAlpha();
end;

function IEPNG_Decomp_setInterlaceHandling(decomp: IEPNG_Decompressor): integer;
begin
  result := decomp.setInterlaceHandling();
end;

procedure IEPNG_Decomp_readUpdateInfo(decomp: IEPNG_Decompressor);
begin
  decomp.readUpdateInfo();
end;

function IEPNG_Decomp_getXPixelsPerMeter(decomp: IEPNG_Decompressor): dword;
begin
  result := decomp.getXPixelsPerMeter();
end;

function IEPNG_Decomp_getYPixelsPerMeter(decomp: IEPNG_Decompressor): dword;
begin
  result := decomp.getYPixelsPerMeter();
end;

function IEPNG_Decomp_getPalette(decomp: IEPNG_Decompressor; var palette: IEPNG_Color; var numPalette: integer): dword;
begin
  result := decomp.getPalette(@palette, numPalette);
end;

function IEPNG_Decomp_getInterlaceType(decomp: IEPNG_Decompressor): byte;
begin
  result := decomp.getInterlaceType();
end;

function IEPNG_Decomp_getChannels(decomp: IEPNG_Decompressor): byte;
begin
  result := decomp.getChannels();
end;

procedure IEPNG_Decomp_readRows(decomp: IEPNG_Decompressor; row: pointer; displayRow: pointer; numRows: dword);
begin
  decomp.readRows(row, displayRow, numRows);
end;

procedure IEPNG_Decomp_readEnd(decomp: IEPNG_Decompressor);
begin
  decomp.readEnd();
end;

function IEPNG_Decomp_getTRNS(decomp: IEPNG_Decompressor; trans: pointer; var numTrans: integer): dword;
begin
  result := decomp.getTRNS(trans, numTrans);
end;

function IEPNG_sigCmp(sig: pointer; start: dword; numToCheck: dword): integer;
begin
  if IELibAvailable() then
    result := IELib.PNGSigCmp(sig, start, numToCheck)
  else
    result := -1;
end;

function IEPNG_getIOPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor
begin
  result := IELib.PNGGetIOPtr(pngPtr);
end;

function IEPNG_getTextKey(textPtr: IEPNG_Text): PAnsiChar;
begin
  result := textPtr.getKey();
end;

function IEPNG_getTextText(textPtr: IEPNG_Text): PAnsiChar;
begin
  result := textPtr.getText();
end;

function IEPNG_getTextNext(textPtr: IEPNG_Text): IEPNG_Text;
begin
  result := textPtr.getNext();
end;

function IEPNG_getErrorPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor or IEPNG_Compressor
begin
  result := IELib.PNGGetErrorPtr(pngPtr);
end;



{$endif}  // IEUSEDLLPNGLIB


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////



procedure ErrorFunc(png_ptr: pointer; msg: Pointer); cdecl;
var
  aborting: pboolean;
begin
  aborting := IEPNG_getErrorPtr(png_ptr);
  aborting^ := true;
  Abort;
end;

procedure WarnFunc(png_ptr: pointer; msg: Pointer); cdecl;
begin
  // nothing to do
end;

procedure ReadFunc(png_ptr: pointer; data: pointer; length: dword); cdecl;
var
  Stream: TStream;
begin
  Stream := IEPNG_getIOPtr(png_ptr);
  if dword(Stream.Read(PAnsiChar(data)^, length)) < length then
    Abort;
end;


procedure ReadPNGStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; Preview: boolean);
var
  decomp: IEPNG_Decompressor;
  width, height: dword;
  compression_type: integer;
  filter_type: integer;
  bit_depth, color_type, interlace_type: integer;
  i, number_passes, pass, y: integer;
  px: pointer;
  palette: IEPNG_Color;
  num_palette: integer;
  channels, x: integer;
  arowbuf, apx, apx2, pb: pbyte;
  native: boolean;
  text_ptr: IEPNG_Text;
  lper, per: integer;
  trans: pbyte;
  num_trans: integer;
begin
  try
    try
      decomp := IEPNG_Decomp_create(xProgress.Aborting, @ErrorFunc, @WarnFunc);
      if decomp = nil then
      begin
        xProgress.Aborting^ := true;
        exit;
      end;
      IEPNG_Decomp_setReadFunction(decomp, Stream, @ReadFunc);

      IEPNG_Decomp_readInfo(decomp);

      IEPNG_Decomp_getIHDR(decomp, width, height, bit_depth, color_type, interlace_type, Compression_type, filter_type);

      // get text
      x := IEPNG_Decomp_getText(decomp, text_ptr);
      IOParams.PNG_TextKeys.Clear();
      IOParams.PNG_TextValues.Clear();
      for i := 0 to x-1 do
      begin
        IOParams.PNG_TextKeys.Add( string(IEPNG_getTextKey(text_ptr)) );
        IOParams.PNG_TextValues.Add( string(IEPNG_getTextText(text_ptr)) );
        text_ptr := IEPNG_getTextNext(text_ptr);
      end;

      // background
      IOParams.PNG_Background := IEPNG_Decomp_getBackground(decomp, IOParams.PNG_Background);

      if (not IOParams.IsNativePixelFormat) or
         ((bit_depth <> 8) and (bit_depth<>16)) or
         ((color_type <> IEPNG_COLOR_TYPE_PALETTE) and (color_type <> IEPNG_COLOR_TYPE_GRAY)) then
      begin

        // Is paletted? (from 2 to 8 bit)
        if (color_type = IEPNG_COLOR_TYPE_PALETTE) and (bit_depth <= 8) and (bit_depth > 1) then
          IEPNG_Decomp_setExpand(decomp);
        // Is grayscale? (from 2 to 7 bit)
        if (color_type = IEPNG_COLOR_TYPE_GRAY) and (bit_depth < 8) and (bit_depth > 1) then
          IEPNG_Decomp_setExpand(decomp);
        // Is grayscale? (only 16 bit)
        if (bit_depth = 16) then
          IEPNG_Decomp_setStrip16(decomp);

        if (bit_depth < 8) and (bit_depth > 1) then
          IEPNG_Decomp_setPacking(decomp);
        // Is grayscale and not blackwhite?
        if ((color_type = IEPNG_COLOR_TYPE_GRAY) or (color_type = IEPNG_COLOR_TYPE_GRAY_ALPHA)) and (bit_depth > 1) then
          IEPNG_Decomp_setGrayToRGB(decomp);

        if (bit_depth > 1) then
          IEPNG_Decomp_setBGR(decomp);

        native := false;

      end
      else
      begin

        if bit_depth = 16 then
          IEPNG_Decomp_setSwap(decomp);

        native := true;

      end;

      // 3.0.1
      if (bit_depth > 1) and (not native) then  // 3.0.3
        IEPNG_Decomp_setTRNStoAlpha(decomp);

      number_passes := IEPNG_Decomp_setInterlaceHandling(decomp);
      IEPNG_Decomp_readUpdateInfo(decomp);

      IOParams.ImageCount     := 1;
      IOParams.Width          := width;
      IOParams.Height         := height;
      IOParams.OriginalWidth  := width;
      IOParams.OriginalHeight := height;
      IOParams.BitsPerSample  := bit_depth;
      case color_type of
        IEPNG_COLOR_TYPE_GRAY:       IOParams.SamplesPerPixel := 1;
        IEPNG_COLOR_TYPE_PALETTE:    IOParams.SamplesPerPixel := 1;
        IEPNG_COLOR_TYPE_RGB:        IOParams.SamplesPerPixel := 3;
        IEPNG_COLOR_TYPE_RGB_ALPHA:  IOParams.SamplesPerPixel := 4;
        IEPNG_COLOR_TYPE_GRAY_ALPHA: IOParams.SamplesPerPixel := 2;
      end;

      IOParams.DpiX := round(IEPNG_Decomp_getXPixelsPerMeter(decomp) / 100 * CM_per_Inch);
      if IOParams.DpiX = 0 then
        IOParams.DpiX := IEGlobalSettings().DefaultDPIX;

      IOParams.DpiY := round(IEPNG_Decomp_getYPixelsPerMeter(decomp) / 100 * CM_per_Inch);
      if IOParams.DpiY = 0 then
        IOParams.DpiY := IEGlobalSettings().DefaultDPIY;

      IOParams.FreeColorMap();
      if color_type = IEPNG_COLOR_TYPE_PALETTE then
      begin
        // copy palette
        IEPNG_Decomp_getPalette(decomp, palette, num_palette);
        IOParams.FreeColorMap();
        IOParams.fColorMapCount := num_palette;
        getmem(IOParams.fColorMap, 3 * num_palette);
        for y := 0 to num_palette - 1 do
        begin
          IOParams.fColorMap^[y].r := palette^.red;
          IOParams.fColorMap^[y].g := palette^.green;
          IOParams.fColorMap^[y].b := palette^.blue;
          inc(palette);
        end;
      end;
      if IEPNG_Decomp_getInterlaceType(decomp) = IEPNG_INTERLACE_NONE then
        IOParams.PNG_Interlaced := false
      else
        IOParams.PNG_Interlaced := true;

      if Preview then
      begin
        IEPNG_Decomp_destroy(decomp); // also set decomp = nil
        exit;
      end;

      if (bit_depth = 1) and not (color_type = IEPNG_COLOR_TYPE_PALETTE) then
        Bitmap.Allocate(Width, Height, ie1g)
      else
      if native and (IOParams.SamplesPerPixel = 1) then
      begin
        if (IOParams.BitsPerSample <= 8) and (color_type = IEPNG_COLOR_TYPE_PALETTE) then
        begin
          Bitmap.Allocate(Width, Height, ie8p);
          Bitmap.PaletteUsed := 1 shl IOParams.BitsPerSample;
          for i := 0 to IOParams.ColorMapCount - 1 do
            Bitmap.Palette[i] := IOParams.ColorMap[i]
        end
        else
        if (IOParams.BitsPerSample = 8) and (color_type = IEPNG_COLOR_TYPE_GRAY) then
          Bitmap.Allocate(Width, Height, ie8g)
        else
        if (IOParams.BitsPerSample = 16) and (color_type = IEPNG_COLOR_TYPE_GRAY) then
          Bitmap.Allocate(Width, Height, ie16g);
      end
      else
        Bitmap.Allocate(Width, Height, ie24RGB);

      xProgress.per1 := 100 / (height * dword(number_passes));
      xProgress.val  := 0;
      channels := IEPNG_Decomp_getChannels(decomp);
      if (channels = 4) and (number_passes = 1) then
        getmem(arowbuf, width * 4)
      else
      if (channels = 4) and (number_passes > 1) then
        getmem(arowbuf, width * height * 4)
      else
        arowbuf := nil;
      if (channels = 4) then
        bitmap.AlphaChannel.Full := false;

      try

        for pass := 0 to number_passes - 1 do
        begin
          lper := -1;
          for y := 0 to height - 1 do
          begin
            px := bitmap.Scanline[y];
            if (channels = 4) then
            begin
              if number_passes > 1 then
              begin
                apx := arowbuf;
                inc(apx, dword(y) * width * 4);

                IEPNG_Decomp_readRows(decomp, @apx, nil, 1);
              end
              else
              begin
                IEPNG_Decomp_readRows(decomp, @arowbuf, nil, 1);
                apx := arowbuf;
              end;
              apx2 := bitmap.AlphaChannel.ScanLine[y];
              for x := 0 to width - 1 do
              begin
                PRGB(px)^ := PRGB(apx)^;
                inc(apx, 3);
                apx2^ := apx^;
                inc(apx2);
                inc(apx);
                inc(pbyte(px), 3);
              end;
            end
            else
            if (bit_depth = 1) and (color_type = IEPNG_COLOR_TYPE_PALETTE) and (bitmap.PixelFormat = ie24RGB) then
            begin
              // 1 bit depth with color map, convert to ie24RGB
              getmem(apx, width div 8 + 1);
              IEPNG_Decomp_readRows(decomp, @apx, nil, 1);
              for x := 0 to width - 1 do
              begin
                if _GetPixelbw(apx, x) = 0 then
                  PRGB(px)^ := IOParams.fColorMap^[0]
                else
                  PRGB(px)^ := IOParams.fColorMap^[1];
                inc(PRGB(px));
              end;
              freemem(apx);
            end
            else
              IEPNG_Decomp_readRows(decomp, @px, nil, 1);
            // OnProgress
            with xProgress do
            begin
              inc(val);
              if assigned(fOnProgress) then
              begin
                per := trunc(per1 * val);
                if per <> lper then
                  fOnProgress(Sender, per);
                lper := per;
              end;
            end;
            if xProgress.Aborting^ then
              break;
          end;
          if xProgress.Aborting^ then
            break;
        end;
      finally
        if channels = 4 then
          freemem(arowbuf);
      end;
      if not xProgress.Aborting^ then
        IEPNG_Decomp_readEnd(decomp);

      if native and (color_type = IEPNG_COLOR_TYPE_PALETTE) then
      begin
        // read alpha channel for paletted images
        trans := nil;
        IEPNG_Decomp_getTRNS(decomp, @trans, num_trans);
        if trans <> nil then
        begin
          bitmap.AlphaChannel.Full := false;
          for y := 0 to bitmap.Height - 1 do
          begin
            pb  := bitmap.Scanline[y];
            apx := bitmap.AlphaChannel.ScanLine[y];
            for x := 0 to bitmap.Width - 1 do
            begin
              if pb^ < num_trans then
                apx^ := pbytearray(trans)[pb^]
              else
                apx^ := 255;
              inc(pb);
              inc(apx);
            end;
          end;
        end;
      end;

    finally
      IEPNG_Decomp_destroy(decomp); // sets decomp=nil (if necessary...)
    end;
  except
    xProgress.Aborting^ := true;
  end;
end;


// return true it is a PNG stream
function IsPNGStream(Stream: TStream): boolean;
var
  buf: array[0..7] of byte;
begin
  Stream.Read(buf, 8);
  result := IEPNG_sigCmp(@(buf[0]), 0, 4) = 0;
  Stream.Seek(-8, soCurrent);
end;







{$else} // IEINCLUDEPNG

interface

implementation

{$endif}

end.
