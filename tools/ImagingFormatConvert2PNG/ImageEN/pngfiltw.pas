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

unit pngfiltw;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEPNG}

interface

uses Windows, Graphics, classes, sysutils, ImageEnIO, hyiedefs, hyieutils;


procedure WritePNGStream(Stream: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; AlphaChannel: TIEMask);



implementation




uses
    math, ImageEnProc,
    {$ifdef IEUSEVCLZLIB}zlib, {$else}iezlib, {$endif}
    pngfilt, neurquant, ievision;




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

  IEPNG_COMPRESSION_TYPE_BASE    = 0;
  IEPNG_COMPRESSION_TYPE_DEFAULT = IEPNG_COMPRESSION_TYPE_BASE;

  IEPNG_FILTER_TYPE_BASE    = 0;
  IEPNG_FILTER_TYPE_DEFAULT = IEPNG_FILTER_TYPE_BASE;

  IEPNG_RESOLUTION_UNKNOWN = 0;
  IEPNG_RESOLUTION_METER   = 1;

  IEPNG_NO_FILTERS   = $00;
  IEPNG_FILTER_NONE  = $08;
  IEPNG_FILTER_SUB   = $10;
  IEPNG_FILTER_UP    = $20;
  IEPNG_FILTER_AVG   = $40;
  IEPNG_FILTER_PAETH = $80;
  IEPNG_ALL_FILTERS  = IEPNG_FILTER_NONE or IEPNG_FILTER_SUB or IEPNG_FILTER_UP or IEPNG_FILTER_AVG or IEPNG_FILTER_PAETH;


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

  // These describe the color_type field in png_info.
  // color type masks

  PNG_COLOR_MASK_PALETTE = 1;
  PNG_COLOR_MASK_COLOR = 2;
  PNG_COLOR_MASK_ALPHA = 4;

  // color types.  Note that not all combinations are legal

  PNG_COLOR_TYPE_GRAY = 0;
  PNG_COLOR_TYPE_PALETTE = PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_PALETTE;
  PNG_COLOR_TYPE_RGB = PNG_COLOR_MASK_COLOR;
  PNG_COLOR_TYPE_RGB_ALPHA = PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_ALPHA;
  PNG_COLOR_TYPE_GRAY_ALPHA = PNG_COLOR_MASK_ALPHA;

  // These are for the interlacing type.  These values should NOT be changed.

  PNG_INTERLACE_NONE = 0; // Non-interlaced image
  PNG_INTERLACE_ADAM7 = 1; // Adam7 interlacing

  // These are for the oFFs chunk.  These values should NOT be changed.

  PNG_OFFSET_PIXEL = 0; // Offset in pixels
  PNG_OFFSET_MICROMETER = 1; // Offset in micrometers (1/10^6 meter)

  // These are for the pCAL chunk.  These values should NOT be changed.

  PNG_EQUATION_LINEAR = 0; // Linear transformation
  PNG_EQUATION_BASE_E = 1; // Exponential base e transform
  PNG_EQUATION_ARBITRARY = 2; // Arbitrary base exponential transform
  PNG_EQUATION_HYPERBOLIC = 3; // Hyperbolic sine transformation

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


function memcmp(buf1, buf2: pbyte; count: integer): integer; cdecl;
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


{$L pngset.obj}
{$L pngtrans.obj}
{$L pngmem.obj}
{$L pngerror.obj}

{$L pngwrite.obj}
{$L pngwio.obj}
{$L pngwtran.obj}
{$L pngwutil.obj}

{$L pngget.obj}
{$L png.obj}

procedure png_set_error_fn(png_ptr: png_structp; error_ptr: png_voidp; error_fn, warning_fn: png_error_ptr); cdecl; external;
function png_set_interlace_handling(png_ptr: png_structp): int; cdecl; external;
procedure png_chunk_warning(png_ptr: png_structp; const mess: png_charp); cdecl; external;
function png_create_write_struct(user_png_ver: png_charp; error_ptr: user_error_ptr; error_fn: png_error_ptr; warn_fn: png_error_ptr): png_structp; cdecl; external;
function png_create_info_struct(png_ptr: png_structp): png_infop; cdecl; external;
procedure png_destroy_write_struct(png_ptr_ptr: png_structpp; info_ptr_ptr: png_infopp); cdecl; external;
procedure png_set_IHDR(png_ptr: png_structp; info_ptr: png_infop; width, height: png_uint_32; bit_depth, color_type, interlace_type, compression_type, filter_type: int); cdecl; external;
procedure png_set_PLTE(png_ptr: png_structp; info_ptr: png_infop; palette: png_colorp; num_palette: int); cdecl; external;
procedure png_set_bKGD(png_ptr: png_structp; info_ptr: png_infop; background: png_color_16p); cdecl; external;
procedure png_set_tRNS(png_ptr: png_structp; info_ptr: png_infop; trans: png_bytep; num_trans: integer; trans_values: png_color_16p); cdecl; external;
procedure png_write_info(png_ptr: png_structp; info_ptr: png_infop); cdecl; external;
procedure png_set_bgr(png_ptr: png_structp); cdecl; external;
procedure png_set_write_fn(png_ptr: png_structp; io_ptr: png_voidp; write_data_fn: png_rw_ptr; output_flush_fn: png_flush_ptr); cdecl; external;
function png_get_io_ptr(png_ptr: png_structp): png_voidp; cdecl; external;
procedure png_write_rows(png_ptr: png_structp; row: png_bytepp; num_rows: png_uint_32); cdecl; external;
procedure png_write_end(png_ptr: png_structp; info_ptr: png_infop); cdecl; external;
procedure png_set_pHYs(png_ptr: png_structp; info_ptr: png_infop; res_x, res_y: png_uint_32; unit_type: int); cdecl; external;
procedure png_set_filter(png_ptr: png_structp; method, filters: int); cdecl; external;
procedure png_set_compression_level(png_ptr: png_structp; level: int); cdecl; external;
procedure png_set_sBIT(png_ptr: png_structp; info_ptr: png_infop; sig_bits: png_color_8p); cdecl; external;
procedure png_set_text(png_ptr: png_structp; info_ptr: png_infop; text_ptr: png_textp; num_text: int); cdecl; external;
function png_get_error_ptr(png_ptr: png_structp): png_voidp; cdecl; external;

procedure PNG_MEMSET_CHECK; external;
procedure PNG_CREATE_STRUCT; external;
procedure PNG_DESTROY_STRUCT; external;
procedure png_warning; external;
procedure png_malloc; external;
procedure png_free; external;
procedure png_memcpy_check; external;
procedure PNG_DO_STRIP_FILLER; external;
procedure PNG_DO_PACKSWAP; external;
procedure PNG_DO_SWAP; external;
procedure PNG_DO_BGR; external;
procedure PNG_DO_INVERT; external;
procedure PNG_WRITE_DATA; external;
procedure png_create_struct_2; external;
procedure PNG_SET_MEM_FN; external;
procedure png_destroy_struct_2; external;
procedure PNG_SET_INVERT_ALPHA; external;
procedure PNG_SET_INVERT_MONO; external;
procedure PNG_SET_SHIFT; external;
procedure PNG_SET_PACKING; external;
procedure PNG_SET_SWAP_ALPHA; external;
procedure PNG_SET_FILLER; external;
procedure PNG_SET_SWAP; external;
procedure PNG_SET_PACKSWAP; external;
procedure PNG_WRITE_FLUSH; external;


///////////////////////////////////////////////////////////////////////////////

type

IEPNG_Compressor_t = record
  pngPtr:  png_structp;
  infoPtr: png_infop;
end;

IEPNG_Compressor = ^IEPNG_Compressor_t;
IEPNG_Color      = png_colorp;
IEPNG_TextList   = array of tpng_text;


function IEPNG_getIOPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor or IEPNG_Compressor
begin
  result := png_get_io_ptr(pngPtr);
end;

function IEPNG_getErrorPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor or IEPNG_Compressor
begin
  result := png_get_error_ptr(pngPtr);
end;

procedure IEPNG_Comp_destroy(var comp: IEPNG_Compressor);
begin
  if comp <> nil then
  begin
    if comp^.pngPtr <> nil then
      png_destroy_write_struct(@comp^.pngPtr, @comp^.infoPtr);  // png_destroy_write_struct accepts comp^.infoPtr=nil
    FreeMem(comp);
    comp := nil;
  end;
end;

function IEPNG_Comp_create(errorPtr: pointer; errorFunc: pointer; warnFunc: pointer): IEPNG_Compressor;
begin
  result := AllocMem(sizeof(IEPNG_Compressor_t)); // zero filled
  result^.pngPtr := png_create_write_struct('1.2.14', errorPtr, errorFunc, warnFunc);
  if result^.pngPtr <> nil then
    result^.infoPtr := png_create_info_struct(result^.pngPtr);
  if (result^.pngPtr = nil) or (result^.infoPtr = nil) then
    IEPNG_Comp_destroy(result);  // this also sets result=nil
end;

procedure IEPNG_Comp_setWriteFunction(comp: IEPNG_Compressor; ioPtr: pointer; writeFunc: pointer; flushFunc: pointer);
begin
  png_set_write_fn(comp^.pngPtr, ioPtr, writeFunc, flushFunc);
end;

procedure IEPNG_setPalette(comp: IEPNG_Compressor; palette: IEPNG_Color; numPalette: integer);
begin
  png_set_PLTE(comp^.pngPtr, comp^.infoPtr, palette, numPalette);
end;

procedure IEPNG_setTRNS(comp: IEPNG_Compressor; trans: pbyte; numTrans: integer);
begin
  png_set_tRNS(comp^.pngPtr, comp^.infoPtr, png_bytep(trans), numTrans, nil);
end;

procedure IEPNG_setIHDR(comp: IEPNG_Compressor; width: dword; height: dword; bitDepth: integer; colorType: integer; interlaceType: integer; compressionType: integer; filterType: integer);
begin
  png_set_IHDR(comp^.pngPtr, comp^.infoPtr, width, height, bitDepth, colorType, interlaceType, compressionType, filterType);
end;

procedure IEPNG_setPHYS(comp: IEPNG_Compressor; resX: dword; resY: dword; unitType: integer);
begin
  png_set_pHYs(comp^.pngPtr, comp^.infoPtr, resX, resY, unitType);
end;

procedure IEPNG_setFilter(comp: IEPNG_Compressor; method: integer; filters: integer);
begin
  png_set_filter(comp^.pngPtr, method, filters);
end;

procedure IEPNG_setBackground(comp: IEPNG_Compressor; colorValue: TRGB; colorIndex: byte);
var
  color16: png_color_16;
begin
  ZeroMemory(@color16, sizeof(png_color_16));
  color16.index := colorIndex;
  color16.red   := colorValue.r * 257;
  color16.green := colorValue.g * 257;
  color16.blue  := colorValue.b * 257;
  png_set_bKGD(comp^.pngPtr, comp^.infoPtr, @color16);
end;

procedure IEPNG_setCompressionLevel(comp: IEPNG_Compressor; level: integer);
begin
  png_set_compression_level(comp^.pngPtr, level);
end;

procedure IEPNG_setBGR(comp: IEPNG_Compressor);
begin
  png_set_bgr(comp^.pngPtr);
end;

procedure IEPNG_Comp_writeInfo(comp: IEPNG_Compressor);
begin
  png_write_info(comp^.pngPtr, comp^.infoPtr);
end;

function IEPNG_Comp_setInterlaceHandling(comp: IEPNG_Compressor): integer;
begin
  result := png_set_interlace_handling(comp^.pngPtr);
end;

procedure IEPNG_Comp_writeRows(comp: IEPNG_Compressor; row: pointer; numRows: dword);
begin
  png_write_rows(comp^.pngPtr, row, numRows);
end;

procedure IEPNG_Comp_writeEnd(comp: IEPNG_Compressor);
begin
  png_write_end(comp^.pngPtr, comp^.infoPtr);
end;

procedure IEPNG_Comp_setText(comp: IEPNG_Compressor; textList: IEPNG_TextList);
begin
  png_set_text(comp^.pngPtr, comp^.infoPtr, @textList[0], length(textList));
end;

function IEPNG_createTextList(size: integer): IEPNG_TextList;
begin
  SetLength(result, size);
end;

procedure IEPNG_textListSet(var textList: IEPNG_TextList; index: integer; compression: integer; key: string; text: string);
begin
  textList[index].compression := compression;
  textList[index].key         := IEStrDup(PAnsiChar(AnsiString(key)));
  textList[index].text        := IEStrDup(PAnsiChar(AnsiString(text)));
  textList[index].text_length := length( text );
end;

procedure IEPNG_destroyTextList(var textList: IEPNG_TextList);
var
  i: integer;
begin
  for i := 0 to length(textList) - 1 do
  begin
    freemem( textList[i].key );
    freemem( textList[i].text );
  end;
  SetLength(textList, 0);
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

IEPNG_Compressor = TIELibPNGCompressor;
IEPNG_Color      = pointer;
IEPNG_TextList   = TIELibPNGTextList;




function IEPNG_getIOPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor
begin
  result := IELib.PNGGetIOPtr(pngPtr);
end;

function IEPNG_getErrorPtr(pngPtr: pointer): pointer; // this must be pngPtr not IEPNG_Decompressor or IEPNG_Compressor
begin
  result := IELib.PNGGetErrorPtr(pngPtr);
end;

procedure IEPNG_Comp_destroy(var comp: IEPNG_Compressor);
begin
  comp := nil;
end;

function IEPNG_Comp_create(errorPtr: pointer; errorFunc: pointer; warnFunc: pointer): IEPNG_Compressor;
begin
  if IELibAvailable() then
  begin
    result := IELib.createPNGCompressor(errorPtr, errorFunc, warnFunc);
    if not result.isValid() then
      result := nil;
  end
  else
    raise EIEPNGException.Create(IERS_IEVISIONNOTFOUND);
end;

procedure IEPNG_Comp_setWriteFunction(comp: IEPNG_Compressor; ioPtr: pointer; writeFunc: pointer; flushFunc: pointer);
begin
  comp.setWriteFunction(ioPtr, writeFunc, flushFunc);
end;

procedure IEPNG_setPalette(comp: IEPNG_Compressor; palette: IEPNG_Color; numPalette: integer);
begin
  comp.setPalette(palette, numPalette);
end;

procedure IEPNG_setTRNS(comp: IEPNG_Compressor; trans: pbyte; numTrans: integer);
begin
  comp.setTRNS(trans, numTrans);
end;

procedure IEPNG_setIHDR(comp: IEPNG_Compressor; width: dword; height: dword; bitDepth: integer; colorType: integer; interlaceType: integer; compressionType: integer; filterType: integer);
begin
  comp.setIHDR(width, height, bitDepth, colorType, interlaceType, compressionType, filterType);
end;

procedure IEPNG_setPHYS(comp: IEPNG_Compressor; resX: dword; resY: dword; unitType: integer);
begin
  comp.setPHYS(resX, resY, unitType);
end;

procedure IEPNG_setFilter(comp: IEPNG_Compressor; method: integer; filters: integer);
begin
  comp.setFilter(method, filters);
end;

procedure IEPNG_setBackground(comp: IEPNG_Compressor; colorValue: TRGB; colorIndex: byte);
begin
  comp.setBackground(IETRGBToVisionBGR8(colorValue), colorIndex);
end;

procedure IEPNG_setCompressionLevel(comp: IEPNG_Compressor; level: integer);
begin
  comp.setCompressionLevel(level);
end;

procedure IEPNG_setBGR(comp: IEPNG_Compressor);
begin
  comp.setBGR();
end;

function IEPNG_createTextList(size: integer): IEPNG_TextList;
begin
  result := IELib.createPNGTextList(size);
end;

procedure IEPNG_textListSet(var textList: IEPNG_TextList; index: integer; compression: integer; key: string; text: string);
begin
  textList.assign(index, compression, PAnsiChar(AnsiString(key)), PAnsiChar(AnsiString(text)));
end;

procedure IEPNG_destroyTextList(var textList: IEPNG_TextList);
begin
  textList := nil;
end;

procedure IEPNG_Comp_setText(comp: IEPNG_Compressor; textList: IEPNG_TextList);
begin
  comp.setText(textList);
end;

procedure IEPNG_Comp_writeInfo(comp: IEPNG_Compressor);
begin
  comp.writeInfo();
end;

function IEPNG_Comp_setInterlaceHandling(comp: IEPNG_Compressor): integer;
begin
  result := comp.setInterlaceHandling();
end;

procedure IEPNG_Comp_writeRows(comp: IEPNG_Compressor; row: pointer; numRows: dword);
begin
  comp.writeRows(row, numRows);
end;

procedure IEPNG_Comp_writeEnd(comp: IEPNG_Compressor);
begin
  comp.writeEnd();
end;


{$endif}  // IEUSEDLLPNGLIB


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

type
  TIOData = record
    Stream:   TStream;
    Aborting: pboolean;
  end;
  PIOData = ^TIOData;


procedure ErrorFunc(png_ptr: pointer; msg: pointer); cdecl;
begin
  raise EInvalidGraphic.create('Error on creating PNG');
end;

procedure WarnFunc(png_ptr: pointer; msg: Pointer);  cdecl;
begin
end;

procedure WriteFunc(png_ptr: pointer; data: pointer; length: dword); cdecl;
var
  iodata: PIOData;
begin
  iodata := IEPNG_getIOPtr(png_ptr);
  if dword(iodata.Stream.Write(PAnsiChar(data)^, length)) < length then
    iodata.Aborting^ := true;
end;

procedure FlushFunc(png_ptr: pointer); cdecl;
begin
end;


procedure WritePNGStream(Stream: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var xProgress: TProgressRec; AlphaChannel: TIEMask);
var
  comp: IEPNG_Compressor;
  bit_depth, color_type, interlace_type: integer;
  WBitmap: TIEBitmap;
  BackCol, ForeCol: TRGB;
  FreeW: boolean; // if true then free WBitmap
  qt: TIEQuantizer;
  palette: array[0..255] of TRGB;
  ppalette: PRGBROW;
  number_passes, pass, y, x, height, width: integer;
  px, ppx: pointer;
  pp: PRGB;
  brow: pbyte;
  pw: pword;
  nullpr: TProgressRec;
  bitmapwidth1: integer;
  iodata: TIOData;
  px2, px4: PRGBA;
  px_byte, px3: pbyte;
  bb: byte;
  bps: integer;
  hasalpha: boolean;
  px_word: pword;
  i, altindex: integer;
  d, dt: double;
  tcl: TRGB;
  textList: IEPNG_TextList;
  num_text: integer;
begin
  with nullpr do
  begin
    Aborting    := xProgress.Aborting;
    fOnProgress := nil;
    Sender      := nil;
  end;

  ppalette   := nil;
  WBitmap    := nil;
  qt         := nil;
  textList   := nil;
  color_type := 0;

  comp := IEPNG_Comp_create(nil, @ErrorFunc, @WarnFunc);
  if comp = nil then
    raise EInvalidGraphic.create('Error on creating PNG');

  FreeW := false;

  try

    iodata.Stream   := Stream;
    iodata.Aborting := xProgress.Aborting;
    IEPNG_Comp_setWriteFunction(comp, @iodata, @WriteFunc, @FlushFunc);

    // Adjusts unsupported BitsPerSample and SamplesPerPixel
    if (IOParams.SamplesPerPixel=1) and (IOParams.BitsPerSample<>1) and (IOParams.BitsPerSample<>8) and (IOParams.BitsPerSample<>16) then
      IOParams.BitsPerSample := 8;

    // The bitmap to write will be contained in WBitmap
    if (IOParams.BitsPerSample = 1) then
    begin
      // required to save to b/w
      if (Bitmap.PixelFormat = ie1g) then
        WBitmap := Bitmap
      else
      begin
        // convert to 1 bit
        WBitmap := _ConvertTo1bitEx(Bitmap, BackCol, ForeCol);
        if WBitmap = nil then
        begin
          // impossible to convert to 1 bit, convert to ordered dither
          WBitmap := TIEBitmap.Create;
          WBitmap.Assign(Bitmap);
          WBitmap.PixelFormat := ie1g;
        end;
        FreeW := true;
      end;
    end
    else
    begin
      // required to save in true color
      if Bitmap.pixelformat = ie1g then
      begin
        // convert to 24 bit
        WBitmap := TIEBitmap.Create;
        WBitmap.Assign(Bitmap);
        WBitmap.PixelFormat := ie24RGB;
        FreeW := true;
      end
      else
        WBitmap := Bitmap;
    end;
    // assign interlace_type
    if ioparams.PNG_Interlaced then
      interlace_type := IEPNG_INTERLACE_ADAM7
    else
      interlace_type := IEPNG_INTERLACE_NONE;
    // assign bit_depth and color_type
    if ioparams.SamplesPerPixel = 1 then
    begin
      // B/W or palette
      if wbitmap.PixelFormat = ie1g then
      begin
        // B/W
        color_type := IEPNG_COLOR_TYPE_GRAY;
        bit_depth := 1;
      end
      else
      begin
        // palette
        color_type := IEPNG_COLOR_TYPE_PALETTE;
        bit_depth := ioparams.BitsPerSample;
      end;
    end
    else
    begin
      // true color
      color_type := IEPNG_COLOR_TYPE_RGB;
      bit_depth := 8;
    end;
    hasalpha := assigned(AlphaChannel) and (not AlphaChannel.Full);
    if hasalpha and (color_type = IEPNG_COLOR_TYPE_RGB) then
      color_type := color_type or IEPNG_COLOR_MASK_ALPHA;

    // Create palette if needed
    if (ioparams.SamplesPerPixel = 1) and (ioparams.BitsPerSample > 1) and (wbitmap.pixelformat <> ie1g) then
    begin
      if hasalpha then
      begin
        // save alpha channel as an entry in the color map (here 16 bit gray isn't supported)
        bit_depth := 8;
        bps := 1 shl ioparams.BitsPerSample;
        qt := TIEQuantizer.Create(wBitmap, palette, imin(bps, 255)); // entry 0 reserved for transparent layer
        getmem(ppalette, 256 * sizeof(TRGB));
        CopyMemory(@(ppalette[1]), @(palette[0]), 255 * sizeof(TRGB));
        with ppalette[0] do
        begin
          r := ioparams.PNG_Background.r;
          g := ioparams.PNG_Background.g;
          b := ioparams.PNG_Background.b;
        end;
        for x := 0 to 255 do
          bswap(ppalette^[x].r, ppalette^[x].b);
        IEPNG_setPalette(comp, IEPNG_Color(ppalette), imin(bps + 1, 256));
        if hasalpha then
        begin
          bb := 0;
          IEPNG_setTRNS(comp, @bb, 1);
        end;
      end
      else
      begin
        // do not save alpha. Full gray and 16 bit gray supported
        qt := TIEQuantizer.Create(wBitmap, palette, 256);
        getmem(ppalette, 256 * sizeof(TRGB));
        CopyMemory(@(ppalette[0]), @(palette[0]), 256 * sizeof(TRGB));
        for x := 0 to 255 do
          bswap(ppalette^[x].r, ppalette^[x].b);
        if not qt.GrayScale then
          IEPNG_setPalette(comp, IEPNG_Color(ppalette), 256)
        else
          color_type := IEPNG_COLOR_TYPE_GRAY;
      end;
    end
    else
    begin
      qt := nil;
      ppalette := nil;
    end;

    IEPNG_setIHDR(comp, bitmap.Width, bitmap.Height, bit_depth, color_type, interlace_type, IEPNG_COMPRESSION_TYPE_DEFAULT, IEPNG_FILTER_TYPE_DEFAULT);

    // DPI
    IEPNG_setPHYS(comp, round(ioparams.DPIX * 100 / CM_per_Inch), round(ioparams.DPIY * 100 / CM_per_Inch), IEPNG_RESOLUTION_METER);

    // filter
    case ioparams.PNG_Filter of
      ioPNG_FILTER_NONE:  IEPNG_setFilter(comp, 0, IEPNG_FILTER_NONE);
      ioPNG_FILTER_SUB:   IEPNG_setFilter(comp, 0, IEPNG_FILTER_SUB);
      ioPNG_FILTER_PAETH: IEPNG_setFilter(comp, 0, IEPNG_FILTER_PAETH);
      ioPNG_FILTER_UP:    IEPNG_setFilter(comp, 0, IEPNG_FILTER_UP);
      ioPNG_FILTER_AVG:   IEPNG_setFilter(comp, 0, IEPNG_FILTER_AVG);
      ioPNG_FILTER_ALL:   IEPNG_setFilter(comp, 0, IEPNG_ALL_FILTERS);
    end;

    // set background
    if assigned(qt) then
      IEPNG_setBackground(comp, CreateRGB(0, 0, 0), qt.RGBIndex[ioparams.PNG_Background])
    else
      IEPNG_setBackground(comp, ioparams.PNG_Background, 0);

    IEPNG_setCompressionLevel(comp, ioparams.PNG_Compression);

    IEPNG_setBGR(comp);

    // write text
    num_text := imin(IOParams.PNG_TextKeys.Count, IOParams.PNG_TextValues.Count);
    textList := IEPNG_createTextList(num_text);
    for i := 0 to num_text - 1 do
      IEPNG_textListSet(textList, i, -1, IOParams.PNG_TextKeys[i], IOParams.PNG_TextValues[i]);
    IEPNG_Comp_setText(comp, textList);

    IEPNG_Comp_writeInfo(comp);

    // write rows
    number_passes := IEPNG_Comp_setInterlaceHandling(comp);
    height := wbitmap.Height;
    width  := wbitmap.Width;
    xProgress.per1 := 100 / (height * number_passes);
    xProgress.val  := 0;
    px2 := nil;
    if (color_type = IEPNG_COLOR_TYPE_PALETTE) or (color_type = IEPNG_COLOR_TYPE_GRAY) then
      getmem(px, wbitmap.width * imax(1, bit_depth div 8));
    if (color_type and IEPNG_COLOR_MASK_ALPHA) <> 0 then
      getmem(px2, wbitmap.width * sizeof(TRGBA));
    for pass := 0 to number_passes - 1 do
    begin
      for y := 0 to height - 1 do
      begin
        if (color_type and IEPNG_COLOR_MASK_PALETTE) <> 0 then
        begin
          // palette
          brow := px;
          pp := wbitmap.Scanline[y];
          px_byte := pbyte(pp);
          bitmapwidth1 := wbitmap.Width - 1;
          if assigned(AlphaChannel) and (not AlphaChannel.Full) then
          begin
            // alpha channel
            case wbitmap.PixelFormat of
              ie24RGB:
                begin
                  px3 := alphachannel.Scanline[y];
                  for x := 0 to bitmapwidth1 do
                  begin
                    if px3^ < 255 then
                      brow^ := 0
                    else
                      brow^ := qt.RGBIndex[pp^] + 1;
                    inc(brow);
                    inc(pp);
                    inc(px3);
                  end;
                end;
              ie8p:
                begin
                  // search an alternate color for indexes with transparent index
                  d := 100000;
                  tcl := wbitmap.Palette[0];
                  altindex := 0;
                  for i := 1 to wbitmap.PaletteLength-1 do
                  begin
                    with wbitmap.Palette[i] do
                    begin
                      dt := sqr(r-tcl.r)+sqr(g-tcl.g)+sqr(b-tcl.b);
                      if dt < d then
                      begin
                        d := dt;
                        altindex := i;
                      end;
                    end;
                  end;

                  for x := 0 to bitmapwidth1 do
                  begin
                    brow^ := px_byte^;
                    if brow^ = 0 then
                      brow^ := altindex;
                    inc(brow);
                    inc(px_byte);
                  end;
                end;
              ie8g:
                begin
                  px3 := alphachannel.Scanline[y];
                  for x := 0 to bitmapwidth1 do
                  begin
                    brow^ := px_byte^;
                    if px3^ < 255 then
                      brow^ := 0
                    else
                    if brow^ = 0 then
                      brow^ := 1;
                    inc(brow);
                    inc(px_byte);
                    inc(px3);
                  end;
                end;
            end
          end
          else
          begin
            // simple palette
            case wbitmap.PixelFormat of
              ie24RGB:
                for x := 0 to bitmapwidth1 do
                begin
                  brow^ := qt.RGBIndex[pp^];
                  inc(brow);
                  inc(pp);
                end;
              ie8p:
                for x := 0 to bitmapwidth1 do
                begin
                  brow^ := px_byte^;
                  inc(brow);
                  inc(px_byte);
                end;
            end;
          end;
          IEPNG_Comp_writeRows(comp, @px, 1);
        end
        else
        if (color_type and IEPNG_COLOR_MASK_COLOR) <> 0 then
        begin
          // truecolor
          if (color_type and IEPNG_COLOR_MASK_ALPHA) <> 0 then
          begin
            // alpha channel
            pp  := wbitmap.Scanline[y];
            px3 := alphachannel.Scanline[y];
            px4 := px2;
            for x := 0 to width - 1 do
            begin
              with px4^ do
              begin
                r := pp^.r;
                g := pp^.g;
                b := pp^.b;
                a := px3^;
              end;
              inc(pp);
              inc(px3);
              inc(px4);
            end;
            IEPNG_Comp_writeRows(comp, @px2, 1);
          end
          else
          begin
            ppx := wbitmap.Scanline[y];
            IEPNG_Comp_writeRows(comp, @ppx, 1);
          end;
        end
        else
        if (color_type = IEPNG_COLOR_TYPE_GRAY) then
        begin
          // gray scale
          if bit_depth = 16 then
          begin
            case wbitmap.PixelFormat of
              ie24RGB:
                begin
                  pp := wbitmap.Scanline[y];
                  pw := px;
                  for x := 0 to width - 1 do
                  begin
                    pw^ := qt.RGBIndex[pp^];
                    inc(pw);
                    inc(pp);
                  end;
                end;
              ie16g:
                begin
                  px_word := wbitmap.Scanline[y];
                  pw := px;
                  for x := 0 to width - 1 do
                  begin
                    pw^ := px_word^ shr 8;
                    inc(pw);
                    inc(px_word);
                  end;
                end;
            end;
            IEPNG_Comp_writeRows(comp, @px, 1);
          end
          else
          if bit_depth = 8 then
          begin
            case wbitmap.PixelFormat of
              ie24RGB:
                begin
                  pp := wbitmap.Scanline[y];
                  brow := px;
                  for x := 0 to width - 1 do
                  begin
                    brow^ := qt.RGBIndex[pp^];
                    inc(brow);
                    inc(pp);
                  end;
                end;
              ie8g:
                begin
                  px_byte := wbitmap.Scanline[y];
                  brow := px;
                  for x := 0 to width - 1 do
                  begin
                    brow^ := px_byte^;
                    inc(brow);
                    inc(px_byte);
                  end;
                end;
            end;
            IEPNG_Comp_writeRows(comp, @px, 1);
          end
          else
          begin
            ppx := wbitmap.Scanline[y];
            IEPNG_Comp_writeRows(comp, @ppx, 1);
          end;
        end;
        // OnProgress
        with xProgress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
        if xProgress.Aborting^ then
          break;
      end;
      if xProgress.Aborting^ then
        break;
    end;

  finally

    if (color_type and IEPNG_COLOR_MASK_ALPHA) <> 0 then
      freemem(px2);
    if (color_type = IEPNG_COLOR_TYPE_PALETTE) or (color_type = IEPNG_COLOR_TYPE_GRAY) then
      freemem(px);

    // cleanup

    try
      if not xProgress.Aborting^ then
        IEPNG_Comp_writeEnd(comp);
    finally

      if ppalette <> nil then
        freemem(ppalette);

      IEPNG_Comp_destroy(comp);

      if FreeW then
        FreeAndNil(WBitmap);
      if assigned(qt) then
        FreeAndNil(qt);

      IEPNG_destroyTextList(textList);
    end;
  end;
end;










{$ELSE} // IEINCLUDEPNG

interface
implementation

{$ENDIF}




end.
