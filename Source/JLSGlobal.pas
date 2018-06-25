{ ****************************************************************************** }
{ * JPEG-LS Codec https://github.com/zekiguven/pascal_jls                      * }
{ * fixed by QQ 600585@qq.com                                                  * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  https://github.com/zekiguven/pascal_jls

  author : Zeki Guven
}
unit JLSGlobal;

{$I zDefine.inc}

interface

uses
  CoreClasses, PascalStrings, ListEngine;

const
  { Version number }
  JPEGLSVERSION = 'V.2.3'; // last by 600585@qq.com

  { Maximal number of components in the implementation }
  MAX_COMPONENTS = 6;
  MAX_SCANS      = MAX_COMPONENTS;

  { For 1st component of plane interl. mode }
  FIRST = 1;

  { Different colour modes }
  PLANE_INT = 0;
  LINE_INT  = 1;
  PIXEL_INT = 2;

  DEFAULT_COLOR_MODE = LINE_INT;

  { margins for scan lines }
  LEFTMARGIN  = 2;
  RIGHTMARGIN = 1;

  { alphabet size }
  MAXA8    = 256;
  MAXA16   = 65536;
  LUTMAX8  = 256;
  LUTMAX16 = 4501;

  DEF_NEAR  = 0;
  DEF_ALPHA = 255;

  { Quantization threshold basic defaults
    These are the defaults for LOSSLESS, 8 bpp. Defaults for other
    cases are derived from these basic values
  }
  BASIC_T1 = 3;
  BASIC_T2 = 7;
  BASIC_T3 = 21;
  BASIC_Ta = 5;

  CREGIONS = 9; { quantization regions for d-b, b-c, c-a }

  MAXRUN   = 64;
  EOLINE   = 1;
  NOEOLINE = 0;

  { number of different contexts }
  CONTEXTS1 = (CREGIONS * CREGIONS * CREGIONS);

  CONTEXTS = ((CONTEXTS1 + 1) div 2); { all regions, with symmetric merging }

  MAX_C = 127;
  MIN_C = -128;


  // MAXCODE= (N_R_L_ERROR);

  { Number of end-of-run contexts }
  EOR_CONTEXTS = 2;

  { Total number of contexts }
  TOT_CONTEXTS = (CONTEXTS + EOR_CONTEXTS);

  { index of first end-of-run context }
  EOR_0 = CONTEXTS;

  { index of run state }
  RUNSTATE = 0;

  { *** offsets }

  { The longest code the bit IO can facilitate }
  MAXCODELEN = 24;

  { The stat initialization values }
  INITNSTAT      = 1; { init value for N[] }
  MIN_INITABSTAT = 2; { min init value for A[] }
  INITABSLACK    = 6; { init value for A is roughly
    2^(bpp-INITABSLACK) but not less than above }
  INITBIASTAT = 0; { init value for B[] }

  { reset values }
  DEFAULT_RESET = 64;
  MINRESET      = 3;

  BUF_EOF = -1;

  { define color mode strings }
  plane_int_string = 'plane by plane';
  line_int_string  = 'line intlv';
  pixel_int_string = 'sample intlv';

  NEGBUFFSIZE = 4;

type
  Pixel  = Word;
  PPixel = ^Pixel;
  int    = integer;
  pint   = ^int;

  TByteArray = array [0 .. MaxInt div SizeOf(byte) - 1] of byte;
  PByteArray = ^TByteArray;

  TWordArray = array [0 .. MaxInt div SizeOf(Word) - 1] of Word;
  PWordArray = ^TWordArray;

  IntArrayAccess  = packed array [0 .. MaxInt div SizeOf(int) - 1] of int;
  ByteArrayAccess = packed array [0 .. MaxInt - 1] of byte;
  PixelArray      = packed array [-NEGBUFFSIZE .. MaxInt div SizeOf(Word) - (NEGBUFFSIZE + 1)] of Pixel;

  PIntArrayAccess  = ^IntArrayAccess;
  PByteArrayAccess = ^ByteArrayAccess;
  PPixelArray      = ^PixelArray;
  size_t           = Cardinal;
  uint             = Cardinal;
  short            = smallint;
  long             = longint;
  ulong            = NativeUInt;
  TBytes           = packed array of byte;

  TABLE_ARRAY = packed array [0 .. MAX_COMPONENTS - 1] of PWordArray;
  PTABLE      = ^TABLE_ARRAY;

  /// <summary>
  /// Defines the JPEG-LS preset coding parameters as defined in ISO/IEC 14495-1, C.2.4.1.1.
  /// JPEG-LS defines a default set of parameters, but custom parameters can be used.
  /// When used these parameters are written into the encoded bit stream as they are needed for the decoding process.
  /// </summary>
  TJlsCustomParameters = packed record
    /// <summary>
    /// Maximum possible value for any image sample in a scan.
    /// This must be greater than or equal to the actual maximum value for the components in a scan.
    /// </summary>
    MAXVAL: integer;
    /// <summary>
    /// First quantization threshold value for the local gradients.
    /// </summary>
    T1: integer;
    /// <summary>
    /// Second quantization threshold value for the local gradients.
    /// </summary>
    T2: integer;
    /// <summary>
    /// Third quantization threshold value for the local gradients.
    /// </summary>
    T3: integer;
    /// <summary>
    /// Value at which the counters A, B, and N are halved.
    /// </summary>
    RESET: integer;
  end;

  PJlsParameters = ^TJlsParameters;

  TJlsParameters = packed record
    /// <summary>
    /// Width of the image in pixels.
    /// </summary>
    Width: integer;
    /// <summary>
    /// Height of the image in pixels.
    /// </summary>
    Height: integer;
    /// <summary>
    /// The number of valid bits per sample to encode.
    /// Valid range 2 - 16. When greater than 8, pixels are assumed to stored as two bytes per sample, otherwise one byte per sample is assumed.
    /// </summary>
    BitsPerSample: integer;
    /// <summary>
    /// The number of components.
    /// Typical 1 for monochrome images and 3 for color images.
    /// </summary>
    Components: integer;
    /// <summary>
    /// Defines the allowed lossy error. Value 0 defines lossless.
    /// </summary>
    AllowedLossyError: integer;
    Custom: TJlsCustomParameters;
  end;

  Pjpeg_ls_header = ^Tjpeg_ls_header;

  Tjpeg_ls_header = packed record
    columns: integer;                                             { The number of columns }
    rows: integer;                                                { Number of rows }
    alp: integer;                                                 { alphabet size (Max+1) , 2 bytes }
    comp: integer;                                                { number of components, 1 byte }
    _near: integer;                                               { _near-lossless error, 1 byte }
    color_mode: integer;                                          { indicates the color mode , 1 byte }
    need_lse: integer;                                            { Indicates non-default parameters }
    need_table: integer;                                          { Indicates use of mapping table }
    need_restart: integer;                                        { Indicates use of restart markers }
    restart_interval: integer;                                    { The number of MCU's between restart markers }
    shift: integer;                                               { for sparse images, 1 byte }
    T1, T2, T3: integer;                                          { Thresholds, 2 bytes each }
    RES: integer;                                                 { reset value for counters, 2 bytes }
    samplingx: packed array [0 .. MAX_COMPONENTS - 1] of integer; { col. sampling rates 1 byte each }
    samplingy: packed array [0 .. MAX_COMPONENTS - 1] of integer; { row sampling rates }
    comp_ids: packed array [0 .. MAX_COMPONENTS - 1] of integer;  { component id's }
    acc_size: integer;                                            { 1 byte }
    adds: packed array [0 .. MAX_COMPONENTS - 1] of integer;      { size given by acc_size }
    TID: uint;                                                    { Table ID, 1 byte }
    MAXTAB: uint;                                                 { Maximum table index value }
    Wt: uint;                                                     { Width of each table entry, 1 byte }
    TABLE: PTABLE                                                 { The table(s) for each component }
    end;

  type
    TImageInfo = packed record
      bpp16: boolean; { Indicates if 16 bits per pixel mode or not }
      Width: int;
      Height: int;
      Components: integer;
      limit_reduce: integer; { reduction on above for EOR states }
      qbpp: integer;         { bits per sample for quantized prediction errors }
      alpha: integer;        { alphabet size }
      limit: integer;        { limit for unary part of Golomb code }
      RESET: integer;
      highmask: integer;        { for powers of 2, a mask for high bits }
      ceil_half_alpha: integer; { ceil(alpha/2) }
      _near: integer;           { loss tolerance }
      { LOSSY Mode }
      negNEAR: integer;

      qdiv0: PIntegerArray; { quantization table (division via look-up) }
      qdiv: PInteger;       { quantization table (division via look-up) }
      qmul0: PIntegerArray; { dequantization table }
      qmul: PInteger;       { dequantization table }

      quant: int;           { quantization = 2*_near+1 }
      beta: int;            { size of extended alphabet }
      qbeta: int;           { size of quantized alphabet }
      ceil_half_qbeta: int; { ceil(qbeta/2) }
      alpha1eps: int;       { alpha-1+_near }

      N: packed array [0 .. TOT_CONTEXTS - 1] of int;
      A: packed array [0 .. TOT_CONTEXTS - 1] of int;
      B: packed array [0 .. TOT_CONTEXTS - 1] of int;
      C: packed array [0 .. TOT_CONTEXTS - 1] of int;

      vLUT: packed array [0 .. 3 - 1, 0 .. 2 * LUTMAX16 - 1] of int;
      classmap: packed array [0 .. CONTEXTS1 - 1] of int;

    end;

    PImageInfo = ^TImageInfo;

  procedure error(msg: string);
  function safealloc(size: size_t): Pointer;
  function safecalloc(numels, size: size_t): Pointer;

  function predict(Rb, Ra, Rc: Word): Word;

  function IsTrue(AVAlue: integer): boolean;
  function ENDIAN8(x: Word): byte;
  function ENDIAN16(x: Word): Word;

  function check_compatibility(head_frame: Pjpeg_ls_header; head_scan: Pjpeg_ls_header; n_s: int): int;

  function shr_c(Value: Int64; ShiftBits: integer): Int64; overload;
  function shr_c(Value: integer; ShiftBits: integer): integer; overload;

  function Bool_c(AVAlue: boolean): integer;

implementation

uses DoStatusIO;

function ENDIAN8(x: Word): byte;
begin
  Result := (x and $000000FF)
end;

function ENDIAN16(x: Word): Word;
begin
  Result := x; // ( ((x shr 8) or(x shl 8)) and $0000ffff);
end;

function Bool_c(AVAlue: boolean): integer;
begin
  if AVAlue then
      Result := 1
  else
      Result := 0;
end;

function IsTrue(AVAlue: integer): boolean;
begin
  Result := AVAlue <> 0;
end;

function shr_c(Value: Int64; ShiftBits: integer): Int64; overload;
begin
  Result := Value shr ShiftBits;
  if (Value and $8000000000000000) > 0 then
      Result := Result or ($FFFFFFFFFFFFFFFF shl (64 - ShiftBits));
end;

function shr_c(Value: integer; ShiftBits: integer): integer; overload;
begin
  Result := Value shr ShiftBits;
  if (Value and $80000000) > 0 then
      Result := Result or integer($FFFFFFFF shl (64 - ShiftBits));
end;

{ function to print out error messages }
procedure error(msg: string);
begin
  RaiseInfo(msg);
end;

{ function to safely call malloc }
function safealloc(size: size_t): Pointer;
var
  temp: Pointer;
begin
  temp := AllocMem(size);
  if (temp = nil) then
      error('safealloc: Out of memory. Aborting...');
  Result := temp;
end;

{ function to safely call calloc }
function safecalloc(numels, size: size_t): Pointer;
var
  temp: Pointer;
begin
  temp := AllocMem(numels * size);
  if (temp = nil) then
      error('safecalloc: Out of memory. Aborting...');
  Result := temp;
end;

{ macro to predict Px }
function predict(Rb, Ra, Rc: Word): Word;
var
  minx, maxx: Pixel;
begin
  if (Rb > Ra) then
    begin
      minx := Ra;
      maxx := Rb;
    end
  else
    begin
      maxx := Ra;
      minx := Rb;
    end;

  if (Rc >= maxx) then
      Result := minx
  else if (Rc <= minx) then
      Result := maxx
  else
      Result := Ra + Rb - Rc;
end;

{ We first check compatibility with JPEG-LS, then with this implementation }

function check_compatibility(head_frame: Pjpeg_ls_header; head_scan: Pjpeg_ls_header; n_s: int): int;
var
  number_of_scans, i: int;
  maxreset: int;
begin
  Result := 0;
  { Check implemented color modes }
  if ((head_scan^.color_mode > PIXEL_INT)) then
    begin
      DoStatus('Color mode %d not supported.', [head_scan^.color_mode]);
      Result := 10;
      exit;
    end;

  if (head_scan^.color_mode = PLANE_INT) then
      number_of_scans := head_frame^.comp
  else
      number_of_scans := 1;

  { Test standard compatibility }

  if (head_frame^.columns <= 0) or (head_frame^.rows <= 0) then
    begin
      DoStatus('Image size must be positive for this implementation.');
      Result := 10;
      exit;
    end;

  if (head_frame^.alp < 4) then
    begin
      DoStatus('Alphabet size must be >= 4, got %d.', [head_frame^.alp]);
      Result := 10;
      exit;
    end;

  if (head_scan^.T1 > head_scan^.T2) or (head_scan^.T2 > head_scan^.T3) or
    (head_scan^.T1 < head_scan^._near + 1) or (head_scan^.T3 >= head_scan^.alp) then
    begin
      DoStatus('Bad thresholds: must be %d <= Ta <= Tb <= Tc <= %d.', [head_scan^._near + 1, head_scan^.alp - 1]);
      Result := 10;
      exit;
    end;

  if (head_frame^.comp > 255) then
    begin
      DoStatus('Too many components (must be less than 255).');
      Result := 10;
      exit;
    end;

  if (head_scan^._near >= head_scan^.alp) then
    begin
      DoStatus('Error for _near-lossless must be smaller than alphabet (%d), got %d.', [head_scan^.alp, head_scan^._near]);
      Result := 10;
      exit;
    end;

  if (head_scan^.alp >= 256) then
      maxreset := head_scan^.alp - 1
  else
      maxreset := 255;

  if (head_scan^.RES < MINRESET) or (head_scan^.RES > maxreset) then
    begin
      DoStatus('Reset parameter must be between %d and %d.', [MINRESET, head_scan^.alp - 1]);
      Result := 10;
      exit;
    end;

  for i := 0 to pred(head_frame^.comp) do
    if (head_frame^.comp_ids[i] <> (i + 1)) then
      begin
        DoStatus('Components id in frame not compatible with this implementation.');
        Result := 10;
        exit;
      end;

  if (number_of_scans = 1) then
    begin
      if (head_frame^.comp <> head_scan^.comp) then
        begin
          DoStatus('In this implementation, when single scan, all components must be in the scan.');
          Result := 10;
          exit;
        end;

      for i := 0 to pred(head_frame^.comp) do
        if (head_scan^.comp_ids[i] <> (i + 1)) then
          begin
            DoStatus('Components id in single scan not compatible with this implementation.');
            Result := 10;
            exit;
          end;

    end
  else
    begin
      if (head_scan^.comp <> 1) then
        begin
          DoStatus('Only 1 component per scan for plane interleaved mode.');
          Result := 10;
          exit;
        end;

      if (head_scan^.comp_ids[0] <> (n_s + 1)) then
        begin
          DoStatus('Components id in multiple scan not compatible with this implementation.');
          Result := 10;
          exit;
        end;

    end;
end;

end.
