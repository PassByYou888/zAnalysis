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

unit iezlib;

{$R-}
{$Q-}

{$I ie.inc}

{$IFNDEF IEUSEVCLZLIB}

{$IFDEF IEINCLUDEZLIB}


interface

uses Windows, Graphics, classes, sysutils, ImageEnProc, ImageEnIO, hyiedefs, hyieutils;


type

  // portions of ZLib

  EZLibError = class(Exception);

  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

  TZAlloc = function(opaque: Pointer; items, size: Integer): Pointer;
  TZFree = procedure(opaque, block: Pointer);
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  TZStreamRec = packed record
    next_in: PAnsiChar; // next input byte
    avail_in: Longint;  // number of bytes available at next_in
    total_in: Longint;  // total nb of input bytes read so far

    next_out: PAnsiChar; // next output byte should be put here
    avail_out: Longint;  // remaining free space at next_out
    total_out: Longint;  // total nb of bytes output so far

    msg: PAnsiChar; // last error message, NULL if no error
    state: Pointer; // not visible by applications

    zalloc: TZAlloc; // used to allocate the internal state
    zfree: TZFree;   // used to free the internal state
    opaque: Pointer; // private data object passed to zalloc and zfree

    data_type: Integer; // best guess about the data type: ascii or binary
    adler: Longint;     // adler32 value of the uncompressed data
    reserved: Longint;  // reserved for future use
  end;

  TCustomZStream = class(TStream)
  private
    FStream: TStream;
    FStreamPos: Int64;
    FOnProgress: TNotifyEvent;

    FZStream: TZStreamRec;
    FBuffer: array[Word] of AnsiChar;
  protected
    constructor Create(stream: TStream);

    procedure DoProgress; dynamic;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel; windowBits: integer = 15);
    destructor Destroy; override;

    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;

    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream);
    destructor Destroy; override;

    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;

    property OnProgress;
  end;


  procedure ZCompress(const inBuffer: Pointer; inSize: Integer; out outBuffer: Pointer; out outSize: Integer; level: TZCompressionLevel);
  procedure ZDecompress(const inBuffer: Pointer; inSize: Integer; out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
  function ZCompressStr(const s: AnsiString; level: TZCompressionLevel): AnsiString;
  function ZDecompressStr(const s: AnsiString): AnsiString;
  procedure ZCompressStream(inStream, outStream: TStream; level: TZCompressionLevel);
  procedure ZDecompressStream(inStream, outStream: TStream);



  procedure _abort; cdecl; forward;
  function _memcmp(buf1, buf2: pbyte; count: integer): integer; cdecl; forward;
  procedure memset(P: Pointer; B: Byte; count: Integer); cdecl; forward;
  procedure memcpy(dest, source: Pointer; count: Integer); cdecl; forward;
  function _malloc(size: Integer): Pointer; cdecl; forward;
  procedure _free(P: Pointer); cdecl; forward;
  function _fabs(v: double): double; cdecl; forward;
  function _pow(Base, Exponent: double): double; cdecl; forward;
  function __ftol: Integer; cdecl; forward;
  function _strlen(s: PAnsiChar): cardinal; cdecl; forward;
  function _strtod(s: PAnsiChar; var vp: PAnsiChar): double; cdecl;

  procedure crc32; cdecl; external;
  function deflateInit_(var strm: TZStreamRec; level: Integer; version: PAnsiChar; recsize: Integer): Integer;  cdecl; external;
  function deflate(var strm: TZStreamRec; flush: Integer): Integer;  cdecl; external;
  function deflateEnd(var strm: TZStreamRec): Integer;  cdecl; external;
  function inflateInit_(var strm: TZStreamRec; version: PAnsiChar; recsize: Integer): Integer;  cdecl; external;
  function inflate(var strm: TZStreamRec; flush: Integer): Integer;  cdecl; external;
  function inflateEnd(var strm: TZStreamRec): Integer;  cdecl; external;
  function inflateReset(var strm: TZStreamRec): Integer;  cdecl; external;
  procedure deflateInit2_;  cdecl; external;
  procedure deflateReset;  cdecl; external;



implementation


uses math;

{$R-}




const
  ZLIB_VERSION: AnsiString = '1.2.3';

const
  Z_NO_FLUSH = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;
  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = (-1);
  Z_STREAM_ERROR = (-2);
  Z_DATA_ERROR = (-3);
  Z_MEM_ERROR = (-4);
  Z_BUF_ERROR = (-5);
  Z_VERSION_ERROR = (-6);
  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = (-1);
  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_DEFAULT_STRATEGY = 0;
  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;
  Z_DEFLATED = 8;
  z_errmsg: array[0..9] of PWideChar = (
    'need dictionary', // Z_NEED_DICT      (2)
    'stream end', // Z_STREAM_END     (1)
    '', // Z_OK             (0)
    'file error', // Z_ERRNO          (-1)
    'stream error', // Z_STREAM_ERROR   (-2)
    'data error', // Z_DATA_ERROR     (-3)
    'insufficient memory', // Z_MEM_ERROR      (-4)
    'buffer error', // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
    );

// Compatibility with ZipForge (3.0.4)
_z_errmsg: array[0..9] of PAnsiChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    '',                     // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
    );

  ZLevels: array[TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
    );

  SZInvalid = 'Invalid ZStream operation!';

(*
{$L deflate.obj}
{$L inflate.obj}
{$L infblock.obj}
{$L inftrees.obj}
{$L infcodes.obj}
{$L infutil.obj}
{$L inffast.obj}
{$L trees.obj}
{$L adler32.obj}
{$L crc32.obj}
*)

{$L deflate.obj}
{$L inflate.obj}
{$L inftrees.obj}
{$L infback.obj}
{$L inffast.obj}
{$L trees.obj}
{$L adler32.obj}
{$L crc32.obj}
{$L compress.obj}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer; cdecl;
begin
  GetMem(result, items * size);
end;

procedure zcfree(opaque, block: Pointer); cdecl;
begin
  FreeMem(block);
end;

function DeflateInit(var stream: TZStreamRec; level: Integer): Integer; cdecl;
begin
  result := DeflateInit_(stream, level, PAnsiChar(ZLIB_VERSION), SizeOf(TZStreamRec));
end;

function InflateInit(var stream: TZStreamRec): Integer;  cdecl;
begin
  result := InflateInit_(stream, PAnsiChar(ZLIB_VERSION), SizeOf(TZStreamRec));
end;

procedure memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

// compatibility with ZipForge (3.0.4)
procedure _memset(P: Pointer; B: byte; Count: integer); cdecl;
begin
  FillChar(P^, Count, B);
end;

function _memcmp(buf1, buf2: pbyte; count: integer): integer; cdecl;
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


function _strtod(s: PAnsiChar; var vp: PAnsiChar): double; cdecl;
begin
  vp := @s[IEStrLen(s) - 1]; // !!
  result := IEStrToFloatDefA(s, 0);
end;

procedure _abort; cdecl;
begin
end;

procedure memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

// Compatibility with ZipForge (3.0.4)
procedure _memcpy(dest, Source: Pointer; Count: integer); cdecl;
begin
  Move(Source^, dest^, Count);
end;

function _malloc(size: Integer): Pointer; cdecl;
begin
  GetMem(Result, size);
end;

procedure _free(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

function _fabs(v: double): double; cdecl;
begin
  result := abs(v);
end;


function _pow(Base, Exponent: double): double; cdecl;
begin
  if Exponent = 0.0 then
    Result := 1.0 { n**0 = 1 }
  else
  if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0 { 0**n = 0, n > 0 }
  else
  if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := IntPower(Base, Trunc(Exponent))
  else
    Result := Exp(Exponent * Ln(Base))
end;

function __ftol: Integer; cdecl;
var
  f: double;
begin
  asm
    lea    eax, f             //  BC++ passes floats on the FPU stack
    fstp  qword ptr [eax]     //  Delphi passes floats on the CPU stack
  end;
  Result := Trunc(f);
end;

function _strlen(s: PAnsiChar): cardinal; cdecl;
begin
  result := IEStrLen(s);
end;
            
var
  __turboFloat: LongBool = False;


function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(z_errmsg[2 - code]);
  end;
end;

function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZDecompressionError.Create(z_errmsg[2 - code]);
  end;
end;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel);
const
  delta = 256;
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit(zstream, ZLevels[level]));

    try
      while ZCompressCheck(deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PAnsiChar(uint64(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer; out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: TZStreamRec;
  delta: Integer;
  chk: Integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then
    outSize := delta
  else
    outSize := outEstimate;

  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZDecompressCheck(InflateInit(zstream));

    try
      while true do
      begin
        chk := inflate(zstream, Z_NO_FLUSH);
        if (chk = Z_STREAM_END) or (chk = Z_DATA_ERROR) or (chk = Z_NEED_DICT) or (chk = Z_MEM_ERROR) then
          break;
        ZDecompressCheck(chk);  // may raise exception for the cases not covered by previous check

        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PAnsiChar(uint64(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

{** string routines *********************************************************}

function ZCompressStr(const s: AnsiString; level: TZCompressionLevel): AnsiString;
var
  buffer: Pointer;
  size: Integer;
begin
  ZCompress(PAnsiChar(s), Length(s), buffer, size, level);

  SetLength(result, size);
  Move(buffer^, result[1], size);

  FreeMem(buffer);
end;

function ZDecompressStr(const s: AnsiString): AnsiString;
var
  buffer: Pointer;
  size: Integer;
begin
  ZDecompress(PAnsiChar(s), Length(s), buffer, size, 0);

  SetLength(result, size);
  Move(buffer^, result[1], size);

  FreeMem(buffer);
end;

{** stream routines *********************************************************}

procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel);
const
  bufferSize = 32768;
var
  zstream: TZStreamRec;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of AnsiChar;
  outBuffer: array[0..bufferSize - 1] of AnsiChar;
  inSize: Integer;
  outSize: Integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(DeflateInit(zstream, ZLevels[level]));

  inSize := inStream.Read(inBuffer, bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      ZCompressCheck(deflate(zstream, Z_NO_FLUSH));

      // outSize := zstream.next_out - outBuffer;
      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer, bufferSize);
  end;

  repeat
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(deflate(zstream, Z_FINISH));

    // outSize := zstream.next_out - outBuffer;
    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(deflateEnd(zstream));
end;

procedure ZDecompressStream(inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zstream: TZStreamRec;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of AnsiChar;
  outBuffer: array[0..bufferSize - 1] of AnsiChar;
  inSize: Integer;
  outSize: Integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(InflateInit(zstream));

  inSize := inStream.Read(inBuffer, bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      ZCompressCheck(inflate(zstream, Z_NO_FLUSH));

      // outSize := zstream.next_out - outBuffer;
      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer, bufferSize);
  end;

  repeat
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(inflate(zstream, Z_FINISH));

    // outSize := zstream.next_out - outBuffer;
    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(inflateEnd(zstream));
end;

{** TCustomZStream **********************************************************}

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

{** TZCompressionStream *****************************************************}

// windowBits = not used
constructor TZCompressionStream.Create(dest: TStream; compressionLevel: TZCompressionLevel; windowBits: integer);
begin
  inherited Create(dest);

  FZStream.next_out := FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(DeflateInit(FZStream, ZLevels[compressionLevel]));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

  try
    if FStream.Position <> FStreamPos then
      FStream.Position := FStreamPos;

    while ZCompressCheck(deflate(FZStream, Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    deflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(deflate(FZStream, Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer));

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
    end;
  end;

  result := Count;
end;

// 3.0.1
function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  result := FZStream.total_in;
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then
    result := 0
  else
    result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TZDecompressionStream ***************************************************}

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);

  FZStream.next_in := FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(InflateInit(FZStream));
end;

destructor TZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);

  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer, SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := count - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := FBuffer;
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    zresult := ZDecompressCheck(inflate(FZStream, Z_NO_FLUSH));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;

    FZStream.avail_in := 0;
  end;

  result := count - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: array[0..8191] of AnsiChar;
  i: Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));

    FZStream.next_in := FBuffer;
    FZStream.avail_in := 0;

    FStream.Position := 0;
    FStreamPos := 0;
  end
  else
  if ((offset >= 0) and (origin = soFromCurrent)) or
    (((offset - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then
      Dec(offset, FZStream.total_out);

    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do
        ReadBuffer(buf, SizeOf(buf));
      ReadBuffer(buf, offset mod SizeOf(buf));
    end;
  end
  else
  if (offset = 0) and (origin = soFromEnd) then
  begin
    while Read(buf, SizeOf(buf)) > 0 do
      ;
  end
  else
    raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;



{$else} // IEINCLUDEZLIB



interface

uses Windows, Graphics, classes, sysutils, ImageEnProc, ImageEnIO, hyiedefs, hyieutils;

type

  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  TZCompressionStream = class(TStream)
  private
    fDest: TStream;
  public
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel; windowBits: integer = 15);
    destructor Destroy; override;

    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;

  end;

  TZDecompressionStream = class(TStream)
  private
    fSource: TStream;
  public
    constructor Create(source: TStream);
    destructor Destroy; override;

    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;

  end;


implementation

constructor TZCompressionStream.Create(dest: TStream; compressionLevel: TZCompressionLevel; windowBits: integer = 15);
begin
  inherited Create;
  fDest := dest;
end;

destructor TZCompressionStream.Destroy;
begin
  inherited;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  result := fDest.Read(buffer, count);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  result := fDest.Write(buffer, count);
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  result := fDest.Seek(offset, origin);
end;

///////////////////

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create;
  fSource := source;
end;

destructor TZDecompressionStream.Destroy;
begin
  inherited;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
begin
  result := fSource.Read(buffer, count);
end;

function TZDecompressionStream.Write(const buffer; count: Longint): Longint;
begin
  result := fSource.Write(buffer, count);
end;

function TZDecompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  result := fSource.Seek(offset, origin);
end;


{$endif}  // IEINCLUDEZLIB

{$else}  // IEUSEVCLZLIB

interface

implementation

{$endif}  // IEUSEVCLZLIB

end.
