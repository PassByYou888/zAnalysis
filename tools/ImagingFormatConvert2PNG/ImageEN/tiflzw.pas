(*
File version 1001
*)

unit tiflzw;

{$R-}
{$Q-}

// Example of TIFF-LZW decompression plug-in for ImageEn

{$I ie.inc}

interface

uses Windows, classes, sysutils, hyieutils;


function TIFFLZWDecompress(CompBuf: pbyte; LineSize: integer; var Id: pointer; FillOrder: integer): pbyte;
procedure TIFFLZWCompress(indata: pbyte; inputlen: integer; outstream: TStream; var id: pointer);


implementation

{$R-}

const
  EOICODE = 257;
  CLEARCODE = 256;
  MAXPREALLOC = 32; // preallocated byte (min 3) (great is more quick)
  DECOMPBLOCKSIZE = 1024;

type

  // string table
  TSItem = record
    Data: pbyte;
    Dim: integer;
    PreAlloc: array[0..MAXPREALLOC - 1] of byte; // preallocated bytes
  end;
  PSItem = ^TSItem;

  // LZW decompressor record
  TTIFLZWDec = record
    // single row decompressed (in-class allocated)
    fDecomp: pbyte;
    fDecompAllocated: integer;
    // compressed buffer (out-class allocated)
    fComp: pbyte;
    // row length in bytes. fComp is decompressed in blocks of fLineSize
    fLineSize: integer;
    // Position (in bit) of next code to read
    fNextCode: integer;
    // Length of current code
    fDimCode: integer;
    // position of next byte to write in fDecomp
    fWPos: integer;
    fWByte: pbyte;
    //
    fOldCode: integer;
    // String table
    STableSize: integer; // number of elements in STable
    STable: array[0..4096] of TSItem; // max 12 bit
    //
    Aborting: boolean;
    fFillOrder: integer;  // the same as TIFF specifications
  end;
  PTIFLZWDec = ^TTIFLZWDec;



// return next code from fComp (based on fNextCode and fDimCode)
// Note: fDimCode is from 9 to 12
function GetNextCode(plzw: PTIFLZWDec): integer;
begin

  with plzw^ do
  begin
    result := pinteger(@pbytearray(fComp)[ fNextCode shr 3 ])^;

    // 3.0.1, to support FillOrder=2 reading TIFF-LZW files (1/8/2008, 21:17)
    if fFillOrder=2 then
    begin
      ReverseBitsB(pbytearray(@result)[0]);
      ReverseBitsB(pbytearray(@result)[1]);
      ReverseBitsB(pbytearray(@result)[2]);
      ReverseBitsB(pbytearray(@result)[3]);
    end;

    // invert bytes of the double word result
    {$ifdef IEUSEASM}
    asm
      mov EAX, @result
      bswap EAX
      mov @result, EAX
    end;
    {$else}
    result := IESwapDWord(result);
    {$endif}

    result := (result shl (fNextCode and 7)) shr (32 - fDimCode);

    inc(fNextCode, fDimCode);
  end;
end;


// Free table memory
procedure FreeTable(plzw: PTIFLZWDec);
var
  q: integer;
begin
  with plzw^ do
  begin
    for q := 256 to STableSize - 1 do
      if STable[q].Dim > MAXPREALLOC then
        freemem(STable[q].Data);
    STableSize := 0;
  end;
end;


// Init table
procedure InitializeTable(plzw: PTIFLZWDec);
begin
  FreeTable(plzw); // free table if allocated
  plzw^.STableSize := 258;
  plzw^.fDimCode := 9;
end;


// 3.0.3
procedure PutCode(plzw: PTIFLZWDec; code: integer);
var
  ps: pbyte;
  cdim: integer;
begin

  with plzw^ do
  begin

    cdim := STable[code].Dim;

    if fWPos+cdim>=fDecompAllocated then
    begin
      // can happen when decompressed block exceed line size (goes at next line)
      inc(fDecompAllocated, DECOMPBLOCKSIZE);
      ReallocMem(fDecomp, fDecompAllocated);
      fWByte := @(pbytearray(fDecomp)[fWPos]);
    end;

    if code < 256 then
    begin
      fWByte^ := code; inc(fWByte);
      inc(fWPos);
    end
    else
    begin
      with STable[code] do
      begin
        case cdim of
          1:
            begin
              fWByte^ := Data^;
              inc(fWByte);
            end;
          2:
            begin
              ps := pbyte(Data);
              fWByte^ := ps^; inc(ps); inc(fWByte);
              fWByte^ := ps^; inc(fWByte);
            end;
          3:
            begin
              ps := pbyte(Data);
              fWByte^ := ps^; inc(ps); inc(fWByte);
              fWByte^ := ps^; inc(ps); inc(fWByte);
              fWByte^ := ps^; inc(fWByte);
            end;
          4:
            begin
              ps := pbyte(Data);
              fWByte^ := ps^; inc(ps); inc(fWByte);
              fWByte^ := ps^; inc(ps); inc(fWByte);
              fWByte^ := ps^; inc(ps); inc(fWByte);
              fWByte^ := ps^; inc(fWByte);
            end
          else
          begin
            CopyMemory(fWByte, Data, cdim);
            inc(fWByte, cdim);
          end;
        end;
        inc(fWPos, cdim);
      end;
    end;

  end;

end;



// Adds to table OldCode + the first char in Code
// 3.0.3
function AddConcatToTable(plzw: PTIFLZWDec; Code, OldCode: integer): boolean;
var
  sz: integer;
  ps, pd: pbyte;
begin
  with plzw^ do
  begin

    with STable[STableSize] do
    begin
      // copy the whole OldCode data
      if OldCode < 256 then
      begin
        sz := 1;
        Dim := 2;
        Data := @PreAlloc;
        Data^ := OldCode;
      end
      else
      begin
        sz := STable[OldCode].Dim;
        Dim := sz + 1;
        if Dim > MAXPREALLOC then
          getmem(Data, Dim)
        else
          Data := @PreAlloc;

        if sz=1 then
          Data^ := STable[OldCode].Data^
        else
        if sz=2 then
        begin
          ps := STable[OldCode].Data;
          pd := Data;
          pd^ := ps^; inc(pd); inc(ps);
          pd^ := ps^;
        end
        else
        if sz=3 then
        begin
          ps := STable[OldCode].Data;
          pd := Data;
          pd^ := ps^; inc(pd); inc(ps);
          pd^ := ps^; inc(pd); inc(ps);
          pd^ := ps^;
        end
        else
          CopyMemory(Data, STable[OldCode].Data, sz);
      end;

      // copy first byte of Code data
      pbytearray(Data)[sz] := STable[Code].Data^; // first char
    end;

    inc(STableSize);
    case STableSize of
       511: fDimCode := 10;
      1023: fDimCode := 11;
      2047: fDimCode := 12;
    end;

    if STableSize > high(STable) then
      Aborting := True; // table overflow

    result := not Aborting;

  end;

end;


// decompress fLineSize bytes
// 3.0.3
function GetNextline(plzw: PTIFLZWDec): pbyte;
var
  Code: integer;
  OldCode: integer;
begin

  result := nil;

  with plzw^ do
  begin

    OldCode := fOldCode;

    if fWPos > fLineSize then
    begin
      // copy the rest of previous row
      move(pbytearray(fDecomp)[fLineSize], fDecomp^, fWPos - fLineSize);
      fWPos  := fWPos - fLineSize;
      fWByte := @(pbytearray(fDecomp)[fWPos]);
    end
    else
    begin
      fWPos  := 0;
      fWByte := pbyte(fDecomp);
    end;

    while fWPos < fLineSize do
    begin

      Code := GetNextCode(plzw);

      if OldCode = -1 then
        OldCode := Code;

      if Code = CLEARCODE then
      begin
        InitializeTable(plzw);
        Code := GetNextCode(plzw);
        if Code = EOICODE then
          break;
        if Code >= STableSize then
        begin
          // invalid code, must be < STableSize (because we haven't OldCode now)
          Aborting := true;
          exit;
        end;
        PutCode(plzw, Code);
        OldCode := Code;
      end

      else
      if Code = EOICODE then
        break

      else
      if Code < 256 then // just an optimization
      begin
        fWByte^ := code; inc(fWByte); inc(fWPos);
        if not AddConcatToTable(plzw, Code, OldCode) then
          exit; // aborting=true implicit (table overflow)
        OldCode := Code;
      end

      else
      if Code < STableSize then
      begin
        PutCode(plzw, Code);
        if not AddConcatToTable(plzw, Code, OldCode) then
          exit; // aborting=true implicit (table overflow)
        OldCode := Code;
      end

      else
      if Code = STableSize then
      begin
        if not AddConcatToTable(plzw, OldCode, OldCode) then
          exit; // aborting=true implicit (table overflow)
        PutCode(plzw, STableSize - 1);
        OldCode := Code;
      end

      else
      begin
        // invalid Code (do not Abort, sometime images can be successfully loaded also with this error)
        break;
      end;

    end;

    fOldCode := OldCode;

    result := fDecomp;
  end;

end;


// buf = compressed buffer data
// LineSize = length of one line in buf (in bytes)
// 3.0.3
function CreateLzw(buf: pbyte; LineSize: integer; FillOrder: integer): PTIFLZWDec;
var
  i: integer;
begin
  result := allocmem(sizeof(TTIFLZWDec)); // zero filled
  with result^ do
  begin
    fDecompAllocated := LineSize + 1;
    getmem(fDecomp, fDecompAllocated);

    fComp := buf;
    fLineSize := LineSize;
    fNextCode := 0;
    InitializeTable(result);
    fOldCode := -1;
    fWPos := 0;
    fWByte := pbyte(fDecomp);
    Aborting := false;
    fFillOrder := FillOrder;
    for i := 0 to 255 do
    begin
      STable[i].Data := @STable[i].PreAlloc;
      STable[i].Data^ := i;
      STable[i].Dim := 1;
    end;
  end;
end;


procedure DestroyLzw(plzw: PTIFLZWDec);
begin
  FreeTable(plzw);
  freemem(plzw^.fDecomp);
  freemem(plzw);
end;


///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
// CompBuf: compressed buf (of full image)
// LineSize: row size in bytes (this isn't the size of the image)
// Id: is a reference variable (where I store the pointer to TTIFLZWDec object)
// IMPORTANT:
//    - In the first call "Id" is ZERO.
//    - In the nexts call "Id" will be the some returned in the first call
//    - In the last call "CompBuf" will be NIL (you will free your allocated objects)
// rest: the decompressed row (you have not to free it) or nil if error detected

function TIFFLZWDecompress(CompBuf: pbyte; LineSize: integer; var Id: pointer; FillOrder: integer): pbyte;
var
  plzw: PTIFLZWDec;
begin
  result := nil;

  if Id <> nil then
  begin
    plzw := PTIFLZWDec(Id);
    if CompBuf = nil then
    begin
      DestroyLzw(plzw);
      exit; // EXIT POINT
    end;
  end
  else
  begin
    plzw := CreateLzw(CompBuf, LineSize, FillOrder);
    Id := pointer(plzw);
  end;

  if assigned(plzw) then
    result := GetNextLine(plzw);

end;


/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////

// Original C code:
// ppmtogif.c - read a portable pixmap and produce a GIF file
//
// Based on GIFENCOD by David Rowley <mgardi@watdscu.waterloo.edu>.A
// Lempel-Zim compression based on "compress".
//
// Modified by Marcel Wijkstra <wijkstra@fwi.uva.nl>
//
//
// Copyright (C) 1989 by Jef Poskanzer.
//

const
  BITS = 12;
  maxbits = 12;
  maxmaxcode = 1 shl BITS - 1;
  HSIZE = 5003;
  XEOF = -1;

type
  TLZWCompRecord = record
    inpos: integer;
    oStream: TStream;
    CountDown: integer;
    indata: pbyte;
    init_bits: integer;
    n_bits: integer;
    maxcode: integer;
    ClearCode: integer;
    EOFCode: integer;
    free_ent: integer;
    clear_flg: integer;
    a_count: integer;
    htab: array[0..HSIZE - 1] of integer;
    codetab: array[0..HSIZE - 1] of word;
    cur_accum: dword;
    cur_bits: integer;
    accum: array[0..255] of AnsiChar;
    g_init_bits: integer;
    _fcode: integer;
    _i: integer;
    _c: integer;
    _ent: integer;
    _disp: integer;
    _hsize_reg: integer;
    _hshift: integer;
  end;
  PLZWCompRecord = ^TLZWCompRecord;

function NextPixel(var lzwr: TLZWCompRecord): integer;
begin
  with lzwr do
  begin
    if (CountDown = 0) then
    begin
      result := XEOF;
      exit;
    end;
    dec(CountDown);
    result := pbyte(uint64(indata) + inpos)^;
    inc(inpos);
  end;
end;

procedure cl_hash(var lzwr: TLZWCompRecord; hsize: integer);
var
  htab_p: pinteger;
  i, m1: integer;
begin
  htab_p := @(lzwr.htab[0]);
  inc(htab_p, hsize);
  m1 := -1;
  i := hsize - 16;
  repeat
    pinteger(uint64(htab_p) - 4 * 16)^ := m1;
    pinteger(uint64(htab_p) - 4 * 15)^ := m1;
    pinteger(uint64(htab_p) - 4 * 14)^ := m1;
    pinteger(uint64(htab_p) - 4 * 13)^ := m1;
    pinteger(uint64(htab_p) - 4 * 12)^ := m1;
    pinteger(uint64(htab_p) - 4 * 11)^ := m1;
    pinteger(uint64(htab_p) - 4 * 10)^ := m1;
    pinteger(uint64(htab_p) - 4 * 9)^ := m1;
    pinteger(uint64(htab_p) - 4 * 8)^ := m1;
    pinteger(uint64(htab_p) - 4 * 7)^ := m1;
    pinteger(uint64(htab_p) - 4 * 6)^ := m1;
    pinteger(uint64(htab_p) - 4 * 5)^ := m1;
    pinteger(uint64(htab_p) - 4 * 4)^ := m1;
    pinteger(uint64(htab_p) - 4 * 3)^ := m1;
    pinteger(uint64(htab_p) - 4 * 2)^ := m1;
    pinteger(uint64(htab_p) - 4 * 1)^ := m1;
    dec(htab_p, 16);
    dec(i, 16);
  until not (i >= 0);
  inc(i, 16);
  while i > 0 do
  begin
    dec(htab_p);
    htab_p^ := m1;
    dec(i);
  end;
end;

procedure flush_char(var lzwr: TLZWCompRecord);
begin
  with lzwr do
  begin
    if (a_count > 0) then
    begin
      oStream.Write(accum[0], a_count);
      a_count := 0;
    end;
  end;
end;

procedure char_out(var lzwr: TLZWCompRecord; c: integer);
begin
  with lzwr do
  begin
    accum[a_count] := AnsiChar(c);
    inc(a_count);
    if (a_count >= 254) then
      flush_char(lzwr);
  end;
end;

procedure output(var lzwr: TLZWCompRecord; code: integer);
const
  masks: array[0..16] of integer =
   ($0000, $8000, $C000, $E000, $F000,
    $F800, $FC00, $FE00, $FF00, $FF80,
    $FFC0, $FFE0, $FFF0, $FFF8, $FFFC,
    $FFFE, $FFFF);
begin
  with lzwr do
  begin
    cur_accum := cur_accum and (((1 shl cur_bits) - 1) shl (32 - cur_bits));
    if (cur_bits > 0) then
      cur_accum := cur_accum or dword(code shl (32 - n_bits - cur_bits))
    else
      cur_accum := code shl dword(32 - n_bits);
    inc(cur_bits, n_bits);
    while (cur_bits >= 8) do
    begin
      char_out(lzwr, dword(cur_accum and $FF000000) shr 24);
      cur_accum := cur_accum shl 8;
      dec(cur_bits, 8);
    end;
    if (free_ent > maxcode - 1) or (clear_flg <> 0) then
    begin
      if (clear_flg <> 0) then
      begin
        n_bits := g_init_bits;
        maxcode := 1 shl n_bits - 1;
        clear_flg := 0;
      end
      else
      begin
        inc(n_bits);
        if (n_bits = maxbits) then
          maxcode := maxmaxcode
        else
          maxcode := 1 shl n_bits - 1;
      end;
    end;
    if (code = EOFCode) then
    begin
      while (cur_bits > 0) do
      begin
        char_out(lzwr, dword(cur_accum and $FF000000) shr 24);
        cur_accum := cur_accum shl 8;
        dec(cur_bits, 8);
      end;
      flush_char(lzwr);
    end;
  end;
end;

procedure cl_block(var lzwr: TLZWCompRecord);
begin
  with lzwr do
  begin
    cl_hash(lzwr, integer(hsize));
    free_ent := ClearCode + 2;
    clear_flg := 1;
    output(lzwr, integer(ClearCode));
  end;
end;

// op=0 initialize/encode
// op=1 encode
// op=2 finalize

procedure lzwcompress(var lzwr: TLZWCompRecord; op: integer);
label
  probe, nomatch;
begin
  with lzwr do
  begin
    if op = 0 then
    begin
      // initialize
      g_init_bits := init_bits;
      clear_flg := 0;
      n_bits := init_bits;
      maxcode := 1 shl n_bits - 1;
      ClearCode := (1 shl (lzwr.init_bits - 1));
      EOFCode := ClearCode + 1;
      free_ent := ClearCode + 2;
      a_count := 0;
      _ent := NextPixel(lzwr);
      _hshift := 0;
      _fcode := hsize;
      while _fcode < 65536 do
      begin
        inc(_hshift);
        _fcode := _fcode * 2;
      end;
      _hshift := 8 - _hshift;
      _hsize_reg := hsize;
      cl_hash(lzwr, _hsize_reg);
      output(lzwr, ClearCode);
    end;
    if (op = 0) or (op = 1) then
    begin
      // encoding
      while (true) do
      begin
        _c := NextPixel(lzwr);
        if _c = XEOF then
          break;
        _fcode := integer(((integer(_c) shl maxbits) + _ent));
        _i := ((integer(_c) shl _hshift) xor _ent);
        if (lzwr.htab[_i] = _fcode) then
        begin
          _ent := codetab[_i];
          continue;
        end
        else
        if (integer(htab[_i]) < 0) then
          goto nomatch;
        _disp := _hsize_reg - _i;
        if (_i = 0) then
          _disp := 1;
        probe:
        dec(_i, _disp);
        if (_i < 0) then
          inc(_i, _hsize_reg);
        if (htab[_i] = _fcode) then
        begin
          _ent := codetab[_i];
          continue;
        end;
        if (integer(htab[_i]) > 0) then
          goto probe;
        nomatch:
        output(lzwr, integer(_ent));
        _ent := _c;
        if (free_ent < maxmaxcode - 1) then
        begin
          codetab[_i] := free_ent;
          inc(free_ent);
          htab[_i] := _fcode;
        end
        else
          cl_block(lzwr);
      end;
    end
    else
    if op = 2 then
    begin
      // finalize
      output(lzwr, integer(_ent));
      output(lzwr, integer(EOFCode));
    end;
  end;
end;

// indata: decompressed data
// inputlen: indata length (in bytes)
// outstream: compressed data
// Id: is a reference variable (where I store the pointer to TLZWCompRecord object)
// IMPORTANT:
//    - In the first call "Id" is ZERO.
//    - In the nexts call "Id" will be the some returned in the first call
//    - In the last call "indata" will be NIL (you will free your allocated objects)

procedure TIFFLZWCompress(indata: pbyte; inputlen: integer; outstream: TStream; var id: pointer);
var
  lzwr: PLZWCompRecord;
begin
  if id = nil then
  begin
    // initialize/encode
    new(lzwr);
    lzwr^.indata := pbyte(indata);
    lzwr^.oStream := outstream;
    lzwr^.cur_accum := 0;
    lzwr^.cur_bits := 0;
    lzwr^.CountDown := inputlen;
    lzwr^.free_ent := 0;
    lzwr^.inpos := 0;
    lzwr^.init_bits := 8 + 1;
    lzwcompress(lzwr^, 0);
    id := pointer(lzwr);
  end
  else
  if id <> nil then
  begin
    lzwr := PLZWCompRecord(id);
    if indata = nil then
    begin
      // finalize
      lzwcompress(lzwr^, 2);
      dispose(lzwr)
    end
    else
    begin
      // continue encoding
      lzwr^.CountDown := inputlen;
      lzwr^.indata := pbyte(indata);
      lzwr^.inpos := 0;
      lzwcompress(lzwr^, 1);
    end;
  end;
end;



end.
