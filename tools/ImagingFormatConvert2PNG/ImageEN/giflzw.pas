(*
File version 1003
*)

unit giflzw;

{$R-}
{$Q-}

// Example of GIF LZW, NONLZW compression and LZW decompression plug-in for ImageEn

{$I ie.inc}

interface

uses Windows, Graphics, classes, sysutils, hyieutils, hyiedefs;

// Compression
procedure GIFLZWCompress(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar; BitsPerPixel: integer);
procedure GIFNONLZWCompress(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar; BitsPerPixel: integer);

// Decompression
procedure GIFLZWDecompress(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar);

implementation

{$R-}

/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
// Standard LZW Decompression

{$IFOPT R+}{$DEFINE RangeCheck}{$ENDIF}{$R-}

type

  TDecodeData = record
    ReadPos: integer;
    DataLen: integer;
    BitsLeft: Integer;
    CurrByte: Longint;
    PosY: Integer;
    InterlacePass: Integer;
    Step: integer;
    LZWCodeSize: Byte;
    CurrCodeSize: Integer;
    ClearCode: Integer;
    EndingCode: Integer;
    HighCode: Word;
  end;

  TCodeTable = record
    Suffix, Prefix: array[1..4096] of Word;
    CodeSize: Byte;
    TableFull: Boolean;
    Firstprt, Nextprt: Word;
  end;

function GetNL(LineNo, Height: Integer; var InterlacePass: Integer; var Step: integer): Integer;
begin
  result := LineNo;
  Inc(result, step);
  if (result >= height) then
    repeat
      if (Interlacepass > 0) then
        step := step shr 1;
      Inc(Interlacepass);
      result := step shr 1;
    until (result < height);
end;

function InitCompressionStream(InitLZWCodeSize: Byte; var DecData: TDecodeData): boolean;
begin
  result := true;
  with DecData do
  begin
    LZWCodeSize := InitLZWCodeSize;
    if not (LZWCodeSize in [2..9]) then
    begin
      result := false;
      exit;
    end;
    CurrCodeSize := succ(LZWCodeSize);
    ClearCode := 1 shl LZWCodeSize;
    EndingCode := succ(ClearCode);
    HighCode := pred(ClearCode);
    BitsLeft := 0;
    PosY := 0;
    InterlacePass := 0;
    Step := 8;
  end;
end;

function NextCode(var ba: TIEByteArray; var DecData: TDecodeData): word;
const
  CodeMsk: array[0..12] of Word = (
    0, $0001, $0003, $0007, $000F,
    $001F, $003F, $007F, $00FF,
    $01FF, $03FF, $07FF, $0FFF);
var
  LongResult: Longint;
begin
  with DecData do
  begin
    if BitsLeft = 0 then
    begin
      CurrByte := ba.Data^[ReadPos];
      inc(ReadPos);
      BitsLeft := 8;
    end;
    LongResult := CurrByte shr (8 - BitsLeft);
    while CurrCodeSize > BitsLeft do
    begin
      CurrByte := ba.Data^[ReadPos];
      inc(ReadPos);
      LongResult := LongResult or (CurrByte shl BitsLeft);
      inc(BitsLeft, 8);
    end;
    dec(BitsLeft, CurrCodeSize);
    Result := LongResult and CodeMsk[CurrCodeSize];
  end;
end;


procedure GIFLZWDecompress(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar);
var
  SP: integer;
  DecodeDat: array[0..4095] of byte;
  DecData: TDecodeData;
  Prefix: array[0..4095] of integer;
  Suffix: array[0..4095] of integer;
  CurrBuf: word;
  px: pbyte;
  LZWCodeSize: byte;
  CompData: TIEByteArray;
  procedure DecodeCode(var Code: word);
  begin
    while Code > DecData.HighCode do
    begin
      DecodeDat[SP] := Suffix[Code];
      inc(SP);
      Code := Prefix[Code];
    end;
    DecodeDat[SP] := Code;
    Inc(SP);
  end;
  procedure GetDat;
  begin
    with DecData do
      while SP > 0 do
      begin
        dec(SP);
        if posy < height then
          px^ := decodedat[sp];
        inc(px);
        inc(CurrBuf);
        if CurrBuf > Width then
        begin
          if not InterLaced then
            Inc(PosY)
          else
            PosY := GetNL(PosY, Height, InterlacePass, Step);
          CurrBuf := 1;
          px := pbyte(uint64(fData) + PosY * Width + CurrBuf - 1);
        end;
      end;
  end;
  procedure CheckprtValue(var prt, Topprt: Word; var MaxVal: Boolean);
  begin
    if prt >= Topprt then
    begin
      if DecData.CurrCodeSize < 12 then
      begin
        Topprt := Topprt shl 1;
        inc(DecData.CurrCodeSize)
      end
      else
        MaxVal := True;
    end;
  end;

var
  TempOldCode, OldCode: word;
  Code, C: word;
  MaxVal: boolean;
  prt: Word;
  Topprt: Word;
  b, v: byte;
  spos: int64;
begin
  spos := Stream.Position;

  Stream.Read(LZWCodeSize, 1);
  px := pbyte(fdata);
  if not InitCompressionStream(LZWCodeSize, DecData) then
  begin
    Stream.Position := spos; // reset position indicates an error
    exit;
  end;
  DecData.DataLen := 0;

  CompData := TIEByteArray.Create(Stream.Size-Stream.Position+1);

  try

    repeat
      if (Stream.Read(b, 1) = 0) then
        break;
      if b = 0 then
        break;
      v := CompData.AppendFromStream(Stream, b);
      DecData.DataLen := DecData.DataLen + v;
    until false;

    DecData.ReadPos := 0;
    OldCode := 0;
    SP := 0;
    CurrBuf := 1;
    MaxVal := False;
    if DecData.ReadPos >= DecData.DataLen then
      exit;
    C := NextCode(CompData, DecData);
    while C <> DecData.EndingCode do
    begin
      if C = DecData.ClearCode then
      begin
        DecData.CurrCodeSize := DecData.LZWCodeSize + 1;
        prt := DecData.EndingCode + 1;
        Topprt := 1 shl DecData.CurrCodeSize;
        while C = DecData.ClearCode do
        begin
          if DecData.ReadPos >= DecData.DataLen then
            exit;
          C := NextCode(CompData, DecData);
        end;
        if C = DecData.EndingCode then
        begin
          if DecData.ReadPos < DecData.DataLen then
            Stream.Position := spos; // reset position indicates an error
          exit;
        end;
        if C >= prt then
          C := 0;
        OldCode := C;
        DecodeDat[SP] := C;
        inc(SP);
      end
      else
      begin
        Code := C;
        if Code < prt then
        begin
          DecodeCode(Code);
          if prt <= Topprt then
          begin
            Suffix[prt] := Code;
            Prefix[prt] := OldCode;
            inc(prt);
            CheckprtValue(prt, Topprt, MaxVal);
            OldCode := C;
          end;
        end
        else
        begin
          if Code <> prt then
          begin
            Stream.Position := spos; // reset position indicates an error
            exit;
          end;
          TempOldCode := OldCode;
          while OldCode > DecData.HighCode do
          begin
            DecodeDat[SP] := Suffix[OldCode];
            OldCode := Prefix[OldCode];
          end;
          DecodeDat[SP] := OldCode;
          if prt <= Topprt then
          begin
            Suffix[prt] := OldCode;
            Prefix[prt] := TempOldCode;
            inc(prt);
            CheckprtValue(prt, Topprt, MaxVal);
          end;
          DecodeCode(Code);
          OldCode := C;
        end;
      end;
      GetDat;
      if DecData.ReadPos >= DecData.DataLen then
        exit;
      C := NextCode(CompData, DecData);
      if (MaxVal = True) and (C <> DecData.ClearCode) then
      begin
        if DecData.ReadPos < DecData.DataLen then
          Stream.Position := spos; // reset position indicates an error
        exit;
      end;
      MaxVal := False;
    end;

  finally
    CompData.Free();
  end;
end;



/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////

(*-----------------------------------------------------------------------
 *
 * miGIF Compression - mouse and ivo's GIF-compatible compression
 *
 *          -run length encoding compression routines-
 *
 * Copyright (C) 1998 Hutchison Avenue Software Corporation
 *               http://www.hasc.com
 *               info@hasc.com
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  This software is provided "AS IS." The Hutchison Avenue
 * Software Corporation disclaims all warranties, either express or implied,
 * including but not limited to implied warranties of merchantability and
 * fitness for a particular purpose, with respect to this code and accompanying
 * documentation.
 *
 * The miGIF compression routines do not, strictly speaking, generate files
 * conforming to the GIF spec, since the image data is not LZW-compressed
 * (this is the point: in order to avoid transgression of the Unisys patent
 * on the LZW algorithm.)  However, miGIF generates data streams that any
 * reasonably sane LZW decompresser will decompress to what we want.
 *
 * miGIF compression uses run length encoding. It compresses horizontal runs
 * of pixels of the same color. This type of compression gives good results
 * on images with many runs, for example images with lines, text and solid
 * shapes on a solid-colored background. It gives little or no compression
 * on images with few runs, for example digital or scanned photos.
 *
 *                               der Mouse
 *                      mouse@rodents.montreal.qc.ca
 *            7D C8 61 52 5D E7 2D 39  4E F1 31 3E E8 B3 27 4B
 *
 *                             ivo@hasc.com
 *
 * The Graphics Interchange Format(c) is the Copyright property of
 * CompuServe Incorporated.  GIF(sm) is a Service Mark property of
 * CompuServe Incorporated.
 *
 *
*)

type
  varblk = record
    rl_pixel: integer;
    rl_basecode: integer;
    rl_count: integer;
    rl_table_pixel: integer;
    rl_table_max: integer;
    just_cleared: integer;
    out_bits: integer;
    out_bits_init: integer;
    out_count: integer;
    out_bump: integer;
    out_bump_init: integer;
    out_clear: integer;
    out_clear_init: integer;
    max_ocodes: integer;
    code_clear: integer;
    code_eof: integer;
    obuf: dword;
    obits: integer;
    ofile: TStream;
    oblock: array[0..255] of byte;
    oblen: integer;
    //
    Data, Datap: pbyte;
    // interlaced
    fInterlaced: boolean;
    Pass, wlen, y, x: integer;
    iwidth, iheight: integer;
  end;

  /////////////////////////////////////////////////////////////////////////////////////

function isqrt(x: dword): dword;
var
  r: dword;
  v: dword;
begin
  if (x < 2) then
  begin
    result := x;
    exit;
  end;
  v := x;
  r := 1;
  while v <> 0 do
  begin
    v := v shr 2;
    r := r shl 1;
  end;
  repeat
    v := trunc(((x / r) + r) / 2);
    if ((v = r) or (v = r + 1)) then
    begin
      result := r;
      exit;
    end;
    r := v;
  until false;
end;


procedure did_clear(var vb: varblk);
begin
  with vb do
  begin
    out_bits := out_bits_init;
    out_bump := out_bump_init;
    out_clear := out_clear_init;
    out_count := 0;
    rl_table_max := 0;
    just_cleared := 1;
  end;
end;


procedure write_block(var vb: varblk);
begin
  with vb do
  begin
    ofile.Write(oblen, 1);
    ofile.Write(oblock[0], oblen);
    oblen := 0;
  end;
end;


procedure block_out(var vb: varblk; c: byte);
begin
  with vb do
  begin
    oblock[oblen] := c;
    inc(oblen);
    if (oblen >= 255) then
      write_block(vb);
  end;
end;


procedure goutput(var vb: varblk; val: integer);
begin
{$WARNINGS OFF}
  with vb do
  begin
    obuf := obuf or (val shl obits);
    inc(obits, out_bits);
    while (obits >= 8) do
    begin
      block_out(vb, obuf and $FF);
      obuf := obuf shr 8;
      dec(obits, 8);
    end;
  end;
{$WARNINGS ON}
end;


procedure output_plain(var vb: varblk; c: integer);
begin
  with vb do
  begin
    just_cleared := 0;
    goutput(vb, c);
    inc(out_count);
    if (out_count >= out_bump) then
    begin
      inc(out_bits);
      inc(out_bump, 1 shl (out_bits - 1));
    end;
    if (out_count >= out_clear) then
    begin
      goutput(vb, code_clear);
      did_clear(vb);
    end;
  end;
end;


procedure reset_out_clear(var vb: varblk);
begin
  with vb do
  begin
    out_clear := out_clear_init;
    if (out_count >= out_clear) then
    begin
      goutput(vb, code_clear);
      did_clear(vb);
    end;
  end;
end;


procedure rl_flush_fromclear(var vb: varblk; count: integer);
var
  n: integer;
begin
  with vb do
  begin
    out_clear := max_ocodes;
    rl_table_pixel := rl_pixel;
    n := 1;
    while (count > 0) do
    begin
      if (n = 1) then
      begin
        rl_table_max := 1;
        output_plain(vb, rl_pixel);
        dec(count);
      end
      else
      if (count >= n) then
      begin
        rl_table_max := n;
        output_plain(vb, rl_basecode + n - 2);
        dec(count, n);
      end
      else
      if (count = 1) then
      begin
        inc(rl_table_max);
        output_plain(vb, rl_pixel);
        count := 0;
      end
      else
      begin
        inc(rl_table_max);
        output_plain(vb, rl_basecode + count - 2);
        count := 0;
      end;
      if (out_count = 0) then
        n := 1
      else
        inc(n);
    end;
    reset_out_clear(vb);
  end;
end;


function computetc(count: dword; nrepcodes: dword): dword;
var
  perrep: dword;
  n: dword;
begin
  result := 0;
  perrep := trunc((nrepcodes * (nrepcodes + 1)) / 2);
  while (count >= perrep) do
  begin
    inc(result, nrepcodes);
    dec(count, perrep);
  end;
  if (count > 0) then
  begin
    n := isqrt(count);
    while ((n * (n + 1)) >= 2 * count) do
      dec(n);
    while ((n * (n + 1)) < 2 * count) do
      inc(n);
    inc(result, n);
  end;
end;


procedure rl_flush_clearorrep(var vb: varblk; count: integer);
var
  withclr: integer;
begin
  with vb do
  begin
    withclr := 1 + computetc(count, max_ocodes);
    if (withclr < count) then
    begin
      goutput(vb, code_clear);
      did_clear(vb);
      rl_flush_fromclear(vb, count);
    end
    else
    begin
      while count > 0 do
      begin
        output_plain(vb, rl_pixel);
        dec(count);
      end;
    end;
  end;
end;


procedure rl_flush_withtable(var vb: varblk; count: integer);
var
  repmax: integer;
  repleft: integer;
  leftover: integer;
begin
{$WARNINGS OFF}
  with vb do
  begin
    repmax := trunc(count / rl_table_max);
    leftover := count mod rl_table_max;
    if leftover <> 0 then
      repleft := 1
    else
      repleft := 0;
    if (out_count + repmax + repleft > max_ocodes) then
    begin
      repmax := max_ocodes - out_count;
      leftover := count - (repmax * rl_table_max);
      repleft := 1 + computetc(leftover, max_ocodes);
    end;
    if (1 + computetc(count, max_ocodes) < repmax + repleft) then
    begin
      goutput(vb, code_clear);
      did_clear(vb);
      rl_flush_fromclear(vb, count);
      exit;
    end;
    out_clear := max_ocodes;
    while repmax > 0 do
    begin
      output_plain(vb, rl_basecode + rl_table_max - 2);
      dec(repmax);
    end;
    if (leftover <> 0) then
    begin
      if (just_cleared <> 0) then
      begin
        rl_flush_fromclear(vb, leftover);
      end
      else
      if (leftover = 1) then
      begin
        output_plain(vb, rl_pixel);
      end
      else
      begin
        output_plain(vb, rl_basecode + leftover - 2);
      end;
    end;
    reset_out_clear(vb);
  end;
{$WARNINGS ON}
end;


procedure rl_flush(var vb: varblk);
begin
  with vb do
  begin
    if (rl_count = 1) then
    begin
      output_plain(vb, rl_pixel);
      rl_count := 0;
      exit;
    end;
    if (just_cleared <> 0) then
    begin
      rl_flush_fromclear(vb, rl_count);
    end
    else
    if ((rl_table_max < 2) or (rl_table_pixel <> rl_pixel)) then
    begin
      rl_flush_clearorrep(vb, rl_count);
    end
    else
    begin
      rl_flush_withtable(vb, rl_count);
    end;
    rl_count := 0;
  end;
end;


function GetNextPixel(var vb: varblk): integer;
begin
  with vb do
  begin
    dec(x);
    if (x <= 0) then
    begin
      x := iwidth;
      case (Pass) of
        0:
          begin
            inc(y, 8);
            if (y >= iheight) then
            begin
              inc(pass);
              y := 4;
            end;
          end;
        1:
          begin
            inc(y, 8);
            if (y >= iheight) then
            begin
              inc(pass);
              y := 2;
            end;
          end;
        2:
          begin
            inc(y, 4);
            if (y >= iheight) then
            begin
              inc(pass);
              Y := 1;
            end;
          end;
        3:
          inc(y, 2);
      end;
      Datap := pbyte(uint64(Data) + (y * iWidth));
    end;
    result := Datap^;
    inc(Datap);
  end;
end;


procedure GIFNONLZWCompress(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar; BitsPerPixel: integer);
var
  b: byte;
  c, bufdim: integer;
  vb: varblk;
  bufpos: integer;
begin
  bufdim := Height * Width; // only for 8 bitXpixel
  if BitsPerPixel = 1 then
    BitsPerPixel := 2;
  b := BitsPerPixel;
  Stream.Write(b, 1);
  inc(BitsPerPixel);

  bufpos := 0;
  with vb do
  begin
    ofile := Stream;
    obuf := 0;
    obits := 0;
    oblen := 0;
    code_clear := 1 shl (BitsPerPixel - 1);
    code_eof := code_clear + 1;
    rl_basecode := code_eof + 1;
    out_bump_init := (1 shl (BitsPerPixel - 1)) - 1;
    if (BitsPerPixel <= 3) then
      out_clear_init := 9
    else
      out_clear_init := out_bump_init - 1;
    out_bits_init := BitsPerPixel;
    max_ocodes := $1000 - ((1 shl (out_bits_init - 1)) + 3);
    did_clear(vb);
    goutput(vb, code_clear);
    rl_count := 0;
    y := 0;
    Pass := 0;
    iwidth := width;
    iheight := height;
    if Interlaced then
      x := iwidth
    else
      x := bufdim;
    Data := pbyte(fData);
    Datap := Data;
    fInterlaced := Interlaced;
    repeat
      if bufpos < bufdim then
        c := GetNextPixel(vb)
      else
        c := -1;
      if (rl_count > 0) and (c <> rl_pixel) then
        rl_flush(vb);
      if c = -1 then
        break;
      if (rl_pixel = c) then
        inc(rl_count)
      else
      begin
        rl_pixel := c;
        rl_count := 1;
      end;
      inc(bufpos);
    until false;
    goutput(vb, code_eof);
    if (obits > 0) then
      block_out(vb, obuf);
    if (oblen > 0) then
      write_block(vb);
  end;
  //
  c := 0;
  Stream.Write(c, 1);
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
  maxmaxcode = 1 shl BITS;
  HSIZE = 5003;
  XEOF = -1;

type
  TLZWCompRecord = record
    Width, Height: integer;
    curx, cury: integer;
    px: pbyte;
    CountDown: integer;
    Pass: integer;
    Interlace: boolean;
    data: pbyte;
    init_bits: integer;
    n_bits: integer;
    maxcode: integer;
    ClearCode: integer;
    EOFCode: integer;
    free_ent: integer;
    offset: integer;
    in_count: integer;
    out_count: integer;
    clear_flg: integer;
    a_count: integer;
    htab: array[0..HSIZE - 1] of integer;
    codetab: array[0..HSIZE - 1] of word;
    cur_accum: integer;
    cur_bits: integer;
    accum: array[0..255] of AnsiChar;
    os: TStream;
    g_init_bits: integer;
  end;
  PLZWCompRecord = ^TLZWCompRecord;

procedure BumpPixel(var lzwr: TLZWCompRecord);
begin
  with lzwr do
  begin
    inc(curx);
    inc(px);
    if curx = Width then
    begin
      curx := 0;
      if not Interlace then
      begin
        inc(cury);
      end
      else
      begin
        case Pass of
          0:
            begin
              inc(cury, 8);
              if (cury >= Height) then
              begin
                inc(Pass);
                cury := 4;
              end;
            end;
          1:
            begin
              inc(cury, 8);
              if (cury >= Height) then
              begin
                inc(Pass);
                cury := 2;
              end;
            end;
          2:
            begin
              inc(cury, 4);
              if (cury >= Height) then
              begin
                inc(Pass);
                cury := 1;
              end;
            end;
          3:
            begin
              inc(cury, 2);
            end;
        end;
      end;
      px := pbyte(uint64(data) + cury * Width);
    end;
  end;
end;

function GIFNextPixel(var lzwr: TLZWCompRecord): integer;
begin
  with lzwr do
    if (CountDown = 0) then
      result := XEOF
    else
    begin
      dec(CountDown);
      result := px^;
      BumpPixel(lzwr);
    end;
end;

procedure cl_hash(var lzwr: TLZWCompRecord; hsize: integer);
var
  htab_p: pinteger;
  i: integer;
begin
  htab_p := @(lzwr.htab[0]);
  inc(htab_p, hsize);
  i := hsize - 16;
  repeat
    pinteger(uint64(htab_p) - 64)^ := -1;
    pinteger(uint64(htab_p) - 60)^ := -1;
    pinteger(uint64(htab_p) - 56)^ := -1;
    pinteger(uint64(htab_p) - 52)^ := -1;
    pinteger(uint64(htab_p) - 48)^ := -1;
    pinteger(uint64(htab_p) - 44)^ := -1;
    pinteger(uint64(htab_p) - 40)^ := -1;
    pinteger(uint64(htab_p) - 36)^ := -1;
    pinteger(uint64(htab_p) - 32)^ := -1;
    pinteger(uint64(htab_p) - 28)^ := -1;
    pinteger(uint64(htab_p) - 24)^ := -1;
    pinteger(uint64(htab_p) - 20)^ := -1;
    pinteger(uint64(htab_p) - 16)^ := -1;
    pinteger(uint64(htab_p) - 12)^ := -1;
    pinteger(uint64(htab_p) - 8)^ := -1;
    pinteger(uint64(htab_p) - 4)^ := -1;
    dec(htab_p, 16);
    dec(i, 16);
  until not (i >= 0);
  inc(i, 16);
  while i > 0 do
  begin
    dec(htab_p);
    htab_p^ := -1;
    dec(i);
  end;
end;

procedure flush_char(var lzwr: TLZWCompRecord);
var
  bb: byte;
begin
  with lzwr do
  begin
    if (a_count > 0) then
    begin
      bb := a_count;
      os.Write(bb, 1);
      os.Write(accum[0], a_count);
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
   ($0000, $0001, $0003, $0007, $000F,
    $001F, $003F, $007F, $00FF,
    $01FF, $03FF, $07FF, $0FFF,
    $1FFF, $3FFF, $7FFF, $FFFF);
begin
  with lzwr do
  begin
    if (cur_bits > 0) then
      cur_accum := (cur_accum and masks[cur_bits]) or (code shl cur_bits)
    else
      cur_accum := code;
    inc(cur_bits, n_bits);
    while (cur_bits >= 8) do
    begin
      char_out(lzwr, integer(cur_accum and $FF));
      cur_accum := cur_accum shr 8;
      dec(cur_bits, 8);
    end;
    if (free_ent > maxcode) or (clear_flg <> 0) then
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
        char_out(lzwr, integer(cur_accum and $FF));
        cur_accum := cur_accum shr 8;
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

procedure lzwcompress(var lzwr: TLZWCompRecord);
label
  probe, nomatch;
var
  fcode: integer;
  i: integer;
  c: integer;
  ent: integer;
  disp: integer;
  hsize_reg: integer;
  hshift: integer;
begin
  with lzwr do
  begin
    g_init_bits := init_bits;
    offset := 0;
    out_count := 0;
    clear_flg := 0;
    in_count := 1;
    n_bits := init_bits;
    maxcode := 1 shl n_bits - 1;
    ClearCode := (1 shl (lzwr.init_bits - 1));
    EOFCode := ClearCode + 1;
    free_ent := ClearCode + 2;
    a_count := 0;
    ent := GIFNextPixel(lzwr);
    hshift := 0;
    fcode := hsize;
    while fcode < 65536 do
    begin
      inc(hshift);
      fcode := fcode * 2;
    end;
    hshift := 8 - hshift;
    hsize_reg := hsize;
    cl_hash(lzwr, hsize_reg);
    output(lzwr, ClearCode);
    while (true) do
    begin
      c := GIFNextPixel(lzwr);
      if c = XEOF then
        break;
      inc(in_count);
      fcode := (c shl maxbits) + ent;
      i := (c shl hshift) xor ent;
      if (lzwr.htab[i] = fcode) then
      begin
        ent := codetab[i];
        continue;
      end
      else
      if (integer(htab[i]) < 0) then
        goto nomatch;
      if (i = 0) then
        disp := 1
      else
        disp := hsize_reg - i;
      probe:
      dec(i, disp);
      if (i < 0) then
        inc(i, hsize_reg);
      if (htab[i] = fcode) then
      begin
        ent := codetab[i];
        continue;
      end;
      if (integer(htab[i]) > 0) then
        goto probe;
      nomatch:
      output(lzwr, integer(ent));
      inc(out_count);
      ent := c;
      if (free_ent < maxmaxcode) then
      begin
        codetab[i] := free_ent;
        inc(free_ent);
        htab[i] := fcode;
      end
      else
        cl_block(lzwr);
    end;
    output(lzwr, integer(ent));
    inc(out_count);
    output(lzwr, integer(EOFCode));
  end;
end;

procedure GIFLZWCompress(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar; BitsPerPixel: integer);
var
  lzwr: PLZWCompRecord;
  InitCodeSize: integer;
  bb: byte;
begin
  new(lzwr);
  try
    lzwr^.Interlace := Interlaced;
    lzwr^.Width := Width;
    lzwr^.Height := Height;
    lzwr^.data := pbyte(FData);
    lzwr^.cur_accum := 0;
    lzwr^.cur_bits := 0;
    lzwr^.CountDown := Width * Height;
    lzwr^.Pass := 0;
    lzwr^.free_ent := 0;
    if (BitsPerPixel <= 1) then
      InitCodeSize := 2
    else
      InitCodeSize := BitsPerPixel;
    lzwr^.curx := 0;
    lzwr^.cury := 0;
    lzwr^.px := pbyte(fdata);

    bb := InitCodeSize;
    Stream.Write(bb, 1);
    lzwr^.init_bits := InitCodeSize + 1;
    lzwr^.os := Stream;
    lzwcompress(lzwr^);
    bb := 0;
    Stream.Write(bb, 1);

  finally
    dispose(lzwr);
  end;
end;

{$IFDEF RangeCheck}{$R+}{$UNDEF RangeCheck}{$ENDIF}

end.
