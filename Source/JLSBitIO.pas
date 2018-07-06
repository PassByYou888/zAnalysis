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
unit JLSBitIO;

{$INCLUDE zDefine.inc}

interface

uses
  CoreClasses, JLSGlobal;

const
  BufSize    = ((16 * 1024) - NEGBUFFSIZE); { Size of input BYTE buffer }
  BITBUFSIZE = (8 * SizeOf(Cardinal));

type
  TJLSBitIO = class
    FInputStream: TCoreClassStream;
    FOutputStream: TCoreClassStream;
    zeroLUT: packed array [0 .. 255] of Int; { table to find out number of leading zeros }
    { BIT I/O variables }
    reg: Cardinal; // ulong;     { BIT buffer for input/output }
    {
      'buff' is defined as 'rawbuff+4' in bitio.h, so that buff[-4]..buff[-1]
      are well defined. Those locations are used to "return" data to
      the byte buffer when flushing the input bit buffer .
    }

    FP: Int;          { index into byte buffer }
    truebufsize: Int; { true size of byte buffer ( <= BUFSIZE) }
    foundeof: Boolean;

    Bits: Int; { number of bits free in bit buffer (on output) }
    { (number of bits free)-8 in bit buffer (on input) }

    negbuff: packed array [0 .. BufSize + NEGBUFFSIZE - 1] of Byte; { byte I/O buffer, allowing for 4 "negative" locations }
    constructor Create(AInputStream: TCoreClassStream; AOutputStream: TCoreClassStream);

    procedure bitoinit;
    procedure bufiinit;
    procedure bitoflush;
    procedure bitiflush;
    function buff: PByteArray;
    function fillinbuff(fil: TCoreClassStream): Byte;
    procedure myputc(C: Byte);
    procedure FlushBuff;
    procedure fclose(fil: TCoreClassStream);
    function ftell(fil: TCoreClassStream): Int64;
    function mygetc: Int;
    procedure myungetc(X: Byte; fil: TCoreClassStream);

    procedure createzeroLUT;
    procedure bitiinit;
    procedure fillbuffer(no: Integer);
    procedure putbits(X: Int; n: Int);
    procedure put_ones(n: Int);
    procedure put_zeros(n: Int);
  end;

implementation

procedure TJLSBitIO.fclose(fil: TCoreClassStream);
begin

end;

function TJLSBitIO.ftell(fil: TCoreClassStream): Int64;
begin
  Result := fil.Position;
end;

{ creates the bit counting look-up table. }
procedure TJLSBitIO.createzeroLUT;
var
  i, J, k, L: Int;
begin
  J := 1;
  k := 1;
  L := 8;
  for i := 0 to pred(256) do
    begin
      zeroLUT[i] := L;
      Dec(k);
      if (k = 0) then
        begin
          k := J;
          Dec(L);
          J := J * 2;
        end;
    end;
end;

{ loads more data in the input buffer (inline code ) }
procedure TJLSBitIO.fillbuffer(no: Integer);
var
  X: Byte;
begin
  Assert(no + Bits <= 24);
  reg := reg shl no;
  Bits := Bits + no;

  while (Bits >= 0) do
    begin
      X := Byte(mygetc);
      if (X = $FF) then
        begin
          if (Bits < 8) then
            begin
              myungetc($FF, FInputStream);
              Break;
            end
          else begin

              X := Byte(mygetc);

              if (not IsTrue(X and $80)) then { non-marker: drop 0 }
                begin
                  reg := reg or ($FF shl Bits) or ((X and $7F) shl (Bits - 7));
                  Bits := Bits - 15;
                end
              else begin
                  { marker: hope we know what we're doing }
                  { the "1" bit following ff is NOT dropped }
                  reg := reg or ($FF shl Bits) or (X shl (Bits - 8));
                  Bits := Bits - 16;
                end;

              Continue;
            end;
        end;

      reg := reg or (X shl Bits);
      Bits := Bits - 8;
    end;
end;

{ Initializes the bit input routines }
procedure TJLSBitIO.bitiinit;
begin
  Bits := 0;
  reg := 0;
  fillbuffer(24);
end;

function TJLSBitIO.buff: PByteArray;
begin
  Result := @negbuff[NEGBUFFSIZE];
end;

function TJLSBitIO.fillinbuff(fil: TCoreClassStream): Byte;
var
  i: Int;
begin
  { remember 4 last bytes of current buffer (for "undo") }
  for i := 0 to pred(NEGBUFFSIZE) do
    begin
      negbuff[i] := negbuff[Integer(FP + i)];
    end;

  truebufsize := fil.read(buff[0], BufSize);

  if (truebufsize < BufSize) then
    begin
      if (truebufsize <= 0) then
        begin
          if (foundeof) then
            begin
              { second attempt to read past EOF }
              // fprintf(stderr,"*** Premature EOF in compressed file\n");
              Result := 10;
              Exit;
            end
          else
            begin
              { One attempt to read past EOF is OK }
              foundeof := True;
            end;
        end;
      { fill buffer with zeros }
      FillChar(buff^[truebufsize], BufSize - truebufsize, 0);
    end;

  FP := 1;
  Result := buff^[0];
end;

function TJLSBitIO.mygetc: Int;
begin
  Result := BUF_EOF;
  if (FInputStream.Size = FInputStream.Position) and (FP >= BufSize) then
      Exit;

  if (FP >= BufSize)
  then
      Result := fillinbuff(FInputStream)
  else
    begin
      Result := buff^[FP];
      Inc(FP);
    end;
end;

procedure TJLSBitIO.myungetc(X: Byte; fil: TCoreClassStream);
begin
  Dec(FP);
  buff^[FP] := X;
  // if  pByteArray(FDebugStream.Memory)[fp]<>buff^[fp]
  // then raise Exception.Create('Error Message');
end;

{ ****************************************************************************
  *  OUTPUT ROUTINES
  **************************************************************************** }

procedure TJLSBitIO.FlushBuff;
begin
  { fwrite must work correctly, even if fp is equal to 0 }
  FOutputStream.write(buff[0], FP);
  FP := 0;
end;

procedure TJLSBitIO.myputc(C: Byte);
begin
  if (FP >= BufSize) then
      FlushBuff;
  buff^[FP] := C;
  Inc(FP);
end;

{
  Flush the input bit buffer TO A BYTE BOUNDARY. Return unused whole
  bytes to the byte buffer
}
procedure TJLSBitIO.bitiflush;
var
  filled, discard, dbytes, i, k, treg: Int;
  bp: PByte;
begin
  k := 0;
  treg := 0;
  filled := 24 - Bits; { how many bits at the MS part of reg
    have unused data. These correspond to
    at most filled+2 bits from the input
    stream, as at most 2 '0' bits might have
    been dropped by marker processing }

  dbytes := (filled + 2) div 8; { the coorrect number of bytes we need to
    "unget" is either dbytes or dbytes-1 }
  { this solution is more general than what is required here: it
    will work correctly even if the end of a scan is not followed
    by a marker, as will necessarily be the case with the standard JPEG
    format }

  while True do
    begin
      bp := @(buff^[0]);
      Inc(bp, FP - dbytes); { back-in the buffer }
      treg := 0;
      k := 0;
      for i := 0 to pred(dbytes) do
        begin
          if (i > 0) and (PByteArray(bp)^[i - 1] = $FF) and ((PByteArray(bp)^[i] and $80) = 0) then
            begin
              treg := treg or PByteArray(bp)^[i] shl (BITBUFSIZE - 7 - k);
              k := k + 7;
            end
          else
            begin
              treg := treg or (PByteArray(bp)^[i] shl (BITBUFSIZE - 8 - k));
              k := k + 8;
            end;
        end;

      if (k <= filled) then
          Break;
      Dec(dbytes);
    end;

  { check consistency }
  if (filled - k > 7) then
    begin
      // fprintf(stderr,"bitiflush: inconsistent bits=%d filled=%d k=%d\n",bits,filled,k);
      // Result:=10
      Exit;
    end;

  discard := filled - k;
  if (treg <> Int((reg shl discard))) then
    begin
      // fprintf(stderr,"bitiflush: inconsistent bits=%d discard=%d reg=%08x treg=%08x\n",bits, discard, reg, treg);
      Exit; // (10);
    end;

  // if IsTrue( reg and (((1 shl discard)-1) shl (BITBUFSIZE-discard)) )
  // fprintf(stderr,"bitiflush: Warning: discarding nonzero bits; reg=%08x bits=%d discard=%d\n",reg,bits,discard);

  FP := FP - dbytes; { do the unget }
  if (buff^[FP - 1] = $FF) and (buff^[FP] = 0) then
      Inc(FP);

  Bits := 0;
  reg := 0;
end;

{ Flushes the bit output buffer and the byte output buffer }
procedure TJLSBitIO.bitoflush;
var
  outbyte: UINT;
begin
  while (Bits < 32) do
    begin
      outbyte := shr_c(reg, 24);
      myputc(outbyte);
      if (outbyte = $FF) then
        begin
          Bits := Bits + 7;
          reg := reg shl 7;
          reg := reg and (not(1 shl (8 * SizeOf(reg) - 1))); { stuff a 0 at MSB }
        end
      else begin
          Bits := Bits + 8;
          reg := reg shl 8;
        end;
    end;
  FlushBuff;
  bitoinit();
end;

{ Initializes the bit output routines }
procedure TJLSBitIO.bitoinit;
begin
  Bits := 32;
  reg := 0;
  FP := 0;
end;

procedure TJLSBitIO.bufiinit;
begin
  { argument is ignored }
  FP := BufSize;
  truebufsize := 0;
  foundeof := False;
end;

constructor TJLSBitIO.Create(AInputStream, AOutputStream: TCoreClassStream);
begin
  FInputStream := AInputStream;
  FOutputStream := AOutputStream;
end;

procedure TJLSBitIO.putbits(X: Int; n: Int);
var
  outbyte: UINT;
begin
  Assert((n <= 24) and (n >= 0) and ((1 shl n) > X));
  Bits := Bits - n;
  reg := reg or Cardinal(X shl Bits);
  while (Bits <= 24) do
    begin
      if (FP >= BufSize) then
        begin
          FOutputStream.write(buff[0], FP);
          FP := 0;
        end;

      buff^[FP] := shr_c(reg, 24);
      outbyte := buff^[FP];
      Inc(FP);

      if (outbyte = $FF) then
        begin
          Bits := Bits + 7;
          reg := reg shl 7;
          { stuff a 0 at MSB }
          reg := reg and (not(1 shl (8 * SizeOf(reg) - 1)));
        end
      else
        begin
          Bits := Bits + 8;
          reg := reg shl 8;
        end;
    end;
end;

procedure TJLSBitIO.put_ones(n: Int);
var
  NN: UINT;
begin
  if (n < 24) then
    begin
      putbits((1 shl n) - 1, n);
    end
  else
    begin
      NN := n;

      while (NN >= 24) do
        begin
          putbits((1 shl 24) - 1, 24);
          NN := NN - 24;
        end;

      if IsTrue(NN)
      then
          putbits((1 shl NN) - 1, NN);
    end;
end;

procedure TJLSBitIO.put_zeros(n: Int);
begin
  Bits := Bits - n;
  while (Bits <= 24) do
    begin
      if (FP >= BufSize) then
        begin
          FOutputStream.write(buff[0], FP);
          FP := 0;
        end;

      buff^[FP] := shr_c(reg, 24);
      Inc(FP);
      reg := reg shl 8;
      Bits := Bits + 8;
    end;
end;

end. 
 
