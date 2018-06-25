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
unit JLSMelcode;

{$I zDefine.inc}

interface

uses
  JLSGlobal, JLSBitIO;

const
  MELCSTATES                                    = 32; { number of melcode states }

  J: packed array [0 .. MELCSTATES - 1] of Byte = (
    0, 0, 0, 0,
    1, 1, 1, 1,
    2, 2, 2, 2,
    3, 3, 3, 3,
    4, 4,
    5, 5,
    6, 6,
    7, 7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15
    );

type
  TJLSMelcode = class
  private
    FBitIO: TJLSBitIO;
    FImageInfo: PImageInfo;

    melcstate: packed array [0 .. MAX_COMPONENTS - 1] of int; { index to the state packed array }

    melclen: packed array [0 .. MAX_COMPONENTS - 1] of int;
    { contents of the state packed array location
      indexed by melcstate: the "expected"
      run length is 2^melclen, shorter runs are
      encoded by a 1 followed by the run length
      in binary representation, wit a fixed length
      of melclen bits }

    melcorder: packed array [0 .. MAX_COMPONENTS - 1] of ulong; { 2^ melclen }
  public
    constructor Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);
    procedure init_process_run;
    function process_run_dec(lineleft: int; color: int): int;
    procedure process_run_enc(runlen: int; an_eoline: int; color: int);
    procedure close_process_run;
  end;

implementation

constructor TJLSMelcode.Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);
begin
  FBitIO := ABitIO;
  FImageInfo := AImageInfo;
end;

procedure TJLSMelcode.init_process_run;
var
  n_c: int;
begin

  for n_c := 0 to pred(FImageInfo^.components) do
    begin
      melcstate[n_c] := 0;
      melclen[n_c] := J[0];
      melcorder[n_c] := 1 shl melclen[n_c];
    end;
end;

{ decoding routine: reads bits from the input and returns a run length. }
{ argument is the number of pixels left to  end-of-line (bound on run length) }
function TJLSMelcode.process_run_dec(lineleft: int; color: int): int;
var
  runlen: int;
  temp, hits: int;
begin
  runlen := 0;

  while true do
    begin
      temp := FBitIO.zeroLUT[Byte((not shr_c(FBitIO.reg, 24)))]; { number of leading ones in the
        input stream, up to 8 }
      for hits := 1 to temp do
        begin
          runlen := runlen + melcorder[color];
          if (runlen >= lineleft) then
            begin { reached end-of-line }
              if (runlen = lineleft) and (melcstate[color] < MELCSTATES) then
                begin
                  inc(melcstate[color]);
                  melclen[color] := J[melcstate[color]];
                  melcorder[color] := (1 shl melclen[color]);
                end;
              FBitIO.FILLBUFFER(hits); { actual # of 1's consumed }
              result := lineleft;
              exit;
            end;
          if (melcstate[color] < MELCSTATES) then
            begin
              inc(melcstate[color]);
              melclen[color] := J[melcstate[color]];
              melcorder[color] := (1 shl melclen[color]);
            end;
        end;
      if (temp <> 8) then
        begin
          FBitIO.FILLBUFFER(temp + 1); { consume the leading 0 of the remainder encoding }
          break;
        end;
      FBitIO.FILLBUFFER(8);
    end;

  { read the length of the remainder }
  if Istrue(melclen[color]) then
    begin
      temp := shr_c(FBitIO.reg, 32 - (melclen[color]));
      FBitIO.FILLBUFFER(melclen[color]);
      // GETBITS(temp, melclen[color]);  /*** GETBITS is a macro, not a function */
      runlen := runlen + temp;
    end;
  FImageInfo^.limit_reduce := melclen[color] + 1;

  { adjust melcoder parameters }
  if Istrue(melcstate[color]) then
    begin
      dec(melcstate[color]);
      melclen[color] := J[melcstate[color]];
      melcorder[color] := (1 shl melclen[color]);
    end;

  result := runlen;
end;

procedure TJLSMelcode.process_run_enc(runlen: int; an_eoline: int; color: int);
var
  hits: int;
begin
  hits := 0;

  while (runlen >= melcorder[color]) do
    begin
      inc(hits);
      runlen := runlen - melcorder[color];
      if (melcstate[color] < MELCSTATES) then
        begin
          inc(melcstate[color]);
          melclen[color] := J[melcstate[color]];
          melcorder[color] := (1 shl melclen[color]);
        end;
    end;

  { send the required number of "hit" bits (one per occurrence
    of a run of length melcorder). This number is never too big:
    after 31 such "hit" bits, each "hit" would represent a run of 32K
    pixels.
  }
  FBitIO.PUT_ONES(hits);

  if (an_eoline = EOLINE) then
    begin
      { when the run is broken by end-of-line, if there is
        a non-null remainder, send it as if it were
        a max length run }
      if Istrue(runlen) then
          FBitIO.PUT_ONES(1);
      exit;
    end;

  { now send the length of the remainder, encoded as a 0 followed
    by the length in binary representation, to melclen bits }
  FImageInfo^.limit_reduce := melclen[color] + 1;
  FBitIO.PUTBITS(runlen, FImageInfo^.limit_reduce);

  { adjust melcoder parameters }
  if Istrue(melcstate[color]) then
    begin
      dec(melcstate[color]);
      melclen[color] := J[melcstate[color]];
      melcorder[color] := (1 shl melclen[color]);
    end;

end;

procedure TJLSMelcode.close_process_run;
begin
  { retained for compatibility with ranked runs }
end;

end.
