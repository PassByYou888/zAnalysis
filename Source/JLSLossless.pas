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
{ ****************************************************************************** }
{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  https://github.com/zekiguven/pascal_jls

  author : Zeki Guven
}
unit JLSLossless;

{$I zDefine.inc}

interface

uses JLSGlobal, JLSJpegmark, JLSMelcode, JLSBitIO, CoreClasses;

type
  TJLSLossless = class
  private
    FBitIO: TJLSBitIO;
    FMelcode: TJLSMelcode;
    FImageInfo: PImageInfo;
    eor_limit: int;
    procedure clip(var x: int; alpha: int);
  public
    constructor Create(ABitIO: TJLSBitIO; AMelcode: TJLSMelcode; AImageInfo: PImageInfo);
    function lossless_regular_mode_d(Q, SIGN, Px: int): int;
    procedure lossless_regular_mode_e(Q, SIGN, Px: int; xp: PPixel);
    function lossless_end_of_run_d(Ra, Rb: pixel; RItype: int): pixel;
    procedure lossless_end_of_run_e(Ra, Rb, Ix: pixel; RItype: int);
    function lossless_undoscanline(psl: ppixelarray; { previous scanline }
      sl: ppixelarray;                               { current scanline }
      no: int; color: int): int;

    procedure lossless_doscanline(psl: ppixelarray; { previous scanline }
      sl: ppixelarray;                              { current scanline }
      no, color: int);                              { number of values in it }
    { For pixel interleaved mode for LOSSLESS encoding }

    procedure lossless_doscanline_pixel(psl: ppixelarray; { previous scanline }
      sl: ppixelarray;                                    { current scanline }
      no: int);                                           { number of values in it }

    { For DEOCODING pixel interleavde mode for LOSSLESS images }
    function lossless_undoscanline_pixel(psl: ppixelarray; { previous scanline }
      sl: ppixelarray;                                     { current scanline }
      no: int): int;                                       { number of values in it }

  end;

implementation


{ clipping macro }
procedure TJLSLossless.clip(var x: int; alpha: int);
begin
  if IsTrue(x and FImageInfo^.highmask) then
    begin
      if (x < 0) then
          x := 0
      else
          x := alpha - 1;
    end;
end;

{ Do Golomb-Rice statistics and DECODING for LOSSLESS images }
function TJLSLossless.lossless_regular_mode_d(Q, SIGN, Px: int): int;
var
  At, Bt, Nt, nst, Errval, absErrval: int;
  current, k: int;
  temp: int;
  utemp: ulong;
begin
  { This function is called only for regular contexts.
    End_of_run context is treated separately }

  Nt := FImageInfo^.N[Q];
  At := FImageInfo^.A[Q];

  { Estimate k }
  k := 0;
  nst := Nt;
  while (nst < At) do
    begin
      nst := nst * 2;
      inc(k);
    end;

  { Get the number of leading zeros }
  absErrval := 0;
  while true do
    begin
      temp := FBitIO.zeroLUT[shr_c(FBitIO.reg, 24)];
      absErrval := absErrval + temp;
      if (temp <> 8) then
        begin
          FBitIO.FILLBUFFER(temp + 1);
          break;
        end;
      FBitIO.FILLBUFFER(8);
    end;

  if (absErrval < FImageInfo^.limit) then
    begin
      { now add the binary part of the Rice code }
      if IsTrue(k) then
        begin
          absErrval := absErrval shl k;
          utemp := shr_c(FBitIO.reg, (32 - k));
          FBitIO.FILLBUFFER(k);
          absErrval := absErrval + utemp;
        end;
    end
  else begin
      { the original unary would have been too long:
        (mapped value)-1 was sent verbatim }
      absErrval := shr_c(FBitIO.reg, 32 - (FImageInfo^.qbpp));
      FBitIO.FILLBUFFER(FImageInfo^.qbpp);

      inc(absErrval);
    end;

  { Do the Rice mapping }
  if IsTrue(absErrval and 1) then
    begin { negative }
      absErrval := (absErrval + 1) div 2;
      Errval := -absErrval;
    end
  else
    begin
      absErrval := absErrval div 2;
      Errval := absErrval;
    end;

  Bt := FImageInfo^.B[Q];

  if ((k = 0) and (2 * Bt <= -Nt)) then
    begin
      { special case: see encoder side }
      Errval := -(Errval + 1);

      if (Errval < 0)
      then
          absErrval := -Errval
      else
          absErrval := Errval;
    end;

  { center, clip if necessary, and mask final error }
  if (SIGN = -1) then
    begin
      Px := Px - FImageInfo^.C[Q];
      clip(Px, FImageInfo^.alpha);
      { this is valid if alpha is a power of 2 }
      current := (Px - Errval) and (FImageInfo^.alpha - 1);
    end
  else begin
      Px := Px + FImageInfo^.C[Q];
      clip(Px, FImageInfo^.alpha);
      { valid if alpha is a power of 2 }
      current := (Px + Errval) and (FImageInfo^.alpha - 1);
    end;

  { update bias stats }
  Bt := Bt + Errval;
  FImageInfo^.B[Q] := Bt;

  { update Golomb-Rice stats }
  FImageInfo^.A[Q] := FImageInfo^.A[Q] + absErrval;

  { check reset (joint for Rice-Golomb and bias cancelation) }
  if (Nt = FImageInfo^.reset) then
    begin
      Nt := shr_c(Nt, 1);
      FImageInfo^.N[Q] := Nt;
      FImageInfo^.A[Q] := shr_c(FImageInfo^.A[Q], 1);
      Bt := shr_c(Bt, 1);
      FImageInfo^.B[Q] := Bt;
    end;

  { Do bias estimation for NEXT pixel }
  inc(Nt);
  FImageInfo^.N[Q] := Nt;
  if (Bt <= -Nt) then
    begin

      if (FImageInfo^.C[Q] > MIN_C) then
          dec(FImageInfo^.C[Q]);
      FImageInfo^.B[Q] := FImageInfo^.B[Q] + Nt;
      Bt := FImageInfo^.B[Q];

      if (Bt <= -Nt) then
          FImageInfo^.B[Q] := -Nt + 1;

    end
  else
    if (Bt > 0) then
    begin

      if (FImageInfo^.C[Q] < MAX_C) then
          inc(FImageInfo^.C[Q]);
      FImageInfo^.B[Q] := FImageInfo^.B[Q] - Nt;
      Bt := FImageInfo^.B[Q];

      if (Bt > 0) then
          FImageInfo^.B[Q] := 0;
    end;

  Result := current;
end;

{ Do end of run DECODING for LOSSLESS images }
function TJLSLossless.lossless_end_of_run_d(Ra, Rb: pixel; RItype: int): pixel;
var
  Ix,
    Errval,
    absErrval,
    MErrval,
    k,
    Q,
    oldmap,
    Nt,
    At: int;
  temp: int;
  utemp: ulong;
begin
  Q := EOR_0 + RItype;
  Nt := FImageInfo^.N[Q];
  At := FImageInfo^.A[Q];

  if IsTrue(RItype) then
      At := At + Nt div 2;

  { Estimate k }
  k := 0;
  while (Nt < At) do
    begin
      Nt := Nt * 2;
      inc(k)
    end;

  { read and decode the Golomb code }
  { Get the number of leading zeros }
  MErrval := 0;
  while true do
    begin
      temp := FBitIO.zeroLUT[shr_c(FBitIO.reg, 24)];
      MErrval := MErrval + temp;
      if (temp <> 8) then
        begin
          FBitIO.FILLBUFFER(temp + 1);
          break;
        end;

      FBitIO.FILLBUFFER(8);
    end;

  eor_limit := FImageInfo^.limit - FImageInfo^.limit_reduce;

  if (MErrval < eor_limit) then
    begin
      { now add the binary part of the Golomb code }
      if IsTrue(k) then
        begin
          MErrval := MErrval shl k;
          utemp := shr_c(FBitIO.reg, 32 - (k));
          FBitIO.FILLBUFFER(k);
          MErrval := MErrval + utemp;
        end;
    end
  else
    begin
      { the original unary would have been too long:
        (mapped value)-1 was sent verbatim }
      MErrval := shr_c(FBitIO.reg, 32 - (FImageInfo^.qbpp));
      FBitIO.FILLBUFFER(FImageInfo^.qbpp);

      inc(MErrval);
    end;

  if ((k = 0) and IsTrue(RItype or MErrval) and (2 * FImageInfo^.B[Q] < Nt)) then
      oldmap := 1
  else
      oldmap := 0;
  {
    Note: the Boolean variable 'oldmap' is not
    identical to the variable 'map' in the
    JPEG-LS draft. We have
    oldmap = (qdiff<0) ? (1-map) : map;
  }

  MErrval := MErrval + (RItype + oldmap);

  if IsTrue(MErrval and 1) then
    begin { negative }
      Errval := oldmap - (MErrval + 1) div 2;
      absErrval := -Errval - RItype;
      inc(FImageInfo^.B[Q]);
    end
  else begin { nonnegative }
      Errval := MErrval div 2;
      absErrval := Errval - RItype;
    end;

  if (Rb < Ra) then
      Ix := (Rb - Errval) and (FImageInfo^.alpha - 1)
  else { includes case a==b }
      Ix := (Rb + Errval) and (FImageInfo^.alpha - 1);

  { update stats }
  FImageInfo^.A[Q] := FImageInfo^.A[Q] + absErrval;
  if (FImageInfo^.N[Q] = FImageInfo^.reset) then
    begin
      FImageInfo^.N[Q] := shr_c(FImageInfo^.N[Q], 1);
      FImageInfo^.A[Q] := shr_c(FImageInfo^.A[Q], 1);
      FImageInfo^.B[Q] := shr_c(FImageInfo^.B[Q], 1);
    end;

  inc(FImageInfo^.N[Q]); { for next pixel }

  Result := Ix;
end;

function TJLSLossless.lossless_undoscanline(psl: ppixelarray; { previous scanline }
  sl: ppixelarray;                                            { current scanline }
  no: int; color: int): int;
var
  i, psfix: int;
  diff: int;
  Ra, Rb, Rc, Rd: pixel;
  SIGN: int;
  cont: int;
  Px: pixel;
  N, m: int;
  minx, maxx: pixel;
begin
  psfix := 0;

  { **********************************************/
    /* Do for all pixels in the row in 8-bit mode */
    /********************************************** }
  if (FImageInfo^.bpp16 = FALSE) then
    begin

      Rc := psl^[0];
      Rb := psl^[1];
      Ra := sl^[0];

      i := 1;

      repeat

        Rd := psl^[i + 1];

        { Quantize the gradient }
        cont := FImageInfo^.vLUT[0][Rd - Rb + LUTMAX8] +
          FImageInfo^.vLUT[1][Rb - Rc + LUTMAX8] +
          FImageInfo^.vLUT[2][Rc - Ra + LUTMAX8];

        if (cont = 0) then
          begin
            { *********** RUN STATE ********** }

            { get length of the run }
            { arg is # of pixels left }
            m := FMelcode.process_run_dec(no - i + 1, color);
            N := m;

            if (m > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }
                repeat
                  sl^[i] := Ra;
                  inc(i);
                  dec(N);
                until not(N > 0);

                if (i > no) then { end of line }
                  begin
                    Result := 0;
                    exit;
                  end;
                { update context pixels }
                Rb := psl^[i];
                Rd := psl^[i + 1];
              end;

            { Do end of run encoding for LOSSLESS images }
            if (Ra = Rb) then
                Ra := lossless_end_of_run_d(Ra, Rb, 1)
            else
                Ra := lossless_end_of_run_d(Ra, Rb, 0);

          end { Run state block }
        else
          begin
            { ************ REGULAR CONTEXT ********** }

            Px := predict(Rb, Ra, Rc);

            { map symmetric contexts }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { decode a Rice code of a given context }
            Ra := lossless_regular_mode_d(cont, SIGN, Px);

          end;
        sl^[i] := Ra;
        Rc := Rb;
        Rb := Rd;
        inc(i);

      until not(i <= no);

    end
  else

    { ***********************************************/
      /* Do for all pixels in the row in 16-bit mode */
      /*********************************************** }
    begin

      Rc := ENDIAN16(psl^[0]);
      Rb := ENDIAN16(psl^[1]);
      Ra := ENDIAN16(sl^[0]);

      i := 1;
      repeat

        Rd := ENDIAN16(psl^[i + 1]);

        { Quantize the gradient }

        { Following segment assumes that T3 <= LUTMAX16 }
        { This condition should have been checked when the
          lookup tables were built }
        diff := Rd - Rb;

        if (diff < 0) then
          begin
            if (diff > -LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][diff + LUTMAX16]
            else
                cont := 7 * CREGIONS * CREGIONS
          end
        else
          begin
            if (diff < LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][diff + LUTMAX16]
            else
                cont := 8 * CREGIONS * CREGIONS;
          end;

        diff := Rb - Rc;
        if (diff < 0) then
          begin
            if (diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][diff + LUTMAX16]
            else
                cont := cont + 7 * CREGIONS;
          end
        else
          begin
            if (diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][diff + LUTMAX16]
            else
                cont := cont + 8 * CREGIONS;
          end;

        diff := Rc - Ra;
        if (diff < 0) then
          begin
            if (diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][diff + LUTMAX16]
            else
                cont := cont + 7;
          end
        else
          begin
            if (diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][diff + LUTMAX16]
            else
                cont := cont + 8;
          end;

        if (cont = 0) then
          begin

            { ********** RUN STATE ********** }

            { get length of the run }
            { arg is # of pixels left }
            N := FMelcode.process_run_dec(no - i + 1, color);
            m := N;

            if (m > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }

                repeat

                  sl^[i] := ENDIAN16(Ra);
                  inc(i);
                  dec(N);
                until not(N > 0);

                if (i > no) then
                  { end of line }
                  begin
                    Result := 0;
                    exit;
                  end;

                { update context pixels }
                Rb := ENDIAN16(psl^[i]);
                Rd := ENDIAN16(psl^[i + 1]);
              end;

            { Do end of run encoding for LOSSLESS images }
            if (Ra = Rb) then
                Ra := lossless_end_of_run_d(Ra, Rb, 1)
            else
                Ra := lossless_end_of_run_d(Ra, Rb, 0);

          end
        else
          begin
            { ********** REGULAR CONTEXT ********** }

            Px := predict(Rb, Ra, Rc);

            { map symmetric contexts }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { decode a Rice code of a given context }
            Ra := lossless_regular_mode_d(cont, SIGN, Px);

          end;
        sl^[i] := ENDIAN16(Ra);

        // if  FDebugCounter<> ENDIAN16(Ra)
        // then raise Exception.Create('Error Message Pos : '+IntToStr(FDebugStream.Position-18)+' Pos Val: '+IntToStr(FDebugCounter)+'<> (Ra) i: '+IntTostr(ENDIAN16(Ra))+' i: '+IntToStr(i));

        Rc := Rb;
        Rb := Rd;
        inc(i);

      until not(i <= no);
    end; { End "if 8/16 bit" }

  Result := 0;

end;

function TJLSLossless.lossless_undoscanline_pixel(psl, sl: ppixelarray;
  no: int): int;
var
  i, psfix, n_c, color, enter_run, break_run, was_in_run, test_run: int;
  SIGN: int;
  cont: int;
  c_cont: packed array [0 .. MAX_COMPONENTS - 1] of int;
  N, m: int;
  Ra, Rb, Rc, Rd, Px: pixel;
  c_aa: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_bb: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_cc: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_dd: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_xx: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  diff: int;
begin
  enter_run := 0;
  was_in_run := 0;
  psfix := 0;

  { **********************************************
    * Do for all pixels in the row in 8-bit mode *
    ********************************************** }
  if (FImageInfo^.bpp16 = FALSE) then
    begin

      for n_c := 0 to pred(FImageInfo^.components) do
        begin
          c_cc[n_c] := psl^[n_c];
          c_bb[n_c] := psl^[FImageInfo^.components + n_c];
          c_aa[n_c] := sl^[n_c];
        end;

      i := FImageInfo^.components;
      color := -1;

      repeat

        if not IsTrue(was_in_run) then
            color := (color + 1) mod FImageInfo^.components
        else
            color := 0;

        if (color = 0) then

          for n_c := 0 to pred(FImageInfo^.components) do
            begin
              c_dd[n_c] := psl^[i + FImageInfo^.components + n_c];

              { Quantize the gradient }
              c_cont[n_c] := FImageInfo^.vLUT[0][c_dd[n_c] - c_bb[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[1][c_bb[n_c] - c_cc[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[2][c_cc[n_c] - c_aa[n_c] + LUTMAX8];
            end;

        Ra := c_aa[color];
        Rb := c_bb[color];
        Rc := c_cc[color];
        Rd := c_dd[color];
        cont := c_cont[color];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (color = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                if (c_cont[n_c] <> 0) then
                  begin
                    test_run := 0;
                    break;
                  end;
              end;
          end;

        if IsTrue(test_run) then
          begin
            { ********** RUN STATE ********* }
            enter_run := 0;
            was_in_run := 1;

            { get length of the run }
            { arg is # of pixels left }
            N := FMelcode.process_run_dec((no + FImageInfo^.components - 1 - i + 1) div FImageInfo^.components, 0);
            m := N;

            if (m > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }
                repeat
                  for n_c := 0 to pred(FImageInfo^.components) do
                    begin
                      sl^[i] := c_aa[n_c];
                      inc(i);
                    end;
                  dec(N);
                until not(N > 0);

                if (i > no + FImageInfo^.components - 1) then { end of line }
                  begin
                    Result := 0;
                    exit;
                  end;
                { update context pixels }
                for n_c := 0 to pred(FImageInfo^.components) do
                  begin
                    c_bb[n_c] := psl^[i + n_c];
                    c_dd[n_c] := psl^[i + FImageInfo^.components + n_c];
                  end;

              end;

            { here we handle the "end-of-run" stat }

            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                { The end of run is processed for each component }
                Ra := c_aa[n_c];
                Rb := c_bb[n_c];
                c_xx[n_c] := lossless_end_of_run_d(Ra, Rb, 0);
                c_aa[n_c] := c_xx[n_c];
              end; { Components loop }

          end { Run state block }
        else
          begin

            { ******* REGULAR CONTEXT ******* }
            Px := predict(Rb, Ra, Rc);

            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { decode a Rice code of a given context }
            Ra := lossless_regular_mode_d(cont, SIGN, Px);
            c_aa[color] := Ra;
          end;

        if not IsTrue(was_in_run) then
          begin
            sl^[i] := Ra;
            c_cc[color] := Rb;
            c_bb[color] := Rd;
            inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                sl^[i + n_c] := c_aa[n_c];
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            inc(i, FImageInfo^.components);
          end;

      until not(i <= (no + FImageInfo^.components - 1));

    end
  else
    begin
      { ***********************************************
        * Do for all pixels in the row in 16-bit mode *
        *********************************************** }

      for n_c := 0 to pred(FImageInfo^.components) do
        begin
          c_cc[n_c] := ENDIAN16(psl^[n_c]);
          c_bb[n_c] := ENDIAN16(psl^[FImageInfo^.components + n_c]);
          c_aa[n_c] := ENDIAN16(sl^[n_c]);
        end;

      i := FImageInfo^.components;
      color := -1;

      repeat

        if not IsTrue(was_in_run)
        then
            color := (color + 1) mod FImageInfo^.components
        else
            color := 0;

        if (color = 0) then
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin

                c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.components + n_c]);

                { Quantize the gradient }

                { Following segment assumes that T3 <= LUTMAX16 }
                { This condition should have been checked when the
                  lookup tables were built }
                diff := c_dd[n_c] - c_bb[n_c];
                if (diff < 0) then
                  begin
                    if (diff > -LUTMAX16) then
                        c_cont[n_c] := FImageInfo^.vLUT[0][diff + LUTMAX16]
                    else
                        c_cont[n_c] := 7 * CREGIONS * CREGIONS;
                  end
                else
                  begin
                    if (diff < LUTMAX16) then
                        c_cont[n_c] := FImageInfo^.vLUT[0][diff + LUTMAX16]
                    else
                        c_cont[n_c] := 8 * CREGIONS * CREGIONS;
                  end;

                diff := c_bb[n_c] - c_cc[n_c];
                if (diff < 0) then
                  begin
                    if (diff > -LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 7 * CREGIONS;
                  end
                else
                  begin
                    if (diff < LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 8 * CREGIONS;
                  end;

                diff := c_cc[n_c] - c_aa[n_c];
                if (diff < 0) then
                  begin
                    if (diff > -LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 7;
                  end
                else
                  begin
                    if (diff < LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 8;
                  end;

              end;
          end;

        Ra := c_aa[color];
        Rb := c_bb[color];
        Rc := c_cc[color];
        Rd := c_dd[color];
        cont := c_cont[color];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (color = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              if (c_cont[n_c] <> 0) then
                begin
                  test_run := 0;
                  break;
                end;
          end;

        if IsTrue(test_run) then
          begin

            { ********* RUN STATE ********* }
            enter_run := 1;
            was_in_run := 1;

            { get length of the run }
            { arg is # of pixels left }
            N := FMelcode.process_run_dec((no + FImageInfo^.components - 1 - i + 1) div FImageInfo^.components, 0);
            m := N;
            if (m > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }

                repeat

                  sl^[i] := ENDIAN16(c_aa[n_c]);
                  inc(i);
                  dec(N);
                until not(N > 0);

                if (i > no) then
                  { end of line }
                  begin
                    Result := 0;
                    exit;
                  end;

                { update context pixels }
                c_bb[n_c] := ENDIAN16(psl^[i + n_c]);
                c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.components + n_c]);
              end;

            { here we handle the "end-of-run" state }
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                { The end of run is processed for each component }
                Ra := c_aa[n_c];
                Rb := c_bb[n_c];
                c_xx[n_c] := lossless_end_of_run_d(Ra, Rb, 0);
                c_aa[n_c] := c_xx[n_c];
              end; { Components loop }

          end { Run state block }
        else
          begin

            { ******** REGULAR CONTEXT ******** }

            Px := predict(Rb, Ra, Rc);

            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { decode a Rice code of a given context }
            Ra := lossless_regular_mode_d(cont, SIGN, Px);
            c_aa[color] := Ra;

          end;

        if not IsTrue(was_in_run) then
          begin
            sl^[i] := ENDIAN16(Ra);
            c_cc[color] := Rb;
            c_bb[color] := Rd;
            inc(i);
          end
        else begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                sl^[i + n_c] := ENDIAN16(c_aa[n_c]);
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.components;
          end;

      until not(i <= (no + FImageInfo^.components - 1));

    end; { ends "if 8/16 bit }

  Result := 0;
end;

{ Do end of run encoding for LOSSLESS images }
procedure TJLSLossless.lossless_end_of_run_e(Ra, Rb, Ix: pixel; RItype: int);
var
  Errval,
    MErrval,
    Q,
    absErrval,
    oldmap,
    k,
    At,
    unary: int;
  Nt: int;
begin
  Q := EOR_0 + RItype;
  Nt := FImageInfo^.N[Q];
  At := FImageInfo^.A[Q];

  Errval := Ix - Rb;
  if IsTrue(RItype) then
      At := At + shr_c(Nt, 1)
  else begin
      if (Rb < Ra) then
          Errval := -Errval;
    end;

  { Estimate k }
  k := 0;
  while (Nt < At) do
    begin
      Nt := Nt shl 1;
      inc(k);
    end;

  if (Errval < 0) then
      Errval := Errval + FImageInfo^.alpha;
  if (Errval >= FImageInfo^.ceil_half_alpha) then
      Errval := Errval - FImageInfo^.alpha;

  if ((k = 0) and IsTrue(Errval) and ((FImageInfo^.B[Q] shl 1) < Nt)) then
      oldmap := 1
  else
      oldmap := 0;

  { Note: the Boolean variable 'oldmap' is not
    identical to the variable 'map' in the
    JPEG-LS draft. We have
    oldmap = (Errval<0) ? (1-map) : map;
  }

  { Error mapping for run-interrupted sample (Figure A.22) }
  if (Errval < 0) then
    begin
      MErrval := -(Errval shl 1) - 1 - RItype + oldmap;
      inc(FImageInfo^.B[Q]);
    end
  else
      MErrval := (Errval shl 1) - RItype - oldmap;

  absErrval := shr_c((MErrval + 1 - RItype), 1);

  { Update variables for run-interruped sample (Figure A.23) }
  FImageInfo^.A[Q] := FImageInfo^.A[Q] + absErrval;

  if (FImageInfo^.N[Q] = FImageInfo^.reset) then
    begin
      FImageInfo^.N[Q] := shr_c(FImageInfo^.N[Q], 1);
      FImageInfo^.A[Q] := shr_c(FImageInfo^.A[Q], 1);
      FImageInfo^.B[Q] := shr_c(FImageInfo^.B[Q], 1);
    end;

  inc(FImageInfo^.N[Q]); { for next pixel }

  { Do the actual Golomb encoding: }
  eor_limit := FImageInfo^.limit - FImageInfo^.limit_reduce;
  unary := shr_c(MErrval, k);
  if (unary < eor_limit) then
    begin
      FBitIO.put_zeros(unary);
      FBitIO.putbits((1 shl k) + (MErrval and ((1 shl k) - 1)), k + 1);
    end
  else begin
      FBitIO.put_zeros(eor_limit);
      FBitIO.putbits((1 shl FImageInfo^.qbpp) + MErrval - 1, FImageInfo^.qbpp + 1);
    end;
end;

{ Do Golomb statistics and ENCODING for LOSS-LESS images }
procedure TJLSLossless.lossless_regular_mode_e(Q, SIGN, Px: int; xp: PPixel);
var
  At, Nt, nst, Bt, absErrval, Errval, MErrval, Ix: int;
  unary: int;
  temp: int;
  k: byte;

begin
  Ix := xp^; { current pixel }

  Nt := FImageInfo^.N[Q];
  At := FImageInfo^.A[Q];

  { Prediction correction (A.4.2), compute prediction error (A.4.3)
    , and error quantization (A.4.4) }
  Px := Px + (SIGN) * FImageInfo^.C[Q];
  clip(Px, FImageInfo^.alpha);
  Errval := SIGN * (Ix - Px);

  { Modulo reduction of predication error (A.4.5) }
  if (Errval < 0) then
      Errval := Errval + FImageInfo^.alpha; { Errval is now in [0.. alpha-1] }

  { Estimate k - Golomb coding variable computation (A.5.1) }

  nst := Nt;
  k := 0;
  while (nst < At) do begin nst := nst shl 1; inc(k);
    end;

  { Do Rice mapping and compute magnitude of Errval }
  Bt := FImageInfo^.B[Q];

  { Error Mapping (A.5.2) }
  if (k = 0) and ((Bt shl 1) <= -Nt)
  then
      temp := 1
  else
      temp := 0;

  if (Errval >= FImageInfo^.ceil_half_alpha) then
    begin
      Errval := Errval - FImageInfo^.alpha;
      absErrval := -Errval;
      MErrval := (absErrval shl 1) - 1 - temp;
    end
  else begin
      absErrval := Errval;
      MErrval := (Errval shl 1) + temp;
    end;

  { update bias stats (after correction of the difference) (A.6.1) }
  Bt := Bt + Errval;
  FImageInfo^.B[Q] := Bt;

  { update Golomb stats }
  FImageInfo^.A[Q] := FImageInfo^.A[Q] + absErrval;

  { check for reset }
  if (Nt = FImageInfo^.reset) then
    begin
      { reset for Golomb and bias cancelation at the same time }
      Nt := shr_c(Nt, 1);
      FImageInfo^.N[Q] := Nt;
      FImageInfo^.A[Q] := shr_c(FImageInfo^.A[Q], 1);
      Bt := shr_c(Bt, 1);
      FImageInfo^.B[Q] := Bt;
    end;

  inc(Nt);
  FImageInfo^.N[Q] := Nt;

  { Do bias estimation for NEXT pixel }
  { Bias cancelation tries to put error in (-1,0] (A.6.2) }
  if (Bt <= -Nt) then
    begin
      if (FImageInfo^.C[Q] > MIN_C) then
          dec(FImageInfo^.C[Q]);
      FImageInfo^.B[Q] := FImageInfo^.B[Q] + Nt;
      if (FImageInfo^.B[Q] <= -Nt) then
          FImageInfo^.B[Q] := -Nt + 1;

    end
  else if (Bt > 0) then
    begin

      if (FImageInfo^.C[Q] < MAX_C) then
          inc(FImageInfo^.C[Q]);
      FImageInfo^.B[Q] := FImageInfo^.B[Q] - Nt;
      if (FImageInfo^.B[Q] > 0) then
          FImageInfo^.B[Q] := 0;
    end;

  { Actually output the code: Mapped Error Encoding (Appendix G) }
  unary := shr_c(MErrval, k);
  if (unary < FImageInfo^.limit) then
    begin
      FBitIO.put_zeros(unary);
      FBitIO.putbits((1 shl k) + (MErrval and ((1 shl k) - 1)), k + 1);
    end
  else begin
      FBitIO.put_zeros(FImageInfo^.limit);
      FBitIO.putbits((1 shl FImageInfo^.qbpp) + MErrval - 1, FImageInfo^.qbpp + 1);
    end;
end;

{ For line and plane interleaved mode in LOSS-LESS mode }
constructor TJLSLossless.Create(ABitIO: TJLSBitIO; AMelcode: TJLSMelcode; AImageInfo: PImageInfo);
begin
  FBitIO := ABitIO;
  FMelcode := AMelcode;
  FImageInfo := AImageInfo;
end;

procedure TJLSLossless.lossless_doscanline(psl: ppixelarray; { previous scanline }
  sl: ppixelarray;                                           { current scanline }
  no, color: int);                                           { number of values in it }
var
  i: int;
  Ra, Rb, Rc, Rd, { context pixels }
  Ix,             { current pixel }
  Px: pixel;      { predicted current pixel }

  SIGN: int; { sign of current context }
  cont: int; { context }

  RUNcnt: int;
  diff: int;
begin
  { *** watch it! actual pixels in the scan line are numbered 1 to no .
    pixels with indices < 1 or > no are dummy "border" pixels }

  i := 1; { pixel indices in a scan line go from 1 to no }

  { **********************************************/
    /* Do for all pixels in the row in 8-bit mode */
    /********************************************** }
  if (FImageInfo^.bpp16 = FALSE) then
    begin

      Rc := psl^[0];
      Rb := psl^[1];
      Ra := sl^[0];

      { For 8-bit Image }

      repeat

        Ix := sl^[i];
        Rd := psl^[i + 1];

        { Context determination }

        { Quantize the gradient }
        { partial context number: if (b-e) is used then its
          contribution is added after determination of the run state.
          Also, sign flipping, if any, occurs after run
          state determination }

        cont := FImageInfo^.vLUT[0][Rd - Rb + LUTMAX8] +
          FImageInfo^.vLUT[1][Rb - Rc + LUTMAX8] +
          FImageInfo^.vLUT[2][Rc - Ra + LUTMAX8];

        if (cont = 0) then
          begin
            { *************** RUN STATE *************************** }

            RUNcnt := 0;

            if (Ix = Ra) then
              begin
                while (true) do
                  begin

                    inc(RUNcnt);
                    inc(i);
                    if (i > no) then
                      begin
                        { Run-lenght coding when reach end of line (A.7.1.2) }
                        FMelcode.process_run_enc(RUNcnt, EOLINE, color);
                        exit; { end of line }
                      end;

                    Ix := sl^[i];

                    if (Ix <> Ra) then { Run is broken }
                      begin
                        Rd := psl^[i + 1];
                        Rb := psl^[i];
                        break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }

            { Run-lenght coding when end of line not reached (A.7.1.2) }
            FMelcode.process_run_enc(RUNcnt, NOEOLINE, color);

            { This is the END_OF_RUN state }
            if (Ra = Rb)
            then
                lossless_end_of_run_e(Ra, Rb, Ix, 1)
            else
                lossless_end_of_run_e(Ra, Rb, Ix, 0);

          end
        else
          begin

            { *************** REGULAR CONTEXT ******************* }

            Px := predict(Rb, Ra, Rc);

            { do symmetric context merging }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { output a rice code }
            lossless_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        sl^[i] := Ix;

        Ra := Ix;
        Rc := Rb;
        Rb := Rd;
        inc(i);
      until not(i <= no);

    end
  else
    begin

      { *********************************************** }
      { * Do for all pixels in the row in 16-bit mode * }
      { *********************************************** }

      Rc := ENDIAN16(psl^[0]);
      Rb := ENDIAN16(psl^[1]);
      Ra := ENDIAN16(sl^[0]);

      { For 16-bit Image }

      repeat

        Ix := ENDIAN16(sl^[i]);
        Rd := ENDIAN16(psl^[i + 1]);

        { Context determination }

        { Quantize the gradient }
        { partial context number: if (b-e) is used then its
          contribution is added after determination of the run state.
          Also, sign flipping, if any, occurs after run
          state determination }

        { Following segment assumes that T3 <= LUTMAX16 }
        { This condition should have been checked when the
          lookup tables were built }
        diff := Rd - Rb;

        if (diff < 0) then
          begin
            if (diff > -LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][diff + LUTMAX16]
            else
                cont := 7 * CREGIONS * CREGIONS;
          end
        else
          begin
            if (diff < LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][diff + LUTMAX16]
            else
                cont := 8 * CREGIONS * CREGIONS;
          end;

        diff := Rb - Rc;

        if (diff < 0) then
          begin
            if (diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][diff + LUTMAX16]
            else
                cont := cont + 7 * CREGIONS;
          end
        else
          begin
            if (diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][diff + LUTMAX16]
            else
                cont := cont + 8 * CREGIONS;
          end;

        diff := Rc - Ra;

        if (diff < 0) then
          begin
            if (diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][diff + LUTMAX16]
            else
                cont := cont + 7;
          end
        else
          begin
            if (diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][diff + LUTMAX16]
            else
                cont := cont + 8;
          end;

        if (cont = 0) then { Run state? }
          begin
            { *************** RUN STATE *************************** }

            RUNcnt := 0;

            if (Ix = Ra) then
              begin
                while true do
                  begin

                    inc(RUNcnt);
                    inc(i);
                    if (i > no) then
                      begin
                        { Run-lenght coding when reach end of line (A.7.1.2) }
                        FMelcode.process_run_enc(RUNcnt, EOLINE, color);
                        exit; { end of line }
                      end;

                    Ix := ENDIAN16(sl^[i]);

                    if (Ix <> Ra) then { Run is broken }
                      begin
                        Rd := ENDIAN16(psl^[i + 1]);
                        Rb := ENDIAN16(psl^[i]);
                        break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }

            { Run-lenght coding when end of line not reached (A.7.1.2) }
            FMelcode.process_run_enc(RUNcnt, NOEOLINE, color);

            { This is the END_OF_RUN state }
            if (Ra = Rb)
            then
                lossless_end_of_run_e(Ra, Rb, Ix, 1)
            else
                lossless_end_of_run_e(Ra, Rb, Ix, 0);

          end
        else begin

            { *************** REGULAR CONTEXT ******************* }

            Px := predict(Rb, Ra, Rc);

            { do symmetric context merging }

            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { output a rice code }
            lossless_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        sl^[i] := ENDIAN16(Ix);
        Ra := Ix;
        Rc := Rb;
        Rb := Rd;
        inc(i);
      until not(i <= no);
    end;

end;

procedure TJLSLossless.lossless_doscanline_pixel(psl, sl: ppixelarray;
  no: int);
var
  i, n_c, enter_run, break_run, was_in_run, test_run: int;
  color: int; { Index to the component, 0..COMPONENTS-1 }
  c_aa: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_bb: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_cc: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_dd: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  c_xx: packed array [0 .. MAX_COMPONENTS - 1] of pixel;
  Ra, Rb, Rc, Rd, { context pixels }
  Ix,             { current pixel }
  Px: pixel;      { predicted current pixel }
  SIGN: int;      { sign of current context }
  cont: int;
  c_cont: packed array [0 .. MAX_COMPONENTS - 1] of int; { context }
  RUNcnt: int;
  diff: int;
begin

  enter_run := 0;
  was_in_run := 0;

  if (FImageInfo^.bpp16 = FALSE) then
    begin
      { **********************************************
        * Do for all pixels in the row in 8-bit mode *
        ********************************************** }

      for n_c := 0 to pred(FImageInfo^.components) do
        begin
          c_cc[n_c] := ENDIAN8(psl^[n_c]);
          c_bb[n_c] := ENDIAN8(psl^[FImageInfo^.components + n_c]);
          c_aa[n_c] := ENDIAN8(sl^[n_c]);
        end;

      i := FImageInfo^.components; { pixel indices in a scan line go from COMPONENTS to no }
      color := -1;

      repeat

        if not IsTrue(was_in_run) then
            color := (color + 1) mod FImageInfo^.components
        else
            color := 0;

        Ix := ENDIAN8(sl^[i]);

        for n_c := 0 to pred(FImageInfo^.components) do
            c_xx[n_c] := ENDIAN8(sl^[i + n_c]);

        if (color = 0) then
          for n_c := 0 to pred(FImageInfo^.components) do
            begin
              c_dd[n_c] := ENDIAN8(psl^[i + FImageInfo^.components + n_c]);

              { Context determination }

              { Quantize the gradient }
              { partial context number: if (b-e) is used
                then its contribution is added after
                determination of the run state.
                Also, sign flipping, if any, occurs after run
                state determination }

              c_cont[n_c] := FImageInfo^.vLUT[0][c_dd[n_c] - c_bb[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[1][c_bb[n_c] - c_cc[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[2][c_cc[n_c] - c_aa[n_c] + LUTMAX8];
            end;

        Ra := c_aa[color];
        Rb := c_bb[color];
        Rc := c_cc[color];
        Rd := c_dd[color];
        cont := c_cont[color];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (color = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              if (c_cont[n_c] <> 0) then
                begin
                  test_run := 0;
                  break;
                end;
          end;

        { Run state? }
        if IsTrue(test_run) then
          begin
            { *************** RUN STATE *************************** }

            enter_run := 1;
            was_in_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              if (ENDIAN8(sl^[i + n_c]) <> c_bb[n_c]) then
                  enter_run := 0;
            RUNcnt := 0;
            if IsTrue(enter_run) then
              begin
                while true do
                  begin
                    inc(RUNcnt);
                    i := i + FImageInfo^.components;
                    if ((i) > (no + FImageInfo^.components - 1)) then
                      begin
                        FMelcode.process_run_enc(RUNcnt, EOLINE, 0);
                        exit; { end of line }
                      end;

                    for n_c := 0 to pred(FImageInfo^.components) do
                        c_xx[n_c] := ENDIAN8(sl^[i + n_c]);

                    break_run := 0;

                    for n_c := 0 to pred(FImageInfo^.components) do
                      if (c_xx[n_c] <> c_aa[n_c]) then
                          break_run := 1;

                    if IsTrue(break_run) then { Run is broken }
                      begin
                        for n_c := 0 to pred(FImageInfo^.components) do
                          begin
                            c_dd[n_c] := ENDIAN8(psl^[i + FImageInfo^.components + n_c]);
                            c_bb[n_c] := ENDIAN8(psl^[i + n_c]);
                          end;
                        break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }

            FMelcode.process_run_enc(RUNcnt, NOEOLINE, 0);

            { This is the END_OF_RUN state }
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                { The end of run is done for each component }
                Ix := c_xx[n_c];
                Ra := c_aa[n_c];
                Rb := c_bb[n_c];

                lossless_end_of_run_e(Ra, Rb, Ix, 0);

              end; { loop for components }

          end { Run state block }
        else
          begin

            { *************** REGULAR CONTEXT ******************* }
            Px := predict(Rb, Ra, Rc);

            { map symmetric contexts }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { output a rice code }

            lossless_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        if not IsTrue(was_in_run) then
          begin
            c_aa[color] := Ix;
            c_cc[color] := Rb;
            c_bb[color] := Rd;
            inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                c_aa[n_c] := c_xx[n_c];
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.components;
          end;

      until not(i <= (no + FImageInfo^.components - 1));

    end
  else
    begin

      { **********************************************
        * Do for all pixels in the row in 16-bit mode*
        ********************************************** }

      for n_c := 0 to pred(FImageInfo^.components) do
        begin
          c_cc[n_c] := ENDIAN16(psl^[n_c]);
          c_bb[n_c] := ENDIAN16(psl^[FImageInfo^.components + n_c]);
          c_aa[n_c] := ENDIAN16(sl^[n_c]);
        end;

      i := FImageInfo^.components; { pixel indices in a scan line go from COMPONENTS to no }
      color := -1;

      repeat

        if not IsTrue(was_in_run) then
            color := (color + 1) mod FImageInfo^.components
        else
            color := 0;

        Ix := ENDIAN16(sl^[i]);

        for n_c := 0 to pred(FImageInfo^.components) do
            c_xx[n_c] := ENDIAN16(sl^[i + n_c]);

        if (color = 0) then
          for n_c := 0 to pred(FImageInfo^.components) do
            begin
              c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.components + n_c]);

              { Context determination }

              { Quantize the gradient }
              { partial context number: if (b-e) is used
                then its contribution is added after
                determination of the run state.
                Also, sign flipping, if any, occurs after run
                state determination }

              { Following segment assumes that Sc <= LUTMAX16 }
              { This condition should have been checked when the
                lookup tables were built }
              diff := c_dd[n_c] - c_bb[n_c];
              if (diff < 0) then
                begin
                  if (diff > -LUTMAX16) then
                      c_cont[n_c] := FImageInfo^.vLUT[0][diff + LUTMAX16]
                  else
                      c_cont[n_c] := 7 * CREGIONS * CREGIONS;
                end
              else
                begin
                  if (diff < LUTMAX16) then
                      c_cont[n_c] := FImageInfo^.vLUT[0][diff + LUTMAX16]
                  else
                      c_cont[n_c] := 8 * CREGIONS * CREGIONS;
                end;

              diff := c_bb[n_c] - c_cc[n_c];
              if (diff < 0) then
                begin
                  if (diff > -LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 7 * CREGIONS;
                end
              else
                begin
                  if (diff < LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 8 * CREGIONS;
                end;

              diff := c_cc[n_c] - c_aa[n_c];
              if (diff < 0) then
                begin
                  if (diff > -LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 7;
                end
              else
                begin
                  if (diff < LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 8;
                end;
            end;

        Ra := c_aa[color];
        Rb := c_bb[color];
        Rc := c_cc[color];
        Rd := c_dd[color];
        cont := c_cont[color];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (color = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              if (c_cont[n_c] <> 0) then
                begin
                  test_run := 0;
                  break;
                end;
          end;

        { Run state? }
        if IsTrue(test_run) then
          begin
            { *************** RUN STATE *************************** }

            enter_run := 1;
            was_in_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              if (ENDIAN16(sl^[i + n_c]) <> c_bb[n_c]) then
                  enter_run := 0;
            RUNcnt := 0;
            if IsTrue(enter_run) then
              begin
                while true do
                  begin
                    inc(RUNcnt);
                    i := i + FImageInfo^.components;
                    if ((i) > (no + FImageInfo^.components - 1)) then
                      begin
                        FMelcode.process_run_enc(RUNcnt, EOLINE, 0);
                        exit; { end of line }
                      end;

                    for n_c := 0 to pred(FImageInfo^.components) do
                        c_xx[n_c] := ENDIAN16(sl^[i + n_c]);

                    break_run := 0;

                    for n_c := 0 to pred(FImageInfo^.components) do
                      if (c_xx[n_c] <> c_aa[n_c]) then
                          break_run := 1;

                    if IsTrue(break_run) then { Run is broken }
                      begin
                        for n_c := 0 to pred(FImageInfo^.components) do
                          begin
                            c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.components + n_c]);
                            c_bb[n_c] := ENDIAN16(psl^[i + n_c]);
                          end;
                        break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }
            FMelcode.process_run_enc(RUNcnt, NOEOLINE, 0);

            { This is the END_OF_RUN state }
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                { The end of run is done for each component }
                Ix := c_xx[n_c];
                Ra := c_aa[n_c];
                Rb := c_bb[n_c];

                lossless_end_of_run_e(Ra, Rb, Ix, 0);

              end; { loop for components }

          end { Run state block }
        else
          begin
            { *************** REGULAR CONTEXT ******************* }
            Px := predict(Rb, Ra, Rc);
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                SIGN := -1;
                cont := -cont;
              end
            else
                SIGN := +1;

            { output a rice code }
            lossless_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        if not IsTrue(was_in_run) then
          begin
            c_aa[color] := Ix;
            c_cc[color] := Rb;
            c_bb[color] := Rd;
            inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                c_aa[n_c] := c_xx[n_c];
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.components;
          end;

      until not(i <= (no + FImageInfo^.components - 1));

    end; { ends "if" for 8 or 16 bit }

end;

end.
