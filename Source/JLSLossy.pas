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
unit JLSLossy;

{$I zDefine.inc}

interface

uses JLSGlobal, JLSMelcode, JLSBitio;

type
  TJLSLossy = class
    FBitIO: TJLSBitIO;
    FMelcode: TJLSMelcode;
    FImageInfo: PImageInfo;
    eor_limit: int;
    procedure clip(var x: int; alpha: int);
  public
    constructor Create(ABitIO: TJLSBitIO; AMelcode: TJLSMelcode; AImageInfo: PImageInfo);
    { Do Golomb-Rice statistics and DECODING for LOSSY images }
    function lossy_regular_mode_d(Q: int; SIGN: int; Px: int): int;
    { Do Golomb statistics and ENCODING for LOSSY images }
    procedure lossy_regular_mode_e(Q: int; SIGN: int; Px: int; xp: PPixel);
    { Do end of run DECODING for LOSSY images }
    function lossy_end_of_run_d(Ra: Pixel; Rb: Pixel; RItype: int): Pixel;
    { Do end of run encoding for LOSSY images -  returns reconstructed value of Ix }
    function lossy_end_of_run_e(Ra: Pixel; Rb: Pixel; Ix: Pixel; RItype: int): Pixel;
    { For DECODING line and plane interleaved mode for LOSSY images }
    function lossy_undoscanline(psl: PPixelArray; { previous scanline }
      sl: PPixelArray;                            { current scanline }
      no: int; color: int): int;                  { number of values in it }
    { For DECODING pixel interleaved mode in LOSSY mode }
    function lossy_undoscanline_pixel(psl: PPixelArray; { previous scanline }
      sl: PPixelArray;                                  { current scanline }
      no: int): int;                                    { number of values in it }

    { For line and plane interleaved mode in LOSSY mode }
    procedure lossy_doscanline(psl: PPixelArray; { previous scanline }
      sl: PPixelArray;                           { current scanline }
      no: int; color: int);                      { number of values in it }
    { For pixel interleavde mode for LOSSY encoding }
    procedure lossy_doscanline_pixel(psl: PPixelArray; { previous scanline }
      sl: PPixelArray;                                 { current scanline }
      no: int);                                        { number of values in it }
  end;

implementation

{ For DECODING line and plane interleaved mode for LOSSY images }
procedure TJLSLossy.clip(var x: int; alpha: int);
begin
  if IsTrue(x and FImageInfo^.highmask) then
    begin
      if (x < 0) then
          x := 0
      else
          x := alpha - 1;
    end;
end;

constructor TJLSLossy.Create(ABitIO: TJLSBitIO; AMelcode: TJLSMelcode;
  AImageInfo: PImageInfo);
begin
  FBitIO := ABitIO;
  FMelcode := AMelcode;
  FImageInfo := AImageInfo;
end;

procedure TJLSLossy.lossy_doscanline(psl, sl: PPixelArray; no, color: int);
var
  i: int;
  Ra, Rb, Rc, Rd, { context pixels }
  Ix,             { current pixel }
  Px: Pixel;      { predicted current pixel }
  Rx: int;        { reconstructed current pixel }
  SIGN: int;      { sign of current context }
  cont: int;      { context }
  unary: int;
  RItype: int;
  RUNcnt: int;
  diff: int;
  delta: int;
begin

  i := 1; { pixel indices in a scan line go from 1 to no }

  { **********************************************
    * Do for all pixels in the row in 8-bit mode *
    ********************************************** }

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

            if (delta <= FImageInfo^._near) and (delta >= FImageInfo^.negNEAR) then
              begin

                while (True) do
                  begin
                    Inc(RUNcnt);

                    sl^[i] := Ra;
                    Inc(i);
                    if (i > no) then
                      begin
                        { Run-lenght coding when reach end of line (A.7.1.2) }
                        FMelcode.process_run_enc(RUNcnt, EOLINE, color);
                        Exit; { end of line }
                      end;

                    Ix := sl^[i];

                    delta := Ix - Ra;
                    if (delta > FImageInfo^._near) or (delta < FImageInfo^.negNEAR) then { Run is broken }
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
            if ((Rb - Ra) <= FImageInfo^._near) and ((Rb - Ra) >= FImageInfo^.negNEAR) then
                RItype := 1
            else
                RItype := 0;
            Ix := lossy_end_of_run_e(Ra, Rb, Ix, RItype);

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
            lossy_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        sl^[i] := Ix;
        Ra := Ix;
        Rc := Rb;
        Rb := Rd;
        Inc(i);

      until not(i <= no);

    end
  else
    begin { 16 bit mode instead of 8 }

      { ***********************************************
        * Do for all pixels in the row in 16-bit mode *
        *********************************************** }

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

        { Following segment assumes that Sc <= LUTMAX16 }
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

            delta := Ix - Ra;
            RUNcnt := 0;

            if (delta <= FImageInfo^._near) and (delta >= FImageInfo^.negNEAR) then
              begin
                while True do
                  begin
                    Inc(RUNcnt);

                    sl^[i] := ENDIAN16(Ra);
                    Inc(i);
                    if (i > no) then
                      begin
                        { Run-lenght coding when reach end of line (A.7.1.2) }
                        FMelcode.process_run_enc(RUNcnt, EOLINE, color);
                        Exit; { end of line }
                      end;

                    Ix := ENDIAN16(sl^[i]);

                    delta := Ix - Ra;
                    if (delta > FImageInfo^._near) or (delta < FImageInfo^.negNEAR) then { Run is broken }
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
            if ((Rb - Ra) <= FImageInfo^._near) and ((Rb - Ra) >= FImageInfo^.negNEAR) then
                RItype := 1
            else
                RItype := 0;
            Ix := lossy_end_of_run_e(Ra, Rb, Ix, RItype);

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
            lossy_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        sl^[i] := ENDIAN16(Ix);
        Ra := Ix;
        Rc := Rb;
        Rb := Rd;
        Inc(i);
      until not(i <= no);
    end; { for "if" 16 or 8 bit mode }

end;

procedure TJLSLossy.lossy_doscanline_pixel(psl, sl: PPixelArray; no: int);
var
  i, n_c, enter_run, break_run, was_in_run, test_run: int;
  color: int; { Index to the component, 0..COMPONENTS-1 }
  c_aa: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_bb: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_cc: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_dd: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_xx: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  Ra, Rb, Rc, Rd, { context pixels }
  Ix,             { current pixel }
  Px: int;        { predicted current pixel }
  SIGN: int;      { sign of current context }
  cont: int;
  c_cont: packed array [0 .. MAX_COMPONENTS - 1] of int; { context }
  RUNcnt: int;
  delta: packed array [0 .. MAX_COMPONENTS - 1] of int;
  diff: int;
begin

  enter_run := 0;
  was_in_run := 0;

  if (FImageInfo^.bpp16 = FALSE) then
    begin
      { ********************************************** }
      { * Do for all pixels in the row in 8-bit mode * }
      { ********************************************** }

      for n_c := 0 to pred(FImageInfo^.components) do
        begin
          c_cc[n_c] := psl^[n_c];
          c_bb[n_c] := psl^[FImageInfo^.components + n_c];
          c_aa[n_c] := sl^[n_c];
        end;

      i := FImageInfo^.components; { pixel indices in a scan line go from COMPONENTS to no }
      color := -1;

      repeat

        if not IsTrue(was_in_run) then
            color := (color + 1) mod FImageInfo^.components
        else
            color := 0;

        Ix := sl^[i];

        for n_c := 0 to pred(FImageInfo^.components) do
            c_xx[n_c] := sl^[i + n_c];

        if (color = 0) then
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                c_dd[n_c] := psl^[i + FImageInfo^.components + n_c];

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
          end;

        Ra := c_aa[color];
        Rb := c_bb[color];
        Rc := c_cc[color];
        Rd := c_dd[color];
        cont := c_cont[color];

        test_run := 0;
        was_in_run := 0;
        enter_run := 0;

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
            { *************** RUN STATE *************************** }

            enter_run := 1;
            was_in_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                delta[n_c] := sl^[i + n_c] - c_aa[n_c];
                if (delta[n_c] > FImageInfo^._near) or (delta[n_c] < FImageInfo^.negNEAR) then
                    enter_run := 0;
              end;

            RUNcnt := 0;

            if IsTrue(enter_run) then
              begin
                while (True) do
                  begin
                    Inc(RUNcnt);

                    for n_c := 0 to pred(FImageInfo^.components) do
                        sl^[i + n_c] := c_aa[n_c];

                    i := i + FImageInfo^.components;
                    if ((i) > (no + FImageInfo^.components - 1)) then
                      begin
                        FMelcode.process_run_enc(RUNcnt, EOLINE, 0);
                        Exit; { end of line }
                      end;

                    for n_c := 0 to pred(FImageInfo^.components) do
                        c_xx[n_c] := sl^[i + n_c];

                    break_run := 0;
                    for n_c := 0 to pred(FImageInfo^.components) do
                      begin
                        delta[n_c] := c_xx[n_c] - c_aa[n_c];
                        if (delta[n_c] > FImageInfo^._near) or (delta[n_c] < FImageInfo^.negNEAR) then
                            break_run := 1;
                      end;

                    if IsTrue(break_run) then
                      begin
                        for n_c := 0 to pred(FImageInfo^.components) do
                          begin
                            c_dd[n_c] := psl^[i + FImageInfo^.components + n_c];
                            c_bb[n_c] := psl^[i + n_c];
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
                Rb := c_bb[n_c];
                Ra := c_aa[n_c];

                { Handle two special EOR states }
                Ix := lossy_end_of_run_e(Ra, Rb, Ix, 0);
                c_xx[n_c] := Ix;

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
            lossy_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        if not IsTrue(was_in_run) then
          begin
            c_aa[color] := Ix;
            sl^[i] := Ix; { store reconstructed x }

            c_cc[color] := Rb;
            c_bb[color] := Rd;
            Inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                c_aa[n_c] := c_xx[n_c];
                sl^[i + n_c] := c_xx[n_c];

                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.components;
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

        if IsTrue(test_run) then
          begin
            { *************** RUN STATE *************************** }

            enter_run := 1;
            was_in_run := 1;
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                delta[n_c] := ENDIAN16(sl^[i + n_c]) - c_aa[n_c];
                if (delta[n_c] > FImageInfo^._near) or (delta[n_c] < FImageInfo^.negNEAR) then
                    enter_run := 0;
              end;
            RUNcnt := 0;

            if IsTrue(enter_run) then
              begin
                while True do
                  begin
                    Inc(RUNcnt);

                    for n_c := 0 to pred(FImageInfo^.components) do
                        sl^[i + n_c] := ENDIAN16(c_aa[n_c]);

                    i := i + FImageInfo^.components;
                    if ((i) > (no + FImageInfo^.components - 1)) then
                      begin
                        FMelcode.process_run_enc(RUNcnt, EOLINE, 0);
                        Exit; { end of line }
                      end;

                    for n_c := 0 to pred(FImageInfo^.components) do
                        c_xx[n_c] := ENDIAN16(sl^[i + n_c]);

                    break_run := 0;
                    for n_c := 0 to pred(FImageInfo^.components) do
                      begin
                        delta[n_c] := c_xx[n_c] - c_aa[n_c];
                        if (delta[n_c] > FImageInfo^._near) or (delta[n_c] < FImageInfo^.negNEAR) then
                            break_run := 1;
                      end;

                    if IsTrue(break_run) then
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
                Rb := c_bb[n_c];
                Ra := c_aa[n_c];

                { Handle two special EOR states }
                Ix := lossy_end_of_run_e(Ra, Rb, Ix, 0);
                c_xx[n_c] := Ix;

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
            lossy_regular_mode_e(cont, SIGN, Px, @Ix);
          end;

        { context for next pixel: }
        if not IsTrue(was_in_run) then
          begin
            c_aa[color] := Ix;
            sl^[i] := ENDIAN16(Ix); { store reconstructed x }
            c_cc[color] := Rb;
            c_bb[color] := Rd;
            Inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                c_aa[n_c] := c_xx[n_c];
                sl^[i + n_c] := ENDIAN16(c_xx[n_c]);

                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.components;
          end;

      until not(i <= (no + FImageInfo^.components - 1));

    end; { ends 'if' for 8 or 16 bit }

end;

function TJLSLossy.lossy_end_of_run_d(Ra, Rb: Pixel; RItype: int): Pixel;
var
  xpr,
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
      Inc(k)
    end;

  { read and decode the Golomb code }
  { Get the number of leading zeros }
  MErrval := 0;
  while True do
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

      Inc(MErrval);
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
      Inc(FImageInfo^.B[Q]);
    end
  else begin { nonnegative }
      Errval := MErrval div 2;
      absErrval := Errval - RItype;
    end;

  Errval := FImageInfo^.qmul0^[(FImageInfo^.alpha - 1) + Errval]; { de-quantize prediction error }
  if IsTrue(RItype) then
    begin
      Ix := Ra + Errval;
    end
  else begin
      if (Rb < Ra) then
          Ix := Rb - Errval
      else
          Ix := Rb + Errval;
    end;

  if (Ix < FImageInfo^.negNEAR) then
      Ix := Ix + FImageInfo^.beta
  else if (Ix > FImageInfo^.alpha1eps) then
      Ix := Ix - FImageInfo^.beta;

  clip(Ix, FImageInfo^.alpha);

  { update stats }
  FImageInfo^.A[Q] := FImageInfo^.A[Q] + absErrval;
  if (FImageInfo^.N[Q] = FImageInfo^.reset) then
    begin
      FImageInfo^.N[Q] := shr_c(FImageInfo^.N[Q], 1);
      FImageInfo^.A[Q] := shr_c(FImageInfo^.A[Q], 1);
      FImageInfo^.B[Q] := shr_c(FImageInfo^.B[Q], 1);
    end;

  Inc(FImageInfo^.N[Q]); { for next pixel }

  Result := Ix;

end;

function TJLSLossy.lossy_end_of_run_e(Ra, Rb, Ix: Pixel; RItype: int): Pixel;
var
  qErrval, iqErrval, xpr,
    MErrval,
    Q,
    absErrval,
    oldmap,
    k,
    Nt,
    At,
    Errval: int;
  Rx: int; { reconstructed pixel }
  unary: int;
begin
  Q := EOR_0 + RItype;
  Nt := FImageInfo^.N[Q];
  At := FImageInfo^.A[Q];

  if IsTrue(RItype) then
    begin
      if (xpr = Ra) then
          Errval := Ix - 1
      else
          Errval := Ix;
      At := At + (Nt div 2);
    end
  else begin
      if (xpr = Rb) then
          Errval := Ix - 1
      else
          Errval := Ix;

      if (Rb < Ra) then
          Errval := -Errval;
    end;

  qErrval := PIntArrayAccess(FImageInfo^.qdiv)^[Errval];
  iqErrval := PIntArrayAccess(FImageInfo^.qmul)^[qErrval];

  if IsTrue(RItype) or (Rb >= Ra) then
      Rx := xpr + iqErrval
  else
      Rx := xpr - iqErrval;

  clip(Rx, FImageInfo^.alpha); { reconstructed pixel }
  Ix := Rx;

  { Estimate k }
  k := 0;
  while (Nt < At) do
    begin
      Nt := Nt * 2;
      Inc(k)
    end;

  if (qErrval < 0) then
      qErrval := qErrval + FImageInfo^.qbeta;

  if (qErrval >= FImageInfo^.ceil_half_qbeta) then
      qErrval := qErrval - FImageInfo^.qbeta;

  if ((k = 0) and IsTrue(qErrval) and (2 * FImageInfo^.B[Q] < Nt)) then
      oldmap := 1
  else
      oldmap := 0;

  {
    Note: the Boolean variable 'oldmap' is not
    identical to the variable 'map' in the
    JPEG-LS draft. We have
    oldmap = (qErrval<0) ? (1-map) : map;
  }

  { Error mapping for run-interrupted sample (Figure A.22) }
  if (qErrval < 0) then
    begin
      MErrval := -2 * qErrval - 1 - RItype + oldmap;
      Inc(FImageInfo^.B[Q]);
    end
  else
      MErrval := 2 * qErrval - RItype - oldmap;

  absErrval := (MErrval + 1 - RItype) div 2;

  { update stats }
  FImageInfo^.A[Q] := FImageInfo^.A[Q] + absErrval;
  if (FImageInfo^.N[Q] = FImageInfo^.reset) then
    begin
      FImageInfo^.N[Q] := shr_c(FImageInfo^.N[Q], 1);
      FImageInfo^.A[Q] := shr_c(FImageInfo^.A[Q], 1);
      FImageInfo^.B[Q] := shr_c(FImageInfo^.B[Q], 1);
    end;

  Inc(FImageInfo^.N[Q]); { for next pixel }

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

  Result := Ix;
end;

function TJLSLossy.lossy_regular_mode_d(Q, SIGN, Px: int): int;
var
  At, Bt, Nt, Errval, absErrval: int;
  current, k, nst: int;
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
      Inc(k);
    end;

  { Get the number of leading zeros }
  absErrval := 0;
  while True do
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

      Inc(absErrval);
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

  if ((k = 0) and (FImageInfo^._near = 0) and (2 * Bt <= -Nt)) then
    begin
      { special case: see encoder side }
      Errval := -(Errval + 1);

      if (Errval < 0)
      then
          absErrval := -Errval
      else
          absErrval := Errval;
    end;

  Errval := FImageInfo^.qmul0^[(FImageInfo^.alpha - 1) + Errval]; { dequantize prediction error }

  { center, clip if necessary, and mask final error }
  if (SIGN = -1) then
    begin
      Px := Px - FImageInfo^.C[Q];
      clip(Px, FImageInfo^.alpha);
      current := (Px - Errval);
    end
  else begin
      Px := Px + FImageInfo^.C[Q];
      clip(Px, FImageInfo^.alpha);
      current := (Px + Errval);
    end;

  { first, we reduce mod beta in the range -_near <= x <= alpha-1+_near,
    then we clip to [0,alpha] }
  if (current < FImageInfo^.negNEAR) then
      current := current + FImageInfo^.beta
  else if (current > FImageInfo^.alpha1eps) then
      current := current - FImageInfo^.beta;

  clip(current, FImageInfo^.alpha);

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
  Inc(Nt);
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
          Inc(FImageInfo^.C[Q]);
      FImageInfo^.B[Q] := FImageInfo^.B[Q] - Nt;
      Bt := FImageInfo^.B[Q];

      if (Bt > 0) then
          FImageInfo^.B[Q] := 0;
    end;

  Result := current;

end;

procedure TJLSLossy.lossy_regular_mode_e(Q, SIGN, Px: int; xp: PPixel);
var
  At, Bt, Nt, absErrval, Errval, MErrval,
    qErrval, iqErrval, Rx, Ix, nst: int;
  unary: int;
  temp: int;
  k: byte;
begin
  Ix := xp^; { current pixel }

  Nt := FImageInfo^.N[Q];
  At := FImageInfo^.A[Q];
  { Estimate k - Golomb coding variable computation (A.5.1) }
  k := 0;
  while (nst < At) do
    begin
      nst := nst shl 1;
      Inc(k);
    end;

  { Prediction correction (A.4.2), compute prediction error (A.4.3)
    , and error quantization (A.4.4) }

  Px := Px + (SIGN) * FImageInfo^.C[Q];
  clip(Px, FImageInfo^.alpha);
  Errval := SIGN * (Ix - Px);
  qErrval := PIntArrayAccess(FImageInfo^.qdiv)^[Errval];
  iqErrval := PIntArrayAccess(FImageInfo^.qmul)^[qErrval];
  Rx := Px + SIGN * iqErrval;

  clip(Rx, FImageInfo^.alpha);
  xp^ := Rx; { store reconstructed pixel in scan line }

  { Modulo reduction of predication error (A.4.5) }
  if (qErrval < 0) then
      qErrval := qErrval + FImageInfo^.qbeta; { qErrval is now in [0..qbeta-1] }

  { Do Rice mapping and compute magnitude of diff }
  Bt := FImageInfo^.B[Q];

  { Error Mapping (A.5.2) }
  temp := Bool_c((k = 0) and (FImageInfo^._near = 0) and ((Bt shl 1) <= -Nt));
  if (qErrval >= FImageInfo^.ceil_half_qbeta) then
    begin
      qErrval := qErrval - FImageInfo^.qbeta;
      absErrval := -qErrval;
      MErrval := 2 * absErrval - 1 - temp;
    end
  else begin
      absErrval := qErrval;
      MErrval := 2 * qErrval + temp;
    end;

  { update bias stats (after correction of the difference) (A.6.1) }

  Errval := PIntArrayAccess(FImageInfo^.qmul)^[qErrval]; { convert back to alphabet space }

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

  { Do bias estimation for NEXT pixel }
  { Bias cancelation tries to put error in (-1,0] (A.6.2) }
  Inc(Nt);
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
  else if (Bt > 0) then
    begin

      if (FImageInfo^.C[Q] < MAX_C) then
          Inc(FImageInfo^.C[Q]);

      FImageInfo^.B[Q] := FImageInfo^.B[Q] - Nt;
      Bt := FImageInfo^.B[Q];

      if (Bt > 0) then
          FImageInfo^.B[Q] := 0;

    end;

  { Actually output the code: Mapped Error Encoding (A.5.3) }
  unary := shr_c(MErrval, k);
  if (unary < FImageInfo^.limit) then
    begin
      FBitIO.put_zeros(unary);
      FBitIO.putbits((1 shl k) + (MErrval and ((1 shl k) - 1)), k + 1);
    end
  else
    begin
      FBitIO.put_zeros(FImageInfo^.limit);
      FBitIO.putbits((1 shl FImageInfo^.qbpp) + MErrval - 1, FImageInfo^.qbpp + 1);
    end;

end;

function TJLSLossy.lossy_undoscanline(psl: PPixelArray; { previous scanline }
  sl: PPixelArray;                                      { current scanline }
  no: int; color: int): int;                            { number of values in it }
{ *** watch it! actual pixels in the scan line are numbered 1 to no .
  pixels with indices < 1 or > no are dummy "border" pixels  * }
var
  i, psfix: int;
  Ra, Rb, Rc, Rd: Pixel;
  SIGN: int;
  cont: int;
  run_int_type: int;

  Px: Pixel;
  N, m: int;
  minx, maxx: Pixel;
  diff: int;

begin
  psfix := 0;

  { **********************************************
    * Do for all pixels in the row in 8-bit mode *
    ********************************************** }
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
            { ********** RUN STATE ********* }

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
                  Inc(i);
                  dec(N);
                until not(N > 0);

                if (i > no) then { end of line }
                  begin
                    Result := 0;
                    Exit;
                  end;
                { update context pixels }
                Rb := psl^[i];
                Rd := psl^[i + 1];

              end;

            { here we handle the "end-of-run" state }
            if ((Rb - Ra) <= FImageInfo^._near) and ((Rb - Ra) >= FImageInfo^.negNEAR) then
                run_int_type := 1
            else
                run_int_type := 0;
            Ra := lossy_end_of_run_d(Ra, Rb, run_int_type);
          end
        else
          begin

            { ****** REGULAR CONTEXT ****** }

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
            Ra := lossy_regular_mode_d(cont, SIGN, Px);
          end;

        sl^[i] := Ra;
        Rc := Rb;
        Rb := Rd;
        Inc(i);

      until not(i <= no);

    end
  else
    { ***********************************************
      * Do for all pixels in the row in 16-bit mode  *
      ************************************************ }
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
                  Inc(i);
                  dec(N);
                until not(N > 0);

                if (i > no) then
                  { end of line }
                  begin
                    Result := 0;
                    Exit;
                  end;

                { update context pixels }
                Rb := ENDIAN16(psl^[i]);
                Rd := ENDIAN16(psl^[i + 1]);
              end;

            { here we handle the "end-of-run" state, which is
              treated separately from regular states }

            if ((Rb - Ra) <= FImageInfo^._near) and ((Rb - Ra) >= FImageInfo^.negNEAR) then
                run_int_type := 1
            else
                run_int_type := 0;
            Ra := lossy_end_of_run_d(Ra, Rb, run_int_type);

          end
        else
          begin

            { ******REGULAR CONTEXT ****** }

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
            Ra := lossy_regular_mode_d(cont, SIGN, Px);
          end;

        sl^[i] := ENDIAN16(Ra);
        Rc := Rb;
        Rb := Rd;
        Inc(i);

      until not(i <= no);

    end; { ends "if 8/16 bit" }

  Result := 0;
end;

function TJLSLossy.lossy_undoscanline_pixel(psl: PPixelArray; { previous scanline }
  sl: PPixelArray;                                            { current scanline }
  no: int): int;                                              { number of values in it }
{ *** watch it! actual pixels in the scan line are numbered 1 to no .
  pixels with indices < 1 or > no are dummy "border" pixels }
var
  i, psfix, n_c, color, enter_run, break_run, was_in_run,
    test_run: int;
  Ra, Rb, Rc, Rd, Px: Pixel;
  c_aa: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_bb: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_cc: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_dd: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_xx: packed array [0 .. MAX_COMPONENTS - 1] of Pixel;
  SIGN: int;
  cont: int;
  c_cont: packed array [0 .. MAX_COMPONENTS - 1] of int;
  N, m: int;
  diff: int;
begin

  enter_run := 0;
  was_in_run := 0;
  psfix := 0;

  { ********************************************** }
  { * Do for all pixels in the row in 8-bit mode * }
  { ********************************************** }
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

        if not IsTrue(was_in_run)
        then
            color := (color + 1) mod FImageInfo^.components
        else
            color := 0;

        if (color = 0) then
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                c_dd[n_c] := psl^[i + FImageInfo^.components + n_c];

                { Quantize the gradient }
                c_cont[n_c] := FImageInfo^.vLUT[0][c_dd[n_c] - c_bb[n_c] + LUTMAX8] +
                  FImageInfo^.vLUT[1][c_bb[n_c] - c_cc[n_c] + LUTMAX8] +
                  FImageInfo^.vLUT[2][c_cc[n_c] - c_aa[n_c] + LUTMAX8];
              end;
          end;
        ;

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

            if IsTrue(test_run) then
              begin
                { ********* RUN STATE ********* }
                enter_run := 0;
                was_in_run := 1;

                { get length of the run }
                { arg is # of pixels left }

                N := FMelcode.process_run_dec((no + FImageInfo^.components - 1 - i + 1) div FImageInfo^.components, 0);
                m := N;

                if (m > 0) then
                  begin
                    { run of nonzero length, otherwise
                      we go directly to the end-of-run state }
                    repeat
                      for n_c := 0 to pred(FImageInfo^.components) do
                        begin
                          sl^[i] := c_aa[n_c];
                          Inc(i);
                        end;
                      dec(N);
                    until not(N > 0);

                    if (i > no + FImageInfo^.components - 1) then { end of line }
                      begin
                        Result := 0;
                        Exit;
                      end;

                    { update context pixels }
                    for n_c := 0 to pred(FImageInfo^.components) do
                      begin
                        c_bb[n_c] := psl^[i + n_c];
                        c_dd[n_c] := psl^[i + FImageInfo^.components + n_c];
                      end;
                  end;

                { here we handle the "end-of-run" state }
                for n_c := 0 to pred(FImageInfo^.components) do
                  begin
                    { The end of run is processed for each component }
                    Ra := c_aa[n_c];
                    Rb := c_bb[n_c];

                    c_xx[n_c] := lossy_end_of_run_d(Ra, Rb, 0);
                    c_aa[n_c] := c_xx[n_c];

                  end; { Components loop }
              end
            else
              begin
                { ****** REGULAR CONTEXT ******* }

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
                Ra := lossy_regular_mode_d(cont, SIGN, Px);
                c_aa[color] := Ra;
              end;

            if not IsTrue(was_in_run) then
              begin
                sl^[i] := Ra;
                c_cc[color] := Rb;
                c_bb[color] := Rd;
                Inc(i);
              end
            else
              begin
                for n_c := 0 to pred(FImageInfo^.components) do
                  begin
                    sl^[i + n_c] := c_aa[n_c];
                    c_cc[n_c] := c_bb[n_c];
                    c_bb[n_c] := c_dd[n_c];
                  end;
                Inc(i, FImageInfo^.components);
              end;
          end;

      until not(i <= (no + FImageInfo^.components - 1));

    end
  else
    begin
      { *********************************************** }
      { * Do for all pixels in the row in 16-bit mode * }
      { *********************************************** }

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
                  if (diff > -LUTMAX16)
                  then
                      c_cont[n_c] := FImageInfo^.vLUT[0][diff + LUTMAX16]
                  else
                      c_cont[n_c] := 7 * CREGIONS * CREGIONS;
                end
              else
                begin
                  if (diff < LUTMAX16)
                  then
                      c_cont[n_c] := FImageInfo^.vLUT[0][diff + LUTMAX16]
                  else
                      c_cont[n_c] := 8 * CREGIONS * CREGIONS;
                end;

              diff := c_bb[n_c] - c_cc[n_c];
              if (diff < 0) then
                begin
                  if (diff > -LUTMAX16)
                  then
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
                  if (diff < LUTMAX16)
                  then
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

            { ********* RUN STATE ********* }
            was_in_run := 1;
            enter_run := was_in_run;

            { get length of the run }
            { arg is # of pixels left }
            N := FMelcode.process_run_dec((no + FImageInfo^.components - 1 - i + 1) div FImageInfo^.components, 0);
            m := N;

            if (m > 0) then
              begin
                { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }
                repeat
                  for n_c := 0 to pred(FImageInfo^.components) do
                    begin
                      sl^[i] := ENDIAN16(c_aa[n_c]);
                      Inc(i);
                    end;
                  dec(N);
                until not(N > 0);

                if (i > no + FImageInfo^.components - 1) then
                  begin { end of line }
                    Result := 0;
                    Exit;
                  end;

                { update context pixels }
                for n_c := 0 to pred(FImageInfo^.components) do
                  begin
                    c_bb[n_c] := ENDIAN16(psl^[i + n_c]);
                    c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.components + n_c]);
                  end;
              end;

            { here we handle the "end-of-run" state }
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                { The end of run is processed for each component }
                Ra := c_aa[n_c];
                Rb := c_bb[n_c];
                c_xx[n_c] := lossy_end_of_run_d(Ra, Rb, 0);
                c_aa[n_c] := c_xx[n_c];
              end; { Components loop }

          end
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
            Ra := lossy_regular_mode_d(cont, SIGN, Px);
            c_aa[color] := Ra;
          end;

        if not IsTrue(was_in_run) then
          begin
            sl^[i] := ENDIAN16(Ra);
            c_cc[color] := Rb;
            c_bb[color] := Rd;
            Inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.components) do
              begin
                sl^[i + n_c] := ENDIAN16(c_aa[n_c]);
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;

            Inc(i, FImageInfo^.components);
          end;

      until not(i <= (no + FImageInfo^.components - 1));
    end; { for "if 8/16 bit" mode }

  Result := 0;

end;

end.
