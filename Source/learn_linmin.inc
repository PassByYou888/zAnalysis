{ ****************************************************************************** }
{ * Normalizes direction/step pair support, by QQ 600585@qq.com                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
type
  TLinMinState = packed record
    BRACKT: Boolean;
    STAGE1: Boolean;
    INFOC: TLInt;
    DG: TLFloat;
    DGM: TLFloat;
    DGINIT: TLFloat;
    DGTEST: TLFloat;
    DGX: TLFloat;
    DGXM: TLFloat;
    DGY: TLFloat;
    DGYM: TLFloat;
    FINIT: TLFloat;
    FTEST1: TLFloat;
    FM: TLFloat;
    FX: TLFloat;
    FXM: TLFloat;
    FY: TLFloat;
    FYM: TLFloat;
    STX: TLFloat;
    STY: TLFloat;
    STMIN: TLFloat;
    STMAX: TLFloat;
    WIDTH: TLFloat;
    WIDTH1: TLFloat;
    XTRAPF: TLFloat;
  end;

procedure LinMinNormalizeD(var D: TLVec; var Stp: TLFloat; N: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MCSRCH(const N: TLInt; var X: TLVec;
  var F: TLFloat; var G: TLVec; const S: TLVec;
  var Stp: TLFloat; STPMAX: TLFloat; var INFO: TLInt;
  var NFEV: TLInt; var WA: TLVec; var State: TLinMinState; var Stage: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


const
  LM_FTOL      = 0.001;
  LM_XTOL      = 100 * MachineEpsilon;
  LM_GTOL      = 0.3;
  LM_MAXFEV    = 20;
  LM_STPMIN    = 1.0E-50;
  LM_DefSTPMAX = 1.0E+50;

procedure MCSTEP(var STX: TLFloat; var FX: TLFloat; var DX: TLFloat;
  var STY: TLFloat; var FY: TLFloat; var DY: TLFloat;
  var Stp: TLFloat; const FP: TLFloat; const DP: TLFloat;
  var BRACKT: Boolean; const STMIN: TLFloat; const STMAX: TLFloat;
  var INFO: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Normalizes direction/step pair: makes |D|=1, scales Stp.
  If |D|=0, it returns, leavind D/Stp unchanged.
  ************************************************************************ *)
procedure LinMinNormalizeD(var D: TLVec; var Stp: TLFloat;
  N: TLInt);
var
  MX: TLFloat;
  S : TLFloat;
  I : TLInt;
begin

  //
  // first, scale D to avoid underflow/overflow durng squaring
  //
  MX := 0;
  I := 0;
  while I <= N - 1 do
    begin
      MX := Max(MX, AbsReal(D[I]));
      Inc(I);
    end;
  if AP_FP_Eq(MX, 0) then
    begin
      Exit;
    end;
  S := 1 / MX;
  APVMul(@D[0], 0, N - 1, S);
  Stp := Stp / S;

  //
  // normalize D
  //
  S := APVDotProduct(@D[0], 0, N - 1, @D[0], 0, N - 1);
  S := 1 / Sqrt(S);
  APVMul(@D[0], 0, N - 1, S);
  Stp := Stp / S;
end;

procedure MCSRCH(const N: TLInt; var X: TLVec;
  var F: TLFloat; var G: TLVec; const S: TLVec;
  var Stp: TLFloat; STPMAX: TLFloat; var INFO: TLInt;
  var NFEV: TLInt; var WA: TLVec; var State: TLinMinState;
  var Stage: TLInt);
var
  V   : TLFloat;
  P5  : TLFloat;
  P66 : TLFloat;
  ZERO: TLFloat;
begin

  //
  // init
  //
  P5 := 0.5;
  P66 := 0.66;
  State.XTRAPF := 4.0;
  ZERO := 0;
  if AP_FP_Eq(STPMAX, 0) then
    begin
      STPMAX := LM_DefSTPMAX;
    end;
  if AP_FP_Less(Stp, LM_STPMIN) then
    begin
      Stp := LM_STPMIN;
    end;
  if AP_FP_Greater(Stp, STPMAX) then
    begin
      Stp := STPMAX;
    end;

  //
  // Main cycle
  //
  while True do
    begin
      if Stage = 0 then
        begin

          //
          // NEXT
          //
          Stage := 2;
          Continue;
        end;
      if Stage = 2 then
        begin
          State.INFOC := 1;
          INFO := 0;

          //
          // CHECK THE INPUT PARAMETERS FOR ERRORS.
          //
          if (N <= 0) or AP_FP_Less_Eq(Stp, 0) or AP_FP_Less(LM_FTOL, 0) or
            AP_FP_Less(LM_GTOL, ZERO) or AP_FP_Less(LM_XTOL, ZERO) or
            AP_FP_Less(LM_STPMIN, ZERO) or AP_FP_Less(STPMAX, LM_STPMIN) or (LM_MAXFEV <= 0)
          then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // COMPUTE THE INITIAL GRADIENT IN THE SEARCH DIRECTION
          // AND CHECK THAT S IS A DESCENT DIRECTION.
          //
          V := APVDotProduct(@G[0], 0, N - 1, @S[0], 0, N - 1);
          State.DGINIT := V;
          if AP_FP_Greater_Eq(State.DGINIT, 0) then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // INITIALIZE LOCAL VARIABLES.
          //
          State.BRACKT := False;
          State.STAGE1 := True;
          NFEV := 0;
          State.FINIT := F;
          State.DGTEST := LM_FTOL * State.DGINIT;
          State.WIDTH := STPMAX - LM_STPMIN;
          State.WIDTH1 := State.WIDTH / P5;
          APVMove(@WA[0], 0, N - 1, @X[0], 0, N - 1);

          //
          // THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
          // FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
          // THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
          // FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
          // THE INTERVAL OF UNCERTAINTY.
          // THE VARIABLES STP, F, DG CONTAIN THE VALUES OF THE STEP,
          // FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
          //
          State.STX := 0;
          State.FX := State.FINIT;
          State.DGX := State.DGINIT;
          State.STY := 0;
          State.FY := State.FINIT;
          State.DGY := State.DGINIT;

          //
          // NEXT
          //
          Stage := 3;
          Continue;
        end;
      if Stage = 3 then
        begin

          //
          // START OF ITERATION.
          //
          // SET THE MINIMUM AND MAXIMUM STEPS TO CORRESPOND
          // TO THE PRESENT INTERVAL OF UNCERTAINTY.
          //
          if State.BRACKT then
            begin
              if AP_FP_Less(State.STX, State.STY) then
                begin
                  State.STMIN := State.STX;
                  State.STMAX := State.STY;
                end
              else
                begin
                  State.STMIN := State.STY;
                  State.STMAX := State.STX;
                end;
            end
          else
            begin
              State.STMIN := State.STX;
              State.STMAX := Stp + State.XTRAPF * (Stp - State.STX);
            end;

          //
          // FORCE THE STEP TO BE WITHIN THE BOUNDS STPMAX AND LM_STPMIN.
          //
          if AP_FP_Greater(Stp, STPMAX) then
            begin
              Stp := STPMAX;
            end;
          if AP_FP_Less(Stp, LM_STPMIN) then
            begin
              Stp := LM_STPMIN;
            end;

          //
          // IF AN UNUSUAL TERMINATION IS TO OCCUR THEN LET
          // STP BE THE LOWEST POINT OBTAINED SO FAR.
          //
          if State.BRACKT and (AP_FP_Less_Eq(Stp, State.STMIN) or
            AP_FP_Greater_Eq(Stp, State.STMAX)) or (NFEV >= LM_MAXFEV - 1) or
            (State.INFOC = 0) or State.BRACKT and
            AP_FP_Less_Eq(State.STMAX - State.STMIN, LM_XTOL * State.STMAX) then
            begin
              Stp := State.STX;
            end;

          //
          // EVALUATE THE FUNCTION AND GRADIENT AT STP
          // AND COMPUTE THE DIRECTIONAL DERIVATIVE.
          //
          APVMove(@X[0], 0, N - 1, @WA[0], 0, N - 1);
          APVAdd(@X[0], 0, N - 1, @S[0], 0, N - 1, Stp);

          //
          // NEXT
          //
          Stage := 4;
          Exit;
        end;
      if Stage = 4 then
        begin
          INFO := 0;
          NFEV := NFEV + 1;
          V := APVDotProduct(@G[0], 0, N - 1, @S[0], 0, N - 1);
          State.DG := V;
          State.FTEST1 := State.FINIT + Stp * State.DGTEST;

          //
          // TEST FOR CONVERGENCE.
          //
          if State.BRACKT and (AP_FP_Less_Eq(Stp, State.STMIN) or
            AP_FP_Greater_Eq(Stp, State.STMAX)) or (State.INFOC = 0) then
            begin
              INFO := 6;
            end;
          if AP_FP_Eq(Stp, STPMAX) and AP_FP_Less_Eq(F, State.FTEST1) and
            AP_FP_Less_Eq(State.DG, State.DGTEST) then
            begin
              INFO := 5;
            end;
          if AP_FP_Eq(Stp, LM_STPMIN) and (AP_FP_Greater(F, State.FTEST1) or
            AP_FP_Greater_Eq(State.DG, State.DGTEST)) then
            begin
              INFO := 4;
            end;
          if NFEV >= LM_MAXFEV then
            begin
              INFO := 3;
            end;
          if State.BRACKT and AP_FP_Less_Eq(State.STMAX - State.STMIN,
            LM_XTOL * State.STMAX) then
            begin
              INFO := 2;
            end;
          if AP_FP_Less_Eq(F, State.FTEST1) and AP_FP_Less_Eq(AbsReal(State.DG),
            -LM_GTOL * State.DGINIT) then
            begin
              INFO := 1;
            end;

          //
          // CHECK FOR TERMINATION.
          //
          if INFO <> 0 then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // IN THE FIRST STAGE WE SEEK A STEP FOR WHICH THE MODIFIED
          // FUNCTION HAS A NONPOSITIVE VALUE AND NONNEGATIVE DERIVATIVE.
          //
          if State.STAGE1 and AP_FP_Less_Eq(F, State.FTEST1) and
            AP_FP_Greater_Eq(State.DG, Min(LM_FTOL, LM_GTOL) * State.DGINIT) then
            begin
              State.STAGE1 := False;
            end;

          //
          // A MODIFIED FUNCTION IS USED TO PREDICT THE STEP ONLY IF
          // WE HAVE NOT OBTAINED A STEP FOR WHICH THE MODIFIED
          // FUNCTION HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE
          // DERIVATIVE, AND IF A LOWER FUNCTION VALUE HAS BEEN
          // OBTAINED BUT THE DECREASE IS NOT SUFFICIENT.
          //
          if State.STAGE1 and AP_FP_Less_Eq(F, State.FX) and
            AP_FP_Greater(F, State.FTEST1) then
            begin

              //
              // DEFINE THE MODIFIED FUNCTION AND DERIVATIVE VALUES.
              //
              State.FM := F - Stp * State.DGTEST;
              State.FXM := State.FX - State.STX * State.DGTEST;
              State.FYM := State.FY - State.STY * State.DGTEST;
              State.DGM := State.DG - State.DGTEST;
              State.DGXM := State.DGX - State.DGTEST;
              State.DGYM := State.DGY - State.DGTEST;

              //
              // CALL CSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
              // AND TO COMPUTE THE NEW STEP.
              //
              MCSTEP(State.STX, State.FXM, State.DGXM, State.STY, State.FYM,
                State.DGYM, Stp, State.FM, State.DGM, State.BRACKT, State.STMIN,
                State.STMAX, State.INFOC);

              //
              // RESET THE FUNCTION AND GRADIENT VALUES FOR F.
              //
              State.FX := State.FXM + State.STX * State.DGTEST;
              State.FY := State.FYM + State.STY * State.DGTEST;
              State.DGX := State.DGXM + State.DGTEST;
              State.DGY := State.DGYM + State.DGTEST;
            end
          else
            begin

              //
              // CALL MCSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
              // AND TO COMPUTE THE NEW STEP.
              //
              MCSTEP(State.STX, State.FX, State.DGX, State.STY, State.FY, State.DGY,
                Stp, F, State.DG, State.BRACKT, State.STMIN, State.STMAX,
                State.INFOC);
            end;

          //
          // FORCE A SUFFICIENT DECREASE IN THE SIZE OF THE
          // INTERVAL OF UNCERTAINTY.
          //
          if State.BRACKT then
            begin
              if AP_FP_Greater_Eq(AbsReal(State.STY - State.STX), P66 * State.WIDTH1)
              then
                begin
                  Stp := State.STX + P5 * (State.STY - State.STX);
                end;
              State.WIDTH1 := State.WIDTH;
              State.WIDTH := AbsReal(State.STY - State.STX);
            end;

          //
          // NEXT.
          //
          Stage := 3;
          Continue;
        end;
    end;
end;

procedure MCSTEP(var STX: TLFloat; var FX: TLFloat; var DX: TLFloat;
  var STY: TLFloat; var FY: TLFloat; var DY: TLFloat;
  var Stp: TLFloat; const FP: TLFloat; const DP: TLFloat;
  var BRACKT: Boolean; const STMIN: TLFloat; const STMAX: TLFloat;
  var INFO: TLInt);
var
  BOUND: Boolean;
  GAMMA: TLFloat;
  P    : TLFloat;
  Q    : TLFloat;
  R    : TLFloat;
  S    : TLFloat;
  SGND : TLFloat;
  STPC : TLFloat;
  STPF : TLFloat;
  STPQ : TLFloat;
  THETA: TLFloat;
begin
  INFO := 0;

  //
  // CHECK THE INPUT PARAMETERS FOR ERRORS.
  //
  if BRACKT and (AP_FP_Less_Eq(Stp, Min(STX, STY)) or AP_FP_Greater_Eq(Stp,
    Max(STX, STY))) or AP_FP_Greater_Eq(DX * (Stp - STX), 0) or
    AP_FP_Less(STMAX, STMIN) then
    begin
      Exit;
    end;

  //
  // DETERMINE IF THE DERIVATIVES HAVE OPPOSITE SIGN.
  //
  SGND := DP * (DX / AbsReal(DX));

  //
  // FIRST CASE. A HIGHER FUNCTION VALUE.
  // THE MINIMUM IS BRACKETED. IF THE CUBIC STEP IS CLOSER
  // TO STX THAN THE QUADRATIC STEP, THE CUBIC STEP IS TAKEN,
  // ELSE THE AVERAGE OF THE CUBIC AND QUADRATIC STEPS IS TAKEN.
  //
  if AP_FP_Greater(FP, FX) then
    begin
      INFO := 1;
      BOUND := True;
      THETA := 3 * (FX - FP) / (Stp - STX) + DX + DP;
      S := Max(AbsReal(THETA), Max(AbsReal(DX), AbsReal(DP)));
      GAMMA := S * Sqrt(AP_Sqr(THETA / S) - DX / S * (DP / S));
      if AP_FP_Less(Stp, STX) then
        begin
          GAMMA := -GAMMA;
        end;
      P := GAMMA - DX + THETA;
      Q := GAMMA - DX + GAMMA + DP;
      R := P / Q;
      STPC := STX + R * (Stp - STX);
      STPQ := STX + DX / ((FX - FP) / (Stp - STX) + DX) / 2 * (Stp - STX);
      if AP_FP_Less(AbsReal(STPC - STX), AbsReal(STPQ - STX)) then
        begin
          STPF := STPC;
        end
      else
        begin
          STPF := STPC + (STPQ - STPC) / 2;
        end;
      BRACKT := True;
    end
  else
    begin
      if AP_FP_Less(SGND, 0) then
        begin

          //
          // SECOND CASE. A LOWER FUNCTION VALUE AND DERIVATIVES OF
          // OPPOSITE SIGN. THE MINIMUM IS BRACKETED. IF THE CUBIC
          // STEP IS CLOSER TO STX THAN THE QUADRATIC (SECANT) STEP,
          // THE CUBIC STEP IS TAKEN, ELSE THE QUADRATIC STEP IS TAKEN.
          //
          INFO := 2;
          BOUND := False;
          THETA := 3 * (FX - FP) / (Stp - STX) + DX + DP;
          S := Max(AbsReal(THETA), Max(AbsReal(DX), AbsReal(DP)));
          GAMMA := S * Sqrt(AP_Sqr(THETA / S) - DX / S * (DP / S));
          if AP_FP_Greater(Stp, STX) then
            begin
              GAMMA := -GAMMA;
            end;
          P := GAMMA - DP + THETA;
          Q := GAMMA - DP + GAMMA + DX;
          R := P / Q;
          STPC := Stp + R * (STX - Stp);
          STPQ := Stp + DP / (DP - DX) * (STX - Stp);
          if AP_FP_Greater(AbsReal(STPC - Stp), AbsReal(STPQ - Stp)) then
            begin
              STPF := STPC;
            end
          else
            begin
              STPF := STPQ;
            end;
          BRACKT := True;
        end
      else
        begin
          if AP_FP_Less(AbsReal(DP), AbsReal(DX)) then
            begin

              //
              // THIRD CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
              // SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DECREASES.
              // THE CUBIC STEP IS ONLY USED IF THE CUBIC TENDS TO INFINITY
              // IN THE DIRECTION OF THE STEP OR IF THE MINIMUM OF THE CUBIC
              // IS BEYOND STP. OTHERWISE THE CUBIC STEP IS DEFINED TO BE
              // EITHER LM_STPMIN OR STPMAX. THE QUADRATIC (SECANT) STEP IS ALSO
              // COMPUTED AND IF THE MINIMUM IS BRACKETED THEN THE THE STEP
              // CLOSEST TO STX IS TAKEN, ELSE THE STEP FARTHEST AWAY IS TAKEN.
              //
              INFO := 3;
              BOUND := True;
              THETA := 3 * (FX - FP) / (Stp - STX) + DX + DP;
              S := Max(AbsReal(THETA), Max(AbsReal(DX), AbsReal(DP)));

              //
              // THE CASE GAMMA = 0 ONLY ARISES IF THE CUBIC DOES NOT TEND
              // TO INFINITY IN THE DIRECTION OF THE STEP.
              //
              GAMMA := S * Sqrt(Max(0, AP_Sqr(THETA / S) - DX / S * (DP / S)));
              if AP_FP_Greater(Stp, STX) then
                begin
                  GAMMA := -GAMMA;
                end;
              P := GAMMA - DP + THETA;
              Q := GAMMA + (DX - DP) + GAMMA;
              R := P / Q;
              if AP_FP_Less(R, 0) and AP_FP_Neq(GAMMA, 0) then
                begin
                  STPC := Stp + R * (STX - Stp);
                end
              else
                begin
                  if AP_FP_Greater(Stp, STX) then
                    begin
                      STPC := STMAX;
                    end
                  else
                    begin
                      STPC := STMIN;
                    end;
                end;
              STPQ := Stp + DP / (DP - DX) * (STX - Stp);
              if BRACKT then
                begin
                  if AP_FP_Less(AbsReal(Stp - STPC), AbsReal(Stp - STPQ)) then
                    begin
                      STPF := STPC;
                    end
                  else
                    begin
                      STPF := STPQ;
                    end;
                end
              else
                begin
                  if AP_FP_Greater(AbsReal(Stp - STPC), AbsReal(Stp - STPQ)) then
                    begin
                      STPF := STPC;
                    end
                  else
                    begin
                      STPF := STPQ;
                    end;
                end;
            end
          else
            begin

              //
              // FOURTH CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
              // SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DOES
              // NOT DECREASE. IF THE MINIMUM IS NOT BRACKETED, THE STEP
              // IS EITHER LM_STPMIN OR STPMAX, ELSE THE CUBIC STEP IS TAKEN.
              //
              INFO := 4;
              BOUND := False;
              if BRACKT then
                begin
                  THETA := 3 * (FP - FY) / (STY - Stp) + DY + DP;
                  S := Max(AbsReal(THETA), Max(AbsReal(DY), AbsReal(DP)));
                  GAMMA := S * Sqrt(AP_Sqr(THETA / S) - DY / S * (DP / S));
                  if AP_FP_Greater(Stp, STY) then
                    begin
                      GAMMA := -GAMMA;
                    end;
                  P := GAMMA - DP + THETA;
                  Q := GAMMA - DP + GAMMA + DY;
                  R := P / Q;
                  STPC := Stp + R * (STY - Stp);
                  STPF := STPC;
                end
              else
                begin
                  if AP_FP_Greater(Stp, STX) then
                    begin
                      STPF := STMAX;
                    end
                  else
                    begin
                      STPF := STMIN;
                    end;
                end;
            end;
        end;
    end;

  //
  // UPDATE THE INTERVAL OF UNCERTAINTY. THIS UPDATE DOES NOT
  // DEPEND ON THE NEW STEP OR THE CASE ANALYSIS ABOVE.
  //
  if AP_FP_Greater(FP, FX) then
    begin
      STY := Stp;
      FY := FP;
      DY := DP;
    end
  else
    begin
      if AP_FP_Less(SGND, 0.0) then
        begin
          STY := STX;
          FY := FX;
          DY := DX;
        end;
      STX := Stp;
      FX := FP;
      DX := DP;
    end;

  //
  // COMPUTE THE NEW STEP AND SAFEGUARD IT.
  //
  STPF := Min(STMAX, STPF);
  STPF := Max(STMIN, STPF);
  Stp := STPF;
  if BRACKT and BOUND then
    begin
      if AP_FP_Greater(STY, STX) then
        begin
          Stp := Min(STX + 0.66 * (STY - STX), Stp);
        end
      else
        begin
          Stp := Max(STX + 0.66 * (STY - STX), Stp);
        end;
    end;
end;
