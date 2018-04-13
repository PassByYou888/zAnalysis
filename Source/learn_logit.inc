type
  TLogitModel = packed record
    W: TLVec;
  end;

  PLogitModel = ^TLogitModel;

  TLogitMCState = packed record
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

  (* ************************************************************************
    MNLReport structure contains information about training process:
    * NGrad     -   number of gradient calculations
    * NHess     -   number of Hessian calculations
    ************************************************************************ *)
  TMNLReport = packed record
    NGrad: TLInt;
    NHess: TLInt;
  end;

procedure MNLTrainH(const XY: TLMatrix; NPoints: TLInt; NVars: TLInt; NClasses: TLInt;
  var Info: TLInt; var LM: TLogitModel; var Rep: TMNLReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MNLProcess(var LM: TLogitModel; const X: TLVec; var Y: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MNLUnpack(const LM: TLogitModel; var A: TLMatrix; var NVars: TLInt; var NClasses: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MNLPack(const A: TLMatrix; NVars: TLInt; NClasses: TLInt; var LM: TLogitModel); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MNLCopy(const LM1: TLogitModel; var LM2: TLogitModel); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MNLSerialize(const LM: TLogitModel; var RA: TLVec; var RLen: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MNLUnserialize(const RA: TLVec; var LM: TLogitModel); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MNLAvgCE(var LM: TLogitModel; const XY: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MNLRelClsError(var LM: TLogitModel; const XY: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MNLRMSError(var LM: TLogitModel; const XY: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MNLAvgError(var LM: TLogitModel; const XY: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MNLAvgRelError(var LM: TLogitModel; const XY: TLMatrix; SSize: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MNLClsError(var LM: TLogitModel; const XY: TLMatrix; NPoints: TLInt): TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


const
  LogitXTOL   = 100 * MachineEpsilon;
  LogitFTOL   = 0.0001;
  LogitGTOL   = 0.3;
  LogitMAXFEV = 20;
  LogitSTPMIN = 1.0E-2;
  LogitSTPMAX = 1.0E5;
  LogitVNum   = 6;

procedure MNLIExp(var W: TLVec; const X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MNLAllErrors(var LM: TLogitModel; const XY: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MNLMCSRCH(const N: TLInt; var X: TLVec;
  var F: TLFloat; var G: TLVec; const S: TLVec;
  var STP: TLFloat; var Info: TLInt; var NFEV: TLInt;
  var WA: TLVec; var State: TLogitMCState;
  var Stage: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MNLMCSTEP(var STX: TLFloat; var FX: TLFloat;
  var DX: TLFloat; var STY: TLFloat; var FY: TLFloat;
  var DY: TLFloat; var STP: TLFloat; const FP: TLFloat;
  const DP: TLFloat; var BRACKT: Boolean; const STMIN: TLFloat;
  const STMAX: TLFloat; var Info: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  //  This subroutine trains logit model.
  //
  //  INPUT PARAMETERS:
  //  XY          -   training set, array[0..NPoints-1,0..NVars]
  //                  First NVars columns store values of independent
  //                  variables, next column stores number of class (from 0 to NClasses-1) which dataset element belongs to. Fractional
  //                  values are rounded to nearest integer.
  //  NPoints     -   training set size, NPoints>=1
  //  NVars       -   number of independent variables, NVars>=1
  //  NClasses    -   number of classes, NClasses>=2
  //
  //  OUTPUT PARAMETERS:
  //  Info        -   return code:
  //  * -2, if there is a point with class number outside of [0..NClasses-1].
  //  * -1, if incorrect parameters was passed (NPoints<NVars+2, NVars<1, NClasses<2).
  //  *  1, if task has been solved
  //  LM          -   model built
  //  Rep         -   training report
  ************************************************************************ *)
procedure MNLTrainH(const XY: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; var Info: TLInt;
  var LM: TLogitModel; var Rep: TMNLReport);
var
  I         : TLInt;
  J         : TLInt;
  K         : TLInt;
  SSize     : TLInt;
  AllSame   : Boolean;
  Offs      : TLInt;
  Threshold : TLFloat;
  WMinStep  : TLFloat;
  Decay     : TLFloat;
  WDim      : TLInt;
  ExpOffs   : TLInt;
  V         : TLFloat;
  S         : TLFloat;
  Network   : TMultiLayerPerceptron;
  NIn       : TLInt;
  NOut      : TLInt;
  WCount    : TLInt;
  E         : TLFloat;
  G         : TLVec;
  H         : TLMatrix;
  SPD       : Boolean;
  X         : TLVec;
  Y         : TLVec;
  WBase     : TLVec;
  WStep     : TLFloat;
  WDir      : TLVec;
  WORK      : TLVec;
  MCStage   : TLInt;
  MCState   : TLogitMCState;
  MCInfo    : TLInt;
  MCNFEV    : TLInt;
  SolverInfo: TLInt;
  SolverRep : TDenseSolverReport;
begin
  Threshold := 1000 * MachineEpsilon;
  WMinStep := 0.001;
  Decay := 0.001;

  //
  // Test for inputs
  //
  if (NPoints < NVars + 2) or (NVars < 1) or (NClasses < 2) then
    begin
      Info := -1;
      Exit;
    end;
  I := 0;
  while I <= NPoints - 1 do
    begin
      if (Round(XY[I, NVars]) < 0) or (Round(XY[I, NVars]) >= NClasses) then
        begin
          Info := -2;
          Exit;
        end;
      Inc(I);
    end;
  Info := 1;

  //
  // Initialize data
  //
  Rep.NGrad := 0;
  Rep.NHess := 0;

  //
  // Allocate array
  //
  WDim := (NVars + 1) * (NClasses - 1);
  Offs := 5;
  ExpOffs := Offs + WDim;
  SSize := 5 + (NVars + 1) * (NClasses - 1) + NClasses;
  SetLength(LM.W, SSize (* - 1 + 1 // optimized compiler *) );
  LM.W[0] := SSize;
  LM.W[1] := LogitVNum;
  LM.W[2] := NVars;
  LM.W[3] := NClasses;
  LM.W[4] := Offs;

  //
  // Degenerate case: all outputs are equal
  //
  AllSame := True;
  I := 1;
  while I <= NPoints - 1 do
    begin
      if Round(XY[I, NVars]) <> Round(XY[I - 1, NVars]) then
        begin
          AllSame := False;
        end;
      Inc(I);
    end;
  if AllSame then
    begin
      I := 0;
      while I <= (NVars + 1) * (NClasses - 1) - 1 do
        begin
          LM.W[Offs + I] := 0;
          Inc(I);
        end;
      V := -2 * Ln(MinRealNumber);
      K := Round(XY[0, NVars]);
      if K = NClasses - 1 then
        begin
          I := 0;
          while I <= NClasses - 2 do
            begin
              LM.W[Offs + I * (NVars + 1) + NVars] := -V;
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= NClasses - 2 do
            begin
              if I = K then
                begin
                  LM.W[Offs + I * (NVars + 1) + NVars] := +V;
                end
              else
                begin
                  LM.W[Offs + I * (NVars + 1) + NVars] := 0;
                end;
              Inc(I);
            end;
        end;
      Exit;
    end;

  //
  // General case.
  // Prepare task and network. Allocate space.
  //
  MLPCreateC0(NVars, NClasses, Network);
  MLPInitPreprocessor(Network, XY, NPoints);
  MLPProperties(Network, NIn, NOut, WCount);
  I := 0;
  while I <= WCount - 1 do
    begin
      Network.Weights[I] := (2 * RandomReal - 1) / NVars;
      Inc(I);
    end;
  SetLength(G, WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(H, WCount (* - 1 + 1 // optimized compiler *) , WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(WBase, WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(WDir, WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(WORK, WCount (* - 1 + 1 // optimized compiler *) );

  //
  // First stage: optimize in gradient direction.
  //
  K := 0;
  while K <= WCount div 3 + 10 do
    begin

      //
      // Calculate gradient in starting point
      //
      MLPGradNBatch(Network, XY, NPoints, E, G);
      V := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      E := E + 0.5 * Decay * V;
      APVAdd(@G[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
      Rep.NGrad := Rep.NGrad + 1;

      //
      // Setup optimization scheme
      //
      APVMoveNeg(@WDir[0], 0, WCount - 1, @G[0], 0, WCount - 1);
      V := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
      WStep := Sqrt(V);
      V := 1 / Sqrt(V);
      APVMul(@WDir[0], 0, WCount - 1, V);
      MCStage := 0;
      MNLMCSRCH(WCount, Network.Weights, E, G, WDir, WStep, MCInfo, MCNFEV, WORK, MCState, MCStage);
      while MCStage <> 0 do
        begin
          MLPGradNBatch(Network, XY, NPoints, E, G);
          V := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          E := E + 0.5 * Decay * V;
          APVAdd(@G[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
          Rep.NGrad := Rep.NGrad + 1;
          MNLMCSRCH(WCount, Network.Weights, E, G, WDir, WStep, MCInfo, MCNFEV, WORK, MCState, MCStage);
        end;
      Inc(K);
    end;

  //
  // Second stage: use Hessian when we are close to the minimum
  //
  while True do
    begin

      //
      // Calculate and update E/G/H
      //
      MLPHessianNBatch(Network, XY, NPoints, E, G, H);
      V := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      E := E + 0.5 * Decay * V;
      APVAdd(@G[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
      K := 0;
      while K <= WCount - 1 do
        begin
          H[K, K] := H[K, K] + Decay;
          Inc(K);
        end;
      Rep.NHess := Rep.NHess + 1;

      //
      // Select step direction
      // NOTE: it is important to use lower-triangle Cholesky
      // factorization since it is much faster than higher-triangle version.
      //
      SPD := SPDMatrixCholesky(H, WCount, False);
      SPDMatrixCholeskySolve(H, WCount, False, G, SolverInfo, SolverRep, WDir);
      SPD := SolverInfo > 0;
      if SPD then
        begin

          //
          // H is positive definite.
          // Step in Newton direction.
          //
          APVMul(@WDir[0], 0, WCount - 1, -1);
          SPD := True;
        end
      else
        begin

          //
          // H is indefinite.
          // Step in gradient direction.
          //
          APVMoveNeg(@WDir[0], 0, WCount - 1, @G[0], 0, WCount - 1);
          SPD := False;
        end;

      //
      // Optimize in WDir direction
      //
      V := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
      WStep := Sqrt(V);
      V := 1 / Sqrt(V);
      APVMul(@WDir[0], 0, WCount - 1, V);
      MCStage := 0;
      MNLMCSRCH(WCount, Network.Weights, E, G, WDir, WStep, MCInfo, MCNFEV, WORK, MCState, MCStage);
      while MCStage <> 0 do
        begin
          MLPGradNBatch(Network, XY, NPoints, E, G);
          V := APVDotProduct(@Network.Weights[0], 0, WCount - 1,
            @Network.Weights[0], 0, WCount - 1);
          E := E + 0.5 * Decay * V;
          APVAdd(@G[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
          Rep.NGrad := Rep.NGrad + 1;
          MNLMCSRCH(WCount, Network.Weights, E, G, WDir, WStep, MCInfo, MCNFEV, WORK, MCState, MCStage);
        end;
      if SPD and ((MCInfo = 2) or (MCInfo = 4) or (MCInfo = 6)) then
        begin
          Break;
        end;
    end;

  //
  // Convert from NN format to MNL format
  //
  APVMove(@LM.W[0], Offs, Offs + WCount - 1, @Network.Weights[0], 0,
    WCount - 1);
  K := 0;
  while K <= NVars - 1 do
    begin
      I := 0;
      while I <= NClasses - 2 do
        begin
          S := Network.ColumnSigmas[K];
          if AP_FP_Eq(S, 0) then
            begin
              S := 1;
            end;
          J := Offs + (NVars + 1) * I;
          V := LM.W[J + K];
          LM.W[J + K] := V / S;
          LM.W[J + NVars] := LM.W[J + NVars] + V * Network.ColumnMeans[K] / S;
          Inc(I);
        end;
      Inc(K);
    end;
  K := 0;
  while K <= NClasses - 2 do
    begin
      LM.W[Offs + (NVars + 1) * K + NVars] :=
        -LM.W[Offs + (NVars + 1) * K + NVars];
      Inc(K);
    end;
end;

(* ************************************************************************
  Procesing

  INPUT PARAMETERS:
  LM      -   logit model, passed by non-constant reference  (some fields of structure are used as temporaries  when calculating model output).
  X       -   input vector,  array[0..NVars-1].

  OUTPUT PARAMETERS:
  Y       -   result, array[0..NClasses-1]
  Vector of posterior probabilities for classification task.
  Subroutine does not allocate memory for this vector, it is
  responsibility of a caller to allocate it. Array  must  be
  at least [0..NClasses-1].
  ************************************************************************ *)
procedure MNLProcess(var LM: TLogitModel; const X: TLVec; var Y: TLVec);
var
  NVars   : TLInt;
  NClasses: TLInt;
  Offs    : TLInt;
  I       : TLInt;
  I1      : TLInt;
  S       : TLFloat;
begin
  Assert(AP_FP_Eq(LM.W[1], LogitVNum), 'MNLProcess: unexpected model version');
  NVars := Round(LM.W[2]);
  NClasses := Round(LM.W[3]);
  Offs := Round(LM.W[4]);
  MNLIExp(LM.W, X);
  S := 0;
  I1 := Offs + (NVars + 1) * (NClasses - 1);
  I := I1;
  while I <= I1 + NClasses - 1 do
    begin
      S := S + LM.W[I];
      Inc(I);
    end;
  I := 0;
  while I <= NClasses - 1 do
    begin
      Y[I] := LM.W[I1 + I] / S;
      Inc(I);
    end;
end;

(* ************************************************************************
  Unpacks coefficients of logit model. Logit model have form:

  P(class=i) = S(i) / (S(0) + S(1) + ... +S(M-1))
  S(i) = Exp(A[i,0]*X[0] + ... + A[i,N-1]*X[N-1] + A[i,N]), when i<M-1
  S(M-1) = 1

  INPUT PARAMETERS:
  LM          -   logit model in ALGLIB format

  OUTPUT PARAMETERS:
  V           -   coefficients, array[0..NClasses-2,0..NVars]
  NVars       -   number of independent variables
  NClasses    -   number of classes
  ************************************************************************ *)
procedure MNLUnpack(const LM: TLogitModel; var A: TLMatrix;
  var NVars: TLInt; var NClasses: TLInt);
var
  Offs: TLInt;
  I   : TLInt;
begin
  Assert(AP_FP_Eq(LM.W[1], LogitVNum), 'MNLUnpack: unexpected model version');
  NVars := Round(LM.W[2]);
  NClasses := Round(LM.W[3]);
  Offs := Round(LM.W[4]);
  SetLength(A, NClasses - 2 + 1, NVars + 1);
  I := 0;
  while I <= NClasses - 2 do
    begin
      APVMove(@A[I][0], 0, NVars, @LM.W[0], Offs + I * (NVars + 1),
        Offs + I * (NVars + 1) + NVars);
      Inc(I);
    end;
end;

(* ************************************************************************
  "Packs" coefficients and creates logit model in ALGLIB format (MNLUnpack reversed).

  INPUT PARAMETERS:
  A           -   model (see MNLUnpack)
  NVars       -   number of independent variables
  NClasses    -   number of classes

  OUTPUT PARAMETERS:
  LM          -   logit model.
  ************************************************************************ *)
procedure MNLPack(const A: TLMatrix; NVars: TLInt;
  NClasses: TLInt; var LM: TLogitModel);
var
  Offs : TLInt;
  I    : TLInt;
  WDim : TLInt;
  SSize: TLInt;
begin
  WDim := (NVars + 1) * (NClasses - 1);
  Offs := 5;
  SSize := 5 + (NVars + 1) * (NClasses - 1) + NClasses;
  SetLength(LM.W, SSize (* - 1 + 1 // optimized compiler *) );
  LM.W[0] := SSize;
  LM.W[1] := LogitVNum;
  LM.W[2] := NVars;
  LM.W[3] := NClasses;
  LM.W[4] := Offs;
  I := 0;
  while I <= NClasses - 2 do
    begin
      APVMove(@LM.W[0], Offs + I * (NVars + 1), Offs + I * (NVars + 1) + NVars,
        @A[I][0], 0, NVars);
      Inc(I);
    end;
end;

(* ************************************************************************
  Copying of TLogitModel strucure

  INPUT PARAMETERS:
  LM1 -   original

  OUTPUT PARAMETERS:
  LM2 -   copy
  ************************************************************************ *)
procedure MNLCopy(const LM1: TLogitModel; var LM2: TLogitModel);
var
  K: TLInt;
begin
  K := Round(LM1.W[0]);
  SetLength(LM2.W, K (* - 1 + 1 // optimized compiler *) );
  APVMove(@LM2.W[0], 0, K - 1, @LM1.W[0], 0, K - 1);
end;

(* ************************************************************************
  Serialization of TLogitModel strucure

  INPUT PARAMETERS:
  LM      -   original

  OUTPUT PARAMETERS:
  RA      -   packed array of real numbers which stores model,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure MNLSerialize(const LM: TLogitModel; var RA: TLVec;
  var RLen: TLInt);
begin
  RLen := Round(LM.W[0]) + 1;
  SetLength(RA, RLen (* - 1 + 1 // optimized compiler *) );
  RA[0] := LogitVNum;
  APVMove(@RA[0], 1, RLen - 1, @LM.W[0], 0, RLen - 2);
end;

(* ************************************************************************
  Unserialization of TLogitModel strucure

  INPUT PARAMETERS:
  RA      -   real array which stores model

  OUTPUT PARAMETERS:
  LM      -   restored model
  ************************************************************************ *)
procedure MNLUnserialize(const RA: TLVec; var LM: TLogitModel);
begin
  Assert(Round(RA[0]) = LogitVNum, 'MNLUnserialize: incorrect array!');
  SetLength(LM.W, Round(RA[1]) (* - 1 + 1 // optimized compiler *) );
  APVMove(@LM.W[0], 0, Round(RA[1]) - 1, @RA[0], 1, Round(RA[1]));
end;

(* ************************************************************************
  Average cross-entropy (in bits per element) on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  CrossEntropy/(NPoints*ln(2)).
  ************************************************************************ *)
function MNLAvgCE(var LM: TLogitModel; const XY: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  NVars   : TLInt;
  NClasses: TLInt;
  I       : TLInt;
  WorkX   : TLVec;
  WorkY   : TLVec;
begin
  Assert(AP_FP_Eq(LM.W[1], LogitVNum), 'MNLClsError: unexpected model version');
  NVars := Round(LM.W[2]);
  NClasses := Round(LM.W[3]);
  SetLength(WorkX, NVars (* - 1 + 1 // optimized compiler *) );
  SetLength(WorkY, NClasses (* - 1 + 1 // optimized compiler *) );
  Result := 0;
  I := 0;
  while I <= NPoints - 1 do
    begin
      Assert((Round(XY[I, NVars]) >= 0) and (Round(XY[I, NVars]) < NClasses),
        'MNLAvgCE: incorrect class number!');

      //
      // Process
      //
      APVMove(@WorkX[0], 0, NVars - 1, @XY[I][0], 0, NVars - 1);
      MNLProcess(LM, WorkX, WorkY);
      if AP_FP_Greater(WorkY[Round(XY[I, NVars])], 0) then
        begin
          Result := Result - Ln(WorkY[Round(XY[I, NVars])]);
        end
      else
        begin
          Result := Result - Ln(MinRealNumber);
        end;
      Inc(I);
    end;
  Result := Result / (NPoints * Ln(2));
end;

(* ************************************************************************
  Relative classification error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  percent of incorrectly classified cases.
  ************************************************************************ *)
function MNLRelClsError(var LM: TLogitModel; const XY: TLMatrix;
  NPoints: TLInt): TLFloat;
begin
  Result := AP_Float(MNLClsError(LM, XY, NPoints)) / NPoints;
end;

(* ************************************************************************
  RMS error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  root mean square error (error when estimating posterior probabilities).
  ************************************************************************ *)
function MNLRMSError(var LM: TLogitModel; const XY: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  Assert(Round(LM.W[1]) = LogitVNum, 'MNLRMSError: Incorrect MNL version!');
  MNLAllErrors(LM, XY, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := RMS;
end;

(* ************************************************************************
  Average error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  average error (error when estimating posterior probabilities).
  ************************************************************************ *)
function MNLAvgError(var LM: TLogitModel; const XY: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  Assert(Round(LM.W[1]) = LogitVNum, 'MNLRMSError: Incorrect MNL version!');
  MNLAllErrors(LM, XY, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := Avg;
end;

(* ************************************************************************
  Average relative error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  average relative error (error when estimating posterior probabilities).
  ************************************************************************ *)
function MNLAvgRelError(var LM: TLogitModel; const XY: TLMatrix;
  SSize: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  Assert(Round(LM.W[1]) = LogitVNum, 'MNLRMSError: Incorrect MNL version!');
  MNLAllErrors(LM, XY, SSize, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := AvgRel;
end;

(* ************************************************************************
  Classification error on test set = MNLRelClsError*NPoints
  ************************************************************************ *)
function MNLClsError(var LM: TLogitModel; const XY: TLMatrix;
  NPoints: TLInt): TLInt;
var
  NVars   : TLInt;
  NClasses: TLInt;
  I       : TLInt;
  J       : TLInt;
  WorkX   : TLVec;
  WorkY   : TLVec;
  NMAX    : TLInt;
begin
  Assert(AP_FP_Eq(LM.W[1], LogitVNum), 'MNLClsError: unexpected model version');
  NVars := Round(LM.W[2]);
  NClasses := Round(LM.W[3]);
  SetLength(WorkX, NVars (* - 1 + 1 // optimized compiler *) );
  SetLength(WorkY, NClasses (* - 1 + 1 // optimized compiler *) );
  Result := 0;
  I := 0;
  while I <= NPoints - 1 do
    begin

      //
      // Process
      //
      APVMove(@WorkX[0], 0, NVars - 1, @XY[I][0], 0, NVars - 1);
      MNLProcess(LM, WorkX, WorkY);

      //
      // Logit version of the answer
      //
      NMAX := 0;
      J := 0;
      while J <= NClasses - 1 do
        begin
          if AP_FP_Greater(WorkY[J], WorkY[NMAX]) then
            begin
              NMAX := J;
            end;
          Inc(J);
        end;

      //
      // compare
      //
      if NMAX <> Round(XY[I, NVars]) then
        begin
          Result := Result + 1;
        end;
      Inc(I);
    end;
end;

(* ************************************************************************
  Internal subroutine. Places exponents of the anti-overflow shifted
  internal linear outputs into the service part of the W array.
  ************************************************************************ *)
procedure MNLIExp(var W: TLVec; const X: TLVec);
var
  NVars   : TLInt;
  NClasses: TLInt;
  Offs    : TLInt;
  I       : TLInt;
  I1      : TLInt;
  V       : TLFloat;
  MX      : TLFloat;
begin
  Assert(AP_FP_Eq(W[1], LogitVNum), 'LOGIT: unexpected model version');
  NVars := Round(W[2]);
  NClasses := Round(W[3]);
  Offs := Round(W[4]);
  I1 := Offs + (NVars + 1) * (NClasses - 1);
  I := 0;
  while I <= NClasses - 2 do
    begin
      V := APVDotProduct(@W[0], Offs + I * (NVars + 1), Offs + I * (NVars + 1) +
        NVars - 1, @X[0], 0, NVars - 1);
      W[I1 + I] := V + W[Offs + I * (NVars + 1) + NVars];
      Inc(I);
    end;
  W[I1 + NClasses - 1] := 0;
  MX := 0;
  I := I1;
  while I <= I1 + NClasses - 1 do
    begin
      MX := Max(MX, W[I]);
      Inc(I);
    end;
  I := I1;
  while I <= I1 + NClasses - 1 do
    begin
      W[I] := Exp(W[I] - MX);
      Inc(I);
    end;
end;

(* ************************************************************************
  Calculation of all types of errors
  ************************************************************************ *)
procedure MNLAllErrors(var LM: TLogitModel; const XY: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat);
var
  NVars   : TLInt;
  NClasses: TLInt;
  I       : TLInt;
  Buf     : TLVec;
  WorkX   : TLVec;
  Y       : TLVec;
  DY      : TLVec;
begin
  Assert(Round(LM.W[1]) = LogitVNum, 'MNL unit: Incorrect MNL version!');
  NVars := Round(LM.W[2]);
  NClasses := Round(LM.W[3]);
  SetLength(WorkX, NVars (* - 1 + 1 // optimized compiler *) );
  SetLength(Y, NClasses (* - 1 + 1 // optimized compiler *) );
  SetLength(DY, 0 + 1);
  DSErrAllocate(NClasses, Buf);
  I := 0;
  while I <= NPoints - 1 do
    begin
      APVMove(@WorkX[0], 0, NVars - 1, @XY[I][0], 0, NVars - 1);
      MNLProcess(LM, WorkX, Y);
      DY[0] := XY[I, NVars];
      DSErrAccumulate(Buf, Y, DY);
      Inc(I);
    end;
  DSErrFinish(Buf);
  RelCls := Buf[0];
  AvgCE := Buf[1];
  RMS := Buf[2];
  Avg := Buf[3];
  AvgRel := Buf[4];
end;

(* ************************************************************************
  THE  PURPOSE  OF  MCSRCH  IS  TO  FIND A STEP WHICH SATISFIES A SUFFICIENT
  DECREASE CONDITION AND A CURVATURE CONDITION.

  AT EACH STAGE THE SUBROUTINE  UPDATES  AN  INTERVAL  OF  UNCERTAINTY  WITH
  ENDPOINTS  STX  AND  STY.  THE INTERVAL OF UNCERTAINTY IS INITIALLY CHOSEN
  SO THAT IT CONTAINS A MINIMIZER OF THE MODIFIED FUNCTION

  F(X+STP*S) - F(X) - LogitFTOL*STP*(GRADF(X)'S).

  IF  A STEP  IS OBTAINED FOR  WHICH THE MODIFIED FUNCTION HAS A NONPOSITIVE
  FUNCTION  VALUE  AND  NONNEGATIVE  DERIVATIVE,   THEN   THE   INTERVAL  OF
  UNCERTAINTY IS CHOSEN SO THAT IT CONTAINS A MINIMIZER OF F(X+STP*S).

  THE  ALGORITHM  IS  DESIGNED TO FIND A STEP WHICH SATISFIES THE SUFFICIENT
  DECREASE CONDITION

  F(X+STP*S) .LE. F(X) + LogitFTOL*STP*(GRADF(X)'S),

  AND THE CURVATURE CONDITION

  ABS(GRADF(X+STP*S)'S)) .LE. LogitGTOL*ABS(GRADF(X)'S).

  IF  LogitFTOL  IS  LESS  THAN LogitGTOL AND IF, FOR EXAMPLE, THE FUNCTION IS BOUNDED
  BELOW,  THEN  THERE  IS  ALWAYS  A  STEP  WHICH SATISFIES BOTH CONDITIONS.
  IF  NO  STEP  CAN BE FOUND  WHICH  SATISFIES  BOTH  CONDITIONS,  THEN  THE
  ALGORITHM  USUALLY STOPS  WHEN  ROUNDING ERRORS  PREVENT FURTHER PROGRESS.
  IN THIS CASE STP ONLY SATISFIES THE SUFFICIENT DECREASE CONDITION.

  PARAMETERS DESCRIPRION

  N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER OF VARIABLES.

  X IS  AN  ARRAY  OF  LENGTH N. ON INPUT IT MUST CONTAIN THE BASE POINT FOR
  THE LINE SEARCH. ON OUTPUT IT CONTAINS X+STP*S.

  F IS  A  VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F AT X. ON OUTPUT
  IT CONTAINS THE VALUE OF F AT X + STP*S.

  G IS AN packed array of LENGTH N. ON INPUT IT MUST CONTAIN THE GRADIENT OF F AT X.
  ON OUTPUT IT CONTAINS THE GRADIENT OF F AT X + STP*S.

  S IS AN INPUT packed array of LENGTH N WHICH SPECIFIES THE SEARCH DIRECTION.

  STP  IS  A NONNEGATIVE VARIABLE. ON INPUT STP CONTAINS AN INITIAL ESTIMATE
  OF A SATISFACTORY STEP. ON OUTPUT STP CONTAINS THE FINAL ESTIMATE.

  LogitFTOL AND LogitGTOL ARE NONNEGATIVE INPUT VARIABLES. TERMINATION OCCURS WHEN THE
  SUFFICIENT DECREASE CONDITION AND THE DIRECTIONAL DERIVATIVE CONDITION ARE
  SATISFIED.

  LogitXTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS WHEN THE RELATIVE
  WIDTH OF THE INTERVAL OF UNCERTAINTY IS AT MOST LogitXTOL.

  LogitSTPMIN AND LogitSTPMAX ARE NONNEGATIVE INPUT VARIABLES WHICH SPECIFY LOWER  AND
  UPPER BOUNDS FOR THE STEP.

  LogitMAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION OCCURS WHEN THE
  NUMBER OF CALLS TO FCN IS AT LEAST LogitMAXFEV BY THE END OF AN ITERATION.

  INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
  INFO = 0  IMPROPER INPUT PARAMETERS.

  INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
  DIRECTIONAL DERIVATIVE CONDITION HOLD.

  INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY
  IS AT MOST LogitXTOL.

  INFO = 3  NUMBER OF CALLS TO FCN HAS REACHED LogitMAXFEV.

  INFO = 4  THE STEP IS AT THE LOWER BOUND LogitSTPMIN.

  INFO = 5  THE STEP IS AT THE UPPER BOUND LogitSTPMAX.

  INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
  THERE MAY NOT BE A STEP WHICH SATISFIES THE
  SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
  TOLERANCES MAY BE TOO SMALL.

  NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF CALLS TO FCN.

  WA IS A WORK packed array of LENGTH N.

  ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
  JORGE J. MORE', DAVID J. THUENTE
  ************************************************************************ *)
procedure MNLMCSRCH(const N: TLInt; var X: TLVec;
  var F: TLFloat; var G: TLVec; const S: TLVec;
  var STP: TLFloat; var Info: TLInt; var NFEV: TLInt;
  var WA: TLVec; var State: TLogitMCState; var Stage: TLInt);
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
          Info := 0;

          //
          // CHECK THE INPUT PARAMETERS FOR ERRORS.
          //
          if (N <= 0) or AP_FP_Less_Eq(STP, 0) or AP_FP_Less(LogitFTOL, 0) or
            AP_FP_Less(LogitGTOL, ZERO) or AP_FP_Less(LogitXTOL, ZERO) or
            AP_FP_Less(LogitSTPMIN, ZERO) or AP_FP_Less(LogitSTPMAX, LogitSTPMIN) or (LogitMAXFEV <= 0)
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
          State.DGTEST := LogitFTOL * State.DGINIT;
          State.WIDTH := LogitSTPMAX - LogitSTPMIN;
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
              State.STMAX := STP + State.XTRAPF * (STP - State.STX);
            end;

          //
          // FORCE THE STEP TO BE WITHIN THE BOUNDS LogitSTPMAX AND LogitSTPMIN.
          //
          if AP_FP_Greater(STP, LogitSTPMAX) then
            begin
              STP := LogitSTPMAX;
            end;
          if AP_FP_Less(STP, LogitSTPMIN) then
            begin
              STP := LogitSTPMIN;
            end;

          //
          // IF AN UNUSUAL TERMINATION IS TO OCCUR THEN LET
          // STP BE THE LOWEST POINT OBTAINED SO FAR.
          //
          if State.BRACKT and (AP_FP_Less_Eq(STP, State.STMIN) or
            AP_FP_Greater_Eq(STP, State.STMAX)) or (NFEV >= LogitMAXFEV - 1) or
            (State.INFOC = 0) or State.BRACKT and
            AP_FP_Less_Eq(State.STMAX - State.STMIN, LogitXTOL * State.STMAX) then
            begin
              STP := State.STX;
            end;

          //
          // EVALUATE THE FUNCTION AND GRADIENT AT STP
          // AND COMPUTE THE DIRECTIONAL DERIVATIVE.
          //
          APVMove(@X[0], 0, N - 1, @WA[0], 0, N - 1);
          APVAdd(@X[0], 0, N - 1, @S[0], 0, N - 1, STP);

          //
          // NEXT
          //
          Stage := 4;
          Exit;
        end;
      if Stage = 4 then
        begin
          Info := 0;
          NFEV := NFEV + 1;
          V := APVDotProduct(@G[0], 0, N - 1, @S[0], 0, N - 1);
          State.DG := V;
          State.FTEST1 := State.FINIT + STP * State.DGTEST;

          //
          // TEST FOR CONVERGENCE.
          //
          if State.BRACKT and (AP_FP_Less_Eq(STP, State.STMIN) or
            AP_FP_Greater_Eq(STP, State.STMAX)) or (State.INFOC = 0) then
            begin
              Info := 6;
            end;
          if AP_FP_Eq(STP, LogitSTPMAX) and AP_FP_Less_Eq(F, State.FTEST1) and
            AP_FP_Less_Eq(State.DG, State.DGTEST) then
            begin
              Info := 5;
            end;
          if AP_FP_Eq(STP, LogitSTPMIN) and (AP_FP_Greater(F, State.FTEST1) or
            AP_FP_Greater_Eq(State.DG, State.DGTEST)) then
            begin
              Info := 4;
            end;
          if NFEV >= LogitMAXFEV then
            begin
              Info := 3;
            end;
          if State.BRACKT and AP_FP_Less_Eq(State.STMAX - State.STMIN,
            LogitXTOL * State.STMAX) then
            begin
              Info := 2;
            end;
          if AP_FP_Less_Eq(F, State.FTEST1) and AP_FP_Less_Eq(AbsReal(State.DG),
            -LogitGTOL * State.DGINIT) then
            begin
              Info := 1;
            end;

          //
          // CHECK FOR TERMINATION.
          //
          if Info <> 0 then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // IN THE FIRST STAGE WE SEEK A STEP FOR WHICH THE MODIFIED
          // FUNCTION HAS A NONPOSITIVE VALUE AND NONNEGATIVE DERIVATIVE.
          //
          if State.STAGE1 and AP_FP_Less_Eq(F, State.FTEST1) and
            AP_FP_Greater_Eq(State.DG, Min(LogitFTOL, LogitGTOL) * State.DGINIT) then
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
              State.FM := F - STP * State.DGTEST;
              State.FXM := State.FX - State.STX * State.DGTEST;
              State.FYM := State.FY - State.STY * State.DGTEST;
              State.DGM := State.DG - State.DGTEST;
              State.DGXM := State.DGX - State.DGTEST;
              State.DGYM := State.DGY - State.DGTEST;

              //
              // CALL CSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
              // AND TO COMPUTE THE NEW STEP.
              //
              MNLMCSTEP(State.STX, State.FXM, State.DGXM, State.STY, State.FYM,
                State.DGYM, STP, State.FM, State.DGM, State.BRACKT, State.STMIN,
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
              MNLMCSTEP(State.STX, State.FX, State.DGX, State.STY, State.FY,
                State.DGY, STP, F, State.DG, State.BRACKT, State.STMIN, State.STMAX,
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
                  STP := State.STX + P5 * (State.STY - State.STX);
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

procedure MNLMCSTEP(var STX: TLFloat; var FX: TLFloat;
  var DX: TLFloat; var STY: TLFloat; var FY: TLFloat;
  var DY: TLFloat; var STP: TLFloat; const FP: TLFloat;
  const DP: TLFloat; var BRACKT: Boolean; const STMIN: TLFloat;
  const STMAX: TLFloat; var Info: TLInt);
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
  Info := 0;

  //
  // CHECK THE INPUT PARAMETERS FOR ERRORS.
  //
  if BRACKT and (AP_FP_Less_Eq(STP, Min(STX, STY)) or AP_FP_Greater_Eq(STP,
    Max(STX, STY))) or AP_FP_Greater_Eq(DX * (STP - STX), 0) or
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
      Info := 1;
      BOUND := True;
      THETA := 3 * (FX - FP) / (STP - STX) + DX + DP;
      S := Max(AbsReal(THETA), Max(AbsReal(DX), AbsReal(DP)));
      GAMMA := S * Sqrt(AP_Sqr(THETA / S) - DX / S * (DP / S));
      if AP_FP_Less(STP, STX) then
        begin
          GAMMA := -GAMMA;
        end;
      P := GAMMA - DX + THETA;
      Q := GAMMA - DX + GAMMA + DP;
      R := P / Q;
      STPC := STX + R * (STP - STX);
      STPQ := STX + DX / ((FX - FP) / (STP - STX) + DX) / 2 * (STP - STX);
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
          Info := 2;
          BOUND := False;
          THETA := 3 * (FX - FP) / (STP - STX) + DX + DP;
          S := Max(AbsReal(THETA), Max(AbsReal(DX), AbsReal(DP)));
          GAMMA := S * Sqrt(AP_Sqr(THETA / S) - DX / S * (DP / S));
          if AP_FP_Greater(STP, STX) then
            begin
              GAMMA := -GAMMA;
            end;
          P := GAMMA - DP + THETA;
          Q := GAMMA - DP + GAMMA + DX;
          R := P / Q;
          STPC := STP + R * (STX - STP);
          STPQ := STP + DP / (DP - DX) * (STX - STP);
          if AP_FP_Greater(AbsReal(STPC - STP), AbsReal(STPQ - STP)) then
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
              // EITHER LogitSTPMIN OR LogitSTPMAX. THE QUADRATIC (SECANT) STEP IS ALSO
              // COMPUTED AND IF THE MINIMUM IS BRACKETED THEN THE THE STEP
              // CLOSEST TO STX IS TAKEN, ELSE THE STEP FARTHEST AWAY IS TAKEN.
              //
              Info := 3;
              BOUND := True;
              THETA := 3 * (FX - FP) / (STP - STX) + DX + DP;
              S := Max(AbsReal(THETA), Max(AbsReal(DX), AbsReal(DP)));

              //
              // THE CASE GAMMA = 0 ONLY ARISES IF THE CUBIC DOES NOT TEND
              // TO INFINITY IN THE DIRECTION OF THE STEP.
              //
              GAMMA := S * Sqrt(Max(0, AP_Sqr(THETA / S) - DX / S * (DP / S)));
              if AP_FP_Greater(STP, STX) then
                begin
                  GAMMA := -GAMMA;
                end;
              P := GAMMA - DP + THETA;
              Q := GAMMA + (DX - DP) + GAMMA;
              R := P / Q;
              if AP_FP_Less(R, 0) and AP_FP_Neq(GAMMA, 0) then
                begin
                  STPC := STP + R * (STX - STP);
                end
              else
                begin
                  if AP_FP_Greater(STP, STX) then
                    begin
                      STPC := STMAX;
                    end
                  else
                    begin
                      STPC := STMIN;
                    end;
                end;
              STPQ := STP + DP / (DP - DX) * (STX - STP);
              if BRACKT then
                begin
                  if AP_FP_Less(AbsReal(STP - STPC), AbsReal(STP - STPQ)) then
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
                  if AP_FP_Greater(AbsReal(STP - STPC), AbsReal(STP - STPQ)) then
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
              // IS EITHER LogitSTPMIN OR LogitSTPMAX, ELSE THE CUBIC STEP IS TAKEN.
              //
              Info := 4;
              BOUND := False;
              if BRACKT then
                begin
                  THETA := 3 * (FP - FY) / (STY - STP) + DY + DP;
                  S := Max(AbsReal(THETA), Max(AbsReal(DY), AbsReal(DP)));
                  GAMMA := S * Sqrt(AP_Sqr(THETA / S) - DY / S * (DP / S));
                  if AP_FP_Greater(STP, STY) then
                    begin
                      GAMMA := -GAMMA;
                    end;
                  P := GAMMA - DP + THETA;
                  Q := GAMMA - DP + GAMMA + DY;
                  R := P / Q;
                  STPC := STP + R * (STY - STP);
                  STPF := STPC;
                end
              else
                begin
                  if AP_FP_Greater(STP, STX) then
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
      STY := STP;
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
      STX := STP;
      FX := FP;
      DX := DP;
    end;

  //
  // COMPUTE THE NEW STEP AND SAFEGUARD IT.
  //
  STPF := Min(STMAX, STPF);
  STPF := Max(STMIN, STPF);
  STP := STPF;
  if BRACKT and BOUND then
    begin
      if AP_FP_Greater(STY, STX) then
        begin
          STP := Min(STX + 0.66 * (STY - STX), STP);
        end
      else
        begin
          STP := Max(STX + 0.66 * (STY - STX), STP);
        end;
    end;
end;
