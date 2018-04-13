{ ****************************************************************************** }
{ * Training support, by QQ 600585@qq.com                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
type
  (* ************************************************************************
    Training report:
    * NGrad     - number of gradient calculations
    * NHess     - number of Hessian calculations
    * NCholesky - number of Cholesky decompositions
    ************************************************************************ *)
  TMLPReport = packed record
    NGrad: TLInt;
    NHess: TLInt;
    NCholesky: TLInt;
  end;

  (* ************************************************************************
    Cross-validation estimates of generalization error
    ************************************************************************ *)
  TMLPCVReport = packed record
    RelCLSError: TLFloat;
    AvgCE: TLFloat;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
  end;

procedure MLPTrainLM(var Network: TMultiLayerPerceptron; const XY: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPTrainLM_MT(var Network: TMultiLayerPerceptron; const XY: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPTrainLBFGS(var Network: TMultiLayerPerceptron;
  const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MaxIts: TLInt;
  var Info: TLInt; var Rep: TMLPReport; IsTerminated: PBoolean;
  out EBest: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPTrainLBFGS_MT(var Network: TMultiLayerPerceptron;
  const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MaxIts: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPTrainLBFGS_MT_Mod(var Network: TMultiLayerPerceptron;
  const XY: TLMatrix; NPoints: TLInt; Restarts: TLInt;
  WStep, Diameter: TLFloat; MaxIts: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPTrainMonteCarlo(var Network: TMultiLayerPerceptron; const XY: TLMatrix; NPoints: TLInt;
  const MainRestarts, SubRestarts: TLInt; const MinError: TLFloat;
  Diameter: TLFloat; var Info: TLInt; var Rep: TMLPReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPKFoldCVLBFGS(const Network: TMultiLayerPerceptron;
  const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MaxIts: TLInt;
  FoldsCount: TLInt; var Info: TLInt; var Rep: TMLPReport;
  var CVRep: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPKFoldCVLM(const Network: TMultiLayerPerceptron;
  const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; FoldsCount: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var CVRep: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


type
  TMLPProcessData = packed record
    TotalRestarts: TLInt;
    RestartsFinished: TLInt;
    IsTerminated: boolean;
    EBest: TLFloat;
  end;

  PMLPProcessData = ^TMLPProcessData;

const
  MinDecay = 0.001;

procedure MLPKFoldCVGeneral(const N: TMultiLayerPerceptron;
  const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; FoldsCount: TLInt; LMAlgorithm: boolean;
  WStep: TLFloat; MaxIts: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var CVRep: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPKFoldSplit(const XY: TLMatrix; NPoints: TLInt;
  NClasses: TLInt; FoldsCount: TLInt; StratifiedSplits: boolean;
  var Folds: TLIVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  //  Neural network training using modified Levenberg-Marquardt with exact
  //  Hessian calculation and regularization. Subroutine trains neural network
  //  with restarts from random positions.Algorithm is well suited for small
  //  and medium scale problems (hundreds of weights).
  //
  //  INPUT PARAMETERS:
  //  Network     -   neural network with initialized geometry
  //  XY          -   training set
  //  NPoints     -   training set size
  //  Decay       -   weight decay constant, >=0.001 Decay term 'Decay*||Weights||^2' is added to error function.
  //                  If you don't know what Decay to choose, use 0.001.
  //  Restarts    -   number of restarts from random position, >0.
  //                  If you don't know what Restarts to choose, use 2.
  //
  //  OUTPUT PARAMETERS:
  //  Network     -   trained neural network.
  //  Info        -   return code:
  //  * -9, if internal matrix inverse subroutine failed
  //  * -2, if there is a point with class number outside of [0..NOut-1].
  //  * -1, if wrong parameters specified (NPoints<0, Restarts<1).
  //  *  2, if task has been solved.
  //  Rep         -   training report
  ************************************************************************ *)
procedure MLPTrainLM(var Network: TMultiLayerPerceptron; const XY: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport);
var
  NIn        : TLInt;
  NOut       : TLInt;
  WCount     : TLInt;
  LMFTol     : TLFloat;
  LMStepTol  : TLFloat;
  I          : TLInt;
  K          : TLInt;
  V          : TLFloat;
  E          : TLFloat;
  ENew       : TLFloat;
  XNorm2     : TLFloat;
  StepNorm   : TLFloat;
  G          : TLVec;
  D          : TLVec;
  H          : TLMatrix;
  HMod       : TLMatrix;
  Z          : TLMatrix;
  SPD        : boolean;
  Nu         : TLFloat;
  Lambda     : TLFloat;
  LambdaUp   : TLFloat;
  LambdaDown : TLFloat;
  InternalRep: TMinLBFGSReport;
  State      : TMinLBFGSState;
  X          : TLVec;
  Y          : TLVec;
  WBase      : TLVec;
  WDir       : TLVec;
  WT         : TLVec;
  WX         : TLVec;
  Pass       : TLInt;
  WBest      : TLVec;
  EBest      : TLFloat;
  InvInfo    : TLInt;
  InvRep     : TMatInvReport;
  SolverInfo : TLInt;
  SolverRep  : TDenseSolverReport;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  LambdaUp := 10;
  LambdaDown := 0.3;
  LMFTol := 0.001;
  LMStepTol := 0.001;

  //
  // Test for inputs
  //
  if (NPoints <= 0) or (Restarts < 1) then
    begin
      Info := -1;
      Exit;
    end;
  if MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NPoints - 1 do
        begin
          if (Round(XY[I, NIn]) < 0) or (Round(XY[I, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(I);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Initialize data
  //
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;

  //
  // General case.
  // Prepare task and network. Allocate space.
  //
  MLPInitPreprocessor(Network, XY, NPoints);
  SetLength(G, WCount - 1 + 1);
  SetLength(H, WCount - 1 + 1, WCount - 1 + 1);
  SetLength(HMod, WCount - 1 + 1, WCount - 1 + 1);
  SetLength(WBase, WCount - 1 + 1);
  SetLength(WDir, WCount - 1 + 1);
  SetLength(WBest, WCount - 1 + 1);
  SetLength(WT, WCount - 1 + 1);
  SetLength(WX, WCount - 1 + 1);
  EBest := MaxRealNumber;

  //
  // Multiple passes
  //
  Pass := 1;
  while Pass <= Restarts do
    begin

      //
      // Initialize weights
      //
      MLPRandomize(Network);

      //
      // First stage of the hybrid algorithm: LBFGS
      //
      APVMove(@WBase[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 5), WBase, State);
      MinLBFGSSetCond(State, 0, 0, 0, Max(25, WCount));
      while MinLBFGSIteration(State) do
        begin

          //
          // gradient
          //
          APVMove(@Network.Weights[0], 0, WCount - 1, @State.X[0], 0, WCount - 1);
          MLPGradBatch(Network, XY, NPoints, State.F, State.G);

          //
          // weight decay
          //
          V := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          State.F := State.F + 0.5 * Decay * V;
          APVAdd(@State.G[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);

          //
          // next iteration
          //
          Rep.NGrad := Rep.NGrad + 1;
        end;
      MinLBFGSResults(State, WBase, InternalRep);
      APVMove(@Network.Weights[0], 0, WCount - 1, @WBase[0], 0, WCount - 1);

      //
      // Second stage of the hybrid algorithm: LM
      //
      // Initialize H with identity matrix,
      // G with gradient,
      // E with regularized error.
      //
      MLPHessianBatch(Network, XY, NPoints, E, G, H);
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
      Lambda := 0.001;
      Nu := 2;
      while True do
        begin

          //
          // 1. HMod = H+lambda*I
          // 2. Try to solve (H+Lambda*I)*dx = -g.
          // Increase lambda if left part is not positive definite.
          //
          I := 0;
          while I <= WCount - 1 do
            begin
              APVMove(@HMod[I][0], 0, WCount - 1, @H[I][0], 0, WCount - 1);
              HMod[I, I] := HMod[I, I] + Lambda;
              Inc(I);
            end;
          SPD := SPDMatrixCholesky(HMod, WCount, True);
          Rep.NCholesky := Rep.NCholesky + 1;
          if not SPD then
            begin
              Lambda := Lambda * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          SPDMatrixCholeskySolve(HMod, WCount, True, G, SolverInfo, SolverRep, WDir);
          if SolverInfo < 0 then
            begin
              Lambda := Lambda * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          APVMul(@WDir[0], 0, WCount - 1, -1);

          //
          // Lambda found.
          // 1. Save old w in WBase
          // 1. Test some stopping criterions
          // 2. If error(w+wdir)>error(w), increase lambda
          //
          APVAdd(@Network.Weights[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
          XNorm2 := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          StepNorm := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
          StepNorm := Sqrt(StepNorm);
          ENew := MLPError(Network, XY, NPoints) + 0.5 * Decay * XNorm2;
          if AP_FP_Less(StepNorm, LMStepTol * (1 + Sqrt(XNorm2))) then
            begin
              Break;
            end;
          if AP_FP_Greater(ENew, E) then
            begin
              Lambda := Lambda * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;

          //
          // Optimize using inv(cholesky(H)) as preconditioner
          //
          RMatrixTRInverse(HMod, WCount, True, False, InvInfo, InvRep);
          if InvInfo <= 0 then
            begin

              //
              // if matrix can't be inverted then exit with errors
              // TODO: make WCount steps in direction suggested by HMod
              //
              Info := -9;
              Exit;
            end;
          APVMove(@WBase[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          I := 0;
          while I <= WCount - 1 do
            begin
              WT[I] := 0;
              Inc(I);
            end;
          MinLBFGSCreateX(WCount, WCount, WT, 1, State);
          MinLBFGSSetCond(State, 0, 0, 0, 5);
          while MinLBFGSIteration(State) do
            begin

              //
              // gradient
              //
              I := 0;
              while I <= WCount - 1 do
                begin
                  V := APVDotProduct(@State.X[0], I, WCount - 1, @HMod[I][0], I, WCount - 1);
                  Network.Weights[I] := WBase[I] + V;
                  Inc(I);
                end;
              MLPGradBatch(Network, XY, NPoints, State.F, G);
              I := 0;
              while I <= WCount - 1 do
                begin
                  State.G[I] := 0;
                  Inc(I);
                end;
              I := 0;
              while I <= WCount - 1 do
                begin
                  V := G[I];
                  APVAdd(@State.G[0], I, WCount - 1, @HMod[I][0], I, WCount - 1, V);
                  Inc(I);
                end;

              //
              // weight decay
              // grad(x'*x) = A'*(x0+A*t)
              //
              V := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
              State.F := State.F + 0.5 * Decay * V;
              I := 0;
              while I <= WCount - 1 do
                begin
                  V := Decay * Network.Weights[I];
                  APVAdd(@State.G[0], I, WCount - 1, @HMod[I][0], I, WCount - 1, V);
                  Inc(I);
                end;

              //
              // next iteration
              //
              Rep.NGrad := Rep.NGrad + 1;
            end;
          MinLBFGSResults(State, WT, InternalRep);

          //
          // Accept new position.
          // Calculate Hessian
          //
          I := 0;
          while I <= WCount - 1 do
            begin
              V := APVDotProduct(@WT[0], I, WCount - 1, @HMod[I][0], I, WCount - 1);
              Network.Weights[I] := WBase[I] + V;
              Inc(I);
            end;
          MLPHessianBatch(Network, XY, NPoints, E, G, H);
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
          // Update lambda
          //
          Lambda := Lambda * LambdaDown;
          Nu := 2;
        end;

      //
      // update WBest
      //
      V := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      E := 0.5 * Decay * V + MLPError(Network, XY, NPoints);
      if AP_FP_Less(E, EBest) then
        begin
          EBest := E;
          APVMove(@WBest[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
        end;
      Inc(Pass);
    end;

  //
  // copy WBest to output
  //
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

(* ************************************************************************
  Neural network training  using  modified  Levenberg-Marquardt  with  exact
  Hessian calculation and regularization. Subroutine trains  neural  network
  with restarts from random positions. Algorithm is well  suited  for  small
  and medium scale problems (hundreds of weights).

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay constant, >=0.001
  Decay term 'Decay*||Weights||^2' is added to error
  function.
  If you don't know what Decay to choose, use 0.001.
  Restarts    -   number of restarts from random position, >0.
  If you don't know what Restarts to choose, use 2.

  OUTPUT PARAMETERS:
  Network     -   trained neural network.
  Info        -   return code:
  * -9, if internal matrix inverse subroutine failed
  * -2, if there is a point with class number outside of [0..NOut-1].
  * -1, if wrong parameters specified (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report
  ************************************************************************ *)
procedure MLPTrainLM_MT(var Network: TMultiLayerPerceptron; const XY: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport);
var
  I        : TLInt;
  NIn      : TLInt;
  NOut     : TLInt;
  WCount   : TLInt;
  LMStepTol: TLFloat;

  Lambda    : TLFloat;
  LambdaUp  : TLFloat;
  LambdaDown: TLFloat;

  WBest: TLVec;
  EBest: TLFloat;

  zLock  : TCriticalSection;
  RepPtr : ^TMLPReport;
  InfoPtr: ^TLInt;

  NetworkPtr: PMultiLayerPerceptron;

  {$IFDEF FPC}
  procedure Nested_ParallelFor(Pass: PtrInt; Data: Pointer;
    Item: TMultiThreadProcItem);
  var
    _Parallel_I: TLInt;
    NetworkCopy: TMultiLayerPerceptron;
    // X: TLVec;
    // Y: TLVec;
    // D: TLVec;
    // Z: TLMatrix;
    InvInfo    : TLInt;
    InternalRep: TMinLBFGSReport;
    SolverRep  : TDenseSolverReport;
    SolverInfo : TLInt;
    InvRep     : TMatInvReport;
    WT         : TLVec;
    WDir       : TLVec;
    State      : TMinLBFGSState;
    WBase      : TLVec;
    Nu         : TLFloat;
    SPD        : boolean;
    HMod       : TLMatrix;
    H          : TLMatrix;
    G          : TLVec;
    StepNorm   : TLFloat;
    XNorm2     : TLFloat;
    ENew       : TLFloat;
    E          : TLFloat;
    V          : TLFloat;
    K          : TLInt;
  begin

    SetLength(WT, WCount (* - 1 + 1 // optimized compiler *) );
    SetLength(WDir, WCount (* - 1 + 1 // optimized compiler *) );
    SetLength(WBase, WCount (* - 1 + 1 // optimized compiler *) );
    SetLength(HMod, WCount (* - 1 + 1 // optimized compiler *) , WCount (* - 1 + 1 // optimized compiler *) );
    SetLength(H, WCount (* - 1 + 1 // optimized compiler *) , WCount (* - 1 + 1 // optimized compiler *) );
    SetLength(G, WCount (* - 1 + 1 // optimized compiler *) );

    MLPCopy(NetworkPtr^, NetworkCopy);
    //
    // Initialize weights
    //
    MLPRandomize(NetworkCopy);

    //
    // First stage of the hybrid algorithm: LBFGS
    //
    APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
    MinLBFGSCreate(WCount, Min(WCount, 5), WBase, State);
    MinLBFGSSetCond(State, 0, 0, 0, Max(25, WCount));
    while MinLBFGSIteration(State) do
      begin

        //
        // gradient
        //
        APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.X[0], 0,
          WCount - 1);
        MLPGradBatch(NetworkCopy, XY, NPoints, State.F, State.G);

        //
        // weight decay
        //
        V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
          @NetworkCopy.Weights[0], 0, WCount - 1);
        State.F := State.F + 0.5 * Decay * V;
        APVAdd(@State.G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
          WCount - 1, Decay);

        //
        // next iteration
        //
        RepPtr^.NGrad := RepPtr^.NGrad + 1;
      end;
    MinLBFGSResults(State, WBase, InternalRep);
    APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @WBase[0], 0, WCount - 1);

    //
    // Second stage of the hybrid algorithm: LM
    //
    // Initialize H with identity matrix,
    // G with gradient,
    // E with regularized error.
    //
    MLPHessianBatch(NetworkCopy, XY, NPoints, E, G, H);
    V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
      @NetworkCopy.Weights[0], 0, WCount - 1);
    E := E + 0.5 * Decay * V;
    APVAdd(@G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
    K := 0;
    while K <= WCount - 1 do
      begin
        H[K, K] := H[K, K] + Decay;
        Inc(K);
      end;
    RepPtr^.NHess := RepPtr^.NHess + 1;
    Lambda := 0.001;
    Nu := 2;
    while True do
      begin

        //
        // 1. HMod = H+lambda*_Parallel_I
        // 2. Try to solve (H+Lambda*_Parallel_I)*dx = -g.
        // Increase lambda if left part is not positive definite.
        //
        _Parallel_I := 0;
        while _Parallel_I <= WCount - 1 do
          begin
            APVMove(@HMod[_Parallel_I][0], 0, WCount - 1, @H[_Parallel_I][0], 0,
              WCount - 1);
            HMod[_Parallel_I, _Parallel_I] :=
              HMod[_Parallel_I, _Parallel_I] + Lambda;
            Inc(_Parallel_I);
          end;
        SPD := SPDMatrixCholesky(HMod, WCount, True);
        RepPtr^.NCholesky := RepPtr^.NCholesky + 1;
        if not SPD then
          begin
            Lambda := Lambda * LambdaUp * Nu;
            Nu := Nu * 2;
            Continue;
          end;
        SPDMatrixCholeskySolve(HMod, WCount, True, G, SolverInfo,
          SolverRep, WDir);
        if SolverInfo < 0 then
          begin
            Lambda := Lambda * LambdaUp * Nu;
            Nu := Nu * 2;
            Continue;
          end;
        APVMul(@WDir[0], 0, WCount - 1, -1);

        //
        // Lambda found.
        // 1. Save old w in WBase
        // 1. Test some stopping criterions
        // 2. If error(w+wdir)>error(w), increase lambda
        //
        APVAdd(@NetworkCopy.Weights[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
        XNorm2 := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
          @NetworkCopy.Weights[0], 0, WCount - 1);
        StepNorm := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0,
          WCount - 1);
        StepNorm := Sqrt(StepNorm);
        ENew := MLPError(NetworkCopy, XY, NPoints) + 0.5 * Decay * XNorm2;
        if AP_FP_Less(StepNorm, LMStepTol * (1 + Sqrt(XNorm2))) then
          begin
            Break;
          end;
        if AP_FP_Greater(ENew, E) then
          begin
            Lambda := Lambda * LambdaUp * Nu;
            Nu := Nu * 2;
            Continue;
          end;

        //
        // Optimize using inv(cholesky(H)) as preconditioner
        //
        RMatrixTRInverse(HMod, WCount, True, False, InvInfo, InvRep);
        if InvInfo <= 0 then
          begin

            //
            // if matrix can't be inverted then exit with errors
            // TODO: make WCount steps in direction suggested by HMod
            //
            InfoPtr^ := -9;
            Exit;
          end;
        APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        _Parallel_I := 0;
        while _Parallel_I <= WCount - 1 do
          begin
            WT[_Parallel_I] := 0;
            Inc(_Parallel_I);
          end;
        MinLBFGSCreateX(WCount, WCount, WT, 1, State);
        MinLBFGSSetCond(State, 0, 0, 0, 5);
        while MinLBFGSIteration(State) do
          begin

            //
            // gradient
            //
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                V := APVDotProduct(@State.X[0], _Parallel_I, WCount - 1,
                  @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
                NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + V;
                Inc(_Parallel_I);
              end;
            MLPGradBatch(NetworkCopy, XY, NPoints, State.F, G);
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                State.G[_Parallel_I] := 0;
                Inc(_Parallel_I);
              end;
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                V := G[_Parallel_I];
                APVAdd(@State.G[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                  _Parallel_I, WCount - 1, V);
                Inc(_Parallel_I);
              end;

            //
            // weight decay
            // grad(x'*x) = A'*(x0+A*t)
            //
            V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
              @NetworkCopy.Weights[0], 0, WCount - 1);
            State.F := State.F + 0.5 * Decay * V;
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                V := Decay * NetworkCopy.Weights[_Parallel_I];
                APVAdd(@State.G[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                  _Parallel_I, WCount - 1, V);
                Inc(_Parallel_I);
              end;

            //
            // next iteration
            //
            RepPtr^.NGrad := RepPtr^.NGrad + 1;
          end;
        MinLBFGSResults(State, WT, InternalRep);

        //
        // Accept new position.
        // Calculate Hessian
        //
        _Parallel_I := 0;
        while _Parallel_I <= WCount - 1 do
          begin
            V := APVDotProduct(@WT[0], _Parallel_I, WCount - 1,
              @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
            NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + V;
            Inc(_Parallel_I);
          end;
        MLPHessianBatch(NetworkCopy, XY, NPoints, E, G, H);
        V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
          @NetworkCopy.Weights[0], 0, WCount - 1);
        E := E + 0.5 * Decay * V;
        APVAdd(@G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
          WCount - 1, Decay);
        K := 0;
        while K <= WCount - 1 do
          begin
            H[K, K] := H[K, K] + Decay;
            Inc(K);
          end;
        RepPtr^.NHess := RepPtr^.NHess + 1;

        //
        // Update lambda
        //
        Lambda := Lambda * LambdaDown;
        Nu := 2;
      end;

    //
    // update WBest
    //
    V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
      @NetworkCopy.Weights[0], 0, WCount - 1);
    E := 0.5 * Decay * V + MLPError(NetworkCopy, XY, NPoints);
    if AP_FP_Less(E, EBest) then
      begin
        zLock.Enter;
        EBest := E;
        APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        zLock.Leave;
      end;

    // Free
    SetLength(WT, 0);
    SetLength(WDir, 0);
    SetLength(WBase, 0);
    SetLength(HMod, 0, 0);
    SetLength(H, 0, 0);
    SetLength(G, 0);
    MLPFree(NetworkCopy);
  end;
{$ENDIF FPC}


begin
  MLPProperties(Network, NIn, NOut, WCount);
  LambdaUp := 10;
  LambdaDown := 0.3;
  // LMFTol := 0.001;
  LMStepTol := 0.001;

  //
  // Test for inputs
  //
  if (NPoints <= 0) or (Restarts < 1) then
    begin
      Info := -1;
      Exit;
    end;
  if MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NPoints - 1 do
        begin
          if (Round(XY[I, NIn]) < 0) or (Round(XY[I, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(I);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Initialize data
  //
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;

  //
  // General case.
  // Prepare task and network. Allocate space.
  //
  MLPInitPreprocessor(Network, XY, NPoints);
  SetLength(WBest, WCount (* - 1 + 1 // optimized compiler *) );
  EBest := MaxRealNumber;

  //
  // Multiple passes
  //
  NetworkPtr := @Network;
  RepPtr := @Rep;
  InfoPtr := @Info;
  zLock := TCriticalSection.Create;

  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, PtrInt(Restarts));
  {$ELSE}
  TParallel.For(1, Restarts, procedure(Pass: TLInt)
    var
      _Parallel_I: TLInt;
      NetworkCopy: TMultiLayerPerceptron;
      // X: TLVec;
      // Y: TLVec;
      // D: TLVec;
      // Z: TLMatrix;
      InvInfo: TLInt;
      InternalRep: TMinLBFGSReport;
      SolverRep: TDenseSolverReport;
      SolverInfo: TLInt;
      InvRep: TMatInvReport;
      WT: TLVec;
      WDir: TLVec;
      State: TMinLBFGSState;
      WBase: TLVec;
      Nu: TLFloat;
      SPD: boolean;
      HMod: TLMatrix;
      H: TLMatrix;
      G: TLVec;
      StepNorm: TLFloat;
      XNorm2: TLFloat;
      ENew: TLFloat;
      E: TLFloat;
      V: TLFloat;
      K: TLInt;
    begin

      SetLength(WT, WCount (* - 1 + 1 // optimized compiler *) );
      SetLength(WDir, WCount (* - 1 + 1 // optimized compiler *) );
      SetLength(WBase, WCount (* - 1 + 1 // optimized compiler *) );
      SetLength(HMod, WCount (* - 1 + 1 // optimized compiler *) , WCount (* - 1 + 1 // optimized compiler *) );
      SetLength(H, WCount (* - 1 + 1 // optimized compiler *) , WCount (* - 1 + 1 // optimized compiler *) );
      SetLength(G, WCount (* - 1 + 1 // optimized compiler *) );

      MLPCopy(NetworkPtr^, NetworkCopy);
      //
      // Initialize weights
      //
      MLPRandomize(NetworkCopy);

      //
      // First stage of the hybrid algorithm: LBFGS
      //
      APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 5), WBase, State);
      MinLBFGSSetCond(State, 0, 0, 0, Max(25, WCount));
      while MinLBFGSIteration(State) do
        begin

          //
          // gradient
          //
          APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.X[0], 0,
            WCount - 1);
          MLPGradBatch(NetworkCopy, XY, NPoints, State.F, State.G);

          //
          // weight decay
          //
          V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
            @NetworkCopy.Weights[0], 0, WCount - 1);
          State.F := State.F + 0.5 * Decay * V;
          APVAdd(@State.G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1, Decay);

          //
          // next iteration
          //
          RepPtr^.NGrad := RepPtr^.NGrad + 1;
        end;
      MinLBFGSResults(State, WBase, InternalRep);
      APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @WBase[0], 0, WCount - 1);

      //
      // Second stage of the hybrid algorithm: LM
      //
      // Initialize H with identity matrix,
      // G with gradient,
      // E with regularized error.
      //
      MLPHessianBatch(NetworkCopy, XY, NPoints, E, G, H);
      V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
        @NetworkCopy.Weights[0], 0, WCount - 1);
      E := E + 0.5 * Decay * V;
      APVAdd(@G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
        WCount - 1, Decay);
      K := 0;
      while K <= WCount - 1 do
        begin
          H[K, K] := H[K, K] + Decay;
          Inc(K);
        end;
      RepPtr^.NHess := RepPtr^.NHess + 1;
      Lambda := 0.001;
      Nu := 2;
      while True do
        begin

          //
          // 1. HMod = H+lambda*_Parallel_I
          // 2. Try to solve (H+Lambda*_Parallel_I)*dx = -g.
          // Increase lambda if left part is not positive definite.
          //
          _Parallel_I := 0;
          while _Parallel_I <= WCount - 1 do
            begin
              APVMove(@HMod[_Parallel_I][0], 0, WCount - 1, @H[_Parallel_I][0], 0,
                WCount - 1);
              HMod[_Parallel_I, _Parallel_I] := HMod[_Parallel_I,
                _Parallel_I] + Lambda;
              Inc(_Parallel_I);
            end;
          SPD := SPDMatrixCholesky(HMod, WCount, True);
          RepPtr^.NCholesky := RepPtr^.NCholesky + 1;
          if not SPD then
            begin
              Lambda := Lambda * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          SPDMatrixCholeskySolve(HMod, WCount, True, G, SolverInfo,
            SolverRep, WDir);
          if SolverInfo < 0 then
            begin
              Lambda := Lambda * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          APVMul(@WDir[0], 0, WCount - 1, -1);

          //
          // Lambda found.
          // 1. Save old w in WBase
          // 1. Test some stopping criterions
          // 2. If error(w+wdir)>error(w), increase lambda
          //
          APVAdd(@NetworkCopy.Weights[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
          XNorm2 := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
            @NetworkCopy.Weights[0], 0, WCount - 1);
          StepNorm := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0,
            WCount - 1);
          StepNorm := Sqrt(StepNorm);
          ENew := MLPError(NetworkCopy, XY, NPoints) + 0.5 * Decay * XNorm2;
          if AP_FP_Less(StepNorm, LMStepTol * (1 + Sqrt(XNorm2))) then
            begin
              Break;
            end;
          if AP_FP_Greater(ENew, E) then
            begin
              Lambda := Lambda * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;

          //
          // Optimize using inv(cholesky(H)) as preconditioner
          //
          RMatrixTRInverse(HMod, WCount, True, False, InvInfo, InvRep);
          if InvInfo <= 0 then
            begin

              //
              // if matrix can't be inverted then exit with errors
              // TODO: make WCount steps in direction suggested by HMod
              //
              InfoPtr^ := -9;
              Exit;
            end;
          APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          _Parallel_I := 0;
          while _Parallel_I <= WCount - 1 do
            begin
              WT[_Parallel_I] := 0;
              Inc(_Parallel_I);
            end;
          MinLBFGSCreateX(WCount, WCount, WT, 1, State);
          MinLBFGSSetCond(State, 0, 0, 0, 5);
          while MinLBFGSIteration(State) do
            begin

              //
              // gradient
              //
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  V := APVDotProduct(@State.X[0], _Parallel_I, WCount - 1,
                    @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
                  NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + V;
                  Inc(_Parallel_I);
                end;
              MLPGradBatch(NetworkCopy, XY, NPoints, State.F, G);
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  State.G[_Parallel_I] := 0;
                  Inc(_Parallel_I);
                end;
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  V := G[_Parallel_I];
                  APVAdd(@State.G[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                    _Parallel_I, WCount - 1, V);
                  Inc(_Parallel_I);
                end;

              //
              // weight decay
              // grad(x'*x) = A'*(x0+A*t)
              //
              V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
                @NetworkCopy.Weights[0], 0, WCount - 1);
              State.F := State.F + 0.5 * Decay * V;
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  V := Decay * NetworkCopy.Weights[_Parallel_I];
                  APVAdd(@State.G[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                    _Parallel_I, WCount - 1, V);
                  Inc(_Parallel_I);
                end;

              //
              // next iteration
              //
              RepPtr^.NGrad := RepPtr^.NGrad + 1;
            end;
          MinLBFGSResults(State, WT, InternalRep);

          //
          // Accept new position.
          // Calculate Hessian
          //
          _Parallel_I := 0;
          while _Parallel_I <= WCount - 1 do
            begin
              V := APVDotProduct(@WT[0], _Parallel_I, WCount - 1,
                @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
              NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + V;
              Inc(_Parallel_I);
            end;
          MLPHessianBatch(NetworkCopy, XY, NPoints, E, G, H);
          V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
            @NetworkCopy.Weights[0], 0, WCount - 1);
          E := E + 0.5 * Decay * V;
          APVAdd(@G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1, Decay);
          K := 0;
          while K <= WCount - 1 do
            begin
              H[K, K] := H[K, K] + Decay;
              Inc(K);
            end;
          RepPtr^.NHess := RepPtr^.NHess + 1;

          //
          // Update lambda
          //
          Lambda := Lambda * LambdaDown;
          Nu := 2;
        end;

      //
      // update WBest
      //
      V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
        @NetworkCopy.Weights[0], 0, WCount - 1);
      E := 0.5 * Decay * V + MLPError(NetworkCopy, XY, NPoints);
      if AP_FP_Less(E, EBest) then
        begin
          zLock.Enter;
          EBest := E;
          APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          zLock.Leave;
        end;

      // Free
      SetLength(WT, 0);
      SetLength(WDir, 0);
      SetLength(WBase, 0);
      SetLength(HMod, 0, 0);
      SetLength(H, 0, 0);
      SetLength(G, 0);
      MLPFree(NetworkCopy);
    end);
  {$ENDIF FPC}
  zLock.Free;

  //
  // copy WBest to output
  //
  Rep := RepPtr^;
  Info := InfoPtr^;
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

(* ************************************************************************
  Neural  network  training  using  L-BFGS  algorithm  with  regularization.
  Subroutine  trains  neural  network  with  restarts from random positions.
  Algorithm  is  well  suited  for  problems  of  any dimensionality (memory
  requirements and step complexity are linear by weights number).

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay constant, >=0.001 Decay term 'Decay*||Weights||^2' is added to error function. If you don't know what Decay to choose, use 0.001.
  Restarts    -   number of restarts from random position, >0. If you don't know what Restarts to choose, use 2.
  WStep       -   stopping criterion. Algorithm stops if  step  size  is less than WStep. Recommended value - 0.01.
  Zero  step size means stopping after MaxIts iterations.
  MaxIts      -   stopping   criterion.  Algorithm  stops  after  MaxIts
  iterations (NOT gradient  calculations).  Zero  MaxIts
  means stopping when step is sufficiently small.

  OUTPUT PARAMETERS:
  Network     -   trained neural network.
  Info        -   return code:
  * -8, if both WStep=0 and MaxIts=0
  * -2, if there is a point with class number outside of [0..NOut-1].
  * -1, if wrong parameters specified (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report
  ************************************************************************ *)
procedure MLPTrainLBFGS(var Network: TMultiLayerPerceptron;
const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; WStep: TLFloat; MaxIts: TLInt;
var Info: TLInt; var Rep: TMLPReport; IsTerminated: PBoolean;
out EBest: TLFloat);
var
  I          : TLInt;
  Pass       : TLInt;
  NIn        : TLInt;
  NOut       : TLInt;
  WCount     : TLInt;
  W          : TLVec;
  WBest      : TLVec;
  E          : TLFloat;
  V          : TLFloat;
  InternalRep: TMinLBFGSReport;
  State      : TMinLBFGSState;
begin

  EBest := MaxDouble;
  //
  // Test inputs, parse flags, read network geometry
  //
  if AP_FP_Eq(WStep, 0) and (MaxIts = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MaxIts < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NPoints - 1 do
        begin
          if (Round(XY[I, NIn]) < 0) or (Round(XY[I, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(I);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, XY, NPoints);
  SetLength(W, WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(WBest, WCount (* - 1 + 1 // optimized compiler *) );
  EBest := MaxRealNumber;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;
  for Pass := 1 to Restarts do
    begin

      //
      // Process
      //
      MLPRandomize(Network);
      APVMove(@W[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 10), W, State);
      MinLBFGSSetCond(State, 0.0, 0.0, WStep, MaxIts);
      while not IsTerminated^ and MinLBFGSIteration(State) do
        begin
          APVMove(@Network.Weights[0], 0, WCount - 1, @State.X[0], 0, WCount - 1);
          MLPGradNBatch(Network, XY, NPoints, State.F, State.G);
          V := APVDotProduct(@Network.Weights[0], 0, WCount - 1,
            @Network.Weights[0], 0, WCount - 1);
          State.F := State.F + 0.5 * Decay * V;
          APVAdd(@State.G[0], 0, WCount - 1, @Network.Weights[0], 0,
            WCount - 1, Decay);
          Rep.NGrad := Rep.NGrad + 1;
        end;
      MinLBFGSResults(State, W, InternalRep);
      APVMove(@Network.Weights[0], 0, WCount - 1, @W[0], 0, WCount - 1);

      //
      // Compare with best
      //
      V := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0],
        0, WCount - 1);
      E := MLPErrorN(Network, XY, NPoints) + 0.5 * Decay * V;
      if AP_FP_Less(E, EBest) then
        begin
          APVMove(@WBest[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          EBest := E;
        end;
    end;

  //
  // The best network
  //
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

{ WStep - affects the accuracy of optimal weights. high precision starts from 0.01
  Restarts affect the likelihood of finding the best weights > 100
  MaxIts - the higher the accuracy of the required its iterations > = 500
  Diameter - an initial values within the vector weighting > = 2 }
procedure MLPTrainLBFGS_MT(var Network: TMultiLayerPerceptron;
const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; WStep: TLFloat; MaxIts: TLInt;
var Info: TLInt; var Rep: TMLPReport);
var
  I     : TLInt;
  NIn   : TLInt;
  NOut  : TLInt;
  WCount: TLInt;
  WBest : TLVec;
  EBest : TLFloat;

  zLock       : TCriticalSection;
  NetworkPtr  : PMultiLayerPerceptron;
  RepCopy     : TMLPReport;
  zProcessData: PMLPProcessData;

  {$IFDEF FPC}
  procedure Nested_ParallelFor(Pass: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
  var
    E          : TLFloat;
    V          : TLFloat;
    W          : TLVec;
    State      : TMinLBFGSState;
    InternalRep: TMinLBFGSReport;
    NetworkCopy: TMultiLayerPerceptron;
  begin
    SetLength(W, WCount (* - 1 + 1 // optimized compiler *) );
    MLPCopy(NetworkPtr^, NetworkCopy);

    //
    // Process
    //
    MLPRandomize(NetworkCopy);
    APVMove(@W[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
    MinLBFGSCreate(WCount, WCount, W, State);
    MinLBFGSSetCond(State, 0.0, 0.0, WStep, MaxIts);
    while MinLBFGSIteration(State) do
      begin
        APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.X[0], 0, WCount - 1);
        MLPGradNBatch(NetworkCopy, XY, NPoints, State.F, State.G);
        V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        State.F := State.F + 0.5 * Decay * V;
        APVAdd(@State.G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
        RepCopy.NGrad := RepCopy.NGrad + 1;
        if zProcessData^.IsTerminated then
            Break;
      end;
    MinLBFGSResults(State, W, InternalRep);
    APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @W[0], 0, WCount - 1);

    //
    // Compare with best
    //

    E := MLPErrorN(NetworkCopy, XY, NPoints);
    if AP_FP_Less(E, EBest) then
      begin
        zLock.Enter;
        APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1); EBest := E;
        zProcessData^.EBest := EBest;
        zLock.Leave;
      end;
    MLPFree(NetworkCopy);
    MinLBFGSFree(W, State);
    Inc(zProcessData^.RestartsFinished);

    if zProcessData^.IsTerminated then
        Exit;
  end;
{$ENDIF FPC}


begin
  //
  // Test inputs, parse flags, read network geometry
  //
  if AP_FP_Eq(WStep, 0) and (MaxIts = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MaxIts < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NPoints - 1 do
        begin
          if (Round(XY[I, NIn]) < 0) or (Round(XY[I, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(I);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, XY, NPoints);
  SetLength(WBest, WCount (* - 1 + 1 // optimized compiler *) );
  EBest := MaxRealNumber;
  New(zProcessData);
  zProcessData^.TotalRestarts := Restarts;
  zProcessData^.IsTerminated := False;
  zProcessData^.EBest := EBest;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;

  NetworkPtr := @Network;
  RepCopy := Rep;
  zLock := TCriticalSection.Create;

  try
    {$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, PtrInt(Restarts));
    {$ELSE}
    TParallel.For(1, Restarts, procedure(Pass: TLInt)
      var
        E: TLFloat;
        V: TLFloat;
        W: TLVec;
        State: TMinLBFGSState;
        InternalRep: TMinLBFGSReport;
        NetworkCopy: TMultiLayerPerceptron;
      begin
        SetLength(W, WCount (* - 1 + 1 // optimized compiler *) );
        MLPCopy(NetworkPtr^, NetworkCopy);

        //
        // Process
        //
        MLPRandomize(NetworkCopy);
        APVMove(@W[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        MinLBFGSCreate(WCount, WCount, W, State);
        MinLBFGSSetCond(State, 0.0, 0.0, WStep, MaxIts);
        while MinLBFGSIteration(State) do
          begin
            APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.X[0], 0, WCount - 1);
            MLPGradNBatch(NetworkCopy, XY, NPoints, State.F, State.G);
            V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
            State.F := State.F + 0.5 * Decay * V;
            APVAdd(@State.G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
            RepCopy.NGrad := RepCopy.NGrad + 1;
            if zProcessData.IsTerminated then
                Break;
          end;
        MinLBFGSResults(State, W, InternalRep);
        APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @W[0], 0, WCount - 1);

        //
        // Compare with best
        //

        E := MLPErrorN(NetworkCopy, XY, NPoints);
        if AP_FP_Less(E, EBest) then
          begin
            zLock.Enter;
            APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
            EBest := E;
            zProcessData.EBest := EBest;
            zLock.Leave;
          end;
        MLPFree(NetworkCopy);
        MinLBFGSFree(W, State);
        Inc(zProcessData.RestartsFinished);

        if zProcessData.IsTerminated then
            Exit;
      end);
    {$ENDIF FPC}
  finally
    zLock.Free;
    Dispose(zProcessData);

    //
    // The best network
    //
    Rep := RepCopy;

    APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
  end;
end;

{ WStep - affects the accuracy of optimal weights. high precision starts from 0.01
  Restarts affect the likelihood of finding the best weights > 100
  MaxIts - the higher the accuracy of the required its iterations > = 500
  Diameter - an initial values within the vector weighting > = 2 }
procedure MLPTrainLBFGS_MT_Mod(var Network: TMultiLayerPerceptron;
const XY: TLMatrix; NPoints: TLInt; Restarts: TLInt;
WStep, Diameter: TLFloat; MaxIts: TLInt; var Info: TLInt;
var Rep: TMLPReport);
var
  I     : TLInt;
  NIn   : TLInt;
  NOut  : TLInt;
  WCount: TLInt;
  WBest : TLVec;
  EBest : TLFloat;

  zLock     : TCriticalSection;
  NetworkPtr: PMultiLayerPerceptron;
  RepCopy   : TMLPReport;
  {$IFDEF FPC}
  procedure Nested_ParallelFor(Pass: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
  var
    Ebegin     : TLFloat; { the initial error }
    E          : TLFloat;
    V          : TLFloat;
    W          : TLVec;
    State      : TMinLBFGSState;
    InternalRep: TMinLBFGSReport;
    NetworkCopy: TMultiLayerPerceptron;
    Decay      : TLFloat;
    Iteration  : TLInt;
  begin
    try
      Decay := 1;
      Iteration := 0;
      SetLength(W, WCount (* - 1 + 1 // optimized compiler *) );
      MLPCopy(NetworkPtr^, NetworkCopy);

      //
      // Process
      //
      MLPRandomize(NetworkCopy, Diameter);

      APVMove(@W[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 10), W, State);
      MinLBFGSSetCond(State, 0.0, 0.0, WStep, MaxIts);
      Ebegin := MLPErrorN(NetworkCopy, XY, NPoints);

      // Compare with best
      if AP_FP_Less(Ebegin, EBest) then
        begin
          zLock.Enter;
          APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          EBest := Ebegin;
          zLock.Leave;
        end;

      while (EBest > 1E-50) and MinLBFGSIteration(State) do
        begin
          APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.X[0], 0, WCount - 1);
          MLPGradNBatch(NetworkCopy, XY, NPoints, State.F, State.G);
          V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);

          State.F := State.F + 0.5 * Decay * V;
          APVAdd(@State.G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
          RepCopy.NGrad := RepCopy.NGrad + 1;

          // checking
          MinLBFGSResults(State, W, InternalRep);
          APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @W[0], 0, WCount - 1);
          E := MLPErrorN(NetworkCopy, XY, NPoints);

          if (E <= Ebegin) then
            begin
              Ebegin := E;
              if (E < 1E-50) then
                  Break;
            end;

          { dynamic change of decay }
          Inc(Iteration);
          // Decay := 1 / ((10 + Iteration) div 10);
          Decay := 1 / Iteration;
        end;

      // Compare with best
      if AP_FP_Less(Ebegin, EBest) then
        begin
          zLock.Enter;
          APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          EBest := Ebegin;
          zLock.Leave;
        end;

    finally
      MLPFree(NetworkCopy);
      MinLBFGSFree(W, State);
    end;

  end;
{$ENDIF FPC}


begin

  //
  // Test inputs, parse flags, read network geometry
  //
  if AP_FP_Eq(WStep, 0) and (MaxIts = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MaxIts < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NPoints - 1 do
        begin
          if (Round(XY[I, NIn]) < 0) or (Round(XY[I, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(I);
        end;
    end;
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, XY, NPoints);
  SetLength(WBest, WCount (* - 1 + 1 // optimized compiler *) );
  EBest := MaxRealNumber;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;

  NetworkPtr := @Network;
  RepCopy := Rep;
  zLock := TCriticalSection.Create;

  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, PtrInt(Restarts));
  {$ELSE}
  TParallel.For(1, Restarts, procedure(Pass: TLInt)
    var
      Ebegin: TLFloat; { the initial error }
      E: TLFloat;
      V: TLFloat;
      W: TLVec;
      State: TMinLBFGSState;
      InternalRep: TMinLBFGSReport;
      NetworkCopy: TMultiLayerPerceptron;
      Decay: TLFloat;
      Iteration: TLInt;
    begin
      try
        Decay := 1;
        Iteration := 0;
        SetLength(W, WCount (* - 1 + 1 // optimized compiler *) );
        MLPCopy(NetworkPtr^, NetworkCopy);

        //
        // Process
        //
        MLPRandomize(NetworkCopy, Diameter);

        APVMove(@W[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        MinLBFGSCreate(WCount, Min(WCount, 10), W, State);
        MinLBFGSSetCond(State, 0.0, 0.0, WStep, MaxIts);
        Ebegin := MLPErrorN(NetworkCopy, XY, NPoints);

        // Compare with best
        if AP_FP_Less(Ebegin, EBest) then
          begin
            zLock.Enter;
            APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
            EBest := Ebegin;
            zLock.Leave;
          end;

        while (EBest > 1E-50) and MinLBFGSIteration(State) do
          begin
            APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.X[0], 0, WCount - 1);
            MLPGradNBatch(NetworkCopy, XY, NPoints, State.F, State.G);
            V := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);

            State.F := State.F + 0.5 * Decay * V;
            APVAdd(@State.G[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
            RepCopy.NGrad := RepCopy.NGrad + 1;

            // checking
            MinLBFGSResults(State, W, InternalRep);
            APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @W[0], 0, WCount - 1);
            E := MLPErrorN(NetworkCopy, XY, NPoints);

            if (E <= Ebegin) then
              begin
                Ebegin := E;
                if (E < 1E-50) then
                    Break;
              end;

            { dynamic change of decay }
            Inc(Iteration);
            // Decay := 1 / ((10 + Iteration) div 10);
            Decay := 1 / Iteration;
          end;

        // Compare with best
        if AP_FP_Less(Ebegin, EBest) then
          begin
            zLock.Enter;
            APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
              WCount - 1);
            EBest := Ebegin;
            zLock.Leave;
          end;

      finally
        MLPFree(NetworkCopy);
        MinLBFGSFree(W, State);
      end;

    end);
  {$ENDIF FPC}
  zLock.Free;

  //
  // The best network
  //
  Rep := RepCopy;

  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

procedure MLPTrainMonteCarlo(var Network: TMultiLayerPerceptron; const XY: TLMatrix; NPoints: TLInt;
const MainRestarts, SubRestarts: TLInt; const MinError: TLFloat;
Diameter: TLFloat; var Info: TLInt; var Rep: TMLPReport);
var
  I                : TLInt;
  MainPass, SubPass: TLInt;
  NIn              : TLInt;
  NOut             : TLInt;
  WCount           : TLInt;
  W                : TLVec;
  WBest            : TLVec;
  E                : TLFloat;
  EBest            : TLFloat;
  EBestSub         : TLFloat;
begin

  //
  // Test inputs, parse flags, read network geometry
  //
  if (NPoints <= 0) or (MainRestarts < 1) or (SubRestarts < 1) or
    (Diameter < 0.01) then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NPoints - 1 do
        begin
          if (Round(XY[I, NIn]) < 0) or (Round(XY[I, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(I);
        end;
    end;
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, XY, NPoints);
  SetLength(W, WCount (* - 1 + 1 // optimized compler *) );
  SetLength(WBest, WCount (* - 1 + 1 // optimized compler *) );
  APVFillValue(@WBest[0], 0, WCount - 1, 0);
  EBest := MaxRealNumber;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;

  for MainPass := 1 to MainRestarts do
    begin
      EBestSub := MaxRealNumber;
      for SubPass := 1 to SubRestarts do
        begin

          // Process
          MLPRandomize(Network, WBest, Diameter);

          // Compare with best
          E := MLPErrorN(Network, XY, NPoints);
          if AP_FP_Less(E, EBestSub) then
            begin
              if AP_FP_Less(E, EBest) then
                  APVMove(@W[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
              EBestSub := E;
            end;

          if AP_FP_Less(EBestSub, MinError) then
              Break;

        end;

      // The best network
      if AP_FP_Less(EBestSub, EBest) then
        begin
          APVMove(@WBest[0], 0, WCount - 1, @W[0], 0, WCount - 1);
          EBest := EBestSub;
        end;

      if AP_FP_Less(EBest, MinError) then
          Break;

      Diameter := Diameter * 0.5;
    end;
  // The best network
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

(* ************************************************************************
  Cross-validation estimate of generalization error.

  Base algorithm - L-BFGS.

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry.   Network is
  not changed during cross-validation -  it is used only
  as a representative of its architecture.
  XY          -   training set.
  SSize       -   training set size
  Decay       -   weight  decay, same as in MLPTrainLBFGS
  Restarts    -   number of restarts, >0.
  restarts are counted for each partition separately, so
  total number of restarts will be Restarts*FoldsCount.
  WStep       -   stopping criterion, same as in MLPTrainLBFGS
  MaxIts      -   stopping criterion, same as in MLPTrainLBFGS
  FoldsCount  -   number of folds in k-fold cross-validation, 2<=FoldsCount<=SSize. recommended value: 10.

  OUTPUT PARAMETERS:
  Info        -   return code, same as in MLPTrainLBFGS
  Rep         -   report, same as in MLPTrainLM/MLPTrainLBFGS
  CVRep       -   generalization error estimates
  ************************************************************************ *)
procedure MLPKFoldCVLBFGS(const Network: TMultiLayerPerceptron;
const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; WStep: TLFloat; MaxIts: TLInt;
FoldsCount: TLInt; var Info: TLInt; var Rep: TMLPReport;
var CVRep: TMLPCVReport);
begin
  MLPKFoldCVGeneral(Network, XY, NPoints, Decay, Restarts, FoldsCount, False, WStep, MaxIts, Info, Rep, CVRep);
end;

(* ************************************************************************
  Cross-validation estimate of generalization error.

  Base algorithm - Levenberg-Marquardt.

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry.   Network is
  not changed during cross-validation -  it is used only
  as a representative of its architecture.
  XY          -   training set.
  SSize       -   training set size
  Decay       -   weight  decay, same as in MLPTrainLBFGS
  Restarts    -   number of restarts, >0.
  restarts are counted for each partition separately, so
  total number of restarts will be Restarts*FoldsCount.
  FoldsCount  -   number of folds in k-fold cross-validation, 2<=FoldsCount<=SSize. recommended value: 10.

  OUTPUT PARAMETERS:
  Info        -   return code, same as in MLPTrainLBFGS
  Rep         -   report, same as in MLPTrainLM/MLPTrainLBFGS
  CVRep       -   generalization error estimates
  ************************************************************************ *)
procedure MLPKFoldCVLM(const Network: TMultiLayerPerceptron;
const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; FoldsCount: TLInt; var Info: TLInt;
var Rep: TMLPReport; var CVRep: TMLPCVReport);
begin
  MLPKFoldCVGeneral(Network, XY, NPoints, Decay, Restarts, FoldsCount, True, 0.0, 0, Info, Rep, CVRep);
end;

(* ************************************************************************
  Internal cross-validation subroutine
  ************************************************************************ *)
procedure MLPKFoldCVGeneral(const N: TMultiLayerPerceptron;
const XY: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; FoldsCount: TLInt; LMAlgorithm: boolean;
WStep: TLFloat; MaxIts: TLInt; var Info: TLInt;
var Rep: TMLPReport; var CVRep: TMLPCVReport);
var
  I           : TLInt;
  Fold        : TLInt;
  J           : TLInt;
  K           : TLInt;
  Network     : TMultiLayerPerceptron;
  NIn         : TLInt;
  NOut        : TLInt;
  RowLen      : TLInt;
  WCount      : TLInt;
  NClasses    : TLInt;
  TSSize      : TLInt;
  CVSSize     : TLInt;
  CVSet       : TLMatrix;
  TestSet     : TLMatrix;
  Folds       : TLIVec;
  RelCnt      : TLInt;
  InternalRep : TMLPReport;
  X           : TLVec;
  Y           : TLVec;
  IsTerminated: boolean;
  EBest       : TLFloat;
begin

  //
  // Read network geometry, test parameters
  //
  MLPProperties(N, NIn, NOut, WCount);
  if MLPIsSoftmax(N) then
    begin
      NClasses := NOut;
      RowLen := NIn + 1;
    end
  else
    begin
      NClasses := -NOut;
      RowLen := NIn + NOut;
    end;
  if (NPoints <= 0) or (FoldsCount < 2) or (FoldsCount > NPoints) then
    begin
      Info := -1;
      Exit;
    end;
  MLPCopy(N, Network);

  //
  // K-fold out cross-validation.
  // First, estimate generalization error
  //
  SetLength(TestSet, NPoints (* - 1 + 1 // optimized compiler *) , RowLen (* - 1 + 1 // optimized compiler *) );
  SetLength(CVSet, NPoints (* - 1 + 1 // optimized compiler *) , RowLen (* - 1 + 1 // optimized compiler *) );
  SetLength(X, NIn (* - 1 + 1 // optimized compiler *) );
  SetLength(Y, NOut (* - 1 + 1 // optimized compiler *) );
  MLPKFoldSplit(XY, NPoints, NClasses, FoldsCount, False, Folds);
  CVRep.RelCLSError := 0;
  CVRep.AvgCE := 0;
  CVRep.RMSError := 0;
  CVRep.AvgError := 0;
  CVRep.AvgRelError := 0;
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;
  RelCnt := 0;
  Fold := 0;
  while Fold <= FoldsCount - 1 do
    begin

      //
      // Separate set
      //
      TSSize := 0;
      CVSSize := 0;
      I := 0;
      while I <= NPoints - 1 do
        begin
          if Folds[I] = Fold then
            begin
              APVMove(@TestSet[TSSize][0], 0, RowLen - 1, @XY[I][0], 0, RowLen - 1);
              TSSize := TSSize + 1;
            end
          else
            begin
              APVMove(@CVSet[CVSSize][0], 0, RowLen - 1, @XY[I][0], 0, RowLen - 1);
              CVSSize := CVSSize + 1;
            end;
          Inc(I);
        end;

      //
      // Train on CV training set
      //
      if LMAlgorithm then
        begin
          MLPTrainLM(Network, CVSet, CVSSize, Decay, Restarts, Info, InternalRep);
        end
      else
        begin
          IsTerminated := False;
          MLPTrainLBFGS(Network, CVSet, CVSSize, Decay, Restarts, WStep, MaxIts, Info, InternalRep, @IsTerminated, EBest);
        end;
      if Info < 0 then
        begin
          CVRep.RelCLSError := 0;
          CVRep.AvgCE := 0;
          CVRep.RMSError := 0;
          CVRep.AvgError := 0;
          CVRep.AvgRelError := 0;
          Exit;
        end;
      Rep.NGrad := Rep.NGrad + InternalRep.NGrad;
      Rep.NHess := Rep.NHess + InternalRep.NHess;
      Rep.NCholesky := Rep.NCholesky + InternalRep.NCholesky;

      //
      // Estimate error using CV test set
      //
      if MLPIsSoftmax(Network) then
        begin

          //
          // classification-only code
          //
          CVRep.RelCLSError := CVRep.RelCLSError + MLPClsError(Network, TestSet, TSSize);
          CVRep.AvgCE := CVRep.AvgCE + MLPErrorN(Network, TestSet, TSSize);
        end;
      I := 0;
      while I <= TSSize - 1 do
        begin
          APVMove(@X[0], 0, NIn - 1, @TestSet[I][0], 0, NIn - 1);
          MLPProcess(Network, X, Y);
          if MLPIsSoftmax(Network) then
            begin

              //
              // Classification-specific code
              //
              K := Round(TestSet[I, NIn]);
              J := 0;
              while J <= NOut - 1 do
                begin
                  if J = K then
                    begin
                      CVRep.RMSError := CVRep.RMSError + AP_Sqr(Y[J] - 1);
                      CVRep.AvgError := CVRep.AvgError + AbsReal(Y[J] - 1);
                      CVRep.AvgRelError := CVRep.AvgRelError + AbsReal(Y[J] - 1);
                      RelCnt := RelCnt + 1;
                    end
                  else
                    begin
                      CVRep.RMSError := CVRep.RMSError + AP_Sqr(Y[J]);
                      CVRep.AvgError := CVRep.AvgError + AbsReal(Y[J]);
                    end;
                  Inc(J);
                end;
            end
          else
            begin

              //
              // Regression-specific code
              //
              J := 0;
              while J <= NOut - 1 do
                begin
                  CVRep.RMSError := CVRep.RMSError + AP_Sqr(Y[J] - TestSet[I, NIn + J]);
                  CVRep.AvgError := CVRep.AvgError + AbsReal(Y[J] - TestSet[I, NIn + J]);
                  if AP_FP_Neq(TestSet[I, NIn + J], 0) then
                    begin
                      CVRep.AvgRelError := CVRep.AvgRelError + AbsReal((Y[J] - TestSet[I, NIn + J]) / TestSet[I, NIn + J]);
                      RelCnt := RelCnt + 1;
                    end;
                  Inc(J);
                end;
            end;
          Inc(I);
        end;
      Inc(Fold);
    end;
  if MLPIsSoftmax(Network) then
    begin
      CVRep.RelCLSError := CVRep.RelCLSError / NPoints;
      CVRep.AvgCE := CVRep.AvgCE / (Ln(2) * NPoints);
    end;
  CVRep.RMSError := Sqrt(CVRep.RMSError / (NPoints * NOut));
  CVRep.AvgError := CVRep.AvgError / (NPoints * NOut);
  CVRep.AvgRelError := CVRep.AvgRelError / RelCnt;
  Info := 1;
end;

(* ************************************************************************
  Subroutine prepares K-fold split of the training set.

  NOTES:
  "NClasses>0" means that we have classification task.
  "NClasses<0" means regression task with -NClasses real outputs.
  ************************************************************************ *)
procedure MLPKFoldSplit(const XY: TLMatrix; NPoints: TLInt;
NClasses: TLInt; FoldsCount: TLInt; StratifiedSplits: boolean;
var Folds: TLIVec);
var
  I: TLInt;
  J: TLInt;
  K: TLInt;
begin

  //
  // test parameters
  //
  Assert(NPoints > 0, 'MLPKFoldSplit: wrong NPoints!');
  Assert((NClasses > 1) or (NClasses < 0), 'MLPKFoldSplit: wrong NClasses!');
  Assert((FoldsCount >= 2) and (FoldsCount <= NPoints),
    'MLPKFoldSplit: wrong FoldsCount!');
  Assert(not StratifiedSplits,
    'MLPKFoldSplit: stratified splits are not supported!');

  //
  // Folds
  //
  SetLength(Folds, NPoints (* - 1 + 1 // optimized compiler *) );
  I := 0;
  while I <= NPoints - 1 do
    begin
      Folds[I] := I * FoldsCount div NPoints;
      Inc(I);
    end;
  I := 0;
  while I <= NPoints - 2 do
    begin
      J := I + RandomInteger(NPoints - I);
      if J <> I then
        begin
          K := Folds[I];
          Folds[I] := Folds[J];
          Folds[J] := K;
        end;
      Inc(I);
    end;
end;
