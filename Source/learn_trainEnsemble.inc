{ ****************************************************************************** }
{ * Neural networks ensemble support, by QQ 600585@qq.com                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

type
  (* ************************************************************************
    Neural networks ensemble
    ************************************************************************ *)
  TMLPEnsemble = packed record
    StructInfo: TLearnIntegerArray;
    EnsembleSize: TLearnInteger;
    NIn: TLearnInteger;
    NOut: TLearnInteger;
    WCount: TLearnInteger;
    IsSoftmax: Boolean;
    PostProcessing: Boolean;
    Weights: TLearnFloatArray;
    ColumnMeans: TLearnFloatArray;
    ColumnSigmas: TLearnFloatArray;
    SerializedLen: TLearnInteger;
    SerializedMLP: TLearnFloatArray;
    TmpWeights: TLearnFloatArray;
    TmpMeans: TLearnFloatArray;
    TmpSigmas: TLearnFloatArray;
    Neurons: TLearnFloatArray;
    DFDNET: TLearnFloatArray;
    Y: TLearnFloatArray;
  end;

  PMLPEnsemble = ^TMLPEnsemble;

procedure MLPECreate0(NIn: TLearnInteger; NOut: TLearnInteger;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreate1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreate2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; EnsembleSize: TLearnInteger;
  var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateB0(NIn: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat;
  D: TLearnFloat; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateB1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateB2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateR0(NIn: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat;
  B: TLearnFloat; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateR1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateR2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateC0(NIn: TLearnInteger; NOut: TLearnInteger;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateC1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateC2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; EnsembleSize: TLearnInteger;
  var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateFromNetwork(const Network: TMultiLayerPerceptron;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPECopy(const Ensemble1: TMLPEnsemble; var Ensemble2: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPESerialize(var Ensemble: TMLPEnsemble; var RA: TLearnFloatArray; var RLen: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPEUnserialize(const RA: TLearnFloatArray; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPERandomize(var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPEProperties(const Ensemble: TMLPEnsemble; var NIn: TLearnInteger; var NOut: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPEIsSoftmax(const Ensemble: TMLPEnsemble): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPEProcess(var Ensemble: TMLPEnsemble; const X: TLearnFloatArray; var Y: TLearnFloatArray); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MLPERelClsError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPEAvgCE(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPERMSError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPEAvgError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPEAvgRelError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPEBaggingLM(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger; Decay: TLearnFloat; Restarts: TLearnInteger;
  var Info: TLearnInteger; var Rep: TMLPReport; var OOBErrors: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPEBaggingLBFGS(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger; Decay: TLearnFloat; Restarts: TLearnInteger;
  WStep: TLearnFloat; MaxIts: TLearnInteger; var Info: TLearnInteger;
  var Rep: TMLPReport; var OOBErrors: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


const
  MLPNTotalOffset = 3;
  MLPEVNum        = 9;

procedure MLPEAllErrors(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger; var RelCls: TLearnFloat; var AvgCE: TLearnFloat;
  var RMS: TLearnFloat; var Avg: TLearnFloat; var AvgRel: TLearnFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPEBaggingInternal(const MultiThread: Boolean; var Ensemble: TMLPEnsemble;
  const XY: TLearnFloat2DArray; NPoints: TLearnInteger; Decay: TLearnFloat;
  Restarts: TLearnInteger; WStep: TLearnFloat; MaxIts: TLearnInteger;
  LMAlgorithm: Boolean; var Info: TLearnInteger; var Rep: TMLPReport;
  var OOBErrors: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


procedure DSErrAllocate(NClasses: TLearnInteger; var Buf: TLearnFloatArray);
begin
  SetLength(Buf, 7 + 1);
  Buf[0] := 0;
  Buf[1] := 0;
  Buf[2] := 0;
  Buf[3] := 0;
  Buf[4] := 0;
  Buf[5] := NClasses;
  Buf[6] := 0;
  Buf[7] := 0;
end;

procedure DSErrAccumulate(var Buf: TLearnFloatArray; const Y: TLearnFloatArray; const DesiredY: TLearnFloatArray);
var
  NClasses: TLearnInteger;
  NOut    : TLearnInteger;
  Offs    : TLearnInteger;
  MMax    : TLearnInteger;
  RMax    : TLearnInteger;
  J       : TLearnInteger;
  V       : TLearnFloat;
  EV      : TLearnFloat;
begin
  Offs := 5;
  NClasses := Round(Buf[Offs]);
  if NClasses > 0 then
    begin

      //
      // Classification
      //
      RMax := Round(DesiredY[0]);
      MMax := 0;
      J := 1;
      while J <= NClasses - 1 do
        begin
          if AP_FP_Greater(Y[J], Y[MMax]) then
            begin
              MMax := J;
            end;
          Inc(J);
        end;
      if MMax <> RMax then
        begin
          Buf[0] := Buf[0] + 1;
        end;
      if AP_FP_Greater(Y[RMax], 0) then
        begin
          Buf[1] := Buf[1] - Ln(Y[RMax]);
        end
      else
        begin
          Buf[1] := Buf[1] + Ln(MaxRealNumber);
        end;
      J := 0;
      while J <= NClasses - 1 do
        begin
          V := Y[J];
          if J = RMax then
            begin
              EV := 1;
            end
          else
            begin
              EV := 0;
            end;
          Buf[2] := Buf[2] + AP_Sqr(V - EV);
          Buf[3] := Buf[3] + AbsReal(V - EV);
          if AP_FP_Neq(EV, 0) then
            begin
              Buf[4] := Buf[4] + AbsReal((V - EV) / EV);
              Buf[Offs + 2] := Buf[Offs + 2] + 1;
            end;
          Inc(J);
        end;
      Buf[Offs + 1] := Buf[Offs + 1] + 1;
    end
  else
    begin

      //
      // Regression
      //
      NOut := -NClasses;
      RMax := 0;
      J := 1;
      while J <= NOut - 1 do
        begin
          if AP_FP_Greater(DesiredY[J], DesiredY[RMax]) then
            begin
              RMax := J;
            end;
          Inc(J);
        end;
      MMax := 0;
      J := 1;
      while J <= NOut - 1 do
        begin
          if AP_FP_Greater(Y[J], Y[MMax]) then
            begin
              MMax := J;
            end;
          Inc(J);
        end;
      if MMax <> RMax then
        begin
          Buf[0] := Buf[0] + 1;
        end;
      J := 0;
      while J <= NOut - 1 do
        begin
          V := Y[J];
          EV := DesiredY[J];
          Buf[2] := Buf[2] + AP_Sqr(V - EV);
          Buf[3] := Buf[3] + AbsReal(V - EV);
          if AP_FP_Neq(EV, 0) then
            begin
              Buf[4] := Buf[4] + AbsReal((V - EV) / EV);
              Buf[Offs + 2] := Buf[Offs + 2] + 1;
            end;
          Inc(J);
        end;
      Buf[Offs + 1] := Buf[Offs + 1] + 1;
    end;
end;

procedure DSErrFinish(var Buf: TLearnFloatArray);
var
  NOut: TLearnInteger;
  Offs: TLearnInteger;
begin
  Offs := 5;
  NOut := AbsInt(Round(Buf[Offs]));
  if AP_FP_Neq(Buf[Offs + 1], 0) then
    begin
      Buf[0] := Buf[0] / Buf[Offs + 1];
      Buf[1] := Buf[1] / Buf[Offs + 1];
      Buf[2] := Sqrt(Buf[2] / (NOut * Buf[Offs + 1]));
      Buf[3] := Buf[3] / (NOut * Buf[Offs + 1]);
    end;
  if AP_FP_Neq(Buf[Offs + 2], 0) then
    begin
      Buf[4] := Buf[4] / Buf[Offs + 2];
    end;
end;

(* ************************************************************************
  Like MLPCreate0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate0(NIn: TLearnInteger; NOut: TLearnInteger;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreate0(NIn, NOut, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreate1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreate1(NIn, NHid, NOut, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreate2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; EnsembleSize: TLearnInteger;
  var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreate2(NIn, NHid1, NHid2, NOut, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB0(NIn: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat;
  D: TLearnFloat; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateB0(NIn, NOut, B, D, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateB1(NIn, NHid, NOut, B, D, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateB2(NIn, NHid1, NHid2, NOut, B, D, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR0(NIn: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat;
  B: TLearnFloat; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateR0(NIn, NOut, A, B, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateR1(NIn, NHid, NOut, A, B, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateR2(NIn, NHid1, NHid2, NOut, A, B, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC0(NIn: TLearnInteger; NOut: TLearnInteger;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateC0(NIn, NOut, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateC1(NIn, NHid, NOut, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; EnsembleSize: TLearnInteger;
  var Ensemble: TMLPEnsemble);
var
  Net: TMultiLayerPerceptron;
begin
  MLPCreateC2(NIn, NHid1, NHid2, NOut, Net);
  MLPECreateFromNetwork(Net, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Creates ensemble from network. Only network geometry is copied.
  ************************************************************************ *)
procedure MLPECreateFromNetwork(const Network: TMultiLayerPerceptron;
  EnsembleSize: TLearnInteger; var Ensemble: TMLPEnsemble);
var
  I     : TLearnInteger;
  CCount: TLearnInteger;
begin
  Assert(EnsembleSize > 0, 'MLPECreate: incorrect ensemble size!');

  //
  // network properties
  //
  MLPProperties(Network, Ensemble.NIn, Ensemble.NOut, Ensemble.WCount);
  if MLPIsSoftmax(Network) then
    begin
      CCount := Ensemble.NIn;
    end
  else
    begin
      CCount := Ensemble.NIn + Ensemble.NOut;
    end;
  Ensemble.PostProcessing := False;
  Ensemble.IsSoftmax := MLPIsSoftmax(Network);
  Ensemble.EnsembleSize := EnsembleSize;

  //
  // structure information
  //
  SetLength(Ensemble.StructInfo, Network.StructInfo[0] (* - 1 + 1 // optimized compiler *) );
  I := 0;
  while I <= Network.StructInfo[0] - 1 do
    begin
      Ensemble.StructInfo[I] := Network.StructInfo[I];
      Inc(I);
    end;

  //
  // weights, means, sigmas
  //
  SetLength(Ensemble.Weights, EnsembleSize * Ensemble.WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.ColumnMeans, EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.ColumnSigmas, EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  I := 0;
  while I <= EnsembleSize * Ensemble.WCount - 1 do
    begin
      Ensemble.Weights[I] := RandomReal - 0.5;
      Inc(I);
    end;
  I := 0;
  while I <= EnsembleSize - 1 do
    begin
      APVMove(@Ensemble.ColumnMeans[0], I * CCount, (I + 1) * CCount - 1,
        @Network.ColumnMeans[0], 0, CCount - 1);
      APVMove(@Ensemble.ColumnSigmas[0], I * CCount, (I + 1) * CCount - 1,
        @Network.ColumnSigmas[0], 0, CCount - 1);
      Inc(I);
    end;

  //
  // serialized part
  //
  MLPSerialize(Network, Ensemble.SerializedMLP, Ensemble.SerializedLen);

  //
  // temporaries, internal buffers
  //
  SetLength(Ensemble.TmpWeights, Ensemble.WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.TmpMeans, CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.TmpSigmas, CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.Neurons, Ensemble.StructInfo[MLPNTotalOffset] (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.DFDNET, Ensemble.StructInfo[MLPNTotalOffset] (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.Y, Ensemble.NOut (* - 1 + 1 // optimized compiler *) );
end;

(* ************************************************************************
  Copying of TMLPEnsemble strucure

  INPUT PARAMETERS:
  Ensemble1 -   original

  OUTPUT PARAMETERS:
  Ensemble2 -   copy
  ************************************************************************ *)
procedure MLPECopy(const Ensemble1: TMLPEnsemble; var Ensemble2: TMLPEnsemble);
var
  I     : TLearnInteger;
  SSize : TLearnInteger;
  CCount: TLearnInteger;
  NTotal: TLearnInteger;
begin

  //
  // Unload info
  //
  SSize := Ensemble1.StructInfo[0];
  if Ensemble1.IsSoftmax then
    begin
      CCount := Ensemble1.NIn;
    end
  else
    begin
      CCount := Ensemble1.NIn + Ensemble1.NOut;
    end;
  NTotal := Ensemble1.StructInfo[MLPNTotalOffset];

  //
  // Allocate space
  //
  SetLength(Ensemble2.StructInfo, SSize (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.Weights, Ensemble1.EnsembleSize *
    Ensemble1.WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.ColumnMeans, Ensemble1.EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.ColumnSigmas, Ensemble1.EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.TmpWeights, Ensemble1.WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.TmpMeans, CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.TmpSigmas, CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.SerializedMLP, Ensemble1.SerializedLen (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.Neurons, NTotal (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.DFDNET, NTotal (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble2.Y, Ensemble1.NOut (* - 1 + 1 // optimized compiler *) );

  //
  // Copy
  //
  Ensemble2.NIn := Ensemble1.NIn;
  Ensemble2.NOut := Ensemble1.NOut;
  Ensemble2.WCount := Ensemble1.WCount;
  Ensemble2.EnsembleSize := Ensemble1.EnsembleSize;
  Ensemble2.IsSoftmax := Ensemble1.IsSoftmax;
  Ensemble2.PostProcessing := Ensemble1.PostProcessing;
  Ensemble2.SerializedLen := Ensemble1.SerializedLen;
  I := 0;
  while I <= SSize - 1 do
    begin
      Ensemble2.StructInfo[I] := Ensemble1.StructInfo[I];
      Inc(I);
    end;
  APVMove(@Ensemble2.Weights[0], 0, Ensemble1.EnsembleSize * Ensemble1.WCount -
    1, @Ensemble1.Weights[0], 0, Ensemble1.EnsembleSize * Ensemble1.WCount - 1);
  APVMove(@Ensemble2.ColumnMeans[0], 0, Ensemble1.EnsembleSize * CCount - 1,
    @Ensemble1.ColumnMeans[0], 0, Ensemble1.EnsembleSize * CCount - 1);
  APVMove(@Ensemble2.ColumnSigmas[0], 0, Ensemble1.EnsembleSize * CCount - 1,
    @Ensemble1.ColumnSigmas[0], 0, Ensemble1.EnsembleSize * CCount - 1);
  APVMove(@Ensemble2.SerializedMLP[0], 0, Ensemble1.SerializedLen - 1,
    @Ensemble1.SerializedMLP[0], 0, Ensemble1.SerializedLen - 1);
end;

(* ************************************************************************
  Serialization of TMLPEnsemble strucure

  INPUT PARAMETERS:
  Ensemble-   original

  OUTPUT PARAMETERS:
  RA      -   array of real numbers which stores ensemble,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure MLPESerialize(var Ensemble: TMLPEnsemble; var RA: TLearnFloatArray;
  var RLen: TLearnInteger);
var
  I     : TLearnInteger;
  SSize : TLearnInteger;
  NTotal: TLearnInteger;
  CCount: TLearnInteger;
  HSize : TLearnInteger;
  Offs  : TLearnInteger;
begin
  HSize := 13;
  SSize := Ensemble.StructInfo[0];
  if Ensemble.IsSoftmax then
    begin
      CCount := Ensemble.NIn;
    end
  else
    begin
      CCount := Ensemble.NIn + Ensemble.NOut;
    end;
  NTotal := Ensemble.StructInfo[MLPNTotalOffset];
  RLen := HSize + SSize + Ensemble.EnsembleSize * Ensemble.WCount + 2 * CCount *
    Ensemble.EnsembleSize + Ensemble.SerializedLen;

  //
  // RA format:
  // [0]     RLen
  // [1]     Version (MLPEVNum)
  // [2]     EnsembleSize
  // [3]     NIn
  // [4]     NOut
  // [5]     WCount
  // [6]     IsSoftmax 0/1
  // [7]     PostProcessing 0/1
  // [8]     sizeof(StructInfo)
  // [9]     NTotal (sizeof(Neurons), sizeof(DFDNET))
  // [10]    CCount (sizeof(ColumnMeans), sizeof(ColumnSigmas))
  // [11]    data offset
  // [12]    SerializedLen
  //
  // [..]    StructInfo
  // [..]    Weights
  // [..]    ColumnMeans
  // [..]    ColumnSigmas
  //
  SetLength(RA, RLen (* - 1 + 1 // optimized compiler *) );
  RA[0] := RLen;
  RA[1] := MLPEVNum;
  RA[2] := Ensemble.EnsembleSize;
  RA[3] := Ensemble.NIn;
  RA[4] := Ensemble.NOut;
  RA[5] := Ensemble.WCount;
  if Ensemble.IsSoftmax then
    begin
      RA[6] := 1;
    end
  else
    begin
      RA[6] := 0;
    end;
  if Ensemble.PostProcessing then
    begin
      RA[7] := 1;
    end
  else
    begin
      RA[7] := 9;
    end;
  RA[8] := SSize;
  RA[9] := NTotal;
  RA[10] := CCount;
  RA[11] := HSize;
  RA[12] := Ensemble.SerializedLen;
  Offs := HSize;
  I := Offs;
  while I <= Offs + SSize - 1 do
    begin
      RA[I] := Ensemble.StructInfo[I - Offs];
      Inc(I);
    end;
  Offs := Offs + SSize;
  APVMove(@RA[0], Offs, Offs + Ensemble.EnsembleSize * Ensemble.WCount - 1,
    @Ensemble.Weights[0], 0, Ensemble.EnsembleSize * Ensemble.WCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * Ensemble.WCount;
  APVMove(@RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1,
    @Ensemble.ColumnMeans[0], 0, Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1,
    @Ensemble.ColumnSigmas[0], 0, Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@RA[0], Offs, Offs + Ensemble.SerializedLen - 1,
    @Ensemble.SerializedMLP[0], 0, Ensemble.SerializedLen - 1);
  Offs := Offs + Ensemble.SerializedLen;
end;

(* ************************************************************************
  Unserialization of TMLPEnsemble strucure

  INPUT PARAMETERS:
  RA      -   real array which stores ensemble

  OUTPUT PARAMETERS:
  Ensemble-   restored structure
  ************************************************************************ *)
procedure MLPEUnserialize(const RA: TLearnFloatArray; var Ensemble: TMLPEnsemble);
var
  I     : TLearnInteger;
  SSize : TLearnInteger;
  NTotal: TLearnInteger;
  CCount: TLearnInteger;
  HSize : TLearnInteger;
  Offs  : TLearnInteger;
begin
  Assert(Round(RA[1]) = MLPEVNum, 'MLPEUnserialize: incorrect array!');

  //
  // load info
  //
  HSize := 13;
  Ensemble.EnsembleSize := Round(RA[2]);
  Ensemble.NIn := Round(RA[3]);
  Ensemble.NOut := Round(RA[4]);
  Ensemble.WCount := Round(RA[5]);
  Ensemble.IsSoftmax := Round(RA[6]) = 1;
  Ensemble.PostProcessing := Round(RA[7]) = 1;
  SSize := Round(RA[8]);
  NTotal := Round(RA[9]);
  CCount := Round(RA[10]);
  Offs := Round(RA[11]);
  Ensemble.SerializedLen := Round(RA[12]);

  //
  // Allocate arrays
  //
  SetLength(Ensemble.StructInfo, SSize (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.Weights, Ensemble.EnsembleSize * Ensemble.WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.ColumnMeans, Ensemble.EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.ColumnSigmas, Ensemble.EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.TmpWeights, Ensemble.WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.TmpMeans, CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.TmpSigmas, CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.Neurons, NTotal (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.DFDNET, NTotal (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.SerializedMLP, Ensemble.SerializedLen (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.Y, Ensemble.NOut (* - 1 + 1 // optimized compiler *) );

  //
  // load data
  //
  I := Offs;
  while I <= Offs + SSize - 1 do
    begin
      Ensemble.StructInfo[I - Offs] := Round(RA[I]);
      Inc(I);
    end;
  Offs := Offs + SSize;
  APVMove(@Ensemble.Weights[0], 0, Ensemble.EnsembleSize * Ensemble.WCount - 1,
    @RA[0], Offs, Offs + Ensemble.EnsembleSize * Ensemble.WCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * Ensemble.WCount;
  APVMove(@Ensemble.ColumnMeans[0], 0, Ensemble.EnsembleSize * CCount - 1,
    @RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@Ensemble.ColumnSigmas[0], 0, Ensemble.EnsembleSize * CCount - 1,
    @RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@Ensemble.SerializedMLP[0], 0, Ensemble.SerializedLen - 1, @RA[0],
    Offs, Offs + Ensemble.SerializedLen - 1);
  Offs := Offs + Ensemble.SerializedLen;
end;

(* ************************************************************************
  Randomization of MLP ensemble
  ************************************************************************ *)
procedure MLPERandomize(var Ensemble: TMLPEnsemble);
var
  I: TLearnInteger;
begin
  I := 0;
  while I <= Ensemble.EnsembleSize * Ensemble.WCount - 1 do
    begin
      Ensemble.Weights[I] := RandomReal - 0.5;
      Inc(I);
    end;
end;

(* ************************************************************************
  Return ensemble properties (number of inputs and outputs).
  ************************************************************************ *)
procedure MLPEProperties(const Ensemble: TMLPEnsemble; var NIn: TLearnInteger;
  var NOut: TLearnInteger);
begin
  NIn := Ensemble.NIn;
  NOut := Ensemble.NOut;
end;

(* ************************************************************************
  Return normalization type (whether ensemble is SOFTMAX-normalized or not).
  ************************************************************************ *)
function MLPEIsSoftmax(const Ensemble: TMLPEnsemble): Boolean;
begin
  Result := Ensemble.IsSoftmax;
end;

(* ************************************************************************
  Procesing

  INPUT PARAMETERS:
  Ensemble-   neural networks ensemble
  X       -   input vector,  array[0..NIn-1].

  OUTPUT PARAMETERS:
  Y       -   result. Regression estimate when solving regression  task,
  vector of posterior probabilities for classification task.
  Subroutine does not allocate memory for this vector, it is
  responsibility of a caller to allocate it. Array  must  be
  at least [0..NOut-1].
  ************************************************************************ *)
procedure MLPEProcess(var Ensemble: TMLPEnsemble; const X: TLearnFloatArray;
  var Y: TLearnFloatArray);
var
  I : TLearnInteger;
  ES: TLearnInteger;
  WC: TLearnInteger;
  CC: TLearnInteger;
  V : TLearnFloat;
begin
  ES := Ensemble.EnsembleSize;
  WC := Ensemble.WCount;
  if Ensemble.IsSoftmax then
    begin
      CC := Ensemble.NIn;
    end
  else
    begin
      CC := Ensemble.NIn + Ensemble.NOut;
    end;
  V := AP_Float(1) / ES;
  I := 0;
  while I <= Ensemble.NOut - 1 do
    begin
      Y[I] := 0;
      Inc(I);
    end;
  I := 0;
  while I <= ES - 1 do
    begin
      APVMove(@Ensemble.TmpWeights[0], 0, WC - 1, @Ensemble.Weights[0], I * WC,
        (I + 1) * WC - 1);
      APVMove(@Ensemble.TmpMeans[0], 0, CC - 1, @Ensemble.ColumnMeans[0], I * CC,
        (I + 1) * CC - 1);
      APVMove(@Ensemble.TmpSigmas[0], 0, CC - 1, @Ensemble.ColumnSigmas[0],
        I * CC, (I + 1) * CC - 1);
      MLPInternalProcessVector(Ensemble.StructInfo, Ensemble.TmpWeights,
        Ensemble.TmpMeans, Ensemble.TmpSigmas, Ensemble.Neurons, Ensemble.DFDNET,
        X, Ensemble.Y);
      APVAdd(@Y[0], 0, Ensemble.NOut - 1, @Ensemble.Y[0], 0,
        Ensemble.NOut - 1, V);
      Inc(I);
    end;
end;

(* ************************************************************************
  Relative classification error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  percent of incorrectly classified cases.
  Works both for classifier betwork and for regression networks which
  are used as classifiers.
  ************************************************************************ *)
function MLPERelClsError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  RelCls: TLearnFloat;
  AvgCE : TLearnFloat;
  RMS   : TLearnFloat;
  Avg   : TLearnFloat;
  AvgRel: TLearnFloat;
begin
  MLPEAllErrors(Ensemble, XY, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := RelCls;
end;

(* ************************************************************************
  Average cross-entropy (in bits per element) on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  CrossEntropy/(NPoints*LN(2)).
  Zero if ensemble solves regression task.
  ************************************************************************ *)
function MLPEAvgCE(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  RelCls: TLearnFloat;
  AvgCE : TLearnFloat;
  RMS   : TLearnFloat;
  Avg   : TLearnFloat;
  AvgRel: TLearnFloat;
begin
  MLPEAllErrors(Ensemble, XY, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := AvgCE;
end;

(* ************************************************************************
  RMS error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  root mean square error.
  Its meaning for regression task is obvious. As for classification task
  RMS error means error when estimating posterior probabilities.
  ************************************************************************ *)
function MLPERMSError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  RelCls: TLearnFloat;
  AvgCE : TLearnFloat;
  RMS   : TLearnFloat;
  Avg   : TLearnFloat;
  AvgRel: TLearnFloat;
begin
  MLPEAllErrors(Ensemble, XY, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := RMS;
end;

(* ************************************************************************
  Average error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for classification task
  it means average error when estimating posterior probabilities.
  ************************************************************************ *)
function MLPEAvgError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  RelCls: TLearnFloat;
  AvgCE : TLearnFloat;
  RMS   : TLearnFloat;
  Avg   : TLearnFloat;
  AvgRel: TLearnFloat;
begin
  MLPEAllErrors(Ensemble, XY, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := Avg;
end;

(* ************************************************************************
  Average relative error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for classification task
  it means average relative error when estimating posterior probabilities.
  ************************************************************************ *)
function MLPEAvgRelError(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  RelCls: TLearnFloat;
  AvgCE : TLearnFloat;
  RMS   : TLearnFloat;
  Avg   : TLearnFloat;
  AvgRel: TLearnFloat;
begin
  MLPEAllErrors(Ensemble, XY, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := AvgRel;
end;

(* ************************************************************************
  Training neural networks ensemble using  bootstrap  aggregating (bagging).
  Modified Levenberg-Marquardt algorithm is used as base training method.

  INPUT PARAMETERS:
  MultiThread -   parallel train
  Ensemble    -   model with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay coefficient, >=0.001
  Restarts    -   restarts, >0.

  OUTPUT PARAMETERS:
  Ensemble    -   trained model
  Info        -   return code:
  * -2, if there is a point with class number outside of [0..NClasses-1].
  * -1, if incorrect parameters was passed (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report.
  OOBErrors   -   out-of-bag generalization error estimate
  ************************************************************************ *)
procedure MLPEBaggingLM(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger; Decay: TLearnFloat; Restarts: TLearnInteger;
  var Info: TLearnInteger; var Rep: TMLPReport; var OOBErrors: TMLPCVReport);
begin
  MLPEBaggingInternal(MultiThread, Ensemble, XY, NPoints, Decay, Restarts, 0.0, 0, True,
    Info, Rep, OOBErrors);
end;

(* ************************************************************************
  Training neural networks ensemble using  bootstrap  aggregating (bagging).
  L-BFGS algorithm is used as base training method.

  INPUT PARAMETERS:
  MultiThread -   parallel train
  Ensemble    -   model with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay coefficient, >=0.001
  Restarts    -   restarts, >0.
  WStep       -   stopping criterion, same as in MLPTrainLBFGS
  MaxIts      -   stopping criterion, same as in MLPTrainLBFGS

  OUTPUT PARAMETERS:
  Ensemble    -   trained model
  Info        -   return code:
  * -8, if both WStep=0 and MaxIts=0
  * -2, if there is a point with class number outside of [0..NClasses-1].
  * -1, if incorrect parameters was passed (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report.
  OOBErrors   -   out-of-bag generalization error estimate
  ************************************************************************ *)
procedure MLPEBaggingLBFGS(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger; Decay: TLearnFloat; Restarts: TLearnInteger;
  WStep: TLearnFloat; MaxIts: TLearnInteger; var Info: TLearnInteger;
  var Rep: TMLPReport; var OOBErrors: TMLPCVReport);
begin
  MLPEBaggingInternal(MultiThread, Ensemble, XY, NPoints, Decay, Restarts, WStep, MaxIts,
    False, Info, Rep, OOBErrors);
end;

(* ************************************************************************
  Calculation of all types of errors
  ************************************************************************ *)
procedure MLPEAllErrors(var Ensemble: TMLPEnsemble; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger; var RelCls: TLearnFloat; var AvgCE: TLearnFloat;
  var RMS: TLearnFloat; var Avg: TLearnFloat; var AvgRel: TLearnFloat);
var
  I    : TLearnInteger;
  Buf  : TLearnFloatArray;
  WorkX: TLearnFloatArray;
  Y    : TLearnFloatArray;
  DY   : TLearnFloatArray;
begin
  SetLength(WorkX, Ensemble.NIn (* - 1 + 1 // optimized compiler *) );
  SetLength(Y, Ensemble.NOut (* - 1 + 1 // optimized compiler *) );
  if Ensemble.IsSoftmax then
    begin
      SetLength(DY, 0 + 1);
      DSErrAllocate(Ensemble.NOut, Buf);
    end
  else
    begin
      SetLength(DY, Ensemble.NOut (* - 1 + 1 // optimized compiler *) );
      DSErrAllocate(-Ensemble.NOut, Buf);
    end;
  I := 0;
  while I <= NPoints - 1 do
    begin
      APVMove(@WorkX[0], 0, Ensemble.NIn - 1, @XY[I][0], 0, Ensemble.NIn - 1);
      MLPEProcess(Ensemble, WorkX, Y);
      if Ensemble.IsSoftmax then
        begin
          DY[0] := XY[I, Ensemble.NIn];
        end
      else
        begin
          APVMove(@DY[0], 0, Ensemble.NOut - 1, @XY[I][0], Ensemble.NIn,
            Ensemble.NIn + Ensemble.NOut - 1);
        end;
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
  Internal bagging subroutine.
  ************************************************************************ *)
procedure MLPEBaggingInternal(const MultiThread: Boolean; var Ensemble: TMLPEnsemble;
  const XY: TLearnFloat2DArray; NPoints: TLearnInteger; Decay: TLearnFloat;
  Restarts: TLearnInteger; WStep: TLearnFloat; MaxIts: TLearnInteger;
  LMAlgorithm: Boolean; var Info: TLearnInteger; var Rep: TMLPReport;
  var OOBErrors: TMLPCVReport);
var
  XYS         : TLearnFloat2DArray;
  S           : TLearnBooleanArray;
  OOBBuf      : TLearnFloat2DArray;
  OOBCntBuf   : TLearnIntegerArray;
  X           : TLearnFloatArray;
  Y           : TLearnFloatArray;
  DY          : TLearnFloatArray;
  DSBuf       : TLearnFloatArray;
  NIn         : TLearnInteger;
  NOut        : TLearnInteger;
  CCnt        : TLearnInteger;
  PCnt        : TLearnInteger;
  I           : TLearnInteger;
  J           : TLearnInteger;
  K           : TLearnInteger;
  V           : TLearnFloat;
  TmpRep      : TMLPReport;
  Network     : TMultiLayerPerceptron;
  IsTerminated: Boolean;
  EBest       : TLearnFloat;
begin

  //
  // Test for inputs
  //
  if not LMAlgorithm and AP_FP_Eq(WStep, 0) and (MaxIts = 0) then
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
  if Ensemble.IsSoftmax then
    begin
      I := 0;
      while I <= NPoints - 1 do
        begin
          if (Round(XY[I, Ensemble.NIn]) < 0) or
            (Round(XY[I, Ensemble.NIn]) >= Ensemble.NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(I);
        end;
    end;

  //
  // allocate temporaries
  //
  Info := 2;
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;
  OOBErrors.RelClsError := 0;
  OOBErrors.AvgCE := 0;
  OOBErrors.RMSError := 0;
  OOBErrors.AvgError := 0;
  OOBErrors.AvgRelError := 0;
  NIn := Ensemble.NIn;
  NOut := Ensemble.NOut;
  if Ensemble.IsSoftmax then
    begin
      CCnt := NIn + 1;
      PCnt := NIn;
    end
  else
    begin
      CCnt := NIn + NOut;
      PCnt := NIn + NOut;
    end;
  SetLength(XYS, NPoints (* - 1 + 1 // optimized compiler *) , CCnt (* - 1 + 1 // optimized compiler *) );
  SetLength(S, NPoints (* - 1 + 1 // optimized compiler *) );
  SetLength(OOBBuf, NPoints (* - 1 + 1 // optimized compiler *) , NOut (* - 1 + 1 // optimized compiler *) );
  SetLength(OOBCntBuf, NPoints (* - 1 + 1 // optimized compiler *) );
  SetLength(X, NIn (* - 1 + 1 // optimized compiler *) );
  SetLength(Y, NOut (* - 1 + 1 // optimized compiler *) );
  if Ensemble.IsSoftmax then
    begin
      SetLength(DY, 0 + 1);
    end
  else
    begin
      SetLength(DY, NOut (* - 1 + 1 // optimized compiler *) );
    end;
  I := 0;
  while I <= NPoints - 1 do
    begin
      J := 0;
      while J <= NOut - 1 do
        begin
          OOBBuf[I, J] := 0;
          Inc(J);
        end;
      Inc(I);
    end;
  I := 0;
  while I <= NPoints - 1 do
    begin
      OOBCntBuf[I] := 0;
      Inc(I);
    end;
  MLPUnserialize(Ensemble.SerializedMLP, Network);

  //
  // main bagging cycle
  //
  K := 0;
  while K <= Ensemble.EnsembleSize - 1 do
    begin

      //
      // prepare dataset
      //
      I := 0;
      while I <= NPoints - 1 do
        begin
          S[I] := False;
          Inc(I);
        end;
      I := 0;
      while I <= NPoints - 1 do
        begin
          J := RandomInteger(NPoints);
          S[J] := True;
          APVMove(@XYS[I][0], 0, CCnt - 1, @XY[J][0], 0, CCnt - 1);
          Inc(I);
        end;

      //
      // train
      //
      if LMAlgorithm then
        begin
          if MultiThread then
              MLPTrainLM_MT(Network, XYS, NPoints, Decay, Restarts, Info, TmpRep)
          else
              MLPTrainLM(Network, XYS, NPoints, Decay, Restarts, Info, TmpRep);
        end
      else
        begin
          IsTerminated := False;

          if MultiThread then
              MLPTrainLBFGS_MT(Network, XYS, NPoints, Decay, Restarts, WStep, MaxIts, Info, TmpRep)
          else
              MLPTrainLBFGS(Network, XYS, NPoints, Decay, Restarts, WStep, MaxIts, Info, TmpRep, @IsTerminated, EBest);
        end;
      if Info < 0 then
        begin
          Exit;
        end;

      //
      // save results
      //
      Rep.NGrad := Rep.NGrad + TmpRep.NGrad;
      Rep.NHess := Rep.NHess + TmpRep.NHess;
      Rep.NCholesky := Rep.NCholesky + TmpRep.NCholesky;
      APVMove(@Ensemble.Weights[0], K * Ensemble.WCount, (K + 1) * Ensemble.WCount
        - 1, @Network.Weights[0], 0, Ensemble.WCount - 1);
      APVMove(@Ensemble.ColumnMeans[0], K * PCnt, (K + 1) * PCnt - 1,
        @Network.ColumnMeans[0], 0, PCnt - 1);
      APVMove(@Ensemble.ColumnSigmas[0], K * PCnt, (K + 1) * PCnt - 1,
        @Network.ColumnSigmas[0], 0, PCnt - 1);

      //
      // OOB estimates
      //
      I := 0;
      while I <= NPoints - 1 do
        begin
          if not S[I] then
            begin
              APVMove(@X[0], 0, NIn - 1, @XY[I][0], 0, NIn - 1);
              MLPProcess(Network, X, Y);
              APVAdd(@OOBBuf[I][0], 0, NOut - 1, @Y[0], 0, NOut - 1);
              OOBCntBuf[I] := OOBCntBuf[I] + 1;
            end;
          Inc(I);
        end;
      Inc(K);
    end;

  //
  // OOB estimates
  //
  if Ensemble.IsSoftmax then
    begin
      DSErrAllocate(NOut, DSBuf);
    end
  else
    begin
      DSErrAllocate(-NOut, DSBuf);
    end;
  I := 0;
  while I <= NPoints - 1 do
    begin
      if OOBCntBuf[I] <> 0 then
        begin
          V := AP_Float(1) / OOBCntBuf[I];
          APVMove(@Y[0], 0, NOut - 1, @OOBBuf[I][0], 0, NOut - 1, V);
          if Ensemble.IsSoftmax then
            begin
              DY[0] := XY[I, NIn];
            end
          else
            begin
              APVMove(@DY[0], 0, NOut - 1, @XY[I][0], NIn, NIn + NOut - 1, V);
            end;
          DSErrAccumulate(DSBuf, Y, DY);
        end;
      Inc(I);
    end;
  DSErrFinish(DSBuf);
  OOBErrors.RelClsError := DSBuf[0];
  OOBErrors.AvgCE := DSBuf[1];
  OOBErrors.RMSError := DSBuf[2];
  OOBErrors.AvgError := DSBuf[3];
  OOBErrors.AvgRelError := DSBuf[4];
end;
