{ ****************************************************************************** }
{ * Neural networks ensemble support, by QQ 600585@qq.com                      * }
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
  (* ************************************************************************
    Neural networks ensemble
    ************************************************************************ *)
  TMLPEnsemble = packed record
    StructInfo: TLIVec;
    EnsembleSize: TLInt;
    NIn: TLInt;
    NOut: TLInt;
    WCount: TLInt;
    IsSoftmax: Boolean;
    PostProcessing: Boolean;
    Weights: TLVec;
    ColumnMeans: TLVec;
    ColumnSigmas: TLVec;
    SerializedLen: TLInt;
    SerializedMLP: TLVec;
    TmpWeights: TLVec;
    TmpMeans: TLVec;
    TmpSigmas: TLVec;
    Neurons: TLVec;
    DFDNET: TLVec;
    Y: TLVec;
  end;

  PMLPEnsemble = ^TMLPEnsemble;

procedure MLPECreate0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreate1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreate2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPECreateB0(NIn, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPECreateR0(NIn, NOut: TLInt; A, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateR1(NIn, NHid, NOut: TLInt; A, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateR2(NIn, NHid1, NHid2, NOut: TLInt; A, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPECreateC0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateC1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPECreateC2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPECreateFromNetwork(const Network: TMultiLayerPerceptron; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPECopy(const Ensemble1: TMLPEnsemble; var Ensemble2: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPESerialize(var Ensemble: TMLPEnsemble; var RA: TLVec; var RLen: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPEUNSerialize(const RA: TLVec; var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPERandomize(var Ensemble: TMLPEnsemble); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPEProperties(const Ensemble: TMLPEnsemble; var NIn: TLInt; var NOut: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MLPEIsSoftmax(const Ensemble: TMLPEnsemble): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPEProcess(var Ensemble: TMLPEnsemble; const X: TLVec; var Y: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MLPERelClsError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPEAvgCE(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPERMSError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPEAvgError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MLPEAvgRelError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPEBaggingLM(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport; var OOBErrors: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure MLPEBaggingLBFGS(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  WStep: TLFloat; MAXITS: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var OOBErrors: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


const
  MLPNTotalOffset = 3;
  MLPEVNum        = 9;

procedure MLPEAllErrors(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MLPEBaggingInternal(const MultiThread: Boolean; var Ensemble: TMLPEnsemble;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  LMAlgorithm: Boolean; var Info: TLInt; var Rep: TMLPReport;
  var OOBErrors: TMLPCVReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


procedure DSErrAllocate(NClasses: TLInt; var Buf: TLVec);
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

procedure DSErrAccumulate(var Buf: TLVec; const Y: TLVec; const DesiredY: TLVec);
var
  NClasses: TLInt;
  NOut    : TLInt;
  Offs    : TLInt;
  MMAX    : TLInt;
  rmax    : TLInt;
  J       : TLInt;
  v       : TLFloat;
  EV      : TLFloat;
begin
  Offs := 5;
  NClasses := Round(Buf[Offs]);
  if NClasses > 0 then
    begin

      //
      // Classification
      //
      rmax := Round(DesiredY[0]);
      MMAX := 0;
      J := 1;
      while J <= NClasses - 1 do
        begin
          if AP_FP_Greater(Y[J], Y[MMAX]) then
            begin
              MMAX := J;
            end;
          Inc(J);
        end;
      if MMAX <> rmax then
        begin
          Buf[0] := Buf[0] + 1;
        end;
      if AP_FP_Greater(Y[rmax], 0) then
        begin
          Buf[1] := Buf[1] - ln(Y[rmax]);
        end
      else
        begin
          Buf[1] := Buf[1] + ln(MaxRealNumber);
        end;
      J := 0;
      while J <= NClasses - 1 do
        begin
          v := Y[J];
          if J = rmax then
            begin
              EV := 1;
            end
          else
            begin
              EV := 0;
            end;
          Buf[2] := Buf[2] + AP_Sqr(v - EV);
          Buf[3] := Buf[3] + AbsReal(v - EV);
          if AP_FP_NEq(EV, 0) then
            begin
              Buf[4] := Buf[4] + AbsReal((v - EV) / EV);
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
      rmax := 0;
      J := 1;
      while J <= NOut - 1 do
        begin
          if AP_FP_Greater(DesiredY[J], DesiredY[rmax]) then
            begin
              rmax := J;
            end;
          Inc(J);
        end;
      MMAX := 0;
      J := 1;
      while J <= NOut - 1 do
        begin
          if AP_FP_Greater(Y[J], Y[MMAX]) then
            begin
              MMAX := J;
            end;
          Inc(J);
        end;
      if MMAX <> rmax then
        begin
          Buf[0] := Buf[0] + 1;
        end;
      J := 0;
      while J <= NOut - 1 do
        begin
          v := Y[J];
          EV := DesiredY[J];
          Buf[2] := Buf[2] + AP_Sqr(v - EV);
          Buf[3] := Buf[3] + AbsReal(v - EV);
          if AP_FP_NEq(EV, 0) then
            begin
              Buf[4] := Buf[4] + AbsReal((v - EV) / EV);
              Buf[Offs + 2] := Buf[Offs + 2] + 1;
            end;
          Inc(J);
        end;
      Buf[Offs + 1] := Buf[Offs + 1] + 1;
    end;
end;

procedure DSErrFinish(var Buf: TLVec);
var
  NOut: TLInt;
  Offs: TLInt;
begin
  Offs := 5;
  NOut := AbsInt(Round(Buf[Offs]));
  if AP_FP_NEq(Buf[Offs + 1], 0) then
    begin
      Buf[0] := Buf[0] / Buf[Offs + 1];
      Buf[1] := Buf[1] / Buf[Offs + 1];
      Buf[2] := Sqrt(Buf[2] / (NOut * Buf[Offs + 1]));
      Buf[3] := Buf[3] / (NOut * Buf[Offs + 1]);
    end;
  if AP_FP_NEq(Buf[Offs + 2], 0) then
    begin
      Buf[4] := Buf[4] / Buf[Offs + 2];
    end;
end;

(* ************************************************************************
  Like MLPCreate0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreate0(NIn, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreate1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreate1(NIn, NHid, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreate2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreate2(NIn, NHid1, NHid2, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB0(NIn, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateB0(NIn, NOut, b, d, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateB1(NIn, NHid, NOut, b, d, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateB2(NIn, NHid1, NHid2, NOut, b, d, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR0(NIn, NOut: TLInt; A, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateR0(NIn, NOut, A, b, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR1(NIn, NHid, NOut: TLInt; A, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateR1(NIn, NHid, NOut, A, b, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR2(NIn, NHid1, NHid2, NOut: TLInt; A, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateR2(NIn, NHid1, NHid2, NOut, A, b, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateC0(NIn, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateC1(NIn, NHid, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateC2(NIn, NHid1, NHid2, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Creates ensemble from network. Only network geometry is copied.
  ************************************************************************ *)
procedure MLPECreateFromNetwork(const Network: TMultiLayerPerceptron; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  i     : TLInt;
  CCount: TLInt;
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
  i := 0;
  while i <= Network.StructInfo[0] - 1 do
    begin
      Ensemble.StructInfo[i] := Network.StructInfo[i];
      Inc(i);
    end;

  //
  // weights, means, sigmas
  //
  SetLength(Ensemble.Weights, EnsembleSize * Ensemble.WCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.ColumnMeans, EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  SetLength(Ensemble.ColumnSigmas, EnsembleSize * CCount (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= EnsembleSize * Ensemble.WCount - 1 do
    begin
      Ensemble.Weights[i] := RandomReal - 0.5;
      Inc(i);
    end;
  i := 0;
  while i <= EnsembleSize - 1 do
    begin
      APVMove(@Ensemble.ColumnMeans[0], i * CCount, (i + 1) * CCount - 1,
        @Network.ColumnMeans[0], 0, CCount - 1);
      APVMove(@Ensemble.ColumnSigmas[0], i * CCount, (i + 1) * CCount - 1,
        @Network.ColumnSigmas[0], 0, CCount - 1);
      Inc(i);
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
  i     : TLInt;
  SSize : TLInt;
  CCount: TLInt;
  NTotal: TLInt;
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
  i := 0;
  while i <= SSize - 1 do
    begin
      Ensemble2.StructInfo[i] := Ensemble1.StructInfo[i];
      Inc(i);
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
  RA      -   packed array of real numbers which stores ensemble,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure MLPESerialize(var Ensemble: TMLPEnsemble; var RA: TLVec;
  var RLen: TLInt);
var
  i     : TLInt;
  SSize : TLInt;
  NTotal: TLInt;
  CCount: TLInt;
  HSize : TLInt;
  Offs  : TLInt;
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
  i := Offs;
  while i <= Offs + SSize - 1 do
    begin
      RA[i] := Ensemble.StructInfo[i - Offs];
      Inc(i);
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
procedure MLPEUNSerialize(const RA: TLVec; var Ensemble: TMLPEnsemble);
var
  i     : TLInt;
  SSize : TLInt;
  NTotal: TLInt;
  CCount: TLInt;
  HSize : TLInt;
  Offs  : TLInt;
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
  i := Offs;
  while i <= Offs + SSize - 1 do
    begin
      Ensemble.StructInfo[i - Offs] := Round(RA[i]);
      Inc(i);
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
  i: TLInt;
begin
  i := 0;
  while i <= Ensemble.EnsembleSize * Ensemble.WCount - 1 do
    begin
      Ensemble.Weights[i] := RandomReal - 0.5;
      Inc(i);
    end;
end;

(* ************************************************************************
  Return ensemble properties (number of inputs and outputs).
  ************************************************************************ *)
procedure MLPEProperties(const Ensemble: TMLPEnsemble; var NIn: TLInt;
  var NOut: TLInt);
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
procedure MLPEProcess(var Ensemble: TMLPEnsemble; const X: TLVec;
  var Y: TLVec);
var
  i : TLInt;
  ES: TLInt;
  WC: TLInt;
  CC: TLInt;
  v : TLFloat;
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
  v := AP_Float(1) / ES;
  i := 0;
  while i <= Ensemble.NOut - 1 do
    begin
      Y[i] := 0;
      Inc(i);
    end;
  i := 0;
  while i <= ES - 1 do
    begin
      APVMove(@Ensemble.TmpWeights[0], 0, WC - 1, @Ensemble.Weights[0], i * WC,
        (i + 1) * WC - 1);
      APVMove(@Ensemble.TmpMeans[0], 0, CC - 1, @Ensemble.ColumnMeans[0], i * CC,
        (i + 1) * CC - 1);
      APVMove(@Ensemble.TmpSigmas[0], 0, CC - 1, @Ensemble.ColumnSigmas[0],
        i * CC, (i + 1) * CC - 1);
      MLPInternalProcessVector(Ensemble.StructInfo, Ensemble.TmpWeights,
        Ensemble.TmpMeans, Ensemble.TmpSigmas, Ensemble.Neurons, Ensemble.DFDNET,
        X, Ensemble.Y);
      APVAdd(@Y[0], 0, Ensemble.NOut - 1, @Ensemble.Y[0], 0,
        Ensemble.NOut - 1, v);
      Inc(i);
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
function MLPERelClsError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
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
function MLPEAvgCE(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
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
function MLPERMSError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
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
function MLPEAvgError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
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
function MLPEAvgRelError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
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
procedure MLPEBaggingLM(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport; var OOBErrors: TMLPCVReport);
begin
  MLPEBaggingInternal(MultiThread, Ensemble, xy, NPoints, Decay, Restarts, 0.0, 0, True,
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
procedure MLPEBaggingLBFGS(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  WStep: TLFloat; MAXITS: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var OOBErrors: TMLPCVReport);
begin
  MLPEBaggingInternal(MultiThread, Ensemble, xy, NPoints, Decay, Restarts, WStep, MAXITS,
    False, Info, Rep, OOBErrors);
end;

(* ************************************************************************
  Calculation of all types of errors
  ************************************************************************ *)
procedure MLPEAllErrors(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat);
var
  i    : TLInt;
  Buf  : TLVec;
  WorkX: TLVec;
  Y    : TLVec;
  dy   : TLVec;
begin
  SetLength(WorkX, Ensemble.NIn (* - 1 + 1 // optimized compiler *) );
  SetLength(Y, Ensemble.NOut (* - 1 + 1 // optimized compiler *) );
  if Ensemble.IsSoftmax then
    begin
      SetLength(dy, 0 + 1);
      DSErrAllocate(Ensemble.NOut, Buf);
    end
  else
    begin
      SetLength(dy, Ensemble.NOut (* - 1 + 1 // optimized compiler *) );
      DSErrAllocate(-Ensemble.NOut, Buf);
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@WorkX[0], 0, Ensemble.NIn - 1, @xy[i][0], 0, Ensemble.NIn - 1);
      MLPEProcess(Ensemble, WorkX, Y);
      if Ensemble.IsSoftmax then
        begin
          dy[0] := xy[i, Ensemble.NIn];
        end
      else
        begin
          APVMove(@dy[0], 0, Ensemble.NOut - 1, @xy[i][0], Ensemble.NIn,
            Ensemble.NIn + Ensemble.NOut - 1);
        end;
      DSErrAccumulate(Buf, Y, dy);
      Inc(i);
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
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  LMAlgorithm: Boolean; var Info: TLInt; var Rep: TMLPReport;
  var OOBErrors: TMLPCVReport);
var
  XYS         : TLMatrix;
  s           : TLBVec;
  OOBBuf      : TLMatrix;
  OOBCntBuf   : TLIVec;
  X           : TLVec;
  Y           : TLVec;
  dy          : TLVec;
  DSBuf       : TLVec;
  NIn         : TLInt;
  NOut        : TLInt;
  CCnt        : TLInt;
  PCnt        : TLInt;
  i           : TLInt;
  J           : TLInt;
  k           : TLInt;
  v           : TLFloat;
  TmpRep      : TMLPReport;
  Network     : TMultiLayerPerceptron;
  IsTerminated: Boolean;
  EBest       : TLFloat;
begin

  //
  // Test for inputs
  //
  if not LMAlgorithm and AP_FP_Eq(WStep, 0) and (MAXITS = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MAXITS < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  if Ensemble.IsSoftmax then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, Ensemble.NIn]) < 0) or
            (Round(xy[i, Ensemble.NIn]) >= Ensemble.NOut) then
            begin
              Info := -2;
              Exit;
            end;
          Inc(i);
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
  SetLength(s, NPoints (* - 1 + 1 // optimized compiler *) );
  SetLength(OOBBuf, NPoints (* - 1 + 1 // optimized compiler *) , NOut (* - 1 + 1 // optimized compiler *) );
  SetLength(OOBCntBuf, NPoints (* - 1 + 1 // optimized compiler *) );
  SetLength(X, NIn (* - 1 + 1 // optimized compiler *) );
  SetLength(Y, NOut (* - 1 + 1 // optimized compiler *) );
  if Ensemble.IsSoftmax then
    begin
      SetLength(dy, 0 + 1);
    end
  else
    begin
      SetLength(dy, NOut (* - 1 + 1 // optimized compiler *) );
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      J := 0;
      while J <= NOut - 1 do
        begin
          OOBBuf[i, J] := 0;
          Inc(J);
        end;
      Inc(i);
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      OOBCntBuf[i] := 0;
      Inc(i);
    end;
  MLPUNSerialize(Ensemble.SerializedMLP, Network);

  //
  // main bagging cycle
  //
  k := 0;
  while k <= Ensemble.EnsembleSize - 1 do
    begin

      //
      // prepare dataset
      //
      i := 0;
      while i <= NPoints - 1 do
        begin
          s[i] := False;
          Inc(i);
        end;
      i := 0;
      while i <= NPoints - 1 do
        begin
          J := RandomInteger(NPoints);
          s[J] := True;
          APVMove(@XYS[i][0], 0, CCnt - 1, @xy[J][0], 0, CCnt - 1);
          Inc(i);
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
              MLPTrainLBFGS_MT(Network, XYS, NPoints, Decay, Restarts, WStep, MAXITS, Info, TmpRep)
          else
              MLPTrainLBFGS(Network, XYS, NPoints, Decay, Restarts, WStep, MAXITS, Info, TmpRep, @IsTerminated, EBest);
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
      APVMove(@Ensemble.Weights[0], k * Ensemble.WCount, (k + 1) * Ensemble.WCount
        - 1, @Network.Weights[0], 0, Ensemble.WCount - 1);
      APVMove(@Ensemble.ColumnMeans[0], k * PCnt, (k + 1) * PCnt - 1,
        @Network.ColumnMeans[0], 0, PCnt - 1);
      APVMove(@Ensemble.ColumnSigmas[0], k * PCnt, (k + 1) * PCnt - 1,
        @Network.ColumnSigmas[0], 0, PCnt - 1);

      //
      // OOB estimates
      //
      i := 0;
      while i <= NPoints - 1 do
        begin
          if not s[i] then
            begin
              APVMove(@X[0], 0, NIn - 1, @xy[i][0], 0, NIn - 1);
              MLPProcess(Network, X, Y);
              APVAdd(@OOBBuf[i][0], 0, NOut - 1, @Y[0], 0, NOut - 1);
              OOBCntBuf[i] := OOBCntBuf[i] + 1;
            end;
          Inc(i);
        end;
      Inc(k);
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
  i := 0;
  while i <= NPoints - 1 do
    begin
      if OOBCntBuf[i] <> 0 then
        begin
          v := AP_Float(1) / OOBCntBuf[i];
          APVMove(@Y[0], 0, NOut - 1, @OOBBuf[i][0], 0, NOut - 1, v);
          if Ensemble.IsSoftmax then
            begin
              dy[0] := xy[i, NIn];
            end
          else
            begin
              APVMove(@dy[0], 0, NOut - 1, @xy[i][0], NIn, NIn + NOut - 1, v);
            end;
          DSErrAccumulate(DSBuf, Y, dy);
        end;
      Inc(i);
    end;
  DSErrFinish(DSBuf);
  OOBErrors.RelClsError := DSBuf[0];
  OOBErrors.AvgCE := DSBuf[1];
  OOBErrors.RMSError := DSBuf[2];
  OOBErrors.AvgError := DSBuf[3];
  OOBErrors.AvgRelError := DSBuf[4];
end; 
