{ ****************************************************************************** }
{ * machine Learn support, writen by QQ 600585@qq.com                          * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
type
  TMultiLayerPerceptron = packed record
    StructInfo: TLearnIntegerArray;
    Weights: TLearnFloatArray;
    ColumnMeans: TLearnFloatArray;
    ColumnSigmas: TLearnFloatArray;
    Neurons: TLearnFloatArray;
    DFDNET: TLearnFloatArray;
    DError: TLearnFloatArray;
    X: TLearnFloatArray;
    Y: TLearnFloatArray;
    Chunks: TLearnFloat2DArray;
    NWBuf: TLearnFloatArray;
  end;

  PMultiLayerPerceptron = ^TMultiLayerPerceptron;

procedure MLPCreate0(NIn: TLearnInteger; NOut: TLearnInteger; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreate1(NIn: TLearnInteger; NHid: TLearnInteger; NOut: TLearnInteger; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreate2(NIn: TLearnInteger; NHid1: TLearnInteger; NHid2: TLearnInteger; NOut: TLearnInteger; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateB0(NIn: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateB1(NIn: TLearnInteger; NHid: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateB2(NIn: TLearnInteger; NHid1: TLearnInteger; NHid2: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateR0(NIn: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateR1(NIn: TLearnInteger; NHid: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateR2(NIn: TLearnInteger; NHid1: TLearnInteger; NHid2: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateC0(NIn: TLearnInteger; NOut: TLearnInteger; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateC1(NIn: TLearnInteger; NHid: TLearnInteger; NOut: TLearnInteger; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateC2(NIn: TLearnInteger; NHid1: TLearnInteger; NHid2: TLearnInteger; NOut: TLearnInteger; var Network: TMultiLayerPerceptron); forward;
procedure MLPFree(var Network: TMultiLayerPerceptron); forward;
procedure MLPCopy(const Network1: TMultiLayerPerceptron; var Network2: TMultiLayerPerceptron); forward;
procedure MLPSerialize(const Network: TMultiLayerPerceptron; var RA: TLearnFloatArray; var RLen: TLearnInteger); forward;
procedure MLPUnserialize(const RA: TLearnFloatArray; var Network: TMultiLayerPerceptron); forward;
procedure MLPRandomize(var Network: TMultiLayerPerceptron); overload; forward;
procedure MLPRandomize(var Network: TMultiLayerPerceptron; const Diameter: TLearnFloat); overload; forward;
procedure MLPRandomize(var Network: TMultiLayerPerceptron; const WBest: TLearnFloatArray; const Diameter: TLearnFloat); overload; forward;
procedure MLPRandomizeFull(var Network: TMultiLayerPerceptron); forward;
procedure MLPInitPreprocessor(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger); forward;
procedure MLPProperties(const Network: TMultiLayerPerceptron; var NIn: TLearnInteger; var NOut: TLearnInteger; var WCount: TLearnInteger); forward;
function MLPIsSoftmax(const Network: TMultiLayerPerceptron): Boolean; forward;
procedure MLPProcess(var Network: TMultiLayerPerceptron; const X: TLearnFloatArray; var Y: TLearnFloatArray); forward;
function MLPError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger): TLearnFloat; forward;
function MLPErrorN(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger): TLearnFloat; forward;
function MLPClsError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger): TLearnInteger; forward;
function MLPRelClsError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward;
function MLPAvgCE(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward;
function MLPRMSError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward;
function MLPAvgError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward;
function MLPAvgRelError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat; forward;
procedure MLPGrad(var Network: TMultiLayerPerceptron; const X: TLearnFloatArray; const DesiredY: TLearnFloatArray; var E: TLearnFloat; var Grad: TLearnFloatArray); forward;
procedure MLPGradN(var Network: TMultiLayerPerceptron; const X: TLearnFloatArray; const DesiredY: TLearnFloatArray; var E: TLearnFloat; var Grad: TLearnFloatArray); forward;
procedure MLPGradBatch(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat; var Grad: TLearnFloatArray); forward;
procedure MLPGradNBatch(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat; var Grad: TLearnFloatArray); forward;

procedure MLPHessianNBatch(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat; var Grad: TLearnFloatArray; var H: TLearnFloat2DArray); forward;
procedure MLPHessianBatch(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat; var Grad: TLearnFloatArray; var H: TLearnFloat2DArray); forward;

procedure MLPInternalProcessVector(const StructInfo: TLearnIntegerArray;
  const Weights: TLearnFloatArray; const ColumnMeans: TLearnFloatArray;
  const ColumnSigmas: TLearnFloatArray; var Neurons: TLearnFloatArray;
  var DFDNET: TLearnFloatArray; const X: TLearnFloatArray; var Y: TLearnFloatArray); forward;

const
  MLPVNum     = 7;
  NFieldWidth = 4;
  ChunkSize   = 32;

procedure AddInputLayer(NCount: TLearnInteger; var LSizes: TLearnIntegerArray;
  var LTypes: TLearnIntegerArray; var LConnFirst: TLearnIntegerArray;
  var LConnLast: TLearnIntegerArray; var LastProc: TLearnInteger); forward;
procedure AddBiasedSummatorLayer(NCount: TLearnInteger;
  var LSizes: TLearnIntegerArray; var LTypes: TLearnIntegerArray;
  var LConnFirst: TLearnIntegerArray; var LConnLast: TLearnIntegerArray;
  var LastProc: TLearnInteger); forward;
procedure AddActivationLayer(FuncType: TLearnInteger;
  var LSizes: TLearnIntegerArray; var LTypes: TLearnIntegerArray;
  var LConnFirst: TLearnIntegerArray; var LConnLast: TLearnIntegerArray;
  var LastProc: TLearnInteger); forward;
procedure AddZeroLayer(var LSizes: TLearnIntegerArray; var LTypes: TLearnIntegerArray;
  var LConnFirst: TLearnIntegerArray; var LConnLast: TLearnIntegerArray;
  var LastProc: TLearnInteger); forward;
procedure MLPCreate(NIn: TLearnInteger; NOut: TLearnInteger;
  const LSizes: TLearnIntegerArray; const LTypes: TLearnIntegerArray;
  const LConnFirst: TLearnIntegerArray; const LConnLast: TLearnIntegerArray;
  LayersCount: TLearnInteger; IsClsNet: Boolean;
  var Network: TMultiLayerPerceptron); forward;
procedure MLPActivationFunction(NET: TLearnFloat; K: TLearnInteger;
  var F: TLearnFloat; var DF: TLearnFloat; var D2F: TLearnFloat); forward;
procedure MLPHessianBatchInternal(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; SSize: TLearnInteger; NaturalErr: Boolean;
  var E: TLearnFloat; var Grad: TLearnFloatArray; var H: TLearnFloat2DArray); forward;
procedure MLPInternalCalculateGradient(var Network: TMultiLayerPerceptron;
  const Neurons: TLearnFloatArray; const Weights: TLearnFloatArray;
  var DError: TLearnFloatArray; var Grad: TLearnFloatArray;
  NaturalErrorFunc: Boolean); forward;
procedure MLPChunkedGradient(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; CStart: TLearnInteger; CSize: TLearnInteger;
  var E: TLearnFloat; var Grad: TLearnFloatArray;
  NaturalErrorFunc: Boolean); forward;
function SafeCrossEntropy(T: TLearnFloat; Z: TLearnFloat): TLearnFloat; forward;

(* ************************************************************************
  Creates  neural  network  with  NIn  inputs,  NOut outputs, without hidden
  layers, with linear output layer. Network weights are  filled  with  small
  random values.
  ************************************************************************ *)
procedure MLPCreate0(NIn: TLearnInteger; NOut: TLearnInteger;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
begin
  LayersCount := 1 + 2;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);
end;

(* ************************************************************************
  Same  as  MLPCreate0,  but  with  one  hidden  layer  (NHid  neurons) with
  non-linear activation function. Output layer is linear.
  ************************************************************************ *)
procedure MLPCreate1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
begin
  LayersCount := 1 + 3 + 2;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);
end;

(* ************************************************************************
  Same as MLPCreate0, but with two hidden layers (NHid1 and  NHid2  neurons)
  with non-linear activation function. Output layer is linear.
  ************************************************************************ *)
procedure MLPCreate2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
begin
  LayersCount := 1 + 3 + 3 + 2;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);
end;

(* ************************************************************************
  Creates  neural  network  with  NIn  inputs,  NOut outputs, without hidden
  layers with non-linear output layer. Network weights are filled with small
  random values.

  Activation function of the output layer takes values:

  (B, +INF), if D>=0

  or

  (-INF, B), if D<0.
  ************************************************************************ *)
procedure MLPCreateB0(NIn: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat;
  D: TLearnFloat; var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
  I          : TLearnInteger;
begin
  LayersCount := 1 + 3;
  if AP_FP_Greater_Eq(D, 0) then
    begin
      D := 1;
    end
  else
    begin
      D := -1;
    end;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(3, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);

  //
  // Turn on ouputs shift/scaling.
  //
  I := NIn;
  while I <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[I] := B;
      Network.ColumnSigmas[I] := D;
      Inc(I);
    end;
end;

(* ************************************************************************
  Same as MLPCreateB0 but with non-linear hidden layer.
  ************************************************************************ *)
procedure MLPCreateB1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
  I          : TLearnInteger;
begin
  LayersCount := 1 + 3 + 3;
  if AP_FP_Greater_Eq(D, 0) then
    begin
      D := 1;
    end
  else
    begin
      D := -1;
    end;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(3, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);

  //
  // Turn on ouputs shift/scaling.
  //
  I := NIn;
  while I <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[I] := B;
      Network.ColumnSigmas[I] := D;
      Inc(I);
    end;
end;

(* ************************************************************************
  Same as MLPCreateB0 but with two non-linear hidden layers.
  ************************************************************************ *)
procedure MLPCreateB2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; B: TLearnFloat; D: TLearnFloat;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
  I          : TLearnInteger;
begin
  LayersCount := 1 + 3 + 3 + 3;
  if AP_FP_Greater_Eq(D, 0) then
    begin
      D := 1;
    end
  else
    begin
      D := -1;
    end;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(3, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);

  //
  // Turn on ouputs shift/scaling.
  //
  I := NIn;
  while I <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[I] := B;
      Network.ColumnSigmas[I] := D;
      Inc(I);
    end;
end;

(* ************************************************************************
  Creates  neural  network  with  NIn  inputs,  NOut outputs, without hidden
  layers with non-linear output layer. Network weights are filled with small
  random values. Activation function of the output layer takes values [A,B].
  ************************************************************************ *)
procedure MLPCreateR0(NIn: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat;
  B: TLearnFloat; var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
  I          : TLearnInteger;
begin
  LayersCount := 1 + 3;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);

  //
  // Turn on outputs shift/scaling.
  //
  I := NIn;
  while I <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[I] := 0.5 * (A + B);
      Network.ColumnSigmas[I] := 0.5 * (A - B);
      Inc(I);
    end;
end;

(* ************************************************************************
  Same as MLPCreateR0, but with non-linear hidden layer.
  ************************************************************************ *)
procedure MLPCreateR1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
  I          : TLearnInteger;
begin
  LayersCount := 1 + 3 + 3;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);

  //
  // Turn on outputs shift/scaling.
  //
  I := NIn;
  while I <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[I] := 0.5 * (A + B);
      Network.ColumnSigmas[I] := 0.5 * (A - B);
      Inc(I);
    end;
end;

(* ************************************************************************
  Same as MLPCreateR0, but with two non-linear hidden layers.
  ************************************************************************ *)
procedure MLPCreateR2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
  I          : TLearnInteger;
begin
  LayersCount := 1 + 3 + 3 + 3;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);

  //
  // Turn on outputs shift/scaling.
  //
  I := NIn;
  while I <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[I] := 0.5 * (A + B);
      Network.ColumnSigmas[I] := 0.5 * (A - B);
      Inc(I);
    end;
end;

(* ************************************************************************
  Creates classifier network with NIn inputs and NOut possible classes.
  Network contains no hidden layers and linear output layer with SOFTMAX-
  normalization (so outputs sums up to 1.0 and converge to posterior probabilities).
  ************************************************************************ *)
procedure MLPCreateC0(NIn: TLearnInteger; NOut: TLearnInteger;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
begin
  Assert(NOut >= 2, 'MLPCreateC0: NOut<2!');
  LayersCount := 1 + 2 + 1;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut - 1, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddZeroLayer(LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    True, Network);
end;

(* ************************************************************************
  Same as MLPCreateC0, but with one non-linear hidden layer.
  ************************************************************************ *)
procedure MLPCreateC1(NIn: TLearnInteger; NHid: TLearnInteger;
  NOut: TLearnInteger; var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
begin
  Assert(NOut >= 2, 'MLPCreateC1: NOut<2!');
  LayersCount := 1 + 3 + 2 + 1;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut - 1, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddZeroLayer(LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    True, Network);
end;

(* ************************************************************************
  Same as MLPCreateC0, but with two non-linear hidden layers.
  ************************************************************************ *)
procedure MLPCreateC2(NIn: TLearnInteger; NHid1: TLearnInteger;
  NHid2: TLearnInteger; NOut: TLearnInteger;
  var Network: TMultiLayerPerceptron);
var
  LSizes     : TLearnIntegerArray;
  LTypes     : TLearnIntegerArray;
  LConnFirst : TLearnIntegerArray;
  LConnLast  : TLearnIntegerArray;
  LayersCount: TLearnInteger;
  LastProc   : TLearnInteger;
begin
  Assert(NOut >= 2, 'MLPCreateC2: NOut<2!');
  LayersCount := 1 + 3 + 3 + 2 + 1;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LTypes, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LConnLast, LayersCount (* - 1 + 1 // optimized compler *) );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut - 1, LSizes, LTypes, LConnFirst, LConnLast,
    LastProc);
  AddZeroLayer(LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    True, Network);
end;

procedure MLPFree(var Network: TMultiLayerPerceptron);
begin
  SetLength(Network.StructInfo, 0);
  SetLength(Network.Weights, 0);
  SetLength(Network.ColumnMeans, 0);
  SetLength(Network.ColumnSigmas, 0);
  SetLength(Network.Neurons, 0);
  SetLength(Network.DFDNET, 0);
  SetLength(Network.DError, 0);
  SetLength(Network.X, 0);
  SetLength(Network.Y, 0);
  SetLength(Network.Chunks, 0);
  SetLength(Network.NWBuf, 0);
end;

(* ************************************************************************
  Copying of neural network

  INPUT PARAMETERS:
  Network1 -   original

  OUTPUT PARAMETERS:
  Network2 -   copy
  ************************************************************************ *)
procedure MLPCopy(const Network1: TMultiLayerPerceptron;
  var Network2: TMultiLayerPerceptron);
var
  I     : TLearnInteger;
  SSize : TLearnInteger;
  NTotal: TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin

  //
  // Unload info
  //
  SSize := Network1.StructInfo[0];
  NIn := Network1.StructInfo[1];
  NOut := Network1.StructInfo[2];
  NTotal := Network1.StructInfo[3];
  WCount := Network1.StructInfo[4];

  //
  // Allocate space
  //
  SetLength(Network2.StructInfo, SSize (* - 1 + 1 // optimized compler *) );
  SetLength(Network2.Weights, WCount (* - 1 + 1 // optimized compler *) );
  if MLPIsSoftmax(Network1) then
    begin
      SetLength(Network2.ColumnMeans, NIn (* - 1 + 1 // optimized compler *) );
      SetLength(Network2.ColumnSigmas, NIn (* - 1 + 1 // optimized compler *) );
    end
  else
    begin
      SetLength(Network2.ColumnMeans, NIn + NOut (* - 1 + 1 // optimized compler *) );
      SetLength(Network2.ColumnSigmas, NIn + NOut (* - 1 + 1 // optimized compler *) );
    end;
  SetLength(Network2.Neurons, NTotal (* - 1 + 1 // optimized compler *) );
  SetLength(Network2.Chunks, 3 * NTotal + 1, ChunkSize (* - 1 + 1 // optimized compler *) );
  SetLength(Network2.NWBuf, Max(WCount, 2 * NOut) (* - 1 + 1 // optimized compler *) );
  SetLength(Network2.DFDNET, NTotal (* - 1 + 1 // optimized compler *) );
  SetLength(Network2.X, NIn (* - 1 + 1 // optimized compler *) );
  SetLength(Network2.Y, NOut (* - 1 + 1 // optimized compler *) );
  SetLength(Network2.DError, NTotal (* - 1 + 1 // optimized compler *) );

  //
  // Copy
  //
  I := 0;
  while I <= SSize - 1 do
    begin
      Network2.StructInfo[I] := Network1.StructInfo[I];
      Inc(I);
    end;
  APVMove(@Network2.Weights[0], 0, WCount - 1, @Network1.Weights[0], 0,
    WCount - 1);
  if MLPIsSoftmax(Network1) then
    begin
      APVMove(@Network2.ColumnMeans[0], 0, NIn - 1, @Network1.ColumnMeans[0],
        0, NIn - 1);
      APVMove(@Network2.ColumnSigmas[0], 0, NIn - 1, @Network1.ColumnSigmas[0],
        0, NIn - 1);
    end
  else
    begin
      APVMove(@Network2.ColumnMeans[0], 0, NIn + NOut - 1,
        @Network1.ColumnMeans[0], 0, NIn + NOut - 1);
      APVMove(@Network2.ColumnSigmas[0], 0, NIn + NOut - 1,
        @Network1.ColumnSigmas[0], 0, NIn + NOut - 1);
    end;
  APVMove(@Network2.Neurons[0], 0, NTotal - 1, @Network1.Neurons[0], 0,
    NTotal - 1);
  APVMove(@Network2.DFDNET[0], 0, NTotal - 1, @Network1.DFDNET[0], 0,
    NTotal - 1);
  APVMove(@Network2.X[0], 0, NIn - 1, @Network1.X[0], 0, NIn - 1);
  APVMove(@Network2.Y[0], 0, NOut - 1, @Network1.Y[0], 0, NOut - 1);
  APVMove(@Network2.DError[0], 0, NTotal - 1, @Network1.DError[0], 0,
    NTotal - 1);
end;

(* ************************************************************************
  Serialization of TMultiLayerPerceptron strucure

  INPUT PARAMETERS:
  Network -   original

  OUTPUT PARAMETERS:
  RA      -   array of real numbers which stores network,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure MLPSerialize(const Network: TMultiLayerPerceptron;
  var RA: TLearnFloatArray; var RLen: TLearnInteger);
var
  I       : TLearnInteger;
  SSize   : TLearnInteger;
  NTotal  : TLearnInteger;
  NIn     : TLearnInteger;
  NOut    : TLearnInteger;
  WCount  : TLearnInteger;
  SigmaLen: TLearnInteger;
  Offs    : TLearnInteger;
begin

  //
  // Unload info
  //
  SSize := Network.StructInfo[0];
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  WCount := Network.StructInfo[4];
  if MLPIsSoftmax(Network) then
    begin
      SigmaLen := NIn;
    end
  else
    begin
      SigmaLen := NIn + NOut;
    end;

  //
  // RA format:
  // LEN         DESRC.
  // 1           RLen
  // 1           version (MLPVNum)
  // 1           StructInfo size
  // SSize       StructInfo
  // WCount      Weights
  // SigmaLen    ColumnMeans
  // SigmaLen    ColumnSigmas
  //
  RLen := 3 + SSize + WCount + 2 * SigmaLen;
  SetLength(RA, RLen (* - 1 + 1 // optimized compler *) );
  RA[0] := RLen;
  RA[1] := MLPVNum;
  RA[2] := SSize;
  Offs := 3;
  I := 0;
  while I <= SSize - 1 do
    begin
      RA[Offs + I] := Network.StructInfo[I];
      Inc(I);
    end;
  Offs := Offs + SSize;
  APVMove(@RA[0], Offs, Offs + WCount - 1, @Network.Weights[0], 0, WCount - 1);
  Offs := Offs + WCount;
  APVMove(@RA[0], Offs, Offs + SigmaLen - 1, @Network.ColumnMeans[0], 0,
    SigmaLen - 1);
  Offs := Offs + SigmaLen;
  APVMove(@RA[0], Offs, Offs + SigmaLen - 1, @Network.ColumnSigmas[0], 0,
    SigmaLen - 1);
  Offs := Offs + SigmaLen;
end;

(* ************************************************************************
  Unserialization of TMultiLayerPerceptron strucure

  INPUT PARAMETERS:
  RA      -   real array which stores network

  OUTPUT PARAMETERS:
  Network -   restored network
  ************************************************************************ *)
procedure MLPUnserialize(const RA: TLearnFloatArray;
  var Network: TMultiLayerPerceptron);
var
  I       : TLearnInteger;
  SSize   : TLearnInteger;
  NTotal  : TLearnInteger;
  NIn     : TLearnInteger;
  NOut    : TLearnInteger;
  WCount  : TLearnInteger;
  SigmaLen: TLearnInteger;
  Offs    : TLearnInteger;
begin
  Assert(Round(RA[1]) = MLPVNum, 'MLPUnserialize: incorrect array!');

  //
  // Unload StructInfo from IA
  //
  Offs := 3;
  SSize := Round(RA[2]);
  SetLength(Network.StructInfo, SSize (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= SSize - 1 do
    begin
      Network.StructInfo[I] := Round(RA[Offs + I]);
      Inc(I);
    end;
  Offs := Offs + SSize;

  //
  // Unload info from StructInfo
  //
  SSize := Network.StructInfo[0];
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  WCount := Network.StructInfo[4];
  if Network.StructInfo[6] = 0 then
    begin
      SigmaLen := NIn + NOut;
    end
  else
    begin
      SigmaLen := NIn;
    end;

  //
  // Allocate space for other fields
  //
  SetLength(Network.Weights, WCount (* - 1 + 1 // optimized compler *) );
  SetLength(Network.ColumnMeans, SigmaLen (* - 1 + 1 // optimized compler *) );
  SetLength(Network.ColumnSigmas, SigmaLen (* - 1 + 1 // optimized compler *) );
  SetLength(Network.Neurons, NTotal (* - 1 + 1 // optimized compler *) );
  SetLength(Network.Chunks, 3 * NTotal + 1, ChunkSize (* - 1 + 1 // optimized compler *) );
  SetLength(Network.NWBuf, Max(WCount, 2 * NOut) (* - 1 + 1 // optimized compler *) );
  SetLength(Network.DFDNET, NTotal (* - 1 + 1 // optimized compler *) );
  SetLength(Network.X, NIn (* - 1 + 1 // optimized compler *) );
  SetLength(Network.Y, NOut (* - 1 + 1 // optimized compler *) );
  SetLength(Network.DError, NTotal (* - 1 + 1 // optimized compler *) );

  //
  // Copy parameters from RA
  //
  APVMove(@Network.Weights[0], 0, WCount - 1, @RA[0], Offs, Offs + WCount - 1);
  Offs := Offs + WCount;
  APVMove(@Network.ColumnMeans[0], 0, SigmaLen - 1, @RA[0], Offs,
    Offs + SigmaLen - 1);
  Offs := Offs + SigmaLen;
  APVMove(@Network.ColumnSigmas[0], 0, SigmaLen - 1, @RA[0], Offs,
    Offs + SigmaLen - 1);
  Offs := Offs + SigmaLen;
end;

(* ************************************************************************
  Randomization of neural network weights
  ************************************************************************ *)
procedure MLPRandomize(var Network: TMultiLayerPerceptron); overload;
var
  I     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  for I := 0 to WCount - 1 do
    begin
      Network.Weights[I] := RandomReal - 0.5;
    end;
end;

(* ************************************************************************
  Randomization of neural network weights
  ************************************************************************ *)
procedure MLPRandomize(var Network: TMultiLayerPerceptron;
  const Diameter: TLearnFloat); overload;
var
  I     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  for I := 0 to WCount - 1 do
    begin
      Network.Weights[I] := (RandomReal - 0.5) * Diameter;
    end;
end;

(* ************************************************************************
  Randomization of neural network weights
  ************************************************************************ *)
procedure MLPRandomize(var Network: TMultiLayerPerceptron;
  const WBest: TLearnFloatArray; const Diameter: TLearnFloat); overload;
var
  I     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  for I := 0 to WCount - 1 do
    begin
      Network.Weights[I] := WBest[I] + (RandomReal - 0.5) * Diameter;
    end;
end;

(* ************************************************************************
  Randomization of neural network weights and standartisator
  ************************************************************************ *)
procedure MLPRandomizeFull(var Network: TMultiLayerPerceptron);
var
  I     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
  NTotal: TLearnInteger;
  IStart: TLearnInteger;
  Offs  : TLearnInteger;
  NType : TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Process network
  //
  I := 0;
  while I <= WCount - 1 do
    begin
      Network.Weights[I] := RandomReal - 0.5;
      Inc(I);
    end;
  I := 0;
  while I <= NIn - 1 do
    begin
      Network.ColumnMeans[I] := 2 * RandomReal - 1;
      Network.ColumnSigmas[I] := 1.5 * RandomReal + 0.5;
      Inc(I);
    end;
  if not MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NOut - 1 do
        begin
          Offs := IStart + (NTotal - NOut + I) * NFieldWidth;
          NType := Network.StructInfo[Offs + 0];
          if NType = 0 then
            begin

              //
              // Shifts are changed only for linear outputs neurons
              //
              Network.ColumnMeans[NIn + I] := 2 * RandomReal - 1;
            end;
          if (NType = 0) or (NType = 3) then
            begin

              //
              // Scales are changed only for linear or bounded outputs neurons.
              // Note that scale randomization preserves sign.
              //
              Network.ColumnSigmas[NIn + I] := Sign(Network.ColumnSigmas[NIn + I]) *
                (1.5 * RandomReal + 0.5);
            end;
          Inc(I);
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine.
  ************************************************************************ *)
procedure MLPInitPreprocessor(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; SSize: TLearnInteger);
var
  I     : TLearnInteger;
  J     : TLearnInteger;
  JMax  : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
  NTotal: TLearnInteger;
  IStart: TLearnInteger;
  Offs  : TLearnInteger;
  NType : TLearnInteger;
  Means : TLearnFloatArray;
  Sigmas: TLearnFloatArray;
  S     : TLearnFloat;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Means/Sigmas
  //
  if MLPIsSoftmax(Network) then
    begin
      JMax := NIn - 1;
    end
  else
    begin
      JMax := NIn + NOut - 1;
    end;
  SetLength(Means, JMax + 1);
  SetLength(Sigmas, JMax + 1);
  J := 0;
  while J <= JMax do
    begin
      Means[J] := 0;
      I := 0;
      while I <= SSize - 1 do
        begin
          Means[J] := Means[J] + XY[I, J];
          Inc(I);
        end;
      Means[J] := Means[J] / SSize;
      Sigmas[J] := 0;
      I := 0;
      while I <= SSize - 1 do
        begin
          Sigmas[J] := Sigmas[J] + AP_Sqr(XY[I, J] - Means[J]);
          Inc(I);
        end;
      Sigmas[J] := Sqrt(Sigmas[J] / SSize);
      Inc(J);
    end;

  //
  // Inputs
  //
  I := 0;
  while I <= NIn - 1 do
    begin
      Network.ColumnMeans[I] := Means[I];
      Network.ColumnSigmas[I] := Sigmas[I];
      if AP_FP_Eq(Network.ColumnSigmas[I], 0) then
        begin
          Network.ColumnSigmas[I] := 1;
        end;
      Inc(I);
    end;

  //
  // Outputs
  //
  if not MLPIsSoftmax(Network) then
    begin
      I := 0;
      while I <= NOut - 1 do
        begin
          Offs := IStart + (NTotal - NOut + I) * NFieldWidth;
          NType := Network.StructInfo[Offs + 0];

          //
          // Linear outputs
          //
          if NType = 0 then
            begin
              Network.ColumnMeans[NIn + I] := Means[NIn + I];
              Network.ColumnSigmas[NIn + I] := Sigmas[NIn + I];
              if AP_FP_Eq(Network.ColumnSigmas[NIn + I], 0) then
                begin
                  Network.ColumnSigmas[NIn + I] := 1;
                end;
            end;

          //
          // Bounded outputs (half-interval)
          //
          if NType = 3 then
            begin
              S := Means[NIn + I] - Network.ColumnMeans[NIn + I];
              if AP_FP_Eq(S, 0) then
                begin
                  S := Sign(Network.ColumnSigmas[NIn + I]);
                end;
              if AP_FP_Eq(S, 0) then
                begin
                  S := 1.0;
                end;
              Network.ColumnSigmas[NIn + I] := Sign(Network.ColumnSigmas[NIn + I]) *
                AbsReal(S);
              if AP_FP_Eq(Network.ColumnSigmas[NIn + I], 0) then
                begin
                  Network.ColumnSigmas[NIn + I] := 1;
                end;
            end;
          Inc(I);
        end;
    end;
end;

(* ************************************************************************
  Returns information about initialized network: number of inputs, outputs, weights.
  ************************************************************************ *)
procedure MLPProperties(const Network: TMultiLayerPerceptron;
  var NIn: TLearnInteger; var NOut: TLearnInteger; var WCount: TLearnInteger);
begin
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  WCount := Network.StructInfo[4];
end;

(* ************************************************************************
  Tells whether network is SOFTMAX-normalized (i.e. classifier) or not.
  ************************************************************************ *)
function MLPIsSoftmax(const Network: TMultiLayerPerceptron): Boolean;
begin
  Result := Network.StructInfo[6] = 1;
end;

(* ************************************************************************
  Procesing

  INPUT PARAMETERS:
  Network -   neural network
  X       -   input vector,  array[0..NIn-1].

  OUTPUT PARAMETERS:
  Y       -   result. Regression estimate when solving regression  task,
  vector of posterior probabilities for classification task.
  Subroutine does not allocate memory for this vector, it is
  responsibility of a caller to allocate it. Array  must  be
  at least [0..NOut-1].
  ************************************************************************ *)
procedure MLPProcess(var Network: TMultiLayerPerceptron; const X: TLearnFloatArray;
  var Y: TLearnFloatArray);
begin
  MLPInternalProcessVector(Network.StructInfo, Network.Weights,
    Network.ColumnMeans, Network.ColumnSigmas, Network.Neurons,
    Network.DFDNET, X, Y);
end;

(* ************************************************************************
  Error function for neural network, internal subroutine.
  ************************************************************************ *)
function MLPError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray;
  SSize: TLearnInteger): TLearnFloat;
var
  I     : TLearnInteger;
  K     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
  E     : TLearnFloat;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  I := 0;
  while I <= SSize - 1 do
    begin
      APVMove(@Network.X[0], 0, NIn - 1, @XY[I][0], 0, NIn - 1);
      MLPProcess(Network, Network.X, Network.Y);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels outputs
          //
          K := Round(XY[I, NIn]);
          if (K >= 0) and (K < NOut) then
            begin
              Network.Y[K] := Network.Y[K] - 1;
            end;
        end
      else
        begin

          //
          // real outputs
          //
          APVSub(@Network.Y[0], 0, NOut - 1, @XY[I][0], NIn, NIn + NOut - 1);
        end;
      E := APVDotProduct(@Network.Y[0], 0, NOut - 1, @Network.Y[0], 0, NOut - 1);
      Result := Result + E / 2;
      Inc(I);
    end;
end;

(* ************************************************************************
  Natural error function for neural network, internal subroutine.
  ************************************************************************ *)
function MLPErrorN(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray;
  SSize: TLearnInteger): TLearnFloat;
var
  I     : TLearnInteger;
  K     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
  E     : TLearnFloat;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  for I := 0 to SSize - 1 do
    begin

      //
      // Process vector
      //
      APVMove(@Network.X[0], 0, NIn - 1, @XY[I][0], 0, NIn - 1);
      MLPProcess(Network, Network.X, Network.Y);

      //
      // Update error function
      //
      if Network.StructInfo[6] = 0 then
        begin

          //
          // Least squares error function
          //
          APVSub(@Network.Y[0], 0, NOut - 1, @XY[I][0], NIn, NIn + NOut - 1);
          E := APVDotProduct(@Network.Y[0], 0, NOut - 1, @Network.Y[0], 0,
            NOut - 1);
          Result := Result + E * 0.5;
        end
      else
        begin

          //
          // Cross-entropy error function
          //
          K := Round(XY[I, NIn]);
          if (K >= 0) and (K < NOut) then
            begin
              Result := Result + SafeCrossEntropy(1, Network.Y[K]);
            end;
        end;
    end;
end;

(* ************************************************************************
  Classification error
  ************************************************************************ *)
function MLPClsError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray;
  SSize: TLearnInteger): TLearnInteger;
var
  I     : TLearnInteger;
  J     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
  WorkX : TLearnFloatArray;
  WorkY : TLearnFloatArray;
  NN    : TLearnInteger;
  NS    : TLearnInteger;
  NMAX  : TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  SetLength(WorkX, NIn (* - 1 + 1 // optimized compler *) );
  SetLength(WorkY, NOut (* - 1 + 1 // optimized compler *) );
  Result := 0;
  I := 0;
  while I <= SSize - 1 do
    begin

      //
      // Process
      //
      APVMove(@WorkX[0], 0, NIn - 1, @XY[I][0], 0, NIn - 1);
      MLPProcess(Network, WorkX, WorkY);

      //
      // Network version of the answer
      //
      NMAX := 0;
      J := 0;
      while J <= NOut - 1 do
        begin
          if AP_FP_Greater(WorkY[J], WorkY[NMAX]) then
            begin
              NMAX := J;
            end;
          Inc(J);
        end;
      NN := NMAX;

      //
      // Right answer
      //
      if MLPIsSoftmax(Network) then
        begin
          NS := Round(XY[I, NIn]);
        end
      else
        begin
          NMAX := 0;
          J := 0;
          while J <= NOut - 1 do
            begin
              if AP_FP_Greater(XY[I, NIn + J], XY[I, NIn + NMAX]) then
                begin
                  NMAX := J;
                end;
              Inc(J);
            end;
          NS := NMAX;
        end;

      //
      // compare
      //
      if NN <> NS then
        begin
          Result := Result + 1;
        end;
      Inc(I);
    end;
end;

(* ************************************************************************
  Relative classification error on the test set

  INPUT PARAMETERS:
  Network -   network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  percent of incorrectly classified cases. Works both for
  classifier networks and general purpose networks used as
  classifiers.
  ************************************************************************ *)
function MLPRelClsError(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat;
begin
  Result := AP_Double(MLPClsError(Network, XY, NPoints)) / NPoints;
end;

(* ************************************************************************
  Average cross-entropy (in bits per element) on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  CrossEntropy/(NPoints*LN(2)).
  Zero if network solves regression task.
  ************************************************************************ *)
function MLPAvgCE(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  if MLPIsSoftmax(Network) then
    begin
      MLPProperties(Network, NIn, NOut, WCount);
      Result := MLPErrorN(Network, XY, NPoints) / (NPoints * Ln(2));
    end
  else
    begin
      Result := 0;
    end;
end;

(* ************************************************************************
  RMS error on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  root mean square error.
  Its meaning for regression task is obvious. As for
  classification task, RMS error means error when estimating posterior
  probabilities.
  ************************************************************************ *)
function MLPRMSError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := Sqrt(2 * MLPError(Network, XY, NPoints) / (NPoints * NOut));
end;

(* ************************************************************************
  Average error on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for
  classification task, it means average error when estimating posterior
  probabilities.
  ************************************************************************ *)
function MLPAvgError(var Network: TMultiLayerPerceptron; const XY: TLearnFloat2DArray;
  NPoints: TLearnInteger): TLearnFloat;
var
  I     : TLearnInteger;
  J     : TLearnInteger;
  K     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  I := 0;
  while I <= NPoints - 1 do
    begin
      APVMove(@Network.X[0], 0, NIn - 1, @XY[I][0], 0, NIn - 1);
      MLPProcess(Network, Network.X, Network.Y);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels
          //
          K := Round(XY[I, NIn]);
          J := 0;
          while J <= NOut - 1 do
            begin
              if J = K then
                begin
                  Result := Result + AbsReal(1 - Network.Y[J]);
                end
              else
                begin
                  Result := Result + AbsReal(Network.Y[J]);
                end;
              Inc(J);
            end;
        end
      else
        begin

          //
          // real outputs
          //
          J := 0;
          while J <= NOut - 1 do
            begin
              Result := Result + AbsReal(XY[I, NIn + J] - Network.Y[J]);
              Inc(J);
            end;
        end;
      Inc(I);
    end;
  Result := Result / (NPoints * NOut);
end;

(* ************************************************************************
  Average relative error on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for
  classification task, it means average relative error when estimating
  posterior probability of belonging to the correct class.

  ************************************************************************ *)
function MLPAvgRelError(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; NPoints: TLearnInteger): TLearnFloat;
var
  I     : TLearnInteger;
  J     : TLearnInteger;
  K     : TLearnInteger;
  LK    : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  K := 0;
  I := 0;
  while I <= NPoints - 1 do
    begin
      APVMove(@Network.X[0], 0, NIn - 1, @XY[I][0], 0, NIn - 1);
      MLPProcess(Network, Network.X, Network.Y);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels
          //
          LK := Round(XY[I, NIn]);
          J := 0;
          while J <= NOut - 1 do
            begin
              if J = LK then
                begin
                  Result := Result + AbsReal(1 - Network.Y[J]);
                  K := K + 1;
                end;
              Inc(J);
            end;
        end
      else
        begin

          //
          // real outputs
          //
          J := 0;
          while J <= NOut - 1 do
            begin
              if AP_FP_Neq(XY[I, NIn + J], 0) then
                begin
                  Result := Result + AbsReal(XY[I, NIn + J] - Network.Y[J]) /
                    AbsReal(XY[I, NIn + J]);
                  K := K + 1;
                end;
              Inc(J);
            end;
        end;
      Inc(I);
    end;
  if K <> 0 then
    begin
      Result := Result / K;
    end;
end;

(* ************************************************************************
  Gradient calculation. Internal subroutine.
  ************************************************************************ *)
procedure MLPGrad(var Network: TMultiLayerPerceptron; const X: TLearnFloatArray;
  const DesiredY: TLearnFloatArray; var E: TLearnFloat; var Grad: TLearnFloatArray);
var
  I     : TLearnInteger;
  NOut  : TLearnInteger;
  NTotal: TLearnInteger;
begin

  //
  // Prepare dError/dOut, internal structures
  //
  MLPProcess(Network, X, Network.Y);
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  E := 0;
  I := 0;
  while I <= NTotal - 1 do
    begin
      Network.DError[I] := 0;
      Inc(I);
    end;
  I := 0;
  while I <= NOut - 1 do
    begin
      Network.DError[NTotal - NOut + I] := Network.Y[I] - DesiredY[I];
      E := E + AP_Sqr(Network.Y[I] - DesiredY[I]) / 2;
      Inc(I);
    end;

  //
  // gradient
  //
  MLPInternalCalculateGradient(Network, Network.Neurons, Network.Weights,
    Network.DError, Grad, False);
end;

(* ************************************************************************
  Gradient calculation (natural error function). Internal subroutine.
  ************************************************************************ *)
procedure MLPGradN(var Network: TMultiLayerPerceptron; const X: TLearnFloatArray;
  const DesiredY: TLearnFloatArray; var E: TLearnFloat; var Grad: TLearnFloatArray);
var
  S     : TLearnFloat;
  I     : TLearnInteger;
  NOut  : TLearnInteger;
  NTotal: TLearnInteger;
begin

  //
  // Prepare dError/dOut, internal structures
  //
  MLPProcess(Network, X, Network.Y);
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  I := 0;
  while I <= NTotal - 1 do
    begin
      Network.DError[I] := 0;
      Inc(I);
    end;
  E := 0;
  if Network.StructInfo[6] = 0 then
    begin

      //
      // Regression network, least squares
      //
      I := 0;
      while I <= NOut - 1 do
        begin
          Network.DError[NTotal - NOut + I] := Network.Y[I] - DesiredY[I];
          E := E + AP_Sqr(Network.Y[I] - DesiredY[I]) / 2;
          Inc(I);
        end;
    end
  else
    begin

      //
      // Classification network, cross-entropy
      //
      S := 0;
      I := 0;
      while I <= NOut - 1 do
        begin
          S := S + DesiredY[I];
          Inc(I);
        end;
      I := 0;
      while I <= NOut - 1 do
        begin
          Network.DError[NTotal - NOut + I] := S * Network.Y[I] - DesiredY[I];
          E := E + SafeCrossEntropy(DesiredY[I], Network.Y[I]);
          Inc(I);
        end;
    end;

  //
  // gradient
  //
  MLPInternalCalculateGradient(Network, Network.Neurons, Network.Weights,
    Network.DError, Grad, True);
end;

(* ************************************************************************
  Batch gradient calculation. Internal subroutine.
  ************************************************************************ *)
procedure MLPGradBatch(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat;
  var Grad: TLearnFloatArray);
var
  I     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  I := 0;
  while I <= WCount - 1 do
    begin
      Grad[I] := 0;
      Inc(I);
    end;
  E := 0;
  I := 0;
  while I <= SSize - 1 do
    begin
      MLPChunkedGradient(Network, XY, I, Min(SSize, I + ChunkSize) - I, E,
        Grad, False);
      I := I + ChunkSize;
    end;
end;

(* ************************************************************************
  Batch gradient calculation (natural error function). Internal subroutine.
  ************************************************************************ *)
procedure MLPGradNBatch(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat;
  var Grad: TLearnFloatArray);
var
  I     : TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  WCount: TLearnInteger;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  I := 0;
  while I <= WCount - 1 do
    begin
      Grad[I] := 0;
      Inc(I);
    end;
  E := 0;
  I := 0;
  while I <= SSize - 1 do
    begin
      MLPChunkedGradient(Network, XY, I, Min(SSize, I + ChunkSize) - I, E,
        Grad, True);
      I := I + ChunkSize;
    end;
end;

(* ************************************************************************
  Batch Hessian calculation (natural error function) using R-algorithm.
  Internal subroutine.

  Hessian calculation based on R-algorithm described in
  "Fast Exact Multiplication by the Hessian",
  B. A. Pearlmutter,
  Neural Computation, 1994.
  ************************************************************************ *)
procedure MLPHessianNBatch(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat;
  var Grad: TLearnFloatArray; var H: TLearnFloat2DArray);
begin
  MLPHessianBatchInternal(Network, XY, SSize, True, E, Grad, H);
end;

(* ************************************************************************
  Batch Hessian calculation using R-algorithm.
  Internal subroutine.

  Hessian calculation based on R-algorithm described in
  "Fast Exact Multiplication by the Hessian",
  B. A. Pearlmutter,
  Neural Computation, 1994.
  ************************************************************************ *)
procedure MLPHessianBatch(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; SSize: TLearnInteger; var E: TLearnFloat;
  var Grad: TLearnFloatArray; var H: TLearnFloat2DArray);
begin
  MLPHessianBatchInternal(Network, XY, SSize, False, E, Grad, H);
end;

(* ************************************************************************
  Internal subroutine, shouldn't be called by user.
  ************************************************************************ *)
procedure MLPInternalProcessVector(const StructInfo: TLearnIntegerArray;
  const Weights: TLearnFloatArray; const ColumnMeans: TLearnFloatArray;
  const ColumnSigmas: TLearnFloatArray; var Neurons: TLearnFloatArray;
  var DFDNET: TLearnFloatArray; const X: TLearnFloatArray; var Y: TLearnFloatArray);
var
  I     : TLearnInteger;
  N1    : TLearnInteger;
  N2    : TLearnInteger;
  W1    : TLearnInteger;
  W2    : TLearnInteger;
  NTotal: TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  IStart: TLearnInteger;
  Offs  : TLearnInteger;
  NET   : TLearnFloat;
  F     : TLearnFloat;
  DF    : TLearnFloat;
  D2F   : TLearnFloat;
  MX    : TLearnFloat;
  PErr  : Boolean;
begin

  //
  // Read network geometry
  //
  NIn := StructInfo[1];
  NOut := StructInfo[2];
  NTotal := StructInfo[3];
  IStart := StructInfo[5];

  //
  // Inputs standartisation and putting in the network
  //
  for I := 0 to NIn - 1 do
    begin
      if AP_FP_Neq(ColumnSigmas[I], 0) then
        begin
          Neurons[I] := (X[I] - ColumnMeans[I]) / ColumnSigmas[I];
        end
      else
        begin
          Neurons[I] := X[I] - ColumnMeans[I];
        end;
    end;

  //
  // Process network
  //
  I := 0;
  for I := 0 to NTotal - 1 do
    begin
      Offs := IStart + I * NFieldWidth;
      if StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function
          //
          MLPActivationFunction(Neurons[StructInfo[Offs + 2]], StructInfo[Offs + 0],
            F, DF, D2F);
          Neurons[I] := F;
          DFDNET[I] := DF;
        end;
      if StructInfo[Offs + 0] = 0 then
        begin

          //
          // Adaptive summator
          //
          N1 := StructInfo[Offs + 2];
          N2 := N1 + StructInfo[Offs + 1] - 1;
          W1 := StructInfo[Offs + 3];
          W2 := W1 + StructInfo[Offs + 1] - 1;
          NET := APVDotProduct(@Weights[0], W1, W2, @Neurons[0], N1, N2);
          Neurons[I] := NET;
          DFDNET[I] := 1.0;
        end;
      if StructInfo[Offs + 0] < 0 then
        begin
          PErr := True;
          if StructInfo[Offs + 0] = -2 then
            begin

              //
              // input neuron, left unchanged
              //
              PErr := False;
            end;
          if StructInfo[Offs + 0] = -3 then
            begin

              //
              // "-1" neuron
              //
              Neurons[I] := -1;
              PErr := False;
            end;
          if StructInfo[Offs + 0] = -4 then
            begin

              //
              // "0" neuron
              //
              Neurons[I] := 0;
              PErr := False;
            end;
          Assert(not PErr,
            'MLPInternalProcessVector: internal error - unknown neuron type!');
        end;
    end;

  //
  // Extract result
  //
  APVMove(@Y[0], 0, NOut - 1, @Neurons[0], NTotal - NOut, NTotal - 1);

  //
  // Softmax post-processing or standardisation if needed
  //
  Assert((StructInfo[6] = 0) or (StructInfo[6] = 1),
    'MLPInternalProcessVector: unknown normalization type!');
  if StructInfo[6] = 1 then
    begin

      //
      // Softmax
      //
      MX := Y[0];
      for I := 1 to NOut - 1 do
        begin
          MX := Max(MX, Y[I]);
        end;
      NET := 0;
      for I := 0 to NOut - 1 do
        begin
          Y[I] := Exp(Y[I] - MX);
          NET := NET + Y[I];
        end;
      for I := 0 to NOut - 1 do
        begin
          Y[I] := Y[I] / NET;
        end;
    end
  else
    begin

      //
      // Standardisation
      //
      for I := 0 to NOut - 1 do
        begin
          Y[I] := Y[I] * ColumnSigmas[NIn + I] + ColumnMeans[NIn + I];
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine: adding new input layer to network
  ************************************************************************ *)
procedure AddInputLayer(NCount: TLearnInteger; var LSizes: TLearnIntegerArray;
  var LTypes: TLearnIntegerArray; var LConnFirst: TLearnIntegerArray;
  var LConnLast: TLearnIntegerArray; var LastProc: TLearnInteger);
begin
  LSizes[0] := NCount;
  LTypes[0] := -2;
  LConnFirst[0] := 0;
  LConnLast[0] := 0;
  LastProc := 0;
end;

(* ************************************************************************
  Internal subroutine: adding new summator layer to network
  ************************************************************************ *)
procedure AddBiasedSummatorLayer(NCount: TLearnInteger;
  var LSizes: TLearnIntegerArray; var LTypes: TLearnIntegerArray;
  var LConnFirst: TLearnIntegerArray; var LConnLast: TLearnIntegerArray;
  var LastProc: TLearnInteger);
begin
  LSizes[LastProc + 1] := 1;
  LTypes[LastProc + 1] := -3;
  LConnFirst[LastProc + 1] := 0;
  LConnLast[LastProc + 1] := 0;
  LSizes[LastProc + 2] := NCount;
  LTypes[LastProc + 2] := 0;
  LConnFirst[LastProc + 2] := LastProc;
  LConnLast[LastProc + 2] := LastProc + 1;
  LastProc := LastProc + 2;
end;

(* ************************************************************************
  Internal subroutine: adding new summator layer to network
  ************************************************************************ *)
procedure AddActivationLayer(FuncType: TLearnInteger;
  var LSizes: TLearnIntegerArray; var LTypes: TLearnIntegerArray;
  var LConnFirst: TLearnIntegerArray; var LConnLast: TLearnIntegerArray;
  var LastProc: TLearnInteger);
begin
  Assert(FuncType > 0, 'AddActivationLayer: incorrect function type');
  LSizes[LastProc + 1] := LSizes[LastProc];
  LTypes[LastProc + 1] := FuncType;
  LConnFirst[LastProc + 1] := LastProc;
  LConnLast[LastProc + 1] := LastProc;
  LastProc := LastProc + 1;
end;

(* ************************************************************************
  Internal subroutine: adding new zero layer to network
  ************************************************************************ *)
procedure AddZeroLayer(var LSizes: TLearnIntegerArray; var LTypes: TLearnIntegerArray;
  var LConnFirst: TLearnIntegerArray; var LConnLast: TLearnIntegerArray;
  var LastProc: TLearnInteger);
begin
  LSizes[LastProc + 1] := 1;
  LTypes[LastProc + 1] := -4;
  LConnFirst[LastProc + 1] := 0;
  LConnLast[LastProc + 1] := 0;
  LastProc := LastProc + 1;
end;

(* ************************************************************************
  Internal subroutine.
  ************************************************************************ *)
procedure MLPCreate(NIn: TLearnInteger; NOut: TLearnInteger;
  const LSizes: TLearnIntegerArray; const LTypes: TLearnIntegerArray;
  const LConnFirst: TLearnIntegerArray; const LConnLast: TLearnIntegerArray;
  LayersCount: TLearnInteger; IsClsNet: Boolean;
  var Network: TMultiLayerPerceptron);
var
  I         : TLearnInteger;
  J         : TLearnInteger;
  SSize     : TLearnInteger;
  NTotal    : TLearnInteger;
  WCount    : TLearnInteger;
  Offs      : TLearnInteger;
  NProcessed: TLearnInteger;
  WAllocated: TLearnInteger;
  LocalTemp : TLearnIntegerArray;
  LNFirst   : TLearnIntegerArray;
  LNSyn     : TLearnIntegerArray;
begin

  //
  // Check
  //
  Assert(LayersCount > 0, 'MLPCreate: wrong parameters!');
  Assert(LTypes[0] = -2, 'MLPCreate: wrong LTypes[0] (must be -2)!');
  I := 0;
  while I <= LayersCount - 1 do
    begin
      Assert(LSizes[I] > 0, 'MLPCreate: wrong LSizes!');
      Assert((LConnFirst[I] >= 0) and ((LConnFirst[I] < I) or (I = 0)),
        'MLPCreate: wrong LConnFirst!');
      Assert((LConnLast[I] >= LConnFirst[I]) and ((LConnLast[I] < I) or (I = 0)),
        'MLPCreate: wrong LConnLast!');
      Inc(I);
    end;

  //
  // Build network geometry
  //
  SetLength(LNFirst, LayersCount (* - 1 + 1 // optimized compler *) );
  SetLength(LNSyn, LayersCount (* - 1 + 1 // optimized compler *) );
  NTotal := 0;
  WCount := 0;
  I := 0;
  while I <= LayersCount - 1 do
    begin

      //
      // Analyze connections.
      // This code must throw an assertion in case of unknown LTypes[I]
      //
      LNSyn[I] := -1;
      if LTypes[I] >= 0 then
        begin
          LNSyn[I] := 0;
          J := LConnFirst[I];
          while J <= LConnLast[I] do
            begin
              LNSyn[I] := LNSyn[I] + LSizes[J];
              Inc(J);
            end;
        end
      else
        begin
          if (LTypes[I] = -2) or (LTypes[I] = -3) or (LTypes[I] = -4) then
            begin
              LNSyn[I] := 0;
            end;
        end;
      Assert(LNSyn[I] >= 0, 'MLPCreate: internal error #0!');

      //
      // Other info
      //
      LNFirst[I] := NTotal;
      NTotal := NTotal + LSizes[I];
      if LTypes[I] = 0 then
        begin
          WCount := WCount + LNSyn[I] * LSizes[I];
        end;
      Inc(I);
    end;
  SSize := 7 + NTotal * NFieldWidth;

  //
  // Allocate
  //
  SetLength(Network.StructInfo, SSize (* - 1 + 1 // optimized compler *) );
  SetLength(Network.Weights, WCount (* - 1 + 1 // optimized compler *) );
  if IsClsNet then
    begin
      SetLength(Network.ColumnMeans, NIn (* - 1 + 1 // optimized compler *) );
      SetLength(Network.ColumnSigmas, NIn (* - 1 + 1 // optimized compler *) );
    end
  else
    begin
      SetLength(Network.ColumnMeans, NIn + NOut (* - 1 + 1 // optimized compler *) );
      SetLength(Network.ColumnSigmas, NIn + NOut (* - 1 + 1 // optimized compler *) );
    end;
  SetLength(Network.Neurons, NTotal (* - 1 + 1 // optimized compler *) );
  SetLength(Network.Chunks, 3 * NTotal + 1, ChunkSize (* - 1 + 1 // optimized compler *) );
  SetLength(Network.NWBuf, Max(WCount, 2 * NOut) (* - 1 + 1 // optimized compler *) );
  SetLength(Network.DFDNET, NTotal (* - 1 + 1 // optimized compler *) );
  SetLength(Network.X, NIn (* - 1 + 1 // optimized compler *) );
  SetLength(Network.Y, NOut (* - 1 + 1 // optimized compler *) );
  SetLength(Network.DError, NTotal (* - 1 + 1 // optimized compler *) );

  //
  // Fill structure: global info
  //
  Network.StructInfo[0] := SSize;
  Network.StructInfo[1] := NIn;
  Network.StructInfo[2] := NOut;
  Network.StructInfo[3] := NTotal;
  Network.StructInfo[4] := WCount;
  Network.StructInfo[5] := 7;
  if IsClsNet then
    begin
      Network.StructInfo[6] := 1;
    end
  else
    begin
      Network.StructInfo[6] := 0;
    end;

  //
  // Fill structure: neuron connections
  //
  NProcessed := 0;
  WAllocated := 0;
  I := 0;
  while I <= LayersCount - 1 do
    begin
      J := 0;
      while J <= LSizes[I] - 1 do
        begin
          Offs := Network.StructInfo[5] + NProcessed * NFieldWidth;
          Network.StructInfo[Offs + 0] := LTypes[I];
          if LTypes[I] = 0 then
            begin

              //
              // Adaptive summator:
              // * connections with weights to previous neurons
              //
              Network.StructInfo[Offs + 1] := LNSyn[I];
              Network.StructInfo[Offs + 2] := LNFirst[LConnFirst[I]];
              Network.StructInfo[Offs + 3] := WAllocated;
              WAllocated := WAllocated + LNSyn[I];
              NProcessed := NProcessed + 1;
            end;
          if LTypes[I] > 0 then
            begin

              //
              // Activation layer:
              // * each neuron connected to one (only one) of previous neurons.
              // * no weights
              //
              Network.StructInfo[Offs + 1] := 1;
              Network.StructInfo[Offs + 2] := LNFirst[LConnFirst[I]] + J;
              Network.StructInfo[Offs + 3] := -1;
              NProcessed := NProcessed + 1;
            end;
          if (LTypes[I] = -2) or (LTypes[I] = -3) or (LTypes[I] = -4) then
            begin
              NProcessed := NProcessed + 1;
            end;
          Inc(J);
        end;
      Inc(I);
    end;
  Assert(WAllocated = WCount, 'MLPCreate: internal error #1!');
  Assert(NProcessed = NTotal, 'MLPCreate: internal error #2!');

  //
  // Fill weights by small random values
  // Initialize means and sigmas
  //
  I := 0;
  while I <= WCount - 1 do
    begin
      Network.Weights[I] := RandomReal - 0.5;
      Inc(I);
    end;
  I := 0;
  while I <= NIn - 1 do
    begin
      Network.ColumnMeans[I] := 0;
      Network.ColumnSigmas[I] := 1;
      Inc(I);
    end;
  if not IsClsNet then
    begin
      I := 0;
      while I <= NOut - 1 do
        begin
          Network.ColumnMeans[NIn + I] := 0;
          Network.ColumnSigmas[NIn + I] := 1;
          Inc(I);
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine
  ************************************************************************ *)
procedure MLPActivationFunction(NET: TLearnFloat; K: TLearnInteger;
  var F: TLearnFloat; var DF: TLearnFloat; var D2F: TLearnFloat);
var
  NET2: TLearnFloat;
  ARG : TLearnFloat;
  ROOT: TLearnFloat;
  R   : TLearnFloat;
begin
  F := 0;
  DF := 0;
  if K = 1 then
    begin

      //
      // TanH activation function
      //
      if AP_FP_Less(AbsReal(NET), 100) then
        begin
          F := TanH(NET);
        end
      else
        begin
          F := Sign(NET);
        end;
      DF := 1 - AP_Sqr(F);
      D2F := -2 * F * DF;
      Exit;
    end;
  if K = 3 then
    begin

      //
      // EX activation function
      //
      if AP_FP_Greater_Eq(NET, 0) then
        begin
          NET2 := NET * NET;
          ARG := NET2 + 1;
          ROOT := Sqrt(ARG);
          F := NET + ROOT;
          R := NET / ROOT;
          DF := 1 + R;
          D2F := (ROOT - NET * R) / ARG;
        end
      else
        begin
          F := Exp(NET);
          DF := F;
          D2F := F;
        end;
      Exit;
    end;
  if K = 2 then
    begin
      F := Exp(-AP_Sqr(NET));
      DF := -2 * NET * F;
      D2F := -2 * (F + DF * NET);
      Exit;
    end;
end;

(* ************************************************************************
  Internal subroutine for Hessian calculation.

  WARNING!!! Unspeakable math far beyong human capabilities :)
  ************************************************************************ *)
procedure MLPHessianBatchInternal(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; SSize: TLearnInteger; NaturalErr: Boolean;
  var E: TLearnFloat; var Grad: TLearnFloatArray; var H: TLearnFloat2DArray);
var
  NIn     : TLearnInteger;
  NOut    : TLearnInteger;
  WCount  : TLearnInteger;
  NTotal  : TLearnInteger;
  IStart  : TLearnInteger;
  I       : TLearnInteger;
  J       : TLearnInteger;
  K       : TLearnInteger;
  KL      : TLearnInteger;
  Offs    : TLearnInteger;
  N1      : TLearnInteger;
  N2      : TLearnInteger;
  W1      : TLearnInteger;
  W2      : TLearnInteger;
  S       : TLearnFloat;
  T       : TLearnFloat;
  V       : TLearnFloat;
  ET      : TLearnFloat;
  BFlag   : Boolean;
  F       : TLearnFloat;
  DF      : TLearnFloat;
  D2F     : TLearnFloat;
  dEIdYJ  : TLearnFloat;
  MX      : TLearnFloat;
  Q       : TLearnFloat;
  Z       : TLearnFloat;
  S2      : TLearnFloat;
  ExpI    : TLearnFloat;
  ExpJ    : TLearnFloat;
  X       : TLearnFloatArray;
  DesiredY: TLearnFloatArray;
  GT      : TLearnFloatArray;
  Zeros   : TLearnFloatArray;
  RX      : TLearnFloat2DArray;
  RY      : TLearnFloat2DArray;
  RDX     : TLearnFloat2DArray;
  RDY     : TLearnFloat2DArray;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Prepare
  //
  SetLength(X, NIn (* - 1 + 1 // optimized compler *) );
  SetLength(DesiredY, NOut (* - 1 + 1 // optimized compler *) );
  SetLength(Zeros, WCount (* - 1 + 1 // optimized compler *) );
  SetLength(GT, WCount (* - 1 + 1 // optimized compler *) );
  SetLength(RX, NTotal + NOut (* - 1 + 1 // optimized compler *) , WCount (* - 1 + 1 // optimized compler *) );
  SetLength(RY, NTotal + NOut (* - 1 + 1 // optimized compler *) , WCount (* - 1 + 1 // optimized compler *) );
  SetLength(RDX, NTotal + NOut (* - 1 + 1 // optimized compler *) , WCount (* - 1 + 1 // optimized compler *) );
  SetLength(RDY, NTotal + NOut (* - 1 + 1 // optimized compler *) , WCount (* - 1 + 1 // optimized compler *) );
  E := 0;
  I := 0;
  while I <= WCount - 1 do
    begin
      Zeros[I] := 0;
      Inc(I);
    end;
  APVMove(@Grad[0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
  I := 0;
  while I <= WCount - 1 do
    begin
      APVMove(@H[I][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
      Inc(I);
    end;

  //
  // Process
  //
  K := 0;
  while K <= SSize - 1 do
    begin

      //
      // Process vector with MLPGradN.
      // Now Neurons, DFDNET and DError contains results of the last run.
      //
      APVMove(@X[0], 0, NIn - 1, @XY[K][0], 0, NIn - 1);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels outputs
          //
          KL := Round(XY[K, NIn]);
          I := 0;
          while I <= NOut - 1 do
            begin
              if I = KL then
                begin
                  DesiredY[I] := 1;
                end
              else
                begin
                  DesiredY[I] := 0;
                end;
              Inc(I);
            end;
        end
      else
        begin

          //
          // real outputs
          //
          APVMove(@DesiredY[0], 0, NOut - 1, @XY[K][0], NIn, NIn + NOut - 1);
        end;
      if NaturalErr then
        begin
          MLPGradN(Network, X, DesiredY, ET, GT);
        end
      else
        begin
          MLPGrad(Network, X, DesiredY, ET, GT);
        end;

      //
      // grad, error
      //
      E := E + ET;
      APVAdd(@Grad[0], 0, WCount - 1, @GT[0], 0, WCount - 1);

      //
      // Hessian.
      // Forward pass of the R-algorithm
      //
      I := 0;
      while I <= NTotal - 1 do
        begin
          Offs := IStart + I * NFieldWidth;
          APVMove(@RX[I][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
          APVMove(@RY[I][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
          if Network.StructInfo[Offs + 0] > 0 then
            begin

              //
              // Activation function
              //
              N1 := Network.StructInfo[Offs + 2];
              APVMove(@RX[I][0], 0, WCount - 1, @RY[N1][0], 0, WCount - 1);
              V := Network.DFDNET[I];
              APVMove(@RY[I][0], 0, WCount - 1, @RX[I][0], 0, WCount - 1, V);
            end;
          if Network.StructInfo[Offs + 0] = 0 then
            begin

              //
              // Adaptive summator
              //
              N1 := Network.StructInfo[Offs + 2];
              N2 := N1 + Network.StructInfo[Offs + 1] - 1;
              W1 := Network.StructInfo[Offs + 3];
              W2 := W1 + Network.StructInfo[Offs + 1] - 1;
              J := N1;
              while J <= N2 do
                begin
                  V := Network.Weights[W1 + J - N1];
                  APVAdd(@RX[I][0], 0, WCount - 1, @RY[J][0], 0, WCount - 1, V);
                  RX[I, W1 + J - N1] := RX[I, W1 + J - N1] + Network.Neurons[J];
                  Inc(J);
                end;
              APVMove(@RY[I][0], 0, WCount - 1, @RX[I][0], 0, WCount - 1);
            end;
          if Network.StructInfo[Offs + 0] < 0 then
            begin
              BFlag := True;
              if Network.StructInfo[Offs + 0] = -2 then
                begin

                  //
                  // input neuron, left unchanged
                  //
                  BFlag := False;
                end;
              if Network.StructInfo[Offs + 0] = -3 then
                begin

                  //
                  // "-1" neuron, left unchanged
                  //
                  BFlag := False;
                end;
              if Network.StructInfo[Offs + 0] = -4 then
                begin

                  //
                  // "0" neuron, left unchanged
                  //
                  BFlag := False;
                end;
              Assert(not BFlag,
                'MLPHessianNBatch: internal error - unknown neuron type!');
            end;
          Inc(I);
        end;

      //
      // Hessian. Backward pass of the R-algorithm.
      //
      // Stage 1. Initialize RDY
      //
      I := 0;
      while I <= NTotal + NOut - 1 do
        begin
          APVMove(@RDY[I][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
          Inc(I);
        end;
      if Network.StructInfo[6] = 0 then
        begin

          //
          // Standardisation.
          //
          // In context of the Hessian calculation standardisation
          // is considered as additional layer with weightless
          // activation function:
          //
          // F(NET) := Sigma*NET
          //
          // So we add one more layer to forward pass, and
          // make forward/backward pass through this layer.
          //
          I := 0;
          while I <= NOut - 1 do
            begin
              N1 := NTotal - NOut + I;
              N2 := NTotal + I;

              //
              // Forward pass from N1 to N2
              //
              APVMove(@RX[N2][0], 0, WCount - 1, @RY[N1][0], 0, WCount - 1);
              V := Network.ColumnSigmas[NIn + I];
              APVMove(@RY[N2][0], 0, WCount - 1, @RX[N2][0], 0, WCount - 1, V);

              //
              // Initialization of RDY
              //
              APVMove(@RDY[N2][0], 0, WCount - 1, @RY[N2][0], 0, WCount - 1);

              //
              // Backward pass from N2 to N1:
              // 1. Calculate R(dE/dX).
              // 2. No R(dE/dWij) is needed since weight of activation neuron
              // is fixed to 1. So we can update R(dE/dY) for
              // the connected neuron (note that Vij=0, Wij=1)
              //
              DF := Network.ColumnSigmas[NIn + I];
              APVMove(@RDX[N2][0], 0, WCount - 1, @RDY[N2][0], 0, WCount - 1, DF);
              APVAdd(@RDY[N1][0], 0, WCount - 1, @RDX[N2][0], 0, WCount - 1);
              Inc(I);
            end;
        end
      else
        begin

          //
          // Softmax.
          //
          // Initialize RDY using generalized expression for ei'(yi)
          // (see expression (9) from p. 5 of "Fast Exact Multiplication by the Hessian").
          //
          // When we are working with softmax network, generalized
          // expression for ei'(yi) is used because softmax
          // normalization leads to ei, which depends on all y's
          //
          if NaturalErr then
            begin

              //
              // softmax + cross-entropy.
              // We have:
              //
              // S = sum(exp(yk)),
              // ei = sum(trn)*exp(yi)/S-trn_i
              //
              // j=i:   d(ei)/d(yj) = T*exp(yi)*(S-exp(yi))/S^2
              // j<>i:  d(ei)/d(yj) = -T*exp(yi)*exp(yj)/S^2
              //
              T := 0;
              I := 0;
              while I <= NOut - 1 do
                begin
                  T := T + DesiredY[I];
                  Inc(I);
                end;
              MX := Network.Neurons[NTotal - NOut];
              I := 0;
              while I <= NOut - 1 do
                begin
                  MX := Max(MX, Network.Neurons[NTotal - NOut + I]);
                  Inc(I);
                end;
              S := 0;
              I := 0;
              while I <= NOut - 1 do
                begin
                  Network.NWBuf[I] := Exp(Network.Neurons[NTotal - NOut + I] - MX);
                  S := S + Network.NWBuf[I];
                  Inc(I);
                end;
              I := 0;
              while I <= NOut - 1 do
                begin
                  J := 0;
                  while J <= NOut - 1 do
                    begin
                      if J = I then
                        begin
                          dEIdYJ := T * Network.NWBuf[I] * (S - Network.NWBuf[I]) /
                            AP_Sqr(S);
                          APVAdd(@RDY[NTotal - NOut + I][0], 0, WCount - 1,
                            @RY[NTotal - NOut + I][0], 0, WCount - 1, dEIdYJ);
                        end
                      else
                        begin
                          dEIdYJ := -T * Network.NWBuf[I] * Network.NWBuf[J] / AP_Sqr(S);
                          APVAdd(@RDY[NTotal - NOut + I][0], 0, WCount - 1,
                            @RY[NTotal - NOut + J][0], 0, WCount - 1, dEIdYJ);
                        end;
                      Inc(J);
                    end;
                  Inc(I);
                end;
            end
          else
            begin

              //
              // For a softmax + squared error we have expression
              // far beyond human imagination so we dont even try
              // to comment on it. Just enjoy the code...
              //
              // P.S. That's why "natural error" is called "natural" -
              // compact beatiful expressions, fast code....
              //
              MX := Network.Neurons[NTotal - NOut];
              I := 0;
              while I <= NOut - 1 do
                begin
                  MX := Max(MX, Network.Neurons[NTotal - NOut + I]);
                  Inc(I);
                end;
              S := 0;
              S2 := 0;
              I := 0;
              while I <= NOut - 1 do
                begin
                  Network.NWBuf[I] := Exp(Network.Neurons[NTotal - NOut + I] - MX);
                  S := S + Network.NWBuf[I];
                  S2 := S2 + AP_Sqr(Network.NWBuf[I]);
                  Inc(I);
                end;
              Q := 0;
              I := 0;
              while I <= NOut - 1 do
                begin
                  Q := Q + (Network.Y[I] - DesiredY[I]) * Network.NWBuf[I];
                  Inc(I);
                end;
              I := 0;
              while I <= NOut - 1 do
                begin
                  Z := -Q + (Network.Y[I] - DesiredY[I]) * S;
                  ExpI := Network.NWBuf[I];
                  J := 0;
                  while J <= NOut - 1 do
                    begin
                      ExpJ := Network.NWBuf[J];
                      if J = I then
                        begin
                          dEIdYJ := ExpI / AP_Sqr(S) *
                            ((Z + ExpI) * (S - 2 * ExpI) / S + ExpI * S2 / AP_Sqr(S));
                        end
                      else
                        begin
                          dEIdYJ := ExpI * ExpJ / AP_Sqr(S) *
                            (S2 / AP_Sqr(S) - 2 * Z / S - (ExpI + ExpJ) / S +
                            (Network.Y[I] - DesiredY[I]) - (Network.Y[J] - DesiredY[J]));
                        end;
                      APVAdd(@RDY[NTotal - NOut + I][0], 0, WCount - 1,
                        @RY[NTotal - NOut + J][0], 0, WCount - 1, dEIdYJ);
                      Inc(J);
                    end;
                  Inc(I);
                end;
            end;
        end;

      //
      // Hessian. Backward pass of the R-algorithm
      //
      // Stage 2. Process.
      //
      I := NTotal - 1;
      while I >= 0 do
        begin

          //
          // Possible variants:
          // 1. Activation function
          // 2. Adaptive summator
          // 3. Special neuron
          //
          Offs := IStart + I * NFieldWidth;
          if Network.StructInfo[Offs + 0] > 0 then
            begin
              N1 := Network.StructInfo[Offs + 2];

              //
              // First, calculate R(dE/dX).
              //
              MLPActivationFunction(Network.Neurons[N1], Network.StructInfo[Offs + 0],
                F, DF, D2F);
              V := D2F * Network.DError[I];
              APVMove(@RDX[I][0], 0, WCount - 1, @RDY[I][0], 0, WCount - 1, DF);
              APVAdd(@RDX[I][0], 0, WCount - 1, @RX[I][0], 0, WCount - 1, V);

              //
              // No R(dE/dWij) is needed since weight of activation neuron
              // is fixed to 1.
              //
              // So we can update R(dE/dY) for the connected neuron.
              // (note that Vij=0, Wij=1)
              //
              APVAdd(@RDY[N1][0], 0, WCount - 1, @RDX[I][0], 0, WCount - 1);
            end;
          if Network.StructInfo[Offs + 0] = 0 then
            begin

              //
              // Adaptive summator
              //
              N1 := Network.StructInfo[Offs + 2];
              N2 := N1 + Network.StructInfo[Offs + 1] - 1;
              W1 := Network.StructInfo[Offs + 3];
              W2 := W1 + Network.StructInfo[Offs + 1] - 1;

              //
              // First, calculate R(dE/dX).
              //
              APVMove(@RDX[I][0], 0, WCount - 1, @RDY[I][0], 0, WCount - 1);

              //
              // Then, calculate R(dE/dWij)
              //
              J := W1;
              while J <= W2 do
                begin
                  V := Network.Neurons[N1 + J - W1];
                  APVAdd(@H[J][0], 0, WCount - 1, @RDX[I][0], 0, WCount - 1, V);
                  V := Network.DError[I];
                  APVAdd(@H[J][0], 0, WCount - 1, @RY[N1 + J - W1][0], 0,
                    WCount - 1, V);
                  Inc(J);
                end;

              //
              // And finally, update R(dE/dY) for connected neurons.
              //
              J := W1;
              while J <= W2 do
                begin
                  V := Network.Weights[J];
                  APVAdd(@RDY[N1 + J - W1][0], 0, WCount - 1, @RDX[I][0], 0,
                    WCount - 1, V);
                  RDY[N1 + J - W1, J] := RDY[N1 + J - W1, J] + Network.DError[I];
                  Inc(J);
                end;
            end;
          if Network.StructInfo[Offs + 0] < 0 then
            begin
              BFlag := False;
              if (Network.StructInfo[Offs + 0] = -2) or
                (Network.StructInfo[Offs + 0] = -3) or
                (Network.StructInfo[Offs + 0] = -4) then
                begin

                  //
                  // Special neuron type, no back-propagation required
                  //
                  BFlag := True;
                end;
              Assert(BFlag, 'MLPHessianNBatch: unknown neuron type!');
            end;
          Dec(I);
        end;
      Inc(K);
    end;
end;

(* ************************************************************************
  Internal subroutine

  Network must be processed by MLPProcess on X
  ************************************************************************ *)
procedure MLPInternalCalculateGradient(var Network: TMultiLayerPerceptron;
  const Neurons: TLearnFloatArray; const Weights: TLearnFloatArray;
  var DError: TLearnFloatArray; var Grad: TLearnFloatArray; NaturalErrorFunc: Boolean);
var
  I     : TLearnInteger;
  N1    : TLearnInteger;
  N2    : TLearnInteger;
  W1    : TLearnInteger;
  W2    : TLearnInteger;
  NTotal: TLearnInteger;
  IStart: TLearnInteger;
  NIn   : TLearnInteger;
  NOut  : TLearnInteger;
  Offs  : TLearnInteger;
  dEdF  : TLearnFloat;
  DFDNET: TLearnFloat;
  V     : TLearnFloat;
  FOwn  : TLearnFloat;
  DEOwn : TLearnFloat;
  NET   : TLearnFloat;
  MX    : TLearnFloat;
  BFlag : Boolean;
begin

  //
  // Read network geometry
  //
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Pre-processing of dError/dOut:
  // from dError/dOut(normalized) to dError/dOut(non-normalized)
  //
  Assert((Network.StructInfo[6] = 0) or (Network.StructInfo[6] = 1),
    'MLPInternalCalculateGradient: unknown normalization type!');
  if Network.StructInfo[6] = 1 then
    begin

      //
      // Softmax
      //
      if not NaturalErrorFunc then
        begin
          MX := Network.Neurons[NTotal - NOut];
          I := 0;
          while I <= NOut - 1 do
            begin
              MX := Max(MX, Network.Neurons[NTotal - NOut + I]);
              Inc(I);
            end;
          NET := 0;
          I := 0;
          while I <= NOut - 1 do
            begin
              Network.NWBuf[I] := Exp(Network.Neurons[NTotal - NOut + I] - MX);
              NET := NET + Network.NWBuf[I];
              Inc(I);
            end;
          V := APVDotProduct(@Network.DError[0], NTotal - NOut, NTotal - 1,
            @Network.NWBuf[0], 0, NOut - 1);
          I := 0;
          while I <= NOut - 1 do
            begin
              FOwn := Network.NWBuf[I];
              DEOwn := Network.DError[NTotal - NOut + I];
              Network.NWBuf[NOut + I] := (-V + DEOwn * FOwn + DEOwn * (NET - FOwn)) *
                FOwn / AP_Sqr(NET);
              Inc(I);
            end;
          I := 0;
          while I <= NOut - 1 do
            begin
              Network.DError[NTotal - NOut + I] := Network.NWBuf[NOut + I];
              Inc(I);
            end;
        end;
    end
  else
    begin

      //
      // Un-standardisation
      //
      I := 0;
      while I <= NOut - 1 do
        begin
          Network.DError[NTotal - NOut + I] := Network.DError[NTotal - NOut + I] *
            Network.ColumnSigmas[NIn + I];
          Inc(I);
        end;
    end;

  //
  // Backpropagation
  //
  I := NTotal - 1;
  while I >= 0 do
    begin

      //
      // Extract info
      //
      Offs := IStart + I * NFieldWidth;
      if Network.StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function
          //
          dEdF := Network.DError[I];
          DFDNET := Network.DFDNET[I];
          DError[Network.StructInfo[Offs + 2]] :=
            DError[Network.StructInfo[Offs + 2]] + dEdF * DFDNET;
        end;
      if Network.StructInfo[Offs + 0] = 0 then
        begin

          //
          // Adaptive summator
          //
          N1 := Network.StructInfo[Offs + 2];
          N2 := N1 + Network.StructInfo[Offs + 1] - 1;
          W1 := Network.StructInfo[Offs + 3];
          W2 := W1 + Network.StructInfo[Offs + 1] - 1;
          dEdF := Network.DError[I];
          DFDNET := 1.0;
          V := dEdF * DFDNET;
          APVMove(@Grad[0], W1, W2, @Neurons[0], N1, N2, V);
          APVAdd(@DError[0], N1, N2, @Weights[0], W1, W2, V);
        end;
      if Network.StructInfo[Offs + 0] < 0 then
        begin
          BFlag := False;
          if (Network.StructInfo[Offs + 0] = -2) or
            (Network.StructInfo[Offs + 0] = -3) or
            (Network.StructInfo[Offs + 0] = -4) then
            begin

              //
              // Special neuron type, no back-propagation required
              //
              BFlag := True;
            end;
          Assert(BFlag, 'MLPInternalCalculateGradient: unknown neuron type!');
        end;
      Dec(I);
    end;
end;

(* ************************************************************************
  Internal subroutine, chunked gradient
  ************************************************************************ *)
procedure MLPChunkedGradient(var Network: TMultiLayerPerceptron;
  const XY: TLearnFloat2DArray; CStart: TLearnInteger; CSize: TLearnInteger;
  var E: TLearnFloat; var Grad: TLearnFloatArray; NaturalErrorFunc: Boolean);
var
  I       : TLearnInteger;
  J       : TLearnInteger;
  K       : TLearnInteger;
  KL      : TLearnInteger;
  N1      : TLearnInteger;
  N2      : TLearnInteger;
  W1      : TLearnInteger;
  W2      : TLearnInteger;
  C1      : TLearnInteger;
  C2      : TLearnInteger;
  NTotal  : TLearnInteger;
  NIn     : TLearnInteger;
  NOut    : TLearnInteger;
  Offs    : TLearnInteger;
  F       : TLearnFloat;
  DF      : TLearnFloat;
  D2F     : TLearnFloat;
  V       : TLearnFloat;
  S       : TLearnFloat;
  FOwn    : TLearnFloat;
  DEOwn   : TLearnFloat;
  NET     : TLearnFloat;
  LnNET   : TLearnFloat;
  MX      : TLearnFloat;
  BFlag   : Boolean;
  IStart  : TLearnInteger;
  INeurons: TLearnInteger;
  IDFDNET : TLearnInteger;
  IDError : TLearnInteger;
  IZeros  : TLearnInteger;
begin

  //
  // Read network geometry, prepare data
  //
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];
  C1 := CStart;
  C2 := CStart + CSize - 1;
  INeurons := 0;
  IDFDNET := NTotal;
  IDError := 2 * NTotal;
  IZeros := 3 * NTotal;
  J := 0;
  while J <= CSize - 1 do
    begin
      Network.Chunks[IZeros, J] := 0;
      Inc(J);
    end;

  //
  // Forward pass:
  // 1. Load inputs from XY to Chunks[0:NIn-1,0:CSize-1]
  // 2. Forward pass
  //
  I := 0;
  while I <= NIn - 1 do
    begin
      J := 0;
      while J <= CSize - 1 do
        begin
          if AP_FP_Neq(Network.ColumnSigmas[I], 0) then
            begin
              Network.Chunks[I, J] := (XY[C1 + J, I] - Network.ColumnMeans[I]) /
                Network.ColumnSigmas[I];
            end
          else
            begin
              Network.Chunks[I, J] := XY[C1 + J, I] - Network.ColumnMeans[I];
            end;
          Inc(J);
        end;
      Inc(I);
    end;
  I := 0;
  while I <= NTotal - 1 do
    begin
      Offs := IStart + I * NFieldWidth;
      if Network.StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function:
          // * calculate F vector, F(i) = F(NET(i))
          //
          N1 := Network.StructInfo[Offs + 2];
          APVMove(@Network.Chunks[I][0], 0, CSize - 1, @Network.Chunks[N1][0], 0,
            CSize - 1);
          J := 0;
          while J <= CSize - 1 do
            begin
              MLPActivationFunction(Network.Chunks[I, J],
                Network.StructInfo[Offs + 0], F, DF, D2F);
              Network.Chunks[I, J] := F;
              Network.Chunks[IDFDNET + I, J] := DF;
              Inc(J);
            end;
        end;
      if Network.StructInfo[Offs + 0] = 0 then
        begin

          //
          // Adaptive summator:
          // * calculate NET vector, NET(i) = SUM(W(j,i)*Neurons(j),j=N1..N2)
          //
          N1 := Network.StructInfo[Offs + 2];
          N2 := N1 + Network.StructInfo[Offs + 1] - 1;
          W1 := Network.StructInfo[Offs + 3];
          W2 := W1 + Network.StructInfo[Offs + 1] - 1;
          APVMove(@Network.Chunks[I][0], 0, CSize - 1, @Network.Chunks[IZeros][0],
            0, CSize - 1);
          J := N1;
          while J <= N2 do
            begin
              V := Network.Weights[W1 + J - N1];
              APVAdd(@Network.Chunks[I][0], 0, CSize - 1, @Network.Chunks[J][0], 0,
                CSize - 1, V);
              Inc(J);
            end;
        end;
      if Network.StructInfo[Offs + 0] < 0 then
        begin
          BFlag := False;
          if Network.StructInfo[Offs + 0] = -2 then
            begin

              //
              // input neuron, left unchanged
              //
              BFlag := True;
            end;
          if Network.StructInfo[Offs + 0] = -3 then
            begin

              //
              // "-1" neuron
              //
              K := 0;
              while K <= CSize - 1 do
                begin
                  Network.Chunks[I, K] := -1;
                  Inc(K);
                end;
              BFlag := True;
            end;
          if Network.StructInfo[Offs + 0] = -4 then
            begin

              //
              // "0" neuron
              //
              K := 0;
              while K <= CSize - 1 do
                begin
                  Network.Chunks[I, K] := 0;
                  Inc(K);
                end;
              BFlag := True;
            end;
          Assert(BFlag,
            'MLPChunkedGradient: internal error - unknown neuron type!');
        end;
      Inc(I);
    end;

  //
  // Post-processing, error, dError/dOut
  //
  I := 0;
  while I <= NTotal - 1 do
    begin
      APVMove(@Network.Chunks[IDError + I][0], 0, CSize - 1,
        @Network.Chunks[IZeros][0], 0, CSize - 1);
      Inc(I);
    end;
  Assert((Network.StructInfo[6] = 0) or (Network.StructInfo[6] = 1),
    'MLPChunkedGradient: unknown normalization type!');
  if Network.StructInfo[6] = 1 then
    begin

      //
      // Softmax output, classification network.
      //
      // For each K = 0..CSize-1 do:
      // 1. place exp(outputs[k]) to NWBuf[0:NOut-1]
      // 2. place sum(exp(..)) to NET
      // 3. calculate dError/dOut and place it to the second block of Chunks
      //
      K := 0;
      while K <= CSize - 1 do
        begin

          //
          // Normalize
          //
          MX := Network.Chunks[NTotal - NOut, K];
          I := 1;
          while I <= NOut - 1 do
            begin
              MX := Max(MX, Network.Chunks[NTotal - NOut + I, K]);
              Inc(I);
            end;
          NET := 0;
          I := 0;
          while I <= NOut - 1 do
            begin
              Network.NWBuf[I] := Exp(Network.Chunks[NTotal - NOut + I, K] - MX);
              NET := NET + Network.NWBuf[I];
              Inc(I);
            end;

          //
          // Calculate error function and dError/dOut
          //
          if NaturalErrorFunc then
            begin

              //
              // Natural error func.
              //
              //
              S := 1;
              LnNET := Ln(NET);
              KL := Round(XY[CStart + K, NIn]);
              I := 0;
              while I <= NOut - 1 do
                begin
                  if I = KL then
                    begin
                      V := 1;
                    end
                  else
                    begin
                      V := 0;
                    end;
                  Network.Chunks[IDError + NTotal - NOut + I, K] := S * Network.NWBuf[I]
                    / NET - V;
                  E := E + SafeCrossEntropy(V, Network.NWBuf[I] / NET);
                  Inc(I);
                end;
            end
          else
            begin

              //
              // Least squares error func
              // Error, dError/dOut(normalized)
              //
              KL := Round(XY[CStart + K, NIn]);
              I := 0;
              while I <= NOut - 1 do
                begin
                  if I = KL then
                    begin
                      V := Network.NWBuf[I] / NET - 1;
                    end
                  else
                    begin
                      V := Network.NWBuf[I] / NET;
                    end;
                  Network.NWBuf[NOut + I] := V;
                  E := E + AP_Sqr(V) / 2;
                  Inc(I);
                end;

              //
              // From dError/dOut(normalized) to dError/dOut(non-normalized)
              //
              V := APVDotProduct(@Network.NWBuf[0], NOut, 2 * NOut - 1,
                @Network.NWBuf[0], 0, NOut - 1);
              I := 0;
              while I <= NOut - 1 do
                begin
                  FOwn := Network.NWBuf[I];
                  DEOwn := Network.NWBuf[NOut + I];
                  Network.Chunks[IDError + NTotal - NOut + I, K] :=
                    (-V + DEOwn * FOwn + DEOwn * (NET - FOwn)) * FOwn / AP_Sqr(NET);
                  Inc(I);
                end;
            end;
          Inc(K);
        end;
    end
  else
    begin

      //
      // Normal output, regression network
      //
      // For each K = 0..CSize-1 do:
      // 1. calculate dError/dOut and place it to the second block of Chunks
      //
      I := 0;
      while I <= NOut - 1 do
        begin
          J := 0;
          while J <= CSize - 1 do
            begin
              V := Network.Chunks[NTotal - NOut + I, J] * Network.ColumnSigmas
                [NIn + I] + Network.ColumnMeans[NIn + I] - XY[CStart + J, NIn + I];
              Network.Chunks[IDError + NTotal - NOut + I, J] :=
                V * Network.ColumnSigmas[NIn + I];
              E := E + AP_Sqr(V) / 2;
              Inc(J);
            end;
          Inc(I);
        end;
    end;

  //
  // Backpropagation
  //
  I := NTotal - 1;
  while I >= 0 do
    begin

      //
      // Extract info
      //
      Offs := IStart + I * NFieldWidth;
      if Network.StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function
          //
          N1 := Network.StructInfo[Offs + 2];
          K := 0;
          while K <= CSize - 1 do
            begin
              Network.Chunks[IDError + I, K] := Network.Chunks[IDError + I, K] *
                Network.Chunks[IDFDNET + I, K];
              Inc(K);
            end;
          APVAdd(@Network.Chunks[IDError + N1][0], 0, CSize - 1,
            @Network.Chunks[IDError + I][0], 0, CSize - 1);
        end;
      if Network.StructInfo[Offs + 0] = 0 then
        begin

          //
          // "Normal" activation function
          //
          N1 := Network.StructInfo[Offs + 2];
          N2 := N1 + Network.StructInfo[Offs + 1] - 1;
          W1 := Network.StructInfo[Offs + 3];
          W2 := W1 + Network.StructInfo[Offs + 1] - 1;
          J := W1;
          while J <= W2 do
            begin
              V := APVDotProduct(@Network.Chunks[N1 + J - W1][0], 0, CSize - 1,
                @Network.Chunks[IDError + I][0], 0, CSize - 1);
              Grad[J] := Grad[J] + V;
              Inc(J);
            end;
          J := N1;
          while J <= N2 do
            begin
              V := Network.Weights[W1 + J - N1];
              APVAdd(@Network.Chunks[IDError + J][0], 0, CSize - 1,
                @Network.Chunks[IDError + I][0], 0, CSize - 1, V);
              Inc(J);
            end;
        end;
      if Network.StructInfo[Offs + 0] < 0 then
        begin
          BFlag := False;
          if (Network.StructInfo[Offs + 0] = -2) or
            (Network.StructInfo[Offs + 0] = -3) or
            (Network.StructInfo[Offs + 0] = -4) then
            begin

              //
              // Special neuron type, no back-propagation required
              //
              BFlag := True;
            end;
          Assert(BFlag, 'MLPInternalCalculateGradient: unknown neuron type!');
        end;
      Dec(I);
    end;
end;

(* ************************************************************************
  Returns T*Ln(T/Z), guarded against overflow/underflow.
  Internal subroutine.
  ************************************************************************ *)
function SafeCrossEntropy(T: TLearnFloat; Z: TLearnFloat): TLearnFloat;
var
  R: TLearnFloat;
begin
  if AP_FP_Eq(T, 0) then
    begin
      Result := 0;
    end
  else
    begin
      if AP_FP_Greater(AbsReal(Z), 1) then
        begin

          //
          // Shouldn't be the case with softmax,
          // but we just want to be sure.
          //
          if AP_FP_Eq(T / Z, 0) then
            begin
              R := MinRealNumber;
            end
          else
            begin
              R := T / Z;
            end;
        end
      else
        begin

          //
          // Normal case
          //
          if AP_FP_Eq(Z, 0) or AP_FP_Greater_Eq(AbsReal(T),
            MaxRealNumber * AbsReal(Z)) then
            begin
              R := MaxRealNumber;
            end
          else
            begin
              R := T / Z;
            end;
        end;
      Result := T * Ln(R);
    end;
end;
