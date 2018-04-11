{ ****************************************************************************** }
{ * machine Learn base type support, by QQ 600585@qq.com                       * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
type
  TLearnBooleanArray   = packed array of Boolean;
  PLearnBooleanArray   = ^TLearnBooleanArray;
  TLearnBoolean2DArray = packed array of TLearnBooleanArray;

  PLearnComplex = ^TLearnComplex;

  TLearnComplex = packed record
    X, Y: TLearnFloat;
  end;

  TLearnComplexArray   = packed array of TLearnComplex;
  PLearnComplexArray   = ^TLearnComplexArray;
  TLearnComplex2DArray = packed array of TLearnComplexArray;

  TLearnCommState = packed record
    Stage: TLearnInteger;
    IA: TLearnIntegerArray;
    BA: TLearnBooleanArray;
    RA: TLearnFloatArray;
    CA: TLearnComplexArray;
  end;

function DynamicArrayCopy(const A: TLearnIntegerArray): TLearnIntegerArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLearnFloatArray): TLearnFloatArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLearnComplexArray): TLearnComplexArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLearnBooleanArray): TLearnBooleanArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function DynamicArrayCopy(const A: TLearnInteger2DArray): TLearnInteger2DArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLearnFloat2DArray): TLearnFloat2DArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLearnComplex2DArray): TLearnComplex2DArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DynamicArrayCopy(const A: TLearnBoolean2DArray): TLearnBoolean2DArray; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AbsComplex(const Z: TLearnComplex): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Conj(const Z: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CSqr(const Z: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function C_Complex(const X: TLearnFloat): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Opposite(const Z: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Add(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Mul(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_AddR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_MulR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Sub(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_SubR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_RSub(const R: TLearnFloat; const Z1: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Div(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_DivR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_RDiv(const R: TLearnFloat; const Z2: TLearnComplex): TLearnComplex; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_Equal(const Z1: TLearnComplex; const Z2: TLearnComplex): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_NotEqual(const Z1: TLearnComplex; const Z2: TLearnComplex): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_EqualR(const Z1: TLearnComplex; const R: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function C_NotEqualR(const Z1: TLearnComplex; const R: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure TagSort(var A: TLearnFloatArray; const N: TLearnInteger; var P1: TLearnIntegerArray; var P2: TLearnIntegerArray); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagSortFastI(var A: TLearnFloatArray; var B: TLearnIntegerArray; N: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagSortFastR(var A: TLearnFloatArray; var B: TLearnFloatArray; N: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagSortFast(var A: TLearnFloatArray; const N: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagHeapPushI(var A: TLearnFloatArray; var B: TLearnIntegerArray; var N: TLearnInteger; const VA: TLearnFloat; const VB: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagHeapReplaceTopI(var A: TLearnFloatArray; var B: TLearnIntegerArray; const N: TLearnInteger; const VA: TLearnFloat; const VB: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure TagHeapPopI(var A: TLearnFloatArray; var B: TLearnIntegerArray; var N: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


/// //////////////////////////////////////////////////////////////////////
// Functions/procedures
/// //////////////////////////////////////////////////////////////////////
function AbsReal(X: TLearnFloat): TLearnFloat;
begin
  // Result := Abs(X);
  if X >= 0 then
      AbsReal := X
  else
      AbsReal := -X;
end;

function AbsInt(I: TLearnInteger): TLearnInteger;
begin
  // Result := Abs(I);
  if I >= 0 then
      AbsInt := I
  else
      AbsInt := -I;
end;

function RandomReal(): TLearnFloat;
begin
  RandomReal := Random;
end;

function RandomInteger(I: TLearnInteger): TLearnInteger;
begin
  RandomInteger := Random(I);
end;

function Sign(X: TLearnFloat): TLearnInteger;
begin
  if X > 0 then
      Sign := 1
  else if X < 0 then
      Sign := -1
  else
      Sign := 0;
end;

function AP_Sqr(X: TLearnFloat): TLearnFloat;
begin
  AP_Sqr := X * X;
end;

/// //////////////////////////////////////////////////////////////////////
// dynamical arrays copying
/// //////////////////////////////////////////////////////////////////////
function DynamicArrayCopy(const A: TLearnIntegerArray): TLearnIntegerArray; overload;
var
  I: TLearnInteger;
  R: TLearnIntegerArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
      R[I] := A[I];
  DynamicArrayCopy := R;
end;

function DynamicArrayCopy(const A: TLearnFloatArray): TLearnFloatArray; overload;
var
  I: TLearnInteger;
  R: TLearnFloatArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
      R[I] := A[I];
  DynamicArrayCopy := R;
end;

function DynamicArrayCopy(const A: TLearnComplexArray): TLearnComplexArray; overload;
var
  I: TLearnInteger;
  R: TLearnComplexArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
      R[I] := A[I];
  DynamicArrayCopy := R;
end;

function DynamicArrayCopy(const A: TLearnBooleanArray): TLearnBooleanArray; overload;
var
  I: TLearnInteger;
  R: TLearnBooleanArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
      R[I] := A[I];
  DynamicArrayCopy := R;
end;

function DynamicArrayCopy(const A: TLearnInteger2DArray): TLearnInteger2DArray; overload;
var
  I, J: TLearnInteger;
  R   : TLearnInteger2DArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
    begin
      SetLength(R[I], high(A[I]) + 1);
      for J := low(A[I]) to high(A[I]) do
          R[I, J] := A[I, J];
    end;
  DynamicArrayCopy := R;
end;

function DynamicArrayCopy(const A: TLearnFloat2DArray): TLearnFloat2DArray; overload;
var
  I, J: TLearnInteger;
  R   : TLearnFloat2DArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
    begin
      SetLength(R[I], high(A[I]) + 1);
      for J := low(A[I]) to high(A[I]) do
          R[I, J] := A[I, J];
    end;
  DynamicArrayCopy := R;
end;

function DynamicArrayCopy(const A: TLearnComplex2DArray): TLearnComplex2DArray; overload;
var
  I, J: TLearnInteger;
  R   : TLearnComplex2DArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
    begin
      SetLength(R[I], high(A[I]) + 1);
      for J := low(A[I]) to high(A[I]) do
          R[I, J] := A[I, J];
    end;
  DynamicArrayCopy := R;
end;

function DynamicArrayCopy(const A: TLearnBoolean2DArray): TLearnBoolean2DArray; overload;
var
  I, J: TLearnInteger;
  R   : TLearnBoolean2DArray;
begin
  SetLength(R, high(A) + 1);
  for I := low(A) to high(A) do
    begin
      SetLength(R[I], high(A[I]) + 1);
      for J := low(A[I]) to high(A[I]) do
          R[I, J] := A[I, J];
    end;
  DynamicArrayCopy := R;
end;

/// //////////////////////////////////////////////////////////////////////
// TLearnComplex numbers
/// //////////////////////////////////////////////////////////////////////
function AbsComplex(const Z: TLearnComplex): TLearnFloat;
var
  W   : TLearnFloat;
  XABS: TLearnFloat;
  YABS: TLearnFloat;
  V   : TLearnFloat;
begin
  XABS := AbsReal(Z.X);
  YABS := AbsReal(Z.Y);
  W := Max(XABS, YABS);
  V := Min(XABS, YABS);
  if V = 0 then
    begin
      AbsComplex := W;
    end
  else
    begin
      AbsComplex := W * SQRT(1 + Sqr(V / W));
    end;
end;

function Conj(const Z: TLearnComplex): TLearnComplex;
begin
  Conj.X := Z.X;
  Conj.Y := -Z.Y;
end;

function CSqr(const Z: TLearnComplex): TLearnComplex;
begin
  CSqr.X := Sqr(Z.X) - Sqr(Z.Y);
  CSqr.Y := 2 * Z.X * Z.Y;
end;

function C_Complex(const X: TLearnFloat): TLearnComplex;
begin
  C_Complex.X := X;
  C_Complex.Y := 0;
end;

function C_Opposite(const Z: TLearnComplex): TLearnComplex;
begin
  C_Opposite.X := -Z.X;
  C_Opposite.Y := -Z.Y;
end;

function C_Add(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex;
begin
  C_Add.X := Z1.X + Z2.X;
  C_Add.Y := Z1.Y + Z2.Y;
end;

function C_Mul(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex;
begin
  C_Mul.X := Z1.X * Z2.X - Z1.Y * Z2.Y;
  C_Mul.Y := Z1.X * Z2.Y + Z1.Y * Z2.X;
end;

function C_AddR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex;
begin
  C_AddR.X := Z1.X + R;
  C_AddR.Y := Z1.Y;
end;

function C_MulR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex;
begin
  C_MulR.X := Z1.X * R;
  C_MulR.Y := Z1.Y * R;
end;

function C_Sub(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex;
begin
  C_Sub.X := Z1.X - Z2.X;
  C_Sub.Y := Z1.Y - Z2.Y;
end;

function C_SubR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex;
begin
  C_SubR.X := Z1.X - R;
  C_SubR.Y := Z1.Y;
end;

function C_RSub(const R: TLearnFloat; const Z1: TLearnComplex): TLearnComplex;
begin
  C_RSub.X := R - Z1.X;
  C_RSub.Y := -Z1.Y;
end;

function C_Div(const Z1: TLearnComplex; const Z2: TLearnComplex): TLearnComplex;
var
  A: TLearnFloat;
  B: TLearnFloat;
  C: TLearnFloat;
  D: TLearnFloat;
  E: TLearnFloat;
  F: TLearnFloat;
begin
  A := Z1.X;
  B := Z1.Y;
  C := Z2.X;
  D := Z2.Y;
  if AbsReal(D) < AbsReal(C) then
    begin
      E := D / C;
      F := C + D * E;
      C_Div.X := (A + B * E) / F;
      C_Div.Y := (B - A * E) / F;
    end
  else
    begin
      E := C / D;
      F := D + C * E;
      C_Div.X := (B + A * E) / F;
      C_Div.Y := (-A + B * E) / F;
    end;
end;

function C_DivR(const Z1: TLearnComplex; const R: TLearnFloat): TLearnComplex;
begin
  C_DivR.X := Z1.X / R;
  C_DivR.Y := Z1.Y / R;
end;

function C_RDiv(const R: TLearnFloat; const Z2: TLearnComplex): TLearnComplex;
var
  A: TLearnFloat;
  C: TLearnFloat;
  D: TLearnFloat;
  E: TLearnFloat;
  F: TLearnFloat;
begin
  A := R;
  C := Z2.X;
  D := Z2.Y;
  if AbsReal(D) < AbsReal(C) then
    begin
      E := D / C;
      F := C + D * E;
      C_RDiv.X := A / F;
      C_RDiv.Y := -A * E / F;
    end
  else
    begin
      E := C / D;
      F := D + C * E;
      C_RDiv.X := A * E / F;
      C_RDiv.Y := -A / F;
    end;
end;

function C_Equal(const Z1: TLearnComplex; const Z2: TLearnComplex): Boolean;
begin
  C_Equal := (Z1.X = Z2.X) and (Z1.Y = Z2.Y);
end;

function C_NotEqual(const Z1: TLearnComplex; const Z2: TLearnComplex): Boolean;
begin
  C_NotEqual := (Z1.X <> Z2.X) or (Z1.Y <> Z2.Y);
end;

function C_EqualR(const Z1: TLearnComplex; const R: TLearnFloat): Boolean;
begin
  C_EqualR := (Z1.X = R) and (Z1.Y = 0);
end;

function C_NotEqualR(const Z1: TLearnComplex; const R: TLearnFloat): Boolean;
begin
  C_NotEqualR := (Z1.X <> R) or (Z1.Y <> 0);
end;

/// //////////////////////////////////////////////////////////////////////
// AP BLAS generic interface
/// //////////////////////////////////////////////////////////////////////
function APVDotProduct(V1: PLearnFloat; I11, I12: TLearnInteger; V2: PLearnFloat;
  I21, I22: TLearnInteger): TLearnFloat;
var
  I, C: TLearnInteger;
  R   : TLearnFloat;
begin
  Assert(I12 - I11 = I22 - I21, 'APVDotProduct: arrays of different size!');
  Inc(V1, I11);
  Inc(V2, I21);

  //
  // Generic pascal code
  //
  C := I12 - I11;
  R := 0;
  for I := 0 to C do
    begin
      R := R + V1^ * V2^;
      Inc(V1);
      Inc(V2);
    end;
  APVDotProduct := R;
end;

procedure APVMove(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat;
  I21, I22: TLearnInteger); overload;
var
  I, C: TLearnInteger;
begin
  Assert(I12 - I11 = I22 - I21, 'APVMove: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  //
  // Generic pascal code
  //
  C := I12 - I11;
  for I := 0 to C do
    begin
      VDst^ := VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVMove(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat;
  I21, I22: TLearnInteger; S: TLearnFloat); overload;
var
  I, C: TLearnInteger;
begin
  Assert(I12 - I11 = I22 - I21, 'APVMove: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  //
  // Generic pascal code
  //
  C := I12 - I11;
  for I := 0 to C do
    begin
      VDst^ := S * VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVMoveNeg(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat;
  I21, I22: TLearnInteger);
var
  I, C: TLearnInteger;
begin
  Assert(I12 - I11 = I22 - I21, 'APVMoveNeg: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  //
  // Generic pascal code
  //
  C := I12 - I11;
  for I := 0 to C do
    begin
      VDst^ := -VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVAdd(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat;
  I21, I22: TLearnInteger); overload;
var
  I, C: TLearnInteger;
begin
  Assert(I12 - I11 = I22 - I21, 'APVAdd: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  //
  // Generic pascal code
  //
  C := I12 - I11;
  for I := 0 to C do
    begin
      VDst^ := VDst^ + VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVAdd(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat;
  I21, I22: TLearnInteger; S: TLearnFloat); overload;
var
  I, C: TLearnInteger;
begin
  Assert(I12 - I11 = I22 - I21, 'APVAdd: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  //
  // Generic pascal code
  //
  C := I12 - I11;
  for I := 0 to C do
    begin
      VDst^ := VDst^ + S * VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVSub(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat;
  I21, I22: TLearnInteger); overload;
var
  I, C: TLearnInteger;
begin
  Assert(I12 - I11 = I22 - I21, 'APVSub arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  //
  // Generic pascal code
  //
  C := I12 - I11;
  for I := 0 to C do
    begin
      VDst^ := VDst^ - VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVSub(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat;
  I21, I22: TLearnInteger; S: TLearnFloat); overload;
begin
  Assert(I12 - I11 = I22 - I21, 'APVSub: arrays of different size!');
  APVAdd(VDst, I11, I12, VSrc, I21, I22, -S);
end;

procedure APVMul(VOp: PLearnFloat; I1, I2: TLearnInteger; S: TLearnFloat);
var
  I, C: TLearnInteger;
begin
  Inc(VOp, I1);
  C := I2 - I1;
  for I := 0 to C do
    begin
      VOp^ := S * VOp^;
      Inc(VOp);
    end;
end;

procedure APVFillValue(VOp: PLearnFloat; I1, I2: TLearnInteger; S: TLearnFloat);
var
  I, C: TLearnInteger;
begin
  Inc(VOp, I1);
  C := I2 - I1;
  for I := 0 to C do
    begin
      VOp^ := S;
      Inc(VOp);
    end;
end;

/// //////////////////////////////////////////////////////////////////////
// IEEE-compliant functions
/// //////////////////////////////////////////////////////////////////////

function AP_Float(X: TLearnFloat): TLearnFloat;
begin
  AP_Float := X;
end;

function AP_FP_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean;
begin
  AP_FP_Eq := X = Y;
end;

function AP_FP_NEq(X: TLearnFloat; Y: TLearnFloat): Boolean;
begin
  AP_FP_NEq := X <> Y;
end;

function AP_FP_Less(X: TLearnFloat; Y: TLearnFloat): Boolean;
begin
  AP_FP_Less := X < Y;
end;

function AP_FP_Less_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean;
begin
  AP_FP_Less_Eq := X <= Y;
end;

function AP_FP_Greater(X: TLearnFloat; Y: TLearnFloat): Boolean;
begin
  AP_FP_Greater := X > Y;
end;

function AP_FP_Greater_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean;
begin
  AP_FP_Greater_Eq := X >= Y;
end;

procedure TagSort(var A: TLearnFloatArray; const N: TLearnInteger; var P1: TLearnIntegerArray; var P2: TLearnIntegerArray);
var
  I : TLearnInteger;
  PV: TLearnIntegerArray;
  VP: TLearnIntegerArray;
  LV: TLearnInteger;
  LP: TLearnInteger;
  RV: TLearnInteger;
  RP: TLearnInteger;
begin

  //
  // Special cases
  //
  if N <= 0 then
    begin
      Exit;
    end;
  if N = 1 then
    begin
      SetLength(P1, 0 + 1);
      SetLength(P2, 0 + 1);
      P1[0] := 0;
      P2[0] := 0;
      Exit;
    end;

  //
  // General case, N>1: prepare permutations table P1
  //
  SetLength(P1, N (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= N - 1 do
    begin
      P1[I] := I;
      Inc(I);
    end;

  //
  // General case, N>1: sort, update P1
  //
  TagSortFastI(A, P1, N);

  //
  // General case, N>1: fill permutations table P2
  //
  // To fill P2 we maintain two arrays:
  // * PV, Position(Value). PV[i] contains position of I-th key at the moment
  // * VP, Value(Position). VP[i] contains key which has position I at the moment
  //
  // At each step we making permutation of two items:
  // Left, which is given by position/value pair LP/LV
  // and Right, which is given by RP/RV
  // and updating PV[] and VP[] correspondingly.
  //
  SetLength(PV, N (* - 1 + 1 // optimized compler *) );
  SetLength(VP, N (* - 1 + 1 // optimized compler *) );
  SetLength(P2, N (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= N - 1 do
    begin
      PV[I] := I;
      VP[I] := I;
      Inc(I);
    end;
  I := 0;
  while I <= N - 1 do
    begin

      //
      // calculate LP, LV, RP, RV
      //
      LP := I;
      LV := VP[LP];
      RV := P1[I];
      RP := PV[RV];

      //
      // Fill P2
      //
      P2[I] := RP;

      //
      // update PV and VP
      //
      VP[LP] := RV;
      VP[RP] := LV;
      PV[LV] := RP;
      PV[RV] := LP;
      Inc(I);
    end;
end;

procedure TagSortFastI(var A: TLearnFloatArray; var B: TLearnIntegerArray; N: TLearnInteger);
var
  I   : TLearnInteger;
  K   : TLearnInteger;
  T   : TLearnInteger;
  Tmp : TLearnFloat;
  TmpI: TLearnInteger;
begin

  //
  // Special cases
  //
  if N <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  I := 2;
  repeat
    T := I;
    while T <> 1 do
      begin
        K := T div 2;
        if AP_FP_Greater_Eq(A[K - 1], A[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            Tmp := A[K - 1];
            A[K - 1] := A[T - 1];
            A[T - 1] := Tmp;
            TmpI := B[K - 1];
            B[K - 1] := B[T - 1];
            B[T - 1] := TmpI;
            T := K;
          end;
      end;
    I := I + 1;
  until not(I <= N);
  I := N - 1;
  repeat
    Tmp := A[I];
    A[I] := A[0];
    A[0] := Tmp;
    TmpI := B[I];
    B[I] := B[0];
    B[0] := TmpI;
    T := 1;
    while T <> 0 do
      begin
        K := 2 * T;
        if K > I then
          begin
            T := 0;
          end
        else
          begin
            if K < I then
              begin
                if AP_FP_Greater(A[K], A[K - 1]) then
                  begin
                    K := K + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(A[T - 1], A[K - 1]) then
              begin
                T := 0;
              end
            else
              begin
                Tmp := A[K - 1];
                A[K - 1] := A[T - 1];
                A[T - 1] := Tmp;
                TmpI := B[K - 1];
                B[K - 1] := B[T - 1];
                B[T - 1] := TmpI;
                T := K;
              end;
          end;
      end;
    I := I - 1;
  until not(I >= 1);
end;

procedure TagSortFastR(var A: TLearnFloatArray; var B: TLearnFloatArray;
  N: TLearnInteger);
var
  I   : TLearnInteger;
  K   : TLearnInteger;
  T   : TLearnInteger;
  Tmp : TLearnFloat;
  TmpR: TLearnFloat;
begin

  //
  // Special cases
  //
  if N <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  I := 2;
  repeat
    T := I;
    while T <> 1 do
      begin
        K := T div 2;
        if AP_FP_Greater_Eq(A[K - 1], A[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            Tmp := A[K - 1];
            A[K - 1] := A[T - 1];
            A[T - 1] := Tmp;
            TmpR := B[K - 1];
            B[K - 1] := B[T - 1];
            B[T - 1] := TmpR;
            T := K;
          end;
      end;
    I := I + 1;
  until not(I <= N);
  I := N - 1;
  repeat
    Tmp := A[I];
    A[I] := A[0];
    A[0] := Tmp;
    TmpR := B[I];
    B[I] := B[0];
    B[0] := TmpR;
    T := 1;
    while T <> 0 do
      begin
        K := 2 * T;
        if K > I then
          begin
            T := 0;
          end
        else
          begin
            if K < I then
              begin
                if AP_FP_Greater(A[K], A[K - 1]) then
                  begin
                    K := K + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(A[T - 1], A[K - 1]) then
              begin
                T := 0;
              end
            else
              begin
                Tmp := A[K - 1];
                A[K - 1] := A[T - 1];
                A[T - 1] := Tmp;
                TmpR := B[K - 1];
                B[K - 1] := B[T - 1];
                B[T - 1] := TmpR;
                T := K;
              end;
          end;
      end;
    I := I - 1;
  until not(I >= 1);
end;

procedure TagSortFast(var A: TLearnFloatArray; const N: TLearnInteger);
var
  I  : TLearnInteger;
  K  : TLearnInteger;
  T  : TLearnInteger;
  Tmp: TLearnFloat;
begin

  //
  // Special cases
  //
  if N <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  I := 2;
  repeat
    T := I;
    while T <> 1 do
      begin
        K := T div 2;
        if AP_FP_Greater_Eq(A[K - 1], A[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            Tmp := A[K - 1];
            A[K - 1] := A[T - 1];
            A[T - 1] := Tmp;
            T := K;
          end;
      end;
    I := I + 1;
  until not(I <= N);
  I := N - 1;
  repeat
    Tmp := A[I];
    A[I] := A[0];
    A[0] := Tmp;
    T := 1;
    while T <> 0 do
      begin
        K := 2 * T;
        if K > I then
          begin
            T := 0;
          end
        else
          begin
            if K < I then
              begin
                if AP_FP_Greater(A[K], A[K - 1]) then
                  begin
                    K := K + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(A[T - 1], A[K - 1]) then
              begin
                T := 0;
              end
            else
              begin
                Tmp := A[K - 1];
                A[K - 1] := A[T - 1];
                A[T - 1] := Tmp;
                T := K;
              end;
          end;
      end;
    I := I - 1;
  until not(I >= 1);
end;

(* ************************************************************************
  Heap operations: adds element to the heap

  PARAMETERS:
  A       -   heap itself, must be at least array[0..N]
  B       -   packed array of integer tags, which are updated according to
  permutations in the heap
  N       -   size of the heap (without new element).
  updated on output
  VA      -   value of the element being added
  VB      -   value of the tag
  ************************************************************************ *)
procedure TagHeapPushI(var A: TLearnFloatArray; var B: TLearnIntegerArray;
  var N: TLearnInteger; const VA: TLearnFloat; const VB: TLearnInteger);
var
  J: TLearnInteger;
  K: TLearnInteger;
  V: TLearnFloat;
begin
  if N < 0 then
    begin
      Exit;
    end;

  //
  // N=0 is a special case
  //
  if N = 0 then
    begin
      A[0] := VA;
      B[0] := VB;
      N := N + 1;
      Exit;
    end;

  //
  // add current point to the heap
  // (add to the bottom, then move up)
  //
  // we don't write point to the heap
  // until its final position is determined
  // (it allow us to reduce number of array access operations)
  //
  J := N;
  N := N + 1;
  while J > 0 do
    begin
      K := (J - 1) div 2;
      V := A[K];
      if AP_FP_Less(V, VA) then
        begin

          //
          // swap with higher element
          //
          A[J] := V;
          B[J] := B[K];
          J := K;
        end
      else
        begin

          //
          // element in its place. terminate.
          //
          Break;
        end;
    end;
  A[J] := VA;
  B[J] := VB;
end;

(* ************************************************************************
  Heap operations: replaces top element with new element
  (which is moved down)

  PARAMETERS:
  A       -   heap itself, must be at least array[0..N-1]
  B       -   packed array of integer tags, which are updated according to
  permutations in the heap
  N       -   size of the heap
  VA      -   value of the element which replaces top element
  VB      -   value of the tag
  ************************************************************************ *)
procedure TagHeapReplaceTopI(var A: TLearnFloatArray; var B: TLearnIntegerArray;
  const N: TLearnInteger; const VA: TLearnFloat; const VB: TLearnInteger);
var
  J : TLearnInteger;
  K1: TLearnInteger;
  K2: TLearnInteger;
  V : TLearnFloat;
  V1: TLearnFloat;
  V2: TLearnFloat;
begin
  if N < 1 then
    begin
      Exit;
    end;

  //
  // N=1 is a special case
  //
  if N = 1 then
    begin
      A[0] := VA;
      B[0] := VB;
      Exit;
    end;

  //
  // move down through heap:
  // * J  -   current element
  // * K1 -   first child (always exists)
  // * K2 -   second child (may not exists)
  //
  // we don't write point to the heap
  // until its final position is determined
  // (it allow us to reduce number of array access operations)
  //
  J := 0;
  K1 := 1;
  K2 := 2;
  while K1 < N do
    begin
      if K2 >= N then
        begin

          //
          // only one child.
          //
          // swap and terminate (because this child
          // have no siblings due to heap structure)
          //
          V := A[K1];
          if AP_FP_Greater(V, VA) then
            begin
              A[J] := V;
              B[J] := B[K1];
              J := K1;
            end;
          Break;
        end
      else
        begin

          //
          // two childs
          //
          V1 := A[K1];
          V2 := A[K2];
          if AP_FP_Greater(V1, V2) then
            begin
              if AP_FP_Less(VA, V1) then
                begin
                  A[J] := V1;
                  B[J] := B[K1];
                  J := K1;
                end
              else
                begin
                  Break;
                end;
            end
          else
            begin
              if AP_FP_Less(VA, V2) then
                begin
                  A[J] := V2;
                  B[J] := B[K2];
                  J := K2;
                end
              else
                begin
                  Break;
                end;
            end;
          K1 := 2 * J + 1;
          K2 := 2 * J + 2;
        end;
    end;
  A[J] := VA;
  B[J] := VB;
end;

(* ************************************************************************
  Heap operations: pops top element from the heap

  PARAMETERS:
  A       -   heap itself, must be at least array[0..N-1]
  B       -   packed array of integer tags, which are updated according to
  permutations in the heap
  N       -   size of the heap, N>=1

  On output top element is moved to A[N-1], B[N-1], heap is reordered, N is
  decreased by 1.
  ************************************************************************ *)
procedure TagHeapPopI(var A: TLearnFloatArray; var B: TLearnIntegerArray; var N: TLearnInteger);
var
  VA: TLearnFloat;
  VB: TLearnInteger;
begin
  if N < 1 then
    begin
      Exit;
    end;

  //
  // N=1 is a special case
  //
  if N = 1 then
    begin
      N := 0;
      Exit;
    end;

  //
  // swap top element and last element,
  // then reorder heap
  //
  VA := A[N - 1];
  VB := B[N - 1];
  A[N - 1] := A[0];
  B[N - 1] := B[0];
  N := N - 1;
  TagHeapReplaceTopI(A, B, N, VA, VB);
end;
