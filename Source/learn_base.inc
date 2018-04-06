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
  PLearnComplex = ^TLearnComplex;

  TLearnComplex = packed record
    X, Y: TLearnFloat;
  end;

  TLearnComplexArray   = array of TLearnComplex;
  PLearnComplexArray   = ^TLearnComplexArray;
  TLearnComplex2DArray = array of TLearnComplexArray;

  TLearnCommState = packed record
    Stage: Integer;
    IA: TLearnIntegerArray;
    BA: TLearnBooleanArray;
    RA: TLearnFloatArray;
    CA: TLearnComplexArray;
  end;

const
  MachineEpsilon = 5E-16;
  MaxRealNumber  = 1E300;
  MinRealNumber  = 1E-300;

  /// //////////////////////////////////////////////////////////////////////
  // Functions/procedures
  /// //////////////////////////////////////////////////////////////////////
function AbsReal(X: TLearnFloat): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AbsInt(I: TLearnInteger): TLearnInteger; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomReal(): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomInteger(I: TLearnInteger): TLearnInteger; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Sign(X: TLearnFloat): TLearnInteger; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_Sqr(X: TLearnFloat): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

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

/// //////////////////////////////////////////////////////////////////////
// AP BLAS generic interface

/// //////////////////////////////////////////////////////////////////////
// procedure UseAPBLAS(Flag: Boolean);
function APVDotProduct(V1: PLearnFloat; I11, I12: TLearnInteger; V2: PLearnFloat; I21, I22: TLearnInteger): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMove(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMove(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger; S: TLearnFloat); overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMoveNeg(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVAdd(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVAdd(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger; S: TLearnFloat); overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVSub(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger); overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVSub(VDst: PLearnFloat; I11, I12: TLearnInteger; VSrc: PLearnFloat; I21, I22: TLearnInteger; S: TLearnFloat); overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVMul(VOp: PLearnFloat; I1, I2: TLearnInteger; S: TLearnFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure APVFillValue(VOp: PLearnFloat; I1, I2: TLearnInteger; S: TLearnFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

/// //////////////////////////////////////////////////////////////////////
// IEEE-compliant functions, placed at the end, under 'non-optimization'
// compiler switch
/// //////////////////////////////////////////////////////////////////////
function AP_Float(X: Extended): TLearnFloat; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_Float(X: Single): TLearnFloat; overload; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_NEq(X: TLearnFloat; Y: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Less(X: TLearnFloat; Y: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Less_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Greater(X: TLearnFloat; Y: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AP_FP_Greater_Eq(X: TLearnFloat; Y: TLearnFloat): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


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
  I, C: Integer;
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
  I, C: Integer;
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
  I, C: Integer;
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
  I, C: Integer;
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
  I, C: Integer;
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
  I, C: Integer;
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
  I, C: Integer;
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
  I, C: Integer;
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
  I, C: Integer;
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

function AP_Float(X: Extended): TLearnFloat;
begin
  AP_Float := X;
end;

function AP_Float(X: Single): TLearnFloat;
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
