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
  TLearnCommState = packed record
    Stage: TLInt;
    IA: TLIVec;
    BA: TLBVec;
    RA: TLVec;
    CA: TLComplexVec;
  end;

  /// //////////////////////////////////////////////////////////////////////
  // Functions/procedures
  /// //////////////////////////////////////////////////////////////////////
function AbsReal(X: TLFloat): TLFloat;
begin
  // Result := Abs(X);
  if X >= 0 then
      AbsReal := X
  else
      AbsReal := -X;
end;

function AbsInt(I: TLInt): TLInt;
begin
  // Result := Abs(I);
  if I >= 0 then
      AbsInt := I
  else
      AbsInt := -I;
end;

function RandomReal(): TLFloat;
begin
  RandomReal := Random;
end;

function RandomInteger(I: TLInt): TLInt;
begin
  RandomInteger := Random(I);
end;

function Sign(X: TLFloat): TLInt;
begin
  if X > 0 then
      Sign := 1
  else if X < 0 then
      Sign := -1
  else
      Sign := 0;
end;

function AP_Sqr(X: TLFloat): TLFloat;
begin
  AP_Sqr := X * X;
end;

/// //////////////////////////////////////////////////////////////////////
// dynamical arrays copying
/// //////////////////////////////////////////////////////////////////////
function DynamicArrayCopy(const A: TLIVec): TLIVec; overload;
var
  I: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
      Result[I] := A[I];
end;

function DynamicArrayCopy(const A: TLVec): TLVec; overload;
var
  I: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
      Result[I] := A[I];
end;

function DynamicArrayCopy(const A: TLComplexVec): TLComplexVec; overload;
var
  I: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
      Result[I] := A[I];
end;

function DynamicArrayCopy(const A: TLBVec): TLBVec; overload;
var
  I: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
      Result[I] := A[I];
end;

function DynamicArrayCopy(const A: TLIMatrix): TLIMatrix; overload;
var
  I, J: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
    begin
      SetLength(Result[I], Length(A[I]));
      for J := low(A[I]) to high(A[I]) do
          Result[I, J] := A[I, J];
    end;
end;

function DynamicArrayCopy(const A: TLMatrix): TLMatrix; overload;
var
  I, J: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
    begin
      SetLength(Result[I], Length(A[I]));
      for J := low(A[I]) to high(A[I]) do
          Result[I, J] := A[I, J];
    end;
end;

function DynamicArrayCopy(const A: TLComplexMatrix): TLComplexMatrix; overload;
var
  I, J: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
    begin
      SetLength(Result[I], Length(A[I]));
      for J := low(A[I]) to high(A[I]) do
          Result[I, J] := A[I, J];
    end;
end;

function DynamicArrayCopy(const A: TLBMatrix): TLBMatrix; overload;
var
  I, J: TLInt;
begin
  SetLength(Result, Length(A));
  for I := low(A) to high(A) do
    begin
      SetLength(Result[I], Length(A[I]));
      for J := low(A[I]) to high(A[I]) do
          Result[I, J] := A[I, J];
    end;
end;

/// //////////////////////////////////////////////////////////////////////
// TLComplex numbers
/// //////////////////////////////////////////////////////////////////////
function AbsComplex(const Z: TLComplex): TLFloat;
var
  W: TLFloat;
  XABS: TLFloat;
  YABS: TLFloat;
  V: TLFloat;
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

function Conj(const Z: TLComplex): TLComplex;
begin
  Conj.X := Z.X;
  Conj.Y := -Z.Y;
end;

function CSqr(const Z: TLComplex): TLComplex;
begin
  CSqr.X := Sqr(Z.X) - Sqr(Z.Y);
  CSqr.Y := 2 * Z.X * Z.Y;
end;

function C_Complex(const X: TLFloat): TLComplex;
begin
  C_Complex.X := X;
  C_Complex.Y := 0;
end;

function C_Opposite(const Z: TLComplex): TLComplex;
begin
  C_Opposite.X := -Z.X;
  C_Opposite.Y := -Z.Y;
end;

function C_Add(const Z1: TLComplex; const Z2: TLComplex): TLComplex;
begin
  C_Add.X := Z1.X + Z2.X;
  C_Add.Y := Z1.Y + Z2.Y;
end;

function C_Mul(const Z1: TLComplex; const Z2: TLComplex): TLComplex;
begin
  C_Mul.X := Z1.X * Z2.X - Z1.Y * Z2.Y;
  C_Mul.Y := Z1.X * Z2.Y + Z1.Y * Z2.X;
end;

function C_AddR(const Z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_AddR.X := Z1.X + R;
  C_AddR.Y := Z1.Y;
end;

function C_MulR(const Z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_MulR.X := Z1.X * R;
  C_MulR.Y := Z1.Y * R;
end;

function C_Sub(const Z1: TLComplex; const Z2: TLComplex): TLComplex;
begin
  C_Sub.X := Z1.X - Z2.X;
  C_Sub.Y := Z1.Y - Z2.Y;
end;

function C_SubR(const Z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_SubR.X := Z1.X - R;
  C_SubR.Y := Z1.Y;
end;

function C_RSub(const R: TLFloat; const Z1: TLComplex): TLComplex;
begin
  C_RSub.X := R - Z1.X;
  C_RSub.Y := -Z1.Y;
end;

function C_Div(const Z1: TLComplex; const Z2: TLComplex): TLComplex;
var
  A: TLFloat;
  B: TLFloat;
  C: TLFloat;
  D: TLFloat;
  E: TLFloat;
  F: TLFloat;
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

function C_DivR(const Z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_DivR.X := Z1.X / R;
  C_DivR.Y := Z1.Y / R;
end;

function C_RDiv(const R: TLFloat; const Z2: TLComplex): TLComplex;
var
  A: TLFloat;
  C: TLFloat;
  D: TLFloat;
  E: TLFloat;
  F: TLFloat;
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

function C_Equal(const Z1: TLComplex; const Z2: TLComplex): Boolean;
begin
  C_Equal := (Z1.X = Z2.X) and (Z1.Y = Z2.Y);
end;

function C_NotEqual(const Z1: TLComplex; const Z2: TLComplex): Boolean;
begin
  C_NotEqual := (Z1.X <> Z2.X) or (Z1.Y <> Z2.Y);
end;

function C_EqualR(const Z1: TLComplex; const R: TLFloat): Boolean;
begin
  C_EqualR := (Z1.X = R) and (Z1.Y = 0);
end;

function C_NotEqualR(const Z1: TLComplex; const R: TLFloat): Boolean;
begin
  C_NotEqualR := (Z1.X <> R) or (Z1.Y <> 0);
end;

function APVDotProduct(V1: PLFloat; I11, I12: TLInt; V2: PLFloat; I21, I22: TLInt): TLFloat;
var
  I, C: TLInt;
  R: TLFloat;
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

procedure APVMove(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat;
  I21, I22: TLInt); overload;
var
  I, C: TLInt;
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

procedure APVMove(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat;
  I21, I22: TLInt; s: TLFloat); overload;
var
  I, C: TLInt;
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
      VDst^ := s * VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVMoveNeg(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat;
  I21, I22: TLInt);
var
  I, C: TLInt;
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

procedure APVAdd(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat;
  I21, I22: TLInt); overload;
var
  I, C: TLInt;
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

procedure APVAdd(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat;
  I21, I22: TLInt; s: TLFloat); overload;
var
  I, C: TLInt;
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
      VDst^ := VDst^ + s * VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure APVSub(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat;
  I21, I22: TLInt); overload;
var
  I, C: TLInt;
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

procedure APVSub(VDst: PLFloat; I11, I12: TLInt; VSrc: PLFloat;
  I21, I22: TLInt; s: TLFloat); overload;
begin
  Assert(I12 - I11 = I22 - I21, 'APVSub: arrays of different size!');
  APVAdd(VDst, I11, I12, VSrc, I21, I22, -s);
end;

procedure APVMul(VOp: PLFloat; I1, I2: TLInt; s: TLFloat);
var
  I, C: TLInt;
begin
  Inc(VOp, I1);
  C := I2 - I1;
  for I := 0 to C do
    begin
      VOp^ := s * VOp^;
      Inc(VOp);
    end;
end;

procedure APVFillValue(VOp: PLFloat; I1, I2: TLInt; s: TLFloat);
var
  I, C: TLInt;
begin
  Inc(VOp, I1);
  C := I2 - I1;
  for I := 0 to C do
    begin
      VOp^ := s;
      Inc(VOp);
    end;
end;

/// //////////////////////////////////////////////////////////////////////
// IEEE-compliant functions
/// //////////////////////////////////////////////////////////////////////

function AP_Float(X: TLFloat): TLFloat;
begin
  AP_Float := X;
end;

function AP_FP_Eq(X: TLFloat; Y: TLFloat): Boolean;
begin
  AP_FP_Eq := X = Y;
end;

function AP_FP_NEq(X: TLFloat; Y: TLFloat): Boolean;
begin
  AP_FP_NEq := X <> Y;
end;

function AP_FP_Less(X: TLFloat; Y: TLFloat): Boolean;
begin
  AP_FP_Less := X < Y;
end;

function AP_FP_Less_Eq(X: TLFloat; Y: TLFloat): Boolean;
begin
  AP_FP_Less_Eq := X <= Y;
end;

function AP_FP_Greater(X: TLFloat; Y: TLFloat): Boolean;
begin
  AP_FP_Greater := X > Y;
end;

function AP_FP_Greater_Eq(X: TLFloat; Y: TLFloat): Boolean;
begin
  AP_FP_Greater_Eq := X >= Y;
end;

procedure TagSort(var A: TLVec; const N: TLInt; var P1: TLIVec; var P2: TLIVec);
var
  I: TLInt;
  PV: TLIVec;
  VP: TLIVec;
  LV: TLInt;
  LP: TLInt;
  RV: TLInt;
  RP: TLInt;
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

procedure TagSortFastI(var A: TLVec; var B: TLIVec; N: TLInt);
var
  I: TLInt;
  K: TLInt;
  T: TLInt;
  Tmp: TLFloat;
  TmpI: TLInt;
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

procedure TagSortFastR(var A: TLVec; var B: TLVec;
  N: TLInt);
var
  I: TLInt;
  K: TLInt;
  T: TLInt;
  Tmp: TLFloat;
  TmpR: TLFloat;
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

procedure TagSortFast(var A: TLVec; const N: TLInt);
var
  I: TLInt;
  K: TLInt;
  T: TLInt;
  Tmp: TLFloat;
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
procedure TagHeapPushI(var A: TLVec; var B: TLIVec;
  var N: TLInt; const VA: TLFloat; const VB: TLInt);
var
  J: TLInt;
  K: TLInt;
  V: TLFloat;
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
procedure TagHeapReplaceTopI(var A: TLVec; var B: TLIVec;
  const N: TLInt; const VA: TLFloat; const VB: TLInt);
var
  J: TLInt;
  K1: TLInt;
  K2: TLInt;
  V: TLFloat;
  V1: TLFloat;
  V2: TLFloat;
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
procedure TagHeapPopI(var A: TLVec; var B: TLIVec; var N: TLInt);
var
  VA: TLFloat;
  VB: TLInt;
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
