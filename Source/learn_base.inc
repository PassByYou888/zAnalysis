{ ****************************************************************************** }
{ * machine Learn base type support, by QQ 600585@qq.com                       * }
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
  TLearnCommState = packed record
    Stage: TLInt;
    IA: TLIVec;
    BA: TLBVec;
    RA: TLVec;
    ca: TLComplexVec;
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

function AbsInt(i: TLInt): TLInt;
begin
  // Result := Abs(I);
  if i >= 0 then
      AbsInt := i
  else
      AbsInt := -i;
end;

function RandomReal(): TLFloat;
begin
  RandomReal := Random;
end;

function RandomInteger(i: TLInt): TLInt;
begin
  RandomInteger := Random(i);
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
  i: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
      Result[i] := A[i];
end;

function DynamicArrayCopy(const A: TLVec): TLVec; overload;
var
  i: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
      Result[i] := A[i];
end;

function DynamicArrayCopy(const A: TLComplexVec): TLComplexVec; overload;
var
  i: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
      Result[i] := A[i];
end;

function DynamicArrayCopy(const A: TLBVec): TLBVec; overload;
var
  i: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
      Result[i] := A[i];
end;

function DynamicArrayCopy(const A: TLIMatrix): TLIMatrix; overload;
var
  i, J: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
    begin
      SetLength(Result[i], length(A[i]));
      for J := low(A[i]) to high(A[i]) do
          Result[i, J] := A[i, J];
    end;
end;

function DynamicArrayCopy(const A: TLMatrix): TLMatrix; overload;
var
  i, J: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
    begin
      SetLength(Result[i], length(A[i]));
      for J := low(A[i]) to high(A[i]) do
          Result[i, J] := A[i, J];
    end;
end;

function DynamicArrayCopy(const A: TLComplexMatrix): TLComplexMatrix; overload;
var
  i, J: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
    begin
      SetLength(Result[i], length(A[i]));
      for J := low(A[i]) to high(A[i]) do
          Result[i, J] := A[i, J];
    end;
end;

function DynamicArrayCopy(const A: TLBMatrix): TLBMatrix; overload;
var
  i, J: TLInt;
begin
  SetLength(Result, length(A));
  for i := low(A) to high(A) do
    begin
      SetLength(Result[i], length(A[i]));
      for J := low(A[i]) to high(A[i]) do
          Result[i, J] := A[i, J];
    end;
end;

/// //////////////////////////////////////////////////////////////////////
// TLComplex numbers
/// //////////////////////////////////////////////////////////////////////
function AbsComplex(const Z: TLComplex): TLFloat;
var
  w: TLFloat;
  XABS: TLFloat;
  YABS: TLFloat;
  v: TLFloat;
begin
  XABS := AbsReal(Z.X);
  YABS := AbsReal(Z.Y);
  w := Max(XABS, YABS);
  v := Min(XABS, YABS);
  if v = 0 then
    begin
      AbsComplex := w;
    end
  else
    begin
      AbsComplex := w * Sqrt(1 + Sqr(v / w));
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

function C_Add(const z1: TLComplex; const z2: TLComplex): TLComplex;
begin
  C_Add.X := z1.X + z2.X;
  C_Add.Y := z1.Y + z2.Y;
end;

function C_Mul(const z1: TLComplex; const z2: TLComplex): TLComplex;
begin
  C_Mul.X := z1.X * z2.X - z1.Y * z2.Y;
  C_Mul.Y := z1.X * z2.Y + z1.Y * z2.X;
end;

function C_AddR(const z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_AddR.X := z1.X + R;
  C_AddR.Y := z1.Y;
end;

function C_MulR(const z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_MulR.X := z1.X * R;
  C_MulR.Y := z1.Y * R;
end;

function C_Sub(const z1: TLComplex; const z2: TLComplex): TLComplex;
begin
  C_Sub.X := z1.X - z2.X;
  C_Sub.Y := z1.Y - z2.Y;
end;

function C_SubR(const z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_SubR.X := z1.X - R;
  C_SubR.Y := z1.Y;
end;

function C_RSub(const R: TLFloat; const z1: TLComplex): TLComplex;
begin
  C_RSub.X := R - z1.X;
  C_RSub.Y := -z1.Y;
end;

function C_Div(const z1: TLComplex; const z2: TLComplex): TLComplex;
var
  A: TLFloat;
  b: TLFloat;
  C: TLFloat;
  d: TLFloat;
  E: TLFloat;
  F: TLFloat;
begin
  A := z1.X;
  b := z1.Y;
  C := z2.X;
  d := z2.Y;
  if AbsReal(d) < AbsReal(C) then
    begin
      E := d / C;
      F := C + d * E;
      C_Div.X := (A + b * E) / F;
      C_Div.Y := (b - A * E) / F;
    end
  else
    begin
      E := C / d;
      F := d + C * E;
      C_Div.X := (b + A * E) / F;
      C_Div.Y := (-A + b * E) / F;
    end;
end;

function C_DivR(const z1: TLComplex; const R: TLFloat): TLComplex;
begin
  C_DivR.X := z1.X / R;
  C_DivR.Y := z1.Y / R;
end;

function C_RDiv(const R: TLFloat; const z2: TLComplex): TLComplex;
var
  A: TLFloat;
  C: TLFloat;
  d: TLFloat;
  E: TLFloat;
  F: TLFloat;
begin
  A := R;
  C := z2.X;
  d := z2.Y;
  if AbsReal(d) < AbsReal(C) then
    begin
      E := d / C;
      F := C + d * E;
      C_RDiv.X := A / F;
      C_RDiv.Y := -A * E / F;
    end
  else
    begin
      E := C / d;
      F := d + C * E;
      C_RDiv.X := A * E / F;
      C_RDiv.Y := -A / F;
    end;
end;

function C_Equal(const z1: TLComplex; const z2: TLComplex): Boolean;
begin
  C_Equal := (z1.X = z2.X) and (z1.Y = z2.Y);
end;

function C_NotEqual(const z1: TLComplex; const z2: TLComplex): Boolean;
begin
  C_NotEqual := (z1.X <> z2.X) or (z1.Y <> z2.Y);
end;

function C_EqualR(const z1: TLComplex; const R: TLFloat): Boolean;
begin
  C_EqualR := (z1.X = R) and (z1.Y = 0);
end;

function C_NotEqualR(const z1: TLComplex; const R: TLFloat): Boolean;
begin
  C_NotEqualR := (z1.X <> R) or (z1.Y <> 0);
end;

function APVDotProduct(v1: PLFloat; i11, i12: TLInt; v2: PLFloat; i21, i22: TLInt): TLFloat;
var
  i, C: TLInt;
  R: TLFloat;
begin
  Assert(i12 - i11 = i22 - i21, 'APVDotProduct: arrays of different size!');
  Inc(v1, i11);
  Inc(v2, i21);

  //
  // Generic pascal code
  //
  C := i12 - i11;
  R := 0;
  for i := 0 to C do
    begin
      R := R + v1^ * v2^;
      Inc(v1);
      Inc(v2);
    end;
  APVDotProduct := R;
end;

procedure APVMove(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt); overload;
var
  i, C: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVMove: arrays of different size!');
  Inc(VDst, i11);
  Inc(vSrc, i21);

  //
  // Generic pascal code
  //
  C := i12 - i11;
  for i := 0 to C do
    begin
      VDst^ := vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure APVMove(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt; s: TLFloat); overload;
var
  i, C: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVMove: arrays of different size!');
  Inc(VDst, i11);
  Inc(vSrc, i21);

  //
  // Generic pascal code
  //
  C := i12 - i11;
  for i := 0 to C do
    begin
      VDst^ := s * vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure APVMoveNeg(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt);
var
  i, C: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVMoveNeg: arrays of different size!');
  Inc(VDst, i11);
  Inc(vSrc, i21);

  //
  // Generic pascal code
  //
  C := i12 - i11;
  for i := 0 to C do
    begin
      VDst^ := -vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure APVAdd(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt); overload;
var
  i, C: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVAdd: arrays of different size!');
  Inc(VDst, i11);
  Inc(vSrc, i21);

  //
  // Generic pascal code
  //
  C := i12 - i11;
  for i := 0 to C do
    begin
      VDst^ := VDst^ + vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure APVAdd(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt; s: TLFloat); overload;
var
  i, C: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVAdd: arrays of different size!');
  Inc(VDst, i11);
  Inc(vSrc, i21);

  //
  // Generic pascal code
  //
  C := i12 - i11;
  for i := 0 to C do
    begin
      VDst^ := VDst^ + s * vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure APVSub(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt); overload;
var
  i, C: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVSub arrays of different size!');
  Inc(VDst, i11);
  Inc(vSrc, i21);

  //
  // Generic pascal code
  //
  C := i12 - i11;
  for i := 0 to C do
    begin
      VDst^ := VDst^ - vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure APVSub(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt; s: TLFloat); overload;
begin
  Assert(i12 - i11 = i22 - i21, 'APVSub: arrays of different size!');
  APVAdd(VDst, i11, i12, vSrc, i21, i22, -s);
end;

procedure APVMul(VOp: PLFloat; i1, i2: TLInt; s: TLFloat);
var
  i, C: TLInt;
begin
  Inc(VOp, i1);
  C := i2 - i1;
  for i := 0 to C do
    begin
      VOp^ := s * VOp^;
      Inc(VOp);
    end;
end;

procedure APVFillValue(VOp: PLFloat; i1, i2: TLInt; s: TLFloat);
var
  i, C: TLInt;
begin
  Inc(VOp, i1);
  C := i2 - i1;
  for i := 0 to C do
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

procedure TagSort(var A: TLVec; const n: TLInt; var p1: TLIVec; var p2: TLIVec);
var
  i: TLInt;
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
  if n <= 0 then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      SetLength(p1, 0 + 1);
      SetLength(p2, 0 + 1);
      p1[0] := 0;
      p2[0] := 0;
      Exit;
    end;

  //
  // General case, N>1: prepare permutations table P1
  //
  SetLength(p1, n (* - 1 + 1 // optimized compler *) );
  i := 0;
  while i <= n - 1 do
    begin
      p1[i] := i;
      Inc(i);
    end;

  //
  // General case, N>1: sort, update P1
  //
  TagSortFastI(A, p1, n);

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
  SetLength(PV, n (* - 1 + 1 // optimized compler *) );
  SetLength(VP, n (* - 1 + 1 // optimized compler *) );
  SetLength(p2, n (* - 1 + 1 // optimized compler *) );
  i := 0;
  while i <= n - 1 do
    begin
      PV[i] := i;
      VP[i] := i;
      Inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin

      //
      // calculate LP, LV, RP, RV
      //
      LP := i;
      LV := VP[LP];
      RV := p1[i];
      RP := PV[RV];

      //
      // Fill P2
      //
      p2[i] := RP;

      //
      // update PV and VP
      //
      VP[LP] := RV;
      VP[RP] := LV;
      PV[LV] := RP;
      PV[RV] := LP;
      Inc(i);
    end;
end;

procedure TagSortFastI(var A: TLVec; var b: TLIVec; n: TLInt);
var
  i: TLInt;
  k: TLInt;
  T: TLInt;
  tmp: TLFloat;
  TmpI: TLInt;
begin

  //
  // Special cases
  //
  if n <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  i := 2;
  repeat
    T := i;
    while T <> 1 do
      begin
        k := T div 2;
        if AP_FP_Greater_Eq(A[k - 1], A[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            tmp := A[k - 1];
            A[k - 1] := A[T - 1];
            A[T - 1] := tmp;
            TmpI := b[k - 1];
            b[k - 1] := b[T - 1];
            b[T - 1] := TmpI;
            T := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := A[i];
    A[i] := A[0];
    A[0] := tmp;
    TmpI := b[i];
    b[i] := b[0];
    b[0] := TmpI;
    T := 1;
    while T <> 0 do
      begin
        k := 2 * T;
        if k > i then
          begin
            T := 0;
          end
        else
          begin
            if k < i then
              begin
                if AP_FP_Greater(A[k], A[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(A[T - 1], A[k - 1]) then
              begin
                T := 0;
              end
            else
              begin
                tmp := A[k - 1];
                A[k - 1] := A[T - 1];
                A[T - 1] := tmp;
                TmpI := b[k - 1];
                b[k - 1] := b[T - 1];
                b[T - 1] := TmpI;
                T := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

procedure TagSortFastR(var A: TLVec; var b: TLVec;
  n: TLInt);
var
  i: TLInt;
  k: TLInt;
  T: TLInt;
  tmp: TLFloat;
  TmpR: TLFloat;
begin

  //
  // Special cases
  //
  if n <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  i := 2;
  repeat
    T := i;
    while T <> 1 do
      begin
        k := T div 2;
        if AP_FP_Greater_Eq(A[k - 1], A[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            tmp := A[k - 1];
            A[k - 1] := A[T - 1];
            A[T - 1] := tmp;
            TmpR := b[k - 1];
            b[k - 1] := b[T - 1];
            b[T - 1] := TmpR;
            T := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := A[i];
    A[i] := A[0];
    A[0] := tmp;
    TmpR := b[i];
    b[i] := b[0];
    b[0] := TmpR;
    T := 1;
    while T <> 0 do
      begin
        k := 2 * T;
        if k > i then
          begin
            T := 0;
          end
        else
          begin
            if k < i then
              begin
                if AP_FP_Greater(A[k], A[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(A[T - 1], A[k - 1]) then
              begin
                T := 0;
              end
            else
              begin
                tmp := A[k - 1];
                A[k - 1] := A[T - 1];
                A[T - 1] := tmp;
                TmpR := b[k - 1];
                b[k - 1] := b[T - 1];
                b[T - 1] := TmpR;
                T := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

procedure TagSortFast(var A: TLVec; const n: TLInt);
var
  i: TLInt;
  k: TLInt;
  T: TLInt;
  tmp: TLFloat;
begin

  //
  // Special cases
  //
  if n <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  i := 2;
  repeat
    T := i;
    while T <> 1 do
      begin
        k := T div 2;
        if AP_FP_Greater_Eq(A[k - 1], A[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            tmp := A[k - 1];
            A[k - 1] := A[T - 1];
            A[T - 1] := tmp;
            T := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := A[i];
    A[i] := A[0];
    A[0] := tmp;
    T := 1;
    while T <> 0 do
      begin
        k := 2 * T;
        if k > i then
          begin
            T := 0;
          end
        else
          begin
            if k < i then
              begin
                if AP_FP_Greater(A[k], A[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(A[T - 1], A[k - 1]) then
              begin
                T := 0;
              end
            else
              begin
                tmp := A[k - 1];
                A[k - 1] := A[T - 1];
                A[T - 1] := tmp;
                T := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
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
procedure TagHeapPushI(var A: TLVec; var b: TLIVec;
  var n: TLInt; const VA: TLFloat; const VB: TLInt);
var
  J: TLInt;
  k: TLInt;
  v: TLFloat;
begin
  if n < 0 then
    begin
      Exit;
    end;

  //
  // N=0 is a special case
  //
  if n = 0 then
    begin
      A[0] := VA;
      b[0] := VB;
      n := n + 1;
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
  J := n;
  n := n + 1;
  while J > 0 do
    begin
      k := (J - 1) div 2;
      v := A[k];
      if AP_FP_Less(v, VA) then
        begin

          //
          // swap with higher element
          //
          A[J] := v;
          b[J] := b[k];
          J := k;
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
  b[J] := VB;
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
procedure TagHeapReplaceTopI(var A: TLVec; var b: TLIVec;
  const n: TLInt; const VA: TLFloat; const VB: TLInt);
var
  J: TLInt;
  k1: TLInt;
  k2: TLInt;
  v: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // N=1 is a special case
  //
  if n = 1 then
    begin
      A[0] := VA;
      b[0] := VB;
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
  k1 := 1;
  k2 := 2;
  while k1 < n do
    begin
      if k2 >= n then
        begin

          //
          // only one child.
          //
          // swap and terminate (because this child
          // have no siblings due to heap structure)
          //
          v := A[k1];
          if AP_FP_Greater(v, VA) then
            begin
              A[J] := v;
              b[J] := b[k1];
              J := k1;
            end;
          Break;
        end
      else
        begin

          //
          // two childs
          //
          v1 := A[k1];
          v2 := A[k2];
          if AP_FP_Greater(v1, v2) then
            begin
              if AP_FP_Less(VA, v1) then
                begin
                  A[J] := v1;
                  b[J] := b[k1];
                  J := k1;
                end
              else
                begin
                  Break;
                end;
            end
          else
            begin
              if AP_FP_Less(VA, v2) then
                begin
                  A[J] := v2;
                  b[J] := b[k2];
                  J := k2;
                end
              else
                begin
                  Break;
                end;
            end;
          k1 := 2 * J + 1;
          k2 := 2 * J + 2;
        end;
    end;
  A[J] := VA;
  b[J] := VB;
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
procedure TagHeapPopI(var A: TLVec; var b: TLIVec; var n: TLInt);
var
  VA: TLFloat;
  VB: TLInt;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // N=1 is a special case
  //
  if n = 1 then
    begin
      n := 0;
      Exit;
    end;

  //
  // swap top element and last element,
  // then reorder heap
  //
  VA := A[n - 1];
  VB := b[n - 1];
  A[n - 1] := A[0];
  b[n - 1] := b[0];
  n := n - 1;
  TagHeapReplaceTopI(A, b, n, VA, VB);
end; 
