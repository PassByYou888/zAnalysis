{ ****************************************************************************** }
{ * Real/complex QR, LQ, bi(tri)diagonal, Hessenberg decompositions support    * }
{ * by QQ 600585@qq.com                                                        * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
procedure SymmetricMatrixVectorMultiply(const A: TLMatrix; IsUpper: Boolean;
  i1: TLInt; i2: TLInt; const X: TLVec;
  alpha: TLFloat; var Y: TLVec);
var
  i  : TLInt;
  BA1: TLInt;
  BA2: TLInt;
  BY1: TLInt;
  BY2: TLInt;
  BX1: TLInt;
  BX2: TLInt;
  n  : TLInt;
  v  : TLFloat;
begin
  n := i2 - i1 + 1;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Let A = L + D + U, where
  // L is strictly lower triangular (main diagonal is zero)
  // D is diagonal
  // U is strictly upper triangular (main diagonal is zero)
  //
  // A*x = L*x + D*x + U*x
  //
  // Calculate D*x first
  //
  i := i1;
  while i <= i2 do
    begin
      Y[i - i1 + 1] := A[i, i] * X[i - i1 + 1];
      Inc(i);
    end;

  //
  // Add L*x + U*x
  //
  if IsUpper then
    begin
      i := i1;
      while i <= i2 - 1 do
        begin

          //
          // Add L*x to the result
          //
          v := X[i - i1 + 1];
          BY1 := i - i1 + 2;
          BY2 := n;
          BA1 := i + 1;
          BA2 := i2;
          APVAdd(@Y[0], BY1, BY2, @A[i][0], BA1, BA2, v);

          //
          // Add U*x to the result
          //
          BX1 := i - i1 + 2;
          BX2 := n;
          BA1 := i + 1;
          BA2 := i2;
          v := APVDotProduct(@X[0], BX1, BX2, @A[i][0], BA1, BA2);
          Y[i - i1 + 1] := Y[i - i1 + 1] + v;
          Inc(i);
        end;
    end
  else
    begin
      i := i1 + 1;
      while i <= i2 do
        begin

          //
          // Add L*x to the result
          //
          BX1 := 1;
          BX2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          v := APVDotProduct(@X[0], BX1, BX2, @A[i][0], BA1, BA2);
          Y[i - i1 + 1] := Y[i - i1 + 1] + v;

          //
          // Add U*x to the result
          //
          v := X[i - i1 + 1];
          BY1 := 1;
          BY2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          APVAdd(@Y[0], BY1, BY2, @A[i][0], BA1, BA2, v);
          Inc(i);
        end;
    end;
  APVMul(@Y[0], 1, n, alpha);
end;

procedure SymmetricRank2Update(var A: TLMatrix; IsUpper: Boolean;
  i1: TLInt; i2: TLInt; const X: TLVec;
  const Y: TLVec; var T: TLVec; alpha: TLFloat);
var
  i  : TLInt;
  TP1: TLInt;
  TP2: TLInt;
  v  : TLFloat;
begin
  if IsUpper then
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := i + 1 - i1;
          TP2 := i2 - i1 + 1;
          v := X[i + 1 - i1];
          APVMove(@T[0], TP1, TP2, @Y[0], TP1, TP2, v);
          v := Y[i + 1 - i1];
          APVAdd(@T[0], TP1, TP2, @X[0], TP1, TP2, v);
          APVMul(@T[0], TP1, TP2, alpha);
          APVAdd(@A[i][0], i, i2, @T[0], TP1, TP2);
          Inc(i);
        end;
    end
  else
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := 1;
          TP2 := i + 1 - i1;
          v := X[i + 1 - i1];
          APVMove(@T[0], TP1, TP2, @Y[0], TP1, TP2, v);
          v := Y[i + 1 - i1];
          APVAdd(@T[0], TP1, TP2, @X[0], TP1, TP2, v);
          APVMul(@T[0], TP1, TP2, alpha);
          APVAdd(@A[i][0], i1, i, @T[0], TP1, TP2);
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  Generation of an elementary TLComplex reflection transformation

  The subroutine generates elementary TLComplex reflection H of  order  N,  so
  that, for a given X, the following equality holds true:

  ( X(1) )   ( Beta )
  H' * (  ..  ) = (  0   ),   H'*H = I,   Beta is a real number
  ( X(n) )   (  0   )

  where

  ( V(1) )
  H = 1 - Tau * (  ..  ) * ( conj(V(1)), ..., conj(V(n)) )
  ( V(n) )

  where the first component of vector V equals 1.

  Input parameters:
  X   -   vector. Array with elements [1..N].
  N   -   reflection order.

  Output parameters:
  X   -   components from 2 to N are replaced by vector V.
  The first component is replaced with parameter Beta.
  Tau -   scalar value Tau.

  This subroutine is the modification of CLARFG subroutines  from the LAPACK
  library. It has similar functionality except for the fact that it  doesn’t
  handle errors when intermediate results cause an overflow.
  ************************************************************************ *)
procedure ComplexGenerateReflection(var X: TLComplexVec; n: TLInt;
  var TAU: TLComplex);
var
  J    : TLInt;
  alpha: TLComplex;
  ALPHI: TLFloat;
  ALPHR: TLFloat;
  beta : TLFloat;
  XNORM: TLFloat;
  mx   : TLFloat;
  T    : TLComplex;
  s    : TLFloat;
  v    : TLComplex;
  i_   : TLInt;
begin
  if n <= 0 then
    begin
      TAU := C_Complex(0);
      Exit;
    end;

  //
  // Scale if needed (to avoid overflow/underflow during intermediate
  // calculations).
  //
  mx := 0;
  J := 1;
  while J <= n do
    begin
      mx := Max(AbsComplex(X[J]), mx);
      Inc(J);
    end;
  s := 1;
  if AP_FP_NEq(mx, 0) then
    begin
      if AP_FP_Less(mx, 1) then
        begin
          s := Sqrt(MinRealNumber);
          v := C_Complex(1 / s);
          for i_ := 1 to n do
            begin
              X[i_] := C_Mul(v, X[i_]);
            end;
        end
      else
        begin
          s := Sqrt(MaxRealNumber);
          v := C_Complex(1 / s);
          for i_ := 1 to n do
            begin
              X[i_] := C_Mul(v, X[i_]);
            end;
        end;
    end;

  //
  // calculate
  //
  alpha := X[1];
  mx := 0;
  J := 2;
  while J <= n do
    begin
      mx := Max(AbsComplex(X[J]), mx);
      Inc(J);
    end;
  XNORM := 0;
  if AP_FP_NEq(mx, 0) then
    begin
      J := 2;
      while J <= n do
        begin
          T := C_DivR(X[J], mx);
          XNORM := XNORM + C_Mul(T, Conj(T)).X;
          Inc(J);
        end;
      XNORM := Sqrt(XNORM) * mx;
    end;
  ALPHR := alpha.X;
  ALPHI := alpha.Y;
  if AP_FP_Eq(XNORM, 0) and AP_FP_Eq(ALPHI, 0) then
    begin
      TAU := C_Complex(0);
      X[1] := C_MulR(X[1], s);
      Exit;
    end;
  mx := Max(AbsReal(ALPHR), AbsReal(ALPHI));
  mx := Max(mx, AbsReal(XNORM));
  beta := -mx * Sqrt(AP_Sqr(ALPHR / mx) + AP_Sqr(ALPHI / mx) +
    AP_Sqr(XNORM / mx));
  if AP_FP_Less(ALPHR, 0) then
    begin
      beta := -beta;
    end;
  TAU.X := (beta - ALPHR) / beta;
  TAU.Y := -ALPHI / beta;
  alpha := C_RDiv(1, C_SubR(alpha, beta));
  if n > 1 then
    begin
      for i_ := 2 to n do
        begin
          X[i_] := C_Mul(alpha, X[i_]);
        end;
    end;
  alpha := C_Complex(beta);
  X[1] := alpha;

  //
  // Scale back
  //
  X[1] := C_MulR(X[1], s);
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The  algorithm  pre-multiplies  the  matrix  by  an  elementary reflection
  transformation  which  is  given  by  column  V  and  scalar  Tau (see the
  description of the GenerateReflection). Not the whole matrix  but  only  a
  part of it is transformed (rows from M1 to M2, columns from N1 to N2). Only
  the elements of this submatrix are changed.

  Note: the matrix is multiplied by H, not by H'.   If  it  is  required  to
  multiply the matrix by H', it is necessary to pass Conj(Tau) instead of Tau.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining transformation.
  V       -   column defining transformation.
  Array whose index ranges within [1..M2-M1+1]
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose index goes from N1 to N2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ComplexApplyReflectionFromTheLeft(var C: TLComplexMatrix;
  TAU: TLComplex; const v: TLComplexVec; m1: TLInt; m2: TLInt;
  N1: TLInt; N2: TLInt; var Work: TLComplexVec);
var
  T : TLComplex;
  i : TLInt;
  VM: TLInt;
  i_: TLInt;
begin
  if C_EqualR(TAU, 0) or (N1 > N2) or (m1 > m2) then
    begin
      Exit;
    end;

  //
  // w := C^T * conj(v)
  //
  VM := m2 - m1 + 1;
  i := N1;
  while i <= N2 do
    begin
      Work[i] := C_Complex(0);
      Inc(i);
    end;
  i := m1;
  while i <= m2 do
    begin
      T := Conj(v[i + 1 - m1]);
      for i_ := N1 to N2 do
        begin
          Work[i_] := C_Add(Work[i_], C_Mul(T, C[i, i_]));
        end;
      Inc(i);
    end;

  //
  // C := C - tau * v * w^T
  //
  i := m1;
  while i <= m2 do
    begin
      T := C_Mul(v[i - m1 + 1], TAU);
      for i_ := N1 to N2 do
        begin
          C[i, i_] := C_Sub(C[i, i_], C_Mul(T, Work[i_]));
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The  algorithm  post-multiplies  the  matrix  by  an elementary reflection
  transformation  which  is  given  by  column  V  and  scalar  Tau (see the
  description  of  the  GenerateReflection). Not the whole matrix but only a
  part  of  it  is  transformed (rows from M1 to M2, columns from N1 to N2).
  Only the elements of this submatrix are changed.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining transformation.
  V       -   column defining transformation.
  Array whose index ranges within [1..N2-N1+1]
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose index goes from M1 to M2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ComplexApplyReflectionFromTheRight(var C: TLComplexMatrix;
  TAU: TLComplex; var v: TLComplexVec; m1: TLInt; m2: TLInt;
  N1: TLInt; N2: TLInt; var Work: TLComplexVec);
var
  T  : TLComplex;
  i  : TLInt;
  VM : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if C_EqualR(TAU, 0) or (N1 > N2) or (m1 > m2) then
    begin
      Exit;
    end;

  //
  // w := C * v
  //
  VM := N2 - N1 + 1;
  i := m1;
  while i <= m2 do
    begin
      i1_ := (1) - (N1);
      T := C_Complex(0.0);
      for i_ := N1 to N2 do
        begin
          T := C_Add(T, C_Mul(C[i, i_], v[i_ + i1_]));
        end;
      Work[i] := T;
      Inc(i);
    end;

  //
  // C := C - w * conj(v^T)
  //
  for i_ := 1 to VM do
    begin
      v[i_] := Conj(v[i_]);
    end;
  i := m1;
  while i <= m2 do
    begin
      T := C_Mul(Work[i], TAU);
      i1_ := (1) - (N1);
      for i_ := N1 to N2 do
        begin
          C[i, i_] := C_Sub(C[i, i_], C_Mul(T, v[i_ + i1_]));
        end;
      Inc(i);
    end;
  for i_ := 1 to VM do
    begin
      v[i_] := Conj(v[i_]);
    end;
end;

(* ************************************************************************
  Generation of an elementary reflection transformation

  The subroutine generates elementary reflection H of order N, so that, for
  a given X, the following equality holds true:

  ( X(1) )   ( Beta )
  H * (  ..  ) = (  0   )
  ( X(n) )   (  0   )

  where
  ( V(1) )
  H = 1 - Tau * (  ..  ) * ( V(1), ..., V(n) )
  ( V(n) )

  where the first component of vector V equals 1.

  Input parameters:
  X   -   vector. Array whose index ranges within [1..N].
  N   -   reflection order.

  Output parameters:
  X   -   components from 2 to N are replaced with vector V.
  The first component is replaced with parameter Beta.
  Tau -   scalar value Tau. If X is a null vector, Tau equals 0,
  otherwise 1 <= Tau <= 2.

  This subroutine is the modification of the DLARFG subroutines from
  the LAPACK library.

  MODIFICATIONS:
  24.12.2005 sign(Alpha) was replaced with an analogous to the Fortran SIGN code.
  ************************************************************************ *)
procedure GenerateReflection(var X: TLVec; n: TLInt;
  var TAU: TLFloat);
var
  J    : TLInt;
  alpha: TLFloat;
  XNORM: TLFloat;
  v    : TLFloat;
  beta : TLFloat;
  mx   : TLFloat;
  s    : TLFloat;
begin
  if n <= 1 then
    begin
      TAU := 0;
      Exit;
    end;

  //
  // Scale if needed (to avoid overflow/underflow during intermediate
  // calculations).
  //
  mx := 0;
  J := 1;
  while J <= n do
    begin
      mx := Max(AbsReal(X[J]), mx);
      Inc(J);
    end;
  s := 1;
  if AP_FP_NEq(mx, 0) then
    begin
      if AP_FP_Less_Eq(mx, MinRealNumber / MachineEpsilon) then
        begin
          s := MinRealNumber / MachineEpsilon;
          v := 1 / s;
          APVMul(@X[0], 1, n, v);
          mx := mx * v;
        end
      else
        begin
          if AP_FP_Greater_Eq(mx, MaxRealNumber * MachineEpsilon) then
            begin
              s := MaxRealNumber * MachineEpsilon;
              v := 1 / s;
              APVMul(@X[0], 1, n, v);
              mx := mx * v;
            end;
        end;
    end;

  //
  // XNORM = DNRM2( N-1, X, INCX )
  //
  alpha := X[1];
  XNORM := 0;
  if AP_FP_NEq(mx, 0) then
    begin
      J := 2;
      while J <= n do
        begin
          XNORM := XNORM + AP_Sqr(X[J] / mx);
          Inc(J);
        end;
      XNORM := Sqrt(XNORM) * mx;
    end;
  if AP_FP_Eq(XNORM, 0) then
    begin

      //
      // H  =  I
      //
      TAU := 0;
      X[1] := X[1] * s;
      Exit;
    end;

  //
  // general case
  //
  mx := Max(AbsReal(alpha), AbsReal(XNORM));
  beta := -mx * Sqrt(AP_Sqr(alpha / mx) + AP_Sqr(XNORM / mx));
  if AP_FP_Less(alpha, 0) then
    begin
      beta := -beta;
    end;
  TAU := (beta - alpha) / beta;
  v := 1 / (alpha - beta);
  APVMul(@X[0], 2, n, v);
  X[1] := beta;

  //
  // Scale back outputs
  //
  X[1] := X[1] * s;
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The algorithm pre-multiplies the matrix by an elementary reflection transformation
  which is given by column V and scalar Tau (see the description of the
  GenerateReflection procedure). Not the whole matrix but only a part of it
  is transformed (rows from M1 to M2, columns from N1 to N2). Only the elements
  of this submatrix are changed.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining the transformation.
  V       -   column defining the transformation.
  Array whose index ranges within [1..M2-M1+1].
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose indexes goes from N1 to N2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ApplyReflectionFromTheLeft(var C: TLMatrix; TAU: TLFloat;
  const v: TLVec; m1: TLInt; m2: TLInt;
  N1: TLInt; N2: TLInt; var Work: TLVec);
var
  T : TLFloat;
  i : TLInt;
  VM: TLInt;
begin
  if AP_FP_Eq(TAU, 0) or (N1 > N2) or (m1 > m2) then
    begin
      Exit;
    end;

  //
  // w := C' * v
  //
  VM := m2 - m1 + 1;
  i := N1;
  while i <= N2 do
    begin
      Work[i] := 0;
      Inc(i);
    end;
  i := m1;
  while i <= m2 do
    begin
      T := v[i + 1 - m1];
      APVAdd(@Work[0], N1, N2, @C[i][0], N1, N2, T);
      Inc(i);
    end;

  //
  // C := C - tau * v * w'
  //
  i := m1;
  while i <= m2 do
    begin
      T := v[i - m1 + 1] * TAU;
      APVSub(@C[i][0], N1, N2, @Work[0], N1, N2, T);
      Inc(i);
    end;
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The algorithm post-multiplies the matrix by an elementary reflection transformation
  which is given by column V and scalar Tau (see the description of the
  GenerateReflection procedure). Not the whole matrix but only a part of it
  is transformed (rows from M1 to M2, columns from N1 to N2). Only the
  elements of this submatrix are changed.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining the transformation.
  V       -   column defining the transformation.
  Array whose index ranges within [1..N2-N1+1].
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose indexes goes from M1 to M2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ApplyReflectionFromTheRight(var C: TLMatrix; TAU: TLFloat;
  const v: TLVec; m1: TLInt; m2: TLInt;
  N1: TLInt; N2: TLInt; var Work: TLVec);
var
  T : TLFloat;
  i : TLInt;
  VM: TLInt;
begin
  if AP_FP_Eq(TAU, 0) or (N1 > N2) or (m1 > m2) then
    begin
      Exit;
    end;
  VM := N2 - N1 + 1;
  i := m1;
  while i <= m2 do
    begin
      T := APVDotProduct(@C[i][0], N1, N2, @v[0], 1, VM);
      T := T * TAU;
      APVSub(@C[i][0], N1, N2, @v[0], 1, VM, T);
      Inc(i);
    end;
end;

procedure HermitianMatrixVectorMultiply(const A: TLComplexMatrix;
  IsUpper: Boolean; i1: TLInt; i2: TLInt;
  const X: TLComplexVec; alpha: TLComplex; var Y: TLComplexVec);
var
  i  : TLInt;
  BA1: TLInt;
  BA2: TLInt;
  BY1: TLInt;
  BY2: TLInt;
  BX1: TLInt;
  BX2: TLInt;
  n  : TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  n := i2 - i1 + 1;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Let A = L + D + U, where
  // L is strictly lower triangular (main diagonal is zero)
  // D is diagonal
  // U is strictly upper triangular (main diagonal is zero)
  //
  // A*x = L*x + D*x + U*x
  //
  // Calculate D*x first
  //
  i := i1;
  while i <= i2 do
    begin
      Y[i - i1 + 1] := C_Mul(A[i, i], X[i - i1 + 1]);
      Inc(i);
    end;

  //
  // Add L*x + U*x
  //
  if IsUpper then
    begin
      i := i1;
      while i <= i2 - 1 do
        begin

          //
          // Add L*x to the result
          //
          v := X[i - i1 + 1];
          BY1 := i - i1 + 2;
          BY2 := n;
          BA1 := i + 1;
          BA2 := i2;
          i1_ := (BA1) - (BY1);
          for i_ := BY1 to BY2 do
            begin
              Y[i_] := C_Add(Y[i_], C_Mul(v, Conj(A[i, i_ + i1_])));
            end;

          //
          // Add U*x to the result
          //
          BX1 := i - i1 + 2;
          BX2 := n;
          BA1 := i + 1;
          BA2 := i2;
          i1_ := (BA1) - (BX1);
          v := C_Complex(0.0);
          for i_ := BX1 to BX2 do
            begin
              v := C_Add(v, C_Mul(X[i_], A[i, i_ + i1_]));
            end;
          Y[i - i1 + 1] := C_Add(Y[i - i1 + 1], v);
          Inc(i);
        end;
    end
  else
    begin
      i := i1 + 1;
      while i <= i2 do
        begin

          //
          // Add L*x to the result
          //
          BX1 := 1;
          BX2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          i1_ := (BA1) - (BX1);
          v := C_Complex(0.0);
          for i_ := BX1 to BX2 do
            begin
              v := C_Add(v, C_Mul(X[i_], A[i, i_ + i1_]));
            end;
          Y[i - i1 + 1] := C_Add(Y[i - i1 + 1], v);

          //
          // Add U*x to the result
          //
          v := X[i - i1 + 1];
          BY1 := 1;
          BY2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          i1_ := (BA1) - (BY1);
          for i_ := BY1 to BY2 do
            begin
              Y[i_] := C_Add(Y[i_], C_Mul(v, Conj(A[i, i_ + i1_])));
            end;
          Inc(i);
        end;
    end;
  for i_ := 1 to n do
    begin
      Y[i_] := C_Mul(alpha, Y[i_]);
    end;
end;

procedure HermitianRank2Update(var A: TLComplexMatrix; IsUpper: Boolean;
  i1: TLInt; i2: TLInt; const X: TLComplexVec;
  const Y: TLComplexVec; var T: TLComplexVec; alpha: TLComplex);
var
  i  : TLInt;
  TP1: TLInt;
  TP2: TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  if IsUpper then
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := i + 1 - i1;
          TP2 := i2 - i1 + 1;
          v := C_Mul(alpha, X[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              T[i_] := C_Mul(v, Conj(Y[i_]));
            end;
          v := C_Mul(Conj(alpha), Y[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              T[i_] := C_Add(T[i_], C_Mul(v, Conj(X[i_])));
            end;
          i1_ := (TP1) - (i);
          for i_ := i to i2 do
            begin
              A[i, i_] := C_Add(A[i, i_], T[i_ + i1_]);
            end;
          Inc(i);
        end;
    end
  else
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := 1;
          TP2 := i + 1 - i1;
          v := C_Mul(alpha, X[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              T[i_] := C_Mul(v, Conj(Y[i_]));
            end;
          v := C_Mul(Conj(alpha), Y[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              T[i_] := C_Add(T[i_], C_Mul(v, Conj(X[i_])));
            end;
          i1_ := (TP1) - (i1);
          for i_ := i1 to i do
            begin
              A[i, i_] := C_Add(A[i, i_], T[i_ + i1_]);
            end;
          Inc(i);
        end;
    end;
end;

procedure RMatrixQR(var A: TLMatrix; M: TLInt; n: TLInt; var TAU: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixLQ(var A: TLMatrix; M: TLInt; n: TLInt; var TAU: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixQR(var A: TLComplexMatrix; M: TLInt; n: TLInt; var TAU: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLQ(var A: TLComplexMatrix; M: TLInt; n: TLInt; var TAU: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixQRUnpackQ(const A: TLMatrix; M: TLInt; n: TLInt; const TAU: TLVec; QColumns: TLInt; var q: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixQRUnpackR(const A: TLMatrix; M: TLInt; n: TLInt; var R: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixLQUnpackQ(const A: TLMatrix; M: TLInt; n: TLInt; const TAU: TLVec; QRows: TLInt; var q: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLQUnpackL(const A: TLMatrix; M: TLInt; n: TLInt; var L: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixQRUnpackQ(const A: TLComplexMatrix; M: TLInt; n: TLInt; const TAU: TLComplexVec; QColumns: TLInt; var q: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixQRUnpackR(const A: TLComplexMatrix; M: TLInt; n: TLInt; var R: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLQUnpackQ(const A: TLComplexMatrix; M: TLInt; n: TLInt; const TAU: TLComplexVec; QRows: TLInt; var q: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLQUnpackL(const A: TLComplexMatrix; M: TLInt; n: TLInt; var L: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixBD(var A: TLMatrix; M: TLInt; n: TLInt; var TauQ: TLVec; var TauP: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixBDUnpackQ(const qp: TLMatrix; M: TLInt; n: TLInt; const TauQ: TLVec; QColumns: TLInt; var q: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixBDMultiplyByQ(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauQ: TLVec; var Z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean; DoTranspose: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixBDUnpackPT(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; PTRows: TLInt; var pt: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixBDMultiplyByP(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; var Z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean; DoTranspose: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixBDUnpackDiagonals(const b: TLMatrix; M: TLInt;
  n: TLInt; var IsUpper: Boolean; var d: TLVec; var E: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixHessenberg(var A: TLMatrix; n: TLInt; var TAU: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixHessenbergUnpackQ(const A: TLMatrix; n: TLInt; const TAU: TLVec; var q: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixHessenbergUnpackH(const A: TLMatrix; n: TLInt; var h: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SMatrixTD(var A: TLMatrix; n: TLInt; IsUpper: Boolean; var TAU: TLVec; var d: TLVec; var E: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SMatrixTDUnpackQ(const A: TLMatrix; const n: TLInt; const IsUpper: Boolean; const TAU: TLVec; var q: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HMatrixTD(var A: TLComplexMatrix; n: TLInt; IsUpper: Boolean; var TAU: TLComplexVec; var d: TLVec; var E: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HMatrixTDUnpackQ(const A: TLComplexMatrix; const n: TLInt; const IsUpper: Boolean; const TAU: TLComplexVec; var q: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixQRBaseCase(var A: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var T: TLVec;
  var TAU: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixLQBaseCase(var A: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var T: TLVec;
  var TAU: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixQRBaseCase(var A: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var T: TLComplexVec;
  var TAU: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLQBaseCase(var A: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var T: TLComplexVec;
  var TAU: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixBlockReflector(var A: TLMatrix; var TAU: TLVec;
  ColumnwiseA: Boolean; LengthA: TLInt; BlockSize: TLInt;
  var T: TLMatrix; var Work: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixBlockReflector(var A: TLComplexMatrix;
  var TAU: TLComplexVec; ColumnwiseA: Boolean; LengthA: TLInt;
  BlockSize: TLInt; var T: TLComplexMatrix;
  var Work: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  QR decomposition of a rectangular matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and R in compact form (see below).
  Tau -   packed array of scalar factors which are used to form
  matrix Q. Array whose index ranges within [0.. Min(M-1,N-1)].

  Matrix A is represented as A = QR, where Q is an orthogonal matrix of size
  MxM, R - upper triangular (or upper trapezoid) matrix of size M x N.

  The elements of matrix R are located on and above the main diagonal of
  matrix A. The elements which are located in Tau array and below the main
  diagonal of matrix A are used to form matrix Q as follows:

  Matrix Q is represented as a product of elementary reflections

  Q = H(0)*H(2)*...*H(k-1),

  where k = min(m,n), and each H(i) is in the form

  H(i) = 1 - tau * v * (v^T)

  where tau is a scalar stored in Tau[I]; v - real vector,
  so that v(0:i-1) = 0, v(i) = 1, v(i+1:m-1) stored in A(i+1:m-1,i).
  ************************************************************************ *)
procedure RMatrixQR(var A: TLMatrix; M: TLInt; n: TLInt;
  var TAU: TLVec);
var
  Work      : TLVec;
  T         : TLVec;
  TauBuf    : TLVec;
  MinMN     : TLInt;
  TmpA      : TLMatrix;
  TmpT      : TLMatrix;
  TmpR      : TLMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  J         : TLInt;
  k         : TLInt;
  v         : TLFloat;
  i_        : TLInt;
  i1_       : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(T, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASBlockSize(A));
  SetLength(TmpT, ABLASBlockSize(A), 2 * ABLASBlockSize(A));
  SetLength(TmpR, 2 * ABLASBlockSize(A), n);

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASBlockSize(A) then
        begin
          BlockSize := ABLASBlockSize(A);
        end;
      RowsCount := M - BlockStart;

      //
      // QR decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      RMatrixCopy(RowsCount, BlockSize, A, BlockStart, BlockStart, TmpA, 0, 0);
      RMatrixQRBaseCase(TmpA, RowsCount, BlockSize, Work, T, TauBuf);
      RMatrixCopy(RowsCount, BlockSize, TmpA, 0, 0, A, BlockStart, BlockStart);
      APVMove(@TAU[0], BlockStart, BlockStart + BlockSize - 1, @TauBuf[0], 0,
        BlockSize - 1);

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= n - 1 then
        begin
          if (n - BlockStart - BlockSize >= 2 * ABLASBlockSize(A)) or
            (RowsCount >= 4 * ABLASBlockSize(A)) then
            begin

              //
              // Prepare block reflector
              //
              RMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q'.
              //
              // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
              // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
              //
              RMatrixGEMM(BlockSize, n - BlockStart - BlockSize, RowsCount, 1.0, TmpA,
                0, 0, 1, A, BlockStart, BlockStart + BlockSize, 0, 0.0, TmpR, 0, 0);
              RMatrixGEMM(BlockSize, n - BlockStart - BlockSize, BlockSize, 1.0, TmpT,
                0, 0, 1, TmpR, 0, 0, 0, 0.0, TmpR, BlockSize, 0);
              RMatrixGEMM(RowsCount, n - BlockStart - BlockSize, BlockSize, 1.0, TmpA,
                0, 0, 0, TmpR, BlockSize, 0, 0, 1.0, A, BlockStart,
                BlockStart + BlockSize);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  i1_ := (i) - (1);
                  for i_ := 1 to RowsCount - i do
                    begin
                      T[i_] := TmpA[i_ + i1_, i];
                    end;
                  T[1] := 1;
                  ApplyReflectionFromTheLeft(A, TauBuf[i], T, BlockStart + i, M - 1,
                    BlockStart + BlockSize, n - 1, Work);
                  Inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  LQ decomposition of a rectangular matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices L and Q in compact form (see below)
  Tau -   packed array of scalar factors which are used to form
  matrix Q. Array whose index ranges within [0..Min(M,N)-1].

  Matrix A is represented as A = LQ, where Q is an orthogonal matrix of size
  MxM, L - lower triangular (or lower trapezoid) matrix of size M x N.

  The elements of matrix L are located on and below  the  main  diagonal  of
  matrix A. The elements which are located in Tau array and above  the  main
  diagonal of matrix A are used to form matrix Q as follows:

  Matrix Q is represented as a product of elementary reflections

  Q = H(k-1)*H(k-2)*...*H(1)*H(0),

  where k = min(m,n), and each H(i) is of the form

  H(i) = 1 - tau * v * (v^T)

  where tau is a scalar stored in Tau[I]; v - real vector, so that v(0:i-1)=0,
  v(i) = 1, v(i+1:n-1) stored in A(i,i+1:n-1).
  ************************************************************************ *)
procedure RMatrixLQ(var A: TLMatrix; M: TLInt; n: TLInt;
  var TAU: TLVec);
var
  Work        : TLVec;
  T           : TLVec;
  TauBuf      : TLVec;
  MinMN       : TLInt;
  TmpA        : TLMatrix;
  TmpT        : TLMatrix;
  TmpR        : TLMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  J           : TLInt;
  k           : TLInt;
  v           : TLFloat;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(T, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASBlockSize(A), n);
  SetLength(TmpT, ABLASBlockSize(A), 2 * ABLASBlockSize(A));
  SetLength(TmpR, M, 2 * ABLASBlockSize(A));

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASBlockSize(A) then
        begin
          BlockSize := ABLASBlockSize(A);
        end;
      ColumnsCount := n - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      RMatrixCopy(BlockSize, ColumnsCount, A, BlockStart, BlockStart, TmpA, 0, 0);
      RMatrixLQBaseCase(TmpA, BlockSize, ColumnsCount, Work, T, TauBuf);
      RMatrixCopy(BlockSize, ColumnsCount, TmpA, 0, 0, A, BlockStart, BlockStart);
      APVMove(@TAU[0], BlockStart, BlockStart + BlockSize - 1, @TauBuf[0], 0,
        BlockSize - 1);

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= M - 1 then
        begin
          if M - BlockStart - BlockSize >= 2 * ABLASBlockSize(A) then
            begin

              //
              // Prepare block reflector
              //
              RMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q.
              //
              // Q  = E + Y*T*Y'  = E + TmpA'*TmpT*TmpA
              //
              RMatrixGEMM(M - BlockStart - BlockSize, BlockSize, ColumnsCount, 1.0, A,
                BlockStart + BlockSize, BlockStart, 0, TmpA, 0, 0, 1, 0.0,
                TmpR, 0, 0);
              RMatrixGEMM(M - BlockStart - BlockSize, BlockSize, BlockSize, 1.0, TmpR,
                0, 0, 0, TmpT, 0, 0, 0, 0.0, TmpR, 0, BlockSize);
              RMatrixGEMM(M - BlockStart - BlockSize, ColumnsCount, BlockSize, 1.0,
                TmpR, 0, BlockSize, 0, TmpA, 0, 0, 0, 1.0, A, BlockStart + BlockSize,
                BlockStart);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  APVMove(@T[0], 1, ColumnsCount - i, @TmpA[i][0], i, ColumnsCount - 1);
                  T[1] := 1;
                  ApplyReflectionFromTheRight(A, TauBuf[i], T, BlockStart + BlockSize,
                    M - 1, BlockStart + i, n - 1, Work);
                  Inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  QR decomposition of a rectangular TLComplex matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and R in compact form
  Tau -   packed array of scalar factors which are used to form matrix Q. Array
  whose indexes range within [0.. Min(M,N)-1]

  Matrix A is represented as A = QR, where Q is an orthogonal matrix of size
  MxM, R - upper triangular (or upper trapezoid) matrix of size MxN.
  ************************************************************************ *)
procedure CMatrixQR(var A: TLComplexMatrix; M: TLInt; n: TLInt;
  var TAU: TLComplexVec);
var
  Work      : TLComplexVec;
  T         : TLComplexVec;
  TauBuf    : TLComplexVec;
  MinMN     : TLInt;
  TmpA      : TLComplexMatrix;
  TmpT      : TLComplexMatrix;
  TmpR      : TLComplexMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  J         : TLInt;
  k         : TLInt;
  v         : TLComplex;
  i_        : TLInt;
  i1_       : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(T, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASComplexBlockSize(A));
  SetLength(TmpT, ABLASComplexBlockSize(A), ABLASComplexBlockSize(A));
  SetLength(TmpR, 2 * ABLASComplexBlockSize(A), n);

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASComplexBlockSize(A) then
        begin
          BlockSize := ABLASComplexBlockSize(A);
        end;
      RowsCount := M - BlockStart;

      //
      // QR decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(RowsCount, BlockSize, A, BlockStart, BlockStart, TmpA, 0, 0);
      CMatrixQRBaseCase(TmpA, RowsCount, BlockSize, Work, T, TauBuf);
      CMatrixCopy(RowsCount, BlockSize, TmpA, 0, 0, A, BlockStart, BlockStart);
      i1_ := (0) - (BlockStart);
      for i_ := BlockStart to BlockStart + BlockSize - 1 do
        begin
          TAU[i_] := TauBuf[i_ + i1_];
        end;

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= n - 1 then
        begin
          if n - BlockStart - BlockSize >= 2 * ABLASComplexBlockSize(A) then
            begin

              //
              // Prepare block reflector
              //
              CMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q'.
              //
              // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
              // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
              //
              CMatrixGEMM(BlockSize, n - BlockStart - BlockSize, RowsCount,
                C_Complex(1.0), TmpA, 0, 0, 2, A, BlockStart, BlockStart + BlockSize,
                0, C_Complex(0.0), TmpR, 0, 0);
              CMatrixGEMM(BlockSize, n - BlockStart - BlockSize, BlockSize,
                C_Complex(1.0), TmpT, 0, 0, 2, TmpR, 0, 0, 0, C_Complex(0.0), TmpR,
                BlockSize, 0);
              CMatrixGEMM(RowsCount, n - BlockStart - BlockSize, BlockSize,
                C_Complex(1.0), TmpA, 0, 0, 0, TmpR, BlockSize, 0, 0, C_Complex(1.0),
                A, BlockStart, BlockStart + BlockSize);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  i1_ := (i) - (1);
                  for i_ := 1 to RowsCount - i do
                    begin
                      T[i_] := TmpA[i_ + i1_, i];
                    end;
                  T[1] := C_Complex(1);
                  ComplexApplyReflectionFromTheLeft(A, Conj(TauBuf[i]), T,
                    BlockStart + i, M - 1, BlockStart + BlockSize, n - 1, Work);
                  Inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  LQ decomposition of a rectangular TLComplex matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and L in compact form
  Tau -   packed array of scalar factors which are used to form matrix Q. Array
  whose indexes range within [0.. Min(M,N)-1]

  Matrix A is represented as A = LQ, where Q is an orthogonal matrix of size
  MxM, L - lower triangular (or lower trapezoid) matrix of size MxN.
  ************************************************************************ *)
procedure CMatrixLQ(var A: TLComplexMatrix; M: TLInt; n: TLInt;
  var TAU: TLComplexVec);
var
  Work        : TLComplexVec;
  T           : TLComplexVec;
  TauBuf      : TLComplexVec;
  MinMN       : TLInt;
  TmpA        : TLComplexMatrix;
  TmpT        : TLComplexMatrix;
  TmpR        : TLComplexMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  J           : TLInt;
  k           : TLInt;
  v           : TLComplex;
  i_          : TLInt;
  i1_         : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(T, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASComplexBlockSize(A), n);
  SetLength(TmpT, ABLASComplexBlockSize(A), ABLASComplexBlockSize(A));
  SetLength(TmpR, M, 2 * ABLASComplexBlockSize(A));

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASComplexBlockSize(A) then
        begin
          BlockSize := ABLASComplexBlockSize(A);
        end;
      ColumnsCount := n - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(BlockSize, ColumnsCount, A, BlockStart, BlockStart, TmpA, 0, 0);
      CMatrixLQBaseCase(TmpA, BlockSize, ColumnsCount, Work, T, TauBuf);
      CMatrixCopy(BlockSize, ColumnsCount, TmpA, 0, 0, A, BlockStart, BlockStart);
      i1_ := (0) - (BlockStart);
      for i_ := BlockStart to BlockStart + BlockSize - 1 do
        begin
          TAU[i_] := TauBuf[i_ + i1_];
        end;

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= M - 1 then
        begin
          if M - BlockStart - BlockSize >= 2 * ABLASComplexBlockSize(A) then
            begin

              //
              // Prepare block reflector
              //
              CMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q.
              //
              // Q  = E + Y*T*Y'  = E + TmpA'*TmpT*TmpA
              //
              CMatrixGEMM(M - BlockStart - BlockSize, BlockSize, ColumnsCount,
                C_Complex(1.0), A, BlockStart + BlockSize, BlockStart, 0, TmpA, 0, 0,
                2, C_Complex(0.0), TmpR, 0, 0);
              CMatrixGEMM(M - BlockStart - BlockSize, BlockSize, BlockSize,
                C_Complex(1.0), TmpR, 0, 0, 0, TmpT, 0, 0, 0, C_Complex(0.0), TmpR, 0,
                BlockSize);
              CMatrixGEMM(M - BlockStart - BlockSize, ColumnsCount, BlockSize,
                C_Complex(1.0), TmpR, 0, BlockSize, 0, TmpA, 0, 0, 0, C_Complex(1.0),
                A, BlockStart + BlockSize, BlockStart);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  i1_ := (i) - (1);
                  for i_ := 1 to ColumnsCount - i do
                    begin
                      T[i_] := Conj(TmpA[i, i_ + i1_]);
                    end;
                  T[1] := C_Complex(1);
                  ComplexApplyReflectionFromTheRight(A, TauBuf[i], T,
                    BlockStart + BlockSize, M - 1, BlockStart + i, n - 1, Work);
                  Inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from the QR decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and R in compact form.
  Output of RMatrixQR subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.
  Tau     -   scalar factors which are used to form Q.
  Output of the RMatrixQR subroutine.
  QColumns -  required number of columns of matrix Q. M>=QColumns>=0.

  Output parameters:
  Q       -   first QColumns columns of matrix Q.
  Array whose indexes range within [0..M-1, 0..QColumns-1].
  If QColumns=0, the array remains unchanged.
  ************************************************************************ *)
procedure RMatrixQRUnpackQ(const A: TLMatrix; M: TLInt;
  n: TLInt; const TAU: TLVec; QColumns: TLInt;
  var q: TLMatrix);
var
  Work      : TLVec;
  T         : TLVec;
  TauBuf    : TLVec;
  MinMN     : TLInt;
  RefCnt    : TLInt;
  TmpA      : TLMatrix;
  TmpT      : TLMatrix;
  TmpR      : TLMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  J         : TLInt;
  k         : TLInt;
  v         : TLFloat;
  i_        : TLInt;
  i1_       : TLInt;
begin
  Assert(QColumns <= M, 'UnpackQFromQR: QColumns>M!');
  if (M <= 0) or (n <= 0) or (QColumns <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QColumns);
  SetLength(q, M, QColumns);
  i := 0;
  while i <= M - 1 do
    begin
      J := 0;
      while J <= QColumns - 1 do
        begin
          if i = J then
            begin
              q[i, J] := 1;
            end
          else
            begin
              q[i, J] := 0;
            end;
          Inc(J);
        end;
      Inc(i);
    end;
  SetLength(Work, Max(M, QColumns) + 1);
  SetLength(T, Max(M, QColumns) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASBlockSize(A));
  SetLength(TmpT, ABLASBlockSize(A), 2 * ABLASBlockSize(A));
  SetLength(TmpR, 2 * ABLASBlockSize(A), QColumns);

  //
  // Blocked code
  //
  BlockStart := ABLASBlockSize(A) * (RefCnt div ABLASBlockSize(A));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      RowsCount := M - BlockStart;

      //
      // Copy current block
      //
      RMatrixCopy(RowsCount, BlockSize, A, BlockStart, BlockStart, TmpA, 0, 0);
      APVMove(@TauBuf[0], 0, BlockSize - 1, @TAU[0], BlockStart,
        BlockStart + BlockSize - 1);

      //
      // Update, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QColumns >= 2 * ABLASBlockSize(A) then
        begin

          //
          // Prepare block reflector
          //
          RMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply matrix by Q.
          //
          // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
          //
          RMatrixGEMM(BlockSize, QColumns, RowsCount, 1.0, TmpA, 0, 0, 1, q,
            BlockStart, 0, 0, 0.0, TmpR, 0, 0);
          RMatrixGEMM(BlockSize, QColumns, BlockSize, 1.0, TmpT, 0, 0, 0, TmpR, 0,
            0, 0, 0.0, TmpR, BlockSize, 0);
          RMatrixGEMM(RowsCount, QColumns, BlockSize, 1.0, TmpA, 0, 0, 0, TmpR,
            BlockSize, 0, 0, 1.0, q, BlockStart, 0);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              i1_ := (i) - (1);
              for i_ := 1 to RowsCount - i do
                begin
                  T[i_] := TmpA[i_ + i1_, i];
                end;
              T[1] := 1;
              ApplyReflectionFromTheLeft(q, TauBuf[i], T, BlockStart + i, M - 1, 0,
                QColumns - 1, Work);
              Dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASBlockSize(A);
      BlockSize := ABLASBlockSize(A);
    end;
end;

(* ************************************************************************
  Unpacking of matrix R from the QR decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and R in compact form.
  Output of RMatrixQR subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  R       -   matrix R, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixQRUnpackR(const A: TLMatrix; M: TLInt;
  n: TLInt; var R: TLMatrix);
var
  i: TLInt;
  k: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  k := Min(M, n);
  SetLength(R, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      R[0, i] := 0;
      Inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      APVMove(@R[i][0], 0, n - 1, @R[0][0], 0, n - 1);
      Inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      APVMove(@R[i][0], i, n - 1, @A[i][0], i, n - 1);
      Inc(i);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from the LQ decomposition of a matrix A

  Input parameters:
  A       -   matrices L and Q in compact form.
  Output of RMatrixLQ subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.
  Tau     -   scalar factors which are used to form Q.
  Output of the RMatrixLQ subroutine.
  QRows   -   required number of rows in matrix Q. N>=QRows>=0.

  Output parameters:
  Q       -   first QRows rows of matrix Q. Array whose indexes range
  within [0..QRows-1, 0..N-1]. If QRows=0, the array remains
  unchanged.
  ************************************************************************ *)
procedure RMatrixLQUnpackQ(const A: TLMatrix; M: TLInt;
  n: TLInt; const TAU: TLVec; QRows: TLInt;
  var q: TLMatrix);
var
  Work        : TLVec;
  T           : TLVec;
  TauBuf      : TLVec;
  MinMN       : TLInt;
  RefCnt      : TLInt;
  TmpA        : TLMatrix;
  TmpT        : TLMatrix;
  TmpR        : TLMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  J           : TLInt;
  k           : TLInt;
  v           : TLFloat;
begin
  Assert(QRows <= n, 'RMatrixLQUnpackQ: QRows>N!');
  if (M <= 0) or (n <= 0) or (QRows <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QRows);
  SetLength(Work, Max(M, n) + 1);
  SetLength(T, Max(M, n) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASBlockSize(A), n);
  SetLength(TmpT, ABLASBlockSize(A), 2 * ABLASBlockSize(A));
  SetLength(TmpR, QRows, 2 * ABLASBlockSize(A));
  SetLength(q, QRows, n);
  i := 0;
  while i <= QRows - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          if i = J then
            begin
              q[i, J] := 1;
            end
          else
            begin
              q[i, J] := 0;
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASBlockSize(A) * (RefCnt div ABLASBlockSize(A));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      ColumnsCount := n - BlockStart;

      //
      // Copy submatrix
      //
      RMatrixCopy(BlockSize, ColumnsCount, A, BlockStart, BlockStart, TmpA, 0, 0);
      APVMove(@TauBuf[0], 0, BlockSize - 1, @TAU[0], BlockStart,
        BlockStart + BlockSize - 1);

      //
      // Update matrix, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QRows >= 2 * ABLASBlockSize(A) then
        begin

          //
          // Prepare block reflector
          //
          RMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply the rest of A by Q'.
          //
          // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
          //
          RMatrixGEMM(QRows, BlockSize, ColumnsCount, 1.0, q, 0, BlockStart, 0,
            TmpA, 0, 0, 1, 0.0, TmpR, 0, 0);
          RMatrixGEMM(QRows, BlockSize, BlockSize, 1.0, TmpR, 0, 0, 0, TmpT, 0, 0,
            1, 0.0, TmpR, 0, BlockSize);
          RMatrixGEMM(QRows, ColumnsCount, BlockSize, 1.0, TmpR, 0, BlockSize, 0,
            TmpA, 0, 0, 0, 1.0, q, 0, BlockStart);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              APVMove(@T[0], 1, ColumnsCount - i, @TmpA[i][0], i, ColumnsCount - 1);
              T[1] := 1;
              ApplyReflectionFromTheRight(q, TauBuf[i], T, 0, QRows - 1,
                BlockStart + i, n - 1, Work);
              Dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASBlockSize(A);
      BlockSize := ABLASBlockSize(A);
    end;
end;

(* ************************************************************************
  Unpacking of matrix L from the LQ decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and L in compact form.
  Output of RMatrixLQ subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  L       -   matrix L, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixLQUnpackL(const A: TLMatrix; M: TLInt;
  n: TLInt; var L: TLMatrix);
var
  i: TLInt;
  k: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  SetLength(L, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      L[0, i] := 0;
      Inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      APVMove(@L[i][0], 0, n - 1, @L[0][0], 0, n - 1);
      Inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      k := Min(i, n - 1);
      APVMove(@L[i][0], 0, k, @A[i][0], 0, k);
      Inc(i);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from QR decomposition of a TLComplex matrix A.

  Input parameters:
  A           -   matrices Q and R in compact form.
  Output of CMatrixQR subroutine .
  M           -   number of rows in matrix A. M>=0.
  N           -   number of columns in matrix A. N>=0.
  Tau         -   scalar factors which are used to form Q.
  Output of CMatrixQR subroutine .
  QColumns    -   required number of columns in matrix Q. M>=QColumns>=0.

  Output parameters:
  Q           -   first QColumns columns of matrix Q.
  Array whose index ranges within [0..M-1, 0..QColumns-1].
  If QColumns=0, array isn't changed.
  ************************************************************************ *)
procedure CMatrixQRUnpackQ(const A: TLComplexMatrix; M: TLInt;
  n: TLInt; const TAU: TLComplexVec; QColumns: TLInt;
  var q: TLComplexMatrix);
var
  Work      : TLComplexVec;
  T         : TLComplexVec;
  TauBuf    : TLComplexVec;
  MinMN     : TLInt;
  RefCnt    : TLInt;
  TmpA      : TLComplexMatrix;
  TmpT      : TLComplexMatrix;
  TmpR      : TLComplexMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  J         : TLInt;
  k         : TLInt;
  v         : TLComplex;
  i_        : TLInt;
  i1_       : TLInt;
begin
  Assert(QColumns <= M, 'UnpackQFromQR: QColumns>M!');
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QColumns);
  SetLength(Work, Max(M, n) + 1);
  SetLength(T, Max(M, n) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASComplexBlockSize(A));
  SetLength(TmpT, ABLASComplexBlockSize(A), ABLASComplexBlockSize(A));
  SetLength(TmpR, 2 * ABLASComplexBlockSize(A), QColumns);
  SetLength(q, M, QColumns);
  i := 0;
  while i <= M - 1 do
    begin
      J := 0;
      while J <= QColumns - 1 do
        begin
          if i = J then
            begin
              q[i, J] := C_Complex(1);
            end
          else
            begin
              q[i, J] := C_Complex(0);
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASComplexBlockSize(A) *
    (RefCnt div ABLASComplexBlockSize(A));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      RowsCount := M - BlockStart;

      //
      // QR decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(RowsCount, BlockSize, A, BlockStart, BlockStart, TmpA, 0, 0);
      i1_ := (BlockStart) - (0);
      for i_ := 0 to BlockSize - 1 do
        begin
          TauBuf[i_] := TAU[i_ + i1_];
        end;

      //
      // Update matrix, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QColumns >= 2 * ABLASComplexBlockSize(A) then
        begin

          //
          // Prepare block reflector
          //
          CMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply the rest of A by Q.
          //
          // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
          //
          CMatrixGEMM(BlockSize, QColumns, RowsCount, C_Complex(1.0), TmpA, 0, 0, 2,
            q, BlockStart, 0, 0, C_Complex(0.0), TmpR, 0, 0);
          CMatrixGEMM(BlockSize, QColumns, BlockSize, C_Complex(1.0), TmpT, 0, 0, 0,
            TmpR, 0, 0, 0, C_Complex(0.0), TmpR, BlockSize, 0);
          CMatrixGEMM(RowsCount, QColumns, BlockSize, C_Complex(1.0), TmpA, 0, 0, 0,
            TmpR, BlockSize, 0, 0, C_Complex(1.0), q, BlockStart, 0);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              i1_ := (i) - (1);
              for i_ := 1 to RowsCount - i do
                begin
                  T[i_] := TmpA[i_ + i1_, i];
                end;
              T[1] := C_Complex(1);
              ComplexApplyReflectionFromTheLeft(q, TauBuf[i], T, BlockStart + i,
                M - 1, 0, QColumns - 1, Work);
              Dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASComplexBlockSize(A);
      BlockSize := ABLASComplexBlockSize(A);
    end;
end;

(* ************************************************************************
  Unpacking of matrix R from the QR decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and R in compact form.
  Output of CMatrixQR subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  R       -   matrix R, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure CMatrixQRUnpackR(const A: TLComplexMatrix; M: TLInt;
  n: TLInt; var R: TLComplexMatrix);
var
  i : TLInt;
  k : TLInt;
  i_: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  k := Min(M, n);
  SetLength(R, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      R[0, i] := C_Complex(0);
      Inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      for i_ := 0 to n - 1 do
        begin
          R[i, i_] := R[0, i_];
        end;
      Inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      for i_ := i to n - 1 do
        begin
          R[i, i_] := A[i, i_];
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from LQ decomposition of a TLComplex matrix A.

  Input parameters:
  A           -   matrices Q and R in compact form.
  Output of CMatrixLQ subroutine .
  M           -   number of rows in matrix A. M>=0.
  N           -   number of columns in matrix A. N>=0.
  Tau         -   scalar factors which are used to form Q.
  Output of CMatrixLQ subroutine .
  QRows       -   required number of rows in matrix Q. N>=QColumns>=0.

  Output parameters:
  Q           -   first QRows rows of matrix Q.
  Array whose index ranges within [0..QRows-1, 0..N-1].
  If QRows=0, array isn't changed.
  ************************************************************************ *)
procedure CMatrixLQUnpackQ(const A: TLComplexMatrix; M: TLInt;
  n: TLInt; const TAU: TLComplexVec; QRows: TLInt;
  var q: TLComplexMatrix);
var
  Work        : TLComplexVec;
  T           : TLComplexVec;
  TauBuf      : TLComplexVec;
  MinMN       : TLInt;
  RefCnt      : TLInt;
  TmpA        : TLComplexMatrix;
  TmpT        : TLComplexMatrix;
  TmpR        : TLComplexMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  J           : TLInt;
  k           : TLInt;
  v           : TLComplex;
  i_          : TLInt;
  i1_         : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;

  //
  // Init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QRows);
  SetLength(Work, Max(M, n) + 1);
  SetLength(T, Max(M, n) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASComplexBlockSize(A), n);
  SetLength(TmpT, ABLASComplexBlockSize(A), ABLASComplexBlockSize(A));
  SetLength(TmpR, QRows, 2 * ABLASComplexBlockSize(A));
  SetLength(q, QRows, n);
  i := 0;
  while i <= QRows - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          if i = J then
            begin
              q[i, J] := C_Complex(1);
            end
          else
            begin
              q[i, J] := C_Complex(0);
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASComplexBlockSize(A) *
    (RefCnt div ABLASComplexBlockSize(A));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      ColumnsCount := n - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(BlockSize, ColumnsCount, A, BlockStart, BlockStart, TmpA, 0, 0);
      i1_ := (BlockStart) - (0);
      for i_ := 0 to BlockSize - 1 do
        begin
          TauBuf[i_] := TAU[i_ + i1_];
        end;

      //
      // Update matrix, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QRows >= 2 * ABLASComplexBlockSize(A) then
        begin

          //
          // Prepare block reflector
          //
          CMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply the rest of A by Q'.
          //
          // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
          //
          CMatrixGEMM(QRows, BlockSize, ColumnsCount, C_Complex(1.0), q, 0,
            BlockStart, 0, TmpA, 0, 0, 2, C_Complex(0.0), TmpR, 0, 0);
          CMatrixGEMM(QRows, BlockSize, BlockSize, C_Complex(1.0), TmpR, 0, 0, 0,
            TmpT, 0, 0, 2, C_Complex(0.0), TmpR, 0, BlockSize);
          CMatrixGEMM(QRows, ColumnsCount, BlockSize, C_Complex(1.0), TmpR, 0,
            BlockSize, 0, TmpA, 0, 0, 0, C_Complex(1.0), q, 0, BlockStart);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              i1_ := (i) - (1);
              for i_ := 1 to ColumnsCount - i do
                begin
                  T[i_] := Conj(TmpA[i, i_ + i1_]);
                end;
              T[1] := C_Complex(1);
              ComplexApplyReflectionFromTheRight(q, Conj(TauBuf[i]), T, 0, QRows - 1,
                BlockStart + i, n - 1, Work);
              Dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASComplexBlockSize(A);
      BlockSize := ABLASComplexBlockSize(A);
    end;
end;

(* ************************************************************************
  Unpacking of matrix L from the LQ decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and L in compact form.
  Output of CMatrixLQ subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  L       -   matrix L, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure CMatrixLQUnpackL(const A: TLComplexMatrix; M: TLInt;
  n: TLInt; var L: TLComplexMatrix);
var
  i : TLInt;
  k : TLInt;
  i_: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  SetLength(L, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      L[0, i] := C_Complex(0);
      Inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      for i_ := 0 to n - 1 do
        begin
          L[i, i_] := L[0, i_];
        end;
      Inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      k := Min(i, n - 1);
      for i_ := 0 to k do
        begin
          L[i, i_] := A[i, i_];
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Reduction of a rectangular matrix to  bidiagonal form

  The algorithm reduces the rectangular matrix A to  bidiagonal form by
  orthogonal transformations P and Q: A = Q*B*P.

  Input parameters:
  A       -   source matrix. array[0..M-1, 0..N-1]
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.

  Output parameters:
  A       -   matrices Q, B, P in compact form (see below).
  TauQ    -   scalar factors which are used to form matrix Q.
  TauP    -   scalar factors which are used to form matrix P.

  The main diagonal and one of the  secondary  diagonals  of  matrix  A  are
  replaced with bidiagonal  matrix  B.  Other  elements  contain  elementary
  reflections which form MxM matrix Q and NxN matrix P, respectively.

  If M>=N, B is the upper  bidiagonal  MxN  matrix  and  is  stored  in  the
  corresponding  elements  of  matrix  A.  Matrix  Q  is  represented  as  a
  product   of   elementary   reflections   Q = H(0)*H(1)*...*H(n-1),  where
  H(i) = 1-tau*v*v'. Here tau is a scalar which is stored  in  TauQ[i],  and
  vector v has the following  structure:  v(0:i-1)=0, v(i)=1, v(i+1:m-1)  is
  stored   in   elements   A(i+1:m-1,i).   Matrix   P  is  as  follows:  P =
  G(0)*G(1)*...*G(n-2), where G(i) = 1 - tau*u*u'. Tau is stored in TauP[i],
  u(0:i)=0, u(i+1)=1, u(i+2:n-1) is stored in elements A(i,i+2:n-1).

  If M<N, B is the  lower  bidiagonal  MxN  matrix  and  is  stored  in  the
  corresponding   elements  of  matrix  A.  Q = H(0)*H(1)*...*H(m-2),  where
  H(i) = 1 - tau*v*v', tau is stored in TauQ, v(0:i)=0, v(i+1)=1, v(i+2:m-1)
  is    stored    in   elements   A(i+2:m-1,i).    P = G(0)*G(1)*...*G(m-1),
  G(i) = 1-tau*u*u', tau is stored in  TauP,  u(0:i-1)=0, u(i)=1, u(i+1:n-1)
  is stored in A(i,i+1:n-1).

  EXAMPLE:

  m=6, n=5 (m > n):               m=5, n=6 (m < n):

  (  d   e   u1  u1  u1 )         (  d   u1  u1  u1  u1  u1 )
  (  v1  d   e   u2  u2 )         (  e   d   u2  u2  u2  u2 )
  (  v1  v2  d   e   u3 )         (  v1  e   d   u3  u3  u3 )
  (  v1  v2  v3  d   e  )         (  v1  v2  e   d   u4  u4 )
  (  v1  v2  v3  v4  d  )         (  v1  v2  v3  e   d   u5 )
  (  v1  v2  v3  v4  v5 )

  Here vi and ui are vectors which form H(i) and G(i), and d and e -
  are the diagonal and off-diagonal elements of matrix B.
  ************************************************************************ *)
procedure RMatrixBD(var A: TLMatrix; M: TLInt; n: TLInt;
  var TauQ: TLVec; var TauP: TLVec);
var
  Work : TLVec;
  T    : TLVec;
  MinMN: TLInt;
  MaxMN: TLInt;
  i    : TLInt;
  LTau : TLFloat;
  i_   : TLInt;
  i1_  : TLInt;
begin

  //
  // Prepare
  //
  if (n <= 0) or (M <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  MaxMN := Max(M, n);
  SetLength(Work, MaxMN + 1);
  SetLength(T, MaxMN + 1);
  if M >= n then
    begin
      SetLength(TauQ, n);
      SetLength(TauP, n);
    end
  else
    begin
      SetLength(TauQ, M);
      SetLength(TauP, M);
    end;
  if M >= n then
    begin

      //
      // Reduce to upper bidiagonal form
      //
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Generate elementary reflector H(i) to annihilate A(i+1:m-1,i)
          //
          i1_ := (i) - (1);
          for i_ := 1 to M - i do
            begin
              T[i_] := A[i_ + i1_, i];
            end;
          GenerateReflection(T, M - i, LTau);
          TauQ[i] := LTau;
          i1_ := (1) - (i);
          for i_ := i to M - 1 do
            begin
              A[i_, i] := T[i_ + i1_];
            end;
          T[1] := 1;

          //
          // Apply H(i) to A(i:m-1,i+1:n-1) from the left
          //
          ApplyReflectionFromTheLeft(A, LTau, T, i, M - 1, i + 1, n - 1, Work);
          if i < n - 1 then
            begin

              //
              // Generate elementary reflector G(i) to annihilate
              // A(i,i+2:n-1)
              //
              APVMove(@T[0], 1, n - i - 1, @A[i][0], i + 1, n - 1);
              GenerateReflection(T, n - 1 - i, LTau);
              TauP[i] := LTau;
              APVMove(@A[i][0], i + 1, n - 1, @T[0], 1, n - 1 - i);
              T[1] := 1;

              //
              // Apply G(i) to A(i+1:m-1,i+1:n-1) from the right
              //
              ApplyReflectionFromTheRight(A, LTau, T, i + 1, M - 1, i + 1,
                n - 1, Work);
            end
          else
            begin
              TauP[i] := 0;
            end;
          Inc(i);
        end;
    end
  else
    begin

      //
      // Reduce to lower bidiagonal form
      //
      i := 0;
      while i <= M - 1 do
        begin

          //
          // Generate elementary reflector G(i) to annihilate A(i,i+1:n-1)
          //
          APVMove(@T[0], 1, n - i, @A[i][0], i, n - 1);
          GenerateReflection(T, n - i, LTau);
          TauP[i] := LTau;
          APVMove(@A[i][0], i, n - 1, @T[0], 1, n - i);
          T[1] := 1;

          //
          // Apply G(i) to A(i+1:m-1,i:n-1) from the right
          //
          ApplyReflectionFromTheRight(A, LTau, T, i + 1, M - 1, i, n - 1, Work);
          if i < M - 1 then
            begin

              //
              // Generate elementary reflector H(i) to annihilate
              // A(i+2:m-1,i)
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to M - 1 - i do
                begin
                  T[i_] := A[i_ + i1_, i];
                end;
              GenerateReflection(T, M - 1 - i, LTau);
              TauQ[i] := LTau;
              i1_ := (1) - (i + 1);
              for i_ := i + 1 to M - 1 do
                begin
                  A[i_, i] := T[i_ + i1_];
                end;
              T[1] := 1;

              //
              // Apply H(i) to A(i+1:m-1,i+1:n-1) from the left
              //
              ApplyReflectionFromTheLeft(A, LTau, T, i + 1, M - 1, i + 1,
                n - 1, Work);
            end
          else
            begin
              TauQ[i] := 0;
            end;
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces a matrix to bidiagonal form.

  Input parameters:
  QP          -   matrices Q and P in compact form.
  Output of ToBidiagonal subroutine.
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  TAUQ        -   scalar factors which are used to form Q.
  Output of ToBidiagonal subroutine.
  QColumns    -   required number of columns in matrix Q.
  M>=QColumns>=0.

  Output parameters:
  Q           -   first QColumns columns of matrix Q.
  Array[0..M-1, 0..QColumns-1]
  If QColumns=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDUnpackQ(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauQ: TLVec; QColumns: TLInt;
  var q: TLMatrix);
var
  i: TLInt;
  J: TLInt;
begin
  Assert(QColumns <= M, 'RMatrixBDUnpackQ: QColumns>M!');
  Assert(QColumns >= 0, 'RMatrixBDUnpackQ: QColumns<0!');
  if (M = 0) or (n = 0) or (QColumns = 0) then
    begin
      Exit;
    end;

  //
  // prepare Q
  //
  SetLength(q, M, QColumns);
  i := 0;
  while i <= M - 1 do
    begin
      J := 0;
      while J <= QColumns - 1 do
        begin
          if i = J then
            begin
              q[i, J] := 1;
            end
          else
            begin
              q[i, J] := 0;
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // Calculate
  //
  RMatrixBDMultiplyByQ(qp, M, n, TauQ, q, M, QColumns, False, False);
end;

(* ************************************************************************
  Multiplication by matrix Q which reduces matrix A to  bidiagonal form.

  The algorithm allows pre- or post-multiply by Q or Q'.

  Input parameters:
  QP          -   matrices Q and P in compact form.
  Output of ToBidiagonal subroutine.
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  TAUQ        -   scalar factors which are used to form Q.
  Output of ToBidiagonal subroutine.
  Z           -   multiplied matrix.
  array[0..ZRows-1,0..ZColumns-1]
  ZRows       -   number of rows in matrix Z. If FromTheRight=False,
  ZRows=M, otherwise ZRows can be arbitrary.
  ZColumns    -   number of columns in matrix Z. If FromTheRight=True,
  ZColumns=M, otherwise ZColumns can be arbitrary.
  FromTheRight -  pre- or post-multiply.
  DoTranspose -   multiply by Q or Q'.

  Output parameters:
  Z           -   product of Z and Q.
  Array[0..ZRows-1,0..ZColumns-1]
  If ZRows=0 or ZColumns=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDMultiplyByQ(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauQ: TLVec; var Z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean;
  DoTranspose: Boolean);
var
  i    : TLInt;
  i1   : TLInt;
  i2   : TLInt;
  IStep: TLInt;
  v    : TLVec;
  Work : TLVec;
  mx   : TLInt;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if (M <= 0) or (n <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
    begin
      Exit;
    end;
  Assert(FromTheRight and (ZColumns = M) or not FromTheRight and (ZRows = M),
    'RMatrixBDMultiplyByQ: incorrect Z size!');

  //
  // init
  //
  mx := Max(M, n);
  mx := Max(mx, ZRows);
  mx := Max(mx, ZColumns);
  SetLength(v, mx + 1);
  SetLength(Work, mx + 1);
  if M >= n then
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := 0;
          i2 := n - 1;
          IStep := +1;
        end
      else
        begin
          i1 := n - 1;
          i2 := 0;
          IStep := -1;
        end;
      if DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      i := i1;
      repeat
        i1_ := (i) - (1);
        for i_ := 1 to M - i do
          begin
            v[i_] := qp[i_ + i1_, i];
          end;
        v[1] := 1;
        if FromTheRight then
          begin
            ApplyReflectionFromTheRight(Z, TauQ[i], v, 0, ZRows - 1, i,
              M - 1, Work);
          end
        else
          begin
            ApplyReflectionFromTheLeft(Z, TauQ[i], v, i, M - 1, 0,
              ZColumns - 1, Work);
          end;
        i := i + IStep;
      until i = i2 + IStep;
    end
  else
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := 0;
          i2 := M - 2;
          IStep := +1;
        end
      else
        begin
          i1 := M - 2;
          i2 := 0;
          IStep := -1;
        end;
      if DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      if M - 1 > 0 then
        begin
          i := i1;
          repeat
            i1_ := (i + 1) - (1);
            for i_ := 1 to M - i - 1 do
              begin
                v[i_] := qp[i_ + i1_, i];
              end;
            v[1] := 1;
            if FromTheRight then
              begin
                ApplyReflectionFromTheRight(Z, TauQ[i], v, 0, ZRows - 1, i + 1,
                  M - 1, Work);
              end
            else
              begin
                ApplyReflectionFromTheLeft(Z, TauQ[i], v, i + 1, M - 1, 0,
                  ZColumns - 1, Work);
              end;
            i := i + IStep;
          until i = i2 + IStep;
        end;
    end;
end;

(* ************************************************************************
  Unpacking matrix P which reduces matrix A to bidiagonal form.
  The subroutine returns transposed matrix P.

  Input parameters:
  QP      -   matrices Q and P in compact form.
  Output of ToBidiagonal subroutine.
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.
  TAUP    -   scalar factors which are used to form P.
  Output of ToBidiagonal subroutine.
  PTRows  -   required number of rows of matrix P^T. N >= PTRows >= 0.

  Output parameters:
  PT      -   first PTRows columns of matrix P^T
  Array[0..PTRows-1, 0..N-1]
  If PTRows=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDUnpackPT(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; PTRows: TLInt;
  var pt: TLMatrix);
var
  i: TLInt;
  J: TLInt;
begin
  Assert(PTRows <= n, 'RMatrixBDUnpackPT: PTRows>N!');
  Assert(PTRows >= 0, 'RMatrixBDUnpackPT: PTRows<0!');
  if (M = 0) or (n = 0) or (PTRows = 0) then
    begin
      Exit;
    end;

  //
  // prepare PT
  //
  SetLength(pt, PTRows, n);
  i := 0;
  while i <= PTRows - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          if i = J then
            begin
              pt[i, J] := 1;
            end
          else
            begin
              pt[i, J] := 0;
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // Calculate
  //
  RMatrixBDMultiplyByP(qp, M, n, TauP, pt, PTRows, n, True, True);
end;

(* ************************************************************************
  Multiplication by matrix P which reduces matrix A to  bidiagonal form.

  The algorithm allows pre- or post-multiply by P or P'.

  Input parameters:
  QP          -   matrices Q and P in compact form.
  Output of RMatrixBD subroutine.
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  TAUP        -   scalar factors which are used to form P.
  Output of RMatrixBD subroutine.
  Z           -   multiplied matrix.
  Array whose indexes range within [0..ZRows-1,0..ZColumns-1].
  ZRows       -   number of rows in matrix Z. If FromTheRight=False,
  ZRows=N, otherwise ZRows can be arbitrary.
  ZColumns    -   number of columns in matrix Z. If FromTheRight=True,
  ZColumns=N, otherwise ZColumns can be arbitrary.
  FromTheRight -  pre- or post-multiply.
  DoTranspose -   multiply by P or P'.

  Output parameters:
  Z - product of Z and P.
  Array whose indexes range within [0..ZRows-1,0..ZColumns-1].
  If ZRows=0 or ZColumns=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDMultiplyByP(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; var Z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean;
  DoTranspose: Boolean);
var
  i    : TLInt;
  v    : TLVec;
  Work : TLVec;
  mx   : TLInt;
  i1   : TLInt;
  i2   : TLInt;
  IStep: TLInt;
begin
  if (M <= 0) or (n <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
    begin
      Exit;
    end;
  Assert(FromTheRight and (ZColumns = n) or not FromTheRight and (ZRows = n),
    'RMatrixBDMultiplyByP: incorrect Z size!');

  //
  // init
  //
  mx := Max(M, n);
  mx := Max(mx, ZRows);
  mx := Max(mx, ZColumns);
  SetLength(v, mx + 1);
  SetLength(Work, mx + 1);
  if M >= n then
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := n - 2;
          i2 := 0;
          IStep := -1;
        end
      else
        begin
          i1 := 0;
          i2 := n - 2;
          IStep := +1;
        end;
      if not DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      if n - 1 > 0 then
        begin
          i := i1;
          repeat
            APVMove(@v[0], 1, n - 1 - i, @qp[i][0], i + 1, n - 1);
            v[1] := 1;
            if FromTheRight then
              begin
                ApplyReflectionFromTheRight(Z, TauP[i], v, 0, ZRows - 1, i + 1,
                  n - 1, Work);
              end
            else
              begin
                ApplyReflectionFromTheLeft(Z, TauP[i], v, i + 1, n - 1, 0,
                  ZColumns - 1, Work);
              end;
            i := i + IStep;
          until i = i2 + IStep;
        end;
    end
  else
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := M - 1;
          i2 := 0;
          IStep := -1;
        end
      else
        begin
          i1 := 0;
          i2 := M - 1;
          IStep := +1;
        end;
      if not DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      i := i1;
      repeat
        APVMove(@v[0], 1, n - i, @qp[i][0], i, n - 1);
        v[1] := 1;
        if FromTheRight then
          begin
            ApplyReflectionFromTheRight(Z, TauP[i], v, 0, ZRows - 1, i,
              n - 1, Work);
          end
        else
          begin
            ApplyReflectionFromTheLeft(Z, TauP[i], v, i, n - 1, 0,
              ZColumns - 1, Work);
          end;
        i := i + IStep;
      until i = i2 + IStep;
    end;
end;

(* ************************************************************************
  Unpacking of the main and secondary diagonals of bidiagonal decomposition
  of matrix A.

  Input parameters:
  B   -   output of RMatrixBD subroutine.
  M   -   number of rows in matrix B.
  N   -   number of columns in matrix B.

  Output parameters:
  IsUpper -   True, if the matrix is upper bidiagonal.
  otherwise IsUpper is False.
  D       -   the main diagonal.
  Array whose index ranges within [0..Min(M,N)-1].
  E       -   the secondary diagonal (upper or lower, depending on
  the value of IsUpper).
  Array index ranges within [0..Min(M,N)-1], the last
  element is not used.
  ************************************************************************ *)
procedure RMatrixBDUnpackDiagonals(const b: TLMatrix; M: TLInt;
  n: TLInt; var IsUpper: Boolean; var d: TLVec;
  var E: TLVec);
var
  i: TLInt;
begin
  IsUpper := M >= n;
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  if IsUpper then
    begin
      SetLength(d, n);
      SetLength(E, n);
      i := 0;
      while i <= n - 2 do
        begin
          d[i] := b[i, i];
          E[i] := b[i, i + 1];
          Inc(i);
        end;
      d[n - 1] := b[n - 1, n - 1];
    end
  else
    begin
      SetLength(d, M);
      SetLength(E, M);
      i := 0;
      while i <= M - 2 do
        begin
          d[i] := b[i, i];
          E[i] := b[i + 1, i];
          Inc(i);
        end;
      d[M - 1] := b[M - 1, M - 1];
    end;
end;

(* ************************************************************************
  Reduction of a square matrix to  upper Hessenberg form: Q'*A*Q = H,
  where Q is an orthogonal matrix, H - Hessenberg matrix.

  Input parameters:
  A       -   matrix A with elements [0..N-1, 0..N-1]
  N       -   size of matrix A.

  Output parameters:
  A       -   matrices Q and P in  compact form (see below).
  Tau     -   packed array of scalar factors which are used to form matrix Q.
  Array whose index ranges within [0..N-2]

  Matrix H is located on the main diagonal, on the lower secondary  diagonal
  and above the main diagonal of matrix A. The elements which are used to
  form matrix Q are situated in array Tau and below the lower secondary
  diagonal of matrix A as follows:

  Matrix Q is represented as a product of elementary reflections

  Q = H(0)*H(2)*...*H(n-2),

  where each H(i) is given by

  H(i) = 1 - tau * v * (v^T)

  where tau is a scalar stored in Tau[I]; v - is a real vector,
  so that v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) stored in A(i+2:n-1,i).
  ************************************************************************ *)
procedure RMatrixHessenberg(var A: TLMatrix; n: TLInt;
  var TAU: TLVec);
var
  i   : TLInt;
  v   : TLFloat;
  T   : TLVec;
  Work: TLVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  Assert(n >= 0, 'RMatrixHessenberg: incorrect N!');

  //
  // Quick return if possible
  //
  if n <= 1 then
    begin
      Exit;
    end;
  SetLength(TAU, n - 2 + 1);
  SetLength(T, n + 1);
  SetLength(Work, n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 2 do
    begin

      //
      // Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
      //
      i1_ := (i + 1) - (1);
      for i_ := 1 to n - i - 1 do
        begin
          T[i_] := A[i_ + i1_, i];
        end;
      GenerateReflection(T, n - i - 1, v);
      i1_ := (1) - (i + 1);
      for i_ := i + 1 to n - 1 do
        begin
          A[i_, i] := T[i_ + i1_];
        end;
      TAU[i] := v;
      T[1] := 1;

      //
      // Apply H(i) to A(1:ihi,i+1:ihi) from the right
      //
      ApplyReflectionFromTheRight(A, v, T, 0, n - 1, i + 1, n - 1, Work);

      //
      // Apply H(i) to A(i+1:ihi,i+1:n) from the left
      //
      ApplyReflectionFromTheLeft(A, v, T, i + 1, n - 1, i + 1, n - 1, Work);
      Inc(i);
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces matrix A to upper Hessenberg form

  Input parameters:
  A   -   output of RMatrixHessenberg subroutine.
  N   -   size of matrix A.
  Tau -   scalar factors which are used to form Q.
  Output of RMatrixHessenberg subroutine.

  Output parameters:
  Q   -   matrix Q.
  Array whose indexes range within [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixHessenbergUnpackQ(const A: TLMatrix; n: TLInt;
  const TAU: TLVec; var q: TLMatrix);
var
  i   : TLInt;
  J   : TLInt;
  v   : TLVec;
  Work: TLVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  if n = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(q, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  SetLength(v, n (* - 1 + 1 // optimized compiler *) );
  SetLength(Work, n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          if i = J then
            begin
              q[i, J] := 1;
            end
          else
            begin
              q[i, J] := 0;
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // unpack Q
  //
  i := 0;
  while i <= n - 2 do
    begin

      //
      // Apply H(i)
      //
      i1_ := (i + 1) - (1);
      for i_ := 1 to n - i - 1 do
        begin
          v[i_] := A[i_ + i1_, i];
        end;
      v[1] := 1;
      ApplyReflectionFromTheRight(q, TAU[i], v, 0, n - 1, i + 1, n - 1, Work);
      Inc(i);
    end;
end;

(* ************************************************************************
  Unpacking matrix H (the result of matrix A reduction to upper Hessenberg form)

  Input parameters:
  A   -   output of RMatrixHessenberg subroutine.
  N   -   size of matrix A.

  Output parameters:
  H   -   matrix H. Array whose indexes range within [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixHessenbergUnpackH(const A: TLMatrix; n: TLInt;
  var h: TLMatrix);
var
  i   : TLInt;
  J   : TLInt;
  v   : TLVec;
  Work: TLVec;
begin
  if n = 0 then
    begin
      Exit;
    end;
  SetLength(h, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= i - 2 do
        begin
          h[i, J] := 0;
          Inc(J);
        end;
      J := Max(0, i - 1);
      APVMove(@h[i][0], J, n - 1, @A[i][0], J, n - 1);
      Inc(i);
    end;
end;

(* ************************************************************************
  Reduction of a symmetric matrix which is given by its higher or lower
  triangular part to a tridiagonal matrix using orthogonal similarity
  transformation: Q'*A*Q=T.

  Input parameters:
  A       -   matrix to be transformed
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format. If IsUpper = True, then matrix A is given
  by its upper triangle, and the lower triangle is not used
  and not modified by the algorithm, and vice versa
  if IsUpper = False.

  Output parameters:
  A       -   matrices T and Q in  compact form (see lower)
  Tau     -   packed array of factors which are forming matrices H(i)
  array with elements [0..N-2].
  D       -   main diagonal of symmetric matrix T.
  array with elements [0..N-1].
  E       -   secondary diagonal of symmetric matrix T.
  array with elements [0..N-2].


  If IsUpper=True, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(n-2) . . . H(2) H(0).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a real scalar, and v is a real vector with
  v(i+1:n-1) = 0, v(i) = 1, v(0:i-1) is stored on exit in
  A(0:i-1,i+1), and tau in TAU(i).

  If IsUpper=False, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(0) H(2) . . . H(n-2).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a real scalar, and v is a real vector with
  v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) is stored on exit in A(i+2:n-1,i),
  and tau in TAU(i).

  The contents of A on exit are illustrated by the following examples
  with n = 5:

  if UPLO = 'U':                       if UPLO = 'L':

  (  d   e   v1  v2  v3 )              (  d                  )
  (      d   e   v2  v3 )              (  e   d              )
  (          d   e   v3 )              (  v0  e   d          )
  (              d   e  )              (  v0  v1  e   d      )
  (                  d  )              (  v0  v1  v2  e   d  )

  where d and e denote diagonal and off-diagonal elements of T, and vi
  denotes an element of the vector defining H(i).
  ************************************************************************ *)
procedure SMatrixTD(var A: TLMatrix; n: TLInt; IsUpper: Boolean;
  var TAU: TLVec; var d: TLVec; var E: TLVec);
var
  i    : TLInt;
  alpha: TLFloat;
  TAUI : TLFloat;
  v    : TLFloat;
  T    : TLVec;
  t2   : TLVec;
  t3   : TLVec;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if n <= 0 then
    begin
      Exit;
    end;
  SetLength(T, n + 1);
  SetLength(t2, n + 1);
  SetLength(t3, n + 1);
  if n > 1 then
    begin
      SetLength(TAU, n - 2 + 1);
    end;
  SetLength(d, n (* - 1 + 1 // optimized compiler *) );
  if n > 1 then
    begin
      SetLength(E, n - 2 + 1);
    end;
  if IsUpper then
    begin

      //
      // Reduce the upper triangle of A
      //
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Generate elementary reflector H() = E - tau * v * v'
          //
          if i >= 1 then
            begin
              i1_ := (0) - (2);
              for i_ := 2 to i + 1 do
                begin
                  T[i_] := A[i_ + i1_, i + 1];
                end;
            end;
          T[1] := A[i, i + 1];
          GenerateReflection(T, i + 1, TAUI);
          if i >= 1 then
            begin
              i1_ := (2) - (0);
              for i_ := 0 to i - 1 do
                begin
                  A[i_, i + 1] := T[i_ + i1_];
                end;
            end;
          A[i, i + 1] := T[1];
          E[i] := A[i, i + 1];
          if AP_FP_NEq(TAUI, 0) then
            begin

              //
              // Apply H from both sides to A
              //
              A[i, i + 1] := 1;

              //
              // Compute  x := tau * A * v  storing x in TAU
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  T[i_] := A[i_ + i1_, i + 1];
                end;
              SymmetricMatrixVectorMultiply(A, IsUpper, 0, i, T, TAUI, t3);
              APVMove(@TAU[0], 0, i, @t3[0], 1, i + 1);

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              v := 0.0;
              for i_ := 0 to i do
                begin
                  v := v + TAU[i_] * A[i_, i + 1];
                end;
              alpha := -0.5 * TAUI * v;
              for i_ := 0 to i do
                begin
                  TAU[i_] := TAU[i_] + alpha * A[i_, i + 1];
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  T[i_] := A[i_ + i1_, i + 1];
                end;
              APVMove(@t3[0], 1, i + 1, @TAU[0], 0, i);
              SymmetricRank2Update(A, IsUpper, 0, i, T, t3, t2, -1);
              A[i, i + 1] := E[i];
            end;
          d[i + 1] := A[i + 1, i + 1];
          TAU[i] := TAUI;
          Dec(i);
        end;
      d[0] := A[0, 0];
    end
  else
    begin

      //
      // Reduce the lower triangle of A
      //
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Generate elementary reflector H = E - tau * v * v'
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              T[i_] := A[i_ + i1_, i];
            end;
          GenerateReflection(T, n - i - 1, TAUI);
          i1_ := (1) - (i + 1);
          for i_ := i + 1 to n - 1 do
            begin
              A[i_, i] := T[i_ + i1_];
            end;
          E[i] := A[i + 1, i];
          if AP_FP_NEq(TAUI, 0) then
            begin

              //
              // Apply H from both sides to A
              //
              A[i + 1, i] := 1;

              //
              // Compute  x := tau * A * v  storing y in TAU
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  T[i_] := A[i_ + i1_, i];
                end;
              SymmetricMatrixVectorMultiply(A, IsUpper, i + 1, n - 1, T, TAUI, t2);
              APVMove(@TAU[0], i, n - 2, @t2[0], 1, n - i - 1);

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              i1_ := (i + 1) - (i);
              v := 0.0;
              for i_ := i to n - 2 do
                begin
                  v := v + TAU[i_] * A[i_ + i1_, i];
                end;
              alpha := -0.5 * TAUI * v;
              i1_ := (i + 1) - (i);
              for i_ := i to n - 2 do
                begin
                  TAU[i_] := TAU[i_] + alpha * A[i_ + i1_, i];
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  T[i_] := A[i_ + i1_, i];
                end;
              APVMove(@t2[0], 1, n - i - 1, @TAU[0], i, n - 2);
              SymmetricRank2Update(A, IsUpper, i + 1, n - 1, T, t2, t3, -1);
              A[i + 1, i] := E[i];
            end;
          d[i] := A[i, i];
          TAU[i] := TAUI;
          Inc(i);
        end;
      d[n - 1] := A[n - 1, n - 1];
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces symmetric matrix to a tridiagonal
  form.

  Input parameters:
  A       -   the result of a SMatrixTD subroutine
  N       -   size of matrix A.
  IsUpper -   storage format (a parameter of SMatrixTD subroutine)
  Tau     -   the result of a SMatrixTD subroutine

  Output parameters:
  Q       -   transformation matrix.
  array with elements [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure SMatrixTDUnpackQ(const A: TLMatrix; const n: TLInt;
  const IsUpper: Boolean; const TAU: TLVec; var q: TLMatrix);
var
  i   : TLInt;
  J   : TLInt;
  v   : TLVec;
  Work: TLVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  if n = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(q, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  SetLength(v, n + 1);
  SetLength(Work, n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          if i = J then
            begin
              q[i, J] := 1;
            end
          else
            begin
              q[i, J] := 0;
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // unpack Q
  //
  if IsUpper then
    begin
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (0) - (1);
          for i_ := 1 to i + 1 do
            begin
              v[i_] := A[i_ + i1_, i + 1];
            end;
          v[i + 1] := 1;
          ApplyReflectionFromTheLeft(q, TAU[i], v, 0, i, 0, n - 1, Work);
          Inc(i);
        end;
    end
  else
    begin
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              v[i_] := A[i_ + i1_, i];
            end;
          v[1] := 1;
          ApplyReflectionFromTheLeft(q, TAU[i], v, i + 1, n - 1, 0, n - 1, Work);
          Dec(i);
        end;
    end;
end;

(* ************************************************************************
  Reduction of a Hermitian matrix which is given  by  its  higher  or  lower
  triangular part to a real  tridiagonal  matrix  using  unitary  similarity
  transformation: Q'*A*Q = T.

  Input parameters:
  A       -   matrix to be transformed
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format. If IsUpper = True, then matrix A is  given
  by its upper triangle, and the lower triangle is not  used
  and not modified by the algorithm, and vice versa
  if IsUpper = False.

  Output parameters:
  A       -   matrices T and Q in  compact form (see lower)
  Tau     -   packed array of factors which are forming matrices H(i)
  array with elements [0..N-2].
  D       -   main diagonal of real symmetric matrix T.
  array with elements [0..N-1].
  E       -   secondary diagonal of real symmetric matrix T.
  array with elements [0..N-2].


  If IsUpper=True, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(n-2) . . . H(2) H(0).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a TLComplex scalar, and v is a TLComplex vector with
  v(i+1:n-1) = 0, v(i) = 1, v(0:i-1) is stored on exit in
  A(0:i-1,i+1), and tau in TAU(i).

  If IsUpper=False, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(0) H(2) . . . H(n-2).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a TLComplex scalar, and v is a TLComplex vector with
  v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) is stored on exit in A(i+2:n-1,i),
  and tau in TAU(i).

  The contents of A on exit are illustrated by the following examples
  with n = 5:

  if UPLO = 'U':                       if UPLO = 'L':

  (  d   e   v1  v2  v3 )              (  d                  )
  (      d   e   v2  v3 )              (  e   d              )
  (          d   e   v3 )              (  v0  e   d          )
  (              d   e  )              (  v0  v1  e   d      )
  (                  d  )              (  v0  v1  v2  e   d  )

  where d and e denote diagonal and off-diagonal elements of T, and vi
  denotes an element of the vector defining H(i).
  ************************************************************************ *)
procedure HMatrixTD(var A: TLComplexMatrix; n: TLInt; IsUpper: Boolean;
  var TAU: TLComplexVec; var d: TLVec; var E: TLVec);
var
  i    : TLInt;
  alpha: TLComplex;
  TAUI : TLComplex;
  v    : TLComplex;
  T    : TLComplexVec;
  t2   : TLComplexVec;
  t3   : TLComplexVec;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if n <= 0 then
    begin
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      Assert(AP_FP_Eq(A[i, i].Y, 0));
      Inc(i);
    end;
  if n > 1 then
    begin
      SetLength(TAU, n - 2 + 1);
      SetLength(E, n - 2 + 1);
    end;
  SetLength(d, n (* - 1 + 1 // optimized compiler *) );
  SetLength(T, n (* - 1 + 1 // optimized compiler *) );
  SetLength(t2, n (* - 1 + 1 // optimized compiler *) );
  SetLength(t3, n (* - 1 + 1 // optimized compiler *) );
  if IsUpper then
    begin

      //
      // Reduce the upper triangle of A
      //
      A[n - 1, n - 1] := C_Complex(A[n - 1, n - 1].X);
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Generate elementary reflector H = I+1 - tau * v * v'
          //
          alpha := A[i, i + 1];
          T[1] := alpha;
          if i >= 1 then
            begin
              i1_ := (0) - (2);
              for i_ := 2 to i + 1 do
                begin
                  T[i_] := A[i_ + i1_, i + 1];
                end;
            end;
          ComplexGenerateReflection(T, i + 1, TAUI);
          if i >= 1 then
            begin
              i1_ := (2) - (0);
              for i_ := 0 to i - 1 do
                begin
                  A[i_, i + 1] := T[i_ + i1_];
                end;
            end;
          alpha := T[1];
          E[i] := alpha.X;
          if C_NotEqualR(TAUI, 0) then
            begin

              //
              // Apply H(I+1) from both sides to A
              //
              A[i, i + 1] := C_Complex(1);

              //
              // Compute  x := tau * A * v  storing x in TAU
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  T[i_] := A[i_ + i1_, i + 1];
                end;
              HermitianMatrixVectorMultiply(A, IsUpper, 0, i, T, TAUI, t2);
              i1_ := (1) - (0);
              for i_ := 0 to i do
                begin
                  TAU[i_] := t2[i_ + i1_];
                end;

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              v := C_Complex(0.0);
              for i_ := 0 to i do
                begin
                  v := C_Add(v, C_Mul(Conj(TAU[i_]), A[i_, i + 1]));
                end;
              alpha := C_Opposite(C_Mul(C_MulR(TAUI, 0.5), v));
              for i_ := 0 to i do
                begin
                  TAU[i_] := C_Add(TAU[i_], C_Mul(alpha, A[i_, i + 1]));
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  T[i_] := A[i_ + i1_, i + 1];
                end;
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  t3[i_] := TAU[i_ + i1_];
                end;
              HermitianRank2Update(A, IsUpper, 0, i, T, t3, t2, C_Complex(-1));
            end
          else
            begin
              A[i, i] := C_Complex(A[i, i].X);
            end;
          A[i, i + 1] := C_Complex(E[i]);
          d[i + 1] := A[i + 1, i + 1].X;
          TAU[i] := TAUI;
          Dec(i);
        end;
      d[0] := A[0, 0].X;
    end
  else
    begin

      //
      // Reduce the lower triangle of A
      //
      A[0, 0] := C_Complex(A[0, 0].X);
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Generate elementary reflector H = I - tau * v * v'
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              T[i_] := A[i_ + i1_, i];
            end;
          ComplexGenerateReflection(T, n - i - 1, TAUI);
          i1_ := (1) - (i + 1);
          for i_ := i + 1 to n - 1 do
            begin
              A[i_, i] := T[i_ + i1_];
            end;
          E[i] := A[i + 1, i].X;
          if C_NotEqualR(TAUI, 0) then
            begin

              //
              // Apply H(i) from both sides to A(i+1:n,i+1:n)
              //
              A[i + 1, i] := C_Complex(1);

              //
              // Compute  x := tau * A * v  storing y in TAU
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  T[i_] := A[i_ + i1_, i];
                end;
              HermitianMatrixVectorMultiply(A, IsUpper, i + 1, n - 1, T, TAUI, t2);
              i1_ := (1) - (i);
              for i_ := i to n - 2 do
                begin
                  TAU[i_] := t2[i_ + i1_];
                end;

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              i1_ := (i + 1) - (i);
              v := C_Complex(0.0);
              for i_ := i to n - 2 do
                begin
                  v := C_Add(v, C_Mul(Conj(TAU[i_]), A[i_ + i1_, i]));
                end;
              alpha := C_Opposite(C_Mul(C_MulR(TAUI, 0.5), v));
              i1_ := (i + 1) - (i);
              for i_ := i to n - 2 do
                begin
                  TAU[i_] := C_Add(TAU[i_], C_Mul(alpha, A[i_ + i1_, i]));
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  T[i_] := A[i_ + i1_, i];
                end;
              i1_ := (i) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  t2[i_] := TAU[i_ + i1_];
                end;
              HermitianRank2Update(A, IsUpper, i + 1, n - 1, T, t2, t3,
                C_Complex(-1));
            end
          else
            begin
              A[i + 1, i + 1] := C_Complex(A[i + 1, i + 1].X);
            end;
          A[i + 1, i] := C_Complex(E[i]);
          d[i] := A[i, i].X;
          TAU[i] := TAUI;
          Inc(i);
        end;
      d[n - 1] := A[n - 1, n - 1].X;
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces a Hermitian matrix to a real  tridiagonal
  form.

  Input parameters:
  A       -   the result of a HMatrixTD subroutine
  N       -   size of matrix A.
  IsUpper -   storage format (a parameter of HMatrixTD subroutine)
  Tau     -   the result of a HMatrixTD subroutine

  Output parameters:
  Q       -   transformation matrix.
  array with elements [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure HMatrixTDUnpackQ(const A: TLComplexMatrix; const n: TLInt;
  const IsUpper: Boolean; const TAU: TLComplexVec; var q: TLComplexMatrix);
var
  i   : TLInt;
  J   : TLInt;
  v   : TLComplexVec;
  Work: TLComplexVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  if n = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(q, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  SetLength(v, n + 1);
  SetLength(Work, n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          if i = J then
            begin
              q[i, J] := C_Complex(1);
            end
          else
            begin
              q[i, J] := C_Complex(0);
            end;
          Inc(J);
        end;
      Inc(i);
    end;

  //
  // unpack Q
  //
  if IsUpper then
    begin
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (0) - (1);
          for i_ := 1 to i + 1 do
            begin
              v[i_] := A[i_ + i1_, i + 1];
            end;
          v[i + 1] := C_Complex(1);
          ComplexApplyReflectionFromTheLeft(q, TAU[i], v, 0, i, 0, n - 1, Work);
          Inc(i);
        end;
    end
  else
    begin
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              v[i_] := A[i_ + i1_, i];
            end;
          v[1] := C_Complex(1);
          ComplexApplyReflectionFromTheLeft(q, TAU[i], v, i + 1, n - 1, 0,
            n - 1, Work);
          Dec(i);
        end;
    end;
end;

(* ************************************************************************
  Base case for real QR
  ************************************************************************ *)
procedure RMatrixQRBaseCase(var A: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var T: TLVec;
  var TAU: TLVec);
var
  i    : TLInt;
  k    : TLInt;
  MinMN: TLInt;
  tmp  : TLFloat;
  i_   : TLInt;
  i1_  : TLInt;
begin
  MinMN := Min(M, n);

  //
  // Test the input arguments
  //
  k := MinMN;
  i := 0;
  while i <= k - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
      //
      i1_ := (i) - (1);
      for i_ := 1 to M - i do
        begin
          T[i_] := A[i_ + i1_, i];
        end;
      GenerateReflection(T, M - i, tmp);
      TAU[i] := tmp;
      i1_ := (1) - (i);
      for i_ := i to M - 1 do
        begin
          A[i_, i] := T[i_ + i1_];
        end;
      T[1] := 1;
      if i < n then
        begin

          //
          // Apply H(i) to A(i:m-1,i+1:n-1) from the left
          //
          ApplyReflectionFromTheLeft(A, TAU[i], T, i, M - 1, i + 1, n - 1, Work);
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Base case for real LQ
  ************************************************************************ *)
procedure RMatrixLQBaseCase(var A: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var T: TLVec;
  var TAU: TLVec);
var
  i    : TLInt;
  k    : TLInt;
  MinMN: TLInt;
  tmp  : TLFloat;
begin
  MinMN := Min(M, n);
  k := Min(M, n);
  i := 0;
  while i <= k - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i,i+1:n-1)
      //
      APVMove(@T[0], 1, n - i, @A[i][0], i, n - 1);
      GenerateReflection(T, n - i, tmp);
      TAU[i] := tmp;
      APVMove(@A[i][0], i, n - 1, @T[0], 1, n - i);
      T[1] := 1;
      if i < n then
        begin

          //
          // Apply H(i) to A(i+1:m,i:n) from the right
          //
          ApplyReflectionFromTheRight(A, TAU[i], T, i + 1, M - 1, i, n - 1, Work);
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Base case for TLComplex QR
  ************************************************************************ *)
procedure CMatrixQRBaseCase(var A: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var T: TLComplexVec;
  var TAU: TLComplexVec);
var
  i    : TLInt;
  k    : TLInt;
  MMI  : TLInt;
  MinMN: TLInt;
  tmp  : TLComplex;
  i_   : TLInt;
  i1_  : TLInt;
begin
  MinMN := Min(M, n);
  if MinMN <= 0 then
    begin
      Exit;
    end;

  //
  // Test the input arguments
  //
  k := Min(M, n);
  i := 0;
  while i <= k - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
      //
      MMI := M - i;
      i1_ := (i) - (1);
      for i_ := 1 to MMI do
        begin
          T[i_] := A[i_ + i1_, i];
        end;
      ComplexGenerateReflection(T, MMI, tmp);
      TAU[i] := tmp;
      i1_ := (1) - (i);
      for i_ := i to M - 1 do
        begin
          A[i_, i] := T[i_ + i1_];
        end;
      T[1] := C_Complex(1);
      if i < n - 1 then
        begin

          //
          // Apply H'(i) to A(i:m,i+1:n) from the left
          //
          ComplexApplyReflectionFromTheLeft(A, Conj(TAU[i]), T, i, M - 1, i + 1,
            n - 1, Work);
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Base case for TLComplex LQ
  ************************************************************************ *)
procedure CMatrixLQBaseCase(var A: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var T: TLComplexVec;
  var TAU: TLComplexVec);
var
  i    : TLInt;
  MinMN: TLInt;
  tmp  : TLComplex;
  i_   : TLInt;
  i1_  : TLInt;
begin
  MinMN := Min(M, n);
  if MinMN <= 0 then
    begin
      Exit;
    end;

  //
  // Test the input arguments
  //
  i := 0;
  while i <= MinMN - 1 do
    begin

      //
      // Generate elementary reflector H(i)
      //
      // NOTE: ComplexGenerateReflection() generates left reflector,
      // i.e. H which reduces x by applyiong from the left, but we
      // need RIGHT reflector. So we replace H=E-tau*v*v' by H^H,
      // which changes v to conj(v).
      //
      i1_ := (i) - (1);
      for i_ := 1 to n - i do
        begin
          T[i_] := Conj(A[i, i_ + i1_]);
        end;
      ComplexGenerateReflection(T, n - i, tmp);
      TAU[i] := tmp;
      i1_ := (1) - (i);
      for i_ := i to n - 1 do
        begin
          A[i, i_] := Conj(T[i_ + i1_]);
        end;
      T[1] := C_Complex(1);
      if i < M - 1 then
        begin

          //
          // Apply H'(i)
          //
          ComplexApplyReflectionFromTheRight(A, TAU[i], T, i + 1, M - 1, i,
            n - 1, Work);
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Generate block reflector:
  * fill unused parts of reflectors matrix by zeros
  * fill diagonal of reflectors matrix by ones
  * generate triangular factor T

  PARAMETERS:
  A           -   either LengthA*BlockSize (if ColumnwiseA) or
  BlockSize*LengthA (if not ColumnwiseA) matrix of
  elementary reflectors.
  Modified on exit.
  Tau         -   scalar factors
  ColumnwiseA -   reflectors are stored in rows or in columns
  LengthA     -   length of largest reflector
  BlockSize   -   number of reflectors
  T           -   array[BlockSize,2*BlockSize]. Left BlockSize*BlockSize
  submatrix stores triangular factor on exit.
  WORK        -   array[BlockSize]
  ************************************************************************ *)
procedure RMatrixBlockReflector(var A: TLMatrix; var TAU: TLVec;
  ColumnwiseA: Boolean; LengthA: TLInt; BlockSize: TLInt;
  var T: TLMatrix; var Work: TLVec);
var
  i  : TLInt;
  J  : TLInt;
  k  : TLInt;
  v  : TLFloat;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // fill beginning of new column with zeros,
  // load 1.0 in the first non-zero element
  //
  k := 0;
  while k <= BlockSize - 1 do
    begin
      if ColumnwiseA then
        begin
          i := 0;
          while i <= k - 1 do
            begin
              A[i, k] := 0;
              Inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= k - 1 do
            begin
              A[k, i] := 0;
              Inc(i);
            end;
        end;
      A[k, k] := 1;
      Inc(k);
    end;

  //
  // Calculate Gram matrix of A
  //
  i := 0;
  while i <= BlockSize - 1 do
    begin
      J := 0;
      while J <= BlockSize - 1 do
        begin
          T[i, BlockSize + J] := 0;
          Inc(J);
        end;
      Inc(i);
    end;
  k := 0;
  while k <= LengthA - 1 do
    begin
      J := 1;
      while J <= BlockSize - 1 do
        begin
          if ColumnwiseA then
            begin
              v := A[k, J];
              if AP_FP_NEq(v, 0) then
                begin
                  APVAdd(@T[J][0], BlockSize, BlockSize + J - 1, @A[k][0], 0, J - 1, v);
                end;
            end
          else
            begin
              v := A[J, k];
              if AP_FP_NEq(v, 0) then
                begin
                  i1_ := (0) - (BlockSize);
                  for i_ := BlockSize to BlockSize + J - 1 do
                    begin
                      T[J, i_] := T[J, i_] + v * A[i_ + i1_, k];
                    end;
                end;
            end;
          Inc(J);
        end;
      Inc(k);
    end;

  //
  // Prepare Y (stored in TmpA) and T (stored in TmpT)
  //
  k := 0;
  while k <= BlockSize - 1 do
    begin

      //
      // fill non-zero part of T, use pre-calculated Gram matrix
      //
      APVMove(@Work[0], 0, k - 1, @T[k][0], BlockSize, BlockSize + k - 1);
      i := 0;
      while i <= k - 1 do
        begin
          v := APVDotProduct(@T[i][0], i, k - 1, @Work[0], i, k - 1);
          T[i, k] := -TAU[k] * v;
          Inc(i);
        end;
      T[k, k] := -TAU[k];

      //
      // Rest of T is filled by zeros
      //
      i := k + 1;
      while i <= BlockSize - 1 do
        begin
          T[i, k] := 0;
          Inc(i);
        end;
      Inc(k);
    end;
end;

(* ************************************************************************
  Generate block reflector (TLComplex):
  * fill unused parts of reflectors matrix by zeros
  * fill diagonal of reflectors matrix by ones
  * generate triangular factor T
  ************************************************************************ *)
procedure CMatrixBlockReflector(var A: TLComplexMatrix;
  var TAU: TLComplexVec; ColumnwiseA: Boolean; LengthA: TLInt;
  BlockSize: TLInt; var T: TLComplexMatrix; var Work: TLComplexVec);
var
  i : TLInt;
  k : TLInt;
  v : TLComplex;
  i_: TLInt;
begin

  //
  // Prepare Y (stored in TmpA) and T (stored in TmpT)
  //
  k := 0;
  while k <= BlockSize - 1 do
    begin

      //
      // fill beginning of new column with zeros,
      // load 1.0 in the first non-zero element
      //
      if ColumnwiseA then
        begin
          i := 0;
          while i <= k - 1 do
            begin
              A[i, k] := C_Complex(0);
              Inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= k - 1 do
            begin
              A[k, i] := C_Complex(0);
              Inc(i);
            end;
        end;
      A[k, k] := C_Complex(1);

      //
      // fill non-zero part of T,
      //
      i := 0;
      while i <= k - 1 do
        begin
          if ColumnwiseA then
            begin
              v := C_Complex(0.0);
              for i_ := k to LengthA - 1 do
                begin
                  v := C_Add(v, C_Mul(Conj(A[i_, i]), A[i_, k]));
                end;
            end
          else
            begin
              v := C_Complex(0.0);
              for i_ := k to LengthA - 1 do
                begin
                  v := C_Add(v, C_Mul(A[i, i_], Conj(A[k, i_])));
                end;
            end;
          Work[i] := v;
          Inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          v := C_Complex(0.0);
          for i_ := i to k - 1 do
            begin
              v := C_Add(v, C_Mul(T[i, i_], Work[i_]));
            end;
          T[i, k] := C_Opposite(C_Mul(TAU[k], v));
          Inc(i);
        end;
      T[k, k] := C_Opposite(TAU[k]);

      //
      // Rest of T is filled by zeros
      //
      i := k + 1;
      while i <= BlockSize - 1 do
        begin
          T[i, k] := C_Complex(0);
          Inc(i);
        end;
      Inc(k);
    end;
end; 
