{ ****************************************************************************** }
{ * machine Learn support, writen by QQ 600585@qq.com                          * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
procedure SymmetricMatrixVectorMultiply(const A: TLearnFloat2DArray; IsUpper: Boolean;
  I1: TLearnInteger; I2: TLearnInteger; const X: TLearnFloatArray;
  Alpha: TLearnFloat; var Y: TLearnFloatArray);
var
  I: TLearnInteger;
  BA1: TLearnInteger;
  BA2: TLearnInteger;
  BY1: TLearnInteger;
  BY2: TLearnInteger;
  BX1: TLearnInteger;
  BX2: TLearnInteger;
  N: TLearnInteger;
  V: TLearnFloat;
begin
  N := I2 - I1 + 1;
  if N <= 0 then
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
  I := I1;
  while I <= I2 do
  begin
    Y[I - I1 + 1] := A[I, I] * X[I - I1 + 1];
    Inc(I);
  end;

  //
  // Add L*x + U*x
  //
  if IsUpper then
  begin
    I := I1;
    while I <= I2 - 1 do
    begin

      //
      // Add L*x to the result
      //
      V := X[I - I1 + 1];
      BY1 := I - I1 + 2;
      BY2 := N;
      BA1 := I + 1;
      BA2 := I2;
      APVAdd(@Y[0], BY1, BY2, @A[I][0], BA1, BA2, V);

      //
      // Add U*x to the result
      //
      BX1 := I - I1 + 2;
      BX2 := N;
      BA1 := I + 1;
      BA2 := I2;
      V := APVDotProduct(@X[0], BX1, BX2, @A[I][0], BA1, BA2);
      Y[I - I1 + 1] := Y[I - I1 + 1] + V;
      Inc(I);
    end;
  end
  else
  begin
    I := I1 + 1;
    while I <= I2 do
    begin

      //
      // Add L*x to the result
      //
      BX1 := 1;
      BX2 := I - I1;
      BA1 := I1;
      BA2 := I - 1;
      V := APVDotProduct(@X[0], BX1, BX2, @A[I][0], BA1, BA2);
      Y[I - I1 + 1] := Y[I - I1 + 1] + V;

      //
      // Add U*x to the result
      //
      V := X[I - I1 + 1];
      BY1 := 1;
      BY2 := I - I1;
      BA1 := I1;
      BA2 := I - 1;
      APVAdd(@Y[0], BY1, BY2, @A[I][0], BA1, BA2, V);
      Inc(I);
    end;
  end;
  APVMul(@Y[0], 1, N, Alpha);
end;

procedure SymmetricRank2Update(var A: TLearnFloat2DArray; IsUpper: Boolean;
  I1: TLearnInteger; I2: TLearnInteger; const X: TLearnFloatArray;
  const Y: TLearnFloatArray; var T: TLearnFloatArray; Alpha: TLearnFloat);
var
  I: TLearnInteger;
  TP1: TLearnInteger;
  TP2: TLearnInteger;
  V: TLearnFloat;
begin
  if IsUpper then
  begin
    I := I1;
    while I <= I2 do
    begin
      TP1 := I + 1 - I1;
      TP2 := I2 - I1 + 1;
      V := X[I + 1 - I1];
      APVMove(@T[0], TP1, TP2, @Y[0], TP1, TP2, V);
      V := Y[I + 1 - I1];
      APVAdd(@T[0], TP1, TP2, @X[0], TP1, TP2, V);
      APVMul(@T[0], TP1, TP2, Alpha);
      APVAdd(@A[I][0], I, I2, @T[0], TP1, TP2);
      Inc(I);
    end;
  end
  else
  begin
    I := I1;
    while I <= I2 do
    begin
      TP1 := 1;
      TP2 := I + 1 - I1;
      V := X[I + 1 - I1];
      APVMove(@T[0], TP1, TP2, @Y[0], TP1, TP2, V);
      V := Y[I + 1 - I1];
      APVAdd(@T[0], TP1, TP2, @X[0], TP1, TP2, V);
      APVMul(@T[0], TP1, TP2, Alpha);
      APVAdd(@A[I][0], I1, I, @T[0], TP1, TP2);
      Inc(I);
    end;
  end;
end;

(* ************************************************************************
  Generation of an elementary TLearnComplex reflection transformation

  The subroutine generates elementary TLearnComplex reflection H of  order  N,  so
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
procedure ComplexGenerateReflection(var X: TLearnComplexArray; N: TLearnInteger;
  var Tau: TLearnComplex);
var
  J: TLearnInteger;
  ALPHA: TLearnComplex;
  ALPHI: TLearnFloat;
  ALPHR: TLearnFloat;
  BETA: TLearnFloat;
  XNORM: TLearnFloat;
  MX: TLearnFloat;
  T: TLearnComplex;
  S: TLearnFloat;
  V: TLearnComplex;
  i_: TLearnInteger;
begin
  if N <= 0 then
  begin
    Tau := C_Complex(0);
    Exit;
  end;

  //
  // Scale if needed (to avoid overflow/underflow during intermediate
  // calculations).
  //
  MX := 0;
  J := 1;
  while J <= N do
  begin
    MX := Max(AbsComplex(X[J]), MX);
    Inc(J);
  end;
  S := 1;
  if AP_FP_Neq(MX, 0) then
  begin
    if AP_FP_Less(MX, 1) then
    begin
      S := Sqrt(MinRealNumber);
      V := C_Complex(1 / S);
      for i_ := 1 to N do
      begin
        X[i_] := C_Mul(V, X[i_]);
      end;
    end
    else
    begin
      S := Sqrt(MaxRealNumber);
      V := C_Complex(1 / S);
      for i_ := 1 to N do
      begin
        X[i_] := C_Mul(V, X[i_]);
      end;
    end;
  end;

  //
  // calculate
  //
  ALPHA := X[1];
  MX := 0;
  J := 2;
  while J <= N do
  begin
    MX := Max(AbsComplex(X[J]), MX);
    Inc(J);
  end;
  XNORM := 0;
  if AP_FP_Neq(MX, 0) then
  begin
    J := 2;
    while J <= N do
    begin
      T := C_DivR(X[J], MX);
      XNORM := XNORM + C_Mul(T, Conj(T)).X;
      Inc(J);
    end;
    XNORM := Sqrt(XNORM) * MX;
  end;
  ALPHR := ALPHA.X;
  ALPHI := ALPHA.Y;
  if AP_FP_Eq(XNORM, 0) and AP_FP_Eq(ALPHI, 0) then
  begin
    Tau := C_Complex(0);
    X[1] := C_MulR(X[1], S);
    Exit;
  end;
  MX := Max(AbsReal(ALPHR), AbsReal(ALPHI));
  MX := Max(MX, AbsReal(XNORM));
  BETA := -MX * Sqrt(AP_Sqr(ALPHR / MX) + AP_Sqr(ALPHI / MX) +
    AP_Sqr(XNORM / MX));
  if AP_FP_Less(ALPHR, 0) then
  begin
    BETA := -BETA;
  end;
  Tau.X := (BETA - ALPHR) / BETA;
  Tau.Y := -ALPHI / BETA;
  ALPHA := C_RDiv(1, C_SubR(ALPHA, BETA));
  if N > 1 then
  begin
    for i_ := 2 to N do
    begin
      X[i_] := C_Mul(ALPHA, X[i_]);
    end;
  end;
  ALPHA := C_Complex(BETA);
  X[1] := ALPHA;

  //
  // Scale back
  //
  X[1] := C_MulR(X[1], S);
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
procedure ComplexApplyReflectionFromTheLeft(var C: TLearnComplex2DArray;
  Tau: TLearnComplex; const V: TLearnComplexArray; M1: TLearnInteger; M2: TLearnInteger;
  N1: TLearnInteger; N2: TLearnInteger; var WORK: TLearnComplexArray);
var
  T: TLearnComplex;
  I: TLearnInteger;
  VM: TLearnInteger;
  i_: TLearnInteger;
begin
  if C_EqualR(Tau, 0) or (N1 > N2) or (M1 > M2) then
  begin
    Exit;
  end;

  //
  // w := C^T * conj(v)
  //
  VM := M2 - M1 + 1;
  I := N1;
  while I <= N2 do
  begin
    WORK[I] := C_Complex(0);
    Inc(I);
  end;
  I := M1;
  while I <= M2 do
  begin
    T := Conj(V[I + 1 - M1]);
    for i_ := N1 to N2 do
    begin
      WORK[i_] := C_Add(WORK[i_], C_Mul(T, C[I, i_]));
    end;
    Inc(I);
  end;

  //
  // C := C - tau * v * w^T
  //
  I := M1;
  while I <= M2 do
  begin
    T := C_Mul(V[I - M1 + 1], Tau);
    for i_ := N1 to N2 do
    begin
      C[I, i_] := C_Sub(C[I, i_], C_Mul(T, WORK[i_]));
    end;
    Inc(I);
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
procedure ComplexApplyReflectionFromTheRight(var C: TLearnComplex2DArray;
  Tau: TLearnComplex; var V: TLearnComplexArray; M1: TLearnInteger; M2: TLearnInteger;
  N1: TLearnInteger; N2: TLearnInteger; var WORK: TLearnComplexArray);
var
  T: TLearnComplex;
  I: TLearnInteger;
  VM: TLearnInteger;
  i_: TLearnInteger;
  i1_: TLearnInteger;
begin
  if C_EqualR(Tau, 0) or (N1 > N2) or (M1 > M2) then
  begin
    Exit;
  end;

  //
  // w := C * v
  //
  VM := N2 - N1 + 1;
  I := M1;
  while I <= M2 do
  begin
    i1_ := (1) - (N1);
    T := C_Complex(0.0);
    for i_ := N1 to N2 do
    begin
      T := C_Add(T, C_Mul(C[I, i_], V[i_ + i1_]));
    end;
    WORK[I] := T;
    Inc(I);
  end;

  //
  // C := C - w * conj(v^T)
  //
  for i_ := 1 to VM do
  begin
    V[i_] := Conj(V[i_]);
  end;
  I := M1;
  while I <= M2 do
  begin
    T := C_Mul(WORK[I], Tau);
    i1_ := (1) - (N1);
    for i_ := N1 to N2 do
    begin
      C[I, i_] := C_Sub(C[I, i_], C_Mul(T, V[i_ + i1_]));
    end;
    Inc(I);
  end;
  for i_ := 1 to VM do
  begin
    V[i_] := Conj(V[i_]);
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
procedure GenerateReflection(var X: TLearnFloatArray; N: TLearnInteger;
  var Tau: TLearnFloat);
var
  J: TLearnInteger;
  Alpha: TLearnFloat;
  XNORM: TLearnFloat;
  V: TLearnFloat;
  Beta: TLearnFloat;
  MX: TLearnFloat;
  S: TLearnFloat;
begin
  if N <= 1 then
  begin
    Tau := 0;
    Exit;
  end;

  //
  // Scale if needed (to avoid overflow/underflow during intermediate
  // calculations).
  //
  MX := 0;
  J := 1;
  while J <= N do
  begin
    MX := Max(AbsReal(X[J]), MX);
    Inc(J);
  end;
  S := 1;
  if AP_FP_Neq(MX, 0) then
  begin
    if AP_FP_Less_Eq(MX, MinRealNumber / MachineEpsilon) then
    begin
      S := MinRealNumber / MachineEpsilon;
      V := 1 / S;
      APVMul(@X[0], 1, N, V);
      MX := MX * V;
    end
    else
    begin
      if AP_FP_Greater_Eq(MX, MaxRealNumber * MachineEpsilon) then
      begin
        S := MaxRealNumber * MachineEpsilon;
        V := 1 / S;
        APVMul(@X[0], 1, N, V);
        MX := MX * V;
      end;
    end;
  end;

  //
  // XNORM = DNRM2( N-1, X, INCX )
  //
  Alpha := X[1];
  XNORM := 0;
  if AP_FP_Neq(MX, 0) then
  begin
    J := 2;
    while J <= N do
    begin
      XNORM := XNORM + AP_Sqr(X[J] / MX);
      Inc(J);
    end;
    XNORM := Sqrt(XNORM) * MX;
  end;
  if AP_FP_Eq(XNORM, 0) then
  begin

    //
    // H  =  I
    //
    Tau := 0;
    X[1] := X[1] * S;
    Exit;
  end;

  //
  // general case
  //
  MX := Max(AbsReal(Alpha), AbsReal(XNORM));
  Beta := -MX * Sqrt(AP_Sqr(Alpha / MX) + AP_Sqr(XNORM / MX));
  if AP_FP_Less(Alpha, 0) then
  begin
    Beta := -Beta;
  end;
  Tau := (Beta - Alpha) / Beta;
  V := 1 / (Alpha - Beta);
  APVMul(@X[0], 2, N, V);
  X[1] := Beta;

  //
  // Scale back outputs
  //
  X[1] := X[1] * S;
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
procedure ApplyReflectionFromTheLeft(var C: TLearnFloat2DArray; Tau: TLearnFloat;
  const V: TLearnFloatArray; M1: TLearnInteger; M2: TLearnInteger;
  N1: TLearnInteger; N2: TLearnInteger; var WORK: TLearnFloatArray);
var
  T: TLearnFloat;
  I: TLearnInteger;
  VM: TLearnInteger;
begin
  if AP_FP_Eq(Tau, 0) or (N1 > N2) or (M1 > M2) then
  begin
    Exit;
  end;

  //
  // w := C' * v
  //
  VM := M2 - M1 + 1;
  I := N1;
  while I <= N2 do
  begin
    WORK[I] := 0;
    Inc(I);
  end;
  I := M1;
  while I <= M2 do
  begin
    T := V[I + 1 - M1];
    APVAdd(@WORK[0], N1, N2, @C[I][0], N1, N2, T);
    Inc(I);
  end;

  //
  // C := C - tau * v * w'
  //
  I := M1;
  while I <= M2 do
  begin
    T := V[I - M1 + 1] * Tau;
    APVSub(@C[I][0], N1, N2, @WORK[0], N1, N2, T);
    Inc(I);
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
procedure ApplyReflectionFromTheRight(var C: TLearnFloat2DArray; Tau: TLearnFloat;
  const V: TLearnFloatArray; M1: TLearnInteger; M2: TLearnInteger;
  N1: TLearnInteger; N2: TLearnInteger; var WORK: TLearnFloatArray);
var
  T: TLearnFloat;
  I: TLearnInteger;
  VM: TLearnInteger;
begin
  if AP_FP_Eq(Tau, 0) or (N1 > N2) or (M1 > M2) then
  begin
    Exit;
  end;
  VM := N2 - N1 + 1;
  I := M1;
  while I <= M2 do
  begin
    T := APVDotProduct(@C[I][0], N1, N2, @V[0], 1, VM);
    T := T * Tau;
    APVSub(@C[I][0], N1, N2, @V[0], 1, VM, T);
    Inc(I);
  end;
end;

procedure HermitianMatrixVectorMultiply(const A: TLearnComplex2DArray;
  IsUpper: Boolean; I1: TLearnInteger; I2: TLearnInteger;
  const X: TLearnComplexArray; Alpha: TLearnComplex; var Y: TLearnComplexArray);
var
  I: TLearnInteger;
  BA1: TLearnInteger;
  BA2: TLearnInteger;
  BY1: TLearnInteger;
  BY2: TLearnInteger;
  BX1: TLearnInteger;
  BX2: TLearnInteger;
  N: TLearnInteger;
  V: TLearnComplex;
  i_: TLearnInteger;
  i1_: TLearnInteger;
begin
  N := I2 - I1 + 1;
  if N <= 0 then
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
  I := I1;
  while I <= I2 do
  begin
    Y[I - I1 + 1] := C_Mul(A[I, I], X[I - I1 + 1]);
    Inc(I);
  end;

  //
  // Add L*x + U*x
  //
  if IsUpper then
  begin
    I := I1;
    while I <= I2 - 1 do
    begin

      //
      // Add L*x to the result
      //
      V := X[I - I1 + 1];
      BY1 := I - I1 + 2;
      BY2 := N;
      BA1 := I + 1;
      BA2 := I2;
      i1_ := (BA1) - (BY1);
      for i_ := BY1 to BY2 do
      begin
        Y[i_] := C_Add(Y[i_], C_Mul(V, Conj(A[I, i_ + i1_])));
      end;

      //
      // Add U*x to the result
      //
      BX1 := I - I1 + 2;
      BX2 := N;
      BA1 := I + 1;
      BA2 := I2;
      i1_ := (BA1) - (BX1);
      V := C_Complex(0.0);
      for i_ := BX1 to BX2 do
      begin
        V := C_Add(V, C_Mul(X[i_], A[I, i_ + i1_]));
      end;
      Y[I - I1 + 1] := C_Add(Y[I - I1 + 1], V);
      Inc(I);
    end;
  end
  else
  begin
    I := I1 + 1;
    while I <= I2 do
    begin

      //
      // Add L*x to the result
      //
      BX1 := 1;
      BX2 := I - I1;
      BA1 := I1;
      BA2 := I - 1;
      i1_ := (BA1) - (BX1);
      V := C_Complex(0.0);
      for i_ := BX1 to BX2 do
      begin
        V := C_Add(V, C_Mul(X[i_], A[I, i_ + i1_]));
      end;
      Y[I - I1 + 1] := C_Add(Y[I - I1 + 1], V);

      //
      // Add U*x to the result
      //
      V := X[I - I1 + 1];
      BY1 := 1;
      BY2 := I - I1;
      BA1 := I1;
      BA2 := I - 1;
      i1_ := (BA1) - (BY1);
      for i_ := BY1 to BY2 do
      begin
        Y[i_] := C_Add(Y[i_], C_Mul(V, Conj(A[I, i_ + i1_])));
      end;
      Inc(I);
    end;
  end;
  for i_ := 1 to N do
  begin
    Y[i_] := C_Mul(Alpha, Y[i_]);
  end;
end;

procedure HermitianRank2Update(var A: TLearnComplex2DArray; IsUpper: Boolean;
  I1: TLearnInteger; I2: TLearnInteger; const X: TLearnComplexArray;
  const Y: TLearnComplexArray; var T: TLearnComplexArray; Alpha: TLearnComplex);
var
  I: TLearnInteger;
  TP1: TLearnInteger;
  TP2: TLearnInteger;
  V: TLearnComplex;
  i_: TLearnInteger;
  i1_: TLearnInteger;
begin
  if IsUpper then
  begin
    I := I1;
    while I <= I2 do
    begin
      TP1 := I + 1 - I1;
      TP2 := I2 - I1 + 1;
      V := C_Mul(Alpha, X[I + 1 - I1]);
      for i_ := TP1 to TP2 do
      begin
        T[i_] := C_Mul(V, Conj(Y[i_]));
      end;
      V := C_Mul(Conj(Alpha), Y[I + 1 - I1]);
      for i_ := TP1 to TP2 do
      begin
        T[i_] := C_Add(T[i_], C_Mul(V, Conj(X[i_])));
      end;
      i1_ := (TP1) - (I);
      for i_ := I to I2 do
      begin
        A[I, i_] := C_Add(A[I, i_], T[i_ + i1_]);
      end;
      Inc(I);
    end;
  end
  else
  begin
    I := I1;
    while I <= I2 do
    begin
      TP1 := 1;
      TP2 := I + 1 - I1;
      V := C_Mul(Alpha, X[I + 1 - I1]);
      for i_ := TP1 to TP2 do
      begin
        T[i_] := C_Mul(V, Conj(Y[i_]));
      end;
      V := C_Mul(Conj(Alpha), Y[I + 1 - I1]);
      for i_ := TP1 to TP2 do
      begin
        T[i_] := C_Add(T[i_], C_Mul(V, Conj(X[i_])));
      end;
      i1_ := (TP1) - (I1);
      for i_ := I1 to I do
      begin
        A[I, i_] := C_Add(A[I, i_], T[i_ + i1_]);
      end;
      Inc(I);
    end;
  end;
end;

procedure RMatrixQR(var A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; var Tau: TLearnFloatArray); forward;
procedure RMatrixLQ(var A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; var Tau: TLearnFloatArray); forward;
procedure CMatrixQR(var A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger; var Tau: TLearnComplexArray); forward;
procedure CMatrixLQ(var A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger; var Tau: TLearnComplexArray); forward;
procedure RMatrixQRUnpackQ(const A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; const Tau: TLearnFloatArray; QColumns: TLearnInteger; var Q: TLearnFloat2DArray); forward;

procedure RMatrixQRUnpackR(const A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; var R: TLearnFloat2DArray); forward;
procedure RMatrixLQUnpackQ(const A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; const Tau: TLearnFloatArray; QRows: TLearnInteger; var Q: TLearnFloat2DArray); forward;

procedure RMatrixLQUnpackL(const A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; var L: TLearnFloat2DArray); forward;
procedure CMatrixQRUnpackQ(const A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger; const Tau: TLearnComplexArray; QColumns: TLearnInteger; var Q: TLearnComplex2DArray); forward;
procedure CMatrixQRUnpackR(const A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger; var R: TLearnComplex2DArray); forward;
procedure CMatrixLQUnpackQ(const A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger; const Tau: TLearnComplexArray; QRows: TLearnInteger; var Q: TLearnComplex2DArray); forward;
procedure CMatrixLQUnpackL(const A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger; var L: TLearnComplex2DArray); forward;
procedure RMatrixBD(var A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; var TauQ: TLearnFloatArray; var TauP: TLearnFloatArray); forward;
procedure RMatrixBDUnpackQ(const QP: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger; const TauQ: TLearnFloatArray; QColumns: TLearnInteger; var Q: TLearnFloat2DArray); forward;

procedure RMatrixBDMultiplyByQ(const QP: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const TauQ: TLearnFloatArray; var Z: TLearnFloat2DArray;
  ZRows: TLearnInteger; ZColumns: TLearnInteger; FromTheRight: Boolean; DoTranspose: Boolean); forward;

procedure RMatrixBDUnpackPT(const QP: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const TauP: TLearnFloatArray; PTRows: TLearnInteger; var PT: TLearnFloat2DArray); forward;

procedure RMatrixBDMultiplyByP(const QP: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const TauP: TLearnFloatArray; var Z: TLearnFloat2DArray;
  ZRows: TLearnInteger; ZColumns: TLearnInteger; FromTheRight: Boolean; DoTranspose: Boolean); forward;

procedure RMatrixBDUnpackDiagonals(const B: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var IsUpper: Boolean; var D: TLearnFloatArray; var E: TLearnFloatArray); forward;

procedure RMatrixHessenberg(var A: TLearnFloat2DArray; N: TLearnInteger; var Tau: TLearnFloatArray); forward;

procedure RMatrixHessenbergUnpackQ(const A: TLearnFloat2DArray; N: TLearnInteger; const Tau: TLearnFloatArray; var Q: TLearnFloat2DArray); forward;
procedure RMatrixHessenbergUnpackH(const A: TLearnFloat2DArray; N: TLearnInteger; var H: TLearnFloat2DArray); forward;
procedure SMatrixTD(var A: TLearnFloat2DArray; N: TLearnInteger; IsUpper: Boolean; var Tau: TLearnFloatArray; var D: TLearnFloatArray; var E: TLearnFloatArray); forward;
procedure SMatrixTDUnpackQ(const A: TLearnFloat2DArray; const N: TLearnInteger; const IsUpper: Boolean; const Tau: TLearnFloatArray; var Q: TLearnFloat2DArray); forward;
procedure HMatrixTD(var A: TLearnComplex2DArray; N: TLearnInteger; IsUpper: Boolean; var Tau: TLearnComplexArray; var D: TLearnFloatArray; var E: TLearnFloatArray); forward;
procedure HMatrixTDUnpackQ(const A: TLearnComplex2DArray; const N: TLearnInteger; const IsUpper: Boolean; const Tau: TLearnComplexArray; var Q: TLearnComplex2DArray); forward;

procedure RMatrixQRBaseCase(var A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnFloatArray; var T: TLearnFloatArray;
  var Tau: TLearnFloatArray); forward;
procedure RMatrixLQBaseCase(var A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnFloatArray; var T: TLearnFloatArray;
  var Tau: TLearnFloatArray); forward;
procedure CMatrixQRBaseCase(var A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnComplexArray; var T: TLearnComplexArray;
  var Tau: TLearnComplexArray); forward;
procedure CMatrixLQBaseCase(var A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnComplexArray; var T: TLearnComplexArray;
  var Tau: TLearnComplexArray); forward;
procedure RMatrixBlockReflector(var A: TLearnFloat2DArray; var Tau: TLearnFloatArray;
  ColumnwiseA: Boolean; LengthA: TLearnInteger; BlockSize: TLearnInteger;
  var T: TLearnFloat2DArray; var WORK: TLearnFloatArray); forward;
procedure CMatrixBlockReflector(var A: TLearnComplex2DArray;
  var Tau: TLearnComplexArray; ColumnwiseA: Boolean; LengthA: TLearnInteger;
  BlockSize: TLearnInteger; var T: TLearnComplex2DArray;
  var WORK: TLearnComplexArray); forward;

(* ************************************************************************
  QR decomposition of a rectangular matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and R in compact form (see below).
  Tau -   array of scalar factors which are used to form
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
procedure RMatrixQR(var A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger;
  var Tau: TLearnFloatArray);
var
  WORK      : TLearnFloatArray;
  T         : TLearnFloatArray;
  TauBuf    : TLearnFloatArray;
  MinMN     : TLearnInteger;
  TmpA      : TLearnFloat2DArray;
  TmpT      : TLearnFloat2DArray;
  TmpR      : TLearnFloat2DArray;
  BlockStart: TLearnInteger;
  BlockSize : TLearnInteger;
  RowsCount : TLearnInteger;
  I         : TLearnInteger;
  J         : TLearnInteger;
  K         : TLearnInteger;
  V         : TLearnFloat;
  i_        : TLearnInteger;
  i1_       : TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, N);
  SetLength(WORK, Max(M, N) + 1);
  SetLength(T, Max(M, N) + 1);
  SetLength(Tau, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASBlockSize(A));
  SetLength(TmpT, ABLASBlockSize(A), 2 * ABLASBlockSize(A));
  SetLength(TmpR, 2 * ABLASBlockSize(A), N);

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
      RMatrixQRBaseCase(TmpA, RowsCount, BlockSize, WORK, T, TauBuf);
      RMatrixCopy(RowsCount, BlockSize, TmpA, 0, 0, A, BlockStart, BlockStart);
      APVMove(@Tau[0], BlockStart, BlockStart + BlockSize - 1, @TauBuf[0], 0,
        BlockSize - 1);

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= N - 1 then
        begin
          if (N - BlockStart - BlockSize >= 2 * ABLASBlockSize(A)) or
            (RowsCount >= 4 * ABLASBlockSize(A)) then
            begin

              //
              // Prepare block reflector
              //
              RMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
                TmpT, WORK);

              //
              // Multiply the rest of A by Q'.
              //
              // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
              // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
              //
              RMatrixGEMM(BlockSize, N - BlockStart - BlockSize, RowsCount, 1.0, TmpA,
                0, 0, 1, A, BlockStart, BlockStart + BlockSize, 0, 0.0, TmpR, 0, 0);
              RMatrixGEMM(BlockSize, N - BlockStart - BlockSize, BlockSize, 1.0, TmpT,
                0, 0, 1, TmpR, 0, 0, 0, 0.0, TmpR, BlockSize, 0);
              RMatrixGEMM(RowsCount, N - BlockStart - BlockSize, BlockSize, 1.0, TmpA,
                0, 0, 0, TmpR, BlockSize, 0, 0, 1.0, A, BlockStart,
                BlockStart + BlockSize);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              I := 0;
              while I <= BlockSize - 1 do
                begin
                  i1_ := (I) - (1);
                  for i_ := 1 to RowsCount - I do
                    begin
                      T[i_] := TmpA[i_ + i1_, I];
                    end;
                  T[1] := 1;
                  ApplyReflectionFromTheLeft(A, TauBuf[I], T, BlockStart + I, M - 1,
                    BlockStart + BlockSize, N - 1, WORK);
                  Inc(I);
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
  Tau -   array of scalar factors which are used to form
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
procedure RMatrixLQ(var A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger;
  var Tau: TLearnFloatArray);
var
  WORK        : TLearnFloatArray;
  T           : TLearnFloatArray;
  TauBuf      : TLearnFloatArray;
  MinMN       : TLearnInteger;
  TmpA        : TLearnFloat2DArray;
  TmpT        : TLearnFloat2DArray;
  TmpR        : TLearnFloat2DArray;
  BlockStart  : TLearnInteger;
  BlockSize   : TLearnInteger;
  ColumnsCount: TLearnInteger;
  I           : TLearnInteger;
  J           : TLearnInteger;
  K           : TLearnInteger;
  V           : TLearnFloat;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, N);
  SetLength(WORK, Max(M, N) + 1);
  SetLength(T, Max(M, N) + 1);
  SetLength(Tau, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASBlockSize(A), N);
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
      ColumnsCount := N - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      RMatrixCopy(BlockSize, ColumnsCount, A, BlockStart, BlockStart, TmpA, 0, 0);
      RMatrixLQBaseCase(TmpA, BlockSize, ColumnsCount, WORK, T, TauBuf);
      RMatrixCopy(BlockSize, ColumnsCount, TmpA, 0, 0, A, BlockStart, BlockStart);
      APVMove(@Tau[0], BlockStart, BlockStart + BlockSize - 1, @TauBuf[0], 0,
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
                TmpT, WORK);

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
              I := 0;
              while I <= BlockSize - 1 do
                begin
                  APVMove(@T[0], 1, ColumnsCount - I, @TmpA[I][0], I, ColumnsCount - 1);
                  T[1] := 1;
                  ApplyReflectionFromTheRight(A, TauBuf[I], T, BlockStart + BlockSize,
                    M - 1, BlockStart + I, N - 1, WORK);
                  Inc(I);
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
  QR decomposition of a rectangular TLearnComplex matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and R in compact form
  Tau -   array of scalar factors which are used to form matrix Q. Array
  whose indexes range within [0.. Min(M,N)-1]

  Matrix A is represented as A = QR, where Q is an orthogonal matrix of size
  MxM, R - upper triangular (or upper trapezoid) matrix of size MxN.
  ************************************************************************ *)
procedure CMatrixQR(var A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger;
  var Tau: TLearnComplexArray);
var
  WORK      : TLearnComplexArray;
  T         : TLearnComplexArray;
  TauBuf    : TLearnComplexArray;
  MinMN     : TLearnInteger;
  TmpA      : TLearnComplex2DArray;
  TmpT      : TLearnComplex2DArray;
  TmpR      : TLearnComplex2DArray;
  BlockStart: TLearnInteger;
  BlockSize : TLearnInteger;
  RowsCount : TLearnInteger;
  I         : TLearnInteger;
  J         : TLearnInteger;
  K         : TLearnInteger;
  V         : TLearnComplex;
  i_        : TLearnInteger;
  i1_       : TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, N);
  SetLength(WORK, Max(M, N) + 1);
  SetLength(T, Max(M, N) + 1);
  SetLength(Tau, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASComplexBlockSize(A));
  SetLength(TmpT, ABLASComplexBlockSize(A), ABLASComplexBlockSize(A));
  SetLength(TmpR, 2 * ABLASComplexBlockSize(A), N);

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
      CMatrixQRBaseCase(TmpA, RowsCount, BlockSize, WORK, T, TauBuf);
      CMatrixCopy(RowsCount, BlockSize, TmpA, 0, 0, A, BlockStart, BlockStart);
      i1_ := (0) - (BlockStart);
      for i_ := BlockStart to BlockStart + BlockSize - 1 do
        begin
          Tau[i_] := TauBuf[i_ + i1_];
        end;

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= N - 1 then
        begin
          if N - BlockStart - BlockSize >= 2 * ABLASComplexBlockSize(A) then
            begin

              //
              // Prepare block reflector
              //
              CMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
                TmpT, WORK);

              //
              // Multiply the rest of A by Q'.
              //
              // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
              // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
              //
              CMatrixGEMM(BlockSize, N - BlockStart - BlockSize, RowsCount,
                C_Complex(1.0), TmpA, 0, 0, 2, A, BlockStart, BlockStart + BlockSize,
                0, C_Complex(0.0), TmpR, 0, 0);
              CMatrixGEMM(BlockSize, N - BlockStart - BlockSize, BlockSize,
                C_Complex(1.0), TmpT, 0, 0, 2, TmpR, 0, 0, 0, C_Complex(0.0), TmpR,
                BlockSize, 0);
              CMatrixGEMM(RowsCount, N - BlockStart - BlockSize, BlockSize,
                C_Complex(1.0), TmpA, 0, 0, 0, TmpR, BlockSize, 0, 0, C_Complex(1.0),
                A, BlockStart, BlockStart + BlockSize);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              I := 0;
              while I <= BlockSize - 1 do
                begin
                  i1_ := (I) - (1);
                  for i_ := 1 to RowsCount - I do
                    begin
                      T[i_] := TmpA[i_ + i1_, I];
                    end;
                  T[1] := C_Complex(1);
                  ComplexApplyReflectionFromTheLeft(A, Conj(TauBuf[I]), T,
                    BlockStart + I, M - 1, BlockStart + BlockSize, N - 1, WORK);
                  Inc(I);
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
  LQ decomposition of a rectangular TLearnComplex matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and L in compact form
  Tau -   array of scalar factors which are used to form matrix Q. Array
  whose indexes range within [0.. Min(M,N)-1]

  Matrix A is represented as A = LQ, where Q is an orthogonal matrix of size
  MxM, L - lower triangular (or lower trapezoid) matrix of size MxN.
  ************************************************************************ *)
procedure CMatrixLQ(var A: TLearnComplex2DArray; M: TLearnInteger; N: TLearnInteger;
  var Tau: TLearnComplexArray);
var
  WORK        : TLearnComplexArray;
  T           : TLearnComplexArray;
  TauBuf      : TLearnComplexArray;
  MinMN       : TLearnInteger;
  TmpA        : TLearnComplex2DArray;
  TmpT        : TLearnComplex2DArray;
  TmpR        : TLearnComplex2DArray;
  BlockStart  : TLearnInteger;
  BlockSize   : TLearnInteger;
  ColumnsCount: TLearnInteger;
  I           : TLearnInteger;
  J           : TLearnInteger;
  K           : TLearnInteger;
  V           : TLearnComplex;
  i_          : TLearnInteger;
  i1_         : TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, N);
  SetLength(WORK, Max(M, N) + 1);
  SetLength(T, Max(M, N) + 1);
  SetLength(Tau, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASComplexBlockSize(A), N);
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
      ColumnsCount := N - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(BlockSize, ColumnsCount, A, BlockStart, BlockStart, TmpA, 0, 0);
      CMatrixLQBaseCase(TmpA, BlockSize, ColumnsCount, WORK, T, TauBuf);
      CMatrixCopy(BlockSize, ColumnsCount, TmpA, 0, 0, A, BlockStart, BlockStart);
      i1_ := (0) - (BlockStart);
      for i_ := BlockStart to BlockStart + BlockSize - 1 do
        begin
          Tau[i_] := TauBuf[i_ + i1_];
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
                TmpT, WORK);

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
              I := 0;
              while I <= BlockSize - 1 do
                begin
                  i1_ := (I) - (1);
                  for i_ := 1 to ColumnsCount - I do
                    begin
                      T[i_] := Conj(TmpA[I, i_ + i1_]);
                    end;
                  T[1] := C_Complex(1);
                  ComplexApplyReflectionFromTheRight(A, TauBuf[I], T,
                    BlockStart + BlockSize, M - 1, BlockStart + I, N - 1, WORK);
                  Inc(I);
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
procedure RMatrixQRUnpackQ(const A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const Tau: TLearnFloatArray; QColumns: TLearnInteger;
  var Q: TLearnFloat2DArray);
var
  WORK      : TLearnFloatArray;
  T         : TLearnFloatArray;
  TauBuf    : TLearnFloatArray;
  MinMN     : TLearnInteger;
  RefCnt    : TLearnInteger;
  TmpA      : TLearnFloat2DArray;
  TmpT      : TLearnFloat2DArray;
  TmpR      : TLearnFloat2DArray;
  BlockStart: TLearnInteger;
  BlockSize : TLearnInteger;
  RowsCount : TLearnInteger;
  I         : TLearnInteger;
  J         : TLearnInteger;
  K         : TLearnInteger;
  V         : TLearnFloat;
  i_        : TLearnInteger;
  i1_       : TLearnInteger;
begin
  Assert(QColumns <= M, 'UnpackQFromQR: QColumns>M!');
  if (M <= 0) or (N <= 0) or (QColumns <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, N);
  RefCnt := Min(MinMN, QColumns);
  SetLength(Q, M, QColumns);
  I := 0;
  while I <= M - 1 do
    begin
      J := 0;
      while J <= QColumns - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := 1;
            end
          else
            begin
              Q[I, J] := 0;
            end;
          Inc(J);
        end;
      Inc(I);
    end;
  SetLength(WORK, Max(M, QColumns) + 1);
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
      APVMove(@TauBuf[0], 0, BlockSize - 1, @Tau[0], BlockStart,
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
            TmpT, WORK);

          //
          // Multiply matrix by Q.
          //
          // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
          //
          RMatrixGEMM(BlockSize, QColumns, RowsCount, 1.0, TmpA, 0, 0, 1, Q,
            BlockStart, 0, 0, 0.0, TmpR, 0, 0);
          RMatrixGEMM(BlockSize, QColumns, BlockSize, 1.0, TmpT, 0, 0, 0, TmpR, 0,
            0, 0, 0.0, TmpR, BlockSize, 0);
          RMatrixGEMM(RowsCount, QColumns, BlockSize, 1.0, TmpA, 0, 0, 0, TmpR,
            BlockSize, 0, 0, 1.0, Q, BlockStart, 0);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          I := BlockSize - 1;
          while I >= 0 do
            begin
              i1_ := (I) - (1);
              for i_ := 1 to RowsCount - I do
                begin
                  T[i_] := TmpA[i_ + i1_, I];
                end;
              T[1] := 1;
              ApplyReflectionFromTheLeft(Q, TauBuf[I], T, BlockStart + I, M - 1, 0,
                QColumns - 1, WORK);
              Dec(I);
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
procedure RMatrixQRUnpackR(const A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var R: TLearnFloat2DArray);
var
  I: TLearnInteger;
  K: TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  K := Min(M, N);
  SetLength(R, M, N);
  I := 0;
  while I <= N - 1 do
    begin
      R[0, I] := 0;
      Inc(I);
    end;
  I := 1;
  while I <= M - 1 do
    begin
      APVMove(@R[I][0], 0, N - 1, @R[0][0], 0, N - 1);
      Inc(I);
    end;
  I := 0;
  while I <= K - 1 do
    begin
      APVMove(@R[I][0], I, N - 1, @A[I][0], I, N - 1);
      Inc(I);
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
procedure RMatrixLQUnpackQ(const A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const Tau: TLearnFloatArray; QRows: TLearnInteger;
  var Q: TLearnFloat2DArray);
var
  WORK        : TLearnFloatArray;
  T           : TLearnFloatArray;
  TauBuf      : TLearnFloatArray;
  MinMN       : TLearnInteger;
  RefCnt      : TLearnInteger;
  TmpA        : TLearnFloat2DArray;
  TmpT        : TLearnFloat2DArray;
  TmpR        : TLearnFloat2DArray;
  BlockStart  : TLearnInteger;
  BlockSize   : TLearnInteger;
  ColumnsCount: TLearnInteger;
  I           : TLearnInteger;
  J           : TLearnInteger;
  K           : TLearnInteger;
  V           : TLearnFloat;
begin
  Assert(QRows <= N, 'RMatrixLQUnpackQ: QRows>N!');
  if (M <= 0) or (N <= 0) or (QRows <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, N);
  RefCnt := Min(MinMN, QRows);
  SetLength(WORK, Max(M, N) + 1);
  SetLength(T, Max(M, N) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASBlockSize(A), N);
  SetLength(TmpT, ABLASBlockSize(A), 2 * ABLASBlockSize(A));
  SetLength(TmpR, QRows, 2 * ABLASBlockSize(A));
  SetLength(Q, QRows, N);
  I := 0;
  while I <= QRows - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := 1;
            end
          else
            begin
              Q[I, J] := 0;
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASBlockSize(A) * (RefCnt div ABLASBlockSize(A));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      ColumnsCount := N - BlockStart;

      //
      // Copy submatrix
      //
      RMatrixCopy(BlockSize, ColumnsCount, A, BlockStart, BlockStart, TmpA, 0, 0);
      APVMove(@TauBuf[0], 0, BlockSize - 1, @Tau[0], BlockStart,
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
            TmpT, WORK);

          //
          // Multiply the rest of A by Q'.
          //
          // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
          //
          RMatrixGEMM(QRows, BlockSize, ColumnsCount, 1.0, Q, 0, BlockStart, 0,
            TmpA, 0, 0, 1, 0.0, TmpR, 0, 0);
          RMatrixGEMM(QRows, BlockSize, BlockSize, 1.0, TmpR, 0, 0, 0, TmpT, 0, 0,
            1, 0.0, TmpR, 0, BlockSize);
          RMatrixGEMM(QRows, ColumnsCount, BlockSize, 1.0, TmpR, 0, BlockSize, 0,
            TmpA, 0, 0, 0, 1.0, Q, 0, BlockStart);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          I := BlockSize - 1;
          while I >= 0 do
            begin
              APVMove(@T[0], 1, ColumnsCount - I, @TmpA[I][0], I, ColumnsCount - 1);
              T[1] := 1;
              ApplyReflectionFromTheRight(Q, TauBuf[I], T, 0, QRows - 1,
                BlockStart + I, N - 1, WORK);
              Dec(I);
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
procedure RMatrixLQUnpackL(const A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var L: TLearnFloat2DArray);
var
  I: TLearnInteger;
  K: TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  SetLength(L, M, N);
  I := 0;
  while I <= N - 1 do
    begin
      L[0, I] := 0;
      Inc(I);
    end;
  I := 1;
  while I <= M - 1 do
    begin
      APVMove(@L[I][0], 0, N - 1, @L[0][0], 0, N - 1);
      Inc(I);
    end;
  I := 0;
  while I <= M - 1 do
    begin
      K := Min(I, N - 1);
      APVMove(@L[I][0], 0, K, @A[I][0], 0, K);
      Inc(I);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from QR decomposition of a TLearnComplex matrix A.

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
procedure CMatrixQRUnpackQ(const A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; const Tau: TLearnComplexArray; QColumns: TLearnInteger;
  var Q: TLearnComplex2DArray);
var
  WORK      : TLearnComplexArray;
  T         : TLearnComplexArray;
  TauBuf    : TLearnComplexArray;
  MinMN     : TLearnInteger;
  RefCnt    : TLearnInteger;
  TmpA      : TLearnComplex2DArray;
  TmpT      : TLearnComplex2DArray;
  TmpR      : TLearnComplex2DArray;
  BlockStart: TLearnInteger;
  BlockSize : TLearnInteger;
  RowsCount : TLearnInteger;
  I         : TLearnInteger;
  J         : TLearnInteger;
  K         : TLearnInteger;
  V         : TLearnComplex;
  i_        : TLearnInteger;
  i1_       : TLearnInteger;
begin
  Assert(QColumns <= M, 'UnpackQFromQR: QColumns>M!');
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, N);
  RefCnt := Min(MinMN, QColumns);
  SetLength(WORK, Max(M, N) + 1);
  SetLength(T, Max(M, N) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASComplexBlockSize(A));
  SetLength(TmpT, ABLASComplexBlockSize(A), ABLASComplexBlockSize(A));
  SetLength(TmpR, 2 * ABLASComplexBlockSize(A), QColumns);
  SetLength(Q, M, QColumns);
  I := 0;
  while I <= M - 1 do
    begin
      J := 0;
      while J <= QColumns - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := C_Complex(1);
            end
          else
            begin
              Q[I, J] := C_Complex(0);
            end;
          Inc(J);
        end;
      Inc(I);
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
          TauBuf[i_] := Tau[i_ + i1_];
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
            TmpT, WORK);

          //
          // Multiply the rest of A by Q.
          //
          // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
          //
          CMatrixGEMM(BlockSize, QColumns, RowsCount, C_Complex(1.0), TmpA, 0, 0, 2,
            Q, BlockStart, 0, 0, C_Complex(0.0), TmpR, 0, 0);
          CMatrixGEMM(BlockSize, QColumns, BlockSize, C_Complex(1.0), TmpT, 0, 0, 0,
            TmpR, 0, 0, 0, C_Complex(0.0), TmpR, BlockSize, 0);
          CMatrixGEMM(RowsCount, QColumns, BlockSize, C_Complex(1.0), TmpA, 0, 0, 0,
            TmpR, BlockSize, 0, 0, C_Complex(1.0), Q, BlockStart, 0);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          I := BlockSize - 1;
          while I >= 0 do
            begin
              i1_ := (I) - (1);
              for i_ := 1 to RowsCount - I do
                begin
                  T[i_] := TmpA[i_ + i1_, I];
                end;
              T[1] := C_Complex(1);
              ComplexApplyReflectionFromTheLeft(Q, TauBuf[I], T, BlockStart + I,
                M - 1, 0, QColumns - 1, WORK);
              Dec(I);
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
procedure CMatrixQRUnpackR(const A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; var R: TLearnComplex2DArray);
var
  I : TLearnInteger;
  K : TLearnInteger;
  i_: TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  K := Min(M, N);
  SetLength(R, M, N);
  I := 0;
  while I <= N - 1 do
    begin
      R[0, I] := C_Complex(0);
      Inc(I);
    end;
  I := 1;
  while I <= M - 1 do
    begin
      for i_ := 0 to N - 1 do
        begin
          R[I, i_] := R[0, i_];
        end;
      Inc(I);
    end;
  I := 0;
  while I <= K - 1 do
    begin
      for i_ := I to N - 1 do
        begin
          R[I, i_] := A[I, i_];
        end;
      Inc(I);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from LQ decomposition of a TLearnComplex matrix A.

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
procedure CMatrixLQUnpackQ(const A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; const Tau: TLearnComplexArray; QRows: TLearnInteger;
  var Q: TLearnComplex2DArray);
var
  WORK        : TLearnComplexArray;
  T           : TLearnComplexArray;
  TauBuf      : TLearnComplexArray;
  MinMN       : TLearnInteger;
  RefCnt      : TLearnInteger;
  TmpA        : TLearnComplex2DArray;
  TmpT        : TLearnComplex2DArray;
  TmpR        : TLearnComplex2DArray;
  BlockStart  : TLearnInteger;
  BlockSize   : TLearnInteger;
  ColumnsCount: TLearnInteger;
  I           : TLearnInteger;
  J           : TLearnInteger;
  K           : TLearnInteger;
  V           : TLearnComplex;
  i_          : TLearnInteger;
  i1_         : TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;

  //
  // Init
  //
  MinMN := Min(M, N);
  RefCnt := Min(MinMN, QRows);
  SetLength(WORK, Max(M, N) + 1);
  SetLength(T, Max(M, N) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASComplexBlockSize(A), N);
  SetLength(TmpT, ABLASComplexBlockSize(A), ABLASComplexBlockSize(A));
  SetLength(TmpR, QRows, 2 * ABLASComplexBlockSize(A));
  SetLength(Q, QRows, N);
  I := 0;
  while I <= QRows - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := C_Complex(1);
            end
          else
            begin
              Q[I, J] := C_Complex(0);
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASComplexBlockSize(A) *
    (RefCnt div ABLASComplexBlockSize(A));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      ColumnsCount := N - BlockStart;

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
          TauBuf[i_] := Tau[i_ + i1_];
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
            TmpT, WORK);

          //
          // Multiply the rest of A by Q'.
          //
          // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
          //
          CMatrixGEMM(QRows, BlockSize, ColumnsCount, C_Complex(1.0), Q, 0,
            BlockStart, 0, TmpA, 0, 0, 2, C_Complex(0.0), TmpR, 0, 0);
          CMatrixGEMM(QRows, BlockSize, BlockSize, C_Complex(1.0), TmpR, 0, 0, 0,
            TmpT, 0, 0, 2, C_Complex(0.0), TmpR, 0, BlockSize);
          CMatrixGEMM(QRows, ColumnsCount, BlockSize, C_Complex(1.0), TmpR, 0,
            BlockSize, 0, TmpA, 0, 0, 0, C_Complex(1.0), Q, 0, BlockStart);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          I := BlockSize - 1;
          while I >= 0 do
            begin
              i1_ := (I) - (1);
              for i_ := 1 to ColumnsCount - I do
                begin
                  T[i_] := Conj(TmpA[I, i_ + i1_]);
                end;
              T[1] := C_Complex(1);
              ComplexApplyReflectionFromTheRight(Q, Conj(TauBuf[I]), T, 0, QRows - 1,
                BlockStart + I, N - 1, WORK);
              Dec(I);
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
procedure CMatrixLQUnpackL(const A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; var L: TLearnComplex2DArray);
var
  I : TLearnInteger;
  K : TLearnInteger;
  i_: TLearnInteger;
begin
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  SetLength(L, M, N);
  I := 0;
  while I <= N - 1 do
    begin
      L[0, I] := C_Complex(0);
      Inc(I);
    end;
  I := 1;
  while I <= M - 1 do
    begin
      for i_ := 0 to N - 1 do
        begin
          L[I, i_] := L[0, i_];
        end;
      Inc(I);
    end;
  I := 0;
  while I <= M - 1 do
    begin
      K := Min(I, N - 1);
      for i_ := 0 to K do
        begin
          L[I, i_] := A[I, i_];
        end;
      Inc(I);
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
procedure RMatrixBD(var A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger;
  var TauQ: TLearnFloatArray; var TauP: TLearnFloatArray);
var
  WORK : TLearnFloatArray;
  T    : TLearnFloatArray;
  MinMN: TLearnInteger;
  MaxMN: TLearnInteger;
  I    : TLearnInteger;
  LTau : TLearnFloat;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin

  //
  // Prepare
  //
  if (N <= 0) or (M <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, N);
  MaxMN := Max(M, N);
  SetLength(WORK, MaxMN + 1);
  SetLength(T, MaxMN + 1);
  if M >= N then
    begin
      SetLength(TauQ, N);
      SetLength(TauP, N);
    end
  else
    begin
      SetLength(TauQ, M);
      SetLength(TauP, M);
    end;
  if M >= N then
    begin

      //
      // Reduce to upper bidiagonal form
      //
      I := 0;
      while I <= N - 1 do
        begin

          //
          // Generate elementary reflector H(i) to annihilate A(i+1:m-1,i)
          //
          i1_ := (I) - (1);
          for i_ := 1 to M - I do
            begin
              T[i_] := A[i_ + i1_, I];
            end;
          GenerateReflection(T, M - I, LTau);
          TauQ[I] := LTau;
          i1_ := (1) - (I);
          for i_ := I to M - 1 do
            begin
              A[i_, I] := T[i_ + i1_];
            end;
          T[1] := 1;

          //
          // Apply H(i) to A(i:m-1,i+1:n-1) from the left
          //
          ApplyReflectionFromTheLeft(A, LTau, T, I, M - 1, I + 1, N - 1, WORK);
          if I < N - 1 then
            begin

              //
              // Generate elementary reflector G(i) to annihilate
              // A(i,i+2:n-1)
              //
              APVMove(@T[0], 1, N - I - 1, @A[I][0], I + 1, N - 1);
              GenerateReflection(T, N - 1 - I, LTau);
              TauP[I] := LTau;
              APVMove(@A[I][0], I + 1, N - 1, @T[0], 1, N - 1 - I);
              T[1] := 1;

              //
              // Apply G(i) to A(i+1:m-1,i+1:n-1) from the right
              //
              ApplyReflectionFromTheRight(A, LTau, T, I + 1, M - 1, I + 1,
                N - 1, WORK);
            end
          else
            begin
              TauP[I] := 0;
            end;
          Inc(I);
        end;
    end
  else
    begin

      //
      // Reduce to lower bidiagonal form
      //
      I := 0;
      while I <= M - 1 do
        begin

          //
          // Generate elementary reflector G(i) to annihilate A(i,i+1:n-1)
          //
          APVMove(@T[0], 1, N - I, @A[I][0], I, N - 1);
          GenerateReflection(T, N - I, LTau);
          TauP[I] := LTau;
          APVMove(@A[I][0], I, N - 1, @T[0], 1, N - I);
          T[1] := 1;

          //
          // Apply G(i) to A(i+1:m-1,i:n-1) from the right
          //
          ApplyReflectionFromTheRight(A, LTau, T, I + 1, M - 1, I, N - 1, WORK);
          if I < M - 1 then
            begin

              //
              // Generate elementary reflector H(i) to annihilate
              // A(i+2:m-1,i)
              //
              i1_ := (I + 1) - (1);
              for i_ := 1 to M - 1 - I do
                begin
                  T[i_] := A[i_ + i1_, I];
                end;
              GenerateReflection(T, M - 1 - I, LTau);
              TauQ[I] := LTau;
              i1_ := (1) - (I + 1);
              for i_ := I + 1 to M - 1 do
                begin
                  A[i_, I] := T[i_ + i1_];
                end;
              T[1] := 1;

              //
              // Apply H(i) to A(i+1:m-1,i+1:n-1) from the left
              //
              ApplyReflectionFromTheLeft(A, LTau, T, I + 1, M - 1, I + 1,
                N - 1, WORK);
            end
          else
            begin
              TauQ[I] := 0;
            end;
          Inc(I);
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
procedure RMatrixBDUnpackQ(const QP: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const TauQ: TLearnFloatArray; QColumns: TLearnInteger;
  var Q: TLearnFloat2DArray);
var
  I: TLearnInteger;
  J: TLearnInteger;
begin
  Assert(QColumns <= M, 'RMatrixBDUnpackQ: QColumns>M!');
  Assert(QColumns >= 0, 'RMatrixBDUnpackQ: QColumns<0!');
  if (M = 0) or (N = 0) or (QColumns = 0) then
    begin
      Exit;
    end;

  //
  // prepare Q
  //
  SetLength(Q, M, QColumns);
  I := 0;
  while I <= M - 1 do
    begin
      J := 0;
      while J <= QColumns - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := 1;
            end
          else
            begin
              Q[I, J] := 0;
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // Calculate
  //
  RMatrixBDMultiplyByQ(QP, M, N, TauQ, Q, M, QColumns, False, False);
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
procedure RMatrixBDMultiplyByQ(const QP: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const TauQ: TLearnFloatArray; var Z: TLearnFloat2DArray;
  ZRows: TLearnInteger; ZColumns: TLearnInteger; FromTheRight: Boolean;
  DoTranspose: Boolean);
var
  I    : TLearnInteger;
  I1   : TLearnInteger;
  I2   : TLearnInteger;
  IStep: TLearnInteger;
  V    : TLearnFloatArray;
  WORK : TLearnFloatArray;
  Mx   : TLearnInteger;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin
  if (M <= 0) or (N <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
    begin
      Exit;
    end;
  Assert(FromTheRight and (ZColumns = M) or not FromTheRight and (ZRows = M),
    'RMatrixBDMultiplyByQ: incorrect Z size!');

  //
  // init
  //
  Mx := Max(M, N);
  Mx := Max(Mx, ZRows);
  Mx := Max(Mx, ZColumns);
  SetLength(V, Mx + 1);
  SetLength(WORK, Mx + 1);
  if M >= N then
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          I1 := 0;
          I2 := N - 1;
          IStep := +1;
        end
      else
        begin
          I1 := N - 1;
          I2 := 0;
          IStep := -1;
        end;
      if DoTranspose then
        begin
          I := I1;
          I1 := I2;
          I2 := I;
          IStep := -IStep;
        end;

      //
      // Process
      //
      I := I1;
      repeat
        i1_ := (I) - (1);
        for i_ := 1 to M - I do
          begin
            V[i_] := QP[i_ + i1_, I];
          end;
        V[1] := 1;
        if FromTheRight then
          begin
            ApplyReflectionFromTheRight(Z, TauQ[I], V, 0, ZRows - 1, I,
              M - 1, WORK);
          end
        else
          begin
            ApplyReflectionFromTheLeft(Z, TauQ[I], V, I, M - 1, 0,
              ZColumns - 1, WORK);
          end;
        I := I + IStep;
      until I = I2 + IStep;
    end
  else
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          I1 := 0;
          I2 := M - 2;
          IStep := +1;
        end
      else
        begin
          I1 := M - 2;
          I2 := 0;
          IStep := -1;
        end;
      if DoTranspose then
        begin
          I := I1;
          I1 := I2;
          I2 := I;
          IStep := -IStep;
        end;

      //
      // Process
      //
      if M - 1 > 0 then
        begin
          I := I1;
          repeat
            i1_ := (I + 1) - (1);
            for i_ := 1 to M - I - 1 do
              begin
                V[i_] := QP[i_ + i1_, I];
              end;
            V[1] := 1;
            if FromTheRight then
              begin
                ApplyReflectionFromTheRight(Z, TauQ[I], V, 0, ZRows - 1, I + 1,
                  M - 1, WORK);
              end
            else
              begin
                ApplyReflectionFromTheLeft(Z, TauQ[I], V, I + 1, M - 1, 0,
                  ZColumns - 1, WORK);
              end;
            I := I + IStep;
          until I = I2 + IStep;
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
procedure RMatrixBDUnpackPT(const QP: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const TauP: TLearnFloatArray; PTRows: TLearnInteger;
  var PT: TLearnFloat2DArray);
var
  I: TLearnInteger;
  J: TLearnInteger;
begin
  Assert(PTRows <= N, 'RMatrixBDUnpackPT: PTRows>N!');
  Assert(PTRows >= 0, 'RMatrixBDUnpackPT: PTRows<0!');
  if (M = 0) or (N = 0) or (PTRows = 0) then
    begin
      Exit;
    end;

  //
  // prepare PT
  //
  SetLength(PT, PTRows, N);
  I := 0;
  while I <= PTRows - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          if I = J then
            begin
              PT[I, J] := 1;
            end
          else
            begin
              PT[I, J] := 0;
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // Calculate
  //
  RMatrixBDMultiplyByP(QP, M, N, TauP, PT, PTRows, N, True, True);
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
procedure RMatrixBDMultiplyByP(const QP: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; const TauP: TLearnFloatArray; var Z: TLearnFloat2DArray;
  ZRows: TLearnInteger; ZColumns: TLearnInteger; FromTheRight: Boolean;
  DoTranspose: Boolean);
var
  I    : TLearnInteger;
  V    : TLearnFloatArray;
  WORK : TLearnFloatArray;
  Mx   : TLearnInteger;
  I1   : TLearnInteger;
  I2   : TLearnInteger;
  IStep: TLearnInteger;
begin
  if (M <= 0) or (N <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
    begin
      Exit;
    end;
  Assert(FromTheRight and (ZColumns = N) or not FromTheRight and (ZRows = N),
    'RMatrixBDMultiplyByP: incorrect Z size!');

  //
  // init
  //
  Mx := Max(M, N);
  Mx := Max(Mx, ZRows);
  Mx := Max(Mx, ZColumns);
  SetLength(V, Mx + 1);
  SetLength(WORK, Mx + 1);
  if M >= N then
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          I1 := N - 2;
          I2 := 0;
          IStep := -1;
        end
      else
        begin
          I1 := 0;
          I2 := N - 2;
          IStep := +1;
        end;
      if not DoTranspose then
        begin
          I := I1;
          I1 := I2;
          I2 := I;
          IStep := -IStep;
        end;

      //
      // Process
      //
      if N - 1 > 0 then
        begin
          I := I1;
          repeat
            APVMove(@V[0], 1, N - 1 - I, @QP[I][0], I + 1, N - 1);
            V[1] := 1;
            if FromTheRight then
              begin
                ApplyReflectionFromTheRight(Z, TauP[I], V, 0, ZRows - 1, I + 1,
                  N - 1, WORK);
              end
            else
              begin
                ApplyReflectionFromTheLeft(Z, TauP[I], V, I + 1, N - 1, 0,
                  ZColumns - 1, WORK);
              end;
            I := I + IStep;
          until I = I2 + IStep;
        end;
    end
  else
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          I1 := M - 1;
          I2 := 0;
          IStep := -1;
        end
      else
        begin
          I1 := 0;
          I2 := M - 1;
          IStep := +1;
        end;
      if not DoTranspose then
        begin
          I := I1;
          I1 := I2;
          I2 := I;
          IStep := -IStep;
        end;

      //
      // Process
      //
      I := I1;
      repeat
        APVMove(@V[0], 1, N - I, @QP[I][0], I, N - 1);
        V[1] := 1;
        if FromTheRight then
          begin
            ApplyReflectionFromTheRight(Z, TauP[I], V, 0, ZRows - 1, I,
              N - 1, WORK);
          end
        else
          begin
            ApplyReflectionFromTheLeft(Z, TauP[I], V, I, N - 1, 0,
              ZColumns - 1, WORK);
          end;
        I := I + IStep;
      until I = I2 + IStep;
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
procedure RMatrixBDUnpackDiagonals(const B: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var IsUpper: Boolean; var D: TLearnFloatArray;
  var E: TLearnFloatArray);
var
  I: TLearnInteger;
begin
  IsUpper := M >= N;
  if (M <= 0) or (N <= 0) then
    begin
      Exit;
    end;
  if IsUpper then
    begin
      SetLength(D, N);
      SetLength(E, N);
      I := 0;
      while I <= N - 2 do
        begin
          D[I] := B[I, I];
          E[I] := B[I, I + 1];
          Inc(I);
        end;
      D[N - 1] := B[N - 1, N - 1];
    end
  else
    begin
      SetLength(D, M);
      SetLength(E, M);
      I := 0;
      while I <= M - 2 do
        begin
          D[I] := B[I, I];
          E[I] := B[I + 1, I];
          Inc(I);
        end;
      D[M - 1] := B[M - 1, M - 1];
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
  Tau     -   array of scalar factors which are used to form matrix Q.
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
procedure RMatrixHessenberg(var A: TLearnFloat2DArray; N: TLearnInteger;
  var Tau: TLearnFloatArray);
var
  I   : TLearnInteger;
  V   : TLearnFloat;
  T   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  i_  : TLearnInteger;
  i1_ : TLearnInteger;
begin
  Assert(N >= 0, 'RMatrixHessenberg: incorrect N!');

  //
  // Quick return if possible
  //
  if N <= 1 then
    begin
      Exit;
    end;
  SetLength(Tau, N - 2 + 1);
  SetLength(T, N + 1);
  SetLength(WORK, N (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= N - 2 do
    begin

      //
      // Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
      //
      i1_ := (I + 1) - (1);
      for i_ := 1 to N - I - 1 do
        begin
          T[i_] := A[i_ + i1_, I];
        end;
      GenerateReflection(T, N - I - 1, V);
      i1_ := (1) - (I + 1);
      for i_ := I + 1 to N - 1 do
        begin
          A[i_, I] := T[i_ + i1_];
        end;
      Tau[I] := V;
      T[1] := 1;

      //
      // Apply H(i) to A(1:ihi,i+1:ihi) from the right
      //
      ApplyReflectionFromTheRight(A, V, T, 0, N - 1, I + 1, N - 1, WORK);

      //
      // Apply H(i) to A(i+1:ihi,i+1:n) from the left
      //
      ApplyReflectionFromTheLeft(A, V, T, I + 1, N - 1, I + 1, N - 1, WORK);
      Inc(I);
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
procedure RMatrixHessenbergUnpackQ(const A: TLearnFloat2DArray; N: TLearnInteger;
  const Tau: TLearnFloatArray; var Q: TLearnFloat2DArray);
var
  I   : TLearnInteger;
  J   : TLearnInteger;
  V   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  i_  : TLearnInteger;
  i1_ : TLearnInteger;
begin
  if N = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(Q, N (* - 1 + 1 // optimized compler *) , N (* - 1 + 1 // optimized compler *) );
  SetLength(V, N (* - 1 + 1 // optimized compler *) );
  SetLength(WORK, N (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := 1;
            end
          else
            begin
              Q[I, J] := 0;
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // unpack Q
  //
  I := 0;
  while I <= N - 2 do
    begin

      //
      // Apply H(i)
      //
      i1_ := (I + 1) - (1);
      for i_ := 1 to N - I - 1 do
        begin
          V[i_] := A[i_ + i1_, I];
        end;
      V[1] := 1;
      ApplyReflectionFromTheRight(Q, Tau[I], V, 0, N - 1, I + 1, N - 1, WORK);
      Inc(I);
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
procedure RMatrixHessenbergUnpackH(const A: TLearnFloat2DArray; N: TLearnInteger;
  var H: TLearnFloat2DArray);
var
  I   : TLearnInteger;
  J   : TLearnInteger;
  V   : TLearnFloatArray;
  WORK: TLearnFloatArray;
begin
  if N = 0 then
    begin
      Exit;
    end;
  SetLength(H, N (* - 1 + 1 // optimized compler *) , N (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= I - 2 do
        begin
          H[I, J] := 0;
          Inc(J);
        end;
      J := Max(0, I - 1);
      APVMove(@H[I][0], J, N - 1, @A[I][0], J, N - 1);
      Inc(I);
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
  Tau     -   array of factors which are forming matrices H(i)
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
procedure SMatrixTD(var A: TLearnFloat2DArray; N: TLearnInteger; IsUpper: Boolean;
  var Tau: TLearnFloatArray; var D: TLearnFloatArray; var E: TLearnFloatArray);
var
  I    : TLearnInteger;
  ALPHA: TLearnFloat;
  TAUI : TLearnFloat;
  V    : TLearnFloat;
  T    : TLearnFloatArray;
  T2   : TLearnFloatArray;
  T3   : TLearnFloatArray;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin
  if N <= 0 then
    begin
      Exit;
    end;
  SetLength(T, N + 1);
  SetLength(T2, N + 1);
  SetLength(T3, N + 1);
  if N > 1 then
    begin
      SetLength(Tau, N - 2 + 1);
    end;
  SetLength(D, N (* - 1 + 1 // optimized compler *) );
  if N > 1 then
    begin
      SetLength(E, N - 2 + 1);
    end;
  if IsUpper then
    begin

      //
      // Reduce the upper triangle of A
      //
      I := N - 2;
      while I >= 0 do
        begin

          //
          // Generate elementary reflector H() = E - tau * v * v'
          //
          if I >= 1 then
            begin
              i1_ := (0) - (2);
              for i_ := 2 to I + 1 do
                begin
                  T[i_] := A[i_ + i1_, I + 1];
                end;
            end;
          T[1] := A[I, I + 1];
          GenerateReflection(T, I + 1, TAUI);
          if I >= 1 then
            begin
              i1_ := (2) - (0);
              for i_ := 0 to I - 1 do
                begin
                  A[i_, I + 1] := T[i_ + i1_];
                end;
            end;
          A[I, I + 1] := T[1];
          E[I] := A[I, I + 1];
          if AP_FP_Neq(TAUI, 0) then
            begin

              //
              // Apply H from both sides to A
              //
              A[I, I + 1] := 1;

              //
              // Compute  x := tau * A * v  storing x in TAU
              //
              i1_ := (0) - (1);
              for i_ := 1 to I + 1 do
                begin
                  T[i_] := A[i_ + i1_, I + 1];
                end;
              SymmetricMatrixVectorMultiply(A, IsUpper, 0, I, T, TAUI, T3);
              APVMove(@Tau[0], 0, I, @T3[0], 1, I + 1);

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              V := 0.0;
              for i_ := 0 to I do
                begin
                  V := V + Tau[i_] * A[i_, I + 1];
                end;
              ALPHA := -0.5 * TAUI * V;
              for i_ := 0 to I do
                begin
                  Tau[i_] := Tau[i_] + ALPHA * A[i_, I + 1];
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (0) - (1);
              for i_ := 1 to I + 1 do
                begin
                  T[i_] := A[i_ + i1_, I + 1];
                end;
              APVMove(@T3[0], 1, I + 1, @Tau[0], 0, I);
              SymmetricRank2Update(A, IsUpper, 0, I, T, T3, T2, -1);
              A[I, I + 1] := E[I];
            end;
          D[I + 1] := A[I + 1, I + 1];
          Tau[I] := TAUI;
          Dec(I);
        end;
      D[0] := A[0, 0];
    end
  else
    begin

      //
      // Reduce the lower triangle of A
      //
      I := 0;
      while I <= N - 2 do
        begin

          //
          // Generate elementary reflector H = E - tau * v * v'
          //
          i1_ := (I + 1) - (1);
          for i_ := 1 to N - I - 1 do
            begin
              T[i_] := A[i_ + i1_, I];
            end;
          GenerateReflection(T, N - I - 1, TAUI);
          i1_ := (1) - (I + 1);
          for i_ := I + 1 to N - 1 do
            begin
              A[i_, I] := T[i_ + i1_];
            end;
          E[I] := A[I + 1, I];
          if AP_FP_Neq(TAUI, 0) then
            begin

              //
              // Apply H from both sides to A
              //
              A[I + 1, I] := 1;

              //
              // Compute  x := tau * A * v  storing y in TAU
              //
              i1_ := (I + 1) - (1);
              for i_ := 1 to N - I - 1 do
                begin
                  T[i_] := A[i_ + i1_, I];
                end;
              SymmetricMatrixVectorMultiply(A, IsUpper, I + 1, N - 1, T, TAUI, T2);
              APVMove(@Tau[0], I, N - 2, @T2[0], 1, N - I - 1);

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              i1_ := (I + 1) - (I);
              V := 0.0;
              for i_ := I to N - 2 do
                begin
                  V := V + Tau[i_] * A[i_ + i1_, I];
                end;
              ALPHA := -0.5 * TAUI * V;
              i1_ := (I + 1) - (I);
              for i_ := I to N - 2 do
                begin
                  Tau[i_] := Tau[i_] + ALPHA * A[i_ + i1_, I];
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              //
              i1_ := (I + 1) - (1);
              for i_ := 1 to N - I - 1 do
                begin
                  T[i_] := A[i_ + i1_, I];
                end;
              APVMove(@T2[0], 1, N - I - 1, @Tau[0], I, N - 2);
              SymmetricRank2Update(A, IsUpper, I + 1, N - 1, T, T2, T3, -1);
              A[I + 1, I] := E[I];
            end;
          D[I] := A[I, I];
          Tau[I] := TAUI;
          Inc(I);
        end;
      D[N - 1] := A[N - 1, N - 1];
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
procedure SMatrixTDUnpackQ(const A: TLearnFloat2DArray; const N: TLearnInteger;
  const IsUpper: Boolean; const Tau: TLearnFloatArray; var Q: TLearnFloat2DArray);
var
  I   : TLearnInteger;
  J   : TLearnInteger;
  V   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  i_  : TLearnInteger;
  i1_ : TLearnInteger;
begin
  if N = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(Q, N (* - 1 + 1 // optimized compler *) , N (* - 1 + 1 // optimized compler *) );
  SetLength(V, N + 1);
  SetLength(WORK, N (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := 1;
            end
          else
            begin
              Q[I, J] := 0;
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // unpack Q
  //
  if IsUpper then
    begin
      I := 0;
      while I <= N - 2 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (0) - (1);
          for i_ := 1 to I + 1 do
            begin
              V[i_] := A[i_ + i1_, I + 1];
            end;
          V[I + 1] := 1;
          ApplyReflectionFromTheLeft(Q, Tau[I], V, 0, I, 0, N - 1, WORK);
          Inc(I);
        end;
    end
  else
    begin
      I := N - 2;
      while I >= 0 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (I + 1) - (1);
          for i_ := 1 to N - I - 1 do
            begin
              V[i_] := A[i_ + i1_, I];
            end;
          V[1] := 1;
          ApplyReflectionFromTheLeft(Q, Tau[I], V, I + 1, N - 1, 0, N - 1, WORK);
          Dec(I);
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
  Tau     -   array of factors which are forming matrices H(i)
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

  where tau is a TLearnComplex scalar, and v is a TLearnComplex vector with
  v(i+1:n-1) = 0, v(i) = 1, v(0:i-1) is stored on exit in
  A(0:i-1,i+1), and tau in TAU(i).

  If IsUpper=False, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(0) H(2) . . . H(n-2).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a TLearnComplex scalar, and v is a TLearnComplex vector with
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
procedure HMatrixTD(var A: TLearnComplex2DArray; N: TLearnInteger; IsUpper: Boolean;
  var Tau: TLearnComplexArray; var D: TLearnFloatArray; var E: TLearnFloatArray);
var
  I    : TLearnInteger;
  ALPHA: TLearnComplex;
  TAUI : TLearnComplex;
  V    : TLearnComplex;
  T    : TLearnComplexArray;
  T2   : TLearnComplexArray;
  T3   : TLearnComplexArray;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin
  if N <= 0 then
    begin
      Exit;
    end;
  I := 0;
  while I <= N - 1 do
    begin
      Assert(AP_FP_Eq(A[I, I].Y, 0));
      Inc(I);
    end;
  if N > 1 then
    begin
      SetLength(Tau, N - 2 + 1);
      SetLength(E, N - 2 + 1);
    end;
  SetLength(D, N (* - 1 + 1 // optimized compler *) );
  SetLength(T, N (* - 1 + 1 // optimized compler *) );
  SetLength(T2, N (* - 1 + 1 // optimized compler *) );
  SetLength(T3, N (* - 1 + 1 // optimized compler *) );
  if IsUpper then
    begin

      //
      // Reduce the upper triangle of A
      //
      A[N - 1, N - 1] := C_Complex(A[N - 1, N - 1].X);
      I := N - 2;
      while I >= 0 do
        begin

          //
          // Generate elementary reflector H = I+1 - tau * v * v'
          //
          ALPHA := A[I, I + 1];
          T[1] := ALPHA;
          if I >= 1 then
            begin
              i1_ := (0) - (2);
              for i_ := 2 to I + 1 do
                begin
                  T[i_] := A[i_ + i1_, I + 1];
                end;
            end;
          ComplexGenerateReflection(T, I + 1, TAUI);
          if I >= 1 then
            begin
              i1_ := (2) - (0);
              for i_ := 0 to I - 1 do
                begin
                  A[i_, I + 1] := T[i_ + i1_];
                end;
            end;
          ALPHA := T[1];
          E[I] := ALPHA.X;
          if C_NotEqualR(TAUI, 0) then
            begin

              //
              // Apply H(I+1) from both sides to A
              //
              A[I, I + 1] := C_Complex(1);

              //
              // Compute  x := tau * A * v  storing x in TAU
              //
              i1_ := (0) - (1);
              for i_ := 1 to I + 1 do
                begin
                  T[i_] := A[i_ + i1_, I + 1];
                end;
              HermitianMatrixVectorMultiply(A, IsUpper, 0, I, T, TAUI, T2);
              i1_ := (1) - (0);
              for i_ := 0 to I do
                begin
                  Tau[i_] := T2[i_ + i1_];
                end;

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              V := C_Complex(0.0);
              for i_ := 0 to I do
                begin
                  V := C_Add(V, C_Mul(Conj(Tau[i_]), A[i_, I + 1]));
                end;
              ALPHA := C_Opposite(C_Mul(C_MulR(TAUI, 0.5), V));
              for i_ := 0 to I do
                begin
                  Tau[i_] := C_Add(Tau[i_], C_Mul(ALPHA, A[i_, I + 1]));
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (0) - (1);
              for i_ := 1 to I + 1 do
                begin
                  T[i_] := A[i_ + i1_, I + 1];
                end;
              i1_ := (0) - (1);
              for i_ := 1 to I + 1 do
                begin
                  T3[i_] := Tau[i_ + i1_];
                end;
              HermitianRank2Update(A, IsUpper, 0, I, T, T3, T2, C_Complex(-1));
            end
          else
            begin
              A[I, I] := C_Complex(A[I, I].X);
            end;
          A[I, I + 1] := C_Complex(E[I]);
          D[I + 1] := A[I + 1, I + 1].X;
          Tau[I] := TAUI;
          Dec(I);
        end;
      D[0] := A[0, 0].X;
    end
  else
    begin

      //
      // Reduce the lower triangle of A
      //
      A[0, 0] := C_Complex(A[0, 0].X);
      I := 0;
      while I <= N - 2 do
        begin

          //
          // Generate elementary reflector H = I - tau * v * v'
          //
          i1_ := (I + 1) - (1);
          for i_ := 1 to N - I - 1 do
            begin
              T[i_] := A[i_ + i1_, I];
            end;
          ComplexGenerateReflection(T, N - I - 1, TAUI);
          i1_ := (1) - (I + 1);
          for i_ := I + 1 to N - 1 do
            begin
              A[i_, I] := T[i_ + i1_];
            end;
          E[I] := A[I + 1, I].X;
          if C_NotEqualR(TAUI, 0) then
            begin

              //
              // Apply H(i) from both sides to A(i+1:n,i+1:n)
              //
              A[I + 1, I] := C_Complex(1);

              //
              // Compute  x := tau * A * v  storing y in TAU
              //
              i1_ := (I + 1) - (1);
              for i_ := 1 to N - I - 1 do
                begin
                  T[i_] := A[i_ + i1_, I];
                end;
              HermitianMatrixVectorMultiply(A, IsUpper, I + 1, N - 1, T, TAUI, T2);
              i1_ := (1) - (I);
              for i_ := I to N - 2 do
                begin
                  Tau[i_] := T2[i_ + i1_];
                end;

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              i1_ := (I + 1) - (I);
              V := C_Complex(0.0);
              for i_ := I to N - 2 do
                begin
                  V := C_Add(V, C_Mul(Conj(Tau[i_]), A[i_ + i1_, I]));
                end;
              ALPHA := C_Opposite(C_Mul(C_MulR(TAUI, 0.5), V));
              i1_ := (I + 1) - (I);
              for i_ := I to N - 2 do
                begin
                  Tau[i_] := C_Add(Tau[i_], C_Mul(ALPHA, A[i_ + i1_, I]));
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (I + 1) - (1);
              for i_ := 1 to N - I - 1 do
                begin
                  T[i_] := A[i_ + i1_, I];
                end;
              i1_ := (I) - (1);
              for i_ := 1 to N - I - 1 do
                begin
                  T2[i_] := Tau[i_ + i1_];
                end;
              HermitianRank2Update(A, IsUpper, I + 1, N - 1, T, T2, T3,
                C_Complex(-1));
            end
          else
            begin
              A[I + 1, I + 1] := C_Complex(A[I + 1, I + 1].X);
            end;
          A[I + 1, I] := C_Complex(E[I]);
          D[I] := A[I, I].X;
          Tau[I] := TAUI;
          Inc(I);
        end;
      D[N - 1] := A[N - 1, N - 1].X;
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
procedure HMatrixTDUnpackQ(const A: TLearnComplex2DArray; const N: TLearnInteger;
  const IsUpper: Boolean; const Tau: TLearnComplexArray; var Q: TLearnComplex2DArray);
var
  I   : TLearnInteger;
  J   : TLearnInteger;
  V   : TLearnComplexArray;
  WORK: TLearnComplexArray;
  i_  : TLearnInteger;
  i1_ : TLearnInteger;
begin
  if N = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(Q, N (* - 1 + 1 // optimized compler *) , N (* - 1 + 1 // optimized compler *) );
  SetLength(V, N + 1);
  SetLength(WORK, N (* - 1 + 1 // optimized compler *) );
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          if I = J then
            begin
              Q[I, J] := C_Complex(1);
            end
          else
            begin
              Q[I, J] := C_Complex(0);
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // unpack Q
  //
  if IsUpper then
    begin
      I := 0;
      while I <= N - 2 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (0) - (1);
          for i_ := 1 to I + 1 do
            begin
              V[i_] := A[i_ + i1_, I + 1];
            end;
          V[I + 1] := C_Complex(1);
          ComplexApplyReflectionFromTheLeft(Q, Tau[I], V, 0, I, 0, N - 1, WORK);
          Inc(I);
        end;
    end
  else
    begin
      I := N - 2;
      while I >= 0 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (I + 1) - (1);
          for i_ := 1 to N - I - 1 do
            begin
              V[i_] := A[i_ + i1_, I];
            end;
          V[1] := C_Complex(1);
          ComplexApplyReflectionFromTheLeft(Q, Tau[I], V, I + 1, N - 1, 0,
            N - 1, WORK);
          Dec(I);
        end;
    end;
end;

(* ************************************************************************
  Base case for real QR
  ************************************************************************ *)
procedure RMatrixQRBaseCase(var A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnFloatArray; var T: TLearnFloatArray;
  var Tau: TLearnFloatArray);
var
  I    : TLearnInteger;
  K    : TLearnInteger;
  MinMN: TLearnInteger;
  Tmp  : TLearnFloat;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin
  MinMN := Min(M, N);

  //
  // Test the input arguments
  //
  K := MinMN;
  I := 0;
  while I <= K - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
      //
      i1_ := (I) - (1);
      for i_ := 1 to M - I do
        begin
          T[i_] := A[i_ + i1_, I];
        end;
      GenerateReflection(T, M - I, Tmp);
      Tau[I] := Tmp;
      i1_ := (1) - (I);
      for i_ := I to M - 1 do
        begin
          A[i_, I] := T[i_ + i1_];
        end;
      T[1] := 1;
      if I < N then
        begin

          //
          // Apply H(i) to A(i:m-1,i+1:n-1) from the left
          //
          ApplyReflectionFromTheLeft(A, Tau[I], T, I, M - 1, I + 1, N - 1, WORK);
        end;
      Inc(I);
    end;
end;

(* ************************************************************************
  Base case for real LQ
  ************************************************************************ *)
procedure RMatrixLQBaseCase(var A: TLearnFloat2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnFloatArray; var T: TLearnFloatArray;
  var Tau: TLearnFloatArray);
var
  I    : TLearnInteger;
  K    : TLearnInteger;
  MinMN: TLearnInteger;
  Tmp  : TLearnFloat;
begin
  MinMN := Min(M, N);
  K := Min(M, N);
  I := 0;
  while I <= K - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i,i+1:n-1)
      //
      APVMove(@T[0], 1, N - I, @A[I][0], I, N - 1);
      GenerateReflection(T, N - I, Tmp);
      Tau[I] := Tmp;
      APVMove(@A[I][0], I, N - 1, @T[0], 1, N - I);
      T[1] := 1;
      if I < N then
        begin

          //
          // Apply H(i) to A(i+1:m,i:n) from the right
          //
          ApplyReflectionFromTheRight(A, Tau[I], T, I + 1, M - 1, I, N - 1, WORK);
        end;
      Inc(I);
    end;
end;

(* ************************************************************************
  Base case for TLearnComplex QR
  ************************************************************************ *)
procedure CMatrixQRBaseCase(var A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnComplexArray; var T: TLearnComplexArray;
  var Tau: TLearnComplexArray);
var
  I    : TLearnInteger;
  K    : TLearnInteger;
  MMI  : TLearnInteger;
  MinMN: TLearnInteger;
  Tmp  : TLearnComplex;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin
  MinMN := Min(M, N);
  if MinMN <= 0 then
    begin
      Exit;
    end;

  //
  // Test the input arguments
  //
  K := Min(M, N);
  I := 0;
  while I <= K - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
      //
      MMI := M - I;
      i1_ := (I) - (1);
      for i_ := 1 to MMI do
        begin
          T[i_] := A[i_ + i1_, I];
        end;
      ComplexGenerateReflection(T, MMI, Tmp);
      Tau[I] := Tmp;
      i1_ := (1) - (I);
      for i_ := I to M - 1 do
        begin
          A[i_, I] := T[i_ + i1_];
        end;
      T[1] := C_Complex(1);
      if I < N - 1 then
        begin

          //
          // Apply H'(i) to A(i:m,i+1:n) from the left
          //
          ComplexApplyReflectionFromTheLeft(A, Conj(Tau[I]), T, I, M - 1, I + 1,
            N - 1, WORK);
        end;
      Inc(I);
    end;
end;

(* ************************************************************************
  Base case for TLearnComplex LQ
  ************************************************************************ *)
procedure CMatrixLQBaseCase(var A: TLearnComplex2DArray; M: TLearnInteger;
  N: TLearnInteger; var WORK: TLearnComplexArray; var T: TLearnComplexArray;
  var Tau: TLearnComplexArray);
var
  I    : TLearnInteger;
  MinMN: TLearnInteger;
  Tmp  : TLearnComplex;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin
  MinMN := Min(M, N);
  if MinMN <= 0 then
    begin
      Exit;
    end;

  //
  // Test the input arguments
  //
  I := 0;
  while I <= MinMN - 1 do
    begin

      //
      // Generate elementary reflector H(i)
      //
      // NOTE: ComplexGenerateReflection() generates left reflector,
      // i.e. H which reduces x by applyiong from the left, but we
      // need RIGHT reflector. So we replace H=E-tau*v*v' by H^H,
      // which changes v to conj(v).
      //
      i1_ := (I) - (1);
      for i_ := 1 to N - I do
        begin
          T[i_] := Conj(A[I, i_ + i1_]);
        end;
      ComplexGenerateReflection(T, N - I, Tmp);
      Tau[I] := Tmp;
      i1_ := (1) - (I);
      for i_ := I to N - 1 do
        begin
          A[I, i_] := Conj(T[i_ + i1_]);
        end;
      T[1] := C_Complex(1);
      if I < M - 1 then
        begin

          //
          // Apply H'(i)
          //
          ComplexApplyReflectionFromTheRight(A, Tau[I], T, I + 1, M - 1, I,
            N - 1, WORK);
        end;
      Inc(I);
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
procedure RMatrixBlockReflector(var A: TLearnFloat2DArray; var Tau: TLearnFloatArray;
  ColumnwiseA: Boolean; LengthA: TLearnInteger; BlockSize: TLearnInteger;
  var T: TLearnFloat2DArray; var WORK: TLearnFloatArray);
var
  I  : TLearnInteger;
  J  : TLearnInteger;
  K  : TLearnInteger;
  V  : TLearnFloat;
  i_ : TLearnInteger;
  i1_: TLearnInteger;
begin

  //
  // fill beginning of new column with zeros,
  // load 1.0 in the first non-zero element
  //
  K := 0;
  while K <= BlockSize - 1 do
    begin
      if ColumnwiseA then
        begin
          I := 0;
          while I <= K - 1 do
            begin
              A[I, K] := 0;
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= K - 1 do
            begin
              A[K, I] := 0;
              Inc(I);
            end;
        end;
      A[K, K] := 1;
      Inc(K);
    end;

  //
  // Calculate Gram matrix of A
  //
  I := 0;
  while I <= BlockSize - 1 do
    begin
      J := 0;
      while J <= BlockSize - 1 do
        begin
          T[I, BlockSize + J] := 0;
          Inc(J);
        end;
      Inc(I);
    end;
  K := 0;
  while K <= LengthA - 1 do
    begin
      J := 1;
      while J <= BlockSize - 1 do
        begin
          if ColumnwiseA then
            begin
              V := A[K, J];
              if AP_FP_Neq(V, 0) then
                begin
                  APVAdd(@T[J][0], BlockSize, BlockSize + J - 1, @A[K][0], 0, J - 1, V);
                end;
            end
          else
            begin
              V := A[J, K];
              if AP_FP_Neq(V, 0) then
                begin
                  i1_ := (0) - (BlockSize);
                  for i_ := BlockSize to BlockSize + J - 1 do
                    begin
                      T[J, i_] := T[J, i_] + V * A[i_ + i1_, K];
                    end;
                end;
            end;
          Inc(J);
        end;
      Inc(K);
    end;

  //
  // Prepare Y (stored in TmpA) and T (stored in TmpT)
  //
  K := 0;
  while K <= BlockSize - 1 do
    begin

      //
      // fill non-zero part of T, use pre-calculated Gram matrix
      //
      APVMove(@WORK[0], 0, K - 1, @T[K][0], BlockSize, BlockSize + K - 1);
      I := 0;
      while I <= K - 1 do
        begin
          V := APVDotProduct(@T[I][0], I, K - 1, @WORK[0], I, K - 1);
          T[I, K] := -Tau[K] * V;
          Inc(I);
        end;
      T[K, K] := -Tau[K];

      //
      // Rest of T is filled by zeros
      //
      I := K + 1;
      while I <= BlockSize - 1 do
        begin
          T[I, K] := 0;
          Inc(I);
        end;
      Inc(K);
    end;
end;

(* ************************************************************************
  Generate block reflector (TLearnComplex):
  * fill unused parts of reflectors matrix by zeros
  * fill diagonal of reflectors matrix by ones
  * generate triangular factor T
  ************************************************************************ *)
procedure CMatrixBlockReflector(var A: TLearnComplex2DArray;
  var Tau: TLearnComplexArray; ColumnwiseA: Boolean; LengthA: TLearnInteger;
  BlockSize: TLearnInteger; var T: TLearnComplex2DArray; var WORK: TLearnComplexArray);
var
  I : TLearnInteger;
  K : TLearnInteger;
  V : TLearnComplex;
  i_: TLearnInteger;
begin

  //
  // Prepare Y (stored in TmpA) and T (stored in TmpT)
  //
  K := 0;
  while K <= BlockSize - 1 do
    begin

      //
      // fill beginning of new column with zeros,
      // load 1.0 in the first non-zero element
      //
      if ColumnwiseA then
        begin
          I := 0;
          while I <= K - 1 do
            begin
              A[I, K] := C_Complex(0);
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= K - 1 do
            begin
              A[K, I] := C_Complex(0);
              Inc(I);
            end;
        end;
      A[K, K] := C_Complex(1);

      //
      // fill non-zero part of T,
      //
      I := 0;
      while I <= K - 1 do
        begin
          if ColumnwiseA then
            begin
              V := C_Complex(0.0);
              for i_ := K to LengthA - 1 do
                begin
                  V := C_Add(V, C_Mul(Conj(A[i_, I]), A[i_, K]));
                end;
            end
          else
            begin
              V := C_Complex(0.0);
              for i_ := K to LengthA - 1 do
                begin
                  V := C_Add(V, C_Mul(A[I, i_], Conj(A[K, i_])));
                end;
            end;
          WORK[I] := V;
          Inc(I);
        end;
      I := 0;
      while I <= K - 1 do
        begin
          V := C_Complex(0.0);
          for i_ := I to K - 1 do
            begin
              V := C_Add(V, C_Mul(T[I, i_], WORK[i_]));
            end;
          T[I, K] := C_Opposite(C_Mul(Tau[K], V));
          Inc(I);
        end;
      T[K, K] := C_Opposite(Tau[K]);

      //
      // Rest of T is filled by zeros
      //
      I := K + 1;
      while I <= BlockSize - 1 do
        begin
          T[I, K] := C_Complex(0);
          Inc(I);
        end;
      Inc(K);
    end;
end;
