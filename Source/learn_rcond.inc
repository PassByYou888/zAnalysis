{ ****************************************************************************** }
{ * Condition number estimate support, by QQ 600585@qq.com                     * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure RMatrixRCondTRInternal(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OneNorm: Boolean; ANORM: TLFloat;
  var RC: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixRCondTRInternal(const A: TLComplexMatrix;
  const n: TLInt; IsUpper: Boolean; IsUnit: Boolean; OneNorm: Boolean;
  ANORM: TLFloat; var RC: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixRCondCholeskyInternal(const CHA: TLMatrix;
  n: TLInt; IsUpper: Boolean; IsNormProvided: Boolean;
  ANORM: TLFloat; var RC: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixRCondCholeskyInternal(const CHA: TLComplexMatrix;
  n: TLInt; IsUpper: Boolean; IsNormProvided: Boolean;
  ANORM: TLFloat; var RC: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixRCondLUInternal(const LUA: TLMatrix; n: TLInt;
  OneNorm: Boolean; IsANormProvided: Boolean; ANORM: TLFloat;
  var RC: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixRCondLUInternal(const LUA: TLComplexMatrix;
  const n: TLInt; OneNorm: Boolean; IsANormProvided: Boolean;
  ANORM: TLFloat; var RC: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixEstimateNorm(n: TLInt; var v: TLVec;
  var X: TLVec; var ISGN: TLIVec; var EST: TLFloat;
  var KASE: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixEstimateNorm(const n: TLInt; var v: TLComplexVec;
  var X: TLComplexVec; var EST: TLFloat; var KASE: TLInt;
  var ISAVE: TLIVec; var RSAVE: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function InternalComplexRCondSCSUM1(const X: TLComplexVec; n: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function InternalComplexRCondICMAX1(const X: TLComplexVec; n: TLInt): TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalComplexRCondSaveAll(var ISAVE: TLIVec;
  var RSAVE: TLVec; var i: TLInt; var ITER: TLInt;
  var J: TLInt; var JLAST: TLInt; var JUMP: TLInt;
  var ABSXI: TLFloat; var ALTSGN: TLFloat; var ESTOLD: TLFloat;
  var Temp: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalComplexRCondLoadAll(var ISAVE: TLIVec;
  var RSAVE: TLVec; var i: TLInt; var ITER: TLInt;
  var J: TLInt; var JLAST: TLInt; var JUMP: TLInt;
  var ABSXI: TLFloat; var ALTSGN: TLFloat; var ESTOLD: TLFloat;
  var Temp: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Estimate of a matrix condition number (1-norm)

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate,  k(A)=INF,
  0.0 is returned in such cases.
  ************************************************************************ *)
function RMatrixRCond1(A: TLMatrix; n: TLInt): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
  T     : TLVec;
begin
  A := DynamicArrayCopy(A);
  Assert(n >= 1, 'RMatrixRCond1: N<1!');
  SetLength(T, n);
  i := 0;
  while i <= n - 1 do
    begin
      T[i] := 0;
      Inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          T[J] := T[J] + AbsReal(A[i, J]);
          Inc(J);
        end;
      Inc(i);
    end;
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NRM := Max(NRM, T[i]);
      Inc(i);
    end;
  RMatrixLU(A, n, n, Pivots);
  RMatrixRCondLUInternal(A, n, True, True, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Estimate of a matrix condition number (infinity-norm).

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate,  k(A)=INF,
  0.0 is returned in such cases.
  ************************************************************************ *)
function RMatrixRCondInf(A: TLMatrix; n: TLInt): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
begin
  A := DynamicArrayCopy(A);
  Assert(n >= 1, 'RMatrixRCondInf: N<1!');
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      v := 0;
      J := 0;
      while J <= n - 1 do
        begin
          v := v + AbsReal(A[i, J]);
          Inc(J);
        end;
      NRM := Max(NRM, v);
      Inc(i);
    end;
  RMatrixLU(A, n, n, Pivots);
  RMatrixRCondLUInternal(A, n, False, True, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Condition number estimate of a symmetric positive definite matrix.

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  It should be noted that 1-norm and inf-norm of condition numbers of symmetric
  matrices are equal, so the algorithm doesn't take into account the
  differences between these types of norms.

  Input parameters:
  A       -   symmetric positive definite matrix which is given by its
  upper or lower triangle depending on the value of
  IsUpper. Array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.

  Result:
  1/LowerBound(cond(A)), if matrix A is positive definite,
  -1, if matrix A is not positive definite, and its condition number
  could not be found by this algorithm.

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate,  k(A)=INF,
  0.0 is returned in such cases.
  ************************************************************************ *)
function SPDMatrixRCond(A: TLMatrix; n: TLInt; IsUpper: Boolean): TLFloat;
var
  i  : TLInt;
  J  : TLInt;
  J1 : TLInt;
  J2 : TLInt;
  v  : TLFloat;
  NRM: TLFloat;
  T  : TLVec;
begin
  A := DynamicArrayCopy(A);
  SetLength(T, n);
  i := 0;
  while i <= n - 1 do
    begin
      T[i] := 0;
      Inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      J := J1;
      while J <= J2 do
        begin
          if i = J then
            begin
              T[i] := T[i] + AbsReal(A[i, i]);
            end
          else
            begin
              T[i] := T[i] + AbsReal(A[i, J]);
              T[J] := T[J] + AbsReal(A[i, J]);
            end;
          Inc(J);
        end;
      Inc(i);
    end;
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NRM := Max(NRM, T[i]);
      Inc(i);
    end;
  if SPDMatrixCholesky(A, n, IsUpper) then
    begin
      SPDMatrixRCondCholeskyInternal(A, n, IsUpper, True, NRM, v);
      Result := v;
    end
  else
    begin
      Result := -1;
    end;
end;

(* ************************************************************************
  Triangular matrix: estimate of a condition number (1-norm)

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A       -   matrix. Array[0..N-1, 0..N-1].
  N       -   size of A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate,  k(A)=INF,
  0.0 is returned in such cases.
  ************************************************************************ *)
function RMatrixTRRCond1(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
  T     : TLVec;
  J1    : TLInt;
  J2    : TLInt;
begin
  Assert(n >= 1, 'RMatrixTRRCond1: N<1!');
  SetLength(T, n);
  i := 0;
  while i <= n - 1 do
    begin
      T[i] := 0;
      Inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i + 1;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i - 1;
        end;
      J := J1;
      while J <= J2 do
        begin
          T[J] := T[J] + AbsReal(A[i, J]);
          Inc(J);
        end;
      if IsUnit then
        begin
          T[i] := T[i] + 1;
        end
      else
        begin
          T[i] := T[i] + AbsReal(A[i, i]);
        end;
      Inc(i);
    end;
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NRM := Max(NRM, T[i]);
      Inc(i);
    end;
  RMatrixRCondTRInternal(A, n, IsUpper, IsUnit, True, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Triangular matrix: estimate of a matrix condition number (infinity-norm).

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate,  k(A)=INF,
  0.0 is returned in such cases.
  ************************************************************************ *)
function RMatrixTRRCondInf(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
  J1    : TLInt;
  J2    : TLInt;
begin
  Assert(n >= 1, 'RMatrixTRRCondInf: N<1!');
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i + 1;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i - 1;
        end;
      v := 0;
      J := J1;
      while J <= J2 do
        begin
          v := v + AbsReal(A[i, J]);
          Inc(J);
        end;
      if IsUnit then
        begin
          v := v + 1;
        end
      else
        begin
          v := v + AbsReal(A[i, i]);
        end;
      NRM := Max(NRM, v);
      Inc(i);
    end;
  RMatrixRCondTRInternal(A, n, IsUpper, IsUnit, False, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Condition number estimate of a Hermitian positive definite matrix.

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  It should be noted that 1-norm and inf-norm of condition numbers of symmetric
  matrices are equal, so the algorithm doesn't take into account the
  differences between these types of norms.

  Input parameters:
  A       -   Hermitian positive definite matrix which is given by its
  upper or lower triangle depending on the value of
  IsUpper. Array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.

  Result:
  1/LowerBound(cond(A)), if matrix A is positive definite,
  -1, if matrix A is not positive definite, and its condition number
  could not be found by this algorithm.

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate,  k(A)=INF,
  0.0 is returned in such cases.
  ************************************************************************ *)
function HPDMatrixRCond(A: TLComplexMatrix; n: TLInt; IsUpper: Boolean)
  : TLFloat;
var
  i  : TLInt;
  J  : TLInt;
  J1 : TLInt;
  J2 : TLInt;
  v  : TLFloat;
  NRM: TLFloat;
  T  : TLVec;
begin
  A := DynamicArrayCopy(A);
  SetLength(T, n);
  i := 0;
  while i <= n - 1 do
    begin
      T[i] := 0;
      Inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      J := J1;
      while J <= J2 do
        begin
          if i = J then
            begin
              T[i] := T[i] + AbsComplex(A[i, i]);
            end
          else
            begin
              T[i] := T[i] + AbsComplex(A[i, J]);
              T[J] := T[J] + AbsComplex(A[i, J]);
            end;
          Inc(J);
        end;
      Inc(i);
    end;
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NRM := Max(NRM, T[i]);
      Inc(i);
    end;
  if HPDMatrixCholesky(A, n, IsUpper) then
    begin
      HPDMatrixRCondCholeskyInternal(A, n, IsUpper, True, NRM, v);
      Result := v;
    end
  else
    begin
      Result := -1;
    end;
end;

(* ************************************************************************
  Estimate of a matrix condition number (1-norm)

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function CMatrixRCond1(A: TLComplexMatrix; n: TLInt): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
  T     : TLVec;
begin
  A := DynamicArrayCopy(A);
  Assert(n >= 1, 'CMatrixRCond1: N<1!');
  SetLength(T, n);
  i := 0;
  while i <= n - 1 do
    begin
      T[i] := 0;
      Inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          T[J] := T[J] + AbsComplex(A[i, J]);
          Inc(J);
        end;
      Inc(i);
    end;
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NRM := Max(NRM, T[i]);
      Inc(i);
    end;
  CMatrixLU(A, n, n, Pivots);
  CMatrixRCondLUInternal(A, n, True, True, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Estimate of a matrix condition number (infinity-norm).

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function CMatrixRCondInf(A: TLComplexMatrix; n: TLInt): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
begin
  A := DynamicArrayCopy(A);
  Assert(n >= 1, 'CMatrixRCondInf: N<1!');
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      v := 0;
      J := 0;
      while J <= n - 1 do
        begin
          v := v + AbsComplex(A[i, J]);
          Inc(J);
        end;
      NRM := Max(NRM, v);
      Inc(i);
    end;
  CMatrixLU(A, n, n, Pivots);
  CMatrixRCondLUInternal(A, n, False, True, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Estimate of the condition number of a matrix given by its LU decomposition (1-norm)

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  LUA         -   LU decomposition of a matrix in compact form. Output of
  the RMatrixLU subroutine.
  N           -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function RMatrixLURCond1(const LUA: TLMatrix; n: TLInt)
  : TLFloat;
var
  v: TLFloat;
begin
  RMatrixRCondLUInternal(LUA, n, True, False, 0, v);
  Result := v;
end;

(* ************************************************************************
  Estimate of the condition number of a matrix given by its LU decomposition
  (infinity norm).

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  LUA     -   LU decomposition of a matrix in compact form. Output of
  the RMatrixLU subroutine.
  N       -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function RMatrixLURCondInf(const LUA: TLMatrix; n: TLInt)
  : TLFloat;
var
  v: TLFloat;
begin
  RMatrixRCondLUInternal(LUA, n, False, False, 0, v);
  Result := v;
end;

(* ************************************************************************
  Condition number estimate of a symmetric positive definite matrix given by
  Cholesky decomposition.

  The algorithm calculates a lower bound of the condition number. In this
  case, the algorithm does not return a lower bound of the condition number,
  but an inverse number (to avoid an overflow in case of a singular matrix).

  It should be noted that 1-norm and inf-norm condition numbers of symmetric
  matrices are equal, so the algorithm doesn't take into account the
  differences between these types of norms.

  Input parameters:
  CD  - Cholesky decomposition of matrix A,
  output of SMatrixCholesky subroutine.
  N   - size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function SPDMatrixCholeskyRCond(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean): TLFloat;
var
  v: TLFloat;
begin
  SPDMatrixRCondCholeskyInternal(A, n, IsUpper, False, 0, v);
  Result := v;
end;

(* ************************************************************************
  Condition number estimate of a Hermitian positive definite matrix given by
  Cholesky decomposition.

  The algorithm calculates a lower bound of the condition number. In this
  case, the algorithm does not return a lower bound of the condition number,
  but an inverse number (to avoid an overflow in case of a singular matrix).

  It should be noted that 1-norm and inf-norm condition numbers of symmetric
  matrices are equal, so the algorithm doesn't take into account the
  differences between these types of norms.

  Input parameters:
  CD  - Cholesky decomposition of matrix A,
  output of SMatrixCholesky subroutine.
  N   - size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate,  k(A)=INF,
  0.0 is returned in such cases.
  ************************************************************************ *)
function HPDMatrixCholeskyRCond(const A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean): TLFloat;
var
  v: TLFloat;
begin
  HPDMatrixRCondCholeskyInternal(A, n, IsUpper, False, 0, v);
  Result := v;
end;

(* ************************************************************************
  Estimate of the condition number of a matrix given by its LU decomposition (1-norm)

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  LUA         -   LU decomposition of a matrix in compact form. Output of
  the CMatrixLU subroutine.
  N           -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function CMatrixLURCond1(const LUA: TLComplexMatrix; n: TLInt)
  : TLFloat;
var
  v: TLFloat;
begin
  Assert(n >= 1, 'CMatrixLURCond1: N<1!');
  CMatrixRCondLUInternal(LUA, n, True, False, 0.0, v);
  Result := v;
end;

(* ************************************************************************
  Estimate of the condition number of a matrix given by its LU decomposition
  (infinity norm).

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  LUA     -   LU decomposition of a matrix in compact form. Output of
  the CMatrixLU subroutine.
  N       -   size of matrix A.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function CMatrixLURCondInf(const LUA: TLComplexMatrix; n: TLInt)
  : TLFloat;
var
  v: TLFloat;
begin
  Assert(n >= 1, 'CMatrixLURCondInf: N<1!');
  CMatrixRCondLUInternal(LUA, n, False, False, 0.0, v);
  Result := v;
end;

(* ************************************************************************
  Triangular matrix: estimate of a condition number (1-norm)

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A       -   matrix. Array[0..N-1, 0..N-1].
  N       -   size of A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function CMatrixTRRCond1(const A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
  T     : TLVec;
  J1    : TLInt;
  J2    : TLInt;
begin
  Assert(n >= 1, 'RMatrixTRRCond1: N<1!');
  SetLength(T, n);
  i := 0;
  while i <= n - 1 do
    begin
      T[i] := 0;
      Inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i + 1;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i - 1;
        end;
      J := J1;
      while J <= J2 do
        begin
          T[J] := T[J] + AbsComplex(A[i, J]);
          Inc(J);
        end;
      if IsUnit then
        begin
          T[i] := T[i] + 1;
        end
      else
        begin
          T[i] := T[i] + AbsComplex(A[i, i]);
        end;
      Inc(i);
    end;
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NRM := Max(NRM, T[i]);
      Inc(i);
    end;
  CMatrixRCondTRInternal(A, n, IsUpper, IsUnit, True, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Triangular matrix: estimate of a matrix condition number (infinity-norm).

  The algorithm calculates a lower bound of the condition number. In this case,
  the algorithm does not return a lower bound of the condition number, but an
  inverse number (to avoid an overflow in case of a singular matrix).

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Result: 1/LowerBound(cond(A))

  NOTE:
  if k(A) is very large, then matrix is  assumed  degenerate, k(A)=INF, 0.0 is returned in such cases.
  ************************************************************************ *)
function CMatrixTRRCondInf(const A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean): TLFloat;
var
  i     : TLInt;
  J     : TLInt;
  v     : TLFloat;
  NRM   : TLFloat;
  Pivots: TLIVec;
  J1    : TLInt;
  J2    : TLInt;
begin
  Assert(n >= 1, 'RMatrixTRRCondInf: N<1!');
  NRM := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i + 1;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i - 1;
        end;
      v := 0;
      J := J1;
      while J <= J2 do
        begin
          v := v + AbsComplex(A[i, J]);
          Inc(J);
        end;
      if IsUnit then
        begin
          v := v + 1;
        end
      else
        begin
          v := v + AbsComplex(A[i, i]);
        end;
      NRM := Max(NRM, v);
      Inc(i);
    end;
  CMatrixRCondTRInternal(A, n, IsUpper, IsUnit, False, NRM, v);
  Result := v;
end;

(* ************************************************************************
  Threshold for rcond: matrices with condition number beyond this  threshold
  are considered singular.

  Threshold must be far enough from underflow, at least Sqr(Threshold) must be greater than underflow.
  ************************************************************************ *)
function RCondThreshold(): TLFloat;
begin
  Result := Sqrt(Sqrt(MinRealNumber));
end;

(* ************************************************************************
  Internal subroutine for condition number estimation
  ************************************************************************ *)
procedure RMatrixRCondTRInternal(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OneNorm: Boolean; ANORM: TLFloat;
  var RC: TLFloat);
var
  EX       : TLVec;
  EV       : TLVec;
  IWORK    : TLIVec;
  tmp      : TLVec;
  v        : TLFloat;
  i        : TLInt;
  J        : TLInt;
  KASE     : TLInt;
  KASE1    : TLInt;
  J1       : TLInt;
  J2       : TLInt;
  AINVNM   : TLFloat;
  MaxGrowth: TLFloat;
  s        : TLFloat;
  MUpper   : Boolean;
  MTrans   : Boolean;
  Munit    : Boolean;
begin

  //
  // RC=0 if something happens
  //
  RC := 0;

  //
  // init
  //
  if OneNorm then
    begin
      KASE1 := 1;
    end
  else
    begin
      KASE1 := 2;
    end;
  MUpper := True;
  MTrans := True;
  Munit := True;
  SetLength(IWORK, n + 1);
  SetLength(tmp, n);

  //
  // prepare parameters for triangular solver
  //
  MaxGrowth := 1 / RCondThreshold;
  s := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i + 1;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i - 1;
        end;
      J := J1;
      while J <= J2 do
        begin
          s := Max(s, AbsReal(A[i, J]));
          Inc(J);
        end;
      if IsUnit then
        begin
          s := Max(s, 1);
        end
      else
        begin
          s := Max(s, AbsReal(A[i, i]));
        end;
      Inc(i);
    end;
  if AP_FP_Eq(s, 0) then
    begin
      s := 1;
    end;
  s := 1 / s;

  //
  // Scale according to S
  //
  ANORM := ANORM * s;

  //
  // Quick return if possible
  // We assume that ANORM<>0 after this block
  //
  if AP_FP_Eq(ANORM, 0) then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      RC := 1;
      Exit;
    end;

  //
  // Estimate the norm of inv(A).
  //
  AINVNM := 0;
  KASE := 0;
  while True do
    begin
      RMatrixEstimateNorm(n, EV, EX, IWORK, AINVNM, KASE);
      if KASE = 0 then
        begin
          Break;
        end;

      //
      // from 1-based array to 0-based
      //
      i := 0;
      while i <= n - 1 do
        begin
          EX[i] := EX[i + 1];
          Inc(i);
        end;

      //
      // multiply by inv(A) or inv(A')
      //
      if KASE = KASE1 then
        begin

          //
          // multiply by inv(A)
          //
          if not RMatrixScaledTRSafeSolve(A, s, n, EX, IsUpper, 0, IsUnit, MaxGrowth)
          then
            begin
              Exit;
            end;
        end
      else
        begin

          //
          // multiply by inv(A')
          //
          if not RMatrixScaledTRSafeSolve(A, s, n, EX, IsUpper, 1, IsUnit, MaxGrowth)
          then
            begin
              Exit;
            end;
        end;

      //
      // from 0-based array to 1-based
      //
      i := n - 1;
      while i >= 0 do
        begin
          EX[i + 1] := EX[i];
          Dec(i);
        end;
    end;

  //
  // Compute the estimate of the reciprocal condition number.
  //
  if AP_FP_NEq(AINVNM, 0) then
    begin
      RC := 1 / AINVNM;
      RC := RC / ANORM;
      if AP_FP_Less(RC, RCondThreshold) then
        begin
          RC := 0;
        end;
    end;
end;

(* ************************************************************************
  Condition number estimation
  ************************************************************************ *)
procedure CMatrixRCondTRInternal(const A: TLComplexMatrix;
  const n: TLInt; IsUpper: Boolean; IsUnit: Boolean; OneNorm: Boolean;
  ANORM: TLFloat; var RC: TLFloat);
var
  EX       : TLComplexVec;
  CWORK2   : TLComplexVec;
  CWORK3   : TLComplexVec;
  CWORK4   : TLComplexVec;
  ISAVE    : TLIVec;
  RSAVE    : TLVec;
  KASE     : TLInt;
  KASE1    : TLInt;
  AINVNM   : TLFloat;
  v        : TLComplex;
  i        : TLInt;
  J        : TLInt;
  J1       : TLInt;
  J2       : TLInt;
  s        : TLFloat;
  MaxGrowth: TLFloat;
begin

  //
  // RC=0 if something happens
  //
  RC := 0;

  //
  // init
  //
  if n <= 0 then
    begin
      Exit;
    end;
  if n = 0 then
    begin
      RC := 1;
      Exit;
    end;
  SetLength(CWORK2, n + 1);

  //
  // prepare parameters for triangular solver
  //
  MaxGrowth := 1 / RCondThreshold;
  s := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i + 1;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i - 1;
        end;
      J := J1;
      while J <= J2 do
        begin
          s := Max(s, AbsComplex(A[i, J]));
          Inc(J);
        end;
      if IsUnit then
        begin
          s := Max(s, 1);
        end
      else
        begin
          s := Max(s, AbsComplex(A[i, i]));
        end;
      Inc(i);
    end;
  if AP_FP_Eq(s, 0) then
    begin
      s := 1;
    end;
  s := 1 / s;

  //
  // Scale according to S
  //
  ANORM := ANORM * s;

  //
  // Quick return if possible
  //
  if AP_FP_Eq(ANORM, 0) then
    begin
      Exit;
    end;

  //
  // Estimate the norm of inv(A).
  //
  AINVNM := 0;
  if OneNorm then
    begin
      KASE1 := 1;
    end
  else
    begin
      KASE1 := 2;
    end;
  KASE := 0;
  while True do
    begin
      CMatrixEstimateNorm(n, CWORK4, EX, AINVNM, KASE, ISAVE, RSAVE);
      if KASE = 0 then
        begin
          Break;
        end;

      //
      // From 1-based to 0-based
      //
      i := 0;
      while i <= n - 1 do
        begin
          EX[i] := EX[i + 1];
          Inc(i);
        end;

      //
      // multiply by inv(A) or inv(A')
      //
      if KASE = KASE1 then
        begin

          //
          // multiply by inv(A)
          //
          if not CMatrixScaledTRSafeSolve(A, s, n, EX, IsUpper, 0, IsUnit, MaxGrowth)
          then
            begin
              Exit;
            end;
        end
      else
        begin

          //
          // multiply by inv(A')
          //
          if not CMatrixScaledTRSafeSolve(A, s, n, EX, IsUpper, 2, IsUnit, MaxGrowth)
          then
            begin
              Exit;
            end;
        end;

      //
      // from 0-based to 1-based
      //
      i := n - 1;
      while i >= 0 do
        begin
          EX[i + 1] := EX[i];
          Dec(i);
        end;
    end;

  //
  // Compute the estimate of the reciprocal condition number.
  //
  if AP_FP_NEq(AINVNM, 0) then
    begin
      RC := 1 / AINVNM;
      RC := RC / ANORM;
      if AP_FP_Less(RC, RCondThreshold) then
        begin
          RC := 0;
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine for condition number estimation
  ************************************************************************ *)
procedure SPDMatrixRCondCholeskyInternal(const CHA: TLMatrix;
  n: TLInt; IsUpper: Boolean; IsNormProvided: Boolean;
  ANORM: TLFloat; var RC: TLFloat);
var
  i        : TLInt;
  J        : TLInt;
  KASE     : TLInt;
  AINVNM   : TLFloat;
  EX       : TLVec;
  EV       : TLVec;
  tmp      : TLVec;
  IWORK    : TLIVec;
  SA       : TLFloat;
  v        : TLFloat;
  MaxGrowth: TLFloat;
begin
  Assert(n >= 1);
  SetLength(tmp, n);

  //
  // RC=0 if something happens
  //
  RC := 0;

  //
  // prepare parameters for triangular solver
  //
  MaxGrowth := 1 / RCondThreshold;
  SA := 0;
  if IsUpper then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := i;
          while J <= n - 1 do
            begin
              SA := Max(SA, AbsComplex(C_Complex(CHA[i, J])));
              Inc(J);
            end;
          Inc(i);
        end;
    end
  else
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= i do
            begin
              SA := Max(SA, AbsComplex(C_Complex(CHA[i, J])));
              Inc(J);
            end;
          Inc(i);
        end;
    end;
  if AP_FP_Eq(SA, 0) then
    begin
      SA := 1;
    end;
  SA := 1 / SA;

  //
  // Estimate the norm of A.
  //
  if not IsNormProvided then
    begin
      KASE := 0;
      ANORM := 0;
      while True do
        begin
          RMatrixEstimateNorm(n, EV, EX, IWORK, ANORM, KASE);
          if KASE = 0 then
            begin
              Break;
            end;
          if IsUpper then
            begin

              //
              // Multiply by U
              //
              i := 1;
              while i <= n do
                begin
                  v := APVDotProduct(@CHA[i - 1][0], i - 1, n - 1, @EX[0], i, n);
                  EX[i] := v;
                  Inc(i);
                end;
              APVMul(@EX[0], 1, n, SA);

              //
              // Multiply by U'
              //
              i := 0;
              while i <= n - 1 do
                begin
                  tmp[i] := 0;
                  Inc(i);
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := EX[i + 1];
                  APVAdd(@tmp[0], i, n - 1, @CHA[i][0], i, n - 1, v);
                  Inc(i);
                end;
              APVMove(@EX[0], 1, n, @tmp[0], 0, n - 1);
              APVMul(@EX[0], 1, n, SA);
            end
          else
            begin

              //
              // Multiply by L'
              //
              i := 0;
              while i <= n - 1 do
                begin
                  tmp[i] := 0;
                  Inc(i);
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := EX[i + 1];
                  APVAdd(@tmp[0], 0, i, @CHA[i][0], 0, i, v);
                  Inc(i);
                end;
              APVMove(@EX[0], 1, n, @tmp[0], 0, n - 1);
              APVMul(@EX[0], 1, n, SA);

              //
              // Multiply by L
              //
              i := n;
              while i >= 1 do
                begin
                  v := APVDotProduct(@CHA[i - 1][0], 0, i - 1, @EX[0], 1, i);
                  EX[i] := v;
                  Dec(i);
                end;
              APVMul(@EX[0], 1, n, SA);
            end;
        end;
    end;

  //
  // Quick return if possible
  //
  if AP_FP_Eq(ANORM, 0) then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      RC := 1;
      Exit;
    end;

  //
  // Estimate the 1-norm of inv(A).
  //
  KASE := 0;
  while True do
    begin
      RMatrixEstimateNorm(n, EV, EX, IWORK, AINVNM, KASE);
      if KASE = 0 then
        begin
          Break;
        end;
      i := 0;
      while i <= n - 1 do
        begin
          EX[i] := EX[i + 1];
          Inc(i);
        end;
      if IsUpper then
        begin

          //
          // Multiply by inv(U').
          //
          if not RMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 1, False,
            MaxGrowth) then
            begin
              Exit;
            end;

          //
          // Multiply by inv(U).
          //
          if not RMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 0, False,
            MaxGrowth) then
            begin
              Exit;
            end;
        end
      else
        begin

          //
          // Multiply by inv(L).
          //
          if not RMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 0, False,
            MaxGrowth) then
            begin
              Exit;
            end;

          //
          // Multiply by inv(L').
          //
          if not RMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 1, False,
            MaxGrowth) then
            begin
              Exit;
            end;
        end;
      i := n - 1;
      while i >= 0 do
        begin
          EX[i + 1] := EX[i];
          Dec(i);
        end;
    end;

  //
  // Compute the estimate of the reciprocal condition number.
  //
  if AP_FP_NEq(AINVNM, 0) then
    begin
      v := 1 / AINVNM;
      RC := v / ANORM;
      if AP_FP_Less(RC, RCondThreshold) then
        begin
          RC := 0;
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine for condition number estimation
  ************************************************************************ *)
procedure HPDMatrixRCondCholeskyInternal(const CHA: TLComplexMatrix;
  n: TLInt; IsUpper: Boolean; IsNormProvided: Boolean;
  ANORM: TLFloat; var RC: TLFloat);
var
  ISAVE    : TLIVec;
  RSAVE    : TLVec;
  EX       : TLComplexVec;
  EV       : TLComplexVec;
  tmp      : TLComplexVec;
  KASE     : TLInt;
  AINVNM   : TLFloat;
  v        : TLComplex;
  i        : TLInt;
  J        : TLInt;
  SA       : TLFloat;
  MaxGrowth: TLFloat;
  i_       : TLInt;
  i1_      : TLInt;
begin
  Assert(n >= 1);
  SetLength(tmp, n);

  //
  // RC=0 if something happens
  //
  RC := 0;

  //
  // prepare parameters for triangular solver
  //
  MaxGrowth := 1 / RCondThreshold;
  SA := 0;
  if IsUpper then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := i;
          while J <= n - 1 do
            begin
              SA := Max(SA, AbsComplex(CHA[i, J]));
              Inc(J);
            end;
          Inc(i);
        end;
    end
  else
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= i do
            begin
              SA := Max(SA, AbsComplex(CHA[i, J]));
              Inc(J);
            end;
          Inc(i);
        end;
    end;
  if AP_FP_Eq(SA, 0) then
    begin
      SA := 1;
    end;
  SA := 1 / SA;

  //
  // Estimate the norm of A
  //
  if not IsNormProvided then
    begin
      ANORM := 0;
      KASE := 0;
      while True do
        begin
          CMatrixEstimateNorm(n, EV, EX, ANORM, KASE, ISAVE, RSAVE);
          if KASE = 0 then
            begin
              Break;
            end;
          if IsUpper then
            begin

              //
              // Multiply by U
              //
              i := 1;
              while i <= n do
                begin
                  i1_ := (i) - (i - 1);
                  v := C_Complex(0.0);
                  for i_ := i - 1 to n - 1 do
                    begin
                      v := C_Add(v, C_Mul(CHA[i - 1, i_], EX[i_ + i1_]));
                    end;
                  EX[i] := v;
                  Inc(i);
                end;
              for i_ := 1 to n do
                begin
                  EX[i_] := C_MulR(EX[i_], SA);
                end;

              //
              // Multiply by U'
              //
              i := 0;
              while i <= n - 1 do
                begin
                  tmp[i] := C_Complex(0);
                  Inc(i);
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := EX[i + 1];
                  for i_ := i to n - 1 do
                    begin
                      tmp[i_] := C_Add(tmp[i_], C_Mul(v, Conj(CHA[i, i_])));
                    end;
                  Inc(i);
                end;
              i1_ := (0) - (1);
              for i_ := 1 to n do
                begin
                  EX[i_] := tmp[i_ + i1_];
                end;
              for i_ := 1 to n do
                begin
                  EX[i_] := C_MulR(EX[i_], SA);
                end;
            end
          else
            begin

              //
              // Multiply by L'
              //
              i := 0;
              while i <= n - 1 do
                begin
                  tmp[i] := C_Complex(0);
                  Inc(i);
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := EX[i + 1];
                  for i_ := 0 to i do
                    begin
                      tmp[i_] := C_Add(tmp[i_], C_Mul(v, Conj(CHA[i, i_])));
                    end;
                  Inc(i);
                end;
              i1_ := (0) - (1);
              for i_ := 1 to n do
                begin
                  EX[i_] := tmp[i_ + i1_];
                end;
              for i_ := 1 to n do
                begin
                  EX[i_] := C_MulR(EX[i_], SA);
                end;

              //
              // Multiply by L
              //
              i := n;
              while i >= 1 do
                begin
                  i1_ := (1) - (0);
                  v := C_Complex(0.0);
                  for i_ := 0 to i - 1 do
                    begin
                      v := C_Add(v, C_Mul(CHA[i - 1, i_], EX[i_ + i1_]));
                    end;
                  EX[i] := v;
                  Dec(i);
                end;
              for i_ := 1 to n do
                begin
                  EX[i_] := C_MulR(EX[i_], SA);
                end;
            end;
        end;
    end;

  //
  // Quick return if possible
  // After this block we assume that ANORM<>0
  //
  if AP_FP_Eq(ANORM, 0) then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      RC := 1;
      Exit;
    end;

  //
  // Estimate the norm of inv(A).
  //
  AINVNM := 0;
  KASE := 0;
  while True do
    begin
      CMatrixEstimateNorm(n, EV, EX, AINVNM, KASE, ISAVE, RSAVE);
      if KASE = 0 then
        begin
          Break;
        end;
      i := 0;
      while i <= n - 1 do
        begin
          EX[i] := EX[i + 1];
          Inc(i);
        end;
      if IsUpper then
        begin

          //
          // Multiply by inv(U').
          //
          if not CMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 2, False,
            MaxGrowth) then
            begin
              Exit;
            end;

          //
          // Multiply by inv(U).
          //
          if not CMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 0, False,
            MaxGrowth) then
            begin
              Exit;
            end;
        end
      else
        begin

          //
          // Multiply by inv(L).
          //
          if not CMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 0, False,
            MaxGrowth) then
            begin
              Exit;
            end;

          //
          // Multiply by inv(L').
          //
          if not CMatrixScaledTRSafeSolve(CHA, SA, n, EX, IsUpper, 2, False,
            MaxGrowth) then
            begin
              Exit;
            end;
        end;
      i := n - 1;
      while i >= 0 do
        begin
          EX[i + 1] := EX[i];
          Dec(i);
        end;
    end;

  //
  // Compute the estimate of the reciprocal condition number.
  //
  if AP_FP_NEq(AINVNM, 0) then
    begin
      RC := 1 / AINVNM;
      RC := RC / ANORM;
      if AP_FP_Less(RC, RCondThreshold) then
        begin
          RC := 0;
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine for condition number estimation
  ************************************************************************ *)
procedure RMatrixRCondLUInternal(const LUA: TLMatrix; n: TLInt;
  OneNorm: Boolean; IsANormProvided: Boolean; ANORM: TLFloat;
  var RC: TLFloat);
var
  EX       : TLVec;
  EV       : TLVec;
  IWORK    : TLIVec;
  tmp      : TLVec;
  v        : TLFloat;
  i        : TLInt;
  J        : TLInt;
  KASE     : TLInt;
  KASE1    : TLInt;
  AINVNM   : TLFloat;
  MaxGrowth: TLFloat;
  SU       : TLFloat;
  SL       : TLFloat;
  MUpper   : Boolean;
  MTrans   : Boolean;
  Munit    : Boolean;
begin

  //
  // RC=0 if something happens
  //
  RC := 0;

  //
  // init
  //
  if OneNorm then
    begin
      KASE1 := 1;
    end
  else
    begin
      KASE1 := 2;
    end;
  MUpper := True;
  MTrans := True;
  Munit := True;
  SetLength(IWORK, n + 1);
  SetLength(tmp, n);

  //
  // prepare parameters for triangular solver
  //
  MaxGrowth := 1 / RCondThreshold;
  SU := 0;
  SL := 1;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= i - 1 do
        begin
          SL := Max(SL, AbsReal(LUA[i, J]));
          Inc(J);
        end;
      J := i;
      while J <= n - 1 do
        begin
          SU := Max(SU, AbsReal(LUA[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(SU, 0) then
    begin
      SU := 1;
    end;
  SU := 1 / SU;
  SL := 1 / SL;

  //
  // Estimate the norm of A.
  //
  if not IsANormProvided then
    begin
      KASE := 0;
      ANORM := 0;
      while True do
        begin
          RMatrixEstimateNorm(n, EV, EX, IWORK, ANORM, KASE);
          if KASE = 0 then
            begin
              Break;
            end;
          if KASE = KASE1 then
            begin

              //
              // Multiply by U
              //
              i := 1;
              while i <= n do
                begin
                  v := APVDotProduct(@LUA[i - 1][0], i - 1, n - 1, @EX[0], i, n);
                  EX[i] := v;
                  Inc(i);
                end;

              //
              // Multiply by L
              //
              i := n;
              while i >= 1 do
                begin
                  if i > 1 then
                    begin
                      v := APVDotProduct(@LUA[i - 1][0], 0, i - 2, @EX[0], 1, i - 1);
                    end
                  else
                    begin
                      v := 0;
                    end;
                  EX[i] := EX[i] + v;
                  Dec(i);
                end;
            end
          else
            begin

              //
              // Multiply by L'
              //
              i := 0;
              while i <= n - 1 do
                begin
                  tmp[i] := 0;
                  Inc(i);
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := EX[i + 1];
                  if i >= 1 then
                    begin
                      APVAdd(@tmp[0], 0, i - 1, @LUA[i][0], 0, i - 1, v);
                    end;
                  tmp[i] := tmp[i] + v;
                  Inc(i);
                end;
              APVMove(@EX[0], 1, n, @tmp[0], 0, n - 1);

              //
              // Multiply by U'
              //
              i := 0;
              while i <= n - 1 do
                begin
                  tmp[i] := 0;
                  Inc(i);
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := EX[i + 1];
                  APVAdd(@tmp[0], i, n - 1, @LUA[i][0], i, n - 1, v);
                  Inc(i);
                end;
              APVMove(@EX[0], 1, n, @tmp[0], 0, n - 1);
            end;
        end;
    end;

  //
  // Scale according to SU/SL
  //
  ANORM := ANORM * SU * SL;

  //
  // Quick return if possible
  // We assume that ANORM<>0 after this block
  //
  if AP_FP_Eq(ANORM, 0) then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      RC := 1;
      Exit;
    end;

  //
  // Estimate the norm of inv(A).
  //
  AINVNM := 0;
  KASE := 0;
  while True do
    begin
      RMatrixEstimateNorm(n, EV, EX, IWORK, AINVNM, KASE);
      if KASE = 0 then
        begin
          Break;
        end;

      //
      // from 1-based array to 0-based
      //
      i := 0;
      while i <= n - 1 do
        begin
          EX[i] := EX[i + 1];
          Inc(i);
        end;

      //
      // multiply by inv(A) or inv(A')
      //
      if KASE = KASE1 then
        begin

          //
          // Multiply by inv(L).
          //
          if not RMatrixScaledTRSafeSolve(LUA, SL, n, EX, not MUpper, 0, Munit,
            MaxGrowth) then
            begin
              Exit;
            end;

          //
          // Multiply by inv(U).
          //
          if not RMatrixScaledTRSafeSolve(LUA, SU, n, EX, MUpper, 0, not Munit,
            MaxGrowth) then
            begin
              Exit;
            end;
        end
      else
        begin

          //
          // Multiply by inv(U').
          //
          if not RMatrixScaledTRSafeSolve(LUA, SU, n, EX, MUpper, 1, not Munit,
            MaxGrowth) then
            begin
              Exit;
            end;

          //
          // Multiply by inv(L').
          //
          if not RMatrixScaledTRSafeSolve(LUA, SL, n, EX, not MUpper, 1, Munit,
            MaxGrowth) then
            begin
              Exit;
            end;
        end;

      //
      // from 0-based array to 1-based
      //
      i := n - 1;
      while i >= 0 do
        begin
          EX[i + 1] := EX[i];
          Dec(i);
        end;
    end;

  //
  // Compute the estimate of the reciprocal condition number.
  //
  if AP_FP_NEq(AINVNM, 0) then
    begin
      RC := 1 / AINVNM;
      RC := RC / ANORM;
      if AP_FP_Less(RC, RCondThreshold) then
        begin
          RC := 0;
        end;
    end;
end;

(* ************************************************************************
  Condition number estimation
  ************************************************************************ *)
procedure CMatrixRCondLUInternal(const LUA: TLComplexMatrix;
  const n: TLInt; OneNorm: Boolean; IsANormProvided: Boolean;
  ANORM: TLFloat; var RC: TLFloat);
var
  EX       : TLComplexVec;
  CWORK2   : TLComplexVec;
  CWORK3   : TLComplexVec;
  CWORK4   : TLComplexVec;
  ISAVE    : TLIVec;
  RSAVE    : TLVec;
  KASE     : TLInt;
  KASE1    : TLInt;
  AINVNM   : TLFloat;
  v        : TLComplex;
  i        : TLInt;
  J        : TLInt;
  SU       : TLFloat;
  SL       : TLFloat;
  MaxGrowth: TLFloat;
  i_       : TLInt;
  i1_      : TLInt;
begin
  if n <= 0 then
    begin
      Exit;
    end;
  SetLength(CWORK2, n + 1);
  RC := 0;
  if n = 0 then
    begin
      RC := 1;
      Exit;
    end;

  //
  // prepare parameters for triangular solver
  //
  MaxGrowth := 1 / RCondThreshold;
  SU := 0;
  SL := 1;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= i - 1 do
        begin
          SL := Max(SL, AbsComplex(LUA[i, J]));
          Inc(J);
        end;
      J := i;
      while J <= n - 1 do
        begin
          SU := Max(SU, AbsComplex(LUA[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(SU, 0) then
    begin
      SU := 1;
    end;
  SU := 1 / SU;
  SL := 1 / SL;

  //
  // Estimate the norm of SU*SL*A.
  //
  if not IsANormProvided then
    begin
      ANORM := 0;
      if OneNorm then
        begin
          KASE1 := 1;
        end
      else
        begin
          KASE1 := 2;
        end;
      KASE := 0;
      repeat
        CMatrixEstimateNorm(n, CWORK4, EX, ANORM, KASE, ISAVE, RSAVE);
        if KASE <> 0 then
          begin
            if KASE = KASE1 then
              begin

                //
                // Multiply by U
                //
                i := 1;
                while i <= n do
                  begin
                    i1_ := (i) - (i - 1);
                    v := C_Complex(0.0);
                    for i_ := i - 1 to n - 1 do
                      begin
                        v := C_Add(v, C_Mul(LUA[i - 1, i_], EX[i_ + i1_]));
                      end;
                    EX[i] := v;
                    Inc(i);
                  end;

                //
                // Multiply by L
                //
                i := n;
                while i >= 1 do
                  begin
                    v := C_Complex(0);
                    if i > 1 then
                      begin
                        i1_ := (1) - (0);
                        v := C_Complex(0.0);
                        for i_ := 0 to i - 2 do
                          begin
                            v := C_Add(v, C_Mul(LUA[i - 1, i_], EX[i_ + i1_]));
                          end;
                      end;
                    EX[i] := C_Add(v, EX[i]);
                    Dec(i);
                  end;
              end
            else
              begin

                //
                // Multiply by L'
                //
                i := 1;
                while i <= n do
                  begin
                    CWORK2[i] := C_Complex(0);
                    Inc(i);
                  end;
                i := 1;
                while i <= n do
                  begin
                    v := EX[i];
                    if i > 1 then
                      begin
                        i1_ := (0) - (1);
                        for i_ := 1 to i - 1 do
                          begin
                            CWORK2[i_] := C_Add(CWORK2[i_],
                              C_Mul(v, Conj(LUA[i - 1, i_ + i1_])));
                          end;
                      end;
                    CWORK2[i] := C_Add(CWORK2[i], v);
                    Inc(i);
                  end;

                //
                // Multiply by U'
                //
                i := 1;
                while i <= n do
                  begin
                    EX[i] := C_Complex(0);
                    Inc(i);
                  end;
                i := 1;
                while i <= n do
                  begin
                    v := CWORK2[i];
                    i1_ := (i - 1) - (i);
                    for i_ := i to n do
                      begin
                        EX[i_] := C_Add(EX[i_], C_Mul(v, Conj(LUA[i - 1, i_ + i1_])));
                      end;
                    Inc(i);
                  end;
              end;
          end;
      until KASE = 0;
    end;

  //
  // Scale according to SU/SL
  //
  ANORM := ANORM * SU * SL;

  //
  // Quick return if possible
  //
  if AP_FP_Eq(ANORM, 0) then
    begin
      Exit;
    end;

  //
  // Estimate the norm of inv(A).
  //
  AINVNM := 0;
  if OneNorm then
    begin
      KASE1 := 1;
    end
  else
    begin
      KASE1 := 2;
    end;
  KASE := 0;
  while True do
    begin
      CMatrixEstimateNorm(n, CWORK4, EX, AINVNM, KASE, ISAVE, RSAVE);
      if KASE = 0 then
        begin
          Break;
        end;

      //
      // From 1-based to 0-based
      //
      i := 0;
      while i <= n - 1 do
        begin
          EX[i] := EX[i + 1];
          Inc(i);
        end;

      //
      // multiply by inv(A) or inv(A')
      //
      if KASE = KASE1 then
        begin

          //
          // Multiply by inv(L).
          //
          if not CMatrixScaledTRSafeSolve(LUA, SL, n, EX, False, 0, True, MaxGrowth)
          then
            begin
              RC := 0;
              Exit;
            end;

          //
          // Multiply by inv(U).
          //
          if not CMatrixScaledTRSafeSolve(LUA, SU, n, EX, True, 0, False, MaxGrowth)
          then
            begin
              RC := 0;
              Exit;
            end;
        end
      else
        begin

          //
          // Multiply by inv(U').
          //
          if not CMatrixScaledTRSafeSolve(LUA, SU, n, EX, True, 2, False, MaxGrowth)
          then
            begin
              RC := 0;
              Exit;
            end;

          //
          // Multiply by inv(L').
          //
          if not CMatrixScaledTRSafeSolve(LUA, SL, n, EX, False, 2, True, MaxGrowth)
          then
            begin
              RC := 0;
              Exit;
            end;
        end;

      //
      // from 0-based to 1-based
      //
      i := n - 1;
      while i >= 0 do
        begin
          EX[i + 1] := EX[i];
          Dec(i);
        end;
    end;

  //
  // Compute the estimate of the reciprocal condition number.
  //
  if AP_FP_NEq(AINVNM, 0) then
    begin
      RC := 1 / AINVNM;
      RC := RC / ANORM;
      if AP_FP_Less(RC, RCondThreshold) then
        begin
          RC := 0;
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine for matrix norm estimation
  ************************************************************************ *)
procedure RMatrixEstimateNorm(n: TLInt; var v: TLVec;
  var X: TLVec; var ISGN: TLIVec; var EST: TLFloat;
  var KASE: TLInt);
var
  ITMAX    : TLInt;
  i        : TLInt;
  T        : TLFloat;
  flg      : Boolean;
  PosITER  : TLInt;
  PosJ     : TLInt;
  PosJLAST : TLInt;
  PosJUMP  : TLInt;
  PosALTSGN: TLInt;
  PosESTOLD: TLInt;
  PosTEMP  : TLInt;
begin
  ITMAX := 5;
  PosALTSGN := n + 1;
  PosESTOLD := n + 2;
  PosTEMP := n + 3;
  PosITER := n + 1;
  PosJ := n + 2;
  PosJLAST := n + 3;
  PosJUMP := n + 4;
  if KASE = 0 then
    begin
      SetLength(v, n + 4);
      SetLength(X, n + 1);
      SetLength(ISGN, n + 5);
      T := AP_Float(1) / n;
      i := 1;
      while i <= n do
        begin
          X[i] := T;
          Inc(i);
        end;
      KASE := 1;
      ISGN[PosJUMP] := 1;
      Exit;
    end;

  //
  // ................ ENTRY   (JUMP = 1)
  // FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
  //
  if ISGN[PosJUMP] = 1 then
    begin
      if n = 1 then
        begin
          v[1] := X[1];
          EST := AbsReal(v[1]);
          KASE := 0;
          Exit;
        end;
      EST := 0;
      i := 1;
      while i <= n do
        begin
          EST := EST + AbsReal(X[i]);
          Inc(i);
        end;
      i := 1;
      while i <= n do
        begin
          if AP_FP_Greater_Eq(X[i], 0) then
            begin
              X[i] := 1;
            end
          else
            begin
              X[i] := -1;
            end;
          ISGN[i] := Sign(X[i]);
          Inc(i);
        end;
      KASE := 2;
      ISGN[PosJUMP] := 2;
      Exit;
    end;

  //
  // ................ ENTRY   (JUMP = 2)
  // FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
  //
  if ISGN[PosJUMP] = 2 then
    begin
      ISGN[PosJ] := 1;
      i := 2;
      while i <= n do
        begin
          if AP_FP_Greater(AbsReal(X[i]), AbsReal(X[ISGN[PosJ]])) then
            begin
              ISGN[PosJ] := i;
            end;
          Inc(i);
        end;
      ISGN[PosITER] := 2;

      //
      // MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
      //
      i := 1;
      while i <= n do
        begin
          X[i] := 0;
          Inc(i);
        end;
      X[ISGN[PosJ]] := 1;
      KASE := 1;
      ISGN[PosJUMP] := 3;
      Exit;
    end;

  //
  // ................ ENTRY   (JUMP = 3)
  // X HAS BEEN OVERWRITTEN BY A*X.
  //
  if ISGN[PosJUMP] = 3 then
    begin
      APVMove(@v[0], 1, n, @X[0], 1, n);
      v[PosESTOLD] := EST;
      EST := 0;
      i := 1;
      while i <= n do
        begin
          EST := EST + AbsReal(v[i]);
          Inc(i);
        end;
      flg := False;
      i := 1;
      while i <= n do
        begin
          if AP_FP_Greater_Eq(X[i], 0) and (ISGN[i] < 0) or AP_FP_Less(X[i], 0) and
            (ISGN[i] >= 0) then
            begin
              flg := True;
            end;
          Inc(i);
        end;

      //
      // REPEATED SIGN VECTOR DETECTED, HENCE ALGORITHM HAS CONVERGED.
      // OR MAY BE CYCLING.
      //
      if not flg or AP_FP_Less_Eq(EST, v[PosESTOLD]) then
        begin
          v[PosALTSGN] := 1;
          i := 1;
          while i <= n do
            begin
              X[i] := v[PosALTSGN] * (1 + AP_Float((i - 1)) / (n - 1));
              v[PosALTSGN] := -v[PosALTSGN];
              Inc(i);
            end;
          KASE := 1;
          ISGN[PosJUMP] := 5;
          Exit;
        end;
      i := 1;
      while i <= n do
        begin
          if AP_FP_Greater_Eq(X[i], 0) then
            begin
              X[i] := 1;
              ISGN[i] := 1;
            end
          else
            begin
              X[i] := -1;
              ISGN[i] := -1;
            end;
          Inc(i);
        end;
      KASE := 2;
      ISGN[PosJUMP] := 4;
      Exit;
    end;

  //
  // ................ ENTRY   (JUMP = 4)
  // X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
  //
  if ISGN[PosJUMP] = 4 then
    begin
      ISGN[PosJLAST] := ISGN[PosJ];
      ISGN[PosJ] := 1;
      i := 2;
      while i <= n do
        begin
          if AP_FP_Greater(AbsReal(X[i]), AbsReal(X[ISGN[PosJ]])) then
            begin
              ISGN[PosJ] := i;
            end;
          Inc(i);
        end;
      if AP_FP_NEq(X[ISGN[PosJLAST]], AbsReal(X[ISGN[PosJ]])) and
        (ISGN[PosITER] < ITMAX) then
        begin
          ISGN[PosITER] := ISGN[PosITER] + 1;
          i := 1;
          while i <= n do
            begin
              X[i] := 0;
              Inc(i);
            end;
          X[ISGN[PosJ]] := 1;
          KASE := 1;
          ISGN[PosJUMP] := 3;
          Exit;
        end;

      //
      // ITERATION COMPLETE.  FINAL STAGE.
      //
      v[PosALTSGN] := 1;
      i := 1;
      while i <= n do
        begin
          X[i] := v[PosALTSGN] * (1 + AP_Float((i - 1)) / (n - 1));
          v[PosALTSGN] := -v[PosALTSGN];
          Inc(i);
        end;
      KASE := 1;
      ISGN[PosJUMP] := 5;
      Exit;
    end;

  //
  // ................ ENTRY   (JUMP = 5)
  // X HAS BEEN OVERWRITTEN BY A*X.
  //
  if ISGN[PosJUMP] = 5 then
    begin
      v[PosTEMP] := 0;
      i := 1;
      while i <= n do
        begin
          v[PosTEMP] := v[PosTEMP] + AbsReal(X[i]);
          Inc(i);
        end;
      v[PosTEMP] := 2 * v[PosTEMP] / (3 * n);
      if AP_FP_Greater(v[PosTEMP], EST) then
        begin
          APVMove(@v[0], 1, n, @X[0], 1, n);
          EST := v[PosTEMP];
        end;
      KASE := 0;
      Exit;
    end;
end;

procedure CMatrixEstimateNorm(const n: TLInt; var v: TLComplexVec;
  var X: TLComplexVec; var EST: TLFloat; var KASE: TLInt;
  var ISAVE: TLIVec; var RSAVE: TLVec);
var
  ITMAX : TLInt;
  i     : TLInt;
  ITER  : TLInt;
  J     : TLInt;
  JLAST : TLInt;
  JUMP  : TLInt;
  ABSXI : TLFloat;
  ALTSGN: TLFloat;
  ESTOLD: TLFloat;
  SAFMIN: TLFloat;
  Temp  : TLFloat;
  i_    : TLInt;
begin

  //
  // Executable Statements ..
  //
  ITMAX := 5;
  SAFMIN := MinRealNumber;
  if KASE = 0 then
    begin
      SetLength(v, n + 1);
      SetLength(X, n + 1);
      SetLength(ISAVE, 5);
      SetLength(RSAVE, 4);
      i := 1;
      while i <= n do
        begin
          X[i] := C_Complex(AP_Float(1) / n);
          Inc(i);
        end;
      KASE := 1;
      JUMP := 1;
      InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
        ALTSGN, ESTOLD, Temp);
      Exit;
    end;
  InternalComplexRCondLoadAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
    ALTSGN, ESTOLD, Temp);

  //
  // ENTRY   (JUMP = 1)
  // FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
  //
  if JUMP = 1 then
    begin
      if n = 1 then
        begin
          v[1] := X[1];
          EST := AbsComplex(v[1]);
          KASE := 0;
          InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
            ALTSGN, ESTOLD, Temp);
          Exit;
        end;
      EST := InternalComplexRCondSCSUM1(X, n);
      i := 1;
      while i <= n do
        begin
          ABSXI := AbsComplex(X[i]);
          if AP_FP_Greater(ABSXI, SAFMIN) then
            begin
              X[i] := C_DivR(X[i], ABSXI);
            end
          else
            begin
              X[i] := C_Complex(1);
            end;
          Inc(i);
        end;
      KASE := 2;
      JUMP := 2;
      InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
        ALTSGN, ESTOLD, Temp);
      Exit;
    end;

  //
  // ENTRY   (JUMP = 2)
  // FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY CTRANS(A)*X.
  //
  if JUMP = 2 then
    begin
      J := InternalComplexRCondICMAX1(X, n);
      ITER := 2;

      //
      // MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
      //
      i := 1;
      while i <= n do
        begin
          X[i] := C_Complex(0);
          Inc(i);
        end;
      X[J] := C_Complex(1);
      KASE := 1;
      JUMP := 3;
      InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
        ALTSGN, ESTOLD, Temp);
      Exit;
    end;

  //
  // ENTRY   (JUMP = 3)
  // X HAS BEEN OVERWRITTEN BY A*X.
  //
  if JUMP = 3 then
    begin
      for i_ := 1 to n do
        begin
          v[i_] := X[i_];
        end;
      ESTOLD := EST;
      EST := InternalComplexRCondSCSUM1(v, n);

      //
      // TEST FOR CYCLING.
      //
      if AP_FP_Less_Eq(EST, ESTOLD) then
        begin

          //
          // ITERATION COMPLETE.  FINAL STAGE.
          //
          ALTSGN := 1;
          i := 1;
          while i <= n do
            begin
              X[i] := C_Complex(ALTSGN * (1 + AP_Float((i - 1)) / (n - 1)));
              ALTSGN := -ALTSGN;
              Inc(i);
            end;
          KASE := 1;
          JUMP := 5;
          InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
            ALTSGN, ESTOLD, Temp);
          Exit;
        end;
      i := 1;
      while i <= n do
        begin
          ABSXI := AbsComplex(X[i]);
          if AP_FP_Greater(ABSXI, SAFMIN) then
            begin
              X[i] := C_DivR(X[i], ABSXI);
            end
          else
            begin
              X[i] := C_Complex(1);
            end;
          Inc(i);
        end;
      KASE := 2;
      JUMP := 4;
      InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
        ALTSGN, ESTOLD, Temp);
      Exit;
    end;

  //
  // ENTRY   (JUMP = 4)
  // X HAS BEEN OVERWRITTEN BY CTRANS(A)*X.
  //
  if JUMP = 4 then
    begin
      JLAST := J;
      J := InternalComplexRCondICMAX1(X, n);
      if AP_FP_NEq(AbsComplex(X[JLAST]), AbsComplex(X[J])) and (ITER < ITMAX) then
        begin
          ITER := ITER + 1;

          //
          // MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
          //
          i := 1;
          while i <= n do
            begin
              X[i] := C_Complex(0);
              Inc(i);
            end;
          X[J] := C_Complex(1);
          KASE := 1;
          JUMP := 3;
          InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
            ALTSGN, ESTOLD, Temp);
          Exit;
        end;

      //
      // ITERATION COMPLETE.  FINAL STAGE.
      //
      ALTSGN := 1;
      i := 1;
      while i <= n do
        begin
          X[i] := C_Complex(ALTSGN * (1 + AP_Float((i - 1)) / (n - 1)));
          ALTSGN := -ALTSGN;
          Inc(i);
        end;
      KASE := 1;
      JUMP := 5;
      InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
        ALTSGN, ESTOLD, Temp);
      Exit;
    end;

  //
  // ENTRY   (JUMP = 5)
  // X HAS BEEN OVERWRITTEN BY A*X.
  //
  if JUMP = 5 then
    begin
      Temp := 2 * (InternalComplexRCondSCSUM1(X, n) / (3 * n));
      if AP_FP_Greater(Temp, EST) then
        begin
          for i_ := 1 to n do
            begin
              v[i_] := X[i_];
            end;
          EST := Temp;
        end;
      KASE := 0;
      InternalComplexRCondSaveAll(ISAVE, RSAVE, i, ITER, J, JLAST, JUMP, ABSXI,
        ALTSGN, ESTOLD, Temp);
      Exit;
    end;
end;

function InternalComplexRCondSCSUM1(const X: TLComplexVec; n: TLInt)
  : TLFloat;
var
  i: TLInt;
begin
  Result := 0;
  i := 1;
  while i <= n do
    begin
      Result := Result + AbsComplex(X[i]);
      Inc(i);
    end;
end;

function InternalComplexRCondICMAX1(const X: TLComplexVec; n: TLInt)
  : TLInt;
var
  i: TLInt;
  M: TLFloat;
begin
  Result := 1;
  M := AbsComplex(X[1]);
  i := 2;
  while i <= n do
    begin
      if AP_FP_Greater(AbsComplex(X[i]), M) then
        begin
          Result := i;
          M := AbsComplex(X[i]);
        end;
      Inc(i);
    end;
end;

procedure InternalComplexRCondSaveAll(var ISAVE: TLIVec;
  var RSAVE: TLVec; var i: TLInt; var ITER: TLInt;
  var J: TLInt; var JLAST: TLInt; var JUMP: TLInt;
  var ABSXI: TLFloat; var ALTSGN: TLFloat; var ESTOLD: TLFloat;
  var Temp: TLFloat);
begin
  ISAVE[0] := i;
  ISAVE[1] := ITER;
  ISAVE[2] := J;
  ISAVE[3] := JLAST;
  ISAVE[4] := JUMP;
  RSAVE[0] := ABSXI;
  RSAVE[1] := ALTSGN;
  RSAVE[2] := ESTOLD;
  RSAVE[3] := Temp;
end;

procedure InternalComplexRCondLoadAll(var ISAVE: TLIVec;
  var RSAVE: TLVec; var i: TLInt; var ITER: TLInt;
  var J: TLInt; var JLAST: TLInt; var JUMP: TLInt;
  var ABSXI: TLFloat; var ALTSGN: TLFloat; var ESTOLD: TLFloat;
  var Temp: TLFloat);
begin
  i := ISAVE[0];
  ITER := ISAVE[1];
  J := ISAVE[2];
  JLAST := ISAVE[3];
  JUMP := ISAVE[4];
  ABSXI := RSAVE[0];
  ALTSGN := RSAVE[1];
  ESTOLD := RSAVE[2];
  Temp := RSAVE[3];
end; 
