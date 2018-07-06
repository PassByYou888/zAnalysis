{ ****************************************************************************** }
{ * Matrix inverse support, by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure RMatrixTRInverseRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLVec;
  var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixTRInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLComplexVec;
  var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixLUInverseRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; var Work: TLVec; var Info: TLInt;
  var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLUInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  n: TLInt; var Work: TLComplexVec; var Info: TLInt;
  var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SPDMatrixCholeskyInverseRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HPDMatrixCholeskyInverseRec(var A: TLComplexMatrix;
  Offs: TLInt; n: TLInt; IsUpper: Boolean;
  var tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Inversion of a matrix given by its LU decomposition.

  INPUT PARAMETERS:
  A       -   LU decomposition of the matrix (output of RMatrixLU subroutine).
  Pivots  -   table of permutations which were made during the LU decomposition
  (the output of RMatrixLU subroutine).
  N       -   size of matrix A.

  OUTPUT PARAMETERS:
  Info    -   return code:
  * -3    A is singular, or VERY close to singular.
  it is filled by zeros in such cases.
  * -1    N<=0 was passed, or incorrect Pivots was passed
  *  1    task is solved (but matrix A may be ill-conditioned,
  check R1/RInf parameters for condition numbers).
  Rep     -   solver report, see below for more info
  A       -   inverse of matrix A.
  Array whose indexes range within [0..N-1, 0..N-1].

  SOLVER REPORT

  Subroutine sets following fields of the Rep structure:
  * R1        reciprocal of condition number: 1/cond(A), 1-norm.
  * RInf      reciprocal of condition number: 1/cond(A), inf-norm.
  ************************************************************************ *)
procedure RMatrixLUInverse(var A: TLMatrix; const Pivots: TLIVec;
  n: TLInt; var Info: TLInt; var Rep: TMatInvReport);
var
  Work: TLVec;
  i: TLInt;
  J: TLInt;
  k: TLInt;
  v: TLFloat;
begin
  Info := 1;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if (Pivots[i] > n - 1) or (Pivots[i] < i) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(i);
    end;

  //
  // calculate condition numbers
  //
  Rep.r1 := RMatrixLURCond1(A, n);
  Rep.RInf := RMatrixLURCondInf(A, n);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              A[i, J] := 0;
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Call cache-oblivious code
  //
  SetLength(Work, n);
  RMatrixLUInverseRec(A, 0, n, Work, Info, Rep);

  //
  // apply permutations
  //
  i := 0;
  while i <= n - 1 do
    begin
      J := n - 2;
      while J >= 0 do
        begin
          k := Pivots[J];
          v := A[i, J];
          A[i, J] := A[i, k];
          A[i, k] := v;
          Dec(J);
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Inversion of a general matrix.

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse

  Result:
  True, if the matrix is not singular.
  False, if the matrix is singular.
  ************************************************************************ *)
procedure RMatrixInverse(var A: TLMatrix; n: TLInt;
  var Info: TLInt; var Rep: TMatInvReport);
var
  Pivots: TLIVec;
begin
  RMatrixLU(A, n, n, Pivots);
  RMatrixLUInverse(A, Pivots, n, Info, Rep);
end;

(* ************************************************************************
  Inversion of a matrix given by its LU decomposition.

  INPUT PARAMETERS:
  A       -   LU decomposition of the matrix (output of CMatrixLU subroutine).
  Pivots  -   table of permutations which were made during the LU decomposition
  (the output of CMatrixLU subroutine).
  N       -   size of matrix A.

  OUTPUT PARAMETERS:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure CMatrixLUInverse(var A: TLComplexMatrix;
  const Pivots: TLIVec; n: TLInt; var Info: TLInt;
  var Rep: TMatInvReport);
var
  Work: TLComplexVec;
  i: TLInt;
  J: TLInt;
  k: TLInt;
  v: TLComplex;
begin
  Info := 1;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if (Pivots[i] > n - 1) or (Pivots[i] < i) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(i);
    end;

  //
  // calculate condition numbers
  //
  Rep.r1 := CMatrixLURCond1(A, n);
  Rep.RInf := CMatrixLURCondInf(A, n);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              A[i, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Call cache-oblivious code
  //
  SetLength(Work, n);
  CMatrixLUInverseRec(A, 0, n, Work, Info, Rep);

  //
  // apply permutations
  //
  i := 0;
  while i <= n - 1 do
    begin
      J := n - 2;
      while J >= 0 do
        begin
          k := Pivots[J];
          v := A[i, J];
          A[i, J] := A[i, k];
          A[i, k] := v;
          Dec(J);
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Inversion of a general matrix.

  Input parameters:
  A   -   matrix, array[0..N-1,0..N-1].
  N   -   size of A.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure CMatrixInverse(var A: TLComplexMatrix; n: TLInt;
  var Info: TLInt; var Rep: TMatInvReport);
var
  Pivots: TLIVec;
begin
  CMatrixLU(A, n, n, Pivots);
  CMatrixLUInverse(A, Pivots, n, Info, Rep);
end;

(* ************************************************************************
  Inversion of a symmetric positive definite matrix which is given
  by Cholesky decomposition.

  Input parameters:
  A       -   Cholesky decomposition of the matrix to be inverted:
  A=U?U or A = L*L'.
  Output of  SPDMatrixCholesky subroutine.
  N       -   size of matrix A.
  IsUpper ?  storage format.
  If IsUpper = True, then matrix A is given as A = U'*U
  (matrix contains upper triangle).
  Similarly, if IsUpper = False, then A = L*L'.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure SPDMatrixCholeskyInverse(var A: TLMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
var
  i: TLInt;
  J: TLInt;
  k: TLInt;
  v: TLFloat;
  AJJ: TLFloat;
  AII: TLFloat;
  tmp: TLVec;
  Info2: TLInt;
  Rep2: TMatInvReport;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := SPDMatrixCholeskyRCond(A, n, IsUpper);
  Rep.RInf := Rep.r1;
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              J := i;
              while J <= n - 1 do
                begin
                  A[i, J] := 0;
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
                  A[i, J] := 0;
                  Inc(J);
                end;
              Inc(i);
            end;
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Inverse
  //
  SetLength(tmp, n);
  SPDMatrixCholeskyInverseRec(A, 0, n, IsUpper, tmp);
end;

(* ************************************************************************
  Inversion of a symmetric positive definite matrix.

  Given an upper or lower triangle of a symmetric positive definite matrix,
  the algorithm generates matrix A^-1 and saves the upper or lower triangle
  depending on the input.

  Input parameters:
  A       -   matrix to be inverted (upper or lower triangle). Array with elements [0..N-1,0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.
  If IsUpper = True, then the upper triangle of matrix A is
  given, otherwise the lower triangle is given.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure SPDMatrixInverse(var A: TLMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;
  if SPDMatrixCholesky(A, n, IsUpper) then
    begin
      SPDMatrixCholeskyInverse(A, n, IsUpper, Info, Rep);
    end
  else
    begin
      Info := -3;
    end;
end;

(* ************************************************************************
  Inversion of a Hermitian positive definite matrix which is given
  by Cholesky decomposition.
  ************************************************************************ *)
procedure HPDMatrixCholeskyInverse(var A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
var
  i: TLInt;
  J: TLInt;
  Info2: TLInt;
  Rep2: TMatInvReport;
  tmp: TLComplexVec;
  v: TLComplex;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := HPDMatrixCholeskyRCond(A, n, IsUpper);
  Rep.RInf := Rep.r1;
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              J := i;
              while J <= n - 1 do
                begin
                  A[i, J] := C_Complex(0);
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
                  A[i, J] := C_Complex(0);
                  Inc(J);
                end;
              Inc(i);
            end;
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Inverse
  //
  SetLength(tmp, n);
  HPDMatrixCholeskyInverseRec(A, 0, n, IsUpper, tmp);
end;

(* ************************************************************************
  Inversion of a Hermitian positive definite matrix.

  Given an upper or lower triangle of a Hermitian positive definite matrix,
  the algorithm generates matrix A^-1 and saves the upper or lower triangle
  depending on the input.

  Input parameters:
  A       -   matrix to be inverted (upper or lower triangle). Array with elements [0..N-1,0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.
  If IsUpper = True, then the upper triangle of matrix A is
  given, otherwise the lower triangle is given.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure HPDMatrixInverse(var A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;
  if HPDMatrixCholesky(A, n, IsUpper) then
    begin
      HPDMatrixCholeskyInverse(A, n, IsUpper, Info, Rep);
    end
  else
    begin
      Info := -3;
    end;
end;

(* ************************************************************************
  Triangular matrix inverse (real)

  The subroutine inverts the following types of matrices:
  * upper triangular
  * upper triangular with unit diagonal
  * lower triangular
  * lower triangular with unit diagonal

  In case of an upper (lower) triangular matrix,  the  inverse  matrix  will
  also be upper (lower) triangular, and after the end of the algorithm,  the
  inverse matrix replaces the source matrix. The elements  below (above) the
  main diagonal are not changed by the algorithm.

  If  the matrix  has a unit diagonal, the inverse matrix also  has  a  unit
  diagonal, and the diagonal elements are not passed to the algorithm.

  Input parameters:
  A       -   matrix, array[0..N-1, 0..N-1].
  N       -   size of A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Output parameters:
  Info    -   same as for RMatrixLUInverse
  Rep     -   same as for RMatrixLUInverse
  A       -   same as for RMatrixLUInverse.
  ************************************************************************ *)
procedure RMatrixTRInverse(var A: TLMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  J: TLInt;
  tmp: TLVec;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := RMatrixTRRCond1(A, n, IsUpper, IsUnit);
  Rep.RInf := RMatrixTRRCondInf(A, n, IsUpper, IsUnit);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              A[i, J] := 0;
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Invert
  //
  SetLength(tmp, n);
  RMatrixTRInverseRec(A, 0, n, IsUpper, IsUnit, tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inverse (TLComplex)

  The subroutine inverts the following types of matrices:
  * upper triangular
  * upper triangular with unit diagonal
  * lower triangular
  * lower triangular with unit diagonal

  In case of an upper (lower) triangular matrix,  the  inverse  matrix  will
  also be upper (lower) triangular, and after the end of the algorithm,  the
  inverse matrix replaces the source matrix. The elements  below (above) the
  main diagonal are not changed by the algorithm.

  If  the matrix  has a unit diagonal, the inverse matrix also  has  a  unit
  diagonal, and the diagonal elements are not passed to the algorithm.

  Input parameters:
  A       -   matrix, array[0..N-1, 0..N-1].
  N       -   size of A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Output parameters:
  Info    -   same as for RMatrixLUInverse
  Rep     -   same as for RMatrixLUInverse
  A       -   same as for RMatrixLUInverse.
  ************************************************************************ *)
procedure CMatrixTRInverse(var A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  J: TLInt;
  tmp: TLComplexVec;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := CMatrixTRRCond1(A, n, IsUpper, IsUnit);
  Rep.RInf := CMatrixTRRCondInf(A, n, IsUpper, IsUnit);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              A[i, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Invert
  //
  SetLength(tmp, n);
  CMatrixTRInverseRec(A, 0, n, IsUpper, IsUnit, tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inversion, recursive subroutine
  ************************************************************************ *)
procedure RMatrixTRInverseRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLVec;
  var Info: TLInt; var Rep: TMatInvReport);
var
  N1: TLInt;
  N2: TLInt;
  i: TLInt;
  J: TLInt;
  v: TLFloat;
  AJJ: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASBlockSize(A) then
    begin
      if IsUpper then
        begin

          //
          // Compute inverse of upper triangular matrix.
          //
          J := 0;
          while J <= n - 1 do
            begin
              if not IsUnit then
                begin
                  if AP_FP_Eq(A[Offs + J, Offs + J], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  A[Offs + J, Offs + J] := 1 / A[Offs + J, Offs + J];
                  AJJ := -A[Offs + J, Offs + J];
                end
              else
                begin
                  AJJ := -1;
                end;

              //
              // Compute elements 1:j-1 of j-th column.
              //
              if J > 0 then
                begin
                  i1_ := (Offs + 0) - (0);
                  for i_ := 0 to J - 1 do
                    begin
                      tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  i := 0;
                  while i <= J - 1 do
                    begin
                      if i < J - 1 then
                        begin
                          v := APVDotProduct(@A[Offs + i][0], Offs + i + 1, Offs + J - 1,
                            @tmp[0], i + 1, J - 1);
                        end
                      else
                        begin
                          v := 0;
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + i, Offs + J] := v + A[Offs + i, Offs + i] * tmp[i];
                        end
                      else
                        begin
                          A[Offs + i, Offs + J] := v + tmp[i];
                        end;
                      Inc(i);
                    end;
                  for i_ := Offs + 0 to Offs + J - 1 do
                    begin
                      A[i_, Offs + J] := AJJ * A[i_, Offs + J];
                    end;
                end;
              Inc(J);
            end;
        end
      else
        begin

          //
          // Compute inverse of lower triangular matrix.
          //
          J := n - 1;
          while J >= 0 do
            begin
              if not IsUnit then
                begin
                  if AP_FP_Eq(A[Offs + J, Offs + J], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  A[Offs + J, Offs + J] := 1 / A[Offs + J, Offs + J];
                  AJJ := -A[Offs + J, Offs + J];
                end
              else
                begin
                  AJJ := -1;
                end;
              if J < n - 1 then
                begin

                  //
                  // Compute elements j+1:n of j-th column.
                  //
                  i1_ := (Offs + J + 1) - (J + 1);
                  for i_ := J + 1 to n - 1 do
                    begin
                      tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  i := J + 1;
                  while i <= n - 1 do
                    begin
                      if i > J + 1 then
                        begin
                          v := APVDotProduct(@A[Offs + i][0], Offs + J + 1, Offs + i - 1,
                            @tmp[0], J + 1, i - 1);
                        end
                      else
                        begin
                          v := 0;
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + i, Offs + J] := v + A[Offs + i, Offs + i] * tmp[i];
                        end
                      else
                        begin
                          A[Offs + i, Offs + J] := v + tmp[i];
                        end;
                      Inc(i);
                    end;
                  for i_ := Offs + J + 1 to Offs + n - 1 do
                    begin
                      A[i_, Offs + J] := AJJ * A[i_, Offs + J];
                    end;
                end;
              Dec(J);
            end;
        end;
      Exit;
    end;

  //
  // Recursive case
  //
  ABLASSplitLength(A, n, N1, N2);
  if N2 > 0 then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= N1 - 1 do
            begin
              APVMul(@A[Offs + i][0], Offs + N1, Offs + n - 1, -1);
              Inc(i);
            end;
          RMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, IsUnit, 0, A, Offs,
            Offs + N1);
          RMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs, Offs + N1);
        end
      else
        begin
          i := 0;
          while i <= N2 - 1 do
            begin
              APVMul(@A[Offs + N1 + i][0], Offs, Offs + N1 - 1, -1);
              Inc(i);
            end;
          RMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
          RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
        end;
      RMatrixTRInverseRec(A, Offs + N1, N2, IsUpper, IsUnit, tmp, Info, Rep);
    end;
  RMatrixTRInverseRec(A, Offs, N1, IsUpper, IsUnit, tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inversion, recursive subroutine
  ************************************************************************ *)
procedure CMatrixTRInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLComplexVec;
  var Info: TLInt; var Rep: TMatInvReport);
var
  N1: TLInt;
  N2: TLInt;
  i: TLInt;
  J: TLInt;
  v: TLComplex;
  AJJ: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASComplexBlockSize(A) then
    begin
      if IsUpper then
        begin

          //
          // Compute inverse of upper triangular matrix.
          //
          J := 0;
          while J <= n - 1 do
            begin
              if not IsUnit then
                begin
                  if C_EqualR(A[Offs + J, Offs + J], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  A[Offs + J, Offs + J] := C_RDiv(1, A[Offs + J, Offs + J]);
                  AJJ := C_Opposite(A[Offs + J, Offs + J]);
                end
              else
                begin
                  AJJ := C_Complex(-1);
                end;

              //
              // Compute elements 1:j-1 of j-th column.
              //
              if J > 0 then
                begin
                  i1_ := (Offs + 0) - (0);
                  for i_ := 0 to J - 1 do
                    begin
                      tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  i := 0;
                  while i <= J - 1 do
                    begin
                      if i < J - 1 then
                        begin
                          i1_ := (i + 1) - (Offs + i + 1);
                          v := C_Complex(0.0);
                          for i_ := Offs + i + 1 to Offs + J - 1 do
                            begin
                              v := C_Add(v, C_Mul(A[Offs + i, i_], tmp[i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          v := C_Complex(0);
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + i, Offs + J] :=
                            C_Add(v, C_Mul(A[Offs + i, Offs + i], tmp[i]));
                        end
                      else
                        begin
                          A[Offs + i, Offs + J] := C_Add(v, tmp[i]);
                        end;
                      Inc(i);
                    end;
                  for i_ := Offs + 0 to Offs + J - 1 do
                    begin
                      A[i_, Offs + J] := C_Mul(AJJ, A[i_, Offs + J]);
                    end;
                end;
              Inc(J);
            end;
        end
      else
        begin

          //
          // Compute inverse of lower triangular matrix.
          //
          J := n - 1;
          while J >= 0 do
            begin
              if not IsUnit then
                begin
                  if C_EqualR(A[Offs + J, Offs + J], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  A[Offs + J, Offs + J] := C_RDiv(1, A[Offs + J, Offs + J]);
                  AJJ := C_Opposite(A[Offs + J, Offs + J]);
                end
              else
                begin
                  AJJ := C_Complex(-1);
                end;
              if J < n - 1 then
                begin

                  //
                  // Compute elements j+1:n of j-th column.
                  //
                  i1_ := (Offs + J + 1) - (J + 1);
                  for i_ := J + 1 to n - 1 do
                    begin
                      tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  i := J + 1;
                  while i <= n - 1 do
                    begin
                      if i > J + 1 then
                        begin
                          i1_ := (J + 1) - (Offs + J + 1);
                          v := C_Complex(0.0);
                          for i_ := Offs + J + 1 to Offs + i - 1 do
                            begin
                              v := C_Add(v, C_Mul(A[Offs + i, i_], tmp[i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          v := C_Complex(0);
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + i, Offs + J] :=
                            C_Add(v, C_Mul(A[Offs + i, Offs + i], tmp[i]));
                        end
                      else
                        begin
                          A[Offs + i, Offs + J] := C_Add(v, tmp[i]);
                        end;
                      Inc(i);
                    end;
                  for i_ := Offs + J + 1 to Offs + n - 1 do
                    begin
                      A[i_, Offs + J] := C_Mul(AJJ, A[i_, Offs + J]);
                    end;
                end;
              Dec(J);
            end;
        end;
      Exit;
    end;

  //
  // Recursive case
  //
  ABLASComplexSplitLength(A, n, N1, N2);
  if N2 > 0 then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= N1 - 1 do
            begin
              for i_ := Offs + N1 to Offs + n - 1 do
                begin
                  A[Offs + i, i_] := C_MulR(A[Offs + i, i_], -1);
                end;
              Inc(i);
            end;
          CMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, IsUnit, 0, A, Offs,
            Offs + N1);
          CMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs, Offs + N1);
        end
      else
        begin
          i := 0;
          while i <= N2 - 1 do
            begin
              for i_ := Offs to Offs + N1 - 1 do
                begin
                  A[Offs + N1 + i, i_] := C_MulR(A[Offs + N1 + i, i_], -1);
                end;
              Inc(i);
            end;
          CMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
          CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
        end;
      CMatrixTRInverseRec(A, Offs + N1, N2, IsUpper, IsUnit, tmp, Info, Rep);
    end;
  CMatrixTRInverseRec(A, Offs, N1, IsUpper, IsUnit, tmp, Info, Rep);
end;

procedure RMatrixLUInverseRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; var Work: TLVec; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  IWS: TLInt;
  J: TLInt;
  JB: TLInt;
  JJ: TLInt;
  JP: TLInt;
  k: TLInt;
  v: TLFloat;
  N1: TLInt;
  N2: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASBlockSize(A) then
    begin

      //
      // Form inv(U)
      //
      RMatrixTRInverseRec(A, Offs, n, True, False, Work, Info, Rep);
      if Info <= 0 then
        begin
          Exit;
        end;

      //
      // Solve the equation inv(A)*L = inv(U) for inv(A).
      //
      J := n - 1;
      while J >= 0 do
        begin

          //
          // Copy current column of L to WORK and replace with zeros.
          //
          i := J + 1;
          while i <= n - 1 do
            begin
              Work[i] := A[Offs + i, Offs + J];
              A[Offs + i, Offs + J] := 0;
              Inc(i);
            end;

          //
          // Compute current column of inv(A).
          //
          if J < n - 1 then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  v := APVDotProduct(@A[Offs + i][0], Offs + J + 1, Offs + n - 1,
                    @Work[0], J + 1, n - 1);
                  A[Offs + i, Offs + J] := A[Offs + i, Offs + J] - v;
                  Inc(i);
                end;
            end;
          Dec(J);
        end;
      Exit;
    end;

  //
  // Recursive code:
  //
  // ( L1      )   ( U1  U12 )
  // A    =  (         ) * (         )
  // ( L12  L2 )   (     U2  )
  //
  // ( W   X )
  // A^-1 =  (       )
  // ( Y   Z )
  //
  ABLASSplitLength(A, n, N1, N2);
  Assert(N2 > 0, 'LUInverseRec: internal error!');

  //
  // X := inv(U1)*U12*inv(U2)
  //
  RMatrixLeftTRSM(N1, N2, A, Offs, Offs, True, False, 0, A, Offs, Offs + N1);
  RMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, True, False, 0, A, Offs,
    Offs + N1);

  //
  // Y := inv(L2)*L12*inv(L1)
  //
  RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, False, True, 0, A,
    Offs + N1, Offs);
  RMatrixRightTRSM(N2, N1, A, Offs, Offs, False, True, 0, A, Offs + N1, Offs);

  //
  // W := inv(L1*U1)+X*Y
  //
  RMatrixLUInverseRec(A, Offs, N1, Work, Info, Rep);
  if Info <= 0 then
    begin
      Exit;
    end;
  RMatrixGEMM(N1, N1, N2, 1.0, A, Offs, Offs + N1, 0, A, Offs + N1, Offs, 0,
    1.0, A, Offs, Offs);

  //
  // X := -X*inv(L2)
  // Y := -inv(U2)*Y
  //
  RMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, False, True, 0, A, Offs,
    Offs + N1);
  i := 0;
  while i <= N1 - 1 do
    begin
      APVMul(@A[Offs + i][0], Offs + N1, Offs + n - 1, -1);
      Inc(i);
    end;
  RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, True, False, 0, A,
    Offs + N1, Offs);
  i := 0;
  while i <= N2 - 1 do
    begin
      APVMul(@A[Offs + N1 + i][0], Offs, Offs + N1 - 1, -1);
      Inc(i);
    end;

  //
  // Z := inv(L2*U2)
  //
  RMatrixLUInverseRec(A, Offs + N1, N2, Work, Info, Rep);
end;

procedure CMatrixLUInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  n: TLInt; var Work: TLComplexVec; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  IWS: TLInt;
  J: TLInt;
  JB: TLInt;
  JJ: TLInt;
  JP: TLInt;
  k: TLInt;
  v: TLComplex;
  N1: TLInt;
  N2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASComplexBlockSize(A) then
    begin

      //
      // Form inv(U)
      //
      CMatrixTRInverseRec(A, Offs, n, True, False, Work, Info, Rep);
      if Info <= 0 then
        begin
          Exit;
        end;

      //
      // Solve the equation inv(A)*L = inv(U) for inv(A).
      //
      J := n - 1;
      while J >= 0 do
        begin

          //
          // Copy current column of L to WORK and replace with zeros.
          //
          i := J + 1;
          while i <= n - 1 do
            begin
              Work[i] := A[Offs + i, Offs + J];
              A[Offs + i, Offs + J] := C_Complex(0);
              Inc(i);
            end;

          //
          // Compute current column of inv(A).
          //
          if J < n - 1 then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  i1_ := (J + 1) - (Offs + J + 1);
                  v := C_Complex(0.0);
                  for i_ := Offs + J + 1 to Offs + n - 1 do
                    begin
                      v := C_Add(v, C_Mul(A[Offs + i, i_], Work[i_ + i1_]));
                    end;
                  A[Offs + i, Offs + J] := C_Sub(A[Offs + i, Offs + J], v);
                  Inc(i);
                end;
            end;
          Dec(J);
        end;
      Exit;
    end;

  //
  // Recursive code:
  //
  // ( L1      )   ( U1  U12 )
  // A    =  (         ) * (         )
  // ( L12  L2 )   (     U2  )
  //
  // ( W   X )
  // A^-1 =  (       )
  // ( Y   Z )
  //
  ABLASComplexSplitLength(A, n, N1, N2);
  Assert(N2 > 0, 'LUInverseRec: internal error!');

  //
  // X := inv(U1)*U12*inv(U2)
  //
  CMatrixLeftTRSM(N1, N2, A, Offs, Offs, True, False, 0, A, Offs, Offs + N1);
  CMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, True, False, 0, A, Offs,
    Offs + N1);

  //
  // Y := inv(L2)*L12*inv(L1)
  //
  CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, False, True, 0, A,
    Offs + N1, Offs);
  CMatrixRightTRSM(N2, N1, A, Offs, Offs, False, True, 0, A, Offs + N1, Offs);

  //
  // W := inv(L1*U1)+X*Y
  //
  CMatrixLUInverseRec(A, Offs, N1, Work, Info, Rep);
  if Info <= 0 then
    begin
      Exit;
    end;
  CMatrixGEMM(N1, N1, N2, C_Complex(1.0), A, Offs, Offs + N1, 0, A, Offs + N1,
    Offs, 0, C_Complex(1.0), A, Offs, Offs);

  //
  // X := -X*inv(L2)
  // Y := -inv(U2)*Y
  //
  CMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, False, True, 0, A, Offs,
    Offs + N1);
  i := 0;
  while i <= N1 - 1 do
    begin
      for i_ := Offs + N1 to Offs + n - 1 do
        begin
          A[Offs + i, i_] := C_MulR(A[Offs + i, i_], -1);
        end;
      Inc(i);
    end;
  CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, True, False, 0, A,
    Offs + N1, Offs);
  i := 0;
  while i <= N2 - 1 do
    begin
      for i_ := Offs to Offs + N1 - 1 do
        begin
          A[Offs + N1 + i, i_] := C_MulR(A[Offs + N1 + i, i_], -1);
        end;
      Inc(i);
    end;

  //
  // Z := inv(L2*U2)
  //
  CMatrixLUInverseRec(A, Offs + N1, N2, Work, Info, Rep);
end;

(* ************************************************************************
  Recursive subroutine for SPD inversion.
  ************************************************************************ *)
procedure SPDMatrixCholeskyInverseRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec);
var
  i: TLInt;
  J: TLInt;
  v: TLFloat;
  N1: TLInt;
  N2: TLInt;
  Info2: TLInt;
  Rep2: TMatInvReport;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASBlockSize(A) then
    begin
      RMatrixTRInverseRec(A, Offs, n, IsUpper, False, tmp, Info2, Rep2);
      if IsUpper then
        begin

          //
          // Compute the product U * U'.
          // NOTE: we never assume that diagonal of U is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + i, Offs + i] := AP_Sqr(A[Offs + i, Offs + i]);
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11  A12 )   ( A11^H        )   ( A11*A11^H+A12*A12^H  A12*A22^H )
                  // (          ) * (              ) = (                                )
                  // (      A22 )   ( A12^H  A22^H )   ( A22*A12^H            A22*A22^H )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  i1_ := (Offs) - (0);
                  for i_ := 0 to i - 1 do
                    begin
                      tmp[i_] := A[i_ + i1_, Offs + i];
                    end;
                  J := 0;
                  while J <= i - 1 do
                    begin
                      v := A[Offs + J, Offs + i];
                      APVAdd(@A[Offs + J][0], Offs + J, Offs + i - 1, @tmp[0], J,
                        i - 1, v);
                      Inc(J);
                    end;
                  v := A[Offs + i, Offs + i];
                  for i_ := Offs to Offs + i - 1 do
                    begin
                      A[i_, Offs + i] := v * A[i_, Offs + i];
                    end;
                  A[Offs + i, Offs + i] := AP_Sqr(A[Offs + i, Offs + i]);
                end;
              Inc(i);
            end;
        end
      else
        begin

          //
          // Compute the product L' * L
          // NOTE: we never assume that diagonal of L is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + i, Offs + i] := AP_Sqr(A[Offs + i, Offs + i]);
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11^H  A21^H )   ( A11      )   ( A11^H*A11+A21^H*A21  A21^H*A22 )
                  // (              ) * (          ) = (                                )
                  // (        A22^H )   ( A21  A22 )   ( A22^H*A21            A22^H*A22 )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  APVMove(@tmp[0], 0, i - 1, @A[Offs + i][0], Offs, Offs + i - 1);
                  J := 0;
                  while J <= i - 1 do
                    begin
                      v := A[Offs + i, Offs + J];
                      APVAdd(@A[Offs + J][0], Offs, Offs + J, @tmp[0], 0, J, v);
                      Inc(J);
                    end;
                  v := A[Offs + i, Offs + i];
                  APVMul(@A[Offs + i][0], Offs, Offs + i - 1, v);
                  A[Offs + i, Offs + i] := AP_Sqr(A[Offs + i, Offs + i]);
                end;
              Inc(i);
            end;
        end;
      Exit;
    end;

  //
  // Recursive code: triangular factor inversion merged with
  // UU' or L'L multiplication
  //
  ABLASSplitLength(A, n, N1, N2);

  //
  // form off-diagonal block of trangular inverse
  //
  if IsUpper then
    begin
      i := 0;
      while i <= N1 - 1 do
        begin
          APVMul(@A[Offs + i][0], Offs + N1, Offs + n - 1, -1);
          Inc(i);
        end;
      RMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, False, 0, A, Offs,
        Offs + N1);
      RMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs, Offs + N1);
    end
  else
    begin
      i := 0;
      while i <= N2 - 1 do
        begin
          APVMul(@A[Offs + N1 + i][0], Offs, Offs + N1 - 1, -1);
          Inc(i);
        end;
      RMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, False, 0, A,
        Offs + N1, Offs);
      RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs + N1, Offs);
    end;

  //
  // invert first diagonal block
  //
  SPDMatrixCholeskyInverseRec(A, Offs, N1, IsUpper, tmp);

  //
  // update first diagonal block with off-diagonal block,
  // update off-diagonal block
  //
  if IsUpper then
    begin
      RMatrixSYRK(N1, N2, 1.0, A, Offs, Offs + N1, 0, 1.0, A, Offs, Offs,
        IsUpper);
      RMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, False, 1, A,
        Offs, Offs + N1);
    end
  else
    begin
      RMatrixSYRK(N1, N2, 1.0, A, Offs + N1, Offs, 1, 1.0, A, Offs, Offs,
        IsUpper);
      RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, False, 1, A,
        Offs + N1, Offs);
    end;

  //
  // invert second diagonal block
  //
  SPDMatrixCholeskyInverseRec(A, Offs + N1, N2, IsUpper, tmp);
end;

(* ************************************************************************
  Recursive subroutine for HPD inversion.
  ************************************************************************ *)
procedure HPDMatrixCholeskyInverseRec(var A: TLComplexMatrix;
  Offs: TLInt; n: TLInt; IsUpper: Boolean;
  var tmp: TLComplexVec);
var
  i: TLInt;
  J: TLInt;
  v: TLComplex;
  N1: TLInt;
  N2: TLInt;
  Info2: TLInt;
  Rep2: TMatInvReport;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASComplexBlockSize(A) then
    begin
      CMatrixTRInverseRec(A, Offs, n, IsUpper, False, tmp, Info2, Rep2);
      if IsUpper then
        begin

          //
          // Compute the product U * U'.
          // NOTE: we never assume that diagonal of U is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + i, Offs + i] := C_Complex(AP_Sqr(A[Offs + i, Offs + i].X) +
                    AP_Sqr(A[Offs + i, Offs + i].Y));
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11  A12 )   ( A11^H        )   ( A11*A11^H+A12*A12^H  A12*A22^H )
                  // (          ) * (              ) = (                                )
                  // (      A22 )   ( A12^H  A22^H )   ( A22*A12^H            A22*A22^H )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  i1_ := (Offs) - (0);
                  for i_ := 0 to i - 1 do
                    begin
                      tmp[i_] := Conj(A[i_ + i1_, Offs + i]);
                    end;
                  J := 0;
                  while J <= i - 1 do
                    begin
                      v := A[Offs + J, Offs + i];
                      i1_ := (J) - (Offs + J);
                      for i_ := Offs + J to Offs + i - 1 do
                        begin
                          A[Offs + J, i_] := C_Add(A[Offs + J, i_],
                            C_Mul(v, tmp[i_ + i1_]));
                        end;
                      Inc(J);
                    end;
                  v := Conj(A[Offs + i, Offs + i]);
                  for i_ := Offs to Offs + i - 1 do
                    begin
                      A[i_, Offs + i] := C_Mul(v, A[i_, Offs + i]);
                    end;
                  A[Offs + i, Offs + i] := C_Complex(AP_Sqr(A[Offs + i, Offs + i].X) +
                    AP_Sqr(A[Offs + i, Offs + i].Y));
                end;
              Inc(i);
            end;
        end
      else
        begin

          //
          // Compute the product L' * L
          // NOTE: we never assume that diagonal of L is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + i, Offs + i] := C_Complex(AP_Sqr(A[Offs + i, Offs + i].X) +
                    AP_Sqr(A[Offs + i, Offs + i].Y));
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11^H  A21^H )   ( A11      )   ( A11^H*A11+A21^H*A21  A21^H*A22 )
                  // (              ) * (          ) = (                                )
                  // (        A22^H )   ( A21  A22 )   ( A22^H*A21            A22^H*A22 )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  i1_ := (Offs) - (0);
                  for i_ := 0 to i - 1 do
                    begin
                      tmp[i_] := A[Offs + i, i_ + i1_];
                    end;
                  J := 0;
                  while J <= i - 1 do
                    begin
                      v := Conj(A[Offs + i, Offs + J]);
                      i1_ := (0) - (Offs);
                      for i_ := Offs to Offs + J do
                        begin
                          A[Offs + J, i_] := C_Add(A[Offs + J, i_],
                            C_Mul(v, tmp[i_ + i1_]));
                        end;
                      Inc(J);
                    end;
                  v := Conj(A[Offs + i, Offs + i]);
                  for i_ := Offs to Offs + i - 1 do
                    begin
                      A[Offs + i, i_] := C_Mul(v, A[Offs + i, i_]);
                    end;
                  A[Offs + i, Offs + i] := C_Complex(AP_Sqr(A[Offs + i, Offs + i].X) +
                    AP_Sqr(A[Offs + i, Offs + i].Y));
                end;
              Inc(i);
            end;
        end;
      Exit;
    end;

  //
  // Recursive code: triangular factor inversion merged with
  // UU' or L'L multiplication
  //
  ABLASComplexSplitLength(A, n, N1, N2);

  //
  // form off-diagonal block of trangular inverse
  //
  if IsUpper then
    begin
      i := 0;
      while i <= N1 - 1 do
        begin
          for i_ := Offs + N1 to Offs + n - 1 do
            begin
              A[Offs + i, i_] := C_MulR(A[Offs + i, i_], -1);
            end;
          Inc(i);
        end;
      CMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, False, 0, A, Offs,
        Offs + N1);
      CMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs, Offs + N1);
    end
  else
    begin
      i := 0;
      while i <= N2 - 1 do
        begin
          for i_ := Offs to Offs + N1 - 1 do
            begin
              A[Offs + N1 + i, i_] := C_MulR(A[Offs + N1 + i, i_], -1);
            end;
          Inc(i);
        end;
      CMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, False, 0, A,
        Offs + N1, Offs);
      CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs + N1, Offs);
    end;

  //
  // invert first diagonal block
  //
  HPDMatrixCholeskyInverseRec(A, Offs, N1, IsUpper, tmp);

  //
  // update first diagonal block with off-diagonal block,
  // update off-diagonal block
  //
  if IsUpper then
    begin
      CMatrixSYRK(N1, N2, 1.0, A, Offs, Offs + N1, 0, 1.0, A, Offs, Offs,
        IsUpper);
      CMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, False, 2, A,
        Offs, Offs + N1);
    end
  else
    begin
      CMatrixSYRK(N1, N2, 1.0, A, Offs + N1, Offs, 2, 1.0, A, Offs, Offs,
        IsUpper);
      CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, False, 2, A,
        Offs + N1, Offs);
    end;

  //
  // invert second diagonal block
  //
  HPDMatrixCholeskyInverseRec(A, Offs + N1, N2, IsUpper, tmp);
end; 
