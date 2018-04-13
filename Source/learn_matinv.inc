{ ****************************************************************************** }
{ * Matrix inverse support, by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
type
  TMatInvReport = packed record
    R1: TLFloat;
    RInf: TLFloat;
  end;

procedure RMatrixLUInverse(var A: TLMatrix; const Pivots: TLIVec;
  N: TLInt; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixInverse(var A: TLMatrix; N: TLInt;
  var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixLUInverse(var A: TLComplexMatrix;
  const Pivots: TLIVec; N: TLInt; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixInverse(var A: TLComplexMatrix; N: TLInt;
  var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixCholeskyInverse(var A: TLMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixInverse(var A: TLMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixCholeskyInverse(var A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixInverse(var A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixTRInverse(var A: TLMatrix; N: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixTRInverse(var A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixTRInverseRec(var A: TLMatrix; Offs: TLInt;
  N: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Tmp: TLVec;
  var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixTRInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  N: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Tmp: TLComplexVec;
  var Info: TLInt; var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixLUInverseRec(var A: TLMatrix; Offs: TLInt;
  N: TLInt; var WORK: TLVec; var Info: TLInt;
  var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLUInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  N: TLInt; var WORK: TLComplexVec; var Info: TLInt;
  var Rep: TMatInvReport); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SPDMatrixCholeskyInverseRec(var A: TLMatrix; Offs: TLInt;
  N: TLInt; IsUpper: Boolean; var Tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HPDMatrixCholeskyInverseRec(var A: TLComplexMatrix;
  Offs: TLInt; N: TLInt; IsUpper: Boolean;
  var Tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


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
  N: TLInt; var Info: TLInt; var Rep: TMatInvReport);
var
  WORK: TLVec;
  I   : TLInt;
  J   : TLInt;
  K   : TLInt;
  V   : TLFloat;
begin
  Info := 1;

  //
  // Quick return if possible
  //
  if N = 0 then
    begin
      Info := -1;
      Exit;
    end;
  I := 0;
  while I <= N - 1 do
    begin
      if (Pivots[I] > N - 1) or (Pivots[I] < I) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(I);
    end;

  //
  // calculate condition numbers
  //
  Rep.R1 := RMatrixLURCond1(A, N);
  Rep.RInf := RMatrixLURCondInf(A, N);
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              A[I, J] := 0;
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Call cache-oblivious code
  //
  SetLength(WORK, N);
  RMatrixLUInverseRec(A, 0, N, WORK, Info, Rep);

  //
  // apply permutations
  //
  I := 0;
  while I <= N - 1 do
    begin
      J := N - 2;
      while J >= 0 do
        begin
          K := Pivots[J];
          V := A[I, J];
          A[I, J] := A[I, K];
          A[I, K] := V;
          Dec(J);
        end;
      Inc(I);
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
procedure RMatrixInverse(var A: TLMatrix; N: TLInt;
  var Info: TLInt; var Rep: TMatInvReport);
var
  Pivots: TLIVec;
begin
  RMatrixLU(A, N, N, Pivots);
  RMatrixLUInverse(A, Pivots, N, Info, Rep);
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
  const Pivots: TLIVec; N: TLInt; var Info: TLInt;
  var Rep: TMatInvReport);
var
  WORK: TLComplexVec;
  I   : TLInt;
  J   : TLInt;
  K   : TLInt;
  V   : TLComplex;
begin
  Info := 1;

  //
  // Quick return if possible
  //
  if N = 0 then
    begin
      Info := -1;
      Exit;
    end;
  I := 0;
  while I <= N - 1 do
    begin
      if (Pivots[I] > N - 1) or (Pivots[I] < I) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(I);
    end;

  //
  // calculate condition numbers
  //
  Rep.R1 := CMatrixLURCond1(A, N);
  Rep.RInf := CMatrixLURCondInf(A, N);
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              A[I, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Call cache-oblivious code
  //
  SetLength(WORK, N);
  CMatrixLUInverseRec(A, 0, N, WORK, Info, Rep);

  //
  // apply permutations
  //
  I := 0;
  while I <= N - 1 do
    begin
      J := N - 2;
      while J >= 0 do
        begin
          K := Pivots[J];
          V := A[I, J];
          A[I, J] := A[I, K];
          A[I, K] := V;
          Dec(J);
        end;
      Inc(I);
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
procedure CMatrixInverse(var A: TLComplexMatrix; N: TLInt;
  var Info: TLInt; var Rep: TMatInvReport);
var
  Pivots: TLIVec;
begin
  CMatrixLU(A, N, N, Pivots);
  CMatrixLUInverse(A, Pivots, N, Info, Rep);
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
procedure SPDMatrixCholeskyInverse(var A: TLMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
var
  I    : TLInt;
  J    : TLInt;
  K    : TLInt;
  V    : TLFloat;
  AJJ  : TLFloat;
  AII  : TLFloat;
  Tmp  : TLVec;
  Info2: TLInt;
  Rep2 : TMatInvReport;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.R1 := SPDMatrixCholeskyRCond(A, N, IsUpper);
  Rep.RInf := Rep.R1;
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      if IsUpper then
        begin
          I := 0;
          while I <= N - 1 do
            begin
              J := I;
              while J <= N - 1 do
                begin
                  A[I, J] := 0;
                  Inc(J);
                end;
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= N - 1 do
            begin
              J := 0;
              while J <= I do
                begin
                  A[I, J] := 0;
                  Inc(J);
                end;
              Inc(I);
            end;
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Inverse
  //
  SetLength(Tmp, N);
  SPDMatrixCholeskyInverseRec(A, 0, N, IsUpper, Tmp);
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
procedure SPDMatrixInverse(var A: TLMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;
  if SPDMatrixCholesky(A, N, IsUpper) then
    begin
      SPDMatrixCholeskyInverse(A, N, IsUpper, Info, Rep);
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
procedure HPDMatrixCholeskyInverse(var A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
var
  I    : TLInt;
  J    : TLInt;
  Info2: TLInt;
  Rep2 : TMatInvReport;
  Tmp  : TLComplexVec;
  V    : TLComplex;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.R1 := HPDMatrixCholeskyRCond(A, N, IsUpper);
  Rep.RInf := Rep.R1;
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      if IsUpper then
        begin
          I := 0;
          while I <= N - 1 do
            begin
              J := I;
              while J <= N - 1 do
                begin
                  A[I, J] := C_Complex(0);
                  Inc(J);
                end;
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= N - 1 do
            begin
              J := 0;
              while J <= I do
                begin
                  A[I, J] := C_Complex(0);
                  Inc(J);
                end;
              Inc(I);
            end;
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Inverse
  //
  SetLength(Tmp, N);
  HPDMatrixCholeskyInverseRec(A, 0, N, IsUpper, Tmp);
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
procedure HPDMatrixInverse(var A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;
  if HPDMatrixCholesky(A, N, IsUpper) then
    begin
      HPDMatrixCholeskyInverse(A, N, IsUpper, Info, Rep);
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
procedure RMatrixTRInverse(var A: TLMatrix; N: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt;
  var Rep: TMatInvReport);
var
  I  : TLInt;
  J  : TLInt;
  Tmp: TLVec;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.R1 := RMatrixTRRCond1(A, N, IsUpper, IsUnit);
  Rep.RInf := RMatrixTRRCondInf(A, N, IsUpper, IsUnit);
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              A[I, J] := 0;
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Invert
  //
  SetLength(Tmp, N);
  RMatrixTRInverseRec(A, 0, N, IsUpper, IsUnit, Tmp, Info, Rep);
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
procedure CMatrixTRInverse(var A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt;
  var Rep: TMatInvReport);
var
  I  : TLInt;
  J  : TLInt;
  Tmp: TLComplexVec;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.R1 := CMatrixTRRCond1(A, N, IsUpper, IsUnit);
  Rep.RInf := CMatrixTRRCondInf(A, N, IsUpper, IsUnit);
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              A[I, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Invert
  //
  SetLength(Tmp, N);
  CMatrixTRInverseRec(A, 0, N, IsUpper, IsUnit, Tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inversion, recursive subroutine
  ************************************************************************ *)
procedure RMatrixTRInverseRec(var A: TLMatrix; Offs: TLInt;
  N: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Tmp: TLVec;
  var Info: TLInt; var Rep: TMatInvReport);
var
  N1 : TLInt;
  N2 : TLInt;
  I  : TLInt;
  J  : TLInt;
  V  : TLFloat;
  AJJ: TLFloat;
  i_ : TLInt;
  i1_: TLInt;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if N <= ABLASBlockSize(A) then
    begin
      if IsUpper then
        begin

          //
          // Compute inverse of upper triangular matrix.
          //
          J := 0;
          while J <= N - 1 do
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
                      Tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  I := 0;
                  while I <= J - 1 do
                    begin
                      if I < J - 1 then
                        begin
                          V := APVDotProduct(@A[Offs + I][0], Offs + I + 1, Offs + J - 1,
                            @Tmp[0], I + 1, J - 1);
                        end
                      else
                        begin
                          V := 0;
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + I, Offs + J] := V + A[Offs + I, Offs + I] * Tmp[I];
                        end
                      else
                        begin
                          A[Offs + I, Offs + J] := V + Tmp[I];
                        end;
                      Inc(I);
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
          J := N - 1;
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
              if J < N - 1 then
                begin

                  //
                  // Compute elements j+1:n of j-th column.
                  //
                  i1_ := (Offs + J + 1) - (J + 1);
                  for i_ := J + 1 to N - 1 do
                    begin
                      Tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  I := J + 1;
                  while I <= N - 1 do
                    begin
                      if I > J + 1 then
                        begin
                          V := APVDotProduct(@A[Offs + I][0], Offs + J + 1, Offs + I - 1,
                            @Tmp[0], J + 1, I - 1);
                        end
                      else
                        begin
                          V := 0;
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + I, Offs + J] := V + A[Offs + I, Offs + I] * Tmp[I];
                        end
                      else
                        begin
                          A[Offs + I, Offs + J] := V + Tmp[I];
                        end;
                      Inc(I);
                    end;
                  for i_ := Offs + J + 1 to Offs + N - 1 do
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
  ABLASSplitLength(A, N, N1, N2);
  if N2 > 0 then
    begin
      if IsUpper then
        begin
          I := 0;
          while I <= N1 - 1 do
            begin
              APVMul(@A[Offs + I][0], Offs + N1, Offs + N - 1, -1);
              Inc(I);
            end;
          RMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, IsUnit, 0, A, Offs,
            Offs + N1);
          RMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs, Offs + N1);
        end
      else
        begin
          I := 0;
          while I <= N2 - 1 do
            begin
              APVMul(@A[Offs + N1 + I][0], Offs, Offs + N1 - 1, -1);
              Inc(I);
            end;
          RMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
          RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
        end;
      RMatrixTRInverseRec(A, Offs + N1, N2, IsUpper, IsUnit, Tmp, Info, Rep);
    end;
  RMatrixTRInverseRec(A, Offs, N1, IsUpper, IsUnit, Tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inversion, recursive subroutine
  ************************************************************************ *)
procedure CMatrixTRInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  N: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Tmp: TLComplexVec;
  var Info: TLInt; var Rep: TMatInvReport);
var
  N1 : TLInt;
  N2 : TLInt;
  I  : TLInt;
  J  : TLInt;
  V  : TLComplex;
  AJJ: TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if N <= ABLASComplexBlockSize(A) then
    begin
      if IsUpper then
        begin

          //
          // Compute inverse of upper triangular matrix.
          //
          J := 0;
          while J <= N - 1 do
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
                      Tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  I := 0;
                  while I <= J - 1 do
                    begin
                      if I < J - 1 then
                        begin
                          i1_ := (I + 1) - (Offs + I + 1);
                          V := C_Complex(0.0);
                          for i_ := Offs + I + 1 to Offs + J - 1 do
                            begin
                              V := C_Add(V, C_Mul(A[Offs + I, i_], Tmp[i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          V := C_Complex(0);
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + I, Offs + J] :=
                            C_Add(V, C_Mul(A[Offs + I, Offs + I], Tmp[I]));
                        end
                      else
                        begin
                          A[Offs + I, Offs + J] := C_Add(V, Tmp[I]);
                        end;
                      Inc(I);
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
          J := N - 1;
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
              if J < N - 1 then
                begin

                  //
                  // Compute elements j+1:n of j-th column.
                  //
                  i1_ := (Offs + J + 1) - (J + 1);
                  for i_ := J + 1 to N - 1 do
                    begin
                      Tmp[i_] := A[i_ + i1_, Offs + J];
                    end;
                  I := J + 1;
                  while I <= N - 1 do
                    begin
                      if I > J + 1 then
                        begin
                          i1_ := (J + 1) - (Offs + J + 1);
                          V := C_Complex(0.0);
                          for i_ := Offs + J + 1 to Offs + I - 1 do
                            begin
                              V := C_Add(V, C_Mul(A[Offs + I, i_], Tmp[i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          V := C_Complex(0);
                        end;
                      if not IsUnit then
                        begin
                          A[Offs + I, Offs + J] :=
                            C_Add(V, C_Mul(A[Offs + I, Offs + I], Tmp[I]));
                        end
                      else
                        begin
                          A[Offs + I, Offs + J] := C_Add(V, Tmp[I]);
                        end;
                      Inc(I);
                    end;
                  for i_ := Offs + J + 1 to Offs + N - 1 do
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
  ABLASComplexSplitLength(A, N, N1, N2);
  if N2 > 0 then
    begin
      if IsUpper then
        begin
          I := 0;
          while I <= N1 - 1 do
            begin
              for i_ := Offs + N1 to Offs + N - 1 do
                begin
                  A[Offs + I, i_] := C_MulR(A[Offs + I, i_], -1);
                end;
              Inc(I);
            end;
          CMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, IsUnit, 0, A, Offs,
            Offs + N1);
          CMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs, Offs + N1);
        end
      else
        begin
          I := 0;
          while I <= N2 - 1 do
            begin
              for i_ := Offs to Offs + N1 - 1 do
                begin
                  A[Offs + N1 + I, i_] := C_MulR(A[Offs + N1 + I, i_], -1);
                end;
              Inc(I);
            end;
          CMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
          CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, IsUnit, 0, A,
            Offs + N1, Offs);
        end;
      CMatrixTRInverseRec(A, Offs + N1, N2, IsUpper, IsUnit, Tmp, Info, Rep);
    end;
  CMatrixTRInverseRec(A, Offs, N1, IsUpper, IsUnit, Tmp, Info, Rep);
end;

procedure RMatrixLUInverseRec(var A: TLMatrix; Offs: TLInt;
  N: TLInt; var WORK: TLVec; var Info: TLInt;
  var Rep: TMatInvReport);
var
  I  : TLInt;
  IWS: TLInt;
  J  : TLInt;
  JB : TLInt;
  JJ : TLInt;
  JP : TLInt;
  K  : TLInt;
  V  : TLFloat;
  N1 : TLInt;
  N2 : TLInt;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if N <= ABLASBlockSize(A) then
    begin

      //
      // Form inv(U)
      //
      RMatrixTRInverseRec(A, Offs, N, True, False, WORK, Info, Rep);
      if Info <= 0 then
        begin
          Exit;
        end;

      //
      // Solve the equation inv(A)*L = inv(U) for inv(A).
      //
      J := N - 1;
      while J >= 0 do
        begin

          //
          // Copy current column of L to WORK and replace with zeros.
          //
          I := J + 1;
          while I <= N - 1 do
            begin
              WORK[I] := A[Offs + I, Offs + J];
              A[Offs + I, Offs + J] := 0;
              Inc(I);
            end;

          //
          // Compute current column of inv(A).
          //
          if J < N - 1 then
            begin
              I := 0;
              while I <= N - 1 do
                begin
                  V := APVDotProduct(@A[Offs + I][0], Offs + J + 1, Offs + N - 1,
                    @WORK[0], J + 1, N - 1);
                  A[Offs + I, Offs + J] := A[Offs + I, Offs + J] - V;
                  Inc(I);
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
  ABLASSplitLength(A, N, N1, N2);
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
  RMatrixLUInverseRec(A, Offs, N1, WORK, Info, Rep);
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
  I := 0;
  while I <= N1 - 1 do
    begin
      APVMul(@A[Offs + I][0], Offs + N1, Offs + N - 1, -1);
      Inc(I);
    end;
  RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, True, False, 0, A,
    Offs + N1, Offs);
  I := 0;
  while I <= N2 - 1 do
    begin
      APVMul(@A[Offs + N1 + I][0], Offs, Offs + N1 - 1, -1);
      Inc(I);
    end;

  //
  // Z := inv(L2*U2)
  //
  RMatrixLUInverseRec(A, Offs + N1, N2, WORK, Info, Rep);
end;

procedure CMatrixLUInverseRec(var A: TLComplexMatrix; Offs: TLInt;
  N: TLInt; var WORK: TLComplexVec; var Info: TLInt;
  var Rep: TMatInvReport);
var
  I  : TLInt;
  IWS: TLInt;
  J  : TLInt;
  JB : TLInt;
  JJ : TLInt;
  JP : TLInt;
  K  : TLInt;
  V  : TLComplex;
  N1 : TLInt;
  N2 : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if N < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if N <= ABLASComplexBlockSize(A) then
    begin

      //
      // Form inv(U)
      //
      CMatrixTRInverseRec(A, Offs, N, True, False, WORK, Info, Rep);
      if Info <= 0 then
        begin
          Exit;
        end;

      //
      // Solve the equation inv(A)*L = inv(U) for inv(A).
      //
      J := N - 1;
      while J >= 0 do
        begin

          //
          // Copy current column of L to WORK and replace with zeros.
          //
          I := J + 1;
          while I <= N - 1 do
            begin
              WORK[I] := A[Offs + I, Offs + J];
              A[Offs + I, Offs + J] := C_Complex(0);
              Inc(I);
            end;

          //
          // Compute current column of inv(A).
          //
          if J < N - 1 then
            begin
              I := 0;
              while I <= N - 1 do
                begin
                  i1_ := (J + 1) - (Offs + J + 1);
                  V := C_Complex(0.0);
                  for i_ := Offs + J + 1 to Offs + N - 1 do
                    begin
                      V := C_Add(V, C_Mul(A[Offs + I, i_], WORK[i_ + i1_]));
                    end;
                  A[Offs + I, Offs + J] := C_Sub(A[Offs + I, Offs + J], V);
                  Inc(I);
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
  ABLASComplexSplitLength(A, N, N1, N2);
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
  CMatrixLUInverseRec(A, Offs, N1, WORK, Info, Rep);
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
  I := 0;
  while I <= N1 - 1 do
    begin
      for i_ := Offs + N1 to Offs + N - 1 do
        begin
          A[Offs + I, i_] := C_MulR(A[Offs + I, i_], -1);
        end;
      Inc(I);
    end;
  CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, True, False, 0, A,
    Offs + N1, Offs);
  I := 0;
  while I <= N2 - 1 do
    begin
      for i_ := Offs to Offs + N1 - 1 do
        begin
          A[Offs + N1 + I, i_] := C_MulR(A[Offs + N1 + I, i_], -1);
        end;
      Inc(I);
    end;

  //
  // Z := inv(L2*U2)
  //
  CMatrixLUInverseRec(A, Offs + N1, N2, WORK, Info, Rep);
end;

(* ************************************************************************
  Recursive subroutine for SPD inversion.
  ************************************************************************ *)
procedure SPDMatrixCholeskyInverseRec(var A: TLMatrix; Offs: TLInt;
  N: TLInt; IsUpper: Boolean; var Tmp: TLVec);
var
  I    : TLInt;
  J    : TLInt;
  V    : TLFloat;
  N1   : TLInt;
  N2   : TLInt;
  Info2: TLInt;
  Rep2 : TMatInvReport;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if N < 1 then
    begin
      Exit;
    end;

  //
  // Base case
  //
  if N <= ABLASBlockSize(A) then
    begin
      RMatrixTRInverseRec(A, Offs, N, IsUpper, False, Tmp, Info2, Rep2);
      if IsUpper then
        begin

          //
          // Compute the product U * U'.
          // NOTE: we never assume that diagonal of U is real
          //
          I := 0;
          while I <= N - 1 do
            begin
              if I = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + I, Offs + I] := AP_Sqr(A[Offs + I, Offs + I]);
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
                  for i_ := 0 to I - 1 do
                    begin
                      Tmp[i_] := A[i_ + i1_, Offs + I];
                    end;
                  J := 0;
                  while J <= I - 1 do
                    begin
                      V := A[Offs + J, Offs + I];
                      APVAdd(@A[Offs + J][0], Offs + J, Offs + I - 1, @Tmp[0], J,
                        I - 1, V);
                      Inc(J);
                    end;
                  V := A[Offs + I, Offs + I];
                  for i_ := Offs to Offs + I - 1 do
                    begin
                      A[i_, Offs + I] := V * A[i_, Offs + I];
                    end;
                  A[Offs + I, Offs + I] := AP_Sqr(A[Offs + I, Offs + I]);
                end;
              Inc(I);
            end;
        end
      else
        begin

          //
          // Compute the product L' * L
          // NOTE: we never assume that diagonal of L is real
          //
          I := 0;
          while I <= N - 1 do
            begin
              if I = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + I, Offs + I] := AP_Sqr(A[Offs + I, Offs + I]);
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
                  APVMove(@Tmp[0], 0, I - 1, @A[Offs + I][0], Offs, Offs + I - 1);
                  J := 0;
                  while J <= I - 1 do
                    begin
                      V := A[Offs + I, Offs + J];
                      APVAdd(@A[Offs + J][0], Offs, Offs + J, @Tmp[0], 0, J, V);
                      Inc(J);
                    end;
                  V := A[Offs + I, Offs + I];
                  APVMul(@A[Offs + I][0], Offs, Offs + I - 1, V);
                  A[Offs + I, Offs + I] := AP_Sqr(A[Offs + I, Offs + I]);
                end;
              Inc(I);
            end;
        end;
      Exit;
    end;

  //
  // Recursive code: triangular factor inversion merged with
  // UU' or L'L multiplication
  //
  ABLASSplitLength(A, N, N1, N2);

  //
  // form off-diagonal block of trangular inverse
  //
  if IsUpper then
    begin
      I := 0;
      while I <= N1 - 1 do
        begin
          APVMul(@A[Offs + I][0], Offs + N1, Offs + N - 1, -1);
          Inc(I);
        end;
      RMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, False, 0, A, Offs,
        Offs + N1);
      RMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs, Offs + N1);
    end
  else
    begin
      I := 0;
      while I <= N2 - 1 do
        begin
          APVMul(@A[Offs + N1 + I][0], Offs, Offs + N1 - 1, -1);
          Inc(I);
        end;
      RMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, False, 0, A,
        Offs + N1, Offs);
      RMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs + N1, Offs);
    end;

  //
  // invert first diagonal block
  //
  SPDMatrixCholeskyInverseRec(A, Offs, N1, IsUpper, Tmp);

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
  SPDMatrixCholeskyInverseRec(A, Offs + N1, N2, IsUpper, Tmp);
end;

(* ************************************************************************
  Recursive subroutine for HPD inversion.
  ************************************************************************ *)
procedure HPDMatrixCholeskyInverseRec(var A: TLComplexMatrix;
  Offs: TLInt; N: TLInt; IsUpper: Boolean;
  var Tmp: TLComplexVec);
var
  I    : TLInt;
  J    : TLInt;
  V    : TLComplex;
  N1   : TLInt;
  N2   : TLInt;
  Info2: TLInt;
  Rep2 : TMatInvReport;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if N < 1 then
    begin
      Exit;
    end;

  //
  // Base case
  //
  if N <= ABLASComplexBlockSize(A) then
    begin
      CMatrixTRInverseRec(A, Offs, N, IsUpper, False, Tmp, Info2, Rep2);
      if IsUpper then
        begin

          //
          // Compute the product U * U'.
          // NOTE: we never assume that diagonal of U is real
          //
          I := 0;
          while I <= N - 1 do
            begin
              if I = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + I, Offs + I] := C_Complex(AP_Sqr(A[Offs + I, Offs + I].X) +
                    AP_Sqr(A[Offs + I, Offs + I].Y));
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
                  for i_ := 0 to I - 1 do
                    begin
                      Tmp[i_] := Conj(A[i_ + i1_, Offs + I]);
                    end;
                  J := 0;
                  while J <= I - 1 do
                    begin
                      V := A[Offs + J, Offs + I];
                      i1_ := (J) - (Offs + J);
                      for i_ := Offs + J to Offs + I - 1 do
                        begin
                          A[Offs + J, i_] := C_Add(A[Offs + J, i_],
                            C_Mul(V, Tmp[i_ + i1_]));
                        end;
                      Inc(J);
                    end;
                  V := Conj(A[Offs + I, Offs + I]);
                  for i_ := Offs to Offs + I - 1 do
                    begin
                      A[i_, Offs + I] := C_Mul(V, A[i_, Offs + I]);
                    end;
                  A[Offs + I, Offs + I] := C_Complex(AP_Sqr(A[Offs + I, Offs + I].X) +
                    AP_Sqr(A[Offs + I, Offs + I].Y));
                end;
              Inc(I);
            end;
        end
      else
        begin

          //
          // Compute the product L' * L
          // NOTE: we never assume that diagonal of L is real
          //
          I := 0;
          while I <= N - 1 do
            begin
              if I = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  A[Offs + I, Offs + I] := C_Complex(AP_Sqr(A[Offs + I, Offs + I].X) +
                    AP_Sqr(A[Offs + I, Offs + I].Y));
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
                  for i_ := 0 to I - 1 do
                    begin
                      Tmp[i_] := A[Offs + I, i_ + i1_];
                    end;
                  J := 0;
                  while J <= I - 1 do
                    begin
                      V := Conj(A[Offs + I, Offs + J]);
                      i1_ := (0) - (Offs);
                      for i_ := Offs to Offs + J do
                        begin
                          A[Offs + J, i_] := C_Add(A[Offs + J, i_],
                            C_Mul(V, Tmp[i_ + i1_]));
                        end;
                      Inc(J);
                    end;
                  V := Conj(A[Offs + I, Offs + I]);
                  for i_ := Offs to Offs + I - 1 do
                    begin
                      A[Offs + I, i_] := C_Mul(V, A[Offs + I, i_]);
                    end;
                  A[Offs + I, Offs + I] := C_Complex(AP_Sqr(A[Offs + I, Offs + I].X) +
                    AP_Sqr(A[Offs + I, Offs + I].Y));
                end;
              Inc(I);
            end;
        end;
      Exit;
    end;

  //
  // Recursive code: triangular factor inversion merged with
  // UU' or L'L multiplication
  //
  ABLASComplexSplitLength(A, N, N1, N2);

  //
  // form off-diagonal block of trangular inverse
  //
  if IsUpper then
    begin
      I := 0;
      while I <= N1 - 1 do
        begin
          for i_ := Offs + N1 to Offs + N - 1 do
            begin
              A[Offs + I, i_] := C_MulR(A[Offs + I, i_], -1);
            end;
          Inc(I);
        end;
      CMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, False, 0, A, Offs,
        Offs + N1);
      CMatrixRightTRSM(N1, N2, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs, Offs + N1);
    end
  else
    begin
      I := 0;
      while I <= N2 - 1 do
        begin
          for i_ := Offs to Offs + N1 - 1 do
            begin
              A[Offs + N1 + I, i_] := C_MulR(A[Offs + N1 + I, i_], -1);
            end;
          Inc(I);
        end;
      CMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, False, 0, A,
        Offs + N1, Offs);
      CMatrixLeftTRSM(N2, N1, A, Offs + N1, Offs + N1, IsUpper, False, 0, A,
        Offs + N1, Offs);
    end;

  //
  // invert first diagonal block
  //
  HPDMatrixCholeskyInverseRec(A, Offs, N1, IsUpper, Tmp);

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
  HPDMatrixCholeskyInverseRec(A, Offs + N1, N2, IsUpper, Tmp);
end;
