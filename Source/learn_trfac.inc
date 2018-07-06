{ ****************************************************************************** }
{ * LU and Cholesky decompositions support, by QQ 600585@qq.com                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure CMatrixLUPRec(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUPRec(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixPLURec(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixPLURec(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixLUP2(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUP2(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixPLU2(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixPLU2(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function HPDMatrixCholeskyRec(var A: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SPDMatrixCholeskyRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function HPDMatrixCholesky2(var aaa: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SPDMatrixCholesky2(var aaa: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  LU decomposition of a general real matrix with row pivoting

  A is represented as A = P*L*U, where:
  * L is lower unitriangular matrix
  * U is upper triangular matrix
  * P = P0*P1*...*PK, K=min(M,N)-1,
  Pi - permutation matrix for I and Pivots[I]

  This is cache-oblivous implementation

  of LU decomposition.
  It is optimized for square matrices. As for rectangular matrices:
  * best case - M>>N
  * worst case - N>>M, small M, large N, matrix does not fit in CPU cache

  INPUT PARAMETERS:
  A       -   array[0..M-1, 0..N-1].
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.


  OUTPUT PARAMETERS:
  A       -   matrices L and U in compact form:
  * L is stored under main diagonal
  * U is stored on and above main diagonal
  Pivots  -   permutation matrix in compact form. array[0..Min(M-1,N-1)].
  ************************************************************************ *)
procedure RMatrixLU(var A: TLMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
begin
  Assert(M > 0, 'RMatrixLU: incorrect M!');
  Assert(n > 0, 'RMatrixLU: incorrect N!');
  RMatrixPLU(A, M, n, Pivots);
end;

(* ************************************************************************
  LU decomposition of a general TLComplex matrix with row pivoting

  A is represented as A = P*L*U, where:
  * L is lower unitriangular matrix
  * U is upper triangular matrix
  * P = P0*P1*...*PK, K=min(M,N)-1,
  Pi - permutation matrix for I and Pivots[I]

  This is cache-oblivous implementation

  of LU decomposition. It is optimized
  for square matrices. As for rectangular matrices:
  * best case - M>>N
  * worst case - N>>M, small M, large N, matrix does not fit in CPU cache

  INPUT PARAMETERS:
  A       -   array[0..M-1, 0..N-1].
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.


  OUTPUT PARAMETERS:
  A       -   matrices L and U in compact form:
  * L is stored under main diagonal
  * U is stored on and above main diagonal
  Pivots  -   permutation matrix in compact form. array[0..Min(M-1,N-1)].
  ************************************************************************ *)
procedure CMatrixLU(var A: TLComplexMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
begin
  Assert(M > 0, 'CMatrixLU: incorrect M!');
  Assert(n > 0, 'CMatrixLU: incorrect N!');
  CMatrixPLU(A, M, n, Pivots);
end;

(* ************************************************************************
  Cache-oblivious Cholesky decomposition

  The algorithm computes Cholesky decomposition  of  a  Hermitian  positive-
  definite matrix. The result of an algorithm is a representation  of  A  as
  A=U'*U  or A=L*L' (here X' detones conj(X^T)).

  INPUT PARAMETERS:
  A       -   upper or lower triangle of a factorized matrix.
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   if IsUpper=True, then A contains an upper triangle of
  a symmetric matrix, otherwise A contains a lower one.

  OUTPUT PARAMETERS:
  A       -   the result of factorization. If IsUpper=True, then
  the upper triangle contains matrix U, so that A = U'*U,
  and the elements below the main diagonal are not modified.
  Similarly, if IsUpper = False.

  RESULT:
  If  the  matrix  is  positive-definite,  the  function  returns  True.
  Otherwise, the function returns False. Contents of A is not determined
  in such case.
  ************************************************************************ *)
function HPDMatrixCholesky(var A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean): Boolean;
var
  tmp: TLComplexVec;
begin
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;
  SetLength(tmp, 2 * n);
  Result := HPDMatrixCholeskyRec(A, 0, n, IsUpper, tmp);
end;

(* ************************************************************************
  Cache-oblivious Cholesky decomposition

  The algorithm computes Cholesky decomposition  of  a  symmetric  positive-
  definite matrix. The result of an algorithm is a representation  of  A  as
  A=U^T*U  or A=L*L^T

  INPUT PARAMETERS:
  A       -   upper or lower triangle of a factorized matrix.
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   if IsUpper=True, then A contains an upper triangle of
  a symmetric matrix, otherwise A contains a lower one.

  OUTPUT PARAMETERS:
  A       -   the result of factorization. If IsUpper=True, then
  the upper triangle contains matrix U, so that A = U^T*U,
  and the elements below the main diagonal are not modified.
  Similarly, if IsUpper = False.

  RESULT:
  If  the  matrix  is  positive-definite,  the  function  returns  True.
  Otherwise, the function returns False. Contents of A is not determined
  in such case.
  ************************************************************************ *)
function SPDMatrixCholesky(var A: TLMatrix; n: TLInt;
  IsUpper: Boolean): Boolean;
var
  tmp: TLVec;
begin
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;
  SetLength(tmp, 2 * n);
  Result := SPDMatrixCholeskyRec(A, 0, n, IsUpper, tmp);
end;

procedure RMatrixLUP(var A: TLMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLVec;
  i: TLInt;
  J: TLInt;
  mx: TLFloat;
  v: TLFloat;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'RMatrixLUP: incorrect M!');
  Assert(n > 0, 'RMatrixLUP: incorrect N!');

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          mx := Max(mx, AbsReal(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := 1 / mx;
      i := 0;
      while i <= M - 1 do
        begin
          APVMul(@A[i][0], 0, n - 1, v);
          Inc(i);
        end;
    end;
  SetLength(Pivots, Min(M, n));
  SetLength(tmp, 2 * Max(M, n));
  RMatrixLUPRec(A, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := mx;
      i := 0;
      while i <= M - 1 do
        begin
          APVMul(@A[i][0], 0, Min(i, n - 1), v);
          Inc(i);
        end;
    end;
end;

procedure CMatrixLUP(var A: TLComplexMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLComplexVec;
  i: TLInt;
  J: TLInt;
  mx: TLFloat;
  v: TLFloat;
  i_: TLInt;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'CMatrixLUP: incorrect M!');
  Assert(n > 0, 'CMatrixLUP: incorrect N!');

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          mx := Max(mx, AbsComplex(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := 1 / mx;
      i := 0;
      while i <= M - 1 do
        begin
          for i_ := 0 to n - 1 do
            begin
              A[i, i_] := C_MulR(A[i, i_], v);
            end;
          Inc(i);
        end;
    end;
  SetLength(Pivots, Min(M, n));
  SetLength(tmp, 2 * Max(M, n));
  CMatrixLUPRec(A, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := mx;
      i := 0;
      while i <= M - 1 do
        begin
          for i_ := 0 to Min(i, n - 1) do
            begin
              A[i, i_] := C_MulR(A[i, i_], v);
            end;
          Inc(i);
        end;
    end;
end;

procedure RMatrixPLU(var A: TLMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLVec;
  i: TLInt;
  J: TLInt;
  mx: TLFloat;
  v: TLFloat;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'RMatrixPLU: incorrect M!');
  Assert(n > 0, 'RMatrixPLU: incorrect N!');
  SetLength(tmp, 2 * Max(M, n));
  SetLength(Pivots, Min(M, n));

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          mx := Max(mx, AbsReal(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := 1 / mx;
      i := 0;
      while i <= M - 1 do
        begin
          APVMul(@A[i][0], 0, n - 1, v);
          Inc(i);
        end;
    end;
  RMatrixPLURec(A, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := mx;
      i := 0;
      while i <= Min(M, n) - 1 do
        begin
          APVMul(@A[i][0], i, n - 1, v);
          Inc(i);
        end;
    end;
end;

procedure CMatrixPLU(var A: TLComplexMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLComplexVec;
  i: TLInt;
  J: TLInt;
  mx: TLFloat;
  v: TLComplex;
  i_: TLInt;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'CMatrixPLU: incorrect M!');
  Assert(n > 0, 'CMatrixPLU: incorrect N!');
  SetLength(tmp, 2 * Max(M, n));
  SetLength(Pivots, Min(M, n));

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          mx := Max(mx, AbsComplex(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := C_Complex(1 / mx);
      i := 0;
      while i <= M - 1 do
        begin
          for i_ := 0 to n - 1 do
            begin
              A[i, i_] := C_Mul(v, A[i, i_]);
            end;
          Inc(i);
        end;
    end;
  CMatrixPLURec(A, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := C_Complex(mx);
      i := 0;
      while i <= Min(M, n) - 1 do
        begin
          for i_ := i to n - 1 do
            begin
              A[i, i_] := C_Mul(v, A[i, i_]);
            end;
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent TLComplex LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure CMatrixLUPRec(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  m1: TLInt;
  m2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASComplexBlockSize(A) then
    begin
      CMatrixLUP2(A, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make N>=M
  //
  // ( A1 )
  // A = (    ), where A1 is square
  // ( A2 )
  //
  // Factorize A1, update A2
  //
  if M > n then
    begin
      CMatrixLUPRec(A, Offs, n, n, Pivots, tmp);
      i := 0;
      while i <= n - 1 do
        begin
          i1_ := (Offs + n) - (0);
          for i_ := 0 to M - n - 1 do
            begin
              tmp[i_] := A[i_ + i1_, Offs + i];
            end;
          for i_ := Offs + n to Offs + M - 1 do
            begin
              A[i_, Offs + i] := A[i_, Pivots[Offs + i]];
            end;
          i1_ := (0) - (Offs + n);
          for i_ := Offs + n to Offs + M - 1 do
            begin
              A[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
            end;
          Inc(i);
        end;
      CMatrixRightTRSM(M - n, n, A, Offs, Offs, True, True, 0, A, Offs + n, Offs);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASComplexSplitLength(A, M, m1, m2);
  CMatrixLUPRec(A, Offs, m1, n, Pivots, tmp);
  if m2 > 0 then
    begin
      i := 0;
      while i <= m1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + m1) - (0);
              for i_ := 0 to m2 - 1 do
                begin
                  tmp[i_] := A[i_ + i1_, Offs + i];
                end;
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  A[i_, Offs + i] := A[i_, Pivots[Offs + i]];
                end;
              i1_ := (0) - (Offs + m1);
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  A[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
                end;
            end;
          Inc(i);
        end;
      CMatrixRightTRSM(m2, m1, A, Offs, Offs, True, True, 0, A, Offs + m1, Offs);
      CMatrixGEMM(M - m1, n - m1, m1, C_Complex(-1.0), A, Offs + m1, Offs, 0, A,
        Offs, Offs + m1, 0, C_Complex(+1.0), A, Offs + m1, Offs + m1);
      CMatrixLUPRec(A, Offs + m1, M - m1, n - m1, Pivots, tmp);
      i := 0;
      while i <= m2 - 1 do
        begin
          if Offs + m1 + i <> Pivots[Offs + m1 + i] then
            begin
              i1_ := (Offs) - (0);
              for i_ := 0 to m1 - 1 do
                begin
                  tmp[i_] := A[i_ + i1_, Offs + m1 + i];
                end;
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  A[i_, Offs + m1 + i] := A[i_, Pivots[Offs + m1 + i]];
                end;
              i1_ := (0) - (Offs);
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  A[i_, Pivots[Offs + m1 + i]] := tmp[i_ + i1_];
                end;
            end;
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent real LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure RMatrixLUPRec(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  m1: TLInt;
  m2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASBlockSize(A) then
    begin
      RMatrixLUP2(A, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make N>=M
  //
  // ( A1 )
  // A = (    ), where A1 is square
  // ( A2 )
  //
  // Factorize A1, update A2
  //
  if M > n then
    begin
      RMatrixLUPRec(A, Offs, n, n, Pivots, tmp);
      i := 0;
      while i <= n - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + n) - (0);
              for i_ := 0 to M - n - 1 do
                begin
                  tmp[i_] := A[i_ + i1_, Offs + i];
                end;
              for i_ := Offs + n to Offs + M - 1 do
                begin
                  A[i_, Offs + i] := A[i_, Pivots[Offs + i]];
                end;
              i1_ := (0) - (Offs + n);
              for i_ := Offs + n to Offs + M - 1 do
                begin
                  A[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
                end;
            end;
          Inc(i);
        end;
      RMatrixRightTRSM(M - n, n, A, Offs, Offs, True, True, 0, A, Offs + n, Offs);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASSplitLength(A, M, m1, m2);
  RMatrixLUPRec(A, Offs, m1, n, Pivots, tmp);
  if m2 > 0 then
    begin
      i := 0;
      while i <= m1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + m1) - (0);
              for i_ := 0 to m2 - 1 do
                begin
                  tmp[i_] := A[i_ + i1_, Offs + i];
                end;
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  A[i_, Offs + i] := A[i_, Pivots[Offs + i]];
                end;
              i1_ := (0) - (Offs + m1);
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  A[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
                end;
            end;
          Inc(i);
        end;
      RMatrixRightTRSM(m2, m1, A, Offs, Offs, True, True, 0, A, Offs + m1, Offs);
      RMatrixGEMM(M - m1, n - m1, m1, -1.0, A, Offs + m1, Offs, 0, A, Offs,
        Offs + m1, 0, +1.0, A, Offs + m1, Offs + m1);
      RMatrixLUPRec(A, Offs + m1, M - m1, n - m1, Pivots, tmp);
      i := 0;
      while i <= m2 - 1 do
        begin
          if Offs + m1 + i <> Pivots[Offs + m1 + i] then
            begin
              i1_ := (Offs) - (0);
              for i_ := 0 to m1 - 1 do
                begin
                  tmp[i_] := A[i_ + i1_, Offs + m1 + i];
                end;
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  A[i_, Offs + m1 + i] := A[i_, Pivots[Offs + m1 + i]];
                end;
              i1_ := (0) - (Offs);
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  A[i_, Pivots[Offs + m1 + i]] := tmp[i_ + i1_];
                end;
            end;
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent TLComplex LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure CMatrixPLURec(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  N1: TLInt;
  N2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASComplexBlockSize(A) then
    begin
      CMatrixPLU2(A, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make M>=N.
  //
  // A = (A1 A2), where A1 is square
  // Factorize A1, update A2
  //
  if n > M then
    begin
      CMatrixPLURec(A, Offs, M, M, Pivots, tmp);
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (Offs + M) - (0);
          for i_ := 0 to n - M - 1 do
            begin
              tmp[i_] := A[Offs + i, i_ + i1_];
            end;
          for i_ := Offs + M to Offs + n - 1 do
            begin
              A[Offs + i, i_] := A[Pivots[Offs + i], i_];
            end;
          i1_ := (0) - (Offs + M);
          for i_ := Offs + M to Offs + n - 1 do
            begin
              A[Pivots[Offs + i], i_] := tmp[i_ + i1_];
            end;
          Inc(i);
        end;
      CMatrixLeftTRSM(M, n - M, A, Offs, Offs, False, True, 0, A, Offs, Offs + M);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASComplexSplitLength(A, n, N1, N2);
  CMatrixPLURec(A, Offs, M, N1, Pivots, tmp);
  if N2 > 0 then
    begin
      i := 0;
      while i <= N1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + N1) - (0);
              for i_ := 0 to N2 - 1 do
                begin
                  tmp[i_] := A[Offs + i, i_ + i1_];
                end;
              for i_ := Offs + N1 to Offs + n - 1 do
                begin
                  A[Offs + i, i_] := A[Pivots[Offs + i], i_];
                end;
              i1_ := (0) - (Offs + N1);
              for i_ := Offs + N1 to Offs + n - 1 do
                begin
                  A[Pivots[Offs + i], i_] := tmp[i_ + i1_];
                end;
            end;
          Inc(i);
        end;
      CMatrixLeftTRSM(N1, N2, A, Offs, Offs, False, True, 0, A, Offs, Offs + N1);
      CMatrixGEMM(M - N1, n - N1, N1, C_Complex(-1.0), A, Offs + N1, Offs, 0, A,
        Offs, Offs + N1, 0, C_Complex(+1.0), A, Offs + N1, Offs + N1);
      CMatrixPLURec(A, Offs + N1, M - N1, n - N1, Pivots, tmp);
      i := 0;
      while i <= N2 - 1 do
        begin
          if Offs + N1 + i <> Pivots[Offs + N1 + i] then
            begin
              i1_ := (Offs) - (0);
              for i_ := 0 to N1 - 1 do
                begin
                  tmp[i_] := A[Offs + N1 + i, i_ + i1_];
                end;
              for i_ := Offs to Offs + N1 - 1 do
                begin
                  A[Offs + N1 + i, i_] := A[Pivots[Offs + N1 + i], i_];
                end;
              i1_ := (0) - (Offs);
              for i_ := Offs to Offs + N1 - 1 do
                begin
                  A[Pivots[Offs + N1 + i], i_] := tmp[i_ + i1_];
                end;
            end;
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent real LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure RMatrixPLURec(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  N1: TLInt;
  N2: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASBlockSize(A) then
    begin
      RMatrixPLU2(A, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make M>=N.
  //
  // A = (A1 A2), where A1 is square
  // Factorize A1, update A2
  //
  if n > M then
    begin
      RMatrixPLURec(A, Offs, M, M, Pivots, tmp);
      i := 0;
      while i <= M - 1 do
        begin
          APVMove(@tmp[0], 0, n - M - 1, @A[Offs + i][0], Offs + M, Offs + n - 1);
          APVMove(@A[Offs + i][0], Offs + M, Offs + n - 1, @A[Pivots[Offs + i]][0],
            Offs + M, Offs + n - 1);
          APVMove(@A[Pivots[Offs + i]][0], Offs + M, Offs + n - 1, @tmp[0], 0,
            n - M - 1);
          Inc(i);
        end;
      RMatrixLeftTRSM(M, n - M, A, Offs, Offs, False, True, 0, A, Offs, Offs + M);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASSplitLength(A, n, N1, N2);
  RMatrixPLURec(A, Offs, M, N1, Pivots, tmp);
  if N2 > 0 then
    begin
      i := 0;
      while i <= N1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              APVMove(@tmp[0], 0, N2 - 1, @A[Offs + i][0], Offs + N1, Offs + n - 1);
              APVMove(@A[Offs + i][0], Offs + N1, Offs + n - 1,
                @A[Pivots[Offs + i]][0], Offs + N1, Offs + n - 1);
              APVMove(@A[Pivots[Offs + i]][0], Offs + N1, Offs + n - 1, @tmp[0],
                0, N2 - 1);
            end;
          Inc(i);
        end;
      RMatrixLeftTRSM(N1, N2, A, Offs, Offs, False, True, 0, A, Offs, Offs + N1);
      RMatrixGEMM(M - N1, n - N1, N1, -1.0, A, Offs + N1, Offs, 0, A, Offs,
        Offs + N1, 0, +1.0, A, Offs + N1, Offs + N1);
      RMatrixPLURec(A, Offs + N1, M - N1, n - N1, Pivots, tmp);
      i := 0;
      while i <= N2 - 1 do
        begin
          if Offs + N1 + i <> Pivots[Offs + N1 + i] then
            begin
              APVMove(@tmp[0], 0, N1 - 1, @A[Offs + N1 + i][0], Offs, Offs + N1 - 1);
              APVMove(@A[Offs + N1 + i][0], Offs, Offs + N1 - 1,
                @A[Pivots[Offs + N1 + i]][0], Offs, Offs + N1 - 1);
              APVMove(@A[Pivots[Offs + N1 + i]][0], Offs, Offs + N1 - 1, @tmp[0],
                0, N1 - 1);
            end;
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  TLComplex LUP kernel
  ************************************************************************ *)
procedure CMatrixLUP2(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  J: TLInt;
  JP: TLInt;
  s: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;

  //
  // main cycle
  //
  J := 0;
  while J <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot, swap columns
      //
      JP := J;
      i := J + 1;
      while i <= n - 1 do
        begin
          if AP_FP_Greater(AbsComplex(A[Offs + J, Offs + i]),
            AbsComplex(A[Offs + J, Offs + JP])) then
            begin
              JP := i;
            end;
          Inc(i);
        end;
      Pivots[Offs + J] := Offs + JP;
      if JP <> J then
        begin
          i1_ := (Offs) - (0);
          for i_ := 0 to M - 1 do
            begin
              tmp[i_] := A[i_ + i1_, Offs + J];
            end;
          for i_ := Offs to Offs + M - 1 do
            begin
              A[i_, Offs + J] := A[i_, Offs + JP];
            end;
          i1_ := (0) - (Offs);
          for i_ := Offs to Offs + M - 1 do
            begin
              A[i_, Offs + JP] := tmp[i_ + i1_];
            end;
        end;

      //
      // LU decomposition of 1x(N-J) matrix
      //
      if C_NotEqualR(A[Offs + J, Offs + J], 0) and (J + 1 <= n - 1) then
        begin
          s := C_RDiv(1, A[Offs + J, Offs + J]);
          for i_ := Offs + J + 1 to Offs + n - 1 do
            begin
              A[Offs + J, i_] := C_Mul(s, A[Offs + J, i_]);
            end;
        end;

      //
      // Update trailing (M-J-1)x(N-J-1) matrix
      //
      if J < Min(M - 1, n - 1) then
        begin
          i1_ := (Offs + J + 1) - (0);
          for i_ := 0 to M - J - 2 do
            begin
              tmp[i_] := A[i_ + i1_, Offs + J];
            end;
          i1_ := (Offs + J + 1) - (M);
          for i_ := M to M + n - J - 2 do
            begin
              tmp[i_] := C_Opposite(A[Offs + J, i_ + i1_]);
            end;
          CMatrixRank1(M - J - 1, n - J - 1, A, Offs + J + 1, Offs + J + 1, tmp,
            0, tmp, M);
        end;
      Inc(J);
    end;
end;

(* ************************************************************************
  Real LUP kernel
  ************************************************************************ *)
procedure RMatrixLUP2(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  J: TLInt;
  JP: TLInt;
  s: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;

  //
  // main cycle
  //
  J := 0;
  while J <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot, swap columns
      //
      JP := J;
      i := J + 1;
      while i <= n - 1 do
        begin
          if AP_FP_Greater(AbsReal(A[Offs + J, Offs + i]),
            AbsReal(A[Offs + J, Offs + JP])) then
            begin
              JP := i;
            end;
          Inc(i);
        end;
      Pivots[Offs + J] := Offs + JP;
      if JP <> J then
        begin
          i1_ := (Offs) - (0);
          for i_ := 0 to M - 1 do
            begin
              tmp[i_] := A[i_ + i1_, Offs + J];
            end;
          for i_ := Offs to Offs + M - 1 do
            begin
              A[i_, Offs + J] := A[i_, Offs + JP];
            end;
          i1_ := (0) - (Offs);
          for i_ := Offs to Offs + M - 1 do
            begin
              A[i_, Offs + JP] := tmp[i_ + i1_];
            end;
        end;

      //
      // LU decomposition of 1x(N-J) matrix
      //
      if AP_FP_NEq(A[Offs + J, Offs + J], 0) and (J + 1 <= n - 1) then
        begin
          s := 1 / A[Offs + J, Offs + J];
          APVMul(@A[Offs + J][0], Offs + J + 1, Offs + n - 1, s);
        end;

      //
      // Update trailing (M-J-1)x(N-J-1) matrix
      //
      if J < Min(M - 1, n - 1) then
        begin
          i1_ := (Offs + J + 1) - (0);
          for i_ := 0 to M - J - 2 do
            begin
              tmp[i_] := A[i_ + i1_, Offs + J];
            end;
          APVMoveNeg(@tmp[0], M, M + n - J - 2, @A[Offs + J][0], Offs + J + 1,
            Offs + n - 1);
          RMatrixRank1(M - J - 1, n - J - 1, A, Offs + J + 1, Offs + J + 1, tmp,
            0, tmp, M);
        end;
      Inc(J);
    end;
end;

(* ************************************************************************
  TLComplex PLU kernel
  ************************************************************************ *)
procedure CMatrixPLU2(var A: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  J: TLInt;
  JP: TLInt;
  s: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  J := 0;
  while J <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot and test for singularity.
      //
      JP := J;
      i := J + 1;
      while i <= M - 1 do
        begin
          if AP_FP_Greater(AbsComplex(A[Offs + i, Offs + J]),
            AbsComplex(A[Offs + JP, Offs + J])) then
            begin
              JP := i;
            end;
          Inc(i);
        end;
      Pivots[Offs + J] := Offs + JP;
      if C_NotEqualR(A[Offs + JP, Offs + J], 0) then
        begin

          //
          // Apply the interchange to rows
          //
          if JP <> J then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  s := A[Offs + J, Offs + i];
                  A[Offs + J, Offs + i] := A[Offs + JP, Offs + i];
                  A[Offs + JP, Offs + i] := s;
                  Inc(i);
                end;
            end;

          //
          // Compute elements J+1:M of J-th column.
          //
          if J + 1 <= M - 1 then
            begin
              s := C_RDiv(1, A[Offs + J, Offs + J]);
              for i_ := Offs + J + 1 to Offs + M - 1 do
                begin
                  A[i_, Offs + J] := C_Mul(s, A[i_, Offs + J]);
                end;
            end;
        end;
      if J < Min(M, n) - 1 then
        begin

          //
          // Update trailing submatrix.
          //
          i1_ := (Offs + J + 1) - (0);
          for i_ := 0 to M - J - 2 do
            begin
              tmp[i_] := A[i_ + i1_, Offs + J];
            end;
          i1_ := (Offs + J + 1) - (M);
          for i_ := M to M + n - J - 2 do
            begin
              tmp[i_] := C_Opposite(A[Offs + J, i_ + i1_]);
            end;
          CMatrixRank1(M - J - 1, n - J - 1, A, Offs + J + 1, Offs + J + 1, tmp,
            0, tmp, M);
        end;
      Inc(J);
    end;
end;

(* ************************************************************************
  Real PLU kernel
  ************************************************************************ *)
procedure RMatrixPLU2(var A: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  J: TLInt;
  JP: TLInt;
  s: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  J := 0;
  while J <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot and test for singularity.
      //
      JP := J;
      i := J + 1;
      while i <= M - 1 do
        begin
          if AP_FP_Greater(AbsReal(A[Offs + i, Offs + J]),
            AbsReal(A[Offs + JP, Offs + J])) then
            begin
              JP := i;
            end;
          Inc(i);
        end;
      Pivots[Offs + J] := Offs + JP;
      if AP_FP_NEq(A[Offs + JP, Offs + J], 0) then
        begin

          //
          // Apply the interchange to rows
          //
          if JP <> J then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  s := A[Offs + J, Offs + i];
                  A[Offs + J, Offs + i] := A[Offs + JP, Offs + i];
                  A[Offs + JP, Offs + i] := s;
                  Inc(i);
                end;
            end;

          //
          // Compute elements J+1:M of J-th column.
          //
          if J + 1 <= M - 1 then
            begin
              s := 1 / A[Offs + J, Offs + J];
              for i_ := Offs + J + 1 to Offs + M - 1 do
                begin
                  A[i_, Offs + J] := s * A[i_, Offs + J];
                end;
            end;
        end;
      if J < Min(M, n) - 1 then
        begin

          //
          // Update trailing submatrix.
          //
          i1_ := (Offs + J + 1) - (0);
          for i_ := 0 to M - J - 2 do
            begin
              tmp[i_] := A[i_ + i1_, Offs + J];
            end;
          APVMoveNeg(@tmp[0], M, M + n - J - 2, @A[Offs + J][0], Offs + J + 1,
            Offs + n - 1);
          RMatrixRank1(M - J - 1, n - J - 1, A, Offs + J + 1, Offs + J + 1, tmp,
            0, tmp, M);
        end;
      Inc(J);
    end;
end;

(* ************************************************************************
  Recursive computational subroutine for HPDMatrixCholesky
  ************************************************************************ *)
function HPDMatrixCholeskyRec(var A: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean;
var
  N1: TLInt;
  N2: TLInt;
begin

  //
  // check N
  //
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;

  //
  // special cases
  //
  if n = 1 then
    begin
      if AP_FP_Greater(A[Offs, Offs].X, 0) then
        begin
          A[Offs, Offs] := C_Complex(Sqrt(A[Offs, Offs].X));
          Result := True;
        end
      else
        begin
          Result := False;
        end;
      Exit;
    end;
  if n <= ABLASComplexBlockSize(A) then
    begin
      Result := HPDMatrixCholesky2(A, Offs, n, IsUpper, tmp);
      Exit;
    end;

  //
  // general case: split task in cache-oblivious manner
  //
  Result := True;
  ABLASComplexSplitLength(A, n, N1, N2);
  Result := HPDMatrixCholeskyRec(A, Offs, N1, IsUpper, tmp);
  if not Result then
    begin
      Exit;
    end;
  if N2 > 0 then
    begin
      if IsUpper then
        begin
          CMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, False, 2, A, Offs,
            Offs + N1);
          CMatrixSYRK(N2, N1, -1.0, A, Offs, Offs + N1, 2, +1.0, A, Offs + N1,
            Offs + N1, IsUpper);
        end
      else
        begin
          CMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, False, 2, A,
            Offs + N1, Offs);
          CMatrixSYRK(N2, N1, -1.0, A, Offs + N1, Offs, 0, +1.0, A, Offs + N1,
            Offs + N1, IsUpper);
        end;
      Result := HPDMatrixCholeskyRec(A, Offs + N1, N2, IsUpper, tmp);
      if not Result then
        begin
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Recursive computational subroutine for SPDMatrixCholesky
  ************************************************************************ *)
function SPDMatrixCholeskyRec(var A: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean;
var
  N1: TLInt;
  N2: TLInt;
begin

  //
  // check N
  //
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;

  //
  // special cases
  //
  if n = 1 then
    begin
      if AP_FP_Greater(A[Offs, Offs], 0) then
        begin
          A[Offs, Offs] := Sqrt(A[Offs, Offs]);
          Result := True;
        end
      else
        begin
          Result := False;
        end;
      Exit;
    end;
  if n <= ABLASBlockSize(A) then
    begin
      Result := SPDMatrixCholesky2(A, Offs, n, IsUpper, tmp);
      Exit;
    end;

  //
  // general case: split task in cache-oblivious manner
  //
  Result := True;
  ABLASSplitLength(A, n, N1, N2);
  Result := SPDMatrixCholeskyRec(A, Offs, N1, IsUpper, tmp);
  if not Result then
    begin
      Exit;
    end;
  if N2 > 0 then
    begin
      if IsUpper then
        begin
          RMatrixLeftTRSM(N1, N2, A, Offs, Offs, IsUpper, False, 1, A, Offs,
            Offs + N1);
          RMatrixSYRK(N2, N1, -1.0, A, Offs, Offs + N1, 1, +1.0, A, Offs + N1,
            Offs + N1, IsUpper);
        end
      else
        begin
          RMatrixRightTRSM(N2, N1, A, Offs, Offs, IsUpper, False, 1, A,
            Offs + N1, Offs);
          RMatrixSYRK(N2, N1, -1.0, A, Offs + N1, Offs, 0, +1.0, A, Offs + N1,
            Offs + N1, IsUpper);
        end;
      Result := SPDMatrixCholeskyRec(A, Offs + N1, N2, IsUpper, tmp);
      if not Result then
        begin
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level-2 Hermitian Cholesky subroutine.
  ************************************************************************ *)
function HPDMatrixCholesky2(var aaa: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean;
var
  i: TLInt;
  J: TLInt;
  k: TLInt;
  J1: TLInt;
  J2: TLInt;
  AJJ: TLFloat;
  v: TLComplex;
  R: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  Result := True;
  if n < 0 then
    begin
      Result := False;
      Exit;
    end;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Exit;
    end;
  if IsUpper then
    begin

      //
      // Compute the Cholesky factorization A = U'*U.
      //
      J := 0;
      while J <= n - 1 do
        begin

          //
          // Compute U(J,J) and test for non-positive-definiteness.
          //
          v := C_Complex(0.0);
          for i_ := Offs to Offs + J - 1 do
            begin
              v := C_Add(v, C_Mul(Conj(aaa[i_, Offs + J]), aaa[i_, Offs + J]));
            end;
          AJJ := C_Sub(aaa[Offs + J, Offs + J], v).X;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + J, Offs + J] := C_Complex(AJJ);
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + J, Offs + J] := C_Complex(AJJ);

          //
          // Compute elements J+1:N-1 of row J.
          //
          if J < n - 1 then
            begin
              if J > 0 then
                begin
                  i1_ := (Offs) - (0);
                  for i_ := 0 to J - 1 do
                    begin
                      tmp[i_] := C_Opposite(Conj(aaa[i_ + i1_, Offs + J]));
                    end;
                  CMatrixMV(n - J - 1, J, aaa, Offs, Offs + J + 1, 1, tmp, 0, tmp, n);
                  i1_ := (n) - (Offs + J + 1);
                  for i_ := Offs + J + 1 to Offs + n - 1 do
                    begin
                      aaa[Offs + J, i_] := C_Add(aaa[Offs + J, i_], tmp[i_ + i1_]);
                    end;
                end;
              R := 1 / AJJ;
              for i_ := Offs + J + 1 to Offs + n - 1 do
                begin
                  aaa[Offs + J, i_] := C_MulR(aaa[Offs + J, i_], R);
                end;
            end;
          Inc(J);
        end;
    end
  else
    begin

      //
      // Compute the Cholesky factorization A = L*L'.
      //
      J := 0;
      while J <= n - 1 do
        begin

          //
          // Compute L(J+1,J+1) and test for non-positive-definiteness.
          //
          v := C_Complex(0.0);
          for i_ := Offs to Offs + J - 1 do
            begin
              v := C_Add(v, C_Mul(Conj(aaa[Offs + J, i_]), aaa[Offs + J, i_]));
            end;
          AJJ := C_Sub(aaa[Offs + J, Offs + J], v).X;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + J, Offs + J] := C_Complex(AJJ);
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + J, Offs + J] := C_Complex(AJJ);

          //
          // Compute elements J+1:N of column J.
          //
          if J < n - 1 then
            begin
              if J > 0 then
                begin
                  i1_ := (Offs) - (0);
                  for i_ := 0 to J - 1 do
                    begin
                      tmp[i_] := Conj(aaa[Offs + J, i_ + i1_]);
                    end;
                  CMatrixMV(n - J - 1, J, aaa, Offs + J + 1, Offs, 0, tmp, 0, tmp, n);
                  i := 0;
                  while i <= n - J - 2 do
                    begin
                      aaa[Offs + J + 1 + i, Offs + J] :=
                        C_DivR(C_Sub(aaa[Offs + J + 1 + i, Offs + J], tmp[n + i]), AJJ);
                      Inc(i);
                    end;
                end
              else
                begin
                  i := 0;
                  while i <= n - J - 2 do
                    begin
                      aaa[Offs + J + 1 + i, Offs + J] :=
                        C_DivR(aaa[Offs + J + 1 + i, Offs + J], AJJ);
                      Inc(i);
                    end;
                end;
            end;
          Inc(J);
        end;
    end;
end;

(* ************************************************************************
  Level-2 Cholesky subroutine
  ************************************************************************ *)
function SPDMatrixCholesky2(var aaa: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean;
var
  i: TLInt;
  J: TLInt;
  k: TLInt;
  J1: TLInt;
  J2: TLInt;
  AJJ: TLFloat;
  v: TLFloat;
  R: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  Result := True;
  if n < 0 then
    begin
      Result := False;
      Exit;
    end;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Exit;
    end;
  if IsUpper then
    begin

      //
      // Compute the Cholesky factorization A = U'*U.
      //
      J := 0;
      while J <= n - 1 do
        begin

          //
          // Compute U(J,J) and test for non-positive-definiteness.
          //
          v := 0.0;
          for i_ := Offs to Offs + J - 1 do
            begin
              v := v + aaa[i_, Offs + J] * aaa[i_, Offs + J];
            end;
          AJJ := aaa[Offs + J, Offs + J] - v;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + J, Offs + J] := AJJ;
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + J, Offs + J] := AJJ;

          //
          // Compute elements J+1:N-1 of row J.
          //
          if J < n - 1 then
            begin
              if J > 0 then
                begin
                  i1_ := (Offs) - (0);
                  for i_ := 0 to J - 1 do
                    begin
                      tmp[i_] := -aaa[i_ + i1_, Offs + J];
                    end;
                  RMatrixMV(n - J - 1, J, aaa, Offs, Offs + J + 1, 1, tmp, 0, tmp, n);
                  APVAdd(@aaa[Offs + J][0], Offs + J + 1, Offs + n - 1, @tmp[0], n,
                    2 * n - J - 2);
                end;
              R := 1 / AJJ;
              APVMul(@aaa[Offs + J][0], Offs + J + 1, Offs + n - 1, R);
            end;
          Inc(J);
        end;
    end
  else
    begin

      //
      // Compute the Cholesky factorization A = L*L'.
      //
      J := 0;
      while J <= n - 1 do
        begin

          //
          // Compute L(J+1,J+1) and test for non-positive-definiteness.
          //
          v := APVDotProduct(@aaa[Offs + J][0], Offs, Offs + J - 1,
            @aaa[Offs + J][0], Offs, Offs + J - 1);
          AJJ := aaa[Offs + J, Offs + J] - v;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + J, Offs + J] := AJJ;
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + J, Offs + J] := AJJ;

          //
          // Compute elements J+1:N of column J.
          //
          if J < n - 1 then
            begin
              if J > 0 then
                begin
                  APVMove(@tmp[0], 0, J - 1, @aaa[Offs + J][0], Offs, Offs + J - 1);
                  RMatrixMV(n - J - 1, J, aaa, Offs + J + 1, Offs, 0, tmp, 0, tmp, n);
                  i := 0;
                  while i <= n - J - 2 do
                    begin
                      aaa[Offs + J + 1 + i, Offs + J] :=
                        (aaa[Offs + J + 1 + i, Offs + J] - tmp[n + i]) / AJJ;
                      Inc(i);
                    end;
                end
              else
                begin
                  i := 0;
                  while i <= n - J - 2 do
                    begin
                      aaa[Offs + J + 1 + i, Offs + J] :=
                        aaa[Offs + J + 1 + i, Offs + J] / AJJ;
                      Inc(i);
                    end;
                end;
            end;
          Inc(J);
        end;
    end;
end; 
