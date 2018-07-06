{ ****************************************************************************** }
{ * Level 2 and Level 3 BLAS operations support, by QQ 600585@qq.com           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure ABLASInternalSplitLength(n: TLInt; nb: TLInt;
  var N1: TLInt; var N2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixRightTRSM2(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; i1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; i1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixRightTRSM2(M: TLInt; n: TLInt;
  const A: TLMatrix; i1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  i2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const A: TLMatrix; i1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  i2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var C: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var C: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var C: TLMatrix; IC: TLInt;
  JC: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Splits matrix length in two parts, left part should match ABLAS block size

  INPUT PARAMETERS
  A   -   real matrix, is passed to ensure that we didn't split TLComplex matrix using real splitting subroutine. matrix itself is not changed.
  N   -   length, N>0

  OUTPUT PARAMETERS
  N1  -   length
  N2  -   length

  N1+N2=N, N1>=N2, N2 may be zero
  ************************************************************************ *)
procedure ABLASSplitLength(const A: TLMatrix; n: TLInt;
  var N1: TLInt; var N2: TLInt);
begin
  if n > ABLASBlockSize(A) then
    begin
      ABLASInternalSplitLength(n, ABLASBlockSize(A), N1, N2);
    end
  else
    begin
      ABLASInternalSplitLength(n, ABLASMicroBlockSize, N1, N2);
    end;
end;

(* ************************************************************************
  TLComplex ABLASSplitLength
  ************************************************************************ *)
procedure ABLASComplexSplitLength(const A: TLComplexMatrix; n: TLInt;
  var N1: TLInt; var N2: TLInt);
begin
  if n > ABLASComplexBlockSize(A) then
    begin
      ABLASInternalSplitLength(n, ABLASComplexBlockSize(A), N1, N2);
    end
  else
    begin
      ABLASInternalSplitLength(n, ABLASMicroBlockSize, N1, N2);
    end;
end;

(* ************************************************************************
  Returns block size - subdivision size where  cache-oblivious  soubroutines switch to the optimized kernel.

  INPUT PARAMETERS
  A   -   real matrix, is passed to ensure that we didn't split TLComplex matrix using real splitting subroutine. matrix itself is not changed.
  ************************************************************************ *)
function ABLASBlockSize(const A: TLMatrix): TLInt;
begin
  Result := 32;
end;

(* ************************************************************************
  Block size for TLComplex subroutines.
  ************************************************************************ *)
function ABLASComplexBlockSize(const A: TLComplexMatrix): TLInt;
begin
  Result := 24;
end;

(* ************************************************************************
  Microblock size
  ************************************************************************ *)
function ABLASMicroBlockSize(): TLInt;
begin
  Result := 8;
end;

(* ************************************************************************
  Cache-oblivous TLComplex "copy-and-transpose"

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  A   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure CMatrixTranspose(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; IA: TLInt; ja: TLInt;
  var b: TLComplexMatrix; IB: TLInt; JB: TLInt);
var
  i  : TLInt;
  s1 : TLInt;
  s2 : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if (M <= 2 * ABLASComplexBlockSize(A)) and (n <= 2 * ABLASComplexBlockSize(A))
  then
    begin

      //
      // base case
      //
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (ja) - (IB);
          for i_ := IB to IB + n - 1 do
            begin
              b[i_, JB + i] := A[IA + i, i_ + i1_];
            end;
          Inc(i);
        end;
    end
  else
    begin

      //
      // Cache-oblivious recursion
      //
      if M > n then
        begin
          ABLASComplexSplitLength(A, M, s1, s2);
          CMatrixTranspose(s1, n, A, IA, ja, b, IB, JB);
          CMatrixTranspose(s2, n, A, IA + s1, ja, b, IB, JB + s1);
        end
      else
        begin
          ABLASComplexSplitLength(A, n, s1, s2);
          CMatrixTranspose(M, s1, A, IA, ja, b, IB, JB);
          CMatrixTranspose(M, s2, A, IA, ja + s1, b, IB + s1, JB);
        end;
    end;
end;

(* ************************************************************************
  Cache-oblivous real "copy-and-transpose"

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  A   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure RMatrixTranspose(M: TLInt; n: TLInt;
  const A: TLMatrix; IA: TLInt; ja: TLInt;
  var b: TLMatrix; IB: TLInt; JB: TLInt);
var
  i  : TLInt;
  s1 : TLInt;
  s2 : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if (M <= 2 * ABLASBlockSize(A)) and (n <= 2 * ABLASBlockSize(A)) then
    begin

      //
      // base case
      //
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (ja) - (IB);
          for i_ := IB to IB + n - 1 do
            begin
              b[i_, JB + i] := A[IA + i, i_ + i1_];
            end;
          Inc(i);
        end;
    end
  else
    begin

      //
      // Cache-oblivious recursion
      //
      if M > n then
        begin
          ABLASSplitLength(A, M, s1, s2);
          RMatrixTranspose(s1, n, A, IA, ja, b, IB, JB);
          RMatrixTranspose(s2, n, A, IA + s1, ja, b, IB, JB + s1);
        end
      else
        begin
          ABLASSplitLength(A, n, s1, s2);
          RMatrixTranspose(M, s1, A, IA, ja, b, IB, JB);
          RMatrixTranspose(M, s2, A, IA, ja + s1, b, IB + s1, JB);
        end;
    end;
end;

(* ************************************************************************
  Copy

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  B   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure CMatrixCopy(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; IA: TLInt; ja: TLInt;
  var b: TLComplexMatrix; IB: TLInt; JB: TLInt);
var
  i  : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  i := 0;
  while i <= M - 1 do
    begin
      i1_ := (ja) - (JB);
      for i_ := JB to JB + n - 1 do
        begin
          b[IB + i, i_] := A[IA + i, i_ + i1_];
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Copy

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  B   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure RMatrixCopy(M: TLInt; n: TLInt; const A: TLMatrix;
  IA: TLInt; ja: TLInt; var b: TLMatrix; IB: TLInt;
  JB: TLInt);
var
  i: TLInt;
begin
  i := 0;
  while i <= M - 1 do
    begin
      APVMove(@b[IB + i][0], JB, JB + n - 1, @A[IA + i][0], ja, ja + n - 1);
      Inc(i);
    end;
end;

(* ************************************************************************
  Rank-1 correction: A := A + u*v'

  INPUT PARAMETERS:
  M   -   number of rows
  N   -   number of columns
  A   -   target matrix, MxN submatrix is updated
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  U   -   vector #1
  IU  -   subvector offset
  V   -   vector #2
  IV  -   subvector offset
  ************************************************************************ *)
procedure CMatrixRank1(M: TLInt; n: TLInt;
  var A: TLComplexMatrix; IA: TLInt; ja: TLInt;
  var u: TLComplexVec; IU: TLInt; var v: TLComplexVec;
  IV: TLInt);
var
  i  : TLInt;
  s  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  if (M = 0) or (n = 0) then
      Exit;
  i := 0;
  while i <= M - 1 do
    begin
      s := u[IU + i];
      i1_ := (IV) - (ja);
      for i_ := ja to ja + n - 1 do
        begin
          A[IA + i, i_] := C_Add(A[IA + i, i_], C_Mul(s, v[i_ + i1_]));
        end;
      Inc(i);
    end;
end;

(* ************************************************************************
  Rank-1 correction: A := A + u*v'

  INPUT PARAMETERS:
  M   -   number of rows
  N   -   number of columns
  A   -   target matrix, MxN submatrix is updated
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  U   -   vector #1
  IU  -   subvector offset
  V   -   vector #2
  IV  -   subvector offset
  ************************************************************************ *)
procedure RMatrixRank1(M: TLInt; n: TLInt; var A: TLMatrix;
  IA: TLInt; ja: TLInt; var u: TLVec; IU: TLInt;
  var v: TLVec; IV: TLInt);
var
  i: TLInt;
  s: TLFloat;
begin
  if (M = 0) or (n = 0) then
      Exit;
  i := 0;
  while i <= M - 1 do
    begin
      s := u[IU + i];
      APVAdd(@A[IA + i][0], ja, ja + n - 1, @v[0], IV, IV + n - 1, s);
      Inc(i);
    end;
end;

(* ************************************************************************
  Matrix-vector product: y := op(A)*x

  INPUT PARAMETERS:
  M   -   number of rows of op(A)
  M>=0
  N   -   number of columns of op(A)
  N>=0
  A   -   target matrix
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  OpA -   operation type:
  * OpA=0     =>  op(A) = A
  * OpA=1     =>  op(A) = A^T
  * OpA=2     =>  op(A) = A^H
  X   -   input vector
  IX  -   subvector offset
  IY  -   subvector offset

  OUTPUT PARAMETERS:
  Y   -   vector which stores result

  if M=0, then subroutine does nothing.
  if N=0, Y is filled by zeros.
  ************************************************************************ *)
procedure CMatrixMV(M: TLInt; n: TLInt; var A: TLComplexMatrix;
  IA: TLInt; ja: TLInt; OpA: TLInt;
  var X: TLComplexVec; ix: TLInt; var Y: TLComplexVec;
  iy: TLInt);
var
  i  : TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  if M = 0 then
      Exit;
  if n = 0 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          Y[iy + i] := C_Complex(0);
          Inc(i);
        end;
      Exit;
    end;
  if OpA = 0 then
    begin

      //
      // y = A*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (ix) - (ja);
          v := C_Complex(0.0);
          for i_ := ja to ja + n - 1 do
            begin
              v := C_Add(v, C_Mul(A[IA + i, i_], X[i_ + i1_]));
            end;
          Y[iy + i] := v;
          Inc(i);
        end;
      Exit;
    end;
  if OpA = 1 then
    begin

      //
      // y = A^T*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          Y[iy + i] := C_Complex(0);
          Inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := X[ix + i];
          i1_ := (ja) - (iy);
          for i_ := iy to iy + M - 1 do
            begin
              Y[i_] := C_Add(Y[i_], C_Mul(v, A[IA + i, i_ + i1_]));
            end;
          Inc(i);
        end;
      Exit;
    end;
  if OpA = 2 then
    begin

      //
      // y = A^H*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          Y[iy + i] := C_Complex(0);
          Inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := X[ix + i];
          i1_ := (ja) - (iy);
          for i_ := iy to iy + M - 1 do
            begin
              Y[i_] := C_Add(Y[i_], C_Mul(v, Conj(A[IA + i, i_ + i1_])));
            end;
          Inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Matrix-vector product: y := op(A)*x

  INPUT PARAMETERS:
  M   -   number of rows of op(A)
  N   -   number of columns of op(A)
  A   -   target matrix
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  OpA -   operation type:
  * OpA=0     =>  op(A) = A
  * OpA=1     =>  op(A) = A^T
  X   -   input vector
  IX  -   subvector offset
  IY  -   subvector offset

  OUTPUT PARAMETERS:
  Y   -   vector which stores result

  if M=0, then subroutine does nothing.
  if N=0, Y is filled by zeros.
  ************************************************************************ *)
procedure RMatrixMV(M: TLInt; n: TLInt; var A: TLMatrix;
  IA: TLInt; ja: TLInt; OpA: TLInt; var X: TLVec;
  ix: TLInt; var Y: TLVec; iy: TLInt);
var
  i: TLInt;
  v: TLFloat;
begin
  if M = 0 then
      Exit;
  if n = 0 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          Y[iy + i] := 0;
          Inc(i);
        end;
      Exit;
    end;
  if OpA = 0 then
    begin

      //
      // y = A*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          v := APVDotProduct(@A[IA + i][0], ja, ja + n - 1, @X[0], ix, ix + n - 1);
          Y[iy + i] := v;
          Inc(i);
        end;
      Exit;
    end;
  if OpA = 1 then
    begin

      //
      // y = A^T*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          Y[iy + i] := 0;
          Inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := X[ix + i];
          APVAdd(@Y[0], iy, iy + M - 1, @A[IA + i][0], ja, ja + M - 1, v);
          Inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  This subroutine calculates X*op(A^-1) where:
  * X is MxN general matrix
  * A is NxN upper/lower triangular/unitriangular matrix
  * "op" may be identity transformation, transposition, conjugate transposition

  Multiplication result replaces X.
  Cache-oblivious algorithm is used.

  INPUT PARAMETERS
  N   -   matrix size, N>=0
  M   -   matrix size, N>=0
  A       -   matrix, actial matrix is stored in A[I1:I1+N-1,J1:J1+N-1]
  I1      -   submatrix offset
  J1      -   submatrix offset
  IsUpper -   whether matrix is upper triangular
  IsUnit  -   whether matrix is unitriangular
  OpType  -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  C   -   matrix, actial matrix is stored in C[I2:I2+M-1,J2:J2+N-1]
  I2  -   submatrix offset
  J2  -   submatrix offset
  ************************************************************************ *)
procedure CMatrixRightTRSM(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; i1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; J2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(A);
  if (M <= bs) and (n <= bs) then
    begin
      CMatrixRightTRSM2(M, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      Exit;
    end;
  if M >= n then
    begin

      //
      // Split X: X*A = (X1 X2)^T*A
      //
      ABLASComplexSplitLength(A, M, s1, s2);
      CMatrixRightTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      CMatrixRightTRSM(s2, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2 + s1, J2);
    end
  else
    begin

      //
      // Split A:
      // (A1  A12)
      // X*op(A) = X*op(       )
      // (     A2)
      //
      // Different variants depending on
      // IsUpper/OpType combinations
      //
      ABLASComplexSplitLength(A, n, s1, s2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1
          // X*A^-1 = (X1 X2)*(       )
          // (     A2)
          //
          CMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          CMatrixGEMM(M, s2, s1, C_Complex(-1.0), X, i2, J2, 0, A, i1, J1 + s1, 0,
            C_Complex(1.0), X, i2, J2 + s1);
          CMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1
          // X*A^-1 = (X1 X2)*(        )
          // (A12' A2')
          //
          CMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          CMatrixGEMM(M, s1, s2, C_Complex(-1.0), X, i2, J2 + s1, 0, A, i1, J1 + s1,
            OpType, C_Complex(1.0), X, i2, J2);
          CMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1
          // X*A^-1 = (X1 X2)*(       )
          // (A21  A2)
          //
          CMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          CMatrixGEMM(M, s1, s2, C_Complex(-1.0), X, i2, J2 + s1, 0, A, i1 + s1, J1,
            0, C_Complex(1.0), X, i2, J2);
          CMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1
          // X*A^-1 = (X1 X2)*(        )
          // (     A2')
          //
          CMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          CMatrixGEMM(M, s2, s1, C_Complex(-1.0), X, i2, J2, 0, A, i1 + s1, J1,
            OpType, C_Complex(1.0), X, i2, J2 + s1);
          CMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  This subroutine calculates op(A^-1)*X where:
  * X is MxN general matrix
  * A is MxM upper/lower triangular/unitriangular matrix
  * "op" may be identity transformation, transposition, conjugate transposition

  Multiplication result replaces X.
  Cache-oblivious algorithm is used.

  INPUT PARAMETERS
  N   -   matrix size, N>=0
  M   -   matrix size, N>=0
  A       -   matrix, actial matrix is stored in A[I1:I1+M-1,J1:J1+M-1]
  I1      -   submatrix offset
  J1      -   submatrix offset
  IsUpper -   whether matrix is upper triangular
  IsUnit  -   whether matrix is unitriangular
  OpType  -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  C   -   matrix, actial matrix is stored in C[I2:I2+M-1,J2:J2+N-1]
  I2  -   submatrix offset
  J2  -   submatrix offset
  ************************************************************************ *)
procedure CMatrixLeftTRSM(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; i1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; J2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(A);
  if (M <= bs) and (n <= bs) then
    begin
      CMatrixLeftTRSM2(M, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      Exit;
    end;
  if n >= M then
    begin

      //
      // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
      //
      ABLASComplexSplitLength(X, n, s1, s2);
      CMatrixLeftTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      CMatrixLeftTRSM(M, s2, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2 + s1);
    end
  else
    begin

      //
      // Split A
      //
      ABLASComplexSplitLength(A, M, s1, s2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1  ( X1 )
          // A^-1*X* = (       )   *(    )
          // (     A2)    ( X2 )
          //
          CMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          CMatrixGEMM(s1, n, s2, C_Complex(-1.0), A, i1, J1 + s1, 0, X, i2 + s1, J2,
            0, C_Complex(1.0), X, i2, J2);
          CMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (A12' A2')   ( X2 )
          //
          CMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          CMatrixGEMM(s2, n, s1, C_Complex(-1.0), A, i1, J1 + s1, OpType, X, i2, J2,
            0, C_Complex(1.0), X, i2 + s1, J2);
          CMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1 ( X1 )
          // A^-1*X = (       )  *(    )
          // (A21  A2)   ( X2 )
          //
          CMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          CMatrixGEMM(s2, n, s1, C_Complex(-1.0), A, i1 + s1, J1, 0, X, i2, J2, 0,
            C_Complex(1.0), X, i2 + s1, J2);
          CMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (     A2')   ( X2 )
          //
          CMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          CMatrixGEMM(s1, n, s2, C_Complex(-1.0), A, i1 + s1, J1, OpType, X,
            i2 + s1, J2, 0, C_Complex(1.0), X, i2, J2);
          CMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixRightTRSM, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixRightTRSM(M: TLInt; n: TLInt;
  const A: TLMatrix; i1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  i2: TLInt; J2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(A);
  if (M <= bs) and (n <= bs) then
    begin
      RMatrixRightTRSM2(M, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      Exit;
    end;
  if M >= n then
    begin

      //
      // Split X: X*A = (X1 X2)^T*A
      //
      ABLASSplitLength(A, M, s1, s2);
      RMatrixRightTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      RMatrixRightTRSM(s2, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2 + s1, J2);
    end
  else
    begin

      //
      // Split A:
      // (A1  A12)
      // X*op(A) = X*op(       )
      // (     A2)
      //
      // Different variants depending on
      // IsUpper/OpType combinations
      //
      ABLASSplitLength(A, n, s1, s2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1
          // X*A^-1 = (X1 X2)*(       )
          // (     A2)
          //
          RMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          RMatrixGEMM(M, s2, s1, -1.0, X, i2, J2, 0, A, i1, J1 + s1, 0, 1.0, X,
            i2, J2 + s1);
          RMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1
          // X*A^-1 = (X1 X2)*(        )
          // (A12' A2')
          //
          RMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          RMatrixGEMM(M, s1, s2, -1.0, X, i2, J2 + s1, 0, A, i1, J1 + s1, OpType,
            1.0, X, i2, J2);
          RMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1
          // X*A^-1 = (X1 X2)*(       )
          // (A21  A2)
          //
          RMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          RMatrixGEMM(M, s1, s2, -1.0, X, i2, J2 + s1, 0, A, i1 + s1, J1, 0, 1.0,
            X, i2, J2);
          RMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1
          // X*A^-1 = (X1 X2)*(        )
          // (     A2')
          //
          RMatrixRightTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          RMatrixGEMM(M, s2, s1, -1.0, X, i2, J2, 0, A, i1 + s1, J1, OpType, 1.0, X,
            i2, J2 + s1);
          RMatrixRightTRSM(M, s2, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2, J2 + s1);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixLeftTRSM, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixLeftTRSM(M: TLInt; n: TLInt;
  const A: TLMatrix; i1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  i2: TLInt; J2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(A);
  if (M <= bs) and (n <= bs) then
    begin
      RMatrixLeftTRSM2(M, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      Exit;
    end;
  if n >= M then
    begin

      //
      // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
      //
      ABLASSplitLength(X, n, s1, s2);
      RMatrixLeftTRSM(M, s1, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
      RMatrixLeftTRSM(M, s2, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2 + s1);
    end
  else
    begin

      //
      // Split A
      //
      ABLASSplitLength(A, M, s1, s2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1  ( X1 )
          // A^-1*X* = (       )   *(    )
          // (     A2)    ( X2 )
          //
          RMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          RMatrixGEMM(s1, n, s2, -1.0, A, i1, J1 + s1, 0, X, i2 + s1, J2, 0, 1.0,
            X, i2, J2);
          RMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (A12' A2')   ( X2 )
          //
          RMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          RMatrixGEMM(s2, n, s1, -1.0, A, i1, J1 + s1, OpType, X, i2, J2, 0, 1.0, X,
            i2 + s1, J2);
          RMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1 ( X1 )
          // A^-1*X = (       )  *(    )
          // (A21  A2)   ( X2 )
          //
          RMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          RMatrixGEMM(s2, n, s1, -1.0, A, i1 + s1, J1, 0, X, i2, J2, 0, 1.0, X,
            i2 + s1, J2);
          RMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (     A2')   ( X2 )
          //
          RMatrixLeftTRSM(s2, n, A, i1 + s1, J1 + s1, IsUpper, IsUnit, OpType, X,
            i2 + s1, J2);
          RMatrixGEMM(s1, n, s2, -1.0, A, i1 + s1, J1, OpType, X, i2 + s1, J2, 0,
            1.0, X, i2, J2);
          RMatrixLeftTRSM(s1, n, A, i1, J1, IsUpper, IsUnit, OpType, X, i2, J2);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  This subroutine calculates  C=alpha*A*A^H+beta*C  or  C=alpha*A^H*A+beta*C
  where:
  * C is NxN Hermitian matrix given by its upper/lower triangle
  * A is NxK matrix when A*A^H is calculated, KxN matrix otherwise

  Additional info:
  * cache-oblivious algorithm is used.
  * multiplication result replaces C. If Beta=0, C elements are not used in
  calculations (not multiplied by zero - just not referenced)
  * if Alpha=0, A is not used (not multiplied by zero - just not referenced)
  * if both Beta and Alpha are zero, C is filled by zeros.

  INPUT PARAMETERS
  N       -   matrix size, N>=0
  K       -   matrix size, K>=0
  Alpha   -   coefficient
  A       -   matrix
  IA      -   submatrix offset
  JA      -   submatrix offset
  OpTypeA -   multiplication type:
  * 0 - A*A^H is calculated
  * 2 - A^H*A is calculated
  Beta    -   coefficient
  C       -   matrix
  IC      -   submatrix offset
  JC      -   submatrix offset
  IsUpper -   whether C is upper triangular or lower triangular
  ************************************************************************ *)
procedure CMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var C: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(A);
  if (n <= bs) and (k <= bs) then
    begin
      CMatrixSYRK2(n, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
      Exit;
    end;
  if k >= n then
    begin

      //
      // Split K
      //
      ABLASComplexSplitLength(A, k, s1, s2);
      if OpTypeA = 0 then
        begin
          CMatrixSYRK(n, s1, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          CMatrixSYRK(n, s2, alpha, A, IA, ja + s1, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end
      else
        begin
          CMatrixSYRK(n, s1, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          CMatrixSYRK(n, s2, alpha, A, IA + s1, ja, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end;
    end
  else
    begin

      //
      // Split N
      //
      ABLASComplexSplitLength(A, n, s1, s2);
      if (OpTypeA = 0) and IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          CMatrixGEMM(s1, s2, k, C_Complex(alpha), A, IA, ja, 0, A, IA + s1, ja, 2,
            C_Complex(beta), C, IC, JC + s1);
          CMatrixSYRK(s2, k, alpha, A, IA + s1, ja, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA = 0) and not IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          CMatrixGEMM(s2, s1, k, C_Complex(alpha), A, IA + s1, ja, 0, A, IA, ja, 2,
            C_Complex(beta), C, IC + s1, JC);
          CMatrixSYRK(s2, k, alpha, A, IA + s1, ja, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          CMatrixGEMM(s1, s2, k, C_Complex(alpha), A, IA, ja, 2, A, IA, ja + s1, 0,
            C_Complex(beta), C, IC, JC + s1);
          CMatrixSYRK(s2, k, alpha, A, IA, ja + s1, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and not IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          CMatrixGEMM(s2, s1, k, C_Complex(alpha), A, IA, ja + s1, 2, A, IA, ja, 0,
            C_Complex(beta), C, IC + s1, JC);
          CMatrixSYRK(s2, k, alpha, A, IA, ja + s1, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixSYRK, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var C: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(A);
  if (n <= bs) and (k <= bs) then
    begin
      RMatrixSYRK2(n, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
      Exit;
    end;
  if k >= n then
    begin

      //
      // Split K
      //
      ABLASSplitLength(A, k, s1, s2);
      if OpTypeA = 0 then
        begin
          RMatrixSYRK(n, s1, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          RMatrixSYRK(n, s2, alpha, A, IA, ja + s1, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end
      else
        begin
          RMatrixSYRK(n, s1, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          RMatrixSYRK(n, s2, alpha, A, IA + s1, ja, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end;
    end
  else
    begin

      //
      // Split N
      //
      ABLASSplitLength(A, n, s1, s2);
      if (OpTypeA = 0) and IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          RMatrixGEMM(s1, s2, k, alpha, A, IA, ja, 0, A, IA + s1, ja, 1, beta, C,
            IC, JC + s1);
          RMatrixSYRK(s2, k, alpha, A, IA + s1, ja, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA = 0) and not IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          RMatrixGEMM(s2, s1, k, alpha, A, IA + s1, ja, 0, A, IA, ja, 1, beta, C,
            IC + s1, JC);
          RMatrixSYRK(s2, k, alpha, A, IA + s1, ja, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          RMatrixGEMM(s1, s2, k, alpha, A, IA, ja, 1, A, IA, ja + s1, 0, beta, C,
            IC, JC + s1);
          RMatrixSYRK(s2, k, alpha, A, IA, ja + s1, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and not IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, A, IA, ja, OpTypeA, beta, C, IC, JC, IsUpper);
          RMatrixGEMM(s2, s1, k, alpha, A, IA, ja + s1, 1, A, IA, ja, 0, beta, C,
            IC + s1, JC);
          RMatrixSYRK(s2, k, alpha, A, IA, ja + s1, OpTypeA, beta, C, IC + s1,
            JC + s1, IsUpper);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  This subroutine calculates C = alpha*op1(A)*op2(B) +beta*C where:
  * C is MxN general matrix
  * op1(A) is MxK matrix
  * op2(B) is KxN matrix
  * "op" may be identity transformation, transposition, conjugate transposition

  Additional info:
  * cache-oblivious algorithm is used.
  * multiplication result replaces C. If Beta=0, C elements are not used in
  calculations (not multiplied by zero - just not referenced)
  * if Alpha=0, A is not used (not multiplied by zero - just not referenced)
  * if both Beta and Alpha are zero, C is filled by zeros.

  INPUT PARAMETERS
  N       -   matrix size, N>0
  M       -   matrix size, N>0
  K       -   matrix size, K>0
  Alpha   -   coefficient
  A       -   matrix
  IA      -   submatrix offset
  JA      -   submatrix offset
  OpTypeA -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  B       -   matrix
  IB      -   submatrix offset
  JB      -   submatrix offset
  OpTypeB -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  Beta    -   coefficient
  C       -   matrix
  IC      -   submatrix offset
  JC      -   submatrix offset
  ************************************************************************ *)
procedure CMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(A);
  if (M <= bs) and (n <= bs) and (k <= bs) then
    begin
      CMatrixGEMMK(M, n, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
        C, IC, JC);
      Exit;
    end;
  if (M >= n) and (M >= k) then
    begin

      //
      // A*B = (A1 A2)^T*B
      //
      ABLASComplexSplitLength(A, M, s1, s2);
      CMatrixGEMM(s1, n, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
        C, IC, JC);
      if OpTypeA = 0 then
        begin
          CMatrixGEMM(s2, n, k, alpha, A, IA + s1, ja, OpTypeA, b, IB, JB, OpTypeB,
            beta, C, IC + s1, JC);
        end
      else
        begin
          CMatrixGEMM(s2, n, k, alpha, A, IA, ja + s1, OpTypeA, b, IB, JB, OpTypeB,
            beta, C, IC + s1, JC);
        end;
      Exit;
    end;
  if (n >= M) and (n >= k) then
    begin

      //
      // A*B = A*(B1 B2)
      //
      ABLASComplexSplitLength(A, n, s1, s2);
      if OpTypeB = 0 then
        begin
          CMatrixGEMM(M, s1, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          CMatrixGEMM(M, s2, k, alpha, A, IA, ja, OpTypeA, b, IB, JB + s1, OpTypeB,
            beta, C, IC, JC + s1);
        end
      else
        begin
          CMatrixGEMM(M, s1, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          CMatrixGEMM(M, s2, k, alpha, A, IA, ja, OpTypeA, b, IB + s1, JB, OpTypeB,
            beta, C, IC, JC + s1);
        end;
      Exit;
    end;
  if (k >= M) and (k >= n) then
    begin

      //
      // A*B = (A1 A2)*(B1 B2)^T
      //
      ABLASComplexSplitLength(A, k, s1, s2);
      if (OpTypeA = 0) and (OpTypeB = 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, A, IA, ja + s1, OpTypeA, b, IB + s1, JB,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      if (OpTypeA = 0) and (OpTypeB <> 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, A, IA, ja + s1, OpTypeA, b, IB, JB + s1,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB = 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, A, IA + s1, ja, OpTypeA, b, IB + s1, JB,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB <> 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, A, IA + s1, ja, OpTypeA, b, IB, JB + s1,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Same as CMatrixGEMM, but for real numbers.
  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var C: TLMatrix; IC: TLInt; JC: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(A);
  if (M <= bs) and (n <= bs) and (k <= bs) then
    begin
      RMatrixGEMMK(M, n, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
        C, IC, JC);
      Exit;
    end;
  if (M >= n) and (M >= k) then
    begin

      //
      // A*B = (A1 A2)^T*B
      //
      ABLASSplitLength(A, M, s1, s2);
      if OpTypeA = 0 then
        begin
          RMatrixGEMM(s1, n, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(s2, n, k, alpha, A, IA + s1, ja, OpTypeA, b, IB, JB, OpTypeB,
            beta, C, IC + s1, JC);
        end
      else
        begin
          RMatrixGEMM(s1, n, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(s2, n, k, alpha, A, IA, ja + s1, OpTypeA, b, IB, JB, OpTypeB,
            beta, C, IC + s1, JC);
        end;
      Exit;
    end;
  if (n >= M) and (n >= k) then
    begin

      //
      // A*B = A*(B1 B2)
      //
      ABLASSplitLength(A, n, s1, s2);
      if OpTypeB = 0 then
        begin
          RMatrixGEMM(M, s1, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(M, s2, k, alpha, A, IA, ja, OpTypeA, b, IB, JB + s1, OpTypeB,
            beta, C, IC, JC + s1);
        end
      else
        begin
          RMatrixGEMM(M, s1, k, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(M, s2, k, alpha, A, IA, ja, OpTypeA, b, IB + s1, JB, OpTypeB,
            beta, C, IC, JC + s1);
        end;
      Exit;
    end;
  if (k >= M) and (k >= n) then
    begin

      //
      // A*B = (A1 A2)*(B1 B2)^T
      //
      ABLASSplitLength(A, k, s1, s2);
      if (OpTypeA = 0) and (OpTypeB = 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, A, IA, ja + s1, OpTypeA, b, IB + s1, JB,
            OpTypeB, 1.0, C, IC, JC);
        end;
      if (OpTypeA = 0) and (OpTypeB <> 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, A, IA, ja + s1, OpTypeA, b, IB, JB + s1,
            OpTypeB, 1.0, C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB = 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, A, IA + s1, ja, OpTypeA, b, IB + s1, JB,
            OpTypeB, 1.0, C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB <> 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, A, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta,
            C, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, A, IA + s1, ja, OpTypeA, b, IB, JB + s1,
            OpTypeB, 1.0, C, IC, JC);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  TLComplex ABLASSplitLength
  ************************************************************************ *)
procedure ABLASInternalSplitLength(n: TLInt; nb: TLInt;
  var N1: TLInt; var N2: TLInt);
var
  R: TLInt;
begin
  if n <= nb then
    begin

      //
      // Block size, no further splitting
      //
      N1 := n;
      N2 := 0;
    end
  else
    begin

      //
      // Greater than block size
      //
      if n mod nb <> 0 then
        begin

          //
          // Split remainder
          //
          N2 := n mod nb;
          N1 := n - N2;
        end
      else
        begin

          //
          // Split on block boundaries
          //
          N2 := n div 2;
          N1 := n - N2;
          if N1 mod nb = 0 then
              Exit;
          R := nb - N1 mod nb;
          N1 := N1 + R;
          N2 := N2 - R;
        end;
    end;
end;

(* ************************************************************************
  Level 2 variant of CMatrixRightTRSM
  ************************************************************************ *)
procedure CMatrixRightTRSM2(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; i1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; J2: TLInt);
var
  i  : TLInt;
  J  : TLInt;
  VC : TLComplex;
  VD : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  if IsUnit then
                    begin
                      VD := C_Complex(1);
                    end
                  else
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := C_Div(X[i2 + i, J2 + J], VD);
                  if J < n - 1 then
                    begin
                      VC := X[i2 + i, J2 + J];
                      i1_ := (J1 + J + 1) - (J2 + J + 1);
                      for i_ := J2 + J + 1 to J2 + n - 1 do
                        begin
                          X[i2 + i, i_] := C_Sub(X[i2 + i, i_],
                            C_Mul(VC, A[i1 + J, i_ + i1_]));
                        end;
                    end;
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := n - 1;
              while J >= 0 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J < n - 1 then
                    begin
                      i1_ := (J1 + J + 1) - (J2 + J + 1);
                      VC := C_Complex(0.0);
                      for i_ := J2 + J + 1 to J2 + n - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[i2 + i, i_], A[i1 + J, i_ + i1_]));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := C_Div(C_Sub(X[i2 + i, J2 + J], VC), VD);
                  Dec(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // X*A^(-H)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := n - 1;
              while J >= 0 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J < n - 1 then
                    begin
                      i1_ := (J1 + J + 1) - (J2 + J + 1);
                      VC := C_Complex(0.0);
                      for i_ := J2 + J + 1 to J2 + n - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[i2 + i, i_], Conj(A[i1 + J, i_ + i1_])));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := Conj(A[i1 + J, J1 + J]);
                    end;
                  X[i2 + i, J2 + J] := C_Div(C_Sub(X[i2 + i, J2 + J], VC), VD);
                  Dec(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := n - 1;
              while J >= 0 do
                begin
                  if IsUnit then
                    begin
                      VD := C_Complex(1);
                    end
                  else
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := C_Div(X[i2 + i, J2 + J], VD);
                  if J > 0 then
                    begin
                      VC := X[i2 + i, J2 + J];
                      i1_ := (J1) - (J2);
                      for i_ := J2 to J2 + J - 1 do
                        begin
                          X[i2 + i, i_] := C_Sub(X[i2 + i, i_],
                            C_Mul(VC, A[i1 + J, i_ + i1_]));
                        end;
                    end;
                  Dec(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J > 0 then
                    begin
                      i1_ := (J1) - (J2);
                      VC := C_Complex(0.0);
                      for i_ := J2 to J2 + J - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[i2 + i, i_], A[i1 + J, i_ + i1_]));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := C_Div(C_Sub(X[i2 + i, J2 + J], VC), VD);
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // X*A^(-H)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J > 0 then
                    begin
                      i1_ := (J1) - (J2);
                      VC := C_Complex(0.0);
                      for i_ := J2 to J2 + J - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[i2 + i, i_], Conj(A[i1 + J, i_ + i1_])));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := Conj(A[i1 + J, J1 + J]);
                    end;
                  X[i2 + i, J2 + J] := C_Div(C_Sub(X[i2 + i, J2 + J], VC), VD);
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level-2 subroutine
  ************************************************************************ *)
procedure CMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const A: TLComplexMatrix; i1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; J2: TLInt);
var
  i : TLInt;
  J : TLInt;
  VC: TLComplex;
  VD: TLComplex;
  i_: TLInt;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              J := i + 1;
              while J <= M - 1 do
                begin
                  VC := A[i1 + i, J1 + J];
                  for i_ := J2 to J2 + n - 1 do
                    begin
                      X[i2 + i, i_] := C_Sub(X[i2 + i, i_], C_Mul(VC, X[i2 + J, i_]));
                    end;
                  Inc(J);
                end;
              if not IsUnit then
                begin
                  VD := C_RDiv(1, A[i1 + i, J1 + i]);
                  for i_ := J2 to J2 + n - 1 do
                    begin
                      X[i2 + i, i_] := C_Mul(VD, X[i2 + i, i_]);
                    end;
                end;
              Dec(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, A[i1 + i, J1 + i]);
                end;
              for i_ := J2 to J2 + n - 1 do
                begin
                  X[i2 + i, i_] := C_Mul(VD, X[i2 + i, i_]);
                end;
              J := i + 1;
              while J <= M - 1 do
                begin
                  VC := A[i1 + i, J1 + J];
                  for i_ := J2 to J2 + n - 1 do
                    begin
                      X[i2 + J, i_] := C_Sub(X[i2 + J, i_], C_Mul(VC, X[i2 + i, i_]));
                    end;
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // A^(-H)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, Conj(A[i1 + i, J1 + i]));
                end;
              for i_ := J2 to J2 + n - 1 do
                begin
                  X[i2 + i, i_] := C_Mul(VD, X[i2 + i, i_]);
                end;
              J := i + 1;
              while J <= M - 1 do
                begin
                  VC := Conj(A[i1 + i, J1 + J]);
                  for i_ := J2 to J2 + n - 1 do
                    begin
                      X[i2 + J, i_] := C_Sub(X[i2 + J, i_], C_Mul(VC, X[i2 + i, i_]));
                    end;
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= i - 1 do
                begin
                  VC := A[i1 + i, J1 + J];
                  for i_ := J2 to J2 + n - 1 do
                    begin
                      X[i2 + i, i_] := C_Sub(X[i2 + i, i_], C_Mul(VC, X[i2 + J, i_]));
                    end;
                  Inc(J);
                end;
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, A[i1 + J, J1 + J]);
                end;
              for i_ := J2 to J2 + n - 1 do
                begin
                  X[i2 + i, i_] := C_Mul(VD, X[i2 + i, i_]);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, A[i1 + i, J1 + i]);
                end;
              for i_ := J2 to J2 + n - 1 do
                begin
                  X[i2 + i, i_] := C_Mul(VD, X[i2 + i, i_]);
                end;
              J := i - 1;
              while J >= 0 do
                begin
                  VC := A[i1 + i, J1 + J];
                  for i_ := J2 to J2 + n - 1 do
                    begin
                      X[i2 + J, i_] := C_Sub(X[i2 + J, i_], C_Mul(VC, X[i2 + i, i_]));
                    end;
                  Dec(J);
                end;
              Dec(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // A^(-H)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, Conj(A[i1 + i, J1 + i]));
                end;
              for i_ := J2 to J2 + n - 1 do
                begin
                  X[i2 + i, i_] := C_Mul(VD, X[i2 + i, i_]);
                end;
              J := i - 1;
              while J >= 0 do
                begin
                  VC := Conj(A[i1 + i, J1 + J]);
                  for i_ := J2 to J2 + n - 1 do
                    begin
                      X[i2 + J, i_] := C_Sub(X[i2 + J, i_], C_Mul(VC, X[i2 + i, i_]));
                    end;
                  Dec(J);
                end;
              Dec(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure RMatrixRightTRSM2(M: TLInt; n: TLInt;
  const A: TLMatrix; i1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  i2: TLInt; J2: TLInt);
var
  i : TLInt;
  J : TLInt;
  vr: TLFloat;
  VD: TLFloat;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  if IsUnit then
                    begin
                      VD := 1;
                    end
                  else
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := X[i2 + i, J2 + J] / VD;
                  if J < n - 1 then
                    begin
                      vr := X[i2 + i, J2 + J];
                      APVSub(@X[i2 + i][0], J2 + J + 1, J2 + n - 1, @A[i1 + J][0],
                        J1 + J + 1, J1 + n - 1, vr);
                    end;
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := n - 1;
              while J >= 0 do
                begin
                  vr := 0;
                  VD := 1;
                  if J < n - 1 then
                    begin
                      vr := APVDotProduct(@X[i2 + i][0], J2 + J + 1, J2 + n - 1,
                        @A[i1 + J][0], J1 + J + 1, J1 + n - 1);
                    end;
                  if not IsUnit then
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := (X[i2 + i, J2 + J] - vr) / VD;
                  Dec(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := n - 1;
              while J >= 0 do
                begin
                  if IsUnit then
                    begin
                      VD := 1;
                    end
                  else
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := X[i2 + i, J2 + J] / VD;
                  if J > 0 then
                    begin
                      vr := X[i2 + i, J2 + J];
                      APVSub(@X[i2 + i][0], J2, J2 + J - 1, @A[i1 + J][0], J1,
                        J1 + J - 1, vr);
                    end;
                  Dec(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  vr := 0;
                  VD := 1;
                  if J > 0 then
                    begin
                      vr := APVDotProduct(@X[i2 + i][0], J2, J2 + J - 1, @A[i1 + J][0],
                        J1, J1 + J - 1);
                    end;
                  if not IsUnit then
                    begin
                      VD := A[i1 + J, J1 + J];
                    end;
                  X[i2 + i, J2 + J] := (X[i2 + i, J2 + J] - vr) / VD;
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure RMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const A: TLMatrix; i1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  i2: TLInt; J2: TLInt);
var
  i : TLInt;
  J : TLInt;
  vr: TLFloat;
  VD: TLFloat;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              J := i + 1;
              while J <= M - 1 do
                begin
                  vr := A[i1 + i, J1 + J];
                  APVSub(@X[i2 + i][0], J2, J2 + n - 1, @X[i2 + J][0], J2,
                    J2 + n - 1, vr);
                  Inc(J);
                end;
              if not IsUnit then
                begin
                  VD := 1 / A[i1 + i, J1 + i];
                  APVMul(@X[i2 + i][0], J2, J2 + n - 1, VD);
                end;
              Dec(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / A[i1 + i, J1 + i];
                end;
              APVMul(@X[i2 + i][0], J2, J2 + n - 1, VD);
              J := i + 1;
              while J <= M - 1 do
                begin
                  vr := A[i1 + i, J1 + J];
                  APVSub(@X[i2 + J][0], J2, J2 + n - 1, @X[i2 + i][0], J2,
                    J2 + n - 1, vr);
                  Inc(J);
                end;
              Inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= i - 1 do
                begin
                  vr := A[i1 + i, J1 + J];
                  APVSub(@X[i2 + i][0], J2, J2 + n - 1, @X[i2 + J][0], J2,
                    J2 + n - 1, vr);
                  Inc(J);
                end;
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / A[i1 + J, J1 + J];
                end;
              APVMul(@X[i2 + i][0], J2, J2 + n - 1, VD);
              Inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / A[i1 + i, J1 + i];
                end;
              APVMul(@X[i2 + i][0], J2, J2 + n - 1, VD);
              J := i - 1;
              while J >= 0 do
                begin
                  vr := A[i1 + i, J1 + J];
                  APVSub(@X[i2 + J][0], J2, J2 + n - 1, @X[i2 + i][0], J2,
                    J2 + n - 1, vr);
                  Dec(J);
                end;
              Dec(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure CMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var C: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  i  : TLInt;
  J  : TLInt;
  J1 : TLInt;
  J2 : TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // Fast exit (nothing to be done)
  //
  if (AP_FP_Eq(alpha, 0) or (k = 0)) and AP_FP_Eq(beta, 1) then
    begin
      Exit;
    end;

  //
  // SYRK
  //
  if OpTypeA = 0 then
    begin

      //
      // C=alpha*A*A^H+beta*C
      //
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
              if AP_FP_NEq(alpha, 0) and (k > 0) then
                begin
                  v := C_Complex(0.0);
                  for i_ := ja to ja + k - 1 do
                    begin
                      v := C_Add(v, C_Mul(A[IA + i, i_], Conj(A[IA + J, i_])));
                    end;
                end
              else
                begin
                  v := C_Complex(0);
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  C[IC + i, JC + J] := C_MulR(v, alpha);
                end
              else
                begin
                  C[IC + i, JC + J] := C_Add(C_MulR(C[IC + i, JC + J], beta),
                    C_MulR(v, alpha));
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end
  else
    begin

      //
      // C=alpha*A^H*A+beta*C
      //
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
          if AP_FP_Eq(beta, 0) then
            begin
              J := J1;
              while J <= J2 do
                begin
                  C[IC + i, JC + J] := C_Complex(0);
                  Inc(J);
                end;
            end
          else
            begin
              for i_ := JC + J1 to JC + J2 do
                begin
                  C[IC + i, i_] := C_MulR(C[IC + i, i_], beta);
                end;
            end;
          Inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              if IsUpper then
                begin
                  J1 := J;
                  J2 := n - 1;
                end
              else
                begin
                  J1 := 0;
                  J2 := J;
                end;
              v := C_MulR(Conj(A[IA + i, ja + J]), alpha);
              i1_ := (ja + J1) - (JC + J1);
              for i_ := JC + J1 to JC + J2 do
                begin
                  C[IC + J, i_] := C_Add(C[IC + J, i_], C_Mul(v, A[IA + i, i_ + i1_]));
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Level 2 subrotuine
  ************************************************************************ *)
procedure RMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var C: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  i : TLInt;
  J : TLInt;
  J1: TLInt;
  J2: TLInt;
  v : TLFloat;
begin

  //
  // Fast exit (nothing to be done)
  //
  if (AP_FP_Eq(alpha, 0) or (k = 0)) and AP_FP_Eq(beta, 1) then
    begin
      Exit;
    end;

  //
  // SYRK
  //
  if OpTypeA = 0 then
    begin

      //
      // C=alpha*A*A^H+beta*C
      //
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
              if AP_FP_NEq(alpha, 0) and (k > 0) then
                begin
                  v := APVDotProduct(@A[IA + i][0], ja, ja + k - 1, @A[IA + J][0],
                    ja, ja + k - 1);
                end
              else
                begin
                  v := 0;
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  C[IC + i, JC + J] := alpha * v;
                end
              else
                begin
                  C[IC + i, JC + J] := beta * C[IC + i, JC + J] + alpha * v;
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end
  else
    begin

      //
      // C=alpha*A^H*A+beta*C
      //
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
          if AP_FP_Eq(beta, 0) then
            begin
              J := J1;
              while J <= J2 do
                begin
                  C[IC + i, JC + J] := 0;
                  Inc(J);
                end;
            end
          else
            begin
              APVMul(@C[IC + i][0], JC + J1, JC + J2, beta);
            end;
          Inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              if IsUpper then
                begin
                  J1 := J;
                  J2 := n - 1;
                end
              else
                begin
                  J1 := 0;
                  J2 := J;
                end;
              v := alpha * A[IA + i, ja + J];
              APVAdd(@C[IC + J][0], JC + J1, JC + J2, @A[IA + i][0], ja + J1,
                ja + J2, v);
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  GEMM kernel
  ************************************************************************ *)
procedure CMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt);
var
  i  : TLInt;
  J  : TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // Special case
  //
  if M * n = 0 then
    begin
      Exit;
    end;

  //
  // Another special case
  //
  if k = 0 then
    begin
      if C_NotEqualR(beta, 0) then
        begin
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  C[IC + i, JC + J] := C_Mul(beta, C[IC + i, JC + J]);
                  Inc(J);
                end;
              Inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  C[IC + i, JC + J] := C_Complex(0);
                  Inc(J);
                end;
              Inc(i);
            end;
        end;
      Exit;
    end;

  //
  // General case
  //
  if (OpTypeA = 0) and (OpTypeB <> 0) then
    begin

      //
      // A*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              if (k = 0) or C_EqualR(alpha, 0) then
                begin
                  v := C_Complex(0);
                end
              else
                begin
                  if OpTypeB = 1 then
                    begin
                      i1_ := (JB) - (ja);
                      v := C_Complex(0.0);
                      for i_ := ja to ja + k - 1 do
                        begin
                          v := C_Add(v, C_Mul(A[IA + i, i_], b[IB + J, i_ + i1_]));
                        end;
                    end
                  else
                    begin
                      i1_ := (JB) - (ja);
                      v := C_Complex(0.0);
                      for i_ := ja to ja + k - 1 do
                        begin
                          v := C_Add(v, C_Mul(A[IA + i, i_], Conj(b[IB + J, i_ + i1_])));
                        end;
                    end;
                end;
              if C_EqualR(beta, 0) then
                begin
                  C[IC + i, JC + J] := C_Mul(alpha, v);
                end
              else
                begin
                  C[IC + i, JC + J] := C_Add(C_Mul(beta, C[IC + i, JC + J]),
                    C_Mul(alpha, v));
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end;
  if (OpTypeA = 0) and (OpTypeB = 0) then
    begin

      //
      // A*B
      //
      i := 0;
      while i <= M - 1 do
        begin
          if C_NotEqualR(beta, 0) then
            begin
              for i_ := JC to JC + n - 1 do
                begin
                  C[IC + i, i_] := C_Mul(beta, C[IC + i, i_]);
                end;
            end
          else
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  C[IC + i, JC + J] := C_Complex(0);
                  Inc(J);
                end;
            end;
          if C_NotEqualR(alpha, 0) then
            begin
              J := 0;
              while J <= k - 1 do
                begin
                  v := C_Mul(alpha, A[IA + i, ja + J]);
                  i1_ := (JB) - (JC);
                  for i_ := JC to JC + n - 1 do
                    begin
                      C[IC + i, i_] := C_Add(C[IC + i, i_],
                        C_Mul(v, b[IB + J, i_ + i1_]));
                    end;
                  Inc(J);
                end;
            end;
          Inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB <> 0) then
    begin

      //
      // A'*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              if C_EqualR(alpha, 0) then
                begin
                  v := C_Complex(0);
                end
              else
                begin
                  if OpTypeA = 1 then
                    begin
                      if OpTypeB = 1 then
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(A[i_, ja + i], b[IB + J, i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(A[i_, ja + i], Conj(b[IB + J, i_ + i1_])));
                            end;
                        end;
                    end
                  else
                    begin
                      if OpTypeB = 1 then
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(Conj(A[i_, ja + i]), b[IB + J, i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(Conj(A[i_, ja + i]),
                                Conj(b[IB + J, i_ + i1_])));
                            end;
                        end;
                    end;
                end;
              if C_EqualR(beta, 0) then
                begin
                  C[IC + i, JC + J] := C_Mul(alpha, v);
                end
              else
                begin
                  C[IC + i, JC + J] := C_Add(C_Mul(beta, C[IC + i, JC + J]),
                    C_Mul(alpha, v));
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB = 0) then
    begin

      //
      // A'*B
      //
      if C_EqualR(beta, 0) then
        begin
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  C[IC + i, JC + J] := C_Complex(0);
                  Inc(J);
                end;
              Inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= M - 1 do
            begin
              for i_ := JC to JC + n - 1 do
                begin
                  C[IC + i, i_] := C_Mul(beta, C[IC + i, i_]);
                end;
              Inc(i);
            end;
        end;
      if C_NotEqualR(alpha, 0) then
        begin
          J := 0;
          while J <= k - 1 do
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  if OpTypeA = 1 then
                    begin
                      v := C_Mul(alpha, A[IA + J, ja + i]);
                    end
                  else
                    begin
                      v := C_Mul(alpha, Conj(A[IA + J, ja + i]));
                    end;
                  i1_ := (JB) - (JC);
                  for i_ := JC to JC + n - 1 do
                    begin
                      C[IC + i, i_] := C_Add(C[IC + i, i_],
                        C_Mul(v, b[IB + J, i_ + i1_]));
                    end;
                  Inc(i);
                end;
              Inc(J);
            end;
        end;
      Exit;
    end;
end;

(* ************************************************************************
  GEMM kernel
  ************************************************************************ *)
procedure RMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var C: TLMatrix; IC: TLInt; JC: TLInt);
var
  i  : TLInt;
  J  : TLInt;
  v  : TLFloat;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // if matrix size is zero
  //
  if M * n = 0 then
    begin
      Exit;
    end;

  //
  // if K=0, then C=Beta*C
  //
  if k = 0 then
    begin
      if AP_FP_NEq(beta, 1) then
        begin
          if AP_FP_NEq(beta, 0) then
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  J := 0;
                  while J <= n - 1 do
                    begin
                      C[IC + i, JC + J] := beta * C[IC + i, JC + J];
                      Inc(J);
                    end;
                  Inc(i);
                end;
            end
          else
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  J := 0;
                  while J <= n - 1 do
                    begin
                      C[IC + i, JC + J] := 0;
                      Inc(J);
                    end;
                  Inc(i);
                end;
            end;
        end;
      Exit;
    end;

  //
  // General case
  //
  if (OpTypeA = 0) and (OpTypeB <> 0) then
    begin

      //
      // A*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              if (k = 0) or AP_FP_Eq(alpha, 0) then
                begin
                  v := 0;
                end
              else
                begin
                  v := APVDotProduct(@A[IA + i][0], ja, ja + k - 1, @b[IB + J][0],
                    JB, JB + k - 1);
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  C[IC + i, JC + J] := alpha * v;
                end
              else
                begin
                  C[IC + i, JC + J] := beta * C[IC + i, JC + J] + alpha * v;
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end;
  if (OpTypeA = 0) and (OpTypeB = 0) then
    begin

      //
      // A*B
      //
      i := 0;
      while i <= M - 1 do
        begin
          if AP_FP_NEq(beta, 0) then
            begin
              APVMul(@C[IC + i][0], JC, JC + n - 1, beta);
            end
          else
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  C[IC + i, JC + J] := 0;
                  Inc(J);
                end;
            end;
          if AP_FP_NEq(alpha, 0) then
            begin
              J := 0;
              while J <= k - 1 do
                begin
                  v := alpha * A[IA + i, ja + J];
                  APVAdd(@C[IC + i][0], JC, JC + n - 1, @b[IB + J][0], JB,
                    JB + n - 1, v);
                  Inc(J);
                end;
            end;
          Inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB <> 0) then
    begin

      //
      // A'*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          J := 0;
          while J <= n - 1 do
            begin
              if AP_FP_Eq(alpha, 0) then
                begin
                  v := 0;
                end
              else
                begin
                  i1_ := (JB) - (IA);
                  v := 0.0;
                  for i_ := IA to IA + k - 1 do
                    begin
                      v := v + A[i_, ja + i] * b[IB + J, i_ + i1_];
                    end;
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  C[IC + i, JC + J] := alpha * v;
                end
              else
                begin
                  C[IC + i, JC + J] := beta * C[IC + i, JC + J] + alpha * v;
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB = 0) then
    begin

      //
      // A'*B
      //
      if AP_FP_Eq(beta, 0) then
        begin
          i := 0;
          while i <= M - 1 do
            begin
              J := 0;
              while J <= n - 1 do
                begin
                  C[IC + i, JC + J] := 0;
                  Inc(J);
                end;
              Inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= M - 1 do
            begin
              APVMul(@C[IC + i][0], JC, JC + n - 1, beta);
              Inc(i);
            end;
        end;
      if AP_FP_NEq(alpha, 0) then
        begin
          J := 0;
          while J <= k - 1 do
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  v := alpha * A[IA + J, ja + i];
                  APVAdd(@C[IC + i][0], JC, JC + n - 1, @b[IB + J][0], JB,
                    JB + n - 1, v);
                  Inc(i);
                end;
              Inc(J);
            end;
        end;
      Exit;
    end;
end; 
