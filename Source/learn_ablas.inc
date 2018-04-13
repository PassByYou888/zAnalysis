{ ****************************************************************************** }
{ * Level 2 and Level 3 BLAS operations support, by QQ 600585@qq.com           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

procedure ABLASSplitLength(const A: TLMatrix; N: TLInt; var N1: TLInt; var N2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ABLASComplexSplitLength(const A: TLComplexMatrix; N: TLInt; var N1: TLInt; var N2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ABLASBlockSize(const A: TLMatrix): TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ABLASComplexBlockSize(const A: TLComplexMatrix): TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ABLASMicroBlockSize(): TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixTranspose(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  var B: TLComplexMatrix; IB: TLInt; JB: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixTranspose(M: TLInt; N: TLInt;
  const A: TLMatrix; IA: TLInt; JA: TLInt;
  var B: TLMatrix; IB: TLInt; JB: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixCopy(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  var B: TLComplexMatrix; IB: TLInt; JB: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixCopy(M: TLInt; N: TLInt; const A: TLMatrix;
  IA: TLInt; JA: TLInt; var B: TLMatrix; IB: TLInt; JB: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixRank1(M: TLInt; N: TLInt;
  var A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  var U: TLComplexVec; IU: TLInt; var v: TLComplexVec;
  IV: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixRank1(M: TLInt; N: TLInt; var A: TLMatrix;
  IA: TLInt; JA: TLInt; var U: TLVec; IU: TLInt;
  var v: TLVec; IV: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixMV(M: TLInt; N: TLInt; var A: TLComplexMatrix;
  IA: TLInt; JA: TLInt; OpA: TLInt;
  var X: TLComplexVec; IX: TLInt; var Y: TLComplexVec; IY: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixMV(M: TLInt; N: TLInt; var A: TLMatrix;
  IA: TLInt; JA: TLInt; OpA: TLInt; var X: TLVec;
  IX: TLInt; var Y: TLVec; IY: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixRightTRSM(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixLeftTRSM(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixRightTRSM(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLeftTRSM(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixSYRK(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixSYRK(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixGEMM(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; Beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixGEMM(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  Beta: TLFloat; var C: TLMatrix; IC: TLInt; JC: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure ABLASInternalSplitLength(N: TLInt; NB: TLInt;
  var N1: TLInt; var N2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixRightTRSM2(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLeftTRSM2(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixRightTRSM2(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixLeftTRSM2(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixSYRK2(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixSYRK2(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixGEMMK(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; Beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RMatrixGEMMK(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  Beta: TLFloat; var C: TLMatrix; IC: TLInt;
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
procedure ABLASSplitLength(const A: TLMatrix; N: TLInt;
  var N1: TLInt; var N2: TLInt);
begin
  if N > ABLASBlockSize(A) then
    begin
      ABLASInternalSplitLength(N, ABLASBlockSize(A), N1, N2);
    end
  else
    begin
      ABLASInternalSplitLength(N, ABLASMicroBlockSize, N1, N2);
    end;
end;

(* ************************************************************************
  TLComplex ABLASSplitLength
  ************************************************************************ *)
procedure ABLASComplexSplitLength(const A: TLComplexMatrix; N: TLInt;
  var N1: TLInt; var N2: TLInt);
begin
  if N > ABLASComplexBlockSize(A) then
    begin
      ABLASInternalSplitLength(N, ABLASComplexBlockSize(A), N1, N2);
    end
  else
    begin
      ABLASInternalSplitLength(N, ABLASMicroBlockSize, N1, N2);
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
procedure CMatrixTranspose(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  var B: TLComplexMatrix; IB: TLInt; JB: TLInt);
var
  I  : TLInt;
  S1 : TLInt;
  S2 : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if (M <= 2 * ABLASComplexBlockSize(A)) and (N <= 2 * ABLASComplexBlockSize(A))
  then
    begin

      //
      // base case
      //
      I := 0;
      while I <= M - 1 do
        begin
          i1_ := (JA) - (IB);
          for i_ := IB to IB + N - 1 do
            begin
              B[i_, JB + I] := A[IA + I, i_ + i1_];
            end;
          Inc(I);
        end;
    end
  else
    begin

      //
      // Cache-oblivious recursion
      //
      if M > N then
        begin
          ABLASComplexSplitLength(A, M, S1, S2);
          CMatrixTranspose(S1, N, A, IA, JA, B, IB, JB);
          CMatrixTranspose(S2, N, A, IA + S1, JA, B, IB, JB + S1);
        end
      else
        begin
          ABLASComplexSplitLength(A, N, S1, S2);
          CMatrixTranspose(M, S1, A, IA, JA, B, IB, JB);
          CMatrixTranspose(M, S2, A, IA, JA + S1, B, IB + S1, JB);
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
procedure RMatrixTranspose(M: TLInt; N: TLInt;
  const A: TLMatrix; IA: TLInt; JA: TLInt;
  var B: TLMatrix; IB: TLInt; JB: TLInt);
var
  I  : TLInt;
  S1 : TLInt;
  S2 : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if (M <= 2 * ABLASBlockSize(A)) and (N <= 2 * ABLASBlockSize(A)) then
    begin

      //
      // base case
      //
      I := 0;
      while I <= M - 1 do
        begin
          i1_ := (JA) - (IB);
          for i_ := IB to IB + N - 1 do
            begin
              B[i_, JB + I] := A[IA + I, i_ + i1_];
            end;
          Inc(I);
        end;
    end
  else
    begin

      //
      // Cache-oblivious recursion
      //
      if M > N then
        begin
          ABLASSplitLength(A, M, S1, S2);
          RMatrixTranspose(S1, N, A, IA, JA, B, IB, JB);
          RMatrixTranspose(S2, N, A, IA + S1, JA, B, IB, JB + S1);
        end
      else
        begin
          ABLASSplitLength(A, N, S1, S2);
          RMatrixTranspose(M, S1, A, IA, JA, B, IB, JB);
          RMatrixTranspose(M, S2, A, IA, JA + S1, B, IB + S1, JB);
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
procedure CMatrixCopy(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  var B: TLComplexMatrix; IB: TLInt; JB: TLInt);
var
  I  : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  I := 0;
  while I <= M - 1 do
    begin
      i1_ := (JA) - (JB);
      for i_ := JB to JB + N - 1 do
        begin
          B[IB + I, i_] := A[IA + I, i_ + i1_];
        end;
      Inc(I);
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
procedure RMatrixCopy(M: TLInt; N: TLInt; const A: TLMatrix;
  IA: TLInt; JA: TLInt; var B: TLMatrix; IB: TLInt;
  JB: TLInt);
var
  I: TLInt;
begin
  I := 0;
  while I <= M - 1 do
    begin
      APVMove(@B[IB + I][0], JB, JB + N - 1, @A[IA + I][0], JA, JA + N - 1);
      Inc(I);
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
procedure CMatrixRank1(M: TLInt; N: TLInt;
  var A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  var U: TLComplexVec; IU: TLInt; var v: TLComplexVec;
  IV: TLInt);
var
  I  : TLInt;
  S  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  if (M = 0) or (N = 0) then
      Exit;
  I := 0;
  while I <= M - 1 do
    begin
      S := U[IU + I];
      i1_ := (IV) - (JA);
      for i_ := JA to JA + N - 1 do
        begin
          A[IA + I, i_] := C_Add(A[IA + I, i_], C_Mul(S, v[i_ + i1_]));
        end;
      Inc(I);
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
procedure RMatrixRank1(M: TLInt; N: TLInt; var A: TLMatrix;
  IA: TLInt; JA: TLInt; var U: TLVec; IU: TLInt;
  var v: TLVec; IV: TLInt);
var
  I: TLInt;
  S: TLFloat;
begin
  if (M = 0) or (N = 0) then
      Exit;
  I := 0;
  while I <= M - 1 do
    begin
      S := U[IU + I];
      APVAdd(@A[IA + I][0], JA, JA + N - 1, @v[0], IV, IV + N - 1, S);
      Inc(I);
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
procedure CMatrixMV(M: TLInt; N: TLInt; var A: TLComplexMatrix;
  IA: TLInt; JA: TLInt; OpA: TLInt;
  var X: TLComplexVec; IX: TLInt; var Y: TLComplexVec;
  IY: TLInt);
var
  I  : TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  if M = 0 then
      Exit;
  if N = 0 then
    begin
      I := 0;
      while I <= M - 1 do
        begin
          Y[IY + I] := C_Complex(0);
          Inc(I);
        end;
      Exit;
    end;
  if OpA = 0 then
    begin

      //
      // y = A*x
      //
      I := 0;
      while I <= M - 1 do
        begin
          i1_ := (IX) - (JA);
          v := C_Complex(0.0);
          for i_ := JA to JA + N - 1 do
            begin
              v := C_Add(v, C_Mul(A[IA + I, i_], X[i_ + i1_]));
            end;
          Y[IY + I] := v;
          Inc(I);
        end;
      Exit;
    end;
  if OpA = 1 then
    begin

      //
      // y = A^T*x
      //
      I := 0;
      while I <= M - 1 do
        begin
          Y[IY + I] := C_Complex(0);
          Inc(I);
        end;
      I := 0;
      while I <= N - 1 do
        begin
          v := X[IX + I];
          i1_ := (JA) - (IY);
          for i_ := IY to IY + M - 1 do
            begin
              Y[i_] := C_Add(Y[i_], C_Mul(v, A[IA + I, i_ + i1_]));
            end;
          Inc(I);
        end;
      Exit;
    end;
  if OpA = 2 then
    begin

      //
      // y = A^H*x
      //
      I := 0;
      while I <= M - 1 do
        begin
          Y[IY + I] := C_Complex(0);
          Inc(I);
        end;
      I := 0;
      while I <= N - 1 do
        begin
          v := X[IX + I];
          i1_ := (JA) - (IY);
          for i_ := IY to IY + M - 1 do
            begin
              Y[i_] := C_Add(Y[i_], C_Mul(v, Conj(A[IA + I, i_ + i1_])));
            end;
          Inc(I);
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
procedure RMatrixMV(M: TLInt; N: TLInt; var A: TLMatrix;
  IA: TLInt; JA: TLInt; OpA: TLInt; var X: TLVec;
  IX: TLInt; var Y: TLVec; IY: TLInt);
var
  I: TLInt;
  v: TLFloat;
begin
  if M = 0 then
      Exit;
  if N = 0 then
    begin
      I := 0;
      while I <= M - 1 do
        begin
          Y[IY + I] := 0;
          Inc(I);
        end;
      Exit;
    end;
  if OpA = 0 then
    begin

      //
      // y = A*x
      //
      I := 0;
      while I <= M - 1 do
        begin
          v := APVDotProduct(@A[IA + I][0], JA, JA + N - 1, @X[0], IX, IX + N - 1);
          Y[IY + I] := v;
          Inc(I);
        end;
      Exit;
    end;
  if OpA = 1 then
    begin

      //
      // y = A^T*x
      //
      I := 0;
      while I <= M - 1 do
        begin
          Y[IY + I] := 0;
          Inc(I);
        end;
      I := 0;
      while I <= N - 1 do
        begin
          v := X[IX + I];
          APVAdd(@Y[0], IY, IY + M - 1, @A[IA + I][0], JA, JA + M - 1, v);
          Inc(I);
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
procedure CMatrixRightTRSM(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASComplexBlockSize(A);
  if (M <= BS) and (N <= BS) then
    begin
      CMatrixRightTRSM2(M, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      Exit;
    end;
  if M >= N then
    begin

      //
      // Split X: X*A = (X1 X2)^T*A
      //
      ABLASComplexSplitLength(A, M, S1, S2);
      CMatrixRightTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      CMatrixRightTRSM(S2, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2 + S1, J2);
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
      ABLASComplexSplitLength(A, N, S1, S2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1
          // X*A^-1 = (X1 X2)*(       )
          // (     A2)
          //
          CMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          CMatrixGEMM(M, S2, S1, C_Complex(-1.0), X, I2, J2, 0, A, I1, J1 + S1, 0,
            C_Complex(1.0), X, I2, J2 + S1);
          CMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1
          // X*A^-1 = (X1 X2)*(        )
          // (A12' A2')
          //
          CMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
          CMatrixGEMM(M, S1, S2, C_Complex(-1.0), X, I2, J2 + S1, 0, A, I1, J1 + S1,
            OpType, C_Complex(1.0), X, I2, J2);
          CMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1
          // X*A^-1 = (X1 X2)*(       )
          // (A21  A2)
          //
          CMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
          CMatrixGEMM(M, S1, S2, C_Complex(-1.0), X, I2, J2 + S1, 0, A, I1 + S1, J1,
            0, C_Complex(1.0), X, I2, J2);
          CMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1
          // X*A^-1 = (X1 X2)*(        )
          // (     A2')
          //
          CMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          CMatrixGEMM(M, S2, S1, C_Complex(-1.0), X, I2, J2, 0, A, I1 + S1, J1,
            OpType, C_Complex(1.0), X, I2, J2 + S1);
          CMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
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
procedure CMatrixLeftTRSM(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASComplexBlockSize(A);
  if (M <= BS) and (N <= BS) then
    begin
      CMatrixLeftTRSM2(M, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      Exit;
    end;
  if N >= M then
    begin

      //
      // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
      //
      ABLASComplexSplitLength(X, N, S1, S2);
      CMatrixLeftTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      CMatrixLeftTRSM(M, S2, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2 + S1);
    end
  else
    begin

      //
      // Split A
      //
      ABLASComplexSplitLength(A, M, S1, S2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1  ( X1 )
          // A^-1*X* = (       )   *(    )
          // (     A2)    ( X2 )
          //
          CMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          CMatrixGEMM(S1, N, S2, C_Complex(-1.0), A, I1, J1 + S1, 0, X, I2 + S1, J2,
            0, C_Complex(1.0), X, I2, J2);
          CMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (A12' A2')   ( X2 )
          //
          CMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          CMatrixGEMM(S2, N, S1, C_Complex(-1.0), A, I1, J1 + S1, OpType, X, I2, J2,
            0, C_Complex(1.0), X, I2 + S1, J2);
          CMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1 ( X1 )
          // A^-1*X = (       )  *(    )
          // (A21  A2)   ( X2 )
          //
          CMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          CMatrixGEMM(S2, N, S1, C_Complex(-1.0), A, I1 + S1, J1, 0, X, I2, J2, 0,
            C_Complex(1.0), X, I2 + S1, J2);
          CMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (     A2')   ( X2 )
          //
          CMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          CMatrixGEMM(S1, N, S2, C_Complex(-1.0), A, I1 + S1, J1, OpType, X,
            I2 + S1, J2, 0, C_Complex(1.0), X, I2, J2);
          CMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixRightTRSM, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixRightTRSM(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASBlockSize(A);
  if (M <= BS) and (N <= BS) then
    begin
      RMatrixRightTRSM2(M, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      Exit;
    end;
  if M >= N then
    begin

      //
      // Split X: X*A = (X1 X2)^T*A
      //
      ABLASSplitLength(A, M, S1, S2);
      RMatrixRightTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      RMatrixRightTRSM(S2, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2 + S1, J2);
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
      ABLASSplitLength(A, N, S1, S2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1
          // X*A^-1 = (X1 X2)*(       )
          // (     A2)
          //
          RMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          RMatrixGEMM(M, S2, S1, -1.0, X, I2, J2, 0, A, I1, J1 + S1, 0, 1.0, X,
            I2, J2 + S1);
          RMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1
          // X*A^-1 = (X1 X2)*(        )
          // (A12' A2')
          //
          RMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
          RMatrixGEMM(M, S1, S2, -1.0, X, I2, J2 + S1, 0, A, I1, J1 + S1, OpType,
            1.0, X, I2, J2);
          RMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1
          // X*A^-1 = (X1 X2)*(       )
          // (A21  A2)
          //
          RMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
          RMatrixGEMM(M, S1, S2, -1.0, X, I2, J2 + S1, 0, A, I1 + S1, J1, 0, 1.0,
            X, I2, J2);
          RMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1
          // X*A^-1 = (X1 X2)*(        )
          // (     A2')
          //
          RMatrixRightTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          RMatrixGEMM(M, S2, S1, -1.0, X, I2, J2, 0, A, I1 + S1, J1, OpType, 1.0, X,
            I2, J2 + S1);
          RMatrixRightTRSM(M, S2, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2, J2 + S1);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixLeftTRSM, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixLeftTRSM(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASBlockSize(A);
  if (M <= BS) and (N <= BS) then
    begin
      RMatrixLeftTRSM2(M, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      Exit;
    end;
  if N >= M then
    begin

      //
      // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
      //
      ABLASSplitLength(X, N, S1, S2);
      RMatrixLeftTRSM(M, S1, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
      RMatrixLeftTRSM(M, S2, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2 + S1);
    end
  else
    begin

      //
      // Split A
      //
      ABLASSplitLength(A, M, S1, S2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1  ( X1 )
          // A^-1*X* = (       )   *(    )
          // (     A2)    ( X2 )
          //
          RMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          RMatrixGEMM(S1, N, S2, -1.0, A, I1, J1 + S1, 0, X, I2 + S1, J2, 0, 1.0,
            X, I2, J2);
          RMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (A12' A2')   ( X2 )
          //
          RMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          RMatrixGEMM(S2, N, S1, -1.0, A, I1, J1 + S1, OpType, X, I2, J2, 0, 1.0, X,
            I2 + S1, J2);
          RMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1 ( X1 )
          // A^-1*X = (       )  *(    )
          // (A21  A2)   ( X2 )
          //
          RMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
          RMatrixGEMM(S2, N, S1, -1.0, A, I1 + S1, J1, 0, X, I2, J2, 0, 1.0, X,
            I2 + S1, J2);
          RMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1 ( X1 )
          // A^-1*X = (        )  *(    )
          // (     A2')   ( X2 )
          //
          RMatrixLeftTRSM(S2, N, A, I1 + S1, J1 + S1, IsUpper, IsUnit, OpType, X,
            I2 + S1, J2);
          RMatrixGEMM(S1, N, S2, -1.0, A, I1 + S1, J1, OpType, X, I2 + S1, J2, 0,
            1.0, X, I2, J2);
          RMatrixLeftTRSM(S1, N, A, I1, J1, IsUpper, IsUnit, OpType, X, I2, J2);
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
procedure CMatrixSYRK(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASComplexBlockSize(A);
  if (N <= BS) and (k <= BS) then
    begin
      CMatrixSYRK2(N, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
      Exit;
    end;
  if k >= N then
    begin

      //
      // Split K
      //
      ABLASComplexSplitLength(A, k, S1, S2);
      if OpTypeA = 0 then
        begin
          CMatrixSYRK(N, S1, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          CMatrixSYRK(N, S2, Alpha, A, IA, JA + S1, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end
      else
        begin
          CMatrixSYRK(N, S1, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          CMatrixSYRK(N, S2, Alpha, A, IA + S1, JA, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end;
    end
  else
    begin

      //
      // Split N
      //
      ABLASComplexSplitLength(A, N, S1, S2);
      if (OpTypeA = 0) and IsUpper then
        begin
          CMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          CMatrixGEMM(S1, S2, k, C_Complex(Alpha), A, IA, JA, 0, A, IA + S1, JA, 2,
            C_Complex(Beta), C, IC, JC + S1);
          CMatrixSYRK(S2, k, Alpha, A, IA + S1, JA, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
          Exit;
        end;
      if (OpTypeA = 0) and not IsUpper then
        begin
          CMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          CMatrixGEMM(S2, S1, k, C_Complex(Alpha), A, IA + S1, JA, 0, A, IA, JA, 2,
            C_Complex(Beta), C, IC + S1, JC);
          CMatrixSYRK(S2, k, Alpha, A, IA + S1, JA, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and IsUpper then
        begin
          CMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          CMatrixGEMM(S1, S2, k, C_Complex(Alpha), A, IA, JA, 2, A, IA, JA + S1, 0,
            C_Complex(Beta), C, IC, JC + S1);
          CMatrixSYRK(S2, k, Alpha, A, IA, JA + S1, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and not IsUpper then
        begin
          CMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          CMatrixGEMM(S2, S1, k, C_Complex(Alpha), A, IA, JA + S1, 2, A, IA, JA, 0,
            C_Complex(Beta), C, IC + S1, JC);
          CMatrixSYRK(S2, k, Alpha, A, IA, JA + S1, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixSYRK, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixSYRK(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASBlockSize(A);
  if (N <= BS) and (k <= BS) then
    begin
      RMatrixSYRK2(N, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
      Exit;
    end;
  if k >= N then
    begin

      //
      // Split K
      //
      ABLASSplitLength(A, k, S1, S2);
      if OpTypeA = 0 then
        begin
          RMatrixSYRK(N, S1, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          RMatrixSYRK(N, S2, Alpha, A, IA, JA + S1, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end
      else
        begin
          RMatrixSYRK(N, S1, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          RMatrixSYRK(N, S2, Alpha, A, IA + S1, JA, OpTypeA, 1.0, C, IC,
            JC, IsUpper);
        end;
    end
  else
    begin

      //
      // Split N
      //
      ABLASSplitLength(A, N, S1, S2);
      if (OpTypeA = 0) and IsUpper then
        begin
          RMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          RMatrixGEMM(S1, S2, k, Alpha, A, IA, JA, 0, A, IA + S1, JA, 1, Beta, C,
            IC, JC + S1);
          RMatrixSYRK(S2, k, Alpha, A, IA + S1, JA, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
          Exit;
        end;
      if (OpTypeA = 0) and not IsUpper then
        begin
          RMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          RMatrixGEMM(S2, S1, k, Alpha, A, IA + S1, JA, 0, A, IA, JA, 1, Beta, C,
            IC + S1, JC);
          RMatrixSYRK(S2, k, Alpha, A, IA + S1, JA, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and IsUpper then
        begin
          RMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          RMatrixGEMM(S1, S2, k, Alpha, A, IA, JA, 1, A, IA, JA + S1, 0, Beta, C,
            IC, JC + S1);
          RMatrixSYRK(S2, k, Alpha, A, IA, JA + S1, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and not IsUpper then
        begin
          RMatrixSYRK(S1, k, Alpha, A, IA, JA, OpTypeA, Beta, C, IC, JC, IsUpper);
          RMatrixGEMM(S2, S1, k, Alpha, A, IA, JA + S1, 1, A, IA, JA, 0, Beta, C,
            IC + S1, JC);
          RMatrixSYRK(S2, k, Alpha, A, IA, JA + S1, OpTypeA, Beta, C, IC + S1,
            JC + S1, IsUpper);
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
procedure CMatrixGEMM(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; Beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASComplexBlockSize(A);
  if (M <= BS) and (N <= BS) and (k <= BS) then
    begin
      CMatrixGEMMK(M, N, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
        C, IC, JC);
      Exit;
    end;
  if (M >= N) and (M >= k) then
    begin

      //
      // A*B = (A1 A2)^T*B
      //
      ABLASComplexSplitLength(A, M, S1, S2);
      CMatrixGEMM(S1, N, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
        C, IC, JC);
      if OpTypeA = 0 then
        begin
          CMatrixGEMM(S2, N, k, Alpha, A, IA + S1, JA, OpTypeA, B, IB, JB, OpTypeB,
            Beta, C, IC + S1, JC);
        end
      else
        begin
          CMatrixGEMM(S2, N, k, Alpha, A, IA, JA + S1, OpTypeA, B, IB, JB, OpTypeB,
            Beta, C, IC + S1, JC);
        end;
      Exit;
    end;
  if (N >= M) and (N >= k) then
    begin

      //
      // A*B = A*(B1 B2)
      //
      ABLASComplexSplitLength(A, N, S1, S2);
      if OpTypeB = 0 then
        begin
          CMatrixGEMM(M, S1, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          CMatrixGEMM(M, S2, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB + S1, OpTypeB,
            Beta, C, IC, JC + S1);
        end
      else
        begin
          CMatrixGEMM(M, S1, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          CMatrixGEMM(M, S2, k, Alpha, A, IA, JA, OpTypeA, B, IB + S1, JB, OpTypeB,
            Beta, C, IC, JC + S1);
        end;
      Exit;
    end;
  if (k >= M) and (k >= N) then
    begin

      //
      // A*B = (A1 A2)*(B1 B2)^T
      //
      ABLASComplexSplitLength(A, k, S1, S2);
      if (OpTypeA = 0) and (OpTypeB = 0) then
        begin
          CMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          CMatrixGEMM(M, N, S2, Alpha, A, IA, JA + S1, OpTypeA, B, IB + S1, JB,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      if (OpTypeA = 0) and (OpTypeB <> 0) then
        begin
          CMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          CMatrixGEMM(M, N, S2, Alpha, A, IA, JA + S1, OpTypeA, B, IB, JB + S1,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB = 0) then
        begin
          CMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          CMatrixGEMM(M, N, S2, Alpha, A, IA + S1, JA, OpTypeA, B, IB + S1, JB,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB <> 0) then
        begin
          CMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          CMatrixGEMM(M, N, S2, Alpha, A, IA + S1, JA, OpTypeA, B, IB, JB + S1,
            OpTypeB, C_Complex(1.0), C, IC, JC);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Same as CMatrixGEMM, but for real numbers.
  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixGEMM(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  Beta: TLFloat; var C: TLMatrix; IC: TLInt; JC: TLInt);
var
  S1: TLInt;
  S2: TLInt;
  BS: TLInt;
begin
  BS := ABLASBlockSize(A);
  if (M <= BS) and (N <= BS) and (k <= BS) then
    begin
      RMatrixGEMMK(M, N, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
        C, IC, JC);
      Exit;
    end;
  if (M >= N) and (M >= k) then
    begin

      //
      // A*B = (A1 A2)^T*B
      //
      ABLASSplitLength(A, M, S1, S2);
      if OpTypeA = 0 then
        begin
          RMatrixGEMM(S1, N, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(S2, N, k, Alpha, A, IA + S1, JA, OpTypeA, B, IB, JB, OpTypeB,
            Beta, C, IC + S1, JC);
        end
      else
        begin
          RMatrixGEMM(S1, N, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(S2, N, k, Alpha, A, IA, JA + S1, OpTypeA, B, IB, JB, OpTypeB,
            Beta, C, IC + S1, JC);
        end;
      Exit;
    end;
  if (N >= M) and (N >= k) then
    begin

      //
      // A*B = A*(B1 B2)
      //
      ABLASSplitLength(A, N, S1, S2);
      if OpTypeB = 0 then
        begin
          RMatrixGEMM(M, S1, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(M, S2, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB + S1, OpTypeB,
            Beta, C, IC, JC + S1);
        end
      else
        begin
          RMatrixGEMM(M, S1, k, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(M, S2, k, Alpha, A, IA, JA, OpTypeA, B, IB + S1, JB, OpTypeB,
            Beta, C, IC, JC + S1);
        end;
      Exit;
    end;
  if (k >= M) and (k >= N) then
    begin

      //
      // A*B = (A1 A2)*(B1 B2)^T
      //
      ABLASSplitLength(A, k, S1, S2);
      if (OpTypeA = 0) and (OpTypeB = 0) then
        begin
          RMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(M, N, S2, Alpha, A, IA, JA + S1, OpTypeA, B, IB + S1, JB,
            OpTypeB, 1.0, C, IC, JC);
        end;
      if (OpTypeA = 0) and (OpTypeB <> 0) then
        begin
          RMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(M, N, S2, Alpha, A, IA, JA + S1, OpTypeA, B, IB, JB + S1,
            OpTypeB, 1.0, C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB = 0) then
        begin
          RMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(M, N, S2, Alpha, A, IA + S1, JA, OpTypeA, B, IB + S1, JB,
            OpTypeB, 1.0, C, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB <> 0) then
        begin
          RMatrixGEMM(M, N, S1, Alpha, A, IA, JA, OpTypeA, B, IB, JB, OpTypeB, Beta,
            C, IC, JC);
          RMatrixGEMM(M, N, S2, Alpha, A, IA + S1, JA, OpTypeA, B, IB, JB + S1,
            OpTypeB, 1.0, C, IC, JC);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  TLComplex ABLASSplitLength
  ************************************************************************ *)
procedure ABLASInternalSplitLength(N: TLInt; NB: TLInt;
  var N1: TLInt; var N2: TLInt);
var
  R: TLInt;
begin
  if N <= NB then
    begin

      //
      // Block size, no further splitting
      //
      N1 := N;
      N2 := 0;
    end
  else
    begin

      //
      // Greater than block size
      //
      if N mod NB <> 0 then
        begin

          //
          // Split remainder
          //
          N2 := N mod NB;
          N1 := N - N2;
        end
      else
        begin

          //
          // Split on block boundaries
          //
          N2 := N div 2;
          N1 := N - N2;
          if N1 mod NB = 0 then
              Exit;
          R := NB - N1 mod NB;
          N1 := N1 + R;
          N2 := N2 - R;
        end;
    end;
end;

(* ************************************************************************
  Level 2 variant of CMatrixRightTRSM
  ************************************************************************ *)
procedure CMatrixRightTRSM2(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt);
var
  I  : TLInt;
  J  : TLInt;
  VC : TLComplex;
  VD : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // Special case
  //
  if N * M = 0 then
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
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  if IsUnit then
                    begin
                      VD := C_Complex(1);
                    end
                  else
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := C_Div(X[I2 + I, J2 + J], VD);
                  if J < N - 1 then
                    begin
                      VC := X[I2 + I, J2 + J];
                      i1_ := (J1 + J + 1) - (J2 + J + 1);
                      for i_ := J2 + J + 1 to J2 + N - 1 do
                        begin
                          X[I2 + I, i_] := C_Sub(X[I2 + I, i_],
                            C_Mul(VC, A[I1 + J, i_ + i1_]));
                        end;
                    end;
                  Inc(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          I := 0;
          while I <= M - 1 do
            begin
              J := N - 1;
              while J >= 0 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J < N - 1 then
                    begin
                      i1_ := (J1 + J + 1) - (J2 + J + 1);
                      VC := C_Complex(0.0);
                      for i_ := J2 + J + 1 to J2 + N - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[I2 + I, i_], A[I1 + J, i_ + i1_]));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := C_Div(C_Sub(X[I2 + I, J2 + J], VC), VD);
                  Dec(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // X*A^(-H)
          //
          I := 0;
          while I <= M - 1 do
            begin
              J := N - 1;
              while J >= 0 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J < N - 1 then
                    begin
                      i1_ := (J1 + J + 1) - (J2 + J + 1);
                      VC := C_Complex(0.0);
                      for i_ := J2 + J + 1 to J2 + N - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[I2 + I, i_], Conj(A[I1 + J, i_ + i1_])));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := Conj(A[I1 + J, J1 + J]);
                    end;
                  X[I2 + I, J2 + J] := C_Div(C_Sub(X[I2 + I, J2 + J], VC), VD);
                  Dec(J);
                end;
              Inc(I);
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
          I := 0;
          while I <= M - 1 do
            begin
              J := N - 1;
              while J >= 0 do
                begin
                  if IsUnit then
                    begin
                      VD := C_Complex(1);
                    end
                  else
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := C_Div(X[I2 + I, J2 + J], VD);
                  if J > 0 then
                    begin
                      VC := X[I2 + I, J2 + J];
                      i1_ := (J1) - (J2);
                      for i_ := J2 to J2 + J - 1 do
                        begin
                          X[I2 + I, i_] := C_Sub(X[I2 + I, i_],
                            C_Mul(VC, A[I1 + J, i_ + i1_]));
                        end;
                    end;
                  Dec(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J > 0 then
                    begin
                      i1_ := (J1) - (J2);
                      VC := C_Complex(0.0);
                      for i_ := J2 to J2 + J - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[I2 + I, i_], A[I1 + J, i_ + i1_]));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := C_Div(C_Sub(X[I2 + I, J2 + J], VC), VD);
                  Inc(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // X*A^(-H)
          //
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if J > 0 then
                    begin
                      i1_ := (J1) - (J2);
                      VC := C_Complex(0.0);
                      for i_ := J2 to J2 + J - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(X[I2 + I, i_], Conj(A[I1 + J, i_ + i1_])));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := Conj(A[I1 + J, J1 + J]);
                    end;
                  X[I2 + I, J2 + J] := C_Div(C_Sub(X[I2 + I, J2 + J], VC), VD);
                  Inc(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level-2 subroutine
  ************************************************************************ *)
procedure CMatrixLeftTRSM2(M: TLInt; N: TLInt;
  const A: TLComplexMatrix; I1: TLInt; J1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; I2: TLInt; J2: TLInt);
var
  I : TLInt;
  J : TLInt;
  VC: TLComplex;
  VD: TLComplex;
  i_: TLInt;
begin

  //
  // Special case
  //
  if N * M = 0 then
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
          I := M - 1;
          while I >= 0 do
            begin
              J := I + 1;
              while J <= M - 1 do
                begin
                  VC := A[I1 + I, J1 + J];
                  for i_ := J2 to J2 + N - 1 do
                    begin
                      X[I2 + I, i_] := C_Sub(X[I2 + I, i_], C_Mul(VC, X[I2 + J, i_]));
                    end;
                  Inc(J);
                end;
              if not IsUnit then
                begin
                  VD := C_RDiv(1, A[I1 + I, J1 + I]);
                  for i_ := J2 to J2 + N - 1 do
                    begin
                      X[I2 + I, i_] := C_Mul(VD, X[I2 + I, i_]);
                    end;
                end;
              Dec(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          I := 0;
          while I <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, A[I1 + I, J1 + I]);
                end;
              for i_ := J2 to J2 + N - 1 do
                begin
                  X[I2 + I, i_] := C_Mul(VD, X[I2 + I, i_]);
                end;
              J := I + 1;
              while J <= M - 1 do
                begin
                  VC := A[I1 + I, J1 + J];
                  for i_ := J2 to J2 + N - 1 do
                    begin
                      X[I2 + J, i_] := C_Sub(X[I2 + J, i_], C_Mul(VC, X[I2 + I, i_]));
                    end;
                  Inc(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // A^(-H)*X
          //
          I := 0;
          while I <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, Conj(A[I1 + I, J1 + I]));
                end;
              for i_ := J2 to J2 + N - 1 do
                begin
                  X[I2 + I, i_] := C_Mul(VD, X[I2 + I, i_]);
                end;
              J := I + 1;
              while J <= M - 1 do
                begin
                  VC := Conj(A[I1 + I, J1 + J]);
                  for i_ := J2 to J2 + N - 1 do
                    begin
                      X[I2 + J, i_] := C_Sub(X[I2 + J, i_], C_Mul(VC, X[I2 + I, i_]));
                    end;
                  Inc(J);
                end;
              Inc(I);
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
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= I - 1 do
                begin
                  VC := A[I1 + I, J1 + J];
                  for i_ := J2 to J2 + N - 1 do
                    begin
                      X[I2 + I, i_] := C_Sub(X[I2 + I, i_], C_Mul(VC, X[I2 + J, i_]));
                    end;
                  Inc(J);
                end;
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, A[I1 + J, J1 + J]);
                end;
              for i_ := J2 to J2 + N - 1 do
                begin
                  X[I2 + I, i_] := C_Mul(VD, X[I2 + I, i_]);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          I := M - 1;
          while I >= 0 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, A[I1 + I, J1 + I]);
                end;
              for i_ := J2 to J2 + N - 1 do
                begin
                  X[I2 + I, i_] := C_Mul(VD, X[I2 + I, i_]);
                end;
              J := I - 1;
              while J >= 0 do
                begin
                  VC := A[I1 + I, J1 + J];
                  for i_ := J2 to J2 + N - 1 do
                    begin
                      X[I2 + J, i_] := C_Sub(X[I2 + J, i_], C_Mul(VC, X[I2 + I, i_]));
                    end;
                  Dec(J);
                end;
              Dec(I);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // A^(-H)*X
          //
          I := M - 1;
          while I >= 0 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, Conj(A[I1 + I, J1 + I]));
                end;
              for i_ := J2 to J2 + N - 1 do
                begin
                  X[I2 + I, i_] := C_Mul(VD, X[I2 + I, i_]);
                end;
              J := I - 1;
              while J >= 0 do
                begin
                  VC := Conj(A[I1 + I, J1 + J]);
                  for i_ := J2 to J2 + N - 1 do
                    begin
                      X[I2 + J, i_] := C_Sub(X[I2 + J, i_], C_Mul(VC, X[I2 + I, i_]));
                    end;
                  Dec(J);
                end;
              Dec(I);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure RMatrixRightTRSM2(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt);
var
  I : TLInt;
  J : TLInt;
  VR: TLFloat;
  VD: TLFloat;
begin

  //
  // Special case
  //
  if N * M = 0 then
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
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  if IsUnit then
                    begin
                      VD := 1;
                    end
                  else
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := X[I2 + I, J2 + J] / VD;
                  if J < N - 1 then
                    begin
                      VR := X[I2 + I, J2 + J];
                      APVSub(@X[I2 + I][0], J2 + J + 1, J2 + N - 1, @A[I1 + J][0],
                        J1 + J + 1, J1 + N - 1, VR);
                    end;
                  Inc(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          I := 0;
          while I <= M - 1 do
            begin
              J := N - 1;
              while J >= 0 do
                begin
                  VR := 0;
                  VD := 1;
                  if J < N - 1 then
                    begin
                      VR := APVDotProduct(@X[I2 + I][0], J2 + J + 1, J2 + N - 1,
                        @A[I1 + J][0], J1 + J + 1, J1 + N - 1);
                    end;
                  if not IsUnit then
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := (X[I2 + I, J2 + J] - VR) / VD;
                  Dec(J);
                end;
              Inc(I);
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
          I := 0;
          while I <= M - 1 do
            begin
              J := N - 1;
              while J >= 0 do
                begin
                  if IsUnit then
                    begin
                      VD := 1;
                    end
                  else
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := X[I2 + I, J2 + J] / VD;
                  if J > 0 then
                    begin
                      VR := X[I2 + I, J2 + J];
                      APVSub(@X[I2 + I][0], J2, J2 + J - 1, @A[I1 + J][0], J1,
                        J1 + J - 1, VR);
                    end;
                  Dec(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  VR := 0;
                  VD := 1;
                  if J > 0 then
                    begin
                      VR := APVDotProduct(@X[I2 + I][0], J2, J2 + J - 1, @A[I1 + J][0],
                        J1, J1 + J - 1);
                    end;
                  if not IsUnit then
                    begin
                      VD := A[I1 + J, J1 + J];
                    end;
                  X[I2 + I, J2 + J] := (X[I2 + I, J2 + J] - VR) / VD;
                  Inc(J);
                end;
              Inc(I);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure RMatrixLeftTRSM2(M: TLInt; N: TLInt;
  const A: TLMatrix; I1: TLInt; J1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix;
  I2: TLInt; J2: TLInt);
var
  I : TLInt;
  J : TLInt;
  VR: TLFloat;
  VD: TLFloat;
begin

  //
  // Special case
  //
  if N * M = 0 then
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
          I := M - 1;
          while I >= 0 do
            begin
              J := I + 1;
              while J <= M - 1 do
                begin
                  VR := A[I1 + I, J1 + J];
                  APVSub(@X[I2 + I][0], J2, J2 + N - 1, @X[I2 + J][0], J2,
                    J2 + N - 1, VR);
                  Inc(J);
                end;
              if not IsUnit then
                begin
                  VD := 1 / A[I1 + I, J1 + I];
                  APVMul(@X[I2 + I][0], J2, J2 + N - 1, VD);
                end;
              Dec(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          I := 0;
          while I <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / A[I1 + I, J1 + I];
                end;
              APVMul(@X[I2 + I][0], J2, J2 + N - 1, VD);
              J := I + 1;
              while J <= M - 1 do
                begin
                  VR := A[I1 + I, J1 + J];
                  APVSub(@X[I2 + J][0], J2, J2 + N - 1, @X[I2 + I][0], J2,
                    J2 + N - 1, VR);
                  Inc(J);
                end;
              Inc(I);
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
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= I - 1 do
                begin
                  VR := A[I1 + I, J1 + J];
                  APVSub(@X[I2 + I][0], J2, J2 + N - 1, @X[I2 + J][0], J2,
                    J2 + N - 1, VR);
                  Inc(J);
                end;
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / A[I1 + J, J1 + J];
                end;
              APVMul(@X[I2 + I][0], J2, J2 + N - 1, VD);
              Inc(I);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          I := M - 1;
          while I >= 0 do
            begin
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / A[I1 + I, J1 + I];
                end;
              APVMul(@X[I2 + I][0], J2, J2 + N - 1, VD);
              J := I - 1;
              while J >= 0 do
                begin
                  VR := A[I1 + I, J1 + J];
                  APVSub(@X[I2 + J][0], J2, J2 + N - 1, @X[I2 + I][0], J2,
                    J2 + N - 1, VR);
                  Dec(J);
                end;
              Dec(I);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure CMatrixSYRK2(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLComplexMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  I  : TLInt;
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
  if (AP_FP_Eq(Alpha, 0) or (k = 0)) and AP_FP_Eq(Beta, 1) then
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
      I := 0;
      while I <= N - 1 do
        begin
          if IsUpper then
            begin
              J1 := I;
              J2 := N - 1;
            end
          else
            begin
              J1 := 0;
              J2 := I;
            end;
          J := J1;
          while J <= J2 do
            begin
              if AP_FP_Neq(Alpha, 0) and (k > 0) then
                begin
                  v := C_Complex(0.0);
                  for i_ := JA to JA + k - 1 do
                    begin
                      v := C_Add(v, C_Mul(A[IA + I, i_], Conj(A[IA + J, i_])));
                    end;
                end
              else
                begin
                  v := C_Complex(0);
                end;
              if AP_FP_Eq(Beta, 0) then
                begin
                  C[IC + I, JC + J] := C_MulR(v, Alpha);
                end
              else
                begin
                  C[IC + I, JC + J] := C_Add(C_MulR(C[IC + I, JC + J], Beta),
                    C_MulR(v, Alpha));
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end
  else
    begin

      //
      // C=alpha*A^H*A+beta*C
      //
      I := 0;
      while I <= N - 1 do
        begin
          if IsUpper then
            begin
              J1 := I;
              J2 := N - 1;
            end
          else
            begin
              J1 := 0;
              J2 := I;
            end;
          if AP_FP_Eq(Beta, 0) then
            begin
              J := J1;
              while J <= J2 do
                begin
                  C[IC + I, JC + J] := C_Complex(0);
                  Inc(J);
                end;
            end
          else
            begin
              for i_ := JC + J1 to JC + J2 do
                begin
                  C[IC + I, i_] := C_MulR(C[IC + I, i_], Beta);
                end;
            end;
          Inc(I);
        end;
      I := 0;
      while I <= k - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              if IsUpper then
                begin
                  J1 := J;
                  J2 := N - 1;
                end
              else
                begin
                  J1 := 0;
                  J2 := J;
                end;
              v := C_MulR(Conj(A[IA + I, JA + J]), Alpha);
              i1_ := (JA + J1) - (JC + J1);
              for i_ := JC + J1 to JC + J2 do
                begin
                  C[IC + J, i_] := C_Add(C[IC + J, i_], C_Mul(v, A[IA + I, i_ + i1_]));
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Level 2 subrotuine
  ************************************************************************ *)
procedure RMatrixSYRK2(N: TLInt; k: TLInt; Alpha: TLFloat;
  const A: TLMatrix; IA: TLInt; JA: TLInt;
  OpTypeA: TLInt; Beta: TLFloat; var C: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  I : TLInt;
  J : TLInt;
  J1: TLInt;
  J2: TLInt;
  v : TLFloat;
begin

  //
  // Fast exit (nothing to be done)
  //
  if (AP_FP_Eq(Alpha, 0) or (k = 0)) and AP_FP_Eq(Beta, 1) then
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
      I := 0;
      while I <= N - 1 do
        begin
          if IsUpper then
            begin
              J1 := I;
              J2 := N - 1;
            end
          else
            begin
              J1 := 0;
              J2 := I;
            end;
          J := J1;
          while J <= J2 do
            begin
              if AP_FP_Neq(Alpha, 0) and (k > 0) then
                begin
                  v := APVDotProduct(@A[IA + I][0], JA, JA + k - 1, @A[IA + J][0],
                    JA, JA + k - 1);
                end
              else
                begin
                  v := 0;
                end;
              if AP_FP_Eq(Beta, 0) then
                begin
                  C[IC + I, JC + J] := Alpha * v;
                end
              else
                begin
                  C[IC + I, JC + J] := Beta * C[IC + I, JC + J] + Alpha * v;
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end
  else
    begin

      //
      // C=alpha*A^H*A+beta*C
      //
      I := 0;
      while I <= N - 1 do
        begin
          if IsUpper then
            begin
              J1 := I;
              J2 := N - 1;
            end
          else
            begin
              J1 := 0;
              J2 := I;
            end;
          if AP_FP_Eq(Beta, 0) then
            begin
              J := J1;
              while J <= J2 do
                begin
                  C[IC + I, JC + J] := 0;
                  Inc(J);
                end;
            end
          else
            begin
              APVMul(@C[IC + I][0], JC + J1, JC + J2, Beta);
            end;
          Inc(I);
        end;
      I := 0;
      while I <= k - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              if IsUpper then
                begin
                  J1 := J;
                  J2 := N - 1;
                end
              else
                begin
                  J1 := 0;
                  J2 := J;
                end;
              v := Alpha * A[IA + I, JA + J];
              APVAdd(@C[IC + J][0], JC + J1, JC + J2, @A[IA + I][0], JA + J1,
                JA + J2, v);
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  GEMM kernel
  ************************************************************************ *)
procedure CMatrixGEMMK(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLComplex; const A: TLComplexMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; Beta: TLComplex;
  var C: TLComplexMatrix; IC: TLInt; JC: TLInt);
var
  I  : TLInt;
  J  : TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // Special case
  //
  if M * N = 0 then
    begin
      Exit;
    end;

  //
  // Another special case
  //
  if k = 0 then
    begin
      if C_NotEqualR(Beta, 0) then
        begin
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  C[IC + I, JC + J] := C_Mul(Beta, C[IC + I, JC + J]);
                  Inc(J);
                end;
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  C[IC + I, JC + J] := C_Complex(0);
                  Inc(J);
                end;
              Inc(I);
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
      I := 0;
      while I <= M - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              if (k = 0) or C_EqualR(Alpha, 0) then
                begin
                  v := C_Complex(0);
                end
              else
                begin
                  if OpTypeB = 1 then
                    begin
                      i1_ := (JB) - (JA);
                      v := C_Complex(0.0);
                      for i_ := JA to JA + k - 1 do
                        begin
                          v := C_Add(v, C_Mul(A[IA + I, i_], B[IB + J, i_ + i1_]));
                        end;
                    end
                  else
                    begin
                      i1_ := (JB) - (JA);
                      v := C_Complex(0.0);
                      for i_ := JA to JA + k - 1 do
                        begin
                          v := C_Add(v, C_Mul(A[IA + I, i_], Conj(B[IB + J, i_ + i1_])));
                        end;
                    end;
                end;
              if C_EqualR(Beta, 0) then
                begin
                  C[IC + I, JC + J] := C_Mul(Alpha, v);
                end
              else
                begin
                  C[IC + I, JC + J] := C_Add(C_Mul(Beta, C[IC + I, JC + J]),
                    C_Mul(Alpha, v));
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end;
  if (OpTypeA = 0) and (OpTypeB = 0) then
    begin

      //
      // A*B
      //
      I := 0;
      while I <= M - 1 do
        begin
          if C_NotEqualR(Beta, 0) then
            begin
              for i_ := JC to JC + N - 1 do
                begin
                  C[IC + I, i_] := C_Mul(Beta, C[IC + I, i_]);
                end;
            end
          else
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  C[IC + I, JC + J] := C_Complex(0);
                  Inc(J);
                end;
            end;
          if C_NotEqualR(Alpha, 0) then
            begin
              J := 0;
              while J <= k - 1 do
                begin
                  v := C_Mul(Alpha, A[IA + I, JA + J]);
                  i1_ := (JB) - (JC);
                  for i_ := JC to JC + N - 1 do
                    begin
                      C[IC + I, i_] := C_Add(C[IC + I, i_],
                        C_Mul(v, B[IB + J, i_ + i1_]));
                    end;
                  Inc(J);
                end;
            end;
          Inc(I);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB <> 0) then
    begin

      //
      // A'*B'
      //
      I := 0;
      while I <= M - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              if C_EqualR(Alpha, 0) then
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
                              v := C_Add(v, C_Mul(A[i_, JA + I], B[IB + J, i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(A[i_, JA + I], Conj(B[IB + J, i_ + i1_])));
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
                              v := C_Add(v, C_Mul(Conj(A[i_, JA + I]), B[IB + J, i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(Conj(A[i_, JA + I]),
                                Conj(B[IB + J, i_ + i1_])));
                            end;
                        end;
                    end;
                end;
              if C_EqualR(Beta, 0) then
                begin
                  C[IC + I, JC + J] := C_Mul(Alpha, v);
                end
              else
                begin
                  C[IC + I, JC + J] := C_Add(C_Mul(Beta, C[IC + I, JC + J]),
                    C_Mul(Alpha, v));
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB = 0) then
    begin

      //
      // A'*B
      //
      if C_EqualR(Beta, 0) then
        begin
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  C[IC + I, JC + J] := C_Complex(0);
                  Inc(J);
                end;
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= M - 1 do
            begin
              for i_ := JC to JC + N - 1 do
                begin
                  C[IC + I, i_] := C_Mul(Beta, C[IC + I, i_]);
                end;
              Inc(I);
            end;
        end;
      if C_NotEqualR(Alpha, 0) then
        begin
          J := 0;
          while J <= k - 1 do
            begin
              I := 0;
              while I <= M - 1 do
                begin
                  if OpTypeA = 1 then
                    begin
                      v := C_Mul(Alpha, A[IA + J, JA + I]);
                    end
                  else
                    begin
                      v := C_Mul(Alpha, Conj(A[IA + J, JA + I]));
                    end;
                  i1_ := (JB) - (JC);
                  for i_ := JC to JC + N - 1 do
                    begin
                      C[IC + I, i_] := C_Add(C[IC + I, i_],
                        C_Mul(v, B[IB + J, i_ + i1_]));
                    end;
                  Inc(I);
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
procedure RMatrixGEMMK(M: TLInt; N: TLInt; k: TLInt;
  Alpha: TLFloat; const A: TLMatrix; IA: TLInt;
  JA: TLInt; OpTypeA: TLInt; const B: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  Beta: TLFloat; var C: TLMatrix; IC: TLInt; JC: TLInt);
var
  I  : TLInt;
  J  : TLInt;
  v  : TLFloat;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // if matrix size is zero
  //
  if M * N = 0 then
    begin
      Exit;
    end;

  //
  // if K=0, then C=Beta*C
  //
  if k = 0 then
    begin
      if AP_FP_Neq(Beta, 1) then
        begin
          if AP_FP_Neq(Beta, 0) then
            begin
              I := 0;
              while I <= M - 1 do
                begin
                  J := 0;
                  while J <= N - 1 do
                    begin
                      C[IC + I, JC + J] := Beta * C[IC + I, JC + J];
                      Inc(J);
                    end;
                  Inc(I);
                end;
            end
          else
            begin
              I := 0;
              while I <= M - 1 do
                begin
                  J := 0;
                  while J <= N - 1 do
                    begin
                      C[IC + I, JC + J] := 0;
                      Inc(J);
                    end;
                  Inc(I);
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
      I := 0;
      while I <= M - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              if (k = 0) or AP_FP_Eq(Alpha, 0) then
                begin
                  v := 0;
                end
              else
                begin
                  v := APVDotProduct(@A[IA + I][0], JA, JA + k - 1, @B[IB + J][0],
                    JB, JB + k - 1);
                end;
              if AP_FP_Eq(Beta, 0) then
                begin
                  C[IC + I, JC + J] := Alpha * v;
                end
              else
                begin
                  C[IC + I, JC + J] := Beta * C[IC + I, JC + J] + Alpha * v;
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end;
  if (OpTypeA = 0) and (OpTypeB = 0) then
    begin

      //
      // A*B
      //
      I := 0;
      while I <= M - 1 do
        begin
          if AP_FP_Neq(Beta, 0) then
            begin
              APVMul(@C[IC + I][0], JC, JC + N - 1, Beta);
            end
          else
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  C[IC + I, JC + J] := 0;
                  Inc(J);
                end;
            end;
          if AP_FP_Neq(Alpha, 0) then
            begin
              J := 0;
              while J <= k - 1 do
                begin
                  v := Alpha * A[IA + I, JA + J];
                  APVAdd(@C[IC + I][0], JC, JC + N - 1, @B[IB + J][0], JB,
                    JB + N - 1, v);
                  Inc(J);
                end;
            end;
          Inc(I);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB <> 0) then
    begin

      //
      // A'*B'
      //
      I := 0;
      while I <= M - 1 do
        begin
          J := 0;
          while J <= N - 1 do
            begin
              if AP_FP_Eq(Alpha, 0) then
                begin
                  v := 0;
                end
              else
                begin
                  i1_ := (JB) - (IA);
                  v := 0.0;
                  for i_ := IA to IA + k - 1 do
                    begin
                      v := v + A[i_, JA + I] * B[IB + J, i_ + i1_];
                    end;
                end;
              if AP_FP_Eq(Beta, 0) then
                begin
                  C[IC + I, JC + J] := Alpha * v;
                end
              else
                begin
                  C[IC + I, JC + J] := Beta * C[IC + I, JC + J] + Alpha * v;
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB = 0) then
    begin

      //
      // A'*B
      //
      if AP_FP_Eq(Beta, 0) then
        begin
          I := 0;
          while I <= M - 1 do
            begin
              J := 0;
              while J <= N - 1 do
                begin
                  C[IC + I, JC + J] := 0;
                  Inc(J);
                end;
              Inc(I);
            end;
        end
      else
        begin
          I := 0;
          while I <= M - 1 do
            begin
              APVMul(@C[IC + I][0], JC, JC + N - 1, Beta);
              Inc(I);
            end;
        end;
      if AP_FP_Neq(Alpha, 0) then
        begin
          J := 0;
          while J <= k - 1 do
            begin
              I := 0;
              while I <= M - 1 do
                begin
                  v := Alpha * A[IA + J, JA + I];
                  APVAdd(@C[IC + I][0], JC, JC + N - 1, @B[IB + J][0], JB,
                    JB + N - 1, v);
                  Inc(I);
                end;
              Inc(J);
            end;
        end;
      Exit;
    end;
end;
