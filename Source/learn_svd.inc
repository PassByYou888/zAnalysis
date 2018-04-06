{ ****************************************************************************** }
{ * Singular value decomposition support, by QQ 600585@qq.com                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

(* ************************************************************************
  Singular value decomposition of a rectangular matrix.

  The algorithm calculates the singular value decomposition of a matrix of
  size MxN: A = U * S * V^T

  The algorithm finds the singular values and, optionally, matrices U and V^T.
  The algorithm can find both first min(M,N) columns of matrix U and rows of
  matrix V^T (singular vectors), and matrices U and V^T wholly (of sizes MxM
  and NxN respectively).

  Take into account that the subroutine does not return matrix V but V^T.

  Input parameters:
  A           -   matrix to be decomposed.
  Array whose indexes range within [0..M-1, 0..N-1].
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  UNeeded     -   0, 1 or 2. See the description of the parameter U.
  VTNeeded    -   0, 1 or 2. See the description of the parameter VT.
  AdditionalMemory -
  If the parameter:
  * equals 0, the algorithm doesn’t use additional
  memory (lower requirements, lower performance).
  * equals 1, the algorithm uses additional
  memory of size min(M,N)*min(M,N) of real numbers.
  It often speeds up the algorithm.
  * equals 2, the algorithm uses additional
  memory of size M*min(M,N) of real numbers.
  It allows to get a maximum performance.
  The recommended value of the parameter is 2.

  Output parameters:
  W           -   contains singular values in descending order.
  U           -   if UNeeded=0, U isn't changed, the left singular vectors
  are not calculated.
  if Uneeded=1, U contains left singular vectors (first
  min(M,N) columns of matrix U). Array whose indexes range
  within [0..M-1, 0..Min(M,N)-1].
  if UNeeded=2, U contains matrix U wholly. Array whose
  indexes range within [0..M-1, 0..M-1].
  VT          -   if VTNeeded=0, VT isn’t changed, the right singular vectors
  are not calculated.
  if VTNeeded=1, VT contains right singular vectors (first
  min(M,N) rows of matrix V^T). Array whose indexes range
  within [0..min(M,N)-1, 0..N-1].
  if VTNeeded=2, VT contains matrix V^T wholly. Array whose
  indexes range within [0..N-1, 0..N-1].
  ************************************************************************ *)
function RMatrixSVD(A: TLearnFloat2DArray; const M, N, UNeeded, VTNeeded, AdditionalMemory: TLearnInteger;
  var W: TLearnFloatArray; var U: TLearnFloat2DArray; var VT: TLearnFloat2DArray): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  TauQ   : TLearnFloatArray;
  TauP   : TLearnFloatArray;
  Tau    : TLearnFloatArray;
  E      : TLearnFloatArray;
  WORK   : TLearnFloatArray;
  T2     : TLearnFloat2DArray;
  IsUpper: Boolean;
  MinMN  : TLearnInteger;
  NCU    : TLearnInteger;
  NRVT   : TLearnInteger;
  NRU    : TLearnInteger;
  NCVT   : TLearnInteger;
  I      : TLearnInteger;
  J      : TLearnInteger;
begin
  A := DynamicArrayCopy(A);
  Result := True;
  if (M = 0) or (N = 0) then
    begin
      Exit;
    end;
  Assert((UNeeded >= 0) and (UNeeded <= 2),
    'SVDDecomposition: wrong parameters!');
  Assert((VTNeeded >= 0) and (VTNeeded <= 2),
    'SVDDecomposition: wrong parameters!');
  Assert((AdditionalMemory >= 0) and (AdditionalMemory <= 2),
    'SVDDecomposition: wrong parameters!');

  //
  // initialize
  //
  MinMN := Min(M, N);
  SetLength(W, MinMN + 1);
  NCU := 0;
  NRU := 0;
  if UNeeded = 1 then
    begin
      NRU := M;
      NCU := MinMN;
      SetLength(U, NRU (* - 1 + 1 // optimized compiler *) , NCU (* - 1 + 1 // optimized compiler *) );
    end;
  if UNeeded = 2 then
    begin
      NRU := M;
      NCU := M;
      SetLength(U, NRU (* - 1 + 1 // optimized compiler *) , NCU (* - 1 + 1 // optimized compiler *) );
    end;
  NRVT := 0;
  NCVT := 0;
  if VTNeeded = 1 then
    begin
      NRVT := MinMN;
      NCVT := N;
      SetLength(VT, NRVT (* - 1 + 1 // optimized compiler *) , NCVT (* - 1 + 1 // optimized compiler *) );
    end;
  if VTNeeded = 2 then
    begin
      NRVT := N;
      NCVT := N;
      SetLength(VT, NRVT (* - 1 + 1 // optimized compiler *) , NCVT (* - 1 + 1 // optimized compiler *) );
    end;

  //
  // M much larger than N
  // Use bidiagonal reduction with QR-decomposition
  //
  if AP_FP_Greater(M, 1.6 * N) then
    begin
      if UNeeded = 0 then
        begin

          //
          // No left singular vectors to be computed
          //
          RMatrixQR(A, M, N, Tau);
          I := 0;
          while I <= N - 1 do
            begin
              J := 0;
              while J <= I - 1 do
                begin
                  A[I, J] := 0;
                  Inc(J);
                end;
              Inc(I);
            end;
          RMatrixBD(A, N, N, TauQ, TauP);
          RMatrixBDUnpackPT(A, N, N, TauP, NRVT, VT);
          RMatrixBDUnpackDiagonals(A, N, N, IsUpper, W, E);
          Result := RMatrixBDSVD(W, E, N, IsUpper, False, U, 0, A, 0, VT, NCVT);
          Exit;
        end
      else
        begin

          //
          // Left singular vectors (may be full matrix U) to be computed
          //
          RMatrixQR(A, M, N, Tau);
          RMatrixQRUnpackQ(A, M, N, Tau, NCU, U);
          I := 0;
          while I <= N - 1 do
            begin
              J := 0;
              while J <= I - 1 do
                begin
                  A[I, J] := 0;
                  Inc(J);
                end;
              Inc(I);
            end;
          RMatrixBD(A, N, N, TauQ, TauP);
          RMatrixBDUnpackPT(A, N, N, TauP, NRVT, VT);
          RMatrixBDUnpackDiagonals(A, N, N, IsUpper, W, E);
          if AdditionalMemory < 1 then
            begin

              //
              // No additional memory can be used
              //
              RMatrixBDMultiplyByQ(A, N, N, TauQ, U, M, N, True, False);
              Result := RMatrixBDSVD(W, E, N, IsUpper, False, U, M, A, 0, VT, NCVT);
            end
          else
            begin

              //
              // Large U. Transforming intermediate matrix T2
              //
              SetLength(WORK, Max(M, N) + 1);
              RMatrixBDUnpackQ(A, N, N, TauQ, N, T2);
              CopyMatrix(U, 0, M - 1, 0, N - 1, A, 0, M - 1, 0, N - 1);
              InplaceTranspose(T2, 0, N - 1, 0, N - 1, WORK);
              Result := RMatrixBDSVD(W, E, N, IsUpper, False, U, 0, T2, N, VT, NCVT);
              MatrixMatrixMultiply(A, 0, M - 1, 0, N - 1, False, T2, 0, N - 1, 0,
                N - 1, True, 1.0, U, 0, M - 1, 0, N - 1, 0.0, WORK);
            end;
          Exit;
        end;
    end;

  //
  // N much larger than M
  // Use bidiagonal reduction with LQ-decomposition
  //
  if AP_FP_Greater(N, 1.6 * M) then
    begin
      if VTNeeded = 0 then
        begin

          //
          // No right singular vectors to be computed
          //
          RMatrixLQ(A, M, N, Tau);
          I := 0;
          while I <= M - 1 do
            begin
              J := I + 1;
              while J <= M - 1 do
                begin
                  A[I, J] := 0;
                  Inc(J);
                end;
              Inc(I);
            end;
          RMatrixBD(A, M, M, TauQ, TauP);
          RMatrixBDUnpackQ(A, M, M, TauQ, NCU, U);
          RMatrixBDUnpackDiagonals(A, M, M, IsUpper, W, E);
          SetLength(WORK, M + 1);
          InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
          Result := RMatrixBDSVD(W, E, M, IsUpper, False, A, 0, U, NRU, VT, 0);
          InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
          Exit;
        end
      else
        begin

          //
          // Right singular vectors (may be full matrix VT) to be computed
          //
          RMatrixLQ(A, M, N, Tau);
          RMatrixLQUnpackQ(A, M, N, Tau, NRVT, VT);
          I := 0;
          while I <= M - 1 do
            begin
              J := I + 1;
              while J <= M - 1 do
                begin
                  A[I, J] := 0;
                  Inc(J);
                end;
              Inc(I);
            end;
          RMatrixBD(A, M, M, TauQ, TauP);
          RMatrixBDUnpackQ(A, M, M, TauQ, NCU, U);
          RMatrixBDUnpackDiagonals(A, M, M, IsUpper, W, E);
          SetLength(WORK, Max(M, N) + 1);
          InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
          if AdditionalMemory < 1 then
            begin

              //
              // No additional memory available
              //
              RMatrixBDMultiplyByP(A, M, M, TauP, VT, M, N, False, True);
              Result := RMatrixBDSVD(W, E, M, IsUpper, False, A, 0, U, NRU, VT, N);
            end
          else
            begin

              //
              // Large VT. Transforming intermediate matrix T2
              //
              RMatrixBDUnpackPT(A, M, M, TauP, M, T2);
              Result := RMatrixBDSVD(W, E, M, IsUpper, False, A, 0, U, NRU, T2, M);
              CopyMatrix(VT, 0, M - 1, 0, N - 1, A, 0, M - 1, 0, N - 1);
              MatrixMatrixMultiply(T2, 0, M - 1, 0, M - 1, False, A, 0, M - 1, 0,
                N - 1, False, 1.0, VT, 0, M - 1, 0, N - 1, 0.0, WORK);
            end;
          InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
          Exit;
        end;
    end;

  //
  // M<=N
  // We can use inplace transposition of U to get rid of columnwise operations
  //
  if M <= N then
    begin
      RMatrixBD(A, M, N, TauQ, TauP);
      RMatrixBDUnpackQ(A, M, N, TauQ, NCU, U);
      RMatrixBDUnpackPT(A, M, N, TauP, NRVT, VT);
      RMatrixBDUnpackDiagonals(A, M, N, IsUpper, W, E);
      SetLength(WORK, M + 1);
      InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
      Result := RMatrixBDSVD(W, E, MinMN, IsUpper, False, A, 0, U, NRU, VT, NCVT);
      InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
      Exit;
    end;

  //
  // Simple bidiagonal reduction
  //
  RMatrixBD(A, M, N, TauQ, TauP);
  RMatrixBDUnpackQ(A, M, N, TauQ, NCU, U);
  RMatrixBDUnpackPT(A, M, N, TauP, NRVT, VT);
  RMatrixBDUnpackDiagonals(A, M, N, IsUpper, W, E);
  if (AdditionalMemory < 2) or (UNeeded = 0) then
    begin

      //
      // We cant use additional memory or there is no need in such operations
      //
      Result := RMatrixBDSVD(W, E, MinMN, IsUpper, False, U, NRU, A, 0, VT, NCVT);
    end
  else
    begin

      //
      // We can use additional memory
      //
      SetLength(T2, MinMN (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      CopyAndTranspose(U, 0, M - 1, 0, MinMN - 1, T2, 0, MinMN - 1, 0, M - 1);
      Result := RMatrixBDSVD(W, E, MinMN, IsUpper, False, U, 0, T2, M, VT, NCVT);
      CopyAndTranspose(T2, 0, MinMN - 1, 0, M - 1, U, 0, M - 1, 0, MinMN - 1);
    end;
end;
