{ ****************************************************************************** }
{ * Singular value decomposition support, by QQ 600585@qq.com                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
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
function RMatrixSVD(A: TLMatrix; const M, n, UNeeded, VTNeeded, AdditionalMemory: TLInt;
  var w: TLVec; var u: TLMatrix; var VT: TLMatrix): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  TauQ   : TLVec;
  TauP   : TLVec;
  TAU    : TLVec;
  E      : TLVec;
  Work   : TLVec;
  t2     : TLMatrix;
  IsUpper: Boolean;
  MinMN  : TLInt;
  NCU    : TLInt;
  NRVT   : TLInt;
  NRU    : TLInt;
  NCVT   : TLInt;
  i      : TLInt;
  J      : TLInt;
begin
  A := DynamicArrayCopy(A);
  Result := True;
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  Assert((UNeeded >= 0) and (UNeeded <= 2), 'SVDDecomposition: wrong parameters!');
  Assert((VTNeeded >= 0) and (VTNeeded <= 2), 'SVDDecomposition: wrong parameters!');
  Assert((AdditionalMemory >= 0) and (AdditionalMemory <= 2), 'SVDDecomposition: wrong parameters!');

  //
  // initialize
  //
  MinMN := Min(M, n);
  SetLength(w, MinMN + 1);
  NCU := 0;
  NRU := 0;
  if UNeeded = 1 then
    begin
      NRU := M;
      NCU := MinMN;
      SetLength(u, NRU (* - 1 + 1 // optimized compiler *) , NCU (* - 1 + 1 // optimized compiler *) );
    end;
  if UNeeded = 2 then
    begin
      NRU := M;
      NCU := M;
      SetLength(u, NRU (* - 1 + 1 // optimized compiler *) , NCU (* - 1 + 1 // optimized compiler *) );
    end;
  NRVT := 0;
  NCVT := 0;
  if VTNeeded = 1 then
    begin
      NRVT := MinMN;
      NCVT := n;
      SetLength(VT, NRVT (* - 1 + 1 // optimized compiler *) , NCVT (* - 1 + 1 // optimized compiler *) );
    end;
  if VTNeeded = 2 then
    begin
      NRVT := n;
      NCVT := n;
      SetLength(VT, NRVT (* - 1 + 1 // optimized compiler *) , NCVT (* - 1 + 1 // optimized compiler *) );
    end;

  //
  // M much larger than N
  // Use bidiagonal reduction with QR-decomposition
  //
  if AP_FP_Greater(M, 1.6 * n) then
    begin
      if UNeeded = 0 then
        begin

          //
          // No left singular vectors to be computed
          //
          RMatrixQR(A, M, n, TAU);
          i := 0;
          while i <= n - 1 do
            begin
              J := 0;
              while J <= i - 1 do
                begin
                  A[i, J] := 0;
                  Inc(J);
                end;
              Inc(i);
            end;
          RMatrixBD(A, n, n, TauQ, TauP);
          RMatrixBDUnpackPT(A, n, n, TauP, NRVT, VT);
          RMatrixBDUnpackDiagonals(A, n, n, IsUpper, w, E);
          Result := RMatrixBDSVD(w, E, n, IsUpper, False, u, 0, A, 0, VT, NCVT);
          Exit;
        end
      else
        begin

          //
          // Left singular vectors (may be full matrix U) to be computed
          //
          RMatrixQR(A, M, n, TAU);
          RMatrixQRUnpackQ(A, M, n, TAU, NCU, u);
          i := 0;
          while i <= n - 1 do
            begin
              J := 0;
              while J <= i - 1 do
                begin
                  A[i, J] := 0;
                  Inc(J);
                end;
              Inc(i);
            end;
          RMatrixBD(A, n, n, TauQ, TauP);
          RMatrixBDUnpackPT(A, n, n, TauP, NRVT, VT);
          RMatrixBDUnpackDiagonals(A, n, n, IsUpper, w, E);
          if AdditionalMemory < 1 then
            begin

              //
              // No additional memory can be used
              //
              RMatrixBDMultiplyByQ(A, n, n, TauQ, u, M, n, True, False);
              Result := RMatrixBDSVD(w, E, n, IsUpper, False, u, M, A, 0, VT, NCVT);
            end
          else
            begin

              //
              // Large U. Transforming intermediate matrix T2
              //
              SetLength(Work, Max(M, n) + 1);
              RMatrixBDUnpackQ(A, n, n, TauQ, n, t2);
              CopyMatrix(u, 0, M - 1, 0, n - 1, A, 0, M - 1, 0, n - 1);
              InplaceTranspose(t2, 0, n - 1, 0, n - 1, Work);
              Result := RMatrixBDSVD(w, E, n, IsUpper, False, u, 0, t2, n, VT, NCVT);
              MatrixMatrixMultiply(A, 0, M - 1, 0, n - 1, False, t2, 0, n - 1, 0,
                n - 1, True, 1.0, u, 0, M - 1, 0, n - 1, 0.0, Work);
            end;
          Exit;
        end;
    end;

  //
  // N much larger than M
  // Use bidiagonal reduction with LQ-decomposition
  //
  if AP_FP_Greater(n, 1.6 * M) then
    begin
      if VTNeeded = 0 then
        begin

          //
          // No right singular vectors to be computed
          //
          RMatrixLQ(A, M, n, TAU);
          i := 0;
          while i <= M - 1 do
            begin
              J := i + 1;
              while J <= M - 1 do
                begin
                  A[i, J] := 0;
                  Inc(J);
                end;
              Inc(i);
            end;
          RMatrixBD(A, M, M, TauQ, TauP);
          RMatrixBDUnpackQ(A, M, M, TauQ, NCU, u);
          RMatrixBDUnpackDiagonals(A, M, M, IsUpper, w, E);
          SetLength(Work, M + 1);
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          Result := RMatrixBDSVD(w, E, M, IsUpper, False, A, 0, u, NRU, VT, 0);
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          Exit;
        end
      else
        begin

          //
          // Right singular vectors (may be full matrix VT) to be computed
          //
          RMatrixLQ(A, M, n, TAU);
          RMatrixLQUnpackQ(A, M, n, TAU, NRVT, VT);
          i := 0;
          while i <= M - 1 do
            begin
              J := i + 1;
              while J <= M - 1 do
                begin
                  A[i, J] := 0;
                  Inc(J);
                end;
              Inc(i);
            end;
          RMatrixBD(A, M, M, TauQ, TauP);
          RMatrixBDUnpackQ(A, M, M, TauQ, NCU, u);
          RMatrixBDUnpackDiagonals(A, M, M, IsUpper, w, E);
          SetLength(Work, Max(M, n) + 1);
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          if AdditionalMemory < 1 then
            begin

              //
              // No additional memory available
              //
              RMatrixBDMultiplyByP(A, M, M, TauP, VT, M, n, False, True);
              Result := RMatrixBDSVD(w, E, M, IsUpper, False, A, 0, u, NRU, VT, n);
            end
          else
            begin

              //
              // Large VT. Transforming intermediate matrix T2
              //
              RMatrixBDUnpackPT(A, M, M, TauP, M, t2);
              Result := RMatrixBDSVD(w, E, M, IsUpper, False, A, 0, u, NRU, t2, M);
              CopyMatrix(VT, 0, M - 1, 0, n - 1, A, 0, M - 1, 0, n - 1);
              MatrixMatrixMultiply(t2, 0, M - 1, 0, M - 1, False, A, 0, M - 1, 0,
                n - 1, False, 1.0, VT, 0, M - 1, 0, n - 1, 0.0, Work);
            end;
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          Exit;
        end;
    end;

  //
  // M<=N
  // We can use inplace transposition of U to get rid of columnwise operations
  //
  if M <= n then
    begin
      RMatrixBD(A, M, n, TauQ, TauP);
      RMatrixBDUnpackQ(A, M, n, TauQ, NCU, u);
      RMatrixBDUnpackPT(A, M, n, TauP, NRVT, VT);
      RMatrixBDUnpackDiagonals(A, M, n, IsUpper, w, E);
      SetLength(Work, M + 1);
      InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
      Result := RMatrixBDSVD(w, E, MinMN, IsUpper, False, A, 0, u, NRU, VT, NCVT);
      InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
      Exit;
    end;

  //
  // Simple bidiagonal reduction
  //
  RMatrixBD(A, M, n, TauQ, TauP);
  RMatrixBDUnpackQ(A, M, n, TauQ, NCU, u);
  RMatrixBDUnpackPT(A, M, n, TauP, NRVT, VT);
  RMatrixBDUnpackDiagonals(A, M, n, IsUpper, w, E);
  if (AdditionalMemory < 2) or (UNeeded = 0) then
    begin

      //
      // We cant use additional memory or there is no need in such operations
      //
      Result := RMatrixBDSVD(w, E, MinMN, IsUpper, False, u, NRU, A, 0, VT, NCVT);
    end
  else
    begin

      //
      // We can use additional memory
      //
      SetLength(t2, MinMN (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      CopyAndTranspose(u, 0, M - 1, 0, MinMN - 1, t2, 0, MinMN - 1, 0, M - 1);
      Result := RMatrixBDSVD(w, E, MinMN, IsUpper, False, u, 0, t2, M, VT, NCVT);
      CopyAndTranspose(t2, 0, MinMN - 1, 0, M - 1, u, 0, M - 1, 0, MinMN - 1);
    end;
end; 
