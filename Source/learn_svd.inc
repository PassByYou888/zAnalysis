{ ****************************************************************************** }
{ * machine Learn support, writen by QQ 600585@qq.com                          * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
function VectorNorm2(const X: TLearnFloatArray; I1: TLearnInteger;
  I2: TLearnInteger): TLearnFloat;
var
  N    : TLearnInteger;
  IX   : TLearnInteger;
  ABSXI: TLearnFloat;
  SCL  : TLearnFloat;
  SSQ  : TLearnFloat;
begin
  N := I2 - I1 + 1;
  if N < 1 then
    begin
      Result := 0;
      Exit;
    end;
  if N = 1 then
    begin
      Result := AbsReal(X[I1]);
      Exit;
    end;
  SCL := 0;
  SSQ := 1;
  IX := I1;
  while IX <= I2 do
    begin
      if AP_FP_Neq(X[IX], 0) then
        begin
          ABSXI := AbsReal(X[IX]);
          if AP_FP_Less(SCL, ABSXI) then
            begin
              SSQ := 1 + SSQ * AP_Sqr(SCL / ABSXI);
              SCL := ABSXI;
            end
          else
            begin
              SSQ := SSQ + AP_Sqr(ABSXI / SCL);
            end;
        end;
      Inc(IX);
    end;
  Result := SCL * SQRT(SSQ);
end;

function VectorIdxAbsMax(const X: TLearnFloatArray; I1: TLearnInteger;
  I2: TLearnInteger): TLearnInteger;
var
  I: TLearnInteger;
  A: TLearnFloat;
begin
  Result := I1;
  A := AbsReal(X[Result]);
  I := I1 + 1;
  while I <= I2 do
    begin
      if AP_FP_Greater(AbsReal(X[I]), AbsReal(X[Result])) then
        begin
          Result := I;
        end;
      Inc(I);
    end;
end;

function ColumnIdxAbsMax(const X: TLearnFloat2DArray; I1: TLearnInteger;
  I2: TLearnInteger; J: TLearnInteger): TLearnInteger;
var
  I: TLearnInteger;
  A: TLearnFloat;
begin
  Result := I1;
  A := AbsReal(X[Result, J]);
  I := I1 + 1;
  while I <= I2 do
    begin
      if AP_FP_Greater(AbsReal(X[I, J]), AbsReal(X[Result, J])) then
        begin
          Result := I;
        end;
      Inc(I);
    end;
end;

function RowIdxAbsMax(const X: TLearnFloat2DArray; J1: TLearnInteger;
  J2: TLearnInteger; I: TLearnInteger): TLearnInteger;
var
  J: TLearnInteger;
  A: TLearnFloat;
begin
  Result := J1;
  A := AbsReal(X[I, Result]);
  J := J1 + 1;
  while J <= J2 do
    begin
      if AP_FP_Greater(AbsReal(X[I, J]), AbsReal(X[I, Result])) then
        begin
          Result := J;
        end;
      Inc(J);
    end;
end;

function UpperHessenberg1Norm(const A: TLearnFloat2DArray; I1: TLearnInteger;
  I2: TLearnInteger; J1: TLearnInteger; J2: TLearnInteger;
  var WORK: TLearnFloatArray): TLearnFloat;
var
  I: TLearnInteger;
  J: TLearnInteger;
begin
  Assert(I2 - I1 = J2 - J1, 'UpperHessenberg1Norm: I2-I1<>J2-J1!');
  J := J1;
  while J <= J2 do
    begin
      WORK[J] := 0;
      Inc(J);
    end;
  I := I1;
  while I <= I2 do
    begin
      J := Max(J1, J1 + I - I1 - 1);
      while J <= J2 do
        begin
          WORK[J] := WORK[J] + AbsReal(A[I, J]);
          Inc(J);
        end;
      Inc(I);
    end;
  Result := 0;
  J := J1;
  while J <= J2 do
    begin
      Result := Max(Result, WORK[J]);
      Inc(J);
    end;
end;

procedure CopyMatrix(const A: TLearnFloat2DArray; IS1: TLearnInteger;
  IS2: TLearnInteger; JS1: TLearnInteger; JS2: TLearnInteger;
  var B: TLearnFloat2DArray; ID1: TLearnInteger; ID2: TLearnInteger;
  JD1: TLearnInteger; JD2: TLearnInteger);
var
  ISRC: TLearnInteger;
  IDST: TLearnInteger;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
    begin
      Exit;
    end;
  Assert(IS2 - IS1 = ID2 - ID1, 'CopyMatrix: different sizes!');
  Assert(JS2 - JS1 = JD2 - JD1, 'CopyMatrix: different sizes!');
  ISRC := IS1;
  while ISRC <= IS2 do
    begin
      IDST := ISRC - IS1 + ID1;
      APVMove(@B[IDST][0], JD1, JD2, @A[ISRC][0], JS1, JS2);
      Inc(ISRC);
    end;
end;

procedure InplaceTranspose(var A: TLearnFloat2DArray; I1: TLearnInteger;
  I2: TLearnInteger; J1: TLearnInteger; J2: TLearnInteger;
  var WORK: TLearnFloatArray);
var
  I  : TLearnInteger;
  J  : TLearnInteger;
  IPS: TLearnInteger;
  JPS: TLearnInteger;
  L  : TLearnInteger;
  i_ : TLearnInteger;
  i1_: TLearnInteger;
begin
  if (I1 > I2) or (J1 > J2) then
    begin
      Exit;
    end;
  Assert(I1 - I2 = J1 - J2, 'InplaceTranspose error: incorrect array size!');
  I := I1;
  while I <= I2 - 1 do
    begin
      J := J1 + I - I1;
      IPS := I + 1;
      JPS := J1 + IPS - I1;
      L := I2 - I;
      i1_ := (IPS) - (1);
      for i_ := 1 to L do
        begin
          WORK[i_] := A[i_ + i1_, J];
        end;
      i1_ := (JPS) - (IPS);
      for i_ := IPS to I2 do
        begin
          A[i_, J] := A[I, i_ + i1_];
        end;
      APVMove(@A[I][0], JPS, J2, @WORK[0], 1, L);
      Inc(I);
    end;
end;

procedure CopyAndTranspose(const A: TLearnFloat2DArray; IS1: TLearnInteger;
  IS2: TLearnInteger; JS1: TLearnInteger; JS2: TLearnInteger;
  var B: TLearnFloat2DArray; ID1: TLearnInteger; ID2: TLearnInteger;
  JD1: TLearnInteger; JD2: TLearnInteger);
var
  ISRC: TLearnInteger;
  JDST: TLearnInteger;
  i_  : TLearnInteger;
  i1_ : TLearnInteger;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
    begin
      Exit;
    end;
  Assert(IS2 - IS1 = JD2 - JD1, 'CopyAndTranspose: different sizes!');
  Assert(JS2 - JS1 = ID2 - ID1, 'CopyAndTranspose: different sizes!');
  ISRC := IS1;
  while ISRC <= IS2 do
    begin
      JDST := ISRC - IS1 + JD1;
      i1_ := (JS1) - (ID1);
      for i_ := ID1 to ID2 do
        begin
          B[i_, JDST] := A[ISRC, i_ + i1_];
        end;
      Inc(ISRC);
    end;
end;

procedure MatrixVectorMultiply(const A: TLearnFloat2DArray; I1: TLearnInteger;
  I2: TLearnInteger; J1: TLearnInteger; J2: TLearnInteger; Trans: Boolean;
  const X: TLearnFloatArray; IX1: TLearnInteger; IX2: TLearnInteger;
  Alpha: TLearnFloat; var Y: TLearnFloatArray; IY1: TLearnInteger;
  IY2: TLearnInteger; Beta: TLearnFloat);
var
  I: TLearnInteger;
  V: TLearnFloat;
begin
  if not Trans then
    begin

      //
      // y := alpha*A*x + beta*y;
      //
      if (I1 > I2) or (J1 > J2) then
        begin
          Exit;
        end;
      Assert(J2 - J1 = IX2 - IX1, 'MatrixVectorMultiply: A and X dont match!');
      Assert(I2 - I1 = IY2 - IY1, 'MatrixVectorMultiply: A and Y dont match!');

      //
      // beta*y
      //
      if AP_FP_Eq(Beta, 0) then
        begin
          I := IY1;
          while I <= IY2 do
            begin
              Y[I] := 0;
              Inc(I);
            end;
        end
      else
        begin
          APVMul(@Y[0], IY1, IY2, Beta);
        end;

      //
      // alpha*A*x
      //
      I := I1;
      while I <= I2 do
        begin
          V := APVDotProduct(@A[I][0], J1, J2, @X[0], IX1, IX2);
          Y[IY1 + I - I1] := Y[IY1 + I - I1] + Alpha * V;
          Inc(I);
        end;
    end
  else
    begin

      //
      // y := alpha*A'*x + beta*y;
      //
      if (I1 > I2) or (J1 > J2) then
        begin
          Exit;
        end;
      Assert(I2 - I1 = IX2 - IX1, 'MatrixVectorMultiply: A and X dont match!');
      Assert(J2 - J1 = IY2 - IY1, 'MatrixVectorMultiply: A and Y dont match!');

      //
      // beta*y
      //
      if AP_FP_Eq(Beta, 0) then
        begin
          I := IY1;
          while I <= IY2 do
            begin
              Y[I] := 0;
              Inc(I);
            end;
        end
      else
        begin
          APVMul(@Y[0], IY1, IY2, Beta);
        end;

      //
      // alpha*A'*x
      //
      I := I1;
      while I <= I2 do
        begin
          V := Alpha * X[IX1 + I - I1];
          APVAdd(@Y[0], IY1, IY2, @A[I][0], J1, J2, V);
          Inc(I);
        end;
    end;
end;

function Pythag2(X: TLearnFloat; Y: TLearnFloat): TLearnFloat;
var
  W   : TLearnFloat;
  XABS: TLearnFloat;
  YABS: TLearnFloat;
  Z   : TLearnFloat;
begin
  XABS := AbsReal(X);
  YABS := AbsReal(Y);
  W := Max(XABS, YABS);
  Z := Min(XABS, YABS);
  if AP_FP_Eq(Z, 0) then
    begin
      Result := W;
    end
  else
    begin
      Result := W * SQRT(1 + AP_Sqr(Z / W));
    end;
end;

procedure MatrixMatrixMultiply(const A: TLearnFloat2DArray; AI1: TLearnInteger;
  AI2: TLearnInteger; AJ1: TLearnInteger; AJ2: TLearnInteger; TransA: Boolean;
  const B: TLearnFloat2DArray; BI1: TLearnInteger; BI2: TLearnInteger;
  BJ1: TLearnInteger; BJ2: TLearnInteger; TransB: Boolean; Alpha: TLearnFloat;
  var C: TLearnFloat2DArray; CI1: TLearnInteger; CI2: TLearnInteger;
  CJ1: TLearnInteger; CJ2: TLearnInteger; Beta: TLearnFloat;
  var WORK: TLearnFloatArray);
var
  ARows: TLearnInteger;
  ACols: TLearnInteger;
  BRows: TLearnInteger;
  BCols: TLearnInteger;
  CRows: TLearnInteger;
  CCols: TLearnInteger;
  I    : TLearnInteger;
  J    : TLearnInteger;
  K    : TLearnInteger;
  L    : TLearnInteger;
  R    : TLearnInteger;
  V    : TLearnFloat;
  i_   : TLearnInteger;
  i1_  : TLearnInteger;
begin

  //
  // Setup
  //
  if not TransA then
    begin
      ARows := AI2 - AI1 + 1;
      ACols := AJ2 - AJ1 + 1;
    end
  else
    begin
      ARows := AJ2 - AJ1 + 1;
      ACols := AI2 - AI1 + 1;
    end;
  if not TransB then
    begin
      BRows := BI2 - BI1 + 1;
      BCols := BJ2 - BJ1 + 1;
    end
  else
    begin
      BRows := BJ2 - BJ1 + 1;
      BCols := BI2 - BI1 + 1;
    end;
  Assert(ACols = BRows, 'MatrixMatrixMultiply: incorrect matrix sizes!');
  if (ARows <= 0) or (ACols <= 0) or (BRows <= 0) or (BCols <= 0) then
    begin
      Exit;
    end;
  CRows := ARows;
  CCols := BCols;

  //
  // Test WORK
  //
  I := Max(ARows, ACols);
  I := Max(BRows, I);
  I := Max(I, BCols);
  WORK[1] := 0;
  WORK[I] := 0;

  //
  // Prepare C
  //
  if AP_FP_Eq(Beta, 0) then
    begin
      I := CI1;
      while I <= CI2 do
        begin
          J := CJ1;
          while J <= CJ2 do
            begin
              C[I, J] := 0;
              Inc(J);
            end;
          Inc(I);
        end;
    end
  else
    begin
      I := CI1;
      while I <= CI2 do
        begin
          APVMul(@C[I][0], CJ1, CJ2, Beta);
          Inc(I);
        end;
    end;

  //
  // A*B
  //
  if not TransA and not TransB then
    begin
      L := AI1;
      while L <= AI2 do
        begin
          R := BI1;
          while R <= BI2 do
            begin
              V := Alpha * A[L, AJ1 + R - BI1];
              K := CI1 + L - AI1;
              APVAdd(@C[K][0], CJ1, CJ2, @B[R][0], BJ1, BJ2, V);
              Inc(R);
            end;
          Inc(L);
        end;
      Exit;
    end;

  //
  // A*B'
  //
  if not TransA and TransB then
    begin
      if ARows * ACols < BRows * BCols then
        begin
          R := BI1;
          while R <= BI2 do
            begin
              L := AI1;
              while L <= AI2 do
                begin
                  V := APVDotProduct(@A[L][0], AJ1, AJ2, @B[R][0], BJ1, BJ2);
                  C[CI1 + L - AI1, CJ1 + R - BI1] := C[CI1 + L - AI1, CJ1 + R - BI1]
                    + Alpha * V;
                  Inc(L);
                end;
              Inc(R);
            end;
          Exit;
        end
      else
        begin
          L := AI1;
          while L <= AI2 do
            begin
              R := BI1;
              while R <= BI2 do
                begin
                  V := APVDotProduct(@A[L][0], AJ1, AJ2, @B[R][0], BJ1, BJ2);
                  C[CI1 + L - AI1, CJ1 + R - BI1] := C[CI1 + L - AI1, CJ1 + R - BI1]
                    + Alpha * V;
                  Inc(R);
                end;
              Inc(L);
            end;
          Exit;
        end;
    end;

  //
  // A'*B
  //
  if TransA and not TransB then
    begin
      L := AJ1;
      while L <= AJ2 do
        begin
          R := BI1;
          while R <= BI2 do
            begin
              V := Alpha * A[AI1 + R - BI1, L];
              K := CI1 + L - AJ1;
              APVAdd(@C[K][0], CJ1, CJ2, @B[R][0], BJ1, BJ2, V);
              Inc(R);
            end;
          Inc(L);
        end;
      Exit;
    end;

  //
  // A'*B'
  //
  if TransA and TransB then
    begin
      if ARows * ACols < BRows * BCols then
        begin
          R := BI1;
          while R <= BI2 do
            begin
              I := 1;
              while I <= CRows do
                begin
                  WORK[I] := 0.0;
                  Inc(I);
                end;
              L := AI1;
              while L <= AI2 do
                begin
                  V := Alpha * B[R, BJ1 + L - AI1];
                  K := CJ1 + R - BI1;
                  APVAdd(@WORK[0], 1, CRows, @A[L][0], AJ1, AJ2, V);
                  Inc(L);
                end;
              i1_ := (1) - (CI1);
              for i_ := CI1 to CI2 do
                begin
                  C[i_, K] := C[i_, K] + WORK[i_ + i1_];
                end;
              Inc(R);
            end;
          Exit;
        end
      else
        begin
          L := AJ1;
          while L <= AJ2 do
            begin
              K := AI2 - AI1 + 1;
              i1_ := (AI1) - (1);
              for i_ := 1 to K do
                begin
                  WORK[i_] := A[i_ + i1_, L];
                end;
              R := BI1;
              while R <= BI2 do
                begin
                  V := APVDotProduct(@WORK[0], 1, K, @B[R][0], BJ1, BJ2);
                  C[CI1 + L - AJ1, CJ1 + R - BI1] := C[CI1 + L - AJ1, CJ1 + R - BI1]
                    + Alpha * V;
                  Inc(R);
                end;
              Inc(L);
            end;
          Exit;
        end;
    end;
end;

function RMatrixSVD(A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger;
  UNeeded: TLearnInteger; VTNeeded: TLearnInteger;
  AdditionalMemory: TLearnInteger; var W: TLearnFloatArray; var U: TLearnFloat2DArray;
  var VT: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


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
function RMatrixSVD(A: TLearnFloat2DArray; M: TLearnInteger; N: TLearnInteger;
  UNeeded: TLearnInteger; VTNeeded: TLearnInteger;
  AdditionalMemory: TLearnInteger; var W: TLearnFloatArray; var U: TLearnFloat2DArray;
  var VT: TLearnFloat2DArray): Boolean;
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
      SetLength(U, NRU (* - 1 + 1 // optimized compler *) , NCU (* - 1 + 1 // optimized compler *) );
    end;
  if UNeeded = 2 then
    begin
      NRU := M;
      NCU := M;
      SetLength(U, NRU (* - 1 + 1 // optimized compler *) , NCU (* - 1 + 1 // optimized compler *) );
    end;
  NRVT := 0;
  NCVT := 0;
  if VTNeeded = 1 then
    begin
      NRVT := MinMN;
      NCVT := N;
      SetLength(VT, NRVT (* - 1 + 1 // optimized compler *) , NCVT (* - 1 + 1 // optimized compler *) );
    end;
  if VTNeeded = 2 then
    begin
      NRVT := N;
      NCVT := N;
      SetLength(VT, NRVT (* - 1 + 1 // optimized compler *) , NCVT (* - 1 + 1 // optimized compler *) );
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
      SetLength(T2, MinMN (* - 1 + 1 // optimized compler *) , M (* - 1 + 1 // optimized compler *) );
      CopyAndTranspose(U, 0, M - 1, 0, MinMN - 1, T2, 0, MinMN - 1, 0, M - 1);
      Result := RMatrixBDSVD(W, E, MinMN, IsUpper, False, U, 0, T2, M, VT, NCVT);
      CopyAndTranspose(T2, 0, MinMN - 1, 0, M - 1, U, 0, M - 1, 0, MinMN - 1);
    end;
end;
