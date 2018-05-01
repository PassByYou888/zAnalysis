{ ****************************************************************************** }
{ * Bidiagonal SVD support, by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

function BidiagonalSVDDecompositionInternal(var D: TLVec;
  E: TLVec; N: TLInt; IsUpper: Boolean;
  IsFractionalAccuracyRequired: Boolean; var U: TLMatrix;
  UStart: TLInt; NRU: TLInt; var C: TLMatrix;
  CStart: TLInt; NCC: TLInt; var VT: TLMatrix;
  VStart: TLInt; NCVT: TLInt): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function ExtSignBDSQR(a: TLFloat; b: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure Svd2X2(F: TLFloat; G: TLFloat; H: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SvdV2X2(F: TLFloat; G: TLFloat; H: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat; var SNR: TLFloat;
  var CSR: TLFloat; var SNL: TLFloat; var CSL: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Singular value decomposition of a bidiagonal matrix (extended algorithm)

  The algorithm performs the singular value decomposition  of  a  bidiagonal
  matrix B (upper or lower) representing it as B = Q*S*P^T, where Q and  P -
  orthogonal matrices, S - diagonal matrix with non-negative elements on the
  main diagonal, in descending order.

  The  algorithm  finds  singular  values.  In  addition,  the algorithm can
  calculate  matrices  Q  and P (more precisely, not the matrices, but their
  product  with  given  matrices U and VT - U*Q and (P^T)*VT)).  Of  course,
  matrices U and VT can be of any type, including identity. Furthermore, the
  algorithm can calculate Q'*C (this product is calculated more  effectively
  than U*Q,  because  this calculation operates with rows instead  of matrix
  columns).

  The feature of the algorithm is its ability to find  all  singular  values
  including those which are arbitrarily close to 0  with  relative  accuracy
  close to  machine precision. If the parameter IsFractionalAccuracyRequired
  is set to True, all singular values will have high relative accuracy close
  to machine precision. If the parameter is set to False, only  the  biggest
  singular value will have relative accuracy  close  to  machine  precision.
  The absolute error of other singular values is equal to the absolute error
  of the biggest singular value.

  Input parameters:
  D       -   main diagonal of matrix B.
  Array whose index ranges within [0..N-1].
  E       -   superdiagonal (or subdiagonal) of matrix B.
  Array whose index ranges within [0..N-2].
  N       -   size of matrix B.
  IsUpper -   True, if the matrix is upper bidiagonal.
  IsFractionalAccuracyRequired -
  accuracy to search singular values with.
  U       -   matrix to be multiplied by Q.
  Array whose indexes range within [0..NRU-1, 0..N-1].
  The matrix can be bigger, in that case only the  submatrix
  [0..NRU-1, 0..N-1] will be multiplied by Q.
  NRU     -   number of rows in matrix U.
  C       -   matrix to be multiplied by Q'.
  Array whose indexes range within [0..N-1, 0..NCC-1].
  The matrix can be bigger, in that case only the  submatrix
  [0..N-1, 0..NCC-1] will be multiplied by Q'.
  NCC     -   number of columns in matrix C.
  VT      -   matrix to be multiplied by P^T.
  Array whose indexes range within [0..N-1, 0..NCVT-1].
  The matrix can be bigger, in that case only the  submatrix
  [0..N-1, 0..NCVT-1] will be multiplied by P^T.
  NCVT    -   number of columns in matrix VT.

  Output parameters:
  D       -   singular values of matrix B in descending order.
  U       -   if NRU>0, contains matrix U*Q.
  VT      -   if NCVT>0, contains matrix (P^T)*VT.
  C       -   if NCC>0, contains matrix Q'*C.

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged (rare case).

  Additional information:
  The type of convergence is controlled by the internal  parameter  TOL.
  If the parameter is greater than 0, the singular values will have
  relative accuracy TOL. If TOL<0, the singular values will have
  absolute accuracy ABS(TOL)*norm(B).
  By default, |TOL| falls within the range of 10*Epsilon and 100*Epsilon,
  where Epsilon is the machine precision. It is not  recommended  to  use
  TOL less than 10*Epsilon since this will  considerably  slow  down  the
  algorithm and may not lead to error decreasing.
  ************************************************************************ *)
function RMatrixBDSVD(var D: TLVec; E: TLVec; N: TLInt;
  IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean; var U: TLMatrix;
  NRU: TLInt; var C: TLMatrix; NCC: TLInt;
  var VT: TLMatrix; NCVT: TLInt): Boolean;
var
  D1: TLVec;
  E1: TLVec;
begin
  E := DynamicArrayCopy(E);
  SetLength(D1, N + 1);
  APVMove(@D1[0], 1, N, @D[0], 0, N - 1);
  if N > 1 then
    begin
      SetLength(E1, N (* - 1 + 1 // optimized compiler *) );
      APVMove(@E1[0], 1, N - 1, @E[0], 0, N - 2);
    end;
  Result := BidiagonalSVDDecompositionInternal(D1, E1, N, IsUpper,
    IsFractionalAccuracyRequired, U, 0, NRU, C, 0, NCC, VT, 0, NCVT);
  APVMove(@D[0], 0, N - 1, @D1[0], 1, N);
end;

function BidiagonalSVDDecomposition(var D: TLVec; E: TLVec;
  N: TLInt; IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean;
  var U: TLMatrix; NRU: TLInt; var C: TLMatrix;
  NCC: TLInt; var VT: TLMatrix; NCVT: TLInt): Boolean;
begin
  E := DynamicArrayCopy(E);
  Result := BidiagonalSVDDecompositionInternal(D, E, N, IsUpper,
    IsFractionalAccuracyRequired, U, 1, NRU, C, 1, NCC, VT, 1, NCVT);
end;

(* ************************************************************************
  Internal working subroutine for bidiagonal decomposition
  ************************************************************************ *)
function BidiagonalSVDDecompositionInternal(var D: TLVec;
  E: TLVec; N: TLInt; IsUpper: Boolean;
  IsFractionalAccuracyRequired: Boolean; var U: TLMatrix;
  UStart: TLInt; NRU: TLInt; var C: TLMatrix;
  CStart: TLInt; NCC: TLInt; var VT: TLMatrix;
  VStart: TLInt; NCVT: TLInt): Boolean;
var
  I              : TLInt;
  IDIR           : TLInt;
  ISUB           : TLInt;
  ITER           : TLInt;
  J              : TLInt;
  LL             : TLInt;
  LLL            : TLInt;
  M              : TLInt;
  MAXIT          : TLInt;
  OLDLL          : TLInt;
  OLDM           : TLInt;
  ABSE           : TLFloat;
  ABSS           : TLFloat;
  COSL           : TLFloat;
  COSR           : TLFloat;
  CS             : TLFloat;
  EPS            : TLFloat;
  F              : TLFloat;
  G              : TLFloat;
  H              : TLFloat;
  MU             : TLFloat;
  OLDCS          : TLFloat;
  OLDSN          : TLFloat;
  R              : TLFloat;
  SHIFT          : TLFloat;
  SIGMN          : TLFloat;
  SIGMX          : TLFloat;
  SINL           : TLFloat;
  SINR           : TLFloat;
  SLL            : TLFloat;
  SMAX           : TLFloat;
  SMIN           : TLFloat;
  SMINL          : TLFloat;
  SMINLO         : TLFloat;
  SMINOA         : TLFloat;
  SN             : TLFloat;
  THRESH         : TLFloat;
  TOL            : TLFloat;
  TOLMUL         : TLFloat;
  UNFL           : TLFloat;
  WORK0          : TLVec;
  WORK1          : TLVec;
  WORK2          : TLVec;
  WORK3          : TLVec;
  MAXITR         : TLInt;
  MatrixSplitFlag: Boolean;
  IterFlag       : Boolean;
  UTemp          : TLVec;
  VTTemp         : TLVec;
  CTemp          : TLVec;
  ETemp          : TLVec;
  RightSide      : Boolean;
  FwdDir         : Boolean;
  Tmp            : TLFloat;
  MM1            : TLInt;
  MM0            : TLInt;
  BChangeDir     : Boolean;
  UEnd           : TLInt;
  CEnd           : TLInt;
  VEnd           : TLInt;
  i_             : TLInt;
begin
  E := DynamicArrayCopy(E);
  Result := True;
  if N = 0 then
    begin
      Exit;
    end;
  if N = 1 then
    begin
      if AP_FP_Less(D[1], 0) then
        begin
          D[1] := -D[1];
          if NCVT > 0 then
            begin
              APVMul(@VT[VStart][0], VStart, VStart + NCVT - 1, -1);
            end;
        end;
      Exit;
    end;

  //
  // init
  //
  SetLength(WORK0, N (* - 1 + 1 // optimized compiler *) );
  SetLength(WORK1, N (* - 1 + 1 // optimized compiler *) );
  SetLength(WORK2, N (* - 1 + 1 // optimized compiler *) );
  SetLength(WORK3, N (* - 1 + 1 // optimized compiler *) );
  UEnd := UStart + Max(NRU - 1, 0);
  VEnd := VStart + Max(NCVT - 1, 0);
  CEnd := CStart + Max(NCC - 1, 0);
  SetLength(UTemp, UEnd + 1);
  SetLength(VTTemp, VEnd + 1);
  SetLength(CTemp, CEnd + 1);
  MAXITR := 12;
  RightSide := True;
  FwdDir := True;

  //
  // resize E from N-1 to N
  //
  SetLength(ETemp, N + 1);
  I := 1;
  while I <= N - 1 do
    begin
      ETemp[I] := E[I];
      Inc(I);
    end;
  SetLength(E, N + 1);
  I := 1;
  while I <= N - 1 do
    begin
      E[I] := ETemp[I];
      Inc(I);
    end;
  E[N] := 0;
  IDIR := 0;

  //
  // Get machine constants
  //
  EPS := MachineEpsilon;
  UNFL := MinRealNumber;

  //
  // If matrix lower bidiagonal, rotate to be upper bidiagonal
  // by applying Givens rotations on the left
  //
  if not IsUpper then
    begin
      I := 1;
      while I <= N - 1 do
        begin
          GenerateRotation(D[I], E[I], CS, SN, R);
          D[I] := R;
          E[I] := SN * D[I + 1];
          D[I + 1] := CS * D[I + 1];
          WORK0[I] := CS;
          WORK1[I] := SN;
          Inc(I);
        end;

      //
      // Update singular vectors if desired
      //
      if NRU > 0 then
        begin
          ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, 1 + UStart - 1,
            N + UStart - 1, WORK0, WORK1, U, UTemp);
        end;
      if NCC > 0 then
        begin
          ApplyRotationsFromTheLeft(FwdDir, 1 + CStart - 1, N + CStart - 1, CStart,
            CEnd, WORK0, WORK1, C, CTemp);
        end;
    end;

  //
  // Compute singular values to relative accuracy TOL
  // (By setting TOL to be negative, algorithm will compute
  // singular values to absolute accuracy ABS(TOL)*norm(input matrix))
  //
  TOLMUL := Max(10, Min(100, Power(EPS, -0.125)));
  TOL := TOLMUL * EPS;
  if not IsFractionalAccuracyRequired then
    begin
      TOL := -TOL;
    end;

  //
  // Compute approximate maximum, minimum singular values
  //
  SMAX := 0;
  I := 1;
  while I <= N do
    begin
      SMAX := Max(SMAX, ABSReal(D[I]));
      Inc(I);
    end;
  I := 1;
  while I <= N - 1 do
    begin
      SMAX := Max(SMAX, ABSReal(E[I]));
      Inc(I);
    end;
  SMINL := 0;
  if AP_FP_Greater_Eq(TOL, 0) then
    begin

      //
      // Relative accuracy desired
      //
      SMINOA := ABSReal(D[1]);
      if AP_FP_Neq(SMINOA, 0) then
        begin
          MU := SMINOA;
          I := 2;
          while I <= N do
            begin
              MU := ABSReal(D[I]) * (MU / (MU + ABSReal(E[I - 1])));
              SMINOA := Min(SMINOA, MU);
              if AP_FP_Eq(SMINOA, 0) then
                begin
                  Break;
                end;
              Inc(I);
            end;
        end;
      SMINOA := SMINOA / SQRT(N);
      THRESH := Max(TOL * SMINOA, MAXITR * N * N * UNFL);
    end
  else
    begin

      //
      // Absolute accuracy desired
      //
      THRESH := Max(ABSReal(TOL) * SMAX, MAXITR * N * N * UNFL);
    end;

  //
  // Prepare for main iteration loop for the singular values
  // (MAXIT is the maximum number of passes through the inner
  // loop permitted before nonconvergence signalled.)
  //
  MAXIT := MAXITR * N * N;
  ITER := 0;
  OLDLL := -1;
  OLDM := -1;

  //
  // M points to last element of unconverged part of matrix
  //
  M := N;

  //
  // Begin main iteration loop
  //
  while True do
    begin

      //
      // Check for convergence or exceeding iteration count
      //
      if M <= 1 then
        begin
          Break;
        end;
      if ITER > MAXIT then
        begin
          Result := False;
          Exit;
        end;

      //
      // Find diagonal block of matrix to work on
      //
      if AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(ABSReal(D[M]), THRESH) then
        begin
          D[M] := 0;
        end;
      SMAX := ABSReal(D[M]);
      SMIN := SMAX;
      MatrixSplitFlag := False;
      LLL := 1;
      while LLL <= M - 1 do
        begin
          LL := M - LLL;
          ABSS := ABSReal(D[LL]);
          ABSE := ABSReal(E[LL]);
          if AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(ABSS, THRESH) then
            begin
              D[LL] := 0;
            end;
          if AP_FP_Less_Eq(ABSE, THRESH) then
            begin
              MatrixSplitFlag := True;
              Break;
            end;
          SMIN := Min(SMIN, ABSS);
          SMAX := Max(SMAX, Max(ABSS, ABSE));
          Inc(LLL);
        end;
      if not MatrixSplitFlag then
        begin
          LL := 0;
        end
      else
        begin

          //
          // Matrix splits since E(LL) = 0
          //
          E[LL] := 0;
          if LL = M - 1 then
            begin

              //
              // Convergence of bottom singular value, return to top of loop
              //
              M := M - 1;
              Continue;
            end;
        end;
      LL := LL + 1;

      //
      // E(LL) through E(M-1) are nonzero, E(LL-1) is zero
      //
      if LL = M - 1 then
        begin

          //
          // 2 by 2 block, handle separately
          //
          SvdV2X2(D[M - 1], E[M - 1], D[M], SIGMN, SIGMX, SINR, COSR, SINL, COSL);
          D[M - 1] := SIGMX;
          E[M - 1] := 0;
          D[M] := SIGMN;

          //
          // Compute singular vectors, if desired
          //
          if NCVT > 0 then
            begin
              MM0 := M + (VStart - 1);
              MM1 := M - 1 + (VStart - 1);
              APVMove(@VTTemp[0], VStart, VEnd, @VT[MM1][0], VStart, VEnd, COSR);
              APVAdd(@VTTemp[0], VStart, VEnd, @VT[MM0][0], VStart, VEnd, SINR);
              APVMul(@VT[MM0][0], VStart, VEnd, COSR);
              APVSub(@VT[MM0][0], VStart, VEnd, @VT[MM1][0], VStart, VEnd, SINR);
              APVMove(@VT[MM1][0], VStart, VEnd, @VTTemp[0], VStart, VEnd);
            end;
          if NRU > 0 then
            begin
              MM0 := M + UStart - 1;
              MM1 := M - 1 + UStart - 1;
              for i_ := UStart to UEnd do
                begin
                  UTemp[i_] := COSL * U[i_, MM1];
                end;
              for i_ := UStart to UEnd do
                begin
                  UTemp[i_] := UTemp[i_] + SINL * U[i_, MM0];
                end;
              for i_ := UStart to UEnd do
                begin
                  U[i_, MM0] := COSL * U[i_, MM0];
                end;
              for i_ := UStart to UEnd do
                begin
                  U[i_, MM0] := U[i_, MM0] - SINL * U[i_, MM1];
                end;
              for i_ := UStart to UEnd do
                begin
                  U[i_, MM1] := UTemp[i_];
                end;
            end;
          if NCC > 0 then
            begin
              MM0 := M + CStart - 1;
              MM1 := M - 1 + CStart - 1;
              APVMove(@CTemp[0], CStart, CEnd, @C[MM1][0], CStart, CEnd, COSL);
              APVAdd(@CTemp[0], CStart, CEnd, @C[MM0][0], CStart, CEnd, SINL);
              APVMul(@C[MM0][0], CStart, CEnd, COSL);
              APVSub(@C[MM0][0], CStart, CEnd, @C[MM1][0], CStart, CEnd, SINL);
              APVMove(@C[MM1][0], CStart, CEnd, @CTemp[0], CStart, CEnd);
            end;
          M := M - 2;
          Continue;
        end;

      //
      // If working on new submatrix, choose shift direction
      // (from larger end diagonal element towards smaller)
      //
      // Previously was
      // "if (LL>OLDM) or (M<OLDLL) then"
      // fixed thanks to Michael Rolle < m@rolle.name >
      // Very strange that LAPACK still contains it.
      //
      BChangeDir := False;
      if (IDIR = 1) and AP_FP_Less(ABSReal(D[LL]), 1.0E-3 * ABSReal(D[M])) then
        begin
          BChangeDir := True;
        end;
      if (IDIR = 2) and AP_FP_Less(ABSReal(D[M]), 1.0E-3 * ABSReal(D[LL])) then
        begin
          BChangeDir := True;
        end;
      if (LL <> OLDLL) or (M <> OLDM) or BChangeDir then
        begin
          if AP_FP_Greater_Eq(ABSReal(D[LL]), ABSReal(D[M])) then
            begin

              //
              // Chase bulge from top (big end) to bottom (small end)
              //
              IDIR := 1;
            end
          else
            begin

              //
              // Chase bulge from bottom (big end) to top (small end)
              //
              IDIR := 2;
            end;
        end;

      //
      // Apply convergence tests
      //
      if IDIR = 1 then
        begin

          //
          // Run convergence test in forward direction
          // First apply standard test to bottom of matrix
          //
          if AP_FP_Less_Eq(ABSReal(E[M - 1]), ABSReal(TOL) * ABSReal(D[M])) or
            AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(ABSReal(E[M - 1]), THRESH) then
            begin
              E[M - 1] := 0;
              Continue;
            end;
          if AP_FP_Greater_Eq(TOL, 0) then
            begin

              //
              // If relative accuracy desired,
              // apply convergence criterion forward
              //
              MU := ABSReal(D[LL]);
              SMINL := MU;
              IterFlag := False;
              LLL := LL;
              while LLL <= M - 1 do
                begin
                  if AP_FP_Less_Eq(ABSReal(E[LLL]), TOL * MU) then
                    begin
                      E[LLL] := 0;
                      IterFlag := True;
                      Break;
                    end;
                  SMINLO := SMINL;
                  MU := ABSReal(D[LLL + 1]) * (MU / (MU + ABSReal(E[LLL])));
                  SMINL := Min(SMINL, MU);
                  Inc(LLL);
                end;
              if IterFlag then
                begin
                  Continue;
                end;
            end;
        end
      else
        begin

          //
          // Run convergence test in backward direction
          // First apply standard test to top of matrix
          //
          if AP_FP_Less_Eq(ABSReal(E[LL]), ABSReal(TOL) * ABSReal(D[LL])) or
            AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(ABSReal(E[LL]), THRESH) then
            begin
              E[LL] := 0;
              Continue;
            end;
          if AP_FP_Greater_Eq(TOL, 0) then
            begin

              //
              // If relative accuracy desired,
              // apply convergence criterion backward
              //
              MU := ABSReal(D[M]);
              SMINL := MU;
              IterFlag := False;
              LLL := M - 1;
              while LLL >= LL do
                begin
                  if AP_FP_Less_Eq(ABSReal(E[LLL]), TOL * MU) then
                    begin
                      E[LLL] := 0;
                      IterFlag := True;
                      Break;
                    end;
                  SMINLO := SMINL;
                  MU := ABSReal(D[LLL]) * (MU / (MU + ABSReal(E[LLL])));
                  SMINL := Min(SMINL, MU);
                  Dec(LLL);
                end;
              if IterFlag then
                begin
                  Continue;
                end;
            end;
        end;
      OLDLL := LL;
      OLDM := M;

      //
      // Compute shift.  First, test if shifting would ruin relative
      // accuracy, and if so set the shift to zero.
      //
      if AP_FP_Greater_Eq(TOL, 0) and AP_FP_Less_Eq(N * TOL * (SMINL / SMAX),
        Max(EPS, 0.01 * TOL)) then
        begin

          //
          // Use a zero shift to avoid loss of relative accuracy
          //
          SHIFT := 0;
        end
      else
        begin

          //
          // Compute the shift from 2-by-2 block at end of matrix
          //
          if IDIR = 1 then
            begin
              SLL := ABSReal(D[LL]);
              Svd2X2(D[M - 1], E[M - 1], D[M], SHIFT, R);
            end
          else
            begin
              SLL := ABSReal(D[M]);
              Svd2X2(D[LL], E[LL], D[LL + 1], SHIFT, R);
            end;

          //
          // Test if shift negligible, and if so set to zero
          //
          if AP_FP_Greater(SLL, 0) then
            begin
              if AP_FP_Less(AP_Sqr(SHIFT / SLL), EPS) then
                begin
                  SHIFT := 0;
                end;
            end;
        end;

      //
      // Increment iteration count
      //
      ITER := ITER + M - LL;

      //
      // If SHIFT = 0, do simplified QR iteration
      //
      if AP_FP_Eq(SHIFT, 0) then
        begin
          if IDIR = 1 then
            begin

              //
              // Chase bulge from top to bottom
              // Save cosines and sines for later singular vector updates
              //
              CS := 1;
              OLDCS := 1;
              I := LL;
              while I <= M - 1 do
                begin
                  GenerateRotation(D[I] * CS, E[I], CS, SN, R);
                  if I > LL then
                    begin
                      E[I - 1] := OLDSN * R;
                    end;
                  GenerateRotation(OLDCS * R, D[I + 1] * SN, OLDCS, OLDSN, Tmp);
                  D[I] := Tmp;
                  WORK0[I - LL + 1] := CS;
                  WORK1[I - LL + 1] := SN;
                  WORK2[I - LL + 1] := OLDCS;
                  WORK3[I - LL + 1] := OLDSN;
                  Inc(I);
                end;
              H := D[M] * CS;
              D[M] := H * OLDCS;
              E[M - 1] := H * OLDSN;

              //
              // Update singular vectors
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, WORK0, WORK1, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, WORK2, WORK3, U, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, WORK2, WORK3, C, CTemp);
                end;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(ABSReal(E[M - 1]), THRESH) then
                begin
                  E[M - 1] := 0;
                end;
            end
          else
            begin

              //
              // Chase bulge from bottom to top
              // Save cosines and sines for later singular vector updates
              //
              CS := 1;
              OLDCS := 1;
              I := M;
              while I >= LL + 1 do
                begin
                  GenerateRotation(D[I] * CS, E[I - 1], CS, SN, R);
                  if I < M then
                    begin
                      E[I] := OLDSN * R;
                    end;
                  GenerateRotation(OLDCS * R, D[I - 1] * SN, OLDCS, OLDSN, Tmp);
                  D[I] := Tmp;
                  WORK0[I - LL] := CS;
                  WORK1[I - LL] := -SN;
                  WORK2[I - LL] := OLDCS;
                  WORK3[I - LL] := -OLDSN;
                  Dec(I);
                end;
              H := D[LL] * CS;
              D[LL] := H * OLDCS;
              E[LL] := H * OLDSN;

              //
              // Update singular vectors
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, WORK2, WORK3, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(not FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, WORK0, WORK1, U, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, WORK0, WORK1, C, CTemp);
                end;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(ABSReal(E[LL]), THRESH) then
                begin
                  E[LL] := 0;
                end;
            end;
        end
      else
        begin

          //
          // Use nonzero shift
          //
          if IDIR = 1 then
            begin

              //
              // Chase bulge from top to bottom
              // Save cosines and sines for later singular vector updates
              //
              F := (ABSReal(D[LL]) - SHIFT) *
                (ExtSignBDSQR(1, D[LL]) + SHIFT / D[LL]);
              G := E[LL];
              I := LL;
              while I <= M - 1 do
                begin
                  GenerateRotation(F, G, COSR, SINR, R);
                  if I > LL then
                    begin
                      E[I - 1] := R;
                    end;
                  F := COSR * D[I] + SINR * E[I];
                  E[I] := COSR * E[I] - SINR * D[I];
                  G := SINR * D[I + 1];
                  D[I + 1] := COSR * D[I + 1];
                  GenerateRotation(F, G, COSL, SINL, R);
                  D[I] := R;
                  F := COSL * E[I] + SINL * D[I + 1];
                  D[I + 1] := COSL * D[I + 1] - SINL * E[I];
                  if I < M - 1 then
                    begin
                      G := SINL * E[I + 1];
                      E[I + 1] := COSL * E[I + 1];
                    end;
                  WORK0[I - LL + 1] := COSR;
                  WORK1[I - LL + 1] := SINR;
                  WORK2[I - LL + 1] := COSL;
                  WORK3[I - LL + 1] := SINL;
                  Inc(I);
                end;
              E[M - 1] := F;

              //
              // Update singular vectors
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, WORK0, WORK1, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, WORK2, WORK3, U, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, WORK2, WORK3, C, CTemp);
                end;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(ABSReal(E[M - 1]), THRESH) then
                begin
                  E[M - 1] := 0;
                end;
            end
          else
            begin

              //
              // Chase bulge from bottom to top
              // Save cosines and sines for later singular vector updates
              //
              F := (ABSReal(D[M]) - SHIFT) * (ExtSignBDSQR(1, D[M]) + SHIFT / D[M]);
              G := E[M - 1];
              I := M;
              while I >= LL + 1 do
                begin
                  GenerateRotation(F, G, COSR, SINR, R);
                  if I < M then
                    begin
                      E[I] := R;
                    end;
                  F := COSR * D[I] + SINR * E[I - 1];
                  E[I - 1] := COSR * E[I - 1] - SINR * D[I];
                  G := SINR * D[I - 1];
                  D[I - 1] := COSR * D[I - 1];
                  GenerateRotation(F, G, COSL, SINL, R);
                  D[I] := R;
                  F := COSL * E[I - 1] + SINL * D[I - 1];
                  D[I - 1] := COSL * D[I - 1] - SINL * E[I - 1];
                  if I > LL + 1 then
                    begin
                      G := SINL * E[I - 2];
                      E[I - 2] := COSL * E[I - 2];
                    end;
                  WORK0[I - LL] := COSR;
                  WORK1[I - LL] := -SINR;
                  WORK2[I - LL] := COSL;
                  WORK3[I - LL] := -SINL;
                  Dec(I);
                end;
              E[LL] := F;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(ABSReal(E[LL]), THRESH) then
                begin
                  E[LL] := 0;
                end;

              //
              // Update singular vectors if desired
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, WORK2, WORK3, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(not FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, WORK0, WORK1, U, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, WORK0, WORK1, C, CTemp);
                end;
            end;
        end;

      //
      // QR iteration finished, go back and check convergence
      //
      Continue;
    end;

  //
  // All singular values converged, so make them positive
  //
  I := 1;
  while I <= N do
    begin
      if AP_FP_Less(D[I], 0) then
        begin
          D[I] := -D[I];

          //
          // Change sign of singular vectors, if desired
          //
          if NCVT > 0 then
            begin
              APVMul(@VT[I + VStart - 1][0], VStart, VEnd, -1);
            end;
        end;
      Inc(I);
    end;

  //
  // Sort the singular values into decreasing order (insertion sort on
  // singular values, but only one transposition per singular vector)
  //
  I := 1;
  while I <= N - 1 do
    begin

      //
      // Scan for smallest D(I)
      //
      ISUB := 1;
      SMIN := D[1];
      J := 2;
      while J <= N + 1 - I do
        begin
          if AP_FP_Less_Eq(D[J], SMIN) then
            begin
              ISUB := J;
              SMIN := D[J];
            end;
          Inc(J);
        end;
      if ISUB <> N + 1 - I then
        begin

          //
          // Swap singular values and vectors
          //
          D[ISUB] := D[N + 1 - I];
          D[N + 1 - I] := SMIN;
          if NCVT > 0 then
            begin
              J := N + 1 - I;
              APVMove(@VTTemp[0], VStart, VEnd, @VT[ISUB + VStart - 1][0],
                VStart, VEnd);
              APVMove(@VT[ISUB + VStart - 1][0], VStart, VEnd, @VT[J + VStart - 1][0],
                VStart, VEnd);
              APVMove(@VT[J + VStart - 1][0], VStart, VEnd, @VTTemp[0], VStart, VEnd);
            end;
          if NRU > 0 then
            begin
              J := N + 1 - I;
              for i_ := UStart to UEnd do
                begin
                  UTemp[i_] := U[i_, ISUB + UStart - 1];
                end;
              for i_ := UStart to UEnd do
                begin
                  U[i_, ISUB + UStart - 1] := U[i_, J + UStart - 1];
                end;
              for i_ := UStart to UEnd do
                begin
                  U[i_, J + UStart - 1] := UTemp[i_];
                end;
            end;
          if NCC > 0 then
            begin
              J := N + 1 - I;
              APVMove(@CTemp[0], CStart, CEnd, @C[ISUB + CStart - 1][0],
                CStart, CEnd);
              APVMove(@C[ISUB + CStart - 1][0], CStart, CEnd, @C[J + CStart - 1][0],
                CStart, CEnd);
              APVMove(@C[J + CStart - 1][0], CStart, CEnd, @CTemp[0], CStart, CEnd);
            end;
        end;
      Inc(I);
    end;
end;

function ExtSignBDSQR(a: TLFloat; b: TLFloat): TLFloat;
begin
  if AP_FP_Greater_Eq(b, 0) then
    begin
      Result := ABSReal(a);
    end
  else
    begin
      Result := -ABSReal(a);
    end;
end;

procedure Svd2X2(F: TLFloat; G: TLFloat; H: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat);
var
  AAS : TLFloat;
  AT  : TLFloat;
  AU  : TLFloat;
  C   : TLFloat;
  FA  : TLFloat;
  FHMN: TLFloat;
  FHMX: TLFloat;
  GA  : TLFloat;
  HA  : TLFloat;
begin
  FA := ABSReal(F);
  GA := ABSReal(G);
  HA := ABSReal(H);
  FHMN := Min(FA, HA);
  FHMX := Max(FA, HA);
  if AP_FP_Eq(FHMN, 0) then
    begin
      SSMIN := 0;
      if AP_FP_Eq(FHMX, 0) then
        begin
          SSMAX := GA;
        end
      else
        begin
          SSMAX := Max(FHMX, GA) * SQRT(1 + AP_Sqr(Min(FHMX, GA) / Max(FHMX, GA)));
        end;
    end
  else
    begin
      if AP_FP_Less(GA, FHMX) then
        begin
          AAS := 1 + FHMN / FHMX;
          AT := (FHMX - FHMN) / FHMX;
          AU := AP_Sqr(GA / FHMX);
          C := 2 / (SQRT(AAS * AAS + AU) + SQRT(AT * AT + AU));
          SSMIN := FHMN * C;
          SSMAX := FHMX / C;
        end
      else
        begin
          AU := FHMX / GA;
          if AP_FP_Eq(AU, 0) then
            begin

              //
              // Avoid possible harmful underflow if exponent range
              // asymmetric (true SSMIN may not underflow even if
              // AU underflows)
              //
              SSMIN := FHMN * FHMX / GA;
              SSMAX := GA;
            end
          else
            begin
              AAS := 1 + FHMN / FHMX;
              AT := (FHMX - FHMN) / FHMX;
              C := 1 / (SQRT(1 + AP_Sqr(AAS * AU)) + SQRT(1 + AP_Sqr(AT * AU)));
              SSMIN := FHMN * C * AU;
              SSMIN := SSMIN + SSMIN;
              SSMAX := GA / (C + C);
            end;
        end;
    end;
end;

procedure SvdV2X2(F: TLFloat; G: TLFloat; H: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat; var SNR: TLFloat;
  var CSR: TLFloat; var SNL: TLFloat; var CSL: TLFloat);
var
  GASMAL: Boolean;
  SWP   : Boolean;
  PMAX  : TLInt;
  a     : TLFloat;
  CLT   : TLFloat;
  CRT   : TLFloat;
  D     : TLFloat;
  FA    : TLFloat;
  FT    : TLFloat;
  GA    : TLFloat;
  GT    : TLFloat;
  HA    : TLFloat;
  HT    : TLFloat;
  L     : TLFloat;
  M     : TLFloat;
  MM    : TLFloat;
  R     : TLFloat;
  S     : TLFloat;
  SLT   : TLFloat;
  SRT   : TLFloat;
  T     : TLFloat;
  TEMP  : TLFloat;
  TSIGN : TLFloat;
  TT    : TLFloat;
  V     : TLFloat;
begin
  FT := F;
  FA := ABSReal(FT);
  HT := H;
  HA := ABSReal(H);

  //
  // PMAX points to the maximum absolute element of matrix
  // PMAX = 1 if F largest in absolute values
  // PMAX = 2 if G largest in absolute values
  // PMAX = 3 if H largest in absolute values
  //
  PMAX := 1;
  SWP := AP_FP_Greater(HA, FA);
  if SWP then
    begin

      //
      // Now FA .ge. HA
      //
      PMAX := 3;
      TEMP := FT;
      FT := HT;
      HT := TEMP;
      TEMP := FA;
      FA := HA;
      HA := TEMP;
    end;
  GT := G;
  GA := ABSReal(GT);
  if AP_FP_Eq(GA, 0) then
    begin

      //
      // Diagonal matrix
      //
      SSMIN := HA;
      SSMAX := FA;
      CLT := 1;
      CRT := 1;
      SLT := 0;
      SRT := 0;
    end
  else
    begin
      GASMAL := True;
      if AP_FP_Greater(GA, FA) then
        begin
          PMAX := 2;
          if AP_FP_Less(FA / GA, MachineEpsilon) then
            begin

              //
              // Case of very large GA
              //
              GASMAL := False;
              SSMAX := GA;
              if AP_FP_Greater(HA, 1) then
                begin
                  V := GA / HA;
                  SSMIN := FA / V;
                end
              else
                begin
                  V := FA / GA;
                  SSMIN := V * HA;
                end;
              CLT := 1;
              SLT := HT / GT;
              SRT := 1;
              CRT := FT / GT;
            end;
        end;
      if GASMAL then
        begin

          //
          // Normal case
          //
          D := FA - HA;
          if AP_FP_Eq(D, FA) then
            begin
              L := 1;
            end
          else
            begin
              L := D / FA;
            end;
          M := GT / FT;
          T := 2 - L;
          MM := M * M;
          TT := T * T;
          S := SQRT(TT + MM);
          if AP_FP_Eq(L, 0) then
            begin
              R := ABSReal(M);
            end
          else
            begin
              R := SQRT(L * L + MM);
            end;
          a := 0.5 * (S + R);
          SSMIN := HA / a;
          SSMAX := FA * a;
          if AP_FP_Eq(MM, 0) then
            begin

              //
              // Note that M is very tiny
              //
              if AP_FP_Eq(L, 0) then
                begin
                  T := ExtSignBDSQR(2, FT) * ExtSignBDSQR(1, GT);
                end
              else
                begin
                  T := GT / ExtSignBDSQR(D, FT) + M / T;
                end;
            end
          else
            begin
              T := (M / (S + T) + M / (R + L)) * (1 + a);
            end;
          L := SQRT(T * T + 4);
          CRT := 2 / L;
          SRT := T / L;
          CLT := (CRT + SRT * M) / a;
          V := HT / FT;
          SLT := V * SRT / a;
        end;
    end;
  if SWP then
    begin
      CSL := SRT;
      SNL := CRT;
      CSR := SLT;
      SNR := CLT;
    end
  else
    begin
      CSL := CLT;
      SNL := SLT;
      CSR := CRT;
      SNR := SRT;
    end;

  //
  // Correct signs of SSMAX and SSMIN
  //
  if PMAX = 1 then
    begin
      TSIGN := ExtSignBDSQR(1, CSR) * ExtSignBDSQR(1, CSL) * ExtSignBDSQR(1, F);
    end;
  if PMAX = 2 then
    begin
      TSIGN := ExtSignBDSQR(1, SNR) * ExtSignBDSQR(1, CSL) * ExtSignBDSQR(1, G);
    end;
  if PMAX = 3 then
    begin
      TSIGN := ExtSignBDSQR(1, SNR) * ExtSignBDSQR(1, SNL) * ExtSignBDSQR(1, H);
    end;
  SSMAX := ExtSignBDSQR(SSMAX, TSIGN);
  SSMIN := ExtSignBDSQR(SSMIN, TSIGN * ExtSignBDSQR(1, F) * ExtSignBDSQR(1, H));
end;
