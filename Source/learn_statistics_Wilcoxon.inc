{ ****************************************************************************** }
{ *  Wilcoxon signed-rank test support, by QQ 600585@qq.com                    * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

procedure WCheb(const X, C: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var R: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W5(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W6(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W7(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W8(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W9(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W10(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W11(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W12(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W13(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W14(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W15(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W16(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W17(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W18(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W19(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W20(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W21(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W22(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W23(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W24(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W25(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W26(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W27(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W28(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W29(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W30(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W40(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W60(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W120(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function W200(const S: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function WSigma(const S: TLFloat; const N: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Wilcoxon signed-rank test

  This test checks three hypotheses about the median of the given sample.
  The following tests are performed:
  * two-tailed test (null hypothesis - the median is equal to the given value)
  * left-tailed test (null hypothesis - the median is greater than or equal to the given value)
  * right-tailed test (null hypothesis - the median is less than or equal to the given value)

  Requirements:
  * the scale of measurement should be ordinal, interval or ratio (i.e. the test could not be applied to nominal variables).
  * the distribution should be continuous and symmetric relative to its median.
  * number of distinct values in the X array should be greater than 4

  The test is non-parametric and doesn't require distribution X to be normal

  Input parameters:
  X       -   sample. Array whose index goes from 0 to N-1.
  N       -   size of the sample.
  E       -   assumed median value.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.

  To calculate p-values, special approximation is used. This method lets us calculate p-values with two decimal places in interval [0.0001, 1].
  "Two decimal places" does not sound very impressive, but in practice the relative error of less than 1% is enough to make a decision.
  There is no approximation outside the [0.0001, 1] interval. Therefore,  if the significance level outlies this interval, the test returns 0.0001.
  ************************************************************************ *)
procedure WilcoxonSignedRankTest(const X: TLVec; N: TLInt; E: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
var
  I    : TLInt;
  J    : TLInt;
  K    : TLInt;
  T    : TLInt;
  Tmp  : TLFloat;
  TmpI : TLInt;
  NS   : TLInt;
  R    : TLVec;
  C    : TLIVec;
  W    : TLFloat;
  P    : TLFloat;
  MP   : TLFloat;
  S    : TLFloat;
  Sigma: TLFloat;
  Mu   : TLFloat;
begin
  //
  // Prepare
  //
  if N < 5 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;
  NS := 0;
  I := 0;
  while I <= N - 1 do
    begin
      if AP_FP_Eq(X[I], E) then
        begin
          Inc(I);
          Continue;
        end;
      X[NS] := X[I];
      NS := NS + 1;
      Inc(I);
    end;
  if NS < 5 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;
  SetLength(R, NS (* - 1 + 1 // optimized compiler *) );
  SetLength(C, NS (* - 1 + 1 // optimized compiler *) );
  I := 0;
  while I <= NS - 1 do
    begin
      R[I] := AbsReal(X[I] - E);
      C[I] := I;
      Inc(I);
    end;

  //
  // sort {R, C}
  //
  if NS <> 1 then
    begin
      I := 2;
      repeat
        T := I;
        while T <> 1 do
          begin
            K := T div 2;
            if AP_FP_Greater_Eq(R[K - 1], R[T - 1]) then
              begin
                T := 1;
              end
            else
              begin
                Tmp := R[K - 1];
                R[K - 1] := R[T - 1];
                R[T - 1] := Tmp;
                TmpI := C[K - 1];
                C[K - 1] := C[T - 1];
                C[T - 1] := TmpI;
                T := K;
              end;
          end;
        I := I + 1;
      until not(I <= NS);
      I := NS - 1;
      repeat
        Tmp := R[I];
        R[I] := R[0];
        R[0] := Tmp;
        TmpI := C[I];
        C[I] := C[0];
        C[0] := TmpI;
        T := 1;
        while T <> 0 do
          begin
            K := 2 * T;
            if K > I then
              begin
                T := 0;
              end
            else
              begin
                if K < I then
                  begin
                    if AP_FP_Greater(R[K], R[K - 1]) then
                      begin
                        K := K + 1;
                      end;
                  end;
                if AP_FP_Greater_Eq(R[T - 1], R[K - 1]) then
                  begin
                    T := 0;
                  end
                else
                  begin
                    Tmp := R[K - 1];
                    R[K - 1] := R[T - 1];
                    R[T - 1] := Tmp;
                    TmpI := C[K - 1];
                    C[K - 1] := C[T - 1];
                    C[T - 1] := TmpI;
                    T := K;
                  end;
              end;
          end;
        I := I - 1;
      until not(I >= 1);
    end;

  //
  // compute tied ranks
  //
  I := 0;
  while I <= NS - 1 do
    begin
      J := I + 1;
      while J <= NS - 1 do
        begin
          if AP_FP_Neq(R[J], R[I]) then
            begin
              Break;
            end;
          J := J + 1;
        end;
      K := I;
      while K <= J - 1 do
        begin
          R[K] := 1 + AP_Float((I + J - 1)) / 2;
          Inc(K);
        end;
      I := J;
    end;

  //
  // Compute W+
  //
  W := 0;
  I := 0;
  while I <= NS - 1 do
    begin
      if AP_FP_Greater(X[C[I]], E) then
        begin
          W := W + R[I];
        end;
      Inc(I);
    end;

  //
  // Result
  //
  Mu := AP_Float(NS * (NS + 1)) / 4;
  Sigma := Sqrt(AP_Float(NS * (NS + 1) * (2 * NS + 1)) / 24);
  S := (W - Mu) / Sigma;
  if AP_FP_Less_Eq(S, 0) then
    begin
      P := Exp(WSigma(-(W - Mu) / Sigma, NS));
      MP := 1 - Exp(WSigma(-(W - 1 - Mu) / Sigma, NS));
    end
  else
    begin
      MP := Exp(WSigma((W - Mu) / Sigma, NS));
      P := 1 - Exp(WSigma((W + 1 - Mu) / Sigma, NS));
    end;
  BothTails := Max(2 * Min(P, MP), 1.0E-4);
  LeftTail := Max(P, 1.0E-4);
  RightTail := Max(MP, 1.0E-4);
end;

(* ************************************************************************
  Sequential Chebyshev interpolation.
  ************************************************************************ *)
procedure WCheb(const X, C: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var R: TLFloat);
var
  T: TLFloat;
begin
  R := R + C * TJ;
  T := 2 * X * TJ1 - TJ;
  TJ := TJ1;
  TJ1 := T;
end;

(* ************************************************************************
  Tail(S, 5)
  ************************************************************************ *)
function W5(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-3.708099E+00 * S + 7.500000E+00);
  if W >= 7 then
    begin
      R := -6.931E-01;
    end;
  if W = 6 then
    begin
      R := -9.008E-01;
    end;
  if W = 5 then
    begin
      R := -1.163E+00;
    end;
  if W = 4 then
    begin
      R := -1.520E+00;
    end;
  if W = 3 then
    begin
      R := -1.856E+00;
    end;
  if W = 2 then
    begin
      R := -2.367E+00;
    end;
  if W = 1 then
    begin
      R := -2.773E+00;
    end;
  if W <= 0 then
    begin
      R := -3.466E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 6)
  ************************************************************************ *)
function W6(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-4.769696E+00 * S + 1.050000E+01);
  if W >= 10 then
    begin
      R := -6.931E-01;
    end;
  if W = 9 then
    begin
      R := -8.630E-01;
    end;
  if W = 8 then
    begin
      R := -1.068E+00;
    end;
  if W = 7 then
    begin
      R := -1.269E+00;
    end;
  if W = 6 then
    begin
      R := -1.520E+00;
    end;
  if W = 5 then
    begin
      R := -1.856E+00;
    end;
  if W = 4 then
    begin
      R := -2.213E+00;
    end;
  if W = 3 then
    begin
      R := -2.549E+00;
    end;
  if W = 2 then
    begin
      R := -3.060E+00;
    end;
  if W = 1 then
    begin
      R := -3.466E+00;
    end;
  if W <= 0 then
    begin
      R := -4.159E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 7)
  ************************************************************************ *)
function W7(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-5.916080E+00 * S + 1.400000E+01);
  if W >= 14 then
    begin
      R := -6.325E-01;
    end;
  if W = 13 then
    begin
      R := -7.577E-01;
    end;
  if W = 12 then
    begin
      R := -9.008E-01;
    end;
  if W = 11 then
    begin
      R := -1.068E+00;
    end;
  if W = 10 then
    begin
      R := -1.241E+00;
    end;
  if W = 9 then
    begin
      R := -1.451E+00;
    end;
  if W = 8 then
    begin
      R := -1.674E+00;
    end;
  if W = 7 then
    begin
      R := -1.908E+00;
    end;
  if W = 6 then
    begin
      R := -2.213E+00;
    end;
  if W = 5 then
    begin
      R := -2.549E+00;
    end;
  if W = 4 then
    begin
      R := -2.906E+00;
    end;
  if W = 3 then
    begin
      R := -3.243E+00;
    end;
  if W = 2 then
    begin
      R := -3.753E+00;
    end;
  if W = 1 then
    begin
      R := -4.159E+00;
    end;
  if W <= 0 then
    begin
      R := -4.852E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 8)
  ************************************************************************ *)
function W8(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-7.141428E+00 * S + 1.800000E+01);
  if W >= 18 then
    begin
      R := -6.399E-01;
    end;
  if W = 17 then
    begin
      R := -7.494E-01;
    end;
  if W = 16 then
    begin
      R := -8.630E-01;
    end;
  if W = 15 then
    begin
      R := -9.913E-01;
    end;
  if W = 14 then
    begin
      R := -1.138E+00;
    end;
  if W = 13 then
    begin
      R := -1.297E+00;
    end;
  if W = 12 then
    begin
      R := -1.468E+00;
    end;
  if W = 11 then
    begin
      R := -1.653E+00;
    end;
  if W = 10 then
    begin
      R := -1.856E+00;
    end;
  if W = 9 then
    begin
      R := -2.079E+00;
    end;
  if W = 8 then
    begin
      R := -2.326E+00;
    end;
  if W = 7 then
    begin
      R := -2.601E+00;
    end;
  if W = 6 then
    begin
      R := -2.906E+00;
    end;
  if W = 5 then
    begin
      R := -3.243E+00;
    end;
  if W = 4 then
    begin
      R := -3.599E+00;
    end;
  if W = 3 then
    begin
      R := -3.936E+00;
    end;
  if W = 2 then
    begin
      R := -4.447E+00;
    end;
  if W = 1 then
    begin
      R := -4.852E+00;
    end;
  if W <= 0 then
    begin
      R := -5.545E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 9)
  ************************************************************************ *)
function W9(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-8.440972E+00 * S + 2.250000E+01);
  if W >= 22 then
    begin
      R := -6.931E-01;
    end;
  if W = 21 then
    begin
      R := -7.873E-01;
    end;
  if W = 20 then
    begin
      R := -8.912E-01;
    end;
  if W = 19 then
    begin
      R := -1.002E+00;
    end;
  if W = 18 then
    begin
      R := -1.120E+00;
    end;
  if W = 17 then
    begin
      R := -1.255E+00;
    end;
  if W = 16 then
    begin
      R := -1.394E+00;
    end;
  if W = 15 then
    begin
      R := -1.547E+00;
    end;
  if W = 14 then
    begin
      R := -1.717E+00;
    end;
  if W = 13 then
    begin
      R := -1.895E+00;
    end;
  if W = 12 then
    begin
      R := -2.079E+00;
    end;
  if W = 11 then
    begin
      R := -2.287E+00;
    end;
  if W = 10 then
    begin
      R := -2.501E+00;
    end;
  if W = 9 then
    begin
      R := -2.742E+00;
    end;
  if W = 8 then
    begin
      R := -3.019E+00;
    end;
  if W = 7 then
    begin
      R := -3.294E+00;
    end;
  if W = 6 then
    begin
      R := -3.599E+00;
    end;
  if W = 5 then
    begin
      R := -3.936E+00;
    end;
  if W = 4 then
    begin
      R := -4.292E+00;
    end;
  if W = 3 then
    begin
      R := -4.629E+00;
    end;
  if W = 2 then
    begin
      R := -5.140E+00;
    end;
  if W = 1 then
    begin
      R := -5.545E+00;
    end;
  if W <= 0 then
    begin
      R := -6.238E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 10)
  ************************************************************************ *)
function W10(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-9.810708E+00 * S + 2.750000E+01);
  if W >= 27 then
    begin
      R := -6.931E-01;
    end;
  if W = 26 then
    begin
      R := -7.745E-01;
    end;
  if W = 25 then
    begin
      R := -8.607E-01;
    end;
  if W = 24 then
    begin
      R := -9.551E-01;
    end;
  if W = 23 then
    begin
      R := -1.057E+00;
    end;
  if W = 22 then
    begin
      R := -1.163E+00;
    end;
  if W = 21 then
    begin
      R := -1.279E+00;
    end;
  if W = 20 then
    begin
      R := -1.402E+00;
    end;
  if W = 19 then
    begin
      R := -1.533E+00;
    end;
  if W = 18 then
    begin
      R := -1.674E+00;
    end;
  if W = 17 then
    begin
      R := -1.826E+00;
    end;
  if W = 16 then
    begin
      R := -1.983E+00;
    end;
  if W = 15 then
    begin
      R := -2.152E+00;
    end;
  if W = 14 then
    begin
      R := -2.336E+00;
    end;
  if W = 13 then
    begin
      R := -2.525E+00;
    end;
  if W = 12 then
    begin
      R := -2.727E+00;
    end;
  if W = 11 then
    begin
      R := -2.942E+00;
    end;
  if W = 10 then
    begin
      R := -3.170E+00;
    end;
  if W = 9 then
    begin
      R := -3.435E+00;
    end;
  if W = 8 then
    begin
      R := -3.713E+00;
    end;
  if W = 7 then
    begin
      R := -3.987E+00;
    end;
  if W = 6 then
    begin
      R := -4.292E+00;
    end;
  if W = 5 then
    begin
      R := -4.629E+00;
    end;
  if W = 4 then
    begin
      R := -4.986E+00;
    end;
  if W = 3 then
    begin
      R := -5.322E+00;
    end;
  if W = 2 then
    begin
      R := -5.833E+00;
    end;
  if W = 1 then
    begin
      R := -6.238E+00;
    end;
  if W <= 0 then
    begin
      R := -6.931E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 11)
  ************************************************************************ *)
function W11(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-1.124722E+01 * S + 3.300000E+01);
  if W >= 33 then
    begin
      R := -6.595E-01;
    end;
  if W = 32 then
    begin
      R := -7.279E-01;
    end;
  if W = 31 then
    begin
      R := -8.002E-01;
    end;
  if W = 30 then
    begin
      R := -8.782E-01;
    end;
  if W = 29 then
    begin
      R := -9.615E-01;
    end;
  if W = 28 then
    begin
      R := -1.050E+00;
    end;
  if W = 27 then
    begin
      R := -1.143E+00;
    end;
  if W = 26 then
    begin
      R := -1.243E+00;
    end;
  if W = 25 then
    begin
      R := -1.348E+00;
    end;
  if W = 24 then
    begin
      R := -1.459E+00;
    end;
  if W = 23 then
    begin
      R := -1.577E+00;
    end;
  if W = 22 then
    begin
      R := -1.700E+00;
    end;
  if W = 21 then
    begin
      R := -1.832E+00;
    end;
  if W = 20 then
    begin
      R := -1.972E+00;
    end;
  if W = 19 then
    begin
      R := -2.119E+00;
    end;
  if W = 18 then
    begin
      R := -2.273E+00;
    end;
  if W = 17 then
    begin
      R := -2.437E+00;
    end;
  if W = 16 then
    begin
      R := -2.607E+00;
    end;
  if W = 15 then
    begin
      R := -2.788E+00;
    end;
  if W = 14 then
    begin
      R := -2.980E+00;
    end;
  if W = 13 then
    begin
      R := -3.182E+00;
    end;
  if W = 12 then
    begin
      R := -3.391E+00;
    end;
  if W = 11 then
    begin
      R := -3.617E+00;
    end;
  if W = 10 then
    begin
      R := -3.863E+00;
    end;
  if W = 9 then
    begin
      R := -4.128E+00;
    end;
  if W = 8 then
    begin
      R := -4.406E+00;
    end;
  if W = 7 then
    begin
      R := -4.680E+00;
    end;
  if W = 6 then
    begin
      R := -4.986E+00;
    end;
  if W = 5 then
    begin
      R := -5.322E+00;
    end;
  if W = 4 then
    begin
      R := -5.679E+00;
    end;
  if W = 3 then
    begin
      R := -6.015E+00;
    end;
  if W = 2 then
    begin
      R := -6.526E+00;
    end;
  if W = 1 then
    begin
      R := -6.931E+00;
    end;
  if W <= 0 then
    begin
      R := -7.625E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 12)
  ************************************************************************ *)
function W12(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-1.274755E+01 * S + 3.900000E+01);
  if W >= 39 then
    begin
      R := -6.633E-01;
    end;
  if W = 38 then
    begin
      R := -7.239E-01;
    end;
  if W = 37 then
    begin
      R := -7.878E-01;
    end;
  if W = 36 then
    begin
      R := -8.556E-01;
    end;
  if W = 35 then
    begin
      R := -9.276E-01;
    end;
  if W = 34 then
    begin
      R := -1.003E+00;
    end;
  if W = 33 then
    begin
      R := -1.083E+00;
    end;
  if W = 32 then
    begin
      R := -1.168E+00;
    end;
  if W = 31 then
    begin
      R := -1.256E+00;
    end;
  if W = 30 then
    begin
      R := -1.350E+00;
    end;
  if W = 29 then
    begin
      R := -1.449E+00;
    end;
  if W = 28 then
    begin
      R := -1.552E+00;
    end;
  if W = 27 then
    begin
      R := -1.660E+00;
    end;
  if W = 26 then
    begin
      R := -1.774E+00;
    end;
  if W = 25 then
    begin
      R := -1.893E+00;
    end;
  if W = 24 then
    begin
      R := -2.017E+00;
    end;
  if W = 23 then
    begin
      R := -2.148E+00;
    end;
  if W = 22 then
    begin
      R := -2.285E+00;
    end;
  if W = 21 then
    begin
      R := -2.429E+00;
    end;
  if W = 20 then
    begin
      R := -2.581E+00;
    end;
  if W = 19 then
    begin
      R := -2.738E+00;
    end;
  if W = 18 then
    begin
      R := -2.902E+00;
    end;
  if W = 17 then
    begin
      R := -3.076E+00;
    end;
  if W = 16 then
    begin
      R := -3.255E+00;
    end;
  if W = 15 then
    begin
      R := -3.443E+00;
    end;
  if W = 14 then
    begin
      R := -3.645E+00;
    end;
  if W = 13 then
    begin
      R := -3.852E+00;
    end;
  if W = 12 then
    begin
      R := -4.069E+00;
    end;
  if W = 11 then
    begin
      R := -4.310E+00;
    end;
  if W = 10 then
    begin
      R := -4.557E+00;
    end;
  if W = 9 then
    begin
      R := -4.821E+00;
    end;
  if W = 8 then
    begin
      R := -5.099E+00;
    end;
  if W = 7 then
    begin
      R := -5.373E+00;
    end;
  if W = 6 then
    begin
      R := -5.679E+00;
    end;
  if W = 5 then
    begin
      R := -6.015E+00;
    end;
  if W = 4 then
    begin
      R := -6.372E+00;
    end;
  if W = 3 then
    begin
      R := -6.708E+00;
    end;
  if W = 2 then
    begin
      R := -7.219E+00;
    end;
  if W = 1 then
    begin
      R := -7.625E+00;
    end;
  if W <= 0 then
    begin
      R := -8.318E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 13)
  ************************************************************************ *)
function W13(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-1.430909E+01 * S + 4.550000E+01);
  if W >= 45 then
    begin
      R := -6.931E-01;
    end;
  if W = 44 then
    begin
      R := -7.486E-01;
    end;
  if W = 43 then
    begin
      R := -8.068E-01;
    end;
  if W = 42 then
    begin
      R := -8.683E-01;
    end;
  if W = 41 then
    begin
      R := -9.328E-01;
    end;
  if W = 40 then
    begin
      R := -1.001E+00;
    end;
  if W = 39 then
    begin
      R := -1.072E+00;
    end;
  if W = 38 then
    begin
      R := -1.146E+00;
    end;
  if W = 37 then
    begin
      R := -1.224E+00;
    end;
  if W = 36 then
    begin
      R := -1.306E+00;
    end;
  if W = 35 then
    begin
      R := -1.392E+00;
    end;
  if W = 34 then
    begin
      R := -1.481E+00;
    end;
  if W = 33 then
    begin
      R := -1.574E+00;
    end;
  if W = 32 then
    begin
      R := -1.672E+00;
    end;
  if W = 31 then
    begin
      R := -1.773E+00;
    end;
  if W = 30 then
    begin
      R := -1.879E+00;
    end;
  if W = 29 then
    begin
      R := -1.990E+00;
    end;
  if W = 28 then
    begin
      R := -2.104E+00;
    end;
  if W = 27 then
    begin
      R := -2.224E+00;
    end;
  if W = 26 then
    begin
      R := -2.349E+00;
    end;
  if W = 25 then
    begin
      R := -2.479E+00;
    end;
  if W = 24 then
    begin
      R := -2.614E+00;
    end;
  if W = 23 then
    begin
      R := -2.755E+00;
    end;
  if W = 22 then
    begin
      R := -2.902E+00;
    end;
  if W = 21 then
    begin
      R := -3.055E+00;
    end;
  if W = 20 then
    begin
      R := -3.215E+00;
    end;
  if W = 19 then
    begin
      R := -3.380E+00;
    end;
  if W = 18 then
    begin
      R := -3.551E+00;
    end;
  if W = 17 then
    begin
      R := -3.733E+00;
    end;
  if W = 16 then
    begin
      R := -3.917E+00;
    end;
  if W = 15 then
    begin
      R := -4.113E+00;
    end;
  if W = 14 then
    begin
      R := -4.320E+00;
    end;
  if W = 13 then
    begin
      R := -4.534E+00;
    end;
  if W = 12 then
    begin
      R := -4.762E+00;
    end;
  if W = 11 then
    begin
      R := -5.004E+00;
    end;
  if W = 10 then
    begin
      R := -5.250E+00;
    end;
  if W = 9 then
    begin
      R := -5.514E+00;
    end;
  if W = 8 then
    begin
      R := -5.792E+00;
    end;
  if W = 7 then
    begin
      R := -6.066E+00;
    end;
  if W = 6 then
    begin
      R := -6.372E+00;
    end;
  if W = 5 then
    begin
      R := -6.708E+00;
    end;
  if W = 4 then
    begin
      R := -7.065E+00;
    end;
  if W = 3 then
    begin
      R := -7.401E+00;
    end;
  if W = 2 then
    begin
      R := -7.912E+00;
    end;
  if W = 1 then
    begin
      R := -8.318E+00;
    end;
  if W <= 0 then
    begin
      R := -9.011E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 14)
  ************************************************************************ *)
function W14(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-1.592953E+01 * S + 5.250000E+01);
  if W >= 52 then
    begin
      R := -6.931E-01;
    end;
  if W = 51 then
    begin
      R := -7.428E-01;
    end;
  if W = 50 then
    begin
      R := -7.950E-01;
    end;
  if W = 49 then
    begin
      R := -8.495E-01;
    end;
  if W = 48 then
    begin
      R := -9.067E-01;
    end;
  if W = 47 then
    begin
      R := -9.664E-01;
    end;
  if W = 46 then
    begin
      R := -1.029E+00;
    end;
  if W = 45 then
    begin
      R := -1.094E+00;
    end;
  if W = 44 then
    begin
      R := -1.162E+00;
    end;
  if W = 43 then
    begin
      R := -1.233E+00;
    end;
  if W = 42 then
    begin
      R := -1.306E+00;
    end;
  if W = 41 then
    begin
      R := -1.383E+00;
    end;
  if W = 40 then
    begin
      R := -1.463E+00;
    end;
  if W = 39 then
    begin
      R := -1.546E+00;
    end;
  if W = 38 then
    begin
      R := -1.632E+00;
    end;
  if W = 37 then
    begin
      R := -1.722E+00;
    end;
  if W = 36 then
    begin
      R := -1.815E+00;
    end;
  if W = 35 then
    begin
      R := -1.911E+00;
    end;
  if W = 34 then
    begin
      R := -2.011E+00;
    end;
  if W = 33 then
    begin
      R := -2.115E+00;
    end;
  if W = 32 then
    begin
      R := -2.223E+00;
    end;
  if W = 31 then
    begin
      R := -2.334E+00;
    end;
  if W = 30 then
    begin
      R := -2.450E+00;
    end;
  if W = 29 then
    begin
      R := -2.570E+00;
    end;
  if W = 28 then
    begin
      R := -2.694E+00;
    end;
  if W = 27 then
    begin
      R := -2.823E+00;
    end;
  if W = 26 then
    begin
      R := -2.956E+00;
    end;
  if W = 25 then
    begin
      R := -3.095E+00;
    end;
  if W = 24 then
    begin
      R := -3.238E+00;
    end;
  if W = 23 then
    begin
      R := -3.387E+00;
    end;
  if W = 22 then
    begin
      R := -3.541E+00;
    end;
  if W = 21 then
    begin
      R := -3.700E+00;
    end;
  if W = 20 then
    begin
      R := -3.866E+00;
    end;
  if W = 19 then
    begin
      R := -4.038E+00;
    end;
  if W = 18 then
    begin
      R := -4.215E+00;
    end;
  if W = 17 then
    begin
      R := -4.401E+00;
    end;
  if W = 16 then
    begin
      R := -4.592E+00;
    end;
  if W = 15 then
    begin
      R := -4.791E+00;
    end;
  if W = 14 then
    begin
      R := -5.004E+00;
    end;
  if W = 13 then
    begin
      R := -5.227E+00;
    end;
  if W = 12 then
    begin
      R := -5.456E+00;
    end;
  if W = 11 then
    begin
      R := -5.697E+00;
    end;
  if W = 10 then
    begin
      R := -5.943E+00;
    end;
  if W = 9 then
    begin
      R := -6.208E+00;
    end;
  if W = 8 then
    begin
      R := -6.485E+00;
    end;
  if W = 7 then
    begin
      R := -6.760E+00;
    end;
  if W = 6 then
    begin
      R := -7.065E+00;
    end;
  if W = 5 then
    begin
      R := -7.401E+00;
    end;
  if W = 4 then
    begin
      R := -7.758E+00;
    end;
  if W = 3 then
    begin
      R := -8.095E+00;
    end;
  if W = 2 then
    begin
      R := -8.605E+00;
    end;
  if W = 1 then
    begin
      R := -9.011E+00;
    end;
  if W <= 0 then
    begin
      R := -9.704E+00;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 15)
  ************************************************************************ *)
function W15(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-1.760682E+01 * S + 6.000000E+01);
  if W >= 60 then
    begin
      R := -6.714E-01;
    end;
  if W = 59 then
    begin
      R := -7.154E-01;
    end;
  if W = 58 then
    begin
      R := -7.613E-01;
    end;
  if W = 57 then
    begin
      R := -8.093E-01;
    end;
  if W = 56 then
    begin
      R := -8.593E-01;
    end;
  if W = 55 then
    begin
      R := -9.114E-01;
    end;
  if W = 54 then
    begin
      R := -9.656E-01;
    end;
  if W = 53 then
    begin
      R := -1.022E+00;
    end;
  if W = 52 then
    begin
      R := -1.081E+00;
    end;
  if W = 51 then
    begin
      R := -1.142E+00;
    end;
  if W = 50 then
    begin
      R := -1.205E+00;
    end;
  if W = 49 then
    begin
      R := -1.270E+00;
    end;
  if W = 48 then
    begin
      R := -1.339E+00;
    end;
  if W = 47 then
    begin
      R := -1.409E+00;
    end;
  if W = 46 then
    begin
      R := -1.482E+00;
    end;
  if W = 45 then
    begin
      R := -1.558E+00;
    end;
  if W = 44 then
    begin
      R := -1.636E+00;
    end;
  if W = 43 then
    begin
      R := -1.717E+00;
    end;
  if W = 42 then
    begin
      R := -1.801E+00;
    end;
  if W = 41 then
    begin
      R := -1.888E+00;
    end;
  if W = 40 then
    begin
      R := -1.977E+00;
    end;
  if W = 39 then
    begin
      R := -2.070E+00;
    end;
  if W = 38 then
    begin
      R := -2.166E+00;
    end;
  if W = 37 then
    begin
      R := -2.265E+00;
    end;
  if W = 36 then
    begin
      R := -2.366E+00;
    end;
  if W = 35 then
    begin
      R := -2.472E+00;
    end;
  if W = 34 then
    begin
      R := -2.581E+00;
    end;
  if W = 33 then
    begin
      R := -2.693E+00;
    end;
  if W = 32 then
    begin
      R := -2.809E+00;
    end;
  if W = 31 then
    begin
      R := -2.928E+00;
    end;
  if W = 30 then
    begin
      R := -3.051E+00;
    end;
  if W = 29 then
    begin
      R := -3.179E+00;
    end;
  if W = 28 then
    begin
      R := -3.310E+00;
    end;
  if W = 27 then
    begin
      R := -3.446E+00;
    end;
  if W = 26 then
    begin
      R := -3.587E+00;
    end;
  if W = 25 then
    begin
      R := -3.732E+00;
    end;
  if W = 24 then
    begin
      R := -3.881E+00;
    end;
  if W = 23 then
    begin
      R := -4.036E+00;
    end;
  if W = 22 then
    begin
      R := -4.195E+00;
    end;
  if W = 21 then
    begin
      R := -4.359E+00;
    end;
  if W = 20 then
    begin
      R := -4.531E+00;
    end;
  if W = 19 then
    begin
      R := -4.707E+00;
    end;
  if W = 18 then
    begin
      R := -4.888E+00;
    end;
  if W = 17 then
    begin
      R := -5.079E+00;
    end;
  if W = 16 then
    begin
      R := -5.273E+00;
    end;
  if W = 15 then
    begin
      R := -5.477E+00;
    end;
  if W = 14 then
    begin
      R := -5.697E+00;
    end;
  if W = 13 then
    begin
      R := -5.920E+00;
    end;
  if W = 12 then
    begin
      R := -6.149E+00;
    end;
  if W = 11 then
    begin
      R := -6.390E+00;
    end;
  if W = 10 then
    begin
      R := -6.636E+00;
    end;
  if W = 9 then
    begin
      R := -6.901E+00;
    end;
  if W = 8 then
    begin
      R := -7.178E+00;
    end;
  if W = 7 then
    begin
      R := -7.453E+00;
    end;
  if W = 6 then
    begin
      R := -7.758E+00;
    end;
  if W = 5 then
    begin
      R := -8.095E+00;
    end;
  if W = 4 then
    begin
      R := -8.451E+00;
    end;
  if W = 3 then
    begin
      R := -8.788E+00;
    end;
  if W = 2 then
    begin
      R := -9.299E+00;
    end;
  if W = 1 then
    begin
      R := -9.704E+00;
    end;
  if W <= 0 then
    begin
      R := -1.040E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 16)
  ************************************************************************ *)
function W16(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-1.933908E+01 * S + 6.800000E+01);
  if W >= 68 then
    begin
      R := -6.733E-01;
    end;
  if W = 67 then
    begin
      R := -7.134E-01;
    end;
  if W = 66 then
    begin
      R := -7.551E-01;
    end;
  if W = 65 then
    begin
      R := -7.986E-01;
    end;
  if W = 64 then
    begin
      R := -8.437E-01;
    end;
  if W = 63 then
    begin
      R := -8.905E-01;
    end;
  if W = 62 then
    begin
      R := -9.391E-01;
    end;
  if W = 61 then
    begin
      R := -9.895E-01;
    end;
  if W = 60 then
    begin
      R := -1.042E+00;
    end;
  if W = 59 then
    begin
      R := -1.096E+00;
    end;
  if W = 58 then
    begin
      R := -1.152E+00;
    end;
  if W = 57 then
    begin
      R := -1.210E+00;
    end;
  if W = 56 then
    begin
      R := -1.270E+00;
    end;
  if W = 55 then
    begin
      R := -1.331E+00;
    end;
  if W = 54 then
    begin
      R := -1.395E+00;
    end;
  if W = 53 then
    begin
      R := -1.462E+00;
    end;
  if W = 52 then
    begin
      R := -1.530E+00;
    end;
  if W = 51 then
    begin
      R := -1.600E+00;
    end;
  if W = 50 then
    begin
      R := -1.673E+00;
    end;
  if W = 49 then
    begin
      R := -1.748E+00;
    end;
  if W = 48 then
    begin
      R := -1.825E+00;
    end;
  if W = 47 then
    begin
      R := -1.904E+00;
    end;
  if W = 46 then
    begin
      R := -1.986E+00;
    end;
  if W = 45 then
    begin
      R := -2.071E+00;
    end;
  if W = 44 then
    begin
      R := -2.158E+00;
    end;
  if W = 43 then
    begin
      R := -2.247E+00;
    end;
  if W = 42 then
    begin
      R := -2.339E+00;
    end;
  if W = 41 then
    begin
      R := -2.434E+00;
    end;
  if W = 40 then
    begin
      R := -2.532E+00;
    end;
  if W = 39 then
    begin
      R := -2.632E+00;
    end;
  if W = 38 then
    begin
      R := -2.735E+00;
    end;
  if W = 37 then
    begin
      R := -2.842E+00;
    end;
  if W = 36 then
    begin
      R := -2.951E+00;
    end;
  if W = 35 then
    begin
      R := -3.064E+00;
    end;
  if W = 34 then
    begin
      R := -3.179E+00;
    end;
  if W = 33 then
    begin
      R := -3.298E+00;
    end;
  if W = 32 then
    begin
      R := -3.420E+00;
    end;
  if W = 31 then
    begin
      R := -3.546E+00;
    end;
  if W = 30 then
    begin
      R := -3.676E+00;
    end;
  if W = 29 then
    begin
      R := -3.810E+00;
    end;
  if W = 28 then
    begin
      R := -3.947E+00;
    end;
  if W = 27 then
    begin
      R := -4.088E+00;
    end;
  if W = 26 then
    begin
      R := -4.234E+00;
    end;
  if W = 25 then
    begin
      R := -4.383E+00;
    end;
  if W = 24 then
    begin
      R := -4.538E+00;
    end;
  if W = 23 then
    begin
      R := -4.697E+00;
    end;
  if W = 22 then
    begin
      R := -4.860E+00;
    end;
  if W = 21 then
    begin
      R := -5.029E+00;
    end;
  if W = 20 then
    begin
      R := -5.204E+00;
    end;
  if W = 19 then
    begin
      R := -5.383E+00;
    end;
  if W = 18 then
    begin
      R := -5.569E+00;
    end;
  if W = 17 then
    begin
      R := -5.762E+00;
    end;
  if W = 16 then
    begin
      R := -5.960E+00;
    end;
  if W = 15 then
    begin
      R := -6.170E+00;
    end;
  if W = 14 then
    begin
      R := -6.390E+00;
    end;
  if W = 13 then
    begin
      R := -6.613E+00;
    end;
  if W = 12 then
    begin
      R := -6.842E+00;
    end;
  if W = 11 then
    begin
      R := -7.083E+00;
    end;
  if W = 10 then
    begin
      R := -7.329E+00;
    end;
  if W = 9 then
    begin
      R := -7.594E+00;
    end;
  if W = 8 then
    begin
      R := -7.871E+00;
    end;
  if W = 7 then
    begin
      R := -8.146E+00;
    end;
  if W = 6 then
    begin
      R := -8.451E+00;
    end;
  if W = 5 then
    begin
      R := -8.788E+00;
    end;
  if W = 4 then
    begin
      R := -9.144E+00;
    end;
  if W = 3 then
    begin
      R := -9.481E+00;
    end;
  if W = 2 then
    begin
      R := -9.992E+00;
    end;
  if W = 1 then
    begin
      R := -1.040E+01;
    end;
  if W <= 0 then
    begin
      R := -1.109E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 17)
  ************************************************************************ *)
function W17(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-2.112463E+01 * S + 7.650000E+01);
  if W >= 76 then
    begin
      R := -6.931E-01;
    end;
  if W = 75 then
    begin
      R := -7.306E-01;
    end;
  if W = 74 then
    begin
      R := -7.695E-01;
    end;
  if W = 73 then
    begin
      R := -8.097E-01;
    end;
  if W = 72 then
    begin
      R := -8.514E-01;
    end;
  if W = 71 then
    begin
      R := -8.946E-01;
    end;
  if W = 70 then
    begin
      R := -9.392E-01;
    end;
  if W = 69 then
    begin
      R := -9.853E-01;
    end;
  if W = 68 then
    begin
      R := -1.033E+00;
    end;
  if W = 67 then
    begin
      R := -1.082E+00;
    end;
  if W = 66 then
    begin
      R := -1.133E+00;
    end;
  if W = 65 then
    begin
      R := -1.185E+00;
    end;
  if W = 64 then
    begin
      R := -1.240E+00;
    end;
  if W = 63 then
    begin
      R := -1.295E+00;
    end;
  if W = 62 then
    begin
      R := -1.353E+00;
    end;
  if W = 61 then
    begin
      R := -1.412E+00;
    end;
  if W = 60 then
    begin
      R := -1.473E+00;
    end;
  if W = 59 then
    begin
      R := -1.536E+00;
    end;
  if W = 58 then
    begin
      R := -1.600E+00;
    end;
  if W = 57 then
    begin
      R := -1.666E+00;
    end;
  if W = 56 then
    begin
      R := -1.735E+00;
    end;
  if W = 55 then
    begin
      R := -1.805E+00;
    end;
  if W = 54 then
    begin
      R := -1.877E+00;
    end;
  if W = 53 then
    begin
      R := -1.951E+00;
    end;
  if W = 52 then
    begin
      R := -2.028E+00;
    end;
  if W = 51 then
    begin
      R := -2.106E+00;
    end;
  if W = 50 then
    begin
      R := -2.186E+00;
    end;
  if W = 49 then
    begin
      R := -2.269E+00;
    end;
  if W = 48 then
    begin
      R := -2.353E+00;
    end;
  if W = 47 then
    begin
      R := -2.440E+00;
    end;
  if W = 46 then
    begin
      R := -2.530E+00;
    end;
  if W = 45 then
    begin
      R := -2.621E+00;
    end;
  if W = 44 then
    begin
      R := -2.715E+00;
    end;
  if W = 43 then
    begin
      R := -2.812E+00;
    end;
  if W = 42 then
    begin
      R := -2.911E+00;
    end;
  if W = 41 then
    begin
      R := -3.012E+00;
    end;
  if W = 40 then
    begin
      R := -3.116E+00;
    end;
  if W = 39 then
    begin
      R := -3.223E+00;
    end;
  if W = 38 then
    begin
      R := -3.332E+00;
    end;
  if W = 37 then
    begin
      R := -3.445E+00;
    end;
  if W = 36 then
    begin
      R := -3.560E+00;
    end;
  if W = 35 then
    begin
      R := -3.678E+00;
    end;
  if W = 34 then
    begin
      R := -3.799E+00;
    end;
  if W = 33 then
    begin
      R := -3.924E+00;
    end;
  if W = 32 then
    begin
      R := -4.052E+00;
    end;
  if W = 31 then
    begin
      R := -4.183E+00;
    end;
  if W = 30 then
    begin
      R := -4.317E+00;
    end;
  if W = 29 then
    begin
      R := -4.456E+00;
    end;
  if W = 28 then
    begin
      R := -4.597E+00;
    end;
  if W = 27 then
    begin
      R := -4.743E+00;
    end;
  if W = 26 then
    begin
      R := -4.893E+00;
    end;
  if W = 25 then
    begin
      R := -5.047E+00;
    end;
  if W = 24 then
    begin
      R := -5.204E+00;
    end;
  if W = 23 then
    begin
      R := -5.367E+00;
    end;
  if W = 22 then
    begin
      R := -5.534E+00;
    end;
  if W = 21 then
    begin
      R := -5.706E+00;
    end;
  if W = 20 then
    begin
      R := -5.884E+00;
    end;
  if W = 19 then
    begin
      R := -6.066E+00;
    end;
  if W = 18 then
    begin
      R := -6.254E+00;
    end;
  if W = 17 then
    begin
      R := -6.451E+00;
    end;
  if W = 16 then
    begin
      R := -6.654E+00;
    end;
  if W = 15 then
    begin
      R := -6.864E+00;
    end;
  if W = 14 then
    begin
      R := -7.083E+00;
    end;
  if W = 13 then
    begin
      R := -7.306E+00;
    end;
  if W = 12 then
    begin
      R := -7.535E+00;
    end;
  if W = 11 then
    begin
      R := -7.776E+00;
    end;
  if W = 10 then
    begin
      R := -8.022E+00;
    end;
  if W = 9 then
    begin
      R := -8.287E+00;
    end;
  if W = 8 then
    begin
      R := -8.565E+00;
    end;
  if W = 7 then
    begin
      R := -8.839E+00;
    end;
  if W = 6 then
    begin
      R := -9.144E+00;
    end;
  if W = 5 then
    begin
      R := -9.481E+00;
    end;
  if W = 4 then
    begin
      R := -9.838E+00;
    end;
  if W = 3 then
    begin
      R := -1.017E+01;
    end;
  if W = 2 then
    begin
      R := -1.068E+01;
    end;
  if W = 1 then
    begin
      R := -1.109E+01;
    end;
  if W <= 0 then
    begin
      R := -1.178E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 18)
  ************************************************************************ *)
function W18(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-2.296193E+01 * S + 8.550000E+01);
  if W >= 85 then
    begin
      R := -6.931E-01;
    end;
  if W = 84 then
    begin
      R := -7.276E-01;
    end;
  if W = 83 then
    begin
      R := -7.633E-01;
    end;
  if W = 82 then
    begin
      R := -8.001E-01;
    end;
  if W = 81 then
    begin
      R := -8.381E-01;
    end;
  if W = 80 then
    begin
      R := -8.774E-01;
    end;
  if W = 79 then
    begin
      R := -9.179E-01;
    end;
  if W = 78 then
    begin
      R := -9.597E-01;
    end;
  if W = 77 then
    begin
      R := -1.003E+00;
    end;
  if W = 76 then
    begin
      R := -1.047E+00;
    end;
  if W = 75 then
    begin
      R := -1.093E+00;
    end;
  if W = 74 then
    begin
      R := -1.140E+00;
    end;
  if W = 73 then
    begin
      R := -1.188E+00;
    end;
  if W = 72 then
    begin
      R := -1.238E+00;
    end;
  if W = 71 then
    begin
      R := -1.289E+00;
    end;
  if W = 70 then
    begin
      R := -1.342E+00;
    end;
  if W = 69 then
    begin
      R := -1.396E+00;
    end;
  if W = 68 then
    begin
      R := -1.452E+00;
    end;
  if W = 67 then
    begin
      R := -1.509E+00;
    end;
  if W = 66 then
    begin
      R := -1.568E+00;
    end;
  if W = 65 then
    begin
      R := -1.628E+00;
    end;
  if W = 64 then
    begin
      R := -1.690E+00;
    end;
  if W = 63 then
    begin
      R := -1.753E+00;
    end;
  if W = 62 then
    begin
      R := -1.818E+00;
    end;
  if W = 61 then
    begin
      R := -1.885E+00;
    end;
  if W = 60 then
    begin
      R := -1.953E+00;
    end;
  if W = 59 then
    begin
      R := -2.023E+00;
    end;
  if W = 58 then
    begin
      R := -2.095E+00;
    end;
  if W = 57 then
    begin
      R := -2.168E+00;
    end;
  if W = 56 then
    begin
      R := -2.244E+00;
    end;
  if W = 55 then
    begin
      R := -2.321E+00;
    end;
  if W = 54 then
    begin
      R := -2.400E+00;
    end;
  if W = 53 then
    begin
      R := -2.481E+00;
    end;
  if W = 52 then
    begin
      R := -2.564E+00;
    end;
  if W = 51 then
    begin
      R := -2.648E+00;
    end;
  if W = 50 then
    begin
      R := -2.735E+00;
    end;
  if W = 49 then
    begin
      R := -2.824E+00;
    end;
  if W = 48 then
    begin
      R := -2.915E+00;
    end;
  if W = 47 then
    begin
      R := -3.008E+00;
    end;
  if W = 46 then
    begin
      R := -3.104E+00;
    end;
  if W = 45 then
    begin
      R := -3.201E+00;
    end;
  if W = 44 then
    begin
      R := -3.301E+00;
    end;
  if W = 43 then
    begin
      R := -3.403E+00;
    end;
  if W = 42 then
    begin
      R := -3.508E+00;
    end;
  if W = 41 then
    begin
      R := -3.615E+00;
    end;
  if W = 40 then
    begin
      R := -3.724E+00;
    end;
  if W = 39 then
    begin
      R := -3.836E+00;
    end;
  if W = 38 then
    begin
      R := -3.950E+00;
    end;
  if W = 37 then
    begin
      R := -4.068E+00;
    end;
  if W = 36 then
    begin
      R := -4.188E+00;
    end;
  if W = 35 then
    begin
      R := -4.311E+00;
    end;
  if W = 34 then
    begin
      R := -4.437E+00;
    end;
  if W = 33 then
    begin
      R := -4.565E+00;
    end;
  if W = 32 then
    begin
      R := -4.698E+00;
    end;
  if W = 31 then
    begin
      R := -4.833E+00;
    end;
  if W = 30 then
    begin
      R := -4.971E+00;
    end;
  if W = 29 then
    begin
      R := -5.113E+00;
    end;
  if W = 28 then
    begin
      R := -5.258E+00;
    end;
  if W = 27 then
    begin
      R := -5.408E+00;
    end;
  if W = 26 then
    begin
      R := -5.561E+00;
    end;
  if W = 25 then
    begin
      R := -5.717E+00;
    end;
  if W = 24 then
    begin
      R := -5.878E+00;
    end;
  if W = 23 then
    begin
      R := -6.044E+00;
    end;
  if W = 22 then
    begin
      R := -6.213E+00;
    end;
  if W = 21 then
    begin
      R := -6.388E+00;
    end;
  if W = 20 then
    begin
      R := -6.569E+00;
    end;
  if W = 19 then
    begin
      R := -6.753E+00;
    end;
  if W = 18 then
    begin
      R := -6.943E+00;
    end;
  if W = 17 then
    begin
      R := -7.144E+00;
    end;
  if W = 16 then
    begin
      R := -7.347E+00;
    end;
  if W = 15 then
    begin
      R := -7.557E+00;
    end;
  if W = 14 then
    begin
      R := -7.776E+00;
    end;
  if W = 13 then
    begin
      R := -7.999E+00;
    end;
  if W = 12 then
    begin
      R := -8.228E+00;
    end;
  if W = 11 then
    begin
      R := -8.469E+00;
    end;
  if W = 10 then
    begin
      R := -8.715E+00;
    end;
  if W = 9 then
    begin
      R := -8.980E+00;
    end;
  if W = 8 then
    begin
      R := -9.258E+00;
    end;
  if W = 7 then
    begin
      R := -9.532E+00;
    end;
  if W = 6 then
    begin
      R := -9.838E+00;
    end;
  if W = 5 then
    begin
      R := -1.017E+01;
    end;
  if W = 4 then
    begin
      R := -1.053E+01;
    end;
  if W = 3 then
    begin
      R := -1.087E+01;
    end;
  if W = 2 then
    begin
      R := -1.138E+01;
    end;
  if W = 1 then
    begin
      R := -1.178E+01;
    end;
  if W <= 0 then
    begin
      R := -1.248E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 19)
  ************************************************************************ *)
function W19(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-2.484955E+01 * S + 9.500000E+01);
  if W >= 95 then
    begin
      R := -6.776E-01;
    end;
  if W = 94 then
    begin
      R := -7.089E-01;
    end;
  if W = 93 then
    begin
      R := -7.413E-01;
    end;
  if W = 92 then
    begin
      R := -7.747E-01;
    end;
  if W = 91 then
    begin
      R := -8.090E-01;
    end;
  if W = 90 then
    begin
      R := -8.445E-01;
    end;
  if W = 89 then
    begin
      R := -8.809E-01;
    end;
  if W = 88 then
    begin
      R := -9.185E-01;
    end;
  if W = 87 then
    begin
      R := -9.571E-01;
    end;
  if W = 86 then
    begin
      R := -9.968E-01;
    end;
  if W = 85 then
    begin
      R := -1.038E+00;
    end;
  if W = 84 then
    begin
      R := -1.080E+00;
    end;
  if W = 83 then
    begin
      R := -1.123E+00;
    end;
  if W = 82 then
    begin
      R := -1.167E+00;
    end;
  if W = 81 then
    begin
      R := -1.213E+00;
    end;
  if W = 80 then
    begin
      R := -1.259E+00;
    end;
  if W = 79 then
    begin
      R := -1.307E+00;
    end;
  if W = 78 then
    begin
      R := -1.356E+00;
    end;
  if W = 77 then
    begin
      R := -1.407E+00;
    end;
  if W = 76 then
    begin
      R := -1.458E+00;
    end;
  if W = 75 then
    begin
      R := -1.511E+00;
    end;
  if W = 74 then
    begin
      R := -1.565E+00;
    end;
  if W = 73 then
    begin
      R := -1.621E+00;
    end;
  if W = 72 then
    begin
      R := -1.678E+00;
    end;
  if W = 71 then
    begin
      R := -1.736E+00;
    end;
  if W = 70 then
    begin
      R := -1.796E+00;
    end;
  if W = 69 then
    begin
      R := -1.857E+00;
    end;
  if W = 68 then
    begin
      R := -1.919E+00;
    end;
  if W = 67 then
    begin
      R := -1.983E+00;
    end;
  if W = 66 then
    begin
      R := -2.048E+00;
    end;
  if W = 65 then
    begin
      R := -2.115E+00;
    end;
  if W = 64 then
    begin
      R := -2.183E+00;
    end;
  if W = 63 then
    begin
      R := -2.253E+00;
    end;
  if W = 62 then
    begin
      R := -2.325E+00;
    end;
  if W = 61 then
    begin
      R := -2.398E+00;
    end;
  if W = 60 then
    begin
      R := -2.472E+00;
    end;
  if W = 59 then
    begin
      R := -2.548E+00;
    end;
  if W = 58 then
    begin
      R := -2.626E+00;
    end;
  if W = 57 then
    begin
      R := -2.706E+00;
    end;
  if W = 56 then
    begin
      R := -2.787E+00;
    end;
  if W = 55 then
    begin
      R := -2.870E+00;
    end;
  if W = 54 then
    begin
      R := -2.955E+00;
    end;
  if W = 53 then
    begin
      R := -3.042E+00;
    end;
  if W = 52 then
    begin
      R := -3.130E+00;
    end;
  if W = 51 then
    begin
      R := -3.220E+00;
    end;
  if W = 50 then
    begin
      R := -3.313E+00;
    end;
  if W = 49 then
    begin
      R := -3.407E+00;
    end;
  if W = 48 then
    begin
      R := -3.503E+00;
    end;
  if W = 47 then
    begin
      R := -3.601E+00;
    end;
  if W = 46 then
    begin
      R := -3.702E+00;
    end;
  if W = 45 then
    begin
      R := -3.804E+00;
    end;
  if W = 44 then
    begin
      R := -3.909E+00;
    end;
  if W = 43 then
    begin
      R := -4.015E+00;
    end;
  if W = 42 then
    begin
      R := -4.125E+00;
    end;
  if W = 41 then
    begin
      R := -4.236E+00;
    end;
  if W = 40 then
    begin
      R := -4.350E+00;
    end;
  if W = 39 then
    begin
      R := -4.466E+00;
    end;
  if W = 38 then
    begin
      R := -4.585E+00;
    end;
  if W = 37 then
    begin
      R := -4.706E+00;
    end;
  if W = 36 then
    begin
      R := -4.830E+00;
    end;
  if W = 35 then
    begin
      R := -4.957E+00;
    end;
  if W = 34 then
    begin
      R := -5.086E+00;
    end;
  if W = 33 then
    begin
      R := -5.219E+00;
    end;
  if W = 32 then
    begin
      R := -5.355E+00;
    end;
  if W = 31 then
    begin
      R := -5.493E+00;
    end;
  if W = 30 then
    begin
      R := -5.634E+00;
    end;
  if W = 29 then
    begin
      R := -5.780E+00;
    end;
  if W = 28 then
    begin
      R := -5.928E+00;
    end;
  if W = 27 then
    begin
      R := -6.080E+00;
    end;
  if W = 26 then
    begin
      R := -6.235E+00;
    end;
  if W = 25 then
    begin
      R := -6.394E+00;
    end;
  if W = 24 then
    begin
      R := -6.558E+00;
    end;
  if W = 23 then
    begin
      R := -6.726E+00;
    end;
  if W = 22 then
    begin
      R := -6.897E+00;
    end;
  if W = 21 then
    begin
      R := -7.074E+00;
    end;
  if W = 20 then
    begin
      R := -7.256E+00;
    end;
  if W = 19 then
    begin
      R := -7.443E+00;
    end;
  if W = 18 then
    begin
      R := -7.636E+00;
    end;
  if W = 17 then
    begin
      R := -7.837E+00;
    end;
  if W = 16 then
    begin
      R := -8.040E+00;
    end;
  if W = 15 then
    begin
      R := -8.250E+00;
    end;
  if W = 14 then
    begin
      R := -8.469E+00;
    end;
  if W = 13 then
    begin
      R := -8.692E+00;
    end;
  if W = 12 then
    begin
      R := -8.921E+00;
    end;
  if W = 11 then
    begin
      R := -9.162E+00;
    end;
  if W = 10 then
    begin
      R := -9.409E+00;
    end;
  if W = 9 then
    begin
      R := -9.673E+00;
    end;
  if W = 8 then
    begin
      R := -9.951E+00;
    end;
  if W = 7 then
    begin
      R := -1.023E+01;
    end;
  if W = 6 then
    begin
      R := -1.053E+01;
    end;
  if W = 5 then
    begin
      R := -1.087E+01;
    end;
  if W = 4 then
    begin
      R := -1.122E+01;
    end;
  if W = 3 then
    begin
      R := -1.156E+01;
    end;
  if W = 2 then
    begin
      R := -1.207E+01;
    end;
  if W = 1 then
    begin
      R := -1.248E+01;
    end;
  if W <= 0 then
    begin
      R := -1.317E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 20)
  ************************************************************************ *)
function W20(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-2.678619E+01 * S + 1.050000E+02);
  if W >= 105 then
    begin
      R := -6.787E-01;
    end;
  if W = 104 then
    begin
      R := -7.078E-01;
    end;
  if W = 103 then
    begin
      R := -7.378E-01;
    end;
  if W = 102 then
    begin
      R := -7.686E-01;
    end;
  if W = 101 then
    begin
      R := -8.004E-01;
    end;
  if W = 100 then
    begin
      R := -8.330E-01;
    end;
  if W = 99 then
    begin
      R := -8.665E-01;
    end;
  if W = 98 then
    begin
      R := -9.010E-01;
    end;
  if W = 97 then
    begin
      R := -9.363E-01;
    end;
  if W = 96 then
    begin
      R := -9.726E-01;
    end;
  if W = 95 then
    begin
      R := -1.010E+00;
    end;
  if W = 94 then
    begin
      R := -1.048E+00;
    end;
  if W = 93 then
    begin
      R := -1.087E+00;
    end;
  if W = 92 then
    begin
      R := -1.128E+00;
    end;
  if W = 91 then
    begin
      R := -1.169E+00;
    end;
  if W = 90 then
    begin
      R := -1.211E+00;
    end;
  if W = 89 then
    begin
      R := -1.254E+00;
    end;
  if W = 88 then
    begin
      R := -1.299E+00;
    end;
  if W = 87 then
    begin
      R := -1.344E+00;
    end;
  if W = 86 then
    begin
      R := -1.390E+00;
    end;
  if W = 85 then
    begin
      R := -1.438E+00;
    end;
  if W = 84 then
    begin
      R := -1.486E+00;
    end;
  if W = 83 then
    begin
      R := -1.536E+00;
    end;
  if W = 82 then
    begin
      R := -1.587E+00;
    end;
  if W = 81 then
    begin
      R := -1.639E+00;
    end;
  if W = 80 then
    begin
      R := -1.692E+00;
    end;
  if W = 79 then
    begin
      R := -1.746E+00;
    end;
  if W = 78 then
    begin
      R := -1.802E+00;
    end;
  if W = 77 then
    begin
      R := -1.859E+00;
    end;
  if W = 76 then
    begin
      R := -1.916E+00;
    end;
  if W = 75 then
    begin
      R := -1.976E+00;
    end;
  if W = 74 then
    begin
      R := -2.036E+00;
    end;
  if W = 73 then
    begin
      R := -2.098E+00;
    end;
  if W = 72 then
    begin
      R := -2.161E+00;
    end;
  if W = 71 then
    begin
      R := -2.225E+00;
    end;
  if W = 70 then
    begin
      R := -2.290E+00;
    end;
  if W = 69 then
    begin
      R := -2.357E+00;
    end;
  if W = 68 then
    begin
      R := -2.426E+00;
    end;
  if W = 67 then
    begin
      R := -2.495E+00;
    end;
  if W = 66 then
    begin
      R := -2.566E+00;
    end;
  if W = 65 then
    begin
      R := -2.639E+00;
    end;
  if W = 64 then
    begin
      R := -2.713E+00;
    end;
  if W = 63 then
    begin
      R := -2.788E+00;
    end;
  if W = 62 then
    begin
      R := -2.865E+00;
    end;
  if W = 61 then
    begin
      R := -2.943E+00;
    end;
  if W = 60 then
    begin
      R := -3.023E+00;
    end;
  if W = 59 then
    begin
      R := -3.104E+00;
    end;
  if W = 58 then
    begin
      R := -3.187E+00;
    end;
  if W = 57 then
    begin
      R := -3.272E+00;
    end;
  if W = 56 then
    begin
      R := -3.358E+00;
    end;
  if W = 55 then
    begin
      R := -3.446E+00;
    end;
  if W = 54 then
    begin
      R := -3.536E+00;
    end;
  if W = 53 then
    begin
      R := -3.627E+00;
    end;
  if W = 52 then
    begin
      R := -3.721E+00;
    end;
  if W = 51 then
    begin
      R := -3.815E+00;
    end;
  if W = 50 then
    begin
      R := -3.912E+00;
    end;
  if W = 49 then
    begin
      R := -4.011E+00;
    end;
  if W = 48 then
    begin
      R := -4.111E+00;
    end;
  if W = 47 then
    begin
      R := -4.214E+00;
    end;
  if W = 46 then
    begin
      R := -4.318E+00;
    end;
  if W = 45 then
    begin
      R := -4.425E+00;
    end;
  if W = 44 then
    begin
      R := -4.534E+00;
    end;
  if W = 43 then
    begin
      R := -4.644E+00;
    end;
  if W = 42 then
    begin
      R := -4.757E+00;
    end;
  if W = 41 then
    begin
      R := -4.872E+00;
    end;
  if W = 40 then
    begin
      R := -4.990E+00;
    end;
  if W = 39 then
    begin
      R := -5.109E+00;
    end;
  if W = 38 then
    begin
      R := -5.232E+00;
    end;
  if W = 37 then
    begin
      R := -5.356E+00;
    end;
  if W = 36 then
    begin
      R := -5.484E+00;
    end;
  if W = 35 then
    begin
      R := -5.614E+00;
    end;
  if W = 34 then
    begin
      R := -5.746E+00;
    end;
  if W = 33 then
    begin
      R := -5.882E+00;
    end;
  if W = 32 then
    begin
      R := -6.020E+00;
    end;
  if W = 31 then
    begin
      R := -6.161E+00;
    end;
  if W = 30 then
    begin
      R := -6.305E+00;
    end;
  if W = 29 then
    begin
      R := -6.453E+00;
    end;
  if W = 28 then
    begin
      R := -6.603E+00;
    end;
  if W = 27 then
    begin
      R := -6.757E+00;
    end;
  if W = 26 then
    begin
      R := -6.915E+00;
    end;
  if W = 25 then
    begin
      R := -7.076E+00;
    end;
  if W = 24 then
    begin
      R := -7.242E+00;
    end;
  if W = 23 then
    begin
      R := -7.411E+00;
    end;
  if W = 22 then
    begin
      R := -7.584E+00;
    end;
  if W = 21 then
    begin
      R := -7.763E+00;
    end;
  if W = 20 then
    begin
      R := -7.947E+00;
    end;
  if W = 19 then
    begin
      R := -8.136E+00;
    end;
  if W = 18 then
    begin
      R := -8.330E+00;
    end;
  if W = 17 then
    begin
      R := -8.530E+00;
    end;
  if W = 16 then
    begin
      R := -8.733E+00;
    end;
  if W = 15 then
    begin
      R := -8.943E+00;
    end;
  if W = 14 then
    begin
      R := -9.162E+00;
    end;
  if W = 13 then
    begin
      R := -9.386E+00;
    end;
  if W = 12 then
    begin
      R := -9.614E+00;
    end;
  if W = 11 then
    begin
      R := -9.856E+00;
    end;
  if W = 10 then
    begin
      R := -1.010E+01;
    end;
  if W = 9 then
    begin
      R := -1.037E+01;
    end;
  if W = 8 then
    begin
      R := -1.064E+01;
    end;
  if W = 7 then
    begin
      R := -1.092E+01;
    end;
  if W = 6 then
    begin
      R := -1.122E+01;
    end;
  if W = 5 then
    begin
      R := -1.156E+01;
    end;
  if W = 4 then
    begin
      R := -1.192E+01;
    end;
  if W = 3 then
    begin
      R := -1.225E+01;
    end;
  if W = 2 then
    begin
      R := -1.276E+01;
    end;
  if W = 1 then
    begin
      R := -1.317E+01;
    end;
  if W <= 0 then
    begin
      R := -1.386E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 21)
  ************************************************************************ *)
function W21(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-2.877064E+01 * S + 1.155000E+02);
  if W >= 115 then
    begin
      R := -6.931E-01;
    end;
  if W = 114 then
    begin
      R := -7.207E-01;
    end;
  if W = 113 then
    begin
      R := -7.489E-01;
    end;
  if W = 112 then
    begin
      R := -7.779E-01;
    end;
  if W = 111 then
    begin
      R := -8.077E-01;
    end;
  if W = 110 then
    begin
      R := -8.383E-01;
    end;
  if W = 109 then
    begin
      R := -8.697E-01;
    end;
  if W = 108 then
    begin
      R := -9.018E-01;
    end;
  if W = 107 then
    begin
      R := -9.348E-01;
    end;
  if W = 106 then
    begin
      R := -9.685E-01;
    end;
  if W = 105 then
    begin
      R := -1.003E+00;
    end;
  if W = 104 then
    begin
      R := -1.039E+00;
    end;
  if W = 103 then
    begin
      R := -1.075E+00;
    end;
  if W = 102 then
    begin
      R := -1.112E+00;
    end;
  if W = 101 then
    begin
      R := -1.150E+00;
    end;
  if W = 100 then
    begin
      R := -1.189E+00;
    end;
  if W = 99 then
    begin
      R := -1.229E+00;
    end;
  if W = 98 then
    begin
      R := -1.269E+00;
    end;
  if W = 97 then
    begin
      R := -1.311E+00;
    end;
  if W = 96 then
    begin
      R := -1.353E+00;
    end;
  if W = 95 then
    begin
      R := -1.397E+00;
    end;
  if W = 94 then
    begin
      R := -1.441E+00;
    end;
  if W = 93 then
    begin
      R := -1.486E+00;
    end;
  if W = 92 then
    begin
      R := -1.533E+00;
    end;
  if W = 91 then
    begin
      R := -1.580E+00;
    end;
  if W = 90 then
    begin
      R := -1.628E+00;
    end;
  if W = 89 then
    begin
      R := -1.677E+00;
    end;
  if W = 88 then
    begin
      R := -1.728E+00;
    end;
  if W = 87 then
    begin
      R := -1.779E+00;
    end;
  if W = 86 then
    begin
      R := -1.831E+00;
    end;
  if W = 85 then
    begin
      R := -1.884E+00;
    end;
  if W = 84 then
    begin
      R := -1.939E+00;
    end;
  if W = 83 then
    begin
      R := -1.994E+00;
    end;
  if W = 82 then
    begin
      R := -2.051E+00;
    end;
  if W = 81 then
    begin
      R := -2.108E+00;
    end;
  if W = 80 then
    begin
      R := -2.167E+00;
    end;
  if W = 79 then
    begin
      R := -2.227E+00;
    end;
  if W = 78 then
    begin
      R := -2.288E+00;
    end;
  if W = 77 then
    begin
      R := -2.350E+00;
    end;
  if W = 76 then
    begin
      R := -2.414E+00;
    end;
  if W = 75 then
    begin
      R := -2.478E+00;
    end;
  if W = 74 then
    begin
      R := -2.544E+00;
    end;
  if W = 73 then
    begin
      R := -2.611E+00;
    end;
  if W = 72 then
    begin
      R := -2.679E+00;
    end;
  if W = 71 then
    begin
      R := -2.748E+00;
    end;
  if W = 70 then
    begin
      R := -2.819E+00;
    end;
  if W = 69 then
    begin
      R := -2.891E+00;
    end;
  if W = 68 then
    begin
      R := -2.964E+00;
    end;
  if W = 67 then
    begin
      R := -3.039E+00;
    end;
  if W = 66 then
    begin
      R := -3.115E+00;
    end;
  if W = 65 then
    begin
      R := -3.192E+00;
    end;
  if W = 64 then
    begin
      R := -3.270E+00;
    end;
  if W = 63 then
    begin
      R := -3.350E+00;
    end;
  if W = 62 then
    begin
      R := -3.432E+00;
    end;
  if W = 61 then
    begin
      R := -3.515E+00;
    end;
  if W = 60 then
    begin
      R := -3.599E+00;
    end;
  if W = 59 then
    begin
      R := -3.685E+00;
    end;
  if W = 58 then
    begin
      R := -3.772E+00;
    end;
  if W = 57 then
    begin
      R := -3.861E+00;
    end;
  if W = 56 then
    begin
      R := -3.952E+00;
    end;
  if W = 55 then
    begin
      R := -4.044E+00;
    end;
  if W = 54 then
    begin
      R := -4.138E+00;
    end;
  if W = 53 then
    begin
      R := -4.233E+00;
    end;
  if W = 52 then
    begin
      R := -4.330E+00;
    end;
  if W = 51 then
    begin
      R := -4.429E+00;
    end;
  if W = 50 then
    begin
      R := -4.530E+00;
    end;
  if W = 49 then
    begin
      R := -4.632E+00;
    end;
  if W = 48 then
    begin
      R := -4.736E+00;
    end;
  if W = 47 then
    begin
      R := -4.842E+00;
    end;
  if W = 46 then
    begin
      R := -4.950E+00;
    end;
  if W = 45 then
    begin
      R := -5.060E+00;
    end;
  if W = 44 then
    begin
      R := -5.172E+00;
    end;
  if W = 43 then
    begin
      R := -5.286E+00;
    end;
  if W = 42 then
    begin
      R := -5.402E+00;
    end;
  if W = 41 then
    begin
      R := -5.520E+00;
    end;
  if W = 40 then
    begin
      R := -5.641E+00;
    end;
  if W = 39 then
    begin
      R := -5.763E+00;
    end;
  if W = 38 then
    begin
      R := -5.889E+00;
    end;
  if W = 37 then
    begin
      R := -6.016E+00;
    end;
  if W = 36 then
    begin
      R := -6.146E+00;
    end;
  if W = 35 then
    begin
      R := -6.278E+00;
    end;
  if W = 34 then
    begin
      R := -6.413E+00;
    end;
  if W = 33 then
    begin
      R := -6.551E+00;
    end;
  if W = 32 then
    begin
      R := -6.692E+00;
    end;
  if W = 31 then
    begin
      R := -6.835E+00;
    end;
  if W = 30 then
    begin
      R := -6.981E+00;
    end;
  if W = 29 then
    begin
      R := -7.131E+00;
    end;
  if W = 28 then
    begin
      R := -7.283E+00;
    end;
  if W = 27 then
    begin
      R := -7.439E+00;
    end;
  if W = 26 then
    begin
      R := -7.599E+00;
    end;
  if W = 25 then
    begin
      R := -7.762E+00;
    end;
  if W = 24 then
    begin
      R := -7.928E+00;
    end;
  if W = 23 then
    begin
      R := -8.099E+00;
    end;
  if W = 22 then
    begin
      R := -8.274E+00;
    end;
  if W = 21 then
    begin
      R := -8.454E+00;
    end;
  if W = 20 then
    begin
      R := -8.640E+00;
    end;
  if W = 19 then
    begin
      R := -8.829E+00;
    end;
  if W = 18 then
    begin
      R := -9.023E+00;
    end;
  if W = 17 then
    begin
      R := -9.223E+00;
    end;
  if W = 16 then
    begin
      R := -9.426E+00;
    end;
  if W = 15 then
    begin
      R := -9.636E+00;
    end;
  if W = 14 then
    begin
      R := -9.856E+00;
    end;
  if W = 13 then
    begin
      R := -1.008E+01;
    end;
  if W = 12 then
    begin
      R := -1.031E+01;
    end;
  if W = 11 then
    begin
      R := -1.055E+01;
    end;
  if W = 10 then
    begin
      R := -1.079E+01;
    end;
  if W = 9 then
    begin
      R := -1.106E+01;
    end;
  if W = 8 then
    begin
      R := -1.134E+01;
    end;
  if W = 7 then
    begin
      R := -1.161E+01;
    end;
  if W = 6 then
    begin
      R := -1.192E+01;
    end;
  if W = 5 then
    begin
      R := -1.225E+01;
    end;
  if W = 4 then
    begin
      R := -1.261E+01;
    end;
  if W = 3 then
    begin
      R := -1.295E+01;
    end;
  if W = 2 then
    begin
      R := -1.346E+01;
    end;
  if W = 1 then
    begin
      R := -1.386E+01;
    end;
  if W <= 0 then
    begin
      R := -1.456E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 22)
  ************************************************************************ *)
function W22(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-3.080179E+01 * S + 1.265000E+02);
  if W >= 126 then
    begin
      R := -6.931E-01;
    end;
  if W = 125 then
    begin
      R := -7.189E-01;
    end;
  if W = 124 then
    begin
      R := -7.452E-01;
    end;
  if W = 123 then
    begin
      R := -7.722E-01;
    end;
  if W = 122 then
    begin
      R := -7.999E-01;
    end;
  if W = 121 then
    begin
      R := -8.283E-01;
    end;
  if W = 120 then
    begin
      R := -8.573E-01;
    end;
  if W = 119 then
    begin
      R := -8.871E-01;
    end;
  if W = 118 then
    begin
      R := -9.175E-01;
    end;
  if W = 117 then
    begin
      R := -9.486E-01;
    end;
  if W = 116 then
    begin
      R := -9.805E-01;
    end;
  if W = 115 then
    begin
      R := -1.013E+00;
    end;
  if W = 114 then
    begin
      R := -1.046E+00;
    end;
  if W = 113 then
    begin
      R := -1.080E+00;
    end;
  if W = 112 then
    begin
      R := -1.115E+00;
    end;
  if W = 111 then
    begin
      R := -1.151E+00;
    end;
  if W = 110 then
    begin
      R := -1.187E+00;
    end;
  if W = 109 then
    begin
      R := -1.224E+00;
    end;
  if W = 108 then
    begin
      R := -1.262E+00;
    end;
  if W = 107 then
    begin
      R := -1.301E+00;
    end;
  if W = 106 then
    begin
      R := -1.340E+00;
    end;
  if W = 105 then
    begin
      R := -1.381E+00;
    end;
  if W = 104 then
    begin
      R := -1.422E+00;
    end;
  if W = 103 then
    begin
      R := -1.464E+00;
    end;
  if W = 102 then
    begin
      R := -1.506E+00;
    end;
  if W = 101 then
    begin
      R := -1.550E+00;
    end;
  if W = 100 then
    begin
      R := -1.594E+00;
    end;
  if W = 99 then
    begin
      R := -1.640E+00;
    end;
  if W = 98 then
    begin
      R := -1.686E+00;
    end;
  if W = 97 then
    begin
      R := -1.733E+00;
    end;
  if W = 96 then
    begin
      R := -1.781E+00;
    end;
  if W = 95 then
    begin
      R := -1.830E+00;
    end;
  if W = 94 then
    begin
      R := -1.880E+00;
    end;
  if W = 93 then
    begin
      R := -1.930E+00;
    end;
  if W = 92 then
    begin
      R := -1.982E+00;
    end;
  if W = 91 then
    begin
      R := -2.034E+00;
    end;
  if W = 90 then
    begin
      R := -2.088E+00;
    end;
  if W = 89 then
    begin
      R := -2.142E+00;
    end;
  if W = 88 then
    begin
      R := -2.198E+00;
    end;
  if W = 87 then
    begin
      R := -2.254E+00;
    end;
  if W = 86 then
    begin
      R := -2.312E+00;
    end;
  if W = 85 then
    begin
      R := -2.370E+00;
    end;
  if W = 84 then
    begin
      R := -2.429E+00;
    end;
  if W = 83 then
    begin
      R := -2.490E+00;
    end;
  if W = 82 then
    begin
      R := -2.551E+00;
    end;
  if W = 81 then
    begin
      R := -2.614E+00;
    end;
  if W = 80 then
    begin
      R := -2.677E+00;
    end;
  if W = 79 then
    begin
      R := -2.742E+00;
    end;
  if W = 78 then
    begin
      R := -2.808E+00;
    end;
  if W = 77 then
    begin
      R := -2.875E+00;
    end;
  if W = 76 then
    begin
      R := -2.943E+00;
    end;
  if W = 75 then
    begin
      R := -3.012E+00;
    end;
  if W = 74 then
    begin
      R := -3.082E+00;
    end;
  if W = 73 then
    begin
      R := -3.153E+00;
    end;
  if W = 72 then
    begin
      R := -3.226E+00;
    end;
  if W = 71 then
    begin
      R := -3.300E+00;
    end;
  if W = 70 then
    begin
      R := -3.375E+00;
    end;
  if W = 69 then
    begin
      R := -3.451E+00;
    end;
  if W = 68 then
    begin
      R := -3.529E+00;
    end;
  if W = 67 then
    begin
      R := -3.607E+00;
    end;
  if W = 66 then
    begin
      R := -3.687E+00;
    end;
  if W = 65 then
    begin
      R := -3.769E+00;
    end;
  if W = 64 then
    begin
      R := -3.851E+00;
    end;
  if W = 63 then
    begin
      R := -3.935E+00;
    end;
  if W = 62 then
    begin
      R := -4.021E+00;
    end;
  if W = 61 then
    begin
      R := -4.108E+00;
    end;
  if W = 60 then
    begin
      R := -4.196E+00;
    end;
  if W = 59 then
    begin
      R := -4.285E+00;
    end;
  if W = 58 then
    begin
      R := -4.376E+00;
    end;
  if W = 57 then
    begin
      R := -4.469E+00;
    end;
  if W = 56 then
    begin
      R := -4.563E+00;
    end;
  if W = 55 then
    begin
      R := -4.659E+00;
    end;
  if W = 54 then
    begin
      R := -4.756E+00;
    end;
  if W = 53 then
    begin
      R := -4.855E+00;
    end;
  if W = 52 then
    begin
      R := -4.955E+00;
    end;
  if W = 51 then
    begin
      R := -5.057E+00;
    end;
  if W = 50 then
    begin
      R := -5.161E+00;
    end;
  if W = 49 then
    begin
      R := -5.266E+00;
    end;
  if W = 48 then
    begin
      R := -5.374E+00;
    end;
  if W = 47 then
    begin
      R := -5.483E+00;
    end;
  if W = 46 then
    begin
      R := -5.594E+00;
    end;
  if W = 45 then
    begin
      R := -5.706E+00;
    end;
  if W = 44 then
    begin
      R := -5.821E+00;
    end;
  if W = 43 then
    begin
      R := -5.938E+00;
    end;
  if W = 42 then
    begin
      R := -6.057E+00;
    end;
  if W = 41 then
    begin
      R := -6.177E+00;
    end;
  if W = 40 then
    begin
      R := -6.300E+00;
    end;
  if W = 39 then
    begin
      R := -6.426E+00;
    end;
  if W = 38 then
    begin
      R := -6.553E+00;
    end;
  if W = 37 then
    begin
      R := -6.683E+00;
    end;
  if W = 36 then
    begin
      R := -6.815E+00;
    end;
  if W = 35 then
    begin
      R := -6.949E+00;
    end;
  if W = 34 then
    begin
      R := -7.086E+00;
    end;
  if W = 33 then
    begin
      R := -7.226E+00;
    end;
  if W = 32 then
    begin
      R := -7.368E+00;
    end;
  if W = 31 then
    begin
      R := -7.513E+00;
    end;
  if W = 30 then
    begin
      R := -7.661E+00;
    end;
  if W = 29 then
    begin
      R := -7.813E+00;
    end;
  if W = 28 then
    begin
      R := -7.966E+00;
    end;
  if W = 27 then
    begin
      R := -8.124E+00;
    end;
  if W = 26 then
    begin
      R := -8.285E+00;
    end;
  if W = 25 then
    begin
      R := -8.449E+00;
    end;
  if W = 24 then
    begin
      R := -8.617E+00;
    end;
  if W = 23 then
    begin
      R := -8.789E+00;
    end;
  if W = 22 then
    begin
      R := -8.965E+00;
    end;
  if W = 21 then
    begin
      R := -9.147E+00;
    end;
  if W = 20 then
    begin
      R := -9.333E+00;
    end;
  if W = 19 then
    begin
      R := -9.522E+00;
    end;
  if W = 18 then
    begin
      R := -9.716E+00;
    end;
  if W = 17 then
    begin
      R := -9.917E+00;
    end;
  if W = 16 then
    begin
      R := -1.012E+01;
    end;
  if W = 15 then
    begin
      R := -1.033E+01;
    end;
  if W = 14 then
    begin
      R := -1.055E+01;
    end;
  if W = 13 then
    begin
      R := -1.077E+01;
    end;
  if W = 12 then
    begin
      R := -1.100E+01;
    end;
  if W = 11 then
    begin
      R := -1.124E+01;
    end;
  if W = 10 then
    begin
      R := -1.149E+01;
    end;
  if W = 9 then
    begin
      R := -1.175E+01;
    end;
  if W = 8 then
    begin
      R := -1.203E+01;
    end;
  if W = 7 then
    begin
      R := -1.230E+01;
    end;
  if W = 6 then
    begin
      R := -1.261E+01;
    end;
  if W = 5 then
    begin
      R := -1.295E+01;
    end;
  if W = 4 then
    begin
      R := -1.330E+01;
    end;
  if W = 3 then
    begin
      R := -1.364E+01;
    end;
  if W = 2 then
    begin
      R := -1.415E+01;
    end;
  if W = 1 then
    begin
      R := -1.456E+01;
    end;
  if W <= 0 then
    begin
      R := -1.525E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 23)
  ************************************************************************ *)
function W23(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-3.287856E+01 * S + 1.380000E+02);
  if W >= 138 then
    begin
      R := -6.813E-01;
    end;
  if W = 137 then
    begin
      R := -7.051E-01;
    end;
  if W = 136 then
    begin
      R := -7.295E-01;
    end;
  if W = 135 then
    begin
      R := -7.544E-01;
    end;
  if W = 134 then
    begin
      R := -7.800E-01;
    end;
  if W = 133 then
    begin
      R := -8.061E-01;
    end;
  if W = 132 then
    begin
      R := -8.328E-01;
    end;
  if W = 131 then
    begin
      R := -8.601E-01;
    end;
  if W = 130 then
    begin
      R := -8.880E-01;
    end;
  if W = 129 then
    begin
      R := -9.166E-01;
    end;
  if W = 128 then
    begin
      R := -9.457E-01;
    end;
  if W = 127 then
    begin
      R := -9.755E-01;
    end;
  if W = 126 then
    begin
      R := -1.006E+00;
    end;
  if W = 125 then
    begin
      R := -1.037E+00;
    end;
  if W = 124 then
    begin
      R := -1.069E+00;
    end;
  if W = 123 then
    begin
      R := -1.101E+00;
    end;
  if W = 122 then
    begin
      R := -1.134E+00;
    end;
  if W = 121 then
    begin
      R := -1.168E+00;
    end;
  if W = 120 then
    begin
      R := -1.202E+00;
    end;
  if W = 119 then
    begin
      R := -1.237E+00;
    end;
  if W = 118 then
    begin
      R := -1.273E+00;
    end;
  if W = 117 then
    begin
      R := -1.309E+00;
    end;
  if W = 116 then
    begin
      R := -1.347E+00;
    end;
  if W = 115 then
    begin
      R := -1.384E+00;
    end;
  if W = 114 then
    begin
      R := -1.423E+00;
    end;
  if W = 113 then
    begin
      R := -1.462E+00;
    end;
  if W = 112 then
    begin
      R := -1.502E+00;
    end;
  if W = 111 then
    begin
      R := -1.543E+00;
    end;
  if W = 110 then
    begin
      R := -1.585E+00;
    end;
  if W = 109 then
    begin
      R := -1.627E+00;
    end;
  if W = 108 then
    begin
      R := -1.670E+00;
    end;
  if W = 107 then
    begin
      R := -1.714E+00;
    end;
  if W = 106 then
    begin
      R := -1.758E+00;
    end;
  if W = 105 then
    begin
      R := -1.804E+00;
    end;
  if W = 104 then
    begin
      R := -1.850E+00;
    end;
  if W = 103 then
    begin
      R := -1.897E+00;
    end;
  if W = 102 then
    begin
      R := -1.944E+00;
    end;
  if W = 101 then
    begin
      R := -1.993E+00;
    end;
  if W = 100 then
    begin
      R := -2.042E+00;
    end;
  if W = 99 then
    begin
      R := -2.093E+00;
    end;
  if W = 98 then
    begin
      R := -2.144E+00;
    end;
  if W = 97 then
    begin
      R := -2.195E+00;
    end;
  if W = 96 then
    begin
      R := -2.248E+00;
    end;
  if W = 95 then
    begin
      R := -2.302E+00;
    end;
  if W = 94 then
    begin
      R := -2.356E+00;
    end;
  if W = 93 then
    begin
      R := -2.412E+00;
    end;
  if W = 92 then
    begin
      R := -2.468E+00;
    end;
  if W = 91 then
    begin
      R := -2.525E+00;
    end;
  if W = 90 then
    begin
      R := -2.583E+00;
    end;
  if W = 89 then
    begin
      R := -2.642E+00;
    end;
  if W = 88 then
    begin
      R := -2.702E+00;
    end;
  if W = 87 then
    begin
      R := -2.763E+00;
    end;
  if W = 86 then
    begin
      R := -2.825E+00;
    end;
  if W = 85 then
    begin
      R := -2.888E+00;
    end;
  if W = 84 then
    begin
      R := -2.951E+00;
    end;
  if W = 83 then
    begin
      R := -3.016E+00;
    end;
  if W = 82 then
    begin
      R := -3.082E+00;
    end;
  if W = 81 then
    begin
      R := -3.149E+00;
    end;
  if W = 80 then
    begin
      R := -3.216E+00;
    end;
  if W = 79 then
    begin
      R := -3.285E+00;
    end;
  if W = 78 then
    begin
      R := -3.355E+00;
    end;
  if W = 77 then
    begin
      R := -3.426E+00;
    end;
  if W = 76 then
    begin
      R := -3.498E+00;
    end;
  if W = 75 then
    begin
      R := -3.571E+00;
    end;
  if W = 74 then
    begin
      R := -3.645E+00;
    end;
  if W = 73 then
    begin
      R := -3.721E+00;
    end;
  if W = 72 then
    begin
      R := -3.797E+00;
    end;
  if W = 71 then
    begin
      R := -3.875E+00;
    end;
  if W = 70 then
    begin
      R := -3.953E+00;
    end;
  if W = 69 then
    begin
      R := -4.033E+00;
    end;
  if W = 68 then
    begin
      R := -4.114E+00;
    end;
  if W = 67 then
    begin
      R := -4.197E+00;
    end;
  if W = 66 then
    begin
      R := -4.280E+00;
    end;
  if W = 65 then
    begin
      R := -4.365E+00;
    end;
  if W = 64 then
    begin
      R := -4.451E+00;
    end;
  if W = 63 then
    begin
      R := -4.539E+00;
    end;
  if W = 62 then
    begin
      R := -4.628E+00;
    end;
  if W = 61 then
    begin
      R := -4.718E+00;
    end;
  if W = 60 then
    begin
      R := -4.809E+00;
    end;
  if W = 59 then
    begin
      R := -4.902E+00;
    end;
  if W = 58 then
    begin
      R := -4.996E+00;
    end;
  if W = 57 then
    begin
      R := -5.092E+00;
    end;
  if W = 56 then
    begin
      R := -5.189E+00;
    end;
  if W = 55 then
    begin
      R := -5.287E+00;
    end;
  if W = 54 then
    begin
      R := -5.388E+00;
    end;
  if W = 53 then
    begin
      R := -5.489E+00;
    end;
  if W = 52 then
    begin
      R := -5.592E+00;
    end;
  if W = 51 then
    begin
      R := -5.697E+00;
    end;
  if W = 50 then
    begin
      R := -5.804E+00;
    end;
  if W = 49 then
    begin
      R := -5.912E+00;
    end;
  if W = 48 then
    begin
      R := -6.022E+00;
    end;
  if W = 47 then
    begin
      R := -6.133E+00;
    end;
  if W = 46 then
    begin
      R := -6.247E+00;
    end;
  if W = 45 then
    begin
      R := -6.362E+00;
    end;
  if W = 44 then
    begin
      R := -6.479E+00;
    end;
  if W = 43 then
    begin
      R := -6.598E+00;
    end;
  if W = 42 then
    begin
      R := -6.719E+00;
    end;
  if W = 41 then
    begin
      R := -6.842E+00;
    end;
  if W = 40 then
    begin
      R := -6.967E+00;
    end;
  if W = 39 then
    begin
      R := -7.094E+00;
    end;
  if W = 38 then
    begin
      R := -7.224E+00;
    end;
  if W = 37 then
    begin
      R := -7.355E+00;
    end;
  if W = 36 then
    begin
      R := -7.489E+00;
    end;
  if W = 35 then
    begin
      R := -7.625E+00;
    end;
  if W = 34 then
    begin
      R := -7.764E+00;
    end;
  if W = 33 then
    begin
      R := -7.905E+00;
    end;
  if W = 32 then
    begin
      R := -8.049E+00;
    end;
  if W = 31 then
    begin
      R := -8.196E+00;
    end;
  if W = 30 then
    begin
      R := -8.345E+00;
    end;
  if W = 29 then
    begin
      R := -8.498E+00;
    end;
  if W = 28 then
    begin
      R := -8.653E+00;
    end;
  if W = 27 then
    begin
      R := -8.811E+00;
    end;
  if W = 26 then
    begin
      R := -8.974E+00;
    end;
  if W = 25 then
    begin
      R := -9.139E+00;
    end;
  if W = 24 then
    begin
      R := -9.308E+00;
    end;
  if W = 23 then
    begin
      R := -9.481E+00;
    end;
  if W = 22 then
    begin
      R := -9.658E+00;
    end;
  if W = 21 then
    begin
      R := -9.840E+00;
    end;
  if W = 20 then
    begin
      R := -1.003E+01;
    end;
  if W = 19 then
    begin
      R := -1.022E+01;
    end;
  if W = 18 then
    begin
      R := -1.041E+01;
    end;
  if W = 17 then
    begin
      R := -1.061E+01;
    end;
  if W = 16 then
    begin
      R := -1.081E+01;
    end;
  if W = 15 then
    begin
      R := -1.102E+01;
    end;
  if W = 14 then
    begin
      R := -1.124E+01;
    end;
  if W = 13 then
    begin
      R := -1.147E+01;
    end;
  if W = 12 then
    begin
      R := -1.169E+01;
    end;
  if W = 11 then
    begin
      R := -1.194E+01;
    end;
  if W = 10 then
    begin
      R := -1.218E+01;
    end;
  if W = 9 then
    begin
      R := -1.245E+01;
    end;
  if W = 8 then
    begin
      R := -1.272E+01;
    end;
  if W = 7 then
    begin
      R := -1.300E+01;
    end;
  if W = 6 then
    begin
      R := -1.330E+01;
    end;
  if W = 5 then
    begin
      R := -1.364E+01;
    end;
  if W = 4 then
    begin
      R := -1.400E+01;
    end;
  if W = 3 then
    begin
      R := -1.433E+01;
    end;
  if W = 2 then
    begin
      R := -1.484E+01;
    end;
  if W = 1 then
    begin
      R := -1.525E+01;
    end;
  if W <= 0 then
    begin
      R := -1.594E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 24)
  ************************************************************************ *)
function W24(const S: TLFloat): TLFloat;
var
  W: TLInt;
  R: TLFloat;
begin
  W := Round(-3.500000E+01 * S + 1.500000E+02);
  if W >= 150 then
    begin
      R := -6.820E-01;
    end;
  if W = 149 then
    begin
      R := -7.044E-01;
    end;
  if W = 148 then
    begin
      R := -7.273E-01;
    end;
  if W = 147 then
    begin
      R := -7.507E-01;
    end;
  if W = 146 then
    begin
      R := -7.746E-01;
    end;
  if W = 145 then
    begin
      R := -7.990E-01;
    end;
  if W = 144 then
    begin
      R := -8.239E-01;
    end;
  if W = 143 then
    begin
      R := -8.494E-01;
    end;
  if W = 142 then
    begin
      R := -8.754E-01;
    end;
  if W = 141 then
    begin
      R := -9.020E-01;
    end;
  if W = 140 then
    begin
      R := -9.291E-01;
    end;
  if W = 139 then
    begin
      R := -9.567E-01;
    end;
  if W = 138 then
    begin
      R := -9.849E-01;
    end;
  if W = 137 then
    begin
      R := -1.014E+00;
    end;
  if W = 136 then
    begin
      R := -1.043E+00;
    end;
  if W = 135 then
    begin
      R := -1.073E+00;
    end;
  if W = 134 then
    begin
      R := -1.103E+00;
    end;
  if W = 133 then
    begin
      R := -1.135E+00;
    end;
  if W = 132 then
    begin
      R := -1.166E+00;
    end;
  if W = 131 then
    begin
      R := -1.198E+00;
    end;
  if W = 130 then
    begin
      R := -1.231E+00;
    end;
  if W = 129 then
    begin
      R := -1.265E+00;
    end;
  if W = 128 then
    begin
      R := -1.299E+00;
    end;
  if W = 127 then
    begin
      R := -1.334E+00;
    end;
  if W = 126 then
    begin
      R := -1.369E+00;
    end;
  if W = 125 then
    begin
      R := -1.405E+00;
    end;
  if W = 124 then
    begin
      R := -1.441E+00;
    end;
  if W = 123 then
    begin
      R := -1.479E+00;
    end;
  if W = 122 then
    begin
      R := -1.517E+00;
    end;
  if W = 121 then
    begin
      R := -1.555E+00;
    end;
  if W = 120 then
    begin
      R := -1.594E+00;
    end;
  if W = 119 then
    begin
      R := -1.634E+00;
    end;
  if W = 118 then
    begin
      R := -1.675E+00;
    end;
  if W = 117 then
    begin
      R := -1.716E+00;
    end;
  if W = 116 then
    begin
      R := -1.758E+00;
    end;
  if W = 115 then
    begin
      R := -1.800E+00;
    end;
  if W = 114 then
    begin
      R := -1.844E+00;
    end;
  if W = 113 then
    begin
      R := -1.888E+00;
    end;
  if W = 112 then
    begin
      R := -1.932E+00;
    end;
  if W = 111 then
    begin
      R := -1.978E+00;
    end;
  if W = 110 then
    begin
      R := -2.024E+00;
    end;
  if W = 109 then
    begin
      R := -2.070E+00;
    end;
  if W = 108 then
    begin
      R := -2.118E+00;
    end;
  if W = 107 then
    begin
      R := -2.166E+00;
    end;
  if W = 106 then
    begin
      R := -2.215E+00;
    end;
  if W = 105 then
    begin
      R := -2.265E+00;
    end;
  if W = 104 then
    begin
      R := -2.316E+00;
    end;
  if W = 103 then
    begin
      R := -2.367E+00;
    end;
  if W = 102 then
    begin
      R := -2.419E+00;
    end;
  if W = 101 then
    begin
      R := -2.472E+00;
    end;
  if W = 100 then
    begin
      R := -2.526E+00;
    end;
  if W = 99 then
    begin
      R := -2.580E+00;
    end;
  if W = 98 then
    begin
      R := -2.636E+00;
    end;
  if W = 97 then
    begin
      R := -2.692E+00;
    end;
  if W = 96 then
    begin
      R := -2.749E+00;
    end;
  if W = 95 then
    begin
      R := -2.806E+00;
    end;
  if W = 94 then
    begin
      R := -2.865E+00;
    end;
  if W = 93 then
    begin
      R := -2.925E+00;
    end;
  if W = 92 then
    begin
      R := -2.985E+00;
    end;
  if W = 91 then
    begin
      R := -3.046E+00;
    end;
  if W = 90 then
    begin
      R := -3.108E+00;
    end;
  if W = 89 then
    begin
      R := -3.171E+00;
    end;
  if W = 88 then
    begin
      R := -3.235E+00;
    end;
  if W = 87 then
    begin
      R := -3.300E+00;
    end;
  if W = 86 then
    begin
      R := -3.365E+00;
    end;
  if W = 85 then
    begin
      R := -3.432E+00;
    end;
  if W = 84 then
    begin
      R := -3.499E+00;
    end;
  if W = 83 then
    begin
      R := -3.568E+00;
    end;
  if W = 82 then
    begin
      R := -3.637E+00;
    end;
  if W = 81 then
    begin
      R := -3.708E+00;
    end;
  if W = 80 then
    begin
      R := -3.779E+00;
    end;
  if W = 79 then
    begin
      R := -3.852E+00;
    end;
  if W = 78 then
    begin
      R := -3.925E+00;
    end;
  if W = 77 then
    begin
      R := -4.000E+00;
    end;
  if W = 76 then
    begin
      R := -4.075E+00;
    end;
  if W = 75 then
    begin
      R := -4.151E+00;
    end;
  if W = 74 then
    begin
      R := -4.229E+00;
    end;
  if W = 73 then
    begin
      R := -4.308E+00;
    end;
  if W = 72 then
    begin
      R := -4.387E+00;
    end;
  if W = 71 then
    begin
      R := -4.468E+00;
    end;
  if W = 70 then
    begin
      R := -4.550E+00;
    end;
  if W = 69 then
    begin
      R := -4.633E+00;
    end;
  if W = 68 then
    begin
      R := -4.718E+00;
    end;
  if W = 67 then
    begin
      R := -4.803E+00;
    end;
  if W = 66 then
    begin
      R := -4.890E+00;
    end;
  if W = 65 then
    begin
      R := -4.978E+00;
    end;
  if W = 64 then
    begin
      R := -5.067E+00;
    end;
  if W = 63 then
    begin
      R := -5.157E+00;
    end;
  if W = 62 then
    begin
      R := -5.249E+00;
    end;
  if W = 61 then
    begin
      R := -5.342E+00;
    end;
  if W = 60 then
    begin
      R := -5.436E+00;
    end;
  if W = 59 then
    begin
      R := -5.531E+00;
    end;
  if W = 58 then
    begin
      R := -5.628E+00;
    end;
  if W = 57 then
    begin
      R := -5.727E+00;
    end;
  if W = 56 then
    begin
      R := -5.826E+00;
    end;
  if W = 55 then
    begin
      R := -5.927E+00;
    end;
  if W = 54 then
    begin
      R := -6.030E+00;
    end;
  if W = 53 then
    begin
      R := -6.134E+00;
    end;
  if W = 52 then
    begin
      R := -6.240E+00;
    end;
  if W = 51 then
    begin
      R := -6.347E+00;
    end;
  if W = 50 then
    begin
      R := -6.456E+00;
    end;
  if W = 49 then
    begin
      R := -6.566E+00;
    end;
  if W = 48 then
    begin
      R := -6.678E+00;
    end;
  if W = 47 then
    begin
      R := -6.792E+00;
    end;
  if W = 46 then
    begin
      R := -6.907E+00;
    end;
  if W = 45 then
    begin
      R := -7.025E+00;
    end;
  if W = 44 then
    begin
      R := -7.144E+00;
    end;
  if W = 43 then
    begin
      R := -7.265E+00;
    end;
  if W = 42 then
    begin
      R := -7.387E+00;
    end;
  if W = 41 then
    begin
      R := -7.512E+00;
    end;
  if W = 40 then
    begin
      R := -7.639E+00;
    end;
  if W = 39 then
    begin
      R := -7.768E+00;
    end;
  if W = 38 then
    begin
      R := -7.899E+00;
    end;
  if W = 37 then
    begin
      R := -8.032E+00;
    end;
  if W = 36 then
    begin
      R := -8.167E+00;
    end;
  if W = 35 then
    begin
      R := -8.305E+00;
    end;
  if W = 34 then
    begin
      R := -8.445E+00;
    end;
  if W = 33 then
    begin
      R := -8.588E+00;
    end;
  if W = 32 then
    begin
      R := -8.733E+00;
    end;
  if W = 31 then
    begin
      R := -8.881E+00;
    end;
  if W = 30 then
    begin
      R := -9.031E+00;
    end;
  if W = 29 then
    begin
      R := -9.185E+00;
    end;
  if W = 28 then
    begin
      R := -9.341E+00;
    end;
  if W = 27 then
    begin
      R := -9.501E+00;
    end;
  if W = 26 then
    begin
      R := -9.664E+00;
    end;
  if W = 25 then
    begin
      R := -9.830E+00;
    end;
  if W = 24 then
    begin
      R := -1.000E+01;
    end;
  if W = 23 then
    begin
      R := -1.017E+01;
    end;
  if W = 22 then
    begin
      R := -1.035E+01;
    end;
  if W = 21 then
    begin
      R := -1.053E+01;
    end;
  if W = 20 then
    begin
      R := -1.072E+01;
    end;
  if W = 19 then
    begin
      R := -1.091E+01;
    end;
  if W = 18 then
    begin
      R := -1.110E+01;
    end;
  if W = 17 then
    begin
      R := -1.130E+01;
    end;
  if W = 16 then
    begin
      R := -1.151E+01;
    end;
  if W = 15 then
    begin
      R := -1.172E+01;
    end;
  if W = 14 then
    begin
      R := -1.194E+01;
    end;
  if W = 13 then
    begin
      R := -1.216E+01;
    end;
  if W = 12 then
    begin
      R := -1.239E+01;
    end;
  if W = 11 then
    begin
      R := -1.263E+01;
    end;
  if W = 10 then
    begin
      R := -1.287E+01;
    end;
  if W = 9 then
    begin
      R := -1.314E+01;
    end;
  if W = 8 then
    begin
      R := -1.342E+01;
    end;
  if W = 7 then
    begin
      R := -1.369E+01;
    end;
  if W = 6 then
    begin
      R := -1.400E+01;
    end;
  if W = 5 then
    begin
      R := -1.433E+01;
    end;
  if W = 4 then
    begin
      R := -1.469E+01;
    end;
  if W = 3 then
    begin
      R := -1.503E+01;
    end;
  if W = 2 then
    begin
      R := -1.554E+01;
    end;
  if W = 1 then
    begin
      R := -1.594E+01;
    end;
  if W <= 0 then
    begin
      R := -1.664E+01;
    end;
  Result := R;
end;

(* ************************************************************************
  Tail(S, 25)
  ************************************************************************ *)
function W25(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -5.150509E+00, TJ, TJ1, Result);
  WCheb(X, -5.695528E+00, TJ, TJ1, Result);
  WCheb(X, -1.437637E+00, TJ, TJ1, Result);
  WCheb(X, -2.611906E-01, TJ, TJ1, Result);
  WCheb(X, -7.625722E-02, TJ, TJ1, Result);
  WCheb(X, -2.579892E-02, TJ, TJ1, Result);
  WCheb(X, -1.086876E-02, TJ, TJ1, Result);
  WCheb(X, -2.906543E-03, TJ, TJ1, Result);
  WCheb(X, -2.354881E-03, TJ, TJ1, Result);
  WCheb(X, 1.007195E-04, TJ, TJ1, Result);
  WCheb(X, -8.437327E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 26)
  ************************************************************************ *)
function W26(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -5.117622E+00, TJ, TJ1, Result);
  WCheb(X, -5.635159E+00, TJ, TJ1, Result);
  WCheb(X, -1.395167E+00, TJ, TJ1, Result);
  WCheb(X, -2.382823E-01, TJ, TJ1, Result);
  WCheb(X, -6.531987E-02, TJ, TJ1, Result);
  WCheb(X, -2.060112E-02, TJ, TJ1, Result);
  WCheb(X, -8.203697E-03, TJ, TJ1, Result);
  WCheb(X, -1.516523E-03, TJ, TJ1, Result);
  WCheb(X, -1.431364E-03, TJ, TJ1, Result);
  WCheb(X, 6.384553E-04, TJ, TJ1, Result);
  WCheb(X, -3.238369E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 27)
  ************************************************************************ *)
function W27(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -5.089731E+00, TJ, TJ1, Result);
  WCheb(X, -5.584248E+00, TJ, TJ1, Result);
  WCheb(X, -1.359966E+00, TJ, TJ1, Result);
  WCheb(X, -2.203696E-01, TJ, TJ1, Result);
  WCheb(X, -5.753344E-02, TJ, TJ1, Result);
  WCheb(X, -1.761891E-02, TJ, TJ1, Result);
  WCheb(X, -7.096897E-03, TJ, TJ1, Result);
  WCheb(X, -1.419108E-03, TJ, TJ1, Result);
  WCheb(X, -1.581214E-03, TJ, TJ1, Result);
  WCheb(X, 3.033766E-04, TJ, TJ1, Result);
  WCheb(X, -5.901441E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 28)
  ************************************************************************ *)
function W28(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -5.065046E+00, TJ, TJ1, Result);
  WCheb(X, -5.539163E+00, TJ, TJ1, Result);
  WCheb(X, -1.328939E+00, TJ, TJ1, Result);
  WCheb(X, -2.046376E-01, TJ, TJ1, Result);
  WCheb(X, -5.061515E-02, TJ, TJ1, Result);
  WCheb(X, -1.469271E-02, TJ, TJ1, Result);
  WCheb(X, -5.711578E-03, TJ, TJ1, Result);
  WCheb(X, -8.389153E-04, TJ, TJ1, Result);
  WCheb(X, -1.250575E-03, TJ, TJ1, Result);
  WCheb(X, 4.047245E-04, TJ, TJ1, Result);
  WCheb(X, -5.128555E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 29)
  ************************************************************************ *)
function W29(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -5.043413E+00, TJ, TJ1, Result);
  WCheb(X, -5.499756E+00, TJ, TJ1, Result);
  WCheb(X, -1.302137E+00, TJ, TJ1, Result);
  WCheb(X, -1.915129E-01, TJ, TJ1, Result);
  WCheb(X, -4.516329E-02, TJ, TJ1, Result);
  WCheb(X, -1.260064E-02, TJ, TJ1, Result);
  WCheb(X, -4.817269E-03, TJ, TJ1, Result);
  WCheb(X, -5.478130E-04, TJ, TJ1, Result);
  WCheb(X, -1.111668E-03, TJ, TJ1, Result);
  WCheb(X, 4.093451E-04, TJ, TJ1, Result);
  WCheb(X, -5.135860E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 30)
  ************************************************************************ *)
function W30(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -5.024071E+00, TJ, TJ1, Result);
  WCheb(X, -5.464515E+00, TJ, TJ1, Result);
  WCheb(X, -1.278342E+00, TJ, TJ1, Result);
  WCheb(X, -1.800030E-01, TJ, TJ1, Result);
  WCheb(X, -4.046294E-02, TJ, TJ1, Result);
  WCheb(X, -1.076162E-02, TJ, TJ1, Result);
  WCheb(X, -3.968677E-03, TJ, TJ1, Result);
  WCheb(X, -1.911679E-04, TJ, TJ1, Result);
  WCheb(X, -8.619185E-04, TJ, TJ1, Result);
  WCheb(X, 5.125362E-04, TJ, TJ1, Result);
  WCheb(X, -3.984370E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 40)
  ************************************************************************ *)
function W40(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -4.904809E+00, TJ, TJ1, Result);
  WCheb(X, -5.248327E+00, TJ, TJ1, Result);
  WCheb(X, -1.136698E+00, TJ, TJ1, Result);
  WCheb(X, -1.170982E-01, TJ, TJ1, Result);
  WCheb(X, -1.824427E-02, TJ, TJ1, Result);
  WCheb(X, -3.888648E-03, TJ, TJ1, Result);
  WCheb(X, -1.344929E-03, TJ, TJ1, Result);
  WCheb(X, 2.790407E-04, TJ, TJ1, Result);
  WCheb(X, -4.619858E-04, TJ, TJ1, Result);
  WCheb(X, 3.359121E-04, TJ, TJ1, Result);
  WCheb(X, -2.883026E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 60)
  ************************************************************************ *)
function W60(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -4.809656E+00, TJ, TJ1, Result);
  WCheb(X, -5.077191E+00, TJ, TJ1, Result);
  WCheb(X, -1.029402E+00, TJ, TJ1, Result);
  WCheb(X, -7.507931E-02, TJ, TJ1, Result);
  WCheb(X, -6.506226E-03, TJ, TJ1, Result);
  WCheb(X, -1.391278E-03, TJ, TJ1, Result);
  WCheb(X, -4.263635E-04, TJ, TJ1, Result);
  WCheb(X, 2.302271E-04, TJ, TJ1, Result);
  WCheb(X, -2.384348E-04, TJ, TJ1, Result);
  WCheb(X, 1.865587E-04, TJ, TJ1, Result);
  WCheb(X, -1.622355E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 120)
  ************************************************************************ *)
function W120(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -4.729426E+00, TJ, TJ1, Result);
  WCheb(X, -4.934426E+00, TJ, TJ1, Result);
  WCheb(X, -9.433231E-01, TJ, TJ1, Result);
  WCheb(X, -4.492504E-02, TJ, TJ1, Result);
  WCheb(X, 1.673948E-05, TJ, TJ1, Result);
  WCheb(X, -6.077014E-04, TJ, TJ1, Result);
  WCheb(X, -7.215768E-05, TJ, TJ1, Result);
  WCheb(X, 9.086734E-05, TJ, TJ1, Result);
  WCheb(X, -8.447980E-05, TJ, TJ1, Result);
  WCheb(X, 6.705028E-05, TJ, TJ1, Result);
  WCheb(X, -5.828507E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 200)
  ************************************************************************ *)
function W200(const S: TLFloat): TLFloat;
var
  X  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  X := Min(2 * (S - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := X;
  WCheb(X, -4.700240E+00, TJ, TJ1, Result);
  WCheb(X, -4.883080E+00, TJ, TJ1, Result);
  WCheb(X, -9.132168E-01, TJ, TJ1, Result);
  WCheb(X, -3.512684E-02, TJ, TJ1, Result);
  WCheb(X, 1.726342E-03, TJ, TJ1, Result);
  WCheb(X, -5.189796E-04, TJ, TJ1, Result);
  WCheb(X, -1.628659E-06, TJ, TJ1, Result);
  WCheb(X, 4.261786E-05, TJ, TJ1, Result);
  WCheb(X, -4.002498E-05, TJ, TJ1, Result);
  WCheb(X, 3.146287E-05, TJ, TJ1, Result);
  WCheb(X, -2.727576E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S,N), S>=0
  ************************************************************************ *)
function WSigma(const S: TLFloat; const N: TLInt): TLFloat;
var
  F0: TLFloat;
  F1: TLFloat;
  F2: TLFloat;
  F3: TLFloat;
  F4: TLFloat;
  X0: TLFloat;
  X1: TLFloat;
  X2: TLFloat;
  X3: TLFloat;
  X4: TLFloat;
  X : TLFloat;
begin
  if N = 5 then
    begin
      Result := W5(S);
    end;
  if N = 6 then
    begin
      Result := W6(S);
    end;
  if N = 7 then
    begin
      Result := W7(S);
    end;
  if N = 8 then
    begin
      Result := W8(S);
    end;
  if N = 9 then
    begin
      Result := W9(S);
    end;
  if N = 10 then
    begin
      Result := W10(S);
    end;
  if N = 11 then
    begin
      Result := W11(S);
    end;
  if N = 12 then
    begin
      Result := W12(S);
    end;
  if N = 13 then
    begin
      Result := W13(S);
    end;
  if N = 14 then
    begin
      Result := W14(S);
    end;
  if N = 15 then
    begin
      Result := W15(S);
    end;
  if N = 16 then
    begin
      Result := W16(S);
    end;
  if N = 17 then
    begin
      Result := W17(S);
    end;
  if N = 18 then
    begin
      Result := W18(S);
    end;
  if N = 19 then
    begin
      Result := W19(S);
    end;
  if N = 20 then
    begin
      Result := W20(S);
    end;
  if N = 21 then
    begin
      Result := W21(S);
    end;
  if N = 22 then
    begin
      Result := W22(S);
    end;
  if N = 23 then
    begin
      Result := W23(S);
    end;
  if N = 24 then
    begin
      Result := W24(S);
    end;
  if N = 25 then
    begin
      Result := W25(S);
    end;
  if N = 26 then
    begin
      Result := W26(S);
    end;
  if N = 27 then
    begin
      Result := W27(S);
    end;
  if N = 28 then
    begin
      Result := W28(S);
    end;
  if N = 29 then
    begin
      Result := W29(S);
    end;
  if N = 30 then
    begin
      Result := W30(S);
    end;
  if N > 30 then
    begin
      X := 1.0 / N;
      X0 := 1.0 / 30;
      F0 := W30(S);
      X1 := 1.0 / 40;
      F1 := W40(S);
      X2 := 1.0 / 60;
      F2 := W60(S);
      X3 := 1.0 / 120;
      F3 := W120(S);
      X4 := 1.0 / 200;
      F4 := W200(S);
      F1 := ((X - X0) * F1 - (X - X1) * F0) / (X1 - X0);
      F2 := ((X - X0) * F2 - (X - X2) * F0) / (X2 - X0);
      F3 := ((X - X0) * F3 - (X - X3) * F0) / (X3 - X0);
      F4 := ((X - X0) * F4 - (X - X4) * F0) / (X4 - X0);
      F2 := ((X - X1) * F2 - (X - X2) * F1) / (X2 - X1);
      F3 := ((X - X1) * F3 - (X - X3) * F1) / (X3 - X1);
      F4 := ((X - X1) * F4 - (X - X4) * F1) / (X4 - X1);
      F3 := ((X - X2) * F3 - (X - X3) * F2) / (X3 - X2);
      F4 := ((X - X2) * F4 - (X - X4) * F2) / (X4 - X2);
      F4 := ((X - X3) * F4 - (X - X4) * F3) / (X4 - X3);
      Result := F4;
    end;
end;
