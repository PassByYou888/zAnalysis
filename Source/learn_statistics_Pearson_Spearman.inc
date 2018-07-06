{ ****************************************************************************** }
{ * Pearson and Spearman distribution support, by QQ 600585@qq.com             * }
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
  Pearson product-moment correlation coefficient

  Input parameters:
  X       -   sample 1 (array indexes: [0..N-1])
  Y       -   sample 2 (array indexes: [0..N-1])
  N       -   sample size.

  Result:
  Pearson product-moment correlation coefficient
  ************************************************************************ *)
function PearsonCorrelation(const X, Y: TLVec; const n: TLInt): TLFloat;
var
  i    : TLInt;
  XMean: TLFloat;
  YMean: TLFloat;
  s    : TLFloat;
  XV   : TLFloat;
  YV   : TLFloat;
  t1   : TLFloat;
  t2   : TLFloat;
begin
  XV := 0;
  YV := 0;
  if n <= 1 then
    begin
      Result := 0;
      Exit;
    end;

  //
  // Mean
  //
  XMean := 0;
  YMean := 0;
  i := 0;
  while i <= n - 1 do
    begin
      XMean := XMean + X[i];
      YMean := YMean + Y[i];
      Inc(i);
    end;
  XMean := XMean / n;
  YMean := YMean / n;

  //
  // numerator and denominator
  //
  s := 0;
  XV := 0;
  YV := 0;
  i := 0;
  while i <= n - 1 do
    begin
      t1 := X[i] - XMean;
      t2 := Y[i] - YMean;
      XV := XV + AP_Sqr(t1);
      YV := YV + AP_Sqr(t2);
      s := s + t1 * t2;
      Inc(i);
    end;
  if AP_FP_Eq(XV, 0) or AP_FP_Eq(YV, 0) then
      Result := 0
  else
      Result := s / (Sqrt(XV) * Sqrt(YV));
end;

(* ************************************************************************
  Spearman's rank correlation coefficient

  Input parameters:
  X       -   sample 1 (array indexes: [0..N-1])
  Y       -   sample 2 (array indexes: [0..N-1])
  N       -   sample size.

  Result:
  Spearman's rank correlation coefficient
  ************************************************************************ *)
function SpearmanRankCorrelation(const X, Y: TLVec; const n: TLInt): TLFloat;
var
  Nx, Ny: TLVec;
begin
  if n <= 0 then
    begin
      Result := 0;
      Exit;
    end;
  Nx := DynamicArrayCopy(X);
  Ny := DynamicArrayCopy(Y);
  SpearmanRank(Nx, n);
  SpearmanRank(Ny, n);
  Result := PearsonCorrelation(Nx, Ny, n);
  SetLength(Nx, 0);
  SetLength(Ny, 0);
end;

(* ************************************************************************
  Spearman Rank Correlation Coefficient
  ************************************************************************ *)
procedure SpearmanRank(var X: TLVec; n: TLInt);
var
  i   : TLInt;
  J   : TLInt;
  k   : TLInt;
  T   : TLInt;
  tmp : TLFloat;
  TmpI: TLInt;
  R   : TLVec;
  C   : TLIVec;
begin

  //
  // Prepare
  //
  if n < 1 then
      Exit;

  SetLength(R, n (* - 1 + 1 // optimized compiler *) );
  SetLength(C, n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 1 do
    begin
      R[i] := X[i];
      C[i] := i;
      Inc(i);
    end;

  //
  // sort {R, C}
  //
  if n <> 1 then
    begin
      i := 2;
      repeat
        T := i;
        while T <> 1 do
          begin
            k := T div 2;
            if AP_FP_Greater_Eq(R[k - 1], R[T - 1]) then
              begin
                T := 1;
              end
            else
              begin
                tmp := R[k - 1];
                R[k - 1] := R[T - 1];
                R[T - 1] := tmp;
                TmpI := C[k - 1];
                C[k - 1] := C[T - 1];
                C[T - 1] := TmpI;
                T := k;
              end;
          end;
        i := i + 1;
      until not(i <= n);
      i := n - 1;
      repeat
        tmp := R[i];
        R[i] := R[0];
        R[0] := tmp;
        TmpI := C[i];
        C[i] := C[0];
        C[0] := TmpI;
        T := 1;
        while T <> 0 do
          begin
            k := 2 * T;
            if k > i then
              begin
                T := 0;
              end
            else
              begin
                if k < i then
                  begin
                    if AP_FP_Greater(R[k], R[k - 1]) then
                      begin
                        k := k + 1;
                      end;
                  end;
                if AP_FP_Greater_Eq(R[T - 1], R[k - 1]) then
                  begin
                    T := 0;
                  end
                else
                  begin
                    tmp := R[k - 1];
                    R[k - 1] := R[T - 1];
                    R[T - 1] := tmp;
                    TmpI := C[k - 1];
                    C[k - 1] := C[T - 1];
                    C[T - 1] := TmpI;
                    T := k;
                  end;
              end;
          end;
        i := i - 1;
      until not(i >= 1);
    end;

  //
  // compute tied ranks
  //
  i := 0;
  while i <= n - 1 do
    begin
      J := i + 1;
      while J <= n - 1 do
        begin
          if AP_FP_NEq(R[J], R[i]) then
            begin
              Break;
            end;
          J := J + 1;
        end;
      k := i;
      while k <= J - 1 do
        begin
          R[k] := 1 + AP_Float((i + J - 1)) / 2;
          Inc(k);
        end;
      i := J;
    end;

  //
  // back to x
  //
  i := 0;
  while i <= n - 1 do
    begin
      X[C[i]] := R[i];
      Inc(i);
    end;
end;

function SpearmanTail5(const s: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SpearmanTail6(const s: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SpearmanTail7(const s: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SpearmanTail8(const s: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SpearmanTail9(const s: TLFloat): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SpearmanTail(const T: TLFloat; const n: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Pearson's correlation coefficient significance test

  This test checks hypotheses about whether X  and  Y  are  samples  of  two
  continuous  distributions  having  zero  correlation  or   whether   their
  correlation is non-zero.

  The following tests are performed:
  * two-tailed test (null hypothesis - X and Y have zero correlation)
  * left-tailed test (null hypothesis - the correlation coefficient is greater than or equal to 0)
  * right-tailed test (null hypothesis - the correlation coefficient is less than or equal to 0).

  Requirements:
  * the number of elements in each sample is not less than 5
  * normality of distributions of X and Y.

  Input parameters:
  R   -   Pearson's correlation coefficient for X and Y
  N   -   number of elements in samples, N>=5.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.
  ************************************************************************ *)
procedure PearsonCorrelationSignificance(const R: TLFloat; const n: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
var
  T: TLFloat;
  p: TLFloat;
begin

  //
  // Some special cases
  //
  if AP_FP_Greater_Eq(R, 1) then
    begin
      BothTails := 0.0;
      LeftTail := 1.0;
      RightTail := 0.0;
      Exit;
    end;
  if AP_FP_Less_Eq(R, -1) then
    begin
      BothTails := 0.0;
      LeftTail := 0.0;
      RightTail := 1.0;
      Exit;
    end;
  if n < 5 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // General case
  //
  T := R * Sqrt((n - 2) / (1 - AP_Sqr(R)));
  p := StudentTDistribution(n - 2, T);
  BothTails := 2 * Min(p, 1 - p);
  LeftTail := p;
  RightTail := 1 - p;
end;

(* ************************************************************************
  Spearman's rank correlation coefficient significance test

  This test checks hypotheses about whether X  and  Y  are  samples  of  two
  continuous  distributions  having  zero  correlation  or   whether   their
  correlation is non-zero.

  The following tests are performed:
  * two-tailed test (null hypothesis - X and Y have zero correlation)
  * left-tailed test (null hypothesis - the correlation coefficient is greater than or equal to 0)
  * right-tailed test (null hypothesis - the correlation coefficient is less than or equal to 0).

  Requirements:
  * the number of elements in each sample is not less than 5.

  The test is non-parametric and doesn't require distributions X and Y to be
  normal.

  Input parameters:
  R   -   Spearman's rank correlation coefficient for X and Y
  N   -   number of elements in samples, N>=5.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.
  ************************************************************************ *)
procedure SpearmanRankCorrelationSignificance(const R: TLFloat; const n: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
var
  T: TLFloat;
  p: TLFloat;
begin

  //
  // Special case
  //
  if n < 5 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // General case
  //
  if AP_FP_Greater_Eq(R, 1) then
    begin
      T := 1.0E10;
    end
  else
    begin
      if AP_FP_Less_Eq(R, -1) then
        begin
          T := -1.0E10;
        end
      else
        begin
          T := R * Sqrt((n - 2) / (1 - AP_Sqr(R)));
        end;
    end;
  if AP_FP_Less(T, 0) then
    begin
      p := SpearmanTail(T, n);
      BothTails := 2 * p;
      LeftTail := p;
      RightTail := 1 - p;
    end
  else
    begin
      p := SpearmanTail(-T, n);
      BothTails := 2 * p;
      LeftTail := 1 - p;
      RightTail := p;
    end;
end;

(* ************************************************************************
  Tail(S, 5)
  ************************************************************************ *)
function SpearmanTail5(const s: TLFloat): TLFloat;
begin
  if AP_FP_Less(s, 0.000E+00) then
    begin
      Result := StudentTDistribution(3, -s);
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.580E+00) then
    begin
      Result := 8.304E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.322E+00) then
    begin
      Result := 4.163E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.704E+00) then
    begin
      Result := 6.641E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.303E+00) then
    begin
      Result := 1.164E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.003E+00) then
    begin
      Result := 1.748E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 7.584E-01) then
    begin
      Result := 2.249E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 5.468E-01) then
    begin
      Result := 2.581E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.555E-01) then
    begin
      Result := 3.413E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.759E-01) then
    begin
      Result := 3.911E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.741E-03) then
    begin
      Result := 4.747E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 0.000E+00) then
    begin
      Result := 5.248E-01;
      Exit;
    end;
  Result := 0;
end;

(* ************************************************************************
  Tail(S, 6)
  ************************************************************************ *)
function SpearmanTail6(const s: TLFloat): TLFloat;
begin
  if AP_FP_Less(s, 1.001E+00) then
    begin
      Result := StudentTDistribution(4, -s);
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 5.663E+00) then
    begin
      Result := 1.366E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.834E+00) then
    begin
      Result := 8.350E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.968E+00) then
    begin
      Result := 1.668E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.430E+00) then
    begin
      Result := 2.921E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.045E+00) then
    begin
      Result := 5.144E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.747E+00) then
    begin
      Result := 6.797E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.502E+00) then
    begin
      Result := 8.752E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.295E+00) then
    begin
      Result := 1.210E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.113E+00) then
    begin
      Result := 1.487E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.001E+00) then
    begin
      Result := 1.780E-01;
      Exit;
    end;
  Result := 0;
end;

(* ************************************************************************
  Tail(S, 7)
  ************************************************************************ *)
function SpearmanTail7(const s: TLFloat): TLFloat;
begin
  if AP_FP_Less(s, 1.001E+00) then
    begin
      Result := StudentTDistribution(5, -s);
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 8.159E+00) then
    begin
      Result := 2.081E-04;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 5.620E+00) then
    begin
      Result := 1.393E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 4.445E+00) then
    begin
      Result := 3.398E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.728E+00) then
    begin
      Result := 6.187E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.226E+00) then
    begin
      Result := 1.200E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.844E+00) then
    begin
      Result := 1.712E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.539E+00) then
    begin
      Result := 2.408E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.285E+00) then
    begin
      Result := 3.320E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.068E+00) then
    begin
      Result := 4.406E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.879E+00) then
    begin
      Result := 5.478E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.710E+00) then
    begin
      Result := 6.946E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.559E+00) then
    begin
      Result := 8.331E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.420E+00) then
    begin
      Result := 1.001E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.292E+00) then
    begin
      Result := 1.180E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.173E+00) then
    begin
      Result := 1.335E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.062E+00) then
    begin
      Result := 1.513E-01;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.001E+00) then
    begin
      Result := 1.770E-01;
      Exit;
    end;
  Result := 0;
end;

(* ************************************************************************
  Tail(S, 8)
  ************************************************************************ *)
function SpearmanTail8(const s: TLFloat): TLFloat;
begin
  if AP_FP_Less(s, 2.001E+00) then
    begin
      Result := StudentTDistribution(6, -s);
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 1.103E+01) then
    begin
      Result := 2.194E-05;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 7.685E+00) then
    begin
      Result := 2.008E-04;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 6.143E+00) then
    begin
      Result := 5.686E-04;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 5.213E+00) then
    begin
      Result := 1.138E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 4.567E+00) then
    begin
      Result := 2.310E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 4.081E+00) then
    begin
      Result := 3.634E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.697E+00) then
    begin
      Result := 5.369E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.381E+00) then
    begin
      Result := 7.708E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.114E+00) then
    begin
      Result := 1.087E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.884E+00) then
    begin
      Result := 1.397E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.682E+00) then
    begin
      Result := 1.838E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.502E+00) then
    begin
      Result := 2.288E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.340E+00) then
    begin
      Result := 2.883E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.192E+00) then
    begin
      Result := 3.469E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.057E+00) then
    begin
      Result := 4.144E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.001E+00) then
    begin
      Result := 4.804E-02;
      Exit;
    end;
  Result := 0;
end;

(* ************************************************************************
  Tail(S, 9)
  ************************************************************************ *)
function SpearmanTail9(const s: TLFloat): TLFloat;
begin
  if AP_FP_Less(s, 2.001E+00) then
    begin
      Result := StudentTDistribution(7, -s);
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 9.989E+00) then
    begin
      Result := 2.306E-05;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 8.069E+00) then
    begin
      Result := 8.167E-05;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 6.890E+00) then
    begin
      Result := 1.744E-04;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 6.077E+00) then
    begin
      Result := 3.625E-04;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 5.469E+00) then
    begin
      Result := 6.450E-04;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 4.991E+00) then
    begin
      Result := 1.001E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 4.600E+00) then
    begin
      Result := 1.514E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 4.272E+00) then
    begin
      Result := 2.213E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.991E+00) then
    begin
      Result := 2.990E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.746E+00) then
    begin
      Result := 4.101E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.530E+00) then
    begin
      Result := 5.355E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.336E+00) then
    begin
      Result := 6.887E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.161E+00) then
    begin
      Result := 8.598E-03;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 3.002E+00) then
    begin
      Result := 1.065E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.855E+00) then
    begin
      Result := 1.268E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.720E+00) then
    begin
      Result := 1.552E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.595E+00) then
    begin
      Result := 1.836E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.477E+00) then
    begin
      Result := 2.158E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.368E+00) then
    begin
      Result := 2.512E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.264E+00) then
    begin
      Result := 2.942E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.166E+00) then
    begin
      Result := 3.325E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.073E+00) then
    begin
      Result := 3.800E-02;
      Exit;
    end;
  if AP_FP_Greater_Eq(s, 2.001E+00) then
    begin
      Result := 4.285E-02;
      Exit;
    end;
  Result := 0;
end;

(* ************************************************************************
  Tail(T,N), accepts T<0
  ************************************************************************ *)
function SpearmanTail(const T: TLFloat; const n: TLInt): TLFloat;
begin
  if n = 5 then
    begin
      Result := SpearmanTail5(-T);
      Exit;
    end;
  if n = 6 then
    begin
      Result := SpearmanTail6(-T);
      Exit;
    end;
  if n = 7 then
    begin
      Result := SpearmanTail7(-T);
      Exit;
    end;
  if n = 8 then
    begin
      Result := SpearmanTail8(-T);
      Exit;
    end;
  if n = 9 then
    begin
      Result := SpearmanTail9(-T);
      Exit;
    end;
  Result := StudentTDistribution(n - 2, T);
end; 
