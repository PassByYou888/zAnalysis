{ ****************************************************************************** }
{ * Descriptive statistics: mean, variance, etc                                * }
{ * Principal component analysis support, by QQ 600585@qq.com                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure PCABuildBasis(const X: TLMatrix; NPoints: TLInt; NVars: TLInt; var Info: TLInt; var s2: TLVec; var v: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalStatHeapSort(var Arr: TLVec; n: TLInt); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Calculation of the distribution moments: mean, variance, slewness, kurtosis.

  Input parameters:
  X       -   sample. Array with whose indexes range within [0..N-1]
  N       -   sample size.

  Output parameters:
  Mean    -   mean.
  Variance-   variance.
  Skewness-   skewness (if variance<>0; zero otherwise).
  Kurtosis-   kurtosis (if variance<>0; zero otherwise).
  ************************************************************************ *)
procedure CalculateMoments(const X: TLVec; n: TLInt; var Mean: TLFloat; var Variance: TLFloat; var Skewness: TLFloat; var Kurtosis: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i     : TLInt;
  v     : TLFloat;
  v1    : TLFloat;
  v2    : TLFloat;
  StdDev: TLFloat;
begin
  Mean := 0;
  Variance := 0;
  Skewness := 0;
  Kurtosis := 0;
  StdDev := 0;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Mean
  //
  i := 0;
  while i <= n - 1 do
    begin
      Mean := Mean + X[i];
      Inc(i);
    end;
  Mean := Mean / n;

  //
  // Variance (using corrected two-pass algorithm)
  //
  if n <> 1 then
    begin
      v1 := 0;
      i := 0;
      while i <= n - 1 do
        begin
          v1 := v1 + AP_Sqr(X[i] - Mean);
          Inc(i);
        end;
      v2 := 0;
      i := 0;
      while i <= n - 1 do
        begin
          v2 := v2 + (X[i] - Mean);
          Inc(i);
        end;
      v2 := AP_Sqr(v2) / n;
      Variance := (v1 - v2) / (n - 1);
      if AP_FP_Less(Variance, 0) then
        begin
          Variance := 0;
        end;
      StdDev := Sqrt(Variance);
    end;

  //
  // Skewness and kurtosis
  //
  if AP_FP_NEq(StdDev, 0) then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          v := (X[i] - Mean) / StdDev;
          v2 := AP_Sqr(v);
          Skewness := Skewness + v2 * v;
          Kurtosis := Kurtosis + AP_Sqr(v2);
          Inc(i);
        end;
      Skewness := Skewness / n;
      Kurtosis := Kurtosis / n - 3;
    end;
end;

(* ************************************************************************
  ADev

  Input parameters:
  X   -   sample (array indexes: [0..N-1])
  N   -   sample size

  Output parameters:
  ADev-   ADev
  ************************************************************************ *)
procedure CalculateADev(const X: TLVec; const n: TLInt; var ADev: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i   : TLInt;
  Mean: TLFloat;
begin
  Mean := 0;
  ADev := 0;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Mean
  //
  i := 0;
  while i <= n - 1 do
    begin
      Mean := Mean + X[i];
      Inc(i);
    end;
  Mean := Mean / n;

  //
  // ADev
  //
  i := 0;
  while i <= n - 1 do
    begin
      ADev := ADev + AbsReal(X[i] - Mean);
      Inc(i);
    end;
  ADev := ADev / n;
end;

(* ************************************************************************
  Median calculation.

  Input parameters:
  X   -   sample (array indexes: [0..N-1])
  N   -   sample size

  Output parameters:
  Median
  ************************************************************************ *)
procedure CalculateMedian(const X: TLVec; const n: TLInt; var Median: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i   : TLInt;
  IR  : TLInt;
  J   : TLInt;
  L   : TLInt;
  midp: TLInt;
  k   : TLInt;
  A   : TLFloat;
  tval: TLFloat;
begin
  //
  // Some degenerate cases
  //
  Median := 0;
  if n <= 0 then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      Median := X[0];
      Exit;
    end;
  if n = 2 then
    begin
      Median := 0.5 * (X[0] + X[1]);
      Exit;
    end;

  //
  // Common case, N>=3.
  // Choose X[(N-1)/2]
  //
  L := 0;
  IR := n - 1;
  k := (n - 1) div 2;
  while True do
    begin
      if IR <= L + 1 then
        begin

          //
          // 1 or 2 elements in partition
          //
          if (IR = L + 1) and AP_FP_Less(X[IR], X[L]) then
            begin
              tval := X[L];
              X[L] := X[IR];
              X[IR] := tval;
            end;
          Break;
        end
      else
        begin
          midp := (L + IR) div 2;
          tval := X[midp];
          X[midp] := X[L + 1];
          X[L + 1] := tval;
          if AP_FP_Greater(X[L], X[IR]) then
            begin
              tval := X[L];
              X[L] := X[IR];
              X[IR] := tval;
            end;
          if AP_FP_Greater(X[L + 1], X[IR]) then
            begin
              tval := X[L + 1];
              X[L + 1] := X[IR];
              X[IR] := tval;
            end;
          if AP_FP_Greater(X[L], X[L + 1]) then
            begin
              tval := X[L];
              X[L] := X[L + 1];
              X[L + 1] := tval;
            end;
          i := L + 1;
          J := IR;
          A := X[L + 1];
          while True do
            begin
              repeat
                  i := i + 1;
              until AP_FP_Greater_Eq(X[i], A);
              repeat
                  J := J - 1;
              until AP_FP_Less_Eq(X[J], A);
              if J < i then
                begin
                  Break;
                end;
              tval := X[i];
              X[i] := X[J];
              X[J] := tval;
            end;
          X[L + 1] := X[J];
          X[J] := A;
          if J >= k then
            begin
              IR := J - 1;
            end;
          if J <= k then
            begin
              L := i;
            end;
        end;
    end;

  //
  // If N is odd, return result
  //
  if n mod 2 = 1 then
    begin
      Median := X[k];
      Exit;
    end;
  A := X[n - 1];
  i := k + 1;
  while i <= n - 1 do
    begin
      if AP_FP_Less(X[i], A) then
        begin
          A := X[i];
        end;
      Inc(i);
    end;
  Median := 0.5 * (X[k] + A);
end;

(* ************************************************************************
  Percentile calculation.

  Input parameters:
  X   -   sample (array indexes: [0..N-1])
  N   -   sample size, N>1
  P   -   percentile (0<=P<=1)

  Output parameters:
  V   -   percentile
  ************************************************************************ *)
procedure CalculatePercentile(const X: TLVec; const n: TLInt; const p: TLFloat; var v: TLFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  Nx: TLVec;
  i1: TLInt;
  T : TLFloat;
begin
  Nx := DynamicArrayCopy(X);
  Assert(n > 1, 'CalculatePercentile: N<=1!');
  Assert(AP_FP_Greater_Eq(p, 0) and AP_FP_Less_Eq(p, 1),
    'CalculatePercentile: incorrect P!');
  InternalStatHeapSort(Nx, n);
  if AP_FP_Eq(p, 0) then
    begin
      v := Nx[0];
      Exit;
    end;
  if AP_FP_Eq(p, 1) then
    begin
      v := Nx[n - 1];
      Exit;
    end;
  T := p * (n - 1);
  i1 := Floor(T);
  T := T - Floor(T);
  v := Nx[i1] * (1 - T) + Nx[i1 + 1] * T;
end;

procedure InternalStatHeapSort(var Arr: TLVec; n: TLInt);
var
  i  : TLInt;
  k  : TLInt;
  T  : TLInt;
  tmp: TLFloat;
begin
  if n = 1 then
    begin
      Exit;
    end;
  i := 2;
  repeat
    T := i;
    while T <> 1 do
      begin
        k := T div 2;
        if AP_FP_Greater_Eq(Arr[k - 1], Arr[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            tmp := Arr[k - 1];
            Arr[k - 1] := Arr[T - 1];
            Arr[T - 1] := tmp;
            T := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := Arr[i];
    Arr[i] := Arr[0];
    Arr[0] := tmp;
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
                if AP_FP_Greater(Arr[k], Arr[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(Arr[T - 1], Arr[k - 1]) then
              begin
                T := 0;
              end
            else
              begin
                tmp := Arr[k - 1];
                Arr[k - 1] := Arr[T - 1];
                Arr[T - 1] := tmp;
                T := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

(* ************************************************************************
  Principal components analysis

  Subroutine  builds  orthogonal  basis  where  first  axis  corresponds  to
  direction with maximum variance, second axis maximizes variance in subspace
  orthogonal to first axis and so on.

  It should be noted that, unlike LDA, PCA does not use class labels.

  INPUT PARAMETERS:
  X           -   dataset, array[0..NPoints-1,0..NVars-1]. matrix contains ONLY INDEPENDENT VARIABLES.
  NPoints     -   dataset size, NPoints>=0
  NVars       -   number of independent variables, NVars>=1

  Info        -   return code:
  * -4, if SVD subroutine haven't converged
  * -1, if wrong parameters has been passed (NPoints<0, NVars<1)
  *  1, if task is solved
  S2          -   array[0..NVars-1]. variance values corresponding to basis vectors.
  V           -   array[0..NVars-1,0..NVars-1] matrix, whose columns store basis vectors.
  ************************************************************************ *)
procedure PCABuildBasis(const X: TLMatrix; NPoints: TLInt; NVars: TLInt; var Info: TLInt; var s2: TLVec; var v: TLMatrix);
var
  A       : TLMatrix;
  u       : TLMatrix;
  VT      : TLMatrix;
  M       : TLVec;
  T       : TLVec;
  i       : TLInt;
  J       : TLInt;
  Mean    : TLFloat;
  Variance: TLFloat;
  Skewness: TLFloat;
  Kurtosis: TLFloat;
  i_      : TLInt;
begin

  //
  // Check input data
  //
  if (NPoints < 0) or (NVars < 1) then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // Special case: NPoints=0
  //
  if NPoints = 0 then
    begin
      SetLength(s2, NVars (* - 1 + 1 // optimized compiler *) );
      SetLength(v, NVars (* - 1 + 1 // optimized compiler *) , NVars (* - 1 + 1 // optimized compiler *) );
      i := 0;
      while i <= NVars - 1 do
        begin
          s2[i] := 0;
          Inc(i);
        end;
      i := 0;
      while i <= NVars - 1 do
        begin
          J := 0;
          while J <= NVars - 1 do
            begin
              if i = J then
                begin
                  v[i, J] := 1;
                end
              else
                begin
                  v[i, J] := 0;
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Exit;
    end;

  //
  // Calculate means
  //
  SetLength(M, NVars (* - 1 + 1 // optimized compiler *) );
  SetLength(T, NPoints (* - 1 + 1 // optimized compiler *) );
  J := 0;
  while J <= NVars - 1 do
    begin
      for i_ := 0 to NPoints - 1 do
        begin
          T[i_] := X[i_, J];
        end;
      CalculateMoments(T, NPoints, Mean, Variance, Skewness, Kurtosis);
      M[J] := Mean;
      Inc(J);
    end;

  //
  // Center, apply SVD, prepare output
  //
  SetLength(A, Max(NPoints, NVars) (* - 1 + 1 // optimized compiler *) , NVars (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@A[i][0], 0, NVars - 1, @X[i][0], 0, NVars - 1);
      APVSub(@A[i][0], 0, NVars - 1, @M[0], 0, NVars - 1);
      Inc(i);
    end;
  i := NPoints;
  while i <= NVars - 1 do
    begin
      J := 0;
      while J <= NVars - 1 do
        begin
          A[i, J] := 0;
          Inc(J);
        end;
      Inc(i);
    end;
  if not RMatrixSVD(A, Max(NPoints, NVars), NVars, 0, 1, 2, s2, u, VT) then
    begin
      Info := -4;
      Exit;
    end;
  if NPoints <> 1 then
    begin
      i := 0;
      while i <= NVars - 1 do
        begin
          s2[i] := AP_Sqr(s2[i]) / (NPoints - 1);
          Inc(i);
        end;
    end;
  SetLength(v, NVars (* - 1 + 1 // optimized compiler *) , NVars (* - 1 + 1 // optimized compiler *) );
  CopyAndTranspose(VT, 0, NVars - 1, 0, NVars - 1, v, 0, NVars - 1, 0, NVars - 1);
end; 
