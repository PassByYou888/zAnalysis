{ ****************************************************************************** }
{ * Descriptive statistics: mean, variance, etc                                * }
{ * Principal component analysis support, by QQ 600585@qq.com                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

procedure PCABuildBasis(const X: TLearnFloat2DArray; NPoints: TLearnInteger; NVars: TLearnInteger; var Info: TLearnInteger; var S2: TLearnFloatArray; var V: TLearnFloat2DArray); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalStatHeapSort(var Arr: TLearnFloatArray; N: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


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
procedure CalculateMoments(const X: TLearnFloatArray; N: TLearnInteger;
  var Mean: TLearnFloat; var Variance: TLearnFloat; var Skewness: TLearnFloat; var Kurtosis: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  I     : TLearnInteger;
  V     : TLearnFloat;
  V1    : TLearnFloat;
  V2    : TLearnFloat;
  StdDev: TLearnFloat;
begin
  Mean := 0;
  Variance := 0;
  Skewness := 0;
  Kurtosis := 0;
  StdDev := 0;
  if N <= 0 then
    begin
      Exit;
    end;

  //
  // Mean
  //
  I := 0;
  while I <= N - 1 do
    begin
      Mean := Mean + X[I];
      Inc(I);
    end;
  Mean := Mean / N;

  //
  // Variance (using corrected two-pass algorithm)
  //
  if N <> 1 then
    begin
      V1 := 0;
      I := 0;
      while I <= N - 1 do
        begin
          V1 := V1 + AP_Sqr(X[I] - Mean);
          Inc(I);
        end;
      V2 := 0;
      I := 0;
      while I <= N - 1 do
        begin
          V2 := V2 + (X[I] - Mean);
          Inc(I);
        end;
      V2 := AP_Sqr(V2) / N;
      Variance := (V1 - V2) / (N - 1);
      if AP_FP_Less(Variance, 0) then
        begin
          Variance := 0;
        end;
      StdDev := Sqrt(Variance);
    end;

  //
  // Skewness and kurtosis
  //
  if AP_FP_Neq(StdDev, 0) then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          V := (X[I] - Mean) / StdDev;
          V2 := AP_Sqr(V);
          Skewness := Skewness + V2 * V;
          Kurtosis := Kurtosis + AP_Sqr(V2);
          Inc(I);
        end;
      Skewness := Skewness / N;
      Kurtosis := Kurtosis / N - 3;
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
procedure CalculateADev(const X: TLearnFloatArray; const N: TLearnInteger; var ADev: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  I   : TLearnInteger;
  Mean: TLearnFloat;
begin
  Mean := 0;
  ADev := 0;
  if N <= 0 then
    begin
      Exit;
    end;

  //
  // Mean
  //
  I := 0;
  while I <= N - 1 do
    begin
      Mean := Mean + X[I];
      Inc(I);
    end;
  Mean := Mean / N;

  //
  // ADev
  //
  I := 0;
  while I <= N - 1 do
    begin
      ADev := ADev + AbsReal(X[I] - Mean);
      Inc(I);
    end;
  ADev := ADev / N;
end;

(* ************************************************************************
  Median calculation.

  Input parameters:
  X   -   sample (array indexes: [0..N-1])
  N   -   sample size

  Output parameters:
  Median
  ************************************************************************ *)
procedure CalculateMedian(const X: TLearnFloatArray; const N: TLearnInteger; var Median: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  I   : TLearnInteger;
  ir  : TLearnInteger;
  j   : TLearnInteger;
  l   : TLearnInteger;
  midp: TLearnInteger;
  K   : TLearnInteger;
  a   : TLearnFloat;
  tval: TLearnFloat;
begin
  //
  // Some degenerate cases
  //
  Median := 0;
  if N <= 0 then
    begin
      Exit;
    end;
  if N = 1 then
    begin
      Median := X[0];
      Exit;
    end;
  if N = 2 then
    begin
      Median := 0.5 * (X[0] + X[1]);
      Exit;
    end;

  //
  // Common case, N>=3.
  // Choose X[(N-1)/2]
  //
  l := 0;
  ir := N - 1;
  K := (N - 1) div 2;
  while True do
    begin
      if ir <= l + 1 then
        begin

          //
          // 1 or 2 elements in partition
          //
          if (ir = l + 1) and AP_FP_Less(X[ir], X[l]) then
            begin
              tval := X[l];
              X[l] := X[ir];
              X[ir] := tval;
            end;
          Break;
        end
      else
        begin
          midp := (l + ir) div 2;
          tval := X[midp];
          X[midp] := X[l + 1];
          X[l + 1] := tval;
          if AP_FP_Greater(X[l], X[ir]) then
            begin
              tval := X[l];
              X[l] := X[ir];
              X[ir] := tval;
            end;
          if AP_FP_Greater(X[l + 1], X[ir]) then
            begin
              tval := X[l + 1];
              X[l + 1] := X[ir];
              X[ir] := tval;
            end;
          if AP_FP_Greater(X[l], X[l + 1]) then
            begin
              tval := X[l];
              X[l] := X[l + 1];
              X[l + 1] := tval;
            end;
          I := l + 1;
          j := ir;
          a := X[l + 1];
          while True do
            begin
              repeat
                  I := I + 1;
              until AP_FP_Greater_Eq(X[I], a);
              repeat
                  j := j - 1;
              until AP_FP_Less_Eq(X[j], a);
              if j < I then
                begin
                  Break;
                end;
              tval := X[I];
              X[I] := X[j];
              X[j] := tval;
            end;
          X[l + 1] := X[j];
          X[j] := a;
          if j >= K then
            begin
              ir := j - 1;
            end;
          if j <= K then
            begin
              l := I;
            end;
        end;
    end;

  //
  // If N is odd, return result
  //
  if N mod 2 = 1 then
    begin
      Median := X[K];
      Exit;
    end;
  a := X[N - 1];
  I := K + 1;
  while I <= N - 1 do
    begin
      if AP_FP_Less(X[I], a) then
        begin
          a := X[I];
        end;
      Inc(I);
    end;
  Median := 0.5 * (X[K] + a);
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
procedure CalculatePercentile(const X: TLearnFloatArray; const N: TLearnInteger; const P: TLearnFloat; var V: TLearnFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  NX: TLearnFloatArray;
  I1: TLearnInteger;
  T : TLearnFloat;
begin
  NX := DynamicArrayCopy(X);
  Assert(N > 1, 'CalculatePercentile: N<=1!');
  Assert(AP_FP_Greater_Eq(P, 0) and AP_FP_Less_Eq(P, 1),
    'CalculatePercentile: incorrect P!');
  InternalStatHeapSort(NX, N);
  if AP_FP_Eq(P, 0) then
    begin
      V := NX[0];
      Exit;
    end;
  if AP_FP_Eq(P, 1) then
    begin
      V := NX[N - 1];
      Exit;
    end;
  T := P * (N - 1);
  I1 := Floor(T);
  T := T - Floor(T);
  V := NX[I1] * (1 - T) + NX[I1 + 1] * T;
end;

procedure InternalStatHeapSort(var Arr: TLearnFloatArray; N: TLearnInteger);
var
  I  : TLearnInteger;
  K  : TLearnInteger;
  T  : TLearnInteger;
  Tmp: TLearnFloat;
begin
  if N = 1 then
    begin
      Exit;
    end;
  I := 2;
  repeat
    T := I;
    while T <> 1 do
      begin
        K := T div 2;
        if AP_FP_Greater_Eq(Arr[K - 1], Arr[T - 1]) then
          begin
            T := 1;
          end
        else
          begin
            Tmp := Arr[K - 1];
            Arr[K - 1] := Arr[T - 1];
            Arr[T - 1] := Tmp;
            T := K;
          end;
      end;
    I := I + 1;
  until not(I <= N);
  I := N - 1;
  repeat
    Tmp := Arr[I];
    Arr[I] := Arr[0];
    Arr[0] := Tmp;
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
                if AP_FP_Greater(Arr[K], Arr[K - 1]) then
                  begin
                    K := K + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(Arr[T - 1], Arr[K - 1]) then
              begin
                T := 0;
              end
            else
              begin
                Tmp := Arr[K - 1];
                Arr[K - 1] := Arr[T - 1];
                Arr[T - 1] := Tmp;
                T := K;
              end;
          end;
      end;
    I := I - 1;
  until not(I >= 1);
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
procedure PCABuildBasis(const X: TLearnFloat2DArray; NPoints: TLearnInteger; NVars: TLearnInteger; var Info: TLearnInteger; var S2: TLearnFloatArray; var V: TLearnFloat2DArray);
var
  a       : TLearnFloat2DArray;
  U       : TLearnFloat2DArray;
  VT      : TLearnFloat2DArray;
  M       : TLearnFloatArray;
  T       : TLearnFloatArray;
  I       : TLearnInteger;
  j       : TLearnInteger;
  Mean    : TLearnFloat;
  Variance: TLearnFloat;
  Skewness: TLearnFloat;
  Kurtosis: TLearnFloat;
  i_      : TLearnInteger;
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
      SetLength(S2, NVars (* - 1 + 1 // optimized compiler *) );
      SetLength(V, NVars (* - 1 + 1 // optimized compiler *) , NVars (* - 1 + 1 // optimized compiler *) );
      I := 0;
      while I <= NVars - 1 do
        begin
          S2[I] := 0;
          Inc(I);
        end;
      I := 0;
      while I <= NVars - 1 do
        begin
          j := 0;
          while j <= NVars - 1 do
            begin
              if I = j then
                begin
                  V[I, j] := 1;
                end
              else
                begin
                  V[I, j] := 0;
                end;
              Inc(j);
            end;
          Inc(I);
        end;
      Exit;
    end;

  //
  // Calculate means
  //
  SetLength(M, NVars (* - 1 + 1 // optimized compiler *) );
  SetLength(T, NPoints (* - 1 + 1 // optimized compiler *) );
  j := 0;
  while j <= NVars - 1 do
    begin
      for i_ := 0 to NPoints - 1 do
        begin
          T[i_] := X[i_, j];
        end;
      CalculateMoments(T, NPoints, Mean, Variance, Skewness, Kurtosis);
      M[j] := Mean;
      Inc(j);
    end;

  //
  // Center, apply SVD, prepare output
  //
  SetLength(a, Max(NPoints, NVars) (* - 1 + 1 // optimized compiler *) , NVars (* - 1 + 1 // optimized compiler *) );
  I := 0;
  while I <= NPoints - 1 do
    begin
      APVMove(@a[I][0], 0, NVars - 1, @X[I][0], 0, NVars - 1);
      APVSub(@a[I][0], 0, NVars - 1, @M[0], 0, NVars - 1);
      Inc(I);
    end;
  I := NPoints;
  while I <= NVars - 1 do
    begin
      j := 0;
      while j <= NVars - 1 do
        begin
          a[I, j] := 0;
          Inc(j);
        end;
      Inc(I);
    end;
  if not RMatrixSVD(a, Max(NPoints, NVars), NVars, 0, 1, 2, S2, U, VT) then
    begin
      Info := -4;
      Exit;
    end;
  if NPoints <> 1 then
    begin
      I := 0;
      while I <= NVars - 1 do
        begin
          S2[I] := AP_Sqr(S2[I]) / (NPoints - 1);
          Inc(I);
        end;
    end;
  SetLength(V, NVars (* - 1 + 1 // optimized compiler *) , NVars (* - 1 + 1 // optimized compiler *) );
  CopyAndTranspose(VT, 0, NVars - 1, 0, NVars - 1, V, 0, NVars - 1, 0, NVars - 1);
end;
