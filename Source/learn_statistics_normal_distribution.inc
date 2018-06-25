{ ****************************************************************************** }
{ * Normal distribution support, by QQ 600585@qq.com                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

function Erf(const X: TLFloat): TLFloat; forward;
function ErfC(const X: TLFloat): TLFloat; forward;
function InvErf(const E: TLFloat): TLFloat; forward;

(* ************************************************************************
  * Error function
  *
  * The integral is
  *
  *                           x
  *                            -
  *                 2         | |          2
  *   erf(x)  =  --------     |    exp( - t  ) dt.
  *              sqrt(pi)   | |
  *                          -
  *                           0
  *
  * For 0 <= |x| < 1, erf(x) = x * P4(x**2)/Q5(x**2); otherwise
  * erf(x) = 1 - erfc(x).
  *
  *
  * ACCURACY:
  *
  *                      Relative error:
  * arithmetic   domain     # trials      peak         rms
  *    IEEE      0,1         30000       3.7e-16     1.0e-16
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function Erf(const X: TLFloat): TLFloat;
var
  NX: TLFloat;
  XSq: TLFloat;
  S: TLFloat;
  P: TLFloat;
  Q: TLFloat;
begin
  NX := AbsReal(X);
  S := Sign(NX);
  if AP_FP_Less(NX, 0.5) then
    begin
      XSq := NX * NX;
      P := 0.007547728033418631287834;
      P := 0.288805137207594084924010 + XSq * P;
      P := 14.3383842191748205576712 + XSq * P;
      P := 38.0140318123903008244444 + XSq * P;
      P := 3017.82788536507577809226 + XSq * P;
      P := 7404.07142710151470082064 + XSq * P;
      P := 80437.3630960840172832162 + XSq * P;
      Q := 0.0;
      Q := 1.00000000000000000000000 + XSq * Q;
      Q := 38.0190713951939403753468 + XSq * Q;
      Q := 658.070155459240506326937 + XSq * Q;
      Q := 6379.60017324428279487120 + XSq * Q;
      Q := 34216.5257924628539769006 + XSq * Q;
      Q := 80437.3630960840172826266 + XSq * Q;
      Result := S * 1.1283791670955125738961589031 * NX * P / Q;
      Exit;
    end;
  if AP_FP_Greater_Eq(NX, 10) then
    begin
      Result := S;
      Exit;
    end;
  Result := S * (1 - ErfC(NX));
end;

(* ************************************************************************
  * Complementary error function
  *
  *  1 - erf(x) =
  *
  *                           inf.
  *                             -
  *                  2         | |          2
  *   erfc(x)  =  --------     |    exp( - t  ) dt
  *               sqrt(pi)   | |
  *                           -
  *                            x
  *
  *
  * For small x, erfc(x) = 1 - erf(x); otherwise rational
  * approximations are computed.
  *
  *
  * ACCURACY:
  *
  *                      Relative error:
  * arithmetic   domain     # trials      peak         rms
  *    IEEE      0,26.6417   30000       5.7e-14     1.5e-14
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function ErfC(const X: TLFloat): TLFloat;
var
  P: TLFloat;
  Q: TLFloat;
begin
  if AP_FP_Less(X, 0) then
    begin
      Result := 2 - ErfC(-X);
      Exit;
    end;
  if AP_FP_Less(X, 0.5) then
    begin
      Result := 1.0 - Erf(X);
      Exit;
    end;
  if AP_FP_Greater_Eq(X, 10) then
    begin
      Result := 0;
      Exit;
    end;
  P := 0.0;
  P := 0.5641877825507397413087057563 + X * P;
  P := 9.675807882987265400604202961 + X * P;
  P := 77.08161730368428609781633646 + X * P;
  P := 368.5196154710010637133875746 + X * P;
  P := 1143.262070703886173606073338 + X * P;
  P := 2320.439590251635247384768711 + X * P;
  P := 2898.0293292167655611275846 + X * P;
  P := 1826.3348842295112592168999 + X * P;
  Q := 1.0;
  Q := 17.14980943627607849376131193 + X * Q;
  Q := 137.1255960500622202878443578 + X * Q;
  Q := 661.7361207107653469211984771 + X * Q;
  Q := 2094.384367789539593790281779 + X * Q;
  Q := 4429.612803883682726711528526 + X * Q;
  Q := 6089.5424232724435504633068 + X * Q;
  Q := 4958.82756472114071495438422 + X * Q;
  Q := 1826.3348842295112595576438 + X * Q;
  Result := Exp(-AP_Sqr(X)) * P / Q;
end;

(* ************************************************************************
  * Normal distribution function
  *
  * Returns the area under the Gaussian probability density
  * function, integrated from minus infinity to x:
  *
  *                            x
  *                             -
  *                   1        | |          2
  *    ndtr(x)  = ---------    |    exp( - t /2 ) dt
  *               sqrt(2pi)  | |
  *                           -
  *                          -inf.
  *
  *             =  ( 1 + erf(z) ) / 2
  *             =  erfc(z) / 2
  *
  * where z = x/sqrt(2). Computation is via the functions
  * erf and erfc.
  *
  *
  * ACCURACY:
  *
  *                      Relative error:
  * arithmetic   domain     # trials      peak         rms
  *    IEEE     -13,0        30000       3.4e-14     6.7e-15
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function NormalDistribution(const X: TLFloat): TLFloat;
begin
  Result := 0.5 * (Erf(X / 1.41421356237309504880) + 1);
end;

(* ************************************************************************
  Inverse of the error function

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvErf(const E: TLFloat): TLFloat;
begin
  Result := InvNormalDistribution(0.5 * (E + 1)) / Sqrt(2);
end;

(* ************************************************************************
  * Inverse of Normal distribution function
  *
  * Returns the argument, x, for which the area under the
  * Gaussian probability density function (integrated from
  * minus infinity to x) is equal to y.
  *
  *
  * For small arguments 0 < y < exp(-2), the program computes
  * z = sqrt( -2.0 * log(y) );  then the approximation is
  * x = z - log(z)/z  - (1/z) P(1/z) / Q(1/z).
  * There are two rational functions P/Q, one for 0 < y < exp(-32)
  * and the other for y up to exp(-2).  For larger arguments,
  * w = y - 0.5, and  x/sqrt(2pi) = w + w**3 R(w**2)/S(w**2)).
  *
  * ACCURACY:
  *
  * Relative error:
  * arithmetic   domain        # trials      peak         rms
  * IEEE     0.125, 1        20000       7.2e-16     1.3e-16
  * IEEE     3e-308, 0.135   50000       4.6e-16     9.8e-17
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvNormalDistribution(const y0: TLFloat): TLFloat;
var
  Expm2: TLFloat;
  S2Pi: TLFloat;
  X: TLFloat;
  y: TLFloat;
  z: TLFloat;
  y2: TLFloat;
  x0: TLFloat;
  x1: TLFloat;
  code: TLInt;
  P0: TLFloat;
  Q0: TLFloat;
  P1: TLFloat;
  Q1: TLFloat;
  P2: TLFloat;
  Q2: TLFloat;
begin
  Expm2 := 0.13533528323661269189;
  S2Pi := 2.50662827463100050242;
  if AP_FP_Less_Eq(y0, 0) then
    begin
      Result := -MaxRealNumber;
      Exit;
    end;
  if AP_FP_Greater_Eq(y0, 1) then
    begin
      Result := MaxRealNumber;
      Exit;
    end;
  code := 1;
  y := y0;
  if AP_FP_Greater(y, 1.0 - Expm2) then
    begin
      y := 1.0 - y;
      code := 0;
    end;
  if AP_FP_Greater(y, Expm2) then
    begin
      y := y - 0.5;
      y2 := y * y;
      P0 := -59.9633501014107895267;
      P0 := 98.0010754185999661536 + y2 * P0;
      P0 := -56.6762857469070293439 + y2 * P0;
      P0 := 13.9312609387279679503 + y2 * P0;
      P0 := -1.23916583867381258016 + y2 * P0;
      Q0 := 1;
      Q0 := 1.95448858338141759834 + y2 * Q0;
      Q0 := 4.67627912898881538453 + y2 * Q0;
      Q0 := 86.3602421390890590575 + y2 * Q0;
      Q0 := -225.462687854119370527 + y2 * Q0;
      Q0 := 200.260212380060660359 + y2 * Q0;
      Q0 := -82.0372256168333339912 + y2 * Q0;
      Q0 := 15.9056225126211695515 + y2 * Q0;
      Q0 := -1.18331621121330003142 + y2 * Q0;
      X := y + y * y2 * P0 / Q0;
      X := X * S2Pi;
      Result := X;
      Exit;
    end;
  X := Sqrt(-2.0 * Ln(y));
  x0 := X - Ln(X) / X;
  z := 1.0 / X;
  if AP_FP_Less(X, 8.0) then
    begin
      P1 := 4.05544892305962419923;
      P1 := 31.5251094599893866154 + z * P1;
      P1 := 57.1628192246421288162 + z * P1;
      P1 := 44.0805073893200834700 + z * P1;
      P1 := 14.6849561928858024014 + z * P1;
      P1 := 2.18663306850790267539 + z * P1;
      P1 := -1.40256079171354495875 * 0.1 + z * P1;
      P1 := -3.50424626827848203418 * 0.01 + z * P1;
      P1 := -8.57456785154685413611 * 0.0001 + z * P1;
      Q1 := 1;
      Q1 := 15.7799883256466749731 + z * Q1;
      Q1 := 45.3907635128879210584 + z * Q1;
      Q1 := 41.3172038254672030440 + z * Q1;
      Q1 := 15.0425385692907503408 + z * Q1;
      Q1 := 2.50464946208309415979 + z * Q1;
      Q1 := -1.42182922854787788574 * 0.1 + z * Q1;
      Q1 := -3.80806407691578277194 * 0.01 + z * Q1;
      Q1 := -9.33259480895457427372 * 0.0001 + z * Q1;
      x1 := z * P1 / Q1;
    end
  else
    begin
      P2 := 3.23774891776946035970;
      P2 := 6.91522889068984211695 + z * P2;
      P2 := 3.93881025292474443415 + z * P2;
      P2 := 1.33303460815807542389 + z * P2;
      P2 := 2.01485389549179081538 * 0.1 + z * P2;
      P2 := 1.23716634817820021358 * 0.01 + z * P2;
      P2 := 3.01581553508235416007 * 0.0001 + z * P2;
      P2 := 2.65806974686737550832 * 0.000001 + z * P2;
      P2 := 6.23974539184983293730 * 0.000000001 + z * P2;
      Q2 := 1;
      Q2 := 6.02427039364742014255 + z * Q2;
      Q2 := 3.67983563856160859403 + z * Q2;
      Q2 := 1.37702099489081330271 + z * Q2;
      Q2 := 2.16236993594496635890 * 0.1 + z * Q2;
      Q2 := 1.34204006088543189037 * 0.01 + z * Q2;
      Q2 := 3.28014464682127739104 * 0.0001 + z * Q2;
      Q2 := 2.89247864745380683936 * 0.000001 + z * Q2;
      Q2 := 6.79019408009981274425 * 0.000000001 + z * Q2;
      x1 := z * P2 / Q2;
    end;
  X := x0 - x1;
  if code <> 0 then
    begin
      X := -X;
    end;
  Result := X;
end;
