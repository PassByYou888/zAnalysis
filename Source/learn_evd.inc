{ ****************************************************************************** }
{ * Eigensolvers support, by QQ 600585@qq.com                                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

function SMatrixEVD(A: TLearnFloat2DArray; N: TLearnInteger; ZNeeded: TLearnInteger; IsUpper: Boolean;
  var D: TLearnFloatArray; var Z: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SMatrixEVDR(A: TLearnFloat2DArray; N: TLearnInteger; ZNeeded: TLearnInteger;
  IsUpper: Boolean; B1: TLearnFloat; B2: TLearnFloat; var M: TLearnInteger;
  var W: TLearnFloatArray; var Z: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SMatrixEVDI(A: TLearnFloat2DArray; N: TLearnInteger; ZNeeded: TLearnInteger;
  IsUpper: Boolean; I1: TLearnInteger; I2: TLearnInteger;
  var W: TLearnFloatArray; var Z: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function HMatrixEVD(A: TLearnComplex2DArray; N: TLearnInteger; ZNeeded: TLearnInteger; IsUpper: Boolean;
  var D: TLearnFloatArray; var Z: TLearnComplex2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function HMatrixEVDR(A: TLearnComplex2DArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; IsUpper: Boolean; B1: TLearnFloat; B2: TLearnFloat;
  var M: TLearnInteger; var W: TLearnFloatArray; var Z: TLearnComplex2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function HMatrixEVDI(A: TLearnComplex2DArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; IsUpper: Boolean; I1: TLearnInteger;
  I2: TLearnInteger; var W: TLearnFloatArray; var Z: TLearnComplex2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SMatrixTDEVD(var D: TLearnFloatArray; E: TLearnFloatArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; var Z: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SMatrixTDEVDR(var D: TLearnFloatArray; const E: TLearnFloatArray;
  N: TLearnInteger; ZNeeded: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  var M: TLearnInteger; var Z: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SMatrixTDEVDI(var D: TLearnFloatArray; const E: TLearnFloatArray;
  N: TLearnInteger; ZNeeded: TLearnInteger; I1: TLearnInteger;
  I2: TLearnInteger; var Z: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RMatrixEVD(A: TLearnFloat2DArray; N: TLearnInteger; VNeeded: TLearnInteger;
  var WR: TLearnFloatArray; var WI: TLearnFloatArray; var VL: TLearnFloat2DArray;
  var VR: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function InternalBisectionEigenValues(D: TLearnFloatArray; E: TLearnFloatArray;
  N: TLearnInteger; IRANGE: TLearnInteger; IORDER: TLearnInteger;
  VL: TLearnFloat; VU: TLearnFloat; IL: TLearnInteger; IU: TLearnInteger;
  ABSTOL: TLearnFloat; var W: TLearnFloatArray; var M: TLearnInteger;
  var NSPLIT: TLearnInteger; var IBLOCK: TLearnIntegerArray;
  var ISPLIT: TLearnIntegerArray; var ErrorCode: TLearnInteger): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalDSTEIN(const N: TLearnInteger; const D: TLearnFloatArray;
  E: TLearnFloatArray; const M: TLearnInteger; W: TLearnFloatArray;
  const IBLOCK: TLearnIntegerArray; const ISPLIT: TLearnIntegerArray;
  var Z: TLearnFloat2DArray; var IFAIL: TLearnIntegerArray; var INFO: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function TridiagonalEVD(var D: TLearnFloatArray; E: TLearnFloatArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; var Z: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure TdEVDE2(const A: TLearnFloat; const B: TLearnFloat;
  const C: TLearnFloat; var RT1: TLearnFloat; var RT2: TLearnFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure TdEVDEV2(const A: TLearnFloat; const B: TLearnFloat;
  const C: TLearnFloat; var RT1: TLearnFloat; var RT2: TLearnFloat;
  var CS1: TLearnFloat; var SN1: TLearnFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function TdEVDPythag(A: TLearnFloat; B: TLearnFloat): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TdEVDExtSign(A: TLearnFloat; B: TLearnFloat): TLearnFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure TDINInternalDLAGTF(const N: TLearnInteger; var A: TLearnFloatArray;
  const LAMBDA: TLearnFloat; var B: TLearnFloatArray; var C: TLearnFloatArray;
  const TOL: TLearnFloat; var D: TLearnFloatArray; var IIN: TLearnIntegerArray;
  var INFO: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure TDINInternalDLAGTS(const N: TLearnInteger; const A: TLearnFloatArray;
  const B: TLearnFloatArray; const C: TLearnFloatArray; const D: TLearnFloatArray;
  const IIN: TLearnIntegerArray; var Y: TLearnFloatArray; var TOL: TLearnFloat;
  var INFO: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalDLAEBZ(const IJOB: TLearnInteger; const NITMAX: TLearnInteger;
  const N: TLearnInteger; const MMAX: TLearnInteger; const MINP: TLearnInteger;
  const ABSTOL: TLearnFloat; const RELTOL: TLearnFloat;
  const PIVMIN: TLearnFloat; const D: TLearnFloatArray; const E: TLearnFloatArray;
  const E2: TLearnFloatArray; var NVAL: TLearnIntegerArray; var AB: TLearnFloat2DArray;
  var C: TLearnFloatArray; var MOUT: TLearnInteger; var NAB: TLearnInteger2DArray;
  var WORK: TLearnFloatArray; var IWORK: TLearnIntegerArray;
  var INFO: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalTREVC(const T: TLearnFloat2DArray; N: TLearnInteger;
  SIDE: TLearnInteger; HOWMNY: TLearnInteger; VSELECT: TLearnBooleanArray;
  var VL: TLearnFloat2DArray; var VR: TLearnFloat2DArray; var M: TLearnInteger;
  var INFO: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalHSEVDLALN2(const LTRANS: Boolean; const NA: TLearnInteger;
  const NW: TLearnInteger; const SMIN: TLearnFloat; const CA: TLearnFloat;
  const A: TLearnFloat2DArray; const D1: TLearnFloat; const D2: TLearnFloat;
  const B: TLearnFloat2DArray; const WR: TLearnFloat; const WI: TLearnFloat;
  var RSWAP4: TLearnBooleanArray; var ZSWAP4: TLearnBooleanArray;
  var IPIVOT44: TLearnInteger2DArray; var CIV4: TLearnFloatArray; var CRV4: TLearnFloatArray;
  var X: TLearnFloat2DArray; var SCL: TLearnFloat; var XNORM: TLearnFloat;
  var INFO: TLearnInteger); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure InternalHSEVDLADIV(const A: TLearnFloat; const B: TLearnFloat;
  const C: TLearnFloat; const D: TLearnFloat; var P: TLearnFloat;
  var Q: TLearnFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function NonSymmetricEVD(A: TLearnFloat2DArray; N: TLearnInteger;
  VNeeded: TLearnInteger; var WR: TLearnFloatArray; var WI: TLearnFloatArray;
  var VL: TLearnFloat2DArray; var VR: TLearnFloat2DArray): Boolean; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure ToUpperHessenberg(var A: TLearnFloat2DArray; N: TLearnInteger;
  var TAU: TLearnFloatArray); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure UnpackQFromUpperHessenberg(const A: TLearnFloat2DArray; N: TLearnInteger;
  const TAU: TLearnFloatArray; var Q: TLearnFloat2DArray); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure UnpackHFromUpperHessenberg(const A: TLearnFloat2DArray; N: TLearnInteger;
  const TAU: TLearnFloatArray; var H: TLearnFloat2DArray); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Finding the eigenvalues and eigenvectors of a symmetric matrix

  The algorithm finds eigen pairs of a symmetric matrix by reducing it to
  tridiagonal form and using the QL/QR algorithm.

  Input parameters:
  A       -   symmetric matrix which is given by its upper or lower
  triangular part.
  Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.
  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  Output parameters:
  D       -   eigenvalues in ascending order.
  Array whose index ranges within [0..N-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn’t changed;
  * 1, Z contains the eigenvectors.
  Array whose indexes range within [0..N-1, 0..N-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged (rare case).
  ************************************************************************ *)
function SMatrixEVD(A: TLearnFloat2DArray; N: TLearnInteger; ZNeeded: TLearnInteger;
  IsUpper: Boolean; var D: TLearnFloatArray; var Z: TLearnFloat2DArray): Boolean;
var
  TAU: TLearnFloatArray;
  E  : TLearnFloatArray;
begin
  A := DynamicArrayCopy(A);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'SMatrixEVD: incorrect ZNeeded');
  SMatrixTD(A, N, IsUpper, TAU, D, E);
  if ZNeeded = 1 then
    begin
      SMatrixTDUnpackQ(A, N, IsUpper, TAU, Z);
    end;
  Result := SMatrixTDEVD(D, E, N, ZNeeded, Z);
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues (and eigenvectors) of  a  symmetric
  matrix  in  a  given half open interval (A, B] by using  a  bisection  and
  inverse iteration

  Input parameters:
  A       -   symmetric matrix which is given by its upper or lower
  triangular part. Array [0..N-1, 0..N-1].
  N       -   size of matrix A.
  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.
  IsUpperA -  storage format of matrix A.
  B1, B2 -    half open interval (B1, B2] to search eigenvalues in.

  Output parameters:
  M       -   number of eigenvalues found in a given half-interval (M>=0).
  W       -   packed array of the eigenvalues found.
  Array whose index ranges within [0..M-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn’t changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..M-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if successful. M contains the number of eigenvalues in the given
  half-interval (could be equal to 0), W contains the eigenvalues,
  Z contains the eigenvectors (if needed).

  False, if the bisection method subroutine wasn't able to find the
  eigenvalues in the given interval or if the inverse iteration subroutine
  wasn't able to find all the corresponding eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned,
  M is equal to 0.
  ************************************************************************ *)
function SMatrixEVDR(A: TLearnFloat2DArray; N: TLearnInteger; ZNeeded: TLearnInteger;
  IsUpper: Boolean; B1: TLearnFloat; B2: TLearnFloat; var M: TLearnInteger;
  var W: TLearnFloatArray; var Z: TLearnFloat2DArray): Boolean;
var
  TAU: TLearnFloatArray;
  E  : TLearnFloatArray;
begin
  A := DynamicArrayCopy(A);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'SMatrixTDEVDR: incorrect ZNeeded');
  SMatrixTD(A, N, IsUpper, TAU, W, E);
  if ZNeeded = 1 then
    begin
      SMatrixTDUnpackQ(A, N, IsUpper, TAU, Z);
    end;
  Result := SMatrixTDEVDR(W, E, N, ZNeeded, B1, B2, M, Z);
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues and  eigenvectors  of  a  symmetric
  matrix with given indexes by using bisection and inverse iteration methods.

  Input parameters:
  A       -   symmetric matrix which is given by its upper or lower
  triangular part. Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.
  IsUpperA -  storage format of matrix A.
  I1, I2 -    index interval for searching (from I1 to I2).
  0 <= I1 <= I2 <= N-1.

  Output parameters:
  W       -   packed array of the eigenvalues found.
  Array whose index ranges within [0..I2-I1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn’t changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..I2-I1].
  In that case, the eigenvectors are stored in the matrix columns.

  Result:
  True, if successful. W contains the eigenvalues, Z contains the
  eigenvectors (if needed).

  False, if the bisection method subroutine wasn't able to find the
  eigenvalues in the given interval or if the inverse iteration subroutine
  wasn't able to find all the corresponding eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned.
  ************************************************************************ *)
function SMatrixEVDI(A: TLearnFloat2DArray; N: TLearnInteger; ZNeeded: TLearnInteger;
  IsUpper: Boolean; I1: TLearnInteger; I2: TLearnInteger; var W: TLearnFloatArray;
  var Z: TLearnFloat2DArray): Boolean;
var
  TAU: TLearnFloatArray;
  E  : TLearnFloatArray;
begin
  A := DynamicArrayCopy(A);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'SMatrixEVDI: incorrect ZNeeded');
  SMatrixTD(A, N, IsUpper, TAU, W, E);
  if ZNeeded = 1 then
    begin
      SMatrixTDUnpackQ(A, N, IsUpper, TAU, Z);
    end;
  Result := SMatrixTDEVDI(W, E, N, ZNeeded, I1, I2, Z);
end;

(* ************************************************************************
  Finding the eigenvalues and eigenvectors of a Hermitian matrix

  The algorithm finds eigen pairs of a Hermitian matrix by  reducing  it  to
  real tridiagonal form and using the QL/QR algorithm.

  Input parameters:
  A       -   Hermitian matrix which is given  by  its  upper  or  lower
  triangular part.
  Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.
  ZNeeded -   flag controlling whether the eigenvectors  are  needed  or
  not. If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  Output parameters:
  D       -   eigenvalues in ascending order.
  Array whose index ranges within [0..N-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn’t changed;
  * 1, Z contains the eigenvectors.
  Array whose indexes range within [0..N-1, 0..N-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged (rare case).

  Note:
  eigenvectors of Hermitian matrix are defined up to  multiplication  by
  a complex number L, such that |L|=1.
  ************************************************************************ *)
function HMatrixEVD(A: TLearnComplex2DArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; IsUpper: Boolean; var D: TLearnFloatArray;
  var Z: TLearnComplex2DArray): Boolean;
var
  TAU : TLearnComplexArray;
  E   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  T   : TLearnFloat2DArray;
  Q   : TLearnComplex2DArray;
  I   : TLearnInteger;
  K   : TLearnInteger;
  V   : TLearnFloat;
begin
  A := DynamicArrayCopy(A);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'HermitianEVD: incorrect ZNeeded');

  //
  // Reduce to tridiagonal form
  //
  HMatrixTD(A, N, IsUpper, TAU, D, E);
  if ZNeeded = 1 then
    begin
      HMatrixTDUnpackQ(A, N, IsUpper, TAU, Q);
      ZNeeded := 2;
    end;

  //
  // TDEVD
  //
  Result := SMatrixTDEVD(D, E, N, ZNeeded, T);

  //
  // Eigenvectors are needed
  // Calculate Z = Q*T = Re(Q)*T + i*Im(Q)*T
  //
  if Result and (ZNeeded <> 0) then
    begin
      SetLength(WORK, N (* - 1 + 1 // optimized compiler *) );
      SetLength(Z, N (* - 1 + 1 // optimized compiler *) , N (* - 1 + 1 // optimized compiler *) );
      I := 0;
      while I <= N - 1 do
        begin

          //
          // Calculate real part
          //
          K := 0;
          while K <= N - 1 do
            begin
              WORK[K] := 0;
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              V := Q[I, K].X;
              APVAdd(@WORK[0], 0, N - 1, @T[K][0], 0, N - 1, V);
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              Z[I, K].X := WORK[K];
              Inc(K);
            end;

          //
          // Calculate imaginary part
          //
          K := 0;
          while K <= N - 1 do
            begin
              WORK[K] := 0;
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              V := Q[I, K].Y;
              APVAdd(@WORK[0], 0, N - 1, @T[K][0], 0, N - 1, V);
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              Z[I, K].Y := WORK[K];
              Inc(K);
            end;
          Inc(I);
        end;
    end;
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues (and eigenvectors) of  a  Hermitian
  matrix  in  a  given half-interval (A, B] by using a bisection and inverse
  iteration

  Input parameters:
  A       -   Hermitian matrix which is given  by  its  upper  or  lower
  triangular  part.  Array  whose   indexes   range   within
  [0..N-1, 0..N-1].
  N       -   size of matrix A.
  ZNeeded -   flag controlling whether the eigenvectors  are  needed  or
  not. If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.
  IsUpperA -  storage format of matrix A.
  B1, B2 -    half-interval (B1, B2] to search eigenvalues in.

  Output parameters:
  M       -   number of eigenvalues found in a given half-interval, M>=0
  W       -   packed array of the eigenvalues found.
  Array whose index ranges within [0..M-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn’t changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..M-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if successful. M contains the number of eigenvalues in the given
  half-interval (could be equal to 0), W contains the eigenvalues,
  Z contains the eigenvectors (if needed).

  False, if the bisection method subroutine  wasn't  able  to  find  the
  eigenvalues  in  the  given  interval  or  if  the  inverse  iteration
  subroutine  wasn't  able  to  find all the corresponding eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned, M  is
  equal to 0.

  Note:
  eigen vectors of Hermitian matrix are defined up to multiplication  by
  a complex number L, such as |L|=1.
  ************************************************************************ *)
function HMatrixEVDR(A: TLearnComplex2DArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; IsUpper: Boolean; B1: TLearnFloat; B2: TLearnFloat;
  var M: TLearnInteger; var W: TLearnFloatArray; var Z: TLearnComplex2DArray): Boolean;
var
  Q   : TLearnComplex2DArray;
  T   : TLearnFloat2DArray;
  TAU : TLearnComplexArray;
  E   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  I   : TLearnInteger;
  K   : TLearnInteger;
  V   : TLearnFloat;
begin
  A := DynamicArrayCopy(A);
  Assert((ZNeeded = 0) or (ZNeeded = 1),
    'HermitianEigenValuesAndVectorsInInterval: incorrect ZNeeded');

  //
  // Reduce to tridiagonal form
  //
  HMatrixTD(A, N, IsUpper, TAU, W, E);
  if ZNeeded = 1 then
    begin
      HMatrixTDUnpackQ(A, N, IsUpper, TAU, Q);
      ZNeeded := 2;
    end;

  //
  // Bisection and inverse iteration
  //
  Result := SMatrixTDEVDR(W, E, N, ZNeeded, B1, B2, M, T);

  //
  // Eigenvectors are needed
  // Calculate Z = Q*T = Re(Q)*T + i*Im(Q)*T
  //
  if Result and (ZNeeded <> 0) and (M <> 0) then
    begin
      SetLength(WORK, M (* - 1 + 1 // optimized compiler *) );
      SetLength(Z, N (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      I := 0;
      while I <= N - 1 do
        begin

          //
          // Calculate real part
          //
          K := 0;
          while K <= M - 1 do
            begin
              WORK[K] := 0;
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              V := Q[I, K].X;
              APVAdd(@WORK[0], 0, M - 1, @T[K][0], 0, M - 1, V);
              Inc(K);
            end;
          K := 0;
          while K <= M - 1 do
            begin
              Z[I, K].X := WORK[K];
              Inc(K);
            end;

          //
          // Calculate imaginary part
          //
          K := 0;
          while K <= M - 1 do
            begin
              WORK[K] := 0;
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              V := Q[I, K].Y;
              APVAdd(@WORK[0], 0, M - 1, @T[K][0], 0, M - 1, V);
              Inc(K);
            end;
          K := 0;
          while K <= M - 1 do
            begin
              Z[I, K].Y := WORK[K];
              Inc(K);
            end;
          Inc(I);
        end;
    end;
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues and  eigenvectors  of  a  Hermitian
  matrix with given indexes by using bisection and inverse iteration methods

  Input parameters:
  A       -   Hermitian matrix which is given  by  its  upper  or  lower
  triangular part.
  Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  ZNeeded -   flag controlling whether the eigenvectors  are  needed  or
  not. If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.
  IsUpperA -  storage format of matrix A.
  I1, I2 -    index interval for searching (from I1 to I2).
  0 <= I1 <= I2 <= N-1.

  Output parameters:
  W       -   packed array of the eigenvalues found.
  Array whose index ranges within [0..I2-I1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn’t changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..I2-I1].
  In  that  case,  the eigenvectors are stored in the matrix
  columns.

  Result:
  True, if successful. W contains the eigenvalues, Z contains the
  eigenvectors (if needed).

  False, if the bisection method subroutine  wasn't  able  to  find  the
  eigenvalues  in  the  given  interval  or  if  the  inverse  iteration
  subroutine wasn't able to find  all  the  corresponding  eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned.

  Note:
  eigen vectors of Hermitian matrix are defined up to multiplication  by
  a complex number L, such as |L|=1.
  ************************************************************************ *)
function HMatrixEVDI(A: TLearnComplex2DArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; IsUpper: Boolean; I1: TLearnInteger;
  I2: TLearnInteger; var W: TLearnFloatArray; var Z: TLearnComplex2DArray): Boolean;
var
  Q   : TLearnComplex2DArray;
  T   : TLearnFloat2DArray;
  TAU : TLearnComplexArray;
  E   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  I   : TLearnInteger;
  K   : TLearnInteger;
  V   : TLearnFloat;
  M   : TLearnInteger;
begin
  A := DynamicArrayCopy(A);
  Assert((ZNeeded = 0) or (ZNeeded = 1),
    'HermitianEigenValuesAndVectorsByIndexes: incorrect ZNeeded');

  //
  // Reduce to tridiagonal form
  //
  HMatrixTD(A, N, IsUpper, TAU, W, E);
  if ZNeeded = 1 then
    begin
      HMatrixTDUnpackQ(A, N, IsUpper, TAU, Q);
      ZNeeded := 2;
    end;

  //
  // Bisection and inverse iteration
  //
  Result := SMatrixTDEVDI(W, E, N, ZNeeded, I1, I2, T);

  //
  // Eigenvectors are needed
  // Calculate Z = Q*T = Re(Q)*T + i*Im(Q)*T
  //
  M := I2 - I1 + 1;
  if Result and (ZNeeded <> 0) then
    begin
      SetLength(WORK, M (* - 1 + 1 // optimized compiler *) );
      SetLength(Z, N (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      I := 0;
      while I <= N - 1 do
        begin

          //
          // Calculate real part
          //
          K := 0;
          while K <= M - 1 do
            begin
              WORK[K] := 0;
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              V := Q[I, K].X;
              APVAdd(@WORK[0], 0, M - 1, @T[K][0], 0, M - 1, V);
              Inc(K);
            end;
          K := 0;
          while K <= M - 1 do
            begin
              Z[I, K].X := WORK[K];
              Inc(K);
            end;

          //
          // Calculate imaginary part
          //
          K := 0;
          while K <= M - 1 do
            begin
              WORK[K] := 0;
              Inc(K);
            end;
          K := 0;
          while K <= N - 1 do
            begin
              V := Q[I, K].Y;
              APVAdd(@WORK[0], 0, M - 1, @T[K][0], 0, M - 1, V);
              Inc(K);
            end;
          K := 0;
          while K <= M - 1 do
            begin
              Z[I, K].Y := WORK[K];
              Inc(K);
            end;
          Inc(I);
        end;
    end;
end;

(* ************************************************************************
  Finding the eigenvalues and eigenvectors of a tridiagonal symmetric matrix

  The algorithm finds the eigen pairs of a tridiagonal symmetric matrix by
  using an QL/QR algorithm with implicit shifts.

  Input parameters:
  D       -   the main diagonal of a tridiagonal matrix.
  Array whose index ranges within [0..N-1].
  E       -   the secondary diagonal of a tridiagonal matrix.
  Array whose index ranges within [0..N-2].
  N       -   size of matrix A.
  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not needed;
  * 1, the eigenvectors of a tridiagonal matrix
  are multiplied by the square matrix Z. It is used if the
  tridiagonal matrix is obtained by the similarity
  transformation of a symmetric matrix;
  * 2, the eigenvectors of a tridiagonal matrix replace the
  square matrix Z;
  * 3, matrix Z contains the first row of the eigenvectors
  matrix.
  Z       -   if ZNeeded=1, Z contains the square matrix by which the
  eigenvectors are multiplied.
  Array whose indexes range within [0..N-1, 0..N-1].

  Output parameters:
  D       -   eigenvalues in ascending order.
  Array whose index ranges within [0..N-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn’t changed;
  * 1, Z contains the product of a given matrix (from the left)
  and the eigenvectors matrix (from the right);
  * 2, Z contains the eigenvectors.
  * 3, Z contains the first row of the eigenvectors matrix.
  If ZNeeded<3, Z is the array whose indexes range within [0..N-1, 0..N-1].
  In that case, the eigenvectors are stored in the matrix columns.
  If ZNeeded=3, Z is the array whose indexes range within [0..0, 0..N-1].

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged.
  ************************************************************************ *)
function SMatrixTDEVD(var D: TLearnFloatArray; E: TLearnFloatArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; var Z: TLearnFloat2DArray): Boolean;
var
  D1: TLearnFloatArray;
  E1: TLearnFloatArray;
  Z1: TLearnFloat2DArray;
  I : TLearnInteger;
begin
  E := DynamicArrayCopy(E);

  //
  // Prepare 1-based task
  //
  SetLength(D1, N + 1);
  SetLength(E1, N + 1);
  APVMove(@D1[0], 1, N, @D[0], 0, N - 1);
  if N > 1 then
    begin
      APVMove(@E1[0], 1, N - 1, @E[0], 0, N - 2);
    end;
  if ZNeeded = 1 then
    begin
      SetLength(Z1, N + 1, N + 1);
      I := 1;
      while I <= N do
        begin
          APVMove(@Z1[I][0], 1, N, @Z[I - 1][0], 0, N - 1);
          Inc(I);
        end;
    end;

  //
  // Solve 1-based task
  //
  Result := TridiagonalEVD(D1, E1, N, ZNeeded, Z1);
  if not Result then
    begin
      Exit;
    end;

  //
  // Convert back to 0-based result
  //
  APVMove(@D[0], 0, N - 1, @D1[0], 1, N);
  if ZNeeded <> 0 then
    begin
      if ZNeeded = 1 then
        begin
          I := 1;
          while I <= N do
            begin
              APVMove(@Z[I - 1][0], 0, N - 1, @Z1[I][0], 1, N);
              Inc(I);
            end;
          Exit;
        end;
      if ZNeeded = 2 then
        begin
          SetLength(Z, N (* - 1 + 1 // optimized compiler *) , N (* - 1 + 1 // optimized compiler *) );
          I := 1;
          while I <= N do
            begin
              APVMove(@Z[I - 1][0], 0, N - 1, @Z1[I][0], 1, N);
              Inc(I);
            end;
          Exit;
        end;
      if ZNeeded = 3 then
        begin
          SetLength(Z, 0 + 1, N (* - 1 + 1 // optimized compiler *) );
          APVMove(@Z[0][0], 0, N - 1, @Z1[1][0], 1, N);
          Exit;
        end;
      Assert(False, 'SMatrixTDEVD: Incorrect ZNeeded!');
    end;
end;

(* ************************************************************************
  Subroutine for finding the tridiagonal matrix eigenvalues/vectors in a
  given half-interval (A, B] by using bisection and inverse iteration.

  Input parameters:
  D       -   the main diagonal of a tridiagonal matrix.
  Array whose index ranges within [0..N-1].
  E       -   the secondary diagonal of a tridiagonal matrix.
  Array whose index ranges within [0..N-2].
  N       -   size of matrix, N>=0.
  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not needed;
  * 1, the eigenvectors of a tridiagonal matrix are multiplied
  by the square matrix Z. It is used if the tridiagonal
  matrix is obtained by the similarity transformation
  of a symmetric matrix.
  * 2, the eigenvectors of a tridiagonal matrix replace matrix Z.
  A, B    -   half-interval (A, B] to search eigenvalues in.
  Z       -   if ZNeeded is equal to:
  * 0, Z isn't used and remains unchanged;
  * 1, Z contains the square matrix (array whose indexes range
  within [0..N-1, 0..N-1]) which reduces the given symmetric
  matrix to tridiagonal form;
  * 2, Z isn't used (but changed on the exit).

  Output parameters:
  D       -   packed array of the eigenvalues found.
  Array whose index ranges within [0..M-1].
  M       -   number of eigenvalues found in the given half-interval (M>=0).
  Z       -   if ZNeeded is equal to:
  * 0, doesn't contain any information;
  * 1, contains the product of a given NxN matrix Z (from the
  left) and NxM matrix of the eigenvectors found (from the
  right). Array whose indexes range within [0..N-1, 0..M-1].
  * 2, contains the matrix of the eigenvectors found.
  Array whose indexes range within [0..N-1, 0..M-1].

  Result:

  True, if successful. In that case, M contains the number of eigenvalues
  in the given half-interval (could be equal to 0), D contains the eigenvalues,
  Z contains the eigenvectors (if needed).
  It should be noted that the subroutine changes the size of arrays D and Z.

  False, if the bisection method subroutine wasn't able to find the
  eigenvalues in the given interval or if the inverse iteration subroutine
  wasn't able to find all the corresponding eigenvectors. In that case,
  the eigenvalues and eigenvectors are not returned, M is equal to 0.
  ************************************************************************ *)
function SMatrixTDEVDR(var D: TLearnFloatArray; const E: TLearnFloatArray;
  N: TLearnInteger; ZNeeded: TLearnInteger; A: TLearnFloat; B: TLearnFloat;
  var M: TLearnInteger; var Z: TLearnFloat2DArray): Boolean;
var
  ErrorCode: TLearnInteger;
  NSPLIT   : TLearnInteger;
  I        : TLearnInteger;
  J        : TLearnInteger;
  K        : TLearnInteger;
  CR       : TLearnInteger;
  IBLOCK   : TLearnIntegerArray;
  ISPLIT   : TLearnIntegerArray;
  IFAIL    : TLearnIntegerArray;
  D1       : TLearnFloatArray;
  E1       : TLearnFloatArray;
  W        : TLearnFloatArray;
  Z2       : TLearnFloat2DArray;
  Z3       : TLearnFloat2DArray;
  V        : TLearnFloat;
  i_       : TLearnInteger;
  i1_      : TLearnInteger;
begin
  Assert((ZNeeded >= 0) and (ZNeeded <= 2),
    'SMatrixTDEVDR: incorrect ZNeeded!');

  //
  // Special cases
  //
  if AP_FP_Less_Eq(B, A) then
    begin
      M := 0;
      Result := True;
      Exit;
    end;
  if N <= 0 then
    begin
      M := 0;
      Result := True;
      Exit;
    end;

  //
  // Copy D,E to D1, E1
  //
  SetLength(D1, N + 1);
  APVMove(@D1[0], 1, N, @D[0], 0, N - 1);
  if N > 1 then
    begin
      SetLength(E1, N (* - 1 + 1 // optimized compiler *) );
      APVMove(@E1[0], 1, N - 1, @E[0], 0, N - 2);
    end;

  //
  // No eigen vectors
  //
  if ZNeeded = 0 then
    begin
      Result := InternalBisectionEigenValues(D1, E1, N, 2, 1, A, B, 0, 0, -1, W,
        M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result or (M = 0) then
        begin
          M := 0;
          Exit;
        end;
      SetLength(D, M (* - 1 + 1 // optimized compiler *) );
      APVMove(@D[0], 0, M - 1, @W[0], 1, M);
      Exit;
    end;

  //
  // Eigen vectors are multiplied by Z
  //
  if ZNeeded = 1 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(D1, E1, N, 2, 2, A, B, 0, 0, -1, W,
        M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result or (M = 0) then
        begin
          M := 0;
          Exit;
        end;
      InternalDSTEIN(N, D1, E1, M, W, IBLOCK, ISPLIT, Z2, IFAIL, CR);
      if CR <> 0 then
        begin
          M := 0;
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      I := 1;
      while I <= M do
        begin
          K := I;
          J := I;
          while J <= M do
            begin
              if AP_FP_Less(W[J], W[K]) then
                begin
                  K := J;
                end;
              Inc(J);
            end;
          V := W[I];
          W[I] := W[K];
          W[K] := V;
          J := 1;
          while J <= N do
            begin
              V := Z2[J, I];
              Z2[J, I] := Z2[J, K];
              Z2[J, K] := V;
              Inc(J);
            end;
          Inc(I);
        end;

      //
      // Transform Z2 and overwrite Z
      //
      SetLength(Z3, M + 1, N + 1);
      I := 1;
      while I <= M do
        begin
          for i_ := 1 to N do
            begin
              Z3[I, i_] := Z2[i_, I];
            end;
          Inc(I);
        end;
      I := 1;
      while I <= N do
        begin
          J := 1;
          while J <= M do
            begin
              V := APVDotProduct(@Z[I - 1][0], 0, N - 1, @Z3[J][0], 1, N);
              Z2[I, J] := V;
              Inc(J);
            end;
          Inc(I);
        end;
      SetLength(Z, N (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to N - 1 do
            begin
              Z[i_, I - 1] := Z2[i_ + i1_, I];
            end;
          Inc(I);
        end;

      //
      // Store W
      //
      SetLength(D, M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          D[I - 1] := W[I];
          Inc(I);
        end;
      Exit;
    end;

  //
  // Eigen vectors are stored in Z
  //
  if ZNeeded = 2 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(D1, E1, N, 2, 2, A, B, 0, 0, -1, W,
        M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result or (M = 0) then
        begin
          M := 0;
          Exit;
        end;
      InternalDSTEIN(N, D1, E1, M, W, IBLOCK, ISPLIT, Z2, IFAIL, CR);
      if CR <> 0 then
        begin
          M := 0;
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      I := 1;
      while I <= M do
        begin
          K := I;
          J := I;
          while J <= M do
            begin
              if AP_FP_Less(W[J], W[K]) then
                begin
                  K := J;
                end;
              Inc(J);
            end;
          V := W[I];
          W[I] := W[K];
          W[K] := V;
          J := 1;
          while J <= N do
            begin
              V := Z2[J, I];
              Z2[J, I] := Z2[J, K];
              Z2[J, K] := V;
              Inc(J);
            end;
          Inc(I);
        end;

      //
      // Store W
      //
      SetLength(D, M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          D[I - 1] := W[I];
          Inc(I);
        end;
      SetLength(Z, N (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to N - 1 do
            begin
              Z[i_, I - 1] := Z2[i_ + i1_, I];
            end;
          Inc(I);
        end;
      Exit;
    end;
  Result := False;
end;

(* ************************************************************************
  Subroutine for finding tridiagonal matrix eigenvalues/vectors with given
  indexes (in ascending order) by using the bisection and inverse iteraion.

  Input parameters:
  D       -   the main diagonal of a tridiagonal matrix.
  Array whose index ranges within [0..N-1].
  E       -   the secondary diagonal of a tridiagonal matrix.
  Array whose index ranges within [0..N-2].
  N       -   size of matrix. N>=0.
  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not needed;
  * 1, the eigenvectors of a tridiagonal matrix are multiplied
  by the square matrix Z. It is used if the
  tridiagonal matrix is obtained by the similarity transformation
  of a symmetric matrix.
  * 2, the eigenvectors of a tridiagonal matrix replace
  matrix Z.
  I1, I2  -   index interval for searching (from I1 to I2).
  0 <= I1 <= I2 <= N-1.
  Z       -   if ZNeeded is equal to:
  * 0, Z isn't used and remains unchanged;
  * 1, Z contains the square matrix (array whose indexes range within [0..N-1, 0..N-1])
  which reduces the given symmetric matrix to  tridiagonal form;
  * 2, Z isn't used (but changed on the exit).

  Output parameters:
  D       -   packed array of the eigenvalues found.
  Array whose index ranges within [0..I2-I1].
  Z       -   if ZNeeded is equal to:
  * 0, doesn't contain any information;
  * 1, contains the product of a given NxN matrix Z (from the left) and
  Nx(I2-I1) matrix of the eigenvectors found (from the right).
  Array whose indexes range within [0..N-1, 0..I2-I1].
  * 2, contains the matrix of the eigenvalues found.
  Array whose indexes range within [0..N-1, 0..I2-I1].


  Result:

  True, if successful. In that case, D contains the eigenvalues,
  Z contains the eigenvectors (if needed).
  It should be noted that the subroutine changes the size of arrays D and Z.

  False, if the bisection method subroutine wasn't able to find the eigenvalues
  in the given interval or if the inverse iteration subroutine wasn't able
  to find all the corresponding eigenvectors. In that case, the eigenvalues
  and eigenvectors are not returned.
  ************************************************************************ *)
function SMatrixTDEVDI(var D: TLearnFloatArray; const E: TLearnFloatArray;
  N: TLearnInteger; ZNeeded: TLearnInteger; I1: TLearnInteger;
  I2: TLearnInteger; var Z: TLearnFloat2DArray): Boolean;
var
  ErrorCode: TLearnInteger;
  NSPLIT   : TLearnInteger;
  I        : TLearnInteger;
  J        : TLearnInteger;
  K        : TLearnInteger;
  M        : TLearnInteger;
  CR       : TLearnInteger;
  IBLOCK   : TLearnIntegerArray;
  ISPLIT   : TLearnIntegerArray;
  IFAIL    : TLearnIntegerArray;
  W        : TLearnFloatArray;
  D1       : TLearnFloatArray;
  E1       : TLearnFloatArray;
  Z2       : TLearnFloat2DArray;
  Z3       : TLearnFloat2DArray;
  V        : TLearnFloat;
  i_       : TLearnInteger;
  i1_      : TLearnInteger;
begin
  Assert((0 <= I1) and (I1 <= I2) and (I2 < N),
    'SMatrixTDEVDI: incorrect I1/I2!');

  //
  // Copy D,E to D1, E1
  //
  SetLength(D1, N + 1);
  APVMove(@D1[0], 1, N, @D[0], 0, N - 1);
  if N > 1 then
    begin
      SetLength(E1, N (* - 1 + 1 // optimized compiler *) );
      APVMove(@E1[0], 1, N - 1, @E[0], 0, N - 2);
    end;

  //
  // No eigen vectors
  //
  if ZNeeded = 0 then
    begin
      Result := InternalBisectionEigenValues(D1, E1, N, 3, 1, 0, 0, I1 + 1,
        I2 + 1, -1, W, M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result then
        begin
          Exit;
        end;
      if M <> I2 - I1 + 1 then
        begin
          Result := False;
          Exit;
        end;
      SetLength(D, M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          D[I - 1] := W[I];
          Inc(I);
        end;
      Exit;
    end;

  //
  // Eigen vectors are multiplied by Z
  //
  if ZNeeded = 1 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(D1, E1, N, 3, 2, 0, 0, I1 + 1,
        I2 + 1, -1, W, M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result then
        begin
          Exit;
        end;
      if M <> I2 - I1 + 1 then
        begin
          Result := False;
          Exit;
        end;
      InternalDSTEIN(N, D1, E1, M, W, IBLOCK, ISPLIT, Z2, IFAIL, CR);
      if CR <> 0 then
        begin
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      I := 1;
      while I <= M do
        begin
          K := I;
          J := I;
          while J <= M do
            begin
              if AP_FP_Less(W[J], W[K]) then
                begin
                  K := J;
                end;
              Inc(J);
            end;
          V := W[I];
          W[I] := W[K];
          W[K] := V;
          J := 1;
          while J <= N do
            begin
              V := Z2[J, I];
              Z2[J, I] := Z2[J, K];
              Z2[J, K] := V;
              Inc(J);
            end;
          Inc(I);
        end;

      //
      // Transform Z2 and overwrite Z
      //
      SetLength(Z3, M + 1, N + 1);
      I := 1;
      while I <= M do
        begin
          for i_ := 1 to N do
            begin
              Z3[I, i_] := Z2[i_, I];
            end;
          Inc(I);
        end;
      I := 1;
      while I <= N do
        begin
          J := 1;
          while J <= M do
            begin
              V := APVDotProduct(@Z[I - 1][0], 0, N - 1, @Z3[J][0], 1, N);
              Z2[I, J] := V;
              Inc(J);
            end;
          Inc(I);
        end;
      SetLength(Z, N (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to N - 1 do
            begin
              Z[i_, I - 1] := Z2[i_ + i1_, I];
            end;
          Inc(I);
        end;

      //
      // Store W
      //
      SetLength(D, M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          D[I - 1] := W[I];
          Inc(I);
        end;
      Exit;
    end;

  //
  // Eigen vectors are stored in Z
  //
  if ZNeeded = 2 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(D1, E1, N, 3, 2, 0, 0, I1 + 1,
        I2 + 1, -1, W, M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result then
        begin
          Exit;
        end;
      if M <> I2 - I1 + 1 then
        begin
          Result := False;
          Exit;
        end;
      InternalDSTEIN(N, D1, E1, M, W, IBLOCK, ISPLIT, Z2, IFAIL, CR);
      if CR <> 0 then
        begin
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      I := 1;
      while I <= M do
        begin
          K := I;
          J := I;
          while J <= M do
            begin
              if AP_FP_Less(W[J], W[K]) then
                begin
                  K := J;
                end;
              Inc(J);
            end;
          V := W[I];
          W[I] := W[K];
          W[K] := V;
          J := 1;
          while J <= N do
            begin
              V := Z2[J, I];
              Z2[J, I] := Z2[J, K];
              Z2[J, K] := V;
              Inc(J);
            end;
          Inc(I);
        end;

      //
      // Store Z
      //
      SetLength(Z, N (* - 1 + 1 // optimized compiler *) , M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to N - 1 do
            begin
              Z[i_, I - 1] := Z2[i_ + i1_, I];
            end;
          Inc(I);
        end;

      //
      // Store W
      //
      SetLength(D, M (* - 1 + 1 // optimized compiler *) );
      I := 1;
      while I <= M do
        begin
          D[I - 1] := W[I];
          Inc(I);
        end;
      Exit;
    end;
  Result := False;
end;

(* ************************************************************************
  Finding eigenvalues and eigenvectors of a general matrix

  The algorithm finds eigenvalues and eigenvectors of a general matrix by
  using the QR algorithm with multiple shifts. The algorithm can find
  eigenvalues and both left and right eigenvectors.

  The right eigenvector is a vector x such that A*x = w*x, and the left
  eigenvector is a vector y such that y'*A = w*y' (here y' implies a complex
  conjugate transposition of vector y).

  Input parameters:
  A       -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  VNeeded -   flag controlling whether eigenvectors are needed or not.
  If VNeeded is equal to:
  * 0, eigenvectors are not returned;
  * 1, right eigenvectors are returned;
  * 2, left eigenvectors are returned;
  * 3, both left and right eigenvectors are returned.

  Output parameters:
  WR      -   real parts of eigenvalues.
  Array whose index ranges within [0..N-1].
  WR      -   imaginary parts of eigenvalues.
  Array whose index ranges within [0..N-1].
  VL, VR  -   arrays of left and right eigenvectors (if they are needed).
  If WI[i]=0, the respective eigenvalue is a real number,
  and it corresponds to the column number I of matrices VL/VR.
  If WI[i]>0, we have a pair of complex conjugate numbers with
  positive and negative imaginary parts:
  the first eigenvalue WR[i] + sqrt(-1)*WI[i];
  the second eigenvalue WR[i+1] + sqrt(-1)*WI[i+1];
  WI[i]>0
  WI[i+1] = -WI[i] < 0
  In that case, the eigenvector  corresponding to the first
  eigenvalue is located in i and i+1 columns of matrices
  VL/VR (the column number i contains the real part, and the
  column number i+1 contains the imaginary part), and the vector
  corresponding to the second eigenvalue is a complex conjugate to
  the first vector.
  Arrays whose indexes range within [0..N-1, 0..N-1].

  Result:
  True, if the algorithm has converged.
  False, if the algorithm has not converged.

  Note 1:
  Some users may ask the following question: what if WI[N-1]>0?
  WI[N] must contain an eigenvalue which is complex conjugate to the
  N-th eigenvalue, but the array has only size N?
  The answer is as follows: such a situation cannot occur because the
  algorithm finds a pairs of eigenvalues, therefore, if WI[i]>0, I is
  strictly less than N-1.

  Note 2:
  The algorithm performance depends on the value of the internal parameter
  NS of the InternalSchurDecomposition subroutine which defines the number
  of shifts in the QR algorithm (similarly to the block width in block-matrix
  algorithms of linear algebra). If you require maximum performance
  on your machine, it is recommended to adjust this parameter manually.

  See also the InternalTREVC subroutine.
  ************************************************************************ *)
function RMatrixEVD(A: TLearnFloat2DArray; N: TLearnInteger; VNeeded: TLearnInteger;
  var WR: TLearnFloatArray; var WI: TLearnFloatArray; var VL: TLearnFloat2DArray;
  var VR: TLearnFloat2DArray): Boolean;
var
  A1 : TLearnFloat2DArray;
  VL1: TLearnFloat2DArray;
  VR1: TLearnFloat2DArray;
  WR1: TLearnFloatArray;
  WI1: TLearnFloatArray;
  I  : TLearnInteger;
begin
  A := DynamicArrayCopy(A);
  Assert((VNeeded >= 0) and (VNeeded <= 3), 'RMatrixEVD: incorrect VNeeded!');
  SetLength(A1, N + 1, N + 1);
  I := 1;
  while I <= N do
    begin
      APVMove(@A1[I][0], 1, N, @A[I - 1][0], 0, N - 1);
      Inc(I);
    end;
  Result := NonSymmetricEVD(A1, N, VNeeded, WR1, WI1, VL1, VR1);
  if Result then
    begin
      SetLength(WR, N (* - 1 + 1 // optimized compiler *) );
      SetLength(WI, N (* - 1 + 1 // optimized compiler *) );
      APVMove(@WR[0], 0, N - 1, @WR1[0], 1, N);
      APVMove(@WI[0], 0, N - 1, @WI1[0], 1, N);
      if (VNeeded = 2) or (VNeeded = 3) then
        begin
          SetLength(VL, N (* - 1 + 1 // optimized compiler *) , N (* - 1 + 1 // optimized compiler *) );
          I := 0;
          while I <= N - 1 do
            begin
              APVMove(@VL[I][0], 0, N - 1, @VL1[I + 1][0], 1, N);
              Inc(I);
            end;
        end;
      if (VNeeded = 1) or (VNeeded = 3) then
        begin
          SetLength(VR, N (* - 1 + 1 // optimized compiler *) , N (* - 1 + 1 // optimized compiler *) );
          I := 0;
          while I <= N - 1 do
            begin
              APVMove(@VR[I][0], 0, N - 1, @VR1[I + 1][0], 1, N);
              Inc(I);
            end;
        end;
    end;
end;

function InternalBisectionEigenValues(D: TLearnFloatArray; E: TLearnFloatArray;
  N: TLearnInteger; IRANGE: TLearnInteger; IORDER: TLearnInteger;
  VL: TLearnFloat; VU: TLearnFloat; IL: TLearnInteger; IU: TLearnInteger;
  ABSTOL: TLearnFloat; var W: TLearnFloatArray; var M: TLearnInteger;
  var NSPLIT: TLearnInteger; var IBLOCK: TLearnIntegerArray;
  var ISPLIT: TLearnIntegerArray; var ErrorCode: TLearnInteger): Boolean;
var
  FUDGE      : TLearnFloat;
  RELFAC     : TLearnFloat;
  NCNVRG     : Boolean;
  TOOFEW     : Boolean;
  IB         : TLearnInteger;
  IBEGIN     : TLearnInteger;
  IDISCL     : TLearnInteger;
  IDISCU     : TLearnInteger;
  IE         : TLearnInteger;
  IEND       : TLearnInteger;
  IINFO      : TLearnInteger;
  IM         : TLearnInteger;
  IIN        : TLearnInteger;
  IOFF       : TLearnInteger;
  IOUT       : TLearnInteger;
  ITMAX      : TLearnInteger;
  IW         : TLearnInteger;
  IWOFF      : TLearnInteger;
  J          : TLearnInteger;
  ITMP1      : TLearnInteger;
  JB         : TLearnInteger;
  JDISC      : TLearnInteger;
  JE         : TLearnInteger;
  NWL        : TLearnInteger;
  NWU        : TLearnInteger;
  ATOLI      : TLearnFloat;
  BNORM      : TLearnFloat;
  GL         : TLearnFloat;
  GU         : TLearnFloat;
  PIVMIN     : TLearnFloat;
  RTOLI      : TLearnFloat;
  SAFEMN     : TLearnFloat;
  TMP1       : TLearnFloat;
  TMP2       : TLearnFloat;
  TNORM      : TLearnFloat;
  ULP        : TLearnFloat;
  WKILL      : TLearnFloat;
  WL         : TLearnFloat;
  WLU        : TLearnFloat;
  WU         : TLearnFloat;
  WUL        : TLearnFloat;
  ScaleFactor: TLearnFloat;
  T          : TLearnFloat;
  IDUMMA     : TLearnIntegerArray;
  WORK       : TLearnFloatArray;
  IWORK      : TLearnIntegerArray;
  IA1S2      : TLearnIntegerArray;
  RA1S2      : TLearnFloatArray;
  RA1S2X2    : TLearnFloat2DArray;
  IA1S2X2    : TLearnInteger2DArray;
  RA1SIIN    : TLearnFloatArray;
  RA2SIIN    : TLearnFloatArray;
  RA3SIIN    : TLearnFloatArray;
  RA4SIIN    : TLearnFloatArray;
  RA1SIINX2  : TLearnFloat2DArray;
  IA1SIINX2  : TLearnInteger2DArray;
  IWORKSPACE : TLearnIntegerArray;
  RWORKSPACE : TLearnFloatArray;
  TmpI       : TLearnInteger;
begin
  D := DynamicArrayCopy(D);
  E := DynamicArrayCopy(E);

  //
  // Quick return if possible
  //
  M := 0;
  if N = 0 then
    begin
      Result := True;
      Exit;
    end;

  //
  // Get machine constants
  // NB is the minimum vector length for vector bisection, or 0
  // if only scalar is to be done.
  //
  FUDGE := 2;
  RELFAC := 2;
  SAFEMN := MinRealNumber;
  ULP := 2 * MachineEpsilon;
  RTOLI := ULP * RELFAC;
  SetLength(IDUMMA, 1 + 1);
  SetLength(WORK, 4 * N + 1);
  SetLength(IWORK, 3 * N + 1);
  SetLength(W, N + 1);
  SetLength(IBLOCK, N + 1);
  SetLength(ISPLIT, N + 1);
  SetLength(IA1S2, 2 + 1);
  SetLength(RA1S2, 2 + 1);
  SetLength(RA1S2X2, 2 + 1, 2 + 1);
  SetLength(IA1S2X2, 2 + 1, 2 + 1);
  SetLength(RA1SIIN, N + 1);
  SetLength(RA2SIIN, N + 1);
  SetLength(RA3SIIN, N + 1);
  SetLength(RA4SIIN, N + 1);
  SetLength(RA1SIINX2, N + 1, 2 + 1);
  SetLength(IA1SIINX2, N + 1, 2 + 1);
  SetLength(IWORKSPACE, N + 1);
  SetLength(RWORKSPACE, N + 1);

  //
  // Check for Errors
  //
  Result := False;
  ErrorCode := 0;
  if (IRANGE <= 0) or (IRANGE >= 4) then
    begin
      ErrorCode := -4;
    end;
  if (IORDER <= 0) or (IORDER >= 3) then
    begin
      ErrorCode := -5;
    end;
  if N < 0 then
    begin
      ErrorCode := -3;
    end;
  if (IRANGE = 2) and AP_FP_Greater_Eq(VL, VU) then
    begin
      ErrorCode := -6;
    end;
  if (IRANGE = 3) and ((IL < 1) or (IL > Max(1, N))) then
    begin
      ErrorCode := -8;
    end;
  if (IRANGE = 3) and ((IU < Min(N, IL)) or (IU > N)) then
    begin
      ErrorCode := -9;
    end;
  if ErrorCode <> 0 then
    begin
      Exit;
    end;

  //
  // Initialize error flags
  //
  NCNVRG := False;
  TOOFEW := False;

  //
  // Simplifications:
  //
  if (IRANGE = 3) and (IL = 1) and (IU = N) then
    begin
      IRANGE := 1;
    end;

  //
  // Special Case when N=1
  //
  if N = 1 then
    begin
      NSPLIT := 1;
      ISPLIT[1] := 1;
      if (IRANGE = 2) and (AP_FP_Greater_Eq(VL, D[1]) or AP_FP_Less(VU, D[1]))
      then
        begin
          M := 0;
        end
      else
        begin
          W[1] := D[1];
          IBLOCK[1] := 1;
          M := 1;
        end;
      Result := True;
      Exit;
    end;

  //
  // Scaling
  //
  T := ABSReal(D[N]);
  J := 1;
  while J <= N - 1 do
    begin
      T := Max(T, ABSReal(D[J]));
      T := Max(T, ABSReal(E[J]));
      Inc(J);
    end;
  ScaleFactor := 1;
  if AP_FP_Neq(T, 0) then
    begin
      if AP_FP_Greater(T, SQRT(SQRT(MinRealNumber)) * SQRT(MaxRealNumber)) then
        begin
          ScaleFactor := T;
        end;
      if AP_FP_Less(T, SQRT(SQRT(MaxRealNumber)) * SQRT(MinRealNumber)) then
        begin
          ScaleFactor := T;
        end;
      J := 1;
      while J <= N - 1 do
        begin
          D[J] := D[J] / ScaleFactor;
          E[J] := E[J] / ScaleFactor;
          Inc(J);
        end;
      D[N] := D[N] / ScaleFactor;
    end;

  //
  // Compute Splitting Points
  //
  NSPLIT := 1;
  WORK[N] := 0;
  PIVMIN := 1;
  J := 2;
  while J <= N do
    begin
      TMP1 := AP_Sqr(E[J - 1]);
      if AP_FP_Greater(ABSReal(D[J] * D[J - 1]) * AP_Sqr(ULP) + SAFEMN, TMP1) then
        begin
          ISPLIT[NSPLIT] := J - 1;
          NSPLIT := NSPLIT + 1;
          WORK[J - 1] := 0;
        end
      else
        begin
          WORK[J - 1] := TMP1;
          PIVMIN := Max(PIVMIN, TMP1);
        end;
      Inc(J);
    end;
  ISPLIT[NSPLIT] := N;
  PIVMIN := PIVMIN * SAFEMN;

  //
  // Compute Interval and ATOLI
  //
  if IRANGE = 3 then
    begin

      //
      // RANGE='I': Compute the interval containing eigenvalues
      // IL through IU.
      //
      // Compute Gershgorin interval for entire (split) matrix
      // and use it as the initial interval
      //
      GU := D[1];
      GL := D[1];
      TMP1 := 0;
      J := 1;
      while J <= N - 1 do
        begin
          TMP2 := SQRT(WORK[J]);
          GU := Max(GU, D[J] + TMP1 + TMP2);
          GL := Min(GL, D[J] - TMP1 - TMP2);
          TMP1 := TMP2;
          Inc(J);
        end;
      GU := Max(GU, D[N] + TMP1);
      GL := Min(GL, D[N] - TMP1);
      TNORM := Max(ABSReal(GL), ABSReal(GU));
      GL := GL - FUDGE * TNORM * ULP * N - FUDGE * 2 * PIVMIN;
      GU := GU + FUDGE * TNORM * ULP * N + FUDGE * PIVMIN;

      //
      // Compute Iteration parameters
      //
      ITMAX := Ceil((Ln(TNORM + PIVMIN) - Ln(PIVMIN)) / Ln(2)) + 2;
      if AP_FP_Less_Eq(ABSTOL, 0) then
        begin
          ATOLI := ULP * TNORM;
        end
      else
        begin
          ATOLI := ABSTOL;
        end;
      WORK[N + 1] := GL;
      WORK[N + 2] := GL;
      WORK[N + 3] := GU;
      WORK[N + 4] := GU;
      WORK[N + 5] := GL;
      WORK[N + 6] := GU;
      IWORK[1] := -1;
      IWORK[2] := -1;
      IWORK[3] := N + 1;
      IWORK[4] := N + 1;
      IWORK[5] := IL - 1;
      IWORK[6] := IU;

      //
      // Calling DLAEBZ
      //
      // DLAEBZ( 3, ITMAX, N, 2, 2, NB, ATOLI, RTOLI, PIVMIN, D, E,
      // WORK, IWORK( 5 ), WORK( N+1 ), WORK( N+5 ), IOUT,
      // IWORK, W, IBLOCK, IINFO )
      //
      IA1S2[1] := IWORK[5];
      IA1S2[2] := IWORK[6];
      RA1S2[1] := WORK[N + 5];
      RA1S2[2] := WORK[N + 6];
      RA1S2X2[1, 1] := WORK[N + 1];
      RA1S2X2[2, 1] := WORK[N + 2];
      RA1S2X2[1, 2] := WORK[N + 3];
      RA1S2X2[2, 2] := WORK[N + 4];
      IA1S2X2[1, 1] := IWORK[1];
      IA1S2X2[2, 1] := IWORK[2];
      IA1S2X2[1, 2] := IWORK[3];
      IA1S2X2[2, 2] := IWORK[4];
      InternalDLAEBZ(3, ITMAX, N, 2, 2, ATOLI, RTOLI, PIVMIN, D, E, WORK, IA1S2,
        RA1S2X2, RA1S2, IOUT, IA1S2X2, W, IBLOCK, IINFO);
      IWORK[5] := IA1S2[1];
      IWORK[6] := IA1S2[2];
      WORK[N + 5] := RA1S2[1];
      WORK[N + 6] := RA1S2[2];
      WORK[N + 1] := RA1S2X2[1, 1];
      WORK[N + 2] := RA1S2X2[2, 1];
      WORK[N + 3] := RA1S2X2[1, 2];
      WORK[N + 4] := RA1S2X2[2, 2];
      IWORK[1] := IA1S2X2[1, 1];
      IWORK[2] := IA1S2X2[2, 1];
      IWORK[3] := IA1S2X2[1, 2];
      IWORK[4] := IA1S2X2[2, 2];
      if IWORK[6] = IU then
        begin
          WL := WORK[N + 1];
          WLU := WORK[N + 3];
          NWL := IWORK[1];
          WU := WORK[N + 4];
          WUL := WORK[N + 2];
          NWU := IWORK[4];
        end
      else
        begin
          WL := WORK[N + 2];
          WLU := WORK[N + 4];
          NWL := IWORK[2];
          WU := WORK[N + 3];
          WUL := WORK[N + 1];
          NWU := IWORK[3];
        end;
      if (NWL < 0) or (NWL >= N) or (NWU < 1) or (NWU > N) then
        begin
          ErrorCode := 4;
          Result := False;
          Exit;
        end;
    end
  else
    begin

      //
      // RANGE='A' or 'V' -- Set ATOLI
      //
      TNORM := Max(ABSReal(D[1]) + ABSReal(E[1]),
        ABSReal(D[N]) + ABSReal(E[N - 1]));
      J := 2;
      while J <= N - 1 do
        begin
          TNORM := Max(TNORM, ABSReal(D[J]) + ABSReal(E[J - 1]) + ABSReal(E[J]));
          Inc(J);
        end;
      if AP_FP_Less_Eq(ABSTOL, 0) then
        begin
          ATOLI := ULP * TNORM;
        end
      else
        begin
          ATOLI := ABSTOL;
        end;
      if IRANGE = 2 then
        begin
          WL := VL;
          WU := VU;
        end
      else
        begin
          WL := 0;
          WU := 0;
        end;
    end;

  //
  // Find Eigenvalues -- Loop Over Blocks and recompute NWL and NWU.
  // NWL accumulates the number of eigenvalues .le. WL,
  // NWU accumulates the number of eigenvalues .le. WU
  //
  M := 0;
  IEND := 0;
  ErrorCode := 0;
  NWL := 0;
  NWU := 0;
  JB := 1;
  while JB <= NSPLIT do
    begin
      IOFF := IEND;
      IBEGIN := IOFF + 1;
      IEND := ISPLIT[JB];
      IIN := IEND - IOFF;
      if IIN = 1 then
        begin

          //
          // Special Case -- IIN=1
          //
          if (IRANGE = 1) or AP_FP_Greater_Eq(WL, D[IBEGIN] - PIVMIN) then
            begin
              NWL := NWL + 1;
            end;
          if (IRANGE = 1) or AP_FP_Greater_Eq(WU, D[IBEGIN] - PIVMIN) then
            begin
              NWU := NWU + 1;
            end;
          if (IRANGE = 1) or AP_FP_Less(WL, D[IBEGIN] - PIVMIN) and
            AP_FP_Greater_Eq(WU, D[IBEGIN] - PIVMIN) then
            begin
              M := M + 1;
              W[M] := D[IBEGIN];
              IBLOCK[M] := JB;
            end;
        end
      else
        begin

          //
          // General Case -- IIN > 1
          //
          // Compute Gershgorin Interval
          // and use it as the initial interval
          //
          GU := D[IBEGIN];
          GL := D[IBEGIN];
          TMP1 := 0;
          J := IBEGIN;
          while J <= IEND - 1 do
            begin
              TMP2 := ABSReal(E[J]);
              GU := Max(GU, D[J] + TMP1 + TMP2);
              GL := Min(GL, D[J] - TMP1 - TMP2);
              TMP1 := TMP2;
              Inc(J);
            end;
          GU := Max(GU, D[IEND] + TMP1);
          GL := Min(GL, D[IEND] - TMP1);
          BNORM := Max(ABSReal(GL), ABSReal(GU));
          GL := GL - FUDGE * BNORM * ULP * IIN - FUDGE * PIVMIN;
          GU := GU + FUDGE * BNORM * ULP * IIN + FUDGE * PIVMIN;

          //
          // Compute ATOLI for the current submatrix
          //
          if AP_FP_Less_Eq(ABSTOL, 0) then
            begin
              ATOLI := ULP * Max(ABSReal(GL), ABSReal(GU));
            end
          else
            begin
              ATOLI := ABSTOL;
            end;
          if IRANGE > 1 then
            begin
              if AP_FP_Less(GU, WL) then
                begin
                  NWL := NWL + IIN;
                  NWU := NWU + IIN;
                  Inc(JB);
                  Continue;
                end;
              GL := Max(GL, WL);
              GU := Min(GU, WU);
              if AP_FP_Greater_Eq(GL, GU) then
                begin
                  Inc(JB);
                  Continue;
                end;
            end;

          //
          // Set Up Initial Interval
          //
          WORK[N + 1] := GL;
          WORK[N + IIN + 1] := GU;

          //
          // Calling DLAEBZ
          //
          // CALL DLAEBZ( 1, 0, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
          // D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ),
          // IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IM,
          // IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
          //
          TmpI := 1;
          while TmpI <= IIN do
            begin
              RA1SIIN[TmpI] := D[IBEGIN - 1 + TmpI];
              if IBEGIN - 1 + TmpI < N then
                begin
                  RA2SIIN[TmpI] := E[IBEGIN - 1 + TmpI];
                end;
              RA3SIIN[TmpI] := WORK[IBEGIN - 1 + TmpI];
              RA1SIINX2[TmpI, 1] := WORK[N + TmpI];
              RA1SIINX2[TmpI, 2] := WORK[N + TmpI + IIN];
              RA4SIIN[TmpI] := WORK[N + 2 * IIN + TmpI];
              RWORKSPACE[TmpI] := W[M + TmpI];
              IWORKSPACE[TmpI] := IBLOCK[M + TmpI];
              IA1SIINX2[TmpI, 1] := IWORK[TmpI];
              IA1SIINX2[TmpI, 2] := IWORK[TmpI + IIN];
              Inc(TmpI);
            end;
          InternalDLAEBZ(1, 0, IIN, IIN, 1, ATOLI, RTOLI, PIVMIN, RA1SIIN, RA2SIIN,
            RA3SIIN, IDUMMA, RA1SIINX2, RA4SIIN, IM, IA1SIINX2, RWORKSPACE,
            IWORKSPACE, IINFO);
          TmpI := 1;
          while TmpI <= IIN do
            begin
              WORK[N + TmpI] := RA1SIINX2[TmpI, 1];
              WORK[N + TmpI + IIN] := RA1SIINX2[TmpI, 2];
              WORK[N + 2 * IIN + TmpI] := RA4SIIN[TmpI];
              W[M + TmpI] := RWORKSPACE[TmpI];
              IBLOCK[M + TmpI] := IWORKSPACE[TmpI];
              IWORK[TmpI] := IA1SIINX2[TmpI, 1];
              IWORK[TmpI + IIN] := IA1SIINX2[TmpI, 2];
              Inc(TmpI);
            end;
          NWL := NWL + IWORK[1];
          NWU := NWU + IWORK[IIN + 1];
          IWOFF := M - IWORK[1];

          //
          // Compute Eigenvalues
          //
          ITMAX := Ceil((Ln(GU - GL + PIVMIN) - Ln(PIVMIN)) / Ln(2)) + 2;

          //
          // Calling DLAEBZ
          //
          // CALL DLAEBZ( 2, ITMAX, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
          // D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ),
          // IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IOUT,
          // IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
          //
          TmpI := 1;
          while TmpI <= IIN do
            begin
              RA1SIIN[TmpI] := D[IBEGIN - 1 + TmpI];
              if IBEGIN - 1 + TmpI < N then
                begin
                  RA2SIIN[TmpI] := E[IBEGIN - 1 + TmpI];
                end;
              RA3SIIN[TmpI] := WORK[IBEGIN - 1 + TmpI];
              RA1SIINX2[TmpI, 1] := WORK[N + TmpI];
              RA1SIINX2[TmpI, 2] := WORK[N + TmpI + IIN];
              RA4SIIN[TmpI] := WORK[N + 2 * IIN + TmpI];
              RWORKSPACE[TmpI] := W[M + TmpI];
              IWORKSPACE[TmpI] := IBLOCK[M + TmpI];
              IA1SIINX2[TmpI, 1] := IWORK[TmpI];
              IA1SIINX2[TmpI, 2] := IWORK[TmpI + IIN];
              Inc(TmpI);
            end;
          InternalDLAEBZ(2, ITMAX, IIN, IIN, 1, ATOLI, RTOLI, PIVMIN, RA1SIIN,
            RA2SIIN, RA3SIIN, IDUMMA, RA1SIINX2, RA4SIIN, IOUT, IA1SIINX2,
            RWORKSPACE, IWORKSPACE, IINFO);
          TmpI := 1;
          while TmpI <= IIN do
            begin
              WORK[N + TmpI] := RA1SIINX2[TmpI, 1];
              WORK[N + TmpI + IIN] := RA1SIINX2[TmpI, 2];
              WORK[N + 2 * IIN + TmpI] := RA4SIIN[TmpI];
              W[M + TmpI] := RWORKSPACE[TmpI];
              IBLOCK[M + TmpI] := IWORKSPACE[TmpI];
              IWORK[TmpI] := IA1SIINX2[TmpI, 1];
              IWORK[TmpI + IIN] := IA1SIINX2[TmpI, 2];
              Inc(TmpI);
            end;

          //
          // Copy Eigenvalues Into W and IBLOCK
          // Use -JB for block number for unconverged eigenvalues.
          //
          J := 1;
          while J <= IOUT do
            begin
              TMP1 := 0.5 * (WORK[J + N] + WORK[J + IIN + N]);

              //
              // Flag non-convergence.
              //
              if J > IOUT - IINFO then
                begin
                  NCNVRG := True;
                  IB := -JB;
                end
              else
                begin
                  IB := JB;
                end;
              JE := IWORK[J] + 1 + IWOFF;
              while JE <= IWORK[J + IIN] + IWOFF do
                begin
                  W[JE] := TMP1;
                  IBLOCK[JE] := IB;
                  Inc(JE);
                end;
              Inc(J);
            end;
          M := M + IM;
        end;
      Inc(JB);
    end;

  //
  // If RANGE='I', then (WL,WU) contains eigenvalues NWL+1,...,NWU
  // If NWL+1 < IL or NWU > IU, discard extra eigenvalues.
  //
  if IRANGE = 3 then
    begin
      IM := 0;
      IDISCL := IL - 1 - NWL;
      IDISCU := NWU - IU;
      if (IDISCL > 0) or (IDISCU > 0) then
        begin
          JE := 1;
          while JE <= M do
            begin
              if AP_FP_Less_Eq(W[JE], WLU) and (IDISCL > 0) then
                begin
                  IDISCL := IDISCL - 1;
                end
              else
                begin
                  if AP_FP_Greater_Eq(W[JE], WUL) and (IDISCU > 0) then
                    begin
                      IDISCU := IDISCU - 1;
                    end
                  else
                    begin
                      IM := IM + 1;
                      W[IM] := W[JE];
                      IBLOCK[IM] := IBLOCK[JE];
                    end;
                end;
              Inc(JE);
            end;
          M := IM;
        end;
      if (IDISCL > 0) or (IDISCU > 0) then
        begin

          //
          // Code to deal with effects of bad arithmetic:
          // Some low eigenvalues to be discarded are not in (WL,WLU],
          // or high eigenvalues to be discarded are not in (WUL,WU]
          // so just kill off the smallest IDISCL/largest IDISCU
          // eigenvalues, by simply finding the smallest/largest
          // eigenvalue(s).
          //
          // (If N(w) is monotone non-decreasing, this should never
          // happen.)
          //
          if IDISCL > 0 then
            begin
              WKILL := WU;
              JDISC := 1;
              while JDISC <= IDISCL do
                begin
                  IW := 0;
                  JE := 1;
                  while JE <= M do
                    begin
                      if (IBLOCK[JE] <> 0) and (AP_FP_Less(W[JE], WKILL) or (IW = 0)) then
                        begin
                          IW := JE;
                          WKILL := W[JE];
                        end;
                      Inc(JE);
                    end;
                  IBLOCK[IW] := 0;
                  Inc(JDISC);
                end;
            end;
          if IDISCU > 0 then
            begin
              WKILL := WL;
              JDISC := 1;
              while JDISC <= IDISCU do
                begin
                  IW := 0;
                  JE := 1;
                  while JE <= M do
                    begin
                      if (IBLOCK[JE] <> 0) and (AP_FP_Greater(W[JE], WKILL) or (IW = 0))
                      then
                        begin
                          IW := JE;
                          WKILL := W[JE];
                        end;
                      Inc(JE);
                    end;
                  IBLOCK[IW] := 0;
                  Inc(JDISC);
                end;
            end;
          IM := 0;
          JE := 1;
          while JE <= M do
            begin
              if IBLOCK[JE] <> 0 then
                begin
                  IM := IM + 1;
                  W[IM] := W[JE];
                  IBLOCK[IM] := IBLOCK[JE];
                end;
              Inc(JE);
            end;
          M := IM;
        end;
      if (IDISCL < 0) or (IDISCU < 0) then
        begin
          TOOFEW := True;
        end;
    end;

  //
  // If ORDER='B', do nothing -- the eigenvalues are already sorted
  // by block.
  // If ORDER='E', sort the eigenvalues from smallest to largest
  //
  if (IORDER = 1) and (NSPLIT > 1) then
    begin
      JE := 1;
      while JE <= M - 1 do
        begin
          IE := 0;
          TMP1 := W[JE];
          J := JE + 1;
          while J <= M do
            begin
              if AP_FP_Less(W[J], TMP1) then
                begin
                  IE := J;
                  TMP1 := W[J];
                end;
              Inc(J);
            end;
          if IE <> 0 then
            begin
              ITMP1 := IBLOCK[IE];
              W[IE] := W[JE];
              IBLOCK[IE] := IBLOCK[JE];
              W[JE] := TMP1;
              IBLOCK[JE] := ITMP1;
            end;
          Inc(JE);
        end;
    end;
  J := 1;
  while J <= M do
    begin
      W[J] := W[J] * ScaleFactor;
      Inc(J);
    end;
  ErrorCode := 0;
  if NCNVRG then
    begin
      ErrorCode := ErrorCode + 1;
    end;
  if TOOFEW then
    begin
      ErrorCode := ErrorCode + 2;
    end;
  Result := ErrorCode = 0;
end;

procedure InternalDSTEIN(const N: TLearnInteger; const D: TLearnFloatArray;
  E: TLearnFloatArray; const M: TLearnInteger; W: TLearnFloatArray;
  const IBLOCK: TLearnIntegerArray; const ISPLIT: TLearnIntegerArray;
  var Z: TLearnFloat2DArray; var IFAIL: TLearnIntegerArray; var INFO: TLearnInteger);
var
  MAXITS      : TLearnInteger;
  EXTRA       : TLearnInteger;
  B1          : TLearnInteger;
  BLKSIZ      : TLearnInteger;
  BN          : TLearnInteger;
  GPIND       : TLearnInteger;
  I           : TLearnInteger;
  IINFO       : TLearnInteger;
  ITS         : TLearnInteger;
  J           : TLearnInteger;
  J1          : TLearnInteger;
  JBLK        : TLearnInteger;
  JMAX        : TLearnInteger;
  NBLK        : TLearnInteger;
  NRMCHK      : TLearnInteger;
  DTPCRT      : TLearnFloat;
  EPS         : TLearnFloat;
  EPS1        : TLearnFloat;
  NRM         : TLearnFloat;
  ONENRM      : TLearnFloat;
  ORTOL       : TLearnFloat;
  PERTOL      : TLearnFloat;
  SCL         : TLearnFloat;
  SEP         : TLearnFloat;
  TOL         : TLearnFloat;
  XJ          : TLearnFloat;
  XJM         : TLearnFloat;
  ZTR         : TLearnFloat;
  WORK1       : TLearnFloatArray;
  WORK2       : TLearnFloatArray;
  WORK3       : TLearnFloatArray;
  WORK4       : TLearnFloatArray;
  WORK5       : TLearnFloatArray;
  IWORK       : TLearnIntegerArray;
  TmpCriterion: Boolean;
  TI          : TLearnInteger;
  I1          : TLearnInteger;
  I2          : TLearnInteger;
  V           : TLearnFloat;
  i_          : TLearnInteger;
  i1_         : TLearnInteger;
begin
  E := DynamicArrayCopy(E);
  W := DynamicArrayCopy(W);
  MAXITS := 5;
  EXTRA := 2;
  SetLength(WORK1, Max(N, 1) + 1);
  SetLength(WORK2, Max(N - 1, 1) + 1);
  SetLength(WORK3, Max(N, 1) + 1);
  SetLength(WORK4, Max(N, 1) + 1);
  SetLength(WORK5, Max(N, 1) + 1);
  SetLength(IWORK, Max(N, 1) + 1);
  SetLength(IFAIL, Max(M, 1) + 1);
  SetLength(Z, Max(N, 1) + 1, Max(M, 1) + 1);

  //
  // Test the input parameters.
  //
  INFO := 0;
  I := 1;
  while I <= M do
    begin
      IFAIL[I] := 0;
      Inc(I);
    end;
  if N < 0 then
    begin
      INFO := -1;
      Exit;
    end;
  if (M < 0) or (M > N) then
    begin
      INFO := -4;
      Exit;
    end;
  J := 2;
  while J <= M do
    begin
      if IBLOCK[J] < IBLOCK[J - 1] then
        begin
          INFO := -6;
          Break;
        end;
      if (IBLOCK[J] = IBLOCK[J - 1]) and AP_FP_Less(W[J], W[J - 1]) then
        begin
          INFO := -5;
          Break;
        end;
      Inc(J);
    end;
  if INFO <> 0 then
    begin
      Exit;
    end;

  //
  // Quick return if possible
  //
  if (N = 0) or (M = 0) then
    begin
      Exit;
    end;
  if N = 1 then
    begin
      Z[1, 1] := 1;
      Exit;
    end;

  //
  // Some preparations
  //
  TI := N - 1;
  APVMove(@WORK1[0], 1, TI, @E[0], 1, TI);
  SetLength(E, N + 1);
  APVMove(@E[0], 1, TI, @WORK1[0], 1, TI);
  APVMove(@WORK1[0], 1, M, @W[0], 1, M);
  SetLength(W, N + 1);
  APVMove(@W[0], 1, M, @WORK1[0], 1, M);

  //
  // Get machine constants.
  //
  EPS := MachineEpsilon;

  //
  // Compute eigenvectors of matrix blocks.
  //
  J1 := 1;
  NBLK := 1;
  while NBLK <= IBLOCK[M] do
    begin

      //
      // Find starting and ending indices of block nblk.
      //
      if NBLK = 1 then
        begin
          B1 := 1;
        end
      else
        begin
          B1 := ISPLIT[NBLK - 1] + 1;
        end;
      BN := ISPLIT[NBLK];
      BLKSIZ := BN - B1 + 1;
      if BLKSIZ <> 1 then
        begin

          //
          // Compute reorthogonalization criterion and stopping criterion.
          //
          GPIND := B1;
          ONENRM := ABSReal(D[B1]) + ABSReal(E[B1]);
          ONENRM := Max(ONENRM, ABSReal(D[BN]) + ABSReal(E[BN - 1]));
          I := B1 + 1;
          while I <= BN - 1 do
            begin
              ONENRM := Max(ONENRM, ABSReal(D[I]) + ABSReal(E[I - 1]) +
                ABSReal(E[I]));
              Inc(I);
            end;
          ORTOL := 0.001 * ONENRM;
          DTPCRT := SQRT(0.1 / BLKSIZ);
        end;

      //
      // Loop through eigenvalues of block nblk.
      //
      JBLK := 0;
      J := J1;
      while J <= M do
        begin
          if IBLOCK[J] <> NBLK then
            begin
              J1 := J;
              Break;
            end;
          JBLK := JBLK + 1;
          XJ := W[J];
          if BLKSIZ = 1 then
            begin

              //
              // Skip all the work if the block size is one.
              //
              WORK1[1] := 1;
            end
          else
            begin

              //
              // If eigenvalues j and j-1 are too close, add a relatively
              // small perturbation.
              //
              if JBLK > 1 then
                begin
                  EPS1 := ABSReal(EPS * XJ);
                  PERTOL := 10 * EPS1;
                  SEP := XJ - XJM;
                  if AP_FP_Less(SEP, PERTOL) then
                    begin
                      XJ := XJM + PERTOL;
                    end;
                end;
              ITS := 0;
              NRMCHK := 0;

              //
              // Get random starting vector.
              //
              TI := 1;
              while TI <= BLKSIZ do
                begin
                  WORK1[TI] := 2 * RandomReal - 1;
                  Inc(TI);
                end;

              //
              // Copy the matrix T so it won't be destroyed in factorization.
              //
              TI := 1;
              while TI <= BLKSIZ - 1 do
                begin
                  WORK2[TI] := E[B1 + TI - 1];
                  WORK3[TI] := E[B1 + TI - 1];
                  WORK4[TI] := D[B1 + TI - 1];
                  Inc(TI);
                end;
              WORK4[BLKSIZ] := D[B1 + BLKSIZ - 1];

              //
              // Compute LU factors with partial pivoting  ( PT = LU )
              //
              TOL := 0;
              TDINInternalDLAGTF(BLKSIZ, WORK4, XJ, WORK2, WORK3, TOL, WORK5,
                IWORK, IINFO);

              //
              // Update iteration count.
              //
              repeat
                ITS := ITS + 1;
                if ITS > MAXITS then
                  begin

                    //
                    // If stopping criterion was not satisfied, update info and
                    // store eigenvector number in array ifail.
                    //
                    INFO := INFO + 1;
                    IFAIL[INFO] := J;
                    Break;
                  end;

                //
                // Normalize and scale the righthand side vector Pb.
                //
                V := 0;
                TI := 1;
                while TI <= BLKSIZ do
                  begin
                    V := V + ABSReal(WORK1[TI]);
                    Inc(TI);
                  end;
                SCL := BLKSIZ * ONENRM * Max(EPS, ABSReal(WORK4[BLKSIZ])) / V;
                APVMul(@WORK1[0], 1, BLKSIZ, SCL);

                //
                // Solve the system LU = Pb.
                //
                TDINInternalDLAGTS(BLKSIZ, WORK4, WORK2, WORK3, WORK5, IWORK, WORK1,
                  TOL, IINFO);

                //
                // Reorthogonalize by modified Gram-Schmidt if eigenvalues are
                // close enough.
                //
                if JBLK <> 1 then
                  begin
                    if AP_FP_Greater(ABSReal(XJ - XJM), ORTOL) then
                      begin
                        GPIND := J;
                      end;
                    if GPIND <> J then
                      begin
                        I := GPIND;
                        while I <= J - 1 do
                          begin
                            I1 := B1;
                            I2 := B1 + BLKSIZ - 1;
                            i1_ := (I1) - (1);
                            ZTR := 0.0;
                            for i_ := 1 to BLKSIZ do
                              begin
                                ZTR := ZTR + WORK1[i_] * Z[i_ + i1_, I];
                              end;
                            i1_ := (I1) - (1);
                            for i_ := 1 to BLKSIZ do
                              begin
                                WORK1[i_] := WORK1[i_] - ZTR * Z[i_ + i1_, I];
                              end;
                            Inc(I);
                          end;
                      end;
                  end;

                //
                // Check the infinity norm of the iterate.
                //
                JMAX := VectorIdxAbsMax(WORK1, 1, BLKSIZ);
                NRM := ABSReal(WORK1[JMAX]);

                //
                // Continue for additional iterations after norm reaches
                // stopping criterion.
                //
                TmpCriterion := False;
                if AP_FP_Less(NRM, DTPCRT) then
                  begin
                    TmpCriterion := True;
                  end
                else
                  begin
                    NRMCHK := NRMCHK + 1;
                    if NRMCHK < EXTRA + 1 then
                      begin
                        TmpCriterion := True;
                      end;
                  end;
              until not TmpCriterion;

              //
              // Accept iterate as jth eigenvector.
              //
              SCL := 1 / VectorNorm2(WORK1, 1, BLKSIZ);
              JMAX := VectorIdxAbsMax(WORK1, 1, BLKSIZ);
              if AP_FP_Less(WORK1[JMAX], 0) then
                begin
                  SCL := -SCL;
                end;
              APVMul(@WORK1[0], 1, BLKSIZ, SCL);
            end;
          I := 1;
          while I <= N do
            begin
              Z[I, J] := 0;
              Inc(I);
            end;
          I := 1;
          while I <= BLKSIZ do
            begin
              Z[B1 + I - 1, J] := WORK1[I];
              Inc(I);
            end;

          //
          // Save the shift to check eigenvalue spacing at next
          // iteration.
          //
          XJM := XJ;
          Inc(J);
        end;
      Inc(NBLK);
    end;
end;

function TridiagonalEVD(var D: TLearnFloatArray; E: TLearnFloatArray; N: TLearnInteger;
  ZNeeded: TLearnInteger; var Z: TLearnFloat2DArray): Boolean;
var
  MAXIT       : TLearnInteger;
  I           : TLearnInteger;
  II          : TLearnInteger;
  ISCALE      : TLearnInteger;
  J           : TLearnInteger;
  JTOT        : TLearnInteger;
  K           : TLearnInteger;
  T           : TLearnInteger;
  L           : TLearnInteger;
  L1          : TLearnInteger;
  LEND        : TLearnInteger;
  LENDM1      : TLearnInteger;
  LENDP1      : TLearnInteger;
  LENDSV      : TLearnInteger;
  LM1         : TLearnInteger;
  LSV         : TLearnInteger;
  M           : TLearnInteger;
  MM          : TLearnInteger;
  MM1         : TLearnInteger;
  NM1         : TLearnInteger;
  NMAXIT      : TLearnInteger;
  TmpInt      : TLearnInteger;
  ANORM       : TLearnFloat;
  B           : TLearnFloat;
  C           : TLearnFloat;
  EPS         : TLearnFloat;
  EPS2        : TLearnFloat;
  F           : TLearnFloat;
  G           : TLearnFloat;
  P           : TLearnFloat;
  R           : TLearnFloat;
  RT1         : TLearnFloat;
  RT2         : TLearnFloat;
  S           : TLearnFloat;
  SAFMAX      : TLearnFloat;
  SAFMIN      : TLearnFloat;
  SSFMAX      : TLearnFloat;
  SSFMIN      : TLearnFloat;
  TST         : TLearnFloat;
  Tmp         : TLearnFloat;
  WORK1       : TLearnFloatArray;
  WORK2       : TLearnFloatArray;
  WORKC       : TLearnFloatArray;
  WORKS       : TLearnFloatArray;
  WTEMP       : TLearnFloatArray;
  GotoFlag    : Boolean;
  ZRows       : TLearnInteger;
  WasTranspose: Boolean;
  i_          : TLearnInteger;
begin
  E := DynamicArrayCopy(E);
  Assert((ZNeeded >= 0) and (ZNeeded <= 3),
    'TridiagonalEVD: Incorrent ZNeeded');

  //
  // Quick return if possible
  //
  if (ZNeeded < 0) or (ZNeeded > 3) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
  if N = 0 then
    begin
      Exit;
    end;
  if N = 1 then
    begin
      if (ZNeeded = 2) or (ZNeeded = 3) then
        begin
          SetLength(Z, 1 + 1, 1 + 1);
          Z[1, 1] := 1;
        end;
      Exit;
    end;
  MAXIT := 30;

  //
  // Initialize arrays
  //
  SetLength(WTEMP, N + 1);
  SetLength(WORK1, N (* - 1 + 1 // optimized compiler *) );
  SetLength(WORK2, N (* - 1 + 1 // optimized compiler *) );
  SetLength(WORKC, N + 1);
  SetLength(WORKS, N + 1);

  //
  // Determine the unit roundoff and over/underflow thresholds.
  //
  EPS := MachineEpsilon;
  EPS2 := AP_Sqr(EPS);
  SAFMIN := MinRealNumber;
  SAFMAX := MaxRealNumber;
  SSFMAX := SQRT(SAFMAX) / 3;
  SSFMIN := SQRT(SAFMIN) / EPS2;

  //
  // Prepare Z
  //
  // Here we are using transposition to get rid of column operations
  //
  //
  WasTranspose := False;
  if ZNeeded = 0 then
    begin
      ZRows := 0;
    end;
  if ZNeeded = 1 then
    begin
      ZRows := N;
    end;
  if ZNeeded = 2 then
    begin
      ZRows := N;
    end;
  if ZNeeded = 3 then
    begin
      ZRows := 1;
    end;
  if ZNeeded = 1 then
    begin
      WasTranspose := True;
      InplaceTranspose(Z, 1, N, 1, N, WTEMP);
    end;
  if ZNeeded = 2 then
    begin
      WasTranspose := True;
      SetLength(Z, N + 1, N + 1);
      I := 1;
      while I <= N do
        begin
          J := 1;
          while J <= N do
            begin
              if I = J then
                begin
                  Z[I, J] := 1;
                end
              else
                begin
                  Z[I, J] := 0;
                end;
              Inc(J);
            end;
          Inc(I);
        end;
    end;
  if ZNeeded = 3 then
    begin
      WasTranspose := False;
      SetLength(Z, 1 + 1, N + 1);
      J := 1;
      while J <= N do
        begin
          if J = 1 then
            begin
              Z[1, J] := 1;
            end
          else
            begin
              Z[1, J] := 0;
            end;
          Inc(J);
        end;
    end;
  NMAXIT := N * MAXIT;
  JTOT := 0;

  //
  // Determine where the matrix splits and choose QL or QR iteration
  // for each block, according to whether top or bottom diagonal
  // element is smaller.
  //
  L1 := 1;
  NM1 := N - 1;
  while True do
    begin
      if L1 > N then
        begin
          Break;
        end;
      if L1 > 1 then
        begin
          E[L1 - 1] := 0;
        end;
      GotoFlag := False;
      if L1 <= NM1 then
        begin
          M := L1;
          while M <= NM1 do
            begin
              TST := ABSReal(E[M]);
              if AP_FP_Eq(TST, 0) then
                begin
                  GotoFlag := True;
                  Break;
                end;
              if AP_FP_Less_Eq(TST, SQRT(ABSReal(D[M])) * SQRT(ABSReal(D[M + 1]))
                * EPS) then
                begin
                  E[M] := 0;
                  GotoFlag := True;
                  Break;
                end;
              Inc(M);
            end;
        end;
      if not GotoFlag then
        begin
          M := N;
        end;

      //
      // label 30:
      //
      L := L1;
      LSV := L;
      LEND := M;
      LENDSV := LEND;
      L1 := M + 1;
      if LEND = L then
        begin
          Continue;
        end;

      //
      // Scale submatrix in rows and columns L to LEND
      //
      if L = LEND then
        begin
          ANORM := ABSReal(D[L]);
        end
      else
        begin
          ANORM := Max(ABSReal(D[L]) + ABSReal(E[L]), ABSReal(E[LEND - 1]) +
            ABSReal(D[LEND]));
          I := L + 1;
          while I <= LEND - 1 do
            begin
              ANORM := Max(ANORM, ABSReal(D[I]) + ABSReal(E[I]) + ABSReal(E[I - 1]));
              Inc(I);
            end;
        end;
      ISCALE := 0;
      if AP_FP_Eq(ANORM, 0) then
        begin
          Continue;
        end;
      if AP_FP_Greater(ANORM, SSFMAX) then
        begin
          ISCALE := 1;
          Tmp := SSFMAX / ANORM;
          TmpInt := LEND - 1;
          APVMul(@D[0], L, LEND, Tmp);
          APVMul(@E[0], L, TmpInt, Tmp);
        end;
      if AP_FP_Less(ANORM, SSFMIN) then
        begin
          ISCALE := 2;
          Tmp := SSFMIN / ANORM;
          TmpInt := LEND - 1;
          APVMul(@D[0], L, LEND, Tmp);
          APVMul(@E[0], L, TmpInt, Tmp);
        end;

      //
      // Choose between QL and QR iteration
      //
      if AP_FP_Less(ABSReal(D[LEND]), ABSReal(D[L])) then
        begin
          LEND := LSV;
          L := LENDSV;
        end;
      if LEND > L then
        begin

          //
          // QL Iteration
          //
          // Look for small subdiagonal element.
          //
          while True do
            begin
              GotoFlag := False;
              if L <> LEND then
                begin
                  LENDM1 := LEND - 1;
                  M := L;
                  while M <= LENDM1 do
                    begin
                      TST := AP_Sqr(ABSReal(E[M]));
                      if AP_FP_Less_Eq(TST, EPS2 * ABSReal(D[M]) * ABSReal(D[M + 1]) +
                        SAFMIN) then
                        begin
                          GotoFlag := True;
                          Break;
                        end;
                      Inc(M);
                    end;
                end;
              if not GotoFlag then
                begin
                  M := LEND;
                end;
              if M < LEND then
                begin
                  E[M] := 0;
                end;
              P := D[L];
              if M <> L then
                begin

                  //
                  // If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
                  // to compute its eigensystem.
                  //
                  if M = L + 1 then
                    begin
                      if ZNeeded > 0 then
                        begin
                          TdEVDEV2(D[L], E[L], D[L + 1], RT1, RT2, C, S);
                          WORK1[L] := C;
                          WORK2[L] := S;
                          WORKC[1] := WORK1[L];
                          WORKS[1] := WORK2[L];
                          if not WasTranspose then
                            begin
                              ApplyRotationsFromTheRight(False, 1, ZRows, L, L + 1, WORKC,
                                WORKS, Z, WTEMP);
                            end
                          else
                            begin
                              ApplyRotationsFromTheLeft(False, L, L + 1, 1, ZRows, WORKC,
                                WORKS, Z, WTEMP);
                            end;
                        end
                      else
                        begin
                          TdEVDE2(D[L], E[L], D[L + 1], RT1, RT2);
                        end;
                      D[L] := RT1;
                      D[L + 1] := RT2;
                      E[L] := 0;
                      L := L + 2;
                      if L <= LEND then
                        begin
                          Continue;
                        end;

                      //
                      // GOTO 140
                      //
                      Break;
                    end;
                  if JTOT = NMAXIT then
                    begin

                      //
                      // GOTO 140
                      //
                      Break;
                    end;
                  JTOT := JTOT + 1;

                  //
                  // Form shift.
                  //
                  G := (D[L + 1] - P) / (2 * E[L]);
                  R := TdEVDPythag(G, 1);
                  G := D[M] - P + E[L] / (G + TdEVDExtSign(R, G));
                  S := 1;
                  C := 1;
                  P := 0;

                  //
                  // Inner loop
                  //
                  MM1 := M - 1;
                  I := MM1;
                  while I >= L do
                    begin
                      F := S * E[I];
                      B := C * E[I];
                      GenerateRotation(G, F, C, S, R);
                      if I <> M - 1 then
                        begin
                          E[I + 1] := R;
                        end;
                      G := D[I + 1] - P;
                      R := (D[I] - G) * S + 2 * C * B;
                      P := S * R;
                      D[I + 1] := G + P;
                      G := C * R - B;

                      //
                      // If eigenvectors are desired, then save rotations.
                      //
                      if ZNeeded > 0 then
                        begin
                          WORK1[I] := C;
                          WORK2[I] := -S;
                        end;
                      Dec(I);
                    end;

                  //
                  // If eigenvectors are desired, then apply saved rotations.
                  //
                  if ZNeeded > 0 then
                    begin
                      I := L;
                      while I <= M - 1 do
                        begin
                          WORKC[I - L + 1] := WORK1[I];
                          WORKS[I - L + 1] := WORK2[I];
                          Inc(I);
                        end;
                      if not WasTranspose then
                        begin
                          ApplyRotationsFromTheRight(False, 1, ZRows, L, M, WORKC, WORKS,
                            Z, WTEMP);
                        end
                      else
                        begin
                          ApplyRotationsFromTheLeft(False, L, M, 1, ZRows, WORKC, WORKS,
                            Z, WTEMP);
                        end;
                    end;
                  D[L] := D[L] - P;
                  E[L] := G;
                  Continue;
                end;

              //
              // Eigenvalue found.
              //
              D[L] := P;
              L := L + 1;
              if L <= LEND then
                begin
                  Continue;
                end;
              Break;
            end;
        end
      else
        begin

          //
          // QR Iteration
          //
          // Look for small superdiagonal element.
          //
          while True do
            begin
              GotoFlag := False;
              if L <> LEND then
                begin
                  LENDP1 := LEND + 1;
                  M := L;
                  while M >= LENDP1 do
                    begin
                      TST := AP_Sqr(ABSReal(E[M - 1]));
                      if AP_FP_Less_Eq(TST, EPS2 * ABSReal(D[M]) * ABSReal(D[M - 1]) +
                        SAFMIN) then
                        begin
                          GotoFlag := True;
                          Break;
                        end;
                      Dec(M);
                    end;
                end;
              if not GotoFlag then
                begin
                  M := LEND;
                end;
              if M > LEND then
                begin
                  E[M - 1] := 0;
                end;
              P := D[L];
              if M <> L then
                begin

                  //
                  // If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
                  // to compute its eigensystem.
                  //
                  if M = L - 1 then
                    begin
                      if ZNeeded > 0 then
                        begin
                          TdEVDEV2(D[L - 1], E[L - 1], D[L], RT1, RT2, C, S);
                          WORK1[M] := C;
                          WORK2[M] := S;
                          WORKC[1] := C;
                          WORKS[1] := S;
                          if not WasTranspose then
                            begin
                              ApplyRotationsFromTheRight(True, 1, ZRows, L - 1, L, WORKC,
                                WORKS, Z, WTEMP);
                            end
                          else
                            begin
                              ApplyRotationsFromTheLeft(True, L - 1, L, 1, ZRows, WORKC,
                                WORKS, Z, WTEMP);
                            end;
                        end
                      else
                        begin
                          TdEVDE2(D[L - 1], E[L - 1], D[L], RT1, RT2);
                        end;
                      D[L - 1] := RT1;
                      D[L] := RT2;
                      E[L - 1] := 0;
                      L := L - 2;
                      if L >= LEND then
                        begin
                          Continue;
                        end;
                      Break;
                    end;
                  if JTOT = NMAXIT then
                    begin
                      Break;
                    end;
                  JTOT := JTOT + 1;

                  //
                  // Form shift.
                  //
                  G := (D[L - 1] - P) / (2 * E[L - 1]);
                  R := TdEVDPythag(G, 1);
                  G := D[M] - P + E[L - 1] / (G + TdEVDExtSign(R, G));
                  S := 1;
                  C := 1;
                  P := 0;

                  //
                  // Inner loop
                  //
                  LM1 := L - 1;
                  I := M;
                  while I <= LM1 do
                    begin
                      F := S * E[I];
                      B := C * E[I];
                      GenerateRotation(G, F, C, S, R);
                      if I <> M then
                        begin
                          E[I - 1] := R;
                        end;
                      G := D[I] - P;
                      R := (D[I + 1] - G) * S + 2 * C * B;
                      P := S * R;
                      D[I] := G + P;
                      G := C * R - B;

                      //
                      // If eigenvectors are desired, then save rotations.
                      //
                      if ZNeeded > 0 then
                        begin
                          WORK1[I] := C;
                          WORK2[I] := S;
                        end;
                      Inc(I);
                    end;

                  //
                  // If eigenvectors are desired, then apply saved rotations.
                  //
                  if ZNeeded > 0 then
                    begin
                      MM := L - M + 1;
                      I := M;
                      while I <= L - 1 do
                        begin
                          WORKC[I - M + 1] := WORK1[I];
                          WORKS[I - M + 1] := WORK2[I];
                          Inc(I);
                        end;
                      if not WasTranspose then
                        begin
                          ApplyRotationsFromTheRight(True, 1, ZRows, M, L, WORKC, WORKS,
                            Z, WTEMP);
                        end
                      else
                        begin
                          ApplyRotationsFromTheLeft(True, M, L, 1, ZRows, WORKC, WORKS,
                            Z, WTEMP);
                        end;
                    end;
                  D[L] := D[L] - P;
                  E[LM1] := G;
                  Continue;
                end;

              //
              // Eigenvalue found.
              //
              D[L] := P;
              L := L - 1;
              if L >= LEND then
                begin
                  Continue;
                end;
              Break;
            end;
        end;

      //
      // Undo scaling if necessary
      //
      if ISCALE = 1 then
        begin
          Tmp := ANORM / SSFMAX;
          TmpInt := LENDSV - 1;
          APVMul(@D[0], LSV, LENDSV, Tmp);
          APVMul(@E[0], LSV, TmpInt, Tmp);
        end;
      if ISCALE = 2 then
        begin
          Tmp := ANORM / SSFMIN;
          TmpInt := LENDSV - 1;
          APVMul(@D[0], LSV, LENDSV, Tmp);
          APVMul(@E[0], LSV, TmpInt, Tmp);
        end;

      //
      // Check for no convergence to an eigenvalue after a total
      // of N*MAXIT iterations.
      //
      if JTOT >= NMAXIT then
        begin
          Result := False;
          if WasTranspose then
            begin
              InplaceTranspose(Z, 1, N, 1, N, WTEMP);
            end;
          Exit;
        end;
    end;

  //
  // Order eigenvalues and eigenvectors.
  //
  if ZNeeded = 0 then
    begin

      //
      // Sort
      //
      if N = 1 then
        begin
          Exit;
        end;
      if N = 2 then
        begin
          if AP_FP_Greater(D[1], D[2]) then
            begin
              Tmp := D[1];
              D[1] := D[2];
              D[2] := Tmp;
            end;
          Exit;
        end;
      I := 2;
      repeat
        T := I;
        while T <> 1 do
          begin
            K := T div 2;
            if AP_FP_Greater_Eq(D[K], D[T]) then
              begin
                T := 1;
              end
            else
              begin
                Tmp := D[K];
                D[K] := D[T];
                D[T] := Tmp;
                T := K;
              end;
          end;
        I := I + 1;
      until not(I <= N);
      I := N - 1;
      repeat
        Tmp := D[I + 1];
        D[I + 1] := D[1];
        D[+1] := Tmp;
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
                    if AP_FP_Greater(D[K + 1], D[K]) then
                      begin
                        K := K + 1;
                      end;
                  end;
                if AP_FP_Greater_Eq(D[T], D[K]) then
                  begin
                    T := 0;
                  end
                else
                  begin
                    Tmp := D[K];
                    D[K] := D[T];
                    D[T] := Tmp;
                    T := K;
                  end;
              end;
          end;
        I := I - 1;
      until not(I >= 1);
    end
  else
    begin

      //
      // Use Selection Sort to minimize swaps of eigenvectors
      //
      II := 2;
      while II <= N do
        begin
          I := II - 1;
          K := I;
          P := D[I];
          J := II;
          while J <= N do
            begin
              if AP_FP_Less(D[J], P) then
                begin
                  K := J;
                  P := D[J];
                end;
              Inc(J);
            end;
          if K <> I then
            begin
              D[K] := D[I];
              D[I] := P;
              if WasTranspose then
                begin
                  APVMove(@WTEMP[0], 1, N, @Z[I][0], 1, N);
                  APVMove(@Z[I][0], 1, N, @Z[K][0], 1, N);
                  APVMove(@Z[K][0], 1, N, @WTEMP[0], 1, N);
                end
              else
                begin
                  for i_ := 1 to ZRows do
                    begin
                      WTEMP[i_] := Z[i_, I];
                    end;
                  for i_ := 1 to ZRows do
                    begin
                      Z[i_, I] := Z[i_, K];
                    end;
                  for i_ := 1 to ZRows do
                    begin
                      Z[i_, K] := WTEMP[i_];
                    end;
                end;
            end;
          Inc(II);
        end;
      if WasTranspose then
        begin
          InplaceTranspose(Z, 1, N, 1, N, WTEMP);
        end;
    end;
end;

(* ************************************************************************
  DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
  [  A   B  ]
  [  B   C  ].
  On return, RT1 is the eigenvalue of larger absolute value, and RT2
  is the eigenvalue of smaller absolute value.
  ************************************************************************ *)
procedure TdEVDE2(const A: TLearnFloat; const B: TLearnFloat;
  const C: TLearnFloat; var RT1: TLearnFloat; var RT2: TLearnFloat);
var
  AB  : TLearnFloat;
  ACMN: TLearnFloat;
  ACMX: TLearnFloat;
  ADF : TLearnFloat;
  DF  : TLearnFloat;
  RT  : TLearnFloat;
  SM  : TLearnFloat;
  TB  : TLearnFloat;
begin
  SM := A + C;
  DF := A - C;
  ADF := ABSReal(DF);
  TB := B + B;
  AB := ABSReal(TB);
  if AP_FP_Greater(ABSReal(A), ABSReal(C)) then
    begin
      ACMX := A;
      ACMN := C;
    end
  else
    begin
      ACMX := C;
      ACMN := A;
    end;
  if AP_FP_Greater(ADF, AB) then
    begin
      RT := ADF * SQRT(1 + AP_Sqr(AB / ADF));
    end
  else
    begin
      if AP_FP_Less(ADF, AB) then
        begin
          RT := AB * SQRT(1 + AP_Sqr(ADF / AB));
        end
      else
        begin

          //
          // Includes case AB=ADF=0
          //
          RT := AB * SQRT(2);
        end;
    end;
  if AP_FP_Less(SM, 0) then
    begin
      RT1 := 0.5 * (SM - RT);

      //
      // Order of execution important.
      // To get fully accurate smaller eigenvalue,
      // next line needs to be executed in higher precision.
      //
      RT2 := ACMX / RT1 * ACMN - B / RT1 * B;
    end
  else
    begin
      if AP_FP_Greater(SM, 0) then
        begin
          RT1 := 0.5 * (SM + RT);

          //
          // Order of execution important.
          // To get fully accurate smaller eigenvalue,
          // next line needs to be executed in higher precision.
          //
          RT2 := ACMX / RT1 * ACMN - B / RT1 * B;
        end
      else
        begin

          //
          // Includes case RT1 = RT2 = 0
          //
          RT1 := 0.5 * RT;
          RT2 := -0.5 * RT;
        end;
    end;
end;

(* ************************************************************************
  DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix

  [  A   B  ]
  [  B   C  ].

  On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
  eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
  eigenvector for RT1, giving the decomposition

  [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
  [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
  ************************************************************************ *)
procedure TdEVDEV2(const A: TLearnFloat; const B: TLearnFloat;
  const C: TLearnFloat; var RT1: TLearnFloat; var RT2: TLearnFloat;
  var CS1: TLearnFloat; var SN1: TLearnFloat);
var
  SGN1: TLearnInteger;
  SGN2: TLearnInteger;
  AB  : TLearnFloat;
  ACMN: TLearnFloat;
  ACMX: TLearnFloat;
  ACS : TLearnFloat;
  ADF : TLearnFloat;
  CS  : TLearnFloat;
  CT  : TLearnFloat;
  DF  : TLearnFloat;
  RT  : TLearnFloat;
  SM  : TLearnFloat;
  TB  : TLearnFloat;
  TN  : TLearnFloat;
begin

  //
  // Compute the eigenvalues
  //
  SM := A + C;
  DF := A - C;
  ADF := ABSReal(DF);
  TB := B + B;
  AB := ABSReal(TB);
  if AP_FP_Greater(ABSReal(A), ABSReal(C)) then
    begin
      ACMX := A;
      ACMN := C;
    end
  else
    begin
      ACMX := C;
      ACMN := A;
    end;
  if AP_FP_Greater(ADF, AB) then
    begin
      RT := ADF * SQRT(1 + AP_Sqr(AB / ADF));
    end
  else
    begin
      if AP_FP_Less(ADF, AB) then
        begin
          RT := AB * SQRT(1 + AP_Sqr(ADF / AB));
        end
      else
        begin

          //
          // Includes case AB=ADF=0
          //
          RT := AB * SQRT(2);
        end;
    end;
  if AP_FP_Less(SM, 0) then
    begin
      RT1 := 0.5 * (SM - RT);
      SGN1 := -1;

      //
      // Order of execution important.
      // To get fully accurate smaller eigenvalue,
      // next line needs to be executed in higher precision.
      //
      RT2 := ACMX / RT1 * ACMN - B / RT1 * B;
    end
  else
    begin
      if AP_FP_Greater(SM, 0) then
        begin
          RT1 := 0.5 * (SM + RT);
          SGN1 := 1;

          //
          // Order of execution important.
          // To get fully accurate smaller eigenvalue,
          // next line needs to be executed in higher precision.
          //
          RT2 := ACMX / RT1 * ACMN - B / RT1 * B;
        end
      else
        begin

          //
          // Includes case RT1 = RT2 = 0
          //
          RT1 := 0.5 * RT;
          RT2 := -0.5 * RT;
          SGN1 := 1;
        end;
    end;

  //
  // Compute the eigenvector
  //
  if AP_FP_Greater_Eq(DF, 0) then
    begin
      CS := DF + RT;
      SGN2 := 1;
    end
  else
    begin
      CS := DF - RT;
      SGN2 := -1;
    end;
  ACS := ABSReal(CS);
  if AP_FP_Greater(ACS, AB) then
    begin
      CT := -TB / CS;
      SN1 := 1 / SQRT(1 + CT * CT);
      CS1 := CT * SN1;
    end
  else
    begin
      if AP_FP_Eq(AB, 0) then
        begin
          CS1 := 1;
          SN1 := 0;
        end
      else
        begin
          TN := -CS / TB;
          CS1 := 1 / SQRT(1 + TN * TN);
          SN1 := TN * CS1;
        end;
    end;
  if SGN1 = SGN2 then
    begin
      TN := CS1;
      CS1 := -SN1;
      SN1 := TN;
    end;
end;

(* ************************************************************************
  Internal routine
  ************************************************************************ *)
function TdEVDPythag(A: TLearnFloat; B: TLearnFloat): TLearnFloat;
begin
  if AP_FP_Less(ABSReal(A), ABSReal(B)) then
    begin
      Result := ABSReal(B) * SQRT(1 + AP_Sqr(A / B));
    end
  else
    begin
      Result := ABSReal(A) * SQRT(1 + AP_Sqr(B / A));
    end;
end;

(* ************************************************************************
  Internal routine
  ************************************************************************ *)
function TdEVDExtSign(A: TLearnFloat; B: TLearnFloat): TLearnFloat;
begin
  if AP_FP_Greater_Eq(B, 0) then
    begin
      Result := ABSReal(A);
    end
  else
    begin
      Result := -ABSReal(A);
    end;
end;

procedure TDINInternalDLAGTF(const N: TLearnInteger; var A: TLearnFloatArray;
  const LAMBDA: TLearnFloat; var B: TLearnFloatArray; var C: TLearnFloatArray;
  const TOL: TLearnFloat; var D: TLearnFloatArray; var IIN: TLearnIntegerArray;
  var INFO: TLearnInteger);
var
  K     : TLearnInteger;
  EPS   : TLearnFloat;
  MULT  : TLearnFloat;
  PIV1  : TLearnFloat;
  PIV2  : TLearnFloat;
  SCALE1: TLearnFloat;
  SCALE2: TLearnFloat;
  TEMP  : TLearnFloat;
  TL    : TLearnFloat;
begin
  INFO := 0;
  if N < 0 then
    begin
      INFO := -1;
      Exit;
    end;
  if N = 0 then
    begin
      Exit;
    end;
  A[1] := A[1] - LAMBDA;
  IIN[N] := 0;
  if N = 1 then
    begin
      if AP_FP_Eq(A[1], 0) then
        begin
          IIN[1] := 1;
        end;
      Exit;
    end;
  EPS := MachineEpsilon;
  TL := Max(TOL, EPS);
  SCALE1 := ABSReal(A[1]) + ABSReal(B[1]);
  K := 1;
  while K <= N - 1 do
    begin
      A[K + 1] := A[K + 1] - LAMBDA;
      SCALE2 := ABSReal(C[K]) + ABSReal(A[K + 1]);
      if K < N - 1 then
        begin
          SCALE2 := SCALE2 + ABSReal(B[K + 1]);
        end;
      if AP_FP_Eq(A[K], 0) then
        begin
          PIV1 := 0;
        end
      else
        begin
          PIV1 := ABSReal(A[K]) / SCALE1;
        end;
      if AP_FP_Eq(C[K], 0) then
        begin
          IIN[K] := 0;
          PIV2 := 0;
          SCALE1 := SCALE2;
          if K < N - 1 then
            begin
              D[K] := 0;
            end;
        end
      else
        begin
          PIV2 := ABSReal(C[K]) / SCALE2;
          if AP_FP_Less_Eq(PIV2, PIV1) then
            begin
              IIN[K] := 0;
              SCALE1 := SCALE2;
              C[K] := C[K] / A[K];
              A[K + 1] := A[K + 1] - C[K] * B[K];
              if K < N - 1 then
                begin
                  D[K] := 0;
                end;
            end
          else
            begin
              IIN[K] := 1;
              MULT := A[K] / C[K];
              A[K] := C[K];
              TEMP := A[K + 1];
              A[K + 1] := B[K] - MULT * TEMP;
              if K < N - 1 then
                begin
                  D[K] := B[K + 1];
                  B[K + 1] := -MULT * D[K];
                end;
              B[K] := TEMP;
              C[K] := MULT;
            end;
        end;
      if AP_FP_Less_Eq(Max(PIV1, PIV2), TL) and (IIN[N] = 0) then
        begin
          IIN[N] := K;
        end;
      Inc(K);
    end;
  if AP_FP_Less_Eq(ABSReal(A[N]), SCALE1 * TL) and (IIN[N] = 0) then
    begin
      IIN[N] := N;
    end;
end;

procedure TDINInternalDLAGTS(const N: TLearnInteger; const A: TLearnFloatArray;
  const B: TLearnFloatArray; const C: TLearnFloatArray; const D: TLearnFloatArray;
  const IIN: TLearnIntegerArray; var Y: TLearnFloatArray; var TOL: TLearnFloat;
  var INFO: TLearnInteger);
var
  K     : TLearnInteger;
  ABSAK : TLearnFloat;
  AK    : TLearnFloat;
  BIGNUM: TLearnFloat;
  EPS   : TLearnFloat;
  PERT  : TLearnFloat;
  SFMIN : TLearnFloat;
  TEMP  : TLearnFloat;
begin
  INFO := 0;
  if N < 0 then
    begin
      INFO := -1;
      Exit;
    end;
  if N = 0 then
    begin
      Exit;
    end;
  EPS := MachineEpsilon;
  SFMIN := MinRealNumber;
  BIGNUM := 1 / SFMIN;
  if AP_FP_Less_Eq(TOL, 0) then
    begin
      TOL := ABSReal(A[1]);
      if N > 1 then
        begin
          TOL := Max(TOL, Max(ABSReal(A[2]), ABSReal(B[1])));
        end;
      K := 3;
      while K <= N do
        begin
          TOL := Max(TOL, Max(ABSReal(A[K]), Max(ABSReal(B[K - 1]),
            ABSReal(D[K - 2]))));
          Inc(K);
        end;
      TOL := TOL * EPS;
      if AP_FP_Eq(TOL, 0) then
        begin
          TOL := EPS;
        end;
    end;
  K := 2;
  while K <= N do
    begin
      if IIN[K - 1] = 0 then
        begin
          Y[K] := Y[K] - C[K - 1] * Y[K - 1];
        end
      else
        begin
          TEMP := Y[K - 1];
          Y[K - 1] := Y[K];
          Y[K] := TEMP - C[K - 1] * Y[K];
        end;
      Inc(K);
    end;
  K := N;
  while K >= 1 do
    begin
      if K <= N - 2 then
        begin
          TEMP := Y[K] - B[K] * Y[K + 1] - D[K] * Y[K + 2];
        end
      else
        begin
          if K = N - 1 then
            begin
              TEMP := Y[K] - B[K] * Y[K + 1];
            end
          else
            begin
              TEMP := Y[K];
            end;
        end;
      AK := A[K];
      PERT := ABSReal(TOL);
      if AP_FP_Less(AK, 0) then
        begin
          PERT := -PERT;
        end;
      while True do
        begin
          ABSAK := ABSReal(AK);
          if AP_FP_Less(ABSAK, 1) then
            begin
              if AP_FP_Less(ABSAK, SFMIN) then
                begin
                  if AP_FP_Eq(ABSAK, 0) or AP_FP_Greater(ABSReal(TEMP) * SFMIN, ABSAK)
                  then
                    begin
                      AK := AK + PERT;
                      PERT := 2 * PERT;
                      Continue;
                    end
                  else
                    begin
                      TEMP := TEMP * BIGNUM;
                      AK := AK * BIGNUM;
                    end;
                end
              else
                begin
                  if AP_FP_Greater(ABSReal(TEMP), ABSAK * BIGNUM) then
                    begin
                      AK := AK + PERT;
                      PERT := 2 * PERT;
                      Continue;
                    end;
                end;
            end;
          Break;
        end;
      Y[K] := TEMP / AK;
      Dec(K);
    end;
end;

procedure InternalDLAEBZ(const IJOB: TLearnInteger; const NITMAX: TLearnInteger;
  const N: TLearnInteger; const MMAX: TLearnInteger; const MINP: TLearnInteger;
  const ABSTOL: TLearnFloat; const RELTOL: TLearnFloat;
  const PIVMIN: TLearnFloat; const D: TLearnFloatArray; const E: TLearnFloatArray;
  const E2: TLearnFloatArray; var NVAL: TLearnIntegerArray; var AB: TLearnFloat2DArray;
  var C: TLearnFloatArray; var MOUT: TLearnInteger; var NAB: TLearnInteger2DArray;
  var WORK: TLearnFloatArray; var IWORK: TLearnIntegerArray; var INFO: TLearnInteger);
var
  ITMP1: TLearnInteger;
  ITMP2: TLearnInteger;
  J    : TLearnInteger;
  JI   : TLearnInteger;
  JIT  : TLearnInteger;
  JP   : TLearnInteger;
  KF   : TLearnInteger;
  KFNEW: TLearnInteger;
  KL   : TLearnInteger;
  KLNEW: TLearnInteger;
  TMP1 : TLearnFloat;
  TMP2 : TLearnFloat;
begin
  INFO := 0;
  if (IJOB < 1) or (IJOB > 3) then
    begin
      INFO := -1;
      Exit;
    end;

  //
  // Initialize NAB
  //
  if IJOB = 1 then
    begin

      //
      // Compute the number of eigenvalues in the initial intervals.
      //
      MOUT := 0;

      //
      // DIR$ NOVECTOR
      //
      JI := 1;
      while JI <= MINP do
        begin
          JP := 1;
          while JP <= 2 do
            begin
              TMP1 := D[1] - AB[JI, JP];
              if AP_FP_Less(ABSReal(TMP1), PIVMIN) then
                begin
                  TMP1 := -PIVMIN;
                end;
              NAB[JI, JP] := 0;
              if AP_FP_Less_Eq(TMP1, 0) then
                begin
                  NAB[JI, JP] := 1;
                end;
              J := 2;
              while J <= N do
                begin
                  TMP1 := D[J] - E2[J - 1] / TMP1 - AB[JI, JP];
                  if AP_FP_Less(ABSReal(TMP1), PIVMIN) then
                    begin
                      TMP1 := -PIVMIN;
                    end;
                  if AP_FP_Less_Eq(TMP1, 0) then
                    begin
                      NAB[JI, JP] := NAB[JI, JP] + 1;
                    end;
                  Inc(J);
                end;
              Inc(JP);
            end;
          MOUT := MOUT + NAB[JI, 2] - NAB[JI, 1];
          Inc(JI);
        end;
      Exit;
    end;

  //
  // Initialize for loop
  //
  // KF and KL have the following meaning:
  // Intervals 1,...,KF-1 have converged.
  // Intervals KF,...,KL  still need to be refined.
  //
  KF := 1;
  KL := MINP;

  //
  // If IJOB=2, initialize C.
  // If IJOB=3, use the user-supplied starting point.
  //
  if IJOB = 2 then
    begin
      JI := 1;
      while JI <= MINP do
        begin
          C[JI] := 0.5 * (AB[JI, 1] + AB[JI, 2]);
          Inc(JI);
        end;
    end;

  //
  // Iteration loop
  //
  JIT := 1;
  while JIT <= NITMAX do
    begin

      //
      // Loop over intervals
      //
      //
      // Serial Version of the loop
      //
      KLNEW := KL;
      JI := KF;
      while JI <= KL do
        begin

          //
          // Compute N(w), the number of eigenvalues less than w
          //
          TMP1 := C[JI];
          TMP2 := D[1] - TMP1;
          ITMP1 := 0;
          if AP_FP_Less_Eq(TMP2, PIVMIN) then
            begin
              ITMP1 := 1;
              TMP2 := Min(TMP2, -PIVMIN);
            end;

          //
          // A series of compiler directives to defeat vectorization
          // for the next loop
          //
          // *$PL$ CMCHAR=' '
          // CDIR$          NEXTSCALAR
          // C$DIR          SCALAR
          // CDIR$          NEXT SCALAR
          // CVD$L          NOVECTOR
          // CDEC$          NOVECTOR
          // CVD$           NOVECTOR
          // *VDIR          NOVECTOR
          // *VOCL          LOOP,SCALAR
          // CIBM           PREFER SCALAR
          // *$PL$ CMCHAR='*'
          //
          J := 2;
          while J <= N do
            begin
              TMP2 := D[J] - E2[J - 1] / TMP2 - TMP1;
              if AP_FP_Less_Eq(TMP2, PIVMIN) then
                begin
                  ITMP1 := ITMP1 + 1;
                  TMP2 := Min(TMP2, -PIVMIN);
                end;
              Inc(J);
            end;
          if IJOB <= 2 then
            begin

              //
              // IJOB=2: Choose all intervals containing eigenvalues.
              //
              // Insure that N(w) is monotone
              //
              ITMP1 := Min(NAB[JI, 2], Max(NAB[JI, 1], ITMP1));

              //
              // Update the Queue -- add intervals if both halves
              // contain eigenvalues.
              //
              if ITMP1 = NAB[JI, 2] then
                begin

                  //
                  // No eigenvalue in the upper interval:
                  // just use the lower interval.
                  //
                  AB[JI, 2] := TMP1;
                end
              else
                begin
                  if ITMP1 = NAB[JI, 1] then
                    begin

                      //
                      // No eigenvalue in the lower interval:
                      // just use the upper interval.
                      //
                      AB[JI, 1] := TMP1;
                    end
                  else
                    begin
                      if KLNEW < MMAX then
                        begin

                          //
                          // Eigenvalue in both intervals -- add upper to queue.
                          //
                          KLNEW := KLNEW + 1;
                          AB[KLNEW, 2] := AB[JI, 2];
                          NAB[KLNEW, 2] := NAB[JI, 2];
                          AB[KLNEW, 1] := TMP1;
                          NAB[KLNEW, 1] := ITMP1;
                          AB[JI, 2] := TMP1;
                          NAB[JI, 2] := ITMP1;
                        end
                      else
                        begin
                          INFO := MMAX + 1;
                          Exit;
                        end;
                    end;
                end;
            end
          else
            begin

              //
              // IJOB=3: Binary search.  Keep only the interval
              // containing  w  s.t. N(w) = NVAL
              //
              if ITMP1 <= NVAL[JI] then
                begin
                  AB[JI, 1] := TMP1;
                  NAB[JI, 1] := ITMP1;
                end;
              if ITMP1 >= NVAL[JI] then
                begin
                  AB[JI, 2] := TMP1;
                  NAB[JI, 2] := ITMP1;
                end;
            end;
          Inc(JI);
        end;
      KL := KLNEW;

      //
      // Check for convergence
      //
      KFNEW := KF;
      JI := KF;
      while JI <= KL do
        begin
          TMP1 := ABSReal(AB[JI, 2] - AB[JI, 1]);
          TMP2 := Max(ABSReal(AB[JI, 2]), ABSReal(AB[JI, 1]));
          if AP_FP_Less(TMP1, Max(ABSTOL, Max(PIVMIN, RELTOL * TMP2))) or
            (NAB[JI, 1] >= NAB[JI, 2]) then
            begin

              //
              // Converged -- Swap with position KFNEW,
              // then increment KFNEW
              //
              if JI > KFNEW then
                begin
                  TMP1 := AB[JI, 1];
                  TMP2 := AB[JI, 2];
                  ITMP1 := NAB[JI, 1];
                  ITMP2 := NAB[JI, 2];
                  AB[JI, 1] := AB[KFNEW, 1];
                  AB[JI, 2] := AB[KFNEW, 2];
                  NAB[JI, 1] := NAB[KFNEW, 1];
                  NAB[JI, 2] := NAB[KFNEW, 2];
                  AB[KFNEW, 1] := TMP1;
                  AB[KFNEW, 2] := TMP2;
                  NAB[KFNEW, 1] := ITMP1;
                  NAB[KFNEW, 2] := ITMP2;
                  if IJOB = 3 then
                    begin
                      ITMP1 := NVAL[JI];
                      NVAL[JI] := NVAL[KFNEW];
                      NVAL[KFNEW] := ITMP1;
                    end;
                end;
              KFNEW := KFNEW + 1;
            end;
          Inc(JI);
        end;
      KF := KFNEW;

      //
      // Choose Midpoints
      //
      JI := KF;
      while JI <= KL do
        begin
          C[JI] := 0.5 * (AB[JI, 1] + AB[JI, 2]);
          Inc(JI);
        end;

      //
      // If no more intervals to refine, quit.
      //
      if KF > KL then
        begin
          Break;
        end;
      Inc(JIT);
    end;

  //
  // Converged
  //
  INFO := Max(KL + 1 - KF, 0);
  MOUT := KL;
end;

(* ************************************************************************
  Internal subroutine
  ************************************************************************ *)
procedure InternalTREVC(const T: TLearnFloat2DArray; N: TLearnInteger;
  SIDE: TLearnInteger; HOWMNY: TLearnInteger; VSELECT: TLearnBooleanArray;
  var VL: TLearnFloat2DArray; var VR: TLearnFloat2DArray; var M: TLearnInteger;
  var INFO: TLearnInteger);
var
  ALLV    : Boolean;
  BOTHV   : Boolean;
  LEFTV   : Boolean;
  OVER    : Boolean;
  PAIR    : Boolean;
  RIGHTV  : Boolean;
  SOMEV   : Boolean;
  I       : TLearnInteger;
  IERR    : TLearnInteger;
  II      : TLearnInteger;
  IP      : TLearnInteger;
  IIS     : TLearnInteger;
  J       : TLearnInteger;
  J1      : TLearnInteger;
  J2      : TLearnInteger;
  JNXT    : TLearnInteger;
  K       : TLearnInteger;
  KI      : TLearnInteger;
  N2      : TLearnInteger;
  BETA    : TLearnFloat;
  BIGNUM  : TLearnFloat;
  EMAX    : TLearnFloat;
  OVFL    : TLearnFloat;
  REC     : TLearnFloat;
  REMAX   : TLearnFloat;
  SCL     : TLearnFloat;
  SMIN    : TLearnFloat;
  SMLNUM  : TLearnFloat;
  ULP     : TLearnFloat;
  UNFL    : TLearnFloat;
  VCRIT   : TLearnFloat;
  VMAX    : TLearnFloat;
  WI      : TLearnFloat;
  WR      : TLearnFloat;
  XNORM   : TLearnFloat;
  X       : TLearnFloat2DArray;
  WORK    : TLearnFloatArray;
  TEMP    : TLearnFloatArray;
  TEMP11  : TLearnFloat2DArray;
  TEMP22  : TLearnFloat2DArray;
  TEMP11B : TLearnFloat2DArray;
  TEMP21B : TLearnFloat2DArray;
  TEMP12B : TLearnFloat2DArray;
  TEMP22B : TLearnFloat2DArray;
  SkipFlag: Boolean;
  K1      : TLearnInteger;
  K2      : TLearnInteger;
  K3      : TLearnInteger;
  K4      : TLearnInteger;
  VT      : TLearnFloat;
  RSWAP4  : TLearnBooleanArray;
  ZSWAP4  : TLearnBooleanArray;
  IPIVOT44: TLearnInteger2DArray;
  CIV4    : TLearnFloatArray;
  CRV4    : TLearnFloatArray;
  i_      : TLearnInteger;
  i1_     : TLearnInteger;
begin
  VSELECT := DynamicArrayCopy(VSELECT);
  SetLength(X, 2 + 1, 2 + 1);
  SetLength(TEMP11, 1 + 1, 1 + 1);
  SetLength(TEMP11B, 1 + 1, 1 + 1);
  SetLength(TEMP21B, 2 + 1, 1 + 1);
  SetLength(TEMP12B, 1 + 1, 2 + 1);
  SetLength(TEMP22B, 2 + 1, 2 + 1);
  SetLength(TEMP22, 2 + 1, 2 + 1);
  SetLength(WORK, 3 * N + 1);
  SetLength(TEMP, N + 1);
  SetLength(RSWAP4, 4 + 1);
  SetLength(ZSWAP4, 4 + 1);
  SetLength(IPIVOT44, 4 + 1, 4 + 1);
  SetLength(CIV4, 4 + 1);
  SetLength(CRV4, 4 + 1);
  if HOWMNY <> 1 then
    begin
      if (SIDE = 1) or (SIDE = 3) then
        begin
          SetLength(VR, N + 1, N + 1);
        end;
      if (SIDE = 2) or (SIDE = 3) then
        begin
          SetLength(VL, N + 1, N + 1);
        end;
    end;

  //
  // Decode and test the input parameters
  //
  BOTHV := SIDE = 3;
  RIGHTV := (SIDE = 1) or BOTHV;
  LEFTV := (SIDE = 2) or BOTHV;
  ALLV := HOWMNY = 2;
  OVER := HOWMNY = 1;
  SOMEV := HOWMNY = 3;
  INFO := 0;
  if N < 0 then
    begin
      INFO := -2;
      Exit;
    end;
  if not RIGHTV and not LEFTV then
    begin
      INFO := -3;
      Exit;
    end;
  if not ALLV and not OVER and not SOMEV then
    begin
      INFO := -4;
      Exit;
    end;

  //
  // Set M to the number of columns required to store the selected
  // eigenvectors, standardize the array SELECT if necessary, and
  // test MM.
  //
  if SOMEV then
    begin
      M := 0;
      PAIR := False;
      J := 1;
      while J <= N do
        begin
          if PAIR then
            begin
              PAIR := False;
              VSELECT[J] := False;
            end
          else
            begin
              if J < N then
                begin
                  if AP_FP_Eq(T[J + 1, J], 0) then
                    begin
                      if VSELECT[J] then
                        begin
                          M := M + 1;
                        end;
                    end
                  else
                    begin
                      PAIR := True;
                      if VSELECT[J] or VSELECT[J + 1] then
                        begin
                          VSELECT[J] := True;
                          M := M + 2;
                        end;
                    end;
                end
              else
                begin
                  if VSELECT[N] then
                    begin
                      M := M + 1;
                    end;
                end;
            end;
          Inc(J);
        end;
    end
  else
    begin
      M := N;
    end;

  //
  // Quick return if possible.
  //
  if N = 0 then
    begin
      Exit;
    end;

  //
  // Set the constants to control overflow.
  //
  UNFL := MinRealNumber;
  OVFL := 1 / UNFL;
  ULP := MachineEpsilon;
  SMLNUM := UNFL * (N / ULP);
  BIGNUM := (1 - ULP) / SMLNUM;

  //
  // Compute 1-norm of each column of strictly upper triangular
  // part of T to control overflow in triangular solver.
  //
  WORK[1] := 0;
  J := 2;
  while J <= N do
    begin
      WORK[J] := 0;
      I := 1;
      while I <= J - 1 do
        begin
          WORK[J] := WORK[J] + ABSReal(T[I, J]);
          Inc(I);
        end;
      Inc(J);
    end;

  //
  // Index IP is used to specify the real or complex eigenvalue:
  // IP = 0, real eigenvalue,
  // 1, first of conjugate complex pair: (wr,wi)
  // -1, second of conjugate complex pair: (wr,wi)
  //
  N2 := 2 * N;
  if RIGHTV then
    begin

      //
      // Compute right eigenvectors.
      //
      IP := 0;
      IIS := M;
      KI := N;
      while KI >= 1 do
        begin
          SkipFlag := False;
          if IP = 1 then
            begin
              SkipFlag := True;
            end
          else
            begin
              if KI <> 1 then
                begin
                  if AP_FP_Neq(T[KI, KI - 1], 0) then
                    begin
                      IP := -1;
                    end;
                end;
              if SOMEV then
                begin
                  if IP = 0 then
                    begin
                      if not VSELECT[KI] then
                        begin
                          SkipFlag := True;
                        end;
                    end
                  else
                    begin
                      if not VSELECT[KI - 1] then
                        begin
                          SkipFlag := True;
                        end;
                    end;
                end;
            end;
          if not SkipFlag then
            begin

              //
              // Compute the KI-th eigenvalue (WR,WI).
              //
              WR := T[KI, KI];
              WI := 0;
              if IP <> 0 then
                begin
                  WI := SQRT(ABSReal(T[KI, KI - 1])) * SQRT(ABSReal(T[KI - 1, KI]));
                end;
              SMIN := Max(ULP * (ABSReal(WR) + ABSReal(WI)), SMLNUM);
              if IP = 0 then
                begin

                  //
                  // Real right eigenvector
                  //
                  WORK[KI + N] := 1;

                  //
                  // Form right-hand side
                  //
                  K := 1;
                  while K <= KI - 1 do
                    begin
                      WORK[K + N] := -T[K, KI];
                      Inc(K);
                    end;

                  //
                  // Solve the upper quasi-triangular system:
                  // (T(1:KI-1,1:KI-1) - WR)*X = SCALE*WORK.
                  //
                  JNXT := KI - 1;
                  J := KI - 1;
                  while J >= 1 do
                    begin
                      if J > JNXT then
                        begin
                          Dec(J);
                          Continue;
                        end;
                      J1 := J;
                      J2 := J;
                      JNXT := J - 1;
                      if J > 1 then
                        begin
                          if AP_FP_Neq(T[J, J - 1], 0) then
                            begin
                              J1 := J - 1;
                              JNXT := J - 2;
                            end;
                        end;
                      if J1 = J2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          TEMP11[1, 1] := T[J, J];
                          TEMP11B[1, 1] := WORK[J + N];
                          InternalHSEVDLALN2(False, 1, 1, SMIN, 1, TEMP11, 1.0, 1.0,
                            TEMP11B, WR, 0.0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale X(1,1) to avoid overflow when updating
                          // the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              if AP_FP_Greater(WORK[J], BIGNUM / XNORM) then
                                begin
                                  X[1, 1] := X[1, 1] / XNORM;
                                  SCL := SCL / XNORM;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              K1 := N + 1;
                              K2 := N + KI;
                              APVMul(@WORK[0], K1, K2, SCL);
                            end;
                          WORK[J + N] := X[1, 1];

                          //
                          // Update right-hand side
                          //
                          K1 := 1 + N;
                          K2 := J - 1 + N;
                          K3 := J - 1;
                          VT := -X[1, 1];
                          i1_ := (1) - (K1);
                          for i_ := K1 to K2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J];
                            end;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          TEMP22[1, 1] := T[J - 1, J - 1];
                          TEMP22[1, 2] := T[J - 1, J];
                          TEMP22[2, 1] := T[J, J - 1];
                          TEMP22[2, 2] := T[J, J];
                          TEMP21B[1, 1] := WORK[J - 1 + N];
                          TEMP21B[2, 1] := WORK[J + N];
                          InternalHSEVDLALN2(False, 2, 1, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP21B, WR, 0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale X(1,1) and X(2,1) to avoid overflow when
                          // updating the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              BETA := Max(WORK[J - 1], WORK[J]);
                              if AP_FP_Greater(BETA, BIGNUM / XNORM) then
                                begin
                                  X[1, 1] := X[1, 1] / XNORM;
                                  X[2, 1] := X[2, 1] / XNORM;
                                  SCL := SCL / XNORM;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              K1 := 1 + N;
                              K2 := KI + N;
                              APVMul(@WORK[0], K1, K2, SCL);
                            end;
                          WORK[J - 1 + N] := X[1, 1];
                          WORK[J + N] := X[2, 1];

                          //
                          // Update right-hand side
                          //
                          K1 := 1 + N;
                          K2 := J - 2 + N;
                          K3 := J - 2;
                          K4 := J - 1;
                          VT := -X[1, 1];
                          i1_ := (1) - (K1);
                          for i_ := K1 to K2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, K4];
                            end;
                          VT := -X[2, 1];
                          i1_ := (1) - (K1);
                          for i_ := K1 to K2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J];
                            end;
                        end;
                      Dec(J);
                    end;

                  //
                  // Copy the vector x or Q*x to VR and normalize.
                  //
                  if not OVER then
                    begin
                      K1 := 1 + N;
                      K2 := KI + N;
                      i1_ := (K1) - (1);
                      for i_ := 1 to KI do
                        begin
                          VR[i_, IIS] := WORK[i_ + i1_];
                        end;
                      II := ColumnIdxAbsMax(VR, 1, KI, IIS);
                      REMAX := 1 / ABSReal(VR[II, IIS]);
                      for i_ := 1 to KI do
                        begin
                          VR[i_, IIS] := REMAX * VR[i_, IIS];
                        end;
                      K := KI + 1;
                      while K <= N do
                        begin
                          VR[K, IIS] := 0;
                          Inc(K);
                        end;
                    end
                  else
                    begin
                      if KI > 1 then
                        begin
                          for i_ := 1 to N do
                            begin
                              TEMP[i_] := VR[i_, KI];
                            end;
                          MatrixVectorMultiply(VR, 1, N, 1, KI - 1, False, WORK, 1 + N,
                            KI - 1 + N, 1.0, TEMP, 1, N, WORK[KI + N]);
                          for i_ := 1 to N do
                            begin
                              VR[i_, KI] := TEMP[i_];
                            end;
                        end;
                      II := ColumnIdxAbsMax(VR, 1, N, KI);
                      REMAX := 1 / ABSReal(VR[II, KI]);
                      for i_ := 1 to N do
                        begin
                          VR[i_, KI] := REMAX * VR[i_, KI];
                        end;
                    end;
                end
              else
                begin

                  //
                  // Complex right eigenvector.
                  //
                  // Initial solve
                  // [ (T(KI-1,KI-1) T(KI-1,KI) ) - (WR + I* WI)]*X = 0.
                  // [ (T(KI,KI-1)   T(KI,KI)   )               ]
                  //
                  if AP_FP_Greater_Eq(ABSReal(T[KI - 1, KI]), ABSReal(T[KI, KI - 1]))
                  then
                    begin
                      WORK[KI - 1 + N] := 1;
                      WORK[KI + N2] := WI / T[KI - 1, KI];
                    end
                  else
                    begin
                      WORK[KI - 1 + N] := -WI / T[KI, KI - 1];
                      WORK[KI + N2] := 1;
                    end;
                  WORK[KI + N] := 0;
                  WORK[KI - 1 + N2] := 0;

                  //
                  // Form right-hand side
                  //
                  K := 1;
                  while K <= KI - 2 do
                    begin
                      WORK[K + N] := -WORK[KI - 1 + N] * T[K, KI - 1];
                      WORK[K + N2] := -WORK[KI + N2] * T[K, KI];
                      Inc(K);
                    end;

                  //
                  // Solve upper quasi-triangular system:
                  // (T(1:KI-2,1:KI-2) - (WR+i*WI))*X = SCALE*(WORK+i*WORK2)
                  //
                  JNXT := KI - 2;
                  J := KI - 2;
                  while J >= 1 do
                    begin
                      if J > JNXT then
                        begin
                          Dec(J);
                          Continue;
                        end;
                      J1 := J;
                      J2 := J;
                      JNXT := J - 1;
                      if J > 1 then
                        begin
                          if AP_FP_Neq(T[J, J - 1], 0) then
                            begin
                              J1 := J - 1;
                              JNXT := J - 2;
                            end;
                        end;
                      if J1 = J2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          TEMP11[1, 1] := T[J, J];
                          TEMP12B[1, 1] := WORK[J + N];
                          TEMP12B[1, 2] := WORK[J + N + N];
                          InternalHSEVDLALN2(False, 1, 2, SMIN, 1.0, TEMP11, 1.0, 1.0,
                            TEMP12B, WR, WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale X(1,1) and X(1,2) to avoid overflow when
                          // updating the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              if AP_FP_Greater(WORK[J], BIGNUM / XNORM) then
                                begin
                                  X[1, 1] := X[1, 1] / XNORM;
                                  X[1, 2] := X[1, 2] / XNORM;
                                  SCL := SCL / XNORM;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              K1 := 1 + N;
                              K2 := KI + N;
                              APVMul(@WORK[0], K1, K2, SCL);
                              K1 := 1 + N2;
                              K2 := KI + N2;
                              APVMul(@WORK[0], K1, K2, SCL);
                            end;
                          WORK[J + N] := X[1, 1];
                          WORK[J + N2] := X[1, 2];

                          //
                          // Update the right-hand side
                          //
                          K1 := 1 + N;
                          K2 := J - 1 + N;
                          K3 := 1;
                          K4 := J - 1;
                          VT := -X[1, 1];
                          i1_ := (K3) - (K1);
                          for i_ := K1 to K2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J];
                            end;
                          K1 := 1 + N2;
                          K2 := J - 1 + N2;
                          K3 := 1;
                          K4 := J - 1;
                          VT := -X[1, 2];
                          i1_ := (K3) - (K1);
                          for i_ := K1 to K2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J];
                            end;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          TEMP22[1, 1] := T[J - 1, J - 1];
                          TEMP22[1, 2] := T[J - 1, J];
                          TEMP22[2, 1] := T[J, J - 1];
                          TEMP22[2, 2] := T[J, J];
                          TEMP22B[1, 1] := WORK[J - 1 + N];
                          TEMP22B[1, 2] := WORK[J - 1 + N + N];
                          TEMP22B[2, 1] := WORK[J + N];
                          TEMP22B[2, 2] := WORK[J + N + N];
                          InternalHSEVDLALN2(False, 2, 2, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP22B, WR, WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale X to avoid overflow when updating
                          // the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              BETA := Max(WORK[J - 1], WORK[J]);
                              if AP_FP_Greater(BETA, BIGNUM / XNORM) then
                                begin
                                  REC := 1 / XNORM;
                                  X[1, 1] := X[1, 1] * REC;
                                  X[1, 2] := X[1, 2] * REC;
                                  X[2, 1] := X[2, 1] * REC;
                                  X[2, 2] := X[2, 2] * REC;
                                  SCL := SCL * REC;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              APVMul(@WORK[0], 1 + N, KI + N, SCL);
                              APVMul(@WORK[0], 1 + N2, KI + N2, SCL);
                            end;
                          WORK[J - 1 + N] := X[1, 1];
                          WORK[J + N] := X[2, 1];
                          WORK[J - 1 + N2] := X[1, 2];
                          WORK[J + N2] := X[2, 2];

                          //
                          // Update the right-hand side
                          //
                          VT := -X[1, 1];
                          i1_ := (1) - (N + 1);
                          for i_ := N + 1 to N + J - 2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J - 1];
                            end;
                          VT := -X[2, 1];
                          i1_ := (1) - (N + 1);
                          for i_ := N + 1 to N + J - 2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J];
                            end;
                          VT := -X[1, 2];
                          i1_ := (1) - (N2 + 1);
                          for i_ := N2 + 1 to N2 + J - 2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J - 1];
                            end;
                          VT := -X[2, 2];
                          i1_ := (1) - (N2 + 1);
                          for i_ := N2 + 1 to N2 + J - 2 do
                            begin
                              WORK[i_] := WORK[i_] + VT * T[i_ + i1_, J];
                            end;
                        end;
                      Dec(J);
                    end;

                  //
                  // Copy the vector x or Q*x to VR and normalize.
                  //
                  if not OVER then
                    begin
                      i1_ := (N + 1) - (1);
                      for i_ := 1 to KI do
                        begin
                          VR[i_, IIS - 1] := WORK[i_ + i1_];
                        end;
                      i1_ := (N2 + 1) - (1);
                      for i_ := 1 to KI do
                        begin
                          VR[i_, IIS] := WORK[i_ + i1_];
                        end;
                      EMAX := 0;
                      K := 1;
                      while K <= KI do
                        begin
                          EMAX := Max(EMAX, ABSReal(VR[K, IIS - 1]) + ABSReal(VR[K, IIS]));
                          Inc(K);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := 1 to KI do
                        begin
                          VR[i_, IIS - 1] := REMAX * VR[i_, IIS - 1];
                        end;
                      for i_ := 1 to KI do
                        begin
                          VR[i_, IIS] := REMAX * VR[i_, IIS];
                        end;
                      K := KI + 1;
                      while K <= N do
                        begin
                          VR[K, IIS - 1] := 0;
                          VR[K, IIS] := 0;
                          Inc(K);
                        end;
                    end
                  else
                    begin
                      if KI > 2 then
                        begin
                          for i_ := 1 to N do
                            begin
                              TEMP[i_] := VR[i_, KI - 1];
                            end;
                          MatrixVectorMultiply(VR, 1, N, 1, KI - 2, False, WORK, 1 + N,
                            KI - 2 + N, 1.0, TEMP, 1, N, WORK[KI - 1 + N]);
                          for i_ := 1 to N do
                            begin
                              VR[i_, KI - 1] := TEMP[i_];
                            end;
                          for i_ := 1 to N do
                            begin
                              TEMP[i_] := VR[i_, KI];
                            end;
                          MatrixVectorMultiply(VR, 1, N, 1, KI - 2, False, WORK, 1 + N2,
                            KI - 2 + N2, 1.0, TEMP, 1, N, WORK[KI + N2]);
                          for i_ := 1 to N do
                            begin
                              VR[i_, KI] := TEMP[i_];
                            end;
                        end
                      else
                        begin
                          VT := WORK[KI - 1 + N];
                          for i_ := 1 to N do
                            begin
                              VR[i_, KI - 1] := VT * VR[i_, KI - 1];
                            end;
                          VT := WORK[KI + N2];
                          for i_ := 1 to N do
                            begin
                              VR[i_, KI] := VT * VR[i_, KI];
                            end;
                        end;
                      EMAX := 0;
                      K := 1;
                      while K <= N do
                        begin
                          EMAX := Max(EMAX, ABSReal(VR[K, KI - 1]) + ABSReal(VR[K, KI]));
                          Inc(K);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := 1 to N do
                        begin
                          VR[i_, KI - 1] := REMAX * VR[i_, KI - 1];
                        end;
                      for i_ := 1 to N do
                        begin
                          VR[i_, KI] := REMAX * VR[i_, KI];
                        end;
                    end;
                end;
              IIS := IIS - 1;
              if IP <> 0 then
                begin
                  IIS := IIS - 1;
                end;
            end;
          if IP = 1 then
            begin
              IP := 0;
            end;
          if IP = -1 then
            begin
              IP := 1;
            end;
          Dec(KI);
        end;
    end;
  if LEFTV then
    begin

      //
      // Compute left eigenvectors.
      //
      IP := 0;
      IIS := 1;
      KI := 1;
      while KI <= N do
        begin
          SkipFlag := False;
          if IP = -1 then
            begin
              SkipFlag := True;
            end
          else
            begin
              if KI <> N then
                begin
                  if AP_FP_Neq(T[KI + 1, KI], 0) then
                    begin
                      IP := 1;
                    end;
                end;
              if SOMEV then
                begin
                  if not VSELECT[KI] then
                    begin
                      SkipFlag := True;
                    end;
                end;
            end;
          if not SkipFlag then
            begin

              //
              // Compute the KI-th eigenvalue (WR,WI).
              //
              WR := T[KI, KI];
              WI := 0;
              if IP <> 0 then
                begin
                  WI := SQRT(ABSReal(T[KI, KI + 1])) * SQRT(ABSReal(T[KI + 1, KI]));
                end;
              SMIN := Max(ULP * (ABSReal(WR) + ABSReal(WI)), SMLNUM);
              if IP = 0 then
                begin

                  //
                  // Real left eigenvector.
                  //
                  WORK[KI + N] := 1;

                  //
                  // Form right-hand side
                  //
                  K := KI + 1;
                  while K <= N do
                    begin
                      WORK[K + N] := -T[KI, K];
                      Inc(K);
                    end;

                  //
                  // Solve the quasi-triangular system:
                  // (T(KI+1:N,KI+1:N) - WR)'*X = SCALE*WORK
                  //
                  VMAX := 1;
                  VCRIT := BIGNUM;
                  JNXT := KI + 1;
                  J := KI + 1;
                  while J <= N do
                    begin
                      if J < JNXT then
                        begin
                          Inc(J);
                          Continue;
                        end;
                      J1 := J;
                      J2 := J;
                      JNXT := J + 1;
                      if J < N then
                        begin
                          if AP_FP_Neq(T[J + 1, J], 0) then
                            begin
                              J2 := J + 1;
                              JNXT := J + 2;
                            end;
                        end;
                      if J1 = J2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          // Scale if necessary to avoid overflow when forming
                          // the right-hand side.
                          //
                          if AP_FP_Greater(WORK[J], VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@WORK[0], KI + N, N + N, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 1 + N) - (KI + 1);
                          VT := 0.0;
                          for i_ := KI + 1 to J - 1 do
                            begin
                              VT := VT + T[i_, J] * WORK[i_ + i1_];
                            end;
                          WORK[J + N] := WORK[J + N] - VT;

                          //
                          // Solve (T(J,J)-WR)'*X = WORK
                          //
                          TEMP11[1, 1] := T[J, J];
                          TEMP11B[1, 1] := WORK[J + N];
                          InternalHSEVDLALN2(False, 1, 1, SMIN, 1.0, TEMP11, 1.0, 1.0,
                            TEMP11B, WR, 0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              APVMul(@WORK[0], KI + N, N + N, SCL);
                            end;
                          WORK[J + N] := X[1, 1];
                          VMAX := Max(ABSReal(WORK[J + N]), VMAX);
                          VCRIT := BIGNUM / VMAX;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          // Scale if necessary to avoid overflow when forming
                          // the right-hand side.
                          //
                          BETA := Max(WORK[J], WORK[J + 1]);
                          if AP_FP_Greater(BETA, VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@WORK[0], KI + N, N + N, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 1 + N) - (KI + 1);
                          VT := 0.0;
                          for i_ := KI + 1 to J - 1 do
                            begin
                              VT := VT + T[i_, J] * WORK[i_ + i1_];
                            end;
                          WORK[J + N] := WORK[J + N] - VT;
                          i1_ := (KI + 1 + N) - (KI + 1);
                          VT := 0.0;
                          for i_ := KI + 1 to J - 1 do
                            begin
                              VT := VT + T[i_, J + 1] * WORK[i_ + i1_];
                            end;
                          WORK[J + 1 + N] := WORK[J + 1 + N] - VT;

                          //
                          // Solve
                          // [T(J,J)-WR   T(J,J+1)     ]'* X = SCALE*( WORK1 )
                          // [T(J+1,J)    T(J+1,J+1)-WR]             ( WORK2 )
                          //
                          TEMP22[1, 1] := T[J, J];
                          TEMP22[1, 2] := T[J, J + 1];
                          TEMP22[2, 1] := T[J + 1, J];
                          TEMP22[2, 2] := T[J + 1, J + 1];
                          TEMP21B[1, 1] := WORK[J + N];
                          TEMP21B[2, 1] := WORK[J + 1 + N];
                          InternalHSEVDLALN2(True, 2, 1, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP21B, WR, 0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              APVMul(@WORK[0], KI + N, N + N, SCL);
                            end;
                          WORK[J + N] := X[1, 1];
                          WORK[J + 1 + N] := X[2, 1];
                          VMAX := Max(ABSReal(WORK[J + N]),
                            Max(ABSReal(WORK[J + 1 + N]), VMAX));
                          VCRIT := BIGNUM / VMAX;
                        end;
                      Inc(J);
                    end;

                  //
                  // Copy the vector x or Q*x to VL and normalize.
                  //
                  if not OVER then
                    begin
                      i1_ := (KI + N) - (KI);
                      for i_ := KI to N do
                        begin
                          VL[i_, IIS] := WORK[i_ + i1_];
                        end;
                      II := ColumnIdxAbsMax(VL, KI, N, IIS);
                      REMAX := 1 / ABSReal(VL[II, IIS]);
                      for i_ := KI to N do
                        begin
                          VL[i_, IIS] := REMAX * VL[i_, IIS];
                        end;
                      K := 1;
                      while K <= KI - 1 do
                        begin
                          VL[K, IIS] := 0;
                          Inc(K);
                        end;
                    end
                  else
                    begin
                      if KI < N then
                        begin
                          for i_ := 1 to N do
                            begin
                              TEMP[i_] := VL[i_, KI];
                            end;
                          MatrixVectorMultiply(VL, 1, N, KI + 1, N, False, WORK, KI + 1 + N,
                            N + N, 1.0, TEMP, 1, N, WORK[KI + N]);
                          for i_ := 1 to N do
                            begin
                              VL[i_, KI] := TEMP[i_];
                            end;
                        end;
                      II := ColumnIdxAbsMax(VL, 1, N, KI);
                      REMAX := 1 / ABSReal(VL[II, KI]);
                      for i_ := 1 to N do
                        begin
                          VL[i_, KI] := REMAX * VL[i_, KI];
                        end;
                    end;
                end
              else
                begin

                  //
                  // Complex left eigenvector.
                  //
                  // Initial solve:
                  // ((T(KI,KI)    T(KI,KI+1) )' - (WR - I* WI))*X = 0.
                  // ((T(KI+1,KI) T(KI+1,KI+1))                )
                  //
                  if AP_FP_Greater_Eq(ABSReal(T[KI, KI + 1]), ABSReal(T[KI + 1, KI]))
                  then
                    begin
                      WORK[KI + N] := WI / T[KI, KI + 1];
                      WORK[KI + 1 + N2] := 1;
                    end
                  else
                    begin
                      WORK[KI + N] := 1;
                      WORK[KI + 1 + N2] := -WI / T[KI + 1, KI];
                    end;
                  WORK[KI + 1 + N] := 0;
                  WORK[KI + N2] := 0;

                  //
                  // Form right-hand side
                  //
                  K := KI + 2;
                  while K <= N do
                    begin
                      WORK[K + N] := -WORK[KI + N] * T[KI, K];
                      WORK[K + N2] := -WORK[KI + 1 + N2] * T[KI + 1, K];
                      Inc(K);
                    end;

                  //
                  // Solve complex quasi-triangular system:
                  // ( T(KI+2,N:KI+2,N) - (WR-i*WI) )*X = WORK1+i*WORK2
                  //
                  VMAX := 1;
                  VCRIT := BIGNUM;
                  JNXT := KI + 2;
                  J := KI + 2;
                  while J <= N do
                    begin
                      if J < JNXT then
                        begin
                          Inc(J);
                          Continue;
                        end;
                      J1 := J;
                      J2 := J;
                      JNXT := J + 1;
                      if J < N then
                        begin
                          if AP_FP_Neq(T[J + 1, J], 0) then
                            begin
                              J2 := J + 1;
                              JNXT := J + 2;
                            end;
                        end;
                      if J1 = J2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          // Scale if necessary to avoid overflow when
                          // forming the right-hand side elements.
                          //
                          if AP_FP_Greater(WORK[J], VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@WORK[0], KI + N, N + N, REC);
                              APVMul(@WORK[0], KI + N2, N + N2, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 2 + N) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to J - 1 do
                            begin
                              VT := VT + T[i_, J] * WORK[i_ + i1_];
                            end;
                          WORK[J + N] := WORK[J + N] - VT;
                          i1_ := (KI + 2 + N2) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to J - 1 do
                            begin
                              VT := VT + T[i_, J] * WORK[i_ + i1_];
                            end;
                          WORK[J + N2] := WORK[J + N2] - VT;

                          //
                          // Solve (T(J,J)-(WR-i*WI))*(X11+i*X12)= WK+I*WK2
                          //
                          TEMP11[1, 1] := T[J, J];
                          TEMP12B[1, 1] := WORK[J + N];
                          TEMP12B[1, 2] := WORK[J + N + N];
                          InternalHSEVDLALN2(False, 1, 2, SMIN, 1.0, TEMP11, 1.0, 1.0,
                            TEMP12B, WR, -WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              APVMul(@WORK[0], KI + N, N + N, SCL);
                              APVMul(@WORK[0], KI + N2, N + N2, SCL);
                            end;
                          WORK[J + N] := X[1, 1];
                          WORK[J + N2] := X[1, 2];
                          VMAX := Max(ABSReal(WORK[J + N]),
                            Max(ABSReal(WORK[J + N2]), VMAX));
                          VCRIT := BIGNUM / VMAX;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          // Scale if necessary to avoid overflow when forming
                          // the right-hand side elements.
                          //
                          BETA := Max(WORK[J], WORK[J + 1]);
                          if AP_FP_Greater(BETA, VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@WORK[0], KI + N, N + N, REC);
                              APVMul(@WORK[0], KI + N2, N + N2, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 2 + N) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to J - 1 do
                            begin
                              VT := VT + T[i_, J] * WORK[i_ + i1_];
                            end;
                          WORK[J + N] := WORK[J + N] - VT;
                          i1_ := (KI + 2 + N2) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to J - 1 do
                            begin
                              VT := VT + T[i_, J] * WORK[i_ + i1_];
                            end;
                          WORK[J + N2] := WORK[J + N2] - VT;
                          i1_ := (KI + 2 + N) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to J - 1 do
                            begin
                              VT := VT + T[i_, J + 1] * WORK[i_ + i1_];
                            end;
                          WORK[J + 1 + N] := WORK[J + 1 + N] - VT;
                          i1_ := (KI + 2 + N2) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to J - 1 do
                            begin
                              VT := VT + T[i_, J + 1] * WORK[i_ + i1_];
                            end;
                          WORK[J + 1 + N2] := WORK[J + 1 + N2] - VT;

                          //
                          // Solve 2-by-2 complex linear equation
                          // ([T(j,j)   T(j,j+1)  ]'-(wr-i*wi)*I)*X = SCALE*B
                          // ([T(j+1,j) T(j+1,j+1)]             )
                          //
                          TEMP22[1, 1] := T[J, J];
                          TEMP22[1, 2] := T[J, J + 1];
                          TEMP22[2, 1] := T[J + 1, J];
                          TEMP22[2, 2] := T[J + 1, J + 1];
                          TEMP22B[1, 1] := WORK[J + N];
                          TEMP22B[1, 2] := WORK[J + N + N];
                          TEMP22B[2, 1] := WORK[J + 1 + N];
                          TEMP22B[2, 2] := WORK[J + 1 + N + N];
                          InternalHSEVDLALN2(True, 2, 2, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP22B, WR, -WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, X, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_Neq(SCL, 1) then
                            begin
                              APVMul(@WORK[0], KI + N, N + N, SCL);
                              APVMul(@WORK[0], KI + N2, N + N2, SCL);
                            end;
                          WORK[J + N] := X[1, 1];
                          WORK[J + N2] := X[1, 2];
                          WORK[J + 1 + N] := X[2, 1];
                          WORK[J + 1 + N2] := X[2, 2];
                          VMAX := Max(ABSReal(X[1, 1]), VMAX);
                          VMAX := Max(ABSReal(X[1, 2]), VMAX);
                          VMAX := Max(ABSReal(X[2, 1]), VMAX);
                          VMAX := Max(ABSReal(X[2, 2]), VMAX);
                          VCRIT := BIGNUM / VMAX;
                        end;
                      Inc(J);
                    end;

                  //
                  // Copy the vector x or Q*x to VL and normalize.
                  //
                  if not OVER then
                    begin
                      i1_ := (KI + N) - (KI);
                      for i_ := KI to N do
                        begin
                          VL[i_, IIS] := WORK[i_ + i1_];
                        end;
                      i1_ := (KI + N2) - (KI);
                      for i_ := KI to N do
                        begin
                          VL[i_, IIS + 1] := WORK[i_ + i1_];
                        end;
                      EMAX := 0;
                      K := KI;
                      while K <= N do
                        begin
                          EMAX := Max(EMAX, ABSReal(VL[K, IIS]) + ABSReal(VL[K, IIS + 1]));
                          Inc(K);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := KI to N do
                        begin
                          VL[i_, IIS] := REMAX * VL[i_, IIS];
                        end;
                      for i_ := KI to N do
                        begin
                          VL[i_, IIS + 1] := REMAX * VL[i_, IIS + 1];
                        end;
                      K := 1;
                      while K <= KI - 1 do
                        begin
                          VL[K, IIS] := 0;
                          VL[K, IIS + 1] := 0;
                          Inc(K);
                        end;
                    end
                  else
                    begin
                      if KI < N - 1 then
                        begin
                          for i_ := 1 to N do
                            begin
                              TEMP[i_] := VL[i_, KI];
                            end;
                          MatrixVectorMultiply(VL, 1, N, KI + 2, N, False, WORK, KI + 2 + N,
                            N + N, 1.0, TEMP, 1, N, WORK[KI + N]);
                          for i_ := 1 to N do
                            begin
                              VL[i_, KI] := TEMP[i_];
                            end;
                          for i_ := 1 to N do
                            begin
                              TEMP[i_] := VL[i_, KI + 1];
                            end;
                          MatrixVectorMultiply(VL, 1, N, KI + 2, N, False, WORK,
                            KI + 2 + N2, N + N2, 1.0, TEMP, 1, N, WORK[KI + 1 + N2]);
                          for i_ := 1 to N do
                            begin
                              VL[i_, KI + 1] := TEMP[i_];
                            end;
                        end
                      else
                        begin
                          VT := WORK[KI + N];
                          for i_ := 1 to N do
                            begin
                              VL[i_, KI] := VT * VL[i_, KI];
                            end;
                          VT := WORK[KI + 1 + N2];
                          for i_ := 1 to N do
                            begin
                              VL[i_, KI + 1] := VT * VL[i_, KI + 1];
                            end;
                        end;
                      EMAX := 0;
                      K := 1;
                      while K <= N do
                        begin
                          EMAX := Max(EMAX, ABSReal(VL[K, KI]) + ABSReal(VL[K, KI + 1]));
                          Inc(K);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := 1 to N do
                        begin
                          VL[i_, KI] := REMAX * VL[i_, KI];
                        end;
                      for i_ := 1 to N do
                        begin
                          VL[i_, KI + 1] := REMAX * VL[i_, KI + 1];
                        end;
                    end;
                end;
              IIS := IIS + 1;
              if IP <> 0 then
                begin
                  IIS := IIS + 1;
                end;
            end;
          if IP = -1 then
            begin
              IP := 0;
            end;
          if IP = 1 then
            begin
              IP := -1;
            end;
          Inc(KI);
        end;
    end;
end;

(* ************************************************************************
  DLALN2 solves a system of the form  (ca A - w D ) X = s B
  or (ca A' - w D) X = s B   with possible scaling ("s") and
  perturbation of A.  (A' means A-transpose.)

  A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
  real diagonal matrix, w is a real or complex value, and X and B are
  NA x 1 matrices -- real if w is real, complex if w is complex.  NA
  may be 1 or 2.

  If w is complex, X and B are represented as NA x 2 matrices,
  the first column of each being the real part and the second
  being the imaginary part.

  "s" is a scaling factor (.LE. 1), computed by DLALN2, which is
  so chosen that X can be computed without overflow.  X is further
  scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
  than overflow.

  If both singular values of (ca A - w D) are less than SMIN,
  SMIN*identity will be used instead of (ca A - w D).  If only one
  singular value is less than SMIN, one element of (ca A - w D) will be
  perturbed enough to make the smallest singular value roughly SMIN.
  If both singular values are at least SMIN, (ca A - w D) will not be
  perturbed.  In any case, the perturbation will be at most some small
  multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
  are computed by infinity-norm approximations, and thus will only be
  correct to a factor of 2 or so.

  Note: all input quantities are assumed to be smaller than overflow
  by a reasonable factor.  (See BIGNUM.)
  ************************************************************************ *)
procedure InternalHSEVDLALN2(const LTRANS: Boolean; const NA: TLearnInteger;
  const NW: TLearnInteger; const SMIN: TLearnFloat; const CA: TLearnFloat;
  const A: TLearnFloat2DArray; const D1: TLearnFloat; const D2: TLearnFloat;
  const B: TLearnFloat2DArray; const WR: TLearnFloat; const WI: TLearnFloat;
  var RSWAP4: TLearnBooleanArray; var ZSWAP4: TLearnBooleanArray;
  var IPIVOT44: TLearnInteger2DArray; var CIV4: TLearnFloatArray; var CRV4: TLearnFloatArray;
  var X: TLearnFloat2DArray; var SCL: TLearnFloat; var XNORM: TLearnFloat;
  var INFO: TLearnInteger);
var
  ICMAX : TLearnInteger;
  J     : TLearnInteger;
  BBND  : TLearnFloat;
  BI1   : TLearnFloat;
  BI2   : TLearnFloat;
  BIGNUM: TLearnFloat;
  BNORM : TLearnFloat;
  BR1   : TLearnFloat;
  BR2   : TLearnFloat;
  CI21  : TLearnFloat;
  CI22  : TLearnFloat;
  CMAX  : TLearnFloat;
  CNORM : TLearnFloat;
  CR21  : TLearnFloat;
  CR22  : TLearnFloat;
  CSI   : TLearnFloat;
  CSR   : TLearnFloat;
  LI21  : TLearnFloat;
  LR21  : TLearnFloat;
  SMINI : TLearnFloat;
  SMLNUM: TLearnFloat;
  TEMP  : TLearnFloat;
  U22ABS: TLearnFloat;
  UI11  : TLearnFloat;
  UI11R : TLearnFloat;
  UI12  : TLearnFloat;
  UI12S : TLearnFloat;
  UI22  : TLearnFloat;
  UR11  : TLearnFloat;
  UR11R : TLearnFloat;
  UR12  : TLearnFloat;
  UR12S : TLearnFloat;
  UR22  : TLearnFloat;
  XI1   : TLearnFloat;
  XI2   : TLearnFloat;
  XR1   : TLearnFloat;
  XR2   : TLearnFloat;
  TMP1  : TLearnFloat;
  TMP2  : TLearnFloat;
begin
  ZSWAP4[1] := False;
  ZSWAP4[2] := False;
  ZSWAP4[3] := True;
  ZSWAP4[4] := True;
  RSWAP4[1] := False;
  RSWAP4[2] := True;
  RSWAP4[3] := False;
  RSWAP4[4] := True;
  IPIVOT44[1, 1] := 1;
  IPIVOT44[2, 1] := 2;
  IPIVOT44[3, 1] := 3;
  IPIVOT44[4, 1] := 4;
  IPIVOT44[1, 2] := 2;
  IPIVOT44[2, 2] := 1;
  IPIVOT44[3, 2] := 4;
  IPIVOT44[4, 2] := 3;
  IPIVOT44[1, 3] := 3;
  IPIVOT44[2, 3] := 4;
  IPIVOT44[3, 3] := 1;
  IPIVOT44[4, 3] := 2;
  IPIVOT44[1, 4] := 4;
  IPIVOT44[2, 4] := 3;
  IPIVOT44[3, 4] := 2;
  IPIVOT44[4, 4] := 1;
  SMLNUM := 2 * MinRealNumber;
  BIGNUM := 1 / SMLNUM;
  SMINI := Max(SMIN, SMLNUM);

  //
  // Don't check for input errors
  //
  INFO := 0;

  //
  // Standard Initializations
  //
  SCL := 1;
  if NA = 1 then
    begin

      //
      // 1 x 1  (i.e., scalar) system   C X = B
      //
      if NW = 1 then
        begin

          //
          // Real 1x1 system.
          //
          // C = ca A - w D
          //
          CSR := CA * A[1, 1] - WR * D1;
          CNORM := ABSReal(CSR);

          //
          // If | C | < SMINI, use C = SMINI
          //
          if AP_FP_Less(CNORM, SMINI) then
            begin
              CSR := SMINI;
              CNORM := SMINI;
              INFO := 1;
            end;

          //
          // Check scaling for  X = B / C
          //
          BNORM := ABSReal(B[1, 1]);
          if AP_FP_Less(CNORM, 1) and AP_FP_Greater(BNORM, 1) then
            begin
              if AP_FP_Greater(BNORM, BIGNUM * CNORM) then
                begin
                  SCL := 1 / BNORM;
                end;
            end;

          //
          // Compute X
          //
          X[1, 1] := B[1, 1] * SCL / CSR;
          XNORM := ABSReal(X[1, 1]);
        end
      else
        begin

          //
          // Complex 1x1 system (w is complex)
          //
          // C = ca A - w D
          //
          CSR := CA * A[1, 1] - WR * D1;
          CSI := -WI * D1;
          CNORM := ABSReal(CSR) + ABSReal(CSI);

          //
          // If | C | < SMINI, use C = SMINI
          //
          if AP_FP_Less(CNORM, SMINI) then
            begin
              CSR := SMINI;
              CSI := 0;
              CNORM := SMINI;
              INFO := 1;
            end;

          //
          // Check scaling for  X = B / C
          //
          BNORM := ABSReal(B[1, 1]) + ABSReal(B[1, 2]);
          if AP_FP_Less(CNORM, 1) and AP_FP_Greater(BNORM, 1) then
            begin
              if AP_FP_Greater(BNORM, BIGNUM * CNORM) then
                begin
                  SCL := 1 / BNORM;
                end;
            end;

          //
          // Compute X
          //
          InternalHSEVDLADIV(SCL * B[1, 1], SCL * B[1, 2], CSR, CSI, TMP1, TMP2);
          X[1, 1] := TMP1;
          X[1, 2] := TMP2;
          XNORM := ABSReal(X[1, 1]) + ABSReal(X[1, 2]);
        end;
    end
  else
    begin

      //
      // 2x2 System
      //
      // Compute the real part of  C = ca A - w D  (or  ca A' - w D )
      //
      CRV4[1 + 0] := CA * A[1, 1] - WR * D1;
      CRV4[2 + 2] := CA * A[2, 2] - WR * D2;
      if LTRANS then
        begin
          CRV4[1 + 2] := CA * A[2, 1];
          CRV4[2 + 0] := CA * A[1, 2];
        end
      else
        begin
          CRV4[2 + 0] := CA * A[2, 1];
          CRV4[1 + 2] := CA * A[1, 2];
        end;
      if NW = 1 then
        begin

          //
          // Real 2x2 system  (w is real)
          //
          // Find the largest element in C
          //
          CMAX := 0;
          ICMAX := 0;
          J := 1;
          while J <= 4 do
            begin
              if AP_FP_Greater(ABSReal(CRV4[J]), CMAX) then
                begin
                  CMAX := ABSReal(CRV4[J]);
                  ICMAX := J;
                end;
              Inc(J);
            end;

          //
          // If norm(C) < SMINI, use SMINI*identity.
          //
          if AP_FP_Less(CMAX, SMINI) then
            begin
              BNORM := Max(ABSReal(B[1, 1]), ABSReal(B[2, 1]));
              if AP_FP_Less(SMINI, 1) and AP_FP_Greater(BNORM, 1) then
                begin
                  if AP_FP_Greater(BNORM, BIGNUM * SMINI) then
                    begin
                      SCL := 1 / BNORM;
                    end;
                end;
              TEMP := SCL / SMINI;
              X[1, 1] := TEMP * B[1, 1];
              X[2, 1] := TEMP * B[2, 1];
              XNORM := TEMP * BNORM;
              INFO := 1;
              Exit;
            end;

          //
          // Gaussian elimination with complete pivoting.
          //
          UR11 := CRV4[ICMAX];
          CR21 := CRV4[IPIVOT44[2, ICMAX]];
          UR12 := CRV4[IPIVOT44[3, ICMAX]];
          CR22 := CRV4[IPIVOT44[4, ICMAX]];
          UR11R := 1 / UR11;
          LR21 := UR11R * CR21;
          UR22 := CR22 - UR12 * LR21;

          //
          // If smaller pivot < SMINI, use SMINI
          //
          if AP_FP_Less(ABSReal(UR22), SMINI) then
            begin
              UR22 := SMINI;
              INFO := 1;
            end;
          if RSWAP4[ICMAX] then
            begin
              BR1 := B[2, 1];
              BR2 := B[1, 1];
            end
          else
            begin
              BR1 := B[1, 1];
              BR2 := B[2, 1];
            end;
          BR2 := BR2 - LR21 * BR1;
          BBND := Max(ABSReal(BR1 * (UR22 * UR11R)), ABSReal(BR2));
          if AP_FP_Greater(BBND, 1) and AP_FP_Less(ABSReal(UR22), 1) then
            begin
              if AP_FP_Greater_Eq(BBND, BIGNUM * ABSReal(UR22)) then
                begin
                  SCL := 1 / BBND;
                end;
            end;
          XR2 := BR2 * SCL / UR22;
          XR1 := SCL * BR1 * UR11R - XR2 * (UR11R * UR12);
          if ZSWAP4[ICMAX] then
            begin
              X[1, 1] := XR2;
              X[2, 1] := XR1;
            end
          else
            begin
              X[1, 1] := XR1;
              X[2, 1] := XR2;
            end;
          XNORM := Max(ABSReal(XR1), ABSReal(XR2));

          //
          // Further scaling if  norm(A) norm(X) > overflow
          //
          if AP_FP_Greater(XNORM, 1) and AP_FP_Greater(CMAX, 1) then
            begin
              if AP_FP_Greater(XNORM, BIGNUM / CMAX) then
                begin
                  TEMP := CMAX / BIGNUM;
                  X[1, 1] := TEMP * X[1, 1];
                  X[2, 1] := TEMP * X[2, 1];
                  XNORM := TEMP * XNORM;
                  SCL := TEMP * SCL;
                end;
            end;
        end
      else
        begin

          //
          // Complex 2x2 system  (w is complex)
          //
          // Find the largest element in C
          //
          CIV4[1 + 0] := -WI * D1;
          CIV4[2 + 0] := 0;
          CIV4[1 + 2] := 0;
          CIV4[2 + 2] := -WI * D2;
          CMAX := 0;
          ICMAX := 0;
          J := 1;
          while J <= 4 do
            begin
              if AP_FP_Greater(ABSReal(CRV4[J]) + ABSReal(CIV4[J]), CMAX) then
                begin
                  CMAX := ABSReal(CRV4[J]) + ABSReal(CIV4[J]);
                  ICMAX := J;
                end;
              Inc(J);
            end;

          //
          // If norm(C) < SMINI, use SMINI*identity.
          //
          if AP_FP_Less(CMAX, SMINI) then
            begin
              BNORM := Max(ABSReal(B[1, 1]) + ABSReal(B[1, 2]),
                ABSReal(B[2, 1]) + ABSReal(B[2, 2]));
              if AP_FP_Less(SMINI, 1) and AP_FP_Greater(BNORM, 1) then
                begin
                  if AP_FP_Greater(BNORM, BIGNUM * SMINI) then
                    begin
                      SCL := 1 / BNORM;
                    end;
                end;
              TEMP := SCL / SMINI;
              X[1, 1] := TEMP * B[1, 1];
              X[2, 1] := TEMP * B[2, 1];
              X[1, 2] := TEMP * B[1, 2];
              X[2, 2] := TEMP * B[2, 2];
              XNORM := TEMP * BNORM;
              INFO := 1;
              Exit;
            end;

          //
          // Gaussian elimination with complete pivoting.
          //
          UR11 := CRV4[ICMAX];
          UI11 := CIV4[ICMAX];
          CR21 := CRV4[IPIVOT44[2, ICMAX]];
          CI21 := CIV4[IPIVOT44[2, ICMAX]];
          UR12 := CRV4[IPIVOT44[3, ICMAX]];
          UI12 := CIV4[IPIVOT44[3, ICMAX]];
          CR22 := CRV4[IPIVOT44[4, ICMAX]];
          CI22 := CIV4[IPIVOT44[4, ICMAX]];
          if (ICMAX = 1) or (ICMAX = 4) then
            begin

              //
              // Code when off-diagonals of pivoted C are real
              //
              if AP_FP_Greater(ABSReal(UR11), ABSReal(UI11)) then
                begin
                  TEMP := UI11 / UR11;
                  UR11R := 1 / (UR11 * (1 + AP_Sqr(TEMP)));
                  UI11R := -TEMP * UR11R;
                end
              else
                begin
                  TEMP := UR11 / UI11;
                  UI11R := -1 / (UI11 * (1 + AP_Sqr(TEMP)));
                  UR11R := -TEMP * UI11R;
                end;
              LR21 := CR21 * UR11R;
              LI21 := CR21 * UI11R;
              UR12S := UR12 * UR11R;
              UI12S := UR12 * UI11R;
              UR22 := CR22 - UR12 * LR21;
              UI22 := CI22 - UR12 * LI21;
            end
          else
            begin

              //
              // Code when diagonals of pivoted C are real
              //
              UR11R := 1 / UR11;
              UI11R := 0;
              LR21 := CR21 * UR11R;
              LI21 := CI21 * UR11R;
              UR12S := UR12 * UR11R;
              UI12S := UI12 * UR11R;
              UR22 := CR22 - UR12 * LR21 + UI12 * LI21;
              UI22 := -UR12 * LI21 - UI12 * LR21;
            end;
          U22ABS := ABSReal(UR22) + ABSReal(UI22);

          //
          // If smaller pivot < SMINI, use SMINI
          //
          if AP_FP_Less(U22ABS, SMINI) then
            begin
              UR22 := SMINI;
              UI22 := 0;
              INFO := 1;
            end;
          if RSWAP4[ICMAX] then
            begin
              BR2 := B[1, 1];
              BR1 := B[2, 1];
              BI2 := B[1, 2];
              BI1 := B[2, 2];
            end
          else
            begin
              BR1 := B[1, 1];
              BR2 := B[2, 1];
              BI1 := B[1, 2];
              BI2 := B[2, 2];
            end;
          BR2 := BR2 - LR21 * BR1 + LI21 * BI1;
          BI2 := BI2 - LI21 * BR1 - LR21 * BI1;
          BBND := Max((ABSReal(BR1) + ABSReal(BI1)) *
            (U22ABS * (ABSReal(UR11R) + ABSReal(UI11R))),
            ABSReal(BR2) + ABSReal(BI2));
          if AP_FP_Greater(BBND, 1) and AP_FP_Less(U22ABS, 1) then
            begin
              if AP_FP_Greater_Eq(BBND, BIGNUM * U22ABS) then
                begin
                  SCL := 1 / BBND;
                  BR1 := SCL * BR1;
                  BI1 := SCL * BI1;
                  BR2 := SCL * BR2;
                  BI2 := SCL * BI2;
                end;
            end;
          InternalHSEVDLADIV(BR2, BI2, UR22, UI22, XR2, XI2);
          XR1 := UR11R * BR1 - UI11R * BI1 - UR12S * XR2 + UI12S * XI2;
          XI1 := UI11R * BR1 + UR11R * BI1 - UI12S * XR2 - UR12S * XI2;
          if ZSWAP4[ICMAX] then
            begin
              X[1, 1] := XR2;
              X[2, 1] := XR1;
              X[1, 2] := XI2;
              X[2, 2] := XI1;
            end
          else
            begin
              X[1, 1] := XR1;
              X[2, 1] := XR2;
              X[1, 2] := XI1;
              X[2, 2] := XI2;
            end;
          XNORM := Max(ABSReal(XR1) + ABSReal(XI1), ABSReal(XR2) + ABSReal(XI2));

          //
          // Further scaling if  norm(A) norm(X) > overflow
          //
          if AP_FP_Greater(XNORM, 1) and AP_FP_Greater(CMAX, 1) then
            begin
              if AP_FP_Greater(XNORM, BIGNUM / CMAX) then
                begin
                  TEMP := CMAX / BIGNUM;
                  X[1, 1] := TEMP * X[1, 1];
                  X[2, 1] := TEMP * X[2, 1];
                  X[1, 2] := TEMP * X[1, 2];
                  X[2, 2] := TEMP * X[2, 2];
                  XNORM := TEMP * XNORM;
                  SCL := TEMP * SCL;
                end;
            end;
        end;
    end;
end;

(* ************************************************************************
  performs complex division in  real arithmetic

  a + i*b
  p + i*q = ---------
  c + i*d

  The algorithm is due to Robert L. Smith and can be found
  in D. Knuth, The art of Computer Programming, Vol.2, p.195
  ************************************************************************ *)
procedure InternalHSEVDLADIV(const A: TLearnFloat; const B: TLearnFloat;
  const C: TLearnFloat; const D: TLearnFloat; var P: TLearnFloat;
  var Q: TLearnFloat);
var
  E: TLearnFloat;
  F: TLearnFloat;
begin
  if AP_FP_Less(ABSReal(D), ABSReal(C)) then
    begin
      E := D / C;
      F := C + D * E;
      P := (A + B * E) / F;
      Q := (B - A * E) / F;
    end
  else
    begin
      E := C / D;
      F := D + C * E;
      P := (B + A * E) / F;
      Q := (-A + B * E) / F;
    end;
end;

function NonSymmetricEVD(A: TLearnFloat2DArray; N: TLearnInteger;
  VNeeded: TLearnInteger; var WR: TLearnFloatArray; var WI: TLearnFloatArray;
  var VL: TLearnFloat2DArray; var VR: TLearnFloat2DArray): Boolean;
var
  S   : TLearnFloat2DArray;
  TAU : TLearnFloatArray;
  SEL : TLearnBooleanArray;
  I   : TLearnInteger;
  INFO: TLearnInteger;
  M   : TLearnInteger;
begin
  A := DynamicArrayCopy(A);
  Assert((VNeeded >= 0) and (VNeeded <= 3),
    'NonSymmetricEVD: incorrect VNeeded!');
  if VNeeded = 0 then
    begin

      //
      // Eigen values only
      //
      ToUpperHessenberg(A, N, TAU);
      InternalSchurDecomposition(A, N, 0, 0, WR, WI, S, INFO);
      Result := INFO = 0;
      Exit;
    end;

  //
  // Eigen values and vectors
  //
  ToUpperHessenberg(A, N, TAU);
  UnpackQFromUpperHessenberg(A, N, TAU, S);
  InternalSchurDecomposition(A, N, 1, 1, WR, WI, S, INFO);
  Result := INFO = 0;
  if not Result then
    begin
      Exit;
    end;
  if (VNeeded = 1) or (VNeeded = 3) then
    begin
      SetLength(VR, N + 1, N + 1);
      I := 1;
      while I <= N do
        begin
          APVMove(@VR[I][0], 1, N, @S[I][0], 1, N);
          Inc(I);
        end;
    end;
  if (VNeeded = 2) or (VNeeded = 3) then
    begin
      SetLength(VL, N + 1, N + 1);
      I := 1;
      while I <= N do
        begin
          APVMove(@VL[I][0], 1, N, @S[I][0], 1, N);
          Inc(I);
        end;
    end;
  InternalTREVC(A, N, VNeeded, 1, SEL, VL, VR, M, INFO);
  Result := INFO = 0;
end;

procedure ToUpperHessenberg(var A: TLearnFloat2DArray; N: TLearnInteger;
  var TAU: TLearnFloatArray);
var
  I   : TLearnInteger;
  IP1 : TLearnInteger;
  NMI : TLearnInteger;
  V   : TLearnFloat;
  T   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  i_  : TLearnInteger;
  i1_ : TLearnInteger;
begin
  Assert(N >= 0, 'ToUpperHessenberg: incorrect N!');

  //
  // Quick return if possible
  //
  if N <= 1 then
    begin
      Exit;
    end;
  SetLength(TAU, N (* - 1 + 1 // optimized compiler *) );
  SetLength(T, N + 1);
  SetLength(WORK, N + 1);
  I := 1;
  while I <= N - 1 do
    begin

      //
      // Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
      //
      IP1 := I + 1;
      NMI := N - I;
      i1_ := (IP1) - (1);
      for i_ := 1 to NMI do
        begin
          T[i_] := A[i_ + i1_, I];
        end;
      GenerateReflection(T, NMI, V);
      i1_ := (1) - (IP1);
      for i_ := IP1 to N do
        begin
          A[i_, I] := T[i_ + i1_];
        end;
      TAU[I] := V;
      T[1] := 1;

      //
      // Apply H(i) to A(1:ihi,i+1:ihi) from the right
      //
      ApplyReflectionFromTheRight(A, V, T, 1, N, I + 1, N, WORK);

      //
      // Apply H(i) to A(i+1:ihi,i+1:n) from the left
      //
      ApplyReflectionFromTheLeft(A, V, T, I + 1, N, I + 1, N, WORK);
      Inc(I);
    end;
end;

procedure UnpackQFromUpperHessenberg(const A: TLearnFloat2DArray; N: TLearnInteger;
  const TAU: TLearnFloatArray; var Q: TLearnFloat2DArray);
var
  I   : TLearnInteger;
  J   : TLearnInteger;
  V   : TLearnFloatArray;
  WORK: TLearnFloatArray;
  IP1 : TLearnInteger;
  NMI : TLearnInteger;
  i_  : TLearnInteger;
  i1_ : TLearnInteger;
begin
  if N = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(Q, N + 1, N + 1);
  SetLength(V, N + 1);
  SetLength(WORK, N + 1);
  I := 1;
  while I <= N do
    begin
      J := 1;
      while J <= N do
        begin
          if I = J then
            begin
              Q[I, J] := 1;
            end
          else
            begin
              Q[I, J] := 0;
            end;
          Inc(J);
        end;
      Inc(I);
    end;

  //
  // unpack Q
  //
  I := 1;
  while I <= N - 1 do
    begin

      //
      // Apply H(i)
      //
      IP1 := I + 1;
      NMI := N - I;
      i1_ := (IP1) - (1);
      for i_ := 1 to NMI do
        begin
          V[i_] := A[i_ + i1_, I];
        end;
      V[1] := 1;
      ApplyReflectionFromTheRight(Q, TAU[I], V, 1, N, I + 1, N, WORK);
      Inc(I);
    end;
end;

procedure UnpackHFromUpperHessenberg(const A: TLearnFloat2DArray; N: TLearnInteger;
  const TAU: TLearnFloatArray; var H: TLearnFloat2DArray);
var
  I   : TLearnInteger;
  J   : TLearnInteger;
  V   : TLearnFloatArray;
  WORK: TLearnFloatArray;
begin
  if N = 0 then
    begin
      Exit;
    end;
  SetLength(H, N + 1, N + 1);
  I := 1;
  while I <= N do
    begin
      J := 1;
      while J <= I - 2 do
        begin
          H[I, J] := 0;
          Inc(J);
        end;
      J := Max(1, I - 1);
      APVMove(@H[I][0], J, N, @A[I][0], J, N);
      Inc(I);
    end;
end;
