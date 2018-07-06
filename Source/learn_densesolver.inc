{ ****************************************************************************** }
{ * Dense linear system solver support, by QQ 600585@qq.com                    * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
procedure XSum(var w: TLVec; mx: TLFloat; n: TLInt;
  var R: TLFloat; var RErr: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function XFastPow(R: TLFloat; n: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  More precise dot-product. Absolute error of  subroutine  result  is  about
  1 ulp of max(MX,V), where:
  MX = max( |a[i]*b[i]| )
  V  = |(a,b)|

  INPUT PARAMETERS
  A       -   array[0..N-1], vector 1
  B       -   array[0..N-1], vector 2
  N       -   vectors length, N<2^29.
  Temp    -   array[0..N-1], pre-allocated temporary storage

  OUTPUT PARAMETERS
  R       -   (A,B)
  RErr    -   estimate of error. This estimate accounts for both  errors
  during  calculation  of  (A,B)  and  errors  introduced by
  rounding of A and B to fit in TLFloat (about 1 ulp).
  ************************************************************************ *)
procedure XDot(const A: TLVec; const b: TLVec; n: TLInt;
  var Temp: TLVec; var R: TLFloat; var RErr: TLFloat);
var
  i : TLInt;
  mx: TLFloat;
  v : TLFloat;
begin

  //
  // special cases:
  // * N=0
  //
  if n = 0 then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  mx := 0;
  i := 0;
  while i <= n - 1 do
    begin
      v := A[i] * b[i];
      Temp[i] := v;
      mx := Max(mx, AbsReal(v));
      Inc(i);
    end;
  if AP_FP_Eq(mx, 0) then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  XSum(Temp, mx, n, R, RErr);
end;

(* ************************************************************************
  More precise TLComplex dot-product. Absolute error of  subroutine  result is
  about 1 ulp of max(MX,V), where:
  MX = max( |a[i]*b[i]| )
  V  = |(a,b)|

  INPUT PARAMETERS
  A       -   array[0..N-1], vector 1
  B       -   array[0..N-1], vector 2
  N       -   vectors length, N<2^29.
  Temp    -   array[0..2*N-1], pre-allocated temporary storage

  OUTPUT PARAMETERS
  R       -   (A,B)
  RErr    -   estimate of error. This estimate accounts for both  errors
  during  calculation  of  (A,B)  and  errors  introduced by
  rounding of A and B to fit in TLFloat (about 1 ulp).
  ************************************************************************ *)
procedure XCDot(const A: TLComplexVec; const b: TLComplexVec;
  n: TLInt; var Temp: TLVec; var R: TLComplex;
  var RErr: TLFloat);
var
  i    : TLInt;
  mx   : TLFloat;
  v    : TLFloat;
  RErrX: TLFloat;
  RErrY: TLFloat;
begin

  //
  // special cases:
  // * N=0
  //
  if n = 0 then
    begin
      R := C_Complex(0);
      RErr := 0;
      Exit;
    end;

  //
  // calculate real part
  //
  mx := 0;
  i := 0;
  while i <= n - 1 do
    begin
      v := A[i].X * b[i].X;
      Temp[2 * i + 0] := v;
      mx := Max(mx, AbsReal(v));
      v := -A[i].Y * b[i].Y;
      Temp[2 * i + 1] := v;
      mx := Max(mx, AbsReal(v));
      Inc(i);
    end;
  if AP_FP_Eq(mx, 0) then
    begin
      R.X := 0;
      RErrX := 0;
    end
  else
    begin
      XSum(Temp, mx, 2 * n, R.X, RErrX);
    end;

  //
  // calculate imaginary part
  //
  mx := 0;
  i := 0;
  while i <= n - 1 do
    begin
      v := A[i].X * b[i].Y;
      Temp[2 * i + 0] := v;
      mx := Max(mx, AbsReal(v));
      v := A[i].Y * b[i].X;
      Temp[2 * i + 1] := v;
      mx := Max(mx, AbsReal(v));
      Inc(i);
    end;
  if AP_FP_Eq(mx, 0) then
    begin
      R.Y := 0;
      RErrY := 0;
    end
  else
    begin
      XSum(Temp, mx, 2 * n, R.Y, RErrY);
    end;

  //
  // total error
  //
  if AP_FP_Eq(RErrX, 0) and AP_FP_Eq(RErrY, 0) then
    begin
      RErr := 0;
    end
  else
    begin
      RErr := Max(RErrX, RErrY) *
        Sqrt(1 + AP_Sqr(Min(RErrX, RErrY) / Max(RErrX, RErrY)));
    end;
end;

(* ************************************************************************
  Internal subroutine for extra-precise calculation of SUM(w[i]).

  INPUT PARAMETERS:
  W   -   array[0..N-1], values to be added
  W is modified during calculations.
  MX  -   max(W[i])
  N   -   array size

  OUTPUT PARAMETERS:
  R   -   SUM(w[i])
  RErr-   error estimate for R
  ************************************************************************ *)
procedure XSum(var w: TLVec; mx: TLFloat; n: TLInt;
  var R: TLFloat; var RErr: TLFloat);
var
  i       : TLInt;
  k       : TLInt;
  KS      : TLInt;
  v       : TLFloat;
  s       : TLFloat;
  LN2     : TLFloat;
  Chunk   : TLFloat;
  InvChunk: TLFloat;
  AllZeros: Boolean;
begin

  //
  // special cases:
  // * N=0
  // * N is too large to use TLInt arithmetics
  //
  if n = 0 then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  if AP_FP_Eq(mx, 0) then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  Assert(n < 536870912, 'XDot: N is too large!');

  //
  // Prepare
  //
  LN2 := ln(2);
  RErr := mx * MachineEpsilon;

  //
  // 1. find S such that 0.5<=S*MX<1
  // 2. multiply W by S, so task is normalized in some sense
  // 3. S:=1/S so we can obtain original vector multiplying by S
  //
  k := Round(ln(mx) / LN2);
  s := XFastPow(2, -k);
  while AP_FP_Greater_Eq(s * mx, 1) do
    begin
      s := 0.5 * s;
    end;
  while AP_FP_Less(s * mx, 0.5) do
    begin
      s := 2 * s;
    end;
  APVMul(@w[0], 0, n - 1, s);
  s := 1 / s;

  //
  // find Chunk=2^M such that N*Chunk<2^29
  //
  // we have chosen upper limit (2^29) with enough space left
  // to tolerate possible problems with rounding and N's close
  // to the limit, so we don't want to be very strict here.
  //
  k := Trunc(ln(AP_Float(536870912) / n) / LN2);
  Chunk := XFastPow(2, k);
  if AP_FP_Less(Chunk, 2) then
    begin
      Chunk := 2;
    end;
  InvChunk := 1 / Chunk;

  //
  // calculate result
  //
  R := 0;
  APVMul(@w[0], 0, n - 1, Chunk);
  while True do
    begin
      s := s * InvChunk;
      AllZeros := True;
      KS := 0;
      i := 0;
      while i <= n - 1 do
        begin
          v := w[i];
          k := Trunc(v);
          if AP_FP_NEq(v, k) then
            begin
              AllZeros := False;
            end;
          w[i] := Chunk * (v - k);
          KS := KS + k;
          Inc(i);
        end;
      R := R + s * KS;
      v := AbsReal(R);
      if AllZeros or AP_FP_Eq(s * n + mx, mx) then
        begin
          Break;
        end;
    end;

  //
  // correct error
  //
  RErr := Max(RErr, AbsReal(R) * MachineEpsilon);
end;

(* ************************************************************************
  Fast Pow
  ************************************************************************ *)
function XFastPow(R: TLFloat; n: TLInt): TLFloat;
begin
  if n > 0 then
    begin
      if n mod 2 = 0 then
        begin
          Result := AP_Sqr(XFastPow(R, n div 2));
        end
      else
        begin
          Result := R * XFastPow(R, n - 1);
        end;
      Exit;
    end;
  if n = 0 then
    begin
      Result := 1;
    end;
  if n < 0 then
    begin
      Result := XFastPow(1 / R, -n);
    end;
end;

type
  TDenseSolverReport = packed record
    r1: TLFloat;
    RInf: TLFloat;
  end;

  TDenseSolverLSReport = packed record
    r2: TLFloat;
    Cx: TLMatrix;
    n: TLInt;
    k: TLInt;
  end;

procedure RMatrixSolve(const A: TLMatrix; n: TLInt;
  const b: TLVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixSolveM(const A: TLMatrix; n: TLInt;
  const b: TLMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUSolve(const LUA: TLMatrix; const p: TLIVec;
  n: TLInt; const b: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUSolveM(const LUA: TLMatrix; const p: TLIVec;
  n: TLInt; const b: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixMixedSolve(const A: TLMatrix; const LUA: TLMatrix;
  const p: TLIVec; n: TLInt; const b: TLVec;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixMixedSolveM(const A: TLMatrix; const LUA: TLMatrix;
  const p: TLIVec; n: TLInt; const b: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixSolveM(const A: TLComplexMatrix; n: TLInt;
  const b: TLComplexMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixSolve(const A: TLComplexMatrix; n: TLInt;
  const b: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixLUSolveM(const LUA: TLComplexMatrix; const p: TLIVec;
  n: TLInt; const b: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixLUSolve(const LUA: TLComplexMatrix; const p: TLIVec;
  n: TLInt; const b: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixMixedSolveM(const A: TLComplexMatrix;
  const LUA: TLComplexMatrix; const p: TLIVec; n: TLInt;
  const b: TLComplexMatrix; M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixMixedSolve(const A: TLComplexMatrix;
  const LUA: TLComplexMatrix; const p: TLIVec; n: TLInt;
  const b: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixSolveM(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixSolve(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixCholeskySolveM(const CHA: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixCholeskySolve(const CHA: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixSolveM(const A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixSolve(const A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixCholeskySolveM(const CHA: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixCholeskySolve(const CHA: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixSolveLS(const A: TLMatrix; NRows: TLInt;
  NCols: TLInt; const b: TLVec; Threshold: TLFloat;
  var Info: TLInt; var Rep: TDenseSolverLSReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUSolveInternal(const LUA: TLMatrix;
  const p: TLIVec; const ScaleA: TLFloat; n: TLInt;
  const A: TLMatrix; HaveA: Boolean; const b: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SPDMatrixCholeskySolveInternal(const CHA: TLMatrix;
  const SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  const A: TLMatrix; HaveA: Boolean; const b: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLUSolveInternal(const LUA: TLComplexMatrix;
  const p: TLIVec; const ScaleA: TLFloat; n: TLInt;
  const A: TLComplexMatrix; HaveA: Boolean; const b: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HPDMatrixCholeskySolveInternal(const CHA: TLComplexMatrix;
  const SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  const A: TLComplexMatrix; HaveA: Boolean; const b: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DenseSolverRFSMax(n: TLInt; r1: TLFloat; RInf: TLFloat)
  : TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DenseSolverRFSMaxV2(n: TLInt; r2: TLFloat)
  : TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RBasicLUSolve(const LUA: TLMatrix; const p: TLIVec;
  ScaleA: TLFloat; n: TLInt; var XB: TLVec;
  var tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SPDBasicCholeskySolve(const CHA: TLMatrix;
  SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  var XB: TLVec; var tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CBasicLUSolve(const LUA: TLComplexMatrix; const p: TLIVec;
  ScaleA: TLFloat; n: TLInt; var XB: TLComplexVec;
  var tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HPDBasicCholeskySolve(const CHA: TLComplexMatrix;
  SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  var XB: TLComplexVec; var tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


(* ************************************************************************
  Dense solver.

  This  subroutine  solves  a  system  A*x=b,  where A is NxN non-denegerate
  real matrix, x and b are vectors.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * iterative refinement
  * O(N^3) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   return code:
  * -3    A is singular, or VERY close to singular.
  X is filled by zeros in such cases.
  * -1    N<=0 was passed
  *  1    task is solved (but matrix A may be ill-conditioned,
  check R1/RInf parameters for condition numbers).
  Rep     -   solver report, see below for more info
  X       -   array[0..N-1], it contains:
  * solution of A*x=b if A is non-singular (well-conditioned
  or ill-conditioned, but not very close to singular)
  * zeros,  if  A  is  singular  or  VERY  close to singular
  (in this case Info=-3).

  SOLVER REPORT

  Subroutine sets following fields of the Rep structure:
  * R1        reciprocal of condition number: 1/cond(A), 1-norm.
  * RInf      reciprocal of condition number: 1/cond(A), inf-norm.
  ************************************************************************ *)
procedure RMatrixSolve(const A: TLMatrix; n: TLInt;
  const b: TLVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  RMatrixSolveM(A, n, BM, 1, True, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver.

  Similar to RMatrixSolve() but solves task with multiple right parts (where
  b and x are NxM matrices).

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * optional iterative refinement
  * O(N^3+M*N^2) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size
  RFS     -   iterative refinement switch:
  * True - refinement is used.
  Less performance, more precision.
  * False - refinement is not used.
  More performance, less precision.

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure RMatrixSolveM(const A: TLMatrix; n: TLInt;
  const b: TLMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  DA    : TLMatrix;
  EmptyA: TLMatrix;
  p     : TLIVec;
  ScaleA: TLFloat;
  i     : TLInt;
  J     : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, n, n);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  ScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          ScaleA := Max(ScaleA, AbsReal(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  i := 0;
  while i <= n - 1 do
    begin
      APVMove(@DA[i][0], 0, n - 1, @A[i][0], 0, n - 1);
      Inc(i);
    end;
  RMatrixLU(DA, n, n, p);
  if RFS then
    begin
      RMatrixLUSolveInternal(DA, p, ScaleA, n, A, True, b, M, Info, Rep, X);
    end
  else
    begin
      RMatrixLUSolveInternal(DA, p, ScaleA, n, EmptyA, False, b, M, Info, Rep, X);
    end;
end;

(* ************************************************************************
  Dense solver.

  This  subroutine  solves  a  system  A*X=B,  where A is NxN non-denegerate
  real matrix given by its LU decomposition, X and B are NxM real matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * O(N^2) complexity
  * condition number estimation

  No iterative refinement  is provided because exact form of original matrix
  is not known to subroutine. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  LUA     -   array[0..N-1,0..N-1], LU decomposition, RMatrixLU result
  P       -   array[0..N-1], pivots array, RMatrixLU result
  N       -   size of A
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure RMatrixLUSolve(const LUA: TLMatrix; const p: TLIVec;
  n: TLInt; const b: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  RMatrixLUSolveM(LUA, p, n, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver.

  Similar to RMatrixLUSolve() but solves task with multiple right parts
  (where b and x are NxM matrices).

  Algorithm features:
  * automatic detection of degenerate cases
  * O(M*N^2) complexity
  * condition number estimation

  No iterative refinement  is provided because exact form of original matrix
  is not known to subroutine. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  LUA     -   array[0..N-1,0..N-1], LU decomposition, RMatrixLU result
  P       -   array[0..N-1], pivots array, RMatrixLU result
  N       -   size of A
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure RMatrixLUSolveM(const LUA: TLMatrix; const p: TLIVec;
  n: TLInt; const b: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  EmptyA: TLMatrix;
  i     : TLInt;
  J     : TLInt;
  ScaleA: TLFloat;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // 1. scale matrix, max(|U[i,j]|)
  // we assume that LU is in its normal form, i.e. |L[i,j]|<=1
  // 2. solve
  //
  ScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      J := i;
      while J <= n - 1 do
        begin
          ScaleA := Max(ScaleA, AbsReal(LUA[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  RMatrixLUSolveInternal(LUA, p, ScaleA, n, EmptyA, False, b, M, Info, Rep, X);
end;

(* ************************************************************************
  Dense solver.

  This  subroutine  solves  a  system  A*x=b,  where BOTH ORIGINAL A AND ITS
  LU DECOMPOSITION ARE KNOWN. You can use it if for some  reasons  you  have
  both A and its LU decomposition.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * iterative refinement
  * O(N^2) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  LUA     -   array[0..N-1,0..N-1], LU decomposition, RMatrixLU result
  P       -   array[0..N-1], pivots array, RMatrixLU result
  N       -   size of A
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolveM
  Rep     -   same as in RMatrixSolveM
  X       -   same as in RMatrixSolveM
  ************************************************************************ *)
procedure RMatrixMixedSolve(const A: TLMatrix; const LUA: TLMatrix;
  const p: TLIVec; n: TLInt; const b: TLVec;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  RMatrixMixedSolveM(A, LUA, p, n, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver.

  Similar to RMatrixMixedSolve() but  solves task with multiple right  parts
  (where b and x are NxM matrices).

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * iterative refinement
  * O(M*N^2) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  LUA     -   array[0..N-1,0..N-1], LU decomposition, RMatrixLU result
  P       -   array[0..N-1], pivots array, RMatrixLU result
  N       -   size of A
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolveM
  Rep     -   same as in RMatrixSolveM
  X       -   same as in RMatrixSolveM
  ************************************************************************ *)
procedure RMatrixMixedSolveM(const A: TLMatrix; const LUA: TLMatrix;
  const p: TLIVec; n: TLInt; const b: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix);
var
  ScaleA: TLFloat;
  i     : TLInt;
  J     : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  ScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          ScaleA := Max(ScaleA, AbsReal(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  RMatrixLUSolveInternal(LUA, p, ScaleA, n, A, True, b, M, Info, Rep, X);
end;

(* ************************************************************************
  Dense solver. Same as RMatrixSolveM(), but for TLComplex matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * iterative refinement
  * O(N^3+M*N^2) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size
  RFS     -   iterative refinement switch:
  * True - refinement is used.
  Less performance, more precision.
  * False - refinement is not used.
  More performance, less precision.

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure CMatrixSolveM(const A: TLComplexMatrix; n: TLInt;
  const b: TLComplexMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  DA    : TLComplexMatrix;
  EmptyA: TLComplexMatrix;
  p     : TLIVec;
  ScaleA: TLFloat;
  i     : TLInt;
  J     : TLInt;
  i_    : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, n, n);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  ScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          ScaleA := Max(ScaleA, AbsComplex(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  i := 0;
  while i <= n - 1 do
    begin
      for i_ := 0 to n - 1 do
        begin
          DA[i, i_] := A[i, i_];
        end;
      Inc(i);
    end;
  CMatrixLU(DA, n, n, p);
  if RFS then
    begin
      CMatrixLUSolveInternal(DA, p, ScaleA, n, A, True, b, M, Info, Rep, X);
    end
  else
    begin
      CMatrixLUSolveInternal(DA, p, ScaleA, n, EmptyA, False, b, M, Info, Rep, X);
    end;
end;

(* ************************************************************************
  Dense solver. Same as RMatrixSolve(), but for TLComplex matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * iterative refinement
  * O(N^3) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure CMatrixSolve(const A: TLComplexMatrix; n: TLInt;
  const b: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  CMatrixSolveM(A, n, BM, 1, True, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver. Same as RMatrixLUSolveM(), but for TLComplex matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * O(M*N^2) complexity
  * condition number estimation

  No iterative refinement  is provided because exact form of original matrix
  is not known to subroutine. Use CMatrixSolve or CMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  LUA     -   array[0..N-1,0..N-1], LU decomposition, RMatrixLU result
  P       -   array[0..N-1], pivots array, RMatrixLU result
  N       -   size of A
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure CMatrixLUSolveM(const LUA: TLComplexMatrix; const p: TLIVec;
  n: TLInt; const b: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  EmptyA: TLComplexMatrix;
  i     : TLInt;
  J     : TLInt;
  ScaleA: TLFloat;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // 1. scale matrix, max(|U[i,j]|)
  // we assume that LU is in its normal form, i.e. |L[i,j]|<=1
  // 2. solve
  //
  ScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      J := i;
      while J <= n - 1 do
        begin
          ScaleA := Max(ScaleA, AbsComplex(LUA[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  CMatrixLUSolveInternal(LUA, p, ScaleA, n, EmptyA, False, b, M, Info, Rep, X);
end;

(* ************************************************************************
  Dense solver. Same as RMatrixLUSolve(), but for TLComplex matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * O(N^2) complexity
  * condition number estimation

  No iterative refinement is provided because exact form of original matrix
  is not known to subroutine. Use CMatrixSolve or CMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  LUA     -   array[0..N-1,0..N-1], LU decomposition, CMatrixLU result
  P       -   array[0..N-1], pivots array, CMatrixLU result
  N       -   size of A
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure CMatrixLUSolve(const LUA: TLComplexMatrix; const p: TLIVec;
  n: TLInt; const b: TLComplexVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  CMatrixLUSolveM(LUA, p, n, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver. Same as RMatrixMixedSolveM(), but for TLComplex matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * iterative refinement
  * O(M*N^2) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  LUA     -   array[0..N-1,0..N-1], LU decomposition, CMatrixLU result
  P       -   array[0..N-1], pivots array, CMatrixLU result
  N       -   size of A
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolveM
  Rep     -   same as in RMatrixSolveM
  X       -   same as in RMatrixSolveM
  ************************************************************************ *)
procedure CMatrixMixedSolveM(const A: TLComplexMatrix;
  const LUA: TLComplexMatrix; const p: TLIVec; n: TLInt;
  const b: TLComplexMatrix; M: TLInt; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  ScaleA: TLFloat;
  i     : TLInt;
  J     : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  ScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      J := 0;
      while J <= n - 1 do
        begin
          ScaleA := Max(ScaleA, AbsComplex(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  CMatrixLUSolveInternal(LUA, p, ScaleA, n, A, True, b, M, Info, Rep, X);
end;

(* ************************************************************************
  Dense solver. Same as RMatrixMixedSolve(), but for TLComplex matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * iterative refinement
  * O(N^2) complexity

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  LUA     -   array[0..N-1,0..N-1], LU decomposition, CMatrixLU result
  P       -   array[0..N-1], pivots array, CMatrixLU result
  N       -   size of A
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolveM
  Rep     -   same as in RMatrixSolveM
  X       -   same as in RMatrixSolveM
  ************************************************************************ *)
procedure CMatrixMixedSolve(const A: TLComplexMatrix;
  const LUA: TLComplexMatrix; const p: TLIVec; n: TLInt;
  const b: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  CMatrixMixedSolveM(A, LUA, p, n, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver. Same as RMatrixSolveM(), but for symmetric positive definite
  matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * O(N^3+M*N^2) complexity
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  IsUpper -   what half of A is provided
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve.
  Returns -3 for non-SPD matrices.
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure SPDMatrixSolveM(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  DA        : TLMatrix;
  SqrtScaleA: TLFloat;
  i         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, n, n);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  SqrtScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsReal(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  SqrtScaleA := Sqrt(SqrtScaleA);
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      APVMove(@DA[i][0], J1, J2, @A[i][0], J1, J2);
      Inc(i);
    end;
  if not SPDMatrixCholesky(DA, n, IsUpper) then
    begin
      SetLength(X, n, M);
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[i, J] := 0;
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;
  SPDMatrixCholeskySolveInternal(DA, SqrtScaleA, n, IsUpper, A, True, b, M,
    Info, Rep, X);
end;

(* ************************************************************************
  Dense solver. Same as RMatrixSolve(), but for SPD matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * O(N^3) complexity
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  IsUpper -   what half of A is provided
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Returns -3 for non-SPD matrices.
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure SPDMatrixSolve(const A: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  SPDMatrixSolveM(A, n, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver. Same as RMatrixLUSolveM(), but for SPD matrices  represented
  by their Cholesky decomposition.

  Algorithm features:
  * automatic detection of degenerate cases
  * O(M*N^2) complexity
  * condition number estimation
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  CHA     -   array[0..N-1,0..N-1], Cholesky decomposition,
  SPDMatrixCholesky result
  N       -   size of CHA
  IsUpper -   what half of CHA is provided
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure SPDMatrixCholeskySolveM(const CHA: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  EmptyA    : TLMatrix;
  SqrtScaleA: TLFloat;
  i         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // 1. scale matrix, max(|U[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  SqrtScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsReal(CHA[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  SPDMatrixCholeskySolveInternal(CHA, SqrtScaleA, n, IsUpper, EmptyA, False, b,
    M, Info, Rep, X);
end;

(* ************************************************************************
  Dense solver. Same as RMatrixLUSolve(), but for  SPD matrices  represented
  by their Cholesky decomposition.

  Algorithm features:
  * automatic detection of degenerate cases
  * O(N^2) complexity
  * condition number estimation
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  CHA     -   array[0..N-1,0..N-1], Cholesky decomposition,
  SPDMatrixCholesky result
  N       -   size of A
  IsUpper -   what half of CHA is provided
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure SPDMatrixCholeskySolve(const CHA: TLMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  SPDMatrixCholeskySolveM(CHA, n, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver. Same as RMatrixSolveM(), but for Hermitian positive definite
  matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * O(N^3+M*N^2) complexity
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  IsUpper -   what half of A is provided
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve.
  Returns -3 for non-HPD matrices.
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure HPDMatrixSolveM(const A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  DA        : TLComplexMatrix;
  SqrtScaleA: TLFloat;
  i         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
  i_        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, n, n);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  SqrtScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsComplex(A[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  SqrtScaleA := Sqrt(SqrtScaleA);
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      for i_ := J1 to J2 do
        begin
          DA[i, i_] := A[i, i_];
        end;
      Inc(i);
    end;
  if not HPDMatrixCholesky(DA, n, IsUpper) then
    begin
      SetLength(X, n, M);
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[i, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;
  HPDMatrixCholeskySolveInternal(DA, SqrtScaleA, n, IsUpper, A, True, b, M,
    Info, Rep, X);
end;

(* ************************************************************************
  Dense solver. Same as RMatrixSolve(),  but for Hermitian positive definite
  matrices.

  Algorithm features:
  * automatic detection of degenerate cases
  * condition number estimation
  * O(N^3) complexity
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  A       -   array[0..N-1,0..N-1], system matrix
  N       -   size of A
  IsUpper -   what half of A is provided
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Returns -3 for non-HPD matrices.
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure HPDMatrixSolve(const A: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  HPDMatrixSolveM(A, n, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver. Same as RMatrixLUSolveM(), but for HPD matrices  represented
  by their Cholesky decomposition.

  Algorithm features:
  * automatic detection of degenerate cases
  * O(M*N^2) complexity
  * condition number estimation
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  CHA     -   array[0..N-1,0..N-1], Cholesky decomposition,
  HPDMatrixCholesky result
  N       -   size of CHA
  IsUpper -   what half of CHA is provided
  B       -   array[0..N-1,0..M-1], right part
  M       -   right part size

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure HPDMatrixCholeskySolveM(const CHA: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  EmptyA    : TLComplexMatrix;
  SqrtScaleA: TLFloat;
  i         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // 1. scale matrix, max(|U[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  SqrtScaleA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if IsUpper then
        begin
          J1 := i;
          J2 := n - 1;
        end
      else
        begin
          J1 := 0;
          J2 := i;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsComplex(CHA[i, J]));
          Inc(J);
        end;
      Inc(i);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  HPDMatrixCholeskySolveInternal(CHA, SqrtScaleA, n, IsUpper, EmptyA, False, b,
    M, Info, Rep, X);
end;

(* ************************************************************************
  Dense solver. Same as RMatrixLUSolve(), but for  HPD matrices  represented
  by their Cholesky decomposition.

  Algorithm features:
  * automatic detection of degenerate cases
  * O(N^2) complexity
  * condition number estimation
  * matrix is represented by its upper or lower triangle

  No iterative refinement is provided because such partial representation of
  matrix does not allow efficient calculation of extra-precise  matrix-vector
  products for large matrices. Use RMatrixSolve or RMatrixMixedSolve  if  you
  need iterative refinement.

  INPUT PARAMETERS
  CHA     -   array[0..N-1,0..N-1], Cholesky decomposition,
  SPDMatrixCholesky result
  N       -   size of A
  IsUpper -   what half of CHA is provided
  B       -   array[0..N-1], right part

  OUTPUT PARAMETERS
  Info    -   same as in RMatrixSolve
  Rep     -   same as in RMatrixSolve
  X       -   same as in RMatrixSolve
  ************************************************************************ *)
procedure HPDMatrixCholeskySolve(const CHA: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; const b: TLComplexVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if n <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, n, 1);
  for i_ := 0 to n - 1 do
    begin
      BM[i_, 0] := b[i_];
    end;
  HPDMatrixCholeskySolveM(CHA, n, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, n);
  for i_ := 0 to n - 1 do
    begin
      X[i_] := XM[i_, 0];
    end;
end;

(* ************************************************************************
  Dense solver.

  This subroutine finds solution of the linear system A*X=B with non-square,
  possibly degenerate A.  System  is  solved in the least squares sense, and
  general least squares solution  X = X0 + CX*y  which  minimizes |A*X-B| is
  returned. If A is non-degenerate, solution in the  usual sense is returned

  Algorithm features:
  * automatic detection of degenerate cases
  * iterative refinement
  * O(N^3) complexity

  INPUT PARAMETERS
  A       -   array[0..NRows-1,0..NCols-1], system matrix
  NRows   -   vertical size of A
  NCols   -   horizontal size of A
  B       -   array[0..NCols-1], right part
  Threshold-  a number in [0,1]. Singular values  beyond  Threshold  are
  considered  zero.  Set  it to 0.0, if you don't understand
  what it means, so the solver will choose good value on its
  own.

  OUTPUT PARAMETERS
  Info    -   return code:
  * -4    SVD subroutine failed
  * -1    if NRows<=0 or NCols<=0 or Threshold<0 was passed
  *  1    if task is solved
  Rep     -   solver report, see below for more info
  X       -   array[0..N-1,0..M-1], it contains:
  * solution of A*X=B if A is non-singular (well-conditioned
  or ill-conditioned, but not very close to singular)
  * zeros,  if  A  is  singular  or  VERY  close to singular
  (in this case Info=-3).

  SOLVER REPORT

  Subroutine sets following fields of the Rep structure:
  * R2        reciprocal of condition number: 1/cond(A), 2-norm.
  * N         = NCols
  * K         dim(Null(A))
  * CX        array[0..N-1,0..K-1], kernel of A.
  Columns of CX store such vectors that A*CX[i]=0.
  ************************************************************************ *)
procedure RMatrixSolveLS(const A: TLMatrix; NRows: TLInt;
  NCols: TLInt; const b: TLVec; Threshold: TLFloat;
  var Info: TLInt; var Rep: TDenseSolverLSReport; var X: TLVec);
var
  SV               : TLVec;
  u                : TLMatrix;
  VT               : TLMatrix;
  RP               : TLVec;
  UTB              : TLVec;
  SUTB             : TLVec;
  tmp              : TLVec;
  TA               : TLVec;
  TX               : TLVec;
  Buf              : TLVec;
  w                : TLVec;
  i                : TLInt;
  J                : TLInt;
  NSV              : TLInt;
  KernelIdx        : TLInt;
  v                : TLFloat;
  VErr             : TLFloat;
  SVDFailed        : Boolean;
  ZeroA            : Boolean;
  RFS              : TLInt;
  NRFS             : TLInt;
  TerminateNextTime: Boolean;
  SmallErr         : Boolean;
  i_               : TLInt;
begin
  if (NRows <= 0) or (NCols <= 0) or AP_FP_Less(Threshold, 0) then
    begin
      Info := -1;
      Exit;
    end;
  if AP_FP_Eq(Threshold, 0) then
    begin
      Threshold := 1000 * MachineEpsilon;
    end;

  //
  // Factorize A first
  //
  SVDFailed := not RMatrixSVD(A, NRows, NCols, 1, 2, 2, SV, u, VT);
  ZeroA := AP_FP_Eq(SV[0], 0);
  if SVDFailed or ZeroA then
    begin
      if SVDFailed then
        begin
          Info := -4;
        end
      else
        begin
          Info := 1;
        end;
      SetLength(X, NCols);
      i := 0;
      while i <= NCols - 1 do
        begin
          X[i] := 0;
          Inc(i);
        end;
      Rep.n := NCols;
      Rep.k := NCols;
      SetLength(Rep.Cx, NCols, NCols);
      i := 0;
      while i <= NCols - 1 do
        begin
          J := 0;
          while J <= NCols - 1 do
            begin
              if i = J then
                begin
                  Rep.Cx[i, J] := 1;
                end
              else
                begin
                  Rep.Cx[i, J] := 0;
                end;
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r2 := 0;
      Exit;
    end;
  NSV := Min(NCols, NRows);
  if NSV = NCols then
    begin
      Rep.r2 := SV[NSV - 1] / SV[0];
    end
  else
    begin
      Rep.r2 := 0;
    end;
  Rep.n := NCols;
  Info := 1;

  //
  // Iterative refinement of xc combined with solution:
  // 1. xc = 0
  // 2. calculate r = bc-A*xc using extra-precise dot product
  // 3. solve A*y = r
  // 4. update x:=x+r
  // 5. goto 2
  //
  // This cycle is executed until one of two things happens:
  // 1. maximum number of iterations reached
  // 2. last iteration decreased error to the lower limit
  //
  SetLength(UTB, NSV);
  SetLength(SUTB, NSV);
  SetLength(X, NCols);
  SetLength(tmp, NCols);
  SetLength(TA, NCols + 1);
  SetLength(TX, NCols + 1);
  SetLength(Buf, NCols + 1);
  i := 0;
  while i <= NCols - 1 do
    begin
      X[i] := 0;
      Inc(i);
    end;
  KernelIdx := NSV;
  i := 0;
  while i <= NSV - 1 do
    begin
      if AP_FP_Less_Eq(SV[i], Threshold * SV[0]) then
        begin
          KernelIdx := i;
          Break;
        end;
      Inc(i);
    end;
  Rep.k := NCols - KernelIdx;
  NRFS := DenseSolverRFSMaxV2(NCols, Rep.r2);
  TerminateNextTime := False;
  SetLength(RP, NRows);
  RFS := 0;
  while RFS <= NRFS do
    begin
      if TerminateNextTime then
        begin
          Break;
        end;

      //
      // calculate right part
      //
      if RFS = 0 then
        begin
          APVMove(@RP[0], 0, NRows - 1, @b[0], 0, NRows - 1);
        end
      else
        begin
          SmallErr := True;
          i := 0;
          while i <= NRows - 1 do
            begin
              APVMove(@TA[0], 0, NCols - 1, @A[i][0], 0, NCols - 1);
              TA[NCols] := -1;
              APVMove(@TX[0], 0, NCols - 1, @X[0], 0, NCols - 1);
              TX[NCols] := b[i];
              XDot(TA, TX, NCols + 1, Buf, v, VErr);
              RP[i] := -v;
              SmallErr := SmallErr and AP_FP_Less(AbsReal(v), 4 * VErr);
              Inc(i);
            end;
          if SmallErr then
            begin
              TerminateNextTime := True;
            end;
        end;

      //
      // solve A*dx = rp
      //
      i := 0;
      while i <= NCols - 1 do
        begin
          tmp[i] := 0;
          Inc(i);
        end;
      i := 0;
      while i <= NSV - 1 do
        begin
          UTB[i] := 0;
          Inc(i);
        end;
      i := 0;
      while i <= NRows - 1 do
        begin
          v := RP[i];
          APVAdd(@UTB[0], 0, NSV - 1, @u[i][0], 0, NSV - 1, v);
          Inc(i);
        end;
      i := 0;
      while i <= NSV - 1 do
        begin
          if i < KernelIdx then
            begin
              SUTB[i] := UTB[i] / SV[i];
            end
          else
            begin
              SUTB[i] := 0;
            end;
          Inc(i);
        end;
      i := 0;
      while i <= NSV - 1 do
        begin
          v := SUTB[i];
          APVAdd(@tmp[0], 0, NCols - 1, @VT[i][0], 0, NCols - 1, v);
          Inc(i);
        end;

      //
      // update x:  x:=x+dx
      //
      APVAdd(@X[0], 0, NCols - 1, @tmp[0], 0, NCols - 1);
      Inc(RFS);
    end;

  //
  // fill CX
  //
  if Rep.k > 0 then
    begin
      SetLength(Rep.Cx, NCols, Rep.k);
      i := 0;
      while i <= Rep.k - 1 do
        begin
          for i_ := 0 to NCols - 1 do
            begin
              Rep.Cx[i_, i] := VT[KernelIdx + i, i_];
            end;
          Inc(i);
        end;
    end;
end;

(* ************************************************************************
  Internal LU solver
  ************************************************************************ *)
procedure RMatrixLUSolveInternal(const LUA: TLMatrix;
  const p: TLIVec; const ScaleA: TLFloat; n: TLInt;
  const A: TLMatrix; HaveA: Boolean; const b: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix);
var
  i                : TLInt;
  J                : TLInt;
  k                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLVec;
  Y                : TLVec;
  BC               : TLVec;
  XA               : TLVec;
  XB               : TLVec;
  TX               : TLVec;
  v                : TLFloat;
  VErr             : TLFloat;
  MXB              : TLFloat;
  ScaleRight       : TLFloat;
  SmallErr         : Boolean;
  TerminateNextTime: Boolean;
  i_               : TLInt;
begin
  Assert(AP_FP_Greater(ScaleA, 0));

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if (p[i] > n - 1) or (p[i] < i) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(i);
    end;
  SetLength(X, n, M);
  SetLength(Y, n);
  SetLength(XC, n);
  SetLength(BC, n);
  SetLength(TX, n + 1);
  SetLength(XA, n + 1);
  SetLength(XB, n + 1);

  //
  // estimate condition number, test for near singularity
  //
  Rep.r1 := RMatrixLURCond1(LUA, n);
  Rep.RInf := RMatrixLURCondInf(LUA, n);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[i, J] := 0;
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  k := 0;
  while k <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to n - 1 do
        begin
          BC[i_] := b[i_, k];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      i := 0;
      while i <= n - 1 do
        begin
          MXB := Max(MXB, AbsReal(BC[i]));
          Inc(i);
        end;
      if AP_FP_Eq(MXB, 0) then
        begin
          MXB := 1;
        end;
      ScaleRight := 1 / MXB;

      //
      // First, non-iterative part of solution process.
      // We use separate code for this task because
      // XDot is quite slow and we want to save time.
      //
      APVMove(@XC[0], 0, n - 1, @BC[0], 0, n - 1, ScaleRight);
      RBasicLUSolve(LUA, p, ScaleA, n, XC, TX);

      //
      // Iterative refinement of xc:
      // * calculate r = bc-A*xc using extra-precise dot product
      // * solve A*y = r
      // * update x:=x+r
      //
      // This cycle is executed until one of two things happens:
      // 1. maximum number of iterations reached
      // 2. last iteration decreased error to the lower limit
      //
      if HaveA then
        begin
          NRFS := DenseSolverRFSMax(n, Rep.r1, Rep.RInf);
          TerminateNextTime := False;
          RFS := 0;
          while RFS <= NRFS - 1 do
            begin
              if TerminateNextTime then
                begin
                  Break;
                end;

              //
              // generate right part
              //
              SmallErr := True;
              APVMove(@XB[0], 0, n - 1, @XC[0], 0, n - 1);
              i := 0;
              while i <= n - 1 do
                begin
                  APVMove(@XA[0], 0, n - 1, @A[i][0], 0, n - 1, ScaleA);
                  XA[n] := -1;
                  XB[n] := ScaleRight * BC[i];
                  XDot(XA, XB, n + 1, TX, v, VErr);
                  Y[i] := -v;
                  SmallErr := SmallErr and AP_FP_Less(AbsReal(v), 4 * VErr);
                  Inc(i);
                end;
              if SmallErr then
                begin
                  TerminateNextTime := True;
                end;

              //
              // solve and update
              //
              RBasicLUSolve(LUA, p, ScaleA, n, Y, TX);
              APVAdd(@XC[0], 0, n - 1, @Y[0], 0, n - 1);
              Inc(RFS);
            end;
        end;

      //
      // Store xc.
      // Post-scale result.
      //
      v := ScaleA * MXB;
      for i_ := 0 to n - 1 do
        begin
          X[i_, k] := v * XC[i_];
        end;
      Inc(k);
    end;
end;

(* ************************************************************************
  Internal Cholesky solver
  ************************************************************************ *)
procedure SPDMatrixCholeskySolveInternal(const CHA: TLMatrix;
  const SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  const A: TLMatrix; HaveA: Boolean; const b: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix);
var
  i                : TLInt;
  J                : TLInt;
  k                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLVec;
  Y                : TLVec;
  BC               : TLVec;
  XA               : TLVec;
  XB               : TLVec;
  TX               : TLVec;
  v                : TLFloat;
  VErr             : TLFloat;
  MXB              : TLFloat;
  ScaleRight       : TLFloat;
  SmallErr         : Boolean;
  TerminateNextTime: Boolean;
  i_               : TLInt;
begin
  Assert(AP_FP_Greater(SqrtScaleA, 0));

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(X, n, M);
  SetLength(Y, n);
  SetLength(XC, n);
  SetLength(BC, n);
  SetLength(TX, n + 1);
  SetLength(XA, n + 1);
  SetLength(XB, n + 1);

  //
  // estimate condition number, test for near singularity
  //
  Rep.r1 := SPDMatrixCholeskyRCond(CHA, n, IsUpper);
  Rep.RInf := Rep.r1;
  if AP_FP_Less(Rep.r1, RCondThreshold) then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[i, J] := 0;
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  k := 0;
  while k <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to n - 1 do
        begin
          BC[i_] := b[i_, k];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      i := 0;
      while i <= n - 1 do
        begin
          MXB := Max(MXB, AbsReal(BC[i]));
          Inc(i);
        end;
      if AP_FP_Eq(MXB, 0) then
        begin
          MXB := 1;
        end;
      ScaleRight := 1 / MXB;

      //
      // First, non-iterative part of solution process.
      // We use separate code for this task because
      // XDot is quite slow and we want to save time.
      //
      APVMove(@XC[0], 0, n - 1, @BC[0], 0, n - 1, ScaleRight);
      SPDBasicCholeskySolve(CHA, SqrtScaleA, n, IsUpper, XC, TX);

      //
      // Store xc.
      // Post-scale result.
      //
      v := AP_Sqr(SqrtScaleA) * MXB;
      for i_ := 0 to n - 1 do
        begin
          X[i_, k] := v * XC[i_];
        end;
      Inc(k);
    end;
end;

(* ************************************************************************
  Internal LU solver
  ************************************************************************ *)
procedure CMatrixLUSolveInternal(const LUA: TLComplexMatrix;
  const p: TLIVec; const ScaleA: TLFloat; n: TLInt;
  const A: TLComplexMatrix; HaveA: Boolean; const b: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix);
var
  i                : TLInt;
  J                : TLInt;
  k                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLComplexVec;
  Y                : TLComplexVec;
  BC               : TLComplexVec;
  XA               : TLComplexVec;
  XB               : TLComplexVec;
  TX               : TLComplexVec;
  TmpBuf           : TLVec;
  v                : TLComplex;
  VErr             : TLFloat;
  MXB              : TLFloat;
  ScaleRight       : TLFloat;
  SmallErr         : Boolean;
  TerminateNextTime: Boolean;
  i_               : TLInt;
begin
  Assert(AP_FP_Greater(ScaleA, 0));

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if (p[i] > n - 1) or (p[i] < i) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(i);
    end;
  SetLength(X, n, M);
  SetLength(Y, n);
  SetLength(XC, n);
  SetLength(BC, n);
  SetLength(TX, n);
  SetLength(XA, n + 1);
  SetLength(XB, n + 1);
  SetLength(TmpBuf, 2 * n + 2);

  //
  // estimate condition number, test for near singularity
  //
  Rep.r1 := CMatrixLURCond1(LUA, n);
  Rep.RInf := CMatrixLURCondInf(LUA, n);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[i, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  k := 0;
  while k <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to n - 1 do
        begin
          BC[i_] := b[i_, k];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      i := 0;
      while i <= n - 1 do
        begin
          MXB := Max(MXB, AbsComplex(BC[i]));
          Inc(i);
        end;
      if AP_FP_Eq(MXB, 0) then
        begin
          MXB := 1;
        end;
      ScaleRight := 1 / MXB;

      //
      // First, non-iterative part of solution process.
      // We use separate code for this task because
      // XDot is quite slow and we want to save time.
      //
      for i_ := 0 to n - 1 do
        begin
          XC[i_] := C_MulR(BC[i_], ScaleRight);
        end;
      CBasicLUSolve(LUA, p, ScaleA, n, XC, TX);

      //
      // Iterative refinement of xc:
      // * calculate r = bc-A*xc using extra-precise dot product
      // * solve A*y = r
      // * update x:=x+r
      //
      // This cycle is executed until one of two things happens:
      // 1. maximum number of iterations reached
      // 2. last iteration decreased error to the lower limit
      //
      if HaveA then
        begin
          NRFS := DenseSolverRFSMax(n, Rep.r1, Rep.RInf);
          TerminateNextTime := False;
          RFS := 0;
          while RFS <= NRFS - 1 do
            begin
              if TerminateNextTime then
                begin
                  Break;
                end;

              //
              // generate right part
              //
              SmallErr := True;
              for i_ := 0 to n - 1 do
                begin
                  XB[i_] := XC[i_];
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  for i_ := 0 to n - 1 do
                    begin
                      XA[i_] := C_MulR(A[i, i_], ScaleA);
                    end;
                  XA[n] := C_Complex(-1);
                  XB[n] := C_MulR(BC[i], ScaleRight);
                  XCDot(XA, XB, n + 1, TmpBuf, v, VErr);
                  Y[i] := C_Opposite(v);
                  SmallErr := SmallErr and AP_FP_Less(AbsComplex(v), 4 * VErr);
                  Inc(i);
                end;
              if SmallErr then
                begin
                  TerminateNextTime := True;
                end;

              //
              // solve and update
              //
              CBasicLUSolve(LUA, p, ScaleA, n, Y, TX);
              for i_ := 0 to n - 1 do
                begin
                  XC[i_] := C_Add(XC[i_], Y[i_]);
                end;
              Inc(RFS);
            end;
        end;

      //
      // Store xc.
      // Post-scale result.
      //
      v := C_Complex(ScaleA * MXB);
      for i_ := 0 to n - 1 do
        begin
          X[i_, k] := C_Mul(v, XC[i_]);
        end;
      Inc(k);
    end;
end;

(* ************************************************************************
  Internal Cholesky solver
  ************************************************************************ *)
procedure HPDMatrixCholeskySolveInternal(const CHA: TLComplexMatrix;
  const SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  const A: TLComplexMatrix; HaveA: Boolean; const b: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix);
var
  i                : TLInt;
  J                : TLInt;
  k                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLComplexVec;
  Y                : TLComplexVec;
  BC               : TLComplexVec;
  XA               : TLComplexVec;
  XB               : TLComplexVec;
  TX               : TLComplexVec;
  v                : TLFloat;
  VErr             : TLFloat;
  MXB              : TLFloat;
  ScaleRight       : TLFloat;
  SmallErr         : Boolean;
  TerminateNextTime: Boolean;
  i_               : TLInt;
begin
  Assert(AP_FP_Greater(SqrtScaleA, 0));

  //
  // prepare: check inputs, allocate space...
  //
  if (n <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(X, n, M);
  SetLength(Y, n);
  SetLength(XC, n);
  SetLength(BC, n);
  SetLength(TX, n + 1);
  SetLength(XA, n + 1);
  SetLength(XB, n + 1);

  //
  // estimate condition number, test for near singularity
  //
  Rep.r1 := HPDMatrixCholeskyRCond(CHA, n, IsUpper);
  Rep.RInf := Rep.r1;
  if AP_FP_Less(Rep.r1, RCondThreshold) then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[i, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  k := 0;
  while k <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to n - 1 do
        begin
          BC[i_] := b[i_, k];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      i := 0;
      while i <= n - 1 do
        begin
          MXB := Max(MXB, AbsComplex(BC[i]));
          Inc(i);
        end;
      if AP_FP_Eq(MXB, 0) then
        begin
          MXB := 1;
        end;
      ScaleRight := 1 / MXB;

      //
      // First, non-iterative part of solution process.
      // We use separate code for this task because
      // XDot is quite slow and we want to save time.
      //
      for i_ := 0 to n - 1 do
        begin
          XC[i_] := C_MulR(BC[i_], ScaleRight);
        end;
      HPDBasicCholeskySolve(CHA, SqrtScaleA, n, IsUpper, XC, TX);

      //
      // Store xc.
      // Post-scale result.
      //
      v := AP_Sqr(SqrtScaleA) * MXB;
      for i_ := 0 to n - 1 do
        begin
          X[i_, k] := C_MulR(XC[i_], v);
        end;
      Inc(k);
    end;
end;

(* ************************************************************************
  Internal subroutine.
  Returns maximum count of RFS iterations as function of:
  1. machine epsilon
  2. task size.
  3. condition number
  ************************************************************************ *)
function DenseSolverRFSMax(n: TLInt; r1: TLFloat; RInf: TLFloat)
  : TLInt;
begin
  Result := 5;
end;

(* ************************************************************************
  Internal subroutine.
  Returns maximum count of RFS iterations as function of:
  1. machine epsilon
  2. task size.
  3. norm-2 condition number
  ************************************************************************ *)
function DenseSolverRFSMaxV2(n: TLInt; r2: TLFloat): TLInt;
begin
  Result := DenseSolverRFSMax(n, 0, 0);
end;

(* ************************************************************************
  Basic LU solver for ScaleA*PLU*x = y.

  This subroutine assumes that:
  * L is well-scaled, and it is U which needs scaling by ScaleA.
  * A=PLU is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure RBasicLUSolve(const LUA: TLMatrix; const p: TLIVec;
  ScaleA: TLFloat; n: TLInt; var XB: TLVec;
  var tmp: TLVec);
var
  i: TLInt;
  v: TLFloat;
begin
  i := 0;
  while i <= n - 1 do
    begin
      if p[i] <> i then
        begin
          v := XB[i];
          XB[i] := XB[p[i]];
          XB[p[i]] := v;
        end;
      Inc(i);
    end;
  i := 1;
  while i <= n - 1 do
    begin
      v := APVDotProduct(@LUA[i][0], 0, i - 1, @XB[0], 0, i - 1);
      XB[i] := XB[i] - v;
      Inc(i);
    end;
  XB[n - 1] := XB[n - 1] / (ScaleA * LUA[n - 1, n - 1]);
  i := n - 2;
  while i >= 0 do
    begin
      APVMove(@tmp[0], i + 1, n - 1, @LUA[i][0], i + 1, n - 1, ScaleA);
      v := APVDotProduct(@tmp[0], i + 1, n - 1, @XB[0], i + 1, n - 1);
      XB[i] := (XB[i] - v) / (ScaleA * LUA[i, i]);
      Dec(i);
    end;
end;

(* ************************************************************************
  Basic Cholesky solver for ScaleA*Cholesky(A)'*x = y.

  This subroutine assumes that:
  * A*ScaleA is well scaled
  * A is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure SPDBasicCholeskySolve(const CHA: TLMatrix;
  SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  var XB: TLVec; var tmp: TLVec);
var
  i: TLInt;
  v: TLFloat;
begin

  //
  // A = L*L' or A=U'*U
  //
  if IsUpper then
    begin

      //
      // Solve U'*y=b first.
      //
      i := 0;
      while i <= n - 1 do
        begin
          XB[i] := XB[i] / (SqrtScaleA * CHA[i, i]);
          if i < n - 1 then
            begin
              v := XB[i];
              APVMove(@tmp[0], i + 1, n - 1, @CHA[i][0], i + 1, n - 1, SqrtScaleA);
              APVSub(@XB[0], i + 1, n - 1, @tmp[0], i + 1, n - 1, v);
            end;
          Inc(i);
        end;

      //
      // Solve U*x=y then.
      //
      i := n - 1;
      while i >= 0 do
        begin
          if i < n - 1 then
            begin
              APVMove(@tmp[0], i + 1, n - 1, @CHA[i][0], i + 1, n - 1, SqrtScaleA);
              v := APVDotProduct(@tmp[0], i + 1, n - 1, @XB[0], i + 1, n - 1);
              XB[i] := XB[i] - v;
            end;
          XB[i] := XB[i] / (SqrtScaleA * CHA[i, i]);
          Dec(i);
        end;
    end
  else
    begin

      //
      // Solve L*y=b first
      //
      i := 0;
      while i <= n - 1 do
        begin
          if i > 0 then
            begin
              APVMove(@tmp[0], 0, i - 1, @CHA[i][0], 0, i - 1, SqrtScaleA);
              v := APVDotProduct(@tmp[0], 0, i - 1, @XB[0], 0, i - 1);
              XB[i] := XB[i] - v;
            end;
          XB[i] := XB[i] / (SqrtScaleA * CHA[i, i]);
          Inc(i);
        end;

      //
      // Solve L'*x=y then.
      //
      i := n - 1;
      while i >= 0 do
        begin
          XB[i] := XB[i] / (SqrtScaleA * CHA[i, i]);
          if i > 0 then
            begin
              v := XB[i];
              APVMove(@tmp[0], 0, i - 1, @CHA[i][0], 0, i - 1, SqrtScaleA);
              APVSub(@XB[0], 0, i - 1, @tmp[0], 0, i - 1, v);
            end;
          Dec(i);
        end;
    end;
end;

(* ************************************************************************
  Basic LU solver for ScaleA*PLU*x = y.

  This subroutine assumes that:
  * L is well-scaled, and it is U which needs scaling by ScaleA.
  * A=PLU is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure CBasicLUSolve(const LUA: TLComplexMatrix; const p: TLIVec;
  ScaleA: TLFloat; n: TLInt; var XB: TLComplexVec;
  var tmp: TLComplexVec);
var
  i : TLInt;
  v : TLComplex;
  i_: TLInt;
begin
  i := 0;
  while i <= n - 1 do
    begin
      if p[i] <> i then
        begin
          v := XB[i];
          XB[i] := XB[p[i]];
          XB[p[i]] := v;
        end;
      Inc(i);
    end;
  i := 1;
  while i <= n - 1 do
    begin
      v := C_Complex(0.0);
      for i_ := 0 to i - 1 do
        begin
          v := C_Add(v, C_Mul(LUA[i, i_], XB[i_]));
        end;
      XB[i] := C_Sub(XB[i], v);
      Inc(i);
    end;
  XB[n - 1] := C_Div(XB[n - 1], C_MulR(LUA[n - 1, n - 1], ScaleA));
  i := n - 2;
  while i >= 0 do
    begin
      for i_ := i + 1 to n - 1 do
        begin
          tmp[i_] := C_MulR(LUA[i, i_], ScaleA);
        end;
      v := C_Complex(0.0);
      for i_ := i + 1 to n - 1 do
        begin
          v := C_Add(v, C_Mul(tmp[i_], XB[i_]));
        end;
      XB[i] := C_Div(C_Sub(XB[i], v), C_MulR(LUA[i, i], ScaleA));
      Dec(i);
    end;
end;

(* ************************************************************************
  Basic Cholesky solver for ScaleA*Cholesky(A)'*x = y.

  This subroutine assumes that:
  * A*ScaleA is well scaled
  * A is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure HPDBasicCholeskySolve(const CHA: TLComplexMatrix;
  SqrtScaleA: TLFloat; n: TLInt; IsUpper: Boolean;
  var XB: TLComplexVec; var tmp: TLComplexVec);
var
  i : TLInt;
  v : TLComplex;
  i_: TLInt;
begin

  //
  // A = L*L' or A=U'*U
  //
  if IsUpper then
    begin

      //
      // Solve U'*y=b first.
      //
      i := 0;
      while i <= n - 1 do
        begin
          XB[i] := C_Div(XB[i], C_MulR(Conj(CHA[i, i]), SqrtScaleA));
          if i < n - 1 then
            begin
              v := XB[i];
              for i_ := i + 1 to n - 1 do
                begin
                  tmp[i_] := C_MulR(Conj(CHA[i, i_]), SqrtScaleA);
                end;
              for i_ := i + 1 to n - 1 do
                begin
                  XB[i_] := C_Sub(XB[i_], C_Mul(v, tmp[i_]));
                end;
            end;
          Inc(i);
        end;

      //
      // Solve U*x=y then.
      //
      i := n - 1;
      while i >= 0 do
        begin
          if i < n - 1 then
            begin
              for i_ := i + 1 to n - 1 do
                begin
                  tmp[i_] := C_MulR(CHA[i, i_], SqrtScaleA);
                end;
              v := C_Complex(0.0);
              for i_ := i + 1 to n - 1 do
                begin
                  v := C_Add(v, C_Mul(tmp[i_], XB[i_]));
                end;
              XB[i] := C_Sub(XB[i], v);
            end;
          XB[i] := C_Div(XB[i], C_MulR(CHA[i, i], SqrtScaleA));
          Dec(i);
        end;
    end
  else
    begin

      //
      // Solve L*y=b first
      //
      i := 0;
      while i <= n - 1 do
        begin
          if i > 0 then
            begin
              for i_ := 0 to i - 1 do
                begin
                  tmp[i_] := C_MulR(CHA[i, i_], SqrtScaleA);
                end;
              v := C_Complex(0.0);
              for i_ := 0 to i - 1 do
                begin
                  v := C_Add(v, C_Mul(tmp[i_], XB[i_]));
                end;
              XB[i] := C_Sub(XB[i], v);
            end;
          XB[i] := C_Div(XB[i], C_MulR(CHA[i, i], SqrtScaleA));
          Inc(i);
        end;

      //
      // Solve L'*x=y then.
      //
      i := n - 1;
      while i >= 0 do
        begin
          XB[i] := C_Div(XB[i], C_MulR(Conj(CHA[i, i]), SqrtScaleA));
          if i > 0 then
            begin
              v := XB[i];
              for i_ := 0 to i - 1 do
                begin
                  tmp[i_] := C_MulR(Conj(CHA[i, i_]), SqrtScaleA);
                end;
              for i_ := 0 to i - 1 do
                begin
                  XB[i_] := C_Sub(XB[i_], C_Mul(v, tmp[i_]));
                end;
            end;
          Dec(i);
        end;
    end;
end; 
