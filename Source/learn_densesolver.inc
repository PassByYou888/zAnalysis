{ ****************************************************************************** }
{ * Dense linear system solver support, by QQ 600585@qq.com                    * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
procedure XSum(var W: TLVec; MX: TLFloat; N: TLInt;
  var R: TLFloat; var RErr: TLFloat); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function XFastPow(R: TLFloat; N: TLInt): TLFloat; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


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
procedure XDot(const A: TLVec; const B: TLVec; N: TLInt;
  var Temp: TLVec; var R: TLFloat; var RErr: TLFloat);
var
  I : TLInt;
  MX: TLFloat;
  V : TLFloat;
begin

  //
  // special cases:
  // * N=0
  //
  if N = 0 then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  MX := 0;
  I := 0;
  while I <= N - 1 do
    begin
      V := A[I] * B[I];
      Temp[I] := V;
      MX := Max(MX, AbsReal(V));
      Inc(I);
    end;
  if AP_FP_Eq(MX, 0) then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  XSum(Temp, MX, N, R, RErr);
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
procedure XCDot(const A: TLComplexVec; const B: TLComplexVec;
  N: TLInt; var Temp: TLVec; var R: TLComplex;
  var RErr: TLFloat);
var
  I    : TLInt;
  MX   : TLFloat;
  V    : TLFloat;
  RErrX: TLFloat;
  RErrY: TLFloat;
begin

  //
  // special cases:
  // * N=0
  //
  if N = 0 then
    begin
      R := C_Complex(0);
      RErr := 0;
      Exit;
    end;

  //
  // calculate real part
  //
  MX := 0;
  I := 0;
  while I <= N - 1 do
    begin
      V := A[I].X * B[I].X;
      Temp[2 * I + 0] := V;
      MX := Max(MX, AbsReal(V));
      V := -A[I].Y * B[I].Y;
      Temp[2 * I + 1] := V;
      MX := Max(MX, AbsReal(V));
      Inc(I);
    end;
  if AP_FP_Eq(MX, 0) then
    begin
      R.X := 0;
      RErrX := 0;
    end
  else
    begin
      XSum(Temp, MX, 2 * N, R.X, RErrX);
    end;

  //
  // calculate imaginary part
  //
  MX := 0;
  I := 0;
  while I <= N - 1 do
    begin
      V := A[I].X * B[I].Y;
      Temp[2 * I + 0] := V;
      MX := Max(MX, AbsReal(V));
      V := A[I].Y * B[I].X;
      Temp[2 * I + 1] := V;
      MX := Max(MX, AbsReal(V));
      Inc(I);
    end;
  if AP_FP_Eq(MX, 0) then
    begin
      R.Y := 0;
      RErrY := 0;
    end
  else
    begin
      XSum(Temp, MX, 2 * N, R.Y, RErrY);
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
procedure XSum(var W: TLVec; MX: TLFloat; N: TLInt;
  var R: TLFloat; var RErr: TLFloat);
var
  I       : TLInt;
  K       : TLInt;
  KS      : TLInt;
  V       : TLFloat;
  S       : TLFloat;
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
  if N = 0 then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  if AP_FP_Eq(MX, 0) then
    begin
      R := 0;
      RErr := 0;
      Exit;
    end;
  Assert(N < 536870912, 'XDot: N is too large!');

  //
  // Prepare
  //
  LN2 := Ln(2);
  RErr := MX * MachineEpsilon;

  //
  // 1. find S such that 0.5<=S*MX<1
  // 2. multiply W by S, so task is normalized in some sense
  // 3. S:=1/S so we can obtain original vector multiplying by S
  //
  K := Round(Ln(MX) / LN2);
  S := XFastPow(2, -K);
  while AP_FP_Greater_Eq(S * MX, 1) do
    begin
      S := 0.5 * S;
    end;
  while AP_FP_Less(S * MX, 0.5) do
    begin
      S := 2 * S;
    end;
  APVMul(@W[0], 0, N - 1, S);
  S := 1 / S;

  //
  // find Chunk=2^M such that N*Chunk<2^29
  //
  // we have chosen upper limit (2^29) with enough space left
  // to tolerate possible problems with rounding and N's close
  // to the limit, so we don't want to be very strict here.
  //
  K := Trunc(Ln(AP_Float(536870912) / N) / LN2);
  Chunk := XFastPow(2, K);
  if AP_FP_Less(Chunk, 2) then
    begin
      Chunk := 2;
    end;
  InvChunk := 1 / Chunk;

  //
  // calculate result
  //
  R := 0;
  APVMul(@W[0], 0, N - 1, Chunk);
  while True do
    begin
      S := S * InvChunk;
      AllZeros := True;
      KS := 0;
      I := 0;
      while I <= N - 1 do
        begin
          V := W[I];
          K := Trunc(V);
          if AP_FP_Neq(V, K) then
            begin
              AllZeros := False;
            end;
          W[I] := Chunk * (V - K);
          KS := KS + K;
          Inc(I);
        end;
      R := R + S * KS;
      V := AbsReal(R);
      if AllZeros or AP_FP_Eq(S * N + MX, MX) then
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
function XFastPow(R: TLFloat; N: TLInt): TLFloat;
begin
  if N > 0 then
    begin
      if N mod 2 = 0 then
        begin
          Result := AP_Sqr(XFastPow(R, N div 2));
        end
      else
        begin
          Result := R * XFastPow(R, N - 1);
        end;
      Exit;
    end;
  if N = 0 then
    begin
      Result := 1;
    end;
  if N < 0 then
    begin
      Result := XFastPow(1 / R, -N);
    end;
end;

type
  TDenseSolverReport = packed record
    R1: TLFloat;
    RInf: TLFloat;
  end;

  TDenseSolverLSReport = packed record
    R2: TLFloat;
    CX: TLMatrix;
    N: TLInt;
    K: TLInt;
  end;

procedure RMatrixSolve(const A: TLMatrix; N: TLInt;
  const B: TLVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixSolveM(const A: TLMatrix; N: TLInt;
  const B: TLMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUSolve(const LUA: TLMatrix; const P: TLIVec;
  N: TLInt; const B: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUSolveM(const LUA: TLMatrix; const P: TLIVec;
  N: TLInt; const B: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixMixedSolve(const A: TLMatrix; const LUA: TLMatrix;
  const P: TLIVec; N: TLInt; const B: TLVec;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixMixedSolveM(const A: TLMatrix; const LUA: TLMatrix;
  const P: TLIVec; N: TLInt; const B: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixSolveM(const A: TLComplexMatrix; N: TLInt;
  const B: TLComplexMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixSolve(const A: TLComplexMatrix; N: TLInt;
  const B: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixLUSolveM(const LUA: TLComplexMatrix; const P: TLIVec;
  N: TLInt; const B: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixLUSolve(const LUA: TLComplexMatrix; const P: TLIVec;
  N: TLInt; const B: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixMixedSolveM(const A: TLComplexMatrix;
  const LUA: TLComplexMatrix; const P: TLIVec; N: TLInt;
  const B: TLComplexMatrix; M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure CMatrixMixedSolve(const A: TLComplexMatrix;
  const LUA: TLComplexMatrix; const P: TLIVec; N: TLInt;
  const B: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixSolveM(const A: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixSolve(const A: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixCholeskySolveM(const CHA: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SPDMatrixCholeskySolve(const CHA: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixSolveM(const A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixSolve(const A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixCholeskySolveM(const CHA: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure HPDMatrixCholeskySolve(const CHA: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixSolveLS(const A: TLMatrix; NRows: TLInt;
  NCols: TLInt; const B: TLVec; Threshold: TLFloat;
  var Info: TLInt; var Rep: TDenseSolverLSReport; var X: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure RMatrixLUSolveInternal(const LUA: TLMatrix;
  const P: TLIVec; const ScaleA: TLFloat; N: TLInt;
  const A: TLMatrix; HaveA: Boolean; const B: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SPDMatrixCholeskySolveInternal(const CHA: TLMatrix;
  const SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  const A: TLMatrix; HaveA: Boolean; const B: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CMatrixLUSolveInternal(const LUA: TLComplexMatrix;
  const P: TLIVec; const ScaleA: TLFloat; N: TLInt;
  const A: TLComplexMatrix; HaveA: Boolean; const B: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HPDMatrixCholeskySolveInternal(const CHA: TLComplexMatrix;
  const SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  const A: TLComplexMatrix; HaveA: Boolean; const B: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DenseSolverRFSMax(N: TLInt; R1: TLFloat; RInf: TLFloat)
  : TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DenseSolverRFSMaxV2(N: TLInt; R2: TLFloat)
  : TLInt; forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RBasicLUSolve(const LUA: TLMatrix; const P: TLIVec;
  ScaleA: TLFloat; N: TLInt; var XB: TLVec;
  var Tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SPDBasicCholeskySolve(const CHA: TLMatrix;
  SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  var XB: TLVec; var Tmp: TLVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CBasicLUSolve(const LUA: TLComplexMatrix; const P: TLIVec;
  ScaleA: TLFloat; N: TLInt; var XB: TLComplexVec;
  var Tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure HPDBasicCholeskySolve(const CHA: TLComplexMatrix;
  SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  var XB: TLComplexVec; var Tmp: TLComplexVec); forward; {$IFDEF INLINE_ASM} inline; {$ENDIF}


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
procedure RMatrixSolve(const A: TLMatrix; N: TLInt;
  const B: TLVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  RMatrixSolveM(A, N, BM, 1, True, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
procedure RMatrixSolveM(const A: TLMatrix; N: TLInt;
  const B: TLMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  DA    : TLMatrix;
  EmptyA: TLMatrix;
  P     : TLIVec;
  ScaleA: TLFloat;
  I     : TLInt;
  J     : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, N, N);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  ScaleA := 0;
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          ScaleA := Max(ScaleA, AbsReal(A[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  I := 0;
  while I <= N - 1 do
    begin
      APVMove(@DA[I][0], 0, N - 1, @A[I][0], 0, N - 1);
      Inc(I);
    end;
  RMatrixLU(DA, N, N, P);
  if RFS then
    begin
      RMatrixLUSolveInternal(DA, P, ScaleA, N, A, True, B, M, Info, Rep, X);
    end
  else
    begin
      RMatrixLUSolveInternal(DA, P, ScaleA, N, EmptyA, False, B, M, Info, Rep, X);
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
procedure RMatrixLUSolve(const LUA: TLMatrix; const P: TLIVec;
  N: TLInt; const B: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  RMatrixLUSolveM(LUA, P, N, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
procedure RMatrixLUSolveM(const LUA: TLMatrix; const P: TLIVec;
  N: TLInt; const B: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  EmptyA: TLMatrix;
  I     : TLInt;
  J     : TLInt;
  ScaleA: TLFloat;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
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
  I := 0;
  while I <= N - 1 do
    begin
      J := I;
      while J <= N - 1 do
        begin
          ScaleA := Max(ScaleA, AbsReal(LUA[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  RMatrixLUSolveInternal(LUA, P, ScaleA, N, EmptyA, False, B, M, Info, Rep, X);
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
  const P: TLIVec; N: TLInt; const B: TLVec;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  RMatrixMixedSolveM(A, LUA, P, N, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
  const P: TLIVec; N: TLInt; const B: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix);
var
  ScaleA: TLFloat;
  I     : TLInt;
  J     : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
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
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          ScaleA := Max(ScaleA, AbsReal(A[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  RMatrixLUSolveInternal(LUA, P, ScaleA, N, A, True, B, M, Info, Rep, X);
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
procedure CMatrixSolveM(const A: TLComplexMatrix; N: TLInt;
  const B: TLComplexMatrix; M: TLInt; RFS: Boolean;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  DA    : TLComplexMatrix;
  EmptyA: TLComplexMatrix;
  P     : TLIVec;
  ScaleA: TLFloat;
  I     : TLInt;
  J     : TLInt;
  i_    : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, N, N);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  ScaleA := 0;
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          ScaleA := Max(ScaleA, AbsComplex(A[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  I := 0;
  while I <= N - 1 do
    begin
      for i_ := 0 to N - 1 do
        begin
          DA[I, i_] := A[I, i_];
        end;
      Inc(I);
    end;
  CMatrixLU(DA, N, N, P);
  if RFS then
    begin
      CMatrixLUSolveInternal(DA, P, ScaleA, N, A, True, B, M, Info, Rep, X);
    end
  else
    begin
      CMatrixLUSolveInternal(DA, P, ScaleA, N, EmptyA, False, B, M, Info, Rep, X);
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
procedure CMatrixSolve(const A: TLComplexMatrix; N: TLInt;
  const B: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  CMatrixSolveM(A, N, BM, 1, True, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
procedure CMatrixLUSolveM(const LUA: TLComplexMatrix; const P: TLIVec;
  N: TLInt; const B: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  EmptyA: TLComplexMatrix;
  I     : TLInt;
  J     : TLInt;
  ScaleA: TLFloat;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
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
  I := 0;
  while I <= N - 1 do
    begin
      J := I;
      while J <= N - 1 do
        begin
          ScaleA := Max(ScaleA, AbsComplex(LUA[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  CMatrixLUSolveInternal(LUA, P, ScaleA, N, EmptyA, False, B, M, Info, Rep, X);
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
procedure CMatrixLUSolve(const LUA: TLComplexMatrix; const P: TLIVec;
  N: TLInt; const B: TLComplexVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  CMatrixLUSolveM(LUA, P, N, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
  const LUA: TLComplexMatrix; const P: TLIVec; N: TLInt;
  const B: TLComplexMatrix; M: TLInt; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  ScaleA: TLFloat;
  I     : TLInt;
  J     : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
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
  I := 0;
  while I <= N - 1 do
    begin
      J := 0;
      while J <= N - 1 do
        begin
          ScaleA := Max(ScaleA, AbsComplex(A[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(ScaleA, 0) then
    begin
      ScaleA := 1;
    end;
  ScaleA := 1 / ScaleA;
  CMatrixLUSolveInternal(LUA, P, ScaleA, N, A, True, B, M, Info, Rep, X);
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
  const LUA: TLComplexMatrix; const P: TLIVec; N: TLInt;
  const B: TLComplexVec; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  CMatrixMixedSolveM(A, LUA, P, N, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
procedure SPDMatrixSolveM(const A: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  DA        : TLMatrix;
  SqrtScaleA: TLFloat;
  I         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, N, N);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  SqrtScaleA := 0;
  I := 0;
  while I <= N - 1 do
    begin
      if IsUpper then
        begin
          J1 := I;
          J2 := N - 1;
        end
      else
        begin
          J1 := 0;
          J2 := I;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsReal(A[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  SqrtScaleA := Sqrt(SqrtScaleA);
  I := 0;
  while I <= N - 1 do
    begin
      if IsUpper then
        begin
          J1 := I;
          J2 := N - 1;
        end
      else
        begin
          J1 := 0;
          J2 := I;
        end;
      APVMove(@DA[I][0], J1, J2, @A[I][0], J1, J2);
      Inc(I);
    end;
  if not SPDMatrixCholesky(DA, N, IsUpper) then
    begin
      SetLength(X, N, M);
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[I, J] := 0;
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;
  SPDMatrixCholeskySolveInternal(DA, SqrtScaleA, N, IsUpper, A, True, B, M,
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
procedure SPDMatrixSolve(const A: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  SPDMatrixSolveM(A, N, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
procedure SPDMatrixCholeskySolveM(const CHA: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLMatrix);
var
  EmptyA    : TLMatrix;
  SqrtScaleA: TLFloat;
  I         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
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
  I := 0;
  while I <= N - 1 do
    begin
      if IsUpper then
        begin
          J1 := I;
          J2 := N - 1;
        end
      else
        begin
          J1 := 0;
          J2 := I;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsReal(CHA[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  SPDMatrixCholeskySolveInternal(CHA, SqrtScaleA, N, IsUpper, EmptyA, False, B,
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
procedure SPDMatrixCholeskySolve(const CHA: TLMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLVec);
var
  BM: TLMatrix;
  XM: TLMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  SPDMatrixCholeskySolveM(CHA, N, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
procedure HPDMatrixSolveM(const A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  DA        : TLComplexMatrix;
  SqrtScaleA: TLFloat;
  I         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
  i_        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(DA, N, N);

  //
  // 1. scale matrix, max(|A[i,j]|)
  // 2. factorize scaled matrix
  // 3. solve
  //
  SqrtScaleA := 0;
  I := 0;
  while I <= N - 1 do
    begin
      if IsUpper then
        begin
          J1 := I;
          J2 := N - 1;
        end
      else
        begin
          J1 := 0;
          J2 := I;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsComplex(A[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  SqrtScaleA := Sqrt(SqrtScaleA);
  I := 0;
  while I <= N - 1 do
    begin
      if IsUpper then
        begin
          J1 := I;
          J2 := N - 1;
        end
      else
        begin
          J1 := 0;
          J2 := I;
        end;
      for i_ := J1 to J2 do
        begin
          DA[I, i_] := A[I, i_];
        end;
      Inc(I);
    end;
  if not HPDMatrixCholesky(DA, N, IsUpper) then
    begin
      SetLength(X, N, M);
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[I, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;
  HPDMatrixCholeskySolveInternal(DA, SqrtScaleA, N, IsUpper, A, True, B, M,
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
procedure HPDMatrixSolve(const A: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  HPDMatrixSolveM(A, N, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
procedure HPDMatrixCholeskySolveM(const CHA: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexMatrix; M: TLInt;
  var Info: TLInt; var Rep: TDenseSolverReport; var X: TLComplexMatrix);
var
  EmptyA    : TLComplexMatrix;
  SqrtScaleA: TLFloat;
  I         : TLInt;
  J         : TLInt;
  J1        : TLInt;
  J2        : TLInt;
begin

  //
  // prepare: check inputs, allocate space...
  //
  if (N <= 0) or (M <= 0) then
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
  I := 0;
  while I <= N - 1 do
    begin
      if IsUpper then
        begin
          J1 := I;
          J2 := N - 1;
        end
      else
        begin
          J1 := 0;
          J2 := I;
        end;
      J := J1;
      while J <= J2 do
        begin
          SqrtScaleA := Max(SqrtScaleA, AbsComplex(CHA[I, J]));
          Inc(J);
        end;
      Inc(I);
    end;
  if AP_FP_Eq(SqrtScaleA, 0) then
    begin
      SqrtScaleA := 1;
    end;
  SqrtScaleA := 1 / SqrtScaleA;
  HPDMatrixCholeskySolveInternal(CHA, SqrtScaleA, N, IsUpper, EmptyA, False, B,
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
procedure HPDMatrixCholeskySolve(const CHA: TLComplexMatrix; N: TLInt;
  IsUpper: Boolean; const B: TLComplexVec; var Info: TLInt;
  var Rep: TDenseSolverReport; var X: TLComplexVec);
var
  BM: TLComplexMatrix;
  XM: TLComplexMatrix;
  i_: TLInt;
begin
  if N <= 0 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(BM, N, 1);
  for i_ := 0 to N - 1 do
    begin
      BM[i_, 0] := B[i_];
    end;
  HPDMatrixCholeskySolveM(CHA, N, IsUpper, BM, 1, Info, Rep, XM);
  SetLength(X, N);
  for i_ := 0 to N - 1 do
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
  NCols: TLInt; const B: TLVec; Threshold: TLFloat;
  var Info: TLInt; var Rep: TDenseSolverLSReport; var X: TLVec);
var
  SV               : TLVec;
  U                : TLMatrix;
  VT               : TLMatrix;
  RP               : TLVec;
  UTB              : TLVec;
  SUTB             : TLVec;
  Tmp              : TLVec;
  TA               : TLVec;
  TX               : TLVec;
  Buf              : TLVec;
  W                : TLVec;
  I                : TLInt;
  J                : TLInt;
  NSV              : TLInt;
  KernelIdx        : TLInt;
  V                : TLFloat;
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
  SVDFailed := not RMatrixSVD(A, NRows, NCols, 1, 2, 2, SV, U, VT);
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
      I := 0;
      while I <= NCols - 1 do
        begin
          X[I] := 0;
          Inc(I);
        end;
      Rep.N := NCols;
      Rep.K := NCols;
      SetLength(Rep.CX, NCols, NCols);
      I := 0;
      while I <= NCols - 1 do
        begin
          J := 0;
          while J <= NCols - 1 do
            begin
              if I = J then
                begin
                  Rep.CX[I, J] := 1;
                end
              else
                begin
                  Rep.CX[I, J] := 0;
                end;
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R2 := 0;
      Exit;
    end;
  NSV := Min(NCols, NRows);
  if NSV = NCols then
    begin
      Rep.R2 := SV[NSV - 1] / SV[0];
    end
  else
    begin
      Rep.R2 := 0;
    end;
  Rep.N := NCols;
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
  SetLength(Tmp, NCols);
  SetLength(TA, NCols + 1);
  SetLength(TX, NCols + 1);
  SetLength(Buf, NCols + 1);
  I := 0;
  while I <= NCols - 1 do
    begin
      X[I] := 0;
      Inc(I);
    end;
  KernelIdx := NSV;
  I := 0;
  while I <= NSV - 1 do
    begin
      if AP_FP_Less_Eq(SV[I], Threshold * SV[0]) then
        begin
          KernelIdx := I;
          Break;
        end;
      Inc(I);
    end;
  Rep.K := NCols - KernelIdx;
  NRFS := DenseSolverRFSMaxV2(NCols, Rep.R2);
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
          APVMove(@RP[0], 0, NRows - 1, @B[0], 0, NRows - 1);
        end
      else
        begin
          SmallErr := True;
          I := 0;
          while I <= NRows - 1 do
            begin
              APVMove(@TA[0], 0, NCols - 1, @A[I][0], 0, NCols - 1);
              TA[NCols] := -1;
              APVMove(@TX[0], 0, NCols - 1, @X[0], 0, NCols - 1);
              TX[NCols] := B[I];
              XDot(TA, TX, NCols + 1, Buf, V, VErr);
              RP[I] := -V;
              SmallErr := SmallErr and AP_FP_Less(AbsReal(V), 4 * VErr);
              Inc(I);
            end;
          if SmallErr then
            begin
              TerminateNextTime := True;
            end;
        end;

      //
      // solve A*dx = rp
      //
      I := 0;
      while I <= NCols - 1 do
        begin
          Tmp[I] := 0;
          Inc(I);
        end;
      I := 0;
      while I <= NSV - 1 do
        begin
          UTB[I] := 0;
          Inc(I);
        end;
      I := 0;
      while I <= NRows - 1 do
        begin
          V := RP[I];
          APVAdd(@UTB[0], 0, NSV - 1, @U[I][0], 0, NSV - 1, V);
          Inc(I);
        end;
      I := 0;
      while I <= NSV - 1 do
        begin
          if I < KernelIdx then
            begin
              SUTB[I] := UTB[I] / SV[I];
            end
          else
            begin
              SUTB[I] := 0;
            end;
          Inc(I);
        end;
      I := 0;
      while I <= NSV - 1 do
        begin
          V := SUTB[I];
          APVAdd(@Tmp[0], 0, NCols - 1, @VT[I][0], 0, NCols - 1, V);
          Inc(I);
        end;

      //
      // update x:  x:=x+dx
      //
      APVAdd(@X[0], 0, NCols - 1, @Tmp[0], 0, NCols - 1);
      Inc(RFS);
    end;

  //
  // fill CX
  //
  if Rep.K > 0 then
    begin
      SetLength(Rep.CX, NCols, Rep.K);
      I := 0;
      while I <= Rep.K - 1 do
        begin
          for i_ := 0 to NCols - 1 do
            begin
              Rep.CX[i_, I] := VT[KernelIdx + I, i_];
            end;
          Inc(I);
        end;
    end;
end;

(* ************************************************************************
  Internal LU solver
  ************************************************************************ *)
procedure RMatrixLUSolveInternal(const LUA: TLMatrix;
  const P: TLIVec; const ScaleA: TLFloat; N: TLInt;
  const A: TLMatrix; HaveA: Boolean; const B: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix);
var
  I                : TLInt;
  J                : TLInt;
  K                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLVec;
  Y                : TLVec;
  BC               : TLVec;
  XA               : TLVec;
  XB               : TLVec;
  TX               : TLVec;
  V                : TLFloat;
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
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  I := 0;
  while I <= N - 1 do
    begin
      if (P[I] > N - 1) or (P[I] < I) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(I);
    end;
  SetLength(X, N, M);
  SetLength(Y, N);
  SetLength(XC, N);
  SetLength(BC, N);
  SetLength(TX, N + 1);
  SetLength(XA, N + 1);
  SetLength(XB, N + 1);

  //
  // estimate condition number, test for near singularity
  //
  Rep.R1 := RMatrixLURCond1(LUA, N);
  Rep.RInf := RMatrixLURCondInf(LUA, N);
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[I, J] := 0;
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  K := 0;
  while K <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to N - 1 do
        begin
          BC[i_] := B[i_, K];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      I := 0;
      while I <= N - 1 do
        begin
          MXB := Max(MXB, AbsReal(BC[I]));
          Inc(I);
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
      APVMove(@XC[0], 0, N - 1, @BC[0], 0, N - 1, ScaleRight);
      RBasicLUSolve(LUA, P, ScaleA, N, XC, TX);

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
          NRFS := DenseSolverRFSMax(N, Rep.R1, Rep.RInf);
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
              APVMove(@XB[0], 0, N - 1, @XC[0], 0, N - 1);
              I := 0;
              while I <= N - 1 do
                begin
                  APVMove(@XA[0], 0, N - 1, @A[I][0], 0, N - 1, ScaleA);
                  XA[N] := -1;
                  XB[N] := ScaleRight * BC[I];
                  XDot(XA, XB, N + 1, TX, V, VErr);
                  Y[I] := -V;
                  SmallErr := SmallErr and AP_FP_Less(AbsReal(V), 4 * VErr);
                  Inc(I);
                end;
              if SmallErr then
                begin
                  TerminateNextTime := True;
                end;

              //
              // solve and update
              //
              RBasicLUSolve(LUA, P, ScaleA, N, Y, TX);
              APVAdd(@XC[0], 0, N - 1, @Y[0], 0, N - 1);
              Inc(RFS);
            end;
        end;

      //
      // Store xc.
      // Post-scale result.
      //
      V := ScaleA * MXB;
      for i_ := 0 to N - 1 do
        begin
          X[i_, K] := V * XC[i_];
        end;
      Inc(K);
    end;
end;

(* ************************************************************************
  Internal Cholesky solver
  ************************************************************************ *)
procedure SPDMatrixCholeskySolveInternal(const CHA: TLMatrix;
  const SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  const A: TLMatrix; HaveA: Boolean; const B: TLMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLMatrix);
var
  I                : TLInt;
  J                : TLInt;
  K                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLVec;
  Y                : TLVec;
  BC               : TLVec;
  XA               : TLVec;
  XB               : TLVec;
  TX               : TLVec;
  V                : TLFloat;
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
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(X, N, M);
  SetLength(Y, N);
  SetLength(XC, N);
  SetLength(BC, N);
  SetLength(TX, N + 1);
  SetLength(XA, N + 1);
  SetLength(XB, N + 1);

  //
  // estimate condition number, test for near singularity
  //
  Rep.R1 := SPDMatrixCholeskyRCond(CHA, N, IsUpper);
  Rep.RInf := Rep.R1;
  if AP_FP_Less(Rep.R1, RCondThreshold) then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[I, J] := 0;
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  K := 0;
  while K <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to N - 1 do
        begin
          BC[i_] := B[i_, K];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      I := 0;
      while I <= N - 1 do
        begin
          MXB := Max(MXB, AbsReal(BC[I]));
          Inc(I);
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
      APVMove(@XC[0], 0, N - 1, @BC[0], 0, N - 1, ScaleRight);
      SPDBasicCholeskySolve(CHA, SqrtScaleA, N, IsUpper, XC, TX);

      //
      // Store xc.
      // Post-scale result.
      //
      V := AP_Sqr(SqrtScaleA) * MXB;
      for i_ := 0 to N - 1 do
        begin
          X[i_, K] := V * XC[i_];
        end;
      Inc(K);
    end;
end;

(* ************************************************************************
  Internal LU solver
  ************************************************************************ *)
procedure CMatrixLUSolveInternal(const LUA: TLComplexMatrix;
  const P: TLIVec; const ScaleA: TLFloat; N: TLInt;
  const A: TLComplexMatrix; HaveA: Boolean; const B: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix);
var
  I                : TLInt;
  J                : TLInt;
  K                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLComplexVec;
  Y                : TLComplexVec;
  BC               : TLComplexVec;
  XA               : TLComplexVec;
  XB               : TLComplexVec;
  TX               : TLComplexVec;
  TmpBuf           : TLVec;
  V                : TLComplex;
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
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  I := 0;
  while I <= N - 1 do
    begin
      if (P[I] > N - 1) or (P[I] < I) then
        begin
          Info := -1;
          Exit;
        end;
      Inc(I);
    end;
  SetLength(X, N, M);
  SetLength(Y, N);
  SetLength(XC, N);
  SetLength(BC, N);
  SetLength(TX, N);
  SetLength(XA, N + 1);
  SetLength(XB, N + 1);
  SetLength(TmpBuf, 2 * N + 2);

  //
  // estimate condition number, test for near singularity
  //
  Rep.R1 := CMatrixLURCond1(LUA, N);
  Rep.RInf := CMatrixLURCondInf(LUA, N);
  if AP_FP_Less(Rep.R1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[I, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  K := 0;
  while K <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to N - 1 do
        begin
          BC[i_] := B[i_, K];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      I := 0;
      while I <= N - 1 do
        begin
          MXB := Max(MXB, AbsComplex(BC[I]));
          Inc(I);
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
      for i_ := 0 to N - 1 do
        begin
          XC[i_] := C_MulR(BC[i_], ScaleRight);
        end;
      CBasicLUSolve(LUA, P, ScaleA, N, XC, TX);

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
          NRFS := DenseSolverRFSMax(N, Rep.R1, Rep.RInf);
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
              for i_ := 0 to N - 1 do
                begin
                  XB[i_] := XC[i_];
                end;
              I := 0;
              while I <= N - 1 do
                begin
                  for i_ := 0 to N - 1 do
                    begin
                      XA[i_] := C_MulR(A[I, i_], ScaleA);
                    end;
                  XA[N] := C_Complex(-1);
                  XB[N] := C_MulR(BC[I], ScaleRight);
                  XCDot(XA, XB, N + 1, TmpBuf, V, VErr);
                  Y[I] := C_Opposite(V);
                  SmallErr := SmallErr and AP_FP_Less(AbsComplex(V), 4 * VErr);
                  Inc(I);
                end;
              if SmallErr then
                begin
                  TerminateNextTime := True;
                end;

              //
              // solve and update
              //
              CBasicLUSolve(LUA, P, ScaleA, N, Y, TX);
              for i_ := 0 to N - 1 do
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
      V := C_Complex(ScaleA * MXB);
      for i_ := 0 to N - 1 do
        begin
          X[i_, K] := C_Mul(V, XC[i_]);
        end;
      Inc(K);
    end;
end;

(* ************************************************************************
  Internal Cholesky solver
  ************************************************************************ *)
procedure HPDMatrixCholeskySolveInternal(const CHA: TLComplexMatrix;
  const SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  const A: TLComplexMatrix; HaveA: Boolean; const B: TLComplexMatrix;
  M: TLInt; var Info: TLInt; var Rep: TDenseSolverReport;
  var X: TLComplexMatrix);
var
  I                : TLInt;
  J                : TLInt;
  K                : TLInt;
  RFS              : TLInt;
  NRFS             : TLInt;
  XC               : TLComplexVec;
  Y                : TLComplexVec;
  BC               : TLComplexVec;
  XA               : TLComplexVec;
  XB               : TLComplexVec;
  TX               : TLComplexVec;
  V                : TLFloat;
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
  if (N <= 0) or (M <= 0) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(X, N, M);
  SetLength(Y, N);
  SetLength(XC, N);
  SetLength(BC, N);
  SetLength(TX, N + 1);
  SetLength(XA, N + 1);
  SetLength(XB, N + 1);

  //
  // estimate condition number, test for near singularity
  //
  Rep.R1 := HPDMatrixCholeskyRCond(CHA, N, IsUpper);
  Rep.RInf := Rep.R1;
  if AP_FP_Less(Rep.R1, RCondThreshold) then
    begin
      I := 0;
      while I <= N - 1 do
        begin
          J := 0;
          while J <= M - 1 do
            begin
              X[I, J] := C_Complex(0);
              Inc(J);
            end;
          Inc(I);
        end;
      Rep.R1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;
  Info := 1;

  //
  // solve
  //
  K := 0;
  while K <= M - 1 do
    begin

      //
      // copy B to contiguous storage
      //
      for i_ := 0 to N - 1 do
        begin
          BC[i_] := B[i_, K];
        end;

      //
      // Scale right part:
      // * MX stores max(|Bi|)
      // * ScaleRight stores actual scaling applied to B when solving systems
      // it is chosen to make |scaleRight*b| close to 1.
      //
      MXB := 0;
      I := 0;
      while I <= N - 1 do
        begin
          MXB := Max(MXB, AbsComplex(BC[I]));
          Inc(I);
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
      for i_ := 0 to N - 1 do
        begin
          XC[i_] := C_MulR(BC[i_], ScaleRight);
        end;
      HPDBasicCholeskySolve(CHA, SqrtScaleA, N, IsUpper, XC, TX);

      //
      // Store xc.
      // Post-scale result.
      //
      V := AP_Sqr(SqrtScaleA) * MXB;
      for i_ := 0 to N - 1 do
        begin
          X[i_, K] := C_MulR(XC[i_], V);
        end;
      Inc(K);
    end;
end;

(* ************************************************************************
  Internal subroutine.
  Returns maximum count of RFS iterations as function of:
  1. machine epsilon
  2. task size.
  3. condition number
  ************************************************************************ *)
function DenseSolverRFSMax(N: TLInt; R1: TLFloat; RInf: TLFloat)
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
function DenseSolverRFSMaxV2(N: TLInt; R2: TLFloat): TLInt;
begin
  Result := DenseSolverRFSMax(N, 0, 0);
end;

(* ************************************************************************
  Basic LU solver for ScaleA*PLU*x = y.

  This subroutine assumes that:
  * L is well-scaled, and it is U which needs scaling by ScaleA.
  * A=PLU is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure RBasicLUSolve(const LUA: TLMatrix; const P: TLIVec;
  ScaleA: TLFloat; N: TLInt; var XB: TLVec;
  var Tmp: TLVec);
var
  I: TLInt;
  V: TLFloat;
begin
  I := 0;
  while I <= N - 1 do
    begin
      if P[I] <> I then
        begin
          V := XB[I];
          XB[I] := XB[P[I]];
          XB[P[I]] := V;
        end;
      Inc(I);
    end;
  I := 1;
  while I <= N - 1 do
    begin
      V := APVDotProduct(@LUA[I][0], 0, I - 1, @XB[0], 0, I - 1);
      XB[I] := XB[I] - V;
      Inc(I);
    end;
  XB[N - 1] := XB[N - 1] / (ScaleA * LUA[N - 1, N - 1]);
  I := N - 2;
  while I >= 0 do
    begin
      APVMove(@Tmp[0], I + 1, N - 1, @LUA[I][0], I + 1, N - 1, ScaleA);
      V := APVDotProduct(@Tmp[0], I + 1, N - 1, @XB[0], I + 1, N - 1);
      XB[I] := (XB[I] - V) / (ScaleA * LUA[I, I]);
      Dec(I);
    end;
end;

(* ************************************************************************
  Basic Cholesky solver for ScaleA*Cholesky(A)'*x = y.

  This subroutine assumes that:
  * A*ScaleA is well scaled
  * A is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure SPDBasicCholeskySolve(const CHA: TLMatrix;
  SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  var XB: TLVec; var Tmp: TLVec);
var
  I: TLInt;
  V: TLFloat;
begin

  //
  // A = L*L' or A=U'*U
  //
  if IsUpper then
    begin

      //
      // Solve U'*y=b first.
      //
      I := 0;
      while I <= N - 1 do
        begin
          XB[I] := XB[I] / (SqrtScaleA * CHA[I, I]);
          if I < N - 1 then
            begin
              V := XB[I];
              APVMove(@Tmp[0], I + 1, N - 1, @CHA[I][0], I + 1, N - 1, SqrtScaleA);
              APVSub(@XB[0], I + 1, N - 1, @Tmp[0], I + 1, N - 1, V);
            end;
          Inc(I);
        end;

      //
      // Solve U*x=y then.
      //
      I := N - 1;
      while I >= 0 do
        begin
          if I < N - 1 then
            begin
              APVMove(@Tmp[0], I + 1, N - 1, @CHA[I][0], I + 1, N - 1, SqrtScaleA);
              V := APVDotProduct(@Tmp[0], I + 1, N - 1, @XB[0], I + 1, N - 1);
              XB[I] := XB[I] - V;
            end;
          XB[I] := XB[I] / (SqrtScaleA * CHA[I, I]);
          Dec(I);
        end;
    end
  else
    begin

      //
      // Solve L*y=b first
      //
      I := 0;
      while I <= N - 1 do
        begin
          if I > 0 then
            begin
              APVMove(@Tmp[0], 0, I - 1, @CHA[I][0], 0, I - 1, SqrtScaleA);
              V := APVDotProduct(@Tmp[0], 0, I - 1, @XB[0], 0, I - 1);
              XB[I] := XB[I] - V;
            end;
          XB[I] := XB[I] / (SqrtScaleA * CHA[I, I]);
          Inc(I);
        end;

      //
      // Solve L'*x=y then.
      //
      I := N - 1;
      while I >= 0 do
        begin
          XB[I] := XB[I] / (SqrtScaleA * CHA[I, I]);
          if I > 0 then
            begin
              V := XB[I];
              APVMove(@Tmp[0], 0, I - 1, @CHA[I][0], 0, I - 1, SqrtScaleA);
              APVSub(@XB[0], 0, I - 1, @Tmp[0], 0, I - 1, V);
            end;
          Dec(I);
        end;
    end;
end;

(* ************************************************************************
  Basic LU solver for ScaleA*PLU*x = y.

  This subroutine assumes that:
  * L is well-scaled, and it is U which needs scaling by ScaleA.
  * A=PLU is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure CBasicLUSolve(const LUA: TLComplexMatrix; const P: TLIVec;
  ScaleA: TLFloat; N: TLInt; var XB: TLComplexVec;
  var Tmp: TLComplexVec);
var
  I : TLInt;
  V : TLComplex;
  i_: TLInt;
begin
  I := 0;
  while I <= N - 1 do
    begin
      if P[I] <> I then
        begin
          V := XB[I];
          XB[I] := XB[P[I]];
          XB[P[I]] := V;
        end;
      Inc(I);
    end;
  I := 1;
  while I <= N - 1 do
    begin
      V := C_Complex(0.0);
      for i_ := 0 to I - 1 do
        begin
          V := C_Add(V, C_Mul(LUA[I, i_], XB[i_]));
        end;
      XB[I] := C_Sub(XB[I], V);
      Inc(I);
    end;
  XB[N - 1] := C_Div(XB[N - 1], C_MulR(LUA[N - 1, N - 1], ScaleA));
  I := N - 2;
  while I >= 0 do
    begin
      for i_ := I + 1 to N - 1 do
        begin
          Tmp[i_] := C_MulR(LUA[I, i_], ScaleA);
        end;
      V := C_Complex(0.0);
      for i_ := I + 1 to N - 1 do
        begin
          V := C_Add(V, C_Mul(Tmp[i_], XB[i_]));
        end;
      XB[I] := C_Div(C_Sub(XB[I], V), C_MulR(LUA[I, I], ScaleA));
      Dec(I);
    end;
end;

(* ************************************************************************
  Basic Cholesky solver for ScaleA*Cholesky(A)'*x = y.

  This subroutine assumes that:
  * A*ScaleA is well scaled
  * A is well-conditioned, so no zero divisions or overflow may occur
  ************************************************************************ *)
procedure HPDBasicCholeskySolve(const CHA: TLComplexMatrix;
  SqrtScaleA: TLFloat; N: TLInt; IsUpper: Boolean;
  var XB: TLComplexVec; var Tmp: TLComplexVec);
var
  I : TLInt;
  V : TLComplex;
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
      I := 0;
      while I <= N - 1 do
        begin
          XB[I] := C_Div(XB[I], C_MulR(Conj(CHA[I, I]), SqrtScaleA));
          if I < N - 1 then
            begin
              V := XB[I];
              for i_ := I + 1 to N - 1 do
                begin
                  Tmp[i_] := C_MulR(Conj(CHA[I, i_]), SqrtScaleA);
                end;
              for i_ := I + 1 to N - 1 do
                begin
                  XB[i_] := C_Sub(XB[i_], C_Mul(V, Tmp[i_]));
                end;
            end;
          Inc(I);
        end;

      //
      // Solve U*x=y then.
      //
      I := N - 1;
      while I >= 0 do
        begin
          if I < N - 1 then
            begin
              for i_ := I + 1 to N - 1 do
                begin
                  Tmp[i_] := C_MulR(CHA[I, i_], SqrtScaleA);
                end;
              V := C_Complex(0.0);
              for i_ := I + 1 to N - 1 do
                begin
                  V := C_Add(V, C_Mul(Tmp[i_], XB[i_]));
                end;
              XB[I] := C_Sub(XB[I], V);
            end;
          XB[I] := C_Div(XB[I], C_MulR(CHA[I, I], SqrtScaleA));
          Dec(I);
        end;
    end
  else
    begin

      //
      // Solve L*y=b first
      //
      I := 0;
      while I <= N - 1 do
        begin
          if I > 0 then
            begin
              for i_ := 0 to I - 1 do
                begin
                  Tmp[i_] := C_MulR(CHA[I, i_], SqrtScaleA);
                end;
              V := C_Complex(0.0);
              for i_ := 0 to I - 1 do
                begin
                  V := C_Add(V, C_Mul(Tmp[i_], XB[i_]));
                end;
              XB[I] := C_Sub(XB[I], V);
            end;
          XB[I] := C_Div(XB[I], C_MulR(CHA[I, I], SqrtScaleA));
          Inc(I);
        end;

      //
      // Solve L'*x=y then.
      //
      I := N - 1;
      while I >= 0 do
        begin
          XB[I] := C_Div(XB[I], C_MulR(Conj(CHA[I, I]), SqrtScaleA));
          if I > 0 then
            begin
              V := XB[I];
              for i_ := 0 to I - 1 do
                begin
                  Tmp[i_] := C_MulR(Conj(CHA[I, i_]), SqrtScaleA);
                end;
              for i_ := 0 to I - 1 do
                begin
                  XB[i_] := C_Sub(XB[i_], C_Mul(V, Tmp[i_]));
                end;
            end;
          Dec(I);
        end;
    end;
end;
