
function VectorNorm2(const X: TLVec; const i1, i2: TLInt): TLFloat;
var
  n    : TLInt;
  ix   : TLInt;
  ABSXI: TLFloat;
  SCL  : TLFloat;
  SSQ  : TLFloat;
begin
  n := i2 - i1 + 1;
  if n < 1 then
    begin
      Result := 0;
      Exit;
    end;
  if n = 1 then
    begin
      Result := AbsReal(X[i1]);
      Exit;
    end;
  SCL := 0;
  SSQ := 1;
  ix := i1;
  while ix <= i2 do
    begin
      if AP_FP_NEq(X[ix], 0) then
        begin
          ABSXI := AbsReal(X[ix]);
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
      Inc(ix);
    end;
  Result := SCL * Sqrt(SSQ);
end;

function VectorIdxAbsMax(const X: TLVec; const i1, i2: TLInt): TLInt;
var
  i: TLInt;
  A: TLFloat;
begin
  Result := i1;
  A := AbsReal(X[Result]);
  i := i1 + 1;
  while i <= i2 do
    begin
      if AP_FP_Greater(AbsReal(X[i]), AbsReal(X[Result])) then
        begin
          Result := i;
        end;
      Inc(i);
    end;
end;

function ColumnIdxAbsMax(const X: TLMatrix; const i1, i2, J: TLInt): TLInt;
var
  i: TLInt;
  A: TLFloat;
begin
  Result := i1;
  A := AbsReal(X[Result, J]);
  i := i1 + 1;
  while i <= i2 do
    begin
      if AP_FP_Greater(AbsReal(X[i, J]), AbsReal(X[Result, J])) then
        begin
          Result := i;
        end;
      Inc(i);
    end;
end;

function RowIdxAbsMax(const X: TLMatrix; const J1, J2, i: TLInt): TLInt;
var
  J: TLInt;
  A: TLFloat;
begin
  Result := J1;
  A := AbsReal(X[i, Result]);
  J := J1 + 1;
  while J <= J2 do
    begin
      if AP_FP_Greater(AbsReal(X[i, J]), AbsReal(X[i, Result])) then
        begin
          Result := J;
        end;
      Inc(J);
    end;
end;

function UpperHessenberg1Norm(const A: TLMatrix; const i1, i2, J1, J2: TLInt; var Work: TLVec): TLFloat;
var
  i: TLInt;
  J: TLInt;
begin
  Assert(i2 - i1 = J2 - J1, 'UpperHessenberg1Norm: I2-I1<>J2-J1!');
  J := J1;
  while J <= J2 do
    begin
      Work[J] := 0;
      Inc(J);
    end;
  i := i1;
  while i <= i2 do
    begin
      J := Max(J1, J1 + i - i1 - 1);
      while J <= J2 do
        begin
          Work[J] := Work[J] + AbsReal(A[i, J]);
          Inc(J);
        end;
      Inc(i);
    end;
  Result := 0;
  J := J1;
  while J <= J2 do
    begin
      Result := Max(Result, Work[J]);
      Inc(J);
    end;
end;

procedure CopyMatrix(const A: TLMatrix; const IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; const ID1, id2, JD1, JD2: TLInt);
var
  isrc: TLInt;
  idst: TLInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  Assert(IS2 - IS1 = id2 - ID1, 'CopyMatrix: different sizes!');
  Assert(JS2 - JS1 = JD2 - JD1, 'CopyMatrix: different sizes!');
  isrc := IS1;
  while isrc <= IS2 do
    begin
      idst := isrc - IS1 + ID1;
      APVMove(@b[idst][0], JD1, JD2, @A[isrc][0], JS1, JS2);
      Inc(isrc);
    end;
end;

procedure InplaceTranspose(var A: TLMatrix; const i1, i2, J1, J2: TLInt; var Work: TLVec);
var
  i  : TLInt;
  J  : TLInt;
  IPS: TLInt;
  JPS: TLInt;
  L  : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if (i1 > i2) or (J1 > J2) then
      Exit;
  Assert(i1 - i2 = J1 - J2, 'InplaceTranspose error: incorrect array size!');
  i := i1;
  while i <= i2 - 1 do
    begin
      J := J1 + i - i1;
      IPS := i + 1;
      JPS := J1 + IPS - i1;
      L := i2 - i;
      i1_ := (IPS) - (1);
      for i_ := 1 to L do
        begin
          Work[i_] := A[i_ + i1_, J];
        end;
      i1_ := (JPS) - (IPS);
      for i_ := IPS to i2 do
        begin
          A[i_, J] := A[i, i_ + i1_];
        end;
      APVMove(@A[i][0], JPS, J2, @Work[0], 1, L);
      Inc(i);
    end;
end;

procedure CopyAndTranspose(const A: TLMatrix; IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; ID1, id2, JD1, JD2: TLInt);
var
  isrc: TLInt;
  jdst: TLInt;
  i_  : TLInt;
  i1_ : TLInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  Assert(IS2 - IS1 = JD2 - JD1, 'CopyAndTranspose: different sizes!');
  Assert(JS2 - JS1 = id2 - ID1, 'CopyAndTranspose: different sizes!');
  isrc := IS1;
  while isrc <= IS2 do
    begin
      jdst := isrc - IS1 + JD1;
      i1_ := (JS1) - (ID1);
      for i_ := ID1 to id2 do
        begin
          b[i_, jdst] := A[isrc, i_ + i1_];
        end;
      Inc(isrc);
    end;
end;

procedure MatrixVectorMultiply(const A: TLMatrix; const i1, i2, J1, J2: TLInt; const Trans: Boolean;
  const X: TLVec; const IX1, IX2: TLInt; const alpha: TLFloat;
  var Y: TLVec; const IY1, IY2: TLInt; const beta: TLFloat);
var
  i: TLInt;
  v: TLFloat;
begin
  if not Trans then
    begin

      //
      // y := alpha*A*x + beta*y;
      //
      if (i1 > i2) or (J1 > J2) then
          Exit;
      Assert(J2 - J1 = IX2 - IX1, 'MatrixVectorMultiply: A and X dont match!');
      Assert(i2 - i1 = IY2 - IY1, 'MatrixVectorMultiply: A and Y dont match!');

      //
      // beta*y
      //
      if AP_FP_Eq(beta, 0) then
        begin
          i := IY1;
          while i <= IY2 do
            begin
              Y[i] := 0;
              Inc(i);
            end;
        end
      else
        begin
          APVMul(@Y[0], IY1, IY2, beta);
        end;

      //
      // alpha*A*x
      //
      i := i1;
      while i <= i2 do
        begin
          v := APVDotProduct(@A[i][0], J1, J2, @X[0], IX1, IX2);
          Y[IY1 + i - i1] := Y[IY1 + i - i1] + alpha * v;
          Inc(i);
        end;
    end
  else
    begin

      //
      // y := alpha*A'*x + beta*y;
      //
      if (i1 > i2) or (J1 > J2) then
          Exit;
      Assert(i2 - i1 = IX2 - IX1, 'MatrixVectorMultiply: A and X dont match!');
      Assert(J2 - J1 = IY2 - IY1, 'MatrixVectorMultiply: A and Y dont match!');

      //
      // beta*y
      //
      if AP_FP_Eq(beta, 0) then
        begin
          i := IY1;
          while i <= IY2 do
            begin
              Y[i] := 0;
              Inc(i);
            end;
        end
      else
        begin
          APVMul(@Y[0], IY1, IY2, beta);
        end;

      //
      // alpha*A'*x
      //
      i := i1;
      while i <= i2 do
        begin
          v := alpha * X[IX1 + i - i1];
          APVAdd(@Y[0], IY1, IY2, @A[i][0], J1, J2, v);
          Inc(i);
        end;
    end;
end;

function Pythag2(X: TLFloat; Y: TLFloat): TLFloat;
var
  w   : TLFloat;
  XABS: TLFloat;
  YABS: TLFloat;
  Z   : TLFloat;
begin
  XABS := AbsReal(X);
  YABS := AbsReal(Y);
  w := Max(XABS, YABS);
  Z := Min(XABS, YABS);
  if AP_FP_Eq(Z, 0) then
    begin
      Result := w;
    end
  else
    begin
      Result := w * Sqrt(1 + AP_Sqr(Z / w));
    end;
end;

procedure MatrixMatrixMultiply(const A: TLMatrix; const AI1, AI2, AJ1, AJ2: TLInt; const TransA: Boolean;
  const b: TLMatrix; const BI1, BI2, BJ1, BJ2: TLInt; const TransB: Boolean;
  const alpha: TLFloat;
  var C: TLMatrix; const CI1, CI2, CJ1, CJ2: TLInt;
  const beta: TLFloat;
  var Work: TLVec);
var
  ARows: TLInt;
  ACols: TLInt;
  BRows: TLInt;
  BCols: TLInt;
  CRows: TLInt;
  CCols: TLInt;
  i    : TLInt;
  J    : TLInt;
  k    : TLInt;
  L    : TLInt;
  R    : TLInt;
  v    : TLFloat;
  i_   : TLInt;
  i1_  : TLInt;
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
      Exit;
  CRows := ARows;
  CCols := BCols;

  //
  // Test WORK
  //
  i := Max(ARows, ACols);
  i := Max(BRows, i);
  i := Max(i, BCols);
  Work[1] := 0;
  Work[i] := 0;

  //
  // Prepare C
  //
  if AP_FP_Eq(beta, 0) then
    begin
      i := CI1;
      while i <= CI2 do
        begin
          J := CJ1;
          while J <= CJ2 do
            begin
              C[i, J] := 0;
              Inc(J);
            end;
          Inc(i);
        end;
    end
  else
    begin
      i := CI1;
      while i <= CI2 do
        begin
          APVMul(@C[i][0], CJ1, CJ2, beta);
          Inc(i);
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
              v := alpha * A[L, AJ1 + R - BI1];
              k := CI1 + L - AI1;
              APVAdd(@C[k][0], CJ1, CJ2, @b[R][0], BJ1, BJ2, v);
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
                  v := APVDotProduct(@A[L][0], AJ1, AJ2, @b[R][0], BJ1, BJ2);
                  C[CI1 + L - AI1, CJ1 + R - BI1] := C[CI1 + L - AI1, CJ1 + R - BI1]
                    + alpha * v;
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
                  v := APVDotProduct(@A[L][0], AJ1, AJ2, @b[R][0], BJ1, BJ2);
                  C[CI1 + L - AI1, CJ1 + R - BI1] := C[CI1 + L - AI1, CJ1 + R - BI1]
                    + alpha * v;
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
              v := alpha * A[AI1 + R - BI1, L];
              k := CI1 + L - AJ1;
              APVAdd(@C[k][0], CJ1, CJ2, @b[R][0], BJ1, BJ2, v);
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
              i := 1;
              while i <= CRows do
                begin
                  Work[i] := 0.0;
                  Inc(i);
                end;
              L := AI1;
              while L <= AI2 do
                begin
                  v := alpha * b[R, BJ1 + L - AI1];
                  k := CJ1 + R - BI1;
                  APVAdd(@Work[0], 1, CRows, @A[L][0], AJ1, AJ2, v);
                  Inc(L);
                end;
              i1_ := (1) - (CI1);
              for i_ := CI1 to CI2 do
                begin
                  C[i_, k] := C[i_, k] + Work[i_ + i1_];
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
              k := AI2 - AI1 + 1;
              i1_ := (AI1) - (1);
              for i_ := 1 to k do
                begin
                  Work[i_] := A[i_ + i1_, L];
                end;
              R := BI1;
              while R <= BI2 do
                begin
                  v := APVDotProduct(@Work[0], 1, k, @b[R][0], BJ1, BJ2);
                  C[CI1 + L - AJ1, CJ1 + R - BI1] := C[CI1 + L - AJ1, CJ1 + R - BI1]
                    + alpha * v;
                  Inc(R);
                end;
              Inc(L);
            end;
          Exit;
        end;
    end;
end; 
