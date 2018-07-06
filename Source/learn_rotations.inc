{ ****************************************************************************** }
{ * matrix rotations support, by QQ 600585@qq.com                              * }
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
  Application of a sequence of elementary rotations to a matrix

  The algorithm pre-multiplies the matrix by a sequence of rotation
  transformations which is given by arrays C and S. Depending on the value
  of the IsForward parameter either 1 and 2, 3 and 4 and so on (if IsForward=true)
  rows are rotated, or the rows N and N-1, N-2 and N-3 and so on, are rotated.

  Not the whole matrix but only a part of it is transformed (rows from M1 to
  M2, columns from N1 to N2). Only the elements of this submatrix are changed.

  Input parameters:
  IsForward   -   the sequence of the rotation application.
  M1,M2       -   the range of rows to be transformed.
  N1, N2      -   the range of columns to be transformed.
  C,S         -   transformation coefficients.
  Array whose index ranges within [1..M2-M1].
  A           -   processed matrix.
  WORK        -   working array whose index ranges within [N1..N2].

  Output parameters:
  A           -   transformed matrix.
  ************************************************************************ *)
procedure ApplyRotationsFromTheLeft(IsForward: Boolean; m1: TLInt; m2: TLInt; N1: TLInt; N2: TLInt;
  const C: TLVec; const s: TLVec; var A: TLMatrix; var Work: TLVec);
var
  J    : TLInt;
  JP1  : TLInt;
  CTemp: TLFloat;
  STEMP: TLFloat;
  Temp : TLFloat;
begin
  if (m1 > m2) or (N1 > N2) then
    begin
      Exit;
    end;

  //
  // Form  P * A
  //
  if IsForward then
    begin
      if N1 <> N2 then
        begin

          //
          // Common case: N1<>N2
          //
          J := m1;
          while J <= m2 - 1 do
            begin
              CTemp := C[J - m1 + 1];
              STEMP := s[J - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := J + 1;
                  APVMove(@Work[0], N1, N2, @A[JP1][0], N1, N2, CTemp);
                  APVSub(@Work[0], N1, N2, @A[J][0], N1, N2, STEMP);
                  APVMul(@A[J][0], N1, N2, CTemp);
                  APVAdd(@A[J][0], N1, N2, @A[JP1][0], N1, N2, STEMP);
                  APVMove(@A[JP1][0], N1, N2, @Work[0], N1, N2);
                end;
              Inc(J);
            end;
        end
      else
        begin

          //
          // Special case: N1=N2
          //
          J := m1;
          while J <= m2 - 1 do
            begin
              CTemp := C[J - m1 + 1];
              STEMP := s[J - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := A[J + 1, N1];
                  A[J + 1, N1] := CTemp * Temp - STEMP * A[J, N1];
                  A[J, N1] := STEMP * Temp + CTemp * A[J, N1];
                end;
              Inc(J);
            end;
        end;
    end
  else
    begin
      if N1 <> N2 then
        begin

          //
          // Common case: N1<>N2
          //
          J := m2 - 1;
          while J >= m1 do
            begin
              CTemp := C[J - m1 + 1];
              STEMP := s[J - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := J + 1;
                  APVMove(@Work[0], N1, N2, @A[JP1][0], N1, N2, CTemp);
                  APVSub(@Work[0], N1, N2, @A[J][0], N1, N2, STEMP);
                  APVMul(@A[J][0], N1, N2, CTemp);
                  APVAdd(@A[J][0], N1, N2, @A[JP1][0], N1, N2, STEMP);
                  APVMove(@A[JP1][0], N1, N2, @Work[0], N1, N2);
                end;
              Dec(J);
            end;
        end
      else
        begin

          //
          // Special case: N1=N2
          //
          J := m2 - 1;
          while J >= m1 do
            begin
              CTemp := C[J - m1 + 1];
              STEMP := s[J - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := A[J + 1, N1];
                  A[J + 1, N1] := CTemp * Temp - STEMP * A[J, N1];
                  A[J, N1] := STEMP * Temp + CTemp * A[J, N1];
                end;
              Dec(J);
            end;
        end;
    end;
end;

(* ************************************************************************
  Application of a sequence of elementary rotations to a matrix

  The algorithm post-multiplies the matrix by a sequence of rotation
  transformations which is given by arrays C and S. Depending on the value
  of the IsForward parameter either 1 and 2, 3 and 4 and so on (if IsForward=true)
  rows are rotated, or the rows N and N-1, N-2 and N-3 and so on are rotated.

  Not the whole matrix but only a part of it is transformed (rows from M1
  to M2, columns from N1 to N2). Only the elements of this submatrix are changed.

  Input parameters:
  IsForward   -   the sequence of the rotation application.
  M1,M2       -   the range of rows to be transformed.
  N1, N2      -   the range of columns to be transformed.
  C,S         -   transformation coefficients.
  Array whose index ranges within [1..N2-N1].
  A           -   processed matrix.
  WORK        -   working array whose index ranges within [M1..M2].

  Output parameters:
  A           -   transformed matrix.
  ************************************************************************ *)
procedure ApplyRotationsFromTheRight(IsForward: Boolean; m1: TLInt; m2: TLInt; N1: TLInt; N2: TLInt;
  const C: TLVec; const s: TLVec; var A: TLMatrix; var Work: TLVec);
var
  J    : TLInt;
  JP1  : TLInt;
  CTemp: TLFloat;
  STEMP: TLFloat;
  Temp : TLFloat;
  i_   : TLInt;
begin

  //
  // Form A * P'
  //
  if IsForward then
    begin
      if m1 <> m2 then
        begin

          //
          // Common case: M1<>M2
          //
          J := N1;
          while J <= N2 - 1 do
            begin
              CTemp := C[J - N1 + 1];
              STEMP := s[J - N1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := J + 1;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := CTemp * A[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := Work[i_] - STEMP * A[i_, J];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      A[i_, J] := CTemp * A[i_, J];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      A[i_, J] := A[i_, J] + STEMP * A[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      A[i_, JP1] := Work[i_];
                    end;
                end;
              Inc(J);
            end;
        end
      else
        begin

          //
          // Special case: M1=M2
          //
          J := N1;
          while J <= N2 - 1 do
            begin
              CTemp := C[J - N1 + 1];
              STEMP := s[J - N1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := A[m1, J + 1];
                  A[m1, J + 1] := CTemp * Temp - STEMP * A[m1, J];
                  A[m1, J] := STEMP * Temp + CTemp * A[m1, J];
                end;
              Inc(J);
            end;
        end;
    end
  else
    begin
      if m1 <> m2 then
        begin

          //
          // Common case: M1<>M2
          //
          J := N2 - 1;
          while J >= N1 do
            begin
              CTemp := C[J - N1 + 1];
              STEMP := s[J - N1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := J + 1;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := CTemp * A[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := Work[i_] - STEMP * A[i_, J];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      A[i_, J] := CTemp * A[i_, J];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      A[i_, J] := A[i_, J] + STEMP * A[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      A[i_, JP1] := Work[i_];
                    end;
                end;
              Dec(J);
            end;
        end
      else
        begin

          //
          // Special case: M1=M2
          //
          J := N2 - 1;
          while J >= N1 do
            begin
              CTemp := C[J - N1 + 1];
              STEMP := s[J - N1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := A[m1, J + 1];
                  A[m1, J + 1] := CTemp * Temp - STEMP * A[m1, J];
                  A[m1, J] := STEMP * Temp + CTemp * A[m1, J];
                end;
              Dec(J);
            end;
        end;
    end;
end;

(* ************************************************************************
  The subroutine generates the elementary rotation, so that:

  [  CS  SN  ]  .  [ F ]  =  [ R ]
  [ -SN  CS  ]     [ G ]     [ 0 ]

  CS**2 + SN**2 = 1
  ************************************************************************ *)
procedure GenerateRotation(F: TLFloat; g: TLFloat; var cs: TLFloat; var sn: TLFloat; var R: TLFloat);
var
  f1: TLFloat;
  G1: TLFloat;
begin
  if AP_FP_Eq(g, 0) then
    begin
      cs := 1;
      sn := 0;
      R := F;
    end
  else
    begin
      if AP_FP_Eq(F, 0) then
        begin
          cs := 0;
          sn := 1;
          R := g;
        end
      else
        begin
          f1 := F;
          G1 := g;
          if AP_FP_Greater(AbsReal(f1), AbsReal(G1)) then
            begin
              R := AbsReal(f1) * Sqrt(1 + AP_Sqr(G1 / f1));
            end
          else
            begin
              R := AbsReal(G1) * Sqrt(1 + AP_Sqr(f1 / G1));
            end;
          cs := f1 / R;
          sn := G1 / R;
          if AP_FP_Greater(AbsReal(F), AbsReal(g)) and AP_FP_Less(cs, 0) then
            begin
              cs := -cs;
              sn := -sn;
              R := -R;
            end;
        end;
    end;
end; 
