{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
  //    Maintained by Christian-W. Budde (Christian@pcjv.de)                    //
  //    Copyright (c) 2012-2017                                                 //
  //                                                                            //
  //  Based on:                                                                 //
  //    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
  //    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
  //                                                                            //
  //  Original License:                                                         //
  //    Anti-Grain Geometry - Version 2.4 (Public License)                      //
  //    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
  //    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
  //                                                                            //
  //  Permission to copy, use, modify, sell and distribute this software        //
  //  is granted provided this copyright notice appears in all copies.          //
  //  This software is provided "as is" without express or implied              //
  //  warranty, and with no claim as to its suitability for any purpose.        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)
unit AggSimulEq;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics;

procedure SwapArrays(a1, a2: PDouble; n: Cardinal);
function MatrixPivot(M: PDouble; Row, Rows, Cols: Cardinal): Integer;
function SimulEqSolve(Left, Right, EqResult: PDouble; Size, RightCols: Cardinal): Boolean;

implementation

procedure SwapArrays(a1, a2: PDouble; n: Cardinal);
var
  i: Cardinal;
  tmp: Double;
begin
  i := 0;

  while i < n do
    begin
      tmp := a1^;
      a1^ := a2^;
      a2^ := tmp;

      Inc(PtrComp(a1), SizeOf(Double));
      Inc(PtrComp(a2), SizeOf(Double));
      Inc(i);
    end;
end;

function MatrixPivot(M: PDouble; Row, Rows, Cols: Cardinal): Integer;
var
  i: Cardinal;
  k: Integer;
  MaxVal, tmp: Double;
begin
  k := Row;

  MaxVal := -1.0;

  i := Row;

  while i < Rows do
    begin
      tmp := Abs(PDouble(PtrComp(M) + (i * Cols + Row) * SizeOf(Double))^);

      if (tmp > MaxVal) and (tmp <> 0.0) then
        begin
          MaxVal := tmp;

          k := i;
        end;

      Inc(i);
    end;

  if PDouble(PtrComp(M) + (k * Cols + Row) * SizeOf(Double))^ = 0.0 then
    begin
      Result := -1;

      Exit;
    end;

  if k <> Row then
    begin
      SwapArrays(PDouble(PtrComp(M) + k * Cols * SizeOf(Double)),
        PDouble(PtrComp(M) + Row * Cols * SizeOf(Double)), Cols);

      Result := k;

      Exit;
    end;

  Result := 0;
end;

function SimulEqSolve(Left, Right, EqResult: PDouble; Size,
  RightCols: Cardinal): Boolean;
var
  M: Integer;

  i, J, k, Adx: Cardinal;

  a1: Double;
  tmp: PDouble;
begin
  Result := False;

  // Alloc
  Adx := Size + RightCols;

  AggGetMem(Pointer(tmp), Size * Adx * SizeOf(Double));
  try
    for i := 0 to Size - 1 do
      begin
        for J := 0 to Size - 1 do
            PDouble(PtrComp(tmp) + (i * Adx + J) * SizeOf(Double))^ :=
            PDouble(PtrComp(Left) + (i * Size + J) * SizeOf(Double))^;

        for J := 0 to RightCols - 1 do
            PDouble(PtrComp(tmp) + (i * Adx + Size + J) * SizeOf(Double))^ :=
            PDouble(PtrComp(Right) + (i * RightCols + J) * SizeOf(Double))^;
      end;

    for k := 0 to Size - 1 do
      begin
        if MatrixPivot(tmp, k, Size, Size + RightCols) < 0 then
            Exit;

        a1 := PDouble(PtrComp(tmp) + (k * Adx + k) * SizeOf(Double))^;
        J := k;

        while J < Size + RightCols do
          begin
            PDouble(PtrComp(tmp) + (k * Adx + J) * SizeOf(Double))^ :=
              PDouble(PtrComp(tmp) + (k * Adx + J) * SizeOf(Double))^ / a1;

            Inc(J);
          end;

        i := k + 1;

        while i < Size do
          begin
            a1 := PDouble(PtrComp(tmp) + (i * Adx + k) * SizeOf(Double))^;
            J := k;

            while J < Size + RightCols do
              begin
                PDouble(PtrComp(tmp) + (i * Adx + J) * SizeOf(Double))^ :=
                  PDouble(PtrComp(tmp) + (i * Adx + J) * SizeOf(Double))^ - a1 *
                  PDouble(PtrComp(tmp) + (k * Adx + J) * SizeOf(Double))^;

                Inc(J);
              end;

            Inc(i);
          end;
      end;

    for k := 0 to RightCols - 1 do
      begin
        M := Integer(Size - 1);

        while M >= 0 do
          begin
            PDouble(PtrComp(EqResult) + (M * RightCols + k) * SizeOf(Double))^ :=
              PDouble(PtrComp(tmp) + (M * Adx + Size + k) * SizeOf(Double))^;

            J := M + 1;

            while J < Size do
              begin
                PDouble(PtrComp(EqResult) + (M * RightCols + k) * SizeOf(Double))^ :=
                  PDouble(PtrComp(EqResult) + (M * RightCols + k) * SizeOf(Double))^ -
                  (PDouble(PtrComp(tmp) + (M * Adx + J) * SizeOf(Double))^ *
                  PDouble(PtrComp(EqResult) + (J * RightCols + k) * SizeOf(Double))^);

                Inc(J);
              end;

            Dec(M);
          end;
      end;

    Result := True;
  finally
      AggFreeMem(Pointer(tmp), Size * Adx * SizeOf(Double));
  end;
end;

end. 