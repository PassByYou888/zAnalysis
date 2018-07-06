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
unit AggClipLiangBarsky;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics;

function ClippingFlagsInteger(X, Y: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsDouble(X, Y: Double; ClipBox: PRectDouble): Cardinal;

function ClippingFlagsXInteger(X: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsXDouble(X: Double; ClipBox: PRectDouble): Cardinal;

function ClippingFlagsYInteger(Y: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsYDouble(Y: Double; ClipBox: PRectDouble): Cardinal;

function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer; const ClipBox: TRectInteger; X, Y: PInteger): Cardinal; overload;
function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer; const ClipBox: TRectInteger; Point: PPointInteger): Cardinal; overload;

function ClipLiangBarskyDouble(x1, y1, x2, y2: Double; ClipBox: PRectDouble; X, Y: PDouble): Cardinal;

implementation

// Determine the clipping code of the vertex according to the
// Cyrus-Beck line clipping algorithm
//
// |        |
// 0110  |  0010  | 0011
// |        |
// -------+--------+-------- ClipBox.y2
// |        |
// 0100  |  0000  | 0001
// |        |
// -------+--------+-------- ClipBox.y1
// |        |
// 1100  |  1000  | 1001
// |        |
// ClipBox.x1  ClipBox.x2
//

function ClippingFlagsInteger(X, Y: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := Cardinal(X > ClipBox.x2) or (Cardinal(Y > ClipBox.y2) shl 1) or
    (Cardinal(X < ClipBox.x1) shl 2) or (Cardinal(Y < ClipBox.y1) shl 3);
end;

function ClippingFlagsDouble(X, Y: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := Cardinal(X > ClipBox.x2) or (Cardinal(Y > ClipBox.y2) shl 1) or
    (Cardinal(X < ClipBox.x1) shl 2) or (Cardinal(Y < ClipBox.y1) shl 3);
end;

function ClippingFlagsXInteger(X: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := Cardinal(X > ClipBox.x2) or (Cardinal(X < ClipBox.x1) shl 2);
end;

function ClippingFlagsXDouble(X: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := Cardinal(X > ClipBox.x2) or (Cardinal(X < ClipBox.x1) shl 2);
end;

function ClippingFlagsYInteger(Y: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := (Cardinal(Y > ClipBox.y2) shl 1) or
    (Cardinal(Y < ClipBox.y1) shl 3);
end;

function ClippingFlagsYDouble(Y: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := (Cardinal(Y > ClipBox.y2) shl 1) or
    (Cardinal(Y < ClipBox.y1) shl 3);
end;

function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer;
  const ClipBox: TRectInteger; X, Y: PInteger): Cardinal;
const
  CNearZero = 1E-30;
var
  Inside, Outside, TempIn, TempOut, Delta: TPointDouble;
  TIn1, TIn2, TOut1: Double;
  np: Cardinal;
begin
  Delta.X := x2 - x1;
  Delta.Y := y2 - y1;

  np := 0;

  // bump off of the vertical
  if Delta.X = 0.0 then
    if x1 > ClipBox.x1 then
        Delta.X := -CNearZero
    else
        Delta.X := CNearZero;

  // bump off of the horizontal
  if Delta.Y = 0.0 then
    if y1 > ClipBox.y1 then
        Delta.Y := -CNearZero
    else
        Delta.Y := CNearZero;

  if Delta.X > 0.0 then
    begin
      // points to right
      Inside.X := ClipBox.x1;
      Outside.X := ClipBox.x2;
    end
  else
    begin
      Inside.X := ClipBox.x2;
      Outside.X := ClipBox.x1;
    end;

  if Delta.Y > 0.0 then
    begin
      // points up
      Inside.Y := ClipBox.y1;
      Outside.Y := ClipBox.y2;
    end
  else
    begin
      Inside.Y := ClipBox.y2;
      Outside.Y := ClipBox.y1;
    end;

  TempIn.X := (Inside.X - x1) / Delta.X;
  TempIn.Y := (Inside.Y - y1) / Delta.Y;

  if TempIn.X < TempIn.Y then
    begin
      // hits x first
      TIn1 := TempIn.X;
      TIn2 := TempIn.Y;
    end
  else
    begin
      // hits y first
      TIn1 := TempIn.Y;
      TIn2 := TempIn.X;
    end;

  if TIn1 <= 1.0 then
    begin
      if 0.0 < TIn1 then
        begin
          X^ := Trunc(Inside.X);
          Y^ := Trunc(Inside.Y);

          Inc(PtrComp(X), SizeOf(Integer));
          Inc(PtrComp(Y), SizeOf(Integer));
          Inc(np);
        end;

      if TIn2 <= 1.0 then
        begin
          TempOut.X := (Outside.X - x1) / Delta.X;
          TempOut.Y := (Outside.Y - y1) / Delta.Y;

          if TempOut.X < TempOut.Y then
              TOut1 := TempOut.X
          else
              TOut1 := TempOut.Y;

          if (TIn2 > 0.0) or (TOut1 > 0.0) then
            if TIn2 <= TOut1 then
              begin
                if TIn2 > 0.0 then
                  begin
                    if TempIn.X > TempIn.Y then
                      begin
                        X^ := Trunc(Inside.X);
                        Y^ := Trunc(y1 + TempIn.X * Delta.Y);
                      end
                    else
                      begin
                        X^ := Trunc(x1 + TempIn.Y * Delta.X);
                        Y^ := Trunc(Inside.Y);
                      end;

                    Inc(PtrComp(X), SizeOf(Integer));
                    Inc(PtrComp(Y), SizeOf(Integer));
                    Inc(np);
                  end;

                if TOut1 < 1.0 then
                  if TempOut.X < TempOut.Y then
                    begin
                      X^ := Trunc(Outside.X);
                      Y^ := Trunc(y1 + TempOut.X * Delta.Y);
                    end
                  else
                    begin
                      X^ := Trunc(x1 + TempOut.Y * Delta.X);
                      Y^ := Trunc(Outside.Y);
                    end
                else
                  begin
                    X^ := x2;
                    Y^ := y2;
                  end;

                Inc(PtrComp(X), SizeOf(Integer));
                Inc(PtrComp(Y), SizeOf(Integer));
                Inc(np);
              end
            else
              begin
                if TempIn.X > TempIn.Y then
                  begin
                    X^ := Trunc(Inside.X);
                    Y^ := Trunc(Outside.Y);
                  end
                else
                  begin
                    X^ := Trunc(Outside.X);
                    Y^ := Trunc(Inside.Y);
                  end;

                Inc(PtrComp(X), SizeOf(Integer));
                Inc(PtrComp(Y), SizeOf(Integer));
                Inc(np);
              end;
        end;
    end;

  Result := np;
end;

function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer;
  const ClipBox: TRectInteger; Point: PPointInteger): Cardinal;
const
  CNearZero = 1E-30;
var
  Delta, Inside, Outside, TempIn, TempOut: TPointDouble;
  TempIn1, TempIn2, TempOut1: Double;
  np: Cardinal;
begin
  Delta.X := x2 - x1;
  Delta.Y := y2 - y1;

  np := 0;

  // bump off of the vertical
  if Delta.X = 0.0 then
    if x1 > ClipBox.x1 then
        Delta.X := -CNearZero
    else
        Delta.X := CNearZero;

  // bump off of the horizontal
  if Delta.Y = 0.0 then
    if y1 > ClipBox.y1 then
        Delta.Y := -CNearZero
    else
        Delta.Y := CNearZero;

  if Delta.X > 0.0 then
    begin
      // points to right
      Inside.X := ClipBox.x1;
      Outside.X := ClipBox.x2;
    end
  else
    begin
      Inside.X := ClipBox.x2;
      Outside.X := ClipBox.x1;
    end;

  if Delta.Y > 0.0 then
    begin
      // points up
      Inside.Y := ClipBox.y1;
      Outside.Y := ClipBox.y2;
    end
  else
    begin
      Inside.Y := ClipBox.y2;
      Outside.Y := ClipBox.y1;
    end;

  TempIn.X := (Inside.X - x1) / Delta.X;
  TempIn.Y := (Inside.Y - y1) / Delta.Y;

  if TempIn.X < TempIn.Y then
    begin
      // hits x first
      TempIn1 := TempIn.X;
      TempIn2 := TempIn.Y;
    end
  else
    begin
      // hits y first
      TempIn1 := TempIn.Y;
      TempIn2 := TempIn.X;
    end;

  if TempIn1 <= 1.0 then
    begin
      if 0.0 < TempIn1 then
        begin
          Point^.X := Trunc(Inside.X);
          Point^.Y := Trunc(Inside.Y);

          Inc(Point);
          Inc(np);
        end;

      if TempIn2 <= 1.0 then
        begin
          TempOut.X := (Outside.X - x1) / Delta.X;
          TempOut.Y := (Outside.Y - y1) / Delta.Y;

          if TempOut.X < TempOut.Y then
              TempOut1 := TempOut.X
          else
              TempOut1 := TempOut.Y;

          if (TempIn2 > 0.0) or (TempOut1 > 0.0) then
            if TempIn2 <= TempOut1 then
              begin
                if TempIn2 > 0.0 then
                  begin
                    if TempIn.X > TempIn.Y then
                      begin
                        Point^.X := Trunc(Inside.X);
                        Point^.Y := Trunc(y1 + TempIn.X * Delta.Y);
                      end
                    else
                      begin
                        Point^.X := Trunc(x1 + TempIn.Y * Delta.X);
                        Point^.Y := Trunc(Inside.Y);
                      end;

                    Inc(Point);
                    Inc(np);
                  end;

                if TempOut1 < 1.0 then
                  if TempOut.X < TempOut.Y then
                    begin
                      Point^.X := Trunc(Outside.X);
                      Point^.Y := Trunc(y1 + TempOut.X * Delta.Y);
                    end
                  else
                    begin
                      Point^.X := Trunc(x1 + TempOut.Y * Delta.X);
                      Point^.Y := Trunc(Outside.Y);
                    end
                else
                  begin
                    Point^.X := x2;
                    Point^.Y := y2;
                  end;

                Inc(Point);
                Inc(np);
              end
            else
              begin
                if TempIn.X > TempIn.Y then
                  begin
                    Point^.X := Trunc(Inside.X);
                    Point^.Y := Trunc(Outside.Y);
                  end
                else
                  begin
                    Point^.X := Trunc(Outside.X);
                    Point^.Y := Trunc(Inside.Y);
                  end;

                Inc(Point);
                Inc(np);
              end;
        end;
    end;

  Result := np;
end;

function ClipLiangBarskyDouble(x1, y1, x2, y2: Double; ClipBox: PRectDouble;
  X, Y: PDouble): Cardinal;
const
  CNearZero = 1E-30;
var
  Delta, Inside, Outside, TempIn, TempOut: TPointDouble;
  TIn1, TIn2, TOut1: Double;
  np: Cardinal;
begin
  Delta.X := x2 - x1;
  Delta.Y := y2 - y1;

  np := 0;

  // bump off of the vertical
  if Delta.X = 0.0 then
    if x1 > ClipBox.x1 then
        Delta.X := -CNearZero
    else
        Delta.X := CNearZero;

  // bump off of the horizontal
  if Delta.Y = 0.0 then
    if y1 > ClipBox.y1 then
        Delta.Y := -CNearZero
    else
        Delta.Y := CNearZero;

  if Delta.X > 0.0 then
    begin
      // points to right
      Inside.X := ClipBox.x1;
      Outside.X := ClipBox.x2;
    end
  else
    begin
      Inside.X := ClipBox.x2;
      Outside.X := ClipBox.x1;
    end;

  if Delta.Y > 0.0 then
    begin
      // points up
      Inside.Y := ClipBox.y1;
      Outside.Y := ClipBox.y2;
    end
  else
    begin
      Inside.Y := ClipBox.y2;
      Outside.Y := ClipBox.y1;
    end;

  TempIn.X := (Inside.X - x1) / Delta.X;
  TempIn.Y := (Inside.Y - y1) / Delta.Y;

  if TempIn.X < TempIn.Y then
    begin
      // hits x first
      TIn1 := TempIn.X;
      TIn2 := TempIn.Y;
    end
  else
    begin
      // hits y first
      TIn1 := TempIn.Y;
      TIn2 := TempIn.X;
    end;

  if TIn1 <= 1.0 then
    begin
      if 0.0 < TIn1 then
        begin
          X^ := Inside.X;
          Y^ := Inside.Y;

          Inc(PtrComp(X), SizeOf(Integer));
          Inc(PtrComp(Y), SizeOf(Integer));
          Inc(np);
        end;

      if TIn2 <= 1.0 then
        begin
          TempOut.X := (Outside.X - x1) / Delta.X;
          TempOut.Y := (Outside.Y - y1) / Delta.Y;

          if TempOut.X < TempOut.Y then
              TOut1 := TempOut.X
          else
              TOut1 := TempOut.Y;

          if (TIn2 > 0.0) or (TOut1 > 0.0) then
            if TIn2 <= TOut1 then
              begin
                if TIn2 > 0.0 then
                  begin
                    if TempIn.X > TempIn.Y then
                      begin
                        X^ := Inside.X;
                        Y^ := y1 + TempIn.X * Delta.Y;

                      end
                    else
                      begin
                        X^ := x1 + TempIn.Y * Delta.X;
                        Y^ := Inside.Y;
                      end;

                    Inc(PtrComp(X), SizeOf(Integer));
                    Inc(PtrComp(Y), SizeOf(Integer));
                    Inc(np);
                  end;

                if TOut1 < 1.0 then
                  if TempOut.X < TempOut.Y then
                    begin
                      X^ := Outside.X;
                      Y^ := y1 + TempOut.X * Delta.Y;
                    end
                  else
                    begin
                      X^ := x1 + TempOut.Y * Delta.X;
                      Y^ := Outside.Y;
                    end
                else
                  begin
                    X^ := x2;
                    Y^ := y2;
                  end;

                Inc(PtrComp(X), SizeOf(Integer));
                Inc(PtrComp(Y), SizeOf(Integer));
                Inc(np);

              end
            else
              begin
                if TempIn.X > TempIn.Y then
                  begin
                    X^ := Inside.X;
                    Y^ := Outside.Y;
                  end
                else
                  begin
                    X^ := Outside.X;
                    Y^ := Inside.Y;
                  end;

                Inc(PtrComp(X), SizeOf(Integer));
                Inc(PtrComp(Y), SizeOf(Integer));
                Inc(np);
              end;
        end;
    end;

  Result := np;
end;

end. 
