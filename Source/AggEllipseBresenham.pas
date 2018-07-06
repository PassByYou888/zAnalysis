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
unit AggEllipseBresenham;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics;

type
  TAggEllipseBresenhamInterpolator = packed record
  private
    FRadiusSquared, FTwoRadiusSquared: TPointInteger;
    FDelta, FInc: TPointInteger;
    FCurF: Integer;
  public
    procedure Initialize(radius: Integer); overload;
    procedure Initialize(RX, RY: Integer); overload;

    procedure IncOperator;

    property deltax: Integer read FDelta.X;
    property deltay: Integer read FDelta.Y;
  end;

implementation


{ TAggEllipseBresenhamInterpolator }

procedure TAggEllipseBresenhamInterpolator.Initialize(radius: Integer);
begin
  FRadiusSquared := PointInteger(radius * radius, radius * radius);

  FTwoRadiusSquared.X := FRadiusSquared.X shl 1;
  FTwoRadiusSquared.Y := FRadiusSquared.Y shl 1;

  FDelta := PointInteger(0);

  FInc.X := 0;
  FInc.Y := -radius * FTwoRadiusSquared.X;
  FCurF := 0;
end;

procedure TAggEllipseBresenhamInterpolator.Initialize(RX, RY: Integer);
begin
  FRadiusSquared := PointInteger(RX * RX, RY * RY);

  FTwoRadiusSquared.X := FRadiusSquared.X shl 1;
  FTwoRadiusSquared.Y := FRadiusSquared.Y shl 1;

  FDelta := PointInteger(0);

  FInc.X := 0;
  FInc.Y := -RY * FTwoRadiusSquared.X;
  FCurF := 0;
end;

procedure TAggEllipseBresenhamInterpolator.IncOperator;
var
  mx, my, Mxy, Minimum, fx, fy, FXY: Integer;
  flag: Boolean;
begin
  mx := FCurF + FInc.X + FRadiusSquared.Y;
  fx := mx;

  if mx < 0 then
      mx := -mx;

  my := FCurF + FInc.Y + FRadiusSquared.X;
  fy := my;

  if my < 0 then
      my := -my;

  Mxy := FCurF + FInc.X + FRadiusSquared.Y + FInc.Y + FRadiusSquared.X;
  FXY := Mxy;

  if Mxy < 0 then
      Mxy := -Mxy;

  Minimum := mx;
  flag := True;

  if Minimum > my then
    begin
      Minimum := my;
      flag := False;
    end;

  FDelta := PointInteger(0);

  if Minimum > Mxy then
    begin
      Inc(FInc.X, FTwoRadiusSquared.Y);
      Inc(FInc.Y, FTwoRadiusSquared.X);

      FCurF := FXY;

      FDelta.X := 1;
      FDelta.Y := 1;

      Exit;
    end;

  if flag then
    begin
      Inc(FInc.X, FTwoRadiusSquared.Y);

      FCurF := fx;
      FDelta.X := 1;

      Exit;
    end;

  Inc(FInc.Y, FTwoRadiusSquared.X);

  FCurF := fy;
  FDelta.Y := 1;
end;

end. 
