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
unit AggRendererPrimitives;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggRendererBase,
  AggColor32,
  AggDdaLine,
  AggEllipseBresenham;

type
  TAggRendererPrimitives = class
  private
    FRenderBase: TAggRendererBase;
    FCurrent: TPointInteger;
    procedure SetFillColor(Value: TAggColor);
    procedure SetLineColor(Value: TAggColor);
  protected
    FFillColor, FLineColor: TAggColor;
  public
    constructor Create(RendBase: TAggRendererBase);

    function Coord(C: Double): Integer;

    procedure Rectangle(x1, y1, x2, y2: Integer); overload;
    procedure Rectangle(Rect: TRectInteger); overload;
    procedure SolidRectangle(x1, y1, x2, y2: Integer); overload;
    procedure SolidRectangle(Rect: TRectInteger); overload;
    procedure OutlinedRectangle(x1, y1, x2, y2: Integer); overload;
    procedure OutlinedRectangle(Rect: TRectInteger); overload;

    procedure Ellipse(X, Y, RX, RY: Integer);
    procedure SolidEllipse(X, Y, RX, RY: Integer);
    procedure OutlinedEllipse(X, Y, RX, RY: Integer);

    procedure Line(x1, y1, x2, y2: Integer; Last: Boolean = False); overload;
    procedure Line(Point1, Point2: TPointInteger; Last: Boolean = False); overload;

    procedure MoveTo(X, Y: Integer); overload;
    procedure MoveTo(Point: TPointInteger); overload;
    procedure LineTo(X, Y: Integer; Last: Boolean = False); overload;
    procedure LineTo(Point: TPointInteger; Last: Boolean = False); overload;

    property RenderBase: TAggRendererBase read FRenderBase;

    property FillColor: TAggColor read FFillColor write SetFillColor;
    property LineColor: TAggColor read FLineColor write SetLineColor;
  end;

implementation


{ TAggRendererPrimitives }

constructor TAggRendererPrimitives.Create(RendBase: TAggRendererBase);
begin
  Assert(RendBase is TAggRendererBase);

  FRenderBase := RendBase;

  FCurrent := PointInteger(0);
end;

function TAggRendererPrimitives.Coord(C: Double): Integer;
begin
  Result := Trunc(C * CAggSubpixelSize);
end;

procedure TAggRendererPrimitives.SetFillColor(Value: TAggColor);
begin
  FFillColor := Value;
end;

procedure TAggRendererPrimitives.SetLineColor(Value: TAggColor);
begin
  FLineColor := Value;
end;

procedure TAggRendererPrimitives.Rectangle(x1, y1, x2, y2: Integer);
begin
  with FRenderBase do
    begin
      BlendHorizontalLine(x1, y1, x2 - 1, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x2, y1, y2 - 1, @FLineColor, CAggCoverFull);
      BlendHorizontalLine(x1 + 1, y2, x2, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x1, y1 + 1, y2, @FLineColor, CAggCoverFull);
    end;
end;

procedure TAggRendererPrimitives.Rectangle(Rect: TRectInteger);
begin
  with Rect, FRenderBase do
    begin
      BlendHorizontalLine(x1, y1, x2 - 1, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x2, y1, y2 - 1, @FLineColor, CAggCoverFull);
      BlendHorizontalLine(x1 + 1, y2, x2, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x1, y1 + 1, y2, @FLineColor, CAggCoverFull);
    end;
end;

procedure TAggRendererPrimitives.SolidRectangle(x1, y1, x2, y2: Integer);
begin
  FRenderBase.BlendBar(x1, y1, x2, y2, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.SolidRectangle(Rect: TRectInteger);
begin
  FRenderBase.BlendBar(Rect.x1, Rect.y1, Rect.x2, Rect.y2, @FFillColor,
    CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedRectangle(x1, y1, x2, y2: Integer);
begin
  Rectangle(x1, y1, x2, y2);
  FRenderBase.BlendBar(x1 + 1, y1 + 1, x2 - 1, y2 - 1, @FFillColor,
    CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedRectangle(Rect: TRectInteger);
begin
  Rectangle(Rect.x1, Rect.y1, Rect.x2, Rect.y2);
  FRenderBase.BlendBar(Rect.x1 + 1, Rect.y1 + 1, Rect.x2 - 1, Rect.y2 - 1,
    @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.Ellipse(X, Y, RX, RY: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta: TPointInteger;
begin
  Ei.Initialize(RX, RY);

  Delta := PointInteger(0, -RY);

  repeat
    Inc(Delta.X, Ei.deltax);
    Inc(Delta.Y, Ei.deltay);

    with FRenderBase do
      begin
        BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);
        BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
        BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
        BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);
      end;

    Ei.IncOperator;
  until Delta.Y >= 0;
end;

procedure TAggRendererPrimitives.SolidEllipse(X, Y, RX, RY: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta, LastDelta: TPointInteger;
begin
  Ei.Initialize(RX, RY);

  Delta := PointInteger(0, -RY);
  LastDelta := Delta;

  repeat
    Inc(Delta.X, Ei.deltax);
    Inc(Delta.Y, Ei.deltay);

    if Delta.Y <> LastDelta.Y then
      begin
        FRenderBase.BlendHorizontalLine(X - LastDelta.X, Y + LastDelta.Y,
          X + LastDelta.X, @FFillColor, CAggCoverFull);
        FRenderBase.BlendHorizontalLine(X - LastDelta.X, Y - LastDelta.Y,
          X + LastDelta.X, @FFillColor, CAggCoverFull);
      end;

    LastDelta := Delta;

    Ei.IncOperator;
  until Delta.Y >= 0;

  FRenderBase.BlendHorizontalLine(X - LastDelta.X, Y + LastDelta.Y,
    X + LastDelta.X, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedEllipse(X, Y, RX, RY: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta: TPointInteger;
begin
  Ei.Initialize(RX, RY);

  Delta := PointInteger(0, -RY);
  repeat
    Inc(Delta.X, Ei.deltax);
    Inc(Delta.Y, Ei.deltay);

    FRenderBase.BlendPixel(X + Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(X + Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(X - Delta.X, Y - Delta.Y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(X - Delta.X, Y + Delta.Y, @FLineColor, CAggCoverFull);

    if (Ei.deltay <> 0) and (Delta.X <> 0) then
      begin
        FRenderBase.BlendHorizontalLine(X - Delta.X + 1, Y + Delta.Y,
          X + Delta.X - 1, @FFillColor, CAggCoverFull);
        FRenderBase.BlendHorizontalLine(X - Delta.X + 1, Y - Delta.Y,
          X + Delta.X - 1, @FFillColor, CAggCoverFull);
      end;

    Ei.IncOperator;
  until Delta.Y >= 0;
end;

procedure TAggRendererPrimitives.Line(x1, y1, x2, y2: Integer;
  Last: Boolean = False);
var
  Li: TAggLineBresenhamInterpolator;
  Len: Cardinal;
begin
  Li.Initialize(x1, y1, x2, y2);

  Len := Li.length;

  if Len = 0 then
    begin
      if Last then
          FRenderBase.BlendPixel(Li.LineLowResolution(x1),
          Li.LineLowResolution(y1), @FLineColor, CAggCoverFull);

      Exit;
    end;

  if Last then
      Inc(Len);

  if Li.IsVer then
    repeat
      FRenderBase.BlendPixel(Li.x2, Li.y1, @FLineColor, CAggCoverFull);

      Li.Vstep;

      Dec(Len);
    until Len = 0
  else
    repeat
      FRenderBase.BlendPixel(Li.x1, Li.y2, @FLineColor, CAggCoverFull);

      Li.HStep;

      Dec(Len);
    until Len = 0;
end;

procedure TAggRendererPrimitives.Line(Point1, Point2: TPointInteger; Last: Boolean);
var
  Li: TAggLineBresenhamInterpolator;
  Len: Cardinal;
begin
  Li.Initialize(Point1, Point2);

  Len := Li.length;

  if Len = 0 then
    begin
      if Last then
          FRenderBase.BlendPixel(Li.LineLowResolution(Point1.X),
          Li.LineLowResolution(Point1.Y), @FLineColor, CAggCoverFull);

      Exit;
    end;

  if Last then
      Inc(Len);

  if Li.IsVer then
    repeat
      FRenderBase.BlendPixel(Li.x2, Li.y1, @FLineColor, CAggCoverFull);

      Li.Vstep;

      Dec(Len);
    until Len = 0
  else
    repeat
      FRenderBase.BlendPixel(Li.x1, Li.y2, @FLineColor, CAggCoverFull);

      Li.HStep;

      Dec(Len);
    until Len = 0;
end;

procedure TAggRendererPrimitives.MoveTo(X, Y: Integer);
begin
  FCurrent := PointInteger(X, Y);
end;

procedure TAggRendererPrimitives.MoveTo(Point: TPointInteger);
begin
  FCurrent := Point;
end;

procedure TAggRendererPrimitives.LineTo(X, Y: Integer; Last: Boolean = False);
begin
  Line(FCurrent.X, FCurrent.Y, X, Y, Last);
  FCurrent := PointInteger(X, Y);
end;

procedure TAggRendererPrimitives.LineTo(Point: TPointInteger; Last: Boolean);
begin
  Line(FCurrent, Point, Last);
  FCurrent := Point;
end;

end. 
