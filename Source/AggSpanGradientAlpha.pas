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
unit AggSpanGradientAlpha;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggColor32,
  AggArray,
  AggSpanGradient,
  AggSpanInterpolatorLinear,
  AggSpanConverter;

type
  TAggGradientAlpha = class(TAggPodAutoArray);

  TAggSpanGradientAlpha = class(TAggSpanConvertor)
  private
    FDownscaleShift: Cardinal;

    FInterpolator: TAggSpanInterpolator;
    FGradientFunction: TAggCustomGradient;
    FAlphaFunction: TAggGradientAlpha;

    FD1, FD2: Integer;
    function GetD1: Double;
    function GetD2: Double;
    procedure SetD1(Value: Double);
    procedure SetD2(Value: Double);
  public
    constructor Create; overload;
    constructor Create(inter: TAggSpanInterpolator;
      Gradient: TAggCustomGradient; Alpha_fnc: TAggGradientAlpha;
      d1, d2: Double); overload;

    procedure Convert(Span: PAggColor; X, Y: Integer; Len: Cardinal); override;

    property d1: Double read GetD1 write SetD1;
    property d2: Double read GetD2 write SetD2;

    property Interpolator: TAggSpanInterpolator read FInterpolator write FInterpolator;
    property GradientFunction: TAggCustomGradient read FGradientFunction write FGradientFunction;
    property AlphaFunction: TAggGradientAlpha read FAlphaFunction write FAlphaFunction;
  end;

  TAggGradientAlphaX = class
  public
    function ArrayOperator(X: TAggColor): TAggColor;
  end;

  TAggGradientAlphaXU8 = class
  public
    function ArrayOperator(X: Integer): Int8u;
  end;

  TAggGradientAlphaOneMinusXU8 = class
  public
    function ArrayOperator(X: Integer): Int8u;
  end;

implementation


{ TAggSpanGradientAlpha }

constructor TAggSpanGradientAlpha.Create;
begin
  FInterpolator := nil;
  FGradientFunction := nil;
  FAlphaFunction := nil;

  FDownscaleShift := 0;

  FD1 := 0;
  FD2 := 0;
end;

constructor TAggSpanGradientAlpha.Create(inter: TAggSpanInterpolator;
  Gradient: TAggCustomGradient; Alpha_fnc: TAggGradientAlpha; d1, d2: Double);
begin
  FInterpolator := inter;
  FGradientFunction := Gradient;
  FAlphaFunction := Alpha_fnc;

  FDownscaleShift := FInterpolator.SubpixelShift - CAggGradientSubpixelShift;

  FD1 := Trunc(d1 * CAggGradientSubpixelSize);
  FD2 := Trunc(d2 * CAggGradientSubpixelSize);
end;

function TAggSpanGradientAlpha.GetD1;
begin
  Result := FD1 / CAggGradientSubpixelSize;
end;

function TAggSpanGradientAlpha.GetD2;
begin
  Result := FD2 / CAggGradientSubpixelSize;
end;

procedure TAggSpanGradientAlpha.SetD1(Value: Double);
begin
  FD1 := Trunc(Value * CAggGradientSubpixelSize);
end;

procedure TAggSpanGradientAlpha.SetD2(Value: Double);
begin
  FD2 := Trunc(Value * CAggGradientSubpixelSize);
end;

procedure TAggSpanGradientAlpha.Convert(Span: PAggColor; X, Y: Integer;
  Len: Cardinal);
var
  DD, d: Integer;
begin
  DD := FD2 - FD1;

  if DD < 1 then
      DD := 1;

  FInterpolator.SetBegin(X + 0.5, Y + 0.5, Len);

  repeat
    FInterpolator.Coordinates(@X, @Y);

    d := FGradientFunction.Calculate(ShrInt32(X, FDownscaleShift),
      ShrInt32(Y, FDownscaleShift), FD2);

    d := ((d - FD1) * FAlphaFunction.Size) div DD;

    if d < 0 then
        d := 0;

    if d >= FAlphaFunction.Size then
        d := FAlphaFunction.Size - 1;

    Span.Rgba8.A := PInt8u(FAlphaFunction.ArrayOperator(d))^;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    FInterpolator.IncOperator;

    Dec(Len);
  until Len = 0;
end;

{ TAggGradientAlphaX }

function TAggGradientAlphaX.ArrayOperator(X: TAggColor): TAggColor;
begin
  Result := X;
end;

{ TAggGradientAlphaXU8 }

function TAggGradientAlphaXU8.ArrayOperator(X: Integer): Int8u;
begin
  Result := Int8u(X);
end;

{ TAggGradientAlphaOneMinusXU8 }

function TAggGradientAlphaOneMinusXU8.ArrayOperator(X: Integer): Int8u;
begin
  Result := Int8u(255 - X);
end;

end. 
