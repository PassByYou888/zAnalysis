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
unit AggDdaLine;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics;

const
  CAggSubpixelShift = 8;
  CAggSubpixelSize  = 1 shl CAggSubpixelShift;
  CAggSubpixelMask  = CAggSubpixelSize - 1;

type
  TAggDdaLineInterpolator = packed record
  private
    fy, FInc, FDeltaY, FFractionShift, YShift: Integer;
    function GetY: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    procedure Initialize(fs: Integer; YS: Integer = 0); overload;
    procedure Initialize(y1, y2: Integer; Count: Cardinal; fs: Integer;
      YS: Integer = 0); overload;

    procedure PlusOperator;
    procedure MinusOperator;
    procedure IncOperator(n: Integer);
    procedure DecOperator(n: Integer);

    property Y: Integer read GetY;
    property deltay: Integer read FDeltaY;
  end;

  TAggDda2LineInterpolator = packed record
  private
    FCount, FLft, FRem, FMod, fy: Integer;
  public
    procedure Initialize(y1, y2, Count: Integer); overload; // Forward-adjusted line
    procedure Initialize(Y, Count: Integer); overload;      // Backward-adjusted line

    procedure PlusOperator;
    procedure MinusOperator;

    procedure AdjustForward;
    procedure AdjustBackward;

    property ModValue: Integer read FMod;
    property RemValue: Integer read FRem;
    property Lft: Integer read FLft;
    property Y: Integer read fy;
  end;

  TAggLineBresenhamInterpolator = packed record
  private
    FLowResolution: TRectInteger;

    FVer: Boolean;
    FLength: Cardinal;
    FInc: Integer;

    FInterpolator: TAggDda2LineInterpolator;
    function GetX2: Integer;
    function GetY2: Integer;
    function GetX2HighResolution: Integer;
    function GetY2HighResolution: Integer;
  public
    procedure Initialize(x1, y1, x2, y2: Integer); overload;
    procedure Initialize(Point1, Point2: TPointInteger); overload;

    function LineLowResolution(v: Integer): Integer;

    function Inc: Integer;

    procedure HStep;
    procedure Vstep;

    property IsVer: Boolean read FVer;
    property length: Cardinal read FLength;
    property x1: Integer read FLowResolution.x1;
    property y1: Integer read FLowResolution.y1;
    property x2: Integer read GetX2;
    property y2: Integer read GetY2;
    property X2HighResolution: Integer read GetX2HighResolution;
    property Y2HighResolution: Integer read GetY2HighResolution;
  end;

implementation


{ TAggDdaLineInterpolator }

procedure TAggDdaLineInterpolator.Initialize(fs: Integer; YS: Integer = 0);
begin
  FFractionShift := fs;

  YShift := YS;
end;

procedure TAggDdaLineInterpolator.Initialize(y1, y2: Integer; Count: Cardinal;
  fs: Integer; YS: Integer = 0);
begin
  Initialize(fs, YS);

  fy := y1;
  FInc := ((y2 - y1) shl FFractionShift) div Count;
  FDeltaY := 0;
end;

procedure TAggDdaLineInterpolator.PlusOperator;
begin
  Inc(FDeltaY, FInc);
end;

procedure TAggDdaLineInterpolator.MinusOperator;
begin
  Dec(FDeltaY, FInc);
end;

procedure TAggDdaLineInterpolator.IncOperator(n: Integer);
begin
  Inc(FDeltaY, FInc * n);
end;

procedure TAggDdaLineInterpolator.DecOperator(n: Integer);
begin
  Dec(FDeltaY, FInc * n);
end;

function TAggDdaLineInterpolator.GetY: Integer;
begin
  Result := fy + (ShrInt32(FDeltaY, FFractionShift - YShift));
end;

{ TAggDda2LineInterpolator }

procedure TAggDda2LineInterpolator.Initialize(y1, y2, Count: Integer);
begin
  if Count <= 0 then
      FCount := 1
  else
      FCount := Count;

  FLft := Trunc((y2 - y1) / FCount);
  FRem := Trunc((y2 - y1) mod FCount);
  FMod := FRem;
  fy := y1;

  if FMod <= 0 then
    begin
      FMod := FMod + Count;
      FRem := FRem + Count;

      Dec(FLft);
    end;

  FMod := FMod - Count;
end;

procedure TAggDda2LineInterpolator.Initialize(Y, Count: Integer);
begin
  if Count <= 0 then
      FCount := 1
  else
      FCount := Count;

  FLft := Y div FCount;
  FRem := Y mod FCount;
  FMod := FRem;
  fy := 0;

  if FMod <= 0 then
    begin
      Inc(FMod, Count);
      Inc(FRem, Count);
      Dec(FLft);
    end;
end;

procedure TAggDda2LineInterpolator.PlusOperator;
begin
  Inc(FMod, FRem);
  Inc(fy, FLft);

  if FMod > 0 then
    begin
      Dec(FMod, FCount);
      Inc(fy);
    end;
end;

procedure TAggDda2LineInterpolator.MinusOperator;
begin
  if FMod <= FRem then
    begin
      Inc(FMod, FCount);
      Dec(fy);
    end;

  Dec(FMod, FRem);
  Dec(fy, FLft);
end;

procedure TAggDda2LineInterpolator.AdjustForward;
begin
  Dec(FMod, FCount);
end;

procedure TAggDda2LineInterpolator.AdjustBackward;
begin
  Inc(FMod, FCount);
end;

{ TAggLineBresenhamInterpolator }

procedure TAggLineBresenhamInterpolator.Initialize(x1, y1, x2, y2: Integer);
begin
  FLowResolution.x1 := LineLowResolution(x1);
  FLowResolution.y1 := LineLowResolution(y1);
  FLowResolution.x2 := LineLowResolution(x2);
  FLowResolution.y2 := LineLowResolution(y2);

  FVer := Abs(FLowResolution.x2 - FLowResolution.x1) < Abs(FLowResolution.y2 - FLowResolution.y1);

  if FVer then
      FLength := Abs(FLowResolution.y2 - FLowResolution.y1)
  else
      FLength := Abs(FLowResolution.x2 - FLowResolution.x1);

  if FVer then
    if y2 > y1 then
        FInc := 1
    else
        FInc := -1
  else if x2 > x1 then
      FInc := 1
  else
      FInc := -1;

  if FVer then
      FInterpolator.Initialize(x1, x2, FLength)
  else
      FInterpolator.Initialize(y1, y2, FLength);
end;

procedure TAggLineBresenhamInterpolator.Initialize(Point1,
  Point2: TPointInteger);
begin
  FLowResolution.Point1 := Point1;
  FLowResolution.Point2 := Point2;

  FVer := Abs(FLowResolution.x2 - FLowResolution.x1) < Abs(FLowResolution.y2 - FLowResolution.y1);

  if FVer then
      FLength := Abs(FLowResolution.y2 - FLowResolution.y1)
  else
      FLength := Abs(FLowResolution.x2 - FLowResolution.x1);

  if FVer then
    if y2 > y1 then
        FInc := 1
    else
        FInc := -1
  else if x2 > x1 then
      FInc := 1
  else
      FInc := -1;

  if FVer then
      FInterpolator.Initialize(x1, x2, FLength)
  else
      FInterpolator.Initialize(y1, y2, FLength);
end;

function TAggLineBresenhamInterpolator.LineLowResolution(v: Integer): Integer;
begin
  Result := ShrInt32(v, CAggSubpixelShift);
end;

function TAggLineBresenhamInterpolator.Inc;
begin
  Result := FInc;
end;

procedure TAggLineBresenhamInterpolator.HStep;
begin
  FInterpolator.PlusOperator;

  FLowResolution.x1 := FLowResolution.x1 + FInc;
end;

procedure TAggLineBresenhamInterpolator.Vstep;
begin
  FInterpolator.PlusOperator;

  FLowResolution.y1 := FLowResolution.y1 + FInc;
end;

function TAggLineBresenhamInterpolator.GetX2: Integer;
begin
  Result := LineLowResolution(FInterpolator.Y);
end;

function TAggLineBresenhamInterpolator.GetY2: Integer;
begin
  Result := LineLowResolution(FInterpolator.Y);
end;

function TAggLineBresenhamInterpolator.GetX2HighResolution: Integer;
begin
  Result := FInterpolator.Y;
end;

function TAggLineBresenhamInterpolator.GetY2HighResolution: Integer;
begin
  Result := FInterpolator.Y;
end;

end. 
