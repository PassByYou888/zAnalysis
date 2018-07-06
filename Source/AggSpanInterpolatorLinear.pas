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
unit AggSpanInterpolatorLinear;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggDdaLine,
  AggTransAffine;

type
  TAggSpanInterpolator = class
  protected
    FSubpixelShift, FSubpixelSize: Cardinal;
    function GetTransformer: TAggTransAffine; virtual; abstract;
    procedure SetTransformer(Trans: TAggTransAffine); virtual; abstract;
  public
    constructor Create(SS: Cardinal = 8); virtual;

    procedure SetBegin(X, Y: Double; Len: Cardinal); virtual; abstract;

    procedure Resynchronize(XE, Ye: Double; Len: Cardinal); virtual; abstract;

    procedure IncOperator; virtual; abstract;
    procedure Coordinates(X, Y: PInteger); overload; virtual; abstract;
    procedure Coordinates(var X, Y: Integer); overload; virtual;

    procedure LocalScale(X, Y: PInteger); virtual;

    property SubpixelShift: Cardinal read FSubpixelShift;
    property Transformer: TAggTransAffine read GetTransformer write SetTransformer;
  end;

  TAggSpanInterpolatorLinear = class(TAggSpanInterpolator)
  private
    FTrans: TAggTransAffine;
    FLineInterpolatorX, FLineInterpolatorY: TAggDda2LineInterpolator;
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload; override;
    constructor Create(Trans: TAggTransAffine; SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; X, Y: Double; Len: Cardinal; SS: Cardinal = 8); overload;

    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure Resynchronize(XE, Ye: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;
  end;

  TAggSpanInterpolatorLinearSubdiv = class(TAggSpanInterpolator)
  private
    FSubdivShift, FSubdivSize, FSubdivMask: Cardinal;

    FTrans: TAggTransAffine;
    FLineInterpolatorX, FLineInterpolatorY: TAggDda2LineInterpolator;

    FSourceX: Integer;
    FSourceY: Double;
    fPos, FLength: Cardinal;

    procedure SetSubdivShift(Shift: Cardinal);
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload; override;
    constructor Create(Trans: TAggTransAffine; ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; X, Y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;

    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;

    property SubdivShift: Cardinal read FSubdivShift write SetSubdivShift;
  end;

implementation


{ TAggSpanInterpolator }

constructor TAggSpanInterpolator.Create(SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;
end;

procedure TAggSpanInterpolator.Coordinates(var X, Y: Integer);
begin
  Coordinates(@X, @Y);
end;

procedure TAggSpanInterpolator.LocalScale;
begin
end;

{ TAggSpanInterpolatorLinear }

constructor TAggSpanInterpolatorLinear.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);
end;

constructor TAggSpanInterpolatorLinear.Create(Trans: TAggTransAffine;
  SS: Cardinal = 8);
begin
  Create(SS);

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorLinear.Create(Trans: TAggTransAffine;
  X, Y: Double; Len: Cardinal; SS: Cardinal = 8);
begin
  Create(Trans, SS);

  SetBegin(X, Y, Len);
end;

function TAggSpanInterpolatorLinear.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorLinear.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorLinear.SetBegin(X, Y: Double; Len: Cardinal);
var
  TX, TY: Double;
  x1, y1, x2, y2: Integer;
begin
  TX := X;
  TY := Y;

  FTrans.Transform(FTrans, @TX, @TY);

  x1 := Trunc(TX * FSubpixelSize);
  y1 := Trunc(TY * FSubpixelSize);

  TX := X + Len;
  TY := Y;

  FTrans.Transform(FTrans, @TX, @TY);

  x2 := Trunc(TX * FSubpixelSize);
  y2 := Trunc(TY * FSubpixelSize);

  FLineInterpolatorX.Initialize(x1, x2, Len);
  FLineInterpolatorY.Initialize(y1, y2, Len);
end;

procedure TAggSpanInterpolatorLinear.Resynchronize;
begin
  FTrans.Transform(FTrans, @XE, @Ye);

  FLineInterpolatorX.Initialize(FLineInterpolatorX.Y, Trunc(XE * FSubpixelSize), Len);
  FLineInterpolatorY.Initialize(FLineInterpolatorY.Y, Trunc(Ye * FSubpixelSize), Len);
end;

procedure TAggSpanInterpolatorLinear.IncOperator;
begin
  FLineInterpolatorX.PlusOperator;
  FLineInterpolatorY.PlusOperator;
end;

procedure TAggSpanInterpolatorLinear.Coordinates(X, Y: PInteger);
begin
  X^ := FLineInterpolatorX.Y;
  Y^ := FLineInterpolatorY.Y;
end;

procedure TAggSpanInterpolatorLinear.Coordinates(var X, Y: Integer);
begin
  X := FLineInterpolatorX.Y;
  Y := FLineInterpolatorY.Y;
end;

{ TAggSpanInterpolatorLinearSubdiv }

constructor TAggSpanInterpolatorLinearSubdiv.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FSubdivShift := 4;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

constructor TAggSpanInterpolatorLinearSubdiv.Create(Trans: TAggTransAffine;
  ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FSubdivShift := ASubdivShift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorLinearSubdiv.Create(Trans: TAggTransAffine;
  X, Y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  Create(Trans, ASubdivShift, SS);

  SetBegin(X, Y, Len);
end;

function TAggSpanInterpolatorLinearSubdiv.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetSubdivShift;
begin
  FSubdivShift := Shift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetBegin;
var
  TX, TY: Double;
  x1, y1: Integer;
begin
  fPos := 1;
  FSourceX := Trunc(X * FSubpixelSize) + FSubpixelSize;
  FSourceY := Y;
  FLength := Len;

  if Len > FSubdivSize then
      Len := FSubdivSize;

  TX := X;
  TY := Y;

  FTrans.Transform(FTrans, @TX, @TY);

  x1 := Trunc(TX * FSubpixelSize);
  y1 := Trunc(TY * FSubpixelSize);

  TX := X + Len;
  TY := Y;

  FTrans.Transform(FTrans, @TX, @TY);

  FLineInterpolatorX.Initialize(x1, Trunc(TX * FSubpixelSize), Len);
  FLineInterpolatorY.Initialize(y1, Trunc(TY * FSubpixelSize), Len);
end;

procedure TAggSpanInterpolatorLinearSubdiv.IncOperator;
var
  TX, TY: Double;
  Len: Cardinal;
begin
  FLineInterpolatorX.PlusOperator;
  FLineInterpolatorY.PlusOperator;

  if fPos >= FSubdivSize then
    begin
      Len := FLength;

      if Len > FSubdivSize then
          Len := FSubdivSize;

      TX := FSourceX / FSubpixelSize + Len;
      TY := FSourceY;

      FTrans.Transform(FTrans, @TX, @TY);

      FLineInterpolatorX.Initialize(FLineInterpolatorX.Y, Trunc(TX * FSubpixelSize), Len);
      FLineInterpolatorY.Initialize(FLineInterpolatorY.Y, Trunc(TY * FSubpixelSize), Len);

      fPos := 0;
    end;

  Inc(FSourceX, FSubpixelSize);
  Inc(fPos);
  Dec(FLength);
end;

procedure TAggSpanInterpolatorLinearSubdiv.Coordinates(X, Y: PInteger);
begin
  X^ := FLineInterpolatorX.Y;
  Y^ := FLineInterpolatorY.Y;
end;

procedure TAggSpanInterpolatorLinearSubdiv.Coordinates(var X, Y: Integer);
begin
  X := FLineInterpolatorX.Y;
  Y := FLineInterpolatorY.Y;
end;

end. 
