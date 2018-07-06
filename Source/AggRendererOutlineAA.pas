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
unit AggRendererOutlineAA;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggColor32,
  AggMath,
  AggLineAABasics,
  AggDdaLine,
  AggEllipseBresenham,
  AggRendererBase,
  AggGammaFunctions,
  AggVertexSource;

const
  CMaxHalfWidth = 64;

  CAggSubpixelShift = CAggLineSubpixelShift;
  CAggSubpixelSize  = 1 shl CAggSubpixelShift;
  CAggSubpixelMask  = CAggSubpixelSize - 1;

  CAggAntiAliasingShift = 8;
  CAggAntiAliasingNum   = 1 shl CAggAntiAliasingShift;
  CAggAntiAliasingMask  = CAggAntiAliasingNum - 1;

type
  TAggCustomDistanceInterpolator = class
  public
    procedure IncX; virtual; abstract;
    procedure DecX; virtual; abstract;
    procedure IncY; virtual; abstract;
    procedure DecY; virtual; abstract;

    procedure SetIncX(Value: Integer); virtual; abstract;
    procedure SetDecX(Value: Integer); virtual; abstract;
    procedure SetIncY(Value: Integer); virtual; abstract;
    procedure SetDecY(Value: Integer); virtual; abstract;
  end;

  TAggCustomDistance0Interpolator = class(TAggCustomDistanceInterpolator)
  protected
    function GetDeltaX: Integer; virtual; abstract;
    function GetDeltaY: Integer; virtual; abstract;
    function GetDistance: Integer; virtual; abstract;
  public
    property Distance: Integer read GetDistance;

    property deltax: Integer read GetDeltaX;
    property deltay: Integer read GetDeltaY;
  end;

  TAggDistanceInterpolator0 = class(TAggCustomDistance0Interpolator)
  private
    FDelta: TPointInteger;
    FDist: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
  public
    constructor Create(x1, y1, x2, y2, X, Y: Integer);

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggDistanceInterpolator1 = class(TAggCustomDistance0Interpolator)
  private
    FDelta: TPointInteger;
    FDist: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
  public
    constructor Create(x1, y1, x2, y2, X, Y: Integer);

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggCustomDistance2Interpolator = class(TAggCustomDistance0Interpolator)
  protected
    function GetDeltaXStart: Integer; virtual; abstract;
    function GetDeltaYStart: Integer; virtual; abstract;
    function GetDeltaXEnd: Integer; virtual; abstract;
    function GetDeltaYEnd: Integer; virtual; abstract;
    function GetDistanceStart: Integer; virtual; abstract;
    function GetDistanceEnd: Integer; virtual; abstract;
  public
    property DistanceStart: Integer read GetDistanceStart;
    property DistanceEnd: Integer read GetDistanceEnd;

    property DxStart: Integer read GetDeltaXStart;
    property DyStart: Integer read GetDeltaYStart;
    property DxEnd: Integer read GetDeltaXEnd;
    property DyEnd: Integer read GetDeltaYEnd;
  end;

  TAggDistanceInterpolator2 = class(TAggCustomDistance2Interpolator)
  private
    FDelta, FDeltaStart: TPointInteger;
    FDist, FDistStart: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDistanceStart: Integer; override;
    function GetDistanceEnd: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
    function GetDeltaXStart: Integer; override;
    function GetDeltaYStart: Integer; override;
    function GetDeltaXEnd: Integer; override;
    function GetDeltaYEnd: Integer; override;
  public
    constructor Create(x1, y1, x2, y2, SX, SY, X, Y: Integer); overload;
    constructor Create(x1, y1, x2, y2, EX, EY, X, Y, Z: Integer); overload;

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggDistanceInterpolator3 = class(TAggCustomDistance2Interpolator)
  private
    FDelta, FDeltaStart, FDeltaEnd: TPointInteger;
    FDist, FDistStart, FDistEnd: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDistanceStart: Integer; override;
    function GetDistanceEnd: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
    function GetDeltaXStart: Integer; override;
    function GetDeltaYStart: Integer; override;
    function GetDeltaXEnd: Integer; override;
    function GetDeltaYEnd: Integer; override;
  public
    constructor Create(x1, y1, x2, y2, SX, SY, EX, EY, X, Y: Integer);

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggCustomLineInterpolator = class
  protected
    function GetWidth: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
  public
    function StepHorizontal: Boolean; virtual; abstract;
    function StepVertical: Boolean; virtual; abstract;

    property width: Integer read GetWidth;
    property Count: Integer read GetCount;
  end;

  TAggRendererOutlineAA = class;

  TAggCustomLineInterpolatorAA = class(TAggCustomLineInterpolator)
  private
    FLineParameters: PAggLineParameters;
    FLineInterpolator: TAggDda2LineInterpolator;
    FRendererBase: TAggRendererOutlineAA;

    FLength, fx, fy, FOldX, FOldY, FCount, FWidth, FMaxExtent, FStep: Integer;

    FDist: array [0 .. CMaxHalfWidth + 1 - 1] of Integer;
    FCovers: array [0 .. CMaxHalfWidth * 2 + 4 - 1] of Int8u;
  protected
    function GetCount: Integer; override;
    function GetWidth: Integer; override;
    function GetVertical: Boolean;
  public
    constructor Create(Ren: TAggRendererOutlineAA; LP: PAggLineParameters);

    function StepHorizontalBase(di: TAggCustomDistance0Interpolator): Integer;
    function StepVerticalBase(di: TAggCustomDistance0Interpolator): Integer;

    property Vertical: Boolean read GetVertical;
  end;

  TAggLineInterpolatorAA0 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator1;
  public
    constructor Create(Ren: TAggRendererOutlineAA; LP: PAggLineParameters);
    destructor Destroy; override;

    function StepHorizontal: Boolean; override;
    function StepVertical: Boolean; override;
  end;

  TAggLineInterpolatorAA1 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator2;
  public
    constructor Create(Ren: TAggRendererOutlineAA; LP: PAggLineParameters; SX, SY: Integer);
    destructor Destroy; override;

    function StepHorizontal: Boolean; override;
    function StepVertical: Boolean; override;
  end;

  TAggLineInterpolatorAA2 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator2;
  public
    constructor Create(Ren: TAggRendererOutlineAA; LP: PAggLineParameters; EX, EY: Integer);
    destructor Destroy; override;

    function StepHorizontal: Boolean; virtual;
    function StepVertical: Boolean; virtual;
  end;

  TAggLineInterpolatorAA3 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator3;
  public
    constructor Create(Ren: TAggRendererOutlineAA; LP: PAggLineParameters; SX, SY, EX, EY: Integer);
    destructor Destroy; override;

    function StepHorizontal: Boolean; virtual;
    function StepVertical: Boolean; virtual;
  end;

  TAggLineProfileAA = class
  private
    FSize: Cardinal;
    FProfile: PInt8u;
    FGamma: array [0 .. CAggAntiAliasingNum - 1] of Int8u;

    FSubpixelWidth: Integer;
    FMinWidth, FSmootherWidth: Double;

    procedure SetMinWidth(Value: Double);
    procedure SetSmootherWidth(Value: Double);
  public
    constructor Create; overload;
    constructor Create(width: Double; GammaFunction: TAggCustomVertexSource); overload;
    destructor Destroy; override;

    procedure SetGamma(Value: TAggCustomVertexSource);
    procedure SetWidth(Value: Double); overload;
    procedure SetWidth(CenterWidth, SmootherWidth: Double); overload;

    function GetValue(GetDist: Integer): Int8u;

    function Profile(Value: Double): PInt8u;

    property ProfileSize: Cardinal read FSize;
    property SubpixelWidth: Integer read FSubpixelWidth;
    property MinWidth: Double read FMinWidth write SetMinWidth;
    property SmootherWidth: Double read FSmootherWidth write SetSmootherWidth;
  end;

  TCompareFunction = function(d: Integer): Boolean;

  TAggRendererOutline = class
  protected
    function GetAccurateJoinOnly: Boolean; virtual; abstract;
    function GetSubpixelWidth: Integer; virtual; abstract;
  public
    procedure SetColor(C: PAggColor); virtual; abstract;

    procedure Semidot(cmp: TCompareFunction; Xc1, Yc1, Xc2, Yc2: Integer); virtual; abstract;

    procedure Line0(LP: PAggLineParameters); virtual; abstract;
    procedure Line1(LP: PAggLineParameters; SX, SY: Integer); virtual; abstract;
    procedure Line2(LP: PAggLineParameters; EX, EY: Integer); virtual; abstract;
    procedure Line3(LP: PAggLineParameters; SX, SY, EX, EY: Integer); virtual; abstract;

    property AccurateJoinOnly: Boolean read GetAccurateJoinOnly;
    property SubpixelWidth: Integer read GetSubpixelWidth;
  end;

  TAggRendererOutlineAA = class(TAggRendererOutline)
  private
    FRendererBase: TAggRendererBase;
    FProfile: TAggLineProfileAA;
    FColor: TAggColor;
    procedure SetProfile(Prof: TAggLineProfileAA);
  protected
    function GetAccurateJoinOnly: Boolean; override;
    function GetSubpixelWidth: Integer; override;
  public
    constructor Create(Ren: TAggRendererBase; Prof: TAggLineProfileAA);

    procedure SetColor(C: PAggColor); override;
    function GetColor: PAggColor;

    function Cover(d: Integer): Int8u;

    procedure BlendSolidHSpan(X, Y: Integer; Len: Cardinal; Covers: PInt8u);
    procedure BlendSolidVSpan(X, Y: Integer; Len: Cardinal; Covers: PInt8u);

    procedure SemidotHorizontalLine(cmp: TCompareFunction; Xc1, Yc1, Xc2, Yc2, x1, y1, x2: Integer);
    procedure Semidot(cmp: TCompareFunction; Xc1, Yc1, Xc2, Yc2: Integer); override;

    procedure Line0(LP: PAggLineParameters); override;
    procedure Line1(LP: PAggLineParameters; SX, SY: Integer); override;
    procedure Line2(LP: PAggLineParameters; EX, EY: Integer); override;
    procedure Line3(LP: PAggLineParameters; SX, SY, EX, EY: Integer); override;

    property Profile: TAggLineProfileAA read FProfile write SetProfile;
  end;

implementation


{ TAggDistanceInterpolator0 }

constructor TAggDistanceInterpolator0.Create(x1, y1, x2, y2, X, Y: Integer);
begin
  FDelta.X := LineMedResolution(x2) - LineMedResolution(x1);
  FDelta.Y := LineMedResolution(y2) - LineMedResolution(y1);

  FDist := (LineMedResolution(X + CAggLineSubpixelSize div 2) -
    LineMedResolution(x2)) * FDelta.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) -
    LineMedResolution(y2)) * FDelta.X;

  FDelta.X := FDelta.X shl CAggLineMrSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator0.IncX;
begin
  Inc(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator0.DecX;
begin
  Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator0.IncY;
begin
  Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.DecY;
begin
  Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);

  if Value > 0 then
      Dec(FDist, FDelta.X);

  if Value < 0 then
      Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);

  if Value > 0 then
      Dec(FDist, FDelta.X);

  if Value < 0 then
      Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);

  if Value > 0 then
      Inc(FDist, FDelta.Y);

  if Value < 0 then
      Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator0.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);

  if Value > 0 then
      Inc(FDist, FDelta.Y);

  if Value < 0 then
      Dec(FDist, FDelta.Y);
end;

function TAggDistanceInterpolator0.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator0.GetDeltaX;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator0.GetDeltaY;
begin
  Result := FDelta.Y;
end;

{ TAggDistanceInterpolator1 }

constructor TAggDistanceInterpolator1.Create(x1, y1, x2, y2, X, Y: Integer);
begin
  FDelta.X := x2 - x1;
  FDelta.Y := y2 - y1;

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - x2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - y2) * FDelta.X);

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;
end;

procedure TAggDistanceInterpolator1.IncX;
begin
  Inc(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator1.DecX;
begin
  Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator1.IncY;
begin
  Dec(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.DecY;
begin
  Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);

  if Value > 0 then
      Dec(FDist, FDelta.X);

  if Value < 0 then
      Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);

  if Value > 0 then
      Dec(FDist, FDelta.X);

  if Value < 0 then
      Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);

  if Value > 0 then
      Inc(FDist, FDelta.Y);

  if Value < 0 then
      Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator1.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);

  if Value > 0 then
      Inc(FDist, FDelta.Y);

  if Value < 0 then
      Dec(FDist, FDelta.Y);
end;

function TAggDistanceInterpolator1.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator1.GetDeltaX;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator1.GetDeltaY;
begin
  Result := FDelta.Y;
end;

{ TAggDistanceInterpolator2 }

constructor TAggDistanceInterpolator2.Create(x1, y1, x2, y2, SX, SY,
  X, Y: Integer);
begin
  FDelta := PointInteger(x2 - x1, y2 - y1);

  FDeltaStart.X := LineMedResolution(SX) - LineMedResolution(x1);
  FDeltaStart.Y := LineMedResolution(SY) - LineMedResolution(y1);

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - x2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - y2) * FDelta.X);

  FDistStart := (LineMedResolution(X + CAggLineSubpixelSize div 2) -
    LineMedResolution(SX)) * FDeltaStart.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) -
    LineMedResolution(SY)) * FDeltaStart.X;

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;

  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;
end;

constructor TAggDistanceInterpolator2.Create(x1, y1, x2, y2, EX, EY, X,
  Y, Z: Integer);
begin
  FDelta := PointInteger(x2 - x1, y2 - y1);

  FDeltaStart.X := LineMedResolution(EX) - LineMedResolution(x2);
  FDeltaStart.Y := LineMedResolution(EY) - LineMedResolution(y2);

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - x2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - y2) * FDelta.X);

  FDistStart := (LineMedResolution(X + CAggLineSubpixelSize div 2) -
    LineMedResolution(EX)) * FDeltaStart.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) -
    LineMedResolution(EY)) * FDeltaStart.X;

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;

  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator2.IncX;
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
end;

procedure TAggDistanceInterpolator2.DecX;
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
end;

procedure TAggDistanceInterpolator2.IncY;
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
end;

procedure TAggDistanceInterpolator2.DecY;
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
end;

procedure TAggDistanceInterpolator2.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);

  if Value > 0 then
    begin
      Dec(FDist, FDelta.X);
      Dec(FDistStart, FDeltaStart.X);
    end;

  if Value < 0 then
    begin
      Inc(FDist, FDelta.X);
      Inc(FDistStart, FDeltaStart.X);
    end;
end;

procedure TAggDistanceInterpolator2.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);

  if Value > 0 then
    begin
      Dec(FDist, FDelta.X);
      Dec(FDistStart, FDeltaStart.X);
    end;

  if Value < 0 then
    begin
      Inc(FDist, FDelta.X);
      Inc(FDistStart, FDeltaStart.X);
    end;
end;

procedure TAggDistanceInterpolator2.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);

  if Value > 0 then
    begin
      Inc(FDist, FDelta.Y);
      Inc(FDistStart, FDeltaStart.Y);
    end;

  if Value < 0 then
    begin
      Dec(FDist, FDelta.Y);
      Dec(FDistStart, FDeltaStart.Y);
    end;
end;

procedure TAggDistanceInterpolator2.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);

  if Value > 0 then
    begin
      Inc(FDist, FDelta.Y);
      Inc(FDistStart, FDeltaStart.Y);
    end;

  if Value < 0 then
    begin
      Dec(FDist, FDelta.Y);
      Dec(FDistStart, FDeltaStart.Y);
    end;
end;

function TAggDistanceInterpolator2.GetDistance: Integer;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator2.GetDistanceStart: Integer;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator2.GetDistanceEnd: Integer;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator2.GetDeltaX: Integer;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator2.GetDeltaY: Integer;
begin
  Result := FDelta.Y;
end;

function TAggDistanceInterpolator2.GetDeltaXStart: Integer;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator2.GetDeltaYStart: Integer;
begin
  Result := FDeltaStart.Y;
end;

function TAggDistanceInterpolator2.GetDeltaXEnd: Integer;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator2.GetDeltaYEnd: Integer;
begin
  Result := FDeltaStart.Y;
end;

{ TAggDistanceInterpolator3 }

constructor TAggDistanceInterpolator3.Create(x1, y1, x2, y2, SX, SY, EX, EY,
  X, Y: Integer);
begin
  FDelta := PointInteger(x2 - x1, y2 - y1);

  FDeltaStart.X := LineMedResolution(SX) - LineMedResolution(x1);
  FDeltaStart.Y := LineMedResolution(SY) - LineMedResolution(y1);

  FDeltaEnd.X := LineMedResolution(EX) - LineMedResolution(x2);
  FDeltaEnd.Y := LineMedResolution(EY) - LineMedResolution(y2);

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - x2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - y2) * FDelta.X);

  FDistStart := (LineMedResolution(X + CAggLineSubpixelSize div 2) -
    LineMedResolution(SX)) * FDeltaStart.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) -
    LineMedResolution(SY)) * FDeltaStart.X;

  FDistEnd := (LineMedResolution(X + CAggLineSubpixelSize div 2) -
    LineMedResolution(EX)) * FDeltaEnd.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) -
    LineMedResolution(EY)) * FDeltaEnd.X;

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;

  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;

  FDeltaEnd.X := FDeltaEnd.X shl CAggLineMrSubpixelShift;
  FDeltaEnd.Y := FDeltaEnd.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator3.IncX;
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator3.DecX;
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator3.IncY;
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator3.DecY;
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator3.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
    begin
      Dec(FDist, FDelta.X);
      Dec(FDistStart, FDeltaStart.X);
      Dec(FDistEnd, FDeltaEnd.X);
    end;

  if Value < 0 then
    begin
      Inc(FDist, FDelta.X);
      Inc(FDistStart, FDeltaStart.X);
      Inc(FDistEnd, FDeltaEnd.X);
    end;
end;

procedure TAggDistanceInterpolator3.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
    begin
      Dec(FDist, FDelta.X);
      Dec(FDistStart, FDeltaStart.X);
      Dec(FDistEnd, FDeltaEnd.X);
    end;

  if Value < 0 then
    begin
      Inc(FDist, FDelta.X);
      Inc(FDistStart, FDeltaStart.X);
      Inc(FDistEnd, FDeltaEnd.X);
    end;
end;

procedure TAggDistanceInterpolator3.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
    begin
      Inc(FDist, FDelta.Y);
      Inc(FDistStart, FDeltaStart.Y);
      Inc(FDistEnd, FDeltaEnd.Y);
    end;

  if Value < 0 then
    begin
      Dec(FDist, FDelta.Y);
      Dec(FDistStart, FDeltaStart.Y);
      Dec(FDistEnd, FDeltaEnd.Y);
    end;
end;

procedure TAggDistanceInterpolator3.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
    begin
      Inc(FDist, FDelta.Y);
      Inc(FDistStart, FDeltaStart.Y);
      Inc(FDistEnd, FDeltaEnd.Y);
    end;

  if Value < 0 then
    begin
      Dec(FDist, FDelta.Y);
      Dec(FDistStart, FDeltaStart.Y);
      Dec(FDistEnd, FDeltaEnd.Y);
    end;
end;

function TAggDistanceInterpolator3.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator3.GetDistanceStart;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator3.GetDistanceEnd;
begin
  Result := FDistEnd;
end;

function TAggDistanceInterpolator3.GetDeltaX;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator3.GetDeltaY;
begin
  Result := FDelta.Y;
end;

function TAggDistanceInterpolator3.GetDeltaXStart;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator3.GetDeltaYStart;
begin
  Result := FDeltaStart.Y;
end;

function TAggDistanceInterpolator3.GetDeltaXEnd;
begin
  Result := FDeltaEnd.X;
end;

function TAggDistanceInterpolator3.GetDeltaYEnd;
begin
  Result := FDeltaEnd.Y;
end;

{ TAggCustomLineInterpolatorAA }

constructor TAggCustomLineInterpolatorAA.Create(Ren: TAggRendererOutlineAA;
  LP: PAggLineParameters);
var
  Li: TAggDda2LineInterpolator;
  i: Cardinal;
  stop: Integer;
begin
  FLineParameters := LP;

  if LP.Vertical then
      FLineInterpolator.Initialize(LineDoubleHighResolution(LP.x2 - LP.x1),
      Abs(LP.y2 - LP.y1))
  else
      FLineInterpolator.Initialize(LineDoubleHighResolution(LP.y2 - LP.y1),
      Abs(LP.x2 - LP.x1) + 1);

  FRendererBase := Ren;

  if LP.Vertical = (LP.IncValue > 0) then
      FLength := -LP.Len
  else
      FLength := LP.Len;

  fx := ShrInt32(LP.x1, CAggLineSubpixelShift);
  fy := ShrInt32(LP.y1, CAggLineSubpixelShift);

  FOldX := fx;
  FOldY := fy;

  if LP.Vertical then
      FCount := Abs(ShrInt32(LP.y2, CAggLineSubpixelShift) - fy)
  else
      FCount := Abs(ShrInt32(LP.x2, CAggLineSubpixelShift) - fx);

  FWidth := Ren.GetSubpixelWidth;
  FMaxExtent := ShrInt32(FWidth, CAggLineSubpixelShift - 2);
  FStep := 0;

  if LP.Vertical then
      Li.Initialize(0, LP.Delta.Y shl CAggLineSubpixelShift, LP.Len)
  else
      Li.Initialize(0, LP.Delta.X shl CAggLineSubpixelShift, LP.Len);

  stop := FWidth + CAggLineSubpixelSize * 2;

  i := 0;

  while i < CMaxHalfWidth do
    begin
      FDist[i] := Li.Y;

      if FDist[i] >= stop then
          Break;

      Li.PlusOperator;

      Inc(i);
    end;

  FDist[i] := $7FFF0000;
end;

function TAggCustomLineInterpolatorAA.StepHorizontalBase;
begin
  FLineInterpolator.PlusOperator;

  Inc(fx, FLineParameters.IncValue);

  fy := ShrInt32(FLineParameters.y1 + FLineInterpolator.Y, CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
      di.SetIncX(fy - FOldY)
  else
      di.SetDecX(fy - FOldY);

  FOldY := fy;

  Result := di.Distance div FLength;
end;

function TAggCustomLineInterpolatorAA.StepVerticalBase;
begin
  FLineInterpolator.PlusOperator;

  Inc(fy, FLineParameters.IncValue);

  fx := ShrInt32(FLineParameters.x1 + FLineInterpolator.Y, CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
      di.SetIncY(fx - FOldX)
  else
      di.SetDecY(fx - FOldX);

  FOldX := fx;

  Result := di.Distance div FLength;
end;

function TAggCustomLineInterpolatorAA.GetVertical;
begin
  Result := FLineParameters.Vertical;
end;

function TAggCustomLineInterpolatorAA.GetWidth;
begin
  Result := FWidth;
end;

function TAggCustomLineInterpolatorAA.GetCount;
begin
  Result := FCount;
end;

{ TAggLineInterpolatorAA0 }

constructor TAggLineInterpolatorAA0.Create;
begin
  inherited Create(Ren, LP);

  FDistanceInterpolator := TAggDistanceInterpolator1.Create(LP.x1, LP.y1, LP.x2,
    LP.y2, LP.x1 and not CAggLineSubpixelMask, LP.y1 and not CAggLineSubpixelMask);

  FLineInterpolator.AdjustForward;
end;

destructor TAggLineInterpolatorAA0.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited;
end;

function TAggLineInterpolatorAA0.StepHorizontal: Boolean;
var
  Dist, dy, s1: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepHorizontalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  p1^ := Int8u(FRendererBase.Cover(s1));

  Inc(PtrComp(p1), SizeOf(Int8u));

  dy := 1;
  Dist := FDist[dy] - s1;

  while Dist <= FWidth do
    begin
      p1^ := Int8u(FRendererBase.Cover(Dist));

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dy);

      Dist := FDist[dy] - s1;
    end;

  dy := 1;
  Dist := FDist[dy] + s1;

  while Dist <= FWidth do
    begin
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := Int8u(FRendererBase.Cover(Dist));

      Inc(dy);

      Dist := FDist[dy] + s1;
    end;

  FRendererBase.BlendSolidVSpan(fx, fy - dy + 1,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;

function TAggLineInterpolatorAA0.StepVertical: Boolean;
var
  Dist, dx, s1: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  p1^ := Int8u(FRendererBase.Cover(s1));

  Inc(PtrComp(p1), SizeOf(Int8u));

  dx := 1;
  Dist := FDist[dx] - s1;

  while Dist <= FWidth do
    begin
      p1^ := Int8u(FRendererBase.Cover(Dist));

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dx);

      Dist := FDist[dx] - s1;
    end;

  dx := 1;
  Dist := FDist[dx] + s1;

  while Dist <= FWidth do
    begin
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := Int8u(FRendererBase.Cover(Dist));

      Inc(dx);

      Dist := FDist[dx] + s1;
    end;

  FRendererBase.BlendSolidHSpan(fx - dx + 1, fy,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;

{ TAggLineInterpolatorAA1 }

constructor TAggLineInterpolatorAA1.Create;
var
  Npix, dx, dy: Integer;
  DistStart: array [0 .. 1] of Integer;
begin
  inherited Create(Ren, LP);

  FDistanceInterpolator := TAggDistanceInterpolator2.Create(LP.x1, LP.y1, LP.x2,
    LP.y2, SX, SY, LP.x1 and not CAggLineSubpixelMask,
    LP.y1 and not CAggLineSubpixelMask);

  Npix := 1;

  if LP.Vertical then
    repeat
      FLineInterpolator.MinusOperator;

      Dec(fy, LP.IncValue);

      fx := ShrInt32(FLineParameters.x1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if LP.IncValue > 0 then
          FDistanceInterpolator.SetDecY(fx - FOldX)
      else
          FDistanceInterpolator.SetIncY(fx - FOldX);

      FOldX := fx;

      DistStart[0] := FDistanceInterpolator.DistanceStart;
      DistStart[1] := DistStart[0];

      dx := 0;

      if DistStart[0] < 0 then
          Inc(Npix);

      repeat
        Inc(DistStart[0], FDistanceInterpolator.DyStart);
        Dec(DistStart[1], FDistanceInterpolator.DyStart);

        if DistStart[0] < 0 then
            Inc(Npix);

        if DistStart[1] < 0 then
            Inc(Npix);

        Inc(dx);

      until FDist[dx] > FWidth;

      Dec(FStep);

      if Npix = 0 then
          Break;

      Npix := 0;

    until FStep < -FMaxExtent
  else
    repeat
      FLineInterpolator.MinusOperator;

      Dec(fx, LP.IncValue);

      fy := ShrInt32(FLineParameters.y1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if LP.IncValue > 0 then
          FDistanceInterpolator.SetDecX(fy - FOldY)
      else
          FDistanceInterpolator.SetIncX(fy - FOldY);

      FOldY := fy;

      DistStart[0] := FDistanceInterpolator.DistanceStart;
      DistStart[1] := DistStart[0];

      dy := 0;

      if DistStart[0] < 0 then
          Inc(Npix);

      repeat
        Dec(DistStart[0], FDistanceInterpolator.DxStart);
        Inc(DistStart[1], FDistanceInterpolator.DxStart);

        if DistStart[0] < 0 then
            Inc(Npix);

        if DistStart[1] < 0 then
            Inc(Npix);

        Inc(dy);

      until FDist[dy] > FWidth;

      Dec(FStep);

      if Npix = 0 then
          Break;

      Npix := 0;

    until FStep < -FMaxExtent;

  FLineInterpolator.AdjustForward;
end;

destructor TAggLineInterpolatorAA1.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited;
end;

function TAggLineInterpolatorAA1.StepHorizontal: Boolean;
var
  DistStart, Dist, dy, s1: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepHorizontalBase(FDistanceInterpolator);

  DistStart := FDistanceInterpolator.DistanceStart;

  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  p1^ := 0;

  if DistStart <= 0 then
      p1^ := Int8u(FRendererBase.Cover(s1));

  Inc(PtrComp(p1), SizeOf(Int8u));

  dy := 1;
  Dist := FDist[dy] - s1;

  while Dist <= FWidth do
    begin
      Dec(DistStart, FDistanceInterpolator.DxStart);

      p1^ := 0;

      if DistStart <= 0 then
          p1^ := Int8u(FRendererBase.Cover(Dist));

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dy);

      Dist := FDist[dy] - s1;
    end;

  dy := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  Dist := FDist[dy] + s1;

  while Dist <= FWidth do
    begin
      Inc(DistStart, FDistanceInterpolator.DxStart);
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := 0;

      if DistStart <= 0 then
          P0^ := Int8u(FRendererBase.Cover(Dist));

      Inc(dy);

      Dist := FDist[dy] + s1;
    end;

  FRendererBase.BlendSolidVSpan(fx, fy - dy + 1,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;

function TAggLineInterpolatorAA1.StepVertical: Boolean;
var
  DistStart, Dist, dx, s1: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  DistStart := FDistanceInterpolator.DistanceStart;

  p1^ := 0;

  if DistStart <= 0 then
      p1^ := Int8u(FRendererBase.Cover(s1));

  Inc(PtrComp(p1), SizeOf(Int8u));

  dx := 1;
  Dist := FDist[dx] - s1;

  while Dist <= FWidth do
    begin
      Inc(DistStart, FDistanceInterpolator.DyStart);

      p1^ := 0;

      if DistStart <= 0 then
          p1^ := Int8u(FRendererBase.Cover(Dist));

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dx);

      Dist := FDist[dx] - s1;
    end;

  dx := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  Dist := FDist[dx] + s1;

  while Dist <= FWidth do
    begin
      Dec(DistStart, FDistanceInterpolator.DyStart);
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := 0;

      if DistStart <= 0 then
          P0^ := Int8u(FRendererBase.Cover(Dist));

      Inc(dx);

      Dist := FDist[dx] + s1;
    end;

  FRendererBase.BlendSolidHSpan(fx - dx + 1, fy,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;

{ TAggLineInterpolatorAA2 }

constructor TAggLineInterpolatorAA2.Create;
begin
  inherited Create(Ren, LP);

  FDistanceInterpolator := TAggDistanceInterpolator2.Create(LP.x1, LP.y1, LP.x2,
    LP.y2, EX, EY, LP.x1 and not CAggLineSubpixelMask,
    LP.y1 and not CAggLineSubpixelMask, 0);

  FLineInterpolator.AdjustForward;

  Dec(FStep, FMaxExtent);
end;

destructor TAggLineInterpolatorAA2.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited
end;

function TAggLineInterpolatorAA2.StepHorizontal: Boolean;
var
  DistEnd, Dist, dy, s1, Npix: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepHorizontalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  p1^ := 0;

  if DistEnd > 0 then
    begin
      p1^ := Int8u(FRendererBase.Cover(s1));

      Inc(Npix);
    end;

  Inc(PtrComp(p1), SizeOf(Int8u));

  dy := 1;
  Dist := FDist[dy] - s1;

  while Dist <= FWidth do
    begin
      Dec(DistEnd, FDistanceInterpolator.DxEnd);

      p1^ := 0;

      if DistEnd > 0 then
        begin
          p1^ := Int8u(FRendererBase.Cover(Dist));

          Inc(Npix);
        end;

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dy);

      Dist := FDist[dy] - s1;
    end;

  dy := 1;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  Dist := FDist[dy] + s1;

  while Dist <= FWidth do
    begin
      Inc(DistEnd, FDistanceInterpolator.DxEnd);
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := 0;

      if DistEnd > 0 then
        begin
          P0^ := Int8u(FRendererBase.Cover(Dist));

          Inc(Npix);
        end;

      Inc(dy);

      Dist := FDist[dy] + s1;
    end;

  FRendererBase.BlendSolidVSpan(fx, fy - dy + 1,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorAA2.StepVertical: Boolean;
var
  DistEnd, Dist, dx, s1, Npix: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  p1^ := 0;

  if DistEnd > 0 then
    begin
      p1^ := Int8u(FRendererBase.Cover(s1));

      Inc(Npix);
    end;

  Inc(PtrComp(p1), SizeOf(Int8u));

  dx := 1;
  Dist := FDist[dx] - s1;

  while Dist <= FWidth do
    begin
      Inc(DistEnd, FDistanceInterpolator.DyEnd);

      p1^ := 0;

      if DistEnd > 0 then
        begin
          p1^ := Int8u(FRendererBase.Cover(Dist));

          Inc(Npix);
        end;

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dx);

      Dist := FDist[dx] - s1;
    end;

  dx := 1;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  Dist := FDist[dx] + s1;

  while Dist <= FWidth do
    begin
      Dec(DistEnd, FDistanceInterpolator.DyEnd);
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := 0;

      if DistEnd > 0 then
        begin
          P0^ := Int8u(FRendererBase.Cover(Dist));

          Inc(Npix);
        end;

      Inc(dx);

      Dist := FDist[dx] + s1;
    end;

  FRendererBase.BlendSolidHSpan(fx - dx + 1, fy,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

{ TAggLineInterpolatorAA3 }

constructor TAggLineInterpolatorAA3.Create(Ren: TAggRendererOutlineAA;
  LP: PAggLineParameters; SX, SY, EX, EY: Integer);
var
  Dist1Start, Dist2Start, Npix, dx, dy: Integer;
begin
  inherited Create(Ren, LP);

  FDistanceInterpolator := TAggDistanceInterpolator3.Create(LP.x1, LP.y1, LP.x2,
    LP.y2, SX, SY, EX, EY, LP.x1 and not CAggLineSubpixelMask,
    LP.y1 and not CAggLineSubpixelMask);

  Npix := 1;

  if LP.Vertical then
    repeat
      FLineInterpolator.MinusOperator;

      Dec(fy, LP.IncValue);

      fx := ShrInt32(FLineParameters.x1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if LP.IncValue > 0 then
          FDistanceInterpolator.SetDecY(fx - FOldX)
      else
          FDistanceInterpolator.SetIncY(fx - FOldX);

      FOldX := fx;

      Dist1Start := FDistanceInterpolator.DistanceStart;
      Dist2Start := Dist1Start;

      dx := 0;

      if Dist1Start < 0 then
          Inc(Npix);

      repeat
        Inc(Dist1Start, FDistanceInterpolator.DyStart);
        Dec(Dist2Start, FDistanceInterpolator.DyStart);

        if Dist1Start < 0 then
            Inc(Npix);

        if Dist2Start < 0 then
            Inc(Npix);

        Inc(dx);
      until FDist[dx] > FWidth;

      if Npix = 0 then
          Break;

      Npix := 0;

      Dec(FStep);
    until FStep < -FMaxExtent
  else
    repeat
      FLineInterpolator.MinusOperator;

      Dec(fx, LP.IncValue);

      fy := ShrInt32(FLineParameters.y1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if LP.IncValue > 0 then
          FDistanceInterpolator.SetDecX(fy - FOldY)
      else
          FDistanceInterpolator.SetIncX(fy - FOldY);

      FOldY := fy;

      Dist1Start := FDistanceInterpolator.DistanceStart;
      Dist2Start := Dist1Start;

      dy := 0;

      if Dist1Start < 0 then
          Inc(Npix);

      repeat
        Dec(Dist1Start, FDistanceInterpolator.DxStart);
        Inc(Dist2Start, FDistanceInterpolator.DxStart);

        if Dist1Start < 0 then
            Inc(Npix);

        if Dist2Start < 0 then
            Inc(Npix);

        Inc(dy);
      until FDist[dy] > FWidth;

      if Npix = 0 then
          Break;

      Npix := 0;

      Dec(FStep);
    until FStep < -FMaxExtent;

  FLineInterpolator.AdjustForward;

  Dec(FStep, FMaxExtent);
end;

destructor TAggLineInterpolatorAA3.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited;
end;

function TAggLineInterpolatorAA3.StepHorizontal: Boolean;
var
  DistStart, DistEnd, GetDist, dy, s1, Npix: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepHorizontalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  p1^ := 0;

  if DistEnd > 0 then
    begin
      if DistStart <= 0 then
          p1^ := Int8u(FRendererBase.Cover(s1));

      Inc(Npix);
    end;

  Inc(PtrComp(p1), SizeOf(Int8u));

  dy := 1;
  GetDist := FDist[dy] - s1;

  while GetDist <= FWidth do
    begin
      Dec(DistStart, FDistanceInterpolator.DxStart);
      Dec(DistEnd, FDistanceInterpolator.DxEnd);

      p1^ := 0;

      if (DistEnd > 0) and (DistStart <= 0) then
        begin
          p1^ := Int8u(FRendererBase.Cover(GetDist));

          Inc(Npix);
        end;

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dy);

      GetDist := FDist[dy] - s1;
    end;

  dy := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  GetDist := FDist[dy] + s1;

  while GetDist <= FWidth do
    begin
      Inc(DistStart, FDistanceInterpolator.DxStart);
      Inc(DistEnd, FDistanceInterpolator.DxEnd);
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := 0;

      if (DistEnd > 0) and (DistStart <= 0) then
        begin
          P0^ := Int8u(FRendererBase.Cover(GetDist));

          Inc(Npix);
        end;

      Inc(dy);

      GetDist := FDist[dy] + s1;
    end;

  FRendererBase.BlendSolidVSpan(fx, fy - dy + 1,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorAA3.StepVertical: Boolean;
var
  DistStart, DistEnd, GetDist, dx, s1, Npix: Integer;
  P0, p1: PInt8u;
begin
  s1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  p1 := P0;

  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  p1^ := 0;

  if DistEnd > 0 then
    begin
      if DistStart <= 0 then
          p1^ := Int8u(FRendererBase.Cover(s1));

      Inc(Npix);
    end;

  Inc(PtrComp(p1), SizeOf(Int8u));

  dx := 1;
  GetDist := FDist[dx] - s1;

  while GetDist <= FWidth do
    begin
      Inc(DistStart, FDistanceInterpolator.DyStart);
      Inc(DistEnd, FDistanceInterpolator.DyEnd);

      p1^ := 0;

      if (DistEnd > 0) and (DistStart <= 0) then
        begin
          p1^ := Int8u(FRendererBase.Cover(GetDist));

          Inc(Npix);
        end;

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dx);

      GetDist := FDist[dx] - s1;
    end;

  dx := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  GetDist := FDist[dx] + s1;

  while GetDist <= FWidth do
    begin
      Dec(DistStart, FDistanceInterpolator.DyStart);
      Dec(DistEnd, FDistanceInterpolator.DyEnd);
      Dec(PtrComp(P0), SizeOf(Int8u));

      P0^ := 0;

      if (DistEnd > 0) and (DistStart <= 0) then
        begin
          P0^ := Int8u(FRendererBase.Cover(GetDist));

          Inc(Npix);
        end;

      Inc(dx);

      GetDist := FDist[dx] + s1;
    end;

  FRendererBase.BlendSolidHSpan(fx - dx + 1, fy,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

{ TAggLineProfileAA }

constructor TAggLineProfileAA.Create;
var
  i: Integer;
begin
  FSize := 0;
  FProfile := 0;

  FSubpixelWidth := 0;
  FMinWidth := 1.0;
  FSmootherWidth := 1.0;

  for i := 0 to CAggAntiAliasingNum - 1 do
      FGamma[i] := Int8u(i);
end;

constructor TAggLineProfileAA.Create(width: Double;
  GammaFunction: TAggCustomVertexSource);
begin
  FSize := 0;
  FProfile := 0;

  FSubpixelWidth := 0;
  FMinWidth := 1.0;
  FSmootherWidth := 1.0;

  SetGamma(GammaFunction);
  SetWidth(width);
end;

destructor TAggLineProfileAA.Destroy;
begin
  AggFreeMem(Pointer(FProfile), FSize * SizeOf(Int8u));
  inherited;
end;

procedure TAggLineProfileAA.SetMinWidth(Value: Double);
begin
  FMinWidth := Value;
end;

procedure TAggLineProfileAA.SetSmootherWidth(Value: Double);
begin
  FSmootherWidth := Value;
end;

procedure TAggLineProfileAA.SetGamma(Value: TAggCustomVertexSource);
var
  i: Integer;
begin
  for i := 0 to CAggAntiAliasingNum - 1 do
      FGamma[i] := Int8u(Trunc(Value.FuncOperatorGamma(i / CAggAntiAliasingMask) *
      CAggAntiAliasingMask + 0.5));
end;

procedure TAggLineProfileAA.SetWidth(Value: Double);
var
  SmootherWidth: Double;
begin
  if Value < 0.0 then
      Value := 0.0;

  if Value < FSmootherWidth then
      Value := Value + Value
  else
      Value := Value + FSmootherWidth;

  Value := 0.5 * Value - FSmootherWidth;
  SmootherWidth := FSmootherWidth;

  if Value < 0.0 then
    begin
      SmootherWidth := SmootherWidth + Value;
      Value := 0.0;
    end;

  SetWidth(Value, SmootherWidth);
end;

function TAggLineProfileAA.GetValue(GetDist: Integer): Int8u;
begin
  Result := PInt8u(PtrComp(FProfile) + (GetDist + CAggSubpixelSize * 2) *
    SizeOf(Int8u))^;
end;

function TAggLineProfileAA.Profile(Value: Double): PInt8u;
var
  Size: Cardinal;
begin
  FSubpixelWidth := Trunc(Value * CAggSubpixelSize);

  Size := FSubpixelWidth + CAggSubpixelSize * 6;

  if Size > FSize then
    begin
      AggFreeMem(Pointer(FProfile), FSize * SizeOf(Int8u));
      AggGetMem(Pointer(FProfile), Size * SizeOf(Int8u));

      FSize := Size;
    end;

  Result := FProfile;
end;

procedure TAggLineProfileAA.SetWidth(CenterWidth, SmootherWidth: Double);
var
  BaseVal, width, k: Double;
  SubpixelCenterWidth, SubpixelSmootherWidth, i, val, SmootherCount: Cardinal;
  Ch, ChCenter, ChCmoother: PInt8u;
begin
  BaseVal := 1.0;

  if CenterWidth = 0.0 then
      CenterWidth := 1.0 / CAggSubpixelSize;

  if SmootherWidth = 0.0 then
      SmootherWidth := 1.0 / CAggSubpixelSize;

  width := CenterWidth + SmootherWidth;

  if width < FMinWidth then
    begin
      k := width / FMinWidth;

      BaseVal := BaseVal * k;
      k := 1 / k;
      CenterWidth := CenterWidth * k;
      SmootherWidth := SmootherWidth * k;
    end;

  Ch := Profile(CenterWidth + SmootherWidth);

  SubpixelCenterWidth := Trunc(CenterWidth * CAggSubpixelSize);
  SubpixelSmootherWidth := Trunc(SmootherWidth * CAggSubpixelSize);

  ChCenter := PInt8u(PtrComp(Ch) + CAggSubpixelSize * 2 * SizeOf(Int8u));
  ChCmoother := PInt8u(PtrComp(ChCenter) + SubpixelCenterWidth *
    SizeOf(Int8u));

  val := FGamma[Trunc(BaseVal * CAggAntiAliasingMask)];

  Ch := ChCenter;

  i := 0;

  while i < SubpixelCenterWidth do
    begin
      Ch^ := Int8u(val);

      Inc(PtrComp(Ch), SizeOf(Int8u));
      Inc(i);
    end;

  i := 0;

  while i < SubpixelSmootherWidth do
    begin
      ChCmoother^ := FGamma
        [Trunc((BaseVal - BaseVal * (i / SubpixelSmootherWidth)) * CAggAntiAliasingMask)];

      Inc(PtrComp(ChCmoother), SizeOf(Int8u));
      Inc(i);
    end;

  SmootherCount := ProfileSize - SubpixelSmootherWidth - SubpixelCenterWidth
    - CAggSubpixelSize * 2;

  val := FGamma[0];

  for i := 0 to SmootherCount - 1 do
    begin
      ChCmoother^ := Int8u(val);

      Inc(PtrComp(ChCmoother), SizeOf(Int8u));
    end;

  Ch := ChCenter;

  for i := 0 to CAggSubpixelSize * 2 - 1 do
    begin
      Ch^ := ChCenter^;

      Dec(PtrComp(Ch), SizeOf(Int8u));
      Inc(PtrComp(ChCenter), SizeOf(Int8u));
    end;
end;

{ TAggRendererOutlineAA }

constructor TAggRendererOutlineAA.Create(Ren: TAggRendererBase;
  Prof: TAggLineProfileAA);
begin
  Assert(Ren is TAggRendererBase);
  FRendererBase := Ren;
  FProfile := Prof;
end;

procedure TAggRendererOutlineAA.SetColor(C: PAggColor);
begin
  FColor := C^;
end;

function TAggRendererOutlineAA.GetColor: PAggColor;
begin
  Result := @FColor;
end;

procedure TAggRendererOutlineAA.SetProfile(Prof: TAggLineProfileAA);
begin
  FProfile := Prof;
end;

function TAggRendererOutlineAA.GetSubpixelWidth: Integer;
begin
  Result := FProfile.SubpixelWidth;
end;

function TAggRendererOutlineAA.Cover(d: Integer): Int8u;
begin
  Result := Int8u(FProfile.GetValue(d));
end;

procedure TAggRendererOutlineAA.BlendSolidHSpan(X, Y: Integer; Len: Cardinal; Covers: PInt8u);
begin
  FRendererBase.BlendSolidHSpan(X, Y, Len, @FColor, Covers);
end;

procedure TAggRendererOutlineAA.BlendSolidVSpan(X, Y: Integer; Len: Cardinal; Covers: PInt8u);
begin
  FRendererBase.BlendSolidVSpan(X, Y, Len, @FColor, Covers);
end;

function TAggRendererOutlineAA.GetAccurateJoinOnly: Boolean;
begin
  Result := False;
end;

procedure TAggRendererOutlineAA.SemidotHorizontalLine(cmp: TCompareFunction;
  Xc1, Yc1, Xc2, Yc2, x1, y1, x2: Integer);
var
  Covers: array [0 .. CMaxHalfWidth * 2 + 4 - 1] of Int8u;
  P0, p1: PInt8u;

  X, Y, w, x0, dx, dy, d: Integer;

  di: TAggDistanceInterpolator0;

begin
  P0 := @Covers[0];
  p1 := @Covers[0];

  X := x1 shl CAggLineSubpixelShift;
  Y := y1 shl CAggLineSubpixelShift;
  w := GetSubpixelWidth;

  di := TAggDistanceInterpolator0.Create(Xc1, Yc1, Xc2, Yc2, X, Y);
  try
    Inc(X, CAggLineSubpixelSize div 2);
    Inc(Y, CAggLineSubpixelSize div 2);

    x0 := x1;
    dx := X - Xc1;
    dy := Y - Yc1;

    repeat
      d := Trunc(FastSqrt(dx * dx + dy * dy));

      p1^ := 0;

      if cmp(di.Distance) and (d <= w) then
          p1^ := Int8u(Cover(d));

      Inc(PtrComp(p1), SizeOf(Int8u));
      Inc(dx, CAggLineSubpixelSize);

      di.IncX;

      Inc(x1);
    until x1 > x2;
  finally
      di.Free;
  end;

  FRendererBase.BlendSolidHSpan(x0, y1,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(Int8u)), GetColor, P0);
end;

procedure TAggRendererOutlineAA.Semidot(cmp: TCompareFunction; Xc1, Yc1,
  Xc2, Yc2: Integer);
var
  Delta: array [0 .. 1] of TPointInteger;
  R, X, Y: Integer;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  R := ShrInt32(GetSubpixelWidth + CAggLineSubpixelMask, CAggLineSubpixelShift);

  if R < 1 then
      R := 1;

  Ei.Initialize(R);

  Delta[0] := PointInteger(0, -R);
  Delta[1] := Delta[0];
  X := ShrInt32(Xc1, CAggLineSubpixelShift);
  Y := ShrInt32(Yc1, CAggLineSubpixelShift);

  repeat
    Inc(Delta[0].X, Ei.deltax);
    Inc(Delta[0].Y, Ei.deltay);

    if Delta[0].Y <> Delta[1].Y then
      begin
        SemidotHorizontalLine(cmp, Xc1, Yc1, Xc2, Yc2, X - Delta[1].X,
          Y + Delta[1].Y, X + Delta[1].X);
        SemidotHorizontalLine(cmp, Xc1, Yc1, Xc2, Yc2, X - Delta[1].X,
          Y - Delta[1].Y, X + Delta[1].X);
      end;

    Delta[1] := Delta[0];

    Ei.IncOperator;
  until Delta[0].Y >= 0;

  SemidotHorizontalLine(cmp, Xc1, Yc1, Xc2, Yc2, X - Delta[1].X, Y + Delta[1].Y,
    X + Delta[1].X);
end;

procedure TAggRendererOutlineAA.Line0(LP: PAggLineParameters);
var
  Li: TAggLineInterpolatorAA0;
begin
  Li := TAggLineInterpolatorAA0.Create(Self, LP);
  try
    if Li.Count <> 0 then
      if Li.Vertical then
        while Li.StepVertical do
        else
          while Li.StepHorizontal do;
  finally
      Li.Free;
  end;
end;

procedure TAggRendererOutlineAA.Line1(LP: PAggLineParameters; SX, SY: Integer);
var
  Li: TAggLineInterpolatorAA1;
begin
  FixDegenerateBisectrixStart(LP, @SX, @SY);

  Li := TAggLineInterpolatorAA1.Create(Self, LP, SX, SY);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;
  finally
      Li.Free;
  end;
end;

procedure TAggRendererOutlineAA.Line2(LP: PAggLineParameters; EX, EY: Integer);
var
  Li: TAggLineInterpolatorAA2;
begin
  FixDegenerateBisectrixEnd(LP, @EX, @EY);

  Li := TAggLineInterpolatorAA2.Create(Self, LP, EX, EY);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;
  finally
      Li.Free;
  end;
end;

procedure TAggRendererOutlineAA.Line3(LP: PAggLineParameters; SX, SY,
  EX, EY: Integer);
var
  Li: TAggLineInterpolatorAA3;
begin
  FixDegenerateBisectrixStart(LP, @SX, @SY);
  FixDegenerateBisectrixEnd(LP, @EX, @EY);

  Li := TAggLineInterpolatorAA3.Create(Self, LP, SX, SY, EX, EY);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;
  finally
      Li.Free;
  end;
end;

end. 
