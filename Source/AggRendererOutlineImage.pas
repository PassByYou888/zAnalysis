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
unit AggRendererOutlineImage;

interface

{$INCLUDE AggCompiler.inc}


uses
  Math,
  AggBasics,
  AggColor32,
  AggLineAABasics,
  AggDdaLine,
  AggRenderingBuffer,
  AggRendererBase,
  AggRendererOutlineAA,
  AggPatternFiltersRgba;

type
  TAggPixelSource = class
  protected
    function GetWidth: Cardinal; virtual; abstract;
    function GetHeight: Cardinal; virtual; abstract;

    function GetWidthDouble: Double; virtual;
    function GetHeightDouble: Double; virtual;
  public
    function Pixel(X, Y: Integer): TAggRgba8; virtual; abstract;

    property width: Cardinal read GetWidth;
    property height: Cardinal read GetHeight;
    property WidthDouble: Double read GetWidthDouble;
    property HeightDouble: Double read GetHeightDouble;
  end;

  TAggLineImageScale = class(TAggPixelSource)
  private
    FSource: TAggPixelSource;
    FHeight, FScale: Double;
  protected
    function GetWidthDouble: Double; override;
    function GetHeightDouble: Double; override;
  public
    constructor Create(Src: TAggPixelSource; AHeight: Double);

    function Pixel(X, Y: Integer): TAggRgba8; override;
  end;

  TAggLineImagePattern = class
  private
    FRenderingBuffer: TAggRenderingBuffer;
    FFilter: TAggPatternFilter;

    FDilation: Cardinal;
    FDilationHR: Integer;

    FData: PAggColor;
    FDataSize, FWidth, FHeight: Cardinal;

    FWidthHR, FHalfHeightHR, FOffsetYhr: Integer;
    procedure SetFilter(aFilter: TAggPatternFilter); overload;
  public
    constructor Create(aFilter: TAggPatternFilter); overload; virtual;
    constructor Create(aFilter: TAggPatternFilter; Src: TAggPixelSource); overload; virtual;
    destructor Destroy; override;

    procedure Build(Src: TAggPixelSource); virtual;

    procedure Pixel(p: PAggColor; X, Y: Integer); virtual;

    property Filter: TAggPatternFilter read FFilter write SetFilter;
    property PatternWidth: Integer read FWidthHR;
    property LineWidth: Integer read FHalfHeightHR;
  end;

  TAggLineImagePatternPow2 = class(TAggLineImagePattern)
  private
    FMask: Cardinal;
  public
    constructor Create(aFilter: TAggPatternFilter); overload; override;
    constructor Create(aFilter: TAggPatternFilter; Src: TAggPixelSource); overload; override;

    procedure Build(Src: TAggPixelSource); override;
    procedure Pixel(p: PAggColor; X, Y: Integer); override;
  end;

  TAggDistanceInterpolator4 = class(TAggCustomDistance2Interpolator)
  private
    FDelta, FDeltaStart, FDeltaPict, FDeltaEnd: TPointInteger;
    FDist, FDistStart, FDistPict, FDistEnd, FLength: Integer;
  protected
    function GetDeltaX: Integer; override;
    function GetDeltaXEnd: Integer; override;
    function GetDeltaXStart: Integer; override;
    function GetDeltaY: Integer; override;
    function GetDeltaYEnd: Integer; override;
    function GetDeltaYStart: Integer; override;
    function GetDistance: Integer; override;
    function GetDistanceStart: Integer; override;
    function GetDistPict: Integer;
    function GetDistanceEnd: Integer; override;
  public
    constructor Create(x1, y1, x2, y2, SX, SY, EX, EY, length: Integer; Scale: Double; X, Y: Integer); overload;

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;

    property length: Integer read FLength;
    property DxPict: Integer read FDeltaPict.X;
    property DyPict: Integer read FDeltaPict.Y;
  end;

  TAggRendererOutlineImage = class;

  TAggLineInterpolatorImage = class // (lineInterpolator)
  private
    FLineParameters: PAggLineParameters;
    FLineInterpolator: TAggDda2LineInterpolator;
    FDistanceInterpolator: TAggDistanceInterpolator4;
    FRendererOutlineImage: TAggRendererOutlineImage;

    fx, fy, FOldX, FOldY, FCount, FWidth: Integer;
    FMaxExtent, FStart, FStep: Integer;

    FDistPos: array [0 .. CMaxHalfWidth + 1 - 1] of Integer;
    FColors: array [0 .. CMaxHalfWidth * 2 + 4 - 1] of TAggColor;
  protected
    function GetPatternEnd: Integer;
    function GetVertical: Boolean;
  public
    constructor Create(Ren: TAggRendererOutlineImage; LP: PAggLineParameters; SX, SY, EX, EY, PatternStart: Integer; ScaleX: Double);
    destructor Destroy; override;

    function StepHorizontal: Boolean;
    function StepVertical: Boolean;

    property PatternEnd: Integer read GetPatternEnd;
    property Vertical: Boolean read GetVertical;
    property width: Integer read FWidth;
    property Count: Integer read FCount;
  end;

  TAggRendererOutlineImage = class(TAggRendererOutline)
  private
    FRendererOutlineImage: TAggRendererBase;
    FPattern: TAggLineImagePattern;
    FStart: Integer;
    FScaleX: Double;

    procedure SetScaleX(s: Double);
    function GetScaleX: Double;

    procedure SetStartX(s: Double);
    function GetStartX: Double;
    procedure SetPattern(p: TAggLineImagePattern);
  protected
    function GetPatternWidth: Integer;
    function GetAccurateJoinOnly: Boolean; override;
    function GetSubpixelWidth: Integer; override;
  public
    constructor Create(Ren: TAggRendererBase; Patt: TAggLineImagePattern);

    procedure Pixel(p: PAggColor; X, Y: Integer);

    procedure BlendColorHSpan(X, Y: Integer; Len: Cardinal; COLORS: PAggColor);
    procedure BlendColorVSpan(X, Y: Integer; Len: Cardinal; COLORS: PAggColor);

    procedure Semidot(cmp: TCompareFunction; Xc1, Yc1, Xc2, Yc2: Integer); override;

    procedure Line0(LP: PAggLineParameters); override;
    procedure Line1(LP: PAggLineParameters; SX, SY: Integer); override;
    procedure Line2(LP: PAggLineParameters; EX, EY: Integer); override;
    procedure Line3(LP: PAggLineParameters; SX, SY, EX, EY: Integer); override;

    property StartX: Double read GetStartX write SetStartX;
    property ScaleX: Double read GetScaleX write SetScaleX;

    property PatternWidth: Integer read GetPatternWidth;
    property Pattern: TAggLineImagePattern read FPattern write SetPattern;
  end;

implementation

{ TAggPixelSource }

function TAggPixelSource.GetWidthDouble;
begin
  Result := GetWidth;
end;

function TAggPixelSource.GetHeightDouble;
begin
  Result := GetHeight;
end;

{ TAggLineImageScale }

constructor TAggLineImageScale.Create(Src: TAggPixelSource; AHeight: Double);
begin
  FSource := Src;
  FHeight := AHeight;

  if AHeight <> 0 then
      FScale := Src.GetHeight / AHeight
  else
      FScale := 0;
end;

function TAggLineImageScale.GetWidthDouble;
begin
  Result := FSource.GetWidth;
end;

function TAggLineImageScale.GetHeightDouble;
begin
  Result := FHeight;
end;

function TAggLineImageScale.Pixel(X, Y: Integer): TAggRgba8;
var
  SourceY: Double;
  h, y1, y2: Integer;
  pix1, pix2: TAggColor;
begin
  SourceY := (Y + 0.5) * FScale - 0.5;

  h := Trunc(FSource.GetHeight) - 1;
  y1 := Trunc(SourceY);
  y2 := y1 + 1;

  if y1 >= 0 then
      pix1.FromRgba8(FSource.Pixel(X, y1));

  if y2 <= h then
      pix2.FromRgba8(FSource.Pixel(X, y2));

  Result := pix1.Gradient8(pix2, SourceY - y1);
end;

{ TAggLineImagePattern }

constructor TAggLineImagePattern.Create(aFilter: TAggPatternFilter);
begin
  FRenderingBuffer := TAggRenderingBuffer.Create;

  Assert(Assigned(aFilter));
  FFilter := aFilter;

  FDilation := aFilter.Dilation + 1;
  FDilationHR := FDilation shl CAggLineSubpixelShift;

  FData := 0;
  FDataSize := 0;
  FWidth := 0;
  FHeight := 0;

  FWidthHR := 0;
  FHalfHeightHR := 0;
  FOffsetYhr := 0;
end;

constructor TAggLineImagePattern.Create(aFilter: TAggPatternFilter;
  Src: TAggPixelSource);
begin
  FRenderingBuffer := TAggRenderingBuffer.Create;

  Assert(Assigned(aFilter));
  FFilter := aFilter;

  FDilation := aFilter.Dilation + 1;
  FDilationHR := FDilation shl CAggLineSubpixelShift;

  FData := 0;
  FDataSize := 0;
  FWidth := 0;
  FHeight := 0;

  FWidthHR := 0;
  FHalfHeightHR := 0;
  FOffsetYhr := 0;

  Build(Src);
end;

destructor TAggLineImagePattern.Destroy;
begin
  AggFreeMem(Pointer(FData), FDataSize);
  FRenderingBuffer.Free;
  inherited;
end;

procedure TAggLineImagePattern.Build(Src: TAggPixelSource);
var
  X, Y, h: Cardinal;
  d1, d2, s1, s2: PAggRgba8;
begin
  FHeight := Ceil(Src.GetHeightDouble);
  FWidth := Ceil(Src.GetWidthDouble);

  FWidthHR := Trunc(Src.GetWidthDouble * CAggLineSubpixelSize);
  FHalfHeightHR := Trunc(Src.GetHeightDouble * CAggLineSubpixelSize * 0.5);
  FOffsetYhr := FDilationHR + FHalfHeightHR - CAggLineSubpixelSize div 2;

  Inc(FHalfHeightHR, CAggLineSubpixelSize div 2);

  AggFreeMem(Pointer(FData), FDataSize);

  FDataSize := (FWidth + FDilation * 2) * (FHeight + FDilation * 2) *
    SizeOf(TAggRgba8);

  AggGetMem(Pointer(FData), FDataSize);

  FRenderingBuffer.Attach(PInt8u(FData), FWidth + FDilation * 2,
    FHeight + FDilation * 2, (FWidth + FDilation * 2) * SizeOf(TAggRgba8));

  if FHeight > 0 then
    for Y := 0 to FHeight - 1 do
      begin
        d1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y + FDilation)) + FDilation *
          SizeOf(TAggRgba8));

        for X := 0 to FWidth - 1 do
          begin
            d1^ := Src.Pixel(X, Y);

            Inc(PtrComp(d1), SizeOf(TAggRgba8));
          end;
      end;

  for Y := 0 to FDilation - 1 do
    begin
      d1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(FDilation + FHeight + Y)) +
        FDilation * SizeOf(TAggRgba8));
      d2 := PAggRgba8(PtrComp(FRenderingBuffer.Row(FDilation - Y - 1)) +
        FDilation * SizeOf(TAggRgba8));

      for X := 0 to FWidth - 1 do
        begin
          d1^.NoColor;
          d2^.NoColor;

          Inc(PtrComp(d1), SizeOf(TAggRgba8));
          Inc(PtrComp(d2), SizeOf(TAggRgba8));
        end;
    end;

  h := FHeight + FDilation * 2;

  for Y := 0 to h - 1 do
    begin
      s1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) +
        FDilation * SizeOf(TAggRgba8));
      s2 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) + (FDilation + FWidth) *
        SizeOf(TAggRgba8));
      d1 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) + (FDilation + FWidth) *
        SizeOf(TAggRgba8));
      d2 := PAggRgba8(PtrComp(FRenderingBuffer.Row(Y)) +
        FDilation * SizeOf(TAggRgba8));

      for X := 0 to FDilation - 1 do
        begin
          d1^ := s1^;

          Inc(PtrComp(d1), SizeOf(TAggRgba8));
          Inc(PtrComp(s1), SizeOf(TAggRgba8));
          Dec(PtrComp(d2), SizeOf(TAggRgba8));
          Dec(PtrComp(s2), SizeOf(TAggRgba8));

          d2^ := s2^;
        end;
    end;
end;

procedure TAggLineImagePattern.Pixel(p: PAggColor; X, Y: Integer);
begin
  FFilter.PixelHighResolution(FRenderingBuffer.Rows, p, X mod FWidthHR + FDilationHR,
    Y + FOffsetYhr);
end;

procedure TAggLineImagePattern.SetFilter(aFilter: TAggPatternFilter);
begin
  FFilter := aFilter;
end;

{ TAggLineImagePatternPow2 }

constructor TAggLineImagePatternPow2.Create(aFilter: TAggPatternFilter);
begin
  inherited Create(aFilter);

  FMask := 0;
end;

constructor TAggLineImagePatternPow2.Create(aFilter: TAggPatternFilter;
  Src: TAggPixelSource);
begin
  inherited Create(aFilter, Src);

  Build(Src);
end;

procedure TAggLineImagePatternPow2.Build(Src: TAggPixelSource);
begin
  inherited Build(Src);

  FMask := 1;

  while FMask < FWidth do
    begin
      FMask := FMask shl 1;
      FMask := FMask or 1;
    end;

  FMask := FMask shl (CAggLineSubpixelShift - 1);
  FMask := FMask or CAggLineSubpixelMask;

  FWidthHR := FMask + 1;
end;

procedure TAggLineImagePatternPow2.Pixel(p: PAggColor; X, Y: Integer);
begin
  FFilter.PixelHighResolution(FRenderingBuffer.Rows, p, (X and FMask) +
    FDilationHR, Y + FOffsetYhr);
end;

{ TAggDistanceInterpolator4 }

constructor TAggDistanceInterpolator4.Create(x1, y1, x2, y2, SX, SY, EX, EY,
  length: Integer; Scale: Double; X, Y: Integer);
var
  d: Double;
  Delta: TPointInteger;
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

  if Scale <> 0 then
      FLength := Trunc(length / Scale)
  else
      FLength := 0;

  d := length * Scale;

  if d <> 0 then
    begin
      d := 1 / d;
      Delta.X := Trunc(((x2 - x1) shl CAggLineSubpixelShift) * d);
      Delta.Y := Trunc(((y2 - y1) shl CAggLineSubpixelShift) * d);
    end
  else
      Delta := PointInteger(0, 0);

  FDeltaPict := PointInteger(-Delta.Y, Delta.X);

  FDistPict := ShrInt32((X + CAggLineSubpixelSize div 2 - (x1 - Delta.Y)) *
    FDeltaPict.Y - (Y + CAggLineSubpixelSize div 2 - (y1 + Delta.X)) *
    FDeltaPict.X, CAggLineSubpixelShift);

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;
  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;
  FDeltaEnd.X := FDeltaEnd.X shl CAggLineMrSubpixelShift;
  FDeltaEnd.Y := FDeltaEnd.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator4.IncX;
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistPict, FDeltaPict.Y);
  Inc(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator4.DecX;
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistPict, FDeltaPict.Y);
  Dec(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator4.IncY;
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistPict, FDeltaPict.X);
  Dec(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator4.DecY;
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistPict, FDeltaPict.X);
  Inc(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator4.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistPict, FDeltaPict.Y);
  Inc(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
    begin
      Dec(FDist, FDelta.X);
      Dec(FDistStart, FDeltaStart.X);
      Dec(FDistPict, FDeltaPict.X);
      Dec(FDistEnd, FDeltaEnd.X);
    end;

  if Value < 0 then
    begin
      Inc(FDist, FDelta.X);
      Inc(FDistStart, FDeltaStart.X);
      Inc(FDistPict, FDeltaPict.X);
      Inc(FDistEnd, FDeltaEnd.X);
    end;
end;

procedure TAggDistanceInterpolator4.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistPict, FDeltaPict.Y);
  Dec(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
    begin
      Dec(FDist, FDelta.X);
      Dec(FDistStart, FDeltaStart.X);
      Dec(FDistPict, FDeltaPict.X);
      Dec(FDistEnd, FDeltaEnd.X);
    end;

  if Value < 0 then
    begin
      Inc(FDist, FDelta.X);
      Inc(FDistStart, FDeltaStart.X);
      Inc(FDistPict, FDeltaPict.X);
      Inc(FDistEnd, FDeltaEnd.X);
    end;
end;

procedure TAggDistanceInterpolator4.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistPict, FDeltaPict.X);
  Dec(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
    begin
      Inc(FDist, FDelta.Y);
      Inc(FDistStart, FDeltaStart.Y);
      Inc(FDistPict, FDeltaPict.Y);
      Inc(FDistEnd, FDeltaEnd.Y);
    end;

  if Value < 0 then
    begin
      Dec(FDist, FDelta.Y);
      Dec(FDistStart, FDeltaStart.Y);
      Dec(FDistPict, FDeltaPict.Y);
      Dec(FDistEnd, FDeltaEnd.Y);
    end;
end;

procedure TAggDistanceInterpolator4.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistPict, FDeltaPict.X);
  Inc(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
    begin
      Inc(FDist, FDelta.Y);
      Inc(FDistStart, FDeltaStart.Y);
      Inc(FDistPict, FDeltaPict.Y);
      Inc(FDistEnd, FDeltaEnd.Y);
    end;

  if Value < 0 then
    begin
      Dec(FDist, FDelta.Y);
      Dec(FDistStart, FDeltaStart.Y);
      Dec(FDistPict, FDeltaPict.Y);
      Dec(FDistEnd, FDeltaEnd.Y);
    end;
end;

function TAggDistanceInterpolator4.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator4.GetDistanceStart;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator4.GetDistPict;
begin
  Result := FDistPict;
end;

function TAggDistanceInterpolator4.GetDistanceEnd;
begin
  Result := FDistEnd;
end;

function TAggDistanceInterpolator4.GetDeltaX: Integer;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator4.GetDeltaY: Integer;
begin
  Result := FDelta.Y;
end;

function TAggDistanceInterpolator4.GetDeltaXStart: Integer;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator4.GetDeltaYStart: Integer;
begin
  Result := FDeltaStart.Y;
end;

function TAggDistanceInterpolator4.GetDeltaXEnd: Integer;
begin
  Result := FDeltaEnd.X;
end;

function TAggDistanceInterpolator4.GetDeltaYEnd: Integer;
begin
  Result := FDeltaEnd.Y;
end;

{ TAggLineInterpolatorImage }

constructor TAggLineInterpolatorImage.Create(Ren: TAggRendererOutlineImage;
  LP: PAggLineParameters; SX, SY, EX, EY, PatternStart: Integer;
  ScaleX: Double);
var
  i: Cardinal;
  Delta: TPointInteger;
  DistStart: array [0 .. 1] of Integer;
  stop, Npix: Integer;
  Li: TAggDda2LineInterpolator;
begin
  FLineParameters := LP;

  if LP.Vertical then
      FLineInterpolator.Initialize(LineDoubleHighResolution(LP.x2 - LP.x1),
      Abs(LP.y2 - LP.y1))
  else
      FLineInterpolator.Initialize(LineDoubleHighResolution(LP.y2 - LP.y1),
      Abs(LP.x2 - LP.x1) + 1);

  FDistanceInterpolator := TAggDistanceInterpolator4.Create(LP.x1, LP.y1, LP.x2,
    LP.y2, SX, SY, EX, EY, LP.Len, ScaleX, LP.x1 and not CAggLineSubpixelMask,
    LP.y1 and not CAggLineSubpixelMask);

  FRendererOutlineImage := Ren;

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

  try
      FStart := PatternStart + (FMaxExtent + 2) * Ren.GetPatternWidth;
  except
      FStart := 0 + (FMaxExtent + 2) * Ren.GetPatternWidth;
  end;

  FStep := 0;

  if LP.Vertical then
      Li.Initialize(0, LP.Delta.Y shl CAggLineSubpixelShift, LP.Len)
  else
      Li.Initialize(0, LP.Delta.X shl CAggLineSubpixelShift, LP.Len);

  stop := FWidth + CAggLineSubpixelSize * 2;
  i := 0;

  while i < CMaxHalfWidth do
    begin
      FDistPos[i] := Li.Y;

      if FDistPos[i] >= stop then
          Break;

      Li.PlusOperator;

      Inc(i);
    end;

  FDistPos[i] := $7FFF0000;

  Npix := 1;

  if LP.Vertical then
    repeat
      FLineInterpolator.MinusOperator;

      Dec(fy, LP.IncValue);

      fx := ShrInt32(FLineParameters.x1 + FLineInterpolator.Y,
        CAggLineSubpixelShift);

      if LP.IncValue > 0 then
          FDistanceInterpolator.SetDecY(fx - FOldX)
      else
          FDistanceInterpolator.SetIncY(fx - FOldX);

      FOldX := fx;

      DistStart[0] := FDistanceInterpolator.GetDistanceStart;
      DistStart[1] := DistStart[0];

      Delta.X := 0;

      if DistStart[0] < 0 then
          Inc(Npix);

      repeat
        Inc(DistStart[0], FDistanceInterpolator.DyStart);
        Dec(DistStart[1], FDistanceInterpolator.DyStart);

        if DistStart[0] < 0 then
            Inc(Npix);

        if DistStart[1] < 0 then
            Inc(Npix);

        Inc(Delta.X);

      until FDistPos[Delta.X] > FWidth;

      if Npix = 0 then
          Break;

      Npix := 0;

      Dec(FStep);

    until FStep < -FMaxExtent
  else
    repeat
      FLineInterpolator.MinusOperator;

      Dec(fx, LP.IncValue);

      fy := ShrInt32(FLineParameters.y1 + FLineInterpolator.Y,
        CAggLineSubpixelShift);

      if LP.IncValue > 0 then
          FDistanceInterpolator.SetDecX(fy - FOldY)
      else
          FDistanceInterpolator.SetIncX(fy - FOldY);

      FOldY := fy;

      DistStart[0] := FDistanceInterpolator.GetDistanceStart;
      DistStart[1] := DistStart[0];

      Delta.Y := 0;

      if DistStart[0] < 0 then
          Inc(Npix);

      repeat
        Dec(DistStart[0], FDistanceInterpolator.DxStart);
        Inc(DistStart[1], FDistanceInterpolator.DxStart);

        if DistStart[0] < 0 then
            Inc(Npix);

        if DistStart[1] < 0 then
            Inc(Npix);

        Inc(Delta.Y);

      until FDistPos[Delta.Y] > FWidth;

      if Npix = 0 then
          Break;

      Npix := 0;

      Dec(FStep);

    until FStep < -FMaxExtent;

  FLineInterpolator.AdjustForward;

  Dec(FStep, FMaxExtent);
end;

destructor TAggLineInterpolatorImage.Destroy;
begin
  FDistanceInterpolator.Free;

  inherited;
end;

function TAggLineInterpolatorImage.StepHorizontal: Boolean;
var
  s1, s2, DistanceStart, DistPict, DistanceEnd, deltay, Distance, Npix: Integer;
  P0, p1: PAggColor;
begin
  FLineInterpolator.PlusOperator;

  Inc(fx, FLineParameters.IncValue);

  fy := ShrInt32(FLineParameters.y1 + FLineInterpolator.Y,
    CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
      FDistanceInterpolator.SetIncX(fy - FOldY)
  else
      FDistanceInterpolator.SetDecX(fy - FOldY);

  FOldY := fy;

  s1 := FDistanceInterpolator.GetDistance div FLineParameters.Len;
  s2 := -s1;

  if FLineParameters.IncValue < 0 then
      s1 := -s1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  P0 := PAggColor(PtrComp(@FColors[0]) + (CMaxHalfWidth + 2) *
    SizeOf(TAggColor));
  p1 := P0;

  Npix := 0;

  p1.Clear;

  if DistanceEnd > 0 then
    begin
      if DistanceStart <= 0 then
          FRendererOutlineImage.Pixel(p1, DistPict, s2);

      Inc(Npix);
    end;

  Inc(PtrComp(p1), SizeOf(TAggColor));

  deltay := 1;
  Distance := FDistPos[deltay];

  while Distance - s1 <= FWidth do
    begin
      Dec(DistanceStart, FDistanceInterpolator.DxStart);
      Dec(DistPict, FDistanceInterpolator.DxPict);
      Dec(DistanceEnd, FDistanceInterpolator.DxEnd);

      p1.Clear;

      if (DistanceEnd > 0) and (DistanceStart <= 0) then
        begin
          if FLineParameters.IncValue > 0 then
              Distance := -Distance;

          FRendererOutlineImage.Pixel(p1, DistPict, s2 - Distance);

          Inc(Npix);
        end;

      Inc(PtrComp(p1), SizeOf(TAggColor));
      Inc(deltay);

      Distance := FDistPos[deltay];
    end;

  deltay := 1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  Distance := FDistPos[deltay];

  while Distance + s1 <= FWidth do
    begin
      Inc(DistanceStart, FDistanceInterpolator.DxStart);
      Inc(DistPict, FDistanceInterpolator.DxPict);
      Inc(DistanceEnd, FDistanceInterpolator.DxEnd);

      Dec(PtrComp(P0), SizeOf(TAggColor));

      P0.Clear;

      if (DistanceEnd > 0) and (DistanceStart <= 0) then
        begin
          if FLineParameters.IncValue > 0 then
              Distance := -Distance;

          FRendererOutlineImage.Pixel(P0, DistPict, s2 + Distance);

          Inc(Npix);
        end;

      Inc(deltay);

      Distance := FDistPos[deltay];
    end;

  FRendererOutlineImage.BlendColorVSpan(fx, fy - deltay + 1,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(TAggColor)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorImage.StepVertical;
var
  s1, s2, DistanceStart, DistPict, DistanceEnd, deltax, Distance, Npix: Integer;
  P0, p1: PAggColor;
begin
  FLineInterpolator.PlusOperator;

  Inc(fy, FLineParameters.IncValue);

  fx := ShrInt32(FLineParameters.x1 + FLineInterpolator.Y,
    CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
      FDistanceInterpolator.SetIncY(fx - FOldX)
  else
      FDistanceInterpolator.SetDecY(fx - FOldX);

  FOldX := fx;

  s1 := FDistanceInterpolator.GetDistance div FLineParameters.Len;
  s2 := -s1;

  if FLineParameters.IncValue > 0 then
      s1 := -s1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  P0 := PAggColor(PtrComp(@FColors[0]) + (CMaxHalfWidth + 2) *
    SizeOf(TAggColor));
  p1 := P0;

  Npix := 0;

  p1.Clear;

  if DistanceEnd > 0 then
    begin
      if DistanceStart <= 0 then
          FRendererOutlineImage.Pixel(p1, DistPict, s2);

      Inc(Npix);
    end;

  Inc(PtrComp(p1), SizeOf(TAggColor));

  deltax := 1;
  Distance := FDistPos[deltax];

  while Distance - s1 <= FWidth do
    begin
      Inc(DistanceStart, FDistanceInterpolator.DyStart);
      Inc(DistPict, FDistanceInterpolator.DyPict);
      Inc(DistanceEnd, FDistanceInterpolator.DyEnd);

      p1.Clear;

      if (DistanceEnd > 0) and (DistanceStart <= 0) then
        begin
          if FLineParameters.IncValue > 0 then
              Distance := -Distance;

          FRendererOutlineImage.Pixel(p1, DistPict, s2 + Distance);

          Inc(Npix);
        end;

      Inc(PtrComp(p1), SizeOf(TAggColor));
      Inc(deltax);

      Distance := FDistPos[deltax];
    end;

  deltax := 1;

  DistanceStart := FDistanceInterpolator.GetDistanceStart;
  DistPict := FDistanceInterpolator.GetDistPict + FStart;
  DistanceEnd := FDistanceInterpolator.GetDistanceEnd;

  Distance := FDistPos[deltax];

  while Distance + s1 <= FWidth do
    begin
      Dec(DistanceStart, FDistanceInterpolator.DyStart);
      Dec(DistPict, FDistanceInterpolator.DyPict);
      Dec(DistanceEnd, FDistanceInterpolator.DyEnd);

      Dec(PtrComp(P0), SizeOf(TAggColor));

      P0.Clear;

      if (DistanceEnd > 0) and (DistanceStart <= 0) then
        begin
          if FLineParameters.IncValue > 0 then
              Distance := -Distance;

          FRendererOutlineImage.Pixel(P0, DistPict, s2 - Distance);

          Inc(Npix);
        end;

      Inc(deltax);

      Distance := FDistPos[deltax];
    end;

  FRendererOutlineImage.BlendColorHSpan(fx - deltax + 1, fy,
    Cardinal((PtrComp(p1) - PtrComp(P0)) div SizeOf(TAggColor)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorImage.GetPatternEnd;
begin
  Result := FStart + FDistanceInterpolator.length;
end;

function TAggLineInterpolatorImage.GetVertical;
begin
  Result := FLineParameters.Vertical;
end;

{ TAggRendererOutlineImage }

constructor TAggRendererOutlineImage.Create(Ren: TAggRendererBase;
  Patt: TAggLineImagePattern);
begin
  Assert(Ren is TAggRendererBase);
  FRendererOutlineImage := Ren;
  FPattern := Patt;
  FStart := 0;
  FScaleX := 1.0;
end;

procedure TAggRendererOutlineImage.SetPattern;
begin
  FPattern := p;
end;

procedure TAggRendererOutlineImage.SetScaleX;
begin
  FScaleX := s;
end;

function TAggRendererOutlineImage.GetScaleX;
begin
  Result := FScaleX;
end;

procedure TAggRendererOutlineImage.SetStartX;
begin
  FStart := Trunc(s * CAggLineSubpixelSize);
end;

function TAggRendererOutlineImage.GetStartX;
begin
  Result := FStart / CAggLineSubpixelSize;
end;

function TAggRendererOutlineImage.GetSubpixelWidth;
begin
  Result := FPattern.LineWidth;
end;

function TAggRendererOutlineImage.GetPatternWidth;
begin
  Result := FPattern.PatternWidth;
end;

procedure TAggRendererOutlineImage.Pixel;
begin
  FPattern.Pixel(p, X, Y);
end;

procedure TAggRendererOutlineImage.BlendColorHSpan;
begin
  FRendererOutlineImage.BlendColorHSpan(X, Y, Len, COLORS, nil);
end;

procedure TAggRendererOutlineImage.BlendColorVSpan;
begin
  FRendererOutlineImage.BlendColorVSpan(X, Y, Len, COLORS, nil);
end;

function TAggRendererOutlineImage.GetAccurateJoinOnly;
begin
  Result := True;
end;

procedure TAggRendererOutlineImage.Semidot;
begin
end;

procedure TAggRendererOutlineImage.Line0;
begin
end;

procedure TAggRendererOutlineImage.Line1;
begin
end;

procedure TAggRendererOutlineImage.Line2;
begin
end;

procedure TAggRendererOutlineImage.Line3(LP: PAggLineParameters; SX, SY,
  EX, EY: Integer);
var
  Li: TAggLineInterpolatorImage;
begin
  FixDegenerateBisectrixStart(LP, @SX, @SY);
  FixDegenerateBisectrixEnd(LP, @EX, @EY);

  Li := TAggLineInterpolatorImage.Create(Self, LP, SX, SY, EX, EY, FStart,
    FScaleX);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;

    FStart := Li.PatternEnd;
  finally
      Li.Free;
  end;
end;

end. 
 
