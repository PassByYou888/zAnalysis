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
unit AggSpanPatternFilterRgba;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggColor32,
  AggSpanPattern,
  AggSpanImageFilter,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer,
  AggSpanAllocator,
  AggImageFilters;

const
  CAggBaseShift = AggColor32.CAggBaseShift;
  CAggBaseMask  = AggColor32.CAggBaseMask;

type
  TAggCustomSpanPatternFilterRgba = class(TAggSpanImageFilter)
  protected
    FWrapModeX, FWrapModeY: TAggWrapMode;
    FOrder: TAggOrder;
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;
  end;

  TAggSpanPatternFilterRgbaNN = class(TAggCustomSpanPatternFilterRgba)
  public
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterRgbaBilinear = class(TAggCustomSpanPatternFilterRgba)
  public
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterRgba2x2 = class(TAggCustomSpanPatternFilterRgba)
  public
    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterRgba = class(TAggCustomSpanPatternFilterRgba)
  public
    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggCustomSpanPatternFilterRgba }

constructor TAggCustomSpanPatternFilterRgba.Create(Alloc: TAggSpanAllocator; Wx,
  Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggCustomSpanPatternFilterRgba.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder);
var
  RGBA: TAggColor;
begin
  RGBA.Clear;

  inherited Create(Alloc, Src, @RGBA, Interpolator, Filter);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

procedure TAggCustomSpanPatternFilterRgba.SetSourceImage(
  Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

{ TAggSpanPatternFilterRgbaNN }

constructor TAggSpanPatternFilterRgbaNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Wx, Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc, Src, Interpolator, nil, Wx, Wy, Order);
end;

function TAggSpanPatternFilterRgbaNN.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@X, @Y);

    X := FWrapModeX.FuncOperator(ShrInt32(X, CAggImageSubpixelShift));
    Y := FWrapModeY.FuncOperator(ShrInt32(Y, CAggImageSubpixelShift));

    ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(Y)) + (X shl 2) *
      SizeOf(Int8u));

    Span.Rgba8.R := PInt8u(PtrComp(ForeGroundPointer) + FOrder.R * SizeOf(Int8u))^;
    Span.Rgba8.g := PInt8u(PtrComp(ForeGroundPointer) + FOrder.g * SizeOf(Int8u))^;
    Span.Rgba8.b := PInt8u(PtrComp(ForeGroundPointer) + FOrder.b * SizeOf(Int8u))^;
    Span.Rgba8.A := PInt8u(PtrComp(ForeGroundPointer) + FOrder.A * SizeOf(Int8u))^;

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterRgbaBilinear }

constructor TAggSpanPatternFilterRgbaBilinear.Create
  (Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
  Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, Interpolator, nil, Wx, Wy, Order);
end;

function TAggSpanPatternFilterRgbaBilinear.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Cardinal;
  HiRes, LoRes: TPointInteger;
  Weight: Integer;
  x1, x2, y1, y2: Cardinal;
  ForeGroundPointer, Ptr1, Ptr2: PInt8u;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Dec(HiRes.X, FilterDeltaXInteger);
    Dec(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    x1 := FWrapModeX.FuncOperator(LoRes.X);
    x2 := FWrapModeX.IncOperator;

    x1 := x1 shl 2;
    x2 := x2 shl 2;

    y1 := FWrapModeY.FuncOperator(LoRes.Y);
    y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(y1);
    Ptr2 := SourceImage.Row(y2);

    Fg[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    HiRes.X := HiRes.X and CAggImageSubpixelMask;
    HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x1 * SizeOf(Int8u));
    Weight := (CAggImageSubpixelSize - HiRes.X) * (CAggImageSubpixelSize - HiRes.Y);

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x2 * SizeOf(Int8u));
    Weight := HiRes.X * (CAggImageSubpixelSize - HiRes.Y);

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x1 * SizeOf(Int8u));
    Weight := (CAggImageSubpixelSize - HiRes.X) * HiRes.Y;

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x2 * SizeOf(Int8u));
    Weight := HiRes.X * HiRes.Y;

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    Span.Rgba8.R := Int8u(Fg[FOrder.R] shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.g := Int8u(Fg[FOrder.g] shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.b := Int8u(Fg[FOrder.b] shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.A := Int8u(Fg[FOrder.A] shr (CAggImageSubpixelShift * 2));

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterRgba2x2 }

function TAggSpanPatternFilterRgba2x2.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Cardinal;

  HiRes, LoRes: TPointInteger;
  Weight: Integer;

  x1, x2, y1, y2: Cardinal;

  ForeGroundPointer, Ptr1, Ptr2: PInt8u;

  Span: PAggColor;

  WeightArray: PInt16;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ShrInt32(Filter.Diameter div 2 - 1, CAggImageSubpixelShift));

  repeat
    Interpolator.Coordinates(@HiRes.X, @HiRes.Y);

    Dec(HiRes.X, FilterDeltaXInteger);
    Dec(HiRes.Y, FilterDeltaYInteger);

    LoRes.X := ShrInt32(HiRes.X, CAggImageSubpixelShift);
    LoRes.Y := ShrInt32(HiRes.Y, CAggImageSubpixelShift);

    x1 := FWrapModeX.FuncOperator(LoRes.X);
    x2 := FWrapModeX.IncOperator;

    x1 := x1 shl 2;
    x2 := x2 shl 2;

    y1 := FWrapModeY.FuncOperator(LoRes.Y);
    y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(y1);
    Ptr2 := SourceImage.Row(y2);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    HiRes.X := HiRes.X and CAggImageSubpixelMask;
    HiRes.Y := HiRes.Y and CAggImageSubpixelMask;

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x1 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
      (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
      SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x2 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16))^
      * PInt16(PtrComp(WeightArray) + (HiRes.Y + CAggImageSubpixelSize) *
      SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x1 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
      (HiRes.X + CAggImageSubpixelSize) * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
      CAggImageFilterSize div 2, CAggImageFilterShift);

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x2 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.X * SizeOf(Int16))^
      * PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^ +
      CAggImageFilterSize div 2, CAggImageFilterShift);

    Inc(Fg[0], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[1], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[2], Weight * ForeGroundPointer^);
    Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    Inc(Fg[3], Weight * ForeGroundPointer^);

    Fg[0] := Fg[0] shr CAggImageFilterShift;
    Fg[1] := Fg[1] shr CAggImageFilterShift;
    Fg[2] := Fg[2] shr CAggImageFilterShift;
    Fg[3] := Fg[3] shr CAggImageFilterShift;

    if Fg[FOrder.A] > CAggBaseMask then
        Fg[FOrder.A] := CAggBaseMask;

    if Fg[FOrder.R] > Fg[FOrder.A] then
        Fg[FOrder.R] := Fg[FOrder.A];

    if Fg[FOrder.g] > Fg[FOrder.A] then
        Fg[FOrder.g] := Fg[FOrder.A];

    if Fg[FOrder.b] > Fg[FOrder.A] then
        Fg[FOrder.b] := Fg[FOrder.A];

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.A := Int8u(Fg[FOrder.A]);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterRgba }

function TAggSpanPatternFilterRgba.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Integer;
  Diameter, CountY: Cardinal;
  HiRes, LoRes: TPointInteger;
  Start, CountX, WeightY, FractX, IntX, Weight: Integer;
  RowPointer, ForeGroundPointer: PInt8u;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  WeightArray := Filter.WeightArray;

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@X, @Y);

    Dec(X, FilterDeltaXInteger);
    Dec(Y, FilterDeltaYInteger);

    HiRes.X := X;
    HiRes.X := Y;

    FractX := HiRes.X and CAggImageSubpixelMask;
    CountY := Diameter;

    LoRes.Y := FWrapModeY.FuncOperator
      (ShrInt32(Y, CAggImageSubpixelShift) + Start);
    IntX := ShrInt32(X, CAggImageSubpixelShift) + Start;
    HiRes.Y := CAggImageSubpixelMask - (HiRes.Y and CAggImageSubpixelMask);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    repeat
      CountX := Diameter;
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      HiRes.X := CAggImageSubpixelMask - FractX;
      LoRes.X := FWrapModeX.FuncOperator(IntX);

      RowPointer := SourceImage.Row(LoRes.Y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) + (LoRes.X shl 2) * SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        Inc(Fg[0], ForeGroundPointer^ * Weight);
        Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Inc(Fg[1], ForeGroundPointer^ * Weight);
        Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Inc(Fg[2], ForeGroundPointer^ * Weight);
        Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Inc(Fg[3], ForeGroundPointer^ * Weight);
        Inc(HiRes.X, CAggImageSubpixelSize);

        LoRes.X := FWrapModeX.IncOperator;

        Dec(CountX);

      until CountX = 0;

      Inc(HiRes.Y, CAggImageSubpixelSize);

      LoRes.Y := FWrapModeY.IncOperator;

      Dec(CountY);

    until CountY = 0;

    Fg[0] := ShrInt32(Fg[0], CAggImageFilterShift);
    Fg[1] := ShrInt32(Fg[1], CAggImageFilterShift);
    Fg[2] := ShrInt32(Fg[2], CAggImageFilterShift);
    Fg[3] := ShrInt32(Fg[3], CAggImageFilterShift);

    if Fg[0] < 0 then
        Fg[0] := 0;

    if Fg[1] < 0 then
        Fg[1] := 0;

    if Fg[2] < 0 then
        Fg[2] := 0;

    if Fg[3] < 0 then
        Fg[3] := 0;

    if Fg[FOrder.A] > CAggBaseMask then
        Fg[FOrder.A] := CAggBaseMask;

    if Fg[FOrder.R] > Fg[FOrder.A] then
        Fg[FOrder.R] := Fg[FOrder.A];

    if Fg[FOrder.g] > Fg[FOrder.A] then
        Fg[FOrder.g] := Fg[FOrder.A];

    if Fg[FOrder.b] > Fg[FOrder.A] then
        Fg[FOrder.b] := Fg[FOrder.A];

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.A := Int8u(Fg[FOrder.A]);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
