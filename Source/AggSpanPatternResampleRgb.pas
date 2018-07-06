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
unit AggSpanPatternResampleRgb;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggColor32,
  AggSpanPattern,
  AggSpanImageResample,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer,
  AggSpanAllocator,
  AggImageFilters;

const
  CAggBaseShift      = AggColor32.CAggBaseShift;
  CAggBaseMask       = AggColor32.CAggBaseMask;
  CAggDownscaleShift = CAggImageFilterShift;

type
  TAggSpanPatternResampleRgbAffine = class(TAggSpanImageResampleAffine)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;

    FOrder: TAggOrder;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternResampleRgb = class(TAggSpanImageResample)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;

    FOrder: TAggOrder;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanPatternResampleRgbAffine }

constructor TAggSpanPatternResampleRgbAffine.Create
  (Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternResampleRgbAffine.Create
  (Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Wx, Wy: TAggWrapMode; Order: TAggOrder);
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

procedure TAggSpanPatternResampleRgbAffine.SetSourceImage(
  Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternResampleRgbAffine.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Intr: TAggSpanInterpolator;
  Fg: array [0 .. 2] of Integer;
  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, TotalWeight, WeightY, Weight: Integer;
  InitialLoResX, InitialHiResX: Integer;
  RowPointer, ForeGroundPointer: PInt8u;
  WeightArray: PInt16;
begin
  Span := Allocator.Span;
  Intr := Interpolator;

  Intr.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  radius.X := ShrInt32(Diameter * FRadiusX, 1);
  radius.Y := ShrInt32(Diameter * FRadiusY, 1);

  Max := PointInteger(SourceImage.width - 1, SourceImage.height - 1);
  WeightArray := Filter.WeightArray;

  repeat
    Intr.Coordinates(@X, @Y);

    Inc(X, FilterDeltaXInteger - radius.X);
    Inc(Y, FilterDeltaYInteger - radius.Y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];

    LoRes.Y := FWrapModeY.FuncOperator(ShrInt32(Y, CAggImageSubpixelShift));
    HiRes.Y := ShrInt32((CAggImageSubpixelMask -
      (Y and CAggImageSubpixelMask)) * FRadiusYInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(X, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (X and CAggImageSubpixelMask)) * FRadiusXInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      LoRes.X := FWrapModeX.FuncOperator(InitialLoResX);
      HiRes.X := InitialHiResX;

      RowPointer := SourceImage.Row(LoRes.Y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) + LoRes.X * 3 *
          SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

        Inc(Fg[0], ForeGroundPointer^ * Weight);
        Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Inc(Fg[1], ForeGroundPointer^ * Weight);
        Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Inc(Fg[2], ForeGroundPointer^ * Weight);
        Inc(TotalWeight, Weight);
        Inc(HiRes.X, FRadiusXInv);

        LoRes.X := FWrapModeX.IncOperator;
      until HiRes.X >= FilterSize;

      Inc(HiRes.Y, FRadiusYInv);

      LoRes.Y := FWrapModeY.IncOperator;
    until HiRes.Y >= FilterSize;

    Fg[0] := EnsureRange(Fg[0] div TotalWeight, 0, CAggBaseMask);
    Fg[1] := EnsureRange(Fg[1] div TotalWeight, 0, CAggBaseMask);
    Fg[2] := EnsureRange(Fg[2] div TotalWeight, 0, CAggBaseMask);

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.A := Int8u(CAggBaseMask);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Intr.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternResampleRgb }

constructor TAggSpanPatternResampleRgb.Create(Alloc: TAggSpanAllocator;
  Wx, Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternResampleRgb.Create(Alloc: TAggSpanAllocator;
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

procedure TAggSpanPatternResampleRgb.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternResampleRgb.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Intr: TAggSpanInterpolator;

  Fg: array [0 .. 2] of Integer;

  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, RX, RY, RxInv, RyInv, TotalWeight: Integer;
  InitialLoResX, InitialHiResX, WeightY, Weight: Integer;

  RowPointer, ForeGroundPointer: PInt8u;

  WeightArray: PInt16;
begin
  Span := Allocator.Span;
  Intr := Interpolator;

  Intr.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;
  WeightArray := Filter.WeightArray;

  repeat
    RxInv := CAggImageSubpixelSize;
    RyInv := CAggImageSubpixelSize;

    Intr.Coordinates(@X, @Y);
    Intr.LocalScale(@RX, @RY);

    RX := ShrInt32(RX * FBlur.X, CAggImageSubpixelShift);
    RY := ShrInt32(RY * FBlur.Y, CAggImageSubpixelShift);

    if RX < CAggImageSubpixelSize then
        RX := CAggImageSubpixelSize
    else
      begin
        if RX > CAggImageSubpixelSize * FScaleLimit then
            RX := CAggImageSubpixelSize * FScaleLimit;

        RxInv := CAggImageSubpixelSize * CAggImageSubpixelSize div RX;
      end;

    if RY < CAggImageSubpixelSize then
        RY := CAggImageSubpixelSize
    else
      begin
        if RY > CAggImageSubpixelSize * FScaleLimit then
            RY := CAggImageSubpixelSize * FScaleLimit;

        RyInv := CAggImageSubpixelSize * CAggImageSubpixelSize div RY;
      end;

    radius.X := ShrInt32(Diameter * RX, 1);
    radius.Y := ShrInt32(Diameter * RY, 1);

    Max := PointInteger(SourceImage.width - 1, SourceImage.height - 1);

    Inc(X, FilterDeltaXInteger - radius.X);
    Inc(Y, FilterDeltaYInteger - radius.Y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];

    LoRes.Y := FWrapModeY.FuncOperator(ShrInt32(Y, CAggImageSubpixelShift));
    HiRes.Y := ShrInt32((CAggImageSubpixelMask -
      (Y and CAggImageSubpixelMask)) * RyInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(X, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (X and CAggImageSubpixelMask)) * RxInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      LoRes.X := FWrapModeX.FuncOperator(InitialLoResX);
      HiRes.X := InitialHiResX;

      RowPointer := SourceImage.Row(LoRes.Y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) + LoRes.X * 3 *
          SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

        Inc(Fg[0], ForeGroundPointer^ * Weight);
        Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Inc(Fg[1], ForeGroundPointer^ * Weight);
        Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Inc(Fg[2], ForeGroundPointer^ * Weight);
        Inc(TotalWeight, Weight);
        Inc(HiRes.X, RxInv);

        LoRes.X := FWrapModeX.IncOperator;
      until HiRes.X >= FilterSize;

      Inc(HiRes.Y, RyInv);

      LoRes.Y := FWrapModeY.IncOperator;
    until HiRes.Y >= FilterSize;

    Fg[0] := EnsureRange(Fg[0] div TotalWeight, 0, CAggBaseMask);
    Fg[1] := EnsureRange(Fg[1] div TotalWeight, 0, CAggBaseMask);
    Fg[2] := EnsureRange(Fg[2] div TotalWeight, 0, CAggBaseMask);

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.A := Int8u(CAggBaseMask);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Intr.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
