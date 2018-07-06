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
unit AggSpanImageResampleRgb;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggColor32,
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
  TAggSpanImageResampleRgbAffine = class(TAggSpanImageResampleAffine)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageResampleRgb = class(TAggSpanImageResample)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageResampleRgbAffine }

constructor TAggSpanImageResampleRgbAffine.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageResampleRgbAffine.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageResampleRgbAffine.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Integer;

  BackR, BackG, BackB, BackA: Int8u;

  Span: PAggColor;

  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, TotalWeight, WeightY, Weight: Integer;
  InitialLoResX, InitialHiResX: Integer;

  ForeGroundPointer: PInt8u;

  WeightArray: PInt16;
begin
  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackR := GetBackgroundColor.Rgba8.R;
  BackG := GetBackgroundColor.Rgba8.g;
  BackB := GetBackgroundColor.Rgba8.b;
  BackA := GetBackgroundColor.Rgba8.A;

  Span := Allocator.Span;

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  radius.X := ShrInt32(Diameter * FRadiusX, 1);
  radius.Y := ShrInt32(Diameter * FRadiusY, 1);

  Max.X := SourceImage.width - 1;
  Max.Y := SourceImage.height - 1;

  WeightArray := Filter.WeightArray;

  repeat
    Interpolator.Coordinates(@X, @Y);

    Inc(X, FilterDeltaXInteger - radius.X);
    Inc(Y, FilterDeltaYInteger - radius.Y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    LoRes.Y := ShrInt32(Y, CAggImageSubpixelShift);
    HiRes.Y := ShrInt32((CAggImageSubpixelMask -
      (Y and CAggImageSubpixelMask)) * FRadiusYInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(X, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (X and CAggImageSubpixelMask)) * FRadiusXInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      LoRes.X := InitialLoResX;
      HiRes.X := InitialHiResX;

      if (LoRes.Y >= 0) and (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            LoRes.X * 3 * SizeOf(Int8u));

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

            if (LoRes.X >= 0) and (LoRes.X <= Max.X) then
              begin
                Inc(Fg[0], ForeGroundPointer^ * Weight);
                Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                Inc(Fg[1], ForeGroundPointer^ * Weight);
                Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                Inc(Fg[2], ForeGroundPointer^ * Weight);
                Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                Inc(Fg[3], CAggBaseMask * Weight);

              end
            else
              begin
                Inc(Fg[FOrder.R], BackR * Weight);
                Inc(Fg[FOrder.g], BackG * Weight);
                Inc(Fg[FOrder.b], BackB * Weight);
                Inc(Fg[3], BackA * Weight);

                Inc(PtrComp(ForeGroundPointer), 3 * SizeOf(Int8u));
              end;

            Inc(TotalWeight, Weight);
            Inc(HiRes.X, FRadiusXInv);
            Inc(LoRes.X);

          until HiRes.X >= FilterSize;

        end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          Inc(TotalWeight, Weight);
          Inc(Fg[FOrder.R], BackR * Weight);
          Inc(Fg[FOrder.g], BackG * Weight);
          Inc(Fg[FOrder.b], BackB * Weight);
          Inc(Fg[3], BackA * Weight);
          Inc(HiRes.X, FRadiusXInv);

        until HiRes.X >= FilterSize;

      Inc(HiRes.Y, FRadiusYInv);
      Inc(LoRes.Y);

    until HiRes.Y >= FilterSize;

    Fg[0] := Fg[0] div TotalWeight;
    Fg[1] := Fg[1] div TotalWeight;
    Fg[2] := Fg[2] div TotalWeight;
    Fg[3] := Fg[3] div TotalWeight;

    if Fg[0] < 0 then
        Fg[0] := 0;

    if Fg[1] < 0 then
        Fg[1] := 0;

    if Fg[2] < 0 then
        Fg[2] := 0;

    if Fg[3] < 0 then
        Fg[3] := 0;

    if Fg[3] > CAggBaseMask then
        Fg[3] := CAggBaseMask;

    if Fg[0] > Fg[3] then
        Fg[0] := Fg[3];

    if Fg[1] > Fg[3] then
        Fg[1] := Fg[3];

    if Fg[2] > Fg[3] then
        Fg[2] := Fg[3];

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.A := Int8u(Fg[3]);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageResampleRgb }

constructor TAggSpanImageResampleRgb.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageResampleRgb.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageResampleRgb.Generate(X, Y: Integer;
  Len: Cardinal): PAggColor;
var
  Span: PAggColor;

  Fg: array [0 .. 3] of Integer;

  BackR, BackG, BackB, BackA: Int8u;

  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, RX, RY, RxInv, RyInv, TotalWeight,
    InitialLoResX, InitialHiResX, WeightY, Weight: Integer;

  WeightArray: PInt16;

  ForeGroundPointer: PInt8u;
begin
  Span := Allocator.Span;

  Interpolator.SetBegin(X + FilterDeltaXDouble, Y + FilterDeltaYDouble, Len);

  BackR := GetBackgroundColor.Rgba8.R;
  BackG := GetBackgroundColor.Rgba8.g;
  BackB := GetBackgroundColor.Rgba8.b;
  BackA := GetBackgroundColor.Rgba8.A;

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  WeightArray := Filter.WeightArray;

  repeat
    RxInv := CAggImageSubpixelSize;
    RyInv := CAggImageSubpixelSize;

    Interpolator.Coordinates(@X, @Y);
    Interpolator.LocalScale(@RX, @RY);

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

    Max.X := SourceImage.width - 1;
    Max.Y := SourceImage.height - 1;

    Inc(X, FilterDeltaXInteger - radius.X);
    Inc(Y, FilterDeltaYInteger - radius.Y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    LoRes.Y := ShrInt32(Y, CAggImageSubpixelShift);
    HiRes.Y := ShrInt32((CAggImageSubpixelMask -
      (Y and CAggImageSubpixelMask)) * RyInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(X, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (X and CAggImageSubpixelMask)) * RxInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.Y * SizeOf(Int16))^;

      LoRes.X := InitialLoResX;
      HiRes.X := InitialHiResX;

      if (LoRes.Y >= 0) and (LoRes.Y <= Max.Y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.Y)) +
            LoRes.X * 3 * SizeOf(Int8));

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

            if (LoRes.X >= 0) and (LoRes.X <= Max.X) then
              begin
                Inc(Fg[0], ForeGroundPointer^ * Weight);
                Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                Inc(Fg[1], ForeGroundPointer^ * Weight);
                Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                Inc(Fg[2], ForeGroundPointer^ * Weight);
                Inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                Inc(Fg[3], CAggBaseMask * Weight);

              end
            else
              begin
                Inc(Fg[FOrder.R], BackR * Weight);
                Inc(Fg[FOrder.g], BackG * Weight);
                Inc(Fg[FOrder.b], BackB * Weight);
                Inc(Fg[3], BackA * Weight);

                Inc(PtrComp(ForeGroundPointer), 3 * SizeOf(Int8u));
              end;

            Inc(TotalWeight, Weight);
            Inc(HiRes.X, RxInv);
            Inc(LoRes.X);

          until HiRes.X >= FilterSize;

        end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.X
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          Inc(TotalWeight, Weight);
          Inc(Fg[FOrder.R], BackR * Weight);
          Inc(Fg[FOrder.g], BackG * Weight);
          Inc(Fg[FOrder.b], BackB * Weight);
          Inc(Fg[3], BackA * Weight);
          Inc(HiRes.X, RxInv);

        until HiRes.X >= FilterSize;

      Inc(HiRes.Y, RyInv);
      Inc(LoRes.Y);

    until HiRes.Y >= FilterSize;

    Fg[0] := Fg[0] div TotalWeight;
    Fg[1] := Fg[1] div TotalWeight;
    Fg[2] := Fg[2] div TotalWeight;
    Fg[3] := Fg[3] div TotalWeight;

    if Fg[0] < 0 then
        Fg[0] := 0;

    if Fg[1] < 0 then
        Fg[1] := 0;

    if Fg[2] < 0 then
        Fg[2] := 0;

    if Fg[3] < 0 then
        Fg[3] := 0;

    if Fg[3] > CAggBaseMask then
        Fg[3] := CAggBaseMask;

    if Fg[0] > Fg[3] then
        Fg[0] := Fg[3];

    if Fg[1] > Fg[3] then
        Fg[1] := Fg[3];

    if Fg[2] > Fg[3] then
        Fg[2] := Fg[3];

    Span.Rgba8.R := Int8u(Fg[FOrder.R]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.A := Int8u(Fg[3]);

    Inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
