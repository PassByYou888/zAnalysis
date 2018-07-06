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
unit AggBlur;

interface

{$INCLUDE AggCompiler.inc}



uses
  AggBasics,
  AggArray,
  AggColor32,
  AggPixelFormat,
  AggPixelFormatTransposer;

type
  TAggStackBlur = class
  private
    FBuffer, FStack: TAggPodVector;
    procedure BlurX(img: TAggPixelFormatProcessor; radius: Cardinal);
    procedure BlurY(img: TAggPixelFormatProcessor; radius: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Blur(img: TAggPixelFormatProcessor; radius: Cardinal);
  end;

  TAggRecursiveBlur = class
  private
    FSum1, FSum2, FBuffer: TAggPodVector;
    procedure BlurX(img: TAggPixelFormatProcessor; radius: Double);
    procedure BlurY(img: TAggPixelFormatProcessor; radius: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Blur(img: TAggPixelFormatProcessor; radius: Double);
  end;

procedure StackBlurGray8(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
procedure StackBlurRgb24(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
procedure StackBlurRgba32(img: TAggPixelFormatProcessor; RX, RY: Cardinal);

implementation

const
  GStackBlur8Mul: array [0..254] of Int16u = (512, 512, 456, 512, 328, 456,
    335, 512, 405, 328, 271, 456, 388, 335, 292, 512, 454, 405, 364, 328, 298,
    271, 496, 456, 420, 388, 360, 335, 312, 292, 273, 512, 482, 454, 428, 405,
    383, 364, 345, 328, 312, 298, 284, 271, 259, 496, 475, 456, 437, 420, 404,
    388, 374, 360, 347, 335, 323, 312, 302, 292, 282, 273, 265, 512, 497, 482,
    468, 454, 441, 428, 417, 405, 394, 383, 373, 364, 354, 345, 337, 328, 320,
    312, 305, 298, 291, 284, 278, 271, 265, 259, 507, 496, 485, 475, 465, 456,
    446, 437, 428, 420, 412, 404, 396, 388, 381, 374, 367, 360, 354, 347, 341,
    335, 329, 323, 318, 312, 307, 302, 297, 292, 287, 282, 278, 273, 269, 265,
    261, 512, 505, 497, 489, 482, 475, 468, 461, 454, 447, 441, 435, 428, 422,
    417, 411, 405, 399, 394, 389, 383, 378, 373, 368, 364, 359, 354, 350, 345,
    341, 337, 332, 328, 324, 320, 316, 312, 309, 305, 301, 298, 294, 291, 287,
    284, 281, 278, 274, 271, 268, 265, 262, 259, 257, 507, 501, 496, 491, 485,
    480, 475, 470, 465, 460, 456, 451, 446, 442, 437, 433, 428, 424, 420, 416,
    412, 408, 404, 400, 396, 392, 388, 385, 381, 377, 374, 370, 367, 363, 360,
    357, 354, 350, 347, 344, 341, 338, 335, 332, 329, 326, 323, 320, 318, 315,
    312, 310, 307, 304, 302, 299, 297, 294, 292, 289, 287, 285, 282, 280, 278,
    275, 273, 271, 269, 267, 265, 263, 261, 259);

  GStackBlur8Shr: array [0..254] of Int8u = (9, 11, 12, 13, 13, 14, 14, 15,
    15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18,
    18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
    21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24);

type
  PStackCalculator = ^TStackCalculator;
  TStackCalculator = packed record
    v, R, g, b, A: Cardinal;
  public
    procedure Clear;

    procedure Add(C: TAggColor); overload;
    procedure Add(C: TAggColor; k: Cardinal); overload;
    procedure Add(const C: TStackCalculator); overload;
    procedure Sub(C: TAggColor); overload;
    procedure Sub(const C: TStackCalculator); overload;

    procedure CalculatePixel(C: PAggColor; ADiv: Cardinal); overload;
    procedure CalculatePixel(C: PAggColor; AMul, AShr: Cardinal); overload;
  end;

  PGaussCalculator = ^TGaussCalculator;
  TGaussCalculator = packed record
    v, R, g, b, A: Double;
  public
    procedure FromPixel(C: TAggColor);

    procedure Calculate(b1, b2, b3, b4: Double;
      const c1, c2, c3, c4: TGaussCalculator);

    procedure ToPixel(C: PAggColor);
  end;


{ TStackCalculator }

procedure TStackCalculator.Clear;
begin
  v := 0;
  R := 0;
  g := 0;
  b := 0;
  A := 0;
end;

procedure TStackCalculator.Add(C: TAggColor);
begin
  Inc(v, C.v);
  Inc(R, C.Rgba8.R);
  Inc(g, C.Rgba8.g);
  Inc(b, C.Rgba8.b);
  Inc(A, C.Rgba8.A);
end;

procedure TStackCalculator.Add(const C: TStackCalculator);
begin
  Inc(v, C.v);
  Inc(R, C.R);
  Inc(g, C.g);
  Inc(b, C.b);
  Inc(A, C.A);
end;

procedure TStackCalculator.Add(C: TAggColor; k: Cardinal);
begin
  Inc(v, C.v * k);
  Inc(R, C.Rgba8.R * k);
  Inc(g, C.Rgba8.g * k);
  Inc(b, C.Rgba8.b * k);
  Inc(A, C.Rgba8.A * k);
end;

procedure TStackCalculator.Sub(C: TAggColor);
begin
  Dec(v, C.v);
  Dec(R, C.Rgba8.R);
  Dec(g, C.Rgba8.g);
  Dec(b, C.Rgba8.b);
  Dec(A, C.Rgba8.A);
end;

procedure TStackCalculator.Sub(const C: TStackCalculator);
begin
  Dec(v, C.v);
  Dec(R, C.R);
  Dec(g, C.g);
  Dec(b, C.b);
  Dec(A, C.A);
end;

procedure TStackCalculator.CalculatePixel(C: PAggColor; ADiv: Cardinal);
begin
  C.v := Int8u(v div ADiv);
  C.Rgba8.R := Int8u(R div ADiv);
  C.Rgba8.g := Int8u(g div ADiv);
  C.Rgba8.b := Int8u(b div ADiv);
  C.Rgba8.A := Int8u(A div ADiv);
end;

procedure TStackCalculator.CalculatePixel(C: PAggColor; AMul, AShr: Cardinal);
begin
  C.v := Int8u((v * AMul) shr AShr);
  C.Rgba8.R := Int8u((R * AMul) shr AShr);
  C.Rgba8.g := Int8u((g * AMul) shr AShr);
  C.Rgba8.b := Int8u((b * AMul) shr AShr);
  C.Rgba8.A := Int8u((A * AMul) shr AShr);
end;


{ TGaussCalculator }

procedure TGaussCalculator.FromPixel(C: TAggColor);
begin
  v := C.v;
  R := C.Rgba8.R;
  g := C.Rgba8.g;
  b := C.Rgba8.b;
  A := C.Rgba8.A;
end;

procedure TGaussCalculator.Calculate(b1, b2, b3, b4: Double;
  const c1, c2, c3, c4: TGaussCalculator);
begin
  v := b1 * c1.v + b2 * c2.v + b3 * c3.v + b4 * c4.v;
  R := b1 * c1.R + b2 * c2.R + b3 * c3.R + b4 * c4.R;
  g := b1 * c1.g + b2 * c2.g + b3 * c3.g + b4 * c4.g;
  b := b1 * c1.b + b2 * c2.b + b3 * c3.b + b4 * c4.b;
  A := b1 * c1.A + b2 * c2.A + b3 * c3.A + b4 * c4.A;
end;

procedure TGaussCalculator.ToPixel(C: PAggColor);
begin
  C.v := Int8u(UnsignedRound(v));
  C.Rgba8.R := Int8u(UnsignedRound(R));
  C.Rgba8.g := Int8u(UnsignedRound(g));
  C.Rgba8.b := Int8u(UnsignedRound(b));
  C.Rgba8.A := Int8u(UnsignedRound(A));
end;


{ TAggStackBlur }

constructor TAggStackBlur.Create;
begin
  FBuffer := TAggPodVector.Create(SizeOf(TAggColor));
  FStack := TAggPodVector.Create(SizeOf(TAggColor));
end;

destructor TAggStackBlur.Destroy;
begin
  FBuffer.Free;
  FStack.Free;
  inherited;
end;

procedure TAggStackBlur.BlurX(img: TAggPixelFormatProcessor; radius: Cardinal);
var
  X, Y, xp, i, StackPointer, StackStart, w, h, Wm: Cardinal;
  ADiv, DivSum, MulSum, ShrSum, MaxVal: Cardinal;
  pix: TAggColor;
  StackPix, TempColor: PAggColor;
  Sum, SumIn, SumOut: TStackCalculator;
begin
  if radius < 1 then
    Exit;

  w := img.width;
  h := img.height;
  Wm := w - 1;
  ADiv := radius * 2 + 1;

  DivSum := (radius + 1) * (radius + 1);
  MulSum := 0;
  ShrSum := 0;
  MaxVal := CAggBaseMask;

  if (MaxVal <= 255) and (radius < 255) then
  begin
    MulSum := GStackBlur8Mul[radius];
    ShrSum := GStackBlur8Shr[radius];
  end;

  FBuffer.Allocate(w, 128);
  FStack.Allocate(ADiv, 32);

  Y := 0;

  while Y < h do
  begin
    Sum.Clear;
    SumIn.Clear;
    SumOut.Clear;

    pix := img.Pixel(img, 0, Y);

    i := 0;

    while i <= radius do
    begin
      Move(pix, FStack[i]^, SizeOf(TAggColor));

      Sum.Add(pix, i + 1);
      SumOut.Add(pix);

      Inc(i);
    end;

    i := 1;

    while i <= radius do
    begin
      if i > Wm then
        pix := img.Pixel(img, Wm, Y)
      else
        pix := img.Pixel(img, i, Y);

      Move(pix, FStack[i + radius]^, SizeOf(TAggColor));

      Sum.Add(pix, radius + 1 - i);
      SumIn.Add(pix);

      Inc(i);
    end;

    StackPointer := radius;

    X := 0;

    while X < w do
    begin
      if MulSum <> 0 then
        Sum.CalculatePixel(PAggColor(FBuffer[X]), MulSum, ShrSum)
      else
        Sum.CalculatePixel(PAggColor(FBuffer[X]), DivSum);

      Sum.Sub(SumOut);

      StackStart := StackPointer + ADiv - radius;

      if StackStart >= ADiv then
        Dec(StackStart, ADiv);

      StackPix := FStack[StackStart];

      SumOut.Sub(StackPix^);

      xp := X + radius + 1;

      if xp > Wm then
        xp := Wm;

      pix := img.Pixel(img, xp, Y);

      StackPix^ := pix;

      SumIn.Add(pix);
      Sum.Add(SumIn);

      Inc(StackPointer);

      if StackPointer >= ADiv then
        StackPointer := 0;

      StackPix := FStack[StackPointer];

      SumOut.Add(StackPix^);
      SumIn.Sub(StackPix^);

      Inc(X);
    end;

    TempColor := FBuffer[0];

    img.CopyColorHSpan(img, 0, Y, w, TempColor);

    Inc(Y);
  end;
end;

procedure TAggStackBlur.BlurY(img: TAggPixelFormatProcessor; radius: Cardinal);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;

procedure TAggStackBlur.Blur(img: TAggPixelFormatProcessor; radius: Cardinal);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  BlurX(img, radius);
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;


{ TAggRecursiveBlur }

constructor TAggRecursiveBlur.Create;
begin
  FSum1 := TAggPodVector.Create(SizeOf(TGaussCalculator));
  FSum2 := TAggPodVector.Create(SizeOf(TGaussCalculator));
  FBuffer := TAggPodVector.Create(SizeOf(TAggColor));
end;

destructor TAggRecursiveBlur.Destroy;
begin
  FSum1.Free;
  FSum2.Free;
  FBuffer.Free;
  inherited;
end;

procedure TAggRecursiveBlur.BlurX(img: TAggPixelFormatProcessor; radius: Double);
var
  s, q, q2, Q3, b0, b1, b2, b3, b: Double;
  w, h, Wm, X, Y: Integer;
  C: TGaussCalculator;
  G0, G1: PGaussCalculator;
begin
  if radius < 0.62 then
    Exit;

  if img.width < 3 then
    Exit;

  s := radius * 0.5;

  if s < 2.5 then
    q := 3.97156 - 4.14554 * Sqrt(1 - 0.26891 * s)
  else
    q := 0.98711 * s - 0.96330;

  q2 := q * q;
  Q3 := q2 * q;
  b0 := 1.0 / (1.578250 + 2.444130 * q + 1.428100 * q2 + 0.422205 * Q3);
  b1 := 2.44413 * q + 2.85619 * q2 + 1.26661 * Q3;
  b2 := -1.42810 * q2 + -1.26661 * Q3;
  b3 := 0.422205 * Q3;
  b := 1 - (b1 + b2 + b3) * b0;
  b1 := b1 * b0;
  b2 := b2 * b0;
  b3 := b3 * b0;
  w := img.width;
  h := img.height;
  Wm := w - 1;

  FSum1.Allocate(w);
  FSum2.Allocate(w);
  FBuffer.Allocate(w);

  Y := 0;

  while Y < h do
  begin
    G0 := PGaussCalculator(FSum1[0]);

    C.FromPixel(img.Pixel(img, 0, Y));
    G0.Calculate(b, b1, b2, b3, C, C, C, C);

    G1 := PGaussCalculator(FSum1[1]);

    C.FromPixel(img.Pixel(img, 1, Y));
    G1.Calculate(b, b1, b2, b3, C, G0^, G0^, G0^);

    C.FromPixel(img.Pixel(img, 2, Y));
    PGaussCalculator(FSum1[2]).Calculate(b, b1, b2, b3, C,
      G1^, G0^, G0^);

    X := 3;

    while X < w do
    begin
      C.FromPixel(img.Pixel(img, X, Y));

      PGaussCalculator(FSum1[X]).Calculate(b, b1, b2, b3, C,
        PGaussCalculator(FSum1[X - 1])^,
        PGaussCalculator(FSum1[X - 2])^,
        PGaussCalculator(FSum1[X - 3])^);

      Inc(X);
    end;

    G0 := PGaussCalculator(FSum1[Wm]);
    G1 := PGaussCalculator(FSum2[Wm]);

    G1.Calculate(b, b1, b2, b3, G0^, G0^, G0^, G0^);

    PGaussCalculator(FSum2[Wm - 1]).Calculate(b, b1, b2, b3,
      PGaussCalculator(FSum1[Wm - 1])^, G1^, G1^, G1^);

    PGaussCalculator(FSum2[Wm - 2]).Calculate(b, b1, b2, b3,
      PGaussCalculator(FSum1[Wm - 2])^, PGaussCalculator(FSum2[Wm - 1])^,
      G1^, G1^);

    G1.ToPixel(PAggColor(FBuffer[Wm]));

    PGaussCalculator(FSum2[Wm - 1])
      .ToPixel(PAggColor(FBuffer[Wm - 1]));

    PGaussCalculator(FSum2[Wm - 2])
      .ToPixel(PAggColor(FBuffer[Wm - 2]));

    X := Wm - 3;

    while X >= 0 do
    begin
      PGaussCalculator(FSum2[X])
        .Calculate(b, b1, b2, b3, PGaussCalculator(FSum1[X])^,
        PGaussCalculator(FSum2[X + 1])^,
        PGaussCalculator(FSum2[X + 2])^,
        PGaussCalculator(FSum2[X + 3])^);

      PGaussCalculator(FSum2[X]).ToPixel(PAggColor(FBuffer[X]));

      Dec(X);
    end;

    img.CopyColorHSpan(img, 0, Y, w, FBuffer[0]);

    Inc(Y);
  end;
end;

procedure TAggRecursiveBlur.BlurY(img: TAggPixelFormatProcessor; radius: Double);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;

procedure TAggRecursiveBlur.Blur(img: TAggPixelFormatProcessor; radius: Double);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  BlurX(img, radius);
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;

procedure StackBlurGray8(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
var
  stride: Integer;
  X, Y, xp, yp, i, pix, StackPixel, Sum, SumIn, SumOut: Cardinal;
  StackPointer, StackStart, w, h, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;
  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  Stack: TAggPodVector;
begin
  w := img.width;
  h := img.height;
  Wm := w - 1;
  Hm := h - 1;

  Stack := TAggPodVector.Create(SizeOf(Int8u));

  if RX > 0 then
  begin
    if RX > 254 then
      RX := 254;

    ADiv := RX * 2 + 1;

    MulSum := GStackBlur8Mul[RX];
    ShrSum := GStackBlur8Shr[RX];

    Stack.Allocate(ADiv);

    Y := 0;

    while Y < h do
    begin
      Sum := 0;
      SumIn := 0;
      SumOut := 0;

      SourcePixelPointer := img.GetPixelPointer(0, Y);
      pix := SourcePixelPointer^;

      i := 0;

      while i <= RX do
      begin
        PInt8u(Stack[i])^ := pix;

        Inc(Sum, pix * (i + 1));
        Inc(SumOut, pix);

        Inc(i);
      end;

      i := 1;

      while i <= RX do
      begin
        if i <= Wm then
          Inc(PtrComp(SourcePixelPointer), img.Step);

        pix := SourcePixelPointer^;

        PInt8u(Stack[i + RX])^ := pix;

        Inc(Sum, pix * (RX + 1 - i));
        Inc(SumIn, pix);

        Inc(i);
      end;

      StackPointer := RX;
      xp := RX;

      if xp > Wm then
        xp := Wm;

      SourcePixelPointer := img.GetPixelPointer(xp, Y);
      DestinationPixelPointer := img.GetPixelPointer(0, Y);

      X := 0;

      while X < w do
      begin
        DestinationPixelPointer^ := Int8u((Sum * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), img.Step);
        Dec(Sum, SumOut);

        StackStart := StackPointer + ADiv - RX;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        Dec(SumOut, PInt8u(Stack[StackStart])^);

        if xp < Wm then
        begin
          Inc(PtrComp(SourcePixelPointer), img.Step);

          pix := SourcePixelPointer^;

          Inc(xp);
        end;

        PInt8u(Stack[StackStart])^ := pix;

        Inc(SumIn, pix);
        Inc(Sum, SumIn);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixel := PInt8u(Stack[StackPointer])^;

        Inc(SumOut, StackPixel);
        Dec(SumIn, StackPixel);

        Inc(X);
      end;

      Inc(Y);
    end;
  end;

  if RY > 0 then
  begin
    if RY > 254 then
      RY := 254;

    ADiv := RY * 2 + 1;

    MulSum := GStackBlur8Mul[RY];
    ShrSum := GStackBlur8Shr[RY];

    Stack.Allocate(ADiv);

    stride := img.stride;

    X := 0;

    while X < w do
    begin
      Sum := 0;
      SumIn := 0;
      SumOut := 0;

      SourcePixelPointer := img.GetPixelPointer(X, 0);
      pix := SourcePixelPointer^;

      i := 0;

      while i <= RY do
      begin
        PInt8u(Stack[i])^ := pix;

        Inc(Sum, pix * (i + 1));
        Inc(SumOut, pix);

        Inc(i);
      end;

      i := 1;

      while i <= RY do
      begin
        if i <= Hm then
          Inc(PtrComp(SourcePixelPointer), stride);

        pix := SourcePixelPointer^;

        PInt8u(Stack[i + RY])^ := pix;

        Inc(Sum, pix * (RY + 1 - i));
        Inc(SumIn, pix);

        Inc(i);
      end;

      StackPointer := RY;
      yp := RY;

      if yp > Hm then
        yp := Hm;

      SourcePixelPointer := img.GetPixelPointer(X, yp);
      DestinationPixelPointer := img.GetPixelPointer(X, 0);

      Y := 0;

      while Y < h do
      begin
        DestinationPixelPointer^ := Int8u((Sum * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), stride);
        Dec(Sum, SumOut);

        StackStart := StackPointer + ADiv - RY;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        Dec(SumOut, PInt8u(Stack[StackStart])^);

        if yp < Hm then
        begin
          Inc(PtrComp(SourcePixelPointer), stride);

          pix := SourcePixelPointer^;

          Inc(yp);
        end;

        PInt8u(Stack[StackStart])^ := pix;

        Inc(SumIn, pix);
        Inc(Sum, SumIn);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixel := PInt8u(Stack[StackPointer])^;

        Inc(SumOut, StackPixel);
        Dec(SumIn, StackPixel);

        Inc(Y);
      end;

      Inc(X);
    end;
  end;

  Stack.Free;
end;

procedure StackBlurRgb24(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
var
  R, g, b, stride: Integer;
  X, Y, xp, yp, i, StackPointer, StackStart: Cardinal;
  SumRed, SumGreen, SumBlue: Cardinal;
  SumInRed, SumInGreen, SumInBlue: Cardinal;
  SumOutRed, SumOutGreen, SumOutBlue: Cardinal;
  w, h, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;
  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  StackPixelPointer: PAggColor;
  Stack: TAggPodArray;
begin
  R := img.Order.R;
  g := img.Order.g;
  b := img.Order.b;

  w := img.width;
  h := img.height;
  Wm := w - 1;
  Hm := h - 1;

  Stack := TAggPodArray.Create(SizeOf(TAggColor));

  if RX > 0 then
  begin
    if RX > 254 then
      RX := 254;

    ADiv := RX * 2 + 1;
    MulSum := GStackBlur8Mul[RX];
    ShrSum := GStackBlur8Shr[RX];

    Stack.Allocate(ADiv);

    Y := 0;

    while Y < h do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;

      SourcePixelPointer := img.GetPixelPointer(0, Y);

      i := 0;

      while i <= RX do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (i + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));

        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        Inc(i);
      end;

      i := 1;

      while i <= RX do
      begin
        if i <= Wm then
          Inc(PtrComp(SourcePixelPointer), img.PixWidth);

        StackPixelPointer := Stack[i + RX];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (RX + 1 - i));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RX + 1 - i));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RX + 1 - i));

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        Inc(i);
      end;

      StackPointer := RX;
      xp := RX;

      if xp > Wm then
        xp := Wm;

      SourcePixelPointer := img.GetPixelPointer(xp, Y);
      DestinationPixelPointer := img.GetPixelPointer(0, Y);

      X := 0;

      while X < w do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), img.PixWidth);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);

        StackStart := StackPointer + ADiv - RX;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.b);

        if xp < Wm then
        begin
          Inc(PtrComp(SourcePixelPointer), img.PixWidth);
          Inc(xp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.g);
        Dec(SumInBlue, StackPixelPointer.Rgba8.b);

        Inc(X);
      end;

      Inc(Y);
    end;
  end;

  if RY > 0 then
  begin
    if RY > 254 then
      RY := 254;

    ADiv := RY * 2 + 1;

    MulSum := GStackBlur8Mul[RY];
    ShrSum := GStackBlur8Shr[RY];

    Stack.Allocate(ADiv);

    stride := img.stride;

    X := 0;

    while X < w do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;

      SourcePixelPointer := img.GetPixelPointer(X, 0);

      i := 0;

      while i <= RY do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (i + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));
        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        Inc(i);
      end;

      i := 1;

      while i <= RY do
      begin
        if i <= Hm then
          Inc(PtrComp(SourcePixelPointer), stride);

        StackPixelPointer := Stack[i + RY];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (RY + 1 - i));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RY + 1 - i));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RY + 1 - i));
        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        Inc(i);
      end;

      StackPointer := RY;
      yp := RY;

      if yp > Hm then
        yp := Hm;

      SourcePixelPointer := img.GetPixelPointer(X, yp);
      DestinationPixelPointer := img.GetPixelPointer(X, 0);

      Y := 0;

      while Y < h do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), stride);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);

        StackStart := StackPointer + ADiv - RY;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.b);

        if yp < Hm then
        begin
          Inc(PtrComp(SourcePixelPointer), stride);

          Inc(yp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.g);
        Dec(SumInBlue, StackPixelPointer.Rgba8.b);

        Inc(Y);
      end;

      Inc(X);
    end;
  end;

  Stack.Free;
end;

procedure StackBlurRgba32(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
var
  R, g, b, A, stride: Integer;
  X, Y, xp, yp, i, StackPointer, StackStart: Cardinal;
  SumRed, SumGreen, SumBlue, SumAlpha: Cardinal;
  SumInRed, SumInGreen, SumInBlue, SumInAlpha: Cardinal;
  SumOutRed, SumOutGreen, SumOutBlue, SumOutAlpha: Cardinal;
  w, h, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;

  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  StackPixelPointer: PAggColor;
  Stack: TAggPodArray;
begin
  R := img.Order.R;
  g := img.Order.g;
  b := img.Order.b;
  A := img.Order.A;

  w := img.width;
  h := img.height;
  Wm := w - 1;
  Hm := h - 1;

  Stack := TAggPodArray.Create(SizeOf(TAggColor));

  if RX > 0 then
  begin
    if RX > 254 then
      RX := 254;

    ADiv := RX * 2 + 1;
    MulSum := GStackBlur8Mul[RX];
    ShrSum := GStackBlur8Shr[RX];

    Stack.Allocate(ADiv);

    Y := 0;

    while Y < h do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumAlpha := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumInAlpha := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;
      SumOutAlpha := 0;

      SourcePixelPointer := img.GetPixelPointer(0, Y);

      i := 0;

      while i <= RX do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (i + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (i + 1));

        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        Inc(SumOutAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(i);
      end;

      i := 1;

      while i <= RX do
      begin
        if i <= Wm then
          Inc(PtrComp(SourcePixelPointer), img.PixWidth);

        StackPixelPointer := Stack[i + RX];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (RX + 1 - i));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RX + 1 - i));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RX + 1 - i));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (RX + 1 - i));

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(i);
      end;

      StackPointer := RX;
      xp := RX;

      if xp > Wm then
        xp := Wm;

      SourcePixelPointer := img.GetPixelPointer(xp, Y);
      DestinationPixelPointer := img.GetPixelPointer(0, Y);

      X := 0;

      while X < w do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + A)^ :=
          Int8u((SumAlpha * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), img.PixWidth);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);
        Dec(SumAlpha, SumOutAlpha);

        StackStart := StackPointer + ADiv - RX;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.b);
        Dec(SumOutAlpha, StackPixelPointer.Rgba8.A);

        if xp < Wm then
        begin
          Inc(PtrComp(SourcePixelPointer), img.PixWidth);
          Inc(xp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);
        Inc(SumAlpha, SumInAlpha);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        Inc(SumOutAlpha, StackPixelPointer.Rgba8.A);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.g);
        Dec(SumInBlue, StackPixelPointer.Rgba8.b);
        Dec(SumInAlpha, StackPixelPointer.Rgba8.A);

        Inc(X);
      end;

      Inc(Y);
    end;
  end;

  if RY > 0 then
  begin
    if RY > 254 then
      RY := 254;

    ADiv := RY * 2 + 1;

    MulSum := GStackBlur8Mul[RY];
    ShrSum := GStackBlur8Shr[RY];

    Stack.Allocate(ADiv);

    stride := img.stride;

    X := 0;

    while X < w do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumAlpha := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumInAlpha := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;
      SumOutAlpha := 0;

      SourcePixelPointer := img.GetPixelPointer(X, 0);

      i := 0;

      while i <= RY do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (i + 1));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (i + 1));
        Inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        Inc(SumOutAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(i);
      end;

      i := 1;

      while i <= RY do
      begin
        if i <= Hm then
          Inc(PtrComp(SourcePixelPointer), stride);

        StackPixelPointer := Stack[i + RY];

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + R)^ * (RY + 1 - i));
        Inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RY + 1 - i));
        Inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RY + 1 - i));
        Inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^ * (RY + 1 - i));
        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);

        Inc(i);
      end;

      StackPointer := RY;
      yp := RY;

      if yp > Hm then
        yp := Hm;

      SourcePixelPointer := img.GetPixelPointer(X, yp);
      DestinationPixelPointer := img.GetPixelPointer(X, 0);

      Y := 0;

      while Y < h do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + R)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + A)^ :=
          Int8u((SumAlpha * MulSum) shr ShrSum);

        Inc(PtrComp(DestinationPixelPointer), stride);

        Dec(SumRed, SumOutRed);
        Dec(SumGreen, SumOutGreen);
        Dec(SumBlue, SumOutBlue);
        Dec(SumAlpha, SumOutAlpha);

        StackStart := StackPointer + ADiv - RY;

        if StackStart >= ADiv then
          Dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        Dec(SumOutRed, StackPixelPointer.Rgba8.R);
        Dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        Dec(SumOutBlue, StackPixelPointer.Rgba8.b);
        Dec(SumOutAlpha, StackPixelPointer.Rgba8.A);

        if yp < Hm then
        begin
          Inc(PtrComp(SourcePixelPointer), stride);

          Inc(yp);
        end;

        StackPixelPointer.Rgba8.R := PInt8u(PtrComp(SourcePixelPointer) + R)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.A := PInt8u(PtrComp(SourcePixelPointer) + A)^;

        Inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + R)^);
        Inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        Inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        Inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + A)^);
        Inc(SumRed, SumInRed);
        Inc(SumGreen, SumInGreen);
        Inc(SumBlue, SumInBlue);
        Inc(SumAlpha, SumInAlpha);

        Inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        Inc(SumOutRed, StackPixelPointer.Rgba8.R);
        Inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        Inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        Inc(SumOutAlpha, StackPixelPointer.Rgba8.A);
        Dec(SumInRed, StackPixelPointer.Rgba8.R);
        Dec(SumInGreen, StackPixelPointer.Rgba8.g);
        Dec(SumInBlue, StackPixelPointer.Rgba8.b);
        Dec(SumInAlpha, StackPixelPointer.Rgba8.A);

        Inc(Y);
      end;

      Inc(X);
    end;
  end;

  Stack.Free;
end;

end. 
