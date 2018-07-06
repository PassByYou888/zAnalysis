//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgbGamma(Gamma: TAggGamma; p: PInt8u;
  CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
var
  R, g, b: Cardinal;
begin
  R := Gamma.dir[TAggOrderRGB(Pointer(p)^).R];
  g := Gamma.dir[TAggOrderRGB(Pointer(p)^).g];
  b := Gamma.dir[TAggOrderRGB(Pointer(p)^).b];

  TAggOrderRGB(Pointer(p)^).R :=
    Int8u(Integer(Gamma.Inv[(((Integer(Gamma.dir[CR]) - R) * alpha)
    shr CAggBaseShift) + R]));
  TAggOrderRGB(Pointer(p)^).g :=
    Int8u(Integer(Gamma.Inv[(((Integer(Gamma.dir[Cg]) - g) * alpha)
    shr CAggBaseShift) + g]));
  TAggOrderRGB(Pointer(p)^).b :=
    Int8u(Integer(Gamma.Inv[(((Integer(Gamma.dir[CB]) - b) * alpha)
    shr CAggBaseShift) + b]));
end;

procedure CopyOrBlendPixRgbGamma(Gamma: TAggGamma; p: PInt8u;
  C: PAggColor; Cover: Cardinal); overload;
var
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
      TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
      TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;

    end
    else
      BlendPixRgbGamma(Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixRgbGamma(Gamma: TAggGamma; p: PInt8u;
  C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
      TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
      TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;
    end
    else
      BlendPixRgbGamma(Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A);
end;

procedure Rgb24GammaBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixRgbGamma(This.Gamma,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X), C, Cover);
end;

procedure Rgb24GammaBlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgbGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24GammaBlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgbGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha,
          Cover);

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24GammaBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;
      end
      else
        BlendPixRgbGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha,
          Covers^);

      Inc(PtrComp(p), 3);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24GammaBlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;
      end
      else
        BlendPixRgbGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha,
          Covers^);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24GammaBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Rgb24GammaBlendColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0;
end;

procedure Rgb24GammaBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PInt8u;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PSrc := SourcePtr;
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + 3 * Xdst *
    SizeOf(Int8u));

  if Cover = 255 then
    repeat
      alpha := PInt8u(PtrComp(PSrc) + From.Order.A)^;

      if alpha <> 0 then
        if alpha = CAggBaseMask then
        begin
          TAggOrderRGB(Pointer(PDst)^).R :=
            PInt8u(PtrComp(PSrc) + From.Order.R)^;
          TAggOrderRGB(Pointer(PDst)^).g :=
            PInt8u(PtrComp(PSrc) + From.Order.g)^;
          TAggOrderRGB(Pointer(PDst)^).b :=
            PInt8u(PtrComp(PSrc) + From.Order.b)^;

        end
        else
          BlendPixRgbGamma(This.Gamma, PDst,
            PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^, alpha);

      Inc(PtrComp(PSrc), 4);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      COLOR.FromRgbaInteger(PInt8u(PtrComp(PSrc) + From.Order.R)^,
        PInt8u(PtrComp(PSrc) + From.Order.g)^,
        PInt8u(PtrComp(PSrc) + From.Order.b)^,
        PInt8u(PtrComp(PSrc) + From.Order.A)^);

      CopyOrBlendPixRgbGamma(This.Gamma, PDst, @COLOR, Cover);

      Inc(PtrComp(PSrc), 4);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Rgb24GammaBlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;
  PSrc, PDst: PInt8u;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      3 * Xdst * SizeOf(Int8u));

    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, PDst, COLOR,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24GammaBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;
  PSrc, PDst: PInt8u;
  COLOR: PAggColor;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      3 * Xdst * SizeOf(Int8u));

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        BlendPixRgbGamma(This.Gamma, PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
          COLOR.Rgba8.b, COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixRgbGamma(This.Gamma, PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
