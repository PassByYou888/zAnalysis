//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgb(p: PInt8u; CR, Cg, CB, alpha: Integer;
  Cover: Cardinal = 0);
begin
  TAggOrderRGB(Pointer(p)^).R := TAggOrderRGB(Pointer(p)^).R +
    ((CR - TAggOrderRGB(Pointer(p)^).R) * alpha) shr CAggBaseShift;
  TAggOrderRGB(Pointer(p)^).g := TAggOrderRGB(Pointer(p)^).g +
    ((Cg - TAggOrderRGB(Pointer(p)^).g) * alpha) shr CAggBaseShift;
  TAggOrderRGB(Pointer(p)^).b := TAggOrderRGB(Pointer(p)^).b +
    ((CB - TAggOrderRGB(Pointer(p)^).b) * alpha) shr CAggBaseShift;
end;

procedure CopyOrBlendPixRgb(p: PInt8u; C: PAggColor;
  Cover: Cardinal); overload;
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
      BlendPixRgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixRgb(p: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
      TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
      TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;
    end
    else
      BlendPixRgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A);
end;

procedure Rgb24CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  p: PAggOrderRgb;
begin
  p := PAggOrderRgb(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  p^.R := C.Rgba8.R;
  p^.g := C.Rgba8.g;
  p^.b := C.Rgba8.b;
end;

procedure Rgb24BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixRgb(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X),
    C, Cover);
end;

function Rgb24Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PAggOrderRgb;
begin
  p := PAggOrderRgb(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  Result.Rgba8.R := p^.R;
  Result.Rgba8.g := p^.g;
  Result.Rgba8.b := p^.b;
  Result.Rgba8.A := $FF;
end;

procedure Rgb24CopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
    TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;

    Inc(PtrComp(p), 3);
    Dec(Len);
  until Len = 0;
end;

procedure Rgb24CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(p)^).R := C.Rgba8.R;
    TAggOrderRGB(Pointer(p)^).g := C.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := C.Rgba8.b;

    p := This.RenderingBuffer.NextRow(p);

    Dec(Len);
  until Len = 0;
end;

procedure Rgb24BlendHorizontalLine(This: TAggPixelFormatProcessor;
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
        BlendPixRgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        BlendPixRgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixRgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(PtrComp(p), 3);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixRgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgb(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgb(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgb(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Rgb24BlendColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgb(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgb(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgb(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0;
end;

procedure Rgb24BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PInt8u;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PSrc := SourcePtr;
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * 3 *
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
          BlendPixRgb(PDst, PInt8u(PtrComp(PSrc) + From.Order.R)^,
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

      CopyOrBlendPixRgb(PDst, @COLOR, Cover);

      Inc(PtrComp(PSrc), 4);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Rgb24CopyColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt8u;

begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(p)^).R := COLORS.Rgba8.R;
    TAggOrderRGB(Pointer(p)^).g := COLORS.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := COLORS.Rgba8.b;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Inc(PtrComp(p), 3);
    Dec(Len);
  until Len = 0;
end;

procedure Rgb24CopyColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  repeat
    TAggOrderRGB(Pointer(p)^).R := COLORS.Rgba8.R;
    TAggOrderRGB(Pointer(p)^).g := COLORS.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := COLORS.Rgba8.b;

    p := PInt8u(This.RenderingBuffer.NextRow(p));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb24BlendFromColor(This, From: TAggPixelFormatProcessor;
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
      Xdst * 3 * SizeOf(Int8u));

    repeat
      CopyOrBlendPixRgb(PDst, COLOR, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendFromLUT(This, From: TAggPixelFormatProcessor;
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
      Xdst * 3 * SizeOf(Int8u));

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        BlendPixRgb(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixRgb(PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
