//
// AggPas 2.4 RM3 pixel format definition file
//

function MakePixel565Gamma(R, g, b: Cardinal): Int16u;
begin
  Result := Int16u(((R and $F8) shl 8) or ((g and $FC) shl 3) or (b shr 3));
end;

procedure MakeColor565Gamma(var COLOR: TAggColor; p: PInt16u);
begin
  COLOR.FromRgbaInteger((p^ shr 8) and $F8, (p^ shr 3) and $FC, (p^ shl 3)
    and $F8);
end;

procedure BlendPixel565Gamma(Gamma: TAggGamma; p: PInt16u;
  CR, Cg, CB, alpha, Cover: Cardinal);
var
  RGB: Int16u;
  R, g, b: Integer;
begin
  RGB := p^;

  R := Gamma.dir[(RGB shr 8) and $F8];
  g := Gamma.dir[(RGB shr 3) and $FC];
  b := Gamma.dir[(RGB shl 3) and $F8];

  p^ := Int16u(((Gamma.Inv[((Gamma.dir[CR] - R) * alpha + (R shl 8)) shr 8]
    shl 8) and $F800) or ((Gamma.Inv[((Gamma.dir[Cg] - g) * alpha + (g shl 8))
    shr 8] shl 3) and $07E0) or
    (Gamma.Inv[((Gamma.dir[CB] - b) * alpha + (b shl 8)) shr 8] shr 3));
end;

procedure CopyOrBlendPixel565Gamma(Gamma: TAggGamma; p: PInt16u;
  C: PAggColor; Cover: Cardinal);
var
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      p^ := MakePixel565Gamma(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b)
    else
      BlendPixel565Gamma(Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha,
      Cover);
  end;
end;

procedure Rgb565GammaCopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u))^ :=
    MakePixel565Gamma(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);
end;

procedure Rgb565GammaBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixel565Gamma(This.Gamma,
    PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u)), C,
    Cover);
end;

function Rgb565GammaPixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
begin
  MakeColor565Gamma(Result, PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X *
    SizeOf(Int16u)));
end;

procedure Rgb565GammaCopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PInt16u;
  v: Int16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  v := MakePixel565Gamma(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

  repeat
    p^ := v;

    Inc(PtrComp(p), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaCopyVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PInt16u;
  v: Int16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  v := MakePixel565Gamma(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

  repeat
    p^ := v;
    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt16u;
  v: Int16u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v := MakePixel565Gamma(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

      repeat
        p^ := v;

        Inc(PtrComp(p), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPixel565Gamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
          alpha, Cover);

        Inc(PtrComp(p), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb565GammaBlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt16u;
  v: Int16u;

  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v := MakePixel565Gamma(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

      repeat
        p^ := v;
        p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPixel565Gamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
          alpha, Cover);

        p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb565GammaBlendSolidHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPixel565Gamma(This.Gamma, p, C, Covers^);

    Inc(PtrComp(Covers));
    Inc(PtrComp(p), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaBlendSolidVSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPixel565Gamma(This.Gamma, p, C, Covers^);

    Inc(PtrComp(Covers));

    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaBlendColorHSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPixel565Gamma(This.Gamma, p, COLORS, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPixel565Gamma(This.Gamma, p, COLORS, Cover);

    Inc(PtrComp(p), SizeOf(Int16u));
    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaBlendColorVSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPixel565Gamma(This.Gamma, p, COLORS, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPixel565Gamma(This.Gamma, p, COLORS, Cover);

    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaCopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt16u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int16u))^,
    PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int16))^,
    Len * SizeOf(Int16u));
end;

procedure Rgb565GammaBlendFrom(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; SourcePtr: PInt8u; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PDst: PInt16u;

  alpha: Cardinal;
begin
  PDst := PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int16u));

  repeat
    alpha := PInt8u(PtrComp(SourcePtr) + From.Order.A)^;

    if alpha <> 0 then
      if (alpha = CAggBaseMask) and (Cover = 255) then
        PDst^ := MakePixel565Gamma(PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.b)^)
      else
        BlendPixel565Gamma(This.Gamma, PDst,
          PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.b)^, alpha, Cover);

    Inc(PtrComp(SourcePtr), 4);
    Inc(PtrComp(PDst), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaCopyColorHSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    p^ := MakePixel565Gamma(COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b);

    Inc(PtrComp(p), SizeOf(Int16u));
    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaCopyColorVSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    p^ := MakePixel565Gamma(COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b);
    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565GammaBlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PInt16u;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt16u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * SizeOf(Int16u));

    repeat
      BlendPixel565Gamma(This.Gamma, PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
        COLOR.Rgba8.b, COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb565GammaBlendFromLUT(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PInt16u;

  COLOR: PAggColor;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt16u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * SizeOf(Int16u));

    repeat
      COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

      BlendPixel565Gamma(This.Gamma, PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
        COLOR.Rgba8.b, COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end; 
 
