//
// AggPas 2.4 RM3 pixel format definition file
//

function MakePix565(R, g, b: Cardinal): Int16u;
begin
  Result := ((R and $F8) shl 8) or ((g and $FC) shl 3) or (b shr 3);
end;

procedure MakeColor565(var COLOR: TAggColor; p: PInt16u);
begin
  COLOR.FromRgbaInteger((p^ shr 8) and $F8, (p^ shr 3) and $FC, (p^ shl 3)
    and $F8);
end;

procedure BlendPix565(p: PInt16u; CR, Cg, CB, alpha: Integer);
var
  RGB: Int16u;
  R, g, b: Integer;
begin
  RGB := p^;

  R := (RGB shr 8) and $F8;
  g := (RGB shr 3) and $FC;
  b := (RGB shl 3) and $F8;

  p^ := ((((CR - R) * alpha + (R shl 8))) and $F800) or
    ((((Cg - g) * alpha + (g shl 8)) shr 5) and $07E0) or
    (((CB - b) * alpha + (b shl 8)) shr 11);
end;

procedure CopyOrBlendPix565(p: PInt16u; C: PAggColor; Cover: Cardinal);
var
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      p^ := MakePix565(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b)
    else
      BlendPix565(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);
  end;
end;

procedure Rgb565CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u))^ :=
    MakePix565(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);
end;

procedure Rgb565BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPix565(PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) +
    X * SizeOf(Int16u)), C, Cover);
end;

function Rgb565Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
begin
  MakeColor565(Result, PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X *
    SizeOf(Int16u)));
end;

procedure Rgb565CopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PInt16u;
  v: Int16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  v := MakePix565(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

  repeat
    p^ := v;

    Inc(PtrComp(p), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565CopyVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PInt16u;
  v: Int16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));
  v := MakePix565(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

  repeat
    p^ := v;
    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb565BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
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
      v := MakePix565(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

      repeat
        p^ := v;

        Inc(PtrComp(p), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPix565(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), SizeOf(Int16u));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb565BlendVerticalLine(This: TAggPixelFormatProcessor; X,
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
      v := MakePix565(C.Rgba8.R, C.Rgba8.g, C.Rgba8.b);

      repeat
        p^ := v;
        p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPix565(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb565BlendSolidHSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPix565(p, C, Covers^);

    Inc(PtrComp(Covers));
    Inc(PtrComp(p), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565BlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    CopyOrBlendPix565(p, C, Covers^);

    Inc(PtrComp(Covers));

    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Dec(Len);
  until Len = 0;
end;

procedure Rgb565BlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPix565(p, COLORS, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPix565(p, COLORS, Cover);

    Inc(PtrComp(p), SizeOf(Int16u));
    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565BlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt16u;

begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPix565(p, COLORS, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPix565(p, COLORS, Cover);

    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565CopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt16u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int16u))^,
    PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int16))^,
    Len * SizeOf(Int16u));
end;

procedure Rgb565BlendFrom(This: TAggPixelFormatProcessor;
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
        PDst^ := MakePix565(PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.b)^)
      else
        BlendPix565(PDst, PInt8u(PtrComp(SourcePtr) + From.Order.R)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.b)^, alpha);

    Inc(PtrComp(SourcePtr), 4);
    Inc(PtrComp(PDst), SizeOf(Int16u));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565CopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    p^ := MakePix565(COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b);

    Inc(PtrComp(p), SizeOf(Int16u));
    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565CopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt16u;

begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(Y)) + X * SizeOf(Int16u));

  repeat
    p^ := MakePix565(COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b);
    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgb565BlendFromColor(This: TAggPixelFormatProcessor;
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
      BlendPix565(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb565BlendFromLUT(This: TAggPixelFormatProcessor;
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

      BlendPix565(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), SizeOf(Int16u));
      Dec(Len);
    until Len = 0;
  end;
end; 
 
