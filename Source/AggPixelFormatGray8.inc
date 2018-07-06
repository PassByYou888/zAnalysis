//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixGray(p: PInt8u; cv, alpha: Cardinal; Cover: Cardinal = 0);
begin
  p^ := Int8u((((cv - p^) * alpha) + (p^ shl CAggBaseShift)) shr CAggBaseShift);
end;

procedure CopyOrBlendPixGray(p: PInt8u; C: PAggColor;
  Cover: Cardinal); overload;
var
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      p^ := C.v
    else
      BlendPixGray(p, C.v, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixGray(p: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
      p^ := C.v
    else
      BlendPixGray(p, C.v, C.Rgba8.A);
end;

procedure Gray8CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset)^ := C.v;
end;

procedure Gray8BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixGray(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) +
    X * This.Step + This.Offset), C, Cover);
end;

function Gray8Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  Result.FromValueInteger(p^);
end;

procedure Gray8CopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    p^ := C.v;

    Inc(PtrComp(p), This.Step);
    Dec(Len);
  until Len = 0;
end;

procedure Gray8CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    p^ := C.v;
    p := This.RenderingBuffer.NextRow(p);

    Dec(Len);
  until Len = 0;
end;

procedure Gray8BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        p^ := C.v;

        Inc(PtrComp(p), This.Step);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixGray(p, C.v, alpha, Cover);

        Inc(PtrComp(p), This.Step);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt8u;

  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        p^ := C.v;
        p := This.RenderingBuffer.NextRow(p);

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixGray(p, C.v, alpha, Cover);

        p := This.RenderingBuffer.NextRow(p);

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8BlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
        p^ := C.v
      else
        BlendPixGray(p, C.v, alpha, Covers^);

      Inc(PtrComp(p), This.Step);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
      This.Offset);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
        p^ := C.v
      else
        BlendPixGray(p, C.v, alpha, Covers^);

      p := This.RenderingBuffer.NextRow(p);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGray(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), This.Step);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      if COLORS.Rgba8.A = CAggBaseMask then
        p^ := COLORS.v
      else
        CopyOrBlendPixGray(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), This.Step);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGray(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), This.Step);
      Dec(Len);
    until Len = 0;
end;

procedure Gray8BlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGray(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));

      p := This.RenderingBuffer.NextRow(p);

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      if COLORS.Rgba8.A = CAggBaseMask then
        p^ := COLORS.v
      else
        CopyOrBlendPixGray(p, COLORS);

      p := This.RenderingBuffer.NextRow(p);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGray(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := This.RenderingBuffer.NextRow(p);

      Dec(Len);
    until Len = 0;
end;

procedure Gray8CopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt8u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int8u))^,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int8u))^,
    Len * SizeOf(Int8u));
end;

procedure Gray8CopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    p^ := COLORS.v;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Inc(PtrComp(p), This.Step);
    Dec(Len);
  until Len = 0;
end;

procedure Gray8CopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  repeat
    p^ := COLORS.v;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));

    p := This.RenderingBuffer.NextRow(p);

    Dec(Len);
  until Len = 0;
end;

procedure Gray8BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  PSrc, PDst: PInt8u;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGray(PDst, COLOR, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(PSrc), PixWidth);
      Inc(PtrComp(PDst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  PSrc, PDst: PInt8u;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGray(PDst, PAggColor(PtrComp(ColorLUT) + PSrc^ *
        SizeOf(TAggColor)), Cover);

      Inc(PtrComp(PSrc), PixWidth);
      Inc(PtrComp(PDst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end; 
 
