//
// AggPas 2.4 RM3 pixel format definition file
//

procedure CublCopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
begin
  This.Blender(This, This.BlendMode,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u)),
    C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, 255);
end;

procedure CublBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor;
  Cover: Int8u);
begin
  This.Blender(This, This.BlendMode,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u)),
    C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

function CublPixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  Result.FromRgbaInteger(PInt8u(PtrComp(p) + This.Order.R)^,
    PInt8u(PtrComp(p) + This.Order.g)^, PInt8u(PtrComp(p) + This.Order.b)^,
    PInt8u(PtrComp(p) + This.Order.A)^);
end;

procedure CublCopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
      C.Rgba8.A, 255);

    Inc(PtrComp(p), 4);
    Dec(Len);
  until Len = 0;
end;

procedure CublCopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
      C.Rgba8.A, 255);

    p := This.RenderingBuffer.NextRow(p);

    Dec(Len);
  until Len = 0;
end;

procedure CublBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
      C.Rgba8.A, Cover);

    Inc(PtrComp(p), 4);
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
      C.Rgba8.A, Cover);

    p := This.RenderingBuffer.NextRow(p);

    Dec(Len);
  until Len = 0;
end;

procedure CublBlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
      C.Rgba8.A, Covers^);

    Inc(PtrComp(Covers), SizeOf(Int8u));
    Inc(PtrComp(p), 4);
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
      C.Rgba8.A, Covers^);

    Inc(PtrComp(Covers), SizeOf(Int8u));

    p := This.RenderingBuffer.NextRow(p);

    Dec(Len);
  until Len = 0;
end;

procedure CublBlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    if Covers <> nil then
    begin
      This.Blender(This, This.BlendMode, p, COLORS.Rgba8.R, COLORS.Rgba8.g,
        COLORS.Rgba8.b, COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

    end
    else
      This.Blender(This, This.BlendMode, p, COLORS.Rgba8.R, COLORS.Rgba8.g,
        COLORS.Rgba8.b, COLORS.Rgba8.A, Cover);

    Inc(PtrComp(p), 4);
    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    if Covers <> nil then
    begin
      This.Blender(This, This.BlendMode, p, COLORS.Rgba8.R, COLORS.Rgba8.g,
        COLORS.Rgba8.b, COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

    end
    else
      This.Blender(This, This.BlendMode, p, COLORS.Rgba8.R, COLORS.Rgba8.g,
        COLORS.Rgba8.b, COLORS.Rgba8.A, Cover);

    p := This.RenderingBuffer.NextRow(p);

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PInt8u;
  Incp      : Integer;
begin
  PSrc := SourcePtr;
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + (Xdst shl 2) *
    SizeOf(Int8u));
  Incp := 4;

  if Xdst > Xsrc then
  begin
    Inc(PtrComp(PSrc), (Len - 1) shl 2);
    Inc(PtrComp(PDst), (Len - 1) shl 2);

    Incp := -4;
  end;

  repeat
    This.Blender(This, This.BlendMode, PDst,
      PInt8u(PtrComp(PSrc) + From.Order.R)^,
      PInt8u(PtrComp(PSrc) + From.Order.g)^,
      PInt8u(PtrComp(PSrc) + From.Order.b)^,
      PInt8u(PtrComp(PSrc) + From.Order.A)^, Cover);

    Inc(PtrComp(PSrc), Incp);
    Inc(PtrComp(PDst), Incp);
    Dec(Len);
  until Len = 0;
end;

procedure CublCopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    PInt8u(PtrComp(p) + This.Order.R)^ := COLORS.Rgba8.R;
    PInt8u(PtrComp(p) + This.Order.g)^ := COLORS.Rgba8.g;
    PInt8u(PtrComp(p) + This.Order.b)^ := COLORS.Rgba8.b;
    PInt8u(PtrComp(p) + This.Order.A)^ := COLORS.Rgba8.A;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Inc(PtrComp(p), 4);
    Dec(Len);
  until Len = 0;
end;

procedure CublCopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2) * SizeOf(Int8u));

  repeat
    PInt8u(PtrComp(p) + This.Order.R)^ := COLORS.Rgba8.R;
    PInt8u(PtrComp(p) + This.Order.g)^ := COLORS.Rgba8.g;
    PInt8u(PtrComp(p) + This.Order.b)^ := COLORS.Rgba8.b;
    PInt8u(PtrComp(p) + This.Order.A)^ := COLORS.Rgba8.A;

    p := This.RenderingBuffer.NextRow(p);

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure CublBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      (Xdst shl 2));

    repeat
      This.Blender(This, This.BlendMode, PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
        COLOR.Rgba8.b, COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure CublBlendFromLUT(This, From: TAggPixelFormatProcessor;
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
      (Xdst shl 2));

    repeat
      COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

      This.Blender(This, This.BlendMode, PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
        COLOR.Rgba8.b, COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 4);
      Dec(Len);
    until Len = 0;
  end;
end; 
 
