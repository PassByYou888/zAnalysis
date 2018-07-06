//
// AggPas 2.4 RM3 pixel format definition file
//

procedure Alpha32CopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PCardinal;
  v: Cardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

  PInt8u(PtrComp(@v) + This.Order.R)^ := C.Rgba8.R;
  PInt8u(PtrComp(@v) + This.Order.g)^ := C.Rgba8.g;
  PInt8u(PtrComp(@v) + This.Order.b)^ := C.Rgba8.b;
  PInt8u(PtrComp(@v) + This.Order.A)^ := C.Rgba8.A;

  if Len > 0 then
    repeat
      p^ := v;

      Inc(PtrComp(p), 4);
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        PInt8u(PtrComp(p) + This.Order.R)^ := C.Rgba8.R;
        PInt8u(PtrComp(p) + This.Order.g)^ := C.Rgba8.g;
        PInt8u(PtrComp(p) + This.Order.b)^ := C.Rgba8.b;
        PInt8u(PtrComp(p) + This.Order.A)^ := CAggBaseMask;
      end
      else
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Covers^);
          1:
            BlendPixArgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);
          2:
            BlendPixBgra(PAggOrderBgra(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Covers^);
          3:
            BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);
        end;

      Inc(PtrComp(p), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)));
  Inc(p, X);

  if Covers <> nil then
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Covers^);
        1:
          CopyOrBlendPixArgb(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Covers^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Covers^);
        3:
          CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Covers^);
      end;

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A);
        1:
          CopyOrBlendPixArgb(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A);
        3:
          CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A);
      end;

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Cover);
        1:
          CopyOrBlendPixArgb(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Cover);
        3:
          CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Cover);
      end;

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PCardinal;
  Incp: Integer;
begin
  PSrc := PCardinal(SourcePtr);
  PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
    (Xdst shl 2) * SizeOf(Int8u));
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(PSrc), ((Len - 1) shl 2) * SizeOf(Int8u));
    Inc(PtrComp(PDst), ((Len - 1) shl 2) * SizeOf(Int8u));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^);
        1:
          CopyOrBlendPixArgb(PDst, PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^);
        3:
          CopyOrBlendPixAbgr(PDst, PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^);
      end;

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^, Cover);
        1:
          CopyOrBlendPixArgb(PDst, PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^, Cover);
        3:
          CopyOrBlendPixAbgr(PDst, PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.A)^, Cover);
      end;

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0;
end;

function Alpha32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PCardinal;
begin
  p := PCardinal(This.RenderingBuffer.Row(Y));

  if p <> nil then
  begin
    Inc(PtrComp(p), X shl 2);

    Result.FromRgbaInteger(PInt8u(PtrComp(p) + This.Order.R)^,
      PInt8u(PtrComp(p) + This.Order.g)^, PInt8u(PtrComp(p) + This.Order.b)
      ^, PInt8u(PtrComp(p) + This.Order.A)^);
  end;
end;

procedure Alpha32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

  PInt8u(PtrComp(p) + This.Order.R)^ := C.Rgba8.R;
  PInt8u(PtrComp(p) + This.Order.g)^ := C.Rgba8.g;
  PInt8u(PtrComp(p) + This.Order.b)^ := C.Rgba8.b;
  PInt8u(PtrComp(p) + This.Order.A)^ := C.Rgba8.A;
end;

procedure Alpha32BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  case This.Order.R of
    0:
      CopyOrBlendPixRgba(PAggOrderRgba(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
    1:
      CopyOrBlendPixArgb(PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
    2:
      CopyOrBlendPixBgra(PAggOrderBgra(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
    3:
      CopyOrBlendPixAbgr(PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) +
        (X shl 2)), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
  end;
end;

procedure Alpha32CopyVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PCardinal;
  v: Cardinal;
begin
  PInt8u(PtrComp(@v) + This.Order.R)^ := C.Rgba8.R;
  PInt8u(PtrComp(@v) + This.Order.g)^ := C.Rgba8.g;
  PInt8u(PtrComp(@v) + This.Order.b)^ := C.Rgba8.b;
  PInt8u(PtrComp(@v) + This.Order.A)^ := C.Rgba8.A;

  if Len > 0 then
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));
      p^ := v;

      Inc(Y);
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;

  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      PInt8u(PtrComp(@v) + This.Order.R)^ := C.Rgba8.R;
      PInt8u(PtrComp(@v) + This.Order.g)^ := C.Rgba8.g;
      PInt8u(PtrComp(@v) + This.Order.b)^ := C.Rgba8.b;
      PInt8u(PtrComp(@v) + This.Order.A)^ := C.Rgba8.A;

      repeat
        p^ := v;

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha);
          1:
            BlendPixArgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);
          2:
            BlendPixBgra(PAggOrderBgra(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha);
          3:
            BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);
        end;

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Cover);
          1:
            BlendPixArgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
          2:
            BlendPixBgra(PAggOrderBgra(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Cover);
          3:
            BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
        end;

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Alpha32BlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      PInt8u(PtrComp(@v) + This.Order.R)^ := C.Rgba8.R;
      PInt8u(PtrComp(@v) + This.Order.g)^ := C.Rgba8.g;
      PInt8u(PtrComp(@v) + This.Order.b)^ := C.Rgba8.b;
      PInt8u(PtrComp(@v) + This.Order.A)^ := C.Rgba8.A;

      repeat
        p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));
        p^ := v;

        Inc(Y);
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha);
          1:
            BlendPixArgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);
          2:
            BlendPixBgra(PAggOrderBgra(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha);
          3:
            BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);
        end;

        Inc(Y);
        Dec(Len);
      until Len = 0
    else
      repeat
        p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Cover);
          1:
            BlendPixArgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
          2:
            BlendPixBgra(PAggOrderBgra(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Cover);
          3:
            BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
        end;

        Inc(Y);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Alpha32BlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        PInt8u(PtrComp(p) + This.Order.R)^ := C.Rgba8.R;
        PInt8u(PtrComp(p) + This.Order.g)^ := C.Rgba8.g;
        PInt8u(PtrComp(p) + This.Order.b)^ := C.Rgba8.b;
        PInt8u(PtrComp(p) + This.Order.A)^ := CAggBaseMask;
      end
      else
        case This.Order.R of
          0:
            BlendPixRgba(PAggOrderRgba(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Covers^);
          1:
            BlendPixArgb(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);
          2:
            BlendPixBgra(PAggOrderBgra(p), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
              alpha, Covers^);
          3:
            BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);
        end;

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(Y);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;

begin
  if Covers <> nil then
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Covers^);
        1:
          CopyOrBlendPixArgb(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Covers^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Covers^);
        3:
          CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Covers^);
      end;

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(Y);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A);
        1:
          CopyOrBlendPixArgb(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A);
        3:
          CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A);
      end;

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(Y);
      Dec(Len);
    until Len = 0
  else
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Cover);
        1:
          CopyOrBlendPixArgb(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), COLORS.Rgba8.R, COLORS.Rgba8.g,
            COLORS.Rgba8.b, COLORS.Rgba8.A, Cover);
        3:
          CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
            COLORS.Rgba8.A, Cover);
      end;

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(Y);
      Dec(Len);
    until Len = 0;
end;

procedure Alpha32CopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, Len)) + (X shl 2));

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

procedure Alpha32CopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PCardinal;
begin
  repeat
    p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(X, Y, 1)) + (X shl 2));

    PInt8u(PtrComp(p) + This.Order.R)^ := COLORS.Rgba8.R;
    PInt8u(PtrComp(p) + This.Order.g)^ := COLORS.Rgba8.g;
    PInt8u(PtrComp(p) + This.Order.b)^ := COLORS.Rgba8.b;
    PInt8u(PtrComp(p) + This.Order.A)^ := COLORS.Rgba8.A;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Inc(Y);
    Dec(Len);
  until Len = 0;
end;

procedure Alpha32BlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PCardinal;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)));
    Inc(PDst, Xdst);

    repeat
      case This.Order.R of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(PDst), COLOR.Rgba8.R, COLOR.Rgba8.g,
            COLOR.Rgba8.b, COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask,
            CAggBaseShift));
        1:
          CopyOrBlendPixArgb(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
            COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(PDst), COLOR.Rgba8.R, COLOR.Rgba8.g,
            COLOR.Rgba8.b, COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask,
            CAggBaseShift));
        3:
          CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
            COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));
      end;

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PDst);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PCardinal;

  COLOR: PAggColor;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)));
    Inc(PDst, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        case This.Order.R of
          0:
            CopyOrBlendPixRgba(PAggOrderRgba(PDst), COLOR.Rgba8.R,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.A);
          1:
            CopyOrBlendPixArgb(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.A);
          2:
            CopyOrBlendPixBgra(PAggOrderBgra(PDst), COLOR.Rgba8.R,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.A);
          3:
            CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.A);
        end;

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PDst);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        case This.Order.R of
          0:
            CopyOrBlendPixRgba(PAggOrderRgba(PDst), COLOR.Rgba8.R,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.A, Cover);
          1:
            CopyOrBlendPixArgb(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.A, Cover);
          2:
            CopyOrBlendPixBgra(PAggOrderBgra(PDst), COLOR.Rgba8.R,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.A, Cover);
          3:
            CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.A, Cover);
        end;

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PDst);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
