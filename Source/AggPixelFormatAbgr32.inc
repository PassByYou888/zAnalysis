//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixAbgr(p: PCardinal; CR, Cg, CB, alpha: Integer;
  Cover: Cardinal = 0);
var
  R, g, b, A: Int8u;

begin
  R := TAggOrderABGR(p^).R;
  g := TAggOrderABGR(p^).g;
  b := TAggOrderABGR(p^).b;
  A := TAggOrderABGR(p^).A;

  TAggOrderABGR(p^).R := ((CR - R) * alpha + (R shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderABGR(p^).g := ((Cg - g) * alpha + (g shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderABGR(p^).b := ((CB - b) * alpha + (b shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderABGR(p^).A := (alpha + A) - ((alpha * A + CAggBaseMask) shr CAggBaseShift);
end;

procedure CopyOrBlendPixAbgr(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  if alpha <> 0 then
    if alpha = CAggBaseMask then
    begin
      TAggOrderABGR(p^).R := CR;
      TAggOrderABGR(p^).g := Cg;
      TAggOrderABGR(p^).b := CB;
      TAggOrderABGR(p^).A := CAggBaseMask;
    end
    else
      BlendPixAbgr(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixAbgr(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixAbgr(p, CR, Cg, CB, alpha)
  else if alpha <> 0 then
  begin
    alpha := (alpha * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderABGR(p^).R := CR;
      TAggOrderABGR(p^).g := Cg;
      TAggOrderABGR(p^).b := CB;
      TAggOrderABGR(p^).A := CAggBaseMask;
    end
    else
      BlendPixAbgr(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Abgr32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  TAggOrderABGR(p^).R := C.Rgba8.R;
  TAggOrderABGR(p^).g := C.Rgba8.g;
  TAggOrderABGR(p^).b := C.Rgba8.b;
  TAggOrderABGR(p^).A := C.Rgba8.A;
end;

procedure Abgr32BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixAbgr(PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2)
    ), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

function Abgr32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PAggOrderAbgr;
begin
  p := PAggOrderAbgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  Result.FromRgbaInteger(p^.R, p^.g, p^.b, p^.A);
end;

procedure Abgr32CopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PInt8u;
  v: Cardinal;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  TAggOrderABGR(v).R := C.Rgba8.R;
  TAggOrderABGR(v).g := C.Rgba8.g;
  TAggOrderABGR(v).b := C.Rgba8.b;
  TAggOrderABGR(v).A := C.Rgba8.A;

  if Len > 0 then
    repeat
      Cardinal(Pointer(p)^) := v;

      Inc(PtrComp(p), 4);
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PCardinal;
  v: Cardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  TAggOrderABGR(v).R := C.Rgba8.R;
  TAggOrderABGR(v).g := C.Rgba8.g;
  TAggOrderABGR(v).b := C.Rgba8.b;
  TAggOrderABGR(v).A := C.Rgba8.A;

  if Len > 0 then
    repeat
      p^ := v;
      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0;
end;

procedure Abgr32BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;

  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderABGR(v).R := C.Rgba8.R;
      TAggOrderABGR(v).g := C.Rgba8.g;
      TAggOrderABGR(v).b := C.Rgba8.b;
      TAggOrderABGR(v).A := C.Rgba8.A;

      repeat
        p^ := v;

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;

  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderABGR(v).R := C.Rgba8.R;
      TAggOrderABGR(v).g := C.Rgba8.g;
      TAggOrderABGR(v).b := C.Rgba8.b;
      TAggOrderABGR(v).A := C.Rgba8.A;

      repeat
        p^ := v;
        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32BlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderABGR(p^).R := C.Rgba8.R;
        TAggOrderABGR(p^).g := C.Rgba8.g;
        TAggOrderABGR(p^).b := C.Rgba8.b;
        TAggOrderABGR(p^).A := CAggBaseMask;
      end
      else
        BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32BlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderABGR(p^).R := C.Rgba8.R;
        TAggOrderABGR(p^).g := C.Rgba8.g;
        TAggOrderABGR(p^).b := C.Rgba8.b;
        TAggOrderABGR(p^).A := CAggBaseMask;
      end
      else
        BlendPixAbgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32BlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32BlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgr(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PCardinal;
  Incp: Integer;
begin
  PSrc := PCardinal(SourcePtr);
  PDst := PCardinal(PtrComp(This.RenderingBuffer.Row(Ydst)) + (Xdst shl 2) *
    SizeOf(Int8u));
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(PSrc), ((Len - 1) shl 2) * SizeOf(Int8u));
    Inc(PtrComp(PDst), ((Len - 1) shl 2) * SizeOf(Int8u));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixAbgr(PDst, TAggOrderABGR(PSrc^).R, TAggOrderABGR(PSrc^).g,
        TAggOrderABGR(PSrc^).b, TAggOrderABGR(PSrc^).A);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgr(PDst, TAggOrderABGR(PSrc^).R, TAggOrderABGR(PSrc^).g,
        TAggOrderABGR(PSrc^).b, TAggOrderABGR(PSrc^).A, Cover);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32CopyColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  repeat
    TAggOrderABGR(Pointer(p)^).R := COLORS.Rgba8.R;
    TAggOrderABGR(Pointer(p)^).g := COLORS.Rgba8.g;
    TAggOrderABGR(Pointer(p)^).b := COLORS.Rgba8.b;
    TAggOrderABGR(Pointer(p)^).A := COLORS.Rgba8.A;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Inc(PtrComp(p), 4);
    Dec(Len);
  until Len = 0;
end;

procedure Abgr32CopyColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  repeat
    TAggOrderABGR(Pointer(p)^).R := COLORS.Rgba8.R;
    TAggOrderABGR(Pointer(p)^).g := COLORS.Rgba8.g;
    TAggOrderABGR(Pointer(p)^).b := COLORS.Rgba8.b;
    TAggOrderABGR(Pointer(p)^).A := COLORS.Rgba8.A;

    p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Abgr32BlendFromColor(This, From: TAggPixelFormatProcessor;
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
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    repeat
      CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32BlendFromLUT(This, From: TAggPixelFormatProcessor;
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
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A, Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
