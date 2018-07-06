//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgbaPre(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  TAggOrderRGBA(p^).R := Int8u(((TAggOrderRGBA(p^).R * alpha) shr CAggBaseShift) + CR);
  TAggOrderRGBA(p^).g := Int8u(((TAggOrderRGBA(p^).g * alpha) shr CAggBaseShift) + Cg);
  TAggOrderRGBA(p^).b := Int8u(((TAggOrderRGBA(p^).b * alpha) shr CAggBaseShift) + CB);
  TAggOrderRGBA(p^).A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderRGBA(p^).A)) shr CAggBaseShift));
end;

procedure BlendPixRgbaPre(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderRGBA(p^).R := Int8u((TAggOrderRGBA(p^).R * alpha + CR * Cover)
    shr CAggBaseShift);
  TAggOrderRGBA(p^).g := Int8u((TAggOrderRGBA(p^).g * alpha + Cg * Cover)
    shr CAggBaseShift);
  TAggOrderRGBA(p^).b := Int8u((TAggOrderRGBA(p^).b * alpha + CB * Cover)
    shr CAggBaseShift);
  TAggOrderRGBA(p^).A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderRGBA(p^).A)) shr CAggBaseShift));
end;

procedure CopyOrBlendPixRgbaPre(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  if alpha <> 0 then
    if alpha = CAggBaseMask then
    begin
      TAggOrderRGBA(p^).R := CR;
      TAggOrderRGBA(p^).g := Cg;
      TAggOrderRGBA(p^).b := CB;
      TAggOrderRGBA(p^).A := CAggBaseMask;
    end
    else
      BlendPixRgbaPre(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixRgbaPre(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixRgbaPre(p, CR, Cg, CB, alpha)
  else if alpha <> 0 then
  begin
    alpha := (alpha * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderRGBA(p^).R := CR;
      TAggOrderRGBA(p^).g := Cg;
      TAggOrderRGBA(p^).b := CB;
      TAggOrderRGBA(p^).A := CAggBaseMask;
    end
    else
      BlendPixRgbaPre(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Rgba32PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixRgbaPre(PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) +
    (X shl 2)), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

procedure Rgba32PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
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
      TAggOrderRGBA(v).R := C.Rgba8.R;
      TAggOrderRGBA(v).g := C.Rgba8.g;
      TAggOrderRGBA(v).b := C.Rgba8.b;
      TAggOrderRGBA(v).A := C.Rgba8.A;

      repeat
        p^ := v;

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixRgbaPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgbaPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32PreBlendVerticalLine(This: TAggPixelFormatProcessor; X,
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
      TAggOrderRGBA(v).R := C.Rgba8.R;
      TAggOrderRGBA(v).g := C.Rgba8.g;
      TAggOrderRGBA(v).b := C.Rgba8.b;
      TAggOrderRGBA(v).A := C.Rgba8.A;

      repeat
        p^ := v;
        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixRgbaPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgbaPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32PreBlendSolidHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        TAggOrderRGBA(p^).R := C.Rgba8.R;
        TAggOrderRGBA(p^).g := C.Rgba8.g;
        TAggOrderRGBA(p^).b := C.Rgba8.b;
        TAggOrderRGBA(p^).A := CAggBaseMask;
      end
      else
        BlendPixRgbaPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32PreBlendSolidVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        TAggOrderRGBA(p^).R := C.Rgba8.R;
        TAggOrderRGBA(p^).g := C.Rgba8.g;
        TAggOrderRGBA(p^).b := C.Rgba8.b;
        TAggOrderRGBA(p^).A := CAggBaseMask;
      end
      else
        BlendPixRgbaPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32PreBlendColorHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbaPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbaPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbaPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32PreBlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbaPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbaPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbaPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32PreBlendFrom(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixRgbaPre(PDst, TAggOrderRGBA(PSrc^).R, TAggOrderRGBA(PSrc^).g,
        TAggOrderRGBA(PSrc^).b, TAggOrderRGBA(PSrc^).A);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbaPre(PDst, TAggOrderRGBA(PSrc^).R, TAggOrderRGBA(PSrc^).g,
        TAggOrderRGBA(PSrc^).b, TAggOrderRGBA(PSrc^).A, Cover);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixRgbaPre(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
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

        CopyOrBlendPixRgbaPre(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixRgbaPre(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A, Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
