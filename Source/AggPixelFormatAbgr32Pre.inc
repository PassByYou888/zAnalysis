//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixAbgrPre(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  TAggOrderABGR(p^).R := Int8u(((TAggOrderABGR(p^).R * alpha) shr CAggBaseShift) + CR);
  TAggOrderABGR(p^).g := Int8u(((TAggOrderABGR(p^).g * alpha) shr CAggBaseShift) + Cg);
  TAggOrderABGR(p^).b := Int8u(((TAggOrderABGR(p^).b * alpha) shr CAggBaseShift) + CB);
  TAggOrderABGR(p^).A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderABGR(p^).A)) shr CAggBaseShift));
end;

procedure BlendPixAbgrPre(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderABGR(p^).R := Int8u((TAggOrderABGR(p^).R * alpha + CR * Cover)
    shr CAggBaseShift);
  TAggOrderABGR(p^).g := Int8u((TAggOrderABGR(p^).g * alpha + Cg * Cover)
    shr CAggBaseShift);
  TAggOrderABGR(p^).b := Int8u((TAggOrderABGR(p^).b * alpha + CB * Cover)
    shr CAggBaseShift);
  TAggOrderABGR(p^).A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderABGR(p^).A)) shr CAggBaseShift));
end;

procedure CopyOrBlendPixAbgrPre(p: PCardinal;
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
      BlendPixAbgrPre(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixAbgrPre(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixAbgrPre(p, CR, Cg, CB, alpha)
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
      BlendPixAbgrPre(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Abgr32PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixAbgrPre(PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) +
    (X shl 2)), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

procedure Abgr32PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
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
        BlendPixAbgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32PreBlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixAbgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32PreBlendSolidHSpan(This: TAggPixelFormatProcessor; X,
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
        TAggOrderABGR(p^).R := C.Rgba8.R;
        TAggOrderABGR(p^).g := C.Rgba8.g;
        TAggOrderABGR(p^).b := C.Rgba8.b;
        TAggOrderABGR(p^).A := CAggBaseMask;
      end
      else
        BlendPixAbgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendSolidVSpan(This: TAggPixelFormatProcessor; X,
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
        TAggOrderABGR(p^).R := C.Rgba8.R;
        TAggOrderABGR(p^).g := C.Rgba8.g;
        TAggOrderABGR(p^).b := C.Rgba8.b;
        TAggOrderABGR(p^).A := CAggBaseMask;
      end
      else
        BlendPixAbgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendColorHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgrPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgrPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendColorVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgrPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgrPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendFrom(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixAbgrPre(PDst, TAggOrderABGR(PSrc^).R, TAggOrderABGR(PSrc^).g,
        TAggOrderABGR(PSrc^).b, TAggOrderABGR(PSrc^).A);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(PDst, TAggOrderABGR(PSrc^).R, TAggOrderABGR(PSrc^).g,
        TAggOrderABGR(PSrc^).b, TAggOrderABGR(PSrc^).A, Cover);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixAbgrPre(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
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

        CopyOrBlendPixAbgrPre(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixAbgrPre(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A, Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
