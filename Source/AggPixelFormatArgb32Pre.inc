//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixPreARGB(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  TAggOrderARGB(p^).R := Int8u(((TAggOrderARGB(p^).R * alpha) shr CAggBaseShift) + CR);
  TAggOrderARGB(p^).g := Int8u(((TAggOrderARGB(p^).g * alpha) shr CAggBaseShift) + Cg);
  TAggOrderARGB(p^).b := Int8u(((TAggOrderARGB(p^).b * alpha) shr CAggBaseShift) + CB);
  TAggOrderARGB(p^).A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderARGB(p^).A)) shr CAggBaseShift));
end;

procedure BlendPixPreARGB(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderARGB(p^).R := Int8u((TAggOrderARGB(p^).R * alpha + CR * Cover)
    shr CAggBaseShift);
  TAggOrderARGB(p^).g := Int8u((TAggOrderARGB(p^).g * alpha + Cg * Cover)
    shr CAggBaseShift);
  TAggOrderARGB(p^).b := Int8u((TAggOrderARGB(p^).b * alpha + CB * Cover)
    shr CAggBaseShift);
  TAggOrderARGB(p^).A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderARGB(p^).A)) shr CAggBaseShift));
end;

procedure CopyOrBlendPixPreARGB(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  if alpha <> 0 then
    if alpha = CAggBaseMask then
    begin
      TAggOrderARGB(p^).R := CR;
      TAggOrderARGB(p^).g := Cg;
      TAggOrderARGB(p^).b := CB;
      TAggOrderARGB(p^).A := CAggBaseMask;
    end
    else
      BlendPixPreARGB(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixPreARGB(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixPreARGB(p, CR, Cg, CB, alpha)
  else if alpha <> 0 then
  begin
    alpha := (alpha * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderARGB(p^).R := CR;
      TAggOrderARGB(p^).g := Cg;
      TAggOrderARGB(p^).b := CB;
      TAggOrderARGB(p^).A := CAggBaseMask;
    end
    else
      BlendPixPreARGB(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure ARGB32PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixPreARGB(PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) +
    (X shl 2)), C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

procedure ARGB32PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
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
      TAggOrderARGB(v).R := C.Rgba8.R;
      TAggOrderARGB(v).g := C.Rgba8.g;
      TAggOrderARGB(v).b := C.Rgba8.b;
      TAggOrderARGB(v).A := C.Rgba8.A;

      repeat
        p^ := v;

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixPreARGB(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixPreARGB(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        Inc(PtrComp(p), SizeOf(Cardinal));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure ARGB32PreBlendVerticalLine(This: TAggPixelFormatProcessor; X,
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
      TAggOrderARGB(v).R := C.Rgba8.R;
      TAggOrderARGB(v).g := C.Rgba8.g;
      TAggOrderARGB(v).b := C.Rgba8.b;
      TAggOrderARGB(v).A := C.Rgba8.A;

      repeat
        p^ := v;
        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixPreARGB(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixPreARGB(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure ARGB32PreBlendSolidHSpan(This: TAggPixelFormatProcessor; X,
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
        TAggOrderARGB(p^).R := C.Rgba8.R;
        TAggOrderARGB(p^).g := C.Rgba8.g;
        TAggOrderARGB(p^).b := C.Rgba8.b;
        TAggOrderARGB(p^).A := CAggBaseMask;
      end
      else
        BlendPixPreARGB(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure ARGB32PreBlendSolidVSpan(This: TAggPixelFormatProcessor; X,
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
        TAggOrderARGB(p^).R := C.Rgba8.R;
        TAggOrderARGB(p^).g := C.Rgba8.g;
        TAggOrderARGB(p^).b := C.Rgba8.b;
        TAggOrderARGB(p^).A := CAggBaseMask;

      end
      else
        BlendPixPreARGB(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure ARGB32PreBlendColorHSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixPreARGB(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);

    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixPreARGB(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixPreARGB(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      Inc(PtrComp(p), 4);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure ARGB32PreBlendColorVSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixPreARGB(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(PtrComp(Covers), SizeOf(Int8u));

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixPreARGB(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixPreARGB(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure ARGB32PreBlendFrom(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixPreARGB(PDst, TAggOrderARGB(PSrc^).R, TAggOrderARGB(PSrc^).g,
        TAggOrderARGB(PSrc^).b, TAggOrderARGB(PSrc^).A);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixPreARGB(PDst, TAggOrderARGB(PSrc^).R, TAggOrderARGB(PSrc^).g,
        TAggOrderARGB(PSrc^).b, TAggOrderARGB(PSrc^).A, Cover);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure ARGB32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixPreARGB(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure ARGB32PreBlendFromLut(This, From: TAggPixelFormatProcessor;
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

        CopyOrBlendPixPreARGB(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixPreARGB(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A, Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
