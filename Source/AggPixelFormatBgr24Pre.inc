//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgrPre(p: PInt8u;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderBGR(Pointer(p)^).R :=
    Int8u((TAggOrderBGR(Pointer(p)^).R * alpha + CR * Cover) shr CAggBaseShift);
  TAggOrderBGR(Pointer(p)^).g :=
    Int8u((TAggOrderBGR(Pointer(p)^).g * alpha + Cg * Cover) shr CAggBaseShift);
  TAggOrderBGR(Pointer(p)^).b :=
    Int8u((TAggOrderBGR(Pointer(p)^).b * alpha + CB * Cover) shr CAggBaseShift);

end;

procedure BlendPixBgrPre(p: PInt8u; CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  TAggOrderBGR(Pointer(p)^).R :=
    Int8u(((TAggOrderBGR(Pointer(p)^).R * alpha) shr CAggBaseShift) + CR);
  TAggOrderBGR(Pointer(p)^).g :=
    Int8u(((TAggOrderBGR(Pointer(p)^).g * alpha) shr CAggBaseShift) + Cg);
  TAggOrderBGR(Pointer(p)^).b :=
    Int8u(((TAggOrderBGR(Pointer(p)^).b * alpha) shr CAggBaseShift) + CB);

end;

procedure CopyOrBlendPixBgrPre(p: PInt8u; C: PAggColor;
  Cover: Cardinal); overload;
var
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderBGR(Pointer(p)^).R := C.Rgba8.R;
      TAggOrderBGR(Pointer(p)^).g := C.Rgba8.g;
      TAggOrderBGR(Pointer(p)^).b := C.Rgba8.b;
    end
    else
      BlendPixBgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgrPre(p: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      TAggOrderBGR(Pointer(p)^).R := C.Rgba8.R;
      TAggOrderBGR(Pointer(p)^).g := C.Rgba8.g;
      TAggOrderBGR(Pointer(p)^).b := C.Rgba8.b;
    end
    else
      BlendPixBgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A);
end;

procedure Bgr24PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixBgrPre(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X),
    C, Cover);
end;

procedure Bgr24PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        TAggOrderBGR(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := C.Rgba8.b;

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24PreBlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        TAggOrderBGR(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := C.Rgba8.b;

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24PreBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        TAggOrderBGR(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := C.Rgba8.b;
      end
      else
        BlendPixBgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(PtrComp(p), 3);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        TAggOrderBGR(Pointer(p)^).R := C.Rgba8.R;
        TAggOrderBGR(Pointer(p)^).g := C.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := C.Rgba8.b;
      end
      else
        BlendPixBgrPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrPre(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrPre(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrPre(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X + X + X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrPre(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(Covers);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrPre(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrPre(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      Dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PInt8u;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PSrc := SourcePtr;
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + 3 * Xdst *
    SizeOf(Int8u));

  if Cover = 255 then
    repeat
      alpha := PInt8u(PtrComp(PSrc) + From.Order.A)^;

      if alpha <> 0 then
        if alpha = CAggBaseMask then
        begin
          TAggOrderBGR(Pointer(PDst)^).R :=
            PInt8u(PtrComp(PSrc) + From.Order.R)^;
          TAggOrderBGR(Pointer(PDst)^).g :=
            PInt8u(PtrComp(PSrc) + From.Order.g)^;
          TAggOrderBGR(Pointer(PDst)^).b :=
            PInt8u(PtrComp(PSrc) + From.Order.b)^;
        end
        else
          BlendPixBgrPre(PDst, PInt8u(PtrComp(PSrc) + From.Order.R)^,
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

      CopyOrBlendPixBgrPre(PDst, @COLOR, Cover);

      Inc(PtrComp(PSrc), 4);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      3 * Xdst * SizeOf(Int8u));

    repeat
      CopyOrBlendPixBgrPre(PDst, COLOR,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
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
      3 * Xdst * SizeOf(Int8u));

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        BlendPixBgrPre(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgrPre(PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
