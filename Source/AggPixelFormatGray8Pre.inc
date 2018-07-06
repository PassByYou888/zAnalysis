//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixGrayPre(p: PInt8u; cv, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  p^ := Int8u((p^ * alpha + cv * Cover) shr CAggBaseShift);
end;

procedure BlendPixGrayPre(p: PInt8u; cv, alpha: Cardinal); overload;
begin
  p^ := Int8u(((p^ * (CAggBaseMask - alpha)) shr CAggBaseShift) + cv);
end;

procedure CopyOrBlendPixGrayPre(p: PInt8u; C: PAggColor;
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
      BlendPixGrayPre(p, C.v, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixPre(p: PInt8u; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
      p^ := C.v
    else
      BlendPixGrayPre(p, C.v, C.Rgba8.A);
end;

procedure Gray8PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixGrayPre(PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X *
    This.Step + This.Offset), C, Cover);
end;

procedure Gray8PreBlendHorizontalLine(This: TAggPixelFormatProcessor; X,
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
        BlendPixGrayPre(p, C.v, alpha, Cover);

        Inc(PtrComp(p), This.Step);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8PreBlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixGrayPre(p, C.v, alpha, Cover);

        p := This.RenderingBuffer.NextRow(p);

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8PreBlendSolidHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixGrayPre(p, C.v, alpha, Covers^);

      Inc(PtrComp(p), This.Step);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendSolidVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
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
        BlendPixGrayPre(p, C.v, alpha, Covers^);

      p := This.RenderingBuffer.NextRow(p);

      Inc(PtrComp(Covers), SizeOf(Int8u));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendColorHSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGrayPre(p, COLORS, Covers^);

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
        CopyOrBlendPixPre(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), This.Step);
      Dec(Len);

    until Len = 0
  else
    repeat
      CopyOrBlendPixGrayPre(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), This.Step);
      Dec(Len);
    until Len = 0;
end;

procedure Gray8PreBlendColorVSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGrayPre(p, COLORS, Covers^);

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
        CopyOrBlendPixPre(p, COLORS);

      p := This.RenderingBuffer.NextRow(p);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGrayPre(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := This.RenderingBuffer.NextRow(p);

      Dec(Len);
    until Len = 0;
end;

procedure Gray8PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) + Xdst
      * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGrayPre(PDst, COLOR, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc, PDst: PInt8u;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) + Xdst
      * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGrayPre(PDst,
        PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), This.Step);
      Dec(Len);
    until Len = 0;
  end;
end; 
 
