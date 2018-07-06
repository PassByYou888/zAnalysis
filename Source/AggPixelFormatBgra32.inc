//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgra(p: PAggOrderBgra; CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
begin
  p^.R := ((CR - p^.R) * alpha + (p^.R shl CAggBaseShift)) shr CAggBaseShift;
  p^.g := ((Cg - p^.g) * alpha + (p^.g shl CAggBaseShift)) shr CAggBaseShift;
  p^.b := ((CB - p^.b) * alpha + (p^.b shl CAggBaseShift)) shr CAggBaseShift;
  p^.A := (alpha + p^.A) - ((alpha * p^.A + CAggBaseMask) shr CAggBaseShift);
end;

procedure CopyOrBlendPixBgra(p: PAggOrderBgra;
  CR, Cg, CB, alpha: Cardinal); inline; overload;
begin
  if alpha <> 0 then
    if alpha = CAggBaseMask then
    begin
      p^.R := CR;
      p^.g := Cg;
      p^.b := CB;
      p^.A := CAggBaseMask;
    end
    else
      BlendPixBgra(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixBgra(p: PAggOrderBgra;
  CR, Cg, CB, alpha, Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixBgra(p, CR, Cg, CB, alpha)
  else if alpha <> 0 then
  begin
    alpha := (alpha * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      p^.R := CR;
      p^.g := Cg;
      p^.b := CB;
      p^.A := CAggBaseMask;
    end
    else
      BlendPixBgra(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Bgra32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  p^.R := C.Rgba8.R;
  p^.g := C.Rgba8.g;
  p^.b := C.Rgba8.b;
  p^.A := C.Rgba8.A;
end;

procedure Bgra32BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  CopyOrBlendPixBgra(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

function Bgra32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  Result.FromRgbaInteger(p^.R, p^.g, p^.b, p^.A);
end;

procedure Bgra32CopyHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  v.R := C.Rgba8.R;
  v.g := C.Rgba8.g;
  v.b := C.Rgba8.b;
  v.A := C.Rgba8.A;

  Fill32Bit(p^, Len, v);
end;

procedure Bgra32CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  v.R := C.Rgba8.R;
  v.g := C.Rgba8.g;
  v.b := C.Rgba8.b;
  v.A := C.Rgba8.A;

  if Len > 0 then
    repeat
      p^ := v;
      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendHorizontalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgra(PtrComp(This.RenderingBuffer.Row(Y)) + (X shl 2));

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v.R := C.Rgba8.R;
      v.g := C.Rgba8.g;
      v.b := C.Rgba8.b;
      v.A := C.Rgba8.A;

      Fill32Bit(p^, Len, v);
    end
    else if Cover = 255 then
      repeat
        BlendPixBgra(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(p);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgra(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        Inc(p);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32BlendVerticalLine(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v.R := C.Rgba8.R;
      v.g := C.Rgba8.g;
      v.b := C.Rgba8.b;
      v.A := C.Rgba8.A;

      repeat
        p^ := v;
        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixBgra(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgra(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgra;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.R := C.Rgba8.R;
        p^.g := C.Rgba8.g;
        p^.b := C.Rgba8.b;
        p^.A := CAggBaseMask;
      end
      else
        BlendPixBgra(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(p);
      Inc(Covers);

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgra;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.R := C.Rgba8.R;
        p^.g := C.Rgba8.g;
        p^.b := C.Rgba8.b;
        p^.A := CAggBaseMask;
      end
      else
        BlendPixBgra(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgra(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(Covers);
      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgra(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(Covers);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PAggOrderBgra;
  Incp: Integer;
begin
  PSrc := PAggOrderBgra(SourcePtr);
  PDst := PAggOrderBgra(This.RenderingBuffer.Row(Ydst));
  Inc(PDst, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(PSrc), ((Len - 1) shl 2));
    Inc(PtrComp(PDst), ((Len - 1) shl 2));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(PDst, PSrc^.R, PSrc^.g, PSrc^.b, PSrc^.A);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(PDst, PSrc^.R, PSrc^.g, PSrc^.b, PSrc^.A, Cover);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32CopyColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  repeat
    p^.R := COLORS.Rgba8.R;
    p^.g := COLORS.Rgba8.g;
    p^.b := COLORS.Rgba8.b;
    p^.A := COLORS.Rgba8.A;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Inc(p);
    Dec(Len);
  until Len = 0;
end;

procedure Bgra32CopyColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  repeat
    p^.R := COLORS.Rgba8.R;
    p^.g := COLORS.Rgba8.g;
    p^.b := COLORS.Rgba8.b;
    p^.A := COLORS.Rgba8.A;

    p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Bgra32BlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8u;
  dest: PAggOrderBgra;
begin
  PixWidth := From.PixWidth;
  Source := Pointer(PtrComp(From.GetRowPointer(Ysrc)) + Xsrc * PixWidth);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(dest, Xdst);

    repeat
      CopyOrBlendPixBgra(dest, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.A, ShrInt32(Source^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Source), PixWidth);
      Inc(dest);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  Source: PInt8u;
  dest: PAggOrderBgra;

  COLOR: PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := Pointer(PtrComp(From.GetRowPointer(Ysrc)) + Xsrc * PixWidth);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(dest, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgra(dest, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(dest), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgra(dest, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A, Cover);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(dest), 4);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
