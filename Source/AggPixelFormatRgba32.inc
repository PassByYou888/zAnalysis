//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgba(p: PAggOrderRgba; CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
begin
  p^.R := ((CR - p^.R) * alpha + (p^.R shl CAggBaseShift)) shr CAggBaseShift;
  p^.g := ((Cg - p^.g) * alpha + (p^.g shl CAggBaseShift)) shr CAggBaseShift;
  p^.b := ((CB - p^.b) * alpha + (p^.b shl CAggBaseShift)) shr CAggBaseShift;
  p^.A := (alpha + p^.A) - ((alpha * p^.A + CAggBaseMask) shr CAggBaseShift);
end;

procedure CopyOrBlendPixRgba(p: PAggOrderRgba;
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
      BlendPixRgba(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixRgba(p: PAggOrderRgba;
  CR, Cg, CB, alpha, Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixRgba(p, CR, Cg, CB, alpha)
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
      BlendPixRgba(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Rgba32CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  p^.R := C.Rgba8.R;
  p^.g := C.Rgba8.g;
  p^.b := C.Rgba8.b;
  p^.A := C.Rgba8.A;
end;

procedure Rgba32BlendPixel(This: TAggPixelFormatProcessor;
  X, Y: Integer; C: PAggColor; Cover: Int8u);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  CopyOrBlendPixRgba(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

function Rgba32Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  Result.FromRgbaInteger(p^.R, p^.g, p^.b, p^.A);
end;

procedure Rgba32CopyHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  v.R := C.Rgba8.R;
  v.g := C.Rgba8.g;
  v.b := C.Rgba8.b;
  v.A := C.Rgba8.A;

  Fill32Bit(p^, Len, v);
end;

procedure Rgba32CopyVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  v.R := C.Rgba8.R;
  v.g := C.Rgba8.g;
  v.b := C.Rgba8.b;
  v.A := C.Rgba8.A;

  if Len > 0 then
    repeat
      p^ := v;
      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
    Inc(p, X);

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
        BlendPixRgba(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(p);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgba(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        Inc(p);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32BlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
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
        p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixRgba(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgba(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PAggOrderRgba;
  PClr: PAggRgba8;
  alpha: Cardinal;
begin
  PClr := @C^.Rgba8;
  if PClr^.A <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    repeat
      alpha := (PClr^.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.R := PClr^.R;
        p^.g := PClr^.g;
        p^.b := PClr^.b;
        p^.A := CAggBaseMask;
      end
      else
        BlendPixRgba(p, PClr^.R, PClr^.g, PClr^.b, alpha, Covers^);

      Inc(p);
      Inc(Covers);

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PAggOrderRgba;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
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
        BlendPixRgba(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgba(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(Covers);
      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendColorVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgba(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(Covers);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendFrom(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; Psrc_: PInt8u; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PSrc, PDst: PAggOrderRgba;
  Incp: Integer;
begin
  PSrc := PAggOrderRgba(Psrc_);
  PDst := PAggOrderRgba(This.RenderingBuffer.Row(Ydst));
  Inc(PDst, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(PtrComp(PSrc), (Len - 1) shl 2);
    Inc(PtrComp(PDst), (Len - 1) shl 2);

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(PDst, TAggOrderRGBA(PSrc^).R, TAggOrderRGBA(PSrc^).g,
        TAggOrderRGBA(PSrc^).b, TAggOrderRGBA(PSrc^).A);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(PDst, TAggOrderRGBA(PSrc^).R, TAggOrderRGBA(PSrc^).g,
        TAggOrderRGBA(PSrc^).b, TAggOrderRGBA(PSrc^).A, Cover);

      Inc(PtrComp(PSrc), Incp);
      Inc(PtrComp(PDst), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Rgba32CopyColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
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

procedure Rgba32CopyColorVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  repeat
    p^.R := COLORS.Rgba8.R;
    p^.g := COLORS.Rgba8.g;
    p^.b := COLORS.Rgba8.b;
    p^.A := COLORS.Rgba8.A;

    p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Rgba32BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PAggOrderRgba;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderRgba(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(PDst, Xdst);

    repeat
      CopyOrBlendPixRgba(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.A, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), Ppsz);
      Inc(PtrComp(PDst), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendFromLUT(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PAggOrderRgba;

  COLOR: PAggColor;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderRgba(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(PDst, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixRgba(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixRgba(PDst, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A, Cover);

        Inc(PtrComp(PSrc), Ppsz);
        Inc(PtrComp(PDst), 4);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
