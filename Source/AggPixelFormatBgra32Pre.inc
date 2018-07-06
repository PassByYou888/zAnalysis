//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  p^.R := Int8u(((p^.R * alpha) shr CAggBaseShift) + CR);
  p^.g := Int8u(((p^.g * alpha) shr CAggBaseShift) + Cg);
  p^.b := Int8u(((p^.b * alpha) shr CAggBaseShift) + CB);
  p^.A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask - p^.A)) shr CAggBaseShift));
end;

procedure BlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB, alpha, Cover: Cardinal);
  overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  p^.R := Int8u((p^.R * alpha + CR * Cover) shr CAggBaseShift);
  p^.g := Int8u((p^.g * alpha + Cg * Cover) shr CAggBaseShift);
  p^.b := Int8u((p^.b * alpha + CB * Cover) shr CAggBaseShift);
  p^.A := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask - p^.A)) shr
    CAggBaseShift));
end;

procedure CopyOrBlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB,
  alpha: Cardinal); inline; overload;
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
      BlendPixBgraPre(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB, alpha,
  Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixBgraPre(p, CR, Cg, CB, alpha)
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
      BlendPixBgraPre(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Bgra32PreBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  CopyOrBlendPixBgraPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A, Cover);
end;

procedure Bgra32PreBlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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

      Fill32Bit(p^, Len, v);
    end
    else if Cover = 255 then
      repeat
        BlendPixBgraPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(p);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgraPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        Inc(p);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32PreBlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
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
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixBgraPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgraPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32PreBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
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
        BlendPixBgraPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(p);
      Inc(PtrComp(Covers));

      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
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
        BlendPixBgraPre(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendColorHorizontalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgraPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(Covers);
      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      Inc(p);
      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendColorVerticalSpan(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgraPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Covers^);

      Inc(Covers);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(p, COLORS.Rgba8.R, COLORS.Rgba8.g, COLORS.Rgba8.b,
        COLORS.Rgba8.A, Cover);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Source, dest: PAggOrderBgra;
  Incp: Integer;
begin
  Source := PAggOrderBgra(SourcePtr);
  dest := PAggOrderBgra(This.RenderingBuffer.Row(Ydst));
  Inc(dest, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    Inc(Source, Len - 1);
    Inc(dest, Len - 1);

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(dest, Source^.R, Source^.g, Source^.b, Source^.A);

      Inc(PtrComp(Source), Incp);
      Inc(PtrComp(dest), Incp);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(dest, Source^.R, Source^.g, Source^.b, Source^.A,
        Cover);

      Inc(PtrComp(Source), Incp);
      Inc(PtrComp(dest), Incp);
      Dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  Source: PInt8u;
  dest: PAggOrderBgra;
begin
  PixWidth := From.PixWidth;
  Source := From.GetRowPointer(Ysrc);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(dest, Xdst);

    repeat
      CopyOrBlendPixBgraPre(dest, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.A, ShrInt32(Source^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Source), PixWidth);
      Inc(PtrComp(dest), 4);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
 PixWidth : Cardinal;

 Source : PInt8u;
 dest : PAggOrderBgra;

 COLOR : PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := From.GetRowPointer(Ysrc);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(dest, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgraPre(dest, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(Source), PixWidth);
        Inc(dest);
        Dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgraPre(dest, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A, Cover);

        Inc(PtrComp(Source), PixWidth);
        Inc(dest);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
