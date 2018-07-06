//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgrGamma(Gamma: TAggGamma; p: PAggOrderBgr;
  CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
var
  R, g, b: Cardinal;
begin
  R := Gamma.dir[TAggOrderBGR(Pointer(p)^).R];
  g := Gamma.dir[TAggOrderBGR(Pointer(p)^).g];
  b := Gamma.dir[TAggOrderBGR(Pointer(p)^).b];

  p^.R := Int8u(Gamma.Inv[(((Integer(Gamma.dir[CR]) - R) * alpha)
    shr CAggBaseShift) + R]);
  p^.g := Int8u(Gamma.Inv[(((Integer(Gamma.dir[Cg]) - g) * alpha)
    shr CAggBaseShift) + g]);
  p^.b := Int8u(Gamma.Inv[(((Integer(Gamma.dir[CB]) - b) * alpha)
    shr CAggBaseShift) + b]);
end;

procedure CopyOrBlendPixBgrGamma(Gamma: TAggGamma; p: PAggOrderBgr;
  C: PAggColor; Cover: Cardinal); overload;
var
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      p^.R := C.Rgba8.R;
      p^.g := C.Rgba8.g;
      p^.b := C.Rgba8.b;
    end
    else
      BlendPixBgrGamma(Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgrGamma(Gamma: TAggGamma; p: PAggOrderBgr;
  C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      p^.R := C.Rgba8.R;
      p^.g := C.Rgba8.g;
      p^.b := C.Rgba8.b;
    end
    else
      BlendPixBgrGamma(Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A);
end;

procedure Bgr24GammaBlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);
  CopyOrBlendPixBgrGamma(This.Gamma, p, C, Cover);
end;

procedure Bgr24GammaBlendHorizontalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        p^.R := C.Rgba8.R;
        p^.g := C.Rgba8.g;
        p^.b := C.Rgba8.b;

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24GammaBlendVerticalLine(This: TAggPixelFormatProcessor;
  X, Y: Integer; Len: Cardinal; C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    alpha := (C.Rgba8.A * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        p^.R := C.Rgba8.R;
        p^.g := C.Rgba8.g;
        p^.b := C.Rgba8.b;

        p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha,
          Cover);

        p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24GammaBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.R := C.Rgba8.R;
        p^.g := C.Rgba8.g;
        p^.b := C.Rgba8.b;
      end
      else
        BlendPixBgrGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b,
          alpha, Covers^);

      Inc(PtrComp(p), 3);
      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; C: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if C.Rgba8.A <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
    Inc(p, X);

    repeat
      alpha := (C.Rgba8.A * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.R := C.Rgba8.R;
        p^.g := C.Rgba8.g;
        p^.b := C.Rgba8.b;
      end
      else
        BlendPixBgrGamma(This.Gamma, p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha,
          Covers^);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(Covers), SizeOf(Int8u));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendColorVerticalSpan(This: TAggPixelFormatProcessor; X,
  Y: Integer; Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(Covers);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PAggOrderBgr;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PSrc := PAggOrderBgr(SourcePtr);
  PDst := PAggOrderBgr(This.RenderingBuffer.Row(Ydst));
  Inc(PDst, Xdst);

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
          BlendPixBgrGamma(This.Gamma, PDst,
            PInt8u(PtrComp(PSrc) + From.Order.R)^,
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

      CopyOrBlendPixBgrGamma(This.Gamma, PDst, @COLOR, Cover);

      Inc(PtrComp(PSrc), 4);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  PSrc: PInt8u;
  PDst: PAggOrderBgr;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(PDst, Xdst);
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, PDst, COLOR,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(PSrc), PixWidth);
      Inc(PtrComp(PDst), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  PSrc: PInt8u;
  PDst: PAggOrderBgr;
  COLOR: PAggColor;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(PDst, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        BlendPixBgrGamma(This.Gamma, PDst, COLOR.Rgba8.R, COLOR.Rgba8.g,
          COLOR.Rgba8.b, COLOR.Rgba8.A);

        Inc(PtrComp(PSrc), PixWidth);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgrGamma(This.Gamma, PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(PSrc), PixWidth);
        Inc(PtrComp(PDst), 3);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
