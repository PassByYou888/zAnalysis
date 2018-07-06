//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgr(p: PAggOrderBgr; CR, Cg, CB, alpha: Integer;
  Cover: Cardinal = 0);
begin
  p^.R := p^.R + ((CR - p^.R) * alpha) shr CAggBaseShift;
  p^.g := p^.g + ((Cg - p^.g) * alpha) shr CAggBaseShift;
  p^.b := p^.b + ((CB - p^.b) * alpha) shr CAggBaseShift;
end;

procedure CopyOrBlendPixBgr(p: PAggOrderBgr; C: PAggColor; Cover: Cardinal);
  overload;
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
      BlendPixBgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgr(p: PAggOrderBgr; C: PAggColor); overload;
begin
  if C.Rgba8.A <> 0 then
    if C.Rgba8.A = CAggBaseMask then
    begin
      p^.R := C.Rgba8.R;
      p^.g := C.Rgba8.g;
      p^.b := C.Rgba8.b;
    end
    else
      BlendPixBgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, C.Rgba8.A);
end;

procedure Bgr24CopyPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  p^.R := C.Rgba8.R;
  p^.g := C.Rgba8.g;
  p^.b := C.Rgba8.b;
end;

procedure Bgr24BlendPixel(This: TAggPixelFormatProcessor; X, Y: Integer;
  C: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);
  CopyOrBlendPixBgr(p, C, Cover);
end;

function Bgr24Pixel(This: TAggPixelFormatProcessor; X, Y: Integer): TAggColor;
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  Result.FromRgbaInteger(p^.R, p^.g, p^.b);
end;

procedure Bgr24CopyHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  repeat
    p^.R := C.Rgba8.R;
    p^.g := C.Rgba8.g;
    p^.b := C.Rgba8.b;

    Inc(p);
    Dec(Len);
  until Len = 0;
end;

procedure Bgr24CopyVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  repeat
    p^.R := C.Rgba8.R;
    p^.g := C.Rgba8.g;
    p^.b := C.Rgba8.b;

    p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

    Dec(Len);
  until Len = 0;
end;

procedure Bgr24BlendHorizontalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        BlendPixBgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha);

        Inc(PtrComp(p), 3);
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24BlendVerticalLine(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Cover: Int8u);
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
        BlendPixBgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Cover);

        p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));
        Dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixBgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      Inc(PtrComp(p), 3);
      Inc(Covers);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; C: PAggColor; Covers: PInt8u);
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
        BlendPixBgr(p, C.Rgba8.R, C.Rgba8.g, C.Rgba8.b, alpha, Covers^);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Inc(PtrComp(Covers));
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgr(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(Covers, 1);
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgr(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgr(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(PtrComp(p), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24BlendColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgr(p, COLORS, Covers^);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));
      Inc(Covers);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgr(p, COLORS);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgr(p, COLORS, Cover);

      Inc(PtrComp(COLORS), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      Dec(Len);
    until Len = 0;
end;

procedure Bgr24BlendFrom(This, From: TAggPixelFormatProcessor;
  PSrc: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  dest: PAggOrderBgr;
  COLOR: TAggColor;
  alpha: Cardinal;
begin
  dest := PAggOrderBgr(This.RenderingBuffer.Row(Ydst));
  Inc(dest, Xdst);

  if Cover = 255 then
    repeat
      alpha := PInt8u(PtrComp(PSrc) + From.Order.A)^;

      if alpha <> 0 then
        if alpha = CAggBaseMask then
        begin
          dest^.R := PInt8u(PtrComp(PSrc) + From.Order.R)^;
          dest^.g := PInt8u(PtrComp(PSrc) + From.Order.g)^;
          dest^.b := PInt8u(PtrComp(PSrc) + From.Order.b)^;
        end
        else
          BlendPixBgr(dest, PInt8u(PtrComp(PSrc) + From.Order.R)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^, alpha);

      Inc(PtrComp(PSrc), 4);
      Inc(dest);
      Dec(Len);
    until Len = 0
  else
    repeat
      COLOR.FromRgbaInteger(PInt8u(PtrComp(PSrc) + From.Order.R)^,
        PInt8u(PtrComp(PSrc) + From.Order.g)^,
        PInt8u(PtrComp(PSrc) + From.Order.b)^,
        PInt8u(PtrComp(PSrc) + From.Order.A)^);

      CopyOrBlendPixBgr(dest, @COLOR, Cover);

      Inc(PtrComp(PSrc), 4);
      Inc(PtrComp(dest), 3);
      Dec(Len);
    until Len = 0;
end;

procedure Bgr24CopyColorHorizontalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  repeat
    p^.R := COLORS.Rgba8.R;
    p^.g := COLORS.Rgba8.g;
    p^.b := COLORS.Rgba8.b;

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Inc(PtrComp(p), 3);
    Dec(Len);
  until Len = 0;
end;

procedure Bgr24CopyColorVerticalSpan(This: TAggPixelFormatProcessor; X, Y: Integer;
  Len: Cardinal; COLORS: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(Y));
  Inc(p, X);

  repeat
    p^.R := COLORS.Rgba8.R;
    p^.g := COLORS.Rgba8.g;
    p^.b := COLORS.Rgba8.b;

    p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

    Inc(PtrComp(COLORS), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;
end;

procedure Bgr24BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8u;
  dest: PAggOrderBgr;
begin
  PixWidth := From.PixWidth;
  Source := PInt8u(From.GetRowPointer(Ysrc));

  if Source <> nil then
  begin
    dest := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(dest, Xdst);

    repeat
      CopyOrBlendPixBgr(dest, COLOR, ShrInt32(Source^ * Cover +
        CAggBaseMask, CAggBaseShift));

      Inc(PtrComp(Source), PixWidth);
      Inc(PtrComp(dest), 3);
      Dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendFromLut(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8u;
  dest: PAggOrderBgr;
  COLOR: PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := PInt8u(From.GetRowPointer(Ysrc));

  if Source <> nil then
  begin
    dest := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    Inc(dest, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        BlendPixBgr(dest, COLOR.Rgba8.R, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.A);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(dest), 3);
        Dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgr(dest,
          PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor)), Cover);

        Inc(PtrComp(Source), PixWidth);
        Inc(PtrComp(dest), 3);
        Dec(Len);
      until Len = 0;
  end;
end; 
 
