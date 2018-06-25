function ClampInt(const Value, Min, Max: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if Value > Max then
      Result := Max
  else if Value < Min then
      Result := Min
  else
      Result := Value;
end;

function ClampByte(const Value, Min, Max: byte): byte; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
begin
  if Value > Max then
      Result := Max
  else if Value < Min then
      Result := Min
  else
      Result := Value;
end;

function ClampByte(const Value: Integer): byte; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
begin
  if Value > 255 then
      Result := 255
  else if Value < 0 then
      Result := 0
  else
      Result := Value;
end;

function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if R1.Left >= R2.Left then
      Dst.Left := R1.Left
  else
      Dst.Left := R2.Left;
  if R1.Right <= R2.Right then
      Dst.Right := R1.Right
  else
      Dst.Right := R2.Right;
  if R1.Top >= R2.Top then
      Dst.Top := R1.Top
  else
      Dst.Top := R2.Top;
  if R1.Bottom <= R2.Bottom then
      Dst.Bottom := R1.Bottom
  else
      Dst.Bottom := R2.Bottom;
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then
      Dst := ZERO_RECT;
end;

procedure OffsetRect(var R: TRect; Dx, Dy: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Inc(R.Left, Dx);
  Inc(R.Top, Dy);
  Inc(R.Right, Dx);
  Inc(R.Bottom, Dy);
end;

function IsRectEmpty(const R: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

procedure MakeMergeTables;
var
  i, j: Integer;
const
  OneByteth: Double = 1.0 / 255.0;
begin
  for j := 0 to 255 do
    for i := 0 to 255 do
      begin
        DivTable[i, j] := Round(i * j * OneByteth);
        if i > 0 then
            RcTable[i, j] := byte(Round(j * 255 / i))
        else
            RcTable[i, j] := 0;
      end;
end;

function TMemoryRaster.GetVertex: TVertexMap;
begin
  if FVertex = nil then
      OpenVertex;

  Result := FVertex;
end;

function TMemoryRaster.GetFont: TFontRaster;
begin
  if FFont = nil then
      OpenFont;

  Result := FFont;
end;

procedure TMemoryRaster.SetFont(f: TFontRaster);
begin
  CloseFont;
  FFont := TFontRaster.Create(f);
end;

function TMemoryRaster.GetAggImage: TMemoryRaster_AggImage;
begin
  if FAggImage = nil then
      OpenAgg;
  Result := FAggImage;
end;

function TMemoryRaster.GetAgg: TMemoryRaster_Agg2D;
begin
  if FAgg = nil then
      OpenAgg;
  Result := FAgg;
end;

procedure TMemoryRaster.FreeAgg;
begin
  if Assigned(FAggImage) then
    begin
      DisposeObject(FAggImage);
      FAggImage := nil;
    end;

  if Assigned(FAgg) then
    begin
      DisposeObject(FAgg);
      FAgg := nil;
    end;
end;

function TMemoryRaster.GetPixel(const X, Y: Integer): TRasterColor;
begin
  Result := PixelPtr[X, Y]^;
end;

procedure TMemoryRaster.SetPixel(const X, Y: Integer; const Value: TRasterColor);
begin
  PixelPtr[X, Y]^ := Value;
end;

function TMemoryRaster.GetPixelBGRA(const X, Y: Integer): TRasterColor;
begin
  Result := RGBA2BGRA(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetPixelBGRA(const X, Y: Integer; const Value: TRasterColor);
begin
  SetPixel(X, Y, BGRA2RGBA(Value));
end;

function TMemoryRaster.GetPixelPtr(const X, Y: Integer): PRasterColor;
begin
  Result := @(FBits^[ClampInt(X, 0, Width - 1) + ClampInt(Y, 0, Height - 1) * Width]);
end;

function TMemoryRaster.GetScanLine(Y: Integer): PRasterColorArray;
begin
  Result := @(FBits^[Y * FWidth]);
end;

function TMemoryRaster.GetPixelRed(const X, Y: Integer): byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(X, Y))^.R;
end;

procedure TMemoryRaster.SetPixelRed(const X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.R := Value;
end;

function TMemoryRaster.GetPixelGreen(const X, Y: Integer): byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(X, Y))^.G;
end;

procedure TMemoryRaster.SetPixelGreen(const X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.G := Value;
end;

function TMemoryRaster.GetPixelBlue(const X, Y: Integer): byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(X, Y))^.B;
end;

procedure TMemoryRaster.SetPixelBlue(const X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.B := Value;
end;

function TMemoryRaster.GetPixelAlpha(const X, Y: Integer): byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(X, Y))^.A;
end;

procedure TMemoryRaster.SetPixelAlpha(const X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.A := Value;
end;

function TMemoryRaster.GetGray(const X, Y: Integer): byte;
begin
  Result := RasterColor2Gray(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGray(const X, Y: Integer; const Value: byte);
begin
  SetPixel(X, Y, RasterColor(Value, Value, Value, 255));
end;

function TMemoryRaster.GetGrayS(const X, Y: Integer): Single;
begin
  Result := RasterColor2GrayS(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGrayS(const X, Y: Integer; const Value: Single);
begin
  SetGray(X, Y, ClampByte(Round(Value * $FF), 0, $FF));
end;

function TMemoryRaster.GetGrayD(const X, Y: Integer): Double;
begin
  Result := RasterColor2GrayD(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGrayD(const X, Y: Integer; const Value: Double);
begin
  SetGrayS(X, Y, Value);
end;

function TMemoryRaster.GetPixelF(const X, Y: TGeoFloat): TRasterColor;
begin
  Result := GetPixel(Round(X), Round(Y));
end;

procedure TMemoryRaster.SetPixelF(const X, Y: TGeoFloat; const Value: TRasterColor);
begin
  SetPixel(Round(X), Round(Y), Value);
end;

function TMemoryRaster.GetPixelVec(const v2: TVec2): TRasterColor;
begin
  Result := GetPixelF(v2[0], v2[1]);
end;

procedure TMemoryRaster.SetPixelVec(const v2: TVec2; const Value: TRasterColor);
begin
  SetPixelF(v2[0], v2[1], Value)
end;

function TMemoryRaster.GetPixelWrapLinear(const X, Y: TGeoFloat): TRasterColor;
  function CoordWrap(const f: TGeoFloat): TGeoFloat; inline;
  begin
    if f < 0 then
        Result := 1 - trunc(f) + f
    else if f > 1 then
        Result := f - trunc(f)
    else
        Result := f;
  end;
  procedure ClampF(var f: TGeoFloat); inline;
  begin
    if f < 0 then
        f := 0
    else if f > 1 then
        f := 1;
  end;

var
  fx, fy: TGeoFloat;
  i_x, i_y: Integer;
  i_x2, i_y2: Integer;
  delta_x, delta_y: TGeoFloat;
  C1, C2, C3, C4: TRasterColorEntry;
  k1, k2, k3, k4: TGeoFloat;
  R, G, B, A: TGeoFloat;
begin
  fx := CoordWrap(X) * Width;
  fy := CoordWrap(Y) * Height;

  i_x := ClampInt(trunc(fx), 0, Width - 1);
  i_y := ClampInt(trunc(fy), 0, Height - 1);

  i_x2 := i_x + 1;
  if i_x2 > Width - 1 then
      i_x2 := 0;
  i_y2 := i_y + 1;
  if i_y2 > Height - 1 then
      i_y2 := 0;

  delta_x := Frac(fx);
  delta_y := Frac(fy);

  k1 := (1 - delta_x) * (1 - delta_y);
  k2 := delta_x * (1 - delta_y);
  k3 := delta_x * delta_y;
  k4 := (1 - delta_x) * delta_y;

  C1.RGBA := Pixel[i_x, i_y];
  C2.RGBA := Pixel[i_x2, i_y];
  C3.RGBA := Pixel[i_x2, i_y2];
  C4.RGBA := Pixel[i_x, i_y2];

  R := ((C1.R / $FF) * k1);
  R := R + ((C2.R / $FF) * k2);
  R := R + ((C3.R / $FF) * k3);
  R := R + ((C4.R / $FF) * k4);

  G := ((C1.G / $FF) * k1);
  G := G + ((C2.G / $FF) * k2);
  G := G + ((C3.G / $FF) * k3);
  G := G + ((C4.G / $FF) * k4);

  B := ((C1.B / $FF) * k1);
  B := B + ((C2.B / $FF) * k2);
  B := B + ((C3.B / $FF) * k3);
  B := B + ((C4.B / $FF) * k4);

  A := ((C1.A / $FF) * k1);
  A := A + ((C2.A / $FF) * k2);
  A := A + ((C3.A / $FF) * k3);
  A := A + ((C4.A / $FF) * k4);

  ClampF(R);
  ClampF(G);
  ClampF(B);
  ClampF(A);

  TRasterColorEntry(Result).R := Round(R * 255);
  TRasterColorEntry(Result).G := Round(G * 255);
  TRasterColorEntry(Result).B := Round(B * 255);
  TRasterColorEntry(Result).A := Round(A * 255);
end;

constructor TMemoryRaster.Create;
begin
  inherited Create;
  FFreeBits := False;
  FBits := nil;
  FWidth := 0;
  FHeight := 0;
  FOuterColor := $00000000; // by default as full transparency black

  FMasterAlpha := $FF;
  FDrawMode := dmBlend;
  FCombineMode := cmBlend;

  FVertex := nil;
  FFont := nil;

  FAggImage := nil;
  FAgg := nil;
  FAggNeed := False;

  FUserObject := nil;
  FUserData := nil;
  FUserText := '';
end;

destructor TMemoryRaster.Destroy;
begin
  Reset;
  inherited Destroy;
end;

procedure TMemoryRaster.SetWorkMemory(WorkMemory: Pointer; NewWidth, NewHeight: Integer);
begin
  CloseVertex;
  FreeAgg;

  if Assigned(FBits) and FFreeBits then
      FreeMem(FBits);

  if WorkMemory = nil then
    begin
      FFreeBits := True;
      FBits := nil;
      FWidth := NewWidth;
      FHeight := NewHeight;
    end
  else
    begin
      FFreeBits := False;
      FBits := PRasterColorArray(WorkMemory);
      FWidth := NewWidth;
      FHeight := NewHeight;
    end;

  if FAggNeed then
      OpenAgg;
end;

procedure TMemoryRaster.SetWorkMemory(Raster: TMemoryRaster);
begin
  SetWorkMemory(@Raster.FBits[0], Raster.Width, Raster.Height);
end;

procedure TMemoryRaster.OpenVertex;
begin
  if FVertex = nil then
      FVertex := TVertexMap.Create(Self);
end;

procedure TMemoryRaster.CloseVertex;
begin
  if FVertex <> nil then
    begin
      DisposeObject(FVertex);
      FVertex := nil;
    end;
end;

procedure TMemoryRaster.OpenFont;
begin
  if FFont = nil then
      FFont := TFontRaster.Create(SystemFont);
end;

procedure TMemoryRaster.CloseFont;
begin
  if FFont <> nil then
    begin
      DisposeObject(FFont);
      FFont := nil;
    end;
end;

procedure TMemoryRaster.OpenAgg;
begin
  if Empty then
    begin
      FAggNeed := True;
      exit;
    end;
  if FAggImage = nil then
      FAggImage := TMemoryRaster_AggImage.Create(Self);

  if FAgg = nil then
    begin
      FAgg := TMemoryRaster_Agg2D.Create(TAggPixelFormat.pfBGRA);
      FAgg.Attach(Self);
    end;
end;

procedure TMemoryRaster.CloseAgg;
begin
  FreeAgg;
  FAggNeed := False;
end;

procedure TMemoryRaster.Clear;
begin
  Clear($FF000000);
end;

procedure TMemoryRaster.Clear(FillColor: TRasterColor);
begin
  if Empty then
      exit;
  FillRasterColor(Bits^[0], Width * Height, FillColor);
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer);
begin
  if (NewWidth = FWidth) and (NewHeight = FHeight) and (Assigned(FBits)) then
      exit;

  if Assigned(FBits) then
    begin
      if FFreeBits then
          FreeMem(FBits);
      FBits := nil;
    end;

  CloseVertex;
  FreeAgg;

  FFreeBits := True;
  GetMem(FBits, NewWidth * NewHeight * SizeOf(TRasterColor));
  FWidth := NewWidth;
  FHeight := NewHeight;

  if FAggNeed then
      OpenAgg;
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor);
begin
  SetSize(NewWidth, NewHeight);
  FillRasterColor(FBits^[0], NewWidth * NewHeight, ClearColor);
end;

function TMemoryRaster.SizeOfPoint: TPoint;
begin
  Result := Point(Width, Height);
end;

function TMemoryRaster.SizeOf2DPoint: TVec2;
begin
  Result := Make2DPoint(Width, Height);
end;

function TMemoryRaster.Size2D: TVec2;
begin
  Result := SizeOf2DPoint;
end;

function TMemoryRaster.Empty: Boolean;
begin
  Result := (FBits = nil) or (FWidth <= 0) or (FHeight <= 0);
end;

function TMemoryRaster.BoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TMemoryRaster.BoundsRectV2: TRectV2;
begin
  Result := MakeRectV2(0, 0, Width, Height);
end;

procedure TMemoryRaster.Reset;
begin
  if Assigned(FBits) then
    begin
      if FFreeBits then
          FreeMem(FBits);
      FBits := nil;
    end;

  if FFont <> nil then
    begin
      DisposeObject(FFont);
      FFont := nil;
    end;

  CloseVertex;
  FreeAgg;

  FFreeBits := False;
  FWidth := 0;
  FHeight := 0;
end;

procedure TMemoryRaster.Assign(sour: TMemoryRaster);
begin
  Reset;
  FWidth := sour.FWidth;
  FHeight := sour.FHeight;

  FDrawMode := sour.FDrawMode;
  FCombineMode := sour.FCombineMode;

  FMasterAlpha := sour.FMasterAlpha;
  FOuterColor := sour.FOuterColor;

  FFreeBits := True;
  GetMem(FBits, sour.FWidth * sour.FHeight * SizeOf(TRasterColor));
  CopyRasterColor(sour.FBits^[0], FBits^[0], sour.FWidth * sour.FHeight);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.FlipHorz;
var
  i, j: Integer;
  p1, p2: PRasterColor;
  tmp: TRasterColor;
  w, W2: Integer;
begin
  w := Width;
  { In-place flipping }
  p1 := PRasterColor(Bits);
  p2 := p1;
  Inc(p2, Width - 1);
  W2 := Width shr 1;
  for j := 0 to Height - 1 do
    begin
      for i := 0 to W2 - 1 do
        begin
          tmp := p1^;
          p1^ := p2^;
          p2^ := tmp;
          Inc(p1);
          Dec(p2);
        end;
      Inc(p1, w - W2);
      Inc(p2, w + W2);
    end;

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.FlipVert;
var
  j, J2: Integer;
  Buffer: PRasterColorArray;
  p1, p2: PRasterColor;
begin
  { in-place }
  J2 := Height - 1;
  GetMem(Buffer, Width shl 2);
  for j := 0 to Height div 2 - 1 do
    begin
      p1 := PixelPtr[0, j];
      p2 := PixelPtr[0, J2];
      CopyRasterColor(p1^, Buffer^, Width);
      CopyRasterColor(p2^, p1^, Width);
      CopyRasterColor(Buffer^, p2^, Width);
      Dec(J2);
    end;
  FreeMem(Buffer);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Rotate90;
var
  tmp: TMemoryRaster;
  X, Y, i, j: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  i := 0;
  for Y := 0 to Height - 1 do
    begin
      j := Height - 1 - Y;
      for X := 0 to Width - 1 do
        begin
          tmp.Bits^[j] := Bits^[i];
          Inc(i);
          Inc(j, Height);
        end;
    end;

  Reset;
  FWidth := tmp.FWidth;
  FHeight := tmp.FHeight;
  FBits := tmp.FBits;

  tmp.FWidth := 0;
  tmp.FHeight := 0;
  tmp.FBits := nil;
  DisposeObject(tmp);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Rotate180;
var
  i, I2: Integer;
  tmp: TRasterColor;
begin
  I2 := Width * Height - 1;
  for i := 0 to Width * Height div 2 - 1 do
    begin
      tmp := Bits^[I2];
      Bits^[I2] := Bits^[i];
      Bits^[i] := tmp;
      Dec(I2);
    end;

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Rotate270;
var
  tmp: TMemoryRaster;
  X, Y, i, j: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  i := 0;
  for Y := 0 to Height - 1 do
    begin
      j := (Width - 1) * Height + Y;
      for X := 0 to Width - 1 do
        begin
          tmp.Bits^[j] := Bits^[i];
          Inc(i);
          Dec(j, Height);
        end;
    end;

  Reset;
  FWidth := tmp.FWidth;
  FHeight := tmp.FHeight;
  FBits := tmp.FBits;

  tmp.FWidth := 0;
  tmp.FHeight := 0;
  tmp.FBits := nil;
  DisposeObject(tmp);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.NoLineZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
var
  j: Integer;
  SourceI, SourceJ: Double;
  SourceIInt, SourceJInt: Integer;
  SourceINext, SourceJNext: Integer;
begin
  for j := 0 to dest.Height - 1 do
    begin
      SourceI := (pass / (dest.Width - 1)) * (Source.Width - 1);
      SourceJ := (j / (dest.Height - 1)) * (Source.Height - 1);

      SourceIInt := trunc(SourceI);
      SourceJInt := trunc(SourceJ);

      dest.Pixel[pass, j] := Source.Pixel[SourceIInt, SourceJInt]
    end;
end;

procedure TMemoryRaster.NoLineZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    NoLineZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}


var
  i: Integer;

begin
  SetSize(NewWidth, NewHeight);

  if (Source.Width > 1) and (Source.Width > 1) and (Width > 1) and (Height > 1) then
    begin
{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Width - 1);
{$ELSE}
      TParallel.For(0, Width - 1, procedure(pass: Integer)
        begin
          NoLineZoomLine(Source, Self, pass);
        end);
{$ENDIF FPC}
{$ELSE}
      for i := Width - 1 downto 0 do
          NoLineZoomLine(Source, Self, i);
{$ENDIF parallel}
    end;
end;

procedure TMemoryRaster.NoLineZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.NoLineZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.ZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
var
  j: Integer;
  SourceI, SourceJ: Double;
begin
  for j := 0 to dest.Height - 1 do
    begin
      SourceI := (pass / (dest.Width - 1));
      SourceJ := (j / (dest.Height - 1));

      dest.Pixel[pass, j] := Source.PixelWrapLinear[SourceI, SourceJ];
    end;
end;

procedure TMemoryRaster.ZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}


var
  i: Integer;

begin
  SetSize(NewWidth, NewHeight);

  if (Source.Width > 1) and (Source.Width > 1) and (Width > 1) and (Height > 1) then
    begin
{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Width - 1);
{$ELSE}
      TParallel.For(0, Width - 1, procedure(pass: Integer)
        begin
          ZoomLine(Source, Self, pass);
        end);
{$ENDIF FPC}
{$ELSE}
      for i := Width - 1 downto 0 do
          ZoomLine(Source, Self, i);
{$ENDIF parallel}
    end;
end;

procedure TMemoryRaster.Zoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.ZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.FastBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      FastBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      // zoom
      n := TMemoryRaster.Create;
      // preprocess zoom
      FastBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.FastBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.FastBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.GaussianBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GaussianBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GaussianBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.GaussianBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.GaussianBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.GrayscaleBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GrayscaleBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GrayscaleBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.GrayscaleBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.GrayscaleBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

function TMemoryRaster.FormatAsBGRA: TMemoryRaster;
var
  dest: TMemoryRaster;
  pass: Integer;
begin
  dest := TMemoryRaster.Create;
  dest.FFreeBits := True;
  GetMem(dest.FBits, Width * Height * SizeOf(TRasterColor));
  dest.FWidth := Width;
  dest.FHeight := Height;

  for pass := 0 to (Width * Height) - 1 do
      dest.FBits^[pass] := RGBA2BGRA(FBits^[pass]);

  Result := dest;
end;

procedure TMemoryRaster.FormatBGRA;
var
  pass: Integer;
begin
  for pass := 0 to (Width * Height) - 1 do
    with TRasterColorEntry(FBits^[pass]) do
        Swap(R, B);
end;

procedure TMemoryRaster.ColorTransparent(c: TRasterColor);
var
  i, j: Integer;
  A: byte;
  ce: TRasterColorEntry;
begin
  ce.RGBA := c;
  A := ce.A;
  for i := 0 to Width - 1 do
    for j := 0 to Height - 1 do
      begin
        ce.RGBA := Pixel[i, j];
        ce.A := A;
        if ce.RGBA = c then
            Pixel[i, j] := RasterColor(0, 0, 0, 0);
      end;
end;

procedure TMemoryRaster.ColorBlend(c: TRasterColor);
var
  i, j: Integer;
begin
  for i := 0 to Width - 1 do
    for j := 0 to Height - 1 do
        Pixel[i, j] := BlendReg(Pixel[i, j], c);
end;

procedure TMemoryRaster.Grayscale;
begin
  RGBToGrayscale(Self);
end;

procedure TMemoryRaster.TransformToGrayRaster(var Output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        Output[j, i] := PixelGray[i, j];
end;

procedure TMemoryRaster.TransformToRedRaster(var Output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        Output[j, i] := PixelRed[i, j];
end;

procedure TMemoryRaster.TransformToGreenRaster(var Output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        Output[j, i] := PixelGreen[i, j];
end;

procedure TMemoryRaster.TransformToBlueRaster(var Output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        Output[j, i] := PixelBlue[i, j];
end;

procedure TMemoryRaster.TransformToAlphaRaster(var Output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        Output[j, i] := PixelAlpha[i, j];
end;

procedure TMemoryRaster.Line(X1, Y1, X2, Y2: Integer; Value: TRasterColor; L: Boolean);
  procedure VertLine(X, Y1, Y2: Integer);
  var
    i, NH, NL: Integer;
    p: PRasterColor;
  begin
    if (X < 0) or (X >= Width) then
        exit;
    Y1 := ClampInt(Y1, 0, Height);
    Y2 := ClampInt(Y2, 0, Height);

    if Y2 < Y1 then
        Swap(Y1, Y2);

    p := PixelPtr[X, Y1];
    i := Y2 - Y1 + 1;
    NH := i shr 2;
    NL := i and $03;
    for i := 0 to NH - 1 do
      begin
        BlendMem(Value, p^);
        Inc(p, Width);
        BlendMem(Value, p^);
        Inc(p, Width);
        BlendMem(Value, p^);
        Inc(p, Width);
        BlendMem(Value, p^);
        Inc(p, Width);
      end;
    for i := 0 to NL - 1 do
      begin
        BlendMem(Value, p^);
        Inc(p, Width);
      end;
  end;

  procedure HorzLine(X1, Y, X2: Integer);
  var
    i: Integer;
  begin
    if (Y < 0) or (Y >= Height) then
        exit;
    X1 := ClampInt(X1, 0, Width - 1);
    X2 := ClampInt(X2, 0, Width - 1);

    if X1 > X2 then
        Swap(X1, X2);

    for i := X1 to X2 do
        BlendMem(Value, PixelPtr[i, Y]^);
  end;

var
  Dy, Dx, Sy, Sx, i, Delta: Integer;
  pi, pl: Integer;
begin
  if FAgg <> nil then
    begin
      FAgg.LineColor := Value;
      FAgg.Line(X1, Y1, X2, Y2);
      exit;
    end;

  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then
        Sx := 1
    else if Dx < 0 then
      begin
        Dx := -Dx;
        Sx := -1;
      end
    else // Dx = 0
      begin
        if Dy > 0 then
            VertLine(X1, Y1, Y2 - 1)
        else if Dy < 0 then
            VertLine(X1, Y2 + 1, Y1);
        if L then
            Pixel[X2, Y2] := Value;
        exit;
      end;

    if Dy > 0 then
        Sy := 1
    else if Dy < 0 then
      begin
        Dy := -Dy;
        Sy := -1;
      end
    else // Dy = 0
      begin
        if X2 > X1 then
            HorzLine(X1, Y1, X2 - 1)
        else
            HorzLine(X2 + 1, Y1, X1);
        if L then
            Pixel[X2, Y2] := Value;
        exit;
      end;

    pi := X1 + Y1 * Width;
    Sy := Sy * Width;
    pl := Width * Height;

    if Dx > Dy then
      begin
        Delta := Dx shr 1;
        for i := 0 to Dx - 1 do
          begin
            if (pi >= 0) and (pi < pl) then
                BlendMem(Value, FBits^[pi]);

            Inc(pi, Sx);
            Inc(Delta, Dy);
            if Delta >= Dx then
              begin
                Inc(pi, Sy);
                Dec(Delta, Dx);
              end;
          end;
      end
    else // Dx < Dy
      begin
        Delta := Dy shr 1;
        for i := 0 to Dy - 1 do
          begin
            if (pi >= 0) and (pi < pl) then
                BlendMem(Value, FBits^[pi]);

            Inc(pi, Sy);
            Inc(Delta, Dx);
            if Delta >= Dy then
              begin
                Inc(pi, Sx);
                Dec(Delta, Dy);
              end;
          end;
      end;
    if (L) and (pi >= 0) and (pi < pl) then
        BlendMem(Value, FBits^[pi]);
  except
  end;
end;

procedure TMemoryRaster.LineF(X1, Y1, X2, Y2: TGeoFloat; Value: TRasterColor; L: Boolean);
begin
  Line(Round(X1), Round(Y1), Round(X2), Round(Y2), Value, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Value: TRasterColor; L: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Value, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Value: TRasterColor; L, Cross: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Value, L);
  if not Cross then
      exit;
  DrawCrossF(p1, Round((FWidth + FHeight) div 2 * 0.02), RasterColorInv(Value));
  DrawCrossF(p2, Round((FWidth + FHeight) div 2 * 0.02), RasterColorInv(Value));
end;

procedure TMemoryRaster.FillRect(X1, Y1, X2, Y2: Integer; Value: TRasterColor);
var
  j, i: Integer;
  p: PRasterColor;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Value;
      FAgg.NoLine;
      FAgg.Rectangle(X1, Y1, X2, Y2);
    end
  else
    begin
      FixRect(X1, Y1, X2, Y2);

      if X1 < 0 then
          X1 := 0
      else if X1 >= Width then
          X1 := Width - 1;

      if X2 < 0 then
          X2 := 0
      else if X2 >= Width then
          X2 := Width - 1;

      if Y1 < 0 then
          Y1 := 0
      else if Y1 >= Height then
          Y1 := Height - 1;
      if Y2 < 0 then
          Y2 := 0
      else if Y2 >= Height then
          Y2 := Height - 1;

      for j := Y1 to Y2 - 1 do
        begin
          i := X1;
          p := @ScanLine[j]^[i];
          while i < X2 do
            begin
              BlendMem(Value, p^);
              Inc(i);
              Inc(p);
            end;
        end;
    end;
end;

procedure TMemoryRaster.FillRect(DstX, DstY, LineDist: Integer; Value: TRasterColor);
var
  l2, X1, Y1, X2, Y2: Integer;
begin
  l2 := LineDist div 2;
  X1 := DstX - l2;
  Y1 := DstY - l2;
  X2 := DstX + l2;
  Y2 := DstY + l2;
  FillRect(X1, Y1, X2, Y2, Value);
end;

procedure TMemoryRaster.FillRect(Dst: TVec2; LineDist: Integer; Value: TRasterColor);
begin
  FillRect(Round(Dst[0]), Round(Dst[1]), LineDist, Value);
end;

procedure TMemoryRaster.FillRect(R: TRectV2; Value: TRasterColor);
begin
  FillRect(Round(R[0, 0]), Round(R[0, 1]), Round(R[1, 0]), Round(R[1, 1]), Value);
end;

procedure TMemoryRaster.FillRect(R: TRectV2; Angle: TGeoFloat; Value: TRasterColor);
var
  A: TGeoFloat;
  r4: TV2Rect4;
  buff: array [0 .. 4] of TPointDouble;
begin
  A := NormalizeDegAngle(Angle);
  if A = 0 then
      FillRect(R, Value)
  else
    begin
      r4 := TV2Rect4.Init(R, A);
      if FAgg <> nil then
        begin
          buff[0].X := r4.LeftTop[0];
          buff[0].Y := r4.LeftTop[1];
          buff[1].X := r4.RightTop[0];
          buff[1].Y := r4.RightTop[1];
          buff[2].X := r4.RightBottom[0];
          buff[2].Y := r4.RightBottom[1];
          buff[3].X := r4.LeftBottom[0];
          buff[3].Y := r4.LeftBottom[1];
          buff[4].X := r4.LeftTop[0];
          buff[4].Y := r4.LeftTop[1];

          FAgg.FillColor := Value;
          FAgg.NoLine;
          FAgg.Polygon(@buff[0], 5);
        end
      else
          ProjectionColor(r4, Value);
    end;
end;

procedure TMemoryRaster.DrawRect(R: TRect; Value: TRasterColor);
begin
  DrawRect(RectV2(R), Value);
end;

procedure TMemoryRaster.DrawRect(R: TRectV2; Value: TRasterColor);
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Rectangle(R[0][0], R[0][1], R[1][0], R[1][1]);
    end
  else
    begin
      LineF(Vec2(R[0][0], R[0][1]), Vec2(R[1][0], R[0][1]), Value, True);
      LineF(Vec2(R[1][0], R[0][1]), Vec2(R[1][0], R[1][1]), Value, True);
      LineF(Vec2(R[1][0], R[1][1]), Vec2(R[0][0], R[1][1]), Value, True);
      LineF(Vec2(R[0][0], R[1][1]), Vec2(R[0][0], R[0][1]), Value, True);
    end;
end;

procedure TMemoryRaster.DrawRect(R: TV2Rect4; Value: TRasterColor);
var
  buff: array [0 .. 4] of TPointDouble;
begin
  if FAgg <> nil then
    begin
      buff[0].X := R.LeftTop[0];
      buff[0].Y := R.LeftTop[1];
      buff[1].X := R.RightTop[0];
      buff[1].Y := R.RightTop[1];
      buff[2].X := R.RightBottom[0];
      buff[2].Y := R.RightBottom[1];
      buff[3].X := R.LeftBottom[0];
      buff[3].Y := R.LeftBottom[1];
      buff[4].X := R.LeftTop[0];
      buff[4].Y := R.LeftTop[1];

      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Polyline(@buff[0], 4);
    end
  else
    begin
      LineF(R.LeftTop, R.RightTop, Value, True);
      LineF(R.RightTop, R.RightBottom, Value, True);
      LineF(R.RightBottom, R.LeftBottom, Value, True);
      LineF(R.LeftBottom, R.LeftTop, Value, True);
    end;
end;

procedure TMemoryRaster.DrawRect(R: TRectV2; Angle: TGeoFloat; Value: TRasterColor);
var
  r4: TV2Rect4;
  buff: array [0 .. 4] of TPointDouble;
begin
  r4 := TV2Rect4.Init(R, Angle);
  if FAgg <> nil then
    begin
      buff[0].X := r4.LeftTop[0];
      buff[0].Y := r4.LeftTop[1];
      buff[1].X := r4.RightTop[0];
      buff[1].Y := r4.RightTop[1];
      buff[2].X := r4.RightBottom[0];
      buff[2].Y := r4.RightBottom[1];
      buff[3].X := r4.LeftBottom[0];
      buff[3].Y := r4.LeftBottom[1];
      buff[4].X := r4.LeftTop[0];
      buff[4].Y := r4.LeftTop[1];

      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Polyline(@buff[0], 5);
    end
  else
    begin
      LineF(r4.LeftTop, r4.RightTop, Value, True);
      LineF(r4.RightTop, r4.RightBottom, Value, True);
      LineF(r4.RightBottom, r4.LeftBottom, Value, True);
      LineF(r4.LeftBottom, r4.LeftTop, Value, True);
    end;
end;

procedure TMemoryRaster.DrawTriangle_Render(t: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
begin
  if Transform then
    begin
      LineF(Vec2Mul(t[0].Render, Size2D), Vec2Mul(t[1].Render, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[1].Render, Size2D), Vec2Mul(t[2].Render, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[2].Render, Size2D), Vec2Mul(t[0].Render, Size2D), Value, True, Cross);
    end
  else
    begin
      LineF(t[0].Render, t[1].Render, Value, True, Cross);
      LineF(t[1].Render, t[2].Render, Value, True, Cross);
      LineF(t[2].Render, t[0].Render, Value, True, Cross);
    end;
end;

procedure TMemoryRaster.DrawTriangle_Sampler(t: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
begin
  if Transform then
    begin
      LineF(Vec2Mul(t[0].Sampler, Size2D), Vec2Mul(t[1].Sampler, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[1].Sampler, Size2D), Vec2Mul(t[2].Sampler, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[2].Sampler, Size2D), Vec2Mul(t[0].Sampler, Size2D), Value, True, Cross);
    end
  else
    begin
      LineF(t[0].Sampler, t[1].Sampler, Value, True, Cross);
      LineF(t[1].Sampler, t[2].Sampler, Value, True, Cross);
      LineF(t[2].Sampler, t[0].Sampler, Value, True, Cross);
    end;
end;

procedure TMemoryRaster.DrawCross(DstX, DstY, LineDist: Integer; Value: TRasterColor);
var
  L, X1, Y1, X2, Y2: Integer;
begin
  L := LineDist div 2;

  X1 := DstX - L;
  Y1 := DstY - L;
  X2 := DstX + L;
  Y2 := DstY + L;
  Line(X1, Y1, X2, Y2, Value, False);

  X1 := DstX - L;
  Y1 := DstY + L;
  X2 := DstX + L;
  Y2 := DstY - L;
  Line(X1, Y1, X2, Y2, Value, False);
end;

procedure TMemoryRaster.DrawCrossF(DstX, DstY, LineDist: TGeoFloat; Value: TRasterColor);
begin
  DrawCross(Round(DstX), Round(DstY), Round(LineDist), Value);
end;

procedure TMemoryRaster.DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Value: TRasterColor);
begin
  DrawCrossF(Dst[0], Dst[1], LineDist, Value);
end;

procedure TMemoryRaster.DrawPointListLine(pl: TVec2List; Value: TRasterColor; wasClose: Boolean);
var
  i: Integer;
  p1, p2: PVec2;
  buff: array of TPointDouble;
begin
  if pl.Count < 2 then
      exit;

  if FAgg <> nil then
    begin
      i := pl.Count;
      if wasClose then
          Inc(i);

      SetLength(buff, i);

      for i := 0 to pl.Count - 1 do
        begin
          p1 := pl[i];
          buff[i].X := p1^[0];
          buff[i].Y := p1^[1];
        end;
      if wasClose then
        begin
          p1 := pl.First;
          buff[pl.Count].X := p1^[0];
          buff[pl.Count].Y := p1^[1];
        end;

      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Polyline(@buff[0], pl.Count + 1);
      SetLength(buff, 0);
    end
  else
    begin
      for i := 1 to pl.Count - 1 do
        begin
          p1 := pl[i - 1];
          p2 := pl[i];
          LineF(p1^, p2^, Value, True);
        end;
      if wasClose then
        begin
          p1 := pl.First;
          p2 := pl.Last;
          LineF(p1^, p2^, Value, True);
        end;
    end;
end;

procedure TMemoryRaster.DrawCircle(cc: TVec2; R: TGeoFloat; Value: TRasterColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Circle(cc[0], cc[1], R);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(R), cc, R);
      DrawPointListLine(vl, Value, True);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillCircle(cc: TVec2; R: TGeoFloat; Value: TRasterColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Value;
      FAgg.NoLine;
      FAgg.Circle(cc[0], cc[1], R);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(R), cc, R);
      Vertex.FillPoly(vl, cc, Value);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.DrawEllipse(cc: TVec2; xRadius, yRadius: TGeoFloat; Value: TRasterColor);
var
  i, n: Integer;
  S, c: TGeoFloatArray;
  vl: TVec2List;
begin
  if xRadius = yRadius then
      DrawCircle(cc, xRadius, Value)
  else if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Ellipse(cc[0], cc[1], xRadius, yRadius);
    end
  else
    begin
      n := Round(Max(xRadius, yRadius) * 0.1) + 5;
      SetLength(S, n);
      SetLength(c, n);
      Dec(n);
      BuildSinCosCache(@S, @c, 0, 90);
      for i := 0 to n do
        begin
          S[i] := S[i] * yRadius;
          c[i] := c[i] * xRadius;
        end;
      vl := TVec2List.Create;
      // first quadrant (top right)
      for i := 0 to n do
          vl.Add(cc[0] + c[i], cc[1] - S[i]);
      // second quadrant (top left)
      for i := n - 1 downto 0 do
          vl.Add(cc[0] - c[i], cc[1] - S[i]);
      // third quadrant (bottom left)
      for i := 1 to n do
          vl.Add(cc[0] - c[i], cc[1] + S[i]);
      // fourth quadrant (bottom right)
      for i := n - 1 downto 0 do
          vl.Add(cc[0] + c[i], cc[1] + S[i]);
      SetLength(S, 0);
      SetLength(c, 0);
      DrawPointListLine(vl, Value, False);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillEllipse(cc: TVec2; xRadius, yRadius: TGeoFloat; Value: TRasterColor);
var
  i, n: Integer;
  S, c: TGeoFloatArray;
  vl: TVec2List;
begin
  if xRadius = yRadius then
      FillCircle(cc, xRadius, Value)
  else if FAgg <> nil then
    begin
      FAgg.FillColor := Value;
      FAgg.NoLine;
      FAgg.Ellipse(cc[0], cc[1], xRadius, yRadius);
    end
  else
    begin
      n := Round(Max(xRadius, yRadius) * 0.1) + 5;
      SetLength(S, n);
      SetLength(c, n);
      Dec(n);
      BuildSinCosCache(@S, @c, 0, 90);
      for i := 0 to n do
        begin
          S[i] := S[i] * yRadius;
          c[i] := c[i] * xRadius;
        end;
      vl := TVec2List.Create;
      // first quadrant (top right)
      for i := 0 to n do
          vl.Add(cc[0] + c[i], cc[1] - S[i]);
      // second quadrant (top left)
      for i := n - 1 downto 0 do
          vl.Add(cc[0] - c[i], cc[1] - S[i]);
      // third quadrant (bottom left)
      for i := 1 to n do
          vl.Add(cc[0] - c[i], cc[1] + S[i]);
      // fourth quadrant (bottom right)
      for i := n - 1 downto 0 do
          vl.Add(cc[0] + c[i], cc[1] + S[i]);
      SetLength(S, 0);
      SetLength(c, 0);
      Vertex.FillPoly(vl, cc, Value);
      DisposeObject(vl);
    end;
end;

function TMemoryRaster.TextSize(Text: SystemString; Siz: TGeoFloat): TVec2;
begin
  Result := Vec2Mul(Font.TextSize(Text), Siz / Font.FontSize)
end;

procedure TMemoryRaster.DrawText(Text: SystemString; X, Y: Integer; RotateVec: TVec2; Angle, alpha, Siz: TGeoFloat; TextColor: TRasterColor);
var
  fSiz: TVec2;
  sc: TGeoFloat;
  fPt: TVec2;
begin
  fSiz := Font.TextSize(Text);
  sc := Siz / Font.FontSize;
  fPt := Vec2Mul(Vec2Sub(fSiz, Vec2Mul(fSiz, sc)), RotateVec);
  Font.Draw(Text, Self, Vec2(X - fPt[0], Y - fPt[1]), TextColor, True, alpha, RotateVec, Angle, sc);
end;

procedure TMemoryRaster.DrawText(Text: SystemString; X, Y: Integer; Siz: TGeoFloat; TextColor: TRasterColor);
begin
  DrawText(Text, X, Y, Vec2(0.5, 0.5), 0, 1, Siz, TextColor);
end;

procedure TMemoryRaster.ProjectionTo2DMap(Dst: TMemoryRaster; const SourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: Single);
begin
  Vertex.DrawRect(SourRect, DestRect, Self, bilinear_sampling, alpha);
end;

procedure TMemoryRaster.ProjectionColor(const DestRect: TV2Rect4; const color: TRasterColor);
begin
  Vertex.DrawRect(DestRect, color);
end;

procedure TMemoryRaster.Draw(Src: TMemoryRaster);
begin
  Src.DrawTo(Self);
end;

procedure TMemoryRaster.Draw(DstX, DstY: Integer; Src: TMemoryRaster);
begin
  Src.DrawTo(Self, DstX, DstY);
end;

procedure TMemoryRaster.Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TMemoryRaster);
begin
  Src.DrawTo(Self, DstX, DstY, SrcRect);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster);
begin
  if (Dst.FAgg <> nil) and (FAggImage <> nil) then
      Dst.FAgg.TransformImage(FAggImage, 0, 0, FWidth, FHeight, 0, 0, FWidth, FHeight)
  else
      BlockTransfer(Dst, 0, 0, Dst.BoundsRect, Self, BoundsRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer; const SrcRect: TRect);
begin
  if (Dst.FAgg <> nil) and (FAggImage <> nil) then
      Dst.FAgg.TransformImage(FAggImage, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstX, DstY, FWidth, FHeight)
  else
      BlockTransfer(Dst, DstX, DstY, Dst.BoundsRect, Self, SrcRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer);
begin
  if (Dst.FAgg <> nil) and (FAggImage <> nil) then
      Dst.FAgg.TransformImage(FAggImage, 0, 0, FWidth, FHeight, DstX, DstY, FWidth, FHeight)
  else
      BlockTransfer(Dst, DstX, DstY, Dst.BoundsRect, Self, BoundsRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; DstPt: TVec2);
begin
  DrawTo(Dst, Round(DstPt[0]), Round(DstPt[1]));
end;

class function TMemoryRaster.CanLoadStream(Stream: TCoreClassStream): Boolean;
var
  bakPos: Int64;
  hflag, hflag2: Word;
  Header: TBmpHeader;
begin
  Result := False;
  try
    bakPos := Stream.Position;

    Stream.Read(hflag, 2);
    if (hflag = $8D42) or (hflag = $8D43) or (hflag = $8D44) or (hflag = $8D45) then
        Result := True
    else if (hflag = $D8FF) or (hflag = $8DFF) then
      begin
        Stream.Read(hflag2, 2);
        Result := (hflag2 = $F7FF);
      end
    else
      begin
        Stream.Position := bakPos;

        Stream.ReadBuffer(Header, SizeOf(TBmpHeader));

        Result := (Header.bfType = $4D42) and
          (Header.biBitCount = 32) and (Header.biPlanes = 1) and
          (Header.biCompression = 0);
      end;

    Stream.Position := bakPos;
  except
  end;
end;

procedure TMemoryRaster.LoadFromBmpStream(Stream: TCoreClassStream);
var
  i, w, j: Integer;
  Header: TBmpHeader;
begin
  Reset;

  Stream.ReadBuffer(Header, SizeOf(TBmpHeader));

  // Check for Windows bitmap magic bytes and general compatibility of the
  // bitmap data that ought to be loaded...
  if (Header.bfType = $4D42) and
    (Header.biBitCount = 32) and (Header.biPlanes = 1) and
    (Header.biCompression = 0) then
    begin
      SetSize(Header.biWidth, abs(Header.biHeight));

      // Check whether the bitmap is saved top-down
      if Header.biHeight > 0 then
        begin
          w := Width shl 2;
          for i := Height - 1 downto 0 do
            begin
              Stream.ReadBuffer(ScanLine[i]^, w);
            end;
        end
      else
        begin
          Stream.ReadBuffer(FBits^, (Width * Height) shl 2);
        end;
    end
  else
    begin
      raise CoreClassException.Create('bmp format failed!');
    end;
end;

procedure TMemoryRaster.LoadFromStream(Stream: TCoreClassStream);
var
  bakPos: Int64;

  hflag, hflag2: Word;
  m64: TMemoryStream64;
begin
  Reset;

  bakPos := Stream.Position;

  Stream.Read(hflag, 2);
  if hflag = $8D42 then
    begin
      m64 := TMemoryStream64.Create;
      DecompressStream(Stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      exit;
    end
  else if hflag = $8D43 then
    begin
      m64 := TMemoryStream64.Create;
      DeflateDecompressStream(Stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      exit;
    end
  else if hflag = $8D44 then
    begin
      m64 := TMemoryStream64.Create;
      BRRCDecompressStream(Stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      exit;
    end
  else if hflag = $4D42 then
    begin
      Stream.Position := bakPos;
      LoadFromBmpStream(Stream);
    end
    // jls endian support
  else if (hflag = $D8FF) or (hflag = $8DFF) then
    begin
      Stream.Read(hflag2, 2);
      Stream.Position := bakPos;
      if (hflag2 = $F7FF) then
          DecodeJpegLSRasterFromStream(Stream, Self);
    end
    // jls alpha
  else if hflag = $8D45 then
    begin
      Stream.Position := bakPos;
      DecodeJpegLSRasterAlphaFromStream(Stream, Self);
    end
  else
      Stream.Position := bakPos;
end;

procedure TMemoryRaster.LoadFromStreamAndResize(Stream: TCoreClassStream; const NewWidth, NewHeight: Integer);
var
  Bmp: TMemoryRaster;
begin
  Bmp := TMemoryRaster.Create;
  ZoomFrom(Bmp, NewWidth, NewHeight);
  DisposeObject(Bmp);
end;

procedure TMemoryRaster.SaveToBmpStream(Stream: TCoreClassStream);
var
  Header: TBmpHeader;
  BitmapSize: Integer;
  i, w: Integer;
begin
  BitmapSize := (FWidth * FHeight) shl 2;

  Header.bfType := $4D42; // Magic bytes for Windows Bitmap
  Header.bfSize := BitmapSize + SizeOf(TBmpHeader);
  Header.bfReserved := 0;
  // Save offset relative. However, the spec says it has to be file absolute,
  // which we can not do properly within a stream...
  Header.bfOffBits := SizeOf(TBmpHeader);
  Header.biSize := $28;
  Header.biWidth := Width;

  Header.biHeight := -Height;

  Header.biPlanes := 1;
  Header.biBitCount := 32;
  Header.biCompression := 0; // bi_rgb
  Header.biSizeImage := BitmapSize;
  Header.biXPelsPerMeter := 0;
  Header.biYPelsPerMeter := 0;
  Header.biClrUsed := 0;
  Header.biClrImportant := 0;

  Stream.WriteBuffer(Header, SizeOf(TBmpHeader));

  Stream.WriteBuffer(Bits^, BitmapSize);
end;

procedure TMemoryRaster.SaveToStream(Stream: TCoreClassStream);
begin
  SaveToBmpStream(Stream);
end;

procedure TMemoryRaster.SaveToZLibCompressStream(Stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D42; // MemoryRaster compress format
  Stream.Write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmpStream(m64);
  m64.Position := 0;
  MaxCompressStream(m64, Stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToDeflateCompressStream(Stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D43; // MemoryRaster compress format
  Stream.Write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmpStream(m64);
  m64.Position := 0;
  DeflateCompressStream(m64, Stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBRRCCompressStream(Stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D44; // MemoryRaster compress format
  Stream.Write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmpStream(m64);
  m64.Position := 0;
  BRRCCompressStream(m64, Stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS1Stream(Stream: TCoreClassStream);
begin
  EncodeJpegLSRasterToStream1(Self, Stream);
end;

procedure TMemoryRaster.SaveToJpegLS3Stream(Stream: TCoreClassStream);
begin
  EncodeJpegLSRasterToStream3(Self, Stream);
end;

procedure TMemoryRaster.SaveToJpegAlphaStream(Stream: TCoreClassStream);
begin
  EncodeJpegLSRasterAlphaToStream(Self, Stream);
end;

class function TMemoryRaster.CanLoadFile(fn: SystemString): Boolean;
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      Result := CanLoadStream(m64);
  except
      Result := False;
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.LoadFromFile(fn: SystemString);
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(m64);
  except
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.LoadFromFileAndResize(fn: SystemString; const NewWidth, NewHeight: Integer);
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStreamAndResize(m64, NewWidth, NewHeight);
  except
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToZLibCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToZLibCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToDeflateCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToDeflateCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBRRCCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToBRRCCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS1File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegLS1Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS3File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegLS3Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegAlphaFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegAlphaStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;
