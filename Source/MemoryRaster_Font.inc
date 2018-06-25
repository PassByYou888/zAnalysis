{$RESOURCE MemoryRaster_DefaultFont.RES}


procedure Init_DefaultFont;
var
  rs: TCoreClassResourceStream;
begin
  rs := TCoreClassResourceStream.Create(hInstance, 'MemoryRasterFont', RT_RCDATA);
  rs.Position := 0;
  SystemFont := TFontRaster.Create;
  SystemFont.LoadFromStream(rs);
  DisposeObject(rs);
end;

procedure Free_DefaultFont;
begin
  DisposeObject(SystemFont);
  SystemFont := nil;
end;

function fr_ComputeNearest(const Sender: PVertexMap; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRasterColor;
var
  fr: TFontRaster;
  tu0, tv0: Integer;
begin
  fr := TFontRaster.PDrawWorkData(Sender^.UserData)^.Owner;
  tu0 := ClampInt(Trunc(TexCoordWrapRepeat(X) * (Sampler.Width)), 0, Sampler.Width - 1);
  tv0 := ClampInt(Trunc(TexCoordWrapRepeat(Y) * (Sampler.Height)), 0, Sampler.Height - 1);

  Result := RasterColor(0, 0, 0, fr.FBitRaster^[tu0 + tv0 * Sampler.Width]);
end;

function fr_ComputeLinear(const Sender: PVertexMap; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRasterColor;
var
  fr: TFontRaster;
  fx, fy: TGeoFloat;
  i_x, i_y: Integer;
  i_x2, i_y2: Integer;
  delta_x, delta_y: TGeoFloat;

  C1, C2, C3, C4: byte;
  k1, k2, k3, k4: TGeoFloat;
  G: TGeoFloat;
begin
  fr := TFontRaster.PDrawWorkData(Sender^.UserData)^.Owner;

  fx := TexCoordWrapRepeat(X) * (Sampler.Width);
  fy := TexCoordWrapRepeat(Y) * (Sampler.Height);

  i_x := ClampInt(Trunc(fx), 0, Sampler.Width - 1);
  i_y := ClampInt(Trunc(fy), 0, Sampler.Height - 1);

  i_x2 := i_x + 1;
  if i_x2 > Sampler.Width - 1 then
      i_x2 := 0;
  i_y2 := i_y + 1;
  if i_y2 > Sampler.Height - 1 then
      i_y2 := 0;

  delta_x := Frac(fx);
  delta_y := Frac(fy);

  k1 := (1 - delta_x) * (1 - delta_y);
  k2 := delta_x * (1 - delta_y);
  k3 := delta_x * delta_y;
  k4 := (1 - delta_x) * delta_y;

  C1 := fr.FBitRaster^[i_x + i_y * Sampler.Width];
  C2 := fr.FBitRaster^[i_x2 + i_y * Sampler.Width];
  C3 := fr.FBitRaster^[i_x2 + i_y2 * Sampler.Width];
  C4 := fr.FBitRaster^[i_x + i_y2 * Sampler.Width];

  G := ((C1 / $FF) * k1);
  G := G + ((C2 / $FF) * k2);
  G := G + ((C3 / $FF) * k3);
  G := G + ((C4 / $FF) * k4);

  if G < 0 then
      G := 0
  else if G > 1.0 then
      G := 1.0;

  Result := RasterColor(0, 0, 0, Round(G * $FF));
end;

procedure fr_ComputeBlend(const Sender: PVertexMap; const f, m: TRasterColor; var B: TRasterColor);
var
  C: TRasterColorEntry;
begin
  C.RGBA := TFontRaster.PDrawWorkData(Sender^.UserData)^.DestColor;
  C.A := TRasterColorEntry(f).A;
  Vertex_ComputeBlend(Sender, C.RGBA, m, B);
end;

constructor TFontRaster.Create;
var
  i: Integer;
begin
  inherited Create;
  FOnlyInstance := True;
  FFontTable := GetMemory(C_MAXWORD * SizeOf(TFontCharDefine));
  SetLength(FFragRaster, C_MAXWORD);
  for i := 0 to C_MAXWORD - 1 do
    begin
      FFontTable^[i] := C_WordDefine;
      FFragRaster[i] := nil;
    end;
  FBitRaster := nil;
  FFontSize := 0;
  FActivtedWord := 0;
  FWidth := 0;
  FHeight := 0;
end;

constructor TFontRaster.Create(ShareFont: TFontRaster);
begin
  inherited Create;
  FOnlyInstance := False;
  FFontTable := ShareFont.FFontTable;
  SetLength(FFragRaster, 0);
  FBitRaster := ShareFont.FBitRaster;
  FFontSize := ShareFont.FFontSize;
  FActivtedWord := ShareFont.FActivtedWord;
  FWidth := ShareFont.FWidth;
  FHeight := ShareFont.FHeight;
end;

destructor TFontRaster.Destroy;
begin
  Clear;
  if FOnlyInstance then
    begin
      FreeMemory(FFontTable);
      SetLength(FFragRaster, 0);
    end;
  inherited Destroy;
end;

procedure TFontRaster.Add(C: TFontRasterChar; Raster: TMemoryRaster);
begin
  Remove(C);
  FFragRaster[Word(C)] := Raster;
end;

procedure TFontRaster.Remove(C: TFontRasterChar);
var
  i: Word;
begin
  i := Word(C);
  if FOnlyInstance then
      FFontTable^[i] := C_WordDefine;
  if FFragRaster[i] <> nil then
    begin
      DisposeObject(FFragRaster[i]);
      FFragRaster[i] := nil;
    end;
end;

procedure TFontRaster.Clear;
var
  i: Integer;
begin
  if FOnlyInstance then
    begin
      for i := 0 to C_MAXWORD - 1 do
        begin
          FFontTable^[i] := C_WordDefine;
          if FFragRaster[i] <> nil then
            begin
              DisposeObject(FFragRaster[i]);
              FFragRaster[i] := nil;
            end;
        end;

      FreeMemory(FBitRaster);
      FBitRaster := nil;

      FActivtedWord := 0;
      FWidth := 0;
      FHeight := 0;
    end;
end;

procedure TFontRaster.Build(fontSiz: Integer);
var
  rHash: THashList;
  HashMapTable: array [0 .. C_MAXWORD - 1] of PFontCharDefine;
  rp: TRectPacking;
  m5: TFontRasterString;
  hp: Pointer;
  i, X, Y: Integer;
  Rect_p: PRectPackData;
  fontchar_p: PFontCharDefine;
  Raster: TMemoryRaster;
  R: TRect;
  fr, fg, fb, fa: TGeoFloat;
  gb: byte;

begin
  if not FOnlyInstance then
      exit;

  DoStatus('build font Hash...');
  rHash := THashList.Create(8192);
  rHash.AutoFreeData := False;

  rp := TRectPacking.Create;
  for i := 0 to C_MAXWORD - 1 do
    begin
      fontchar_p := @(FFontTable^[i]);
      fontchar_p^ := C_WordDefine;
      HashMapTable[i] := fontchar_p;
      Raster := FFragRaster[i];
      if Raster <> nil then
        begin
          m5 := umlMD5String(@Raster.Bits^[0], Raster.Width * Raster.Height * SizeOf(TRasterColor));
          hp := rHash[m5];
          if hp = nil then
            begin
              rHash.Add(m5, fontchar_p);
              rp.Add(fontchar_p, Raster, Raster.BoundsRectV2);
            end
          else
              HashMapTable[i] := PFontCharDefine(hp);
        end;
    end;
  DisposeObject(rHash);

  if rp.Count > 0 then
    begin
      DoStatus('build font space...');

      rp.Build(C_MAXWORD * 1024, C_MAXWORD * 1024);

      FActivtedWord := 0;
      FWidth := Round(rp.MaxWidth);
      FHeight := Round(rp.MaxHeight);

      FBitRaster := GetMemory(FWidth * FHeight);
      FillPtrByte(FBitRaster, FWidth * FHeight, 0);

      DoStatus('compress font raster...');
      for i := 0 to rp.Count - 1 do
        begin
          Rect_p := rp[i];
          if Rect_p^.error then
            begin
              DoStatus('wordRaster Build Error');
              continue;
            end;
          fontchar_p := PFontCharDefine(Rect_p^.Data1);
          Raster := TMemoryRaster(Rect_p^.Data2);

          R := MakeRect(Rect_p^.rect);

          // write word
          fontchar_p^.Activted := True;
          fontchar_p^.X := R.Left;
          fontchar_p^.Y := R.Top;
          fontchar_p^.W := R.Right - R.Left;
          fontchar_p^.H := R.Bottom - R.Top;

          // write raster
          for Y := 0 to Raster.Height - 1 do
            for X := 0 to Raster.Width - 1 do
                FBitRaster^[(Y + R.Top) * FWidth + (X + R.Left)] := Raster.PixelGray[X, Y];

          inc(FActivtedWord);
        end;

      DoStatus('merge font space...');
      for i := 0 to C_MAXWORD - 1 do
        begin
          if HashMapTable[i] <> @(FFontTable^[i]) then
              FFontTable^[i] := HashMapTable[i]^;

          if FFragRaster[i] <> nil then
            begin
              DisposeObject(FFragRaster[i]);
              FFragRaster[i] := nil;
            end;
        end;
    end;

  DisposeObject(rp);

  FFontSize := fontSiz;

  DoStatus('build font done.');
end;

procedure TFontRaster.LoadFromStream(Stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  if not FOnlyInstance then
      exit;

  Clear;
  m64 := TMemoryStream64.Create;
  DecompressStream(Stream, m64);
  m64.Position := 0;

  m64.Read(FFontSize, 4);
  m64.Read(FActivtedWord, 4);
  m64.Read(FWidth, 4);
  m64.Read(FHeight, 4);
  FBitRaster := GetMemory(FWidth * FHeight);
  m64.Read(FBitRaster^[0], FWidth * FHeight);
  m64.Read(FFontTable^[0], C_MAXWORD * SizeOf(TFontCharDefine));

  DisposeObject(m64);
end;

procedure TFontRaster.SaveToStream(Stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.Write(FFontSize, 4);
  m64.Write(FActivtedWord, 4);
  m64.Write(FWidth, 4);
  m64.Write(FHeight, 4);
  m64.Write(FBitRaster^[0], FWidth * FHeight);
  m64.Write(FFontTable^[0], C_MAXWORD * SizeOf(TFontCharDefine));

  MaxCompressStream(m64, Stream);
  DisposeObject(m64);

  Stream.Position := 0;
end;

procedure TFontRaster.ExportRaster(Stream: TCoreClassStream; partitionLine: Boolean);
var
  Raster: TMemoryRaster;
  Y, X: Integer;
  fd: PFontCharDefine;
  R: TRect;
begin
  Raster := TMemoryRaster.Create;
  Raster.SetSize(FWidth, FHeight);
  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
        Raster.PixelGray[X, Y] := FBitRaster^[Y * FWidth + X];

  if partitionLine then
    for X := 0 to C_MAXWORD - 1 do
      begin
        fd := @(FFontTable^[X]);
        R := rect(fd^.X, fd^.Y, fd^.X + fd^.W, fd^.Y + fd^.H);
        Raster.DrawRect(R, RasterColor(255, 0, 0, 200));
      end;

  Raster.SaveToStream(Stream);
  DisposeObject(Raster);
end;

function TFontRaster.CharSize(const C: TFontRasterChar): TPoint;
begin
  with FFontTable^[Word(C)] do
      Result := Point(W, H);
end;

function TFontRaster.TextSize(const s: TFontRasterString; charVec2List: TVec2List): TVec2;
var
  i: Integer;
  C: TFontRasterChar;
  fd: PFontCharDefine;
  rp: TVec2;
  mh: byte;
begin
  Result := Vec2(0, 0);
  rp := Vec2(0, 0);
  mh := 0;
  for i := 1 to s.Len do
    begin
      C := s[i];
      fd := @(FFontTable^[Word(C)]);
      if fd^.H > mh then
          mh := fd^.H;

      if C = #10 then
        begin
          rp[0] := 0;
          rp[1] := rp[1] + mh + 1;
          mh := 0;
          continue;
        end;

      if C = #9 then
        begin
          rp[0] := rp[0] + (FFontTable^[32].W + 1) * 2;
          continue;
        end;

      if C = #13 then
          continue;

      if not fd^.Activted then
          continue;

      if charVec2List <> nil then
          charVec2List.Add(rp[0] + (fd^.W + 1) * 0.5, rp[1] + (fd^.H + 1) * 0.5);
      rp[0] := rp[0] + fd^.W;
      Result[0] := Max(Result[0], rp[0]);
    end;
  Result[1] := rp[1] + mh + 1;
end;

function TFontRaster.TextSize(const s: TFontRasterString): TVec2;
begin
  Result := TextSize(s, nil);
end;

function TFontRaster.TextWidth(const s: TFontRasterString): Word;
var
  v: TVec2;
begin
  v := TextSize(s);
  Result := Round(v[0]);
end;

function TFontRaster.TextHeight(const s: TFontRasterString): Word;
var
  v: TVec2;
begin
  v := TextSize(s);
  Result := Round(v[1]);
end;

function TFontRaster.Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRasterColor;
  const bilinear_sampling: Boolean; const Alpha: TGeoFloat; const Axis: TVec2; const Angle, Scale: TGeoFloat): TVec2;
var
  bak_userData: Pointer;
  dd: TDrawWorkData;
  TempSampler: TMemoryRaster;
  vm: TVertexMap;
  i: Integer;
  C: TFontRasterChar;
  fd: PFontCharDefine;
  cv: TVec2;
  s: TRectV2;
  mh: byte;
  vSiz: TVec2;
  wp: TPoly;
  vl: TVec2List;
  wi: Integer;
begin
  // init triangle raster
  dd.Owner := Self;
  dd.DestColor := dstColor;
  TempSampler := TMemoryRaster.Create;
  TempSampler.SetWorkMemory(nil, FWidth, FHeight);

  vm := Dst.Vertex;
  bak_userData := vm.UserData;
  vm.ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}fr_ComputeNearest;
  vm.ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}fr_ComputeLinear;
  vm.ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}fr_ComputeBlend;
  vm.UserData := @dd;

  // compute rotate
  vl := TVec2List.Create;
  vSiz := TextSize(Text, vl);
  wp := TPoly.Create;
  wp.Position := Vec2Mul(vSiz, Axis);
  for wi := 0 to vl.Count - 1 do
      wp.AddPoint(vl[wi]^);
  DisposeObject(vl);
  wp.Position := Vec2Add(dstVec, Vec2Mul(vSiz, Axis));
  wp.Angle := Angle;
  wp.Scale := Scale;
  wi := 0;

  vm.BeginUpdate;
  // projection
  try
    for i := 1 to Text.Len do
      begin
        C := Text[i];
        fd := @(FFontTable^[Word(C)]);

        if C = #10 then
            continue;

        if C = #9 then
            continue;

        if C = #13 then
            continue;

        if not fd^.Activted then
            continue;

        s := RectV2(fd^.X, fd^.Y, fd^.X + fd^.W + 1, fd^.Y + fd^.H + 1);

        cv := wp.Points[wi];

        vm.DrawRect(TV2Rect4.Init(s, 0),
          TV2Rect4.Init(cv, (fd^.W + 1) * Scale, (fd^.H + 1) * Scale, Angle), TempSampler, bilinear_sampling, 1);

        inc(wi);
      end;
  finally
    vm.EndUpdate;

    DisposeObject(wp);
    DisposeObject(TempSampler);

    // restore vertex parameter
    vm.ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeBlend;
    vm.ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeNearest;
    vm.ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeLinear;
    vm.UserData := bak_userData;
  end;

  Result := vSiz;

end;

procedure TFontRaster.Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRasterColor);
begin
  Draw(Text, Dst, dstVec, dstColor, True, 1.0, Vec2(0.5, 0.5), 0, 1);
end;
