constructor TMemoryRaster_AggImage.Create(Raster: TMemoryRaster);
begin
  inherited Create(PInt8U(Raster.ScanLine[0]), Raster.Width, Raster.Height, 4 * Raster.Width);
end;

procedure TMemoryRaster_AggImage.Attach(Raster: TMemoryRaster);
begin
  inherited Attach(PInt8U(Raster.ScanLine[0]), Raster.Width, Raster.Height, 4 * Raster.Width);
end;

function TMemoryRaster_Agg2D.GetImageBlendColor: TRasterColor;
begin
  Result := AggColor(FImageBlendColor);
end;

procedure TMemoryRaster_Agg2D.SetImageBlendColor(const Value: TRasterColor);
begin
  inherited ImageBlendColor := AggColor(Value);
end;

function TMemoryRaster_Agg2D.GetFillColor: TRasterColor;
begin
  Result := AggColor(FFillColor);
end;

procedure TMemoryRaster_Agg2D.SetFillColor(const Value: TRasterColor);
begin
  inherited FillColor := AggColor(Value);
end;

function TMemoryRaster_Agg2D.GetLineColor: TRasterColor;
begin
  Result := AggColor(FLineColor);
end;

procedure TMemoryRaster_Agg2D.SetLineColor(const Value: TRasterColor);
begin
  inherited LineColor := AggColor(Value);
end;

procedure TMemoryRaster_Agg2D.Attach(Raster: TMemoryRaster);
begin
  inherited Attach(PInt8U(Raster.ScanLine[0]), Raster.Width, Raster.Height, 4 * Raster.Width);
end;

procedure TMemoryRaster_Agg2D.FillLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TRasterColor; Profile: Double);
begin
  inherited FillLinearGradient(X1, Y1, X2, Y2, AggColor(C1), AggColor(C2), Profile);
end;

procedure TMemoryRaster_Agg2D.LineLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TRasterColor; Profile: Double);
begin
  inherited LineLinearGradient(X1, Y1, X2, Y2, AggColor(C1), AggColor(C2), Profile);
end;

procedure TMemoryRaster_Agg2D.FillRadialGradient(X, Y, R: Double; C1, C2: TRasterColor; Profile: Double);
begin
  inherited FillRadialGradient(X, Y, R, AggColor(C1), AggColor(C2), Profile);
end;

procedure TMemoryRaster_Agg2D.LineRadialGradient(X, Y, R: Double; C1, C2: TRasterColor; Profile: Double);
begin
  inherited LineRadialGradient(X, Y, R, AggColor(C1), AggColor(C2), Profile);
end;

procedure TMemoryRaster_Agg2D.FillRadialGradient(X, Y, R: Double; C1, C2, C3: TRasterColor);
begin
  inherited FillRadialGradient(X, Y, R, AggColor(C1), AggColor(C2), AggColor(C3));
end;

procedure TMemoryRaster_Agg2D.LineRadialGradient(X, Y, R: Double; C1, C2, C3: TRasterColor);
begin
  inherited LineRadialGradient(X, Y, R, AggColor(C1), AggColor(C2), AggColor(C3));
end;
