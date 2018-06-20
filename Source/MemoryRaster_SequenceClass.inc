constructor TSequenceMemoryRaster.Create;
begin
  inherited Create;
  FTotal := 1;
  FColumn := 1;
end;

destructor TSequenceMemoryRaster.Destroy;
begin
  inherited Destroy;
end;

procedure TSequenceMemoryRaster.Clear(FillColor: TRasterColor);
begin
  inherited Clear(FillColor);
  FTotal := 1;
  FColumn := 1;
end;

procedure TSequenceMemoryRaster.SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor);
begin
  inherited SetSize(NewWidth, NewHeight, ClearColor);
  FTotal := 1;
  FColumn := 1;
end;

procedure TSequenceMemoryRaster.Reset;
begin
  inherited Reset;
  FTotal := 1;
  FColumn := 1;
end;

procedure TSequenceMemoryRaster.Assign(sour: TMemoryRaster);
begin
  inherited Assign(sour);
  FTotal := 1;
  FColumn := 1;
  if sour is TSequenceMemoryRaster then
    begin
      FTotal := TSequenceMemoryRaster(sour).FTotal;
      FColumn := TSequenceMemoryRaster(sour).FColumn;
    end;
end;

class function TSequenceMemoryRaster.CanLoadStream(Stream: TCoreClassStream): Boolean;
var
  fp: Int64;
  hflag: Word;
  ATotal, AColumn: Integer;
begin
  Result := False;
  fp := Stream.Position;
  if Stream.Read(hflag, 2) <> 2 then
      Exit;
  try
    if hflag = $8888 then
      begin
        if Stream.Read(ATotal, 4) <> 4 then
            Exit;
        if Stream.Read(AColumn, 4) <> 4 then
            Exit;
        Result := inherited CanLoadStream(Stream);
        Stream.Position := fp;
      end
    else
      begin
        Stream.Position := fp;
        Result := inherited CanLoadStream(Stream);
      end;
  except
  end;
end;

procedure TSequenceMemoryRaster.LoadFromStream(Stream: TCoreClassStream);
var
  fp: Int64;
  hflag: Word;
  ATotal, AColumn: Integer;
  deStream: TMemoryStream64;
begin
  Reset;
  fp := Stream.Position;
  if Stream.Read(hflag, 2) <> 2 then
      Exit;
  if hflag = $8888 then
    begin
      if Stream.Read(ATotal, 4) <> 4 then
          Exit;
      if Stream.Read(AColumn, 4) <> 4 then
          Exit;
      inherited LoadFromStream(Stream);
      FTotal := ATotal;
      FColumn := AColumn;
    end
  else
    begin
      Stream.Position := fp;
      inherited LoadFromStream(Stream);
      FTotal := 1;
      FColumn := 1;
    end;
end;

procedure TSequenceMemoryRaster.SaveToStream(Stream: TCoreClassStream);
var
  hflag: Word;
  cStream: TMemoryStream64;
begin
  if FTotal > 1 then
    begin
      hflag := $8888;
      Stream.Write(hflag, 2);
      Stream.Write(FTotal, 4);
      Stream.Write(FColumn, 4);
      inherited SaveToZLibCompressStream(Stream);
      Exit;
    end;
  inherited SaveToStream(Stream);
end;

function TSequenceMemoryRaster.SequenceFrameRect(index: Integer): TRect;
begin
  Result := GetSequenceFrameRect(Self, Total, Column, index);
end;

procedure TSequenceMemoryRaster.ExportSequenceFrame(index: Integer; Output: TMemoryRaster);
begin
  GetSequenceFrameOutput(Self, Total, Column, index, Output);
end;

procedure TSequenceMemoryRaster.ReverseSequence(Output: TSequenceMemoryRaster);
var
  I: Integer;
  R: TRect;
begin
  Output.SetSize(Width, Height);
  for I := 0 to Total - 1 do
    begin
      R := SequenceFrameRect(I);
      BlockTransfer(Output, R.Left, R.Top, Output.BoundsRect, Self, SequenceFrameRect(Total - 1 - I), dmOpaque);
    end;
  Output.FTotal := FTotal;
  Output.FColumn := FColumn;
end;

procedure TSequenceMemoryRaster.GradientSequence(Output: TSequenceMemoryRaster);
var
  I, J: Integer;
  sr, dr: TRect;
begin
  Output.SetSize(FrameWidth * (Total * 2), FrameHeight);
  Output.Column := Total * 2;
  Output.Total := Output.Column;

  J := 0;

  for I := 0 to Total - 1 do
    begin
      dr := Output.SequenceFrameRect(J);
      sr := SequenceFrameRect(I);
      BlockTransfer(Output, dr.Left, dr.Top, Output.BoundsRect, Self, sr, dmOpaque);
      Inc(J);
    end;

  for I := Total - 1 downto 0 do
    begin
      dr := Output.SequenceFrameRect(J);
      sr := SequenceFrameRect(I);
      BlockTransfer(Output, dr.Left, dr.Top, Output.BoundsRect, Self, sr, dmOpaque);
      Inc(J);
    end;
end;

function TSequenceMemoryRaster.FrameWidth: Integer;
begin
  with SequenceFrameRect(0) do
      Result := Right - Left;
end;

function TSequenceMemoryRaster.FrameHeight: Integer;
begin
  with SequenceFrameRect(0) do
      Result := Bottom - Top;
end;

function TSequenceMemoryRaster.FrameRect2D: TRectV2;
begin
  Result := MakeRectV2(0, 0, FrameWidth, FrameHeight);
end;

function TSequenceMemoryRaster.FrameRect: TRect;
begin
  Result := Rect(0, 0, FrameWidth, FrameHeight);
end;
