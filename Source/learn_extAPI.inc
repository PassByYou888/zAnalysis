function LVec(const veclen: TLInt): TLVec;
var
  i: TLInt;
begin
  SetLength(Result, veclen);
  for i := 0 to veclen - 1 do
      Result[i] := 0.0;
end;

function LVec(const v: TLVec): TPascalString;
begin
  Result := TKDTree.KDTreeVec(v);
end;

function LVec(const m: TLMatrix; const veclen: TLInt): TLVec;
var
  i, J, id: TLInt;
begin
  id := 0;
  Result := LVec(veclen);
  for i := 0 to Length(m) - 1 do
    for J := 0 to Length(m[i]) - 1 do
      if id < veclen then
        begin
          Result[id] := m[i, J];
          inc(id);
        end
      else
          exit;
end;

function LVec(const s: TPascalString; const veclen: TLInt): TLVec;
var
  i: TLInt;
  v: TLVec;
begin
  v := TKDTree.KDTreeVec(s.Text);
  Result := LVec(veclen);
  for i := 0 to Length(v) - 1 do
    if i < veclen then
        Result[i] := v[i]
    else
        break;
end;

function LVec(const v: TLVec; const ShortFloat: Boolean): TPascalString;
var
  i: TLInt;
begin
  Result := '';
  for i := 0 to Length(v) - 1 do
    begin
      if i > 0 then
          Result.Append(' ');

      if ShortFloat then
          Result.Append(umlShortFloatToStr(v[i]))
      else
          Result.Append(umlFloatToStr(v[i]));
    end;
end;

function SpearmanLVec(const m: TLMatrix; const veclen: TLInt): TLVec;
begin
  Result := LVec(m, veclen);
  SpearmanRank(Result, Length(Result));
end;

function MaxVec(const v: TLVec): TLFloat;
var
  i: TLInt;
begin
  Result := v[0];

  if Length(v) > 1 then
    for i := 1 to Length(v) - 1 do
      if v[i] > Result then
          Result := v[i];
end;

function MinVec(const v: TLVec): TLFloat;
var
  i: TLInt;
begin
  Result := v[0];

  if Length(v) > 1 then
    for i := 1 to Length(v) - 1 do
      if v[i] < Result then
          Result := v[i];
end;

function MaxVecIndex(const v: TLVec): TLInt;
var
  i: TLInt;
  k: TLFloat;
begin
  k := v[0];
  Result := 0;

  if Length(v) > 1 then
    for i := 1 to Length(v) - 1 do
      if v[i] > k then
        begin
          Result := i;
          k := v[i];
        end;
end;

function MinVecIndex(const v: TLVec): TLInt;
var
  i: TLInt;
  k: TLFloat;
begin
  k := v[0];
  Result := 0;

  if Length(v) > 1 then
    for i := 1 to Length(v) - 1 do
      if v[i] < k then
        begin
          Result := i;
          k := v[i];
        end;
end;

procedure Clamp(var AValue: TLFloat; const AMin, AMax: TLFloat);
begin
  if AMin > AMax then
      Clamp(AValue, AMax, AMin)
  else if AValue > AMax then
      AValue := AMax
  else if AValue < AMin then
      AValue := AMin;
end;

procedure Clamp(var AValue: TLInt; const AMin, AMax: TLInt);
begin
  if AMin > AMax then
      Clamp(AValue, AMax, AMin)
  else if AValue > AMax then
      AValue := AMax
  else if AValue < AMin then
      AValue := AMin;
end;

function MatrixSampler(const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix;
var
  N   : TMemoryRaster;
  i, J: TLInt;
begin
  if (mr.Width = SamplerSize) and (mr.Height = SamplerSize) then
    begin
      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[i, J] := Round(mr.PixelGrayD[i, J] * $FF);
    end
  else
    begin
      N := TMemoryRaster.Create;
      N.ZoomFrom(mr, SamplerSize, SamplerSize);

      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[i, J] := Round(N.PixelGrayD[i, J] * $FF);

      DisposeObject(N);
    end;
end;

function MatrixSampler(const Antialiasing: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix;
var
  N   : TMemoryRaster;
  i, J: TLInt;
begin
  if (mr.Width = SamplerSize) and (mr.Height = SamplerSize) then
    begin
      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[i, J] := Round(mr.PixelGrayD[i, J] * $FF);
    end
  else
    begin
      N := TMemoryRaster.Create;
      if Antialiasing then
          N.GaussianBlurZoomFrom(mr, SamplerSize, SamplerSize)
      else
          N.ZoomFrom(mr, SamplerSize, SamplerSize);

      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[i, J] := Round(N.PixelGrayD[i, J] * $FF);

      DisposeObject(N);
    end;
end;

function LDA(const buff: TLMatrix; const NPoints, NVars, NClasses: TLInt; var w: TLMatrix): TLInt;
begin
  FisherLDAN(buff, NPoints, NVars, NClasses, Result, w);
end;

function LDA(const buff: TLMatrix; const NPoints, NVars, NClasses: TLInt; var w: TLVec): TLInt;
begin
  FisherLDA(buff, NPoints, NVars, NClasses, Result, w);
end;

function LDA(const Fast: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster; var sInfo: string; var output: TLMatrix): Boolean;
var
  k        : TLFloat;
  mrblur, N: TMemoryRaster;
  i, J     : TLInt;
  buff     : TLMatrix;
  rInfo    : TLInt;
begin
  k := SamplerSize / mr.Width;

  N := TMemoryRaster.Create;
  if (not Fast) and (k < 1.0) then
    begin
      mrblur := TMemoryRaster.Create;
      // preprocess liner zoom
      GrayscaleBlur(mr, mrblur, mr.Width / SamplerSize, mr.BoundsRect);
      // zoom
      N.ZoomFrom(mrblur, SamplerSize, Round(mrblur.Height * k));
      DisposeObject(mrblur);
    end
  else
    begin
      // zoom
      N.ZoomFrom(mr, SamplerSize, Round(mr.Height * k));
      if not Fast then
          GrayscaleBlur(N, SamplerSize / mr.Width, N.BoundsRect);
    end;

  SetLength(buff, N.Height, SamplerSize + 1);

  for i := 0 to N.Height - 1 do
    begin
      for J := 0 to SamplerSize - 1 do
          buff[i, J] := N.PixelGrayD[J, i];
      buff[i, SamplerSize] := i;
    end;

  rInfo := LDA(buff, N.Height, SamplerSize, SamplerSize + 1, output);

  case rInfo of
    1: sInfo := 'task is solved';
    2: sInfo := 'there was a multicollinearity in training set, but task has been solved';
    -1: sInfo := 'incorrect parameters was passed (NPoints<0, NVars<1, NClasses<2)';
    -2: sInfo := 'there is a point with class number outside of [0..NClasses-1]';
    -4: sInfo := 'internal EVD subroutine hasn''''t converged';
    else sInfo := 'unknow';
  end;

  Result := rInfo in [1, 2];

  SetLength(buff, 0, 0);
  DisposeObject(N);
end;

function PCA(const buff: TLMatrix; const NPoints, NVars: TLInt; var v: TLMatrix): TLInt;
var
  s: TLVec;
begin
  PCABuildBasis(buff, NPoints, NVars, Result, s, v);
  SetLength(s, 0);
end;

function PCA(const Fast: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster; var sInfo: string; var output: TLMatrix): Boolean;
var
  k        : TLFloat;
  mrblur, N: TMemoryRaster;
  i, J     : TLInt;
  buff     : TLMatrix;
  rInfo    : TLInt;
begin
  Result := False;

  k := SamplerSize / mr.Width;

  N := TMemoryRaster.Create;
  if (not Fast) and (k < 1.0) then
    begin
      mrblur := TMemoryRaster.Create;
      // preprocess liner zoom
      GrayscaleBlur(mr, mrblur, mr.Width / SamplerSize, mr.BoundsRect);
      // zoom
      N.ZoomFrom(mrblur, SamplerSize, Round(mrblur.Height * k));
      DisposeObject(mrblur);
    end
  else
    begin
      // zoom
      N.ZoomFrom(mr, SamplerSize, Round(mr.Height * k));
      if not Fast then
          GrayscaleBlur(N, SamplerSize / mr.Width, N.BoundsRect);
    end;

  SetLength(buff, N.Height, SamplerSize);

  for i := 0 to N.Height - 1 do
    for J := 0 to N.Width - 1 do
        buff[i, J] := N.PixelGrayD[J, i];

  rInfo := PCA(buff, N.Height, N.Width, output);

  case rInfo of
    1: sInfo := 'task is solved';
    -1: sInfo := 'wrong parameters has been passed (NPoints<0, NVars<1)';
    -4: sInfo := 'SVD subroutine haven''''t converged';
    else sInfo := 'unknow';
  end;

  Result := rInfo = 1;

  SetLength(buff, 0, 0);
  DisposeObject(N);
end;

function KMeans(const source: TKMFloat2DArray; const NVars, k: TLInt; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): Boolean;
var
  bakseed: TLInt;
begin
  bakseed := RandSeed;
  RandSeed := 0;

  try
      Result := KMeansCluster(source, NVars, k, 1, KArray, kIndex) = 1;
  finally
      RandSeed := bakseed;
  end;
end;
