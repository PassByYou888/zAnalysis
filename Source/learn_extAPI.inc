{$REGION 'extAPI'}


procedure LAdd(var f: TLFloat; const Value: TLFloat);
begin
  f := f + Value;
end;

procedure LSub(var f: TLFloat; const Value: TLFloat);
begin
  f := f - Value;
end;

procedure LMul(var f: TLFloat; const Value: TLFloat);
begin
  f := f * Value;
end;

procedure LDiv(var f: TLFloat; const Value: TLFloat);
begin
  f := LSafeDivF(f, Value);
end;

function LSafeDivF(const s, d: TLFloat): TLFloat;
begin
  Result := s / d;
  if IsNan(Result) then
      Result := 0;
end;

procedure LSetVec(var v: TLVec; const VDef: TLFloat);
var
  I: TLInt;
begin
  for I := 0 to Length(v) - 1 do
      v[I] := VDef;
end;

procedure LSetVec(var v: TLIVec; const VDef: TLInt);
var
  I: TLInt;
begin
  for I := 0 to Length(v) - 1 do
      v[I] := VDef;
end;

procedure LSetVec(var v: TLBVec; const VDef: Boolean);
var
  I: TLInt;
begin
  for I := 0 to Length(v) - 1 do
      v[I] := VDef;
end;

procedure LSetMatrix(var m: TLMatrix; const VDef: TLFloat);
var
  I: Integer;
begin
  for I := 0 to Length(m) - 1 do
      LSetVec(m[I], VDef);
end;

procedure LSetMatrix(var m: TLIMatrix; const VDef: TLInt);
var
  I: Integer;
begin
  for I := 0 to Length(m) - 1 do
      LSetVec(m[I], VDef);
end;

procedure LSetMatrix(var m: TLBMatrix; const VDef: Boolean);
var
  I: Integer;
begin
  for I := 0 to Length(m) - 1 do
      LSetVec(m[I], VDef);
end;

function LVecCopy(const v: TLVec): TLVec;
var
  I: Integer;
begin
  SetLength(Result, Length(v));
  for I := 0 to Length(v) - 1 do
      Result[I] := v[I];
end;

function LVecCopy(const v: TLIVec): TLIVec;
var
  I: Integer;
begin
  SetLength(Result, Length(v));
  for I := 0 to Length(v) - 1 do
      Result[I] := v[I];
end;

function LVecCopy(const v: TLBVec): TLBVec;
var
  I: Integer;
begin
  SetLength(Result, Length(v));
  for I := 0 to Length(v) - 1 do
      Result[I] := v[I];
end;

function LMatrixCopy(const v: TLMatrix): TLMatrix;
var
  I: Integer;
begin
  SetLength(Result, Length(v));
  for I := 0 to Length(v) - 1 do
      Result[I] := LVecCopy(v[I]);
end;

function LMatrixCopy(const v: TLIMatrix): TLIMatrix;
var
  I: Integer;
begin
  SetLength(Result, Length(v));
  for I := 0 to Length(v) - 1 do
      Result[I] := LVecCopy(v[I]);
end;

function LMatrixCopy(const v: TLBMatrix): TLBMatrix;
var
  I: Integer;
begin
  SetLength(Result, Length(v));
  for I := 0 to Length(v) - 1 do
      Result[I] := LVecCopy(v[I]);
end;

function LVec(const veclen: TLInt; const VDef: TLFloat): TLVec;
begin
  SetLength(Result, veclen);
  LSetVec(Result, VDef);
end;

function LVec(const veclen: TLInt): TLVec;
begin
  SetLength(Result, veclen);
end;

function LVec(const v: TLVec): TPascalString;
begin
  Result := TKDTree.KDTreeVec(v);
end;

function LVec(const m: TLMatrix; const veclen: TLInt): TLVec;
var
  I, J, id: TLInt;
begin
  id := 0;
  Result := LVec(veclen);
  for I := 0 to Length(m) - 1 do
    for J := 0 to Length(m[I]) - 1 do
      if id < veclen then
        begin
          Result[id] := m[I, J];
          inc(id);
        end
      else
          exit;
end;

function LVec(const m: TLMatrix): TLVec;
var
  I, l: TLInt;
begin
  l := 0;
  for I := 0 to Length(m) - 1 do
      inc(l, Length(m[I]));
  Result := LVec(m, l);
end;

function LVec(const s: TPascalString; const veclen: TLInt): TLVec;
var
  I: TLInt;
  v: TLVec;
begin
  v := TKDTree.KDTreeVec(s.Text);
  Result := LVec(veclen);
  for I := 0 to Length(v) - 1 do
    if I < veclen then
        Result[I] := v[I]
    else
        break;
end;

function LVec(const v: TLVec; const ShortFloat: Boolean): TPascalString;
var
  I: TLInt;
begin
  Result := '';
  for I := 0 to Length(v) - 1 do
    begin
      if I > 0 then
          Result.Append(' ');

      if ShortFloat then
          Result.Append(umlShortFloatToStr(v[I]))
      else
          Result.Append(umlFloatToStr(v[I]));
    end;
end;

function LVec(const m: TLBMatrix; const veclen: TLInt): TLBVec;
var
  I, J, id: TLInt;
begin
  id := 0;
  SetLength(Result, veclen);
  for I := 0 to Length(m) - 1 do
    for J := 0 to Length(m[I]) - 1 do
      if id < veclen then
        begin
          Result[id] := m[I, J];
          inc(id);
        end
      else
          exit;
end;

function LVec(const m: TLBMatrix): TLBVec;
var
  I, l: TLInt;
begin
  l := 0;
  for I := 0 to Length(m) - 1 do
      inc(l, Length(m[I]));
  Result := LVec(m, l);
end;

function LVec(const m: TLIMatrix; const veclen: TLInt): TLIVec;
var
  I, J, id: TLInt;
begin
  id := 0;
  SetLength(Result, veclen);
  for I := 0 to Length(m) - 1 do
    for J := 0 to Length(m[I]) - 1 do
      if id < veclen then
        begin
          Result[id] := m[I, J];
          inc(id);
        end
      else
          exit;
end;

function LVec(const m: TLIMatrix): TLIVec;
var
  I, l: TLInt;
begin
  l := 0;
  for I := 0 to Length(m) - 1 do
      inc(l, Length(m[I]));
  Result := LVec(m, l);
end;

function LSpearmanVec(const m: TLMatrix; const veclen: TLInt): TLVec;
begin
  Result := LVec(m, veclen);
  SpearmanRank(Result, Length(Result));
end;

function LAbsMaxVec(const v: TLVec): TLFloat;
var
  I: TLInt;
begin
  Result := AbsReal(v[0]);

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Max(Result, AbsReal(v[I]));
end;

function LMaxVec(const v: TLVec): TLFloat;
var
  I: TLInt;
begin
  Result := v[0];

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Max(Result, v[I]);
end;

function LMaxVec(const v: TLIVec): TLInt;
var
  I: TLInt;
begin
  Result := v[0];

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Max(Result, v[I]);
end;

function LMaxVec(const v: TLMatrix): TLFloat;
var
  I: TLInt;
begin
  Result := LMaxVec(v[0]);

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Max(Result, LMaxVec(v[I]));
end;

function LMaxVec(const v: TLIMatrix): TLInt;
var
  I: TLInt;
begin
  Result := LMaxVec(v[0]);

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Max(Result, LMaxVec(v[I]));
end;

function LMinVec(const v: TLVec): TLFloat;
var
  I: TLInt;
begin
  Result := v[0];

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Min(Result, v[I]);
end;

function LMinVec(const v: TLIVec): TLInt;
var
  I: TLInt;
begin
  Result := v[0];

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Min(Result, v[I]);
end;

function LMinVec(const v: TLMatrix): TLFloat;
var
  I: TLInt;
begin
  Result := LMinVec(v[0]);

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Min(Result, LMinVec(v[I]));
end;

function LMinVec(const v: TLIMatrix): TLInt;
var
  I: TLInt;
begin
  Result := LMinVec(v[0]);

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
        Result := Min(Result, LMinVec(v[I]));
end;

function LMaxVecIndex(const v: TLVec): TLInt;
var
  I: TLInt;
  k: TLFloat;
begin
  k := v[0];
  Result := 0;

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
      if v[I] > k then
        begin
          Result := I;
          k := v[I];
        end;
end;

function LMinVecIndex(const v: TLVec): TLInt;
var
  I: TLInt;
  k: TLFloat;
begin
  k := v[0];
  Result := 0;

  if Length(v) > 1 then
    for I := 1 to Length(v) - 1 do
      if v[I] < k then
        begin
          Result := I;
          k := v[I];
        end;
end;

function LDistance(const v1, v2: TLVec): TLFloat;
begin
  Result := TKDTree.KDTreeDistance(v1, v2);
end;

function LHamming(const v1, v2: TLVec): TLInt;
var
  I: TLInt;
  I1, I2: TLInt;
begin
  Result := 0;
  for I := 0 to Length(v1) - 1 do
    begin
      I1 := Trunc(v1[I]);
      I2 := Trunc(v2[I]);
      inc(Result, I1 xor I2);
    end;
end;

function LHamming(const v1, v2: TLIVec): TLInt;
var
  I: TLInt;
begin
  Result := 0;
  for I := 0 to Length(v1) - 1 do
      inc(Result, v1[I] xor v2[I]);
end;

procedure LClampF(var AValue: TLFloat; const AMin, AMax: TLFloat);
begin
  if AMin > AMax then
      LClampF(AValue, AMax, AMin)
  else if AValue > AMax then
      AValue := AMax
  else if AValue < AMin then
      AValue := AMin;
end;

procedure LClampI(var AValue: TLInt; const AMin, AMax: TLInt);
begin
  if AMin > AMax then
      LClampI(AValue, AMax, AMin)
  else if AValue > AMax then
      AValue := AMax
  else if AValue < AMin then
      AValue := AMin;
end;

function LClamp(const AValue: TLFloat; const AMin, AMax: TLFloat): TLFloat;
begin
  if AMin > AMax then
      Result := LClamp(AValue, AMax, AMin)
  else if AValue > AMax then
      Result := AMax
  else if AValue < AMin then
      Result := AMin
  else
      Result := AValue;
end;

function LClamp(const AValue: TLInt; const AMin, AMax: TLInt): TLInt;
begin
  if AMin > AMax then
      Result := LClamp(AValue, AMax, AMin)
  else if AValue > AMax then
      Result := AMax
  else if AValue < AMin then
      Result := AMin
  else
      Result := AValue;
end;

procedure LFeatureSamplerWithSift(const mr: TMemoryRaster; ln: TLearn; lOut: TLVec);
var
  f: TFeature;
  I: TLInt;
begin
  f := TFeature.CreateWithRaster(mr);
  for I := 0 to f.Count - 1 do
      ln.AddMemory(f[I]^.descriptor, lOut);
  disposeObject(f);
end;

function LMatrixSamplerWithHOG(const mr: TMemoryRaster): TLMatrix;
var
  nmr: TMemoryRaster;
  HOG: THOG;
begin
  nmr := TMemoryRaster.Create;
  nmr.NoLineZoomFrom(mr, SYSTEM_HOGCELLSIZE * (SYSTEM_HOGSAMPLERSIZE - 2), SYSTEM_HOGCELLSIZE * (SYSTEM_HOGSAMPLERSIZE - 2));

  HOG := THOG.CreateAntiLight(System_HOGTable, nmr);
  disposeObject(nmr);
  HOG.BuildFeatureMatrix(Result);
  disposeObject(HOG);
end;

function LMatrixSampler(const mr: TMemoryRaster): TLMatrix;
var
  I, J: Integer;
begin
  SetLength(Result, mr.Height, mr.Width);
  for J := 0 to mr.Height - 1 do
    for I := 0 to mr.Width - 1 do
        Result[J, I] := mr.PixelGrayD[I, J];
end;

function LMatrixSampler(const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix;
var
  N: TMemoryRaster;
  I, J: TLInt;
begin
  if (mr.Width = SamplerSize) and (mr.Height = SamplerSize) then
    begin
      SetLength(Result, SamplerSize, SamplerSize);
      for I := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, I] := N.PixelGrayD[I, J];
    end
  else
    begin
      N := TMemoryRaster.Create;
      N.ZoomFrom(mr, SamplerSize, SamplerSize);

      SetLength(Result, SamplerSize, SamplerSize);
      for I := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, I] := N.PixelGrayD[I, J];

      disposeObject(N);
    end;
end;

function LMatrixSampler(const Antialiasing: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix;
var
  N: TMemoryRaster;
  I, J: TLInt;
begin
  if (mr.Width = SamplerSize) and (mr.Height = SamplerSize) then
    begin
      SetLength(Result, SamplerSize, SamplerSize);
      for I := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, I] := mr.PixelGrayD[I, J];
    end
  else
    begin
      N := TMemoryRaster.Create;
      if Antialiasing then
          N.GaussianBlurZoomFrom(mr, SamplerSize, SamplerSize)
      else
          N.ZoomFrom(mr, SamplerSize, SamplerSize);

      SetLength(Result, SamplerSize, SamplerSize);
      for I := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, I] := N.PixelGrayD[I, J];

      disposeObject(N);
    end;
end;

procedure ZoomLineF(const source, dest: PLMatrix; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt);
var
  J: TLInt;
  SourceIInt, SourceJInt: TLInt;
begin
  for J := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(J / (DestHeight - 1) * (SourceHeight - 1));
      dest^[J, pass] := source^[SourceJInt, SourceIInt];
    end;
end;

procedure LZoomMatrix(var source, dest: TLMatrix; const DestWidth, DestHeight: TLInt);
var
  SourceWidth, SourceHeight: TLInt;
  SourceP, DestP: PLMatrix;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ZoomLineF(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: TLInt;
  begin
    for pass := 0 to DestWidth - 1 do
        ZoomLineF(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF parallel}


begin
  SourceWidth := Length(source[0]);
  SourceHeight := Length(source);
  SetLength(dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @source;
      DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
{$ELSE FPC}
      TParallel.For(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLineF(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure ZoomLineI(const source, dest: PLIMatrix; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt);
var
  J: TLInt;
  SourceIInt, SourceJInt: TLInt;
begin
  for J := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(J / (DestHeight - 1) * (SourceHeight - 1));
      dest^[J, pass] := source^[SourceJInt, SourceIInt];
    end;
end;

procedure LZoomMatrix(var source, dest: TLIMatrix; const DestWidth, DestHeight: TLInt);
var
  SourceWidth, SourceHeight: TLInt;
  SourceP, DestP: PLIMatrix;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ZoomLineI(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: TLInt;
  begin
    for pass := 0 to DestWidth - 1 do
        ZoomLineI(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF parallel}


begin
  SourceWidth := Length(source[0]);
  SourceHeight := Length(source);
  SetLength(dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @source;
      DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
{$ELSE FPC}
      TParallel.For(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLineI(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure ZoomLineB(const source, dest: PLBMatrix; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt);
var
  J: TLInt;
  SourceIInt, SourceJInt: TLInt;
begin
  for J := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(J / (DestHeight - 1) * (SourceHeight - 1));
      dest^[J, pass] := source^[SourceJInt, SourceIInt];
    end;
end;

procedure LZoomMatrix(var source, dest: TLBMatrix; const DestWidth, DestHeight: TLInt);
var
  SourceWidth, SourceHeight: TLInt;
  SourceP, DestP: PLBMatrix;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ZoomLineB(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: TLInt;
  begin
    for pass := 0 to DestWidth - 1 do
        ZoomLineB(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF parallel}


begin
  SourceWidth := Length(source[0]);
  SourceHeight := Length(source);
  SetLength(dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @source;
      DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
{$ELSE FPC}
      TParallel.For(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLineB(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure LSampler(var source: TLBMatrix; const dest: TMemoryRaster);
var
  I, J: TLInt;
begin
  dest.SetSize(Length(source[0]), Length(source));
  for J := 0 to dest.Height - 1 do
    for I := 0 to dest.Width - 1 do
        dest.PixelGray[I, J] := IfThen(source[J, I], 255, 0);
end;

procedure LSampler(var source: TLMatrix; const MaxF: TLFloat; const dest: TMemoryRaster);
var
  I, J: TLInt;
begin
  dest.SetSize(Length(source[0]), Length(source));
  for J := 0 to dest.Height - 1 do
    for I := 0 to dest.Width - 1 do
        dest.PixelGrayD[I, J] := Round(source[J, I] / MaxF * 255);
end;

procedure LSampler(var source: TLMatrix; const dest: TMemoryRaster);
begin
  LSampler(source, LMaxVec(source), dest);
end;

procedure LSampler(var source: TLIMatrix; const MaxI: TLInt; const dest: TMemoryRaster);
var
  I, J: TLInt;
begin
  dest.SetSize(Length(source[0]), Length(source));
  for J := 0 to dest.Height - 1 do
    for I := 0 to dest.Width - 1 do
        dest.PixelGrayD[I, J] := Round(source[J, I] / MaxI * 255);
end;

procedure LSampler(var source: TLIMatrix; const dest: TMemoryRaster);
begin
  LSampler(source, LMaxVec(source), dest);
end;

procedure LSaveMatrix(var source: TLMatrix; dest: TCoreClassStream);
var
  J: TLInt;
  W, H: TLInt;
begin
  H := Length(source);
  W := Length(source[0]);
  dest.Write(H, SizeOf(TLInt));
  dest.Write(W, SizeOf(TLInt));

  for J := 0 to H - 1 do
      dest.Write(source[J][0], W * SizeOf(TLFloat));
end;

procedure LLoadMatrix(source: TCoreClassStream; var dest: TLMatrix);
var
  J: TLInt;
  W, H: TLInt;
begin
  source.Read(H, SizeOf(TLInt));
  source.Read(W, SizeOf(TLInt));
  SetLength(dest, H, W);

  for J := 0 to H - 1 do
      source.Read(dest[J][0], W * SizeOf(TLFloat));
end;

procedure LSaveMatrix(var source: TLIMatrix; dest: TCoreClassStream);
var
  J: TLInt;
  W, H: TLInt;
begin
  H := Length(source);
  W := Length(source[0]);
  dest.Write(H, SizeOf(TLInt));
  dest.Write(W, SizeOf(TLInt));

  for J := 0 to H - 1 do
      dest.Write(source[J][0], W * SizeOf(TLInt));
end;

procedure LLoadMatrix(source: TCoreClassStream; var dest: TLIMatrix);
var
  J: TLInt;
  W, H: TLInt;
begin
  source.Read(H, SizeOf(TLInt));
  source.Read(W, SizeOf(TLInt));
  SetLength(dest, H, W);

  for J := 0 to H - 1 do
      source.Read(dest[J][0], W * SizeOf(TLInt));
end;

procedure LSaveMatrix(var source: TLBMatrix; dest: TCoreClassStream);
var
  J: TLInt;
  W, H: TLInt;
begin
  H := Length(source);
  W := Length(source[0]);
  dest.Write(H, SizeOf(TLInt));
  dest.Write(W, SizeOf(TLInt));

  for J := 0 to H - 1 do
      dest.Write(source[J][0], W);
end;

procedure LLoadMatrix(source: TCoreClassStream; var dest: TLBMatrix);
var
  J: TLInt;
  W, H: TLInt;
begin
  source.Read(H, SizeOf(TLInt));
  source.Read(W, SizeOf(TLInt));
  SetLength(dest, H, W);

  for J := 0 to H - 1 do
      source.Read(dest[J][0], W);
end;

procedure LSaveSampler(var source: TLBMatrix; const fn: SystemString);
var
  m: TMemoryRaster;
begin
  m := NewRaster();
  LSampler(source, m);
  SaveRaster(m, fn);
  disposeObject(m);
end;

procedure LSaveSampler(var source: TLIMatrix; const fn: SystemString);
var
  m: TMemoryRaster;
begin
  m := NewRaster();
  LSampler(source, m);
  SaveRaster(m, fn);
  disposeObject(m);
end;

procedure LSaveSampler(var source: TLIMatrix; const MaxI: TLInt; const fn: SystemString);
var
  m: TMemoryRaster;
begin
  m := NewRaster();
  LSampler(source, MaxI, m);
  SaveRaster(m, fn);
  disposeObject(m);
end;

procedure LSaveSampler(var source: TLMatrix; const fn: SystemString);
var
  m: TMemoryRaster;
begin
  m := NewRaster();
  LSampler(source, m);
  SaveRaster(m, fn);
  disposeObject(m);
end;

procedure LSaveSampler(var source: TLMatrix; const MaxF: TLFloat; const fn: SystemString);
var
  m: TMemoryRaster;
begin
  m := NewRaster();
  LSampler(source, MaxF, m);
  SaveRaster(m, fn);
  disposeObject(m);
end;

function LDA(const m: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLMatrix): Boolean;
var
  rInfo: TLInt;
  buff: TLMatrix;
  W, H, cl, J: TLInt;
begin
  H := Length(m);
  W := Length(m[0]);
  cl := Length(cv);
  SetLength(buff, H, W + 1);
  for J := 0 to Length(m) - 1 do
    begin
      CopyPtr(@m[J][0], @buff[J][0], W * SizeOf(TLFloat));
      buff[J][W] := cv[J];
    end;

  FisherLDAN(buff, H, W, Nclass, rInfo, output);

  SetLength(buff, 0, 0);

  case rInfo of
    1: sInfo := 'task is solved';
    2: sInfo := 'there was a multicollinearity in training set, but task has been solved';
    -1: sInfo := 'incorrect parameters was passed (NPoints<0, NVars<1, NClasses<2)';
    -2: sInfo := 'there is a point with class number outside of [0..NClasses-1]';
    -4: sInfo := 'internal EVD subroutine hasn''''t converged';
    else sInfo := 'unknow';
  end;

  Result := rInfo in [1, 2];
end;

function LDA(const m: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLVec): Boolean;
var
  rInfo: TLInt;
  buff: TLMatrix;
  W, H, cl, J: TLInt;
begin
  H := Length(m);
  W := Length(m[0]);
  cl := Length(cv);
  SetLength(buff, H, W + 1);
  for J := 0 to Length(m) - 1 do
    begin
      CopyPtr(@m[J][0], @buff[J][0], W * SizeOf(TLFloat));
      buff[J][W] := cv[J];
    end;

  FisherLDA(buff, H, W, Nclass + 1, rInfo, output);

  SetLength(buff, 0, 0);

  case rInfo of
    1: sInfo := 'task is solved';
    2: sInfo := 'there was a multicollinearity in training set, but task has been solved';
    -1: sInfo := 'incorrect parameters was passed (NPoints<0, NVars<1, NClasses<2)';
    -2: sInfo := 'there is a point with class number outside of [0..NClasses-1]';
    -4: sInfo := 'internal EVD subroutine hasn''''t converged';
    else sInfo := 'unknow';
  end;

  Result := rInfo in [1, 2];
end;

function LDA(const Fast: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster; var sInfo: SystemString; var output: TLMatrix): Boolean;
var
  k: TLFloat;
  mrblur, N: TMemoryRaster;
  I, J: TLInt;
  buff: TLMatrix;
  rInfo: TLInt;
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
      disposeObject(mrblur);
    end
  else
    begin
      // zoom
      N.ZoomFrom(mr, SamplerSize, Round(mr.Height * k));
      if not Fast then
          GrayscaleBlur(N, SamplerSize / mr.Width, N.BoundsRect);
    end;

  SetLength(buff, N.Height, SamplerSize + 1);

  for I := 0 to N.Height - 1 do
    begin
      for J := 0 to SamplerSize - 1 do
          buff[I, J] := N.PixelGrayD[J, I];
      buff[I, SamplerSize] := I;
    end;

  FisherLDAN(buff, N.Height, SamplerSize, SamplerSize + 1, rInfo, output);

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
  disposeObject(N);
end;

function PCA(const buff: TLMatrix; const NPoints, NVars: TLInt; var v: TLMatrix): TLInt;
var
  s: TLVec;
begin
  PCABuildBasis(buff, NPoints, NVars, Result, s, v);
  SetLength(s, 0);
end;

function PCA(const Fast: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster; var sInfo: SystemString; var output: TLMatrix): Boolean;
var
  k: TLFloat;
  mrblur, N: TMemoryRaster;
  I, J: TLInt;
  buff: TLMatrix;
  rInfo: TLInt;
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
      disposeObject(mrblur);
    end
  else
    begin
      // zoom
      N.ZoomFrom(mr, SamplerSize, Round(mr.Height * k));
      if not Fast then
          GrayscaleBlur(N, SamplerSize / mr.Width, N.BoundsRect);
    end;

  SetLength(buff, N.Height, SamplerSize);

  for I := 0 to N.Height - 1 do
    for J := 0 to N.Width - 1 do
        buff[I, J] := N.PixelGrayD[J, I];

  rInfo := PCA(buff, N.Height, N.Width, output);

  case rInfo of
    1: sInfo := 'task is solved';
    -1: sInfo := 'wrong parameters has been passed (NPoints<0, NVars<1)';
    -4: sInfo := 'SVD subroutine haven''''t converged';
    else sInfo := 'unknow';
  end;

  Result := rInfo = 1;

  SetLength(buff, 0, 0);
  disposeObject(N);
end;

function KMeans(const source: TLMatrix; const NVars, k: TLInt; var KArray: TLMatrix; var kIndex: TLIVec): Boolean;
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

function LMatrix(const l1, l2: TLInt): TLMatrix;
begin
  SetLength(Result, l1, l2);
end;

function LBMatrix(const l1, l2: TLInt): TLBMatrix;
begin
  SetLength(Result, l1, l2);
end;

function LIMatrix(const l1, l2: TLInt): TLIMatrix;
begin
  SetLength(Result, l1, l2);
end;

{$ENDREGION 'extAPI'}
