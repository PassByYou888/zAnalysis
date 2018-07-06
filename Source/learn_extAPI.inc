{$REGION 'extAPI'}


procedure LAdd(var F: TLFloat; const Value: TLFloat);
begin
  F := F + Value;
end;

procedure LSub(var F: TLFloat; const Value: TLFloat);
begin
  F := F - Value;
end;

procedure LMul(var F: TLFloat; const Value: TLFloat);
begin
  F := F * Value;
end;

procedure LDiv(var F: TLFloat; const Value: TLFloat);
begin
  F := LSafeDivF(F, Value);
end;

function LSafeDivF(const s, d: TLFloat): TLFloat;
begin
  Result := s / d;
  if IsNan(Result) then
      Result := 0;
end;

procedure LSetVec(var v: TLVec; const VDef: TLFloat);
var
  i: TLInt;
begin
  for i := 0 to length(v) - 1 do
      v[i] := VDef;
end;

procedure LSetVec(var v: TLIVec; const VDef: TLInt);
var
  i: TLInt;
begin
  for i := 0 to length(v) - 1 do
      v[i] := VDef;
end;

procedure LSetVec(var v: TLBVec; const VDef: Boolean);
var
  i: TLInt;
begin
  for i := 0 to length(v) - 1 do
      v[i] := VDef;
end;

procedure LSetMatrix(var M: TLMatrix; const VDef: TLFloat);
var
  i: Integer;
begin
  for i := 0 to length(M) - 1 do
      LSetVec(M[i], VDef);
end;

procedure LSetMatrix(var M: TLIMatrix; const VDef: TLInt);
var
  i: Integer;
begin
  for i := 0 to length(M) - 1 do
      LSetVec(M[i], VDef);
end;

procedure LSetMatrix(var M: TLBMatrix; const VDef: Boolean);
var
  i: Integer;
begin
  for i := 0 to length(M) - 1 do
      LSetVec(M[i], VDef);
end;

function LVecCopy(const v: TLVec): TLVec;
var
  i: Integer;
begin
  SetLength(Result, length(v));
  for i := 0 to length(v) - 1 do
      Result[i] := v[i];
end;

function LVecCopy(const v: TLIVec): TLIVec;
var
  i: Integer;
begin
  SetLength(Result, length(v));
  for i := 0 to length(v) - 1 do
      Result[i] := v[i];
end;

function LVecCopy(const v: TLBVec): TLBVec;
var
  i: Integer;
begin
  SetLength(Result, length(v));
  for i := 0 to length(v) - 1 do
      Result[i] := v[i];
end;

function LMatrixCopy(const v: TLMatrix): TLMatrix;
var
  i: Integer;
begin
  SetLength(Result, length(v));
  for i := 0 to length(v) - 1 do
      Result[i] := LVecCopy(v[i]);
end;

function LMatrixCopy(const v: TLIMatrix): TLIMatrix;
var
  i: Integer;
begin
  SetLength(Result, length(v));
  for i := 0 to length(v) - 1 do
      Result[i] := LVecCopy(v[i]);
end;

function LMatrixCopy(const v: TLBMatrix): TLBMatrix;
var
  i: Integer;
begin
  SetLength(Result, length(v));
  for i := 0 to length(v) - 1 do
      Result[i] := LVecCopy(v[i]);
end;

function lvec(const veclen: TLInt; const VDef: TLFloat): TLVec;
begin
  SetLength(Result, veclen);
  LSetVec(Result, VDef);
end;

function lvec(const veclen: TLInt): TLVec;
begin
  SetLength(Result, veclen);
end;

function lvec(const v: TLVec): TPascalString;
begin
  Result := TKDTree.KDTreeVec(v);
end;

function lvec(const M: TLMatrix; const veclen: TLInt): TLVec;
var
  i, J, ID: TLInt;
begin
  ID := 0;
  Result := lvec(veclen);
  for i := 0 to length(M) - 1 do
    for J := 0 to length(M[i]) - 1 do
      if ID < veclen then
        begin
          Result[ID] := M[i, J];
          Inc(ID);
        end
      else
          Exit;
end;

function lvec(const M: TLMatrix): TLVec;
var
  i, L: TLInt;
begin
  L := 0;
  for i := 0 to length(M) - 1 do
      Inc(L, length(M[i]));
  Result := lvec(M, L);
end;

function lvec(const s: TPascalString; const veclen: TLInt): TLVec;
var
  i: TLInt;
  v: TLVec;
begin
  v := TKDTree.KDTreeVec(s.Text);
  Result := lvec(veclen);
  for i := 0 to length(v) - 1 do
    if i < veclen then
        Result[i] := v[i]
    else
        Break;
end;

function lvec(const v: TLVec; const ShortFloat: Boolean): TPascalString;
var
  i: TLInt;
begin
  Result := '';
  for i := 0 to length(v) - 1 do
    begin
      if i > 0 then
          Result.Append(' ');

      if ShortFloat then
          Result.Append(umlShortFloatToStr(v[i]))
      else
          Result.Append(umlFloatToStr(v[i]));
    end;
end;

function lvec(const M: TLBMatrix; const veclen: TLInt): TLBVec;
var
  i, J, ID: TLInt;
begin
  ID := 0;
  SetLength(Result, veclen);
  for i := 0 to length(M) - 1 do
    for J := 0 to length(M[i]) - 1 do
      if ID < veclen then
        begin
          Result[ID] := M[i, J];
          Inc(ID);
        end
      else
          Exit;
end;

function lvec(const M: TLBMatrix): TLBVec;
var
  i, L: TLInt;
begin
  L := 0;
  for i := 0 to length(M) - 1 do
      Inc(L, length(M[i]));
  Result := lvec(M, L);
end;

function lvec(const M: TLIMatrix; const veclen: TLInt): TLIVec;
var
  i, J, ID: TLInt;
begin
  ID := 0;
  SetLength(Result, veclen);
  for i := 0 to length(M) - 1 do
    for J := 0 to length(M[i]) - 1 do
      if ID < veclen then
        begin
          Result[ID] := M[i, J];
          Inc(ID);
        end
      else
          Exit;
end;

function lvec(const M: TLIMatrix): TLIVec;
var
  i, L: TLInt;
begin
  L := 0;
  for i := 0 to length(M) - 1 do
      Inc(L, length(M[i]));
  Result := lvec(M, L);
end;

function LSpearmanVec(const M: TLMatrix; const veclen: TLInt): TLVec;
begin
  Result := lvec(M, veclen);
  SpearmanRank(Result, length(Result));
end;

function LAbsMaxVec(const v: TLVec): TLFloat;
var
  i: TLInt;
begin
  Result := AbsReal(v[0]);

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Max(Result, AbsReal(v[i]));
end;

function LMaxVec(const v: TLVec): TLFloat;
var
  i: TLInt;
begin
  Result := v[0];

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Max(Result, v[i]);
end;

function LMaxVec(const v: TLIVec): TLInt;
var
  i: TLInt;
begin
  Result := v[0];

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Max(Result, v[i]);
end;

function LMaxVec(const v: TLMatrix): TLFloat;
var
  i: TLInt;
begin
  Result := LMaxVec(v[0]);

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Max(Result, LMaxVec(v[i]));
end;

function LMaxVec(const v: TLIMatrix): TLInt;
var
  i: TLInt;
begin
  Result := LMaxVec(v[0]);

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Max(Result, LMaxVec(v[i]));
end;

function LMinVec(const v: TLVec): TLFloat;
var
  i: TLInt;
begin
  Result := v[0];

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Min(Result, v[i]);
end;

function LMinVec(const v: TLIVec): TLInt;
var
  i: TLInt;
begin
  Result := v[0];

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Min(Result, v[i]);
end;

function LMinVec(const v: TLMatrix): TLFloat;
var
  i: TLInt;
begin
  Result := LMinVec(v[0]);

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Min(Result, LMinVec(v[i]));
end;

function LMinVec(const v: TLIMatrix): TLInt;
var
  i: TLInt;
begin
  Result := LMinVec(v[0]);

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
        Result := Min(Result, LMinVec(v[i]));
end;

function LMaxVecIndex(const v: TLVec): TLInt;
var
  i: TLInt;
  k: TLFloat;
begin
  k := v[0];
  Result := 0;

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
      if v[i] > k then
        begin
          Result := i;
          k := v[i];
        end;
end;

function LMinVecIndex(const v: TLVec): TLInt;
var
  i: TLInt;
  k: TLFloat;
begin
  k := v[0];
  Result := 0;

  if length(v) > 1 then
    for i := 1 to length(v) - 1 do
      if v[i] < k then
        begin
          Result := i;
          k := v[i];
        end;
end;

function LDistance(const v1, v2: TLVec): TLFloat;
begin
  Result := TKDTree.KDTreeDistance(v1, v2);
end;

function LHamming(const v1, v2: TLVec): TLInt;
var
  i: TLInt;
  i1, i2: TLInt;
begin
  Result := 0;
  for i := 0 to length(v1) - 1 do
    begin
      i1 := Trunc(v1[i]);
      i2 := Trunc(v2[i]);
      Inc(Result, i1 xor i2);
    end;
end;

function LHamming(const v1, v2: TLIVec): TLInt;
var
  i: TLInt;
begin
  Result := 0;
  for i := 0 to length(v1) - 1 do
      Inc(Result, v1[i] xor v2[i]);
end;

procedure LClampF(var AValue: TLFloat; const aMin, aMax: TLFloat);
begin
  if aMin > aMax then
      LClampF(AValue, aMax, aMin)
  else if AValue > aMax then
      AValue := aMax
  else if AValue < aMin then
      AValue := aMin;
end;

procedure LClampI(var AValue: TLInt; const aMin, aMax: TLInt);
begin
  if aMin > aMax then
      LClampI(AValue, aMax, aMin)
  else if AValue > aMax then
      AValue := aMax
  else if AValue < aMin then
      AValue := aMin;
end;

function LClamp(const AValue: TLFloat; const aMin, aMax: TLFloat): TLFloat;
begin
  if aMin > aMax then
      Result := LClamp(AValue, aMax, aMin)
  else if AValue > aMax then
      Result := aMax
  else if AValue < aMin then
      Result := aMin
  else
      Result := AValue;
end;

function LClamp(const AValue: TLInt; const aMin, aMax: TLInt): TLInt;
begin
  if aMin > aMax then
      Result := LClamp(AValue, aMax, aMin)
  else if AValue > aMax then
      Result := aMax
  else if AValue < aMin then
      Result := aMin
  else
      Result := AValue;
end;

procedure LFeatureSamplerWithSift(const mr: TMemoryRaster; ln: TLearn; lOut: TLVec);
var
  F: TFeature;
  i: TLInt;
begin
  F := TFeature.CreateWithRaster(mr);
  for i := 0 to F.Count - 1 do
      ln.AddMemory(F[i]^.descriptor, lOut);
  DisposeObject(F);
end;

function LMatrixSamplerWithHOG(const mr: TMemoryRaster): TLMatrix;
var
  nmr: TMemoryRaster;
  HOG: THOG;
begin
  nmr := TMemoryRaster.Create;
  nmr.NoLineZoomFrom(mr, SYSTEM_HOGCELLSIZE * (SYSTEM_HOGSAMPLERSIZE - 2), SYSTEM_HOGCELLSIZE * (SYSTEM_HOGSAMPLERSIZE - 2));

  HOG := THOG.CreateAntiLight(System_HOGTable, nmr);
  DisposeObject(nmr);
  HOG.BuildFeatureMatrix(Result);
  DisposeObject(HOG);
end;

function LMatrixSampler(const mr: TMemoryRaster): TLMatrix;
var
  i, J: Integer;
begin
  SetLength(Result, mr.height, mr.width);
  for J := 0 to mr.height - 1 do
    for i := 0 to mr.width - 1 do
        Result[J, i] := mr.PixelGrayD[i, J];
end;

function LMatrixSampler(const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix;
var
  n: TMemoryRaster;
  i, J: TLInt;
begin
  if (mr.width = SamplerSize) and (mr.height = SamplerSize) then
    begin
      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, i] := n.PixelGrayD[i, J];
    end
  else
    begin
      n := TMemoryRaster.Create;
      n.ZoomFrom(mr, SamplerSize, SamplerSize);

      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, i] := n.PixelGrayD[i, J];

      DisposeObject(n);
    end;
end;

function LMatrixSampler(const Antialiasing: Boolean; const SamplerSize: TLInt; const mr: TMemoryRaster): TLMatrix;
var
  n: TMemoryRaster;
  i, J: TLInt;
begin
  if (mr.width = SamplerSize) and (mr.height = SamplerSize) then
    begin
      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, i] := mr.PixelGrayD[i, J];
    end
  else
    begin
      n := TMemoryRaster.Create;
      if Antialiasing then
          n.GaussianBlurZoomFrom(mr, SamplerSize, SamplerSize)
      else
          n.ZoomFrom(mr, SamplerSize, SamplerSize);

      SetLength(Result, SamplerSize, SamplerSize);
      for i := 0 to SamplerSize - 1 do
        for J := 0 to SamplerSize - 1 do
            Result[J, i] := n.PixelGrayD[i, J];

      DisposeObject(n);
    end;
end;

procedure ZoomLineF(const Source, dest: PLMatrix; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt);
var
  J: TLInt;
  SourceIInt, SourceJInt: TLInt;
begin
  for J := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(J / (DestHeight - 1) * (SourceHeight - 1));
      dest^[J, pass] := Source^[SourceJInt, SourceIInt];
    end;
end;

procedure LZoomMatrix(var Source, dest: TLMatrix; const DestWidth, DestHeight: TLInt);
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
  SourceWidth := length(Source[0]);
  SourceHeight := length(Source);
  SetLength(dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @Source;
      DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
{$ELSE FPC}
      TParallel.for(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLineF(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure ZoomLineI(const Source, dest: PLIMatrix; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt);
var
  J: TLInt;
  SourceIInt, SourceJInt: TLInt;
begin
  for J := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(J / (DestHeight - 1) * (SourceHeight - 1));
      dest^[J, pass] := Source^[SourceJInt, SourceIInt];
    end;
end;

procedure LZoomMatrix(var Source, dest: TLIMatrix; const DestWidth, DestHeight: TLInt);
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
  SourceWidth := length(Source[0]);
  SourceHeight := length(Source);
  SetLength(dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @Source;
      DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
{$ELSE FPC}
      TParallel.for(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLineI(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure ZoomLineB(const Source, dest: PLBMatrix; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt);
var
  J: TLInt;
  SourceIInt, SourceJInt: TLInt;
begin
  for J := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(J / (DestHeight - 1) * (SourceHeight - 1));
      dest^[J, pass] := Source^[SourceJInt, SourceIInt];
    end;
end;

procedure LZoomMatrix(var Source, dest: TLBMatrix; const DestWidth, DestHeight: TLInt);
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
  SourceWidth := length(Source[0]);
  SourceHeight := length(Source);
  SetLength(dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @Source;
      DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
{$ELSE FPC}
      TParallel.for(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLineB(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure LSampler(var Source: TLBMatrix; const dest: TMemoryRaster);
var
  i, J: TLInt;
begin
  dest.SetSize(length(Source[0]), length(Source));
  for J := 0 to dest.height - 1 do
    for i := 0 to dest.width - 1 do
        dest.PixelGray[i, J] := IfThen(Source[J, i], 255, 0);
end;

procedure LSampler(var Source: TLMatrix; const MaxF: TLFloat; const dest: TMemoryRaster);
var
  i, J: TLInt;
begin
  dest.SetSize(length(Source[0]), length(Source));
  for J := 0 to dest.height - 1 do
    for i := 0 to dest.width - 1 do
        dest.PixelGrayD[i, J] := Round(Source[J, i] / MaxF * 255);
end;

procedure LSampler(var Source: TLMatrix; const dest: TMemoryRaster);
begin
  LSampler(Source, LMaxVec(Source), dest);
end;

procedure LSampler(var Source: TLIMatrix; const MaxI: TLInt; const dest: TMemoryRaster);
var
  i, J: TLInt;
begin
  dest.SetSize(length(Source[0]), length(Source));
  for J := 0 to dest.height - 1 do
    for i := 0 to dest.width - 1 do
        dest.PixelGrayD[i, J] := Round(Source[J, i] / MaxI * 255);
end;

procedure LSampler(var Source: TLIMatrix; const dest: TMemoryRaster);
begin
  LSampler(Source, LMaxVec(Source), dest);
end;

procedure LSaveMatrix(var Source: TLMatrix; dest: TCoreClassStream);
var
  J: TLInt;
  w, h: TLInt;
begin
  h := length(Source);
  w := length(Source[0]);
  dest.write(h, SizeOf(TLInt));
  dest.write(w, SizeOf(TLInt));

  for J := 0 to h - 1 do
      dest.write(Source[J][0], w * SizeOf(TLFloat));
end;

procedure LLoadMatrix(Source: TCoreClassStream; var dest: TLMatrix);
var
  J: TLInt;
  w, h: TLInt;
begin
  Source.read(h, SizeOf(TLInt));
  Source.read(w, SizeOf(TLInt));
  SetLength(dest, h, w);

  for J := 0 to h - 1 do
      Source.read(dest[J][0], w * SizeOf(TLFloat));
end;

procedure LSaveMatrix(var Source: TLIMatrix; dest: TCoreClassStream);
var
  J: TLInt;
  w, h: TLInt;
begin
  h := length(Source);
  w := length(Source[0]);
  dest.write(h, SizeOf(TLInt));
  dest.write(w, SizeOf(TLInt));

  for J := 0 to h - 1 do
      dest.write(Source[J][0], w * SizeOf(TLInt));
end;

procedure LLoadMatrix(Source: TCoreClassStream; var dest: TLIMatrix);
var
  J: TLInt;
  w, h: TLInt;
begin
  Source.read(h, SizeOf(TLInt));
  Source.read(w, SizeOf(TLInt));
  SetLength(dest, h, w);

  for J := 0 to h - 1 do
      Source.read(dest[J][0], w * SizeOf(TLInt));
end;

procedure LSaveMatrix(var Source: TLBMatrix; dest: TCoreClassStream);
var
  J: TLInt;
  w, h: TLInt;
begin
  h := length(Source);
  w := length(Source[0]);
  dest.write(h, SizeOf(TLInt));
  dest.write(w, SizeOf(TLInt));

  for J := 0 to h - 1 do
      dest.write(Source[J][0], w);
end;

procedure LLoadMatrix(Source: TCoreClassStream; var dest: TLBMatrix);
var
  J: TLInt;
  w, h: TLInt;
begin
  Source.read(h, SizeOf(TLInt));
  Source.read(w, SizeOf(TLInt));
  SetLength(dest, h, w);

  for J := 0 to h - 1 do
      Source.read(dest[J][0], w);
end;

procedure LSaveSampler(var Source: TLBMatrix; const fn: SystemString);
var
  M: TMemoryRaster;
begin
  M := NewRaster();
  LSampler(Source, M);
  SaveRaster(M, fn);
  DisposeObject(M);
end;

procedure LSaveSampler(var Source: TLIMatrix; const fn: SystemString);
var
  M: TMemoryRaster;
begin
  M := NewRaster();
  LSampler(Source, M);
  SaveRaster(M, fn);
  DisposeObject(M);
end;

procedure LSaveSampler(var Source: TLIMatrix; const MaxI: TLInt; const fn: SystemString);
var
  M: TMemoryRaster;
begin
  M := NewRaster();
  LSampler(Source, MaxI, M);
  SaveRaster(M, fn);
  DisposeObject(M);
end;

procedure LSaveSampler(var Source: TLMatrix; const fn: SystemString);
var
  M: TMemoryRaster;
begin
  M := NewRaster();
  LSampler(Source, M);
  SaveRaster(M, fn);
  DisposeObject(M);
end;

procedure LSaveSampler(var Source: TLMatrix; const MaxF: TLFloat; const fn: SystemString);
var
  M: TMemoryRaster;
begin
  M := NewRaster();
  LSampler(Source, MaxF, M);
  SaveRaster(M, fn);
  DisposeObject(M);
end;

function LDA(const M: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLMatrix): Boolean;
var
  rInfo: TLInt;
  buff: TLMatrix;
  w, h, cl, J: TLInt;
begin
  h := length(M);
  w := length(M[0]);
  cl := length(cv);
  SetLength(buff, h, w + 1);
  for J := 0 to length(M) - 1 do
    begin
      CopyPtr(@M[J][0], @buff[J][0], w * SizeOf(TLFloat));
      buff[J][w] := cv[J];
    end;

  FisherLDAN(buff, h, w, Nclass, rInfo, output);

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

function LDA(const M: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLVec): Boolean;
var
  rInfo: TLInt;
  buff: TLMatrix;
  w, h, cl, J: TLInt;
begin
  h := length(M);
  w := length(M[0]);
  cl := length(cv);
  SetLength(buff, h, w + 1);
  for J := 0 to length(M) - 1 do
    begin
      CopyPtr(@M[J][0], @buff[J][0], w * SizeOf(TLFloat));
      buff[J][w] := cv[J];
    end;

  FisherLDA(buff, h, w, Nclass + 1, rInfo, output);

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
  mrblur, n: TMemoryRaster;
  i, J: TLInt;
  buff: TLMatrix;
  rInfo: TLInt;
begin
  k := SamplerSize / mr.width;

  n := TMemoryRaster.Create;
  if (not Fast) and (k < 1.0) then
    begin
      mrblur := TMemoryRaster.Create;
      // preprocess liner zoom
      GrayscaleBlur(mr, mrblur, mr.width / SamplerSize, mr.BoundsRect);
      // zoom
      n.ZoomFrom(mrblur, SamplerSize, Round(mrblur.height * k));
      DisposeObject(mrblur);
    end
  else
    begin
      // zoom
      n.ZoomFrom(mr, SamplerSize, Round(mr.height * k));
      if not Fast then
          GrayscaleBlur(n, SamplerSize / mr.width, n.BoundsRect);
    end;

  SetLength(buff, n.height, SamplerSize + 1);

  for i := 0 to n.height - 1 do
    begin
      for J := 0 to SamplerSize - 1 do
          buff[i, J] := n.PixelGrayD[J, i];
      buff[i, SamplerSize] := i;
    end;

  FisherLDAN(buff, n.height, SamplerSize, SamplerSize + 1, rInfo, output);

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
  DisposeObject(n);
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
  mrblur, n: TMemoryRaster;
  i, J: TLInt;
  buff: TLMatrix;
  rInfo: TLInt;
begin
  Result := False;

  k := SamplerSize / mr.width;

  n := TMemoryRaster.Create;
  if (not Fast) and (k < 1.0) then
    begin
      mrblur := TMemoryRaster.Create;
      // preprocess liner zoom
      GrayscaleBlur(mr, mrblur, mr.width / SamplerSize, mr.BoundsRect);
      // zoom
      n.ZoomFrom(mrblur, SamplerSize, Round(mrblur.height * k));
      DisposeObject(mrblur);
    end
  else
    begin
      // zoom
      n.ZoomFrom(mr, SamplerSize, Round(mr.height * k));
      if not Fast then
          GrayscaleBlur(n, SamplerSize / mr.width, n.BoundsRect);
    end;

  SetLength(buff, n.height, SamplerSize);

  for i := 0 to n.height - 1 do
    for J := 0 to n.width - 1 do
        buff[i, J] := n.PixelGrayD[J, i];

  rInfo := PCA(buff, n.height, n.width, output);

  case rInfo of
    1: sInfo := 'task is solved';
    -1: sInfo := 'wrong parameters has been passed (NPoints<0, NVars<1)';
    -4: sInfo := 'SVD subroutine haven''''t converged';
    else sInfo := 'unknow';
  end;

  Result := rInfo = 1;

  SetLength(buff, 0, 0);
  DisposeObject(n);
end;

function KMeans(const Source: TLMatrix; const NVars, k: TLInt; var KArray: TLMatrix; var kIndex: TLIVec): Boolean;
var
  bakseed: TLInt;
begin
  bakseed := RandSeed;
  RandSeed := 0;

  try
      Result := KMeansCluster(Source, NVars, k, 1, KArray, kIndex) = 1;
  finally
      RandSeed := bakseed;
  end;
end;

function LMatrix(const L1, l2: TLInt): TLMatrix;
begin
  SetLength(Result, L1, l2);
end;

function LBMatrix(const L1, l2: TLInt): TLBMatrix;
begin
  SetLength(Result, L1, l2);
end;

function LIMatrix(const L1, l2: TLInt): TLIMatrix;
begin
  SetLength(Result, L1, l2);
end;

{$ENDREGION 'extAPI'} 
