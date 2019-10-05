{ ****************************************************************************** }
{ * memory Rasterization                                                       * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
constructor TRCLines.Create;
begin
  inherited Create;
end;

class function TRCLines.BuildLines(raster: TMemoryRaster; DetectorColor: TRColor; Threshold: TGeoFloat; MinLineLength: Integer): TRCLines;
var
  FLMap: TLinesMap;
  X, Y: Integer;
  isNear: Boolean;
  B, E: Integer;
begin
  Result := TRCLines.Create;
  SetLength(FLMap, raster.Height, raster.Width);
  for Y := 0 to raster.Height - 1 do
    for X := 0 to raster.Width - 1 do
        FLMap[Y, X] := RColorDistance(raster[X, Y], DetectorColor) <= Threshold;

  // build x lines
  for Y := 0 to raster.Height - 1 do
    begin
      isNear := False;
      for X := 0 to raster.Width - 1 do
        begin
          if (FLMap[Y, X]) and (not isNear) then
            begin
              B := X;
              isNear := True;
            end
          else if (not FLMap[Y, X]) and (isNear) then
            begin
              E := X - 1;
              isNear := False;
              if E - B >= MinLineLength then
                  Result.AddRCLine(B, Y, E, Y, lsRow);
            end;
        end;
    end;

  // build y lines
  for X := 0 to raster.Width - 1 do
    begin
      isNear := False;
      for Y := 0 to raster.Height - 1 do
        begin
          if (FLMap[Y, X]) and (not isNear) then
            begin
              B := Y;
              isNear := True;
            end
          else if (not FLMap[Y, X]) and (isNear) then
            begin
              E := Y - 1;
              isNear := False;
              if E - B >= MinLineLength then
                  Result.AddRCLine(X, B, X, E, lsCol);
            end;
        end;
    end;
  SetLength(FLMap, 0, 0);
end;

class function TRCLines.BuildIntersectSegment(raster: TMemoryRaster; DetectorColor: TRColor; Threshold: TGeoFloat; MinLineLength: Integer): TRCLines;
  function GetIntersect(B, E: Integer; Bp, Ep: TPoint; s: TRCLineStyle; L: TRCLines): TVec2List;
  var
    Line: TLineV2;
    i: Integer;
    List: TVec2List;

    procedure DoComputeIntersect(p: PRCLine);
    var
      iPt: TVec2;
    begin
      if p^.Style = s then
        if Intersect(Line, LineV2(p^.Bp, p^.Ep), iPt) then
          begin
            if List = nil then
                List := TVec2List.Create;
            List.Add(iPt);
          end;
    end;

  begin
    Line := LineV2(Bp, Ep);
    List := nil;
    for i := B to E do
        DoComputeIntersect(L[i]);
    Result := List;
  end;

var
  temp: TRCLines;
  i, j, k, xEnd: Integer;
  p: PRCLine;
  L: TVec2List;
  pt1, pt2: TPoint;
begin
  temp := TRCLines.BuildLines(raster, DetectorColor, Threshold, MinLineLength);
  Result := TRCLines.Create;
  xEnd := temp.SumLine(lsRow);

  // segment x lines
  for i := 0 to xEnd do
    begin
      p := temp.Items[i];
      if p^.Style = lsRow then
        begin
          pt1 := p^.Bp;
          pt2 := p^.Ep;
          inc(pt1.X);
          dec(pt2.X);
          L := GetIntersect(xEnd + 1, temp.Count - 1, pt1, pt2, lsCol, temp);
          if L <> nil then
            begin
              L.Insert(0, Vec2(p^.Bp));
              L.Add(Vec2(p^.Ep));
              for k := 0 to L.Count - 2 do
                for j := k + 1 to L.Count - 1 do
                  if j <> k then
                    begin
                      pt1 := MakePoint(L[k]^);
                      pt2 := MakePoint(L[j]^);

                      if pt1.X > pt2.X then
                          swap(pt1.X, pt2.X);

                      if pt1.Y = pt2.Y then
                          Result.AddRCLine(pt1.X, pt1.Y, pt2.X, pt2.Y, lsRow);
                    end;
              DisposeObject(L);
            end
          else
              Result.AddRCLine(p^.Bp.X, p^.Bp.Y, p^.Ep.X, p^.Ep.Y, lsRow);
        end
      else
          break;
    end;

  // segment y lines
  for i := xEnd to temp.Count - 1 do
    begin
      p := temp.Items[i];
      if p^.Style = lsCol then
        begin
          pt1 := p^.Bp;
          pt2 := p^.Ep;
          inc(pt1.Y);
          dec(pt2.Y);
          L := GetIntersect(0, xEnd, pt1, pt2, lsRow, temp);
          if L <> nil then
            begin
              L.Insert(0, Vec2(p^.Bp));
              L.Add(Vec2(p^.Ep));
              for k := 0 to L.Count - 2 do
                for j := k + 1 to L.Count - 1 do
                  if j <> k then
                    begin
                      pt1 := MakePoint(L[k]^);
                      pt2 := MakePoint(L[j]^);

                      if pt1.Y > pt2.Y then
                          swap(pt1.Y, pt2.Y);

                      if pt1.X = pt2.X then
                          Result.AddRCLine(pt1.X, pt1.Y, pt2.X, pt2.Y, lsCol);
                    end;
              DisposeObject(L);
            end
          else
              Result.AddRCLine(p^.Bp.X, p^.Bp.Y, p^.Ep.X, p^.Ep.Y, lsCol);
        end
      else
          break;
    end;

  DisposeObject(temp);
end;

destructor TRCLines.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRCLines.AddRCLine(Bx, By, Ex, Ey: Integer; Style: TRCLineStyle);
var
  p: PRCLine;
begin
  new(p);
  p^.Bp := Point(Bx, By);
  p^.Ep := Point(Ex, Ey);
  p^.Style := Style;
  Add(p);
end;

function TRCLines.SumLine(Style: TRCLineStyle): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i]^.Style = Style then
        inc(Result);
end;

function TRCLines.BuildFormulaBox: TRectV2List;

  function FindXRow(var pt1, pt2: TPoint; var index: Integer): PRCLine;
  var
    i: Integer;
    p: PRCLine;
  begin
    for i := index to Count - 1 do
      begin
        p := Items[i];
        if (p^.Style = lsRow) and (p^.Bp.X = pt1.X) and (p^.Ep.X = pt2.X) then
          begin
            index := i;
            Result := p;
            exit;
          end;
      end;
    Result := nil;
  end;

  function FindXYCol(var pt1, pt2: TPoint; var index: Integer): PRCLine;
  var
    i: Integer;
    p: PRCLine;
  begin
    for i := index to Count - 1 do
      begin
        p := Items[i];
        if (p^.Style = lsCol) and (p^.Bp.Y = pt1.Y) and (p^.Bp.X = pt1.X) and (p^.Ep.Y = pt2.Y) and (p^.Ep.X = pt2.X) then
          begin
            index := i;
            Result := p;
            exit;
          end;
      end;
    Result := nil;
  end;

var
  i: Integer;
  iTop, iLeft, iRight, iBottom: Integer;
  Top, Left, Right, Bottom: PRCLine;
begin
  Result := TRectV2List.Create;
  i := 0;
  for i := 0 to Count - 1 do
    begin
      iTop := i;
      Top := Items[iTop];
      if Top^.Style <> lsRow then
          exit;

      iBottom := iTop + 1;
      Bottom := FindXRow(Top^.Bp, Top^.Ep, iBottom);
      while Bottom <> nil do
        begin
          iLeft := iBottom + 1;
          Left := FindXYCol(Top^.Bp, Bottom^.Bp, iLeft);
          if Left <> nil then
            begin
              iRight := iLeft + 1;
              Right := FindXYCol(Top^.Ep, Bottom^.Ep, iRight);
              if (Right <> nil) then
                  Result.Add(RectV2(Vec2(Left^.Bp), Vec2(Right^.Ep)));
            end;
          inc(iBottom);
          Bottom := FindXRow(Top^.Bp, Top^.Ep, iBottom);
        end;
    end;
end;

procedure TRCLines.Remove(p1, p2, p3, p4: PRCLine);
var
  i: Integer;
  p: PRCLine;
begin
  i := 0;
  while i < Count do
    begin
      p := Items[i];
      if (p = p1) or (p = p2) or (p = p3) or (p = p4) then
          Delete(i)
      else
          inc(i);
    end;
end;

procedure TRCLines.Remove(p: PRCLine);
begin
  dispose(p);
  inherited Remove(p);
end;

procedure TRCLines.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TRCLines.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dispose(Items[i]);
  inherited Clear;
end;
