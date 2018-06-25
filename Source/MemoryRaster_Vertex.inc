function EnsureCcWinding(var t: TTriangle): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  procedure swap_vertex(var A, B: TVert2); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    v: TVert2;
  begin
    v := A; A := B; B := v;
  end;

{
  Make sure the triangle has counter-clockwise winding

  For a triangle A B C, you can find the winding by computing the cross product (B - A) rx (C - A).
  For 2d tri's, with z=0, it will only have a z component.
  To give all the same winding, swap vertices C and B if this z component is negative.
}
begin
  result := false;
  if (t[1].render[0] - t[0].render[0]) * (t[2].render[1] - t[0].render[1]) > (t[2].render[0] - t[0].render[0]) * (t[1].render[1] - t[0].render[1]) then
    begin
      swap_vertex(t[1], t[2]);
      result := true;
    end;
end;

function TexCoordWrapRepeat(const f: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  emulate OpenGL4.4 GL_TEXTURE_WRAP_S/T: GL_REPEAT
}
begin
  if f < 0 then
      result := 1 - trunc(f) + f
  else if f > 1 then
      result := f - trunc(f)
  else
      result := f;
end;

procedure Vertex_ComputeBlend(const Sender: PVertexMap; const f, m: TRasterColor; var B: TRasterColor);
begin
  if m >= 255 then
      BlendMem(f, B)
  else
      BlendMemEx(f, B, m);
end;

function Vertex_ComputeNearest(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRasterColor;
var
  tu0, tv0: Integer;
begin
  tu0 := ClampInt(trunc(TexCoordWrapRepeat(x) * (Sampler.Width)), 0, Sampler.Width - 1);
  tv0 := ClampInt(trunc(TexCoordWrapRepeat(y) * (Sampler.Height)), 0, Sampler.Height - 1);
  result := Sampler.Pixel[tu0, tv0];
end;

function Vertex_ComputeLinear(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRasterColor;
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
  fx := TexCoordWrapRepeat(x) * Sampler.Width;
  fy := TexCoordWrapRepeat(y) * Sampler.Height;

  i_x := ClampInt(trunc(fx), 0, Sampler.Width - 1);
  i_y := ClampInt(trunc(fy), 0, Sampler.Height - 1);

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

  C1.RGBA := Sampler[i_x, i_y];
  C2.RGBA := Sampler[i_x2, i_y];
  C3.RGBA := Sampler[i_x2, i_y2];
  C4.RGBA := Sampler[i_x, i_y2];

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

  TRasterColorEntry(result).R := trunc(R * 255);
  TRasterColorEntry(result).G := trunc(G * 255);
  TRasterColorEntry(result).B := trunc(B * 255);
  TRasterColorEntry(result).A := trunc(A * 255);
end;

procedure TVertexMap.RasterizeTriangle(const ft: TFragSampling; const sc: TRasterColor; const Tex: TMemoryRaster; const t: TTriangle);

  function ComputeDeterminant: TGeoFloat;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := t[0].render[0];
    y1 := t[0].render[1];
    x2 := t[1].render[0];
    y2 := t[1].render[1];
    x3 := t[2].render[0];
    y3 := t[2].render[1];
    result := x1 * y2 - x2 * y1 + x2 * y3 - x3 * y2 + x3 * y1 - x1 * y3;
  end;

  function ComputeInterpolationConsts(const determinant, w1, w2, w3: TGeoFloat): TBilerpConsts;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := t[0].render[0];
    y1 := t[0].render[1];
    x2 := t[1].render[0];
    y2 := t[1].render[1];
    x3 := t[2].render[0];
    y3 := t[2].render[1];
    result.A := ((y2 - y3) * w1 + (y3 - y1) * w2 + (y1 - y2) * w3) / determinant;
    result.B := ((x3 - x2) * w1 + (x1 - x3) * w2 + (x2 - x1) * w3) / determinant;
    result.C := ((x2 * y3 - x3 * y2) * w1 + (x3 * y1 - x1 * y3) * w2 + (x1 * y2 - x2 * y1) * w3) / determinant;
  end;

  function min3(const A, B, C: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    result := min(A, min(B, C));
  end;

  function max3(const A, B, C: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    result := max(A, max(B, C));
  end;

var
  DX12, DX23, DX31, DY12, DY23, DY31: Integer;
  cy1, cy2, cy3: Integer;
  cx1, cx2, cx3: Integer;
  minx, maxx, miny, maxy: Integer;
  i, j: Integer;
  row_last_x: Integer;
  row_frag_count: Integer;
  bitDst: Integer;
  determinant: TGeoFloat;
  attr_u, attr_v: TBilerpConsts;

  procedure RasterizationSetup;
  var
    x1, x2, x3: Integer;
    y1, y2, y3: Integer;
    C1, C2, C3: Integer;
  begin
    // fixed-point coordinates
    x1 := trunc(t[0].render[0] * 16);
    y1 := trunc(t[0].render[1] * 16);
    x2 := trunc(t[1].render[0] * 16);
    y2 := trunc(t[1].render[1] * 16);
    x3 := trunc(t[2].render[0] * 16);
    y3 := trunc(t[2].render[1] * 16);

    // Deltas
    DX12 := x1 - x2;
    DX23 := x2 - x3;
    DX31 := x3 - x1;
    DY12 := y1 - y2;
    DY23 := y2 - y3;
    DY31 := y3 - y1;

    // Bounding rectangle
    minx := (min3(x1, x2, x3) + 15) shr 4;
    maxx := (max3(x1, x2, x3) + 15) shr 4;
    miny := (min3(y1, y2, y3) + 15) shr 4;
    maxy := (max3(y1, y2, y3) + 15) shr 4;

    minx := max(0, minx);
    minx := min(Window.Width, minx);
    miny := max(0, miny);
    miny := min(Window.Height, miny);

    maxx := min(Window.Width, maxx);
    maxx := max(0, maxx);
    maxy := min(Window.Height, maxy);
    maxy := max(0, maxy);

    if minx > maxx then
        swap(minx, maxx);
    if miny > maxy then
        swap(miny, maxy);

    // Half-edge constants
    C1 := Sar32(DY12 * x1 - DX12 * y1, 4);
    C2 := Sar32(DY23 * x2 - DX23 * y2, 4);
    C3 := Sar32(DY31 * x3 - DX31 * y3, 4);

    // Correct for fill convention
    if (DY12 < 0) or ((DY12 = 0) and (DX12 > 0)) then
        inc(C1);
    if (DY23 < 0) or ((DY23 = 0) and (DX23 > 0)) then
        inc(C2);
    if (DY31 < 0) or ((DY31 = 0) and (DX31 > 0)) then
        inc(C3);

    cy1 := C1 + DX12 * miny - DY12 * minx;
    cy2 := C2 + DX23 * miny - DY23 * minx;
    cy3 := C3 + DX31 * miny - DY31 * minx;
  end;

begin
  // pixel rasterization
  RasterizationSetup;

  // init triangle interpolation
  determinant := ComputeDeterminant;
  if determinant = 0 then
      exit;
  attr_u := ComputeInterpolationConsts(determinant, t[0].Sampler[0], t[1].Sampler[0], t[2].Sampler[0]);
  attr_v := ComputeInterpolationConsts(determinant, t[0].Sampler[1], t[1].Sampler[1], t[2].Sampler[1]);

  // Scan through bounding rectangle
  bitDst := miny * Window.Width;
  for j := miny to maxy - 1 do
    begin
      cx1 := cy1;
      cx2 := cy2;
      cx3 := cy3;
      row_frag_count := 0;
      row_last_x := 0;
      for i := minx to maxx - 1 do
        begin
          // When all half-space functions positive, pixel is in triangle
          // test only sign bits
          if (cx1 or cx2 or cx3) > 0 then
            begin
              inc(row_frag_count);
              row_last_x := i;
            end;
          dec(cx1, DY12);
          dec(cx2, DY23);
          dec(cx3, DY31);
        end;
      inc(cy1, DX12);
      inc(cy2, DX23);
      inc(cy3, DX31);

      FillFragment(ft, sc, Tex, bitDst, j, row_last_x + 1 - row_frag_count, row_frag_count, attr_v, attr_u);
      inc(bitDst, Window.Width);
    end;
end;

procedure TVertexMap.FillFragment(const ft: TFragSampling; const sc: TRasterColor; const Tex: TMemoryRaster;
  const bitDst, j, start_x, frag_count: Integer; const attr_v, attr_u: TBilerpConsts);
var
  i, bitX: Integer;
  x, y: TGeoFloat;
begin
  bitX := bitDst + start_x;
  x := attr_u.A * start_x + attr_u.B * j + attr_u.C;
  y := attr_v.A * start_x + attr_v.B * j + attr_v.C;

  for i := 0 to frag_count - 1 do
    begin
      if (bitX < 0) or (bitX >= WindowSize) then
          break;

      if FNearestWriteBuffer[bitX] <> FNearestWriterID then
        begin
          FNearestWriteBuffer[bitX] := FNearestWriterID;
          case ft of
            fsSolid: BlendMem(sc, Window.Bits^[bitX]);
            fsNearest: ComputeBlend(@Self, ComputeNearest(@Self, Tex, x, y), Tex.MasterAlpha, Window.Bits^[bitX]);
            fsLinear: ComputeBlend(@Self, ComputeLinear(@Self, Tex, x, y), Tex.MasterAlpha, Window.Bits^[bitX]);
          end;
        end;

      x := x + attr_u.A;
      y := y + attr_v.A;
      inc(bitX);
    end;
end;

procedure TVertexMap.NewWriterBuffer;
var
  old: Byte;
begin
  if FCurrentUpdate > 0 then
      exit;
  old := FNearestWriterID;
  inc(FNearestWriterID);
  if FNearestWriterID < old then
    begin
      FillPtrByte(@FNearestWriteBuffer[0], Length(FNearestWriteBuffer), FNearestWriterID);
      inc(FNearestWriterID);
    end;
end;

procedure TVertexMap.internal_Draw(const triangle: TTriangle; const Sampler: TRasterColor);
var
  nt: TTriangle;
  offset_u, offset_v: TGeoFloat;
  i: Integer;
begin
  nt := triangle;
  // triangle must have counter-clockwise winding
  EnsureCcWinding(nt);

  // scale vertices to pixel grid
  nt[0].render[0] := nt[0].render[0] * Window.Width;
  nt[0].render[1] := nt[0].render[1] * Window.Height;
  nt[1].render[0] := nt[1].render[0] * Window.Width;
  nt[1].render[1] := nt[1].render[1] * Window.Height;
  nt[2].render[0] := nt[2].render[0] * Window.Width;
  nt[2].render[1] := nt[2].render[1] * Window.Height;

  RasterizeTriangle(TFragSampling.fsSolid, Sampler, nil, nt);
end;

procedure TVertexMap.internal_Draw(const triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
var
  nt: TTriangle;
  offset_u, offset_v: TGeoFloat;
  i: Integer;
begin
  nt := triangle;
  // triangle must have counter-clockwise winding
  EnsureCcWinding(nt);

  // scale vertices to pixel grid
  nt[0].render[0] := nt[0].render[0] * Window.Width;
  nt[0].render[1] := nt[0].render[1] * Window.Height;
  nt[1].render[0] := nt[1].render[0] * Window.Width;
  nt[1].render[1] := nt[1].render[1] * Window.Height;
  nt[2].render[0] := nt[2].render[0] * Window.Width;
  nt[2].render[1] := nt[2].render[1] * Window.Height;

  // offset texel centers
  offset_u := 1 / (2 * Sampler.Width);
  offset_v := 1 / (2 * Sampler.Height);
  for i := 0 to 2 do
    begin
      nt[i].Sampler[0] := nt[i].Sampler[0] - offset_u;
      nt[i].Sampler[1] := nt[i].Sampler[1] - offset_v;
    end;

  if bilinear_sampling then
      RasterizeTriangle(TFragSampling.fsLinear, RasterColor(0, 0, 0), Sampler, nt)
  else
      RasterizeTriangle(TFragSampling.fsNearest, RasterColor(0, 0, 0), Sampler, nt);
end;

procedure TVertexMap.internal_Draw(const triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: Single);
var
  mA: Cardinal;
begin
  mA := Sampler.MasterAlpha;
  if Alpha > 1.0 then
      Sampler.MasterAlpha := ClampByte(trunc(Alpha))
  else
      Sampler.MasterAlpha := ClampByte(trunc(Alpha * 255));
  internal_Draw(triangle, Sampler, bilinear_sampling);
  Sampler.MasterAlpha := mA;
end;

constructor TVertexMap.Create(raster: TMemoryRaster);
begin
  inherited Create;
  SetLength(FNearestWriteBuffer, raster.Width * raster.Height);
  FNearestWriterID := 0;
  FCurrentUpdate := 0;
  ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeBlend;
  ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeNearest;
  ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeLinear;
  Window := raster;
  WindowSize := Window.Width * Window.Height;
  UserData := nil;
end;

destructor TVertexMap.Destroy;
begin
  SetLength(FNearestWriteBuffer, 0);
  inherited Destroy;
end;

procedure TVertexMap.BeginUpdate;
begin
  if FCurrentUpdate = 0 then
      NewWriterBuffer;
  inc(FCurrentUpdate);
end;

procedure TVertexMap.EndUpdate;
begin
  dec(FCurrentUpdate);
end;

procedure TVertexMap.DrawTriangle(const triangle: TTriangle; const Sampler: TRasterColor);
begin
  NewWriterBuffer;
  internal_Draw(triangle, Sampler);
end;

procedure TVertexMap.DrawTriangle(const triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
begin
  NewWriterBuffer;
  internal_Draw(triangle, Sampler, bilinear_sampling);
end;

procedure TVertexMap.DrawTriangle(const triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: TGeoFloat);
begin
  NewWriterBuffer;
  internal_Draw(triangle, Sampler, bilinear_sampling, Alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TV2Rect4; const Sampler: TRasterColor);
var
  rv: TV2Rect4;
  t: TTriangle;
begin
  NewWriterBuffer;
  rv := RenVec.FDiv(Vec2(Window.Width, Window.Height));
  t[0].Sampler := ZeroVec2;
  t[1].Sampler := ZeroVec2;
  t[2].Sampler := ZeroVec2;
  t[0].render := rv.LeftTop;
  t[1].render := rv.RightTop;
  t[2].render := rv.LeftBottom;
  internal_Draw(t, Sampler);
  t[0].render := rv.LeftBottom;
  t[1].render := rv.RightTop;
  t[2].render := rv.RightBottom;
  internal_Draw(t, Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TV2Rect4; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: Single);
var
  sv, rv: TV2Rect4;
  t: TTriangle;
begin
  NewWriterBuffer;
  sv := SamVec.FDiv(Vec2(Sampler.Width, Sampler.Height));
  rv := RenVec.FDiv(Vec2(Window.Width, Window.Height));
  t[0].Sampler := sv.LeftTop;
  t[0].render := rv.LeftTop;
  t[1].Sampler := sv.RightTop;
  t[1].render := rv.RightTop;
  t[2].Sampler := sv.LeftBottom;
  t[2].render := rv.LeftBottom;
  internal_Draw(t, Sampler, bilinear_sampling, Alpha);
  t[0].Sampler := sv.LeftBottom;
  t[0].render := rv.LeftBottom;
  t[1].Sampler := sv.RightTop;
  t[1].render := rv.RightTop;
  t[2].Sampler := sv.RightBottom;
  t[2].render := rv.RightBottom;
  internal_Draw(t, Sampler, bilinear_sampling, Alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const Sampler: TRasterColor);
begin
  DrawRect(TV2Rect4.Init(RenVec, 0), Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TRectV2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: Single);
begin
  DrawRect(TV2Rect4.Init(SamVec, 0), TV2Rect4.Init(RenVec, 0), Sampler, bilinear_sampling, Alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TRasterColor);
begin
  DrawRect(TV2Rect4.Init(RenVec, RenAngle), Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: Single);
begin
  DrawRect(TV2Rect4.Init(SamVec, 0), TV2Rect4.Init(RenVec, RenAngle), Sampler, bilinear_sampling, Alpha);
end;

procedure TVertexMap.DrawRect(const SamVec: TV2Rect4; const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: Single);
begin
  DrawRect(SamVec, TV2Rect4.Init(RenVec, RenAngle), Sampler, bilinear_sampling, Alpha);
end;

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const cen: TVec2; const Sampler: TRasterColor);
var
  rv: TVec2List;
  rCen: TVec2;
  i: Integer;
  t: TTriangle;
begin
  NewWriterBuffer;

  rv := TVec2List.Create;
  rv.Assign(RenVec);
  rv.FDiv(Window.Width, Window.Height);
  rCen := Vec2Div(cen, Vec2(Window.Width, Window.Height));

  t[0].Sampler := ZeroVec2;
  t[1].Sampler := ZeroVec2;
  t[2].Sampler := ZeroVec2;

  for i := 1 to rv.Count - 1 do
    begin
      t[0].render := rCen;
      t[1].render := rv[i]^;
      t[2].render := rv[i - 1]^;
      internal_Draw(t, Sampler);
    end;

  t[0].render := rCen;
  t[1].render := rv.First^;
  t[2].render := rv.Last^;
  internal_Draw(t, Sampler);
  DisposeObject(rv);
end;

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const Sampler: TRasterColor);
begin
  FillPoly(RenVec, RenVec.Centroid, Sampler);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const SamCen, RenCen: TVec2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
var
  sv, rv: TVec2List;
  sCen, rCen: TVec2;
  i: Integer;
  t: TTriangle;
begin
  if SamVec.Count <> RenVec.Count then
      RaiseInfo('vertex error');

  NewWriterBuffer;

  sv := TVec2List.Create;
  sv.Assign(SamVec);
  sv.FDiv(Sampler.Width, Sampler.Height);
  sCen := Vec2Div(SamCen, Vec2(Sampler.Width, Sampler.Height));

  rv := TVec2List.Create;
  rv.Assign(RenVec);
  rv.FDiv(Window.Width, Window.Height);
  rCen := Vec2Div(RenCen, Vec2(Window.Width, Window.Height));

  for i := 1 to sv.Count - 1 do
    begin
      t[0].Sampler := sCen;
      t[0].render := rCen;
      t[1].Sampler := sv[i]^;
      t[1].render := rv[i]^;
      t[2].Sampler := sv[i - 1]^;
      t[2].render := rv[i - 1]^;
      internal_Draw(t, Sampler, bilinear_sampling);
    end;

  t[0].Sampler := sCen;
  t[0].render := rCen;
  t[1].Sampler := sv.First^;
  t[1].render := rv.First^;
  t[2].Sampler := sv.Last^;
  t[2].render := rv.Last^;
  internal_Draw(t, Sampler, bilinear_sampling);

  DisposeObject(sv);
  DisposeObject(rv);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
begin
  FillPoly(SamVec, RenVec, SamVec.Centroid, RenVec.Centroid, Sampler, bilinear_sampling);
end;

procedure TestVetexMap;
var
  B, b2: TMemoryRaster;
  pr: TVertexMap;

  vl: TVec2List;
  bbb: Byte;
begin
  bbb := $FF;
  inc(bbb);
  B := TMemoryRaster.Create;
  B.LoadFromFile('c:\1.bmp');
  b2 := TMemoryRaster.Create;
  b2.SetSize(512, 512, RasterColorF(0, 0, 0, 1));
  b2.Agg.LineWidth := 1;

  pr := b2.Vertex;
  // pr.Debug := true;

  pr.DrawRect(TV2Rect4.Init(B.BoundsRectV2, 0), TV2Rect4.Init(b2.BoundsRectV2, 0), B, false, 0.5);
  pr.DrawRect(TV2Rect4.Init(B.BoundsRectV2, 0), TV2Rect4.Init(Vec2(200, 300), 100, 100, 45), B, true, 1.0);

  pr.DrawRect(TV2Rect4.Init(B.BoundsRectV2, 0), TV2Rect4.Init(Vec2(200, 500), 200, 200, -15), B, false, 1.0);

  pr.DrawRect(TV2Rect4.Init(Vec2(200, 400), 200, 200, 45), RasterColorF(1, 0, 0, 0.5));

  vl := TVec2List.Create;
  vl.AddCirclePoint(20, Vec2(150, 150), 150);
  pr.FillPoly(vl, vl, B, false);
  vl.Transform(200, 200);
  pr.FillPoly(vl, RasterColorF(1, 0, 0, 0.5));

  b2.SaveToFile('c:\test2.bmp');
  B.SaveToFile('c:\test1.bmp');
end;
