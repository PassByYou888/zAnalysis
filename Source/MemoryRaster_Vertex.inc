function EnsureCcWinding(var T: TTriangle): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  procedure swap_vertex(var A, b: TVert2); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    v: TVert2;
  begin
    v := A; A := b; b := v;
  end;

{
  Make sure the triangle has counter-clockwise winding

  For a triangle A B C, you can find the winding by computing the cross product (B - A) rx (C - A).
  For 2d tri's, with z=0, it will only have a z component.
  To give all the same winding, swap vertices C and B if this z component is negative.
}
begin
  Result := False;
  if (T[1].Render[0] - T[0].Render[0]) * (T[2].Render[1] - T[0].Render[1]) > (T[2].Render[0] - T[0].Render[0]) * (T[1].Render[1] - T[0].Render[1]) then
    begin
      swap_vertex(T[1], T[2]);
      Result := True;
    end;
end;

function TexCoordWrapRepeat(const F: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{
  emulate OpenGL4.4 GL_TEXTURE_WRAP_S/T: GL_REPEAT
}
begin
  if F < 0 then
      Result := 1 - Trunc(F) + F
  else if F > 1 then
      Result := F - Trunc(F)
  else
      Result := F;
end;

procedure Vertex_ComputeBlend(const Sender: PVertexMap; const F, M: TRasterColor; var b: TRasterColor);
begin
  if M >= 255 then
      BlendMem(F, b)
  else
      BlendMemEx(F, b, M);
end;

function Vertex_ComputeNearest(const Sender: PVertexMap; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRasterColor;
var
  tu0, tv0: Integer;
begin
  tu0 := ClampInt(Trunc(TexCoordWrapRepeat(X) * (Sampler.width)), 0, Sampler.width - 1);
  tv0 := ClampInt(Trunc(TexCoordWrapRepeat(Y) * (Sampler.height)), 0, Sampler.height - 1);
  Result := Sampler.Pixel[tu0, tv0];
end;

function Vertex_ComputeLinear(const Sender: PVertexMap; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRasterColor;
  procedure ClampF(var F: TGeoFloat); inline;
  begin
    if F < 0 then
        F := 0
    else if F > 1 then
        F := 1;
  end;

var
  fx, fy: TGeoFloat;
  i_x, i_y: Integer;
  i_x2, i_y2: Integer;
  delta_x, delta_y: TGeoFloat;
  c1, c2, c3, c4: TRasterColorEntry;
  k1, k2, k3, K4: TGeoFloat;
  R, g, b, A: TGeoFloat;
begin
  fx := TexCoordWrapRepeat(X) * Sampler.width;
  fy := TexCoordWrapRepeat(Y) * Sampler.height;

  i_x := ClampInt(Trunc(fx), 0, Sampler.width - 1);
  i_y := ClampInt(Trunc(fy), 0, Sampler.height - 1);

  i_x2 := i_x + 1;
  if i_x2 > Sampler.width - 1 then
      i_x2 := 0;
  i_y2 := i_y + 1;
  if i_y2 > Sampler.height - 1 then
      i_y2 := 0;

  delta_x := Frac(fx);
  delta_y := Frac(fy);

  k1 := (1 - delta_x) * (1 - delta_y);
  k2 := delta_x * (1 - delta_y);
  k3 := delta_x * delta_y;
  K4 := (1 - delta_x) * delta_y;

  c1.RGBA := Sampler[i_x, i_y];
  c2.RGBA := Sampler[i_x2, i_y];
  c3.RGBA := Sampler[i_x2, i_y2];
  c4.RGBA := Sampler[i_x, i_y2];

  R := ((c1.R / $FF) * k1);
  R := R + ((c2.R / $FF) * k2);
  R := R + ((c3.R / $FF) * k3);
  R := R + ((c4.R / $FF) * K4);

  g := ((c1.g / $FF) * k1);
  g := g + ((c2.g / $FF) * k2);
  g := g + ((c3.g / $FF) * k3);
  g := g + ((c4.g / $FF) * K4);

  b := ((c1.b / $FF) * k1);
  b := b + ((c2.b / $FF) * k2);
  b := b + ((c3.b / $FF) * k3);
  b := b + ((c4.b / $FF) * K4);

  A := ((c1.A / $FF) * k1);
  A := A + ((c2.A / $FF) * k2);
  A := A + ((c3.A / $FF) * k3);
  A := A + ((c4.A / $FF) * K4);

  ClampF(R);
  ClampF(g);
  ClampF(b);
  ClampF(A);

  TRasterColorEntry(Result).R := Trunc(R * 255);
  TRasterColorEntry(Result).g := Trunc(g * 255);
  TRasterColorEntry(Result).b := Trunc(b * 255);
  TRasterColorEntry(Result).A := Trunc(A * 255);
end;

procedure TVertexMap.RasterizeTriangle(const FT: TFragSampling; const sc: TRasterColor; const tex: TMemoryRaster; const T: TTriangle);

  function ComputeDeterminant: TGeoFloat;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := T[0].Render[0];
    y1 := T[0].Render[1];
    x2 := T[1].Render[0];
    y2 := T[1].Render[1];
    x3 := T[2].Render[0];
    y3 := T[2].Render[1];
    Result := x1 * y2 - x2 * y1 + x2 * y3 - x3 * y2 + x3 * y1 - x1 * y3;
  end;

  function ComputeInterpolationConsts(const Determinant, W1, W2, W3: TGeoFloat): TBilerpConsts;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := T[0].Render[0];
    y1 := T[0].Render[1];
    x2 := T[1].Render[0];
    y2 := T[1].Render[1];
    x3 := T[2].Render[0];
    y3 := T[2].Render[1];
    Result.A := ((y2 - y3) * W1 + (y3 - y1) * W2 + (y1 - y2) * W3) / Determinant;
    Result.b := ((x3 - x2) * W1 + (x1 - x3) * W2 + (x2 - x1) * W3) / Determinant;
    Result.C := ((x2 * y3 - x3 * y2) * W1 + (x3 * y1 - x1 * y3) * W2 + (x1 * y2 - x2 * y1) * W3) / Determinant;
  end;

  function min3(const A, b, C: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    Result := Min(A, Min(b, C));
  end;

  function max3(const A, b, C: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    Result := Max(A, Max(b, C));
  end;

var
  DX12, DX23, DX31, DY12, DY23, DY31: Integer;
  Cy1, Cy2, cy3: Integer;
  Cx1, Cx2, cx3: Integer;
  MinX, MaxX, MinY, MaxY: Integer;
  i, J: Integer;
  row_last_x: Integer;
  row_frag_count: Integer;
  bitDst: Integer;
  Determinant: TGeoFloat;
  attr_u, attr_v: TBilerpConsts;

  procedure RasterizationSetup;
  var
    x1, x2, x3: Integer;
    y1, y2, y3: Integer;
    c1, c2, c3: Integer;
  begin
    // fixed-point coordinates
    x1 := Trunc(T[0].Render[0] * 16);
    y1 := Trunc(T[0].Render[1] * 16);
    x2 := Trunc(T[1].Render[0] * 16);
    y2 := Trunc(T[1].Render[1] * 16);
    x3 := Trunc(T[2].Render[0] * 16);
    y3 := Trunc(T[2].Render[1] * 16);

    // Deltas
    DX12 := x1 - x2;
    DX23 := x2 - x3;
    DX31 := x3 - x1;
    DY12 := y1 - y2;
    DY23 := y2 - y3;
    DY31 := y3 - y1;

    // Bounding rectangle
    MinX := (min3(x1, x2, x3) + 15) shr 4;
    MaxX := (max3(x1, x2, x3) + 15) shr 4;
    MinY := (min3(y1, y2, y3) + 15) shr 4;
    MaxY := (max3(y1, y2, y3) + 15) shr 4;

    MinX := Max(0, MinX);
    MinX := Min(Window.width, MinX);
    MinY := Max(0, MinY);
    MinY := Min(Window.height, MinY);

    MaxX := Min(Window.width, MaxX);
    MaxX := Max(0, MaxX);
    MaxY := Min(Window.height, MaxY);
    MaxY := Max(0, MaxY);

    if MinX > MaxX then
        Swap(MinX, MaxX);
    if MinY > MaxY then
        Swap(MinY, MaxY);

    // Half-edge constants
    c1 := SAR32(DY12 * x1 - DX12 * y1, 4);
    c2 := SAR32(DY23 * x2 - DX23 * y2, 4);
    c3 := SAR32(DY31 * x3 - DX31 * y3, 4);

    // Correct for fill convention
    if (DY12 < 0) or ((DY12 = 0) and (DX12 > 0)) then
        Inc(c1);
    if (DY23 < 0) or ((DY23 = 0) and (DX23 > 0)) then
        Inc(c2);
    if (DY31 < 0) or ((DY31 = 0) and (DX31 > 0)) then
        Inc(c3);

    Cy1 := c1 + DX12 * MinY - DY12 * MinX;
    Cy2 := c2 + DX23 * MinY - DY23 * MinX;
    cy3 := c3 + DX31 * MinY - DY31 * MinX;
  end;

begin
  // pixel rasterization
  RasterizationSetup;

  // init triangle interpolation
  Determinant := ComputeDeterminant;
  if Determinant = 0 then
      Exit;
  attr_u := ComputeInterpolationConsts(Determinant, T[0].Sampler[0], T[1].Sampler[0], T[2].Sampler[0]);
  attr_v := ComputeInterpolationConsts(Determinant, T[0].Sampler[1], T[1].Sampler[1], T[2].Sampler[1]);

  // Scan through bounding rectangle
  bitDst := MinY * Window.width;
  for J := MinY to MaxY - 1 do
    begin
      Cx1 := Cy1;
      Cx2 := Cy2;
      cx3 := cy3;
      row_frag_count := 0;
      row_last_x := 0;
      for i := MinX to MaxX - 1 do
        begin
          // When all half-space functions positive, pixel is in triangle
          // test only sign bits
          if (Cx1 or Cx2 or cx3) > 0 then
            begin
              Inc(row_frag_count);
              row_last_x := i;
            end;
          Dec(Cx1, DY12);
          Dec(Cx2, DY23);
          Dec(cx3, DY31);
        end;
      Inc(Cy1, DX12);
      Inc(Cy2, DX23);
      Inc(cy3, DX31);

      FillFragment(FT, sc, tex, bitDst, J, row_last_x + 1 - row_frag_count, row_frag_count, attr_v, attr_u);
      Inc(bitDst, Window.width);
    end;
end;

procedure TVertexMap.FillFragment(const FT: TFragSampling; const sc: TRasterColor; const tex: TMemoryRaster;
  const bitDst, J, start_x, frag_count: Integer; const attr_v, attr_u: TBilerpConsts);
var
  i, bitX: Integer;
  X, Y: TGeoFloat;
begin
  bitX := bitDst + start_x;
  X := attr_u.A * start_x + attr_u.b * J + attr_u.C;
  Y := attr_v.A * start_x + attr_v.b * J + attr_v.C;

  for i := 0 to frag_count - 1 do
    begin
      if (bitX < 0) or (bitX >= WindowSize) then
          Break;

      if FNearestWriteBuffer[bitX] <> FNearestWriterID then
        begin
          FNearestWriteBuffer[bitX] := FNearestWriterID;
          case FT of
            fsSolid: BlendMem(sc, Window.Bits^[bitX]);
            fsNearest: ComputeBlend(@Self, ComputeNearest(@Self, tex, X, Y), tex.MasterAlpha, Window.Bits^[bitX]);
            fsLinear: ComputeBlend(@Self, ComputeLinear(@Self, tex, X, Y), tex.MasterAlpha, Window.Bits^[bitX]);
          end;
        end;

      X := X + attr_u.A;
      Y := Y + attr_v.A;
      Inc(bitX);
    end;
end;

procedure TVertexMap.NewWriterBuffer;
var
  old: Byte;
begin
  if FCurrentUpdate > 0 then
      Exit;
  old := FNearestWriterID;
  Inc(FNearestWriterID);
  if FNearestWriterID < old then
    begin
      FillPtrByte(@FNearestWriteBuffer[0], length(FNearestWriteBuffer), FNearestWriterID);
      Inc(FNearestWriterID);
    end;
end;

procedure TVertexMap.internal_Draw(const Triangle: TTriangle; const Sampler: TRasterColor);
var
  Nt: TTriangle;
  offset_u, offset_v: TGeoFloat;
  i: Integer;
begin
  Nt := Triangle;
  // triangle must have counter-clockwise winding
  EnsureCcWinding(Nt);

  // scale vertices to pixel grid
  Nt[0].Render[0] := Nt[0].Render[0] * Window.width;
  Nt[0].Render[1] := Nt[0].Render[1] * Window.height;
  Nt[1].Render[0] := Nt[1].Render[0] * Window.width;
  Nt[1].Render[1] := Nt[1].Render[1] * Window.height;
  Nt[2].Render[0] := Nt[2].Render[0] * Window.width;
  Nt[2].Render[1] := Nt[2].Render[1] * Window.height;

  RasterizeTriangle(TFragSampling.fsSolid, Sampler, nil, Nt);
end;

procedure TVertexMap.internal_Draw(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
var
  Nt: TTriangle;
  offset_u, offset_v: TGeoFloat;
  i: Integer;
begin
  Nt := Triangle;
  // triangle must have counter-clockwise winding
  EnsureCcWinding(Nt);

  // scale vertices to pixel grid
  Nt[0].Render[0] := Nt[0].Render[0] * Window.width;
  Nt[0].Render[1] := Nt[0].Render[1] * Window.height;
  Nt[1].Render[0] := Nt[1].Render[0] * Window.width;
  Nt[1].Render[1] := Nt[1].Render[1] * Window.height;
  Nt[2].Render[0] := Nt[2].Render[0] * Window.width;
  Nt[2].Render[1] := Nt[2].Render[1] * Window.height;

  // offset texel centers
  offset_u := 1 / (2 * Sampler.width);
  offset_v := 1 / (2 * Sampler.height);
  for i := 0 to 2 do
    begin
      Nt[i].Sampler[0] := Nt[i].Sampler[0] - offset_u;
      Nt[i].Sampler[1] := Nt[i].Sampler[1] - offset_v;
    end;

  if bilinear_sampling then
      RasterizeTriangle(TFragSampling.fsLinear, RasterColor(0, 0, 0), Sampler, Nt)
  else
      RasterizeTriangle(TFragSampling.fsNearest, RasterColor(0, 0, 0), Sampler, Nt);
end;

procedure TVertexMap.internal_Draw(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  MA: Cardinal;
begin
  MA := Sampler.MasterAlpha;
  if alpha > 1.0 then
      Sampler.MasterAlpha := ClampByte(Trunc(alpha))
  else
      Sampler.MasterAlpha := ClampByte(Trunc(alpha * 255));
  internal_Draw(Triangle, Sampler, bilinear_sampling);
  Sampler.MasterAlpha := MA;
end;

constructor TVertexMap.Create(raster: TMemoryRaster);
begin
  inherited Create;
  SetLength(FNearestWriteBuffer, raster.width * raster.height);
  FNearestWriterID := 0;
  FCurrentUpdate := 0;
  ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeBlend;
  ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeNearest;
  ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeLinear;
  Window := raster;
  WindowSize := Window.width * Window.height;
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
  Inc(FCurrentUpdate);
end;

procedure TVertexMap.EndUpdate;
begin
  Dec(FCurrentUpdate);
end;

procedure TVertexMap.DrawTriangle(const Triangle: TTriangle; const Sampler: TRasterColor);
begin
  NewWriterBuffer;
  internal_Draw(Triangle, Sampler);
end;

procedure TVertexMap.DrawTriangle(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
begin
  NewWriterBuffer;
  internal_Draw(Triangle, Sampler, bilinear_sampling);
end;

procedure TVertexMap.DrawTriangle(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
begin
  NewWriterBuffer;
  internal_Draw(Triangle, Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TV2Rect4; const Sampler: TRasterColor);
var
  RV: TV2Rect4;
  T: TTriangle;
begin
  NewWriterBuffer;
  RV := RenVec.FDiv(vec2(Window.width, Window.height));
  T[0].Sampler := ZeroVec2;
  T[1].Sampler := ZeroVec2;
  T[2].Sampler := ZeroVec2;
  T[0].Render := RV.LeftTop;
  T[1].Render := RV.RightTop;
  T[2].Render := RV.LeftBottom;
  internal_Draw(T, Sampler);
  T[0].Render := RV.LeftBottom;
  T[1].Render := RV.RightTop;
  T[2].Render := RV.RightBottom;
  internal_Draw(T, Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TV2Rect4; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  SV, RV: TV2Rect4;
  T: TTriangle;
begin
  NewWriterBuffer;
  SV := SamVec.FDiv(vec2(Sampler.width, Sampler.height));
  RV := RenVec.FDiv(vec2(Window.width, Window.height));
  T[0].Sampler := SV.LeftTop;
  T[0].Render := RV.LeftTop;
  T[1].Sampler := SV.RightTop;
  T[1].Render := RV.RightTop;
  T[2].Sampler := SV.LeftBottom;
  T[2].Render := RV.LeftBottom;
  internal_Draw(T, Sampler, bilinear_sampling, alpha);
  T[0].Sampler := SV.LeftBottom;
  T[0].Render := RV.LeftBottom;
  T[1].Sampler := SV.RightTop;
  T[1].Render := RV.RightTop;
  T[2].Sampler := SV.RightBottom;
  T[2].Render := RV.RightBottom;
  internal_Draw(T, Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const Sampler: TRasterColor);
begin
  DrawRect(TV2Rect4.Init(RenVec, 0), Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TRectV2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  DrawRect(TV2Rect4.Init(SamVec, 0), TV2Rect4.Init(RenVec, 0), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TRasterColor);
begin
  DrawRect(TV2Rect4.Init(RenVec, RenAngle), Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  DrawRect(TV2Rect4.Init(SamVec, 0), TV2Rect4.Init(RenVec, RenAngle), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const SamVec: TV2Rect4; const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  DrawRect(SamVec, TV2Rect4.Init(RenVec, RenAngle), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const cen: TVec2; const Sampler: TRasterColor);
var
  RV: TVec2List;
  rCen: TVec2;
  i: Integer;
  T: TTriangle;
begin
  NewWriterBuffer;

  RV := TVec2List.Create;
  RV.Assign(RenVec);
  RV.FDiv(Window.width, Window.height);
  rCen := Vec2Div(cen, vec2(Window.width, Window.height));

  T[0].Sampler := ZeroVec2;
  T[1].Sampler := ZeroVec2;
  T[2].Sampler := ZeroVec2;

  for i := 1 to RV.Count - 1 do
    begin
      T[0].Render := rCen;
      T[1].Render := RV[i]^;
      T[2].Render := RV[i - 1]^;
      internal_Draw(T, Sampler);
    end;

  T[0].Render := rCen;
  T[1].Render := RV.First^;
  T[2].Render := RV.Last^;
  internal_Draw(T, Sampler);
  DisposeObject(RV);
end;

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const Sampler: TRasterColor);
begin
  FillPoly(RenVec, RenVec.Centroid, Sampler);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const SamCen, RenCen: TVec2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
var
  SV, RV: TVec2List;
  sCen, rCen: TVec2;
  i: Integer;
  T: TTriangle;
begin
  if SamVec.Count <> RenVec.Count then
      RaiseInfo('vertex error');

  NewWriterBuffer;

  SV := TVec2List.Create;
  SV.Assign(SamVec);
  SV.FDiv(Sampler.width, Sampler.height);
  sCen := Vec2Div(SamCen, vec2(Sampler.width, Sampler.height));

  RV := TVec2List.Create;
  RV.Assign(RenVec);
  RV.FDiv(Window.width, Window.height);
  rCen := Vec2Div(RenCen, vec2(Window.width, Window.height));

  for i := 1 to SV.Count - 1 do
    begin
      T[0].Sampler := sCen;
      T[0].Render := rCen;
      T[1].Sampler := SV[i]^;
      T[1].Render := RV[i]^;
      T[2].Sampler := SV[i - 1]^;
      T[2].Render := RV[i - 1]^;
      internal_Draw(T, Sampler, bilinear_sampling);
    end;

  T[0].Sampler := sCen;
  T[0].Render := rCen;
  T[1].Sampler := SV.First^;
  T[1].Render := RV.First^;
  T[2].Sampler := SV.Last^;
  T[2].Render := RV.Last^;
  internal_Draw(T, Sampler, bilinear_sampling);

  DisposeObject(SV);
  DisposeObject(RV);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
begin
  FillPoly(SamVec, RenVec, SamVec.Centroid, RenVec.Centroid, Sampler, bilinear_sampling);
end;

procedure TestVetexMap;
var
  b, b2: TMemoryRaster;
  pr: TVertexMap;

  vl: TVec2List;
  bbb: Byte;
begin
  bbb := $FF;
  Inc(bbb);
  b := TMemoryRaster.Create;
  b.LoadFromFile('c:\1.bmp');
  b2 := TMemoryRaster.Create;
  b2.SetSize(512, 512, RasterColorF(0, 0, 0, 1));
  b2.Agg.LineWidth := 1;

  pr := b2.Vertex;
  // pr.Debug := true;

  pr.DrawRect(TV2Rect4.Init(b.BoundsRectV2, 0), TV2Rect4.Init(b2.BoundsRectV2, 0), b, False, 0.5);
  pr.DrawRect(TV2Rect4.Init(b.BoundsRectV2, 0), TV2Rect4.Init(vec2(200, 300), 100, 100, 45), b, True, 1.0);

  pr.DrawRect(TV2Rect4.Init(b.BoundsRectV2, 0), TV2Rect4.Init(vec2(200, 500), 200, 200, -15), b, False, 1.0);

  pr.DrawRect(TV2Rect4.Init(vec2(200, 400), 200, 200, 45), RasterColorF(1, 0, 0, 0.5));

  vl := TVec2List.Create;
  vl.AddCirclePoint(20, vec2(150, 150), 150);
  pr.FillPoly(vl, vl, b, False);
  vl.Transform(200, 200);
  pr.FillPoly(vl, RasterColorF(1, 0, 0, 0.5));

  b2.SaveToFile('c:\test2.bmp');
  b.SaveToFile('c:\test1.bmp');
end; 
