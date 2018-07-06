procedure FillRasterColor(var X; Count: Cardinal; Value: TRasterColor);
var
  i: Integer;
  p: PRasterColorArray;
begin
  p := PRasterColorArray(@X);
  for i := Count - 1 downto 0 do
      p^[i] := Value;
end;

procedure CopyRasterColor(const Source; var dest; Count: Cardinal);
begin
  CopyPtr(@Source, @dest, Count shl 2);
end;

procedure BlendBlock(Dst: TMemoryRaster; dstRect: TRect; Src: TMemoryRaster; Srcx, Srcy: Integer; CombineOp: TDrawMode);
var
  SrcP, DstP: PRasterColor;
  sp, DP: PRasterColor;
  mc: TRasterColor;
  w, i, Dsty: Integer;
  BL: TBlendLine;
  ble: TBlendLineEx;
begin
  { Internal routine }
  w := dstRect.Right - dstRect.Left;
  SrcP := Src.PixelPtr[Srcx, Srcy];
  DstP := Dst.PixelPtr[dstRect.Left, dstRect.Top];

  case CombineOp of
    dmOpaque:
      begin
        for Dsty := dstRect.Top to dstRect.Bottom - 1 do
          begin
            CopyRasterColor(SrcP^, DstP^, w);
            Inc(SrcP, Src.width);
            Inc(DstP, Dst.width);
          end;
      end;
    dmBlend:
      if Src.MasterAlpha >= 255 then
        begin
          if Src.CombineMode = cmBlend then
              BL := {$IFDEF FPC}@{$ENDIF FPC}BlendLine
          else
              BL := {$IFDEF FPC}@{$ENDIF FPC}MergeLine;
          for Dsty := dstRect.Top to dstRect.Bottom - 1 do
            begin
              BL(SrcP, DstP, w);
              Inc(SrcP, Src.width);
              Inc(DstP, Dst.width);
            end
        end
      else
        begin
          if Src.CombineMode = cmBlend then
              ble := {$IFDEF FPC}@{$ENDIF FPC}BlendLineEx
          else
              ble := {$IFDEF FPC}@{$ENDIF FPC}MergeLineEx;
          for Dsty := dstRect.Top to dstRect.Bottom - 1 do
            begin
              ble(SrcP, DstP, w, Src.MasterAlpha);
              Inc(SrcP, Src.width);
              Inc(DstP, Dst.width);
            end
        end;
    dmTransparent:
      begin
        mc := Src.OuterColor;
        for Dsty := dstRect.Top to dstRect.Bottom - 1 do
          begin
            sp := SrcP;
            DP := DstP;
            { TODO: Write an optimized routine for fast masked transfers. }
            for i := 0 to w - 1 do
              begin
                if mc <> sp^ then
                    DP^ := sp^;
                Inc(sp);
                Inc(DP);
              end;
            Inc(SrcP, Src.width);
            Inc(DstP, Dst.width);
          end;
      end;
  end;
end;

procedure BlockTransfer(Dst: TMemoryRaster; Dstx: Integer; Dsty: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);
var
  Srcx, Srcy: Integer;
begin
  if Dst.Empty or Src.Empty or ((CombineOp = dmBlend) and (Src.MasterAlpha = 0)) then
      Exit;

  Srcx := SrcRect.Left;
  Srcy := SrcRect.Top;

  IntersectRect(DstClip, DstClip, Dst.BoundsRect);
  IntersectRect(SrcRect, SrcRect, Src.BoundsRect);

  OffsetRect(SrcRect, Dstx - Srcx, Dsty - Srcy);
  IntersectRect(SrcRect, DstClip, SrcRect);
  if IsRectEmpty(SrcRect) then
      Exit;

  DstClip := SrcRect;
  OffsetRect(SrcRect, Srcx - Dstx, Srcy - Dsty);

  BlendBlock(Dst, DstClip, Src, SrcRect.Left, SrcRect.Top, CombineOp);
end;

function RandomRasterColor(const A: Byte = $FF): TRasterColor;
begin
  Result := RasterColor(Random(255), Random(255), Random(255), A);
end;

function RasterColor(const R, g, b: Byte; const A: Byte = $FF): TRasterColor;
begin
  TRasterColorEntry(Result).R := R;
  TRasterColorEntry(Result).g := g;
  TRasterColorEntry(Result).b := b;
  TRasterColorEntry(Result).A := A;
end;

function RasterColorInv(const C: TRasterColor): TRasterColor;
begin
  Result := RasterColor(
    $FF - ClampByte(TRasterColorEntry(C).R),
    $FF - ClampByte(TRasterColorEntry(C).g),
    $FF - ClampByte(TRasterColorEntry(C).b),
    TRasterColorEntry(C).A);
end;

function RasterAlphaColor(const C: TRasterColor; const A: Byte): TRasterColor;
begin
  if TRasterColorEntry(C).A = 0 then
      Result := 0
  else if TRasterColorEntry(C).A = $FF then
      Result := C
  else
    begin
      TRasterColorEntry(Result).R := Trunc(TRasterColorEntry(C).R * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).g := Trunc(TRasterColorEntry(C).g * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).b := Trunc(TRasterColorEntry(C).b * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).A := TRasterColorEntry(C).A;
    end;
end;

function RasterAlphaColorF(const C: TRasterColor; const A: Single): TRasterColor;
begin
  Result := RasterAlphaColor(C, ClampByte(Trunc(A * $FF)));
end;

function RasterColorF(const R, g, b: Single; const A: Single = 1.0): TRasterColor;
begin
  Result := RasterColor(
    ClampByte(Round(R * $FF)),
    ClampByte(Round(g * $FF)),
    ClampByte(Round(b * $FF)),
    ClampByte(Round(A * $FF)));
end;

procedure RasterColor2F(const C: TRasterColor; var R, g, b, A: Single);
begin
  R := TRasterColorEntry(C).R / $FF;
  g := TRasterColorEntry(C).g / $FF;
  b := TRasterColorEntry(C).b / $FF;
  A := TRasterColorEntry(C).A / $FF;
end;

procedure RasterColor2F(const C: TRasterColor; var R, g, b: Single);
begin
  R := TRasterColorEntry(C).R / $FF;
  g := TRasterColorEntry(C).g / $FF;
  b := TRasterColorEntry(C).b / $FF;
end;

function RasterColor2Gray(const C: TRasterColor): Byte;
begin
  with TRasterColorEntry(C) do
      Result := Round((R + g + b) / 3);
end;

function RasterColor2GrayS(const C: TRasterColor): Single;
begin
  with TRasterColorEntry(C) do
      Result := (R + g + b) / 3 / $FF;
end;

function RasterColor2GrayD(const C: TRasterColor): Double;
begin
  with TRasterColorEntry(C) do
      Result := (R + g + b) / 3 / $FF;
end;

function RGBA2BGRA(const sour: TRasterColor): TRasterColor;
begin
  TRasterColorEntry(Result).R := TRasterColorEntry(sour).b;
  TRasterColorEntry(Result).g := TRasterColorEntry(sour).g;
  TRasterColorEntry(Result).b := TRasterColorEntry(sour).R;
  TRasterColorEntry(Result).A := TRasterColorEntry(sour).A;
end;

function BGRA2RGBA(const sour: TRasterColor): TRasterColor;
begin
  TRasterColorEntry(Result).R := TRasterColorEntry(sour).b;
  TRasterColorEntry(Result).g := TRasterColorEntry(sour).g;
  TRasterColorEntry(Result).b := TRasterColorEntry(sour).R;
  TRasterColorEntry(Result).A := TRasterColorEntry(sour).A;
end;

function AggColor(const Value: TRasterColor): TAggColorRgba8;
begin
  Result.R := TRasterColorEntry(Value).R;
  Result.g := TRasterColorEntry(Value).g;
  Result.b := TRasterColorEntry(Value).b;
  Result.A := TRasterColorEntry(Value).A;
end;

function AggColor(const R, g, b: Single; const A: Single = 1.0): TAggColorRgba8;
begin
  Result := AggColor(RasterColorF(R, g, b, A));
end;

function AggColor(const Value: TAggColorRgba8): TRasterColor;
begin
  TRasterColorEntry(Result).R := Value.R;
  TRasterColorEntry(Result).g := Value.g;
  TRasterColorEntry(Result).b := Value.b;
  TRasterColorEntry(Result).A := Value.A;
end;

procedure ComputeSize(const MAX_Width, MAX_Height: Integer; var width, height: Integer);
var
  F: Single;
begin
  if (width > MAX_Width) then
    begin
      F := MAX_Width / width;
      width := Round(width * F);
      height := Round(height * F);
    end;
  if (height > MAX_Height) then
    begin
      F := MAX_Height / height;
      width := Round(width * F);
      height := Round(height * F);
    end;
end;

procedure FastBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect);
type
  TSumRecord = packed record
    b, g, R, A, Sum: Integer;
  end;
var
  LL, RR, TT, BB, xx, yy, i, J, X, Y, RadiusI, Passes: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixel: PRasterColorEntry;
  SumRec: TSumRecord;
  ImgPixel: PRasterColorEntry;
  pixels: array of TRasterColorEntry;
begin
  if dest <> Source then
      dest.Assign(Source);

  if radius < 1 then
      Exit
  else if radius > 256 then
      radius := 256;

  RadiusI := Round(radius / Sqrt(-2 * ln(1 / 255)));
  if RadiusI < 2 then
    begin
      Passes := Round(radius);
      RadiusI := 1;
    end
  else
      Passes := 3;

  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, dest.width - 1);
  RecBottom := Min(Bounds.Bottom, dest.height - 1);

  SetLength(pixels, Max(dest.width, dest.height) + 1);

  // pre-multiply alphas ...
  for Y := RecTop to RecBottom do
    begin
      ImgPixel := PRasterColorEntry(dest.ScanLine[Y]);
      Inc(ImgPixel, RecLeft);
      for X := RecLeft to RecRight do
        with ImgPixel^ do
          begin
            R := DivTable[R, A];
            g := DivTable[g, A];
            b := DivTable[b, A];
            Inc(ImgPixel);
          end;
    end;

  for i := 1 to Passes do
    begin
      // horizontal pass...
      for Y := RecTop to RecBottom do
        begin
          ImagePixel := PRasterColorEntry(@dest.ScanLine[Y]^[RecLeft]);
          // fill the Pixels buffer with a copy of the row's pixels ...
          CopyRasterColor(ImagePixel^, pixels[RecLeft], RecRight - RecLeft + 1);

          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;

          LL := RecLeft;
          RR := RecLeft + RadiusI;
          if RR > RecRight then
              RR := RecRight;
          // update first in row ...
          for xx := LL to RR do
            with pixels[xx] do
              begin
                Inc(SumRec.A, A);
                Inc(SumRec.R, R);
                Inc(SumRec.g, g);
                Inc(SumRec.b, b);
                Inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              g := SumRec.g div SumRec.Sum;
              b := SumRec.b div SumRec.Sum;
            end;
          // update the remaining pixels in the row ...
          for X := RecLeft + 1 to RecRight do
            begin
              Inc(ImagePixel);
              LL := X - RadiusI - 1;
              RR := X + RadiusI;
              if LL >= RecLeft then
                with pixels[LL] do
                  begin
                    Dec(SumRec.A, A);
                    Dec(SumRec.R, R);
                    Dec(SumRec.g, g);
                    Dec(SumRec.b, b);
                    Dec(SumRec.Sum);
                  end;
              if RR <= RecRight then
                with pixels[RR] do
                  begin
                    Inc(SumRec.A, A);
                    Inc(SumRec.R, R);
                    Inc(SumRec.g, g);
                    Inc(SumRec.b, b);
                    Inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  A := SumRec.A div SumRec.Sum;
                  R := SumRec.R div SumRec.Sum;
                  g := SumRec.g div SumRec.Sum;
                  b := SumRec.b div SumRec.Sum;
                end;
            end;
        end;

      // vertical pass...
      for X := RecLeft to RecRight do
        begin
          ImagePixel := PRasterColorEntry(@dest.ScanLine[RecTop]^[X]);
          for J := RecTop to RecBottom do
            begin
              pixels[J] := ImagePixel^;
              Inc(ImagePixel, dest.width);
            end;
          ImagePixel := PRasterColorEntry(@dest.ScanLine[RecTop]^[X]);

          TT := RecTop;
          BB := RecTop + RadiusI;
          if BB > RecBottom then
              BB := RecBottom;
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;
          // update first in col ...
          for yy := TT to BB do
            with pixels[yy] do
              begin
                Inc(SumRec.A, A);
                Inc(SumRec.R, R);
                Inc(SumRec.g, g);
                Inc(SumRec.b, b);
                Inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              g := SumRec.g div SumRec.Sum;
              b := SumRec.b div SumRec.Sum;
            end;
          // update remainder in col ...
          for Y := RecTop + 1 to RecBottom do
            begin
              Inc(ImagePixel, dest.width);
              TT := Y - RadiusI - 1;
              BB := Y + RadiusI;

              if TT >= RecTop then
                with pixels[TT] do
                  begin
                    Dec(SumRec.A, A);
                    Dec(SumRec.R, R);
                    Dec(SumRec.g, g);
                    Dec(SumRec.b, b);
                    Dec(SumRec.Sum);
                  end;
              if BB <= RecBottom then
                with pixels[BB] do
                  begin
                    Inc(SumRec.A, A);
                    Inc(SumRec.R, R);
                    Inc(SumRec.g, g);
                    Inc(SumRec.b, b);
                    Inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  A := SumRec.A div SumRec.Sum;
                  R := SumRec.R div SumRec.Sum;
                  g := SumRec.g div SumRec.Sum;
                  b := SumRec.b div SumRec.Sum;
                end;
            end;
        end;
    end;

  // extract alphas ...
  for Y := RecTop to RecBottom do
    begin
      ImgPixel := PRasterColorEntry(@dest.ScanLine[Y]^[RecLeft]);
      for X := RecLeft to RecRight do
        begin
          ImgPixel^.R := RcTable[ImgPixel^.A, ImgPixel^.R];
          ImgPixel^.g := RcTable[ImgPixel^.A, ImgPixel^.g];
          ImgPixel^.b := RcTable[ImgPixel^.A, ImgPixel^.b];
          Inc(ImgPixel);
        end;
    end;
end;

procedure FastBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect);
begin
  FastBlur(Source, Source, radius, Bounds);
end;

procedure GaussianBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect);
const
  ChannelSize     = 256;
  ChannelSizeMin1 = ChannelSize - 1;
type
  TSumRecInt64 = packed record
    b, g, R, A: Int64;
    Sum: Integer;
  end;
var
  q, i, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PRasterColorEntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  PreMulArray: array of TRasterColorEntry;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
  SourPixel, DestPixel: PRasterColorEntry;
begin
  RadiusI := Round(radius);
  if RadiusI < 1 then
    begin
      if Source <> dest then
          dest.Assign(Source);
      Exit;
    end
  else if RadiusI > 128 then
      RadiusI := 128; // nb: performance degrades exponentially with >> Radius

  // initialize the look-up-table ...
  KernelSize := RadiusI * 2 + 1;
  SetLength(GaussLUT, KernelSize);
  for i := 0 to KernelSize - 1 do
      SetLength(GaussLUT[i], ChannelSize);
  for i := 1 to RadiusI do
    begin
      RadiusRevSq := Round((radius + 1 - i) * (radius + 1 - i));
      for J := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - i][J] := RadiusRevSq * J;
          GaussLUT[RadiusI + i][J] := GaussLUT[RadiusI - i][J];
        end;
    end;
  RadiusSq := Round((radius + 1) * (radius + 1));
  for J := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][J] := RadiusSq * J;

  ImageWidth := Source.width;
  SetLength(SumArray, ImageWidth * Source.height);

  ImagePixels := PRasterColorEntryArray(Source.Bits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.height - 1);

  RowOffset := RecTop * ImageWidth;
  SetLength(PreMulArray, Source.width);
  for Y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      q := (Y * ImageWidth) + RecLeft;
      for X := RecLeft to RecRight do
        with ImagePixels^[q] do
          begin
            PreMulArray[X].A := A;
            PreMulArray[X].R := DivTable[R, A];
            PreMulArray[X].g := DivTable[g, A];
            PreMulArray[X].b := DivTable[b, A];
            Inc(q);
          end;

      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;

          i := Max(X - RadiusI, RecLeft);
          q := i - (X - RadiusI);
          for i := i to Min(X + RadiusI, RecRight) do
            with PreMulArray[i] do
              begin
                Inc(SumRec.A, GaussLUT[q][A]);
                Inc(SumRec.R, GaussLUT[q][R]);
                Inc(SumRec.g, GaussLUT[q][g]);
                Inc(SumRec.b, GaussLUT[q][b]);
                Inc(SumRec.Sum, GaussLUT[q][1]);
                Inc(q);
              end;
          q := RowOffset + X;
          SumArray[q].A := SumRec.A div SumRec.Sum;
          SumArray[q].R := SumRec.R div SumRec.Sum;
          SumArray[q].g := SumRec.g div SumRec.Sum;
          SumArray[q].b := SumRec.b div SumRec.Sum;
        end;
      Inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.width, Source.height);

  RowOffset := RecTop * ImageWidth;
  for Y := RecTop to RecBottom do
    begin
      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;

          i := Max(Y - RadiusI, RecTop);
          q := i - (Y - RadiusI);
          for i := i to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + i * ImageWidth] do
              begin
                Inc(SumRec.A, GaussLUT[q][A]);
                Inc(SumRec.R, GaussLUT[q][R]);
                Inc(SumRec.g, GaussLUT[q][g]);
                Inc(SumRec.b, GaussLUT[q][b]);
                Inc(SumRec.Sum, GaussLUT[q][1]);
                Inc(q);
              end;

          SourPixel := @ImagePixels^[RowOffset + X];

          if Source <> dest then
              DestPixel := @PRasterColorEntryArray(dest.Bits)^[RowOffset + X]
          else
              DestPixel := SourPixel;

          DestPixel^.A := (SumRec.A div SumRec.Sum);
          DestPixel^.R := RcTable[DestPixel^.A, (SumRec.R div SumRec.Sum)];
          DestPixel^.g := RcTable[DestPixel^.A, (SumRec.g div SumRec.Sum)];
          DestPixel^.b := RcTable[DestPixel^.A, (SumRec.b div SumRec.Sum)];
        end;
      Inc(RowOffset, ImageWidth);
    end;
end;

procedure GaussianBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect);
begin
  GaussianBlur(Source, Source, radius, Bounds);
end;

procedure GrayscaleBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect);
const
  ChannelSize     = 256; // ie 1 byte for each of A,R,G & B in TRasterColor
  ChannelSizeMin1 = ChannelSize - 1;
type
  TSumRecInt64 = packed record
    R, A: Int64;
    Sum: Integer;
  end;
var
  q, i, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PRasterColorEntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  PreMulArray: array of TRasterColorEntry;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
  SourPixel, DestPixel: PRasterColorEntry;
begin
  RadiusI := Round(radius);
  if RadiusI < 1 then
    begin
      if Source <> dest then
          dest.Assign(Source);

      dest.Grayscale;
      Exit;
    end
  else if RadiusI > 128 then
      RadiusI := 128; // nb: performance degrades exponentially with >> Radius

  // initialize the look-up-table ...
  KernelSize := RadiusI * 2 + 1;
  SetLength(GaussLUT, KernelSize);
  for i := 0 to KernelSize - 1 do
      SetLength(GaussLUT[i], ChannelSize);
  for i := 1 to RadiusI do
    begin
      RadiusRevSq := Round((radius + 1 - i) * (radius + 1 - i));
      for J := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - i][J] := RadiusRevSq * J;
          GaussLUT[RadiusI + i][J] := GaussLUT[RadiusI - i][J];
        end;
    end;
  RadiusSq := Round((radius + 1) * (radius + 1));
  for J := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][J] := RadiusSq * J;

  ImageWidth := Source.width;
  SetLength(SumArray, ImageWidth * Source.height);

  ImagePixels := PRasterColorEntryArray(Source.Bits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.height - 1);

  RowOffset := RecTop * ImageWidth;
  SetLength(PreMulArray, Source.width);
  for Y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      q := (Y * ImageWidth) + RecLeft;
      for X := RecLeft to RecRight do
        with ImagePixels^[q] do
          begin
            PreMulArray[X].A := A;
            PreMulArray[X].R := DivTable[RasterColor2Gray(RGBA), A];
            Inc(q);
          end;

      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.Sum := 0;

          i := Max(X - RadiusI, RecLeft);
          q := i - (X - RadiusI);
          for i := i to Min(X + RadiusI, RecRight) do
            with PreMulArray[i] do
              begin
                Inc(SumRec.A, GaussLUT[q][A]);
                Inc(SumRec.R, GaussLUT[q][R]);
                Inc(SumRec.Sum, GaussLUT[q][1]);
                Inc(q);
              end;
          q := RowOffset + X;
          SumArray[q].A := SumRec.A div SumRec.Sum;
          SumArray[q].R := SumRec.R div SumRec.Sum;
        end;
      Inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.width, Source.height);

  RowOffset := RecTop * ImageWidth;
  for Y := RecTop to RecBottom do
    begin
      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.Sum := 0;

          i := Max(Y - RadiusI, RecTop);
          q := i - (Y - RadiusI);
          for i := i to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + i * ImageWidth] do
              begin
                Inc(SumRec.A, GaussLUT[q][A]);
                Inc(SumRec.R, GaussLUT[q][R]);
                Inc(SumRec.Sum, GaussLUT[q][1]);
                Inc(q);
              end;

          SourPixel := @ImagePixels^[RowOffset + X];

          if Source <> dest then
              DestPixel := @PRasterColorEntryArray(dest.Bits)^[RowOffset + X]
          else
              DestPixel := SourPixel;

          DestPixel^.A := (SumRec.A div SumRec.Sum);
          DestPixel^.R := RcTable[DestPixel^.A, (SumRec.R div SumRec.Sum)];
          DestPixel^.g := DestPixel^.R;
          DestPixel^.b := DestPixel^.g;
        end;
      Inc(RowOffset, ImageWidth);
    end;
end;

procedure GrayscaleBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect);
begin
  GrayscaleBlur(Source, Source, radius, Bounds);
end;

procedure Antialias32(const DestMR: TMemoryRaster; AXOrigin, AYOrigin, AXFinal, AYFinal: Integer);
var
  LMemo, X, Y: Integer;
  A, R, g, b: Cardinal;
  A0, A1Prev, A1Next, a2: Cardinal;
  r0, R1Prev, R1Next, r2: Cardinal;
  G0, G1Prev, G1Next, g2: Cardinal;
  b0, B1Prev, B1Next, b2: Cardinal;
  P0, p1, p2: PRasterColorArray;
begin
  if AXFinal < AXOrigin then
    begin
      LMemo := AXOrigin;
      AXOrigin := AXFinal;
      AXFinal := LMemo;
    end;

  if AYFinal < AYOrigin then
    begin
      LMemo := AYOrigin;
      AYOrigin := AYFinal;
      AYFinal := LMemo;
    end;

  AXOrigin := Max(1, AXOrigin);
  AYOrigin := Max(1, AYOrigin);
  AXFinal := Min(DestMR.width - 2, AXFinal);
  AYFinal := Min(DestMR.height - 2, AYFinal);

  for Y := AYOrigin to AYFinal do
    begin
      P0 := DestMR.ScanLine[Y - 1];
      p1 := DestMR.ScanLine[Y];
      p2 := DestMR.ScanLine[Y + 1];

      for X := AXOrigin to AXFinal do
        begin
          // alpha component
          A0 := P0^[X] shr 24 and $FF;
          A1Prev := p1^[X - 1] shr 24 and $FF;
          A1Next := p1^[X + 1] shr 24 and $FF;
          a2 := p2^[X] shr 24 and $FF;

          // red component
          r0 := P0^[X] shr 16 and $FF;
          R1Prev := p1^[X - 1] shr 16 and $FF;
          R1Next := p1^[X + 1] shr 16 and $FF;
          r2 := p2^[X] shr 16 and $FF;

          // green component
          G0 := P0^[X] shr 8 and $FF;
          G1Prev := p1^[X - 1] shr 8 and $FF;
          G1Next := p1^[X + 1] shr 8 and $FF;
          g2 := p2^[X] shr 8 and $FF;

          // blue component
          b0 := P0^[X] and $FF;
          B1Prev := p1^[X - 1] and $FF;
          B1Next := p1^[X + 1] and $FF;
          b2 := p2^[X] and $FF;

          // composition
          A := (A0 + a2 + A1Prev + A1Next) div 4;
          R := (r0 + r2 + R1Prev + R1Next) div 4;
          g := (G0 + g2 + G1Prev + G1Next) div 4;
          b := (b0 + b2 + B1Prev + B1Next) div 4;

          p1^[X] := (A shl 24) or (R shl 16) or (g shl 8) or b;
        end;
    end;
end;

procedure Antialias32(const DestMR: TMemoryRaster; const AAmount: Integer);
var
  i: Integer;
begin
  if AAmount >= 1 then
    for i := 1 to AAmount do
        Antialias32(DestMR, 0, 0, DestMR.width, DestMR.height);
end;

procedure HistogramEqualize(const mr: TMemoryRaster);
var
  LHistogram: array [0 .. 255] of Cardinal;
  LMap: array [0 .. 255] of Byte;
  i: Integer;
  LPixelCount: Integer;
  R, g, b: Byte;
  LSum: Cardinal;
  p: PRasterColor;
begin
  if (not Assigned(mr)) or
    (mr.width <= 0) or
    (mr.height <= 0) then
    begin
      Exit;
    end;

  for i := 0 to 255 do
    begin
      LHistogram[i] := 0;
      LHistogram[i] := 0;
      LHistogram[i] := 0;
    end;

  LPixelCount := mr.width * mr.height;

  // calculating histogram
  p := @mr.Bits[0];

  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;

      LHistogram[R] := LHistogram[R] + 1;
      LHistogram[g] := LHistogram[g] + 1;
      LHistogram[b] := LHistogram[b] + 1;

      Inc(p);
    end;

  // calculating the map
  LSum := 0;

  for i := 0 to 255 do
    begin
      LSum := LSum + LHistogram[i];
      LMap[i] := Round(LSum / (mr.width * mr.height * 3) * 255);
    end;

  // doing map
  p := @mr.Bits[0];

  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;

      R := LMap[R];
      g := LMap[g];
      b := LMap[b];

      p^ := (p^ and $FF000000) or (R shl 16) or (g shl 8) or b;

      Inc(p);
    end;
end;

procedure RemoveRedEyes(const mr: TMemoryRaster);
var
  X, Y: Integer;
  w, h: Integer;
  pixptr: PRasterColorEntry;
  nrv, bluf, redq: Single;
  powr, powb, powg: Single;
begin
  w := mr.width;
  h := mr.height;

  for Y := 0 to (h - 1) do
    begin
      for X := 0 to (w - 1) do
        begin
          pixptr := PRasterColorEntry(mr.PixelPtr[X, Y]);
          nrv := pixptr^.g + pixptr^.b;

          if nrv < 1 then
              nrv := 1;

          if pixptr^.g > 1 then
              bluf := pixptr^.b / pixptr^.g
          else
              bluf := pixptr^.b;

          bluf := Max(0.5, Min(1.5, Sqrt(bluf)));
          redq := (pixptr^.R / nrv) * bluf;

          if redq > 0.7 then
            begin
              powr := 1.775 - (redq * 0.75 + 0.25);

              if powr < 0 then
                  powr := 0;

              powr := powr * powr;
              powb := 1 - (1 - powr) / 2;
              powg := 1 - (1 - powr) / 4;

              pixptr^.R := Round(powr * pixptr^.R);
              pixptr^.b := Round(powb * pixptr^.b);
              pixptr^.g := Round(powg * pixptr^.g);
            end;
        end;
    end;
end;

procedure Sepia32(const mr: TMemoryRaster; const Depth: Byte);
var
  LDepth2, i: Integer;
  LPixel: PRasterColorEntry;
begin
  LDepth2 := Depth * 2;
  LPixel := @mr.Bits[0];

  for i := 0 to (mr.width * mr.height - 1) do
    begin
      // blue component = gray scaled color
      LPixel^.b := (LPixel^.R + LPixel^.g + LPixel^.b) div 3;

      // set red component of sepia color
      LPixel^.R := Byte(LPixel^.b + LDepth2);

      if LPixel^.R < LDepth2 then
          LPixel^.R := 255;

      // set green component of sepia color
      LPixel^.g := Byte(LPixel^.b + Depth);

      if LPixel^.g < Depth then
          LPixel^.g := 255;

      Inc(LPixel);
    end;
end;

procedure Sharpen(const DestMR: TMemoryRaster; const SharpenMore: Boolean);
const
  MASK_MATRIX: array [0 .. 24] of Integer =
    (1, 1, 1, 1, 1,
    1, 0, 0, 0, 1,
    1, 0, 0, 0, 1,
    1, 0, 0, 0, 1,
    1, 1, 1, 1, 1);

  SHARPEN_MATRIX_1: array [0 .. 9] of Integer    = (-1, -2, -1, -2, 28, -2, -1, -2, -1, 16);
  SHARPEN_MATRIX_2: array [0 .. 9] of Integer    = (-2, -1, -2, -1, 28, -1, -2, -1, -2, 16);
  SHARPEN_MORE_MATRIX: array [0 .. 9] of Integer = (0, -1, 0, -1, 6, -1, 0, -1, 0, 2);

var
  LMRCopy: TMemoryRaster;
  LSharpenTime: Integer;
  LLastMatrixNumber: Integer;
  i, X, Y, ix, iy, dx: Integer;
  LDiagonal: Integer;
  LDiagonalX: Integer;
  LDiagonalY: Integer;
  AA, RR, GG, BB: Integer;
  A, R, g, b: Byte;
  LMatrix: array [0 .. 24] of Integer;
  LOriginalRow: array of PRasterColorArray;
  LDestRow: array of PRasterColorArray;

  procedure LoadSharpenMatrix(AMatrix: array of Integer);
  var
    i, J: Integer;
  begin
    for J := 0 to 24 do
      begin
        LMatrix[J] := 0;
      end;

    i := 0;

    for J := 6 to 8 do
      begin
        LMatrix[J] := AMatrix[i];
        Inc(i);
      end;

    for J := 11 to 13 do
      begin
        LMatrix[J] := AMatrix[i];
        Inc(i);
      end;

    for J := 16 to 18 do
      begin
        LMatrix[J] := AMatrix[i];
        Inc(i);
      end;

    LLastMatrixNumber := AMatrix[9];
  end;

begin
  LSharpenTime := 0;

  if SharpenMore then
      LoadSharpenMatrix(SHARPEN_MORE_MATRIX)
  else
    begin
      Inc(LSharpenTime);

      if (LSharpenTime mod 2) = 0 then
        begin
          LoadSharpenMatrix(SHARPEN_MATRIX_1);
        end
      else
        begin
          LoadSharpenMatrix(SHARPEN_MATRIX_2);
        end;
    end;

  { scanlines arrays 3 octets (24 bits) optimization bitmaps Maximum 2048
    lines get the access port of the dest and the original bitmap }
  SetLength(LOriginalRow, DestMR.height);
  SetLength(LDestRow, DestMR.height);

  LMRCopy := TMemoryRaster.Create;
  try
    LMRCopy.Assign(DestMR);

    for i := 0 to (DestMR.height - 1) do
      begin
        LOriginalRow[i] := LMRCopy.ScanLine[i];
        LDestRow[i] := DestMR.ScanLine[i];
      end;

    if LLastMatrixNumber = 0 then
      begin
        LLastMatrixNumber := 1;
      end;

    dx := 0;
    for i := 0 to 24 do
      begin
        if (LMatrix[i] and MASK_MATRIX[i]) <> 0 then
          begin
            Inc(dx);
          end;
      end;

    if dx = 0 then
      begin
        LDiagonal := 1;
      end
    else
      begin
        LDiagonal := 2;
      end;

    for Y := 0 to (DestMR.height - 1) do
      begin
        for X := 0 to (DestMR.width - 1) do
          begin
            AA := 0;
            RR := 0;
            GG := 0;
            BB := 0;

            for LDiagonalY := -LDiagonal to LDiagonal do
              begin
                for LDiagonalX := -LDiagonal to LDiagonal do
                  begin
                    iy := Y + LDiagonalY;
                    ix := X + LDiagonalX;

                    { The original routines in the following checking code was
                      if  (iy >= 1) ...
                      and (ix >= 1) ... }
                    if (iy >= 0) and
                      (ix >= 0) and
                      (iy <= (DestMR.height - 1)) and
                      (ix <= (DestMR.width - 1)) then
                      begin
                        A := LOriginalRow[iy]^[ix] shr 24 and $FF;
                        R := LOriginalRow[iy]^[ix] shr 16 and $FF;
                        g := LOriginalRow[iy]^[ix] shr 8 and $FF;
                        b := LOriginalRow[iy]^[ix] and $FF;
                      end
                    else
                      begin
                        A := LOriginalRow[Y]^[X] shr 24 and $FF;
                        R := LOriginalRow[Y]^[X] shr 16 and $FF;
                        g := LOriginalRow[Y]^[X] shr 8 and $FF;
                        b := LOriginalRow[Y]^[X] and $FF;
                      end;

                    i := 12 + LDiagonalY * 5 + LDiagonalX;
                    AA := AA + A * LMatrix[i];
                    RR := RR + R * LMatrix[i];
                    GG := GG + g * LMatrix[i];
                    BB := BB + b * LMatrix[i];
                  end;
              end;

            AA := AA div LLastMatrixNumber;
            RR := RR div LLastMatrixNumber;
            GG := GG div LLastMatrixNumber;
            BB := BB div LLastMatrixNumber;

            A := ClampInt(AA, 0, 255);
            R := ClampInt(RR, 0, 255);
            g := ClampInt(GG, 0, 255);
            b := ClampInt(BB, 0, 255);

            LDestRow[Y]^[X] := (A shl 24) or (R shl 16) or (g shl 8) or b;
          end;
      end;
  finally
    DisposeObject(LMRCopy);
    SetLength(LDestRow, 0);
    SetLength(LOriginalRow, 0);
  end;
end;

procedure CheckParams(Src, Dst: TMemoryRaster; ResizeDst: Boolean = True);
const
  SEmptySource      = 'The source is nil';
  SEmptyDestination = 'Destination is nil';
begin
  if not Assigned(Src) then
      RaiseInfo(SEmptySource);

  if not Assigned(Dst) then
      RaiseInfo(SEmptyDestination);

  if ResizeDst then
      Dst.SetSize(Src.width, Src.height);
end;

procedure AlphaToGrayscale(Src: TMemoryRaster);
var
  i: Integer;
  C: PRasterColorEntry;
begin
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.R := C^.A;
      C^.g := C^.A;
      C^.b := C^.A;
    end;
end;

procedure IntensityToAlpha(Src: TMemoryRaster);
var
  i: Integer;
  C: PRasterColorEntry;
  F: Single;
begin
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.A := ((C^.R * 61 + C^.g * 174 + C^.b * 21) shr 8);
    end;
end;

procedure ReversalAlpha(Src: TMemoryRaster);
var
  i: Integer;
  C: PRasterColorEntry;
begin
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.A := $FF - C^.A;
    end;
end;

procedure RGBToGrayscale(Src: TMemoryRaster);
var
  i: Integer;
  C: PRasterColorEntry;
begin
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.R := RasterColor2Gray(C^.RGBA);
      C^.g := C^.R;
      C^.b := C^.R;
    end;
end;

procedure ColorToTransparent(SrcColor: TRasterColor; Src, Dst: TMemoryRaster);
var
  i, J: Integer;
  C: TRasterColorEntry;
begin
  CheckParams(Src, Dst);
  for i := 0 to Src.width - 1 do
    for J := 0 to Src.height - 1 do
      begin
        C.RGBA := Src[i, J];
        if C.RGBA = SrcColor then
            Dst[i, J] := RasterColor(0, 0, 0, 0)
        else
            Dst[i, J] := C.RGBA;
      end;
end;

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
var
  C: TRasterColor;
  bmp: TMemoryRaster;
  AMaxWidth, AMaxHeight: Integer;
  i: Integer;
  idx, X, Y: Integer;
  newbmp: TMemoryRaster;
  rowcnt: Integer;
begin
  if Column > bmp32List.Count then
      Column := bmp32List.Count;

  AMaxWidth := 0;
  AMaxHeight := 0;
  for i := 0 to bmp32List.Count - 1 do
    begin
      bmp := bmp32List[i] as TMemoryRaster;
      if Transparent then
          bmp.ColorTransparent(bmp[0, 0]);

      if bmp.width > AMaxWidth then
          AMaxWidth := bmp.width;
      if bmp.height > AMaxHeight then
          AMaxHeight := bmp.height;
    end;

  Result := TSequenceMemoryRaster.Create;

  rowcnt := bmp32List.Count div Column;
  if bmp32List.Count mod Column > 0 then
      Inc(rowcnt);

  if Transparent then
      C := RasterColor(0, 0, 0, 0)
  else
      C := RasterColor(0, 0, 0, 1);

  Result.SetSize(AMaxWidth * Column, AMaxHeight * rowcnt, C);

  idx := 0;
  X := 0;
  Y := 0;

  for i := 0 to bmp32List.Count - 1 do
    begin
      bmp := bmp32List[i] as TMemoryRaster;
      if (bmp.width <> AMaxWidth) or (bmp.height <> AMaxHeight) then
        begin
          newbmp := TMemoryRaster.Create;
          newbmp.ZoomFrom(bmp, AMaxWidth, AMaxHeight);
          BlockTransfer(Result, X, Y, Result.BoundsRect, newbmp, newbmp.BoundsRect, dmOpaque);
          DisposeObject(newbmp);
        end
      else
        begin
          BlockTransfer(Result, X, Y, Result.BoundsRect, bmp, bmp.BoundsRect, dmOpaque);
        end;

      if idx + 1 >= Column then
        begin
          idx := 0;
          X := 0;
          Y := Y + AMaxHeight;
        end
      else
        begin
          Inc(idx);
          X := X + AMaxWidth;
        end;
    end;

  Result.Total := bmp32List.Count;
  Result.Column := Column;
end;

function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect;
var
  rowIdx, colIdx: Integer;
  Row: Integer;
  AWidth, AHeight: Integer;
begin
  if Total <= 1 then
      Exit(bmp.BoundsRect);
  if Column > Total then
      Column := Total;

  if index > Total - 1 then
      index := Total - 1;
  if index < 0 then
      index := 0;

  colIdx := index mod Column;
  rowIdx := index div Column;
  Row := Total div Column;
  if Total mod Column > 0 then
      Inc(Row);

  AWidth := bmp.width div Column;
  AHeight := bmp.height div Row;

  Result := Rect(colIdx * AWidth, rowIdx * AHeight, (colIdx + 1) * AWidth, (rowIdx + 1) * AHeight);
end;

procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster);
var
  R: TRect;
  w, h: Integer;
begin
  R := GetSequenceFrameRect(bmp, Total, Column, index);
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  output.SetSize(w, h);
  BlockTransfer(output, 0, 0, output.BoundsRect, bmp, R, dmOpaque);
end;

function BlendReg(F, b: TRasterColor): TRasterColor;
var
  fx: TRasterColorEntry absolute F;
  Bx: TRasterColorEntry absolute b;
  Af, AB: PByteArray;
  FA: Byte;
begin
  FA := fx.A;

  if FA = 0 then
    begin
      Result := b;
      Exit;
    end;

  if FA = $FF then
    begin
      Result := F;
      Exit;
    end;

  with Bx do
    begin
      Af := @DivTable[FA];
      AB := @DivTable[not FA];
      R := Af^[fx.R] + AB^[R];
      g := Af^[fx.g] + AB^[g];
      b := Af^[fx.b] + AB^[b];
    end;
  Result := b;
end;

procedure BlendMem(F: TRasterColor; var b: TRasterColor);
var
  fx: TRasterColorEntry absolute F;
  Bx: TRasterColorEntry absolute b;
  Af, AB: PByteArray;
  FA: Byte;
begin
  FA := fx.A;

  if FA = 0 then
      Exit;

  if FA = $FF then
    begin
      b := F;
      Exit;
    end;

  with Bx do
    begin
      Af := @DivTable[FA];
      AB := @DivTable[not FA];
      R := Af^[fx.R] + AB^[R];
      g := Af^[fx.g] + AB^[g];
      b := Af^[fx.b] + AB^[b];
    end;
end;

function BlendRegEx(F, b, M: TRasterColor): TRasterColor;
var
  fx: TRasterColorEntry absolute F;
  Bx: TRasterColorEntry absolute b;
  Af, AB: PByteArray;
begin
  Af := @DivTable[M];

  M := Af^[fx.A];

  if M = 0 then
    begin
      Result := b;
      Exit;
    end;

  if M = $FF then
    begin
      Result := F;
      Exit;
    end;

  with Bx do
    begin
      Af := @DivTable[M];
      AB := @DivTable[255 - M];
      R := Af^[fx.R] + AB^[R];
      g := Af^[fx.g] + AB^[g];
      b := Af^[fx.b] + AB^[b];
    end;
  Result := b;
end;

procedure BlendMemEx(F: TRasterColor; var b: TRasterColor; M: TRasterColor);
var
  fx: TRasterColorEntry absolute F;
  Bx: TRasterColorEntry absolute b;
  Af, AB: PByteArray;
begin
  Af := @DivTable[M];

  M := Af^[fx.A];

  if M = 0 then
    begin
      Exit;
    end;

  if M >= 255 then
    begin
      b := F;
      Exit;
    end;

  with Bx do
    begin
      Af := @DivTable[M];
      AB := @DivTable[255 - M];
      R := Af^[fx.R] + AB^[R];
      g := Af^[fx.g] + AB^[g];
      b := Af^[fx.b] + AB^[b];
    end;
end;

procedure BlendLine(Src, Dst: PRasterColor; Count: Integer);
begin
  while Count > 0 do
    begin
      BlendMem(Src^, Dst^);
      Inc(Src);
      Inc(Dst);
      Dec(Count);
    end;
end;

procedure BlendLineEx(Src, Dst: PRasterColor; Count: Integer; M: TRasterColor);
begin
  while Count > 0 do
    begin
      BlendMemEx(Src^, Dst^, M);
      Inc(Src);
      Inc(Dst);
      Dec(Count);
    end;
end;

function CombineReg(X, Y, w: TRasterColor): TRasterColor;
var
  XE: TRasterColorEntry absolute X;
  Ye: TRasterColorEntry absolute Y;
  Af, AB: PByteArray;
begin
  if w = 0 then
    begin
      Result := Y;
      Exit;
    end;

  if w >= $FF then
    begin
      Result := X;
      Exit;
    end;

  with XE do
    begin
      Af := @DivTable[w];
      AB := @DivTable[255 - w];
      R := AB^[Ye.R] + Af^[R];
      g := AB^[Ye.g] + Af^[g];
      b := AB^[Ye.b] + Af^[b];
    end;
  Result := X;
end;

procedure CombineMem(X: TRasterColor; var Y: TRasterColor; w: TRasterColor);
var
  XE: TRasterColorEntry absolute X;
  Ye: TRasterColorEntry absolute Y;
  Af, AB: PByteArray;
begin
  if w = 0 then
    begin
      Exit;
    end;

  if w >= $FF then
    begin
      Y := X;
      Exit;
    end;

  with XE do
    begin
      Af := @DivTable[w];
      AB := @DivTable[255 - w];
      R := AB^[Ye.R] + Af^[R];
      g := AB^[Ye.g] + Af^[g];
      b := AB^[Ye.b] + Af^[b];
    end;
  Y := X;
end;

procedure CombineLine(Src, Dst: PRasterColor; Count: Integer; w: TRasterColor);
begin
  while Count > 0 do
    begin
      CombineMem(Src^, Dst^, w);
      Inc(Src);
      Inc(Dst);
      Dec(Count);
    end;
end;

function MergeReg(F, b: TRasterColor): TRasterColor;
var
  FA, BA, WA: TRasterColor;
  fw, BW: PByteArray;
  fx: TRasterColorEntry absolute F;
  Bx: TRasterColorEntry absolute b;
  RX: TRasterColorEntry absolute Result;
begin
  FA := F shr 24;
  BA := b shr 24;
  if FA = $FF then
      Result := F
  else if FA = $0 then
      Result := b
  else if BA = $0 then
      Result := F
  else
    begin
      RX.A := DivTable[FA xor 255, BA xor 255] xor 255;
      WA := RcTable[RX.A, FA];
      fw := @DivTable[WA];
      BW := @DivTable[WA xor $FF];
      RX.R := fw^[fx.R] + BW^[Bx.R];
      RX.g := fw^[fx.g] + BW^[Bx.g];
      RX.b := fw^[fx.b] + BW^[Bx.b];
    end;
end;

function MergeRegEx(F, b, M: TRasterColor): TRasterColor;
begin
  Result := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, b);
end;

procedure MergeMem(F: TRasterColor; var b: TRasterColor);
begin
  b := MergeReg(F, b);
end;

procedure MergeMemEx(F: TRasterColor; var b: TRasterColor; M: TRasterColor);
begin
  b := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, b);
end;

procedure MergeLine(Src, Dst: PRasterColor; Count: Integer);
begin
  while Count > 0 do
    begin
      Dst^ := MergeReg(Src^, Dst^);
      Inc(Src);
      Inc(Dst);
      Dec(Count);
    end;
end;

procedure MergeLineEx(Src, Dst: PRasterColor; Count: Integer; M: TRasterColor);
var
  PM: PByteArray absolute M;
begin
  PM := @DivTable[M];
  while Count > 0 do
    begin
      Dst^ := MergeReg((PM^[Src^ shr 24] shl 24) or (Src^ and $00FFFFFF), Dst^);
      Inc(Src);
      Inc(Dst);
      Dec(Count);
    end;
end;

procedure jls_RasterToRaw3(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
var
  i, J, n: Integer;
  Buf: array of Byte;
  pce: TRasterColorEntry;
begin
  SetLength(Buf, ARaster.width * 3);

  for i := 0 to ARaster.height - 1 do
    begin
      n := 0;
      for J := 0 to ARaster.width - 1 do
        begin
          pce := TRasterColorEntry(ARaster.Pixel[J, i]);
          Buf[n] := pce.b;
          Buf[n + 1] := pce.g;
          Buf[n + 2] := pce.R;
          Inc(n, 3);
        end;
      RawStream.write(Buf[0], ARaster.width * 3)
    end;
  RawStream.Position := 0;
  SetLength(Buf, 0);
end;

procedure jls_RasterToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
var
  i, J: Integer;
  Buf: array of Byte;
begin
  SetLength(Buf, ARaster.width);

  for i := 0 to ARaster.height - 1 do
    begin
      for J := 0 to ARaster.width - 1 do
          Buf[J] := ARaster.PixelGray[J, i];
      RawStream.write(Buf[0], ARaster.width)
    end;
  RawStream.Position := 0;
  SetLength(Buf, 0);
end;

procedure jls_GrayRasterToRaw1(const ARaster: PByteRaster; RawStream: TCoreClassStream);
var
  i, J: Integer;
begin
  for i := 0 to length(ARaster^) - 1 do
      RawStream.write(ARaster^[i][0], length(ARaster^[i]));
  RawStream.Position := 0;
end;

procedure jls_RasterAlphaToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
var
  i, J: Integer;
  Buf: array of Byte;
begin
  SetLength(Buf, ARaster.width);

  for i := 0 to ARaster.height - 1 do
    begin
      for J := 0 to ARaster.width - 1 do
          Buf[J] := ARaster.PixelAlpha[J, i];
      RawStream.write(Buf[0], ARaster.width)
    end;
  RawStream.Position := 0;
  SetLength(Buf, 0);
end;

function EncodeJpegLSRasterAlphaToStream(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
var
  rgbStream, alphaStream: TMemoryStream64;
  hflag: Word;
  rgbSiz, alphaSiz: Integer;
  LInput: TMemoryStream64;
  Info: TJlsParameters;
begin
  Result := False;
  rgbStream := TMemoryStream64.Create;
  alphaStream := TMemoryStream64.Create;

  if EncodeJpegLSRasterToStream3(ARaster, rgbStream) then
    begin
      LInput := TMemoryStream64.Create;
      FillPtrByte(@Info, SizeOf(Info), 0);

      try
        jls_RasterAlphaToRaw1(ARaster, LInput);
        Info.width := ARaster.width;
        Info.height := ARaster.height;
        Info.BitsPerSample := 8;
        Info.Components := 1;
        Info.Custom.t1 := 3;
        Info.Custom.t2 := 7;
        Info.Custom.t3 := 21;
        Info.Custom.Reset := 64;
        Info.AllowedLossyError := 0;

        Result := jpegls_compress(LInput, alphaStream, @Info);
      finally
          LInput.Free;
      end;
    end;

  if Result then
    begin
      rgbStream.Position := 0;
      alphaStream.Position := 0;

      hflag := $8D45;
      rgbSiz := rgbStream.Size;
      alphaSiz := alphaStream.Size;
      stream.write(hflag, 2);
      stream.write(rgbSiz, 4);
      stream.write(alphaSiz, 4);
      stream.write(rgbStream.Memory^, rgbStream.Size);
      stream.write(alphaStream.Memory^, alphaStream.Size);
    end;

  DisposeObject([rgbStream, alphaStream]);
end;

function EncodeJpegLSRasterToStream3(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);

  try
    jls_RasterToRaw3(ARaster, LInput);
    Info.width := ARaster.width;
    Info.height := ARaster.height;
    Info.BitsPerSample := 8;
    Info.Components := 3;
    Info.Custom.t1 := 3;
    Info.Custom.t2 := 7;
    Info.Custom.t3 := 21;
    Info.Custom.Reset := 64;
    Info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, stream, @Info);
  finally
      LInput.Free;
  end;
end;

function EncodeJpegLSRasterToStream1(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);

  try
    jls_RasterToRaw1(ARaster, LInput);
    Info.width := ARaster.width;
    Info.height := ARaster.height;
    Info.BitsPerSample := 8;
    Info.Components := 1;
    Info.Custom.t1 := 3;
    Info.Custom.t2 := 7;
    Info.Custom.t3 := 21;
    Info.Custom.Reset := 64;
    Info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, stream, @Info);
  finally
      LInput.Free;
  end;
end;

procedure jls_RawToRaster(const AStream: TMemoryStream64; var Info: TJlsParameters; const output: TMemoryRaster);
var
  J, i: Integer;
  Src: PByte;
  srcword: PWORD;
  R, g, b, A: Byte;
begin
  case Info.Components of
    1: case Info.BitsPerSample of
        8:
          begin
            output.SetSize(Info.width, Info.height);

            Src := AStream.Memory;
            for J := 0 to output.height - 1 do
              for i := 0 to Info.width - 1 do
                begin
                  output.PixelGray[i, J] := Src^;
                  Inc(Src);
                end;
          end;
        10, 12, 15:
          begin
            output.SetSize(Info.width, Info.height);
            srcword := AStream.Memory;

            for J := 0 to output.height - 1 do
              for i := 0 to Info.width - 1 do
                begin
                  output.PixelGray[i, J] := srcword^;
                  Inc(srcword);
                end;
          end;
        16:
          begin
            output.SetSize(Info.width, Info.height);
            srcword := AStream.Memory;

            for J := 0 to output.height - 1 do
              for i := 0 to Info.width - 1 do
                begin
                  R := Word(((srcword^ and $F800) shr 8)); // to rgb888
                  g := Word(((srcword^ and $07E0) shr 3));
                  b := Word(((srcword^ and $001F) shl 3));
                  output.PixelGray[i, J] := ((R shl 1) + (g shl 2) + g + b) shr 3;
                  Inc(srcword);
                end;
          end;
        else
          RaiseInfo('decode error');
      end;
    3:
      if Info.BitsPerSample = 8 then
        begin
          output.SetSize(Info.width, Info.height);

          Src := AStream.Memory;
          for J := 0 to output.height - 1 do
            for i := 0 to Info.width - 1 do
              begin
                R := Src^;
                Inc(Src);
                g := Src^;
                Inc(Src);
                b := Src^;
                Inc(Src);
                output.Pixel[i, J] := RasterColor(R, g, b, 255);
              end;
        end
      else
          RaiseInfo('decode error');
  end;
end;

function DecodeJpegLSRasterFromStream(const stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
var
  LOutput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LOutput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);
  try
    Result := jpegls_decompress(stream, LOutput, @Info);

    if Result then
        jls_RawToRaster(LOutput, Info, ARaster);
  finally
      LOutput.Free;
  end;
end;

function DecodeJpegLSRasterAlphaFromStream(const stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
var
  hflag: Word;
  rgbSiz, alphaSiz: Integer;
  rgbStream, alphaStream: TMemoryStream64;
  LOutput: TMemoryStream64;
  Info: TJlsParameters;
  i, J: Integer;
  Src: PByte;
begin
  Result := False;
  hflag := 0;
  stream.read(hflag, 2);
  if hflag <> $8D45 then
      Exit;

  stream.read(rgbSiz, 4);
  stream.read(alphaSiz, 4);

  rgbStream := TMemoryStream64.Create;
  alphaStream := TMemoryStream64.Create;

  try
    rgbStream.CopyFrom(stream, rgbSiz);
    if alphaSiz > 0 then
        alphaStream.CopyFrom(stream, alphaSiz);

    rgbStream.Position := 0;
    alphaStream.Position := 0;

    if DecodeJpegLSRasterFromStream(rgbStream, ARaster) and (alphaSiz > 0) then
      begin
        LOutput := TMemoryStream64.Create;
        FillPtrByte(@Info, SizeOf(Info), 0);
        try
          if jpegls_decompress(alphaStream, LOutput, @Info) and (Info.Components = 1) and (Info.BitsPerSample = 8) then
            begin
              Src := LOutput.Memory;
              for J := 0 to ARaster.height - 1 do
                begin
                  for i := 0 to ARaster.width - 1 do
                    begin
                      ARaster.PixelAlpha[i, J] := Src^;
                      Inc(Src);
                    end;
                end;
            end;
        except
        end;
        LOutput.Free;
      end;
  except
  end;

  DisposeObject([rgbStream, alphaStream]);
end;

function EncodeJpegLSGrayRasterToStream(const ARaster: PByteRaster; const stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);

  try
    jls_GrayRasterToRaw1(ARaster, LInput);
    Info.width := length(ARaster^[0]);
    Info.height := length(ARaster^);
    Info.BitsPerSample := 8;
    Info.Components := 1;
    Info.Custom.t1 := 3;
    Info.Custom.t2 := 7;
    Info.Custom.t3 := 21;
    Info.Custom.Reset := 64;
    Info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, stream, @Info);
  finally
      LInput.Free;
  end;
end;

function DecodeJpegLSGrayRasterFromStream(const stream: TCoreClassStream; var ARaster: TByteRaster): Boolean;
var
  LOutput: TMemoryStream64;
  Info: TJlsParameters;
  J, i: Integer;
  Src: PByte;
begin
  Result := False;
  LOutput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);
  try
    if jpegls_decompress(stream, LOutput, @Info) and (Info.Components = 1) and (Info.BitsPerSample = 8) then
      begin
        SetLength(ARaster, Info.height, Info.width);
        Src := LOutput.Memory;
        for J := 0 to Info.height - 1 do
          for i := 0 to Info.width - 1 do
            begin
              ARaster[J, i] := Src^;
              Inc(Src);
            end;
        Result := True;
      end;
  finally
      LOutput.Free;
  end;
end; 
 
