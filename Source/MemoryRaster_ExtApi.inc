procedure FillRasterColor(var X; Count: Cardinal; Value: TRasterColor);
var
  I: Integer;
  p: PRasterColorArray;
begin
  p := PRasterColorArray(@X);
  for I := Count - 1 downto 0 do
      p^[I] := Value;
end;

procedure CopyRasterColor(const Source; var dest; Count: Cardinal);
begin
  CopyPtr(@Source, @dest, Count shl 2);
end;

procedure BlendBlock(Dst: TMemoryRaster; DstRect: TRect; Src: TMemoryRaster; SrcX, SrcY: Integer; CombineOp: TDrawMode);
var
  SrcP, DstP: PRasterColor;
  SP, DP: PRasterColor;
  MC: TRasterColor;
  w, I, DstY: Integer;
  bl: TBlendLine;
  ble: TBlendLineEx;
begin
  { Internal routine }
  w := DstRect.Right - DstRect.Left;
  SrcP := Src.PixelPtr[SrcX, SrcY];
  DstP := Dst.PixelPtr[DstRect.Left, DstRect.Top];

  case CombineOp of
    dmOpaque:
      begin
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
          begin
            CopyRasterColor(SrcP^, DstP^, w);
            Inc(SrcP, Src.Width);
            Inc(DstP, Dst.Width);
          end;
      end;
    dmBlend:
      if Src.MasterAlpha >= 255 then
        begin
          if Src.CombineMode = cmBlend then
              bl := {$IFDEF FPC}@{$ENDIF FPC}BlendLine
          else
              bl := {$IFDEF FPC}@{$ENDIF FPC}MergeLine;
          for DstY := DstRect.Top to DstRect.Bottom - 1 do
            begin
              bl(SrcP, DstP, w);
              Inc(SrcP, Src.Width);
              Inc(DstP, Dst.Width);
            end
        end
      else
        begin
          if Src.CombineMode = cmBlend then
              ble := {$IFDEF FPC}@{$ENDIF FPC}BlendLineEx
          else
              ble := {$IFDEF FPC}@{$ENDIF FPC}MergeLineEx;
          for DstY := DstRect.Top to DstRect.Bottom - 1 do
            begin
              ble(SrcP, DstP, w, Src.MasterAlpha);
              Inc(SrcP, Src.Width);
              Inc(DstP, Dst.Width);
            end
        end;
    dmTransparent:
      begin
        MC := Src.OuterColor;
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
          begin
            SP := SrcP;
            DP := DstP;
            { TODO: Write an optimized routine for fast masked transfers. }
            for I := 0 to w - 1 do
              begin
                if MC <> SP^ then
                    DP^ := SP^;
                Inc(SP);
                Inc(DP);
              end;
            Inc(SrcP, Src.Width);
            Inc(DstP, Dst.Width);
          end;
      end;
  end;
end;

procedure BlockTransfer(Dst: TMemoryRaster; DstX: Integer; DstY: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);
var
  SrcX, SrcY: Integer;
begin
  if Dst.Empty or Src.Empty or ((CombineOp = dmBlend) and (Src.MasterAlpha = 0)) then
      Exit;

  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;

  IntersectRect(DstClip, DstClip, Dst.BoundsRect);
  IntersectRect(SrcRect, SrcRect, Src.BoundsRect);

  OffsetRect(SrcRect, DstX - SrcX, DstY - SrcY);
  IntersectRect(SrcRect, DstClip, SrcRect);
  if IsRectEmpty(SrcRect) then
      Exit;

  DstClip := SrcRect;
  OffsetRect(SrcRect, SrcX - DstX, SrcY - DstY);

  BlendBlock(Dst, DstClip, Src, SrcRect.Left, SrcRect.Top, CombineOp);
end;

function RandomRasterColor(const A: byte = $FF): TRasterColor;
begin
  Result := RasterColor(Random(255), Random(255), Random(255), A);
end;

function RasterColor(const R, G, B: byte; const A: byte = $FF): TRasterColor;
begin
  TRasterColorEntry(Result).R := R;
  TRasterColorEntry(Result).G := G;
  TRasterColorEntry(Result).B := B;
  TRasterColorEntry(Result).A := A;
end;

function RasterColorInv(const c: TRasterColor): TRasterColor;
begin
  Result := RasterColor(
    $FF - ClampByte(TRasterColorEntry(c).R),
    $FF - ClampByte(TRasterColorEntry(c).G),
    $FF - ClampByte(TRasterColorEntry(c).B),
    TRasterColorEntry(c).A);
end;

function RasterAlphaColor(const c: TRasterColor; const A: byte): TRasterColor;
begin
  if TRasterColorEntry(c).A = 0 then
      Result := 0
  else if TRasterColorEntry(c).A = $FF then
      Result := c
  else
    begin
      TRasterColorEntry(Result).R := trunc(TRasterColorEntry(c).R * (TRasterColorEntry(c).A / $FF));
      TRasterColorEntry(Result).G := trunc(TRasterColorEntry(c).G * (TRasterColorEntry(c).A / $FF));
      TRasterColorEntry(Result).B := trunc(TRasterColorEntry(c).B * (TRasterColorEntry(c).A / $FF));
      TRasterColorEntry(Result).A := TRasterColorEntry(c).A;
    end;
end;

function RasterAlphaColorF(const c: TRasterColor; const A: Single): TRasterColor;
begin
  Result := RasterAlphaColor(c, ClampByte(Trunc(A * $FF)));
end;

function RasterColorF(const R, G, B: Single; const A: Single = 1.0): TRasterColor;
begin
  Result := RasterColor(
    ClampByte(Round(R * $FF)),
    ClampByte(Round(G * $FF)),
    ClampByte(Round(B * $FF)),
    ClampByte(Round(A * $FF)));
end;

procedure RasterColor2F(const c: TRasterColor; var R, G, B, A: Single);
begin
  R := TRasterColorEntry(c).R / $FF;
  G := TRasterColorEntry(c).G / $FF;
  B := TRasterColorEntry(c).B / $FF;
  A := TRasterColorEntry(c).A / $FF;
end;

procedure RasterColor2F(const c: TRasterColor; var R, G, B: Single);
begin
  R := TRasterColorEntry(c).R / $FF;
  G := TRasterColorEntry(c).G / $FF;
  B := TRasterColorEntry(c).B / $FF;
end;

function RasterColor2Gray(const c: TRasterColor): byte;
begin
  with TRasterColorEntry(c) do
      Result := Round((R + G + B) / 3);
end;

function RasterColor2GrayS(const c: TRasterColor): Single;
begin
  with TRasterColorEntry(c) do
      Result := (R + G + B) / 3 / $FF;
end;

function RasterColor2GrayD(const c: TRasterColor): Double;
begin
  with TRasterColorEntry(c) do
      Result := (R + G + B) / 3 / $FF;
end;

function RGBA2BGRA(const sour: TRasterColor): TRasterColor;
begin
  TRasterColorEntry(Result).R := TRasterColorEntry(sour).B;
  TRasterColorEntry(Result).G := TRasterColorEntry(sour).G;
  TRasterColorEntry(Result).B := TRasterColorEntry(sour).R;
  TRasterColorEntry(Result).A := TRasterColorEntry(sour).A;
end;

function BGRA2RGBA(const sour: TRasterColor): TRasterColor;
begin
  TRasterColorEntry(Result).R := TRasterColorEntry(sour).B;
  TRasterColorEntry(Result).G := TRasterColorEntry(sour).G;
  TRasterColorEntry(Result).B := TRasterColorEntry(sour).R;
  TRasterColorEntry(Result).A := TRasterColorEntry(sour).A;
end;

function AggColor(const Value: TRasterColor): TAggColorRgba8;
begin
  Result.R := TRasterColorEntry(Value).R;
  Result.G := TRasterColorEntry(Value).G;
  Result.B := TRasterColorEntry(Value).B;
  Result.A := TRasterColorEntry(Value).A;
end;

function AggColor(const R, G, B: Single; const A: Single = 1.0): TAggColorRgba8;
begin
  Result := AggColor(RasterColorF(R, G, B, A));
end;

function AggColor(const Value: TAggColorRgba8): TRasterColor;
begin
  TRasterColorEntry(Result).R := Value.R;
  TRasterColorEntry(Result).G := Value.G;
  TRasterColorEntry(Result).B := Value.B;
  TRasterColorEntry(Result).A := Value.A;
end;

procedure ComputeSize(const MAX_Width, MAX_Height: Integer; var Width, Height: Integer);
var
  F: Single;
begin
  if (Width > MAX_Width) then
    begin
      F := MAX_Width / Width;
      Width := Round(Width * F);
      Height := Round(Height * F);
    end;
  if (Height > MAX_Height) then
    begin
      F := MAX_Height / Height;
      Width := Round(Width * F);
      Height := Round(Height * F);
    end;
end;

procedure FastBlur(Source, dest: TMemoryRaster; Radius: Double; const Bounds: TRect);
type
  TSumRecord = packed record
    B, G, R, A, Sum: Integer;
  end;
var
  LL, RR, TT, BB, XX, YY, I, J, X, Y, RadiusI, Passes: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixel: PRasterColorEntry;
  SumRec: TSumRecord;
  ImgPixel: PRasterColorEntry;
  PixelS: array of TRasterColorEntry;
begin
  if dest <> Source then
      dest.Assign(Source);

  if Radius < 1 then
      Exit
  else if Radius > 256 then
      Radius := 256;

  RadiusI := Round(Radius / Sqrt(-2 * Ln(1 / 255)));
  if RadiusI < 2 then
    begin
      Passes := Round(Radius);
      RadiusI := 1;
    end
  else
      Passes := 3;

  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, dest.Width - 1);
  RecBottom := Min(Bounds.Bottom, dest.Height - 1);

  SetLength(PixelS, Max(dest.Width, dest.Height) + 1);

  // pre-multiply alphas ...
  for Y := RecTop to RecBottom do
    begin
      ImgPixel := PRasterColorEntry(dest.ScanLine[Y]);
      Inc(ImgPixel, RecLeft);
      for X := RecLeft to RecRight do
        with ImgPixel^ do
          begin
            R := DivTable[R, A];
            G := DivTable[G, A];
            B := DivTable[B, A];
            Inc(ImgPixel);
          end;
    end;

  for I := 1 to Passes do
    begin
      // horizontal pass...
      for Y := RecTop to RecBottom do
        begin
          ImagePixel := PRasterColorEntry(@dest.ScanLine[Y]^[RecLeft]);
          // fill the Pixels buffer with a copy of the row's pixels ...
          CopyRasterColor(ImagePixel^, PixelS[RecLeft], RecRight - RecLeft + 1);

          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;

          LL := RecLeft;
          RR := RecLeft + RadiusI;
          if RR > RecRight then
              RR := RecRight;
          // update first in row ...
          for XX := LL to RR do
            with PixelS[XX] do
              begin
                Inc(SumRec.A, A);
                Inc(SumRec.R, R);
                Inc(SumRec.G, G);
                Inc(SumRec.B, B);
                Inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              G := SumRec.G div SumRec.Sum;
              B := SumRec.B div SumRec.Sum;
            end;
          // update the remaining pixels in the row ...
          for X := RecLeft + 1 to RecRight do
            begin
              Inc(ImagePixel);
              LL := X - RadiusI - 1;
              RR := X + RadiusI;
              if LL >= RecLeft then
                with PixelS[LL] do
                  begin
                    Dec(SumRec.A, A);
                    Dec(SumRec.R, R);
                    Dec(SumRec.G, G);
                    Dec(SumRec.B, B);
                    Dec(SumRec.Sum);
                  end;
              if RR <= RecRight then
                with PixelS[RR] do
                  begin
                    Inc(SumRec.A, A);
                    Inc(SumRec.R, R);
                    Inc(SumRec.G, G);
                    Inc(SumRec.B, B);
                    Inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  A := SumRec.A div SumRec.Sum;
                  R := SumRec.R div SumRec.Sum;
                  G := SumRec.G div SumRec.Sum;
                  B := SumRec.B div SumRec.Sum;
                end;
            end;
        end;

      // vertical pass...
      for X := RecLeft to RecRight do
        begin
          ImagePixel := PRasterColorEntry(@dest.ScanLine[RecTop]^[X]);
          for J := RecTop to RecBottom do
            begin
              PixelS[J] := ImagePixel^;
              Inc(ImagePixel, dest.Width);
            end;
          ImagePixel := PRasterColorEntry(@dest.ScanLine[RecTop]^[X]);

          TT := RecTop;
          BB := RecTop + RadiusI;
          if BB > RecBottom then
              BB := RecBottom;
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;
          // update first in col ...
          for YY := TT to BB do
            with PixelS[YY] do
              begin
                Inc(SumRec.A, A);
                Inc(SumRec.R, R);
                Inc(SumRec.G, G);
                Inc(SumRec.B, B);
                Inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              G := SumRec.G div SumRec.Sum;
              B := SumRec.B div SumRec.Sum;
            end;
          // update remainder in col ...
          for Y := RecTop + 1 to RecBottom do
            begin
              Inc(ImagePixel, dest.Width);
              TT := Y - RadiusI - 1;
              BB := Y + RadiusI;

              if TT >= RecTop then
                with PixelS[TT] do
                  begin
                    Dec(SumRec.A, A);
                    Dec(SumRec.R, R);
                    Dec(SumRec.G, G);
                    Dec(SumRec.B, B);
                    Dec(SumRec.Sum);
                  end;
              if BB <= RecBottom then
                with PixelS[BB] do
                  begin
                    Inc(SumRec.A, A);
                    Inc(SumRec.R, R);
                    Inc(SumRec.G, G);
                    Inc(SumRec.B, B);
                    Inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  A := SumRec.A div SumRec.Sum;
                  R := SumRec.R div SumRec.Sum;
                  G := SumRec.G div SumRec.Sum;
                  B := SumRec.B div SumRec.Sum;
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
          ImgPixel^.G := RcTable[ImgPixel^.A, ImgPixel^.G];
          ImgPixel^.B := RcTable[ImgPixel^.A, ImgPixel^.B];
          Inc(ImgPixel);
        end;
    end;
end;

procedure FastBlur(Source: TMemoryRaster; Radius: Double; const Bounds: TRect);
begin
  FastBlur(Source, Source, Radius, Bounds);
end;

procedure GaussianBlur(Source, dest: TMemoryRaster; Radius: Double; const Bounds: TRect);
const
  ChannelSize     = 256;
  ChannelSizeMin1 = ChannelSize - 1;
type
  TSumRecInt64 = packed record
    B, G, R, A: Int64;
    Sum: Integer;
  end;
var
  Q, I, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PRasterColorEntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  PreMulArray: array of TRasterColorEntry;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
  SourPixel, DestPixel: PRasterColorEntry;
begin
  RadiusI := Round(Radius);
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
  for I := 0 to KernelSize - 1 do
      SetLength(GaussLUT[I], ChannelSize);
  for I := 1 to RadiusI do
    begin
      RadiusRevSq := Round((Radius + 1 - I) * (Radius + 1 - I));
      for J := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - I][J] := RadiusRevSq * J;
          GaussLUT[RadiusI + I][J] := GaussLUT[RadiusI - I][J];
        end;
    end;
  RadiusSq := Round((Radius + 1) * (Radius + 1));
  for J := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][J] := RadiusSq * J;

  ImageWidth := Source.Width;
  SetLength(SumArray, ImageWidth * Source.Height);

  ImagePixels := PRasterColorEntryArray(Source.Bits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.Height - 1);

  RowOffset := RecTop * ImageWidth;
  SetLength(PreMulArray, Source.Width);
  for Y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      Q := (Y * ImageWidth) + RecLeft;
      for X := RecLeft to RecRight do
        with ImagePixels^[Q] do
          begin
            PreMulArray[X].A := A;
            PreMulArray[X].R := DivTable[R, A];
            PreMulArray[X].G := DivTable[G, A];
            PreMulArray[X].B := DivTable[B, A];
            Inc(Q);
          end;

      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;

          I := Max(X - RadiusI, RecLeft);
          Q := I - (X - RadiusI);
          for I := I to Min(X + RadiusI, RecRight) do
            with PreMulArray[I] do
              begin
                Inc(SumRec.A, GaussLUT[Q][A]);
                Inc(SumRec.R, GaussLUT[Q][R]);
                Inc(SumRec.G, GaussLUT[Q][G]);
                Inc(SumRec.B, GaussLUT[Q][B]);
                Inc(SumRec.Sum, GaussLUT[Q][1]);
                Inc(Q);
              end;
          Q := RowOffset + X;
          SumArray[Q].A := SumRec.A div SumRec.Sum;
          SumArray[Q].R := SumRec.R div SumRec.Sum;
          SumArray[Q].G := SumRec.G div SumRec.Sum;
          SumArray[Q].B := SumRec.B div SumRec.Sum;
        end;
      Inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.Width, Source.Height);

  RowOffset := RecTop * ImageWidth;
  for Y := RecTop to RecBottom do
    begin
      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;

          I := Max(Y - RadiusI, RecTop);
          Q := I - (Y - RadiusI);
          for I := I to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + I * ImageWidth] do
              begin
                Inc(SumRec.A, GaussLUT[Q][A]);
                Inc(SumRec.R, GaussLUT[Q][R]);
                Inc(SumRec.G, GaussLUT[Q][G]);
                Inc(SumRec.B, GaussLUT[Q][B]);
                Inc(SumRec.Sum, GaussLUT[Q][1]);
                Inc(Q);
              end;

          SourPixel := @ImagePixels^[RowOffset + X];

          if Source <> dest then
              DestPixel := @PRasterColorEntryArray(dest.Bits)^[RowOffset + X]
          else
              DestPixel := SourPixel;

          DestPixel^.A := (SumRec.A div SumRec.Sum);
          DestPixel^.R := RcTable[DestPixel^.A, (SumRec.R div SumRec.Sum)];
          DestPixel^.G := RcTable[DestPixel^.A, (SumRec.G div SumRec.Sum)];
          DestPixel^.B := RcTable[DestPixel^.A, (SumRec.B div SumRec.Sum)];
        end;
      Inc(RowOffset, ImageWidth);
    end;
end;

procedure GaussianBlur(Source: TMemoryRaster; Radius: Double; const Bounds: TRect);
begin
  GaussianBlur(Source, Source, Radius, Bounds);
end;

procedure GrayscaleBlur(Source, dest: TMemoryRaster; Radius: Double; const Bounds: TRect);
const
  ChannelSize     = 256; // ie 1 byte for each of A,R,G & B in TRasterColor
  ChannelSizeMin1 = ChannelSize - 1;
type
  TSumRecInt64 = packed record
    R, A: Int64;
    Sum: Integer;
  end;
var
  Q, I, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PRasterColorEntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  PreMulArray: array of TRasterColorEntry;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
  SourPixel, DestPixel: PRasterColorEntry;
begin
  RadiusI := Round(Radius);
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
  for I := 0 to KernelSize - 1 do
      SetLength(GaussLUT[I], ChannelSize);
  for I := 1 to RadiusI do
    begin
      RadiusRevSq := Round((Radius + 1 - I) * (Radius + 1 - I));
      for J := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - I][J] := RadiusRevSq * J;
          GaussLUT[RadiusI + I][J] := GaussLUT[RadiusI - I][J];
        end;
    end;
  RadiusSq := Round((Radius + 1) * (Radius + 1));
  for J := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][J] := RadiusSq * J;

  ImageWidth := Source.Width;
  SetLength(SumArray, ImageWidth * Source.Height);

  ImagePixels := PRasterColorEntryArray(Source.Bits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.Height - 1);

  RowOffset := RecTop * ImageWidth;
  SetLength(PreMulArray, Source.Width);
  for Y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      Q := (Y * ImageWidth) + RecLeft;
      for X := RecLeft to RecRight do
        with ImagePixels^[Q] do
          begin
            PreMulArray[X].A := A;
            PreMulArray[X].R := DivTable[RasterColor2Gray(RGBA), A];
            Inc(Q);
          end;

      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.Sum := 0;

          I := Max(X - RadiusI, RecLeft);
          Q := I - (X - RadiusI);
          for I := I to Min(X + RadiusI, RecRight) do
            with PreMulArray[I] do
              begin
                Inc(SumRec.A, GaussLUT[Q][A]);
                Inc(SumRec.R, GaussLUT[Q][R]);
                Inc(SumRec.Sum, GaussLUT[Q][1]);
                Inc(Q);
              end;
          Q := RowOffset + X;
          SumArray[Q].A := SumRec.A div SumRec.Sum;
          SumArray[Q].R := SumRec.R div SumRec.Sum;
        end;
      Inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.Width, Source.Height);

  RowOffset := RecTop * ImageWidth;
  for Y := RecTop to RecBottom do
    begin
      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.Sum := 0;

          I := Max(Y - RadiusI, RecTop);
          Q := I - (Y - RadiusI);
          for I := I to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + I * ImageWidth] do
              begin
                Inc(SumRec.A, GaussLUT[Q][A]);
                Inc(SumRec.R, GaussLUT[Q][R]);
                Inc(SumRec.Sum, GaussLUT[Q][1]);
                Inc(Q);
              end;

          SourPixel := @ImagePixels^[RowOffset + X];

          if Source <> dest then
              DestPixel := @PRasterColorEntryArray(dest.Bits)^[RowOffset + X]
          else
              DestPixel := SourPixel;

          DestPixel^.A := (SumRec.A div SumRec.Sum);
          DestPixel^.R := RcTable[DestPixel^.A, (SumRec.R div SumRec.Sum)];
          DestPixel^.G := DestPixel^.R;
          DestPixel^.B := DestPixel^.G;
        end;
      Inc(RowOffset, ImageWidth);
    end;
end;

procedure GrayscaleBlur(Source: TMemoryRaster; Radius: Double; const Bounds: TRect);
begin
  GrayscaleBlur(Source, Source, Radius, Bounds);
end;

procedure Antialias32(const DestMR: TMemoryRaster; AXOrigin, AYOrigin, AXFinal, AYFinal: Integer);
var
  LMemo, X, Y: Integer;
  A, R, G, B: Cardinal;
  a0, A1Prev, A1Next, a2: Cardinal;
  r0, R1Prev, R1Next, R2: Cardinal;
  g0, G1Prev, G1Next, g2: Cardinal;
  b0, B1Prev, B1Next, b2: Cardinal;
  p0, p1, p2: PRasterColorArray;
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
  AXFinal := Min(DestMR.Width - 2, AXFinal);
  AYFinal := Min(DestMR.Height - 2, AYFinal);

  for Y := AYOrigin to AYFinal do
    begin
      p0 := DestMR.ScanLine[Y - 1];
      p1 := DestMR.ScanLine[Y];
      p2 := DestMR.ScanLine[Y + 1];

      for X := AXOrigin to AXFinal do
        begin
          // alpha component
          a0 := p0^[X] shr 24 and $FF;
          A1Prev := p1^[X - 1] shr 24 and $FF;
          A1Next := p1^[X + 1] shr 24 and $FF;
          a2 := p2^[X] shr 24 and $FF;

          // red component
          r0 := p0^[X] shr 16 and $FF;
          R1Prev := p1^[X - 1] shr 16 and $FF;
          R1Next := p1^[X + 1] shr 16 and $FF;
          R2 := p2^[X] shr 16 and $FF;

          // green component
          g0 := p0^[X] shr 8 and $FF;
          G1Prev := p1^[X - 1] shr 8 and $FF;
          G1Next := p1^[X + 1] shr 8 and $FF;
          g2 := p2^[X] shr 8 and $FF;

          // blue component
          b0 := p0^[X] and $FF;
          B1Prev := p1^[X - 1] and $FF;
          B1Next := p1^[X + 1] and $FF;
          b2 := p2^[X] and $FF;

          // composition
          A := (a0 + a2 + A1Prev + A1Next) div 4;
          R := (r0 + R2 + R1Prev + R1Next) div 4;
          G := (g0 + g2 + G1Prev + G1Next) div 4;
          B := (b0 + b2 + B1Prev + B1Next) div 4;

          p1^[X] := (A shl 24) or (R shl 16) or (G shl 8) or B;
        end;
    end;
end;

procedure Antialias32(const DestMR: TMemoryRaster; const AAmount: Integer);
var
  I: Integer;
begin
  if AAmount >= 1 then
    for I := 1 to AAmount do
        Antialias32(DestMR, 0, 0, DestMR.Width, DestMR.Height);
end;

procedure HistogramEqualize(const mr: TMemoryRaster);
var
  LHistogram: array [0 .. 255] of Cardinal;
  LMap: array [0 .. 255] of byte;
  I: Integer;
  LPixelCount: Integer;
  R, G, B: byte;
  LSum: Cardinal;
  p: PRasterColor;
begin
  if (not Assigned(mr)) or
    (mr.Width <= 0) or
    (mr.Height <= 0) then
    begin
      Exit;
    end;

  for I := 0 to 255 do
    begin
      LHistogram[I] := 0;
      LHistogram[I] := 0;
      LHistogram[I] := 0;
    end;

  LPixelCount := mr.Width * mr.Height;

  // calculating histogram
  p := @mr.Bits[0];

  for I := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;

      LHistogram[R] := LHistogram[R] + 1;
      LHistogram[G] := LHistogram[G] + 1;
      LHistogram[B] := LHistogram[B] + 1;

      Inc(p);
    end;

  // calculating the map
  LSum := 0;

  for I := 0 to 255 do
    begin
      LSum := LSum + LHistogram[I];
      LMap[I] := Round(LSum / (mr.Width * mr.Height * 3) * 255);
    end;

  // doing map
  p := @mr.Bits[0];

  for I := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;

      R := LMap[R];
      G := LMap[G];
      B := LMap[B];

      p^ := (p^ and $FF000000) or (R shl 16) or (G shl 8) or B;

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
  w := mr.Width;
  h := mr.Height;

  for Y := 0 to (h - 1) do
    begin
      for X := 0 to (w - 1) do
        begin
          pixptr := PRasterColorEntry(mr.PixelPtr[X, Y]);
          nrv := pixptr^.G + pixptr^.B;

          if nrv < 1 then
              nrv := 1;

          if pixptr^.G > 1 then
              bluf := pixptr^.B / pixptr^.G
          else
              bluf := pixptr^.B;

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
              pixptr^.B := Round(powb * pixptr^.B);
              pixptr^.G := Round(powg * pixptr^.G);
            end;
        end;
    end;
end;

procedure Sepia32(const mr: TMemoryRaster; const Depth: byte);
var
  LDepth2, I: Integer;
  LPixel: PRasterColorEntry;
begin
  LDepth2 := Depth * 2;
  LPixel := @mr.Bits[0];

  for I := 0 to (mr.Width * mr.Height - 1) do
    begin
      // blue component = gray scaled color
      LPixel^.B := (LPixel^.R + LPixel^.G + LPixel^.B) div 3;

      // set red component of sepia color
      LPixel^.R := byte(LPixel^.B + LDepth2);

      if LPixel^.R < LDepth2 then
          LPixel^.R := 255;

      // set green component of sepia color
      LPixel^.G := byte(LPixel^.B + Depth);

      if LPixel^.G < Depth then
          LPixel^.G := 255;

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
  I, X, Y, ix, iy, Dx: Integer;
  LDiagonal: Integer;
  LDiagonalX: Integer;
  LDiagonalY: Integer;
  aa, RR, gg, BB: Integer;
  A, R, G, B: byte;
  LMatrix: array [0 .. 24] of Integer;
  LOriginalRow: array of PRasterColorArray;
  LDestRow: array of PRasterColorArray;

  procedure LoadSharpenMatrix(AMatrix: array of Integer);
  var
    I, J: Integer;
  begin
    for J := 0 to 24 do
      begin
        LMatrix[J] := 0;
      end;

    I := 0;

    for J := 6 to 8 do
      begin
        LMatrix[J] := AMatrix[I];
        Inc(I);
      end;

    for J := 11 to 13 do
      begin
        LMatrix[J] := AMatrix[I];
        Inc(I);
      end;

    for J := 16 to 18 do
      begin
        LMatrix[J] := AMatrix[I];
        Inc(I);
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
  SetLength(LOriginalRow, DestMR.Height);
  SetLength(LDestRow, DestMR.Height);

  LMRCopy := TMemoryRaster.Create;
  try
    LMRCopy.Assign(DestMR);

    for I := 0 to (DestMR.Height - 1) do
      begin
        LOriginalRow[I] := LMRCopy.ScanLine[I];
        LDestRow[I] := DestMR.ScanLine[I];
      end;

    if LLastMatrixNumber = 0 then
      begin
        LLastMatrixNumber := 1;
      end;

    Dx := 0;
    for I := 0 to 24 do
      begin
        if (LMatrix[I] and MASK_MATRIX[I]) <> 0 then
          begin
            Inc(Dx);
          end;
      end;

    if Dx = 0 then
      begin
        LDiagonal := 1;
      end
    else
      begin
        LDiagonal := 2;
      end;

    for Y := 0 to (DestMR.Height - 1) do
      begin
        for X := 0 to (DestMR.Width - 1) do
          begin
            aa := 0;
            RR := 0;
            gg := 0;
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
                      (iy <= (DestMR.Height - 1)) and
                      (ix <= (DestMR.Width - 1)) then
                      begin
                        A := LOriginalRow[iy]^[ix] shr 24 and $FF;
                        R := LOriginalRow[iy]^[ix] shr 16 and $FF;
                        G := LOriginalRow[iy]^[ix] shr 8 and $FF;
                        B := LOriginalRow[iy]^[ix] and $FF;
                      end
                    else
                      begin
                        A := LOriginalRow[Y]^[X] shr 24 and $FF;
                        R := LOriginalRow[Y]^[X] shr 16 and $FF;
                        G := LOriginalRow[Y]^[X] shr 8 and $FF;
                        B := LOriginalRow[Y]^[X] and $FF;
                      end;

                    I := 12 + LDiagonalY * 5 + LDiagonalX;
                    aa := aa + A * LMatrix[I];
                    RR := RR + R * LMatrix[I];
                    gg := gg + G * LMatrix[I];
                    BB := BB + B * LMatrix[I];
                  end;
              end;

            aa := aa div LLastMatrixNumber;
            RR := RR div LLastMatrixNumber;
            gg := gg div LLastMatrixNumber;
            BB := BB div LLastMatrixNumber;

            A := ClampInt(aa, 0, 255);
            R := ClampInt(RR, 0, 255);
            G := ClampInt(gg, 0, 255);
            B := ClampInt(BB, 0, 255);

            LDestRow[Y]^[X] := (A shl 24) or (R shl 16) or (G shl 8) or B;
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
      Dst.SetSize(Src.Width, Src.Height);
end;

procedure AlphaToGrayscale(Src: TMemoryRaster);
var
  I: Integer;
  c: PRasterColorEntry;
begin
  for I := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[I];
      c^.R := c^.A;
      c^.G := c^.A;
      c^.B := c^.A;
    end;
end;

procedure IntensityToAlpha(Src: TMemoryRaster);
var
  I: Integer;
  c: PRasterColorEntry;
  F: Single;
begin
  for I := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[I];
      c^.A := ((c^.R * 61 + c^.G * 174 + c^.B * 21) shr 8);
    end;
end;

procedure ReversalAlpha(Src: TMemoryRaster);
var
  I: Integer;
  c: PRasterColorEntry;
begin
  for I := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[I];
      c^.A := $FF - c^.A;
    end;
end;

procedure RGBToGrayscale(Src: TMemoryRaster);
var
  I: Integer;
  c: PRasterColorEntry;
begin
  for I := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[I];
      c^.R := RasterColor2Gray(c^.RGBA);
      c^.G := c^.R;
      c^.B := c^.R;
    end;
end;

procedure ColorToTransparent(SrcColor: TRasterColor; Src, Dst: TMemoryRaster);
var
  I, J: Integer;
  c: TRasterColorEntry;
begin
  CheckParams(Src, Dst);
  for I := 0 to Src.Width - 1 do
    for J := 0 to Src.Height - 1 do
      begin
        c.RGBA := Src[I, J];
        if c.RGBA = SrcColor then
            Dst[I, J] := RasterColor(0, 0, 0, 0)
        else
            Dst[I, J] := c.RGBA;
      end;
end;

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
var
  c: TRasterColor;
  Bmp: TMemoryRaster;
  AMaxWidth, AMaxHeight: Integer;
  I: Integer;
  idx, X, Y: Integer;
  newbmp: TMemoryRaster;
  rowcnt: Integer;
begin
  if Column > bmp32List.Count then
      Column := bmp32List.Count;

  AMaxWidth := 0;
  AMaxHeight := 0;
  for I := 0 to bmp32List.Count - 1 do
    begin
      Bmp := bmp32List[I] as TMemoryRaster;
      if Transparent then
          Bmp.ColorTransparent(Bmp[0, 0]);

      if Bmp.Width > AMaxWidth then
          AMaxWidth := Bmp.Width;
      if Bmp.Height > AMaxHeight then
          AMaxHeight := Bmp.Height;
    end;

  Result := TSequenceMemoryRaster.Create;

  rowcnt := bmp32List.Count div Column;
  if bmp32List.Count mod Column > 0 then
      Inc(rowcnt);

  if Transparent then
      c := RasterColor(0, 0, 0, 0)
  else
      c := RasterColor(0, 0, 0, 1);

  Result.SetSize(AMaxWidth * Column, AMaxHeight * rowcnt, c);

  idx := 0;
  X := 0;
  Y := 0;

  for I := 0 to bmp32List.Count - 1 do
    begin
      Bmp := bmp32List[I] as TMemoryRaster;
      if (Bmp.Width <> AMaxWidth) or (Bmp.Height <> AMaxHeight) then
        begin
          newbmp := TMemoryRaster.Create;
          newbmp.ZoomFrom(Bmp, AMaxWidth, AMaxHeight);
          BlockTransfer(Result, X, Y, Result.BoundsRect, newbmp, newbmp.BoundsRect, dmOpaque);
          DisposeObject(newbmp);
        end
      else
        begin
          BlockTransfer(Result, X, Y, Result.BoundsRect, Bmp, Bmp.BoundsRect, dmOpaque);
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

function GetSequenceFrameRect(Bmp: TMemoryRaster; Total, Column, index: Integer): TRect;
var
  rowIdx, colIdx: Integer;
  row: Integer;
  AWidth, AHeight: Integer;
begin
  if Total <= 1 then
      Exit(Bmp.BoundsRect);
  if Column > Total then
      Column := Total;

  if index > Total - 1 then
      index := Total - 1;
  if index < 0 then
      index := 0;

  colIdx := index mod Column;
  rowIdx := index div Column;
  row := Total div Column;
  if Total mod Column > 0 then
      Inc(row);

  AWidth := Bmp.Width div Column;
  AHeight := Bmp.Height div row;

  Result := Rect(colIdx * AWidth, rowIdx * AHeight, (colIdx + 1) * AWidth, (rowIdx + 1) * AHeight);
end;

procedure GetSequenceFrameOutput(Bmp: TMemoryRaster; Total, Column, index: Integer; Output: TMemoryRaster);
var
  R: TRect;
  w, h: Integer;
begin
  R := GetSequenceFrameRect(Bmp, Total, Column, index);
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  Output.SetSize(w, h);
  BlockTransfer(Output, 0, 0, Output.BoundsRect, Bmp, R, dmOpaque);
end;

function BlendReg(F, B: TRasterColor): TRasterColor;
var
  FX: TRasterColorEntry absolute F;
  BX: TRasterColorEntry absolute B;
  Af, Ab: PByteArray;
  FA: byte;
begin
  FA := FX.A;

  if FA = 0 then
    begin
      Result := B;
      Exit;
    end;

  if FA = $FF then
    begin
      Result := F;
      Exit;
    end;

  with BX do
    begin
      Af := @DivTable[FA];
      Ab := @DivTable[not FA];
      R := Af^[FX.R] + Ab^[R];
      G := Af^[FX.G] + Ab^[G];
      B := Af^[FX.B] + Ab^[B];
    end;
  Result := B;
end;

procedure BlendMem(F: TRasterColor; var B: TRasterColor);
var
  FX: TRasterColorEntry absolute F;
  BX: TRasterColorEntry absolute B;
  Af, Ab: PByteArray;
  FA: byte;
begin
  FA := FX.A;

  if FA = 0 then
      Exit;

  if FA = $FF then
    begin
      B := F;
      Exit;
    end;

  with BX do
    begin
      Af := @DivTable[FA];
      Ab := @DivTable[not FA];
      R := Af^[FX.R] + Ab^[R];
      G := Af^[FX.G] + Ab^[G];
      B := Af^[FX.B] + Ab^[B];
    end;
end;

function BlendRegEx(F, B, M: TRasterColor): TRasterColor;
var
  FX: TRasterColorEntry absolute F;
  BX: TRasterColorEntry absolute B;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[M];

  M := Af^[FX.A];

  if M = 0 then
    begin
      Result := B;
      Exit;
    end;

  if M = $FF then
    begin
      Result := F;
      Exit;
    end;

  with BX do
    begin
      Af := @DivTable[M];
      Ab := @DivTable[255 - M];
      R := Af^[FX.R] + Ab^[R];
      G := Af^[FX.G] + Ab^[G];
      B := Af^[FX.B] + Ab^[B];
    end;
  Result := B;
end;

procedure BlendMemEx(F: TRasterColor; var B: TRasterColor; M: TRasterColor);
var
  FX: TRasterColorEntry absolute F;
  BX: TRasterColorEntry absolute B;
  Af, Ab: PByteArray;
begin
  Af := @DivTable[M];

  M := Af^[FX.A];

  if M = 0 then
    begin
      Exit;
    end;

  if M >= 255 then
    begin
      B := F;
      Exit;
    end;

  with BX do
    begin
      Af := @DivTable[M];
      Ab := @DivTable[255 - M];
      R := Af^[FX.R] + Ab^[R];
      G := Af^[FX.G] + Ab^[G];
      B := Af^[FX.B] + Ab^[B];
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
  Xe: TRasterColorEntry absolute X;
  Ye: TRasterColorEntry absolute Y;
  Af, Ab: PByteArray;
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

  with Xe do
    begin
      Af := @DivTable[w];
      Ab := @DivTable[255 - w];
      R := Ab^[Ye.R] + Af^[R];
      G := Ab^[Ye.G] + Af^[G];
      B := Ab^[Ye.B] + Af^[B];
    end;
  Result := X;
end;

procedure CombineMem(X: TRasterColor; var Y: TRasterColor; w: TRasterColor);
var
  Xe: TRasterColorEntry absolute X;
  Ye: TRasterColorEntry absolute Y;
  Af, Ab: PByteArray;
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

  with Xe do
    begin
      Af := @DivTable[w];
      Ab := @DivTable[255 - w];
      R := Ab^[Ye.R] + Af^[R];
      G := Ab^[Ye.G] + Af^[G];
      B := Ab^[Ye.B] + Af^[B];
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

function MergeReg(F, B: TRasterColor): TRasterColor;
var
  FA, Ba, Wa: TRasterColor;
  Fw, Bw: PByteArray;
  FX: TRasterColorEntry absolute F;
  BX: TRasterColorEntry absolute B;
  Rx: TRasterColorEntry absolute Result;
begin
  FA := F shr 24;
  Ba := B shr 24;
  if FA = $FF then
      Result := F
  else if FA = $0 then
      Result := B
  else if Ba = $0 then
      Result := F
  else
    begin
      Rx.A := DivTable[FA xor 255, Ba xor 255] xor 255;
      Wa := RcTable[Rx.A, FA];
      Fw := @DivTable[Wa];
      Bw := @DivTable[Wa xor $FF];
      Rx.R := Fw^[FX.R] + Bw^[BX.R];
      Rx.G := Fw^[FX.G] + Bw^[BX.G];
      Rx.B := Fw^[FX.B] + Bw^[BX.B];
    end;
end;

function MergeRegEx(F, B, M: TRasterColor): TRasterColor;
begin
  Result := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
end;

procedure MergeMem(F: TRasterColor; var B: TRasterColor);
begin
  B := MergeReg(F, B);
end;

procedure MergeMemEx(F: TRasterColor; var B: TRasterColor; M: TRasterColor);
begin
  B := MergeReg(DivTable[M, F shr 24] shl 24 or F and $00FFFFFF, B);
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
  I, J, n: Integer;
  buf: array of byte;
  pce: TRasterColorEntry;
begin
  SetLength(buf, ARaster.Width * 3);

  for I := 0 to ARaster.Height - 1 do
    begin
      n := 0;
      for J := 0 to ARaster.Width - 1 do
        begin
          pce := TRasterColorEntry(ARaster.Pixel[J, I]);
          buf[n] := pce.B;
          buf[n + 1] := pce.G;
          buf[n + 2] := pce.R;
          Inc(n, 3);
        end;
      RawStream.Write(buf[0], ARaster.Width * 3)
    end;
  RawStream.Position := 0;
  SetLength(buf, 0);
end;

procedure jls_RasterToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
var
  I, J: Integer;
  buf: array of byte;
begin
  SetLength(buf, ARaster.Width);

  for I := 0 to ARaster.Height - 1 do
    begin
      for J := 0 to ARaster.Width - 1 do
          buf[J] := ARaster.PixelGray[J, I];
      RawStream.Write(buf[0], ARaster.Width)
    end;
  RawStream.Position := 0;
  SetLength(buf, 0);
end;

procedure jls_GrayRasterToRaw1(const ARaster: PByteRaster; RawStream: TCoreClassStream);
var
  I, J: Integer;
begin
  for I := 0 to Length(ARaster^) - 1 do
      RawStream.Write(ARaster^[I][0], Length(ARaster^[I]));
  RawStream.Position := 0;
end;

procedure jls_RasterAlphaToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
var
  I, J: Integer;
  buf: array of byte;
begin
  SetLength(buf, ARaster.Width);

  for I := 0 to ARaster.Height - 1 do
    begin
      for J := 0 to ARaster.Width - 1 do
          buf[J] := ARaster.PixelAlpha[J, I];
      RawStream.Write(buf[0], ARaster.Width)
    end;
  RawStream.Position := 0;
  SetLength(buf, 0);
end;

function EncodeJpegLSRasterAlphaToStream(ARaster: TMemoryRaster; const Stream: TCoreClassStream): Boolean;
var
  rgbStream, alphaStream: TMemoryStream64;
  hflag: Word;
  rgbSiz, alphaSiz: Integer;
  LInput: TMemoryStream64;
  info: TJlsParameters;
begin
  Result := False;
  rgbStream := TMemoryStream64.Create;
  alphaStream := TMemoryStream64.Create;

  if EncodeJpegLSRasterToStream3(ARaster, rgbStream) then
    begin
      LInput := TMemoryStream64.Create;
      FillPtrByte(@info, SizeOf(info), 0);

      try
        jls_RasterAlphaToRaw1(ARaster, LInput);
        info.Width := ARaster.Width;
        info.Height := ARaster.Height;
        info.BitsPerSample := 8;
        info.Components := 1;
        info.Custom.T1 := 3;
        info.Custom.T2 := 7;
        info.Custom.T3 := 21;
        info.Custom.Reset := 64;
        info.AllowedLossyError := 0;

        Result := jpegls_compress(LInput, alphaStream, @info);
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
      Stream.Write(hflag, 2);
      Stream.Write(rgbSiz, 4);
      Stream.Write(alphaSiz, 4);
      Stream.Write(rgbStream.Memory^, rgbStream.Size);
      Stream.Write(alphaStream.Memory^, alphaStream.Size);
    end;

  DisposeObject([rgbStream, alphaStream]);
end;

function EncodeJpegLSRasterToStream3(ARaster: TMemoryRaster; const Stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@info, SizeOf(info), 0);

  try
    jls_RasterToRaw3(ARaster, LInput);
    info.Width := ARaster.Width;
    info.Height := ARaster.Height;
    info.BitsPerSample := 8;
    info.Components := 3;
    info.Custom.T1 := 3;
    info.Custom.T2 := 7;
    info.Custom.T3 := 21;
    info.Custom.Reset := 64;
    info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, Stream, @info);
  finally
      LInput.Free;
  end;
end;

function EncodeJpegLSRasterToStream1(ARaster: TMemoryRaster; const Stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@info, SizeOf(info), 0);

  try
    jls_RasterToRaw1(ARaster, LInput);
    info.Width := ARaster.Width;
    info.Height := ARaster.Height;
    info.BitsPerSample := 8;
    info.Components := 1;
    info.Custom.T1 := 3;
    info.Custom.T2 := 7;
    info.Custom.T3 := 21;
    info.Custom.Reset := 64;
    info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, Stream, @info);
  finally
      LInput.Free;
  end;
end;

procedure jls_RawToRaster(const AStream: TMemoryStream64; var info: TJlsParameters; const Output: TMemoryRaster);
var
  J, I: Integer;
  Src: PBYTE;
  srcword: PWord;
  R, G, B, A: byte;
begin
  case info.Components of
    1: case info.BitsPerSample of
        8:
          begin
            Output.SetSize(info.Width, info.Height);

            Src := AStream.Memory;
            for J := 0 to Output.Height - 1 do
              for I := 0 to info.Width - 1 do
                begin
                  Output.PixelGray[I, J] := Src^;
                  Inc(Src);
                end;
          end;
        10, 12, 15:
          begin
            Output.SetSize(info.Width, info.Height);
            srcword := AStream.Memory;

            for J := 0 to Output.Height - 1 do
              for I := 0 to info.Width - 1 do
                begin
                  Output.PixelGray[I, J] := srcword^;
                  Inc(srcword);
                end;
          end;
        16:
          begin
            Output.SetSize(info.Width, info.Height);
            srcword := AStream.Memory;

            for J := 0 to Output.Height - 1 do
              for I := 0 to info.Width - 1 do
                begin
                  R := Word(((srcword^ and $F800) shr 8)); // to rgb888
                  G := Word(((srcword^ and $07E0) shr 3));
                  B := Word(((srcword^ and $001F) shl 3));
                  Output.PixelGray[I, J] := ((R shl 1) + (G shl 2) + G + B) shr 3;
                  Inc(srcword);
                end;
          end;
        else
          RaiseInfo('decode error');
      end;
    3:
      if info.BitsPerSample = 8 then
        begin
          Output.SetSize(info.Width, info.Height);

          Src := AStream.Memory;
          for J := 0 to Output.Height - 1 do
            for I := 0 to info.Width - 1 do
              begin
                R := Src^;
                Inc(Src);
                G := Src^;
                Inc(Src);
                B := Src^;
                Inc(Src);
                Output.Pixel[I, J] := RasterColor(R, G, B, 255);
              end;
        end
      else
          RaiseInfo('decode error');
  end;
end;

function DecodeJpegLSRasterFromStream(const Stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
var
  LOutput: TMemoryStream64;
  info: TJlsParameters;
begin
  LOutput := TMemoryStream64.Create;
  FillPtrByte(@info, SizeOf(info), 0);
  try
    Result := jpegls_decompress(Stream, LOutput, @info);

    if Result then
        jls_RawToRaster(LOutput, info, ARaster);
  finally
      LOutput.Free;
  end;
end;

function DecodeJpegLSRasterAlphaFromStream(const Stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
var
  hflag: Word;
  rgbSiz, alphaSiz: Integer;
  rgbStream, alphaStream: TMemoryStream64;
  LOutput: TMemoryStream64;
  info: TJlsParameters;
  I, J: Integer;
  Src: PBYTE;
begin
  Result := False;
  hflag := 0;
  Stream.Read(hflag, 2);
  if hflag <> $8D45 then
      Exit;

  Stream.Read(rgbSiz, 4);
  Stream.Read(alphaSiz, 4);

  rgbStream := TMemoryStream64.Create;
  alphaStream := TMemoryStream64.Create;

  try
    rgbStream.CopyFrom(Stream, rgbSiz);
    if alphaSiz > 0 then
        alphaStream.CopyFrom(Stream, alphaSiz);

    rgbStream.Position := 0;
    alphaStream.Position := 0;

    if DecodeJpegLSRasterFromStream(rgbStream, ARaster) and (alphaSiz > 0) then
      begin
        LOutput := TMemoryStream64.Create;
        FillPtrByte(@info, SizeOf(info), 0);
        try
          if jpegls_decompress(alphaStream, LOutput, @info) and (info.Components = 1) and (info.BitsPerSample = 8) then
            begin
              Src := LOutput.Memory;
              for J := 0 to ARaster.Height - 1 do
                begin
                  for I := 0 to ARaster.Width - 1 do
                    begin
                      ARaster.PixelAlpha[I, J] := Src^;
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

function EncodeJpegLSGrayRasterToStream(const ARaster: PByteRaster; const Stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@info, SizeOf(info), 0);

  try
    jls_GrayRasterToRaw1(ARaster, LInput);
    info.Width := Length(ARaster^[0]);
    info.Height := Length(ARaster^);
    info.BitsPerSample := 8;
    info.Components := 1;
    info.Custom.T1 := 3;
    info.Custom.T2 := 7;
    info.Custom.T3 := 21;
    info.Custom.Reset := 64;
    info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, Stream, @info);
  finally
      LInput.Free;
  end;
end;

function DecodeJpegLSGrayRasterFromStream(const Stream: TCoreClassStream; var ARaster: TByteRaster): Boolean;
var
  LOutput: TMemoryStream64;
  info: TJlsParameters;
  J, I: Integer;
  Src: PBYTE;
begin
  Result := False;
  LOutput := TMemoryStream64.Create;
  FillPtrByte(@info, SizeOf(info), 0);
  try
    if jpegls_decompress(Stream, LOutput, @info) and (info.Components = 1) and (info.BitsPerSample = 8) then
      begin
        SetLength(ARaster, info.Height, info.Width);
        Src := LOutput.Memory;
        for J := 0 to info.Height - 1 do
          for I := 0 to info.Width - 1 do
            begin
              ARaster[J, I] := Src^;
              Inc(Src);
            end;
        Result := True;
      end;
  finally
      LOutput.Free;
  end;
end;
