{ ****************************************************************************** }
{ * Pyramid Space support                                                      * }
{ * create by QQ 600585@qq.com                                                 * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit PyramidSpace;

interface

{$I zDefine.inc}


uses CoreClasses, MemoryRaster, Geometry2DUnit;

const
  CMaxSigmaKerenelSize       = 100;
  CPreColorThreshold         = 5.0E-4;
  CJudgeExtremaDiffThreshold = 2.0E-6;
  CNumOctave                 = 3;
  CNumScale                  = 5;
  CScaleFactor               = 1.4142135623730950488;

type
  TGFloat     = Single;
  TGaussVec   = packed array of TGFloat;
  PGaussVec   = ^TGaussVec;
  TGaussSpace = packed array of TGaussVec;
  PGaussSpace = ^TGaussSpace;

  TGaussSpaceIntegral = packed array of TGaussSpace;
  PGaussSpaceIntegral = ^TGaussSpaceIntegral;

  PPyramidData = ^TPyramidData;

  TPyramidData = packed record
  public
    numScale: Integer;
    Width, Height: Integer;
    // scale space(SS)
    SigmaIntegral, MagIntegral, OrtIntegral: TGaussSpaceIntegral;
    // difference of Gaussian Space(DOG)
    Diffs: TGaussSpaceIntegral;

    procedure Build(var OriSampler: TGaussSpace; const factorWidth, factorHeight, NScale: Integer; const GaussSigma: TGFloat);
    procedure Free;
  end;

  TPyramidCoor = packed record
    PyramidVec: TVec2; // coordinate in the pyramid(scale space)
    RealVec: TVec2;    // scaled [0,1] coordinate in the original image
    pyr_id: Integer;
    scale_id: Integer;
    Angle: TGFloat;
    Scale: TGFloat;
  end;

  PPyramidCoor = ^TPyramidCoor;

  TPyramidCoorList = class(TCoreClassObject)
  private
    FList: TCoreClassList;
  protected
    function GetItems(Idx: Integer): PPyramidCoor;
    procedure SetItems(Idx: Integer; Value: PPyramidCoor);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: PPyramidCoor): Integer;
    function Delete(Idx: Integer): Integer;
    procedure Clear;
    function Count: Integer;

    procedure Assign(Source: TPyramidCoorList);

    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream);

    property Items[Idx: Integer]: PPyramidCoor read GetItems write SetItems; default;
  end;

  TPyramid = class(TCoreClassObject)
  private
    Pyramids: array of TPyramidData;
    procedure GetLocalExtrema(const pyr_id, scale_id: Integer; const transform: Boolean; v2List: TVec2List);
  public
    OriginBitmap: TMemoryRaster;
    OriginSampler: TGaussSpace;
    NumOctave: Integer;
    numScale: Integer;
    ScaleFactor: TGFloat;
    PreColorThreshold: TGFloat;
    JudgeExtremaDiffThreshold: TGFloat;

    constructor Create(const bitmap: TMemoryRaster);
    destructor Destroy; override;

    procedure BuildPyramid;

    function GetVecExtrema: TVec2List;
    function GetPyramidExtrema: TPyramidCoorList;
  end;

function between(const Idx, start, over: Integer): Boolean; inline;
function fast_atan(const y, x: TGFloat): TGFloat; inline;
function ClampF(const v, minv, maxv: TGFloat): TGFloat; inline;
procedure CopySampler(var Source, Dest: TGaussSpace); inline;
procedure Sampler(const Source: TMemoryRaster; var Dest: TGaussSpace); overload;
procedure Sampler(var Source: TGaussSpace; const Dest: TMemoryRaster); overload;
procedure ZoomSampler(var Source, Dest: TGaussSpace; const DestWidth, DestHeight: Integer);
procedure SigmaSampler(var Source, Dest: TGaussSpace; const Sigma: TGFloat);
procedure SaveSampler(var Source: TGaussSpace; fileName: string);

implementation

uses
  {$IFDEF parallel}
  {$IFDEF FPC}
  mtprocs,
  {$ELSE}
  Threading,
  {$ENDIF FPC}
  {$ENDIF}
  Math;

function between(const Idx, start, over: Integer): Boolean;
begin
  Result := ((Idx >= start) and (Idx <= over - 1));
end;

function fast_atan(const y, x: TGFloat): TGFloat;
// fast_atan=atan2,so up speed 40%
var
  absx, absy, m, a, s, r: TGFloat;
begin
  absx := abs(x);
  absy := abs(y);
  m := max(absx, absy);
  if m < 1E-6 then
      Exit(-3.14159265358979323846);
  a := Min(absx, absy) / m;
  s := a * a;
  r := ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a;
  if absy > absx then
      r := 1.57079632679489661923 - r;
  if x < 0 then
      r := 3.14159265358979323846 - r;
  if y < 0 then
      r := -r;
  Result := r;
end;

function ClampF(const v, minv, maxv: TGFloat): TGFloat;
begin
  if v < minv then
      Result := minv
  else if v > maxv then
      Result := maxv
  else
      Result := v;
end;

procedure CopySampler(var Source, Dest: TGaussSpace);
var
  i, j: Integer;
begin
  if Length(Dest) <> Length(Source) then
      SetLength(Dest, Length(Source));

  for j := 0 to Length(Source) - 1 do
    begin
      if Length(Dest[j]) <> Length(Source[j]) then
          SetLength(Dest[j], Length(Source[j]));
      for i := 0 to Length(Source[j]) - 1 do
          Dest[j, i] := Source[j, i];
    end;
end;

procedure Sampler(const Source: TMemoryRaster; var Dest: TGaussSpace);
var
  i, j: Integer;
begin
  SetLength(Dest, Source.Height, Source.Width);
  for j := 0 to Source.Height - 1 do
    for i := 0 to Source.Width - 1 do
        Dest[j, i] := Source.PixelGrayS[i, j];
end;

procedure Sampler(var Source: TGaussSpace; const Dest: TMemoryRaster);
var
  i, j: Integer;
begin
  Dest.SetSize(Length(Source[0]), Length(Source));
  for j := 0 to Dest.Height - 1 do
    for i := 0 to Dest.Width - 1 do
        Dest.PixelGrayS[i, j] := Source[j, i];
end;

procedure ZoomLine(const Source, Dest: PGaussSpace; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: Integer); inline;
var
  j: Integer;
  SourceI, SourceJ: TGFloat;
  SourceIInt, SourceJInt: Integer;
  SourceINext, SourceJNext: Integer;
  SourceIOffset, SourceJOffset: TGFloat;
  v1, v2, v3, v4, k1, k2, k3, k4: TGFloat;
begin
  for j := 0 to DestHeight - 1 do
    begin
      SourceI := (pass / (DestWidth - 1)) * (SourceWidth - 1);
      SourceJ := (j / (DestHeight - 1)) * (SourceHeight - 1);

      SourceIInt := Trunc(SourceI);
      SourceJInt := Trunc(SourceJ);

      SourceINext := Min(SourceWidth - 1, SourceIInt + 1);
      SourceJNext := Min(SourceHeight - 1, SourceJInt + 1);

      SourceIOffset := Frac(SourceI);
      SourceJOffset := Frac(SourceJ);

      k1 := (1 - SourceIOffset) * (1 - SourceJOffset);
      k2 := SourceIOffset * (1 - SourceJOffset);
      k3 := SourceIOffset * SourceJOffset;
      k4 := (1 - SourceIOffset) * SourceJOffset;

      v1 := Source^[SourceJInt, SourceIInt];
      v2 := Source^[SourceJInt, SourceINext];
      v3 := Source^[SourceJNext, SourceINext];
      v4 := Source^[SourceJNext, SourceIInt];

      Dest^[j, pass] := ClampF((v1 * k1) + (v2 * k2) + (v3 * k3) + (v4 * k4), 0.0, 1.0);
    end;
end;

procedure ZoomSampler(var Source, Dest: TGaussSpace; const DestWidth, DestHeight: Integer);
var
  SourceWidth, SourceHeight: Integer;
  SourceP, DestP: PGaussSpace;
  {$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ZoomLine(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to DestWidth - 1 do
        ZoomLine(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF}


begin
  SourceWidth := Length(Source[0]);
  SourceHeight := Length(Source);
  SetLength(Dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @Source;
      DestP := @Dest;

      {$IFDEF parallel}
      {$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
      {$ELSE}
      TParallel.For(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLine(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
      {$ENDIF FPC}
      {$ELSE}
      DoFor;
      {$ENDIF}
    end;
end;

type
  TKernelSize32 = 1 .. CMaxSigmaKerenelSize;

  TKernel32 = packed record
    Size: TKernelSize32;
    Weights: packed array [-CMaxSigmaKerenelSize .. CMaxSigmaKerenelSize] of Double;
  end;

procedure BuildSigmaKernel(var k: TKernel32; const ASigma, AMaxData, ADataGranularity: TGFloat);
var
  j: Integer;
  LLowIndex, LHighIndex: Integer;
  LTemp, LDelta: Double;
  LKernelSize32: TKernelSize32;
begin
  LLowIndex := low(k.Weights);
  LHighIndex := high(k.Weights);

  for j := LLowIndex to LHighIndex do
    begin
      LTemp := j / ASigma;
      k.Weights[j] := Exp(-LTemp * LTemp / 2);
    end;

  LTemp := 0;
  for j := LLowIndex to LHighIndex do
      LTemp := LTemp + k.Weights[j];

  for j := LLowIndex to LHighIndex do
      k.Weights[j] := k.Weights[j] / LTemp;

  LKernelSize32 := CMaxSigmaKerenelSize;
  LDelta := ADataGranularity / (2 * AMaxData);
  LTemp := 0;

  while (LTemp < LDelta) and (LKernelSize32 > 1) do
    begin
      LTemp := LTemp + 2 * k.Weights[LKernelSize32];
      Dec(LKernelSize32);
    end;

  k.Size := LKernelSize32;

  LTemp := 0;
  for j := -k.Size to k.Size do
      LTemp := LTemp + k.Weights[j];

  for j := -k.Size to k.Size do
      k.Weights[j] := k.Weights[j] / LTemp;
end;

procedure SigmaRow(var theRow, destRow: TGaussVec; var k: TKernel32);
  function TrimI32(const L, U, theInt: Integer): Integer; inline;
  begin
    Result := theInt;

    while (Result < L) or (Result > U) do
      begin
        if Result > U then
            Result := U - (Result - U)
        else if Result < 0 then
            Result := L - (Result - L);
      end;
  end;

  function TrimF(const n, AMin, AMax: TGFloat): TGFloat; inline;
  begin
    Result := n;
    if Result < AMin then
        Result := AMin
    else if Result > AMax then
        Result := AMax;
  end;

var
  j, n: Integer;
  tb: TGFloat;
begin
  for j := low(theRow) to high(theRow) do
    begin
      tb := 0;
      for n := -k.Size to k.Size do
          tb := tb + k.Weights[n] * theRow[TrimI32(0, high(theRow), j - n)];
      destRow[j] := TrimF(tb, 0, 1);
    end;
end;

procedure SigmaSampler(var Source, Dest: TGaussSpace; const Sigma: TGFloat);
var
  w, h: Integer;
  k: TKernel32;
  SourceP, DestP: PGaussSpace;

  {$IFDEF FPC}
  procedure Nested_ParallelForH(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    SigmaRow(SourceP^[pass], DestP^[pass], k);
  end;
  procedure Nested_ParallelForW(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    j: Integer;
    LPixels: TGaussVec;
  begin
    SetLength(LPixels, h);
    for j := 0 to h - 1 do
        LPixels[j] := DestP^[j, pass];

    SigmaRow(LPixels, LPixels, k);

    for j := 0 to h - 1 do
        DestP^[j, pass] := LPixels[j];

    SetLength(LPixels, 0);
  end;
{$ENDIF FPC}

{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: Integer;
    j: Integer;
    LPixels: TGaussVec;
  begin
    for pass := 0 to h - 1 do
        SigmaRow(SourceP^[pass], DestP^[pass], k);

    SetLength(LPixels, h);
    for pass := 0 to w - 1 do
      begin
        for j := 0 to h - 1 do
            LPixels[j] := DestP^[j, pass];

        SigmaRow(LPixels, LPixels, k);

        for j := 0 to h - 1 do
            DestP^[j, pass] := LPixels[j];
      end;

    SetLength(LPixels, 0);
  end;
{$ENDIF}


begin
  w := Length(Source[0]);
  h := Length(Source);

  if @Dest <> @Source then
      SetLength(Dest, h, w);

  BuildSigmaKernel(k, Sigma, 255, 1);

  SourceP := @Source;
  DestP := @Dest;

  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelForH, 0, h - 1);
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelForW, 0, w - 1);
  {$ELSE}
  TParallel.For(0, h - 1, procedure(pass: Integer)
    begin
      SigmaRow(SourceP^[pass], DestP^[pass], k);
    end);
  TParallel.For(0, w - 1, procedure(pass: Integer)
    var
      j: Integer;
      LPixels: TGaussVec;
    begin
      SetLength(LPixels, h);
      for j := 0 to h - 1 do
          LPixels[j] := DestP^[j, pass];

      SigmaRow(LPixels, LPixels, k);

      for j := 0 to h - 1 do
          DestP^[j, pass] := LPixels[j];

      SetLength(LPixels, 0);
    end);
  {$ENDIF FPC}
  {$ELSE}
  DoFor;
  {$ENDIF}
end;

procedure SaveSampler(var Source: TGaussSpace; fileName: string);
var
  mr: TMemoryRaster;
begin
  mr := TMemoryRaster.Create;
  Sampler(Source, mr);
  mr.SaveToFile(fileName);
  disposeObject(mr);
end;

function TPyramidCoorList.GetItems(Idx: Integer): PPyramidCoor;
begin
  Result := PPyramidCoor(FList[Idx]);
end;

procedure TPyramidCoorList.SetItems(Idx: Integer; Value: PPyramidCoor);
begin
  FList[Idx] := Value;
end;

constructor TPyramidCoorList.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TPyramidCoorList.Destroy;
begin
  Clear;
  disposeObject(FList);
  inherited Destroy;
end;

function TPyramidCoorList.Add(Value: PPyramidCoor): Integer;
begin
  Result := FList.Add(Value);
end;

function TPyramidCoorList.Delete(Idx: Integer): Integer;
begin
  Dispose(PPyramidCoor(FList[Idx]));
  FList.Delete(Idx);
end;

procedure TPyramidCoorList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PPyramidCoor(FList[i]));
  FList.Clear;
end;

function TPyramidCoorList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TPyramidCoorList.Assign(Source: TPyramidCoorList);
var
  i: Integer;
  p: PPyramidCoor;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    begin
      new(p);
      p^ := Source[i]^;
      Add(p);
    end;
end;

procedure TPyramidCoorList.LoadFromStream(Stream: TCoreClassStream);
var
  i, n: Integer;
  p: PPyramidCoor;
begin
  Clear;
  Stream.Read(n, 4);
  for i := 0 to n - 1 do
    begin
      new(p);
      Stream.Read(p^, SizeOf(TPyramidCoor));
      Add(p);
    end;
end;

procedure TPyramidCoorList.SaveToStream(Stream: TCoreClassStream);
var
  i, n: Integer;
  p: PPyramidCoor;
begin
  n := Count;
  Stream.Write(n, 4);
  for i := 0 to FList.Count - 1 do
    begin
      p := PPyramidCoor(FList[i]);
      Stream.Write(p^, SizeOf(TPyramidCoor));
    end;
end;

procedure ComputeMagAndOrt(const Width, Height: Integer; const OriSamplerP, MagP, OrtP: PGaussSpace);
{$IFDEF FPC}
  procedure Nested_ParallelFor(y: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    x: Integer;
    mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
    dy, dx: TGFloat;
  begin
    mag_row := @MagP^[y];
    ort_row := @OrtP^[y];
    orig_row := @OriSamplerP^[y];

    mag_row^[0] := 0;
    ort_row^[0] := 3.14159265358979323846;

    if between(y, 1, Height - 1) then
      begin
        orig_plus := @OriSamplerP^[y + 1];
        orig_minus := @OriSamplerP^[y - 1];

        for x := 1 to Width - 2 do
          begin
            dy := orig_plus^[x] - orig_minus^[x];
            dx := orig_row^[x + 1] - orig_row^[x - 1];
            mag_row^[x] := hypot(dx, dy);
            // when dx==dy==0, no need to set ort
            ort_row^[x] := fast_atan(dy, dx) + 3.14159265358979323846;
          end;
      end
    else
      begin
        for x := 1 to Width - 2 do
          begin
            mag_row^[x] := 0;
            ort_row^[x] := 3.14159265358979323846;
          end;
      end;

    mag_row^[Width - 1] := 0;
    ort_row^[Width - 1] := 3.14159265358979323846;
  end;
{$ENDIF FPC}

{$IFNDEF parallel}
  procedure DoFor;
  var
    y, x: Integer;
    mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
    dy, dx: TGFloat;
  begin
    for y := 0 to Height - 1 do
      begin
        mag_row := @MagP^[y];
        ort_row := @OrtP^[y];
        orig_row := @OriSamplerP^[y];

        mag_row^[0] := 0;
        ort_row^[0] := 3.14159265358979323846;

        if between(y, 1, Height - 1) then
          begin
            orig_plus := @OriSamplerP^[y + 1];
            orig_minus := @OriSamplerP^[y - 1];

            for x := 1 to Width - 2 do
              begin
                dy := orig_plus^[x] - orig_minus^[x];
                dx := orig_row^[x + 1] - orig_row^[x - 1];
                mag_row^[x] := hypot(dx, dy);
                // when dx==dy==0, no need to set ort
                ort_row^[x] := fast_atan(dy, dx) + 3.14159265358979323846;
              end;
          end
        else
          begin
            for x := 1 to Width - 2 do
              begin
                mag_row^[x] := 0;
                ort_row^[x] := 3.14159265358979323846;
              end;
          end;

        mag_row^[Width - 1] := 0;
        ort_row^[Width - 1] := 3.14159265358979323846;
      end;
  end;
{$ENDIF}


begin
  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Height - 1);
  {$ELSE}
  TParallel.For(0, Height - 1, procedure(y: Integer)
    var
      x: Integer;
      mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
      dy, dx: TGFloat;
    begin
      mag_row := @MagP^[y];
      ort_row := @OrtP^[y];
      orig_row := @OriSamplerP^[y];

      mag_row^[0] := 0;
      ort_row^[0] := 3.14159265358979323846;

      if between(y, 1, Height - 1) then
        begin
          orig_plus := @OriSamplerP^[y + 1];
          orig_minus := @OriSamplerP^[y - 1];

          for x := 1 to Width - 2 do
            begin
              dy := orig_plus^[x] - orig_minus^[x];
              dx := orig_row^[x + 1] - orig_row^[x - 1];
              mag_row^[x] := hypot(dx, dy);
              // when dx==dy==0, no need to set ort
              ort_row^[x] := fast_atan(dy, dx) + 3.14159265358979323846;
            end;
        end
      else
        begin
          for x := 1 to Width - 2 do
            begin
              mag_row^[x] := 0;
              ort_row^[x] := 3.14159265358979323846;
            end;
        end;

      mag_row^[Width - 1] := 0;
      ort_row^[Width - 1] := 3.14159265358979323846;
    end);
  {$ENDIF FPC}
  {$ELSE}
  DoFor;
  {$ENDIF}
end;

procedure ComputeDiff(const Width, Height: Integer; const s1, s2, DiffOut: PGaussSpace);
{$IFDEF FPC}
  procedure Nested_ParallelFor(j: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    for i := 0 to Width - 1 do
        DiffOut^[j, i] := abs(s1^[j, i] - s2^[j, i]);
  end;
{$ENDIF FPC}

{$IFNDEF parallel}
  procedure DoFor;
  var
    j, i: Integer;
  begin
    for j := 0 to Height - 1 do
      for i := 0 to Width - 1 do
          DiffOut^[j, i] := abs(s1^[j, i] - s2^[j, i]);
  end;
{$ENDIF}


begin
  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Height - 1);
  {$ELSE}
  TParallel.For(0, Height - 1, procedure(j: Integer)
    var
      i: Integer;
    begin
      for i := 0 to Width - 1 do
          DiffOut^[j, i] := abs(s1^[j, i] - s2^[j, i]);
    end);
  {$ENDIF FPC}
  {$ELSE}
  DoFor;
  {$ENDIF}
end;

procedure TPyramidData.Build(var OriSampler: TGaussSpace; const factorWidth, factorHeight, NScale: Integer; const GaussSigma: TGFloat);
var
  tempSampler: TGaussSpace;
  oriW, oriH: Integer;
  i, j: Integer;
  k: TGFloat;
begin
  oriW := Length(OriSampler[0]);
  oriH := Length(OriSampler);

  Width := factorWidth;
  Height := factorHeight;

  numScale := NScale;
  SetLength(SigmaIntegral, numScale, Height, Width);
  SetLength(MagIntegral, numScale, Height, Width);
  SetLength(OrtIntegral, numScale, Height, Width);
  SetLength(Diffs, numScale - 1, Height, Width);

  if (Width <> oriW) or (Height <> oriH) then
    begin
      SigmaSampler(OriSampler, tempSampler, Min(2.0, max(Width / oriW, Height / oriH) * 0.5));
      ZoomSampler(tempSampler, SigmaIntegral[0], Width, Height);
      SetLength(tempSampler, 0, 0);
    end
  else
    begin
      CopySampler(OriSampler, SigmaIntegral[0]);
    end;

  k := GaussSigma;

  ComputeMagAndOrt(Width, Height, @SigmaIntegral[0], @MagIntegral[0], @OrtIntegral[0]);

  for i := 1 to numScale - 1 do
    begin
      SigmaSampler(SigmaIntegral[0], SigmaIntegral[i], k);
      ComputeMagAndOrt(Width, Height, @SigmaIntegral[i], @MagIntegral[i], @OrtIntegral[i]);
      k := k * GaussSigma;
    end;

  for i := 0 to numScale - 2 do
      ComputeDiff(Width, Height, @SigmaIntegral[i], @SigmaIntegral[i + 1], @Diffs[i]);
end;

procedure TPyramidData.Free;
begin
  SetLength(SigmaIntegral, 0, 0, 0);
  SetLength(MagIntegral, 0, 0, 0);
  SetLength(OrtIntegral, 0, 0, 0);
  SetLength(Diffs, 0, 0, 0);
end;

procedure TPyramid.GetLocalExtrema(const pyr_id, scale_id: Integer; const transform: Boolean; v2List: TVec2List);

  function isExtrema(const x, y: Integer): Boolean;
  var
    dog: PGaussSpace;
    center, cmp1, cmp2, newval: TGFloat;
    bMax, bMin: Boolean;
    di, dj, ds, nl, i: Integer;
  begin
    Result := False;
    dog := @Pyramids[pyr_id].Diffs[scale_id];
    center := dog^[y, x];
    if (center < PreColorThreshold) then
        Exit;
    bMax := True;
    bMin := True;
    cmp1 := center - JudgeExtremaDiffThreshold;
    cmp2 := center + JudgeExtremaDiffThreshold;
    // try same scale
    for di := -1 to 2 - 1 do
      for dj := -1 to 2 - 1 do
        begin
          if (di = 0) and (dj = 0) then
              continue;
          newval := dog^[y + di, x + dj];
          if (newval >= cmp1) then
              bMax := False;
          if (newval <= cmp2) then
              bMin := False;
          if (not bMax) and (not bMin) then
              Exit;
        end;
    // try adjacent scale
    ds := -1;
    while ds < 2 do
      begin
        nl := scale_id + ds;
        for di := -1 to 1 do
          for i := 0 to 2 do
            begin
              newval := Pyramids[pyr_id].Diffs[nl][y + di][x - 1 + i];
              if (newval >= cmp1) then
                  bMax := False;
              if (newval <= cmp2) then
                  bMin := False;
              if (not bMax) and (not bMin) then
                  Exit;
            end;
        inc(ds, 2);
      end;
    Result := True;
  end;

var
  i, j: Integer;
  w, h: Integer;
begin
  w := Pyramids[pyr_id].Width;
  h := Pyramids[pyr_id].Height;
  for i := 1 to h - 2 do
    for j := 1 to w - 2 do
      if isExtrema(j, i) then
        begin
          if transform then
              v2List.Add(j / w * OriginBitmap.Width, i / h * OriginBitmap.Height)
          else
              v2List.Add(j, i);
        end;
end;

constructor TPyramid.Create(const bitmap: TMemoryRaster);
begin
  inherited Create;

  OriginBitmap := bitmap;
  // gray sampler
  Sampler(OriginBitmap, OriginSampler);
  // init param
  NumOctave := CNumOctave;
  numScale := CNumScale;
  ScaleFactor := CScaleFactor;
  PreColorThreshold := CPreColorThreshold;
  JudgeExtremaDiffThreshold := CJudgeExtremaDiffThreshold;
end;

destructor TPyramid.Destroy;
var
  i: Integer;
begin
  if Length(Pyramids) > 0 then
    begin
      for i := low(Pyramids) to high(Pyramids) do
          Pyramids[i].Free;
      SetLength(Pyramids, 0);
    end;
  inherited Destroy;
end;

procedure TPyramid.BuildPyramid;
var
  i: Integer;
  factor: TGFloat;
  w, h: Integer;
begin
  if Length(Pyramids) > 0 then
    begin
      for i := low(Pyramids) to high(Pyramids) do
          Pyramids[i].Free;
      SetLength(Pyramids, 0);
    end;

  SetLength(Pyramids, NumOctave);

  // build scale and diff space
  Pyramids[0].Build(OriginSampler, OriginBitmap.Width, OriginBitmap.Height, numScale, ScaleFactor);
  for i := 1 to NumOctave - 1 do
    begin
      factor := Power(ScaleFactor, -i);
      w := ceil(OriginBitmap.Width * factor);
      h := ceil(OriginBitmap.Height * factor);
      Pyramids[i].Build(OriginSampler, w, h, numScale, ScaleFactor);
    end;
end;

function TPyramid.GetVecExtrema: TVec2List;
var
  i, j: Integer;
begin
  Result := TVec2List.Create;
  for i := 0 to NumOctave - 1 do
    for j := 1 to numScale - 3 do
        GetLocalExtrema(i, j, True, Result);
end;

function TPyramid.GetPyramidExtrema: TPyramidCoorList;
var
  i, j, n: Integer;
  v2l: TVec2List;
  p: PPyramidCoor;
begin
  Result := TPyramidCoorList.Create;
  v2l := TVec2List.Create;
  for i := 0 to NumOctave - 1 do
    for j := 1 to numScale - 3 do
      begin
        GetLocalExtrema(i, j, False, v2l);
        // fill extream with matrix

        //
      end;
  disposeObject(v2l);
end;

procedure TestSampler;
var
  mr: TMemoryRaster;
  buff, buff2, buff3: TGaussSpace;
  ss: TPyramid;
  v2l: TVec2List;
  v2: TVec2;
  i: Integer;
begin
  mr := TMemoryRaster.Create;
  mr.LoadFromFile('c:\1.bmp');

  ss := TPyramid.Create(mr);
  ss.BuildPyramid;
  v2l := ss.GetVecExtrema;
  for i := 0 to v2l.Count - 1 do
    begin
      v2 := v2l[i]^;
      if not PointInRect(v2, mr.Bounds2DRect) then
          raiseInfo('error');

      if PointInRect(v2, RectEndge(mr.Bounds2DRect, -5)) then
          mr.DrawCross(round(v2[0]), round(v2[1]), 10, RasterColorF(1, 0, 0, 0.5));
    end;
  mr.SaveToFile('c:\3.bmp');

  disposeObject([ss, v2l]);

  Sampler(mr, buff);
  ZoomSampler(buff, buff2, 200, 200);
  SigmaSampler(buff2, buff3, 11);
  Sampler(buff3, mr);
  mr.SaveToFile('c:\2.bmp');
  disposeObject(mr);
end;

initialization

// TestSampler;

finalization

end.
