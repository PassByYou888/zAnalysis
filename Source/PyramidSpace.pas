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


uses Math, CoreClasses, MemoryRaster, Geometry2DUnit;

type
  TGFloat = Single;

  TGaussVec   = packed array of TGFloat;
  PGaussVec   = ^TGaussVec;
  TGaussSpace = packed array of TGaussVec;
  PGaussSpace = ^TGaussSpace;

  TGaussSpaceIntegral = packed array of TGaussSpace;
  PGaussSpaceIntegral = ^TGaussSpaceIntegral;

  TPyramidCoor = packed record
    PyramidVec: TVec2; // absolute coordinate in the pyramid(scale space)
    RealVec: TVec2;    // real scaled [0,1] coordinate in the original image
    AbsVec: TVec2;     // absolute sampler coordinal in the original image
    pyr_id: Integer;   // pyramid
    scale_id: Integer; // scale layer id
    Scale: TGFloat;    // scale space

    Angle: TGFloat;     // Angle information
    OriFinish: Boolean; // finish Orientation

    class function Init: TPyramidCoor; static; inline;
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

    function Add(Value: TPyramidCoor): Integer; overload; inline;
    function Add(Value: PPyramidCoor): Integer; overload; inline;
    procedure Add(pl: TPyramidCoorList; const NewCopy: Boolean); overload;
    procedure Delete(Idx: Integer); inline;
    procedure Clear; inline;
    function Count: Integer; inline;

    procedure Assign(Source: TPyramidCoorList);

    property Items[Idx: Integer]: PPyramidCoor read GetItems write SetItems; default;
  end;

  PPyramidData = ^TPyramidData;

  TPyramidData = packed record
  public
    numScale: Integer;
    Width, Height: Integer;
    // scale space(SS)
    SigmaIntegral, MagIntegral, OrtIntegral: TGaussSpaceIntegral;
    // difference of Gaussian Space(DOG)
    Diffs: TGaussSpaceIntegral;
  private
    class procedure ComputeMagAndOrt(const w, h: Integer; const OriSamplerP, MagP, OrtP: PGaussSpace); static;
    class procedure ComputeDiff(const w, h: Integer; const s1, s2, DiffOut: PGaussSpace); static;

    procedure Build(var OriSampler: TGaussSpace; const factorWidth, factorHeight, NScale: Integer; const GaussSigma: TGFloat);
    procedure Free;
  end;

  TDescriptor = class(TCoreClassObject)
  public
  end;

  TPyramid = class(TCoreClassObject)
  private
    Pyramids: array of TPyramidData;

    function isExtrema(const x, y: Integer; const pyr_id, scale_id: Integer): Boolean; inline;
    procedure BuildLocalExtrema(const pyr_id, scale_id: Integer; const transform: Boolean; v2List: TVec2List);

    function ComputePyramidCoor(const ExtremaV2: TVec2; const pyr_id, scale_id: Integer): TPyramidCoorList;
  public
    OriginBitmap: TMemoryRaster;
    OriginSampler: TGaussSpace;

    constructor Create(const bitmap: TMemoryRaster);
    destructor Destroy; override;

    procedure BuildPyramid;

    function BuildAbsoluteExtrema(const transform: Boolean): TVec2List;
    function BuildPyramidExtrema: TPyramidCoorList;
  end;

function between(const Idx, start, over: Integer): Boolean; inline;
function fast_atan(const y, x: TGFloat): TGFloat; inline;
function ClampF(const v, minv, maxv: TGFloat): TGFloat; inline;
procedure CopySampler(var Source, Dest: TGaussSpace); inline;
procedure Sampler(const Source: TMemoryRaster; var Dest: TGaussSpace); overload; inline;
procedure Sampler(var Source: TGaussSpace; const Dest: TMemoryRaster); overload; inline;
procedure ZoomLine(const Source, Dest: PGaussSpace; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: Integer); inline;
procedure ZoomSampler(var Source, Dest: TGaussSpace; const DestWidth, DestHeight: Integer);

type
  TSigmaBuffer = array [0 .. 0] of TGFloat;
  PSigmaBuffer = ^TSigmaBuffer;

  TSigmaKernel = packed record
    SigmaWidth, SigmaCenter: Integer;
    Buff, Weights: PSigmaBuffer;
  end;

procedure BuildSigmaKernel(const Sigma: TGFloat; var kernel: TSigmaKernel);
procedure SigmaRow(var theRow, destRow: TGaussVec; var k: TSigmaKernel); inline;
procedure SigmaSampler(var Source, Dest: TGaussSpace; const Sigma: TGFloat);

procedure SaveSampler(var Source: TGaussSpace; fileName: string);

const
  // gauss kernal
  CGAUSS_Kernel_FACTOR: Integer = 4; // recommendation 4

  // Extrema coordinate
  CPreColorThreshold: TGFloat         = 1.0E-2; // recommendation 5.0E-2
  CJudgeExtremaDiffThreshold: TGFloat = 2.0E-3; // recommendation 2.0E-3

  // pyramid coordinate
  COffsetDepth: Integer              = 4;      // recommendation 4
  COffsetThreshold: TGFloat          = 0.5;    // recommendation 0.5
  CContrastThreshold: TGFloat        = 3.0E-2; // recommendation 3.0E-2
  CEdgeRatio: TGFloat                = 10.0;   // recommendation 10.0
  COrientation_Radius: TGFloat       = 4.5;    // recommendation 4.5
  COrientation_SMOOTH_COUNT: Integer = 2;      // recommendation 2
  CHalfPI: TGFloat                   = 0.5 / PI;

  // pyramid octave
  CNumOctave: Integer = 3; // recommendation 3
  // scale space(SS) and difference of Gaussian Space(DOG)
  CNumScale: Integer = 7; // recommendation 7
  // pyramid scale space factor
  CScaleFactor: TGFloat = 1.4142135623730950488; // recommendation 1.4142135623730950488 .. 2.0
  // gauss sampler factor
  CGaussSigmaFactor: TGFloat = 1.4142135623730950488; // recommendation used CScaleFactor

  // global PI
  CPI: TGFloat = PI;

implementation

uses
  {$IFDEF parallel}
  {$IFDEF FPC}
  mtprocs,
  {$ELSE}
  Threading,
  {$ENDIF FPC}
  {$ENDIF}
  SyncObjs,
  Learn;

function between(const Idx, start, over: Integer): Boolean;
begin
  Result := ((Idx >= start) and (Idx <= over - 1));
end;

function fast_atan(const y, x: TGFloat): TGFloat;
var
  absx, absy, m, a, s, r: TGFloat;
begin
  absx := fabs(x);
  absy := fabs(y);
  m := max(absx, absy);
  if m < 1.0E-6 then
      Exit(-CPI);
  a := min(absx, absy) / m;
  s := a * a;
  r := ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a;
  if absy > absx then
      r := CPI - r;
  if x < 0 then
      r := CPI - r;
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

procedure ZoomLine(const Source, Dest: PGaussSpace; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: Integer);
var
  j: Integer;
  SourceIInt, SourceJInt: Integer;
begin
  for j := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(j / (DestHeight - 1) * (SourceHeight - 1));

      Dest^[j, pass] := Source^[SourceJInt, SourceIInt];
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

procedure BuildSigmaKernel(const Sigma: TGFloat; var kernel: TSigmaKernel);
var
  exp_coeff, wsum, fac: TGFloat;
  i: Integer;
begin
  {$R-}
  kernel.SigmaWidth := ceil(0.3 * (Sigma / 2 - 1) + 0.8) * CGAUSS_Kernel_FACTOR;
  if (kernel.SigmaWidth mod 2 = 0) then
      inc(kernel.SigmaWidth);

  kernel.Buff := System.GetMemory(SizeOf(TGFloat) * kernel.SigmaWidth);

  kernel.SigmaCenter := kernel.SigmaWidth div 2;
  kernel.Weights := @kernel.Buff^[kernel.SigmaCenter];
  kernel.Weights^[0] := 1;

  exp_coeff := -1.0 / (Sigma * Sigma * 2);
  wsum := 1;

  for i := 1 to kernel.SigmaCenter do
    begin
      kernel.Weights^[i] := Exp(i * i * exp_coeff);
      wsum := wsum + kernel.Weights^[i] * 2;
    end;

  fac := 1.0 / wsum;
  kernel.Weights^[0] := fac;

  for i := 1 to kernel.SigmaCenter do
    begin
      kernel.Weights^[i] := kernel.Weights^[i] * fac;
      kernel.Weights^[-i] := kernel.Weights^[i];
    end;
  {$R+}
end;

procedure SigmaRow(var theRow, destRow: TGaussVec; var k: TSigmaKernel);
  function kOffset(const v, l, h: Integer): Integer; inline;
  begin
    Result := v;
    if Result > h then
        Result := h
    else if Result < l then
        Result := l;
  end;

var
  j, n: Integer;
  tb: TGFloat;
begin
  {$R-}
  for j := low(theRow) to high(theRow) do
    begin
      tb := 0;
      for n := -k.SigmaCenter to k.SigmaCenter do
          tb := tb + theRow[kOffset(j + n, 0, high(theRow))] * k.Weights^[n];
      destRow[j] := tb;
    end;
  {$R+}
end;

procedure SigmaSampler(var Source, Dest: TGaussSpace; const Sigma: TGFloat);
var
  w, h: Integer;
  k: TSigmaKernel;
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

  if @Source <> @Dest then
      SetLength(Dest, h, w);

  BuildSigmaKernel(Sigma, k);

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
  System.FreeMemory(k.Buff);
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

class function TPyramidCoor.Init: TPyramidCoor;
begin
  Result.PyramidVec := NullPoint;
  Result.RealVec := NullPoint;
  Result.AbsVec := NullPoint;
  Result.pyr_id := -1;
  Result.scale_id := -1;
  Result.Scale := 0;
  Result.Angle := 0;
  Result.OriFinish := False;
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

function TPyramidCoorList.Add(Value: TPyramidCoor): Integer;
var
  p: PPyramidCoor;
begin
  new(p);
  p^ := Value;
  Result := FList.Add(p);
end;

function TPyramidCoorList.Add(Value: PPyramidCoor): Integer;
begin
  if Value <> nil then
      Result := FList.Add(Value)
  else
      Result := -1;
end;

procedure TPyramidCoorList.Add(pl: TPyramidCoorList; const NewCopy: Boolean);
var
  i: Integer;
  p: PPyramidCoor;
begin
  for i := 0 to pl.Count - 1 do
    begin
      if NewCopy then
        begin
          new(p);
          p^ := pl[i]^;
        end
      else
          p := pl[i];

      FList.Add(p);
    end;
  if not NewCopy then
      pl.FList.Clear;
end;

procedure TPyramidCoorList.Delete(Idx: Integer);
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

class procedure TPyramidData.ComputeMagAndOrt(const w, h: Integer; const OriSamplerP, MagP, OrtP: PGaussSpace);
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
    ort_row^[0] := CPI;

    if between(y, 1, h - 1) then
      begin
        orig_plus := @OriSamplerP^[y + 1];
        orig_minus := @OriSamplerP^[y - 1];

        for x := 1 to w - 2 do
          begin
            dy := orig_plus^[x] - orig_minus^[x];
            dx := orig_row^[x + 1] - orig_row^[x - 1];
            mag_row^[x] := hypot(dx, dy);
            // when dx==dy==0, no need to set ort
            ort_row^[x] := fast_atan(dy, dx) + CPI;
          end;
      end
    else
      begin
        for x := 1 to w - 2 do
          begin
            mag_row^[x] := 0;
            ort_row^[x] := CPI;
          end;
      end;

    mag_row^[w - 1] := 0;
    ort_row^[w - 1] := CPI;
  end;
{$ENDIF FPC}

{$IFNDEF parallel}
  procedure DoFor;
  var
    y, x: Integer;
    mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
    dy, dx: TGFloat;
  begin
    for y := 0 to h - 1 do
      begin
        mag_row := @MagP^[y];
        ort_row := @OrtP^[y];
        orig_row := @OriSamplerP^[y];

        mag_row^[0] := 0;
        ort_row^[0] := CPI;

        if between(y, 1, h - 1) then
          begin
            orig_plus := @OriSamplerP^[y + 1];
            orig_minus := @OriSamplerP^[y - 1];

            for x := 1 to w - 2 do
              begin
                dy := orig_plus^[x] - orig_minus^[x];
                dx := orig_row^[x + 1] - orig_row^[x - 1];
                mag_row^[x] := hypot(dx, dy);
                // when dx==dy==0, no need to set ort
                ort_row^[x] := fast_atan(dy, dx) + CPI;
              end;
          end
        else
          begin
            for x := 1 to w - 2 do
              begin
                mag_row^[x] := 0;
                ort_row^[x] := CPI;
              end;
          end;

        mag_row^[w - 1] := 0;
        ort_row^[w - 1] := CPI;
      end;
  end;
{$ENDIF}


begin
  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, h - 1);
  {$ELSE}
  TParallel.For(0, h - 1, procedure(y: Integer)
    var
      x: Integer;
      mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
      dy, dx: TGFloat;
    begin
      mag_row := @MagP^[y];
      ort_row := @OrtP^[y];
      orig_row := @OriSamplerP^[y];

      mag_row^[0] := 0;
      ort_row^[0] := CPI;

      if between(y, 1, h - 1) then
        begin
          orig_plus := @OriSamplerP^[y + 1];
          orig_minus := @OriSamplerP^[y - 1];

          for x := 1 to w - 2 do
            begin
              dy := orig_plus^[x] - orig_minus^[x];
              dx := orig_row^[x + 1] - orig_row^[x - 1];
              mag_row^[x] := hypot(dx, dy);
              // when dx==dy==0, no need to set ort
              ort_row^[x] := fast_atan(dy, dx) + CPI;
            end;
        end
      else
        begin
          for x := 1 to w - 2 do
            begin
              mag_row^[x] := 0;
              ort_row^[x] := CPI;
            end;
        end;

      mag_row^[w - 1] := 0;
      ort_row^[w - 1] := CPI;
    end);
  {$ENDIF FPC}
  {$ELSE}
  DoFor;
  {$ENDIF}
end;

class procedure TPyramidData.ComputeDiff(const w, h: Integer; const s1, s2, DiffOut: PGaussSpace);
{$IFDEF FPC}
  procedure Nested_ParallelFor(j: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    for i := 0 to w - 1 do
        DiffOut^[j, i] := fabs(s1^[j, i] - s2^[j, i]);
  end;
{$ENDIF FPC}

{$IFNDEF parallel}
  procedure DoFor;
  var
    j, i: Integer;
  begin
    for j := 0 to h - 1 do
      for i := 0 to w - 1 do
          DiffOut^[j, i] := fabs(s1^[j, i] - s2^[j, i]);
  end;
{$ENDIF}


begin
  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, h - 1);
  {$ELSE}
  TParallel.For(0, h - 1, procedure(j: Integer)
    var
      i: Integer;
    begin
      for i := 0 to w - 1 do
          DiffOut^[j, i] := fabs(s1^[j, i] - s2^[j, i]);
    end);
  {$ENDIF FPC}
  {$ELSE}
  DoFor;
  {$ENDIF}
end;

procedure TPyramidData.Build(var OriSampler: TGaussSpace; const factorWidth, factorHeight, NScale: Integer; const GaussSigma: TGFloat);
var
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
  SetLength(MagIntegral, numScale - 1, Height, Width);
  SetLength(OrtIntegral, numScale - 1, Height, Width);
  SetLength(Diffs, numScale - 1, Height, Width);

  if (Width <> oriW) or (Height <> oriH) then
      ZoomSampler(OriSampler, SigmaIntegral[0], Width, Height)
  else
      CopySampler(OriSampler, SigmaIntegral[0]);

  k := GaussSigma;
  for i := 1 to numScale - 1 do
    begin
      SigmaSampler(SigmaIntegral[0], SigmaIntegral[i], k);
      TPyramidData.ComputeMagAndOrt(Width, Height, @SigmaIntegral[i], @MagIntegral[i - 1], @OrtIntegral[i - 1]);
      k := k * CScaleFactor;
    end;

  for i := 0 to numScale - 2 do
      TPyramidData.ComputeDiff(Width, Height, @SigmaIntegral[i], @SigmaIntegral[i + 1], @Diffs[i]);
end;

procedure TPyramidData.Free;
begin
  SetLength(SigmaIntegral, 0, 0, 0);
  SetLength(MagIntegral, 0, 0, 0);
  SetLength(OrtIntegral, 0, 0, 0);
  SetLength(Diffs, 0, 0, 0);
end;

function TPyramid.isExtrema(const x, y: Integer; const pyr_id, scale_id: Integer): Boolean;
var
  dog: PGaussSpace;
  center, cmp1, cmp2, newval: TGFloat;
  bMax, bMin: Boolean;
  di, dj, ds, nl, i: Integer;
begin
  Result := False;
  dog := @Pyramids[pyr_id].Diffs[scale_id];
  center := dog^[y, x];
  if (center < CPreColorThreshold) then
      Exit;
  bMax := True;
  bMin := True;
  cmp1 := center - CJudgeExtremaDiffThreshold;
  cmp2 := center + CJudgeExtremaDiffThreshold;
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

procedure TPyramid.BuildLocalExtrema(const pyr_id, scale_id: Integer; const transform: Boolean; v2List: TVec2List);
var
  w, h: Integer;

  {$IFDEF FPC}
  procedure Nested_ParallelFor(i: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    j: Integer;
  begin
    for j := 1 to w - 2 do
      if isExtrema(j, i, pyr_id, scale_id) then
        begin
          LockObject(v2List);

          if transform then
              v2List.Add(fabs(j / w * OriginBitmap.Width), fabs(i / h * OriginBitmap.Height))
          else
              v2List.Add(j, i);

          UnLockObject(v2List);
        end;
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    i, j: Integer;
  begin
    for i := 1 to h - 2 do
      for j := 1 to w - 2 do
        if isExtrema(j, i, pyr_id, scale_id) then
          begin
            if transform then
                v2List.Add(fabs(j / w * OriginBitmap.Width), fabs(i / h * OriginBitmap.Height))
            else
                v2List.Add(j, i);
          end;
  end;
{$ENDIF}


begin
  w := Pyramids[pyr_id].Width;
  h := Pyramids[pyr_id].Height;

  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, h - 2);
  {$ELSE}
  TParallel.For(1, h - 2, procedure(i: Integer)
    var
      j: Integer;
    begin
      for j := 1 to w - 2 do
        if isExtrema(j, i, pyr_id, scale_id) then
          begin
            LockObject(v2List);

            if transform then
                v2List.Add(fabs(j / w * OriginBitmap.Width), fabs(i / h * OriginBitmap.Height))
            else
                v2List.Add(j, i);

            UnLockObject(v2List);
          end;
    end);
  {$ENDIF FPC}
  {$ELSE}
  DoFor;
  {$ENDIF}
end;

function TPyramid.ComputePyramidCoor(const ExtremaV2: TVec2; const pyr_id, scale_id: Integer): TPyramidCoorList;

// key-pointer integral
  procedure KPIntegral(const dogIntegral: PGaussSpaceIntegral; const x, y, s: Integer; var offset, delta: TLVec);

    function d(const nx, ny, n: TLInt): TGFloat;
    begin
      Result := (dogIntegral^[n, ny, nx]);
    end;

    function ds(const nx, ny: TLInt): TGFloat;
    begin
      Result := (dogIntegral^[s, ny, nx]);
    end;

  var
    v, dxx, dyy, dss, dxy, dys, dsx: TLFloat;
    m: TLMatrix;
    info: TLInt;
    mRep: TMatInvReport;
  begin
    // hessian matrix 3x3
    v := ds(x, y);

    delta[0] := (ds(x + 1, y) - ds(x - 1, y)) / 2;
    delta[1] := (ds(x, y + 1) - ds(x, y - 1)) / 2;
    delta[2] := (d(x, y, s + 1) - d(x, y, s - 1)) / 2;

    dxx := ds(x + 1, y) + ds(x - 1, y) - v - v;
    dyy := ds(x, y + 1) + ds(x, y - 1) - v - v;
    dss := d(x, y, s + 1) + d(x, y, s - 1) - v - v;

    dxy := (ds(x + 1, y + 1) - ds(x + 1, y - 1) - ds(x - 1, y + 1) + ds(x - 1, y - 1)) / 4;
    dys := (d(x, y + 1, s + 1) - d(x, y - 1, s + 1) - d(x, y + 1, s - 1) + d(x, y - 1, s - 1)) / 4;
    dsx := (d(x + 1, y, s + 1) - d(x - 1, y, s + 1) - d(x + 1, y, s - 1) + d(x - 1, y, s - 1)) / 4;

    SetLength(m, 3, 3);
    m[0, 0] := dxx;
    m[1, 1] := dyy;
    m[2, 2] := dss;

    m[0, 1] := dxy;
    m[1, 0] := dxy;

    m[0, 2] := dsx;
    m[2, 0] := dsx;

    m[1, 2] := dys;
    m[2, 1] := dys;

    // Inversion of a matrix given by its LU decomposition and Inverse
    Learn.RMatrixInverse(m, 3, info, mRep);

    // detect a svd matrix
    {$IFDEF DEBUG}
    if info <> 1 then
        RaiseInfo('svd matrix');
    {$ENDIF}
    // matrix multiply
    Learn.MatrixVectorMultiply(m, 0, 2, 0, 2, False, delta, 0, 2, 1, offset, 0, 2, 0);

    SetLength(m, 0, 0);
  end;

  function ComputeAndFixedKeyPoint(var pc: TPyramidCoor): Boolean;
  var
    dog: PGaussSpace;
    w, h, NScale, i: Integer;
    nowx, nowy, nows: Integer;
    dpDone: Boolean;
    offset, delta: TLVec;
    dextr: TLFloat;
  begin
    Result := False;

    dog := @Pyramids[pc.pyr_id].Diffs[pc.scale_id];
    w := Pyramids[pc.pyr_id].Width;
    h := Pyramids[pc.pyr_id].Height;
    NScale := Pyramids[pc.pyr_id].numScale;

    nowx := Trunc(pc.PyramidVec[0]);
    nowy := Trunc(pc.PyramidVec[1]);
    nows := pc.scale_id;

    offset := LVec(3);
    delta := LVec(3);
    dpDone := False;
    for i := 0 to COffsetDepth do
      begin
        if (not between(nowx, 1, w - 1)) or (not between(nowy, 1, h - 1)) or (not between(nows, 1, NScale - 2)) then
            Exit;
        KPIntegral(@Pyramids[pc.pyr_id].Diffs, nowx, nowy, nows, offset, delta);

        dpDone := Learn.AbsMaxVec(offset) < COffsetThreshold;
        if dpDone then
            break;

        inc(nowx, Round(offset[0]));
        inc(nowy, Round(offset[1]));
        inc(nows, Round(offset[2]));
      end;
    if not dpDone then
        Exit;

    dextr := dog^[nowy, nowx] + Learn.APVDotProduct(@offset, 0, 2, @delta, 0, 2) / 2;

    if (dextr < CContrastThreshold) then
        Exit;

    // update pyramid coordinate
    pc.PyramidVec[0] := nowx;
    pc.PyramidVec[1] := nowy;
    pc.scale_id := nows;
    pc.Scale := CGaussSigmaFactor * Power(CScaleFactor, (nows + offset[2]) / NScale);
    pc.RealVec[0] := (nowx + offset[0]) / w;
    pc.RealVec[1] := (nowy + offset[1]) / h;
    Result := True;
  end;

  function wasEndgeReponse(const v2: TVec2; const p: PGaussSpace): Boolean;
  var
    dxx, dxy, dyy, v, det, tr2: TLFloat;
    x, y: Integer;
  begin
    Result := True;
    x := Trunc(v2[0]);
    y := Trunc(v2[1]);
    v := p^[y, x];

    // hessian matrix
    dxx := p^[y, x + 1] + p^[y, x - 1] - v - v;
    dyy := p^[y + 1, x] + p^[y - 1, x] - v - v;
    dxy := (p^[y + 1, x + 1] + p^[y - 1, x - 1] - p^[y + 1, x - 1] - p^[y - 1, x + 1]) / 4;

    det := dxx * dyy - dxy * dxy;

    if det <= 0 then
        Exit;

    tr2 := Learn.AP_Sqr(dxx + dyy);

    if (tr2 / det) < (Learn.AP_Sqr(CEdgeRatio + 1) / CEdgeRatio) then
        Result := False;
  end;

var
  RetCoords: TPyramidCoorList;

  procedure ComputeOrientation(const pc: TPyramidCoor);
  const
    ORI_WINDOW_FACTOR                  = 1.5;
    Orientation_Histogram_Binomial_Num = 36;
    Orientation_Histogram_Peak_Ratio   = 0.8;
  var
    pyramid: PPyramidData;
    mag: PGaussSpace;
    ort: PGaussSpace;
    x, y: Integer;
    gauss_weight_sigma, exp_denom, orient, weight, prev, next, maxbin, fBinomial, thres: TGFloat;
    rad, xx, yy, newx, newy, iBinomial, k, i: Integer;
    hist: array [0 .. Orientation_Histogram_Binomial_Num - 1] of TGFloat;
    np: PPyramidCoor;
  begin
    pyramid := @Pyramids[pc.pyr_id];
    mag := @pyramid^.MagIntegral[pc.scale_id - 1];
    ort := @pyramid^.OrtIntegral[pc.scale_id - 1];

    x := Trunc(pc.PyramidVec[0]);
    y := Trunc(pc.PyramidVec[1]);

    gauss_weight_sigma := pc.Scale * ORI_WINDOW_FACTOR;
    exp_denom := 2 * Learn.AP_Sqr(gauss_weight_sigma);
    rad := Round(pc.Scale * COrientation_Radius);

    for i := low(hist) to high(hist) do
        hist[i] := 0;

    // compute gaussian weighted histogram with inside a circle
    for xx := -rad to rad - 1 do
      begin
        newx := x + xx;
        if not between(newx, 1, pyramid^.Width - 1) then
            continue;

        for yy := -rad to rad - 1 do
          begin
            newy := y + yy;
            if not between(newy, 1, pyramid^.Height - 1) then
                continue;

            if xx * xx + yy * yy > rad * rad then
                continue;
            orient := ort^[newy, newx];
            iBinomial := Round(Orientation_Histogram_Binomial_Num * CHalfPI * orient);
            if (iBinomial = Orientation_Histogram_Binomial_Num) then
                iBinomial := 0;
            // debug detect
            {$IFDEF DEBUG}
            if (iBinomial > Orientation_Histogram_Binomial_Num) then
                RaiseInfo('error');
            {$ENDIF}
            weight := Exp(-(xx * xx + yy * yy) / exp_denom);
            hist[iBinomial] := hist[iBinomial] + weight * mag^[newy, newx];
          end;
      end;

    // smooth the histogram
    for k := 0 to COrientation_SMOOTH_COUNT - 1 do
      begin
        for i := 0 to Orientation_Histogram_Binomial_Num - 1 do
          begin
            prev := hist[IfThen(i = 0, Orientation_Histogram_Binomial_Num - 1, i - 1)];
            next := hist[IfThen(i = Orientation_Histogram_Binomial_Num - 1, 0, i + 1)];
            hist[i] := hist[i] * 0.5 + (prev + next) * 0.25;
          end;
      end;

    maxbin := hist[low(hist)];
    for i := low(hist) + 1 to high(hist) do
      if maxbin < hist[i] then
          maxbin := hist[i];

    thres := maxbin * Orientation_Histogram_Peak_Ratio;

    // choose extreme orientation
    for i := 0 to Orientation_Histogram_Binomial_Num - 1 do
      begin
        prev := hist[IfThen(i = 0, Orientation_Histogram_Binomial_Num - 1, i - 1)];
        next := hist[IfThen(i = Orientation_Histogram_Binomial_Num - 1, 0, i + 1)];

        if (hist[i] > thres) and (hist[i] > max(prev, next)) then
          begin
            // interpolation
            fBinomial := i - 0.5 + (hist[i] - prev) / (prev + next - 2 * hist[i]);

            if (fBinomial < 0) then
                fBinomial := fBinomial + Orientation_Histogram_Binomial_Num
            else if (fBinomial >= Orientation_Histogram_Binomial_Num) then
                fBinomial := fBinomial - Orientation_Histogram_Binomial_Num;

            if RetCoords = nil then
                RetCoords := TPyramidCoorList.Create;

            new(np);
            np^ := pc;
            np^.Angle := fBinomial / Orientation_Histogram_Binomial_Num * 2 * CPI;
            np^.OriFinish := True;
            RetCoords.Add(np);
          end;
      end;
  end;

var
  t: TPyramidCoor;
begin
  Result := nil;
  RetCoords := nil;

  t := TPyramidCoor.Init;
  t.PyramidVec := ExtremaV2;
  t.pyr_id := pyr_id;
  t.scale_id := scale_id;

  if not ComputeAndFixedKeyPoint(t) then
      Exit;

  if not wasEndgeReponse(t.PyramidVec, @Pyramids[t.pyr_id].Diffs[t.scale_id]) then
      Exit;

  t.AbsVec[0] := fabs(t.RealVec[0] * OriginBitmap.Width);
  t.AbsVec[1] := fabs(t.RealVec[1] * OriginBitmap.Height);

  ComputeOrientation(t);

  if RetCoords = nil then
    begin
      RetCoords := TPyramidCoorList.Create;
      RetCoords.Add(t);
    end;

  Result := RetCoords;
end;

constructor TPyramid.Create(const bitmap: TMemoryRaster);
begin
  inherited Create;
  OriginBitmap := bitmap;
  // gray sampler
  Sampler(OriginBitmap, OriginSampler);
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
begin
  if Length(Pyramids) > 0 then
    begin
      for i := low(Pyramids) to high(Pyramids) do
          Pyramids[i].Free;
      SetLength(Pyramids, 0);
    end;

  SetLength(Pyramids, CNumOctave);

  Pyramids[0].Build(OriginSampler, OriginBitmap.Width, OriginBitmap.Height, CNumScale, CGaussSigmaFactor);
  for i := 1 to CNumOctave - 1 do
    begin
      factor := Power(CScaleFactor, -i);

      Pyramids[i].Build(
        OriginSampler,
        ceil(OriginBitmap.Width * factor),
        ceil(OriginBitmap.Height * factor),
        CNumScale, CGaussSigmaFactor);
    end;
end;

function TPyramid.BuildAbsoluteExtrema(const transform: Boolean): TVec2List;
var
  i, j: Integer;
begin
  Result := TVec2List.Create;
  for i := 0 to CNumOctave - 1 do
    for j := 1 to CNumScale - 3 do
        BuildLocalExtrema(i, j, transform, Result);
end;

function TPyramid.BuildPyramidExtrema: TPyramidCoorList;
var
  OctaveIndex, ScaleIndex: Integer;
  ExtremaList: TVec2List;
  PyramidCoordOutput: TPyramidCoorList;

  {$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: TPyramidCoorList;
  begin
    p := ComputePyramidCoor(ExtremaList[pass]^, OctaveIndex, ScaleIndex);
    if p <> nil then
      begin
        LockObject(PyramidCoordOutput);
        PyramidCoordOutput.Add(p, False);
        UnLockObject(PyramidCoordOutput);
        disposeObject(p);
      end;
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: Integer;
    p: TPyramidCoorList;
  begin
    for pass := 0 to ExtremaList.Count - 1 do
      begin
        p := ComputePyramidCoor(ExtremaList[pass]^, OctaveIndex, ScaleIndex);
        if p <> nil then
          begin
            PyramidCoordOutput.Add(p, False);
            disposeObject(p);
          end;
      end;
  end;
{$ENDIF}


begin
  PyramidCoordOutput := TPyramidCoorList.Create;

  for OctaveIndex := 0 to CNumOctave - 1 do
    for ScaleIndex := 1 to CNumScale - 3 do
      begin
        ExtremaList := TVec2List.Create;
        BuildLocalExtrema(OctaveIndex, ScaleIndex, False, ExtremaList);

        {$IFDEF parallel}
        {$IFDEF FPC}
        ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, ExtremaList.Count - 1);
        {$ELSE}
        TParallel.For(0, ExtremaList.Count - 1, procedure(pass: Integer)
          var
            p: TPyramidCoorList;
          begin
            p := ComputePyramidCoor(ExtremaList[pass]^, OctaveIndex, ScaleIndex);
            if p <> nil then
              begin
                LockObject(PyramidCoordOutput);
                PyramidCoordOutput.Add(p, False);
                UnLockObject(PyramidCoordOutput);
                disposeObject(p);
              end;
          end);
        {$ENDIF FPC}
        {$ELSE}
        DoFor;
        {$ENDIF}
        disposeObject(ExtremaList);
      end;

  Result := PyramidCoordOutput;
end;

procedure TestSampler;
var
  mr, mr2: TMemoryRaster;
  Buff, buff2, buff3: TGaussSpace;
  ss: TPyramid;
  v2l: TVec2List;
  v2, v22: TVec2;
  pcl: TPyramidCoorList;
  i: Integer;
begin
  mr := TMemoryRaster.Create;
  mr.LoadFromFile('c:\1.bmp');
  mr2 := TMemoryRaster.Create;
  mr2.Assign(mr);

  ss := TPyramid.Create(mr);
  ss.BuildPyramid;
  v2l := ss.BuildAbsoluteExtrema(True);
  for i := 0 to v2l.Count - 1 do
    begin
      v2 := v2l[i]^;
      if not PointInRect(v2, mr.Bounds2DRect) then
          RaiseInfo('error');

      if PointInRect(v2, RectEndge(mr.Bounds2DRect, -10)) then
          mr.DrawCross(Round(v2[0]), Round(v2[1]), 20, RasterColorF(0, 0, 0, 1));
    end;
  mr.SaveToFile('c:\3.bmp');

  pcl := ss.BuildPyramidExtrema;

  for i := 0 to pcl.Count - 1 do
    begin
      if not PointInRect(pcl[i]^.AbsVec, mr2.Bounds2DRect) then
          RaiseInfo('error');

      if PointInRect(pcl[i]^.AbsVec, RectEndge(mr2.Bounds2DRect, -10)) then
          mr2.DrawCross(Round(pcl[i]^.AbsVec[0]), Round(pcl[i]^.AbsVec[1]), 20, RasterColorF(1, 0, 0, 1.0));

      if pcl[i]^.OriFinish then
        begin
          v2[0] := pcl[i]^.AbsVec[0] + 40 * cos(pcl[i]^.Angle);
          v2[1] := pcl[i]^.AbsVec[1] + 40 * sin(pcl[i]^.Angle);
          if PointInRect(v2, mr2.Bounds2DRect) then
              mr2.Line(pcl[i]^.AbsVec, v2, RasterColorF(0, 1, 0, 1), False);
        end;
    end;
  mr2.SaveToFile('c:\4.bmp');

  disposeObject([ss, v2l, pcl]);
  disposeObject([mr, mr2]);
end;

initialization

finalization

end.
