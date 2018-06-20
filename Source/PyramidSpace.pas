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


uses Math, CoreClasses, MemoryRaster, Geometry2DUnit, UnicodeMixedLib, DataFrameEngine, LearnTypes;

{$REGION 'PyramidTypes'}


type
  TGFloat = Single;

  TGSamplerMode = (gsmColor, gsmGray);

  TSigmaBuffer = array [0 .. MaxInt div SizeOf(TGFloat) - 1] of TGFloat;
  PSigmaBuffer = ^TSigmaBuffer;

  TSigmaKernel = packed record
    SigmaWidth, SigmaCenter: TLInt;
    Weights: PSigmaBuffer;
  end;

  TGaussVec   = packed array of TGFloat;
  PGaussVec   = ^TGaussVec;
  TGaussSpace = packed array of TGaussVec;
  PGaussSpace = ^TGaussSpace;

  TGaussSpaceIntegral = packed array of TGaussSpace;
  PGaussSpaceIntegral = ^TGaussSpaceIntegral;

  TPyramids = class;

  TExtremaCoor = packed record
    PyramidCoor: TVec2; // absolute coordinate in the pyramid(scale space)
    RealCoor: TVec2;    // absolute coordinate in the pyramid(scale space)
    pyr_id: TLInt;      // pyramid id
    scale_id: TLInt;    // scale layer id
    Owner: TPyramids;   // owner
  end;

  PExtremaCoor = ^TExtremaCoor;

  TPyramidCoor = packed record
    PyramidCoor: TVec2;   // absolute coordinate in the pyramid(scale space)
    RealCoor: TVec2;      // real scaled [0,1] coordinate in the original image
    pyr_id: TLInt;        // pyramid id
    scale_id: TLInt;      // scale layer id
    Scale: TGFloat;       // scale space
    Orientation: TGFloat; // Orientation information
    OriFinish: Boolean;   // finish Orientation
    Owner: TPyramids;     // owner
    class function Init: TPyramidCoor; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  PPyramidCoor = ^TPyramidCoor;

  TPyramidCoorList = class(TCoreClassObject)
  private
    FList: TCoreClassList;
    function GetItems(Idx: TLInt): PPyramidCoor;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: TPyramidCoor): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(Value: PPyramidCoor): TLInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(pl: TPyramidCoorList; const NewCopy: Boolean); overload;
    procedure Delete(Idx: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Count: TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Assign(Source: TPyramidCoorList);

    procedure SaveToDataFrame(df: TDataFrameEngine);
    procedure LoadFromDataFrame(df: TDataFrameEngine);

    property Items[Idx: TLInt]: PPyramidCoor read GetItems; default;
  end;

  PPyramidLayer = ^TPyramidLayer;

  TPyramidLayer = packed record
  public
    numScale, Width, Height: TLInt;
    // scale space(SS)
    SigmaIntegral, MagIntegral, OrtIntegral: TGaussSpaceIntegral;
    // difference of Gaussian Space(DOG)
    Diffs: TGaussSpaceIntegral;
  private
    class procedure ComputeMagAndOrt(const w, h: TLInt; const OriSamplerP, MagP, OrtP: PGaussSpace); static;
    class procedure ComputeDiff(const w, h: TLInt; const s1, s2, DiffOut: PGaussSpace); static;

    procedure Build(var OriSampler: TGaussSpace; const factorWidth, factorHeight, NScale: TLInt; const GaussSigma: TGFloat);
    procedure Free;
  end;

  TPyramids = class(TCoreClassObject)
  private
    GaussTransformSpace: TGaussSpace;
    FWidth, FHeight: TLInt;

    Pyramids: packed array of TPyramidLayer;
    SamplerXY: packed array of Byte;
    FViewer: TMemoryRaster;

    function isExtrema(const x, y: TLInt; const pyr_id, scale_id: TLInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure BuildLocalExtrema(const pyr_id, scale_id: TLInt; const transform: Boolean; v2List: TVec2List); overload;
    procedure BuildLocalExtrema(const pyr_id, scale_id: TLInt; ExtremaCoorList: TCoreClassList); overload;

    function KPIntegral(const dogIntegral: PGaussSpaceIntegral; const x, y, s: TLInt; var offset, delta: TLVec): Boolean;
    function ComputeKeyPoint(const ExtremaCoor: TVec2; const pyr_id, scale_id: TLInt; var Output: TPyramidCoor): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComputeEndgeReponse(const ExtremaCoor: TVec2; const p: PGaussSpace): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComputePyramidCoor(const FilterEndge, FilterOri: Boolean; const ExtremaV2: TVec2; const pyr_id, scale_id: TLInt): TPyramidCoorList;
  public
    constructor CreateWithRaster(const raster: TMemoryRaster); overload;
    constructor CreateWithRaster(const fn: string); overload;
    constructor CreateWithRaster(const stream: TCoreClassStream); overload;
    constructor CreateWithGauss(const spr: PGaussSpace);
    destructor Destroy; override;

    property Width: TLInt read FHeight;
    property Height: TLInt read FHeight;

    procedure SetRegion(const clip: TVec2List); overload;
    procedure SetRegion(var mat: TLBMatrix); overload;

    procedure BuildPyramid;

    function BuildAbsoluteExtrema: TVec2List;
    function BuildPyramidExtrema(const FilterOri: Boolean): TPyramidCoorList;

    function BuildViewer(const v2List: TVec2List; const Radius: TGFloat; const color: TRasterColor): TMemoryRaster; overload;
    function BuildViewer(const cList: TPyramidCoorList; const Radius: TGFloat; const color: TRasterColor): TMemoryRaster; overload;
    class procedure BuildToViewer(const cList: TPyramidCoorList; const Radius: TGFloat; const color: TRasterColor; const RasterViewer: TMemoryRaster); overload;
    class procedure BuildToViewer(const cList: TPyramidCoorList; const Radius: TGFloat; const RasterViewer: TMemoryRaster); overload;
  end;

  TFeature = class;

  PDescriptor = ^TDescriptor;

  TDescriptor = packed record
    descriptor: TLVec;    // feature vector
    coor: TVec2;          // real scaled [0,1] coordinate in the original image
    AbsCoor: TVec2;       // absolute sampler coordinal in the original image
    Orientation: TGFloat; // Orientation information
    index: TLInt;         // index in Fetature
    Owner: TFeature;      // owner
  end;

  PMatchInfo = ^TMatchInfo;

  TMatchInfo = packed record
    d1, d2: PDescriptor;
    dist: TGFloat; // Descriptor distance
  end;

  TArrayMatchInfo = array of TMatchInfo;
  PArrayMatchInfo = ^TArrayMatchInfo;

  // feature on sift
  TFeature = class(TCoreClassObject)
  private const
    CPI2               = 2 * PI;
    CSQRT1_2           = 0.707106781186547524401;
    CDESC_HIST_BIN_NUM = 8;
    CNUM_BIN_PER_RAD   = CDESC_HIST_BIN_NUM / CPI2;
    CDESC_HIST_WIDTH   = 4;
    DESCRIPTOR_LENGTH  = CDESC_HIST_WIDTH * CDESC_HIST_WIDTH * CDESC_HIST_BIN_NUM;
  protected
    FDescriptorBuff: packed array of TDescriptor;
    FPyramidCoordList: TPyramidCoorList;
    FWidth, FHeight: TLInt;
    // gray format
    FViewer: packed array of Byte;
    // internal used
    FInternalVec: TVec2;
    // user custom
    FUserData: Pointer;

    procedure ComputeDescriptor(const p: PPyramidCoor; var desc: TLVec); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure BuildFeature(Pyramids: TPyramids);
  public
    constructor CreateWithPyramids(Pyramids: TPyramids);
    constructor CreateWithRaster(const raster: TMemoryRaster; const clip: TVec2List); overload;
    constructor CreateWithRaster(const raster: TMemoryRaster; var mat: TLBMatrix); overload;
    constructor CreateWithRaster(const raster: TMemoryRaster); overload;
    constructor CreateWithRaster(const fn: string); overload;
    constructor CreateWithRaster(const stream: TCoreClassStream); overload;
    constructor CreateWithSampler(const spr: PGaussSpace);
    constructor Create;
    destructor Destroy; override;

    function Count: TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetFD(const index: TLInt): PDescriptor;
    property FD[const index: TLInt]: PDescriptor read GetFD; default;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    function CreateViewer: TMemoryRaster;
    function CreateFeatureViewer(const FeatureRadius: TGFloat; const color: TRasterColor): TMemoryRaster;

    property Width: TLInt read FHeight;
    property Height: TLInt read FHeight;
    property UserData: Pointer read FUserData write FUserData;
  end;

{$ENDREGION 'PyramidTypes'}

{$REGION 'PyramidFunctions'}


function diff(const f1, f2: TGFloat): TGFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function between(const Idx, start, over: TLInt): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function between(const Idx, start, over: TGFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CopySampler(var Source, dest: TGaussSpace); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SamplerAlpha(const Source: TMemoryRaster; var dest: TGaussSpace); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SamplerAlpha(var Source: TGaussSpace; const dest: TMemoryRaster); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Sampler(const Source: TMemoryRaster; const wr, wg, wb: TGFloat; const ColorSap: TLInt; var dest: TGaussSpace); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Sampler(const Source: TMemoryRaster; var dest: TGaussSpace); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Sampler(var Source: TGaussSpace; const dest: TMemoryRaster); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Sampler(var Source: TGaussSpace; var dest: TByteRaster); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ZoomLine(const Source, dest: PGaussSpace; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ZoomSampler(var Source, dest: TGaussSpace; const DestWidth, DestHeight: TLInt);
procedure BuildSigmaKernel(const Sigma: TGFloat; var kernel: TSigmaKernel); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SigmaRow(var theRow, destRow: TGaussVec; var k: TSigmaKernel); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SigmaSampler(var Source, dest: TGaussSpace; const Sigma: TGFloat);
procedure SaveSampler(var Source: TGaussSpace; fileName: string); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SaveSamplerToJpegLS(var Source: TGaussSpace; fileName: string); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ComputeSamplerSize(var Width, Height: TLInt);

// square of euclidean
// need avx + sse or GPU
function e_sqr(const sour, dest: PDescriptor): TGFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// feature match
function MatchFeature(const Source, dest: TFeature; var MatchInfo: TArrayMatchInfo): TLFloat;
function BuildMatchInfoView(var MatchInfo: TArrayMatchInfo; const rectWidth: TLInt; const ViewFeature: Boolean): TMemoryRaster;

procedure TestPyramidSpace;

{$ENDREGION 'PyramidFunctions'}

{$REGION 'Options'}


var
  // sampler
  CRED_WEIGHT_SAMPLER: TGFloat;
  CGREEN_WEIGHT_SAMPLER: TGFloat;
  CBLUE_WEIGHT_SAMPLER: TGFloat;
  CSAMPLER_MODE: TGSamplerMode;
  CMAX_GRAY_COLOR_SAMPLER: TLInt;
  CMAX_SAMPLER_WIDTH: TLInt;
  CMAX_SAMPLER_HEIGHT: TLInt;

  // gauss kernal
  CGAUSS_KERNEL_FACTOR: TLInt;

  // pyramid octave
  CNUMBER_OCTAVE: TLInt;

  // scale space(SS) and difference of Gaussian Space(DOG)
  CNUMBER_SCALE: TLInt;

  // pyramid scale space factor
  CSCALE_FACTOR: TGFloat;
  CSIGMA_FACTOR: TGFloat;

  // Extrema
  CGRAY_THRESHOLD: TGFloat;
  CEXTREMA_DIFF_THRESHOLD: TGFloat;
  CFILTER_MAX_KEYPOINT_ENDGE: TLInt;

  // orientation
  COFFSET_DEPTH: TLInt;
  COFFSET_THRESHOLD: TGFloat;
  CCONTRAST_THRESHOLD: TGFloat;
  CEDGE_RATIO: TGFloat;
  CORIENTATION_RADIUS: TGFloat;
  CORIENTATION_SMOOTH_COUNT: TLInt;

  // feature
  CMATCH_REJECT_NEXT_RATIO: TGFloat;
  CDESC_SCALE_FACTOR: TGFloat;
  CDESC_PROCESS_LIGHT: Boolean;

{$ENDREGION 'Options'}

implementation

uses
{$IFDEF parallel}
{$IFDEF FPC}
  mtprocs,
{$ELSE FPC}
  Threading,
{$ENDIF FPC}
{$ENDIF parallel}
  SyncObjs, Learn;

const
  CPI: TGFloat = PI;

function diff(const f1, f2: TGFloat): TGFloat;
begin
  if f1 < f2 then
      Result := fabs(f2 - f1)
  else
      Result := fabs(f1 - f2);
end;

function between(const Idx, start, over: TLInt): Boolean;
begin
  Result := ((Idx >= start) and (Idx <= over - 1));
end;

function between(const Idx, start, over: TGFloat): Boolean;
begin
  Result := ((Idx >= start) and (Idx <= over - 1));
end;

procedure CopySampler(var Source, dest: TGaussSpace);
var
  i, j: TLInt;
begin
  if Length(dest) <> Length(Source) then
      SetLength(dest, Length(Source));

  for j := 0 to Length(Source) - 1 do
    begin
      if Length(dest[j]) <> Length(Source[j]) then
          SetLength(dest[j], Length(Source[j]));
      for i := 0 to Length(Source[j]) - 1 do
          dest[j, i] := Source[j, i];
    end;
end;

procedure SamplerAlpha(const Source: TMemoryRaster; var dest: TGaussSpace);
var
  i, j: TLInt;
begin
  SetLength(dest, Source.Height, Source.Width);
  for j := 0 to Source.Height - 1 do
    for i := 0 to Source.Width - 1 do
        dest[j, i] := Source.PixelAlpha[i, j] / 255;
end;

procedure SamplerAlpha(var Source: TGaussSpace; const dest: TMemoryRaster);
var
  i, j: TLInt;
begin
  dest.SetSize(Length(Source[0]), Length(Source));
  for j := 0 to dest.Height - 1 do
    for i := 0 to dest.Width - 1 do
        dest.PixelAlpha[i, j] := Round(Clamp(Source[j, i], 0.0, 1.0) * 255);
end;

procedure Sampler(const Source: TMemoryRaster; const wr, wg, wb: TGFloat; const ColorSap: TLInt; var dest: TGaussSpace);
var
  i, j, c, F, w, wf: TLInt;
  r, g, b: TGFloat;
begin
  SetLength(dest, Source.Height, Source.Width);
  if ColorSap > 255 then
      c := 256
  else if ColorSap < 1 then
      c := 2
  else
      c := ColorSap;

  F := 256 div c;

  for j := 0 to Source.Height - 1 do
    for i := 0 to Source.Width - 1 do
      begin
        RasterColor2F(Source.Pixel[i, j], r, g, b);
        w := Trunc((r * wr + g * wg + b * wb) / (wr + wg + wb) * 256);
        wf := (w div F) * F;
        dest[j, i] := Clamp(Round(wf) / 256, 0.0, 1.0);
      end;
end;

procedure Sampler(const Source: TMemoryRaster; var dest: TGaussSpace);
var
  i, j: TLInt;
  r, g, b: TGFloat;
begin
  SetLength(dest, Source.Height, Source.Width);
  for j := 0 to Source.Height - 1 do
    for i := 0 to Source.Width - 1 do
      begin
        if CSAMPLER_MODE = TGSamplerMode.gsmColor then
          begin
            RasterColor2F(Source.Pixel[i, j], r, g, b);
            dest[j, i] := Max(r, Max(g, b));
          end
        else
            dest[j, i] := Source.PixelGrayS[i, j];
      end;
end;

procedure Sampler(var Source: TGaussSpace; const dest: TMemoryRaster);
var
  i, j: TLInt;
begin
  dest.SetSize(Length(Source[0]), Length(Source));
  for j := 0 to dest.Height - 1 do
    for i := 0 to dest.Width - 1 do
        dest.PixelGrayS[i, j] := Source[j, i];
end;

procedure Sampler(var Source: TGaussSpace; var dest: TByteRaster);
var
  i, j: TLInt;
begin
  SetLength(dest, Length(Source), Length(Source[0]));
  for j := 0 to Length(Source) - 1 do
    for i := 0 to Length(Source[j]) - 1 do
        dest[j, i] := Round(Clamp(Source[j, i], 0, 1) * 255);
end;

procedure ZoomLine(const Source, dest: PGaussSpace; const pass, SourceWidth, SourceHeight, DestWidth, DestHeight: TLInt);
var
  j: TLInt;
  SourceIInt, SourceJInt: TLInt;
begin
  for j := 0 to DestHeight - 1 do
    begin
      SourceIInt := Round(pass / (DestWidth - 1) * (SourceWidth - 1));
      SourceJInt := Round(j / (DestHeight - 1) * (SourceHeight - 1));

      dest^[j, pass] := Source^[SourceJInt, SourceIInt];
    end;
end;

procedure ZoomSampler(var Source, dest: TGaussSpace; const DestWidth, DestHeight: TLInt);
var
  SourceWidth, SourceHeight: TLInt;
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
    pass: TLInt;
  begin
    for pass := 0 to DestWidth - 1 do
        ZoomLine(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
  end;
{$ENDIF parallel}


begin
  SourceWidth := Length(Source[0]);
  SourceHeight := Length(Source);
  SetLength(dest, DestHeight, DestWidth);

  if (SourceWidth > 1) and (SourceWidth > 1) and (DestWidth > 1) and (DestHeight > 1) then
    begin
      SourceP := @Source;
      DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, DestWidth - 1);
{$ELSE FPC}
      TParallel.For(0, DestWidth - 1, procedure(pass: Integer)
        begin
          ZoomLine(SourceP, DestP, pass, SourceWidth, SourceHeight, DestWidth, DestHeight);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure BuildSigmaKernel(const Sigma: TGFloat; var kernel: TSigmaKernel);
var
  exp_coeff, wsum, fac: TGFloat;
  i: TLInt;
  p: PSigmaBuffer;
begin
  kernel.SigmaWidth := ceil(0.3 * (Sigma * 0.5 - 1) + 0.8) * CGAUSS_KERNEL_FACTOR;
  if (kernel.SigmaWidth mod 2 = 0) then
      inc(kernel.SigmaWidth);

  kernel.Weights := System.GetMemory(SizeOf(TGFloat) * kernel.SigmaWidth);

  kernel.SigmaCenter := kernel.SigmaWidth div 2;
  p := @kernel.Weights^[kernel.SigmaCenter];
  p^[0] := 1;

  exp_coeff := -1.0 / (Sigma * Sigma * 2);
  wsum := 1;

  for i := 1 to kernel.SigmaCenter do
    begin
      p^[i] := Exp(i * i * exp_coeff);
      wsum := wsum + p^[i] * 2;
    end;

  fac := 1.0 / wsum;
  p^[0] := fac;

  for i := 1 to kernel.SigmaCenter do
    begin
      kernel.Weights^[i + kernel.SigmaCenter] := p^[i] * fac;
      kernel.Weights^[-i + kernel.SigmaCenter] := p^[i];
    end;
end;

procedure SigmaRow(var theRow, destRow: TGaussVec; var k: TSigmaKernel);
  function kOffset(const v, l, h: TLInt): TLInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    Result := v;
    if Result > h then
        Result := h
    else if Result < l then
        Result := l;
  end;

var
  j, n: TLInt;
  tb: TGFloat;
begin
  for j := low(theRow) to high(theRow) do
    begin
      tb := 0;
      for n := -k.SigmaCenter to k.SigmaCenter do
          tb := tb + theRow[kOffset(j + n, 0, high(theRow))] * k.Weights^[n + k.SigmaCenter];
      destRow[j] := tb;
    end;
end;

procedure SigmaSampler(var Source, dest: TGaussSpace; const Sigma: TGFloat);
var
  w, h: TLInt;
  k: TSigmaKernel;
  SourceP, DestP: PGaussSpace;

{$IFDEF FPC}
  procedure Nested_ParallelForH(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    SigmaRow(SourceP^[pass], DestP^[pass], k);
  end;
  procedure Nested_ParallelForW(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    j: TLInt;
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
    pass: TLInt;
    j: TLInt;
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
{$ENDIF parallel}


begin
  w := Length(Source[0]);
  h := Length(Source);

  if @Source <> @dest then
      SetLength(dest, h, w);

  BuildSigmaKernel(Sigma, k);

  SourceP := @Source;
  DestP := @dest;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelForH, 0, h - 1);
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelForW, 0, w - 1);
{$ELSE FPC}
  TParallel.For(0, h - 1, procedure(pass: Integer)
    begin
      SigmaRow(SourceP^[pass], DestP^[pass], k);
    end);
  TParallel.For(0, w - 1, procedure(pass: Integer)
    var
      j: TLInt;
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
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  System.FreeMemory(k.Weights);
end;

procedure SaveSampler(var Source: TGaussSpace; fileName: string);
var
  mr: TMemoryRaster;
begin
  mr := NewRaster();
  Sampler(Source, mr);
  SaveRaster(mr, fileName);
  disposeObject(mr);
end;

procedure SaveSamplerToJpegLS(var Source: TGaussSpace; fileName: string);
var
  gray: TByteRaster;
  fs: TCoreClassFileStream;
begin
  Sampler(Source, gray);
  try
    fs := TCoreClassFileStream.Create(fileName, fmCreate);
    EncodeJpegLSGrayRasterToStream(@gray, fs);
  except
  end;
  disposeObject(fs);
end;

procedure ComputeSamplerSize(var Width, Height: TLInt);
var
  F: TGFloat;
begin
  if (Width > CMAX_SAMPLER_WIDTH) then
    begin
      F := CMAX_SAMPLER_WIDTH / Width;
      Width := Round(Width * F);
      Height := Round(Height * F);
    end;
  if (Height > CMAX_SAMPLER_HEIGHT) then
    begin
      F := CMAX_SAMPLER_HEIGHT / Height;
      Width := Round(Width * F);
      Height := Round(Height * F);
    end;
end;

class function TPyramidCoor.Init: TPyramidCoor;
begin
  Result.PyramidCoor := NullPoint;
  Result.RealCoor := NullPoint;
  Result.pyr_id := -1;
  Result.scale_id := -1;
  Result.Scale := 0;
  Result.Orientation := 0;
  Result.OriFinish := False;
  Result.Owner := nil;
end;

function e_sqr(const sour, dest: PDescriptor): TGFloat;
var
  i: TLInt;
  d0, d1, d2, d3, d4, d5, d6, d7: TGFloat;
begin
  // pure pascal
  Result := 0;
  if Length(sour^.descriptor) <> Length(dest^.descriptor) then
      exit;

  // sqr x8 extract
  // need avx + sse or GPU
  i := 0;
  while i < Length(sour^.descriptor) do
    begin
      d0 := dest^.descriptor[i + 0] - sour^.descriptor[i + 0];
      d0 := d0 * d0;

      d1 := dest^.descriptor[i + 1] - sour^.descriptor[i + 1];
      d1 := d1 * d1;

      d2 := dest^.descriptor[i + 2] - sour^.descriptor[i + 2];
      d2 := d2 * d2;

      d3 := dest^.descriptor[i + 3] - sour^.descriptor[i + 3];
      d3 := d3 * d3;

      d4 := dest^.descriptor[i + 4] - sour^.descriptor[i + 4];
      d4 := d4 * d4;

      d5 := dest^.descriptor[i + 5] - sour^.descriptor[i + 5];
      d5 := d5 * d5;

      d6 := dest^.descriptor[i + 6] - sour^.descriptor[i + 6];
      d6 := d6 * d6;

      d7 := dest^.descriptor[i + 7] - sour^.descriptor[i + 7];
      d7 := d7 * d7;

      Result := Result + d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7;
      inc(i, 8);
    end;
end;

function MatchFeature(const Source, dest: TFeature; var MatchInfo: TArrayMatchInfo): TLFloat;
var
  l: TCoreClassList;
  pf1_len, pf2_len: TLInt;
  pf1, pf2: TFeature;
  sqr_memory: array of array of TGFloat;
  reject_ratio_sqr: TGFloat;

{$IFDEF FPC}
  procedure Nested_ParallelFor_sqr(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    j: TLInt;
  begin
    for j := 0 to pf2_len - 1 do
        sqr_memory[pass, j] := e_sqr(pf1[pass], pf2[j]);
  end;

  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    m_idx, j: TLInt;
    dsc1, dsc2: PDescriptor;
    minf, next_minf: TLFloat;
    d: Double;
    pd: PMatchInfo;
  begin
    dsc1 := pf1[pass];
    m_idx := -1;
    minf := MaxRealNumber;
    next_minf := minf;
    // find dsc1 from feat2
    for j := 0 to pf2_len - 1 do
      begin
        d := min(sqr_memory[pass, j], next_minf);
        if (d < minf) then
          begin
            next_minf := minf;
            minf := d;
            m_idx := j;
          end
        else
            next_minf := min(next_minf, d);
      end;

    /// bidirectional rejection
    if (minf > reject_ratio_sqr * next_minf) then
        exit;

    // fix m_idx
    dsc2 := pf2[m_idx];
    for j := 0 to pf1_len - 1 do
      if j <> pass then
        begin
          d := min(sqr_memory[j, m_idx], next_minf);
          next_minf := min(next_minf, d);
        end;
    if (minf > reject_ratio_sqr * next_minf) then
        exit;

    new(pd);
    pd^.d1 := pf1[pass];
    pd^.d2 := pf2[m_idx];
    LockObject(l);
    l.Add(pd);
    UnLockObject(l);
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: TLInt;
    m_idx, j: TLInt;
    dsc1, dsc2: PDescriptor;
    minf, next_minf: TLFloat;
    d: Double;
    pd: PMatchInfo;
  begin
    for pass := 0 to pf1_len - 1 do
      for j := 0 to pf2_len - 1 do
          sqr_memory[pass, j] := e_sqr(pf1[pass], pf2[j]);

    for pass := 0 to pf1_len - 1 do
      begin
        dsc1 := pf1[pass];
        m_idx := -1;
        minf := MaxRealNumber;
        next_minf := minf;
        // find dsc1 from feat2
        for j := 0 to pf2_len - 1 do
          begin
            d := min(sqr_memory[pass, j], next_minf);
            if (d < minf) then
              begin
                next_minf := minf;
                minf := d;
                m_idx := j;
              end
            else
                next_minf := min(next_minf, d);
          end;

        /// bidirectional rejection
        if (minf > reject_ratio_sqr * next_minf) then
            continue;

        // fix m_idx
        dsc2 := pf2[m_idx];
        for j := 0 to pf1_len - 1 do
          if j <> pass then
            begin
              d := min(sqr_memory[j, m_idx], next_minf);
              next_minf := min(next_minf, d);
            end;
        if (minf > reject_ratio_sqr * next_minf) then
            continue;

        new(pd);
        pd^.d1 := pf1[pass];
        pd^.d2 := pf2[m_idx];
        l.Add(pd);
      end;
  end;
{$ENDIF}
  procedure FillMatchInfoAndFreeTemp;
  var
    i: TLInt;
    pd: PMatchInfo;
  begin
    SetLength(MatchInfo, l.Count);
    for i := 0 to l.Count - 1 do
      begin
        pd := PMatchInfo(l[i]);
        MatchInfo[i] := pd^;
        Dispose(pd);
      end;
  end;

begin
  Result := 0;
  pf1_len := Source.Count;
  pf2_len := dest.Count;

  if (pf1_len = 0) or (pf2_len = 0) then
      exit;

  if pf1_len > pf2_len then
    begin
      swap(pf1_len, pf2_len);
      pf1 := dest;
      pf2 := Source;
    end
  else
    begin
      pf1 := Source;
      pf2 := dest;
    end;

  l := TCoreClassList.Create;
  SetLength(sqr_memory, pf1_len, pf2_len);
  reject_ratio_sqr := CMATCH_REJECT_NEXT_RATIO * CMATCH_REJECT_NEXT_RATIO;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor_sqr, 0, pf1_len - 1);
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, pf1_len - 1);
{$ELSE FPC}
  TParallel.For(0, pf1_len - 1, procedure(pass: Integer)
    var
      j: TLInt;
    begin
      for j := 0 to pf2_len - 1 do
          sqr_memory[pass, j] := e_sqr(pf1[pass], pf2[j]);
    end);

  TParallel.For(0, pf1_len - 1, procedure(pass: Integer)
    var
      m_idx, j: TLInt;
      dsc1, dsc2: PDescriptor;
      minf, next_minf: TLFloat;
      d: Double;
      pd: PMatchInfo;
    begin
      dsc1 := pf1[pass];
      m_idx := -1;
      minf := MaxRealNumber;
      next_minf := minf;
      // find dsc1 from feat2
      for j := 0 to pf2_len - 1 do
        begin
          d := min(sqr_memory[pass, j], next_minf);
          if (d < minf) then
            begin
              next_minf := minf;
              minf := d;
              m_idx := j;
            end
          else
              next_minf := min(next_minf, d);
        end;

      // bidirectional rejection
      if (minf > reject_ratio_sqr * next_minf) then
          exit;

      // fix m_idx
      dsc2 := pf2[m_idx];
      for j := 0 to pf1_len - 1 do
        if j <> pass then
          begin
            d := min(sqr_memory[j, m_idx], next_minf);
            next_minf := min(next_minf, d);
          end;
      if (minf > reject_ratio_sqr * next_minf) then
          exit;

      new(pd);
      pd^.d1 := pf1[pass];
      pd^.d2 := pf2[m_idx];
      pd^.dist := sqr_memory[pass, m_idx];
      LockObject(l);
      l.Add(pd);
      UnLockObject(l);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF}
  // free cache
  SetLength(sqr_memory, 0, 0);
  // fill result
  Result := l.Count;
  FillMatchInfoAndFreeTemp;

  disposeObject(l);
end;

function BuildMatchInfoView(var MatchInfo: TArrayMatchInfo; const rectWidth: TLInt; const ViewFeature: Boolean): TMemoryRaster;
var
  mr1, mr2: TMemoryRaster;
  ft1, ft2: TFeature;
  c: Byte;
  i, j: TLInt;

  bV1, bV2: TVec2;

  p: PMatchInfo;
  rc: TRasterColor;
  v1, v2: TVec2;
begin
  if Length(MatchInfo) = 0 then
    begin
      Result := nil;
      exit;
    end;
  Result := NewRaster();

  ft1 := MatchInfo[0].d1^.Owner;
  ft2 := MatchInfo[0].d2^.Owner;

  if ViewFeature then
    begin
      mr1 := ft1.CreateFeatureViewer(8, RasterColorF(0.4, 0.1, 0.1, 0.9));
      mr2 := ft2.CreateFeatureViewer(8, RasterColorF(0.4, 0.1, 0.1, 0.9));
    end
  else
    begin
      mr1 := ft1.CreateViewer;
      mr2 := ft2.CreateViewer;
    end;

  Result.SetSize(mr1.Width + mr2.Width, Max(mr1.Height, mr2.Height), RasterColor(0, 0, 0, 0));
  Result.Draw(0, 0, mr1);
  Result.Draw(mr1.Width, 0, mr2);
  Result.OpenAgg;

  bV1 := ft1.FInternalVec;
  bV2 := ft2.FInternalVec;

  ft1.FInternalVec := Vec2(0, 0);
  ft2.FInternalVec := Vec2(mr1.Width, 0);

  for i := 0 to Length(MatchInfo) - 1 do
    begin
      p := @MatchInfo[i];
      rc := RasterColor(RandomRange(0, 255), RandomRange(0, 255), RandomRange(0, 255), 255);

      v1 := Vec2Add(p^.d1^.AbsCoor, p^.d1^.Owner.FInternalVec);
      v2 := Vec2Add(p^.d2^.AbsCoor, p^.d2^.Owner.FInternalVec);

      Result.FillRect(v1, rectWidth, rc);
      Result.FillRect(v2, rectWidth, rc);

      Result.LineF(v1, v2, rc, True);
    end;

  ft1.FInternalVec := bV1;
  ft2.FInternalVec := bV2;
  disposeObject([mr1, mr2]);
end;

function TPyramidCoorList.GetItems(Idx: TLInt): PPyramidCoor;
begin
  Result := PPyramidCoor(FList[Idx]);
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

function TPyramidCoorList.Add(Value: TPyramidCoor): TLInt;
var
  p: PPyramidCoor;
begin
  new(p);
  p^ := Value;
  Result := FList.Add(p);
end;

function TPyramidCoorList.Add(Value: PPyramidCoor): TLInt;
begin
  if Value <> nil then
      Result := FList.Add(Value)
  else
      Result := -1;
end;

procedure TPyramidCoorList.Add(pl: TPyramidCoorList; const NewCopy: Boolean);
var
  i: TLInt;
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

procedure TPyramidCoorList.Delete(Idx: TLInt);
begin
  Dispose(PPyramidCoor(FList[Idx]));
  FList.Delete(Idx);
end;

procedure TPyramidCoorList.Clear;
var
  i: TLInt;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PPyramidCoor(FList[i]));
  FList.Clear;
end;

function TPyramidCoorList.Count: TLInt;
begin
  Result := FList.Count;
end;

procedure TPyramidCoorList.Assign(Source: TPyramidCoorList);
var
  i: TLInt;
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

procedure TPyramidCoorList.SaveToDataFrame(df: TDataFrameEngine);
var
  i: Integer;
  p: PPyramidCoor;
  d: TDataFrameEngine;
begin
  for i := 0 to Count - 1 do
    begin
      p := GetItems(i);
      d := TDataFrameEngine.Create;
      d.WriteVec2(p^.PyramidCoor);
      d.WriteVec2(p^.RealCoor);
      d.WriteInteger(p^.pyr_id);
      d.WriteInteger(p^.scale_id);
      d.WriteSingle(p^.Scale);
      d.WriteSingle(p^.Orientation);
      d.WriteBool(p^.OriFinish);
      df.WriteDataFrame(d);
      disposeObject(d);
    end;
end;

procedure TPyramidCoorList.LoadFromDataFrame(df: TDataFrameEngine);
var
  p: PPyramidCoor;
  d: TDataFrameEngine;
begin
  Clear;
  while df.Reader.NotEnd do
    begin
      new(p);
      p^.Init;
      d := TDataFrameEngine.Create;
      df.Reader.ReadDataFrame(d);
      p^.PyramidCoor := d.Reader.ReadVec2;
      p^.RealCoor := d.Reader.ReadVec2;
      p^.pyr_id := d.Reader.ReadInteger;
      p^.scale_id := d.Reader.ReadInteger;
      p^.Scale := d.Reader.ReadSingle;
      p^.Orientation := d.Reader.ReadSingle;
      p^.OriFinish := d.Reader.ReadBool;
      Add(p);
      disposeObject(d);
    end;
end;

class procedure TPyramidLayer.ComputeMagAndOrt(const w, h: TLInt; const OriSamplerP, MagP, OrtP: PGaussSpace);
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    x: TLInt;
    mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
    dy, dx: TGFloat;
  begin
    mag_row := @MagP^[pass];
    ort_row := @OrtP^[pass];
    orig_row := @OriSamplerP^[pass];

    mag_row^[0] := 0;
    ort_row^[0] := CPI;

    if between(pass, 1, h - 1) then
      begin
        orig_plus := @OriSamplerP^[pass + 1];
        orig_minus := @OriSamplerP^[pass - 1];

        for x := 1 to w - 2 do
          begin
            dy := orig_plus^[x] - orig_minus^[x];
            dx := orig_row^[x + 1] - orig_row^[x - 1];
            mag_row^[x] := hypot(dx, dy);
            ort_row^[x] := ArcTan2(dy, dx) + CPI;
          end;
      end
    else
      for x := 1 to w - 2 do
        begin
          mag_row^[x] := 0;
          ort_row^[x] := CPI;
        end;

    mag_row^[w - 1] := 0;
    ort_row^[w - 1] := CPI;
  end;
{$ENDIF FPC}

{$IFNDEF parallel}
  procedure DoFor;
  var
    pass, x: TLInt;
    mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
    dy, dx: TGFloat;
  begin
    for pass := 0 to h - 1 do
      begin
        mag_row := @MagP^[pass];
        ort_row := @OrtP^[pass];
        orig_row := @OriSamplerP^[pass];

        mag_row^[0] := 0;
        ort_row^[0] := CPI;

        if between(pass, 1, h - 1) then
          begin
            orig_plus := @OriSamplerP^[pass + 1];
            orig_minus := @OriSamplerP^[pass - 1];

            for x := 1 to w - 2 do
              begin
                dy := orig_plus^[x] - orig_minus^[x];
                dx := orig_row^[x + 1] - orig_row^[x - 1];
                mag_row^[x] := hypot(dx, dy);
                ort_row^[x] := ArcTan2(dy, dx) + CPI;
              end;
          end
        else
          for x := 1 to w - 2 do
            begin
              mag_row^[x] := 0;
              ort_row^[x] := CPI;
            end;

        mag_row^[w - 1] := 0;
        ort_row^[w - 1] := CPI;
      end;
  end;
{$ENDIF parallel}


begin
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, h - 1);
{$ELSE FPC}
  TParallel.For(0, h - 1, procedure(pass: Integer)
    var
      x: TLInt;
      mag_row, ort_row, orig_row, orig_plus, orig_minus: PGaussVec;
      dy, dx: TGFloat;
    begin
      mag_row := @MagP^[pass];
      ort_row := @OrtP^[pass];
      orig_row := @OriSamplerP^[pass];

      mag_row^[0] := 0;
      ort_row^[0] := CPI;

      if between(pass, 1, h - 1) then
        begin
          orig_plus := @OriSamplerP^[pass + 1];
          orig_minus := @OriSamplerP^[pass - 1];

          for x := 1 to w - 2 do
            begin
              dy := orig_plus^[x] - orig_minus^[x];
              dx := orig_row^[x + 1] - orig_row^[x - 1];
              mag_row^[x] := hypot(dx, dy);
              ort_row^[x] := ArcTan2(dy, dx) + CPI;
            end;
        end
      else
        for x := 1 to w - 2 do
          begin
            mag_row^[x] := 0;
            ort_row^[x] := CPI;
          end;

      mag_row^[w - 1] := 0;
      ort_row^[w - 1] := CPI;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
end;

class procedure TPyramidLayer.ComputeDiff(const w, h: TLInt; const s1, s2, DiffOut: PGaussSpace);
{$IFDEF FPC}
  procedure Nested_ParallelFor(j: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: TLInt;
  begin
    for i := 0 to w - 1 do
        DiffOut^[j, i] := diff(s1^[j, i], s2^[j, i]);
  end;
{$ENDIF FPC}

{$IFNDEF parallel}
  procedure DoFor;
  var
    j, i: TLInt;
  begin
    for j := 0 to h - 1 do
      for i := 0 to w - 1 do
          DiffOut^[j, i] := diff(s1^[j, i], s2^[j, i]);
  end;
{$ENDIF parallel}


begin
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, h - 1);
{$ELSE FPC}
  TParallel.For(0, h - 1, procedure(j: Integer)
    var
      i: TLInt;
    begin
      for i := 0 to w - 1 do
          DiffOut^[j, i] := diff(s1^[j, i], s2^[j, i]);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
end;

procedure TPyramidLayer.Build(var OriSampler: TGaussSpace; const factorWidth, factorHeight, NScale: TLInt; const GaussSigma: TGFloat);
var
  Sap: TGaussSpace;
  oriW, oriH: TLInt;
  i, j: TLInt;
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
    begin
      SigmaSampler(OriSampler, Sap, GaussSigma);
      ZoomSampler(Sap, SigmaIntegral[0], Width, Height);
      SetLength(Sap, 0, 0);
    end
  else
      SigmaSampler(OriSampler, SigmaIntegral[0], GaussSigma);

  k := GaussSigma * GaussSigma;
  for i := 1 to numScale - 1 do
    begin
      SigmaSampler(SigmaIntegral[0], SigmaIntegral[i], k);
      TPyramidLayer.ComputeMagAndOrt(Width, Height, @SigmaIntegral[i], @MagIntegral[i - 1], @OrtIntegral[i - 1]);
      k := k * CSCALE_FACTOR;
    end;

  for i := 1 to numScale - 1 do
      TPyramidLayer.ComputeDiff(Width, Height, @SigmaIntegral[i - 1], @SigmaIntegral[i], @Diffs[i - 1]);
end;

procedure TPyramidLayer.Free;
begin
  SetLength(SigmaIntegral, 0, 0, 0);
  SetLength(MagIntegral, 0, 0, 0);
  SetLength(OrtIntegral, 0, 0, 0);
  SetLength(Diffs, 0, 0, 0);
end;

function TPyramids.isExtrema(const x, y: TLInt; const pyr_id, scale_id: TLInt): Boolean;
var
  dog: PGaussSpace;
  center, cmp1, cmp2, newval: TGFloat;
  bMax, bMin: Boolean;
  di, dj, i: TLInt;
begin
  Result := False;

  dog := @Pyramids[pyr_id].Diffs[scale_id];
  center := dog^[y, x];
  if (center < CGRAY_THRESHOLD) then
      exit;
  bMax := True;
  bMin := True;
  cmp1 := center - CEXTREMA_DIFF_THRESHOLD;
  cmp2 := center + CEXTREMA_DIFF_THRESHOLD;
  // try same scale
  for di := -1 to 1 do
    for dj := -1 to 1 do
      begin
        if (di = 0) and (dj = 0) then
            continue;
        newval := dog^[y + di, x + dj];
        if (newval >= cmp1) then
            bMax := False;
        if (newval <= cmp2) then
            bMin := False;
        if (not bMax) and (not bMin) then
            exit;
      end;

  if not between(scale_id, 1, Length(Pyramids[pyr_id].Diffs) - 1) then
      exit(False);

  // try adjacent scale top
  dog := @Pyramids[pyr_id].Diffs[scale_id - 1];
  for di := -1 to 1 do
    for i := 0 to 2 do
      begin
        newval := dog^[y + di][x - 1 + i];
        if (newval >= cmp1) then
            bMax := False;
        if (newval <= cmp2) then
            bMin := False;
        if (not bMax) and (not bMin) then
            exit;
      end;

  // try adjacent scale bottom
  dog := @Pyramids[pyr_id].Diffs[scale_id + 1];
  for di := -1 to 1 do
    for i := 0 to 2 do
      begin
        newval := dog^[y + di][x - 1 + i];
        if (newval >= cmp1) then
            bMax := False;
        if (newval <= cmp2) then
            bMin := False;
        if (not bMax) and (not bMin) then
            exit;
      end;

  Result := True;
end;

procedure TPyramids.BuildLocalExtrema(const pyr_id, scale_id: TLInt; const transform: Boolean; v2List: TVec2List);
var
  w, h: TLInt;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    x: TLInt;
  begin
    for x := 1 to w - 2 do
      if isExtrema(x, pass, pyr_id, scale_id) then
        begin
          LockObject(v2List);

          if transform then
              v2List.Add(fabs(x / w * FWidth), fabs(pass / h * FHeight))
          else
              v2List.Add(x, pass);

          UnLockObject(v2List);
        end;
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass, x: TLInt;
  begin
    for pass := 1 to h - 2 do
      for x := 1 to w - 2 do
        if isExtrema(x, pass, pyr_id, scale_id) then
          begin
            if transform then
                v2List.Add(fabs(x / w * FWidth), fabs(pass / h * FHeight))
            else
                v2List.Add(x, pass);
          end;
  end;
{$ENDIF parallel}


begin
  w := Pyramids[pyr_id].Width;
  h := Pyramids[pyr_id].Height;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, h - 2);
{$ELSE FPC}
  TParallel.For(1, h - 2, procedure(pass: Integer)
    var
      x: TLInt;
    begin
      for x := 1 to w - 2 do
        if isExtrema(x, pass, pyr_id, scale_id) then
          begin
            LockObject(v2List);

            if transform then
                v2List.Add(fabs(x / w * FWidth), fabs(pass / h * FHeight))
            else
                v2List.Add(x, pass);

            UnLockObject(v2List);
          end;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
end;

procedure TPyramids.BuildLocalExtrema(const pyr_id, scale_id: TLInt; ExtremaCoorList: TCoreClassList);
var
  w, h: TLInt;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    x: TLInt;
    p: PExtremaCoor;
  begin
    for x := 1 to w - 2 do
      if isExtrema(x, pass, pyr_id, scale_id) then
        begin
          LockObject(ExtremaCoorList);

          new(p);
          p^.PyramidCoor := Vec2(x / w, pass / h);
          p^.RealCoor := Vec2(x, pass);
          p^.pyr_id := pyr_id;
          p^.scale_id := scale_id;
          ExtremaCoorList.Add(p);

          UnLockObject(ExtremaCoorList);
        end;
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass, x: TLInt;
    p: PExtremaCoor;
  begin
    for pass := 1 to h - 2 do
      for x := 1 to w - 2 do
        if isExtrema(x, pass, pyr_id, scale_id) then
          begin
            new(p);
            p^.PyramidCoor := Vec2(x / w, pass / h);
            p^.RealCoor := Vec2(x, pass);
            p^.pyr_id := pyr_id;
            p^.scale_id := scale_id;
            ExtremaCoorList.Add(p);
          end;
  end;
{$ENDIF parallel}


begin
  w := Pyramids[pyr_id].Width;
  h := Pyramids[pyr_id].Height;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, h - 2);
{$ELSE FPC}
  TParallel.For(1, h - 2, procedure(pass: Integer)
    var
      x: TLInt;
      p: PExtremaCoor;
    begin
      for x := 1 to w - 2 do
        if isExtrema(x, pass, pyr_id, scale_id) then
          begin
            LockObject(ExtremaCoorList);

            new(p);
            p^.PyramidCoor := Vec2(x / w, pass / h);
            p^.RealCoor := Vec2(x, pass);
            p^.pyr_id := pyr_id;
            p^.scale_id := scale_id;
            ExtremaCoorList.Add(p);

            UnLockObject(ExtremaCoorList);
          end;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
end;

// key-pointer integral
function TPyramids.KPIntegral(const dogIntegral: PGaussSpaceIntegral; const x, y, s: TLInt; var offset, delta: TLVec): Boolean;
var
  v, dxx, dyy, dss, dxy, dys, dsx: TLFloat;
  m: TLMatrix;
  info: TLInt;
  mRep: TMatInvReport;
begin
  // hessian matrix 3x3
  v := dogIntegral^[s, y, x];
  delta[0] := (dogIntegral^[s, y, x + 1] - dogIntegral^[s, y, x - 1]) * 0.5;
  delta[1] := (dogIntegral^[s, y + 1, x] - dogIntegral^[s, y - 1, x]) * 0.5;
  delta[2] := (dogIntegral^[s + 1, y, x] - dogIntegral^[s - 1, y, x]) * 0.5;

  dxx := dogIntegral^[s, y, x + 1] + dogIntegral^[s, y, x - 1] - v - v;
  dyy := dogIntegral^[s, y + 1, x] + dogIntegral^[s, y - 1, x] - v - v;
  dss := dogIntegral^[s + 1, y, x] + dogIntegral^[s - 1, y, x] - v - v;

  dxy := (dogIntegral^[s, y + 1, x + 1] - dogIntegral^[s, y - 1, x + 1] - dogIntegral^[s, y + 1, x - 1] + dogIntegral^[s, y - 1, x - 1]) * 0.25;
  dys := (dogIntegral^[s + 1, y + 1, x] - dogIntegral^[s + 1, y - 1, x] - dogIntegral^[s - 1, y + 1, x] + dogIntegral^[s - 1, y - 1, x]) * 0.25;
  dsx := (dogIntegral^[s + 1, y, x + 1] - dogIntegral^[s + 1, y, x - 1] - dogIntegral^[s - 1, y, x + 1] + dogIntegral^[s - 1, y, x - 1]) * 0.25;

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
  Result := info = 1;
  if Result then
      Learn.MatrixVectorMultiply(m, 0, 2, 0, 2, False, delta, 0, 2, 1, offset, 0, 2, 0);

  SetLength(m, 0, 0);
end;

function TPyramids.ComputeKeyPoint(const ExtremaCoor: TVec2; const pyr_id, scale_id: TLInt; var Output: TPyramidCoor): Boolean;
var
  dog: PGaussSpace;
  w, h, NScale, i: TLInt;
  nowx, nowy, nows: TLInt;
  dpDone: Boolean;
  offset, delta: TLVec;
begin
  Result := False;

  dog := @Pyramids[pyr_id].Diffs[scale_id];
  w := Pyramids[pyr_id].Width;
  h := Pyramids[pyr_id].Height;
  NScale := Pyramids[pyr_id].numScale;

  nowx := Round(ExtremaCoor[0]);
  nowy := Round(ExtremaCoor[1]);
  nows := scale_id;

  offset := LVec(3);
  delta := LVec(3);
  dpDone := False;
  for i := 1 to COFFSET_DEPTH do
    begin
      if (not between(nowx, 1, w - 1)) or (not between(nowy, 1, h - 1)) or (not between(nows, 1, NScale - 2)) then
          exit;
      if not KPIntegral(@Pyramids[pyr_id].Diffs, nowx, nowy, nows, offset, delta) then
          exit;

      dpDone := Learn.LAbsMaxVec(offset) < COFFSET_THRESHOLD;
      // found
      if dpDone then
          break;

      inc(nowx, Round(offset[0]));
      inc(nowy, Round(offset[1]));
      inc(nows, Round(offset[2]));
    end;
  if not dpDone then
      exit;

  if dog^[nowy, nowx] + Learn.APVDotProduct(@offset, 0, 2, @delta, 0, 2) * 0.5 < CCONTRAST_THRESHOLD then
      exit;

  // update coordinate
  Output.Init;
  Output.PyramidCoor[0] := nowx;
  Output.PyramidCoor[1] := nowy;
  Output.Scale := CSIGMA_FACTOR * Power(CSCALE_FACTOR, (nows + offset[2]) / (NScale - 1));
  Output.RealCoor[0] := (nowx + offset[0]) / w;
  Output.RealCoor[1] := (nowy + offset[1]) / h;
  Output.pyr_id := pyr_id;
  Output.scale_id := nows;
  Output.Owner := Self;
  Result := True;
end;

function TPyramids.ComputeEndgeReponse(const ExtremaCoor: TVec2; const p: PGaussSpace): Boolean;
var
  dxx, dxy, dyy, v, det: TLFloat;
  x, y: TLInt;
begin
  x := Round(ExtremaCoor[0]);
  y := Round(ExtremaCoor[1]);

  // hessian matrix
  v := p^[y, x];

  dxx := p^[y, x + 1] + p^[y, x - 1] - v - v;
  dyy := p^[y + 1, x] + p^[y - 1, x] - v - v;
  dxy := (p^[y + 1, x + 1] + p^[y - 1, x - 1] - p^[y + 1, x - 1] - p^[y - 1, x + 1]) / 4;

  det := (dxx * dyy) - (dxy * dxy);

  if det <= 0 then
      exit(True);

  // compute principal curvature by hessian
  Result := ((Learn.AP_Sqr(dxx + dyy) / det) > Learn.AP_Sqr(CEDGE_RATIO + 1) / CEDGE_RATIO);
end;

function TPyramids.ComputePyramidCoor(const FilterEndge, FilterOri: Boolean; const ExtremaV2: TVec2; const pyr_id, scale_id: TLInt): TPyramidCoorList;
var
  RetCoords: TPyramidCoorList;

  procedure ComputeOrientation(const pc: TPyramidCoor);
  const
    ORI_WINDOW_FACTOR                  = 1.5;
    Orientation_Histogram_Binomial_Num = 36;
    Orientation_Histogram_Peak_Ratio   = 0.8;
    CHalfPI                            = 0.5 / PI;
  var
    Pyramid: PPyramidLayer;
    mag: PGaussSpace;
    ort: PGaussSpace;
    x, y: TLInt;
    gauss_weight_sigma, exp_denom, orient, weight, prev, next, fBinomial, thres: TLFloat;
    rad, xx, yy, newx, newy, iBinomial, k, i: TLInt;
    hist: TLVec;
    np: PPyramidCoor;
  begin
    Pyramid := @Pyramids[pc.pyr_id];
    mag := @Pyramid^.MagIntegral[pc.scale_id - 1];
    ort := @Pyramid^.OrtIntegral[pc.scale_id - 1];

    x := Round(pc.PyramidCoor[0]);
    y := Round(pc.PyramidCoor[1]);

    gauss_weight_sigma := pc.Scale * ORI_WINDOW_FACTOR;
    exp_denom := 2 * Learn.AP_Sqr(gauss_weight_sigma);
    rad := Round(pc.Scale * CORIENTATION_RADIUS);

    hist := LVec(Orientation_Histogram_Binomial_Num);

    // compute gaussian weighted histogram with inside a circle
    for xx := -rad to rad do
      begin
        newx := x + xx;
        if not between(newx, 1, Pyramid^.Width - 1) then
            continue;

        for yy := -rad to rad do
          begin
            newy := y + yy;
            if not between(newy, 1, Pyramid^.Height - 1) then
                continue;
            // use a circular gaussian window
            if Learn.AP_Sqr(xx) + Learn.AP_Sqr(yy) > Learn.AP_Sqr(rad) then
                continue;
            orient := ort^[newy, newx];
            iBinomial := Round(Orientation_Histogram_Binomial_Num * CHalfPI * orient);
            if (iBinomial = Orientation_Histogram_Binomial_Num) then
                iBinomial := 0;
            // overflow detect
            if (iBinomial > Orientation_Histogram_Binomial_Num) then
                exit;
            weight := Exp(-(Learn.AP_Sqr(xx) + Learn.AP_Sqr(yy)) / exp_denom);
            LAdd(hist[iBinomial], weight * mag^[newy, newx]);
          end;
      end;

    // smooth the histogram
    for k := CORIENTATION_SMOOTH_COUNT - 1 downto 0 do
      for i := 0 to Orientation_Histogram_Binomial_Num - 1 do
        begin
          prev := hist[IfThen(i = 0, Orientation_Histogram_Binomial_Num - 1, i - 1)];
          next := hist[IfThen(i = Orientation_Histogram_Binomial_Num - 1, 0, i + 1)];
          LMul(hist[i], 0.5);
          LAdd(hist[i], (prev + next) * 0.25);
        end;

    thres := LMaxVec(hist) * Orientation_Histogram_Peak_Ratio;

    // choose extreme orientation
    for i := 0 to Orientation_Histogram_Binomial_Num - 1 do
      begin
        prev := hist[IfThen(i = 0, Orientation_Histogram_Binomial_Num - 1, i - 1)];
        next := hist[IfThen(i = Orientation_Histogram_Binomial_Num - 1, 0, i + 1)];

        if (hist[i] > thres) and (hist[i] > Max(prev, next)) then
          begin
            // interpolation
            fBinomial := i - 0.5 + (hist[i] - prev) / (prev + next - 2 * hist[i]);

            if (fBinomial < 0) then
                LAdd(fBinomial, Orientation_Histogram_Binomial_Num)
            else if (fBinomial >= Orientation_Histogram_Binomial_Num) then
                LSub(fBinomial, Orientation_Histogram_Binomial_Num);

            if RetCoords = nil then
                RetCoords := TPyramidCoorList.Create;

            new(np);
            np^ := pc;
            np^.Orientation := fBinomial / Orientation_Histogram_Binomial_Num * 2 * CPI;
            np^.OriFinish := True;
            RetCoords.Add(np);
          end;
      end;

    SetLength(hist, 0);
  end;

var
  t: TPyramidCoor;
begin
  Result := nil;
  RetCoords := nil;

  if not ComputeKeyPoint(ExtremaV2, pyr_id, scale_id, t) then
      exit;

  if FilterEndge then
    if not ComputeEndgeReponse(t.PyramidCoor, @Pyramids[t.pyr_id].Diffs[t.scale_id]) then
        exit;

  ComputeOrientation(t);

  if (RetCoords = nil) and (not FilterOri) then
    begin
      RetCoords := TPyramidCoorList.Create;
      RetCoords.Add(t);
    end;

  Result := RetCoords;
end;

constructor TPyramids.CreateWithRaster(const raster: TMemoryRaster);
var
  F: TGFloat;
  w, h: TLInt;
begin
  inherited Create;

  FViewer := NewRaster();
  FViewer.Assign(raster);

  w := FViewer.Width;
  h := FViewer.Height;

  if (w > CMAX_SAMPLER_WIDTH) then
    begin
      F := CMAX_SAMPLER_WIDTH / w;
      w := Round(w * F);
      h := Round(h * F);
    end;
  if (h > CMAX_SAMPLER_HEIGHT) then
    begin
      F := CMAX_SAMPLER_HEIGHT / h;
      w := Round(w * F);
      h := Round(h * F);
    end;

  if (w <> FViewer.Width) and (h <> FViewer.Height) then
      FViewer.Zoom(w, h);

  // gray sampler
  if (CMAX_GRAY_COLOR_SAMPLER <= 0) or (CMAX_GRAY_COLOR_SAMPLER >= 255) then
      Sampler(FViewer, GaussTransformSpace)
  else
      Sampler(FViewer,
      CRED_WEIGHT_SAMPLER, CGREEN_WEIGHT_SAMPLER, CBLUE_WEIGHT_SAMPLER,
      CMAX_GRAY_COLOR_SAMPLER, GaussTransformSpace);

  FWidth := FViewer.Width;
  FHeight := FViewer.Height;

  SetLength(SamplerXY, FHeight * FWidth);
  FillPtrByte(@SamplerXY[0], FHeight * FWidth, 1);
  SetLength(Pyramids, 0);
end;

constructor TPyramids.CreateWithRaster(const fn: string);
var
  raster: TMemoryRaster;
begin
  raster := NewRasterFromFile(fn);
  CreateWithRaster(raster);
  disposeObject(raster);
end;

constructor TPyramids.CreateWithRaster(const stream: TCoreClassStream);
var
  raster: TMemoryRaster;
begin
  raster := NewRasterFromStream(stream);
  CreateWithRaster(raster);
  disposeObject(raster);
end;

constructor TPyramids.CreateWithGauss(const spr: PGaussSpace);
var
  F: TGFloat;
  w, h: TLInt;
begin
  inherited Create;

  FViewer := NewRaster();

  w := Length(spr^[0]);
  h := Length(spr^);

  if (w > CMAX_SAMPLER_WIDTH) then
    begin
      F := CMAX_SAMPLER_WIDTH / w;
      w := Round(w * F);
      h := Round(h * F);
    end;
  if (h > CMAX_SAMPLER_HEIGHT) then
    begin
      F := CMAX_SAMPLER_HEIGHT / h;
      w := Round(w * F);
      h := Round(h * F);
    end;

  if (w <> Length(spr^[0])) and (h <> Length(spr^)) then
      ZoomSampler(spr^, GaussTransformSpace, w, h)
  else
      CopySampler(spr^, GaussTransformSpace);

  // gray sampler
  Sampler(GaussTransformSpace, FViewer);

  FWidth := w;
  FHeight := h;

  SetLength(SamplerXY, FHeight * FWidth);
  FillPtrByte(@SamplerXY[0], FHeight * FWidth, 1);
  SetLength(Pyramids, 0);
end;

destructor TPyramids.Destroy;
var
  i: TLInt;
begin
  if Length(Pyramids) > 0 then
    begin
      for i := low(Pyramids) to high(Pyramids) do
          Pyramids[i].Free;
      SetLength(Pyramids, 0);
    end;
  SetLength(SamplerXY, 0);
  disposeObject(FViewer);
  inherited Destroy;
end;

procedure TPyramids.SetRegion(const clip: TVec2List);
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: TLInt;
  begin
    for i := 0 to FWidth - 1 do
        SamplerXY[i + pass * FWidth] := IfThen(clip.PointInHere(Vec2(i, pass)), 1, 0);
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass, i: TLInt;
  begin
    for pass := 0 to FHeight - 1 do
      for i := 0 to FWidth - 1 do
          SamplerXY[i + pass * FWidth] := IfThen(clip.PointInHere(Vec2(i, pass)), 1, 0);
  end;
{$ENDIF parallel}


begin
  if clip.Count > 0 then
    begin
{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, FHeight - 1);
{$ELSE FPC}
      TParallel.For(0, FHeight - 1, procedure(pass: Integer)
        var
          i: TLInt;
        begin
          for i := 0 to FWidth - 1 do
              SamplerXY[i + pass * FWidth] := IfThen(clip.PointInHere(Vec2(i, pass)), 1, 0);
        end);
{$ENDIF FPC}
{$ELSE parallel}
      DoFor;
{$ENDIF parallel}
    end;
end;

procedure TPyramids.SetRegion(var mat: TLBMatrix);
var
  nmat: TLBMatrix;
  j, i: TLInt;
begin
  if (Length(nmat) = Length(mat)) and (Length(nmat[0]) = Length(mat[0])) then
      nmat := LMatrixCopy(mat)
  else
      LZoomMatrix(mat, nmat, FWidth, FHeight);

  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
      if nmat[j, i] then
          SamplerXY[i + j * FWidth] := 1
      else
          SamplerXY[i + j * FWidth] := 0;

  SetLength(nmat, 0, 0);
end;

procedure TPyramids.BuildPyramid;
var
  i: TLInt;
  factor: TGFloat;
begin
  if Length(Pyramids) > 0 then
    begin
      for i := low(Pyramids) to high(Pyramids) do
          Pyramids[i].Free;
      SetLength(Pyramids, 0);
    end;

  SetLength(Pyramids, CNUMBER_OCTAVE);

  Pyramids[0].Build(GaussTransformSpace, FWidth, FHeight, CNUMBER_SCALE, CSIGMA_FACTOR);
  for i := 1 to CNUMBER_OCTAVE - 1 do
    begin
      factor := Power(CSCALE_FACTOR, -i);

      Pyramids[i].Build(
        GaussTransformSpace,
        ceil(FWidth * factor),
        ceil(FHeight * factor),
        CNUMBER_SCALE, CSIGMA_FACTOR);
    end;
end;

function TPyramids.BuildAbsoluteExtrema: TVec2List;
var
  i, j: TLInt;
  v: TVec2;
  x, y: TLInt;
begin
  if Length(Pyramids) = 0 then
      BuildPyramid;
  Result := TVec2List.Create;
  for i := 0 to CNUMBER_OCTAVE - 1 do
    for j := 0 to CNUMBER_SCALE - 2 do
        BuildLocalExtrema(i, j, True, Result);

  i := 0;
  while i < Result.Count do
    begin
      v := Result[i]^;
      x := Round(v[0]);
      y := Round(v[1]);
      j := x + y * FWidth;
      if between(j, 0, Length(SamplerXY)) and (SamplerXY[j] = 0) then
          Result.Delete(i)
      else
          inc(i);
    end;
end;

function TPyramids.BuildPyramidExtrema(const FilterOri: Boolean): TPyramidCoorList;
var
  ExtremaList: TCoreClassList;
  PyramidCoordOutput: TPyramidCoorList;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    ep: PExtremaCoor;
    p: TPyramidCoorList;
  begin
    ep := PExtremaCoor(ExtremaList[pass]);
    p := ComputePyramidCoor(ExtremaList.Count > CFILTER_MAX_KEYPOINT_ENDGE, FilterOri, ep^.RealCoor, ep^.pyr_id, ep^.scale_id);
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
    pass: TLInt;
    ep: PExtremaCoor;
    p: TPyramidCoorList;
  begin
    for pass := 0 to ExtremaList.Count - 1 do
      begin
        ep := PExtremaCoor(ExtremaList[pass]);
        p := ComputePyramidCoor(ExtremaList.Count > CFILTER_MAX_KEYPOINT_ENDGE, FilterOri, ep^.RealCoor, ep^.pyr_id, ep^.scale_id);
        if p <> nil then
          begin
            PyramidCoordOutput.Add(p, False);
            disposeObject(p);
          end;
      end;
  end;
{$ENDIF parallel}


var
  j, i: TLInt;
  x, y: TLInt;
begin
  if Length(Pyramids) = 0 then
      BuildPyramid;

  PyramidCoordOutput := TPyramidCoorList.Create;

  ExtremaList := TCoreClassList.Create;
  for j := 0 to CNUMBER_OCTAVE - 1 do
    for i := 0 to CNUMBER_SCALE - 2 do
        BuildLocalExtrema(j, i, ExtremaList);

  i := 0;
  while i < ExtremaList.Count do
    begin
      with PExtremaCoor(ExtremaList[i])^ do
        begin
          x := Round(RealCoor[0] * FWidth);
          y := Round(RealCoor[1] * FHeight);
        end;
      j := x + y * FWidth;
      if between(j, 0, Length(SamplerXY)) and (SamplerXY[j] = 0) then
          ExtremaList.Delete(i)
      else
          inc(i);
    end;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, ExtremaList.Count - 1);
{$ELSE FPC}
  TParallel.For(0, ExtremaList.Count - 1, procedure(pass: Integer)
    var
      ep: PExtremaCoor;
      p: TPyramidCoorList;
    begin
      ep := PExtremaCoor(ExtremaList[pass]);
      p := ComputePyramidCoor(ExtremaList.Count > CFILTER_MAX_KEYPOINT_ENDGE, FilterOri, ep^.RealCoor, ep^.pyr_id, ep^.scale_id);
      if p <> nil then
        begin
          LockObject(PyramidCoordOutput);
          PyramidCoordOutput.Add(p, False);
          UnLockObject(PyramidCoordOutput);
          disposeObject(p);
        end;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  for i := 0 to ExtremaList.Count - 1 do
      Dispose(PExtremaCoor(ExtremaList[i]));

  disposeObject(ExtremaList);

  i := 0;
  while i < PyramidCoordOutput.Count do
    begin
      with PyramidCoordOutput[i]^ do
        begin
          x := Round(RealCoor[0] * FWidth);
          y := Round(RealCoor[1] * FHeight);
        end;
      j := x + y * FWidth;
      if between(j, 0, Length(SamplerXY)) and (SamplerXY[j] = 0) then
          PyramidCoordOutput.Delete(i)
      else
          inc(i);
    end;

  Result := PyramidCoordOutput;
end;

function TPyramids.BuildViewer(const v2List: TVec2List; const Radius: TGFloat; const color: TRasterColor): TMemoryRaster;
var
  i: TLInt;
  l: TGFloat;
begin
  if Length(Pyramids) = 0 then
      BuildPyramid;
  Result := NewRaster();
  Result.Assign(FViewer);
  l := Radius * 2;

  for i := 0 to v2List.Count - 1 do
      Result.DrawCrossF(v2List[i]^, l, color);
end;

function TPyramids.BuildViewer(const cList: TPyramidCoorList; const Radius: TGFloat; const color: TRasterColor): TMemoryRaster;
var
  i: TLInt;
  l: TLInt;
  invColor: TRasterColor;
  p: PPyramidCoor;
  v1, v2: TVec2;
begin
  if Length(Pyramids) = 0 then
      BuildPyramid;
  Result := NewRaster();
  Result.Assign(FViewer);
  l := Round(Radius * 2);

  invColor := RasterColorInv(color);

  for i := 0 to cList.Count - 1 do
    begin
      p := cList[i];
      v2[0] := p^.RealCoor[0] * FWidth;
      v2[1] := p^.RealCoor[1] * FHeight;
      Result.FillRect(v2, Max(l div 2, 3), color);

      if p^.OriFinish then
        begin
          v1[0] := v2[0] + (Radius) * p^.Scale * cos(p^.Orientation);
          v1[1] := v2[1] + (Radius) * p^.Scale * sin(p^.Orientation);
          Result.LineF(v2, v1, invColor, False);
        end;
    end;
end;

class procedure TPyramids.BuildToViewer(const cList: TPyramidCoorList; const Radius: TGFloat; const color: TRasterColor; const RasterViewer: TMemoryRaster);
var
  i: TLInt;
  l: TLInt;
  invColor: TRasterColor;
  p: PPyramidCoor;
  v1, v2: TVec2;
begin
  l := Round(Radius * 2);

  invColor := RasterColorInv(color);

  for i := 0 to cList.Count - 1 do
    begin
      p := cList[i];
      v2[0] := p^.RealCoor[0] * RasterViewer.Width;
      v2[1] := p^.RealCoor[1] * RasterViewer.Height;
      RasterViewer.FillRect(v2, Max(l div 2, 3), color);

      if p^.OriFinish then
        begin
          v1[0] := v2[0] + (Radius) * p^.Scale * cos(p^.Orientation);
          v1[1] := v2[1] + (Radius) * p^.Scale * sin(p^.Orientation);
          RasterViewer.LineF(v2, v1, invColor, False);
        end;
    end;
end;

class procedure TPyramids.BuildToViewer(const cList: TPyramidCoorList; const Radius: TGFloat; const RasterViewer: TMemoryRaster);
var
  i: TLInt;
  l: TLInt;
  color, invColor: TRasterColor;
  p: PPyramidCoor;
  v1, v2: TVec2;
begin
  l := Round(Radius * 2);

  for i := 0 to cList.Count - 1 do
    begin
      color := RasterColor(Random(255), Random(255), Random(255), 255);
      invColor := RasterColorInv(color);

      p := cList[i];
      v2[0] := p^.RealCoor[0] * RasterViewer.Width;
      v2[1] := p^.RealCoor[1] * RasterViewer.Height;
      RasterViewer.FillRect(v2, Max(l div 2, 3), color);

      if p^.OriFinish then
        begin
          v1[0] := v2[0] + (Radius) * p^.Scale * cos(p^.Orientation);
          v1[1] := v2[1] + (Radius) * p^.Scale * sin(p^.Orientation);
          RasterViewer.LineF(v2, v1, invColor, False);
        end;
    end;
end;

procedure TFeature.ComputeDescriptor(const p: PPyramidCoor; var desc: TLVec);

  procedure Compute_Interpolate(const ybin, xbin, hbin, weight: TLFloat; var hist: TLMatrix); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    ybinf, xbinf, hbinf: TLInt;
    ybind, xbind, hbind, w_y, w_x: TLFloat;
    dy, dx, bin_2d_idx: TLInt;
  begin
    ybinf := floor(ybin);
    xbinf := floor(xbin);
    hbinf := floor(hbin);
    ybind := ybin - ybinf;
    xbind := xbin - xbinf;
    hbind := hbin - hbinf;

    for dy := 0 to 1 do
      if between(ybinf + dy, 0, CDESC_HIST_WIDTH) then
        begin
          w_y := weight * (IfThen(dy <> 0, ybind, 1 - ybind));
          for dx := 0 to 1 do
            if between(xbinf + dx, 0, CDESC_HIST_WIDTH) then
              begin
                w_x := w_y * (IfThen(dx <> 0, xbind, 1 - xbind));
                bin_2d_idx := (ybinf + dy) * CDESC_HIST_WIDTH + (xbinf + dx);
                LAdd(hist[bin_2d_idx, hbinf mod CDESC_HIST_BIN_NUM], w_x * (1 - hbind));
                LAdd(hist[bin_2d_idx, (hbinf + 1) mod CDESC_HIST_BIN_NUM], w_x * hbind);
              end;
        end;
  end;

var
  mag: PGaussSpace;
  ort: PGaussSpace;
  w, h, coorX, coorY: TLInt;
  Orientation, hist_w, exp_denom, y_rot, x_rot, cosort, sinort, ybin, xbin, now_mag, now_ort, weight, hist_bin: TLFloat;
  Radius, i, j, xx, yy, nowx, nowy: TLInt;
  hist: TLMatrix;
  sum: TLFloat;
begin
  mag := @(p^.Owner.Pyramids[p^.pyr_id].MagIntegral[p^.scale_id - 1]);
  ort := @(p^.Owner.Pyramids[p^.pyr_id].OrtIntegral[p^.scale_id - 1]);

  w := p^.Owner.Pyramids[p^.pyr_id].Width;
  h := p^.Owner.Pyramids[p^.pyr_id].Height;
  coorX := Trunc(p^.PyramidCoor[0]);
  coorY := Trunc(p^.PyramidCoor[1]);
  Orientation := p^.Orientation;
  hist_w := p^.Scale * CDESC_SCALE_FACTOR;
  exp_denom := 2 * Learn.AP_Sqr(CDESC_HIST_WIDTH);
  Radius := Round(CSQRT1_2 * hist_w * (CDESC_HIST_WIDTH + 1));
  hist := LMatrix(CDESC_HIST_WIDTH * CDESC_HIST_WIDTH, CDESC_HIST_BIN_NUM);
  cosort := cos(Orientation);
  sinort := sin(Orientation);

  for xx := -Radius to Radius do
    begin
      nowx := coorX + xx;
      if not between(nowx, 1, w - 1) then
          continue;
      for yy := -Radius to Radius do
        begin
          nowy := coorY + yy;

          if not between(nowy, 1, h - 1) then
              continue;

          // to be circle
          if (xx * xx + yy * yy) > (Radius * Radius) then
              continue;

          // coordinate change, relative to major orientation
          // major orientation become (x, 0)
          y_rot := (-xx * sinort + yy * cosort) / hist_w;
          x_rot := (xx * cosort + yy * sinort) / hist_w;

          // calculate 2d bin idx (which bin do I fall into)
          // -0.5 to make the center of bin 1st (x=1.5) falls fully into bin 1st
          ybin := y_rot + CDESC_HIST_WIDTH / 2 - 0.5;
          xbin := x_rot + CDESC_HIST_WIDTH / 2 - 0.5;

          if (not between(ybin, -1, CDESC_HIST_WIDTH)) or (not between(xbin, -1, CDESC_HIST_WIDTH)) then
              continue;

          now_mag := mag^[nowy, nowx];
          now_ort := ort^[nowy, nowx];

          // gaussian & magnitude weight on histogram
          weight := Exp(-(Learn.AP_Sqr(x_rot) + Learn.AP_Sqr(y_rot)) / exp_denom) * now_mag;

          LSub(now_ort, Orientation); // for rotation invariance

          if (now_ort < 0) then
              LAdd(now_ort, CPI2);
          if (now_ort > CPI2) then
              LSub(now_ort, CPI2);

          hist_bin := now_ort * CNUM_BIN_PER_RAD;

          // all three bin idx are float, do Compute_Interpolate
          Compute_Interpolate(xbin, ybin, hist_bin, weight, hist);
        end;
    end;

  desc := LVec(hist);
  SetLength(hist, 0, 0);

  if CDESC_PROCESS_LIGHT then
    begin
      sum := 0;
      for i := 0 to Length(desc) - 1 do
          LAdd(sum, desc[i]);
      for i := 0 to Length(desc) - 1 do
          desc[i] := Learn.AP_Sqr(desc[i] / sum);
    end;
end;

procedure TFeature.BuildFeature(Pyramids: TPyramids);

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ComputeDescriptor(FPyramidCoordList[pass], FDescriptorBuff[pass].descriptor);
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass: TLInt;
  begin
    for pass := 0 to FPyramidCoordList.Count - 1 do
        ComputeDescriptor(FPyramidCoordList[pass], FDescriptorBuff[pass].descriptor);
  end;
{$ENDIF parallel}


var
  i, j: TLInt;
  p: PDescriptor;
begin
  if FPyramidCoordList <> nil then
    begin
      disposeObject(FPyramidCoordList);
      FPyramidCoordList := nil;
    end;
  if Length(Pyramids.Pyramids) = 0 then
      Pyramids.BuildPyramid;

  FPyramidCoordList := Pyramids.BuildPyramidExtrema(True);

  SetLength(FDescriptorBuff, FPyramidCoordList.Count);

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, FPyramidCoordList.Count - 1);
{$ELSE FPC}
  TParallel.For(0, FPyramidCoordList.Count - 1, procedure(pass: Integer)
    begin
      ComputeDescriptor(FPyramidCoordList[pass], FDescriptorBuff[pass].descriptor);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  for i := 0 to Length(FDescriptorBuff) - 1 do
    begin
      FDescriptorBuff[i].coor := FPyramidCoordList[i]^.RealCoor;
      FDescriptorBuff[i].AbsCoor[0] := FPyramidCoordList[i]^.RealCoor[0] * Pyramids.FWidth;
      FDescriptorBuff[i].AbsCoor[1] := FPyramidCoordList[i]^.RealCoor[1] * Pyramids.FHeight;
      FDescriptorBuff[i].Orientation := FPyramidCoordList[i]^.Orientation;
      FDescriptorBuff[i].index := i;
      FDescriptorBuff[i].Owner := Self;
    end;

  FWidth := Pyramids.FWidth;
  FHeight := Pyramids.FHeight;
  SetLength(FViewer, FHeight * FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
      begin
        FViewer[i + j * FWidth] := Pyramids.FViewer.PixelGray[i, j];
        // FViewer[i + j * FWidth] := Round(Clamp(Pyramids.GaussTransformSpace[j, i], 0, 1) * 255);
      end;
end;

constructor TFeature.CreateWithPyramids(Pyramids: TPyramids);
begin
  inherited Create;
  FWidth := 0;
  FHeight := 0;
  SetLength(FViewer, 0);
  FInternalVec := ZeroVec2;
  FUserData := nil;
  FPyramidCoordList := nil;
  BuildFeature(Pyramids);
end;

constructor TFeature.CreateWithRaster(const raster: TMemoryRaster; const clip: TVec2List);
var
  Pyramids: TPyramids;
begin
  Pyramids := TPyramids.CreateWithRaster(raster);
  Pyramids.SetRegion(clip);
  CreateWithPyramids(Pyramids);
  disposeObject(Pyramids);
end;

constructor TFeature.CreateWithRaster(const raster: TMemoryRaster; var mat: TLBMatrix);
var
  Pyramids: TPyramids;
begin
  Pyramids := TPyramids.CreateWithRaster(raster);
  Pyramids.SetRegion(mat);
  CreateWithPyramids(Pyramids);
  disposeObject(Pyramids);
end;

constructor TFeature.CreateWithRaster(const raster: TMemoryRaster);
var
  Pyramids: TPyramids;
begin
  Pyramids := TPyramids.CreateWithRaster(raster);
  CreateWithPyramids(Pyramids);
  disposeObject(Pyramids);
end;

constructor TFeature.CreateWithRaster(const fn: string);
var
  Pyramids: TPyramids;
begin
  Pyramids := TPyramids.CreateWithRaster(fn);
  CreateWithPyramids(Pyramids);
  disposeObject(Pyramids);
end;

constructor TFeature.CreateWithRaster(const stream: TCoreClassStream);
var
  Pyramids: TPyramids;
begin
  Pyramids := TPyramids.CreateWithRaster(stream);
  CreateWithPyramids(Pyramids);
  disposeObject(Pyramids);
end;

constructor TFeature.CreateWithSampler(const spr: PGaussSpace);
var
  Pyramids: TPyramids;
begin
  Pyramids := TPyramids.CreateWithGauss(spr);
  CreateWithPyramids(Pyramids);
  disposeObject(Pyramids);
end;

constructor TFeature.Create;
begin
  inherited Create;
  FWidth := 0;
  FHeight := 0;
  SetLength(FViewer, 0);
  FInternalVec := ZeroVec2;
  FUserData := nil;
  FPyramidCoordList := nil;
  SetLength(FDescriptorBuff, 0);
end;

destructor TFeature.Destroy;
begin
  SetLength(FDescriptorBuff, 0);
  if FPyramidCoordList <> nil then
      disposeObject(FPyramidCoordList);
  SetLength(FViewer, 0);
  inherited Destroy;
end;

function TFeature.Count: TLInt;
begin
  Result := Length(FDescriptorBuff);
end;

function TFeature.GetFD(const index: TLInt): PDescriptor;
begin
  Result := @FDescriptorBuff[index];
end;

procedure TFeature.SaveToStream(stream: TCoreClassStream);
var
  i, l: Integer;
  p: PDescriptor;
begin
  l := Count;
  stream.Write(l, 4);

  for i := 0 to l - 1 do
    begin
      p := @FDescriptorBuff[i];
      stream.Write(p^.descriptor[0], DESCRIPTOR_LENGTH * SizeOf(TLFloat));
      stream.Write(p^.coor[0], SizeOf(TVec2));
      stream.Write(p^.AbsCoor[0], SizeOf(TVec2));
      stream.Write(p^.Orientation, SizeOf(TGFloat));
    end;

  stream.Write(FWidth, 4);
  stream.Write(FHeight, 4);
  stream.Write(FViewer[0], FHeight * FWidth);
end;

procedure TFeature.LoadFromStream(stream: TCoreClassStream);
var
  i, l: Integer;
begin
  stream.Read(l, 4);
  SetLength(FDescriptorBuff, l);

  for i := 0 to l - 1 do
    begin
      SetLength(FDescriptorBuff[i].descriptor, DESCRIPTOR_LENGTH);
      stream.Read(FDescriptorBuff[i].descriptor[0], DESCRIPTOR_LENGTH * SizeOf(TLFloat));
      stream.Read(FDescriptorBuff[i].coor[0], SizeOf(TVec2));
      stream.Read(FDescriptorBuff[i].AbsCoor[0], SizeOf(TVec2));
      stream.Read(FDescriptorBuff[i].Orientation, SizeOf(TGFloat));
      FDescriptorBuff[i].index := i;
      FDescriptorBuff[i].Owner := Self;
    end;

  stream.Read(FWidth, 4);
  stream.Read(FHeight, 4);
  SetLength(FViewer, FHeight * FWidth);
  stream.Read(FViewer[0], FHeight * FWidth);
end;

function TFeature.CreateViewer: TMemoryRaster;
var
  i, j: TLInt;
  c: Byte;
begin
  Result := NewRaster();
  Result.SetSize(FWidth, FHeight);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
      begin
        c := FViewer[i + j * FWidth];
        Result.Pixel[i, j] := RasterColor(c, c, c);
      end;
end;

function TFeature.CreateFeatureViewer(const FeatureRadius: TGFloat; const color: TRasterColor): TMemoryRaster;
var
  i: TLInt;
  l: TLInt;
  invColor: TRasterColor;
  p: PDescriptor;
  v1, v2: TVec2;
begin
  Result := CreateViewer;
  if Count = 0 then
      exit;

  l := Round(FeatureRadius * 2);

  invColor := RasterColorInv(color);

  for i := 0 to Count - 1 do
    begin
      p := GetFD(i);
      v2 := p^.AbsCoor;
      Result.FillRect(v2, Max(l div 2, 3), color);

      v1[0] := v2[0] + (FeatureRadius) * cos(p^.Orientation);
      v1[1] := v2[1] + (FeatureRadius) * sin(p^.Orientation);
      Result.LineF(p^.AbsCoor, v1, invColor, False);
    end;
end;

procedure TestPyramidSpace;
var
  f1, f2: TFeature;
  m: TArrayMatchInfo;
  raster: TMemoryRaster;
  F: TLFloat;
begin
  f1 := TFeature.CreateWithRaster('c:\1.bmp');
  f2 := TFeature.CreateWithRaster('c:\2.bmp');
  F := MatchFeature(f1, f2, m);

  raster := BuildMatchInfoView(m, 10, True);
  if raster <> nil then
    begin
      SaveRaster(raster, 'c:\4.bmp');
      disposeObject(raster);
    end;

  disposeObject(f1);
  disposeObject(f2);
end;

initialization

// sampler
CRED_WEIGHT_SAMPLER := 1.0;
CGREEN_WEIGHT_SAMPLER := 1.0;
CBLUE_WEIGHT_SAMPLER := 1.0;
CSAMPLER_MODE := TGSamplerMode.gsmGray;
CMAX_GRAY_COLOR_SAMPLER := 255;
CMAX_SAMPLER_WIDTH := 1920 div 2;
CMAX_SAMPLER_HEIGHT := 1080 div 2;

// gauss kernal
CGAUSS_KERNEL_FACTOR := 3;

// pyramidoctave
CNUMBER_OCTAVE := 5;

// scalespace(SS) and difference of Gaussian Space(DOG)
CNUMBER_SCALE := 7;

// pyramid scale space factor
CSCALE_FACTOR := 1.4142135623730950488;
CSIGMA_FACTOR := 1.4142135623730950488;

// Extrema
CGRAY_THRESHOLD := 5.0E-2;
CEXTREMA_DIFF_THRESHOLD := 2.0E-5;
CFILTER_MAX_KEYPOINT_ENDGE := 3000;

// orientation
COFFSET_DEPTH := 15;
COFFSET_THRESHOLD := 0.6;
CCONTRAST_THRESHOLD := 3.0E-2;
CEDGE_RATIO := 2.0;
CORIENTATION_RADIUS := 15;
CORIENTATION_SMOOTH_COUNT := 256;

// feature
CMATCH_REJECT_NEXT_RATIO := 0.8;
CDESC_SCALE_FACTOR := 16;
CDESC_PROCESS_LIGHT := True;

finalization

end.
