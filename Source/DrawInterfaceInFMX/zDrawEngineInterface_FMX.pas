{ ****************************************************************************** }
{ * draw engine with FMX Support                                               * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit zDrawEngineInterface_FMX;

{$I ..\zDefine.inc}

interface

uses System.Math.Vectors, System.Math,
  FMX.Forms,
  FMX.Graphics, System.UITypes, System.Types, FMX.Types, FMX.Controls,
  FMX.Types3D, FMX.Surfaces, System.UIConsts, Geometry3DUnit, ListEngine,
  PascalStrings,
  CoreClasses, zDrawEngine, UnicodeMixedLib, Geometry2DUnit, MemoryRaster;

type
  TDrawEngineInterface_FMX = class(TCoreClassInterfacedObject, IDrawEngineInterface)
  private
    FCanvas: TCanvas;
    FOwnerCanvasScale: TDEFloat;
    FLineWidth: TDEFloat;
    FCanvasSave: TCanvasSaveState;
    FDebug: Boolean;
    FCurrSiz: TDEVec;

    procedure SetCanvas(const Value: TCanvas);
  public
    procedure SetSize(r: TDERect);
    procedure SetLineWidth(w: TDEFloat);
    procedure DrawLine(pt1, pt2: TDEVec; color: TDEColor);
    procedure DrawRect(r: TDERect; Angle: TDEFloat; color: TDEColor);
    procedure FillRect(r: TDERect; Angle: TDEFloat; color: TDEColor);
    procedure DrawEllipse(r: TDERect; color: TDEColor);
    procedure FillEllipse(r: TDERect; color: TDEColor);
    procedure DrawText(text: SystemString; size: TDEFloat; r: TDERect; color: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat);
    procedure DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
    procedure Flush;
    procedure ResetState;
    procedure BeginDraw;
    procedure EndDraw;
    function CurrentScreenSize: TDEVec;
    function GetTextSize(text: SystemString; size: TDEFloat): TDEVec;
    function ReadyOK: Boolean;
    function EngineIntfObject: TCoreClassObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSurface(c: TCanvas; OwnerCtrl: TObject);
    property Canvas: TCanvas read FCanvas write SetCanvas;

    property Debug: Boolean read FDebug write FDebug;
    // only work in mobile device and gpu fast mode
    property OwnerCanvasScale: TDEFloat read FOwnerCanvasScale write FOwnerCanvasScale;
    property CanvasScale: TDEFloat read FOwnerCanvasScale write FOwnerCanvasScale;
    property ScreenSize: TDEVec read FCurrSiz;
  end;

  TDETexture_FMX = class(TDETexture)
  protected
{$IF Defined(ANDROID) or Defined(IOS)}
    FTexture: TTexture;
    function GetTexture: TTexture;
{$ELSE}
    FTexture: TBitmap;
    function GetTexture: TBitmap;
{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ReleaseFMXResource; override;
    procedure FastUpdateTexture; override;

{$IF Defined(ANDROID) or Defined(IOS)}
    property Texture: TTexture read GetTexture;
{$ELSE}
    property Texture: TBitmap read GetTexture;
{$ENDIF}
  end;

  TResourceTexture = class(TDETexture_FMX)
  protected
    FLastLoadFile: SystemString;
  public
    constructor Create; overload; override;
    constructor Create(filename: SystemString); overload; virtual;

    procedure LoadFromFileIO(filename: SystemString);
    property LastLoadFile: SystemString read FLastLoadFile;
  end;

  TResourceTextureIntf = class(TCoreClassInterfacedObject)
  public
    Texture: TResourceTexture;
    TextureRect: TDERect;
    SizeScale: TDEVec;

    constructor Create(tex: TResourceTexture); virtual;
    destructor Destroy; override;
    function SizeOfVec: TDEVec;
    procedure ChangeTexture(tex: TResourceTexture); virtual;
  end;

  TResourceTextureCache = class(TCoreClassObject)
  protected
    TextureList: THashObjectList;
  public
    DefaultTexture: TResourceTexture;

    constructor Create; virtual;
    destructor Destroy; override;
    function CreateResourceTexture(filename: SystemString): TResourceTextureIntf;
    procedure ReleaseAllFMXRsource;
  end;

function c2c(c: TDEColor): TAlphaColor; inline; overload;
function c2c(c: TAlphaColor): TDEColor; inline; overload;
function p2p(pt: TDEVec): TPointf; inline; overload;
function r2r(r: TDERect): TRectf; inline; overload;
function AlphaColor2RasterColor(c: TAlphaColor): TRasterColor; inline;
function DE4V2Corners(sour: TDE4V): TCornersF; inline;
function DEColor(c: TAlphaColor): TDEColor; inline; overload;
function PrepareColor(const SrcColor: TAlphaColor; const Opacity: TDEFloat): TAlphaColor; inline;
procedure MakeMatrixRotation(Angle, Width, Height, X, Y, RotationCenter_X, RotationCenter_Y: TDEFloat; var OutputMatrix: TMatrix; var OutputRect: TRectf); inline;

procedure MemoryBitmapToSurface(bmp: TMemoryRaster; Surface: TBitmapSurface); overload; inline;
procedure MemoryBitmapToSurface(bmp: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface); overload; inline;
procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; bmp: TMemoryRaster); inline;
procedure MemoryBitmapToBitmap(b: TMemoryRaster; bmp: TBitmap); overload;
procedure MemoryBitmapToBitmap(b: TMemoryRaster; sourRect: TRect; bmp: TBitmap); overload;
procedure BitmapToMemoryBitmap(bmp: TBitmap; b: TMemoryRaster);
procedure LoadMemoryBitmap(f: SystemString; b: TMemoryRaster); overload;
procedure LoadMemoryBitmap(f: SystemString; b: TSequenceMemoryRaster); overload;
procedure LoadMemoryBitmap(f: SystemString; b: TDETexture); overload;
procedure LoadMemoryBitmap(stream: TCoreClassStream; b: TMemoryRaster); overload;

procedure SaveMemoryBitmap(f: SystemString; b: TMemoryRaster); overload;
procedure SaveMemoryBitmap(b: TMemoryRaster; fileExt: SystemString; destStream: TCoreClassStream); overload;
procedure SaveMemoryBitmap(b: TSequenceMemoryRaster; fileExt: SystemString; destStream: TCoreClassStream); overload;

var
  // resource texture cache
  TextureCache: TResourceTextureCache = nil;

implementation

uses
{$IF Defined(ANDROID) or Defined(IOS)}
  FMX.Canvas.GPU, FMX.TextLayout.GPU, FMX.StrokeBuilder, FMX.Canvas.GPU.Helpers,
{$ENDIF}
  MemoryStream64, MediaCenter;

function c2c(c: TDEColor): TAlphaColor;
begin
  Result := TAlphaColorF.Create(c[0], c[1], c[2], c[3]).ToAlphaColor;
end;

function c2c(c: TAlphaColor): TDEColor;
begin
  with TAlphaColorF.Create(c) do
      Result := DEColor(r, G, b, A);
end;

function p2p(pt: TDEVec): TPointf;
begin
  Result := Point2Pointf(pt);
end;

function r2r(r: TDERect): TRectf;
begin
  Result := MakeRectf(r);
end;

function AlphaColor2RasterColor(c: TAlphaColor): TRasterColor;
var
  ce: TRasterColorEntry;
begin
  ce.r := TAlphaColorRec(c).r;
  ce.G := TAlphaColorRec(c).G;
  ce.b := TAlphaColorRec(c).b;
  ce.A := TAlphaColorRec(c).A;
  Result := ce.RGBA;
end;

function DE4V2Corners(sour: TDE4V): TCornersF;
begin
  with TV2Rect4.Init(sour.MakeRectV2, sour.Angle) do
    begin
      Result[0] := Point2Pointf(LeftTop);
      Result[1] := Point2Pointf(RightTop);
      Result[2] := Point2Pointf(RightBottom);
      Result[3] := Point2Pointf(LeftBottom);
    end;
end;

function DEColor(c: TAlphaColor): TDEColor;
begin
  with TAlphaColorF.Create(c) do
      Result := DEColor(r, G, b, A);
end;

function PrepareColor(const SrcColor: TAlphaColor; const Opacity: TDEFloat): TAlphaColor;
begin
  if Opacity <= 1.0 then
    begin
      TAlphaColorRec(Result).r := Round(TAlphaColorRec(SrcColor).r * Opacity);
      TAlphaColorRec(Result).G := Round(TAlphaColorRec(SrcColor).G * Opacity);
      TAlphaColorRec(Result).b := Round(TAlphaColorRec(SrcColor).b * Opacity);
      TAlphaColorRec(Result).A := Round(TAlphaColorRec(SrcColor).A * Opacity);
    end
  else if (TAlphaColorRec(SrcColor).A < $FF) then
      Result := PremultiplyAlpha(SrcColor)
  else
      Result := SrcColor;
end;

procedure MakeMatrixRotation(Angle, Width, Height, X, Y, RotationCenter_X, RotationCenter_Y: TDEFloat; var OutputMatrix: TMatrix; var OutputRect: TRectf); inline;
const
  Scale_X = 1.0;
  Scale_Y = 1.0;
var
  ScaleMatrix, RotMatrix, M1, M2: TMatrix;
begin
  ScaleMatrix := TMatrix.Identity;
  ScaleMatrix.m11 := Scale_X;
  ScaleMatrix.m22 := Scale_Y;
  OutputMatrix := ScaleMatrix;

  M1 := TMatrix.Identity;
  M1.m31 := -(RotationCenter_X * Width * Scale_X + X);
  M1.m32 := -(RotationCenter_Y * Height * Scale_Y + Y);
  M2 := TMatrix.Identity;
  M2.m31 := RotationCenter_X * Width * Scale_X + X;
  M2.m32 := RotationCenter_Y * Height * Scale_Y + Y;
  RotMatrix := M1 * (TMatrix.CreateRotation(DegToRad(Angle)) * M2);
  OutputMatrix := OutputMatrix * RotMatrix;

  OutputRect.TopLeft := Pointf(X, Y);
  OutputRect.BottomRight := Pointf(X + Width, Y + Height);
end;

procedure MemoryBitmapToSurface(bmp: TMemoryRaster; Surface: TBitmapSurface);
var
  i: Integer;
  p1, p2: PCardinal;
  c: TRasterColorEntry;
  dc: TAlphaColor;
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  Surface.SetSize(bmp.Width, bmp.Height, TPixelFormat.RGBA);
{$ELSE}
  Surface.SetSize(bmp.Width, bmp.Height, TPixelFormat.BGRA);
{$ENDIF}
  p1 := PCardinal(@bmp.Bits[0]);
  p2 := PCardinal(Surface.Bits);
  for i := bmp.Width * bmp.Height - 1 downto 0 do
    begin
{$IF Defined(ANDROID) or Defined(IOS) or Defined(OSX)}
      c.RGBA := RGBA2BGRA(TRasterColor(p1^));
{$ELSE}
      c.RGBA := TRasterColor(p1^);
{$IFEND}
      TAlphaColorRec(dc).r := c.r;
      TAlphaColorRec(dc).G := c.G;
      TAlphaColorRec(dc).b := c.b;
      TAlphaColorRec(dc).A := c.A;
      p2^ := dc;
      inc(p1);
      inc(p2);
    end;
end;

procedure MemoryBitmapToSurface(bmp: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface);
var
  nb: TMemoryRaster;
begin
  nb := TMemoryRaster.Create;
  nb.DrawMode := dmBlend;
  nb.SetSize(sourRect.Width, sourRect.Height, RasterColor(0, 0, 0, 0));
  bmp.DrawTo(nb, 0, 0, sourRect);
  MemoryBitmapToSurface(nb, Surface);
  DisposeObject(nb);
end;

procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; bmp: TMemoryRaster);
var
  X, Y: Integer;
begin
  bmp.SetSize(Surface.Width, Surface.Height);
  for Y := 0 to Surface.Height - 1 do
    for X := 0 to Surface.Width - 1 do
      with TAlphaColorRec(Surface.Pixels[X, Y]) do
          bmp.Pixel[X, Y] := RasterColor(r, G, b, A)
end;

procedure MemoryBitmapToBitmap(b: TMemoryRaster; bmp: TBitmap);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  MemoryBitmapToSurface(b, Surface);
  bmp.Assign(Surface);
  DisposeObject(Surface);
end;

procedure MemoryBitmapToBitmap(b: TMemoryRaster; sourRect: TRect; bmp: TBitmap);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  MemoryBitmapToSurface(b, sourRect, Surface);
  bmp.Assign(Surface);
  DisposeObject(Surface);
end;

procedure BitmapToMemoryBitmap(bmp: TBitmap; b: TMemoryRaster);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  Surface.Assign(bmp);
  SurfaceToMemoryBitmap(Surface, b);
  DisposeObject(Surface);
end;

procedure LoadMemoryBitmap(f: SystemString; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if b.CanLoadFile(f) then
    begin
      b.LoadFromFile(f);
    end
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromFile(f, Surf, TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            SurfaceToMemoryBitmap(Surf, b);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure LoadMemoryBitmap(stream: TCoreClassStream; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if b.CanLoadStream(stream) then
    begin
      b.LoadFromStream(stream);
    end
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromStream(stream, Surf, TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            SurfaceToMemoryBitmap(Surf, b);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure LoadMemoryBitmap(f: SystemString; b: TSequenceMemoryRaster);
begin
  if b.CanLoadFile(f) then
      b.LoadFromFile(f)
  else
      LoadMemoryBitmap(f, TMemoryRaster(b));
end;

procedure LoadMemoryBitmap(f: SystemString; b: TDETexture);
begin
  LoadMemoryBitmap(f, TSequenceMemoryRaster(b));
  b.ReleaseFMXResource;
end;

procedure SaveMemoryBitmap(f: SystemString; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['*.bmp'], f) then
      b.SaveToFile(f)
  else if umlMultipleMatch(['*.seq'], f) then
      b.SaveToZLibCompressFile(f)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(b, Surf);
        TBitmapCodecManager.SaveToFile(f, Surf, nil);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure SaveMemoryBitmap(b: TMemoryRaster; fileExt: SystemString; destStream: TCoreClassStream);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['.bmp'], fileExt) then
      b.SaveToBmpStream(destStream)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(b, Surf);
        TBitmapCodecManager.SaveToStream(destStream, Surf, fileExt);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure SaveMemoryBitmap(b: TSequenceMemoryRaster; fileExt: SystemString; destStream: TCoreClassStream);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['.bmp'], fileExt) then
      b.SaveToBmpStream(destStream)
  else if umlMultipleMatch(['.seq'], fileExt) then
      b.SaveToStream(destStream)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(b, Surf);
        TBitmapCodecManager.SaveToStream(destStream, Surf, fileExt);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure TDrawEngineInterface_FMX.SetCanvas(const Value: TCanvas);
begin
  if Value = nil then
    begin
      FCanvas := nil;
      exit;
    end;

  FCanvas := Value;
  FCurrSiz := DEVec(FCanvas.Width, FCanvas.Height);
end;

procedure TDrawEngineInterface_FMX.SetSize(r: TDERect);
begin
  FCanvas.IntersectClipRect(MakeRectf(r));
end;

procedure TDrawEngineInterface_FMX.SetLineWidth(w: TDEFloat);
begin
  if not IsEqual(FLineWidth, w) then
    begin
      FLineWidth := w;
      FCanvas.Stroke.Thickness := w;
    end;
end;

procedure TDrawEngineInterface_FMX.DrawLine(pt1, pt2: TDEVec; color: TDEColor);
begin
  FCanvas.Stroke.color := c2c(color);
  FCanvas.fill.color := FCanvas.Stroke.color;
  FCanvas.DrawLine(p2p(pt1), p2p(pt2), color[3]);
end;

procedure TDrawEngineInterface_FMX.DrawRect(r: TDERect; Angle: TDEFloat; color: TDEColor);
var
  m, bak: TMatrix;
  rf: TRectf;
begin
  if Angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(Angle, RectWidth(r), RectHeight(r),
        MinValue(r[0, 0], r[1, 0]), MinValue(r[0, 1], r[1, 1]), 0.5, 0.5, m, rf);
      FCanvas.MultiplyMatrix(m);
    end;

  FCanvas.Stroke.color := c2c(color);
  FCanvas.fill.color := FCanvas.Stroke.color;
  FCanvas.DrawRect(r2r(r), 0, 0, [], color[3]);

  if Angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.FillRect(r: TDERect; Angle: TDEFloat; color: TDEColor);
var
  m, bak: TMatrix;
  rf: TRectf;
begin
  if Angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(Angle, RectWidth(r), RectHeight(r),
        MinValue(r[0, 0], r[1, 0]), MinValue(r[0, 1], r[1, 1]), 0.5, 0.5, m, rf);
      FCanvas.MultiplyMatrix(m);
    end;

  FCanvas.Stroke.color := c2c(color);
  FCanvas.fill.color := FCanvas.Stroke.color;
  FCanvas.FillRect(r2r(r), 0, 0, [], color[3]);

  if Angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.DrawEllipse(r: TDERect; color: TDEColor);
begin
  FCanvas.Stroke.color := c2c(color);
  FCanvas.fill.color := FCanvas.Stroke.color;
  FCanvas.DrawEllipse(r2r(r), color[3]);
end;

procedure TDrawEngineInterface_FMX.FillEllipse(r: TDERect; color: TDEColor);
begin
  FCanvas.Stroke.color := c2c(color);
  FCanvas.fill.color := FCanvas.Stroke.color;
  FCanvas.FillEllipse(r2r(r), color[3]);
end;

procedure TDrawEngineInterface_FMX.DrawText(text: SystemString; size: TDEFloat; r: TDERect; color: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat);
var
  m, bak: TMatrix;
  rf: TRectf;
  ta: TTextAlign;
begin
  if Angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(Angle, RectWidth(r), RectHeight(r),
        MinValue(r[0, 0], r[1, 0]), MinValue(r[0, 1], r[1, 1]), RotateVec[0], RotateVec[1], m, rf);
      FCanvas.MultiplyMatrix(m);
    end;

  FCanvas.Stroke.color := c2c(color);
  FCanvas.fill.color := FCanvas.Stroke.color;
  FCanvas.Font.size := size;

  if center then
      ta := TTextAlign.center
  else
      ta := TTextAlign.Leading;

  FCanvas.FillText(r2r(r), text, False, color[3], [], ta, TTextAlign.center);

  if Angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
var
  newSour, newDest: TDE4V;
{$IF Defined(ANDROID) or Defined(IOS)}
{$ELSE}
  m, bak: TMatrix;
  r: TRectf;
{$ENDIF}
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  if not(Canvas is TCustomCanvasGpu) then
    begin
      RaiseInfo('custom error ' + FCanvas.ClassName);
      exit;
    end;

  if TCustomCanvasGpu(Canvas).Context = nil then
      exit;

  newSour := sour;

  // make offset
  with FCanvas.Matrix do
      newDest := dest.Scale(FOwnerCanvasScale).Add(DEVec(m31, m32));

  if (t is TBitmap) then
    begin
      if (TBitmap(t).Width = 0) or (TBitmap(t).Height = 0) then
          exit;
      if not TBitmap(t).HandleAllocated then
          exit;

      if newSour.IsZero then
          newSour := TDE4V.Init(Rectf(0, 0, TBitmap(t).Width, TBitmap(t).Height), 0);
      if newDest.IsZero then
          newDest := newSour;

      lastCanvasHelper.TexRect(DE4V2Corners(dest), DE4V2Corners(newSour), TBitmapCtx(TBitmap(t).Handle).PaintingTexture, PrepareColor($FFFFFFFF, alpha));
    end
  else if t is TTexture then
    begin
      if (TTexture(t).Width = 0) or (TTexture(t).Height = 0) then
          exit;
      if newSour.IsZero then
          newSour := TDE4V.Init(Rect(0, 0, TTexture(t).Width, TTexture(t).Height), 0);
      if newDest.IsZero then
          newDest := newSour;

      lastCanvasHelper.TexRect(DE4V2Corners(dest), DE4V2Corners(newSour), TTexture(t), PrepareColor($FFFFFFFF, alpha));
    end
  else if t is TDETexture_FMX then
    begin
      if (TDETexture_FMX(t).Width = 0) or (TDETexture_FMX(t).Height = 0) then
          exit;

      if newSour.IsZero then
          newSour := TDE4V.Init(DERect(0, 0, TDETexture_FMX(t).Width, TDETexture_FMX(t).Height), 0);
      if newDest.IsZero then
          newDest := newSour;

      lastCanvasHelper.TexRect(DE4V2Corners(newDest), DE4V2Corners(newSour), TDETexture_FMX(t).Texture, PrepareColor($FFFFFFFF, alpha));
    end
  else
      RaiseInfo('texture error! ' + t.ClassName);

{$ELSE}
  newSour := sour;
  newDest := dest;

  if (t is TDETexture_FMX) then
    begin
      if (TDETexture_FMX(t).Width = 0) or (TDETexture_FMX(t).Height = 0) then
          exit;
      if newSour.IsZero then
          newSour := TDE4V.Init(TDETexture_FMX(t).BoundsRectV2, 0);
      if newDest.IsZero then
          newDest := newSour;
      if not IsEqual(newDest.Angle, 0, 0.1) then
        begin
          bak := FCanvas.Matrix;
          MakeMatrixRotation(newDest.Angle, newDest.Width, newDest.Height, MinValue(newDest.Left, newDest.Right), MinValue(newDest.Top, newDest.Bottom), 0.5, 0.5, m, r);
          FCanvas.MultiplyMatrix(m);
          FCanvas.DrawBitmap(TDETexture_FMX(t).Texture, newSour.MakeRectf, newDest.MakeRectf, alpha, False);
          FCanvas.SetMatrix(bak);
        end
      else
          FCanvas.DrawBitmap(TDETexture_FMX(t).Texture, newSour.MakeRectf, newDest.MakeRectf, alpha, False);
    end
  else if (t is TBitmap) then
    begin
      if (TBitmap(t).Width = 0) or (TBitmap(t).Height = 0) then
          exit;
      if newSour.IsZero then
          newSour := TDE4V.Init(TBitmap(t).BoundsF, 0);
      if newDest.IsZero then
          newDest := newSour;
      if not IsEqual(newDest.Angle, 0, 0.1) then
        begin
          bak := FCanvas.Matrix;
          MakeMatrixRotation(newDest.Angle, newDest.Width, newDest.Height, MinValue(newDest.Left, newDest.Right), MinValue(newDest.Top, newDest.Bottom), 0.5, 0.5, m, r);
          FCanvas.MultiplyMatrix(m);
          FCanvas.DrawBitmap(TBitmap(t), newSour.MakeRectf, newDest.MakeRectf, alpha, False);
          FCanvas.SetMatrix(bak);
        end
      else
          FCanvas.DrawBitmap(TBitmap(t), newSour.MakeRectf, newDest.MakeRectf, alpha, False);
    end
  else
      RaiseInfo('no interface texture! ' + t.ClassName);
{$ENDIF}
  if FDebug then
    begin
      FCanvas.Stroke.color := c2c(DEColor(1, 0.5, 0.5, 1));
      FCanvas.fill.color := FCanvas.Stroke.color;
      FCanvas.DrawLine(Point2Pointf(newDest.Centroid), Point2Pointf(PointRotation(newDest.Centroid, (newDest.Width + newDest.Height) * 0.5, (newDest.Angle))), 0.5);
      FCanvas.DrawRect(Geometry2DUnit.TV2Rect4.Init(newDest.MakeRectV2, newDest.Angle).BoundRectf, 0, 0, [], 0.3);
    end;
end;

procedure TDrawEngineInterface_FMX.Flush;
begin
  FCanvas.Flush;
end;

procedure TDrawEngineInterface_FMX.ResetState;
begin
  FLineWidth := FCanvas.Stroke.Thickness;

  FCanvas.Stroke.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.fill.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Thickness := 1;
end;

procedure TDrawEngineInterface_FMX.BeginDraw;
begin
  FCanvasSave := FCanvas.SaveState;
  FCanvas.BeginScene;
end;

procedure TDrawEngineInterface_FMX.EndDraw;
begin
  if FCanvasSave <> nil then
    begin
      FCanvas.RestoreState(FCanvasSave);
      FCanvasSave := nil;
    end;
  FCanvas.EndScene;
end;

function TDrawEngineInterface_FMX.CurrentScreenSize: TDEVec;
begin
  Result := FCurrSiz;
end;

function TDrawEngineInterface_FMX.GetTextSize(text: SystemString; size: TDEFloat): TDEVec;
var
  r: TRectf;
begin
  r := Rectf(0, 0, 10000, 10000);
  FCanvas.Font.size := size;
  FCanvas.MeasureText(r, text, False, [], TTextAlign.Leading, TTextAlign.Leading);
  Result[0] := r.Right;
  Result[1] := r.Bottom;
end;

function TDrawEngineInterface_FMX.ReadyOK: Boolean;
begin
  Result := FCanvas <> nil;
end;

function TDrawEngineInterface_FMX.EngineIntfObject: TCoreClassObject;
begin
  Result := Self;
end;

constructor TDrawEngineInterface_FMX.Create;
begin
  inherited Create;
  FCanvas := nil;
  FOwnerCanvasScale := 1.0;
  FCanvasSave := nil;
  FDebug := False;
  FCurrSiz := DEVec(100, 100);
end;

destructor TDrawEngineInterface_FMX.Destroy;
begin
  inherited Destroy;
end;

procedure TDrawEngineInterface_FMX.SetSurface(c: TCanvas; OwnerCtrl: TObject);
var
  pf: TPointf;
begin
  FCanvas := c;
  if OwnerCtrl is TControl then
    begin
      pf := TControl(OwnerCtrl).AbsoluteScale;
      FOwnerCanvasScale := (pf.X + pf.Y) * 0.5;
      FCurrSiz := DEVec(TControl(OwnerCtrl).Width, TControl(OwnerCtrl).Height);
    end
  else if OwnerCtrl is TCustomForm then
    begin
      FOwnerCanvasScale := 1.0;
      FCurrSiz := DEVec(TCustomForm(OwnerCtrl).ClientWidth, TCustomForm(OwnerCtrl).ClientHeight);
    end
  else
    begin
      FOwnerCanvasScale := 1.0;
      FCurrSiz := DEVec(c.Width, c.Height);
    end;
end;

{$IF Defined(ANDROID) or Defined(IOS)}


function TDETexture_FMX.GetTexture: TTexture;
begin
  if FTexture = nil then
      FastUpdateTexture;
  Result := FTexture;
end;
{$ELSE}


function TDETexture_FMX.GetTexture: TBitmap;
begin
  if FTexture = nil then
      FastUpdateTexture;
  Result := FTexture;
end;
{$ENDIF}


constructor TDETexture_FMX.Create;
begin
  inherited Create;
  FTexture := nil;
end;

destructor TDETexture_FMX.Destroy;
begin
  ReleaseFMXResource;
  inherited Destroy;
end;

procedure TDETexture_FMX.ReleaseFMXResource;
begin
  if FTexture <> nil then
      DisposeObject(FTexture);
  FTexture := nil;
end;

procedure TDETexture_FMX.FastUpdateTexture;
{$IF Defined(ANDROID) or Defined(IOS)}
var
  newbmp: TMemoryRaster;
begin
  ReleaseFMXResource;
  FTexture := TTexture.Create;
  FTexture.Style := [TTextureStyle.Dynamic];
  FTexture.MinFilter := TTextureFilter.Linear;
  FTexture.MagFilter := TTextureFilter.Linear;
  FTexture.PixelFormat := TPixelFormat.BGRA;
  FTexture.SetSize(Width, Height);
  FTexture.Initialize;

  newbmp := FormatAsBGRA;
  FTexture.UpdateTexture(newbmp.Bits, Width * 4);
  DisposeObject(newbmp);
end;
{$ELSE}


begin
  ReleaseFMXResource;
  FTexture := TBitmap.Create;
  MemoryBitmapToBitmap(Self, FTexture);
end;
{$ENDIF}


constructor TResourceTexture.Create;
begin
  inherited Create;
  FLastLoadFile := '';
end;

constructor TResourceTexture.Create(filename: SystemString);
begin
  inherited Create;
  FLastLoadFile := '';

  if filename <> '' then
      LoadFromFileIO(filename);
end;

procedure TResourceTexture.LoadFromFileIO(filename: SystemString);
var
  stream: TCoreClassStream;
begin
  FLastLoadFile := '';
  if FileIOExists(filename) then
    begin
      try
        stream := FileIOOpen(filename);
        stream.Position := 0;
        LoadFromStream(stream);
        DisposeObject(stream);
        FLastLoadFile := filename;
      except
          RaiseInfo('texture "%s" format error! ', [filename]);
      end;
    end
  else
      RaiseInfo('file "%s" no exists', [filename]);
end;

constructor TResourceTextureIntf.Create(tex: TResourceTexture);
begin
  inherited Create;
  Texture := tex;
  TextureRect := Texture.BoundsRectV2;
  SizeScale := DEVec(1.0, 1.0);
end;

destructor TResourceTextureIntf.Destroy;
begin
  inherited Destroy;
end;

function TResourceTextureIntf.SizeOfVec: TDEVec;
begin
  Result := DEVec(RectWidth(TextureRect) * SizeScale[0], RectHeight(TextureRect) * SizeScale[1]);
end;

procedure TResourceTextureIntf.ChangeTexture(tex: TResourceTexture);
begin
  Texture := tex;
  TextureRect := Texture.BoundsRectV2;
  SizeScale := DEVec(1.0, 1.0);
end;

constructor TResourceTextureCache.Create;
begin
  inherited Create;
  TextureList := THashObjectList.Create(True, 1024);
  DefaultTexture := TResourceTexture.Create('');
  DefaultTexture.SetSize(2, 2, RasterColorF(0, 0, 0, 1.0));
end;

destructor TResourceTextureCache.Destroy;
begin
  DisposeObject(TextureList);
  DisposeObject(DefaultTexture);
  inherited Destroy;
end;

function TResourceTextureCache.CreateResourceTexture(filename: SystemString): TResourceTextureIntf;
var
  tex: TResourceTexture;
begin
  if filename = '' then
      exit(nil);

  filename := umlTrimSpace(filename);

  if filename = '' then
    begin
      tex := DefaultTexture;
    end
  else
    begin
      if not TextureList.Exists(filename) then
        begin
          if FileIOExists(filename) then
            begin
              try
                tex := TResourceTexture.Create(filename);
                TextureList.Add(filename, tex);
              except
                  tex := DefaultTexture;
              end;
            end
          else
              tex := DefaultTexture;
        end
      else
          tex := TextureList[filename] as TResourceTexture;
    end;

  Result := TResourceTextureIntf.Create(tex);
  Result.TextureRect := tex.BoundsRectV2;
  Result.SizeScale := DEVec(1.0, 1.0);
end;

procedure TResourceTextureCache.ReleaseAllFMXRsource;
begin
  TextureList.Progress(
    procedure(const Name: PSystemString; obj: TCoreClassObject)
    begin
      if obj is TDETexture_FMX then
          TDETexture_FMX(obj).ReleaseFMXResource;
    end);
end;

function _NewRaster: TMemoryRaster;
begin
  Result := DefaultTextureClass.Create;
end;

function _NewRasterFromFile(const fn: string): TMemoryRaster;
begin
  Result := NewRaster();
  LoadMemoryBitmap(fn, TResourceTexture(Result));
end;

function _NewRasterFromStream(const stream: TCoreClassStream): TMemoryRaster;
begin
  Result := NewRaster();
  LoadMemoryBitmap(stream, TResourceTexture(Result));
end;

procedure _SaveRaster(mr: TMemoryRaster; const fn: string);
begin
  SaveMemoryBitmap(fn, mr);
end;

initialization

DefaultTextureClass := TResourceTexture;

TextureCache := TResourceTextureCache.Create;

NewRaster := _NewRaster;
NewRasterFromFile := _NewRasterFromFile;
NewRasterFromStream := _NewRasterFromStream;
SaveRaster := _SaveRaster;

finalization


DisposeObject(TextureCache);

end.
