{ ****************************************************************************** }
{ * draw engine with FMX Support                                               * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit zDrawEngineInterface_SlowFMX;

{$INCLUDE ..\zDefine.inc}

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
    procedure SetSize(R: TDERect);
    procedure SetLineWidth(w: TDEFloat);
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
    procedure DrawRect(R: TDERect; angle: TDEFloat; COLOR: TDEColor);
    procedure FillRect(R: TDERect; angle: TDEFloat; COLOR: TDEColor);
    procedure DrawEllipse(R: TDERect; COLOR: TDEColor);
    procedure FillEllipse(R: TDERect; COLOR: TDEColor);
    procedure DrawText(Text: SystemString; Size: TDEFloat; R: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
    procedure DrawTexture(T: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
    procedure Flush;
    procedure ResetState;
    procedure BeginDraw;
    procedure EndDraw;
    function CurrentScreenSize: TDEVec;
    function GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec;
    function ReadyOK: Boolean;
    function EngineIntfObject: TCoreClassObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSurface(C: TCanvas; OwnerCtrl: TObject);
    property Canvas: TCanvas read FCanvas write SetCanvas;

    property Debug: Boolean read FDebug write FDebug;
    // only work in mobile device and gpu fast mode
    property OwnerCanvasScale: TDEFloat read FOwnerCanvasScale write FOwnerCanvasScale;
    property CanvasScale: TDEFloat read FOwnerCanvasScale write FOwnerCanvasScale;
    property ScreenSize: TDEVec read FCurrSiz;
  end;

  TDETexture_FMX = class(TDETexture)
  protected
    FTexture: TBitmap;
    function GetTexture: TBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ReleaseFMXResource; override;
    procedure FastUpdateTexture; override;

    property Texture: TBitmap read GetTexture;
  end;

  TResourceTexture = class(TDETexture_FMX)
  protected
    FLastLoadFile: SystemString;
  public
    constructor Create; overload; override;
    constructor Create(fileName: SystemString); overload; virtual;

    procedure LoadFromFileIO(fileName: SystemString);
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
    function CreateResourceTexture(fileName: SystemString): TResourceTextureIntf;
    procedure ReleaseAllFMXRsource;
  end;

function c2c(C: TDEColor): TAlphaColor; inline; overload;
function c2c(C: TAlphaColor): TDEColor; inline; overload;
function p2p(pt: TDEVec): TPointf; inline; overload;
function r2r(R: TDERect): TRectf; inline; overload;
function AlphaColor2RasterColor(C: TAlphaColor): TRasterColor; inline;
function DE4V2Corners(sour: TDE4V): TCornersF; inline;
function DEColor(C: TAlphaColor): TDEColor; inline; overload;
function PrepareColor(const SrcColor: TAlphaColor; const Opacity: TDEFloat): TAlphaColor; inline;
procedure MakeMatrixRotation(angle, width, height, X, Y, RotationCenter_X, RotationCenter_Y: TDEFloat; var OutputMatrix: TMatrix; var OutputRect: TRectf); inline;

procedure MemoryBitmapToSurface(bmp: TMemoryRaster; Surface: TBitmapSurface); overload; inline;
procedure MemoryBitmapToSurface(bmp: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface); overload; inline;
procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; bmp: TMemoryRaster); inline;
procedure MemoryBitmapToBitmap(b: TMemoryRaster; bmp: TBitmap); overload;
procedure MemoryBitmapToBitmap(b: TMemoryRaster; sourRect: TRect; bmp: TBitmap); overload;
procedure BitmapToMemoryBitmap(bmp: TBitmap; b: TMemoryRaster);
procedure LoadMemoryBitmap(F: SystemString; b: TMemoryRaster); overload;
procedure LoadMemoryBitmap(F: SystemString; b: TSequenceMemoryRaster); overload;
procedure LoadMemoryBitmap(F: SystemString; b: TDETexture); overload;
procedure LoadMemoryBitmap(stream: TCoreClassStream; b: TMemoryRaster); overload;

procedure SaveMemoryBitmap(F: SystemString; b: TMemoryRaster); overload;
procedure SaveMemoryBitmap(b: TMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream); overload;
procedure SaveMemoryBitmap(b: TSequenceMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream); overload;

var
  // resource texture cache
  TextureCache: TResourceTextureCache = nil;

implementation

uses
{$IF Defined(ANDROID) or Defined(IOS)}
  FMX.Canvas.GPU, FMX.TextLayout.GPU, FMX.StrokeBuilder, FMX.Canvas.GPU.Helpers,
{$ENDIF}
  MemoryStream64, MediaCenter;

function c2c(C: TDEColor): TAlphaColor;
begin
  Result := TAlphaColorF.Create(C[0], C[1], C[2], C[3]).ToAlphaColor;
end;

function c2c(C: TAlphaColor): TDEColor;
begin
  with TAlphaColorF.Create(C) do
      Result := DEColor(R, g, b, A);
end;

function p2p(pt: TDEVec): TPointf;
begin
  Result := Point2Pointf(pt);
end;

function r2r(R: TDERect): TRectf;
begin
  Result := MakeRectf(R);
end;

function AlphaColor2RasterColor(C: TAlphaColor): TRasterColor;
var
  ce: TRasterColorEntry;
begin
  ce.R := TAlphaColorRec(C).R;
  ce.g := TAlphaColorRec(C).g;
  ce.b := TAlphaColorRec(C).b;
  ce.A := TAlphaColorRec(C).A;
  Result := ce.RGBA;
end;

function DE4V2Corners(sour: TDE4V): TCornersF;
begin
  with TV2Rect4.Init(sour.MakeRectV2, sour.angle) do
    begin
      Result[0] := Point2Pointf(LeftTop);
      Result[1] := Point2Pointf(RightTop);
      Result[2] := Point2Pointf(RightBottom);
      Result[3] := Point2Pointf(LeftBottom);
    end;
end;

function DEColor(C: TAlphaColor): TDEColor;
begin
  with TAlphaColorF.Create(C) do
      Result := DEColor(R, g, b, A);
end;

function PrepareColor(const SrcColor: TAlphaColor; const Opacity: TDEFloat): TAlphaColor;
begin
  if Opacity <= 1.0 then
    begin
      TAlphaColorRec(Result).R := Round(TAlphaColorRec(SrcColor).R * Opacity);
      TAlphaColorRec(Result).g := Round(TAlphaColorRec(SrcColor).g * Opacity);
      TAlphaColorRec(Result).b := Round(TAlphaColorRec(SrcColor).b * Opacity);
      TAlphaColorRec(Result).A := Round(TAlphaColorRec(SrcColor).A * Opacity);
    end
  else if (TAlphaColorRec(SrcColor).A < $FF) then
      Result := PremultiplyAlpha(SrcColor)
  else
      Result := SrcColor;
end;

procedure MakeMatrixRotation(angle, width, height, X, Y, RotationCenter_X, RotationCenter_Y: TDEFloat; var OutputMatrix: TMatrix; var OutputRect: TRectf); inline;
const
  Scale_X = 1.0;
  Scale_Y = 1.0;
var
  ScaleMatrix, rotMatrix, m1, m2: TMatrix;
begin
  ScaleMatrix := TMatrix.identity;
  ScaleMatrix.m11 := Scale_X;
  ScaleMatrix.m22 := Scale_Y;
  OutputMatrix := ScaleMatrix;

  m1 := TMatrix.identity;
  m1.m31 := -(RotationCenter_X * width * Scale_X + X);
  m1.m32 := -(RotationCenter_Y * height * Scale_Y + Y);
  m2 := TMatrix.identity;
  m2.m31 := RotationCenter_X * width * Scale_X + X;
  m2.m32 := RotationCenter_Y * height * Scale_Y + Y;
  rotMatrix := m1 * (TMatrix.CreateRotation(DegToRad(angle)) * m2);
  OutputMatrix := OutputMatrix * rotMatrix;

  OutputRect.TopLeft := Pointf(X, Y);
  OutputRect.BottomRight := Pointf(X + width, Y + height);
end;

procedure MemoryBitmapToSurface(bmp: TMemoryRaster; Surface: TBitmapSurface);
var
  i: Integer;
  p1, p2: PCardinal;
  C: TRasterColorEntry;
  DC: TAlphaColor;
begin
{$IF Defined(ANDROID) or Defined(IOS)}
  Surface.SetSize(bmp.width, bmp.height, TPixelFormat.RGBA);
{$ELSE}
  Surface.SetSize(bmp.width, bmp.height, TPixelFormat.BGRA);
{$ENDIF}
  p1 := PCardinal(@bmp.Bits[0]);
  p2 := PCardinal(Surface.Bits);
  for i := bmp.width * bmp.height - 1 downto 0 do
    begin
{$IF Defined(ANDROID) or Defined(IOS) or Defined(OSX)}
      C.RGBA := RGBA2BGRA(TRasterColor(p1^));
{$ELSE}
      C.RGBA := TRasterColor(p1^);
{$IFEND}
      TAlphaColorRec(DC).R := C.R;
      TAlphaColorRec(DC).g := C.g;
      TAlphaColorRec(DC).b := C.b;
      TAlphaColorRec(DC).A := C.A;
      p2^ := DC;
      Inc(p1);
      Inc(p2);
    end;
end;

procedure MemoryBitmapToSurface(bmp: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface);
var
  nb: TMemoryRaster;
begin
  nb := TMemoryRaster.Create;
  nb.DrawMode := dmBlend;
  nb.SetSize(sourRect.width, sourRect.height, RasterColor(0, 0, 0, 0));
  bmp.DrawTo(nb, 0, 0, sourRect);
  MemoryBitmapToSurface(nb, Surface);
  DisposeObject(nb);
end;

procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; bmp: TMemoryRaster);
var
  X, Y: Integer;
begin
  bmp.SetSize(Surface.width, Surface.height);
  for Y := 0 to Surface.height - 1 do
    for X := 0 to Surface.width - 1 do
      with TAlphaColorRec(Surface.pixels[X, Y]) do
          bmp.Pixel[X, Y] := RasterColor(R, g, b, A)
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

procedure LoadMemoryBitmap(F: SystemString; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if b.CanLoadFile(F) then
    begin
      b.LoadFromFile(F);
    end
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromFile(F, Surf, TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
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

procedure LoadMemoryBitmap(F: SystemString; b: TSequenceMemoryRaster);
begin
  if b.CanLoadFile(F) then
      b.LoadFromFile(F)
  else
      LoadMemoryBitmap(F, TMemoryRaster(b));
end;

procedure LoadMemoryBitmap(F: SystemString; b: TDETexture);
begin
  LoadMemoryBitmap(F, TSequenceMemoryRaster(b));
  b.ReleaseFMXResource;
end;

procedure SaveMemoryBitmap(F: SystemString; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['*.bmp'], F) then
      b.SaveToFile(F)
  else if umlMultipleMatch(['*.seq'], F) then
      b.SaveToZLibCompressFile(F)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(b, Surf);
        TBitmapCodecManager.SaveToFile(F, Surf, nil);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure SaveMemoryBitmap(b: TMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['.bmp'], fileExt) then
      b.SaveToBmpStream(DestStream)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(b, Surf);
        TBitmapCodecManager.SaveToStream(DestStream, Surf, fileExt);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure SaveMemoryBitmap(b: TSequenceMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['.bmp'], fileExt) then
      b.SaveToBmpStream(DestStream)
  else if umlMultipleMatch(['.seq'], fileExt) then
      b.SaveToStream(DestStream)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(b, Surf);
        TBitmapCodecManager.SaveToStream(DestStream, Surf, fileExt);
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
      Exit;
    end;

  FCanvas := Value;
  FCurrSiz := DEVec(FCanvas.width, FCanvas.height);
end;

procedure TDrawEngineInterface_FMX.SetSize(R: TDERect);
begin
  FCanvas.IntersectClipRect(MakeRectf(R));
end;

procedure TDrawEngineInterface_FMX.SetLineWidth(w: TDEFloat);
begin
  if not IsEqual(FLineWidth, w) then
    begin
      FLineWidth := w;
      FCanvas.Stroke.Thickness := w;
    end;
end;

procedure TDrawEngineInterface_FMX.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.DrawLine(p2p(pt1), p2p(pt2), COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.DrawRect(R: TDERect; angle: TDEFloat; COLOR: TDEColor);
var
  M, bak: TMatrix;
  rf: TRectf;
begin
  if angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(angle, RectWidth(R), RectHeight(R),
        MinValue(R[0, 0], R[1, 0]), MinValue(R[0, 1], R[1, 1]), 0.5, 0.5, M, rf);
      FCanvas.MultiplyMatrix(M);
    end;

  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.DrawRect(r2r(R), 0, 0, [], COLOR[3]);

  if angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.FillRect(R: TDERect; angle: TDEFloat; COLOR: TDEColor);
var
  M, bak: TMatrix;
  rf: TRectf;
begin
  if angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(angle, RectWidth(R), RectHeight(R),
        MinValue(R[0, 0], R[1, 0]), MinValue(R[0, 1], R[1, 1]), 0.5, 0.5, M, rf);
      FCanvas.MultiplyMatrix(M);
    end;

  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.FillRect(r2r(R), 0, 0, [], COLOR[3]);

  if angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.DrawEllipse(R: TDERect; COLOR: TDEColor);
begin
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.DrawEllipse(r2r(R), COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.FillEllipse(R: TDERect; COLOR: TDEColor);
begin
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.FillEllipse(r2r(R), COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.DrawText(Text: SystemString; Size: TDEFloat; R: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
var
  M, bak: TMatrix;
  rf: TRectf;
  TA: TTextAlign;
begin
  if angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(angle, RectWidth(R), RectHeight(R),
        MinValue(R[0, 0], R[1, 0]), MinValue(R[0, 1], R[1, 1]), RotateVec[0], RotateVec[1], M, rf);
      FCanvas.MultiplyMatrix(M);
    end;

  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.Font.Size := Size;

  if center then
      TA := TTextAlign.center
  else
      TA := TTextAlign.Leading;

  FCanvas.FillText(r2r(R), Text, False, COLOR[3], [], TA, TTextAlign.center);

  if angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.DrawTexture(T: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
var
  newSour, newDest: TDE4V;
  M, bak: TMatrix;
  R: TRectf;
begin
  newSour := sour;
  newDest := dest;

  if (T is TDETexture_FMX) then
    begin
      if (TDETexture_FMX(T).width = 0) or (TDETexture_FMX(T).height = 0) then
          Exit;
      if newSour.IsZero then
          newSour := TDE4V.Init(TDETexture_FMX(T).BoundsRectV2, 0);
      if newDest.IsZero then
          newDest := newSour;
      if not IsEqual(newDest.angle, 0, 0.1) then
        begin
          bak := FCanvas.Matrix;
          MakeMatrixRotation(newDest.angle, newDest.width, newDest.height,
            MinValue(newDest.Left, newDest.Right), MinValue(newDest.Top, newDest.Bottom), 0.5, 0.5, M, R);
          FCanvas.MultiplyMatrix(M);
          FCanvas.DrawBitmap(TDETexture_FMX(T).Texture, newSour.MakeRectf, newDest.MakeRectf, alpha, False);
          FCanvas.SetMatrix(bak);
        end
      else
          FCanvas.DrawBitmap(TDETexture_FMX(T).Texture, newSour.MakeRectf, newDest.MakeRectf, alpha, True);
    end
  else if (T is TBitmap) then
    begin
      if (TBitmap(T).width = 0) or (TBitmap(T).height = 0) then
          Exit;
      if newSour.IsZero then
          newSour := TDE4V.Init(TBitmap(T).BoundsF, 0);
      if newDest.IsZero then
          newDest := newSour;
      if not IsEqual(newDest.angle, 0, 0.1) then
        begin
          bak := FCanvas.Matrix;
          MakeMatrixRotation(newDest.angle, newDest.width, newDest.height, MinValue(newDest.Left, newDest.Right), MinValue(newDest.Top, newDest.Bottom), 0.5, 0.5, M, R);
          FCanvas.MultiplyMatrix(M);
          FCanvas.DrawBitmap(TBitmap(T), newSour.MakeRectf, newDest.MakeRectf, alpha, False);
          FCanvas.SetMatrix(bak);
        end
      else
          FCanvas.DrawBitmap(TBitmap(T), newSour.MakeRectf, newDest.MakeRectf, alpha, False);
    end
  else
      RaiseInfo('no interface texture! ' + T.ClassName);

  if FDebug then
    begin
      FCanvas.Stroke.COLOR := c2c(DEColor(1, 0.5, 0.5, 1));
      FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
      FCanvas.DrawLine(Point2Pointf(newDest.Centroid), Point2Pointf(PointRotation(newDest.Centroid, (newDest.width + newDest.height) * 0.5, (newDest.angle))), 0.5);
      FCanvas.DrawRect(Geometry2DUnit.TV2Rect4.Init(newDest.MakeRectV2, newDest.angle).BoundRectf, 0, 0, [], 0.3);
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

function TDrawEngineInterface_FMX.GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec;
var
  R: TRectf;
begin
  R := Rectf(0, 0, 10000, 10000);
  FCanvas.Font.Size := Size;
  FCanvas.MeasureText(R, Text, False, [], TTextAlign.Leading, TTextAlign.Leading);
  Result[0] := R.Right;
  Result[1] := R.Bottom;
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

procedure TDrawEngineInterface_FMX.SetSurface(C: TCanvas; OwnerCtrl: TObject);
var
  pf: TPointf;
begin
  FCanvas := C;
  if OwnerCtrl is TControl then
    begin
      pf := TControl(OwnerCtrl).AbsoluteScale;
      FOwnerCanvasScale := (pf.X + pf.Y) * 0.5;
      FCurrSiz := DEVec(TControl(OwnerCtrl).width, TControl(OwnerCtrl).height);
    end
  else if OwnerCtrl is TCustomForm then
    begin
      FOwnerCanvasScale := 1.0;
      FCurrSiz := DEVec(TCustomForm(OwnerCtrl).ClientWidth, TCustomForm(OwnerCtrl).ClientHeight);
    end
  else
    begin
      FOwnerCanvasScale := 1.0;
      FCurrSiz := DEVec(C.width, C.height);
    end;
end;

function TDETexture_FMX.GetTexture: TBitmap;
begin
  if FTexture = nil then
      FastUpdateTexture;
  Result := FTexture;
end;

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
begin
  ReleaseFMXResource;
  FTexture := TBitmap.Create;
  MemoryBitmapToBitmap(Self, FTexture);
end;

constructor TResourceTexture.Create;
begin
  inherited Create;
  FLastLoadFile := '';
end;

constructor TResourceTexture.Create(fileName: SystemString);
begin
  inherited Create;
  FLastLoadFile := '';

  if fileName <> '' then
      LoadFromFileIO(fileName);
end;

procedure TResourceTexture.LoadFromFileIO(fileName: SystemString);
var
  stream: TCoreClassStream;
begin
  FLastLoadFile := '';
  if FileIOExists(fileName) then
    begin
      try
        stream := FileIOOpen(fileName);
        stream.Position := 0;
        LoadFromStream(stream);
        DisposeObject(stream);
        FLastLoadFile := fileName;
      except
          RaiseInfo('texture "%s" format error! ', [fileName]);
      end;
    end
  else
      RaiseInfo('file "%s" no exists', [fileName]);
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

function TResourceTextureCache.CreateResourceTexture(fileName: SystemString): TResourceTextureIntf;
var
  tex: TResourceTexture;
begin
  if fileName = '' then
      Exit(nil);

  fileName := umlTrimSpace(fileName);

  if fileName = '' then
    begin
      tex := DefaultTexture;
    end
  else
    begin
      if not TextureList.Exists(fileName) then
        begin
          if FileIOExists(fileName) then
            begin
              try
                tex := TResourceTexture.Create(fileName);
                TextureList.Add(fileName, tex);
              except
                  tex := DefaultTexture;
              end;
            end
          else
              tex := DefaultTexture;
        end
      else
          tex := TextureList[fileName] as TResourceTexture;
    end;

  Result := TResourceTextureIntf.Create(tex);
  Result.TextureRect := tex.BoundsRectV2;
  Result.SizeScale := DEVec(1.0, 1.0);
end;

procedure TResourceTextureCache.ReleaseAllFMXRsource;
begin
  TextureList.Progress(
    procedure(const Name: PSystemString; Obj: TCoreClassObject)
    begin
      if Obj is TDETexture_FMX then
          TDETexture_FMX(Obj).ReleaseFMXResource;
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
 
