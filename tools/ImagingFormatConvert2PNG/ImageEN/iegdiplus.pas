(*
Copyright (c) 1998-2014 by Carlotta Calandra. All rights reserved.
Copyright (c) 2011-2014 by Xequte software.

This software comes without express or implied warranty.
In no case shall the author be liable for any damage or unwanted behavior of any
computer hardware and/or software.

Author grants you the right to include the component
in your application, whether COMMERCIAL, SHAREWARE, or FREEWARE.

ImageEn, IEvolution and ImageEn ActiveX may not be included in any
commercial, shareware or freeware libraries or components.

www.ImageEn.com
*)

(*
File version 1010
*)

unit iegdiplus;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Classes, sysutils, Graphics,
     {$ifdef IEHASTYPES} Types, {$endif}
     {$ifdef IEHASUITYPES} System.UITypes, {$endif}
     hyiedefs;

type

  TIECanvasSmoothingMode = (iesmInvalid, iesmDefault, iesmBestPerformance, iesmBestRenderingQuality, iesmNone, iesmAntialias);

  TIECanvasPenLineJoin = (ieljMiter, ieljBevel, ieljRound, ieljMiterClipped);

  // GDI+ Pen wrapper and VCL TPen wrapper
  TIEPen = class
    private
      FGHandle: pointer;   // gdi+ handle
      FPen: TPen;          // VCL object
      FColor: TColor;
      FTransparency: integer;
      FWidth: single;
      FStyle: TPenStyle;
      FMode: TPenMode;
      FLineJoin: TIECanvasPenLineJoin;
      procedure ReCreatePen;
      procedure SetWidth(value: single);
      procedure SetTColor(value: TColor);
      procedure SetTransparency(value: integer);
      procedure SetStyle(value: TPenStyle);
      function GetRGBAColor(): TRGBA;
      procedure SetMode(value: TPenMode);
      procedure SetVHandle(value: HPen);
      function GetVHandle: HPen;
      procedure SetLineJoin(value: TIECanvasPenLineJoin);

    public
      constructor Create(Pen: TPen);
      destructor Destroy; override;
      property Width: single read FWidth write SetWidth;
      property Color: TColor read FColor write SetTColor;
      property Transparency: integer read FTransparency write SetTransparency;
      property Style: TPenStyle read FStyle write SetStyle;
      property Mode: TPenMode read FMode write SetMode;  // not available under GDI+
      property Handle: HPen read GetVHandle write SetVHandle;  // VCL TPen handle
      property LineJoin: TIECanvasPenLineJoin read FLineJoin write SetLineJoin;
  end;

  // GDI+ Brush wrapper and VCL TBrush wrapper
  TIEBrush = class
    private
      FGHandle: pointer;  // gdi+ handle
      FBrush: TBrush;    // VCL object
      FColor: TColor;
      FTransparency: integer;
      FBackColor: TColor;
      FBackTransparency: integer;
      FStyle: TBrushStyle;
      procedure ReCreateBrush;
      function GetBitmap: TBitmap;
      procedure SetBitmap(value: TBitmap);
      procedure SetTColor(value: TColor);
      procedure SetBackTColor(value: TColor);
      procedure SetTransparency(value: integer);
      procedure SetBackTransparency(value: integer);
      procedure SetStyle(value: TBrushStyle);
      function GetRGBAColor(): TRGBA;

    public
      constructor Create(Brush: TBrush);
      destructor Destroy; override;
      property Bitmap: TBitmap read GetBitmap write SetBitmap; // not implemented in GDI+
      property Color: TColor read FColor write SetTColor;
      property BackColor: TColor read FBackColor write SetBackTColor;
      property Transparency: integer read FTransparency write SetTransparency;
      property BackTransparency: integer read FBackTransparency write SetBackTransparency;
      property Style: TBrushStyle read FStyle write SetStyle;
  end;

{!!
<FS>TIECanvas

<FM>Description<FN>
TIECanvas is a GDI+ Graphics wrapper and VCL TCanvas wrapper.
!!}
  TIECanvas = class
    private
      FSmoothingMode: TIECanvasSmoothingMode;
      procedure SetSmoothingMode(value: TIECanvasSmoothingMode);
      procedure SetPenPos(value: TPoint);
      function GetHandle: HDC;
      function GetFont: TFont;

    protected
      FGraphics: pointer;
      FCanvas: TCanvas;
      FPen: TIEPen;
      FPenPos: TPoint;
      FBrush: TIEBrush;
      FUseGDIPlus: boolean;
      FImage: pointer;
      FOnDestroy: TNotifyEvent;
      FROIBitmap: TObject;  // must be a TIEBitmap

    public
      constructor Create(Canvas: TCanvas; AntiAlias: boolean = true; UseGDIPlus: boolean = true; Bitmap: TBitmap = nil); overload;
      destructor Destroy; override;
      property SmoothingMode: TIECanvasSmoothingMode read FSmoothingMode write SetSmoothingMode;
      property PenPos: TPoint read FPenPos write SetPenPos;
      property Pen: TIEPen read FPen;
      property Brush: TIEBrush read FBrush;
      procedure MoveTo(X, Y: integer);
      procedure LineTo(X, Y: integer);
      procedure FillRect(const Rect: TRect);
      procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
      procedure Rectangle(const Rect: TRect); overload;
      procedure Ellipse(X1, Y1, X2, Y2: Integer); overload;
      procedure Ellipse(const Rect: TRect); overload;
      procedure DrawLine(X1, Y1, X2, Y2: integer);
      procedure DrawLinesPath(points: TIE2DPointArray);
      procedure Polygon(Points: array of TPoint);
      procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
      procedure Polyline(Points: array of TPoint);
      property Handle: HDC read GetHandle;
      function TextWidth(const Text: WideString): Integer;
      function TextHeight(const Text: WideString): Integer;
      procedure TextOut(X, Y: Integer; const Text: string);
      procedure TextOut2(X, Y: Integer; const Text: string);
      property Font: TFont read GetFont;
      procedure TextRect(Rect: TRect; X, Y: Integer; const Text: WideString);
      procedure TextRectEx(Rect: TRect; X, Y: integer; const Text: WideString);
      function TextExtent(const Text: WideString): TSize;
      procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
      property GDICanvas: TCanvas read FCanvas;
      procedure Rotate(Angle: double);
      procedure Translate(dx: double; dy: double);
      procedure ResetTransform();
      procedure Pie(X: single; Y: single; Width: single; Height: single; StartAngle: single; SweepAngle: single); overload;
      procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); overload;

      property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;

      property ROIBitmap: TObject read FROIBitmap write FROIBitmap;
  end;

function IEGDIPEnabled: boolean;
function IEGDIPAvailable: boolean;
procedure IEGDIPLoadLibrary;
procedure IEGDIPUnLoadLibrary;


procedure IEInitialize_iegdiplus;
procedure IEFinalize_iegdiplus;




implementation

uses imageenproc, hyieutils, iesettings;



type

  TGdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;
    DebugEventCallback      : pointer;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs  : BOOL;
  end;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

var
  StartupInput: TGDIPlusStartupInput;
  gdiplusToken: ULONG;
  IE_GDIPLUSHandle : THandle;
  gdiplusRefCount: integer;

const
  IE_CompositingModeSourceOver = 0;
  IE_CompositingModeSourceCopy = 1;

const
  IE_CompositingQualityInvalid          = -1;
  IE_CompositingQualityDefault          = 0;
  IE_CompositingQualityHighSpeed        = 1;
  IE_CompositingQualityHighQuality      = 2;
  IE_CompositingQualityGammaCorrected   = 3;
  IE_CompositingQualityAssumeLinear     = 4;

const
  IE_PixelFormatIndexed        = $00010000; // Indexes into a palette
  IE_PixelFormatGDI            = $00020000; // Is a GDI-supported format
  IE_PixelFormatAlpha          = $00040000; // Has an alpha component
  IE_PixelFormatPAlpha         = $00080000; // Pre-multiplied alpha
  IE_PixelFormatExtended       = $00100000; // Extended color 16 bits/channel
  IE_PixelFormatCanonical      = $00200000;
  IE_PixelFormatUndefined      = 0;
  IE_PixelFormatDontCare       = 0;
  IE_PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or IE_PixelFormatIndexed or IE_PixelFormatGDI);
  IE_PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or IE_PixelFormatIndexed or IE_PixelFormatGDI);
  IE_PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or IE_PixelFormatIndexed or IE_PixelFormatGDI);
  IE_PixelFormat8bppGrayScale  = (4  or (8 shl 8) or IE_PixelFormatExtended); // !!!! supported?
  IE_PixelFormat16bppGrayScale = (4  or (16 shl 8) or IE_PixelFormatExtended);
  IE_PixelFormat16bppRGB555    = (5  or (16 shl 8) or IE_PixelFormatGDI);
  IE_PixelFormat16bppRGB565    = (6  or (16 shl 8) or IE_PixelFormatGDI);
  IE_PixelFormat16bppARGB1555  = (7  or (16 shl 8) or IE_PixelFormatAlpha or IE_PixelFormatGDI);
  IE_PixelFormat24bppRGB       = (8  or (24 shl 8) or IE_PixelFormatGDI);
  IE_PixelFormat32bppRGB       = (9  or (32 shl 8) or IE_PixelFormatGDI);
  IE_PixelFormat32bppARGB      = (10 or (32 shl 8) or IE_PixelFormatAlpha or IE_PixelFormatGDI or IE_PixelFormatCanonical);
  IE_PixelFormat32bppPARGB     = (11 or (32 shl 8) or IE_PixelFormatAlpha or IE_PixelFormatPAlpha or IE_PixelFormatGDI);
  IE_PixelFormat48bppRGB       = (12 or (48 shl 8) or IE_PixelFormatExtended);
  IE_PixelFormat64bppARGB      = (13 or (64 shl 8) or IE_PixelFormatAlpha  or IE_PixelFormatCanonical or IE_PixelFormatExtended);
  IE_PixelFormat64bppPARGB     = (14 or (64 shl 8) or IE_PixelFormatAlpha  or IE_PixelFormatPAlpha or IE_PixelFormatExtended);
  IE_PixelFormatMax            = 15;


var

  ///// GDI+ functions
  IE_GdiplusStartup : function(out token: ULONG; input: PGdiplusStartupInput; output: pointer): ULONG; stdcall;
  IE_GdiplusShutdown : procedure(token: ULONG); stdcall;
  IE_GdipCreateFromHDC : function(hdc: HDC; out graphics: pointer): ULONG; stdcall;
  IE_GdipCreateFromHDC2 : function(hdc: HDC; hDevice: THandle; out graphics: pointer): ULONG; stdcall;
  IE_GdipDeleteGraphics : function(graphics: pointer): ULONG; stdcall;
  IE_GdipCreatePen1 : function(color: TRGBA; width: Single; unit_: ULONG; out pen: pointer): ULONG; stdcall;
  IE_GdipDeletePen : function(pen: pointer): ULONG; stdcall;
  IE_GdipSetPenWidth : function(pen: pointer; width: Single): ULONG; stdcall;
  IE_GdipSetPenColor : function(pen: pointer; argb: TRGBA): ULONG; stdcall;
  IE_GdipSetPenDashStyle : function(pen: pointer; dashstyle: ULONG): ULONG; stdcall;
  IE_GdipDrawLine : function(graphics: pointer; pen: pointer; x1: Single; y1: Single; x2: Single; y2: Single): ULONG; stdcall;
  IE_GdipSetSmoothingMode : function(graphics: pointer; smoothingMode: ULONG): ULONG; stdcall; // -1=invalid, 0=default, 1=BestPerformance, 2=BestRenderingQuality, 3=None, 4=Antialias
  IE_GdipSetPenDashArray : function(pen: pointer; dash: PSingle; count: Integer): ULONG; stdcall;
  IE_GdipDeleteBrush : function(brush: pointer): ULONG; stdcall;
  IE_GdipCreateSolidFill : function(color: TRGBA; out brush: pointer): ULONG; stdcall;
  IE_GdipFillRectangle : function(graphics: pointer; brush: pointer; x: Single; y: Single; width: Single; height: Single): ULONG; stdcall;
  IE_GdipDrawRectangle : function(graphics: pointer; pen: pointer; x: Single; y: Single; width: Single; height: Single): ULONG; stdcall;
  IE_GdipCreateHatchBrush : function(hatchstyle: Integer; forecol: TRGBA; backcol: TRGBA; out brush: pointer): ULONG; stdcall;
  IE_GdipDrawEllipse : function(graphics: pointer; pen: pointer; x: Single; y: Single; width: Single; height: Single): ULONG; stdcall;
  IE_GdipFillEllipse : function(graphics: pointer; brush: pointer; x: Single; y: Single; width: Single; height: Single): ULONG; stdcall;
  IE_GdipDrawPolygonI : function(graphics: pointer; pen: pointer; points: pointer; count: Integer): ULONG; stdcall;
  IE_GdipFillPolygonI : function(graphics: pointer; brush: pointer; points: pointer; count: Integer; fillMode: ULONG): ULONG; stdcall;
  IE_GdipDrawPie : function(graphics: pointer; pen: pointer; x: single; y: single; width: single; height: single; startAngle: single; sweepAngle: single): ULONG; stdcall;
  IE_GdipFillPie : function(graphics: pointer; brush: pointer; x: single; y: single; width: single; height: single; startAngle: single; sweepAngle: single): ULONG; stdcall;
  IE_GdipDrawLinesI : function(graphics: pointer; pen: pointer; points: pointer; count: Integer): ULONG; stdcall;
  IE_GdipDrawArc : function(graphics: pointer; pen: pointer; x: Single; y: Single; width: Single; height: Single; startAngle: Single; sweepAngle: Single): ULONG; stdcall;
  IE_GdipCreatePath : function(brushMode: ULONG; out path: pointer): ULONG; stdcall;
  IE_GdipAddPathArc : function(path: pointer; x, y, width, height, startAngle, sweepAngle: Single): ULONG; stdcall;
  IE_GdipDrawPath : function(graphics: pointer; pen: pointer; path: pointer): ULONG; stdcall;
  IE_GdipFillPath : function(graphics: pointer; brush: pointer; path: pointer): ULONG; stdcall;
  IE_GdipDeletePath : function(path: pointer): ULONG; stdcall;
  IE_GdipAddPathLine : function(path: pointer; x1, y1, x2, y2: Single): ULONG; stdcall;
  IE_GdipSetPenLineJoin : function(pen: pointer; lineJoin: ULONG): ULONG; stdcall;
  IE_GdipSetWorldTransform : function(graphics: pointer; matrix: pointer): ULONG; stdcall;
  IE_GdipGetWorldTransform : function(graphics: pointer; matrix: pointer): ULONG; stdcall;
  IE_GdipResetWorldTransform : function(graphics: pointer): ULONG; stdcall;
  IE_GdipCreateMatrix : function(out matrix: pointer): ULONG; stdcall;
  IE_GdipCreateMatrix2 : function(m11: single; m12: single; m21: single; m22: single; dx: single; dy: single; out matrix: pointer): ULONG; stdcall;
  IE_GdipDeleteMatrix : function(matrix: pointer): ULONG; stdcall;
  IE_GdipGetDC : function(graphics: pointer; var hdc: HDC): ULONG; stdcall;
  IE_GdipReleaseDC : function(graphics: pointer; hdc: HDC): ULONG; stdcall;
  IE_GdipFlush : function(graphics: pointer; intention: ULONG): ULONG; stdcall;
  IE_GdipRotateWorldTransform : function(graphics: pointer; angle: Single; order: ULONG): ULONG; stdcall;
  IE_GdipTranslateWorldTransform : function(graphics: pointer; dx: Single; dy: Single; order: ULONG): ULONG; stdcall;
  IE_GdipScaleWorldTransform : function(graphics: pointer; sx: Single; sy: Single; order: ULONG): ULONG; stdcall;
  IE_GdipStartPathFigure : function(path: pointer): ULONG; stdcall;
  IE_GdipClosePathFigure : function(path: pointer): ULONG; stdcall;
  IE_GdipResetPath : function(path: pointer): ULONG; stdcall;
  IE_GdipSetCompositingMode : function(graphics: pointer; compositingMode: ULONG): ULONG; stdcall;
  IE_GdipSetCompositingQuality : function(graphics: pointer; compositingQuality: ULONG): ULONG; stdcall;
  IE_GdipCreateBitmapFromScan0: function(width: Integer; height: Integer; stride: Integer; format: ULONG; scan0: pointer; out bitmap: pointer): ULONG; stdcall;
  IE_GdipGetImageGraphicsContext : function(image: pointer; out graphics: pointer): ULONG; stdcall;
  IE_GdipDisposeImage : function(image: pointer): ULONG; stdcall;
  IE_GdipGetNearestColor : function(graphics: pointer; var argb: TRGBA): ULONG; stdcall;
  IE_GdipSetInterpolationMode : function(graphics: pointer; interpolationMode: ULONG): ULONG; stdcall;
  IE_GdipCreateHalftonePalette : function(): THandle; stdcall;
  IE_GdipCreateBitmapFromHBITMAP : function(hbm: HBITMAP; hpal: HPALETTE; out bitmap: pointer): ULONG; stdcall;


// Library handling

function IEGDIPAvailable: boolean;
begin
  result := IE_GDIPLUSHandle<>0;
end;

function IEGDIPEnabled: boolean;
begin
  result := IEGlobalSettings().UseGDIPlus and (IE_GDIPLUSHandle <> 0);
end;

procedure IEGDIPLoadLibrary;
begin
  {$ifdef IEUSEGDIPLUS}
  inc(gdiplusRefCount);
  if not IEGDIPAvailable and IEGlobalSettings().UseGDIPlus and (IE_GDIPlusHandle=0) then
  begin
    IE_GDIPLUSHandle := LoadLibrary('gdiplus.dll');
    if IE_GDIPLUSHandle<>0 then
    begin
      @IE_GdiplusStartup := GetProcAddress(IE_GDIPLUSHandle, 'GdiplusStartup');
      @IE_GdiplusShutdown := GetProcAddress(IE_GDIPLUSHandle, 'GdiplusShutdown');
      @IE_GdipCreateFromHDC := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateFromHDC');
      @IE_GdipCreateFromHDC2 := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateFromHDC2');
      @IE_GdipDeleteGraphics := GetProcAddress(IE_GDIPLUSHandle, 'GdipDeleteGraphics');
      @IE_GdipCreatePen1 := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreatePen1');
      @IE_GdipDeletePen := GetProcAddress(IE_GDIPLUSHandle, 'GdipDeletePen');
      @IE_GdipDrawLine := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawLine');
      @IE_GdipSetSmoothingMode := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetSmoothingMode');
      @IE_GdipSetPenWidth := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetPenWidth');
      @IE_GdipSetPenColor := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetPenColor');
      @IE_GdipSetPenDashStyle := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetPenDashStyle');
      @IE_GdipSetPenDashArray := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetPenDashArray');
      @IE_GdipDeleteBrush := GetProcAddress(IE_GDIPLUSHandle, 'GdipDeleteBrush');
      @IE_GdipCreateSolidFill := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateSolidFill');
      @IE_GdipFillRectangle := GetProcAddress(IE_GDIPLUSHandle, 'GdipFillRectangle');
      @IE_GdipDrawRectangle := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawRectangle');
      @IE_GdipCreateHatchBrush := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateHatchBrush');
      @IE_GdipDrawEllipse := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawEllipse');
      @IE_GdipFillEllipse := GetProcAddress(IE_GDIPLUSHandle, 'GdipFillEllipse');
      @IE_GdipDrawPolygonI := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawPolygonI');
      @IE_GdipFillPolygonI := GetProcAddress(IE_GDIPLUSHandle, 'GdipFillPolygonI');
      @IE_GdipDrawPie := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawPie');
      @IE_GdipFillPie := GetProcAddress(IE_GDIPLUSHandle, 'GdipFillPie');
      @IE_GdipDrawLinesI := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawLinesI');
      @IE_GdipDrawArc := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawArc');
      @IE_GdipCreatePath := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreatePath');
      @IE_GdipAddPathArc := GetProcAddress(IE_GDIPLUSHandle, 'GdipAddPathArc');
      @IE_GdipDrawPath := GetProcAddress(IE_GDIPLUSHandle, 'GdipDrawPath');
      @IE_GdipFillPath := GetProcAddress(IE_GDIPLUSHandle, 'GdipFillPath');
      @IE_GdipDeletePath := GetProcAddress(IE_GDIPLUSHandle, 'GdipDeletePath');
      @IE_GdipAddPathLine := GetProcAddress(IE_GDIPLUSHandle, 'GdipAddPathLine');
      @IE_GdipSetPenLineJoin := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetPenLineJoin');
      @IE_GdipSetWorldTransform := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetWorldTransform');
      @IE_GdipGetWorldTransform := GetProcAddress(IE_GDIPLUSHandle, 'GdipGetWorldTransform');
      @IE_GdipResetWorldTransform := GetProcAddress(IE_GDIPLUSHandle, 'GdipResetWorldTransform');
      @IE_GdipCreateMatrix := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateMatrix');
      @IE_GdipCreateMatrix2 := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateMatrix2');
      @IE_GdipDeleteMatrix := GetProcAddress(IE_GDIPLUSHandle, 'GdipDeleteMatrix');
      @IE_GdipGetDC := GetProcAddress(IE_GDIPLUSHandle, 'GdipGetDC');
      @IE_GdipReleaseDC := GetProcAddress(IE_GDIPLUSHandle, 'GdipReleaseDC');
      @IE_GdipFlush := GetProcAddress(IE_GDIPLUSHandle, 'GdipFlush');
      @IE_GdipRotateWorldTransform := GetProcAddress(IE_GDIPLUSHandle, 'GdipRotateWorldTransform');
      @IE_GdipTranslateWorldTransform := GetProcAddress(IE_GDIPLUSHandle, 'GdipTranslateWorldTransform');
      @IE_GdipScaleWorldTransform := GetProcAddress(IE_GDIPLUSHandle, 'GdipScaleWorldTransform');
      @IE_GdipStartPathFigure := GetProcAddress(IE_GDIPLUSHandle, 'GdipStartPathFigure');
      @IE_GdipClosePathFigure := GetProcAddress(IE_GDIPLUSHandle, 'GdipClosePathFigure');
      @IE_GdipResetPath := GetProcAddress(IE_GDIPLUSHandle, 'GdipResetPath');
      @IE_GdipSetCompositingMode := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetCompositingMode');
      @IE_GdipSetCompositingQuality := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetCompositingQuality');
      @IE_GdipCreateBitmapFromScan0 := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateBitmapFromScan0');
      @IE_GdipGetImageGraphicsContext := GetProcAddress(IE_GDIPLUSHandle, 'GdipGetImageGraphicsContext');
      @IE_GdipDisposeImage := GetProcAddress(IE_GDIPLUSHandle, 'GdipDisposeImage');
      @IE_GdipGetNearestColor := GetProcAddress(IE_GDIPLUSHandle, 'GdipGetNearestColor');
      @IE_GdipSetInterpolationMode := GetProcAddress(IE_GDIPLUSHandle, 'GdipSetInterpolationMode');
      @IE_GdipCreateHalftonePalette := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateHalftonePalette');
      @IE_GdipCreateBitmapFromHBITMAP := GetProcAddress(IE_GDIPLUSHandle, 'GdipCreateBitmapFromHBITMAP');

      StartupInput.DebugEventCallback := nil;
      StartupInput.SuppressBackgroundThread := False;
      StartupInput.SuppressExternalCodecs   := False;
      StartupInput.GdiplusVersion := 1;
      if IE_GdiplusStartup(gdiplusToken, @StartupInput, nil) <> 0 then
        IEGDIPUnLoadLibrary();
    end;
  end;
  {$endif}
end;

procedure IEGDIPUnLoadLibrary();
begin
  {$ifdef IEUSEGDIPLUS}
  dec(gdiplusRefCount);
  if IEGDIPAvailable and (gdiplusRefCount=0) then
  begin
    IE_GdiplusShutdown(gdiplusToken);
    FreeLibrary(IE_GDIPLUSHandle);
    IE_GDIPLUSHandle := 0;
  end;
  {$endif}
end;


////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
// TIECanvas

constructor TIECanvas.Create(Canvas: TCanvas; AntiAlias: boolean; UseGDIPlus: boolean; Bitmap: TBitmap);
begin
  inherited Create;
  FOnDestroy := nil;
  FROIBitmap := nil;
  FGraphics := nil;
  FImage := nil;
  FCanvas := Canvas;
  FPen := TIEPen.Create(FCanvas.Pen);
  FBrush := TIEBrush.Create(FCanvas.Brush);
  FUseGDIPlus := IEGDIPEnabled and UseGDIPlus;
  if FUseGDIPlus then
  begin
    if Bitmap = nil then
      IE_GdipCreateFromHDC(FCanvas.Handle, FGraphics)
    else
      IE_GdipCreateFromHDC2(FCanvas.Handle, Bitmap.Handle, FGraphics);
    if FGraphics = nil then
    begin
      FreeAndNil(FPen);
      FreeAndNil(FBrush);
      raise Exception.Create('Cannot create TIECanvas. GdipCreateFromHDC failed.');
    end;
  end;
  if Antialias then
    SmoothingMode := iesmAntialias
  else
    SmoothingMode := iesmBestPerformance;
end;

destructor TIECanvas.Destroy;
begin
  if assigned(FOnDestroy) then
    FOnDestroy(self);
  FBrush.Free;
  FPen.Free;
  if FUseGDIPlus then
  begin
    if FImage <> nil then
      IE_GdipDisposeImage(FImage);
    IE_GdipDeleteGraphics(FGraphics);
  end;
  inherited;
end;

function TIECanvas.GetHandle: HDC;
begin
  result := FCanvas.Handle;
end;

procedure TIECanvas.SetSmoothingMode(value: TIECanvasSmoothingMode);
begin
  if FUseGDIPlus then
    IE_GdipSetSmoothingMode(FGraphics, integer(value)-1);
end;

procedure TIECanvas.SetPenPos(value: TPoint);
begin
  FPenPos := value;
  if not FUseGDIPlus then
    FCanvas.PenPos := value;
end;

procedure TIECanvas.MoveTo(X, Y: integer);
begin
  FPenPos := Point(X, Y);
  if not FUseGDIPlus then
    FCanvas.MoveTo(X, Y);
end;

procedure TIECanvas.LineTo(X, Y: integer);
begin
  if FUseGDIPlus then
  begin
    IE_GdipDrawLine(FGraphics, FPen.FGHandle, FPenPos.X, FPenPos.Y, X, Y);
    SetPenPos( Point(X, Y) );
  end
  else
    FCanvas.LineTo(X, Y);
end;

procedure TIECanvas.DrawLine(X1, Y1, X2, Y2: integer);
var
  p2: array [0..1] of TPoint;
begin
  if FUseGDIPlus then
  begin
    IE_GdipDrawLine(FGraphics, FPen.FGHandle, X1, Y1, X2, Y2);
  end
  else
  begin
    // draws also the last point
    p2[0].x := x1;
    p2[0].y := y1;
    p2[1].x := x2;
    p2[1].y := y2;
    FCanvas.Polygon(p2);
  end;
end;

procedure TIECanvas.DrawLinesPath(points: TIE2DPointArray);
var
  path: pointer;
  i: integer;
begin
  IE_GdipCreatePath(0, Path);
  try
    for i := 0 to high(points) - 1 do
    begin
      IE_GdipAddPathLine(Path, points[i].X, points[i].Y, points[i+1].X, points[i+1].Y);
    end;
    IE_GdipDrawPath(FGraphics, FPen.FGHandle, Path);
  finally
    IE_GdipDeletePath(Path);
  end;
end;

procedure TIECanvas.FillRect( const Rect: TRect );
var
  x1, y1, x2, y2: integer;
begin
  if FUseGDIPlus then
  begin
    x1 := Rect.Left;
    y1 := Rect.Top;
    x2 := Rect.Right;
    y2 := Rect.Bottom;
    OrdCor( x1, y1, x2, y2 );
    IE_GdipFillRectangle( FGraphics, FBrush.FGHandle, x1, y1, x2 - x1 - 1, y2 - y1 - 1 );
  end
  else
    FCanvas.FillRect( Rect );
end;

procedure TIECanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  Rectangle(Rect(X1, Y1, X2, Y2));
end;

procedure TIECanvas.Rectangle(const Rect: TRect);
var
  x1, y1, x2, y2: integer;
begin
  if FUseGDIPlus then
  begin
    x1 := Rect.Left;
    y1 := Rect.Top;
    x2 := Rect.Right;
    y2 := Rect.Bottom;
    OrdCor(x1, y1, x2, y2);
    IE_GdipFillRectangle(FGraphics, FBrush.FGHandle, x1, y1, x2 - x1 - 1, y2 - y1 - 1);
    IE_GdipDrawRectangle(FGraphics, FPen.FGHandle, x1, y1, x2 - x1 - 1, y2 - y1 - 1);
  end
  else
    FCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TIECanvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  Ellipse(Rect(X1, Y1, X2, Y2));
end;

procedure TIECanvas.Ellipse( const Rect: TRect );
var
x1, y1, x2, y2: integer;
hp: integer;
begin
  if FUseGDIPlus then
  begin
    x1 := Rect.Left;
    y1 := Rect.Top;
    x2 := Rect.Right;
    y2 := Rect.Bottom;

    // this code avoids width and height of ellipse to become less than half of pen width
    if FPen.Width<>1 then
    begin
      hp := round( FPen.Width / 2 );

      if ( x2 < x1 ) then
        x1 := x1 + hp;
      if ( ( x1 <= x2 ) and ( x2 - x1 - 1 <= hp ) ) then
        x2 := x1 + hp;
      if ( ( x2 < x1 ) and ( x1 - x2 - 1 <= hp ) ) then
        x2 := x1 - hp;

      if ( y2 < y1 ) then
        y1 := y1 + hp;
      if ( ( y1 <= y2 ) and ( y2 - y1 - 1 <= hp ) ) then
        y2 := y1 + hp;
      if ( ( y2 < y1 ) and ( y1 - y2 - 1 <= hp ) ) then
        y2 := y1 - hp;

      if ( x2 - hp ) = x1 then
        x1 := x1 - hp;

      if ( y2 - hp ) = y1 then
        y1 := y1 - hp;

      if x2 = x1 then
      begin
        x1 := x1 - ( hp + 1 );
        y2 := y2 - ( hp + 1 );
      end;

      if y2 = y1 then
      begin
        y1 := y1 - ( hp + 1 );
        y2 := y2 - ( hp + 1 );
      end;

      if x2 < x1 then
      begin
        x1 := x1 - ( hp );
        x2 := x2 - ( hp - 1 );
      end;
    end;

    IE_GdipFillEllipse( FGraphics, FBrush.FGHandle, x1, y1, x2 - x1 - 1, y2 - y1 - 1 );
    IE_GdipDrawEllipse( FGraphics, FPen.FGHandle, x1, y1, x2 - x1 - 1, y2 - y1 - 1 );
  end
  else
    FCanvas.Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TIECanvas.Polygon(Points: array of TPoint);
begin
  if FUseGDIPlus then
  begin
    IE_GdipDrawPolygonI(FGraphics, FPen.FGHandle, @Points[0], length(Points));
    IE_GdipFillPolygonI(FGraphics, FBrush.FGHandle, @Points[0], length(Points), 0);
  end
  else
    FCanvas.Polygon(Points);
end;

procedure TIECanvas.Pie(X: single; Y: single; Width: single; Height: single; StartAngle: single; SweepAngle: single);
begin
  if FUseGDIPlus then
  begin
    IE_GdipDrawPie(FGraphics, FPen.FGHandle, X, Y, Width, Height, StartAngle, SweepAngle);
    IE_GdipFillPie(FGraphics, FBrush.FGHandle, X, Y, Width, Height, StartAngle, SweepAngle);
  end;
end;

procedure TIECanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
var
  a1, a2: single;
  cx, cy: integer;
begin
  if FUseGDIPlus then
  begin
    cx := (X1 + X2) div 2;
    cy := (Y1 + Y2) div 2;
    a1 := IEAngle2(cx, cy, X4, Y4) * (180 / PI);
    a2 := IEAngle3(X3, Y3, cx, cy, X4, Y4) * (180 / PI);
    IE_GdipDrawPie(FGraphics, FPen.FGHandle, X1, Y1, X2 - X1 + 1, Y2 - Y1 + 1, -a1, a2);
    IE_GdipFillPie(FGraphics, FBrush.FGHandle, X1, Y1, X2 - X1 + 1, Y2 - Y1 + 1, -a1, a2);
  end
  else
    FCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;


procedure TIECanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
var
  a1, a2: single;
  cx, cy: integer;
begin
  if FUseGDIPlus then
  begin
    cx := (X1+X2) div 2;
    cy := (Y1+Y2) div 2;
    a1 := IEAngle2(cx, cy, X4, Y4) * (180/PI);
    a2 := IEAngle3(X3, Y3, cx, cy, X4, Y4) * (180/PI);
    IE_GdipDrawArc(FGraphics, FPen.FGHandle, X1, Y1, X2-X1+1, Y2-Y1+1, -a1, a2);
  end
  else
    FCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TIECanvas.Polyline(Points: array of TPoint);
begin
  if FUseGDIPlus then
    IE_GdipDrawLinesI(FGraphics, FPen.FGHandle, @Points[0], length(Points))
  else
    FCanvas.Polyline(Points);
end;

// todo...
function TIECanvas.TextWidth(const Text: WideString): Integer;
begin
  Result := TextExtent(Text).cX;
end;

// todo...
function TIECanvas.TextHeight(const Text: WideString): Integer;
begin
  Result := TextExtent(Text).cY;
end;

// todo...
procedure TIECanvas.TextOut(X, Y: Integer; const Text: string);
begin
  FCanvas.TextOut(X, Y, Text);
end;

// C++Builder 2009 converts "TextOut" to TextOutA, so we need this
procedure TIECanvas.TextOut2(X, Y: Integer; const Text: string);
begin
  TextOut(X, Y, Text);
end;


// todo...
function TIECanvas.GetFont: TFont;
begin
  result := FCanvas.Font;
end;

// todo...
procedure TIECanvas.TextRect(Rect: TRect; X, Y: Integer; const Text: WideString);
var
  Options: Longint;
begin
  //FCanvas.TextRect(Rect, X, Y, Text);
  Options := ETO_CLIPPED or FCanvas.TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((FCanvas.TextFlags and ETO_RTLREADING) <> 0) and (FCanvas.CanvasOrientation = coRightToLeft) then
    Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(FCanvas.Handle, X, Y, Options, @Rect, PWideChar(Text), Length(Text), nil);
end;

// like TextRect, but without ETO_OPAQUE in TextFlags
procedure TIECanvas.TextRectEx(Rect: TRect; X, Y: integer; const Text: WideString);
var
  Options: Longint;
begin
  Options := ETO_CLIPPED or FCanvas.TextFlags;
  if ((FCanvas.TextFlags and ETO_RTLREADING) <> 0) and (FCanvas.CanvasOrientation = coRightToLeft) then
    inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(FCanvas.Handle, X, Y, Options, @Rect, PWideChar(Text), Length(Text), nil);
end;

// todo...
function TIECanvas.TextExtent(const Text: WideString): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  GetTextExtentPoint32W(FCanvas.Handle, PWideChar(Text), Length(Text), Result);
end;


{
procedure TIECanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
var
  path: pointer;
  width, height: integer;
begin
  if FUseGDIPlus then
  begin
    OrdCor(X1, Y1, X2, Y2);
    width := X2 - X1;
    height := Y2 - Y1;
    if width < X3 then
      X3 := width;
    if height < Y3 then
      Y3 := height;
    IE_GdipCreatePath(0, path);

    IE_GdipAddPathArc(path, X1, Y1, X3, Y3, 180, 90);
    IE_GdipAddPathArc(path, X1 + width - X3, Y1, X3, Y3, 270, 90);
    IE_GdipAddPathArc(path, X1 + width - X3, Y1 + height - Y3, X3, Y3, 0, 90);
    IE_GdipAddPathArc(path, X1, Y1 + height - Y3, X3, Y3, 90, 90);
    IE_GdipAddPathLine(path, X1, Y1 + height - Y3, X1, Y1 + Y3 / 2);

    IE_GdipClosePathFigure(path);

    IE_GdipFillPath(FGraphics, FBrush.FGHandle, path);
    IE_GdipDrawPath(FGraphics, FPen.FGHandle, path);
    IE_GdipDeletePath(path);
  end
  else
    FCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3);
end;
//}

//{
procedure TIECanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
var
  path: pointer;
  width, height: integer;
  cwidth: integer;
  cheight: integer;
begin
  if FUseGDIPlus then
  begin

    OrdCor(X1, Y1, X2, Y2);

    width := X2 - X1;
    height := Y2 - Y1;

    if(X3 > width) then
      X3 := width;
    if(Y3 > height) then
      Y3 := height;

    cwidth  := X3;
    cheight := Y3;

    IE_GdipCreatePath(0, path);

    IE_GdipAddPathArc(path, x1, y1, cwidth, cheight, 180, 90);

    if X3 = 20 then
    begin
      inc(cwidth);
      dec(width);
    end;
    if Y3 = 20 then
    begin
      inc(cheight);
      dec(height);
    end;

    IE_GdipAddPathArc(path, x1 + width - X3 - 1, y1, cwidth, cheight, 270, 90);
    IE_GdipAddPathArc(path, x1 + width - X3 - 1, y1 + height - Y3 - 1, cwidth, cheight, 0, 90);
    IE_GdipAddPathArc(path, x1, y1 + height - Y3 - 1, cwidth, cheight, 90, 90);

    IE_GdipClosePathFigure(path);
    IE_GdipFillPath(FGraphics, FBrush.FGHandle, path);
    IE_GdipDrawPath(FGraphics, FPen.FGHandle, path);
    IE_GdipDeletePath(path);
  end
  else
    FCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3);
end;
//}

// angle in degrees
procedure TIECanvas.Rotate(Angle: double);
begin
  if FUseGDIPlus then
  begin
    IE_GdipRotateWorldTransform(FGraphics, Angle, 0);
  end;
end;

procedure TIECanvas.Translate(dx: double; dy: double);
begin
  if FUseGDIPlus then
  begin
    IE_GdipTranslateWorldTransform(FGraphics, dx, dy, 0);
  end;
  OffsetViewportOrgEx(FCanvas.Handle, trunc(dx), trunc(dy), nil);
end;

procedure TIECanvas.ResetTransform();
begin
  if FUseGDIPlus then
  begin
    IE_GdipResetWorldTransform(FGraphics);
  end;
end;


// end of TIECanvas
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
// TIEPen

constructor TIEPen.Create(Pen: TPen);
begin
  inherited Create;
  FPen := Pen;
  FGHandle := nil;
  FTransparency := 255;
  if assigned(Pen) then
  begin
    FColor := Pen.Color;
    FWidth := Pen.Width;
    FStyle := Pen.Style;
    FMode  := Pen.Mode;
  end
  else
  begin
    FColor := clWhite;
    FWidth := 1;
    FStyle := psSolid;
    FMode  := pmCopy;
  end;
  FLineJoin := ieljMiter;
  if IEGDIPEnabled then
    ReCreatePen;
end;

destructor TIEPen.Destroy;
begin
  if IEGDIPEnabled then
    IE_GdipDeletePen(FGHandle);
  inherited;
end;

function TIEPen.GetRGBAColor(): TRGBA;
begin
  if FStyle = psClear then
    result := TColor2TRGBA(FColor, 0)
  else
    result := TColor2TRGBA(FColor, FTransparency);
end;

procedure TIEPen.ReCreatePen;
begin
  if FGHandle<>nil then
    IE_GdipDeletePen(FGHandle);
  FGHandle := nil;
  IE_GdipCreatePen1(GetRGBAColor(), FWidth, 2, FGHandle);
  if FGHandle=nil then
    raise Exception.Create('Cannot create TIEPen. GdipCreatePen1 failed.');
  SetStyle( FStyle );
  SetLineJoin( FLineJoin );
end;

procedure TIEPen.SetTColor(value: TColor);
begin
  FColor := value;
  if IEGDIPEnabled then
    IE_GdipSetPenColor(FGHandle, GetRGBAColor());
  if assigned(FPen) then
    FPen.Color := value;
end;

procedure TIEPen.SetTransparency(value: integer);
begin
  FTransparency := value;
  if IEGDIPEnabled then
    IE_GdipSetPenColor(FGHandle, GetRGBAColor());
end;

procedure TIEPen.SetWidth(value: single);
begin
  FWidth := value;
  if IEGDIPEnabled then
    IE_GdipSetPenWidth(FGHandle, FWidth);
  if assigned(FPen) then
    FPen.Width := trunc(value);
end;

procedure TIEPen.SetStyle(value: TPenStyle);
begin
  FStyle := value;
  if IEGDIPEnabled then
    case FStyle of
      psClear: 
        begin
          IE_GdipSetPenColor(FGHandle, GetRGBAColor());
          IE_GdipSetPenDashStyle(FGHandle, 0);
        end;
      psInsideFrame: 
        IE_GdipSetPenDashStyle(FGHandle, 0);
      else
        IE_GdipSetPenDashStyle(FGHandle, integer(FStyle));
    end;
  if assigned(FPen) then
    FPen.Style := value;
end;

// unsupported in GDI+
procedure TIEPen.SetMode(value: TPenMode);
begin
  if assigned(FPen) then
    FPen.Mode := value;
end;

procedure TIEPen.SetLineJoin(value: TIECanvasPenLineJoin);
begin
  FLineJoin := value;
  if IEGDIPEnabled then
    IE_GdipSetPenLineJoin(FGHandle, ULONG(value));
end;

procedure TIEPen.SetVHandle(value: HPen);
begin
  if assigned(FPen) then
    FPen.Handle := value;
end;

function TIEPen.GetVHandle: HPen;
begin
  if assigned(FPen) then
    result := fPen.Handle
  else
    result := 0;
end;


// end of TIEPen
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
// TIEBrush

constructor TIEBrush.Create(Brush: TBrush);
begin
  inherited Create;
  FBrush := Brush;
  FGHandle := nil;
  if assigned(Brush) then
  begin
    FColor := Brush.Color;
    FStyle := Brush.Style;
  end
  else
  begin
    FColor := clWhite;
    FStyle := bsSolid;
  end;
  FBackColor := clBlack;
  FBackTransparency := 0;
  FTransparency := 255;
  if IEGDIPEnabled then
    ReCreateBrush;
end;

destructor TIEBrush.Destroy;
begin
  if IEGDIPEnabled then
    IE_GdipDeleteBrush(FGHandle);
  inherited;
end;

function TIEBrush.GetRGBAColor(): TRGBA;
begin
  if FStyle=bsClear then
    result := TColor2TRGBA(FColor, 0)
  else
    result := TColor2TRGBA(FColor, FTransparency);
end;

procedure TIEBrush.ReCreateBrush;
begin
  if FGHandle <> nil then
    IE_GdipDeleteBrush(FGHandle);
  FGHandle := nil;
  case FStyle of
    bsSolid, bsClear: 
      begin
        IE_GdipCreateSolidFill(GetRGBAColor(), FGHandle);
        if FGHandle = nil then
          raise Exception.Create('Cannot create TIEBrush. GdipCreateSolidFill failed.');
      end;
    bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross: 
      begin
        IE_GdipCreateHatchBrush(integer(FStyle)-2, GetRGBAColor(), TColor2TRGBA(FBackColor, FBackTransparency), FGHandle);
        if FGHandle = nil then
          raise Exception.Create('Cannot create TIEBrush. GdipCreateHatchBrush failed.');
      end;
  end;
end;

// not implemented in GDI+
function TIEBrush.GetBitmap: TBitmap;
begin
  if assigned(FBrush) then
    result := FBrush.Bitmap
  else
    result := nil;
end;

// not implemented in GDI+
procedure TIEBrush.SetBitmap(value: TBitmap);
begin
  if assigned(FBrush) then
    FBrush.Bitmap := value;
end;

procedure TIEBrush.SetTColor(value: TColor);
begin
  FColor := value;
  if IEGDIPEnabled then
    ReCreateBrush;
  if assigned(FBrush) then
    FBrush.Color := value;
end;

procedure TIEBrush.SetBackTColor(value: TColor);
begin
  FBackColor := value;
  if IEGDIPEnabled then
    ReCreateBrush;
end;

procedure TIEBrush.SetTransparency(value: integer);
begin
  FTransparency := value;
  if IEGDIPEnabled then
    ReCreateBrush;
end;

procedure TIEBrush.SetBackTransparency(value: integer);
begin
  FBackTransparency := value;
  if IEGDIPEnabled then
    ReCreateBrush;
end;

procedure TIEBrush.SetStyle(value: TBrushStyle);
begin
  FStyle := value;
  if IEGDIPEnabled then
    ReCreateBrush;
  if assigned(FBrush) then
    FBrush.Style := value;
end;


// end of TIEBrush
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

// note: don't call here IEGDIPLoadLibrary!! It Cannot be called in "initialization" sections.
procedure IEInitialize_iegdiplus;
begin
  IE_GDIPlusHandle := 0;
  gdiplusRefCount := 0;
end;

// note: don't call here IEGDIPUnLoadLibrary()!!
procedure IEFinalize_iegdiplus;
begin
end;


end.
