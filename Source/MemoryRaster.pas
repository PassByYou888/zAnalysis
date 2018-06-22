{ ****************************************************************************** }
{ * memory Rasterization                                                       * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit MemoryRaster;

{$I zDefine.inc}

interface

uses Types, Math, Variants, CoreClasses, MemoryStream64, Geometry2DUnit,
  PascalStrings, UnicodeMixedLib,
{$IFDEF FPC}
  UPascalStrings,
{$ENDIF FPC}
  zExpression, OpCode,
  ListEngine,
  AggBasics, Agg2D, AggColor32,
  JLSCodec;

{$REGION 'Type'}


type
  TRasterColor = TAggPackedRgba8;
  PRasterColor = ^TRasterColor;

  TRasterColorArray = packed array [0 .. MaxInt div SizeOf(TRasterColor) - 1] of TRasterColor;
  PRasterColorArray = ^TRasterColorArray;

  TRasterColorEntry = packed record
    case byte of
      0: (B, G, R, A: byte);
      1: (RGBA: TRasterColor);
      2: (Bytes: array [0 .. 3] of byte);
  end;

  PRasterColorEntry = ^TRasterColorEntry;

  TRasterColorEntryArray = array [0 .. MaxInt div SizeOf(TRasterColorEntry) - 1] of TRasterColorEntry;
  PRasterColorEntryArray = ^TRasterColorEntryArray;

  TArrayOfRasterColorEntry = array of TRasterColorEntry;

  TDrawMode    = (dmOpaque, dmBlend, dmTransparent);
  TCombineMode = (cmBlend, cmMerge);

  TByteRaster = array of array of byte;
  PByteRaster = ^TByteRaster;

  TMemoryRaster_AggImage = class;
  TMemoryRaster_Agg2D    = class;
  TFontRaster            = class;

  TMemoryRaster = class(TCoreClassInterfacedObject)
  private
    FFreeBits: Boolean;
    FBits: PRasterColorArray;
    FWidth, FHeight: Integer;
    FDrawMode: TDrawMode;
    FCombineMode: TCombineMode;

    FFont: TFontRaster;

    FAggNeed: Boolean;
    FAggImage: TMemoryRaster_AggImage;
    FAgg: TMemoryRaster_Agg2D;

    FMasterAlpha: Cardinal;
    FOuterColor: TRasterColor;

    FUserObject: TCoreClassObject;
    FUserData: Pointer;

    function GetFont: TFontRaster;
    procedure SetFont(f: TFontRaster); overload;

    function GetAggImage: TMemoryRaster_AggImage;
    function GetAgg: TMemoryRaster_Agg2D;
    procedure FreeAgg;

    function GetPixel(const X, Y: Integer): TRasterColor;
    procedure SetPixel(const X, Y: Integer; const Value: TRasterColor);

    function GetPixelBGRA(const X, Y: Integer): TRasterColor;
    procedure SetPixelBGRA(const X, Y: Integer; const Value: TRasterColor);

    function GetPixelPtr(const X, Y: Integer): PRasterColor;

    function GetScanLine(Y: Integer): PRasterColorArray;

    function GetPixelRed(const X, Y: Integer): byte;
    procedure SetPixelRed(const X, Y: Integer; const Value: byte);

    function GetPixelGreen(const X, Y: Integer): byte;
    procedure SetPixelGreen(const X, Y: Integer; const Value: byte);

    function GetPixelBlue(const X, Y: Integer): byte;
    procedure SetPixelBlue(const X, Y: Integer; const Value: byte);

    function GetPixelAlpha(const X, Y: Integer): byte;
    procedure SetPixelAlpha(const X, Y: Integer; const Value: byte);

    function GetGray(const X, Y: Integer): byte;
    procedure SetGray(const X, Y: Integer; const Value: byte);

    function GetGrayS(const X, Y: Integer): TGeoFloat;
    procedure SetGrayS(const X, Y: Integer; const Value: TGeoFloat);

    function GetGrayD(const X, Y: Integer): Double;
    procedure SetGrayD(const X, Y: Integer; const Value: Double);

    function GetPixelF(const X, Y: TGeoFloat): TRasterColor;
    procedure SetPixelF(const X, Y: TGeoFloat; const Value: TRasterColor);

    function GetPixelVec(const v2: TVec2): TRasterColor;
    procedure SetPixelVec(const v2: TVec2; const Value: TRasterColor);

    function GetPixelWrapLinear(const X, Y: TGeoFloat): TRasterColor;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { memory map }
    procedure SetWorkMemory(WorkMemory: Pointer; NewWidth, NewHeight: Integer); overload;
    procedure SetWorkMemory(Raster:TMemoryRaster); overload;

    { font raster support }
    procedure OpenFont;
    procedure CloseFont;
    property Font: TFontRaster read GetFont write SetFont;

    { Advanced rasterization }
    property AggImage: TMemoryRaster_AggImage read GetAggImage;
    property Agg: TMemoryRaster_Agg2D read GetAgg;
    procedure OpenAgg;
    procedure CloseAgg;

    { general }
    procedure Clear; overload;
    procedure Clear(FillColor: TRasterColor); overload; virtual;
    procedure SetSize(NewWidth, NewHeight: Integer); overload; virtual;
    procedure SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor); overload; virtual;
    function SizeOfPoint: TPoint;
    function SizeOf2DPoint: T2DPoint;
    function Size2D: TVec2;
    function Empty: Boolean;
    function BoundsRect: TRect;
    function BoundsRectV2: TRectV2;

    { operation }
    procedure Reset; virtual;
    procedure Assign(sour: TMemoryRaster); virtual;
    procedure FlipHorz;
    procedure FlipVert;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;
    procedure NoLineZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
    procedure NoLineZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure NoLineZoom(const NewWidth, NewHeight: Integer);
    procedure ZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
    procedure ZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure Zoom(const NewWidth, NewHeight: Integer);
    procedure FastBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure FastBlurZoom(const NewWidth, NewHeight: Integer);
    procedure GaussianBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure GaussianBlurZoom(const NewWidth, NewHeight: Integer);
    procedure GrayscaleBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure GrayscaleBlurZoom(const NewWidth, NewHeight: Integer);
    function FormatAsBGRA: TMemoryRaster;
    procedure FormatBGRA;
    procedure ColorTransparent(C: TRasterColor);
    procedure ColorBlend(C: TRasterColor);
    procedure Grayscale;
    procedure TransformToGrayRaster(var Output: TByteRaster);
    procedure TransformToRedRaster(var Output: TByteRaster);
    procedure TransformToGreenRaster(var Output: TByteRaster);
    procedure TransformToBlueRaster(var Output: TByteRaster);
    procedure TransformToAlphaRaster(var Output: TByteRaster);

    { shape support }
    procedure Line(X1, Y1, X2, Y2: Integer; Value: TRasterColor; L: Boolean); virtual;
    procedure LineF(X1, Y1, X2, Y2: TGeoFloat; Value: TRasterColor; L: Boolean); overload;
    procedure LineF(p1, p2: T2DPoint; Value: TRasterColor; L: Boolean); overload;
    procedure LineF(p1, p2: T2DPoint; Value: TRasterColor; L, Cross: Boolean); overload;
    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TRasterColor); overload;
    procedure FillRect(DstX, DstY, LineDist: Integer; Value: TRasterColor); overload;
    procedure FillRect(Dst: TVec2; LineDist: Integer; Value: TRasterColor); overload;
    procedure FillRect(R: TRectV2; Value: TRasterColor); overload;
    procedure FillRect(R: TRectV2; Angle:TGeoFloat; Value: TRasterColor); overload;
    procedure DrawRect(R: TRect; Value: TRasterColor); overload;
    procedure DrawRect(R: TRectV2; Value: TRasterColor); overload;
    procedure DrawRect(R: TV2Rect4; Value: TRasterColor); overload;
    procedure DrawRect(R: TRectV2; Angle:TGeoFloat; Value: TRasterColor); overload;
    procedure DrawTriangle_Render(t: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
    procedure DrawTriangle_Sampler(t: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
    procedure DrawCross(DstX, DstY, LineDist: Integer; Value: TRasterColor); overload;
    procedure DrawCrossF(DstX, DstY, LineDist: TGeoFloat; Value: TRasterColor); overload;
    procedure DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Value: TRasterColor); overload;
    procedure DrawPointListLine(pl: TVec2List; Value: TRasterColor; wasClose: Boolean);
    procedure DrawCircle(cc: TVec2; R: TGeoFloat; Value: TRasterColor);
    procedure FillCircle(cc: TVec2; R: TGeoFloat; Value: TRasterColor);
    { text support }
    function TextSize(Text: SystemString; Siz: TGeoFloat): TVec2;
    procedure DrawText(Text: SystemString; X, Y: Integer; RotateVec: TVec2; Angle, Alpha, Siz: TGeoFloat; TextColor: TRasterColor); overload;
    procedure DrawText(Text: SystemString; X, Y: Integer; Siz: TGeoFloat; TextColor: TRasterColor); overload;

    { hardware pipe simulate : openGL:GLTexture Sampler }
    procedure ProjectionTo2DMap(Dst: TMemoryRaster; const SourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const Alpha: TGeoFloat);
    { triangle projection }
    procedure ProjectionColor(const DestRect: TV2Rect4; const color:TRasterColor);

    { blend draw }
    procedure Draw(Src: TMemoryRaster); overload;
    procedure Draw(DstX, DstY: Integer; Src: TMemoryRaster); overload;
    procedure Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstPt: TVec2); overload;

    { file format }
    class function CanLoadStream(Stream: TCoreClassStream): Boolean; virtual;
    procedure LoadFromBmpStream(Stream: TCoreClassStream);
    procedure LoadFromStream(Stream: TCoreClassStream); virtual;
    procedure LoadFromStreamAndResize(Stream: TCoreClassStream; const NewWidth, NewHeight: Integer);

    procedure SaveToBmpStream(Stream: TCoreClassStream);             // published format
    procedure SaveToStream(Stream: TCoreClassStream); virtual;       // published format
    procedure SaveToZLibCompressStream(Stream: TCoreClassStream);    // no published format
    procedure SaveToDeflateCompressStream(Stream: TCoreClassStream); // no published format
    procedure SaveToBRRCCompressStream(Stream: TCoreClassStream);    // no published format
    procedure SaveToJpegLS1Stream(Stream: TCoreClassStream);         // published format
    procedure SaveToJpegLS3Stream(Stream: TCoreClassStream);         // published format
    procedure SaveToJpegAlphaStream(Stream: TCoreClassStream);       // no published format

    class function CanLoadFile(fn: SystemString): Boolean;
    procedure LoadFromFile(fn: SystemString); virtual;
    procedure LoadFromFileAndResize(fn: SystemString; const NewWidth, NewHeight: Integer);

    { save bitmap format file }
    procedure SaveToFile(fn: SystemString);                // published format
    procedure SaveToZLibCompressFile(fn: SystemString);    // no published format
    procedure SaveToDeflateCompressFile(fn: SystemString); // no published format
    procedure SaveToBRRCCompressFile(fn: SystemString);    // no published format
    procedure SaveToJpegLS1File(fn: SystemString);         // published format
    procedure SaveToJpegLS3File(fn: SystemString);         // published format
    procedure SaveToJpegAlphaFile(fn: SystemString);       // no published format

    { raster }
    property Pixel[const X, Y: Integer]: TRasterColor read GetPixel write SetPixel; default;
    property PixelBGRA[const X, Y: Integer]: TRasterColor read GetPixelBGRA write SetPixelBGRA;
    property PixelPtr[const X, Y: Integer]: PRasterColor read GetPixelPtr;
    property PixelRed[const X, Y: Integer]: byte read GetPixelRed write SetPixelRed;
    property PixelGreen[const X, Y: Integer]: byte read GetPixelGreen write SetPixelGreen;
    property PixelBlue[const X, Y: Integer]: byte read GetPixelBlue write SetPixelBlue;
    property PixelAlpha[const X, Y: Integer]: byte read GetPixelAlpha write SetPixelAlpha;
    property PixelGray[const X, Y: Integer]: byte read GetGray write SetGray;
    property PixelGrayS[const X, Y: Integer]: TGeoFloat read GetGrayS write SetGrayS;
    property PixelGrayD[const X, Y: Integer]: Double read GetGrayD write SetGrayD;
    property PixelF[const X, Y: TGeoFloat]: TRasterColor read GetPixelF write SetPixelF;
    property PixelVec[const v2: TVec2]: TRasterColor read GetPixelVec write SetPixelVec;
    property PixelWrapLinear[const X, Y: TGeoFloat]: TRasterColor read GetPixelWrapLinear;
    property ScanLine[Y: Integer]: PRasterColorArray read GetScanLine;
    property Bits: PRasterColorArray read FBits;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    { blend options }
    property DrawMode: TDrawMode read FDrawMode write FDrawMode default dmOpaque;
    property CombineMode: TCombineMode read FCombineMode write FCombineMode default cmBlend;
    property MasterAlpha: Cardinal read FMasterAlpha write FMasterAlpha;
    property OuterColor: TRasterColor read FOuterColor write FOuterColor;

    { user define }
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
    property UserData: Pointer read FUserData write FUserData;
  end;

  TMemoryRasterClass = class of TMemoryRaster;

  TSequenceMemoryRaster = class(TMemoryRaster)
  protected
    FTotal: Integer;
    FColumn: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear(FillColor: TRasterColor); override;
    procedure SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor); override;

    procedure Reset; override;
    procedure Assign(sour: TMemoryRaster); override;

    class function CanLoadStream(Stream: TCoreClassStream): Boolean; override;
    procedure LoadFromStream(Stream: TCoreClassStream); override;
    procedure SaveToStream(Stream: TCoreClassStream); override;

    property Total: Integer read FTotal write FTotal;
    property Column: Integer read FColumn write FColumn;

    function SequenceFrameRect(index: Integer): TRect;
    procedure ExportSequenceFrame(index: Integer; Output: TMemoryRaster);
    procedure ReverseSequence(Output: TSequenceMemoryRaster);
    procedure GradientSequence(Output: TSequenceMemoryRaster);
    function FrameWidth: Integer;
    function FrameHeight: Integer;
    function FrameRect2D: TRectV2;
    function FrameRect: TRect;
  end;

  TSequenceMemoryRasterClass = class of TSequenceMemoryRaster;

  TMemoryRaster_AggImage = class(TAgg2DImage)
  public
    constructor Create(Raster: TMemoryRaster); overload;
    procedure Attach(Raster: TMemoryRaster); overload;
  end;

  TMemoryRaster_Agg2D = class(TAgg2D)
  private
    function GetImageBlendColor: TRasterColor;
    procedure SetImageBlendColor(const Value: TRasterColor);
    function GetFillColor: TRasterColor;
    procedure SetFillColor(const Value: TRasterColor);
    function GetLineColor: TRasterColor;
    procedure SetLineColor(const Value: TRasterColor);
  public
    procedure Attach(Raster: TMemoryRaster); overload;

    procedure FillLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TRasterColor; Profile: Double = 1);
    procedure LineLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TRasterColor; Profile: Double = 1);

    procedure FillRadialGradient(X, Y, R: Double; C1, C2: TRasterColor; Profile: Double = 1); overload;
    procedure LineRadialGradient(X, Y, R: Double; C1, C2: TRasterColor; Profile: Double = 1); overload;

    procedure FillRadialGradient(X, Y, R: Double; C1, C2, C3: TRasterColor); overload;
    procedure LineRadialGradient(X, Y, R: Double; C1, C2, C3: TRasterColor); overload;

    property ImageBlendColor: TRasterColor read GetImageBlendColor write SetImageBlendColor;
    property FillColor: TRasterColor read GetFillColor write SetFillColor;
    property LineColor: TRasterColor read GetLineColor write SetLineColor;
  end;

  PVertexMap = ^TVertexMap;

  TVertexMap = packed record
  private type
    { Setup interpolation constants for linearly varying vaues }
    TBilerpConsts = packed record
      A, B, C: TGeoFloat;
    end;

    { fragment mode }
    TFragSampling = (fsSolid, fsNearest, fsLinear);

    TSamplerBlend        = procedure(const Sender: PVertexMap; const f, m: TRasterColor; var B: TRasterColor);
    TComputeSamplerColor = function(const Sender: PVertexMap; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRasterColor;
  private
    procedure RasterizeTriangle(const ft: TFragSampling; const sc: TRasterColor; const Tex: TMemoryRaster; const t: TTriangle);

    procedure FillFragment(const ft: TFragSampling; const sc: TRasterColor; const Tex: TMemoryRaster;
      const bitDst, j, start_x, frag_count: Integer; const attr_v, attr_u: TBilerpConsts); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    Window: TMemoryRaster;
    WindowSize: Integer;
    ComputeNearest: TComputeSamplerColor;
    ComputeLinear: TComputeSamplerColor;
    ComputeBlend: TSamplerBlend;
    Debug: Boolean;
    UserData: Pointer;

    procedure Init(const W: TMemoryRaster);

    procedure DrawTriangle(const triangle: TTriangle; const Sampler: TRasterColor); overload;
    procedure DrawTriangle(const triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
    procedure DrawTriangle(const triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: TGeoFloat); overload;

    (*
      SamVec: (TV2Rect4) sampler Absolute coordiantes
      RenVec: (TV2Rect4) renderer Absolute coordiantes
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const RenVec: TV2Rect4; const Sampler: TRasterColor); overload;
    procedure DrawRect(const SamVec, RenVec: TV2Rect4; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: TGeoFloat); overload;

    (*
      SamVec: (TRectV2) sampler Absolute coordiantes
      RenVec: (TRectV2) renderer Absolute coordiantes
      RenAngle: (TGeoFloat) renderer rotation
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const RenVec: TRectV2; const Sampler: TRasterColor); overload;
    procedure DrawRect(const SamVec, RenVec: TRectV2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: TGeoFloat); overload;
    procedure DrawRect(const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TRasterColor); overload;
    procedure DrawRect(const SamVec, RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: TGeoFloat); overload;

    (*
      SamVec: (TV2Rect4) sampler Absolute coordiantes
      RenVec: (TRectV2) renderer Absolute coordiantes
      RenAngle: (TGeoFloat) renderer rotation
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const SamVec: TV2Rect4; const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const Alpha: TGeoFloat); overload;

    (*
      SamVec: (TVec2List) sampler Absolute coordiantes
      RenVec: (TVec2List) renderer Absolute coordiantes
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawPoly(const RenVec: TVec2List; const Sampler: TRasterColor); overload;
    procedure DrawPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
  end;

  TFontRaster = class(TCoreClassObject)
  private type
    PFontCharDefine = ^TFontCharDefine;

    TFontCharDefine = packed record
      Activted: Boolean;
      X, Y: Word;
      W, H: byte;
    end;

    TFontTable = array [0 .. MaxInt div SizeOf(TFontCharDefine) - 1] of TFontCharDefine;
    PFontTable = ^TFontTable;

    TFontBitRaster = array [0 .. MaxInt - 1] of byte;
    PFontBitRaster = ^TFontBitRaster;

{$IFDEF FPC}
    TFontRasterString = TUPascalString;
    TFontRasterChar   = USystemChar;
{$ELSE FPC}
    TFontRasterString = TPascalString;
    TFontRasterChar   = SystemChar;
{$ENDIF FPC}

    TDrawWorkData = record
      Owner: TFontRaster;
      DestColor: TRasterColor;
    end;

    PDrawWorkData = ^TDrawWorkData;
  private const
    C_WordDefine: TFontCharDefine = (Activted: False; X: 0; Y: 0; W: 0; H: 0);
    C_MAXWORD                     = $FFFF;
  protected
    FOnlyInstance: Boolean;
    FFontTable: PFontTable;
    FFragRaster: array of TMemoryRaster;
    FBitRaster: PFontBitRaster;
    FFontSize: Integer;
    FActivtedWord: Integer;
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create; overload;
    constructor Create(ShareFont: TFontRaster); overload;
    destructor Destroy; override;

    // generate word
    procedure Add(C: TFontRasterChar; Raster: TMemoryRaster);
    procedure Remove(C: TFontRasterChar);
    procedure Clear;
    procedure Build(fontSiz: Integer);

    property FontSize: Integer read FFontSize;
    property ActivtedWord: Integer read FActivtedWord;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    // store
    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream);
    procedure ExportRaster(Stream: TCoreClassStream; partitionLine: Boolean);

    // draw font
    function CharSize(const C: TFontRasterChar): TPoint;
    function TextSize(const s: TFontRasterString; charVec2List: TVec2List): TVec2; overload;
    function TextSize(const s: TFontRasterString): TVec2; overload;
    function TextWidth(const s: TFontRasterString): Word;
    function TextHeight(const s: TFontRasterString): Word;

    function Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRasterColor;
      const bilinear_sampling: Boolean; const Alpha: TGeoFloat; const Axis: TVec2; const Angle, Scale: TGeoFloat): TVec2; overload;

    procedure Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRasterColor); overload;
  end;

{$ENDREGION 'Type'}

{$REGION 'RasterAPI'}

procedure FillRasterColor(var X; Count: Cardinal; Value: TRasterColor); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CopyRasterColor(const Source; var dest; Count: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure BlendBlock(Dst: TMemoryRaster; dstRect: TRect; Src: TMemoryRaster; SrcX, SrcY: Integer; CombineOp: TDrawMode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure BlockTransfer(Dst: TMemoryRaster; DstX: Integer; DstY: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);
function RandomRasterColor(const A: byte = $FF): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor(const R, G, B: byte; const A: byte = $FF): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColorInv(const C: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterAlphaColor(const C: TRasterColor; const A: byte): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterAlphaColorF(const C: TRasterColor; const A: Single): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RasterColorF(const R, G, B: TGeoFloat; const A: TGeoFloat = 1.0): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RasterColor2F(const C: TRasterColor; var R, G, B, A: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RasterColor2F(const C: TRasterColor; var R, G, B: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RasterColor2Gray(const C: TRasterColor): byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayS(const C: TRasterColor): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayD(const C: TRasterColor): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RGBA2BGRA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BGRA2RGBA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AggColor(const Value: TRasterColor): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function AggColor(const R, G, B: TGeoFloat; const A: TGeoFloat = 1.0): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function AggColor(const Value: TAggColorRgba8): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

procedure ComputeSize(const MAX_Width, MAX_Height: Integer; var Width, Height: Integer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure FastBlur(Source, dest: TMemoryRaster; Radius: Double; const Bounds: TRect); overload;
procedure FastBlur(Source: TMemoryRaster; Radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source, dest: TMemoryRaster; Radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source: TMemoryRaster; Radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source, dest: TMemoryRaster; Radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source: TMemoryRaster; Radius: Double; const Bounds: TRect); overload;

procedure Antialias32(const DestMR: TMemoryRaster; AXOrigin, AYOrigin, AXFinal, AYFinal: Integer); overload;
procedure Antialias32(const DestMR: TMemoryRaster; const AAmount: Integer); overload;
procedure HistogramEqualize(const mr: TMemoryRaster);
procedure RemoveRedEyes(const mr: TMemoryRaster);
procedure Sepia32(const mr: TMemoryRaster; const Depth: byte);
procedure Sharpen(const DestMR: TMemoryRaster; const SharpenMore: Boolean);

procedure AlphaToGrayscale(Src: TMemoryRaster);
procedure IntensityToAlpha(Src: TMemoryRaster);
procedure ReversalAlpha(Src: TMemoryRaster);
procedure RGBToGrayscale(Src: TMemoryRaster);

procedure ColorToTransparent(SrcColor: TRasterColor; Src, Dst: TMemoryRaster);

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
function GetSequenceFrameRect(Bmp: TMemoryRaster; Total, Column, index: Integer): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure GetSequenceFrameOutput(Bmp: TMemoryRaster; Total, Column, index: Integer; Output: TMemoryRaster); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function BlendReg(f, B: TRasterColor): TRasterColor; register;
procedure BlendMem(f: TRasterColor; var B: TRasterColor); register;
function BlendRegEx(f, B, m: TRasterColor): TRasterColor; register;
procedure BlendMemEx(f: TRasterColor; var B: TRasterColor; m: TRasterColor); register;
procedure BlendLine(Src, Dst: PRasterColor; Count: Integer); register;
procedure BlendLineEx(Src, Dst: PRasterColor; Count: Integer; m: TRasterColor); register;
function CombineReg(X, Y, W: TRasterColor): TRasterColor; register;
procedure CombineMem(X: TRasterColor; var Y: TRasterColor; W: TRasterColor); register;
procedure CombineLine(Src, Dst: PRasterColor; Count: Integer; W: TRasterColor); register;
function MergeReg(f, B: TRasterColor): TRasterColor; register;
function MergeRegEx(f, B, m: TRasterColor): TRasterColor; register;
procedure MergeMem(f: TRasterColor; var B: TRasterColor); register;
procedure MergeMemEx(f: TRasterColor; var B: TRasterColor; m: TRasterColor); register;
procedure MergeLine(Src, Dst: PRasterColor; Count: Integer); register;
procedure MergeLineEx(Src, Dst: PRasterColor; Count: Integer; m: TRasterColor); register;

{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  fixed by 600585@qq.com, v2.3
  2018-5
}
procedure jls_RasterToRaw3(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
procedure jls_RasterToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
procedure jls_GrayRasterToRaw1(const ARaster: PByteRaster; RawStream: TCoreClassStream);
procedure jls_RasterAlphaToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);

function EncodeJpegLSRasterAlphaToStream(ARaster: TMemoryRaster; const Stream: TCoreClassStream): Boolean;
function EncodeJpegLSRasterToStream3(ARaster: TMemoryRaster; const Stream: TCoreClassStream): Boolean;
function EncodeJpegLSRasterToStream1(ARaster: TMemoryRaster; const Stream: TCoreClassStream): Boolean; overload;

function DecodeJpegLSRasterFromStream(const Stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
function DecodeJpegLSRasterAlphaFromStream(const Stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;

function EncodeJpegLSGrayRasterToStream(const ARaster: PByteRaster; const Stream: TCoreClassStream): Boolean; overload;
function DecodeJpegLSGrayRasterFromStream(const Stream: TCoreClassStream; var ARaster: TByteRaster): Boolean;
{$ENDREGION 'RasterAPI'}


var
  NewRaster: function: TMemoryRaster;
  NewRasterFromFile: function(const fn: string): TMemoryRaster;
  NewRasterFromStream: function(const Stream: TCoreClassStream): TMemoryRaster;
  SaveRaster: procedure(mr: TMemoryRaster; const fn: string);

implementation

uses
{$IFDEF parallel}
{$IFDEF FPC}
  mtprocs,
{$ELSE}
  Threading,
{$ENDIF FPC}
{$ENDIF}
  CoreCompress, DoStatusIO;

{$I zDefine.inc}

{$REGION 'InternalDefines'}


var
  RcTable: array [byte, byte] of byte;
  DivTable: array [byte, byte] of byte;
  SystemFont: TFontRaster;

type
  TLUT8            = array [byte] of byte;
  TLogicalOperator = (loXOR, loAND, loOR);
  TByteArray       = array [0 .. MaxInt div SizeOf(byte) - 1] of byte;
  PByteArray       = ^TByteArray;

  TBmpHeader = packed record
    bfType: Word;
    bfSize: Integer;
    bfReserved: Integer;
    bfOffBits: Integer;
    biSize: Integer;
    biWidth: Integer;
    biHeight: Integer;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: Integer;
    biSizeImage: Integer;
    biXPelsPerMeter: Integer;
    biYPelsPerMeter: Integer;
    biClrUsed: Integer;
    biClrImportant: Integer;
  end;

  TBlendLine   = procedure(Src, Dst: PRasterColor; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PRasterColor; Count: Integer; m: TRasterColor);

const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
{$ENDREGION 'InternalDefines'}

{$I MemoryRaster_RasterClass.inc}
{$I MemoryRaster_SequenceClass.inc}
{$I MemoryRaster_Vertex.inc}
{$I MemoryRaster_Agg.inc}
{$I MemoryRaster_Font.inc}


function _NewRaster: TMemoryRaster;
begin
  result := TMemoryRaster.Create;
end;

function _NewRasterFromFile(const fn: string): TMemoryRaster;
begin
  result := NewRaster();
  result.LoadFromFile(fn);
end;

function _NewRasterFromStream(const Stream: TCoreClassStream): TMemoryRaster;
begin
  result := NewRaster();
  result.LoadFromStream(Stream);
end;

procedure _SaveRaster(mr: TMemoryRaster; const fn: string);
begin
  mr.SaveToFile(fn);
end;

{$I MemoryRaster_ExtApi.inc}


initialization

MakeMergeTables;

{$IFDEF FPC}
NewRaster := @_NewRaster;
NewRasterFromFile := @_NewRasterFromFile;
NewRasterFromStream := @_NewRasterFromStream;
SaveRaster := @_SaveRaster;
{$ELSE FPC}
NewRaster := _NewRaster;
NewRasterFromFile := _NewRasterFromFile;
NewRasterFromStream := _NewRasterFromStream;
SaveRaster := _SaveRaster;
{$ENDIF FPC}

Init_DefaultFont;

finalization

Free_DefaultFont;

end.
