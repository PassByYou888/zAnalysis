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
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit MemoryRaster;

{$INCLUDE zDefine.inc}

interface

uses Types, Math, Variants, CoreClasses, MemoryStream64, Geometry2DUnit, PascalStrings, UnicodeMixedLib,
{$IFDEF FPC}
  UPascalStrings,
{$ENDIF FPC}
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
    case Byte of
      0: (b, g, R, A: Byte);
      1: (RGBA: TRasterColor);
      2: (Bytes: array [0 .. 3] of Byte);
  end;

  PRasterColorEntry = ^TRasterColorEntry;

  TRasterColorEntryArray = array [0 .. MaxInt div SizeOf(TRasterColorEntry) - 1] of TRasterColorEntry;
  PRasterColorEntryArray = ^TRasterColorEntryArray;

  TArrayOfRasterColorEntry = array of TRasterColorEntry;

  TDrawMode    = (dmOpaque, dmBlend, dmTransparent);
  TCombineMode = (cmBlend, cmMerge);

  TByteRaster = array of array of Byte;
  PByteRaster = ^TByteRaster;

  TMemoryRaster_AggImage = class;
  TMemoryRaster_Agg2D    = class;
  TVertexMap             = class;
  TFontRaster            = class;

  TMemoryRaster = class(TCoreClassObject)
  private
    FFreeBits: Boolean;
    FBits: PRasterColorArray;
    FWidth, FHeight: Integer;
    FDrawMode: TDrawMode;
    FCombineMode: TCombineMode;

    FVertex: TVertexMap;
    FFont: TFontRaster;

    FAggNeed: Boolean;
    FAggImage: TMemoryRaster_AggImage;
    FAgg: TMemoryRaster_Agg2D;

    FMasterAlpha: Cardinal;
    FOuterColor: TRasterColor;

    FUserObject: TCoreClassObject;
    FUserData: Pointer;
    FUserText: SystemString;

    function GetVertex: TVertexMap;

    function GetFont: TFontRaster;
    procedure SetFont(F: TFontRaster); overload;

    function GetAggImage: TMemoryRaster_AggImage;
    function GetAgg: TMemoryRaster_Agg2D;
    procedure FreeAgg;

    function GetPixel(const X, Y: Integer): TRasterColor;
    procedure SetPixel(const X, Y: Integer; const Value: TRasterColor);

    function GetPixelBGRA(const X, Y: Integer): TRasterColor;
    procedure SetPixelBGRA(const X, Y: Integer; const Value: TRasterColor);

    function GetPixelPtr(const X, Y: Integer): PRasterColor;

    function GetScanLine(Y: Integer): PRasterColorArray;

    function GetPixelRed(const X, Y: Integer): Byte;
    procedure SetPixelRed(const X, Y: Integer; const Value: Byte);

    function GetPixelGreen(const X, Y: Integer): Byte;
    procedure SetPixelGreen(const X, Y: Integer; const Value: Byte);

    function GetPixelBlue(const X, Y: Integer): Byte;
    procedure SetPixelBlue(const X, Y: Integer; const Value: Byte);

    function GetPixelAlpha(const X, Y: Integer): Byte;
    procedure SetPixelAlpha(const X, Y: Integer; const Value: Byte);

    function GetGray(const X, Y: Integer): Byte;
    procedure SetGray(const X, Y: Integer; const Value: Byte);

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
    procedure SetWorkMemory(raster: TMemoryRaster); overload;

    { triangle vertex map }
    procedure OpenVertex;
    procedure CloseVertex;
    property Vertex: TVertexMap read GetVertex;

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
    function SizeOf2DPoint: TVec2;
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
    procedure TransformToGrayRaster(var output: TByteRaster);
    procedure TransformToRedRaster(var output: TByteRaster);
    procedure TransformToGreenRaster(var output: TByteRaster);
    procedure TransformToBlueRaster(var output: TByteRaster);
    procedure TransformToAlphaRaster(var output: TByteRaster);

    { shape support }
    procedure Line(x1, y1, x2, y2: Integer; Value: TRasterColor; L: Boolean); virtual;
    procedure LineF(x1, y1, x2, y2: TGeoFloat; Value: TRasterColor; L: Boolean); overload;
    procedure LineF(p1, p2: TVec2; Value: TRasterColor; L: Boolean); overload;
    procedure LineF(p1, p2: TVec2; Value: TRasterColor; L, Cross: Boolean); overload;
    procedure FillRect(x1, y1, x2, y2: Integer; Value: TRasterColor); overload;
    procedure FillRect(Dstx, Dsty, LineDist: Integer; Value: TRasterColor); overload;
    procedure FillRect(Dst: TVec2; LineDist: Integer; Value: TRasterColor); overload;
    procedure FillRect(R: TRectV2; Value: TRasterColor); overload;
    procedure FillRect(R: TRectV2; angle: TGeoFloat; Value: TRasterColor); overload;
    procedure DrawRect(R: TRect; Value: TRasterColor); overload;
    procedure DrawRect(R: TRectV2; Value: TRasterColor); overload;
    procedure DrawRect(R: TV2Rect4; Value: TRasterColor); overload;
    procedure DrawRect(R: TRectV2; angle: TGeoFloat; Value: TRasterColor); overload;
    procedure DrawTriangle_Render(T: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
    procedure DrawTriangle_Sampler(T: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
    procedure DrawCross(Dstx, Dsty, LineDist: Integer; Value: TRasterColor); overload;
    procedure DrawCrossF(Dstx, Dsty, LineDist: TGeoFloat; Value: TRasterColor); overload;
    procedure DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Value: TRasterColor); overload;
    procedure DrawPointListLine(pl: TVec2List; Value: TRasterColor; wasClose: Boolean);
    procedure DrawCircle(CC: TVec2; R: TGeoFloat; Value: TRasterColor);
    procedure FillCircle(CC: TVec2; R: TGeoFloat; Value: TRasterColor);
    procedure DrawEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Value: TRasterColor);
    procedure FillEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Value: TRasterColor);
    { text support }
    function TextSize(Text: SystemString; siz: TGeoFloat): TVec2;
    procedure DrawText(Text: SystemString; X, Y: Integer; RotateVec: TVec2; angle, alpha, siz: TGeoFloat; TextColor: TRasterColor); overload;
    procedure DrawText(Text: SystemString; X, Y: Integer; siz: TGeoFloat; TextColor: TRasterColor); overload;

    { hardware pipe simulate : openGL:GLTexture Sampler }
    procedure ProjectionTo2DMap(Dst: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
    { triangle projection }
    procedure ProjectionColor(const DestRect: TV2Rect4; const COLOR: TRasterColor);

    { blend draw }
    procedure Draw(Src: TMemoryRaster); overload;
    procedure Draw(Dstx, Dsty: Integer; Src: TMemoryRaster); overload;
    procedure Draw(Dstx, Dsty: Integer; const SrcRect: TRect; Src: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster; Dstx, Dsty: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TMemoryRaster; Dstx, Dsty: Integer); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstPt: TVec2); overload;

    { file format }
    class function CanLoadStream(stream: TCoreClassStream): Boolean; virtual;
    procedure LoadFromBmpStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream); virtual;
    procedure LoadFromStreamAndResize(stream: TCoreClassStream; const NewWidth, NewHeight: Integer);

    procedure SaveToBmpStream(stream: TCoreClassStream);             // published format
    procedure SaveToStream(stream: TCoreClassStream); virtual;       // published format
    procedure SaveToZLibCompressStream(stream: TCoreClassStream);    // no published format
    procedure SaveToDeflateCompressStream(stream: TCoreClassStream); // no published format
    procedure SaveToBRRCCompressStream(stream: TCoreClassStream);    // no published format
    procedure SaveToJpegLS1Stream(stream: TCoreClassStream);         // published format
    procedure SaveToJpegLS3Stream(stream: TCoreClassStream);         // published format
    procedure SaveToJpegAlphaStream(stream: TCoreClassStream);       // no published format

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
    property PixelRed[const X, Y: Integer]: Byte read GetPixelRed write SetPixelRed;
    property PixelGreen[const X, Y: Integer]: Byte read GetPixelGreen write SetPixelGreen;
    property PixelBlue[const X, Y: Integer]: Byte read GetPixelBlue write SetPixelBlue;
    property PixelAlpha[const X, Y: Integer]: Byte read GetPixelAlpha write SetPixelAlpha;
    property PixelGray[const X, Y: Integer]: Byte read GetGray write SetGray;
    property PixelGrayS[const X, Y: Integer]: TGeoFloat read GetGrayS write SetGrayS;
    property PixelGrayD[const X, Y: Integer]: Double read GetGrayD write SetGrayD;
    property PixelF[const X, Y: TGeoFloat]: TRasterColor read GetPixelF write SetPixelF;
    property PixelVec[const v2: TVec2]: TRasterColor read GetPixelVec write SetPixelVec;
    property PixelWrapLinear[const X, Y: TGeoFloat]: TRasterColor read GetPixelWrapLinear;
    property ScanLine[Y: Integer]: PRasterColorArray read GetScanLine;
    property Bits: PRasterColorArray read FBits;
    property width: Integer read FWidth;
    property height: Integer read FHeight;

    { blend options }
    property DrawMode: TDrawMode read FDrawMode write FDrawMode default dmOpaque;
    property CombineMode: TCombineMode read FCombineMode write FCombineMode default cmBlend;
    property MasterAlpha: Cardinal read FMasterAlpha write FMasterAlpha;
    property OuterColor: TRasterColor read FOuterColor write FOuterColor;

    { user define }
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
    property UserData: Pointer read FUserData write FUserData;
    property UserText: SystemString read FUserText write FUserText;
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

    class function CanLoadStream(stream: TCoreClassStream): Boolean; override;
    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;

    property Total: Integer read FTotal write FTotal;
    property Column: Integer read FColumn write FColumn;

    function SequenceFrameRect(index: Integer): TRect;
    procedure ExportSequenceFrame(index: Integer; output: TMemoryRaster);
    procedure ReverseSequence(output: TSequenceMemoryRaster);
    procedure GradientSequence(output: TSequenceMemoryRaster);
    function FrameWidth: Integer;
    function FrameHeight: Integer;
    function FrameRect2D: TRectV2;
    function FrameRect: TRect;
  end;

  TSequenceMemoryRasterClass = class of TSequenceMemoryRaster;

  TMemoryRaster_AggImage = class(TAgg2DImage)
  public
    constructor Create(raster: TMemoryRaster); overload;
    procedure Attach(raster: TMemoryRaster); overload;
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
    procedure Attach(raster: TMemoryRaster); overload;

    procedure FillLinearGradient(x1, y1, x2, y2: Double; c1, c2: TRasterColor; Profile: Double = 1);
    procedure LineLinearGradient(x1, y1, x2, y2: Double; c1, c2: TRasterColor; Profile: Double = 1);

    procedure FillRadialGradient(X, Y, R: Double; c1, c2: TRasterColor; Profile: Double = 1); overload;
    procedure LineRadialGradient(X, Y, R: Double; c1, c2: TRasterColor; Profile: Double = 1); overload;

    procedure FillRadialGradient(X, Y, R: Double; c1, c2, c3: TRasterColor); overload;
    procedure LineRadialGradient(X, Y, R: Double; c1, c2, c3: TRasterColor); overload;

    property ImageBlendColor: TRasterColor read GetImageBlendColor write SetImageBlendColor;
    property FillColor: TRasterColor read GetFillColor write SetFillColor;
    property LineColor: TRasterColor read GetLineColor write SetLineColor;
  end;

  PVertexMap = ^TVertexMap;

  TVertexMap = class(TCoreClassObject)
  private type
    { Setup interpolation constants for linearly varying vaues }
    TBilerpConsts = packed record
      A, b, C: TGeoFloat;
    end;

    { fragment mode }
    TFragSampling = (fsSolid, fsNearest, fsLinear);

    TNearestWriteBuffer = array of Byte;
    PNearestWriteBuffer = ^TNearestWriteBuffer;

    TSamplerBlend        = procedure(const Sender: PVertexMap; const F, M: TRasterColor; var b: TRasterColor);
    TComputeSamplerColor = function(const Sender: PVertexMap; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRasterColor;
  private
    // rasterization nearest templet
    FNearestWriteBuffer: TNearestWriteBuffer;
    FNearestWriterID: Byte;
    FCurrentUpdate: ShortInt;
    // sampler shader
    ComputeNearest: TComputeSamplerColor;
    ComputeLinear: TComputeSamplerColor;
    ComputeBlend: TSamplerBlend;

    procedure RasterizeTriangle(const FT: TFragSampling; const sc: TRasterColor; const tex: TMemoryRaster; const T: TTriangle);

    procedure FillFragment(const FT: TFragSampling; const sc: TRasterColor; const tex: TMemoryRaster;
      const bitDst, J, start_x, frag_count: Integer; const attr_v, attr_u: TBilerpConsts); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure NewWriterBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure internal_Draw(const Triangle: TTriangle; const Sampler: TRasterColor); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure internal_Draw(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure internal_Draw(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    // render window
    Window: TMemoryRaster;
    WindowSize: Integer;
    // user define
    UserData: Pointer;

    constructor Create(raster: TMemoryRaster);
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DrawTriangle(const Triangle: TTriangle; const Sampler: TRasterColor); overload;
    procedure DrawTriangle(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
    procedure DrawTriangle(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TV2Rect4) sampler Absolute coordiantes
      RenVec: (TV2Rect4) renderer Absolute coordiantes
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const RenVec: TV2Rect4; const Sampler: TRasterColor); overload;
    procedure DrawRect(const SamVec, RenVec: TV2Rect4; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TRectV2) sampler Absolute coordiantes
      RenVec: (TRectV2) renderer Absolute coordiantes
      RenAngle: (TGeoFloat) renderer rotation
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const RenVec: TRectV2; const Sampler: TRasterColor); overload;
    procedure DrawRect(const SamVec, RenVec: TRectV2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
    procedure DrawRect(const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TRasterColor); overload;
    procedure DrawRect(const SamVec, RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TV2Rect4) sampler Absolute coordiantes
      RenVec: (TRectV2) renderer Absolute coordiantes
      RenAngle: (TGeoFloat) renderer rotation
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const SamVec: TV2Rect4; const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TVec2List) sampler Absolute coordiantes
      RenVec: (TVec2List) renderer Absolute coordiantes
      cen: Centroid coordinate
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure FillPoly(const RenVec: TVec2List; const cen: TVec2; const Sampler: TRasterColor); overload;
    procedure FillPoly(const RenVec: TVec2List; const Sampler: TRasterColor); overload;
    procedure FillPoly(const SamVec, RenVec: TVec2List; const SamCen, RenCen: TVec2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
    procedure FillPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
  end;

  TFontRaster = class(TCoreClassObject)
  private type
    PFontCharDefine = ^TFontCharDefine;

    TFontCharDefine = packed record
      Activted: Boolean;
      X, Y: Word;
      w, h: Byte;
    end;

    TFontTable = array [0 .. MaxInt div SizeOf(TFontCharDefine) - 1] of TFontCharDefine;
    PFontTable = ^TFontTable;

    TFontBitRaster = array [0 .. MaxInt - 1] of Byte;
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
    C_WordDefine: TFontCharDefine = (Activted: False; X: 0; Y: 0; w: 0; h: 0);
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
    procedure Add(C: TFontRasterChar; raster: TMemoryRaster);
    procedure Remove(C: TFontRasterChar);
    procedure Clear;
    procedure Build(fontSiz: Integer);

    property FontSize: Integer read FFontSize;
    property ActivtedWord: Integer read FActivtedWord;
    property width: Integer read FWidth;
    property height: Integer read FHeight;

    // store
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure ExportRaster(stream: TCoreClassStream; partitionLine: Boolean);

    // draw font
    function CharSize(const C: TFontRasterChar): TPoint;
    function TextSize(const s: TFontRasterString; charVec2List: TVec2List): TVec2; overload;
    function TextSize(const s: TFontRasterString): TVec2; overload;
    function TextWidth(const s: TFontRasterString): Word;
    function TextHeight(const s: TFontRasterString): Word;

    function Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRasterColor;
      const bilinear_sampling: Boolean; const alpha: TGeoFloat; const Axis: TVec2; const angle, Scale: TGeoFloat): TVec2; overload;

    procedure Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRasterColor); overload;
  end;

{$ENDREGION 'Type'}

{$REGION 'RasterAPI'}


procedure FillRasterColor(var X; Count: Cardinal; Value: TRasterColor); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure CopyRasterColor(const Source; var dest; Count: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure BlendBlock(Dst: TMemoryRaster; dstRect: TRect; Src: TMemoryRaster; Srcx, Srcy: Integer; CombineOp: TDrawMode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure BlockTransfer(Dst: TMemoryRaster; Dstx: Integer; Dsty: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);
function RandomRasterColor(const A: Byte = $FF): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor(const R, g, b: Byte; const A: Byte = $FF): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColorInv(const C: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterAlphaColor(const C: TRasterColor; const A: Byte): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterAlphaColorF(const C: TRasterColor; const A: Single): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RasterColorF(const R, g, b: TGeoFloat; const A: TGeoFloat = 1.0): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RasterColor2F(const C: TRasterColor; var R, g, b, A: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure RasterColor2F(const C: TRasterColor; var R, g, b: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RasterColor2Gray(const C: TRasterColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayS(const C: TRasterColor): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayD(const C: TRasterColor): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RGBA2BGRA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BGRA2RGBA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AggColor(const Value: TRasterColor): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function AggColor(const R, g, b: TGeoFloat; const A: TGeoFloat = 1.0): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function AggColor(const Value: TAggColorRgba8): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

procedure ComputeSize(const MAX_Width, MAX_Height: Integer; var width, height: Integer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure FastBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure FastBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;

procedure Antialias32(const DestMR: TMemoryRaster; AXOrigin, AYOrigin, AXFinal, AYFinal: Integer); overload;
procedure Antialias32(const DestMR: TMemoryRaster; const AAmount: Integer); overload;
procedure HistogramEqualize(const mr: TMemoryRaster);
procedure RemoveRedEyes(const mr: TMemoryRaster);
procedure Sepia32(const mr: TMemoryRaster; const Depth: Byte);
procedure Sharpen(const DestMR: TMemoryRaster; const SharpenMore: Boolean);

procedure AlphaToGrayscale(Src: TMemoryRaster);
procedure IntensityToAlpha(Src: TMemoryRaster);
procedure ReversalAlpha(Src: TMemoryRaster);
procedure RGBToGrayscale(Src: TMemoryRaster);

procedure ColorToTransparent(SrcColor: TRasterColor; Src, Dst: TMemoryRaster);

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function BlendReg(F, b: TRasterColor): TRasterColor; register;
procedure BlendMem(F: TRasterColor; var b: TRasterColor); register;
function BlendRegEx(F, b, M: TRasterColor): TRasterColor; register;
procedure BlendMemEx(F: TRasterColor; var b: TRasterColor; M: TRasterColor); register;
procedure BlendLine(Src, Dst: PRasterColor; Count: Integer); register;
procedure BlendLineEx(Src, Dst: PRasterColor; Count: Integer; M: TRasterColor); register;
function CombineReg(X, Y, w: TRasterColor): TRasterColor; register;
procedure CombineMem(X: TRasterColor; var Y: TRasterColor; w: TRasterColor); register;
procedure CombineLine(Src, Dst: PRasterColor; Count: Integer; w: TRasterColor); register;
function MergeReg(F, b: TRasterColor): TRasterColor; register;
function MergeRegEx(F, b, M: TRasterColor): TRasterColor; register;
procedure MergeMem(F: TRasterColor; var b: TRasterColor); register;
procedure MergeMemEx(F: TRasterColor; var b: TRasterColor; M: TRasterColor); register;
procedure MergeLine(Src, Dst: PRasterColor; Count: Integer); register;
procedure MergeLineEx(Src, Dst: PRasterColor; Count: Integer; M: TRasterColor); register;

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

function EncodeJpegLSRasterAlphaToStream(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
function EncodeJpegLSRasterToStream3(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
function EncodeJpegLSRasterToStream1(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean; overload;

function DecodeJpegLSRasterFromStream(const stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
function DecodeJpegLSRasterAlphaFromStream(const stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;

function EncodeJpegLSGrayRasterToStream(const ARaster: PByteRaster; const stream: TCoreClassStream): Boolean; overload;
function DecodeJpegLSGrayRasterFromStream(const stream: TCoreClassStream; var ARaster: TByteRaster): Boolean;
{$ENDREGION 'RasterAPI'}


var
  NewRaster: function: TMemoryRaster;
  NewRasterFromFile: function(const fn: string): TMemoryRaster;
  NewRasterFromStream: function(const stream: TCoreClassStream): TMemoryRaster;
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

{$INCLUDE zDefine.inc}

{$REGION 'InternalDefines'}


var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;
  SystemFont: TFontRaster;

type
  TLUT8            = array [Byte] of Byte;
  TLogicalOperator = (loXOR, loAND, loOR);
  TByteArray       = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
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
  TBlendLineEx = procedure(Src, Dst: PRasterColor; Count: Integer; M: TRasterColor);

const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
{$ENDREGION 'InternalDefines'}

{$INCLUDE MemoryRaster_RasterClass.inc}
{$INCLUDE MemoryRaster_SequenceClass.inc}
{$INCLUDE MemoryRaster_Vertex.inc}
{$INCLUDE MemoryRaster_Agg.inc}
{$INCLUDE MemoryRaster_Font.inc}
{$INCLUDE MemoryRaster_ExtApi.inc}


function _NewRaster: TMemoryRaster;
begin
  Result := TMemoryRaster.Create;
end;

function _NewRasterFromFile(const fn: string): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LoadFromFile(fn);
end;

function _NewRasterFromStream(const stream: TCoreClassStream): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LoadFromStream(stream);
end;

procedure _SaveRaster(mr: TMemoryRaster; const fn: string);
begin
  mr.SaveToFile(fn);
end;

initialization

MakeMergeTables;

NewRaster := {$IFDEF FPC}@{$ENDIF FPC}_NewRaster;
NewRasterFromFile := {$IFDEF FPC}@{$ENDIF FPC}_NewRasterFromFile;
NewRasterFromStream := {$IFDEF FPC}@{$ENDIF FPC}_NewRasterFromStream;
SaveRaster := {$IFDEF FPC}@{$ENDIF FPC}_SaveRaster;

Init_DefaultFont;

finalization

Free_DefaultFont;

end. 
 
