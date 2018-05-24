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

interface

uses Types, Math, Variants, CoreClasses, MemoryStream64,
  Geometry2DUnit, PascalStrings, UnicodeMixedLib, JLSCodec;

{$I zDefine.inc}


type
  TRasterColor = type Cardinal;
  PRasterColor = ^TRasterColor;

  TRasterColorArray = packed array [0 .. MaxInt div SizeOf(TRasterColor) - 1] of TRasterColor;
  PRasterColorArray = ^TRasterColorArray;

  TRasterColorEntry = packed record
    case byte of
      0: (R, G, B, A: byte);
      1: (RGBA: TRasterColor);
  end;

  PRasterColorEntry = ^TRasterColorEntry;

  PRasterColorEntryArray   = ^TRasterColorEntryArray;
  TRasterColorEntryArray   = array [0 .. MaxInt div SizeOf(TRasterColorEntry) - 1] of TRasterColorEntry;
  TArrayOfRasterColorEntry = array of TRasterColorEntry;

  TDrawMode    = (dmOpaque, dmBlend, dmTransparent);
  TCombineMode = (cmBlend, cmMerge);

  TByteRaster = array of array of byte;
  PByteRaster = ^TByteRaster;

  TMemoryRaster = class(TCoreClassObject)
  private
    FBits: PRasterColorArray;
    FWidth, FHeight: Integer;
    FDrawMode: TDrawMode;
    FCombineMode: TCombineMode;

    FMD5: UnicodeMixedLib.TMD5;

    FMasterAlpha: Cardinal;
    FOuterColor: TRasterColor;
    FData: Variant;

    function GetPixel(X, Y: Integer): TRasterColor;
    procedure SetPixel(X, Y: Integer; const Value: TRasterColor);

    function GetPixelBGRA(X, Y: Integer): TRasterColor;
    procedure SetPixelBGRA(X, Y: Integer; const Value: TRasterColor);

    function GetPixelPtr(X, Y: Integer): PRasterColor;
    function GetScanLine(Y: Integer): PRasterColorArray;

    function GetPixelRed(X, Y: Integer): byte;
    procedure SetPixelRed(X, Y: Integer; const Value: byte);

    function GetPixelGreen(X, Y: Integer): byte;
    procedure SetPixelGreen(X, Y: Integer; const Value: byte);

    function GetPixelBlue(X, Y: Integer): byte;
    procedure SetPixelBlue(X, Y: Integer; const Value: byte);

    function GetPixelAlpha(X, Y: Integer): byte;
    procedure SetPixelAlpha(X, Y: Integer; const Value: byte);

    function GetGray(X, Y: Integer): byte;
    procedure SetGray(X, Y: Integer; const Value: byte);

    function GetGrayS(X, Y: Integer): Single;
    procedure SetGrayS(X, Y: Integer; const Value: Single);

    function GetGrayD(X, Y: Integer): Double;
    procedure SetGrayD(X, Y: Integer; const Value: Double);

    function GetPixelF(X, Y: TGeoFloat): TRasterColor;
    procedure SetPixelF(X, Y: TGeoFloat; const Value: TRasterColor);

    function GetPixelVec(const v2: TVec2): TRasterColor;
    procedure SetPixelVec(const v2: TVec2; const Value: TRasterColor);
  public
    constructor Create; virtual;
    destructor Destroy; override;

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
    function RebuildMD5: UnicodeMixedLib.TMD5;

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
    procedure ColorTransparent(c: TRasterColor);
    procedure ColorBlend(c: TRasterColor);
    procedure Grayscale;
    procedure TransformToGrayRaster(var Output: TByteRaster);
    procedure TransformToRedRaster(var Output: TByteRaster);
    procedure TransformToGreenRaster(var Output: TByteRaster);
    procedure TransformToBlueRaster(var Output: TByteRaster);
    procedure TransformToAlphaRaster(var Output: TByteRaster);

    procedure VertLine(X, Y1, Y2: Integer; Value: TRasterColor);
    procedure HorzLine(X1, Y, X2: Integer; Value: TRasterColor);
    procedure Line(X1, Y1, X2, Y2: Integer; Value: TRasterColor; L: Boolean); overload;
    procedure Line(p1, p2: T2DPoint; Value: TRasterColor; L: Boolean); overload;
    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TRasterColor); overload;
    procedure FillRect(DstX, DstY, LineDist: Integer; Value: TRasterColor); overload;
    procedure FillRect(Dst: TVec2; LineDist: Integer; Value: TRasterColor); overload;
    procedure DrawCross(DstX, DstY, LineDist: Integer; Value: TRasterColor); overload;
    procedure DrawCross(Dst: TVec2; LineDist: Integer; Value: TRasterColor); overload;
    procedure DrawPointListLine(pl: T2DPointList; Value: TRasterColor; wasClose: Boolean);

    procedure Draw(DstX, DstY: Integer; Src: TMemoryRaster); overload;
    procedure Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstPt: TVec2); overload;

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
    procedure SaveToFile(fn: SystemString); // published format

    { custom format }
    procedure SaveToZLibCompressFile(fn: SystemString);    // no published format
    procedure SaveToDeflateCompressFile(fn: SystemString); // no published format
    procedure SaveToBRRCCompressFile(fn: SystemString);    // no published format
    procedure SaveToJpegLS1File(fn: SystemString);         // published format
    procedure SaveToJpegLS3File(fn: SystemString);         // published format
    procedure SaveToJpegAlphaFile(fn: SystemString);       // no published format

    property Pixel[X, Y: Integer]: TRasterColor read GetPixel write SetPixel; default;
    property PixelBGRA[X, Y: Integer]: TRasterColor read GetPixelBGRA write SetPixelBGRA;
    property PixelPtr[X, Y: Integer]: PRasterColor read GetPixelPtr;
    property PixelRed[X, Y: Integer]: byte read GetPixelRed write SetPixelRed;
    property PixelGreen[X, Y: Integer]: byte read GetPixelGreen write SetPixelGreen;
    property PixelBlue[X, Y: Integer]: byte read GetPixelBlue write SetPixelBlue;
    property PixelAlpha[X, Y: Integer]: byte read GetPixelAlpha write SetPixelAlpha;
    property PixelGray[X, Y: Integer]: byte read GetGray write SetGray;
    property PixelGrayS[X, Y: Integer]: Single read GetGrayS write SetGrayS;
    property PixelGrayD[X, Y: Integer]: Double read GetGrayD write SetGrayD;
    property PixelF[X, Y: TGeoFloat]: TRasterColor read GetPixelF write SetPixelF;
    property PixelVec[const v2: TVec2]: TRasterColor read GetPixelVec write SetPixelVec;

    property ScanLine[Y: Integer]: PRasterColorArray read GetScanLine;
    property Bits: PRasterColorArray read FBits;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    property DrawMode: TDrawMode read FDrawMode write FDrawMode default dmOpaque;
    property CombineMode: TCombineMode read FCombineMode write FCombineMode default cmBlend;
    property MasterAlpha: Cardinal read FMasterAlpha write FMasterAlpha;
    property OuterColor: TRasterColor read FOuterColor write FOuterColor;

    property Data: Variant read FData write FData;
    property MD5: UnicodeMixedLib.TMD5 read FMD5;
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

procedure BlendBlock(Dst: TMemoryRaster; DstRect: TRect; Src: TMemoryRaster; SrcX, SrcY: Integer; CombineOp: TDrawMode); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure BlockTransfer(Dst: TMemoryRaster; DstX: Integer; DstY: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);
function RandomRasterColor(const A: byte = $FF): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColor(const R, G, B: byte; const A: byte = $FF): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColorInv(const c: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Red(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Green(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Blue(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function Alpha(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function RasterColorF(const R, G, B: Single; const A: Single = 1.0): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RasterColor2F(const c: TRasterColor; var R, G, B, A: Single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RasterColor2F(const c: TRasterColor; var R, G, B: Single); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColorD(const R, G, B: Double; const A: Double = 1.0): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RasterColor2D(const c: TRasterColor; var R, G, B, A: Double); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RasterColor2D(const c: TRasterColor; var R, G, B: Double); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function RasterColor2Gray(const c: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColor2GrayS(const c: TRasterColor): Single; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColor2GrayD(const c: TRasterColor): Double; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RGBA2BGRA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function BGRA2RGBA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}

procedure ComputeSize(const MAX_Width, MAX_Height: Integer; var Width, Height: Integer); overload; {$IFDEF INLINE_ASM}inline; {$ENDIF}

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
function GetSequenceFrameRect(Bmp: TMemoryRaster; Total, Column, index: Integer): TRect; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure GetSequenceFrameOutput(Bmp: TMemoryRaster; Total, Column, index: Integer; Output: TMemoryRaster); {$IFDEF INLINE_ASM}inline; {$ENDIF}

function BlendReg(F, B: TRasterColor): TRasterColor; register;
procedure BlendMem(F: TRasterColor; var B: TRasterColor); register;
function BlendRegEx(F, B, M: TRasterColor): TRasterColor; register;
procedure BlendMemEx(F: TRasterColor; var B: TRasterColor; M: TRasterColor); register;
procedure BlendLine(Src, Dst: PRasterColor; Count: Integer); register;
procedure BlendLineEx(Src, Dst: PRasterColor; Count: Integer; M: TRasterColor); register;
function CombineReg(X, Y, w: TRasterColor): TRasterColor; register;
procedure CombineMem(X: TRasterColor; var Y: TRasterColor; w: TRasterColor); register;
procedure CombineLine(Src, Dst: PRasterColor; Count: Integer; w: TRasterColor); register;
function MergeReg(F, B: TRasterColor): TRasterColor; register;
function MergeRegEx(F, B, M: TRasterColor): TRasterColor; register;
procedure MergeMem(F: TRasterColor; var B: TRasterColor); register;
procedure MergeMemEx(F: TRasterColor; var B: TRasterColor; M: TRasterColor); register;
procedure MergeLine(Src, Dst: PRasterColor; Count: Integer); register;
procedure MergeLineEx(Src, Dst: PRasterColor; Count: Integer; M: TRasterColor); register;

{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  fixed by 600585@qq.com, v2.2
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
  CoreCompress;

var
  RcTable: array [byte, byte] of byte;
  DivTable: array [byte, byte] of byte;

type
  TLUT8            = array [byte] of byte;
  TLogicalOperator = (loXOR, loAND, loOR);

  TByteArray = array [0 .. MaxInt div SizeOf(byte) - 1] of byte;
  PByteArray = ^TByteArray;

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

  SEmptyBitmap      = 'The bitmap is nil';
  SEmptySource      = 'The source is nil';
  SEmptyDestination = 'Destination is nil';

procedure FillRasterColor(var X; Count: Cardinal; Value: TRasterColor); {$IFDEF INLINE_ASM}inline; {$ENDIF}
var
  I: Integer;
  p: PRasterColorArray;
begin
  p := PRasterColorArray(@X);
  for I := Count - 1 downto 0 do
      p^[I] := Value;
end;

procedure MoveCardinal(const Source; var dest; Count: Integer); {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Move(Source, dest, Count shl 2);
end;

function ClampInt(const Value, Min, Max: Integer): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  if Value > Max then
      Result := Max
  else if Value < Min then
      Result := Min
  else
      Result := Value;
end;

function ClampByte(const Value, Min, Max: byte): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  if Value > Max then
      Result := Max
  else if Value < Min then
      Result := Min
  else
      Result := Value;
end;

function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  if R1.Left >= R2.Left then
      Dst.Left := R1.Left
  else
      Dst.Left := R2.Left;
  if R1.Right <= R2.Right then
      Dst.Right := R1.Right
  else
      Dst.Right := R2.Right;
  if R1.Top >= R2.Top then
      Dst.Top := R1.Top
  else
      Dst.Top := R2.Top;
  if R1.Bottom <= R2.Bottom then
      Dst.Bottom := R1.Bottom
  else
      Dst.Bottom := R2.Bottom;
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then
      Dst := ZERO_RECT;
end;

procedure OffsetRect(var R: TRect; Dx, Dy: Integer); {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Inc(R.Left, Dx);
  Inc(R.Top, Dy);
  Inc(R.Right, Dx);
  Inc(R.Bottom, Dy);
end;

function IsRectEmpty(const R: TRect): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

function TMemoryRaster.GetPixel(X, Y: Integer): TRasterColor;
begin
  if X < 0 then
      X := 0
  else if X >= Width then
      X := Width - 1;

  if Y < 0 then
      Y := 0
  else if Y >= Height then
      Y := Height - 1;

  Result := FBits^[X + Y * Width];
end;

procedure TMemoryRaster.SetPixel(X, Y: Integer; const Value: TRasterColor);
begin
  if X < 0 then
      X := 0
  else if X >= Width then
      X := Width - 1;

  if Y < 0 then
      Y := 0
  else if Y >= Height then
      Y := Height - 1;

  FBits^[X + Y * Width] := Value;
end;

function TMemoryRaster.GetPixelBGRA(X, Y: Integer): TRasterColor;
begin
  Result := RGBA2BGRA(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetPixelBGRA(X, Y: Integer; const Value: TRasterColor);
begin
  SetPixel(X, Y, BGRA2RGBA(Value));
end;

function TMemoryRaster.GetPixelPtr(X, Y: Integer): PRasterColor;
begin
  if X < 0 then
      X := 0
  else if X >= Width then
      X := Width - 1;

  if Y < 0 then
      Y := 0
  else if Y >= Height then
      Y := Height - 1;

  Result := @(FBits^[X + Y * Width]);
end;

function TMemoryRaster.GetScanLine(Y: Integer): PRasterColorArray;
begin
  Result := @(FBits^[Y * FWidth]);
end;

function TMemoryRaster.GetPixelRed(X, Y: Integer): byte;
begin
  Result := Red(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetPixelRed(X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.R := Value;
end;

function TMemoryRaster.GetPixelGreen(X, Y: Integer): byte;
begin
  Result := Green(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetPixelGreen(X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.G := Value;
end;

function TMemoryRaster.GetPixelBlue(X, Y: Integer): byte;
begin
  Result := Blue(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetPixelBlue(X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.B := Value;
end;

function TMemoryRaster.GetPixelAlpha(X, Y: Integer): byte;
begin
  Result := Alpha(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetPixelAlpha(X, Y: Integer; const Value: byte);
begin
  PRasterColorEntry(GetPixelPtr(X, Y))^.A := Value;
end;

function TMemoryRaster.GetGray(X, Y: Integer): byte;
begin
  Result := RasterColor2Gray(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGray(X, Y: Integer; const Value: byte);
begin
  SetPixel(X, Y, RasterColor(Value, Value, Value, 255));
end;

function TMemoryRaster.GetGrayS(X, Y: Integer): Single;
begin
  Result := RasterColor2GrayS(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGrayS(X, Y: Integer; const Value: Single);
begin
  SetGray(X, Y, ClampByte(Round(Value * $FF), 0, $FF));
end;

function TMemoryRaster.GetGrayD(X, Y: Integer): Double;
begin
  Result := RasterColor2GrayD(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGrayD(X, Y: Integer; const Value: Double);
begin
  SetGrayS(X, Y, Value);
end;

function TMemoryRaster.GetPixelF(X, Y: TGeoFloat): TRasterColor;
begin
  Result := GetPixel(Round(X), Round(Y));
end;

procedure TMemoryRaster.SetPixelF(X, Y: TGeoFloat; const Value: TRasterColor);
begin
  SetPixel(Round(X), Round(Y), Value);
end;

function TMemoryRaster.GetPixelVec(const v2: TVec2): TRasterColor;
begin
  Result := GetPixelF(v2[0], v2[1]);
end;

procedure TMemoryRaster.SetPixelVec(const v2: TVec2; const Value: TRasterColor);
begin
  SetPixelF(v2[0], v2[1], Value)
end;

constructor TMemoryRaster.Create;
begin
  inherited Create;
  FBits := nil;
  FWidth := 0;
  FHeight := 0;
  FOuterColor := $00000000; // by default as full transparency black

  FMasterAlpha := $FF;
  FDrawMode := dmBlend;
  FCombineMode := cmBlend;

  FData := NULL;
  FMD5 := NullMD5;
end;

destructor TMemoryRaster.Destroy;
begin
  Reset;
  inherited Destroy;
end;

procedure TMemoryRaster.Clear;
begin
  Clear($FF000000);
end;

procedure TMemoryRaster.Clear(FillColor: TRasterColor);
begin
  if Empty then
      Exit;
  FillRasterColor(Bits^[0], Width * Height, FillColor);
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer);
begin
  if (NewWidth = FWidth) and (NewHeight = FHeight) and (Assigned(FBits)) then
      Exit;

  if Assigned(FBits) then
    begin
      FreeMem(FBits);
      FBits := nil;
    end;

  GetMem(FBits, NewWidth * NewHeight * 4);
  FWidth := NewWidth;
  FHeight := NewHeight;
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor);
begin
  SetSize(NewWidth, NewHeight);
  FillRasterColor(FBits^[0], NewWidth * NewHeight, ClearColor);
end;

function TMemoryRaster.SizeOfPoint: TPoint;
begin
  Result := Point(Width, Height);
end;

function TMemoryRaster.SizeOf2DPoint: T2DPoint;
begin
  Result := Make2DPoint(Width, Height);
end;

function TMemoryRaster.Size2D: TVec2;
begin
  Result := SizeOf2DPoint;
end;

function TMemoryRaster.Empty: Boolean;
begin
  Result := (FBits = nil) or (FWidth <= 0) or (FHeight <= 0);
end;

function TMemoryRaster.BoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TMemoryRaster.BoundsRectV2: TRectV2;
begin
  Result := MakeRectV2(0, 0, Width, Height);
end;

function TMemoryRaster.RebuildMD5: UnicodeMixedLib.TMD5;
begin
  if FBits <> nil then
      FMD5 := umlMD5(PBYTE(FBits), Width * Height * 4)
  else
      FMD5 := NullMD5;

  Result := FMD5;
end;

procedure TMemoryRaster.Reset;
begin
  if Assigned(FBits) then
    begin
      FreeMem(FBits);
      FBits := nil;
    end;

  FWidth := 0;
  FHeight := 0;
  FMD5 := NullMD5;
end;

procedure TMemoryRaster.Assign(sour: TMemoryRaster);
begin
  Reset;
  FWidth := sour.FWidth;
  FHeight := sour.FHeight;

  FDrawMode := sour.FDrawMode;
  FCombineMode := sour.FCombineMode;

  FMasterAlpha := sour.FMasterAlpha;
  FOuterColor := sour.FOuterColor;

  GetMem(FBits, sour.FWidth * sour.FHeight * 4);
  MoveCardinal(sour.FBits^[0], FBits^[0], sour.FWidth * sour.FHeight);
  FMD5 := sour.FMD5;
end;

procedure TMemoryRaster.FlipHorz;
var
  I, J: Integer;
  p1, p2: PRasterColor;
  tmp: TRasterColor;
  w, W2: Integer;
begin
  w := Width;
  { In-place flipping }
  p1 := PRasterColor(Bits);
  p2 := p1;
  Inc(p2, Width - 1);
  W2 := Width shr 1;
  for J := 0 to Height - 1 do
    begin
      for I := 0 to W2 - 1 do
        begin
          tmp := p1^;
          p1^ := p2^;
          p2^ := tmp;
          Inc(p1);
          Dec(p2);
        end;
      Inc(p1, w - W2);
      Inc(p2, w + W2);
    end;
end;

procedure TMemoryRaster.FlipVert;
var
  J, J2: Integer;
  Buffer: PRasterColorArray;
  p1, p2: PRasterColor;
begin
  { in-place }
  J2 := Height - 1;
  GetMem(Buffer, Width shl 2);
  for J := 0 to Height div 2 - 1 do
    begin
      p1 := PixelPtr[0, J];
      p2 := PixelPtr[0, J2];
      MoveCardinal(p1^, Buffer^, Width);
      MoveCardinal(p2^, p1^, Width);
      MoveCardinal(Buffer^, p2^, Width);
      Dec(J2);
    end;
  FreeMem(Buffer);
end;

procedure TMemoryRaster.Rotate90;
var
  tmp: TMemoryRaster;
  X, Y, I, J: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  I := 0;
  for Y := 0 to Height - 1 do
    begin
      J := Height - 1 - Y;
      for X := 0 to Width - 1 do
        begin
          tmp.Bits^[J] := Bits^[I];
          Inc(I);
          Inc(J, Height);
        end;
    end;

  Reset;
  FWidth := tmp.FWidth;
  FHeight := tmp.FHeight;
  FBits := tmp.FBits;

  tmp.FWidth := 0;
  tmp.FHeight := 0;
  tmp.FBits := nil;
  DisposeObject(tmp);
end;

procedure TMemoryRaster.Rotate180;
var
  I, I2: Integer;
  tmp: TRasterColor;
begin
  I2 := Width * Height - 1;
  for I := 0 to Width * Height div 2 - 1 do
    begin
      tmp := Bits^[I2];
      Bits^[I2] := Bits^[I];
      Bits^[I] := tmp;
      Dec(I2);
    end;
end;

procedure TMemoryRaster.Rotate270;
var
  tmp: TMemoryRaster;
  X, Y, I, J: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  I := 0;
  for Y := 0 to Height - 1 do
    begin
      J := (Width - 1) * Height + Y;
      for X := 0 to Width - 1 do
        begin
          tmp.Bits^[J] := Bits^[I];
          Inc(I);
          Dec(J, Height);
        end;
    end;

  Reset;
  FWidth := tmp.FWidth;
  FHeight := tmp.FHeight;
  FBits := tmp.FBits;

  tmp.FWidth := 0;
  tmp.FHeight := 0;
  tmp.FBits := nil;
  DisposeObject(tmp);
end;

procedure TMemoryRaster.NoLineZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
var
  J: Integer;
  SourceI, SourceJ: Double;
  SourceIInt, SourceJInt: Integer;
  SourceINext, SourceJNext: Integer;
begin
  for J := 0 to dest.Height - 1 do
    begin
      SourceI := (pass / (dest.Width - 1)) * (Source.Width - 1);
      SourceJ := (J / (dest.Height - 1)) * (Source.Height - 1);

      SourceIInt := Trunc(SourceI);
      SourceJInt := Trunc(SourceJ);

      dest.Pixel[pass, J] := Source.Pixel[SourceIInt, SourceJInt]
    end;
end;

procedure TMemoryRaster.NoLineZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    NoLineZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}


var
  I: Integer;

begin
  SetSize(NewWidth, NewHeight);

  if (Source.Width > 1) and (Source.Width > 1) and (Width > 1) and (Height > 1) then
    begin
      {$IFDEF parallel}
      {$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Width - 1);
      {$ELSE}
      TParallel.For(0, Width - 1, procedure(pass: Integer)
        begin
          NoLineZoomLine(Source, Self, pass);
        end);
      {$ENDIF FPC}
      {$ELSE}
      for I := Width - 1 downto 0 do
          NoLineZoomLine(Source, Self, I);
      {$ENDIF parallel}
    end;
end;

procedure TMemoryRaster.NoLineZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.NoLineZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);
end;

procedure TMemoryRaster.ZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
type
  PFloatColorEntry = ^TFloatColorEntry;

  TFloatColorEntry = packed record
    colorEntry: TRasterColorEntry;
    k: Double;
  end;

  function RasterColorEntryMul(var f1, f2, f3, f4: TFloatColorEntry): TRasterColorEntry; inline;
    procedure ClampComponent(var Value: Double); inline;
    begin
      if Value < 0 then
          Value := 0
      else if Value > 1 then
          Value := 1;
    end;

  var
    R, G, B, A: Double;
  begin
    R := ((f1.colorEntry.R / $FF) * f1.k);
    G := ((f1.colorEntry.G / $FF) * f1.k);
    B := ((f1.colorEntry.B / $FF) * f1.k);
    A := ((f1.colorEntry.A / $FF) * f1.k);

    R := R + ((f2.colorEntry.R / $FF) * f2.k);
    G := G + ((f2.colorEntry.G / $FF) * f2.k);
    B := B + ((f2.colorEntry.B / $FF) * f2.k);
    A := A + ((f2.colorEntry.A / $FF) * f2.k);

    R := R + ((f3.colorEntry.R / $FF) * f3.k);
    G := G + ((f3.colorEntry.G / $FF) * f3.k);
    B := B + ((f3.colorEntry.B / $FF) * f3.k);
    A := A + ((f3.colorEntry.A / $FF) * f3.k);

    R := R + ((f4.colorEntry.R / $FF) * f4.k);
    G := G + ((f4.colorEntry.G / $FF) * f4.k);
    B := B + ((f4.colorEntry.B / $FF) * f4.k);
    A := A + ((f4.colorEntry.A / $FF) * f4.k);

    ClampComponent(R);
    ClampComponent(G);
    ClampComponent(B);
    ClampComponent(A);

    Result.R := Round(R * $FF);
    Result.G := Round(G * $FF);
    Result.B := Round(B * $FF);
    Result.A := Round(A * $FF);
  end;

var
  J: Integer;
  SourceI, SourceJ: Double;
  SourceIInt, SourceJInt: Integer;
  SourceINext, SourceJNext: Integer;
  SourceIOffset, SourceJOffset: Double;

  D1, D2, D3, D4: TFloatColorEntry;
begin
  for J := 0 to dest.Height - 1 do
    begin
      SourceI := (pass / (dest.Width - 1)) * (Source.Width - 1);
      SourceJ := (J / (dest.Height - 1)) * (Source.Height - 1);

      SourceIInt := Trunc(SourceI);
      SourceJInt := Trunc(SourceJ);

      SourceINext := Min(Source.Width - 1, SourceIInt + 1);
      SourceJNext := Min(Source.Height - 1, SourceJInt + 1);

      SourceIOffset := Frac(SourceI);
      SourceJOffset := Frac(SourceJ);

      D1.k := (1 - SourceIOffset) * (1 - SourceJOffset);
      D2.k := SourceIOffset * (1 - SourceJOffset);
      D3.k := SourceIOffset * SourceJOffset;
      D4.k := (1 - SourceIOffset) * SourceJOffset;

      D1.colorEntry.RGBA := (Source.Pixel[SourceIInt, SourceJInt]);
      D2.colorEntry.RGBA := (Source.Pixel[SourceINext, SourceJInt]);
      D3.colorEntry.RGBA := (Source.Pixel[SourceINext, SourceJNext]);
      D4.colorEntry.RGBA := (Source.Pixel[SourceIInt, SourceJNext]);

      dest.Pixel[pass, J] := RasterColorEntryMul(D1, D2, D3, D4).RGBA;
    end;
end;

procedure TMemoryRaster.ZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}


var
  I: Integer;

begin
  SetSize(NewWidth, NewHeight);

  if (Source.Width > 1) and (Source.Width > 1) and (Width > 1) and (Height > 1) then
    begin
      {$IFDEF parallel}
      {$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Width - 1);
      {$ELSE}
      TParallel.For(0, Width - 1, procedure(pass: Integer)
        begin
          ZoomLine(Source, Self, pass);
        end);
      {$ENDIF FPC}
      {$ELSE}
      for I := Width - 1 downto 0 do
          ZoomLine(Source, Self, I);
      {$ENDIF parallel}
    end;
end;

procedure TMemoryRaster.Zoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.ZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);
end;

procedure TMemoryRaster.FastBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      FastBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      // zoom
      n := TMemoryRaster.Create;
      // preprocess zoom
      FastBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.FastBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.FastBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);
end;

procedure TMemoryRaster.GaussianBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GaussianBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GaussianBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.GaussianBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.GaussianBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);
end;

procedure TMemoryRaster.GrayscaleBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GrayscaleBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GrayscaleBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.GrayscaleBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.GrayscaleBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);
end;

function TMemoryRaster.FormatAsBGRA: TMemoryRaster;
var
  I: Integer;
begin
  Result := TMemoryRaster.Create;
  GetMem(Result.FBits, Width * Height * 4);
  Result.FWidth := Width;
  Result.FHeight := Height;

  for I := (Width * Height) - 1 downto 0 do
      Result.FBits^[I] := RGBA2BGRA(FBits^[I]);
end;

procedure TMemoryRaster.FormatBGRA;
var
  I: Integer;
begin
  for I := (Width * Height) - 1 downto 0 do
      FBits^[I] := RGBA2BGRA(FBits^[I]);
end;

procedure TMemoryRaster.ColorTransparent(c: TRasterColor);
var
  I, J: Integer;
  A: byte;
  ce: TRasterColorEntry;
begin
  ce.RGBA := c;
  A := ce.A;
  for I := 0 to Width - 1 do
    for J := 0 to Height - 1 do
      begin
        ce.RGBA := Pixel[I, J];
        ce.A := A;
        if ce.RGBA = c then
            Pixel[I, J] := RasterColor(0, 0, 0, 0);
      end;
end;

procedure TMemoryRaster.ColorBlend(c: TRasterColor);
var
  I, J: Integer;
begin
  for I := 0 to Width - 1 do
    for J := 0 to Height - 1 do
        Pixel[I, J] := BlendReg(Pixel[I, J], c);
end;

procedure TMemoryRaster.Grayscale;
begin
  RGBToGrayscale(Self);
end;

procedure TMemoryRaster.TransformToGrayRaster(var Output: TByteRaster);
var
  I, J: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for J := 0 to FHeight - 1 do
    for I := 0 to FWidth - 1 do
        Output[J, I] := PixelGray[I, J];
end;

procedure TMemoryRaster.TransformToRedRaster(var Output: TByteRaster);
var
  I, J: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for J := 0 to FHeight - 1 do
    for I := 0 to FWidth - 1 do
        Output[J, I] := PixelRed[I, J];
end;

procedure TMemoryRaster.TransformToGreenRaster(var Output: TByteRaster);
var
  I, J: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for J := 0 to FHeight - 1 do
    for I := 0 to FWidth - 1 do
        Output[J, I] := PixelGreen[I, J];
end;

procedure TMemoryRaster.TransformToBlueRaster(var Output: TByteRaster);
var
  I, J: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for J := 0 to FHeight - 1 do
    for I := 0 to FWidth - 1 do
        Output[J, I] := PixelBlue[I, J];
end;

procedure TMemoryRaster.TransformToAlphaRaster(var Output: TByteRaster);
var
  I, J: Integer;
begin
  SetLength(Output, FHeight, FWidth);
  for J := 0 to FHeight - 1 do
    for I := 0 to FWidth - 1 do
        Output[J, I] := PixelAlpha[I, J];
end;

procedure TMemoryRaster.VertLine(X, Y1, Y2: Integer; Value: TRasterColor);
var
  I, NH, NL: Integer;
  p: PRasterColor;
begin
  if (X < 0) or (X >= Width) then
      Exit;
  Y1 := ClampInt(Y1, 0, Height);
  Y2 := ClampInt(Y2, 0, Height);

  if Y2 < Y1 then
      swap(Y1, Y2);

  p := PixelPtr[X, Y1];
  I := Y2 - Y1 + 1;
  NH := I shr 2;
  NL := I and $03;
  for I := 0 to NH - 1 do
    begin
      p^ := BlendReg(Value, p^);
      Inc(p, Width);
      p^ := BlendReg(Value, p^);
      Inc(p, Width);
      p^ := BlendReg(Value, p^);
      Inc(p, Width);
      p^ := BlendReg(Value, p^);
      Inc(p, Width);
    end;
  for I := 0 to NL - 1 do
    begin
      p^ := BlendReg(Value, p^);
      Inc(p, Width);
    end;
end;

procedure TMemoryRaster.HorzLine(X1, Y, X2: Integer; Value: TRasterColor);
var
  p: PRasterColor;
  I: Integer;
begin
  if (Y < 0) or (Y >= Height) then
      Exit;
  X1 := ClampInt(X1, 0, Width);
  X2 := ClampInt(X2, 0, Width);

  if X1 > X2 then
      swap(X1, X2);

  for I := X1 to X2 - 1 do
    begin
      p := PixelPtr[I, Y];
      p^ := BlendReg(Value, p^);
    end;
end;

procedure TMemoryRaster.Line(X1, Y1, X2, Y2: Integer; Value: TRasterColor; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  pc: PRasterColor;
  pi, pl: Integer;
begin
  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then
        Sx := 1
    else if Dx < 0 then
      begin
        Dx := -Dx;
        Sx := -1;
      end
    else // Dx = 0
      begin
        if Dy > 0 then
            VertLine(X1, Y1, Y2 - 1, Value)
        else if Dy < 0 then
            VertLine(X1, Y2 + 1, Y1, Value);
        if L then
            Pixel[X2, Y2] := Value;
        Exit;
      end;

    if Dy > 0 then
        Sy := 1
    else if Dy < 0 then
      begin
        Dy := -Dy;
        Sy := -1;
      end
    else // Dy = 0
      begin
        if X2 > X1 then
            HorzLine(X1, Y1, X2 - 1, Value)
        else
            HorzLine(X2 + 1, Y1, X1, Value);
        if L then
            Pixel[X2, Y2] := Value;
        Exit;
      end;

    pi := X1 + Y1 * Width;
    Sy := Sy * Width;
    pl := Width * Height;

    if Dx > Dy then
      begin
        Delta := Dx shr 1;
        for I := 0 to Dx - 1 do
          begin
            if (pi >= 0) and (pi < pl) then
              begin
                pc := @FBits^[pi];
                pc^ := BlendReg(Value, pc^);
              end;

            Inc(pi, Sx);
            Inc(Delta, Dy);
            if Delta >= Dx then
              begin
                Inc(pi, Sy);
                Dec(Delta, Dx);
              end;
          end;
      end
    else // Dx < Dy
      begin
        Delta := Dy shr 1;
        for I := 0 to Dy - 1 do
          begin
            if (pi >= 0) and (pi < pl) then
              begin
                pc := @FBits^[pi];
                pc^ := BlendReg(Value, pc^);
              end;
            Inc(pi, Sy);
            Inc(Delta, Dx);
            if Delta >= Dy then
              begin
                Inc(pi, Sx);
                Dec(Delta, Dy);
              end;
          end;
      end;
    if (L) and (pi >= 0) and (pi < pl) then
      begin
        pc := @FBits^[pi];
        pc^ := BlendReg(Value, pc^);
      end;
  except
  end;
end;

procedure TMemoryRaster.Line(p1, p2: T2DPoint; Value: TRasterColor; L: Boolean);
begin
  Line(Round(p1[0]), Round(p1[1]), Round(p2[0]), Round(p2[1]), Value, L);
end;

procedure TMemoryRaster.FillRect(X1, Y1, X2, Y2: Integer; Value: TRasterColor);
var
  J, I: Integer;
  p: PRasterColor;
begin
  for J := Y1 to Y2 - 1 do
    for I := X1 to X2 - 1 do
      if PointInRect(I, J, 0, 0, Width, Height) then
        begin
          p := PixelPtr[I, J];
          p^ := BlendReg(Value, p^);
        end;
end;

procedure TMemoryRaster.FillRect(DstX, DstY, LineDist: Integer; Value: TRasterColor);
var
  l2, X1, Y1, X2, Y2: Integer;
begin
  l2 := LineDist div 2;
  X1 := DstX - l2;
  Y1 := DstY - l2;
  X2 := DstX + l2;
  Y2 := DstY + l2;
  FillRect(X1, Y1, X2, Y2, Value);
end;

procedure TMemoryRaster.FillRect(Dst: TVec2; LineDist: Integer; Value: TRasterColor);
begin
  FillRect(Round(Dst[0]), Round(Dst[1]), LineDist, Value);
end;

procedure TMemoryRaster.DrawCross(DstX, DstY, LineDist: Integer; Value: TRasterColor);
var
  L, X1, Y1, X2, Y2: Integer;
begin
  L := LineDist div 2;

  X1 := DstX - L;
  Y1 := DstY - L;
  X2 := DstX + L;
  Y2 := DstY + L;
  Line(X1, Y1, X2, Y2, Value, False);

  X1 := DstX - L;
  Y1 := DstY + L;
  X2 := DstX + L;
  Y2 := DstY - L;
  Line(X1, Y1, X2, Y2, Value, False);
end;

procedure TMemoryRaster.DrawCross(Dst: TVec2; LineDist: Integer; Value: TRasterColor);
begin
  DrawCross(Round(Dst[0]), Round(Dst[1]), LineDist, Value);
end;

procedure TMemoryRaster.DrawPointListLine(pl: T2DPointList; Value: TRasterColor; wasClose: Boolean);
var
  I: Integer;
  p1, p2: P2DPoint;
begin
  if pl.Count < 2 then
      Exit;
  for I := 1 to pl.Count - 1 do
    begin
      p1 := pl[I - 1];
      p2 := pl[I];
      Line(p1^, p2^, Value, True);
    end;
  if wasClose then
    begin
      p1 := pl.First;
      p2 := pl.Last;
      Line(p1^, p2^, Value, True);
    end;
end;

procedure TMemoryRaster.Draw(DstX, DstY: Integer; Src: TMemoryRaster);
begin
  if Assigned(Src) then
      Src.DrawTo(Self, DstX, DstY);
end;

procedure TMemoryRaster.Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TMemoryRaster);
begin
  if Assigned(Src) then
      Src.DrawTo(Self, DstX, DstY, SrcRect);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster);
begin
  BlockTransfer(Dst, 0, 0, Dst.BoundsRect, Self, BoundsRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer; const SrcRect: TRect);
begin
  BlockTransfer(Dst, DstX, DstY, Dst.BoundsRect, Self, SrcRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer);
begin
  BlockTransfer(Dst, DstX, DstY, Dst.BoundsRect, Self, BoundsRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; DstPt: TVec2);
begin
  DrawTo(Dst, Round(DstPt[0]), Round(DstPt[1]));
end;

class function TMemoryRaster.CanLoadStream(Stream: TCoreClassStream): Boolean;
var
  bakPos: Int64;
  hflag: Word;
  Header: TBmpHeader;
begin
  Result := False;
  try
    bakPos := Stream.Position;

    Stream.Read(hflag, 2);
    if (hflag = $8D42) or (hflag = $8D43) or (hflag = $8D44) or (hflag = $8D45) or (hflag = $8DFF) then
        Result := True
    else
      begin
        Stream.Position := bakPos;

        Stream.ReadBuffer(Header, SizeOf(TBmpHeader));

        Result := (Header.bfType = $4D42) and
          (Header.biBitCount = 32) and (Header.biPlanes = 1) and
          (Header.biCompression = 0);
      end;

    Stream.Position := bakPos;
  except
  end;
end;

procedure TMemoryRaster.LoadFromBmpStream(Stream: TCoreClassStream);
var
  I, w, J: Integer;
  Header: TBmpHeader;
begin
  Reset;

  Stream.ReadBuffer(Header, SizeOf(TBmpHeader));

  // Check for Windows bitmap magic bytes and general compatibility of the
  // bitmap data that ought to be loaded...
  if (Header.bfType = $4D42) and
    (Header.biBitCount = 32) and (Header.biPlanes = 1) and
    (Header.biCompression = 0) then
    begin
      SetSize(Header.biWidth, Abs(Header.biHeight));

      // Check whether the bitmap is saved top-down
      if Header.biHeight > 0 then
        begin
          w := Width shl 2;
          for I := Height - 1 downto 0 do
            begin
              Stream.ReadBuffer(ScanLine[I]^, w);
            end;
        end
      else
        begin
          Stream.ReadBuffer(FBits^, (Width * Height) shl 2);
        end;
    end
  else
    begin
      raise CoreClassException.Create('bmp format failed!');
    end;
end;

procedure TMemoryRaster.LoadFromStream(Stream: TCoreClassStream);
var
  bakPos: Int64;

  hflag: Word;
  m64: TMemoryStream64;
begin
  Reset;

  bakPos := Stream.Position;

  Stream.Read(hflag, 2);
  if hflag = $8D42 then
    begin
      m64 := TMemoryStream64.Create;
      DecompressStream(Stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      Exit;
    end
  else if hflag = $8D43 then
    begin
      m64 := TMemoryStream64.Create;
      DeflateDecompressStream(Stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      Exit;
    end
  else if hflag = $8D44 then
    begin
      m64 := TMemoryStream64.Create;
      BRRCDecompressStream(Stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      Exit;
    end
  else if hflag = $4D42 then
    begin
      Stream.Position := bakPos;
      LoadFromBmpStream(Stream);
    end
    // jls endian support
  else if (hflag = $8DFF) then
    begin
      Stream.Position := bakPos;
      DecodeJpegLSRasterFromStream(Stream, Self);
    end
    // jls alpha
  else if hflag = $8D45 then
    begin
      Stream.Position := bakPos;
      DecodeJpegLSRasterAlphaFromStream(Stream, Self);
    end
  else
      Stream.Position := bakPos;
end;

procedure TMemoryRaster.LoadFromStreamAndResize(Stream: TCoreClassStream; const NewWidth, NewHeight: Integer);
var
  Bmp: TMemoryRaster;
begin
  Bmp := TMemoryRaster.Create;
  ZoomFrom(Bmp, NewWidth, NewHeight);
  DisposeObject(Bmp);
end;

procedure TMemoryRaster.SaveToBmpStream(Stream: TCoreClassStream);
var
  Header: TBmpHeader;
  BitmapSize: Integer;
  I, w: Integer;
begin
  BitmapSize := (FWidth * FHeight) shl 2;

  Header.bfType := $4D42; // Magic bytes for Windows Bitmap
  Header.bfSize := BitmapSize + SizeOf(TBmpHeader);
  Header.bfReserved := 0;
  // Save offset relative. However, the spec says it has to be file absolute,
  // which we can not do properly within a stream...
  Header.bfOffBits := SizeOf(TBmpHeader);
  Header.biSize := $28;
  Header.biWidth := Width;

  Header.biHeight := -Height;

  Header.biPlanes := 1;
  Header.biBitCount := 32;
  Header.biCompression := 0; // bi_rgb
  Header.biSizeImage := BitmapSize;
  Header.biXPelsPerMeter := 0;
  Header.biYPelsPerMeter := 0;
  Header.biClrUsed := 0;
  Header.biClrImportant := 0;

  Stream.WriteBuffer(Header, SizeOf(TBmpHeader));

  Stream.WriteBuffer(Bits^, BitmapSize);
end;

procedure TMemoryRaster.SaveToStream(Stream: TCoreClassStream);
begin
  SaveToBmpStream(Stream);
end;

procedure TMemoryRaster.SaveToZLibCompressStream(Stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D42; // MemoryRaster compress format
  Stream.Write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmpStream(m64);
  m64.Position := 0;
  MaxCompressStream(m64, Stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToDeflateCompressStream(Stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D43; // MemoryRaster compress format
  Stream.Write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmpStream(m64);
  m64.Position := 0;
  DeflateCompressStream(m64, Stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBRRCCompressStream(Stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D44; // MemoryRaster compress format
  Stream.Write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmpStream(m64);
  m64.Position := 0;
  BRRCCompressStream(m64, Stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS1Stream(Stream: TCoreClassStream);
begin
  EncodeJpegLSRasterToStream1(Self, Stream);
end;

procedure TMemoryRaster.SaveToJpegLS3Stream(Stream: TCoreClassStream);
begin
  EncodeJpegLSRasterToStream3(Self, Stream);
end;

procedure TMemoryRaster.SaveToJpegAlphaStream(Stream: TCoreClassStream);
begin
  EncodeJpegLSRasterAlphaToStream(Self, Stream);
end;

class function TMemoryRaster.CanLoadFile(fn: SystemString): Boolean;
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      Result := CanLoadStream(m64);
  except
      Result := False;
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.LoadFromFile(fn: SystemString);
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(m64);
  except
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.LoadFromFileAndResize(fn: SystemString; const NewWidth, NewHeight: Integer);
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStreamAndResize(m64, NewWidth, NewHeight);
  except
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToZLibCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToZLibCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToDeflateCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToDeflateCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBRRCCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToBRRCCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS1File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegLS1Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS3File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegLS3Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegAlphaFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegAlphaStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

constructor TSequenceMemoryRaster.Create;
begin
  inherited Create;
  FTotal := 1;
  FColumn := 1;
end;

destructor TSequenceMemoryRaster.Destroy;
begin
  inherited Destroy;
end;

procedure TSequenceMemoryRaster.Clear(FillColor: TRasterColor);
begin
  inherited Clear(FillColor);
  FTotal := 1;
  FColumn := 1;
end;

procedure TSequenceMemoryRaster.SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor);
begin
  inherited SetSize(NewWidth, NewHeight, ClearColor);
  FTotal := 1;
  FColumn := 1;
end;

procedure TSequenceMemoryRaster.Reset;
begin
  inherited Reset;
  FTotal := 1;
  FColumn := 1;
end;

procedure TSequenceMemoryRaster.Assign(sour: TMemoryRaster);
begin
  inherited Assign(sour);
  FTotal := 1;
  FColumn := 1;
  if sour is TSequenceMemoryRaster then
    begin
      FTotal := TSequenceMemoryRaster(sour).FTotal;
      FColumn := TSequenceMemoryRaster(sour).FColumn;
    end;
end;

class function TSequenceMemoryRaster.CanLoadStream(Stream: TCoreClassStream): Boolean;
var
  fp: Int64;
  hflag: Word;
  ATotal, AColumn: Integer;
begin
  Result := False;
  fp := Stream.Position;
  if Stream.Read(hflag, 2) <> 2 then
      Exit;
  try
    if hflag = $8888 then
      begin
        if Stream.Read(ATotal, 4) <> 4 then
            Exit;
        if Stream.Read(AColumn, 4) <> 4 then
            Exit;
        Result := inherited CanLoadStream(Stream);
        Stream.Position := fp;
      end
    else
      begin
        Stream.Position := fp;
        Result := inherited CanLoadStream(Stream);
      end;
  except
  end;
end;

procedure TSequenceMemoryRaster.LoadFromStream(Stream: TCoreClassStream);
var
  fp: Int64;
  hflag: Word;
  ATotal, AColumn: Integer;
  deStream: TMemoryStream64;
begin
  Reset;
  fp := Stream.Position;
  if Stream.Read(hflag, 2) <> 2 then
      Exit;
  if hflag = $8888 then
    begin
      if Stream.Read(ATotal, 4) <> 4 then
          Exit;
      if Stream.Read(AColumn, 4) <> 4 then
          Exit;
      inherited LoadFromStream(Stream);
      FTotal := ATotal;
      FColumn := AColumn;
    end
  else
    begin
      Stream.Position := fp;
      inherited LoadFromStream(Stream);
      FTotal := 1;
      FColumn := 1;
    end;
end;

procedure TSequenceMemoryRaster.SaveToStream(Stream: TCoreClassStream);
var
  hflag: Word;
  cStream: TMemoryStream64;
begin
  if FTotal > 1 then
    begin
      hflag := $8888;
      Stream.Write(hflag, 2);
      Stream.Write(FTotal, 4);
      Stream.Write(FColumn, 4);
      inherited SaveToZLibCompressStream(Stream);
      Exit;
    end;
  inherited SaveToStream(Stream);
end;

function TSequenceMemoryRaster.SequenceFrameRect(index: Integer): TRect;
begin
  Result := GetSequenceFrameRect(Self, Total, Column, index);
end;

procedure TSequenceMemoryRaster.ExportSequenceFrame(index: Integer; Output: TMemoryRaster);
begin
  GetSequenceFrameOutput(Self, Total, Column, index, Output);
end;

procedure TSequenceMemoryRaster.ReverseSequence(Output: TSequenceMemoryRaster);
var
  I: Integer;
  R: TRect;
begin
  Output.SetSize(Width, Height);
  for I := 0 to Total - 1 do
    begin
      R := SequenceFrameRect(I);
      BlockTransfer(Output, R.Left, R.Top, Output.BoundsRect, Self, SequenceFrameRect(Total - 1 - I), dmOpaque);
    end;
  Output.FTotal := FTotal;
  Output.FColumn := FColumn;
end;

procedure TSequenceMemoryRaster.GradientSequence(Output: TSequenceMemoryRaster);
var
  I, J: Integer;
  sr, dr: TRect;
begin
  Output.SetSize(FrameWidth * (Total * 2), FrameHeight);
  Output.Column := Total * 2;
  Output.Total := Output.Column;

  J := 0;

  for I := 0 to Total - 1 do
    begin
      dr := Output.SequenceFrameRect(J);
      sr := SequenceFrameRect(I);
      BlockTransfer(Output, dr.Left, dr.Top, Output.BoundsRect, Self, sr, dmOpaque);
      Inc(J);
    end;

  for I := Total - 1 downto 0 do
    begin
      dr := Output.SequenceFrameRect(J);
      sr := SequenceFrameRect(I);
      BlockTransfer(Output, dr.Left, dr.Top, Output.BoundsRect, Self, sr, dmOpaque);
      Inc(J);
    end;
end;

function TSequenceMemoryRaster.FrameWidth: Integer;
begin
  with SequenceFrameRect(0) do
      Result := Right - Left;
end;

function TSequenceMemoryRaster.FrameHeight: Integer;
begin
  with SequenceFrameRect(0) do
      Result := Bottom - Top;
end;

function TSequenceMemoryRaster.FrameRect2D: TRectV2;
begin
  Result := MakeRectV2(0, 0, FrameWidth, FrameHeight);
end;

function TSequenceMemoryRaster.FrameRect: TRect;
begin
  Result := Rect(0, 0, FrameWidth, FrameHeight);
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
            MoveCardinal(SrcP^, DstP^, w);
            Inc(SrcP, Src.Width);
            Inc(DstP, Dst.Width);
          end;
      end;
    dmBlend:
      if Src.MasterAlpha >= 255 then
        begin
          {$IFDEF FPC}
          if Src.CombineMode = cmBlend then
              bl := @BlendLine
          else
              bl := @MergeLine;
          {$ELSE}
          if Src.CombineMode = cmBlend then
              bl := BlendLine
          else
              bl := MergeLine;
          {$ENDIF}
          for DstY := DstRect.Top to DstRect.Bottom - 1 do
            begin
              bl(SrcP, DstP, w);
              Inc(SrcP, Src.Width);
              Inc(DstP, Dst.Width);
            end
        end
      else
        begin
          {$IFDEF FPC}
          if Src.CombineMode = cmBlend then
              ble := @BlendLineEx
          else
              ble := @MergeLineEx;
          {$ELSE}
          if Src.CombineMode = cmBlend then
              ble := BlendLineEx
          else
              ble := MergeLineEx;
          {$ENDIF}
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
  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

function RasterColorInv(const c: TRasterColor): TRasterColor;
begin
  Result := RasterColor(
    $FF - ClampByte(TRasterColorEntry(c).R, 0, $FF),
    $FF - ClampByte(TRasterColorEntry(c).G, 0, $FF),
    $FF - ClampByte(TRasterColorEntry(c).B, 0, $FF),
    TRasterColorEntry(c).A);
end;

function Red(const RasterColor: TRasterColor): byte;
begin
  Result := (RasterColor and $00FF0000) shr 16;
end;

function Green(const RasterColor: TRasterColor): byte;
begin
  Result := (RasterColor and $0000FF00) shr 8;
end;

function Blue(const RasterColor: TRasterColor): byte;
begin
  Result := RasterColor and $000000FF;
end;

function Alpha(const RasterColor: TRasterColor): byte;
begin
  Result := RasterColor shr 24;
end;

function RasterColorF(const R, G, B: Single; const A: Single = 1.0): TRasterColor;
begin
  Result := RasterColor(
    ClampByte(Round(R * $FF), 0, $FF),
    ClampByte(Round(G * $FF), 0, $FF),
    ClampByte(Round(B * $FF), 0, $FF),
    ClampByte(Round(A * $FF), 0, $FF));
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

function RasterColorD(const R, G, B: Double; const A: Double = 1.0): TRasterColor;
begin
  Result := RasterColor(
    ClampByte(Round(R * $FF), 0, $FF),
    ClampByte(Round(G * $FF), 0, $FF),
    ClampByte(Round(B * $FF), 0, $FF),
    ClampByte(Round(A * $FF), 0, $FF));
end;

procedure RasterColor2D(const c: TRasterColor; var R, G, B, A: Double);
begin
  R := TRasterColorEntry(c).R / $FF;
  G := TRasterColorEntry(c).G / $FF;
  B := TRasterColorEntry(c).B / $FF;
  A := TRasterColorEntry(c).A / $FF;
end;

procedure RasterColor2D(const c: TRasterColor; var R, G, B: Double);
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
          MoveCardinal(ImagePixel^, PixelS[RecLeft], RecRight - RecLeft + 1);

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
begin
  if not Assigned(Src) then
      raise CoreClassException.Create(SEmptySource);

  if not Assigned(Dst) then
      raise CoreClassException.Create(SEmptyDestination);

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

  if M = $FF then
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

procedure MakeMergeTables;
var
  I, J: Integer;
const
  OneByteth: Double = 1.0 / 255.0;
begin
  for J := 0 to 255 do
    for I := 0 to 255 do
      begin
        DivTable[I, J] := Round(I * J * OneByteth);
        if I > 0 then
            RcTable[I, J] := byte(Round(J * 255 / I))
        else
            RcTable[I, J] := 0;
      end;
end;

function _NewRaster: TMemoryRaster;
begin
  Result := TMemoryRaster.Create;
end;

function _NewRasterFromFile(const fn: string): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LoadFromFile(fn);
end;

function _NewRasterFromStream(const Stream: TCoreClassStream): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LoadFromStream(Stream);
end;

procedure _SaveRaster(mr: TMemoryRaster; const fn: string);
begin
  mr.SaveToFile(fn);
end;

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

finalization

end.
