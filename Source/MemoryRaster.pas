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

uses Types, Math, Variants, CoreClasses, Geometry2DUnit, PascalStrings, UnicodeMixedLib;

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
    function Bounds2DRect: T2DRect;
    function RebuildMD5: UnicodeMixedLib.TMD5;

    procedure Reset; virtual;
    procedure Assign(sour: TMemoryRaster); virtual;

    procedure FlipHorz;
    procedure FlipVert;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;

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
    procedure ColorTransparent(c: TRasterColor);
    procedure ColorBlend(c: TRasterColor);
    procedure Grayscale;

    procedure VertLine(X, Y1, Y2: Integer; Value: TRasterColor);
    procedure HorzLine(X1, Y, X2: Integer; Value: TRasterColor);
    procedure Line(X1, Y1, X2, Y2: Integer; Value: TRasterColor; L: Boolean); overload;
    procedure Line(p1, p2: T2DPoint; Value: TRasterColor; L: Boolean); overload;
    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TRasterColor);
    procedure DrawCross(DstX, DstY, LineDist: Integer; Value: TRasterColor);
    procedure DrawPointListLine(pl: T2DPointList; Value: TRasterColor; wasClose: Boolean);

    procedure Draw(DstX, DstY: Integer; Src: TMemoryRaster); overload;
    procedure Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer); overload;

    class function CanLoadStream(Stream: TCoreClassStream): Boolean; virtual;
    procedure LoadFromBmpStream(Stream: TCoreClassStream);
    procedure LoadFromStream(Stream: TCoreClassStream); virtual;
    procedure LoadFromStreamAndResize(Stream: TCoreClassStream; const NewWidth, NewHeight: Integer);

    procedure SaveToBmpStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream); virtual;
    procedure SaveToZLibCompressStream(Stream: TCoreClassStream); virtual;
    procedure SaveToDeflateCompressStream(Stream: TCoreClassStream); virtual;
    procedure SaveToBRRCCompressStream(Stream: TCoreClassStream); virtual;

    class function CanLoadFile(fn: SystemString): Boolean;
    procedure LoadFromFile(fn: SystemString); virtual;
    procedure LoadFromFileAndResize(fn: SystemString; const NewWidth, NewHeight: Integer);

    { save bitmap format file }
    procedure SaveToFile(fn: SystemString);

    { custom format }
    procedure SaveToZLibCompressFile(fn: SystemString);
    procedure SaveToDeflateCompressFile(fn: SystemString);
    procedure SaveToBRRCCompressFile(fn: SystemString);

    property Pixel[X, Y: Integer]: TRasterColor read GetPixel write SetPixel; default;
    property PixelBGRA[X, Y: Integer]: TRasterColor read GetPixelBGRA write SetPixelBGRA;
    property PixelPtr[X, Y: Integer]: PRasterColor read GetPixelPtr;
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
    procedure ExportSequenceFrame(index: Integer; output: TMemoryRaster);
    procedure ReverseSequence(output: TSequenceMemoryRaster);
    procedure GradientSequence(output: TSequenceMemoryRaster);
    function FrameWidth: Integer;
    function FrameHeight: Integer;
    function FrameRect2D: T2DRect;
    function FrameRect: TRect;
  end;

  TSequenceMemoryRasterClass = class of TSequenceMemoryRaster;

procedure BlendBlock(Dst: TMemoryRaster; DstRect: TRect; Src: TMemoryRaster; SrcX, SrcY: Integer; CombineOp: TDrawMode); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure BlockTransfer(Dst: TMemoryRaster; DstX: Integer; DstY: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);
function RasterColor(const R, G, B: byte; const A: byte = $FF): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RedComponent(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function GreenComponent(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function BlueComponent(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function AlphaComponent(const RasterColor: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function RasterColorF(const R, G, B: Single; const A: Single = 1.0): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RasterColor2F(const c: TRasterColor; var R, G, B, A: Single); {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColorD(const R, G, B: Double; const A: Double = 1.0): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure RasterColor2D(const c: TRasterColor; var R, G, B, A: Double); {$IFDEF INLINE_ASM}inline; {$ENDIF}

function RasterColor2Gray(const c: TRasterColor): byte; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColor2GrayS(const c: TRasterColor): Single; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColor2GrayD(const c: TRasterColor): Double; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RGBA2BGRA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function BGRA2RGBA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}

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
procedure GetSequenceFrameOutput(Bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster); {$IFDEF INLINE_ASM}inline; {$ENDIF}

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

implementation

uses
  {$IFDEF parallel}
  {$IFDEF FPC}
  mtprocs,
  {$ELSE}
  Threading,
  {$ENDIF FPC}
  {$ENDIF}
  MemoryStream64, CoreCompress;

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
  i: Integer;
  p: PRasterColorArray;
begin
  p := PRasterColorArray(@X);
  for i := Count - 1 downto 0 do
      p^[i] := Value;
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

function TMemoryRaster.GetGrayS(X, Y: Integer): Single;
begin
  Result := RasterColor2GrayS(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGrayS(X, Y: Integer; const Value: Single);
var
  p: PRasterColorEntry;
begin
  if X < 0 then
      X := 0
  else if X >= Width then
      X := Width - 1;

  if Y < 0 then
      Y := 0
  else if Y >= Height then
      Y := Height - 1;

  p := @FBits^[X + Y * Width];

  p^.R := ClampByte(Round(Value * $FF), 0, $FF);
  p^.G := p^.R;
  p^.B := p^.G;
  p^.A := $FF;
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

function TMemoryRaster.Bounds2DRect: T2DRect;
begin
  Result := Make2DRect(0, 0, Width, Height);
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
  i, J: Integer;
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
      for i := 0 to W2 - 1 do
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
  X, Y, i, J: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  i := 0;
  for Y := 0 to Height - 1 do
    begin
      J := Height - 1 - Y;
      for X := 0 to Width - 1 do
        begin
          tmp.Bits^[J] := Bits^[i];
          Inc(i);
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
  i, I2: Integer;
  tmp: TRasterColor;
begin
  I2 := Width * Height - 1;
  for i := 0 to Width * Height div 2 - 1 do
    begin
      tmp := Bits^[I2];
      Bits^[I2] := Bits^[i];
      Bits^[i] := tmp;
      Dec(I2);
    end;
end;

procedure TMemoryRaster.Rotate270;
var
  tmp: TMemoryRaster;
  X, Y, i, J: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  i := 0;
  for Y := 0 to Height - 1 do
    begin
      J := (Width - 1) * Height + Y;
      for X := 0 to Width - 1 do
        begin
          tmp.Bits^[J] := Bits^[i];
          Inc(i);
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
  i: Integer;

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
      for i := Width - 1 downto 0 do
          ZoomLine(Source, Self, i);
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
  i: Integer;
begin
  Result := TMemoryRaster.Create;
  GetMem(Result.FBits, Width * Height * 4);
  Result.FWidth := Width;
  Result.FHeight := Height;

  for i := (Width * Height) - 1 downto 0 do
      Result.FBits^[i] := RGBA2BGRA(FBits^[i]);
end;

procedure TMemoryRaster.ColorTransparent(c: TRasterColor);
var
  i, J: Integer;
  A: byte;
  ce: TRasterColorEntry;
begin
  ce.RGBA := c;
  A := ce.A;
  for i := 0 to Width - 1 do
    for J := 0 to Height - 1 do
      begin
        ce.RGBA := Pixel[i, J];
        ce.A := A;
        if ce.RGBA = c then
            Pixel[i, J] := RasterColor(0, 0, 0, 0);
      end;
end;

procedure TMemoryRaster.ColorBlend(c: TRasterColor);
var
  i, J: Integer;
begin
  for i := 0 to Width - 1 do
    for J := 0 to Height - 1 do
        Pixel[i, J] := BlendReg(Pixel[i, J], c);
end;

procedure TMemoryRaster.Grayscale;
begin
  RGBToGrayscale(Self);
end;

procedure TMemoryRaster.VertLine(X, Y1, Y2: Integer; Value: TRasterColor);
var
  i, NH, NL: Integer;
  p: PRasterColor;
begin
  if Y2 < Y1 then
      Exit;
  p := PixelPtr[X, Y1];
  i := Y2 - Y1 + 1;
  NH := i shr 2;
  NL := i and $03;
  for i := 0 to NH - 1 do
    begin
      p^ := Value;
      Inc(p, Width);
      p^ := Value;
      Inc(p, Width);
      p^ := Value;
      Inc(p, Width);
      p^ := Value;
      Inc(p, Width);
    end;
  for i := 0 to NL - 1 do
    begin
      p^ := Value;
      Inc(p, Width);
    end;
end;

procedure TMemoryRaster.HorzLine(X1, Y, X2: Integer; Value: TRasterColor);
begin
  FillRasterColor(Bits^[X1 + Y * Width], X2 - X1 + 1, Value);
end;

procedure TMemoryRaster.Line(X1, Y1, X2, Y2: Integer; Value: TRasterColor; L: Boolean);
var
  Dy, Dx, Sy, Sx, i, Delta: Integer;
  p: PRasterColor;
begin
  ClampInt(X1, 0, Width);
  ClampInt(X2, 0, Width);
  ClampInt(Y1, 0, Height);
  ClampInt(Y2, 0, Height);

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

    p := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    if Dx > Dy then
      begin
        Delta := Dx shr 1;
        for i := 0 to Dx - 1 do
          begin
            p^ := Value;
            Inc(p, Sx);
            Inc(Delta, Dy);
            if Delta >= Dx then
              begin
                Inc(p, Sy);
                Dec(Delta, Dx);
              end;
          end;
      end
    else // Dx < Dy
      begin
        Delta := Dy shr 1;
        for i := 0 to Dy - 1 do
          begin
            p^ := Value;
            Inc(p, Sy);
            Inc(Delta, Dx);
            if Delta >= Dy then
              begin
                Inc(p, Sx);
                Dec(Delta, Dy);
              end;
          end;
      end;
    if L then
        p^ := Value;
  except
  end;
end;

procedure TMemoryRaster.Line(p1, p2: T2DPoint; Value: TRasterColor; L: Boolean);
begin
  Line(Round(p1[0]), Round(p1[1]), Round(p2[0]), Round(p2[1]), Value, L);
end;

procedure TMemoryRaster.FillRect(X1, Y1, X2, Y2: Integer; Value: TRasterColor);
var
  J: Integer;
  p: PRasterColorArray;
begin
  if Assigned(FBits) then
    for J := Y1 to Y2 - 1 do
      begin
        p := Pointer(@Bits^[J * FWidth]);
        FillRasterColor(p[X1], X2 - X1, Value);
      end;
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

procedure TMemoryRaster.DrawPointListLine(pl: T2DPointList; Value: TRasterColor; wasClose: Boolean);
var
  i: Integer;
  p1, p2: P2DPoint;
begin
  if pl.Count < 2 then
      Exit;
  for i := 1 to pl.Count - 1 do
    begin
      p1 := pl[i - 1];
      p2 := pl[i];
      Line(Round(p1^[0]), Round(p1^[1]), Round(p2^[0]), Round(p2^[1]), Value, True);
    end;
  if wasClose then
    begin
      p1 := pl.First;
      p2 := pl.Last;
      Line(Round(p1^[0]), Round(p1^[1]), Round(p2^[0]), Round(p2^[1]), Value, True);
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
    if hflag = $8D42 then
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
  i, w, J: Integer;
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
          for i := Height - 1 downto 0 do
            begin
              Stream.ReadBuffer(ScanLine[i]^, w);
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
  else
      Stream.Position := bakPos;

  LoadFromBmpStream(Stream);
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
  i, w: Integer;
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
  inherited SaveToZLibCompressStream(Stream);
end;

function TSequenceMemoryRaster.SequenceFrameRect(index: Integer): TRect;
begin
  Result := GetSequenceFrameRect(Self, Total, Column, index);
end;

procedure TSequenceMemoryRaster.ExportSequenceFrame(index: Integer; output: TMemoryRaster);
begin
  GetSequenceFrameOutput(Self, Total, Column, index, output);
end;

procedure TSequenceMemoryRaster.ReverseSequence(output: TSequenceMemoryRaster);
var
  i: Integer;
  R: TRect;
begin
  output.SetSize(Width, Height);
  for i := 0 to Total - 1 do
    begin
      R := SequenceFrameRect(i);
      BlockTransfer(output, R.Left, R.Top, output.BoundsRect, Self, SequenceFrameRect(Total - 1 - i), dmOpaque);
    end;
  output.FTotal := FTotal;
  output.FColumn := FColumn;
end;

procedure TSequenceMemoryRaster.GradientSequence(output: TSequenceMemoryRaster);
var
  i, J: Integer;
  sr, dr: TRect;
begin
  output.SetSize(FrameWidth * (Total * 2), FrameHeight);
  output.Column := Total * 2;
  output.Total := output.Column;

  J := 0;

  for i := 0 to Total - 1 do
    begin
      dr := output.SequenceFrameRect(J);
      sr := SequenceFrameRect(i);
      BlockTransfer(output, dr.Left, dr.Top, output.BoundsRect, Self, sr, dmOpaque);
      Inc(J);
    end;

  for i := Total - 1 downto 0 do
    begin
      dr := output.SequenceFrameRect(J);
      sr := SequenceFrameRect(i);
      BlockTransfer(output, dr.Left, dr.Top, output.BoundsRect, Self, sr, dmOpaque);
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

function TSequenceMemoryRaster.FrameRect2D: T2DRect;
begin
  Result := Make2DRect(0, 0, FrameWidth, FrameHeight);
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
  w, i, DstY: Integer;
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
            for i := 0 to w - 1 do
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

function RasterColor(const R, G, B: byte; const A: byte = $FF): TRasterColor;
begin
  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

function RedComponent(const RasterColor: TRasterColor): byte;
begin
  Result := (RasterColor and $00FF0000) shr 16;
end;

function GreenComponent(const RasterColor: TRasterColor): byte;
begin
  Result := (RasterColor and $0000FF00) shr 8;
end;

function BlueComponent(const RasterColor: TRasterColor): byte;
begin
  Result := RasterColor and $000000FF;
end;

function AlphaComponent(const RasterColor: TRasterColor): byte;
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

procedure FastBlur(Source, dest: TMemoryRaster; Radius: Double; const Bounds: TRect);
type
  TSumRecord = packed record
    B, G, R, A, Sum: Integer;
  end;
var
  LL, RR, TT, BB, XX, YY, i, J, X, Y, RadiusI, Passes: Integer;
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

  setLength(PixelS, Max(dest.Width, dest.Height) + 1);

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

  for i := 1 to Passes do
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
  Q, i, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
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
  setLength(GaussLUT, KernelSize);
  for i := 0 to KernelSize - 1 do
      setLength(GaussLUT[i], ChannelSize);
  for i := 1 to RadiusI do
    begin
      RadiusRevSq := Round((Radius + 1 - i) * (Radius + 1 - i));
      for J := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - i][J] := RadiusRevSq * J;
          GaussLUT[RadiusI + i][J] := GaussLUT[RadiusI - i][J];
        end;
    end;
  RadiusSq := Round((Radius + 1) * (Radius + 1));
  for J := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][J] := RadiusSq * J;

  ImageWidth := Source.Width;
  setLength(SumArray, ImageWidth * Source.Height);

  ImagePixels := PRasterColorEntryArray(Source.Bits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.Height - 1);

  RowOffset := RecTop * ImageWidth;
  setLength(PreMulArray, Source.Width);
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

          i := Max(X - RadiusI, RecLeft);
          Q := i - (X - RadiusI);
          for i := i to Min(X + RadiusI, RecRight) do
            with PreMulArray[i] do
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

          i := Max(Y - RadiusI, RecTop);
          Q := i - (Y - RadiusI);
          for i := i to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + i * ImageWidth] do
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
  Q, i, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
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
  setLength(GaussLUT, KernelSize);
  for i := 0 to KernelSize - 1 do
      setLength(GaussLUT[i], ChannelSize);
  for i := 1 to RadiusI do
    begin
      RadiusRevSq := Round((Radius + 1 - i) * (Radius + 1 - i));
      for J := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - i][J] := RadiusRevSq * J;
          GaussLUT[RadiusI + i][J] := GaussLUT[RadiusI - i][J];
        end;
    end;
  RadiusSq := Round((Radius + 1) * (Radius + 1));
  for J := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][J] := RadiusSq * J;

  ImageWidth := Source.Width;
  setLength(SumArray, ImageWidth * Source.Height);

  ImagePixels := PRasterColorEntryArray(Source.Bits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.Height - 1);

  RowOffset := RecTop * ImageWidth;
  setLength(PreMulArray, Source.Width);
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

          i := Max(X - RadiusI, RecLeft);
          Q := i - (X - RadiusI);
          for i := i to Min(X + RadiusI, RecRight) do
            with PreMulArray[i] do
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

          i := Max(Y - RadiusI, RecTop);
          Q := i - (Y - RadiusI);
          for i := i to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + i * ImageWidth] do
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
  i: Integer;
begin
  if AAmount >= 1 then
    for i := 1 to AAmount do
        Antialias32(DestMR, 0, 0, DestMR.Width, DestMR.Height);
end;

procedure HistogramEqualize(const mr: TMemoryRaster);
var
  LHistogram: array [0 .. 255] of Cardinal;
  LMap: array [0 .. 255] of byte;
  i: Integer;
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

  for i := 0 to 255 do
    begin
      LHistogram[i] := 0;
      LHistogram[i] := 0;
      LHistogram[i] := 0;
    end;

  LPixelCount := mr.Width * mr.Height;

  // calculating histogram
  p := @mr.Bits[0];

  for i := 1 to LPixelCount do
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

  for i := 0 to 255 do
    begin
      LSum := LSum + LHistogram[i];
      LMap[i] := Round(LSum / (mr.Width * mr.Height * 3) * 255);
    end;

  // doing map
  p := @mr.Bits[0];

  for i := 1 to LPixelCount do
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
  LDepth2, i: Integer;
  LPixel: PRasterColorEntry;
begin
  LDepth2 := Depth * 2;
  LPixel := @mr.Bits[0];

  for i := 0 to (mr.Width * mr.Height - 1) do
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
  i, X, Y, ix, iy, Dx: Integer;
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
    i, J: Integer;
  begin
    for J := 0 to 24 do
      begin
        LMatrix[J] := 0;
      end;

    i := 0;

    for J := 6 to 8 do
      begin
        LMatrix[J] := AMatrix[i];
        Inc(i);
      end;

    for J := 11 to 13 do
      begin
        LMatrix[J] := AMatrix[i];
        Inc(i);
      end;

    for J := 16 to 18 do
      begin
        LMatrix[J] := AMatrix[i];
        Inc(i);
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
  setLength(LOriginalRow, DestMR.Height);
  setLength(LDestRow, DestMR.Height);

  LMRCopy := TMemoryRaster.Create;
  try
    LMRCopy.Assign(DestMR);

    for i := 0 to (DestMR.Height - 1) do
      begin
        LOriginalRow[i] := LMRCopy.ScanLine[i];
        LDestRow[i] := DestMR.ScanLine[i];
      end;

    if LLastMatrixNumber = 0 then
      begin
        LLastMatrixNumber := 1;
      end;

    Dx := 0;
    for i := 0 to 24 do
      begin
        if (LMatrix[i] and MASK_MATRIX[i]) <> 0 then
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

                    i := 12 + LDiagonalY * 5 + LDiagonalX;
                    aa := aa + A * LMatrix[i];
                    RR := RR + R * LMatrix[i];
                    gg := gg + G * LMatrix[i];
                    BB := BB + B * LMatrix[i];
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
    setLength(LDestRow, 0);
    setLength(LOriginalRow, 0);
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
  i: Integer;
  c: PRasterColorEntry;
begin
  for i := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.R := c^.A;
      c^.G := c^.A;
      c^.B := c^.A;
    end;
end;

procedure IntensityToAlpha(Src: TMemoryRaster);
var
  i: Integer;
  c: PRasterColorEntry;
  F: Single;
begin
  for i := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.A := ((c^.R * 61 + c^.G * 174 + c^.B * 21) shr 8);
    end;
end;

procedure ReversalAlpha(Src: TMemoryRaster);
var
  i: Integer;
  c: PRasterColorEntry;
begin
  for i := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.A := $FF - c^.A;
    end;
end;

procedure RGBToGrayscale(Src: TMemoryRaster);
var
  i: Integer;
  c: PRasterColorEntry;
begin
  for i := (Src.Width * Src.Height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.R := RasterColor2Gray(c^.RGBA);
      c^.G := c^.R;
      c^.B := c^.R;
    end;
end;

procedure ColorToTransparent(SrcColor: TRasterColor; Src, Dst: TMemoryRaster);
var
  i, J: Integer;
  c: TRasterColorEntry;
begin
  CheckParams(Src, Dst);
  for i := 0 to Src.Width - 1 do
    for J := 0 to Src.Height - 1 do
      begin
        c.RGBA := Src[i, J];
        if c.RGBA = SrcColor then
            Dst[i, J] := RasterColor(0, 0, 0, 0)
        else
            Dst[i, J] := c.RGBA;
      end;
end;

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
var
  c: TRasterColor;
  Bmp: TMemoryRaster;
  AMaxWidth, AMaxHeight: Integer;
  i: Integer;
  idx, X, Y: Integer;
  newbmp: TMemoryRaster;
  rowcnt: Integer;
begin
  if Column > bmp32List.Count then
      Column := bmp32List.Count;

  AMaxWidth := 0;
  AMaxHeight := 0;
  for i := 0 to bmp32List.Count - 1 do
    begin
      Bmp := bmp32List[i] as TMemoryRaster;
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

  for i := 0 to bmp32List.Count - 1 do
    begin
      Bmp := bmp32List[i] as TMemoryRaster;
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

procedure GetSequenceFrameOutput(Bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster);
var
  R: TRect;
  w, h: Integer;
begin
  R := GetSequenceFrameRect(Bmp, Total, Column, index);
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  output.SetSize(w, h);
  BlockTransfer(output, 0, 0, output.BoundsRect, Bmp, R, dmOpaque);
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

procedure MakeMergeTables;
var
  i, J: Integer;
const
  OneByteth: Double = 1.0 / 255.0;
begin
  for J := 0 to 255 do
    for i := 0 to 255 do
      begin
        DivTable[i, J] := Round(i * J * OneByteth);
        if i > 0 then
            RcTable[i, J] := byte(Round(J * 255 / i))
        else
            RcTable[i, J] := 0;
      end;
end;

initialization

MakeMergeTables;

finalization

end.
