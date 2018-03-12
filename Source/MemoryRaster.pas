{ ****************************************************************************** }
{ * memory Rasterization                                                       * }
{ * code Modification on the originate                                         * }
{ * https://github.com/graphics32/graphics32                                   * }
{ * modification by QQ 600585@qq.com                                           * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit MemoryRaster;

{$I ..\zDefine.inc}

interface

uses Types, Math, Variants, CoreClasses, Geometry2DUnit, PascalStrings, UnicodeMixedLib;

type
  TRasterColor = type Cardinal;
  PRasterColor = ^TRasterColor;

type
  PRasterColorArray   = ^TRasterColorArray;
  TRasterColorArray   = array [0 .. 0] of TRasterColor;
  TArrayOfRasterColor = array [0 .. 0] of TRasterColor;
  PArrayOfRasterColor = ^TArrayOfRasterColor;

  PRasterColorEntry = ^TRasterColorEntry;

  TRasterColorEntry = packed record
    case Integer of
      0: (R, G, B, A: Byte);
      1: (RGBA: TRasterColor);
  end;

  TDrawMode    = (dmOpaque, dmBlend, dmTransparent);
  TCombineMode = (cmBlend, cmMerge);

  TMemoryRaster = class(TCoreClassObject)
  private
    FBits          : PRasterColorArray;
    FWidth, FHeight: Integer;
    FDrawMode      : TDrawMode;
    FCombineMode   : TCombineMode;

    FMD5: UnicodeMixedLib.TMD5;

    FMasterAlpha: Cardinal;
    FOuterColor : TRasterColor;
    FData       : Variant;

    function GetPixel(X, Y: Integer): TRasterColor;
    procedure SetPixel(X, Y: Integer; const Value: TRasterColor);
    function GetPixelBGRA(X, Y: Integer): TRasterColor;
    procedure SetPixelBGRA(X, Y: Integer; const Value: TRasterColor);
    function GetPixelPtr(X, Y: Integer): PRasterColor;
    function GetScanLine(Y: Integer): PRasterColorArray;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear; overload;
    procedure Clear(FillColor: TRasterColor); overload; virtual;
    procedure SetSize(NewWidth, NewHeight: Integer); overload; virtual;
    procedure SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor); overload; virtual;
    function SizeOfPoint: TPoint;
    function SizeOf2DPoint: T2DPoint;
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
    procedure StretchFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    function FormatAsBGRA: TMemoryRaster;
    procedure ColorTransparent(c: TRasterColor);
    procedure ColorBlend(c: TRasterColor);

    procedure VertLine(X, Y1, Y2: Integer; Value: TRasterColor);
    procedure HorzLine(X1, Y, X2: Integer; Value: TRasterColor);
    procedure Line(X1, Y1, X2, Y2: Integer; Value: TRasterColor; L: Boolean);
    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TRasterColor);

    procedure Draw(DstX, DstY: Integer; Src: TMemoryRaster); overload;
    procedure Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstX, DstY: Integer); overload;

    class function CanLoadStream(Stream: TCoreClassStream): Boolean; virtual;
    procedure LoadFromBmpStream(Stream: TCoreClassStream);
    procedure LoadFromStream(Stream: TCoreClassStream); virtual;
    procedure SaveToBmpStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream); virtual;
    procedure SaveToZLibCompressStream(Stream: TCoreClassStream); virtual;
    procedure SaveToDeflateCompressStream(Stream: TCoreClassStream); virtual;
    procedure SaveToBRRCCompressStream(Stream: TCoreClassStream); virtual;

    class function CanLoadFile(fn: SystemString): Boolean;
    procedure LoadFromFile(fn: SystemString); virtual;
    procedure SaveToFile(fn: SystemString);
    procedure SaveToZLibCompressFile(fn: SystemString);

    property Pixel[X, Y: Integer]: TRasterColor read GetPixel write SetPixel; default;
    property PixelBGRA[X, Y: Integer]: TRasterColor read GetPixelBGRA write SetPixelBGRA;
    property PixelPtr[X, Y: Integer]: PRasterColor read GetPixelPtr;

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
    FTotal : Integer;
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
function RasterColor(R, G, B: Byte; A: Byte = $FF): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RasterColorF(R, G, B: Single; A: Single = 1.0): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RGBA2BGRA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function BGRA2RGBA(const sour: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function RedComponent(RasterColor: TRasterColor): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function GreenComponent(RasterColor: TRasterColor): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function BlueComponent(RasterColor: TRasterColor): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function AlphaComponent(RasterColor: TRasterColor): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure FastBlur(Bmp32: TMemoryRaster; Radius: Single; const Bounds: TRect);

procedure AlphaToGrayscale(Src: TMemoryRaster);
procedure IntensityToAlpha(Src: TMemoryRaster);
procedure ReversalAlpha(Src: TMemoryRaster);

procedure ColorToTransparent(SrcColor: TRasterColor; Src, Dst: TMemoryRaster);

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect; {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster); {$IFDEF INLINE_ASM}inline; {$ENDIF}

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

uses MemoryStream64, CoreCompress;

var
  RcTable : array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;

type
  TLUT8            = array [Byte] of Byte;
  TLogicalOperator = (loXOR, loAND, loOR);

  TByteArray = array [0 .. 32767] of Byte;
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
  p: PArrayOfRasterColor;
begin
  p := PArrayOfRasterColor(@X);
  for i := Count - 1 downto 0 do
      p^[i] := Value;
end;

procedure MoveCardinal(const Source; var dest; Count: Integer); {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Move(Source, dest, Count shl 2);
end;

function ClampInt(Value, Min, Max: Integer): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF}
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

procedure BlendBlock(Dst: TMemoryRaster; DstRect: TRect; Src: TMemoryRaster; SrcX, SrcY: Integer; CombineOp: TDrawMode);
var
  SrcP, DstP: PRasterColor;
  SP, DP    : PRasterColor;
  MC        : TRasterColor;
  w, i, DstY: Integer;
  bl        : TBlendLine;
  ble       : TBlendLineEx;
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

procedure BlockTransfer(
  Dst: TMemoryRaster; DstX: Integer; DstY: Integer; DstClip: TRect;
  Src: TMemoryRaster; SrcRect: TRect;
  CombineOp: TDrawMode);
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

function RasterColor(R, G, B: Byte; A: Byte = $FF): TRasterColor;
begin
  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

function RasterColorF(R, G, B: Single; A: Single = 1.0): TRasterColor;
begin
  Result := RasterColor(
    ClampInt(Round(G * 255), 0, 255),
    ClampInt(Round(G * 255), 0, 255),
    ClampInt(Round(B * 255), 0, 255),
    ClampInt(Round(A * 255), 0, 255));
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

function RedComponent(RasterColor: TRasterColor): Integer;
begin
  Result := (RasterColor and $00FF0000) shr 16;
end;

function GreenComponent(RasterColor: TRasterColor): Integer;
begin
  Result := (RasterColor and $0000FF00) shr 8;
end;

function BlueComponent(RasterColor: TRasterColor): Integer;
begin
  Result := RasterColor and $000000FF;
end;

function AlphaComponent(RasterColor: TRasterColor): Integer;
begin
  Result := RasterColor shr 24;
end;

procedure FastBlur(Bmp32: TMemoryRaster; Radius: Single; const Bounds: TRect);
type
  TSumRecord = packed record
    B, G, R, A: Integer;
    Sum: Integer;
  end;
var
  LL, RR, TT, BB, XX, YY, i, J, X, Y, RadiusI, Passes: Integer;
  RecLeft, RecTop, RecRight, RecBottom               : Integer;
  ImagePixel                                         : PRasterColorEntry;
  SumRec                                             : TSumRecord;
  ImgPixel                                           : PRasterColorEntry;
  Pixels                                             : array of TRasterColorEntry;
begin
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
  RecRight := Min(Bounds.Right, Bmp32.Width - 1);
  RecBottom := Min(Bounds.Bottom, Bmp32.Height - 1);

  SetLength(Pixels, Max(Bmp32.Width, Bmp32.Height) + 1);
  // pre-multiply alphas ...
  for Y := RecTop to RecBottom do
    begin
      ImgPixel := PRasterColorEntry(Bmp32.ScanLine[Y]);
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
          ImagePixel := PRasterColorEntry(@Bmp32.ScanLine[Y][RecLeft]);
          // fill the Pixels buffer with a copy of the row's pixels ...
          MoveCardinal(ImagePixel^, Pixels[RecLeft], RecRight - RecLeft + 1);

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
            with Pixels[XX] do
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
                with Pixels[LL] do
                  begin
                    Dec(SumRec.A, A);
                    Dec(SumRec.R, R);
                    Dec(SumRec.G, G);
                    Dec(SumRec.B, B);
                    Dec(SumRec.Sum);
                  end;
              if RR <= RecRight then
                with Pixels[RR] do
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
          ImagePixel := PRasterColorEntry(@Bmp32.ScanLine[RecTop][X]);
          for J := RecTop to RecBottom do
            begin
              Pixels[J] := ImagePixel^;
              Inc(ImagePixel, Bmp32.Width);
            end;
          ImagePixel := PRasterColorEntry(@Bmp32.ScanLine[RecTop][X]);

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
            with Pixels[YY] do
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
              Inc(ImagePixel, Bmp32.Width);
              TT := Y - RadiusI - 1;
              BB := Y + RadiusI;

              if TT >= RecTop then
                with Pixels[TT] do
                  begin
                    Dec(SumRec.A, A);
                    Dec(SumRec.R, R);
                    Dec(SumRec.G, G);
                    Dec(SumRec.B, B);
                    Dec(SumRec.Sum);
                  end;
              if BB <= RecBottom then
                with Pixels[BB] do
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
      ImgPixel := PRasterColorEntry(@Bmp32.ScanLine[Y][RecLeft]);
      for X := RecLeft to RecRight do
        begin
          ImgPixel^.R := RcTable[ImgPixel^.A, ImgPixel^.R];
          ImgPixel^.G := RcTable[ImgPixel^.A, ImgPixel^.G];
          ImgPixel^.B := RcTable[ImgPixel^.A, ImgPixel^.B];
          Inc(ImgPixel);
        end;
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

procedure ColorToTransparent(SrcColor: TRasterColor; Src, Dst: TMemoryRaster);
var
  i, J: Integer;
  A   : Byte;
  c   : TRasterColorEntry;
begin
  CheckParams(Src, Dst);
  A := AlphaComponent(SrcColor);
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
  c                    : TRasterColor;
  bmp                  : TMemoryRaster;
  AMaxWidth, AMaxHeight: Integer;
  i                    : Integer;
  idx, X, Y            : Integer;
  newbmp               : TMemoryRaster;
  rowcnt               : Integer;
begin
  if Column > bmp32List.Count then
      Column := bmp32List.Count;

  AMaxWidth := 0;
  AMaxHeight := 0;
  for i := 0 to bmp32List.Count - 1 do
    begin
      bmp := bmp32List[i] as TMemoryRaster;
      if Transparent then
          bmp.ColorTransparent(bmp[0, 0]);

      if bmp.Width > AMaxWidth then
          AMaxWidth := bmp.Width;
      if bmp.Height > AMaxHeight then
          AMaxHeight := bmp.Height;
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
      bmp := bmp32List[i] as TMemoryRaster;
      if (bmp.Width <> AMaxWidth) or (bmp.Height <> AMaxHeight) then
        begin
          newbmp := TMemoryRaster.Create;
          newbmp.StretchFrom(bmp, AMaxWidth, AMaxHeight);
          BlockTransfer(Result, X, Y, Result.BoundsRect, newbmp, newbmp.BoundsRect, dmOpaque);
          DisposeObject(newbmp);
        end
      else
        begin
          BlockTransfer(Result, X, Y, Result.BoundsRect, bmp, bmp.BoundsRect, dmOpaque);
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

function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect;
var
  rowIdx, colIdx : Integer;
  row            : Integer;
  AWidth, AHeight: Integer;
begin
  if Total <= 1 then
      Exit(bmp.BoundsRect);
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

  AWidth := bmp.Width div Column;
  AHeight := bmp.Height div row;

  Result := Rect(colIdx * AWidth, rowIdx * AHeight, (colIdx + 1) * AWidth, (rowIdx + 1) * AHeight);
end;

procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster);
var
  R   : TRect;
  w, h: Integer;
begin
  R := GetSequenceFrameRect(bmp, Total, Column, index);
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  output.SetSize(w, h);
  BlockTransfer(output, 0, 0, output.BoundsRect, bmp, R, dmOpaque);
end;

function BlendReg(F, B: TRasterColor): TRasterColor;
var
  FX    : TRasterColorEntry absolute F;
  BX    : TRasterColorEntry absolute B;
  Af, Ab: PByteArray;
  FA    : Byte;
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
  FX    : TRasterColorEntry absolute F;
  BX    : TRasterColorEntry absolute B;
  Af, Ab: PByteArray;
  FA    : Byte;
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
  FX    : TRasterColorEntry absolute F;
  BX    : TRasterColorEntry absolute B;
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
  FX    : TRasterColorEntry absolute F;
  BX    : TRasterColorEntry absolute B;
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
  Xe    : TRasterColorEntry absolute X;
  Ye    : TRasterColorEntry absolute Y;
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
  Xe    : TRasterColorEntry absolute X;
  Ye    : TRasterColorEntry absolute Y;
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
  Fw, Bw    : PByteArray;
  FX        : TRasterColorEntry absolute F;
  BX        : TRasterColorEntry absolute B;
  Rx        : TRasterColorEntry absolute Result;
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
            RcTable[i, J] := Round(J * 255 / i)
        else
            RcTable[i, J] := 0;
      end;
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
  Result := @(FBits^[X + Y * Width]);
end;

function TMemoryRaster.GetScanLine(Y: Integer): PRasterColorArray;
begin
  Result := @(FBits^[Y * FWidth]);
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
  i, J  : Integer;
  P1, P2: PRasterColor;
  tmp   : TRasterColor;
  w, W2 : Integer;
begin
  w := Width;
  { In-place flipping }
  P1 := PRasterColor(Bits);
  P2 := P1;
  Inc(P2, Width - 1);
  W2 := Width shr 1;
  for J := 0 to Height - 1 do
    begin
      for i := 0 to W2 - 1 do
        begin
          tmp := P1^;
          P1^ := P2^;
          P2^ := tmp;
          Inc(P1);
          Dec(P2);
        end;
      Inc(P1, w - W2);
      Inc(P2, w + W2);
    end;
end;

procedure TMemoryRaster.FlipVert;
var
  J, J2 : Integer;
  Buffer: PRasterColorArray;
  P1, P2: PRasterColor;
begin
  { in-place }
  J2 := Height - 1;
  GetMem(Buffer, Width shl 2);
  for J := 0 to Height div 2 - 1 do
    begin
      P1 := PixelPtr[0, J];
      P2 := PixelPtr[0, J2];
      MoveCardinal(P1^, Buffer^, Width);
      MoveCardinal(P2^, P1^, Width);
      MoveCardinal(Buffer^, P2^, Width);
      Dec(J2);
    end;
  FreeMem(Buffer);
end;

procedure TMemoryRaster.Rotate90;
var
  tmp       : TMemoryRaster;
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

  Assign(tmp);
end;

procedure TMemoryRaster.Rotate180;
var
  i, I2: Integer;
  tmp  : TRasterColor;
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
  tmp       : TMemoryRaster;
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

  Assign(tmp);
end;

procedure TMemoryRaster.StretchFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);

type
  TFloatColorEntry = packed record
    colorEntry: TRasterColorEntry;
    k: Double;
  end;

  function RasterColorEntryMul(buff: array of TFloatColorEntry): TRasterColorEntry;
    procedure ClampComponent(var Value: Double); {$IFDEF INLINE_ASM}inline; {$ENDIF}
    begin
      if Value < 0 then
          Value := 0
      else if Value > 1 then
          Value := 1;
    end;

  var
    D         : TFloatColorEntry;
    i         : Integer;
    R, G, B, A: Double;
    c         : TRasterColorEntry;
  begin
    R := 0;
    G := 0;
    B := 0;
    A := 0;
    for D in buff do
      begin
        R := R + ((D.colorEntry.R / 255) * D.k);
        G := G + ((D.colorEntry.G / 255) * D.k);
        B := B + ((D.colorEntry.B / 255) * D.k);
        A := A + ((D.colorEntry.A / 255) * D.k);
      end;
    ClampComponent(R);
    ClampComponent(G);
    ClampComponent(B);
    ClampComponent(A);
    Result.R := Round(R * 255);
    Result.G := Round(G * 255);
    Result.B := Round(B * 255);
    Result.A := Round(A * 255);
  end;

var
  i, J                        : Integer;
  SourceI, SourceJ            : Double;
  SourceIInt, SourceJInt      : Integer;
  SourceINext, SourceJNext    : Integer;
  SourceIOffset, SourceJOffset: Double;
  D1, D2, D3, D4              : TFloatColorEntry;
  dest                        : TRasterColorEntry;
begin
  SetSize(NewWidth, NewHeight);

  if (Source.Width > 1) and (Source.Width > 1) and (Width > 1) and (Width > 1) then
    for i := Width - 1 downto 0 do
      for J := 0 to Height - 1 do
        begin
          SourceI := (i / (Width - 1)) * (Source.Width - 1);
          SourceJ := (J / (Height - 1)) * (Source.Height - 1);

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

          Pixel[i, J] := RasterColorEntryMul([D1, D2, D3, D4]).RGBA;
        end;
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
  A   : Byte;
  ce  : TRasterColorEntry;
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

procedure TMemoryRaster.VertLine(X, Y1, Y2: Integer; Value: TRasterColor);
var
  i, NH, NL: Integer;
  p        : PRasterColor;
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
  p                       : PRasterColor;
  ChangedRect             : TRect;
begin
  ChangedRect := Rect(X1, Y1, X2, Y2);

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
  hflag : Word;
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
  Header : TBmpHeader;
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
  m64  : TMemoryStream64;
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

procedure TMemoryRaster.SaveToBmpStream(Stream: TCoreClassStream);
var
  Header    : TBmpHeader;
  BitmapSize: Integer;
  i, w      : Integer;
begin
  BitmapSize := Width * Height shl 2;

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
  m64  : TMemoryStream64;
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
  m64  : TMemoryStream64;
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
  m64  : TMemoryStream64;
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
  fp             : Int64;
  hflag          : Word;
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
  fp             : Int64;
  hflag          : Word;
  ATotal, AColumn: Integer;
  deStream       : TMemoryStream64;
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
  hflag  : Word;
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
  i, J  : Integer;
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


initialization

MakeMergeTables;

finalization

end.
