{ ****************************************************************************** }
{ * Yuv for Mpeg support       by qq600585                                     * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit Y4M;

{$I zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, h264image, h264StdInt, MemoryRaster;

type
  TY4MReader = class
  private
    width, height: uint16_t;
    frame_count, current_frame: uint32_t;
    frame_rate: double;

    fileHandle: TIOHnd;
    file_header_size, frame_header_size: word;
    frame_size: uint32_t;
    img: TPlanarImage;
    procedure ParseHeader;
  public
    constructor Create(const filename: string); overload;
    constructor Create(const stream: TCoreClassStream; const autoFreeSteam: Boolean); overload;
    destructor Destroy; override;

    function ReadFrame: TPlanarImage;

    property FrameWidth: uint16_t read width;
    property FrameHeight: uint16_t read height;
    property FrameCount: uint32_t read frame_count;
    property CurrentFrame: uint32_t read current_frame;
    property FrameRate: double read frame_rate;
  end;

  TY4MWriter = class
  private
    fileHandle: TIOHnd;
    frame_count: uint32_t;
    img: TPlanarImage;
  public
    (*
      w: frame width
      h: frame height
      psf: per second frame
      filename: output filename
    *)
    constructor Create(const w, h, psf: uint16_t; const filename: string); overload;

    (*
      w: frame width
      h: frame height
      psf: per second frame
      stream: output stream
    *)
    constructor Create(const w, h, psf: uint16_t; const stream: TCoreClassStream); overload;

    destructor Destroy; override;

    procedure WriteFrame(raster: TMemoryRaster);

    property FrameCount: uint32_t read frame_count;
    function Y4MSize: Int64_t;
  end;

implementation

type
  TY4MToken    = array [0 .. 8] of uint8_t;
  TFRAME_MAGIC = array [0 .. 5] of uint8_t;

var
  Y4M_Token: TY4MToken      = ($59, $55, $56, $34, $4D, $50, $45, $47, $32);
  FRAME_MAGIC: TFRAME_MAGIC = ($46, $52, $41, $4D, $45, $0A);
  FRAME_MAGIC_SIZE: int32_t = 6;

procedure TY4MReader.ParseHeader;
var
  i, num, denom: integer;
  c, param_c: uint8_t;
  token: TY4MToken;
  s: TPascalString;
begin
  umlBlockRead(fileHandle, token[0], 9);

  if not CompareMemory(@token[0], @Y4M_Token[0], 9) then
      RaiseInfo('Not a Y4M format');

  umlBlockRead(fileHandle, c, 1);

  repeat
    umlBlockRead(fileHandle, param_c, 1);
    s := '';
    umlBlockRead(fileHandle, c, 1);
    repeat
      s.Append(SystemChar(c));
      umlBlockRead(fileHandle, c, 1);
    until (c = 10) or (c = 32);
    case param_c of
      ord('W'):
        width := umlStrToInt(s);
      ord('H'):
        height := umlStrToInt(s);
      ord('F'):
        begin
          num := umlStrToInt(umlGetFirstStr(s, ':'));
          denom := umlStrToInt(umlGetLastStr(s, ':'));
          frame_rate := num / denom;
        end;
    end;
  until c = 10;

  file_header_size := umlFilePOS(fileHandle);
  frame_header_size := 6;
end;

constructor TY4MReader.Create(const filename: string);
begin
  inherited Create;
  frame_count := 0;
  current_frame := 0;

  umlFileOpen(filename, fileHandle, True);
  ParseHeader;

  frame_size := width * height + (width * height div 2);
  frame_count := (umlFileSize(fileHandle) - file_header_size) div (FRAME_MAGIC_SIZE + int64(frame_size));

  img := TPlanarImage.Create(width, height);
end;

constructor TY4MReader.Create(const stream: TCoreClassStream; const autoFreeSteam: Boolean);
begin
  inherited Create;
  frame_count := 0;
  current_frame := 0;

  umlFileOpenAsStream('stream', stream, fileHandle, True);
  fileHandle.AutoFree := autoFreeSteam;
  ParseHeader;

  frame_size := width * height + (width * height div 2);
  frame_count := (umlFileSize(fileHandle) - file_header_size) div (FRAME_MAGIC_SIZE + int64(frame_size));

  img := TPlanarImage.Create(width, height);
end;

destructor TY4MReader.Destroy;
begin
  DisposeObject(img);
  umlFileClose(fileHandle);
  inherited Destroy;
end;

function TY4MReader.ReadFrame: TPlanarImage;
var
  magic: TFRAME_MAGIC;
begin
  umlBlockRead(fileHandle, magic[0], FRAME_MAGIC_SIZE);
  if not CompareMemory(@magic[0], @FRAME_MAGIC[0], FRAME_MAGIC_SIZE) then
      RaiseInfo('Not a Y4M Frame');
  umlBlockRead(fileHandle, img.plane[0]^, frame_size);
  inc(current_frame);
  img.frame_num := current_frame;
  result := img;
end;

constructor TY4MWriter.Create(const w, h, psf: uint16_t; const filename: string);
var
  s: TPascalString;
  b: TBytes;
begin
  inherited Create;
  s := PFormat('YUV4MPEG2 W%d H%d F%d:1 Ip A0:0' + #10, [w, h, psf]);
  s.FastGetBytes(b);

  umlFileCreate(filename, fileHandle);
  umlBlockWrite(fileHandle, b[0], length(b));

  SetLength(b, 0);
  s := '';

  frame_count := 0;
  img := TPlanarImage.Create(w, h);
end;

constructor TY4MWriter.Create(const w, h, psf: uint16_t; const stream: TCoreClassStream);
var
  s: TPascalString;
  b: TBytes;
begin
  inherited Create;
  s := PFormat('YUV4MPEG2 W%d H%d F%d:1 Ip A0:0' + #10, [w, h, psf]);
  s.FastGetBytes(b);

  umlFileCreateAsStream('stream', stream, fileHandle);
  umlBlockWrite(fileHandle, b[0], length(b));

  SetLength(b, 0);
  s := '';

  frame_count := 0;
  img := TPlanarImage.Create(w, h);
end;

destructor TY4MWriter.Destroy;
begin
  DisposeObject(img);
  umlFileClose(fileHandle);
  inherited Destroy;
end;

procedure TY4MWriter.WriteFrame(raster: TMemoryRaster);
var
  frame_size: int32_t;
begin
  umlBlockWrite(fileHandle, FRAME_MAGIC[0], FRAME_MAGIC_SIZE);
  frame_size := img.width * img.height + (img.width * img.height div 2);
  img.LoadFromRaster(raster);
  umlBlockWrite(fileHandle, img.plane[0]^, frame_size);
  inc(frame_count);
end;

function TY4MWriter.Y4MSize: Int64_t;
begin
  result := umlFileSize(fileHandle);
end;

end.
