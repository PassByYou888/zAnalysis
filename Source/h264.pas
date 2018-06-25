{ ****************************************************************************** }
{ * h264 writer support       by qq600585                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit h264;

{$I zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryRaster,
  h264image, h264StdInt, h264Encoder, h264Parameters, Y4M;

type
  TH264Writer = class
  private
    ioHandle: TIOHnd;
    FFrameCount: uint32_t;
    img: TPlanarImage;
    param: TEncodingParameters;
    Encoder: TFevh264Encoder;
    buffer: PByte;
  public
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const filename: TPascalString); overload;
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream); overload;

    destructor Destroy; override;

    procedure WriteFrame(raster: TMemoryRaster);
    procedure WriteY4M(r: TY4MReader);
    procedure Flush;

    property FrameCount: uint32_t read FFrameCount;
    function H264Size: Int64_t;
    function Width: uint16_t;
    function Height: uint16_t;
    function PerSecondFrame: Single;
  end;

implementation

constructor TH264Writer.Create(const w, h, totalframe: int32_t; psf: Single; const filename: TPascalString);
begin
  inherited Create;
  umlFileCreate(filename, ioHandle);
  FFrameCount := 0;
  img := TPlanarImage.Create(w, h);
  param := TEncodingParameters.Create;
  param.SetStreamParams(w, h, totalframe, psf);
  param.AnalysisLevel := 2;
  Encoder := TFevh264Encoder.Create(param);
  buffer := GetMemory(w * h * 4);
end;

constructor TH264Writer.Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream);
begin
  inherited Create;
  umlFileCreateAsStream('stream', stream, ioHandle);
  FFrameCount := 0;
  img := TPlanarImage.Create(w, h);
  param := TEncodingParameters.Create;
  param.SetStreamParams(w, h, totalframe, psf);
  param.AnalysisLevel := 2;
  Encoder := TFevh264Encoder.Create(param);
  buffer := GetMemory(w * h * 4);
end;

destructor TH264Writer.Destroy;
begin
  FreeMemory(buffer);
  DisposeObject(param);
  DisposeObject(Encoder);
  DisposeObject(img);
  umlFileClose(ioHandle);
  inherited Destroy;
end;

procedure TH264Writer.WriteFrame(raster: TMemoryRaster);
var
  t1, t2, pixl, enl: TTimeTick;
  oSiz: uint32_t;
  ssd: int64;
begin
  if FFrameCount >= param.FrameCount then
      exit;

  t1 := GetTimeTick;
  img.LoadFromRaster(raster);
  t2 := GetTimeTick;
  pixl := t2 - t1;

  t1 := t2;
  Encoder.EncodeFrame(img, buffer, oSiz);
  t2 := GetTimeTick;
  enl := t2 - t1;

  umlFileWrite(ioHandle, oSiz, buffer^);

  inc(FFrameCount);
end;

procedure TH264Writer.WriteY4M(r: TY4MReader);
var
  i: int32_t;
  p_img: TPlanarImage;
  raster: TMemoryRaster;
begin
  raster := TMemoryRaster.Create;
  r.SeekFirstFrame;
  for i := r.CurrentFrame to r.FrameCount - 1 do
    begin
      p_img := r.ReadFrame;
      p_img.SaveToRaster(raster);
      WriteFrame(raster);
    end;
  DisposeObject(raster);
end;

procedure TH264Writer.Flush;
begin
  umlFileFlushWrite(ioHandle);
end;

function TH264Writer.H264Size: Int64_t;
begin
  result := umlFileSize(ioHandle);
end;

function TH264Writer.Width: uint16_t;
begin
  result := param.FrameWidth;
end;

function TH264Writer.Height: uint16_t;
begin
  result := param.FrameHeight;
end;

function TH264Writer.PerSecondFrame: Single;
begin
  result := param.FrameRate;
end;

end.
