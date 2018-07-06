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

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryRaster,
  h264Image, h264Stdint, h264Encoder, h264Parameters, Y4M;

type
  TH264Writer = class
  private
    ioHandle: TIOHnd;
    FFrameCount: uint32_t;
    img: TPlanarImage;
    Param: TEncodingParameters;
    encoder: TFevh264Encoder;
    buffer: PByte;
  public
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const fileName: TPascalString); overload;
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream); overload;

    destructor Destroy; override;

    procedure WriteFrame(raster: TMemoryRaster);
    procedure WriteY4M(R: TY4MReader);
    procedure Flush;

    property FrameCount: uint32_t read FFrameCount;
    function H264Size: Int64_t;
    function width: uint16_t;
    function height: uint16_t;
    function PerSecondFrame: Single;
  end;

implementation

constructor TH264Writer.Create(const w, h, totalframe: int32_t; psf: Single; const fileName: TPascalString);
begin
  inherited Create;
  umlFileCreate(fileName, ioHandle);
  FFrameCount := 0;
  img := TPlanarImage.Create(w, h);
  Param := TEncodingParameters.Create;
  Param.SetStreamParams(w, h, totalframe, psf);
  Param.AnalysisLevel := 2;
  encoder := TFevh264Encoder.Create(Param);
  buffer := GetMemory(w * h * 4);
end;

constructor TH264Writer.Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream);
begin
  inherited Create;
  umlFileCreateAsStream('stream', stream, ioHandle);
  FFrameCount := 0;
  img := TPlanarImage.Create(w, h);
  Param := TEncodingParameters.Create;
  Param.SetStreamParams(w, h, totalframe, psf);
  Param.AnalysisLevel := 2;
  encoder := TFevh264Encoder.Create(Param);
  buffer := GetMemory(w * h * 4);
end;

destructor TH264Writer.Destroy;
begin
  FreeMemory(buffer);
  DisposeObject(Param);
  DisposeObject(encoder);
  DisposeObject(img);
  umlFileClose(ioHandle);
  inherited Destroy;
end;

procedure TH264Writer.WriteFrame(raster: TMemoryRaster);
var
  t1, t2, pixl, enl: TTimeTick;
  oSiz: uint32_t;
  ssd: Int64;
begin
  if FFrameCount >= Param.FrameCount then
      Exit;

  t1 := GetTimeTick;
  img.LoadFromRaster(raster);
  t2 := GetTimeTick;
  pixl := t2 - t1;

  t1 := t2;
  encoder.EncodeFrame(img, buffer, oSiz);
  t2 := GetTimeTick;
  enl := t2 - t1;

  umlFileWrite(ioHandle, oSiz, buffer^);

  Inc(FFrameCount);
end;

procedure TH264Writer.WriteY4M(R: TY4MReader);
var
  i: int32_t;
  p_img: TPlanarImage;
  raster: TMemoryRaster;
begin
  raster := TMemoryRaster.Create;
  R.SeekFirstFrame;
  for i := R.CurrentFrame to R.FrameCount - 1 do
    begin
      p_img := R.ReadFrame;
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
  Result := umlFileSize(ioHandle);
end;

function TH264Writer.width: uint16_t;
begin
  Result := Param.FrameWidth;
end;

function TH264Writer.height: uint16_t;
begin
  Result := Param.FrameHeight;
end;

function TH264Writer.PerSecondFrame: Single;
begin
  Result := Param.FrameRate;
end;

end.  
 
