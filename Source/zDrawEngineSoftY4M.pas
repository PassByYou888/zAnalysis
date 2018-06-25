{ ****************************************************************************** }
{ * zDrawEngine Yuv for mpeg soft Rasterization                                * }
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
unit zDrawEngineSoftY4M;

{$I zDefine.inc}

interface

uses Math, Geometry3DUnit, ListEngine, PascalStrings, CoreClasses, zDrawEngine, UnicodeMixedLib, Geometry2DUnit,
  MemoryRaster, Y4M, h264image, h264StdInt;

type
  TDrawEngine_YUV4MPEG = class(TDrawEngine_Raster)
  private
    FYW: TY4MWriter;
  public
    constructor Create(const w, h, psf: uint16_t; const filename: SystemString); overload;
    constructor Create(const w, h, psf: uint16_t; const stream: TCoreClassStream); overload;

    destructor Destroy; override;

    procedure Progress(deltaTime: Double);

    procedure Flush; override;
    function FrameCount: uint32_t;
    function Y4MSize: Int64_t;
    function PerSecondFrame: TDEFloat;
  end;

implementation

constructor TDrawEngine_YUV4MPEG.Create(const w, h, psf: uint16_t; const filename: SystemString);
var
  nw, nh: uint16_t;
begin
  inherited Create;
  nw := w - (w mod 2);
  nh := h - (h mod 2);
  FYW := TY4MWriter.Create(nw, nh, psf, filename);
  memory.SetSize(nw, nh);
end;

constructor TDrawEngine_YUV4MPEG.Create(const w, h, psf: uint16_t; const stream: TCoreClassStream);
var
  nw, nh: uint16_t;
begin
  inherited Create;
  nw := w - (w mod 2);
  nh := h - (h mod 2);
  FYW := TY4MWriter.Create(nw, nh, psf, stream);
  memory.SetSize(nw, nh);
end;

destructor TDrawEngine_YUV4MPEG.Destroy;
begin
  DisposeObject(FYW);
  inherited Destroy;
end;

procedure TDrawEngine_YUV4MPEG.Progress(deltaTime: Double);
begin
  Engine.Progress(1 / FYW.PerSecondFrame);
end;

procedure TDrawEngine_YUV4MPEG.Flush;
begin
  inherited Flush;
  FYW.WriteFrame(memory);
  FYW.Flush;
end;

function TDrawEngine_YUV4MPEG.FrameCount: uint32_t;
begin
  Result := FYW.FrameCount;
end;

function TDrawEngine_YUV4MPEG.Y4MSize: Int64_t;
begin
  Result := FYW.Y4MSize;
end;

function TDrawEngine_YUV4MPEG.PerSecondFrame: TDEFloat;
begin
  Result := FYW.PerSecondFrame;
end;

end.
