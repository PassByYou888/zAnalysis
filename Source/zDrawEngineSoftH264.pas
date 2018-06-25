{ ****************************************************************************** }
{ * zDrawEngine H264 soft Rasterization                                        * }
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
unit zDrawEngineSoftH264;

{$I zDefine.inc}

interface

uses Math, Geometry3DUnit, ListEngine, PascalStrings, CoreClasses, zDrawEngine, UnicodeMixedLib, Geometry2DUnit,
  MemoryRaster, H264, h264image, h264StdInt;

type
  TDrawEngine_H264 = class(TDrawEngine_Raster)
  private
    FH264W: TH264Writer;
  public
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const filename: SystemString); overload;
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream); overload;

    destructor Destroy; override;

    procedure Progress(deltaTime: Double);

    procedure Flush; override;
    function FrameCount: uint32_t;
    function H264Size: Int64_t;
    function PerSecondFrame: TDEFloat;
  end;

implementation

constructor TDrawEngine_H264.Create(const w, h, totalframe: int32_t; psf: Single; const filename: SystemString);
var
  nw, nh: int32_t;
begin
  inherited Create;
  nw := w - (w mod 2);
  nh := h - (h mod 2);
  FH264W := TH264Writer.Create(nw, nh, totalframe, psf, filename);
  memory.SetSize(nw, nh);
end;

constructor TDrawEngine_H264.Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream);
var
  nw, nh: int32_t;
begin
  inherited Create;
  nw := w - (w mod 2);
  nh := h - (h mod 2);
  FH264W := TH264Writer.Create(nw, nh, totalframe, psf, stream);
  memory.SetSize(nw, nh);
end;

destructor TDrawEngine_H264.Destroy;
begin
  DisposeObject(FH264W);
  inherited Destroy;
end;

procedure TDrawEngine_H264.Progress(deltaTime: Double);
begin
  Engine.Progress(1 / FH264W.PerSecondFrame);
end;

procedure TDrawEngine_H264.Flush;
begin
  inherited Flush;
  FH264W.WriteFrame(memory);
  FH264W.Flush;
end;

function TDrawEngine_H264.FrameCount: uint32_t;
begin
  Result := FH264W.FrameCount;
end;

function TDrawEngine_H264.H264Size: Int64_t;
begin
  Result := FH264W.H264Size;
end;

function TDrawEngine_H264.PerSecondFrame: TDEFloat;
begin
  Result := FH264W.PerSecondFrame;
end;

end.
