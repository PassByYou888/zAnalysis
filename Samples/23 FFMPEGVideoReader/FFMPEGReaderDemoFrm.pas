unit FFMPEGReaderDemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  System.IOUtils,

  CoreClasses, PascalStrings, UnicodeMixedLib, TextParsing,
  Geometry2DUnit, zDrawEngine, MemoryRaster, zDrawEngineInterface_SlowFMX,
  FFMPEG, FFMPEG_Reader, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFFMPEGReaderDemoForm = class(TForm)
    Timer1: TTimer;
    FitDrawCheckBox: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    drawIntf: TDrawEngineInterface_FMX;
    raster: TDETexture;
    fr: TFFMPEG_Reader;
  end;

var
  FFMPEGReaderDemoForm: TFFMPEGReaderDemoForm;

implementation

{$R *.fmx}


procedure TFFMPEGReaderDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EnginePool.Clear;
  DisposeObject([drawIntf, raster]);
end;

procedure TFFMPEGReaderDemoForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  raster := DefaultTextureClass.Create;

  // Reader只能解码视频无法解码音频，如果视频流包含音频，会直接做忽略处理
  // 如果是rtsp串流给 rtsp://用户:密码@地址:端口
  // 如果是rtmp串流给 rtmp://用户:密码@地址:端口
  // 如果是视频文件，直接给文件名即可
  fr := TFFMPEG_Reader.Create(umlCombineFileName(TPath.GetLibraryPath, 'market.mp4').Text);
end;

procedure TFFMPEGReaderDemoForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
  fi, fj: TGeoFloat;
  n: SystemString;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [];

  d.FillBox(d.ScreenRect, DEColor(0, 0, 0));
  while not fr.ReadFrame(raster, False) do
    begin
      fr.Seek(0);
      fr.Current_Frame := 0;
    end;
  raster.ReleaseGPUMemory;

  if FitDrawCheckBox.IsChecked then
      d.FitDrawPicture(raster, raster.BoundsRectV2, d.ScreenRect, 1.0)
  else
      d.DrawPicture(raster, raster.BoundsRectV2, d.ScreenRect, 1.0);

  n := Format('%d * %d' + #13#10 + 'time:%f:%f' + #13#10 + 'frame:%d:%d',
    [fr.Width, fr.Height, fr.Current, fr.Total, fr.Current_Frame, fr.CurrentStream_Total_Frame]);
  n := d.RebuildTextColor(n, tsText, '', '', '', '', '|color(0.5,1,0.5)|', '||', '', '', '', '');
  d.BeginCaptureShadow(Vec2(1, 1), 1.0);
  d.DrawText(n, 24, d.ScreenRect, DEColor(1, 1, 1, 1), False);
  d.EndCaptureShadow;
  d.Flush;
end;

procedure TFFMPEGReaderDemoForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress;
  Invalidate;
end;

end.
