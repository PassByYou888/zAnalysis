unit ParallelProjectionFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  System.IOUtils,

  CoreClasses, PascalStrings, UPascalStrings, UnicodeMixedLib, TextParsing, ListEngine,
  DoStatusIO, Geometry2DUnit,
  MemoryStream64, MemoryRaster, zDrawEngine, zDrawEngineInterface_VCL;

type
  TParallelProjectionForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Memo: TMemo;
    Panel1: TPanel;
    ProjButton: TButton;
    leftSplitter: TSplitter;
    Timer: TTimer;
    CheckBox_bilinear_sampling: TCheckBox;
    procedure ProjButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatus_Backcall(Text_: SystemString; const ID: Integer);
  public
    { Public declarations }
    source: TRaster;
  end;

var
  ParallelProjectionForm: TParallelProjectionForm;

implementation

{$R *.dfm}


procedure TParallelProjectionForm.ProjButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      tmp: TRaster;
      r2: TRectV2;
      tk1, tk2: TTimeTick;
      n: U_String;
    begin
      TRasterVertex.Parallel := False;
      tmp := NewRaster();
      tmp.SetSize(Image1.Width * 4, Image1.Height * 4, RColorF(0, 0, 0));
      r2 := RectFit(source.BoundsRectV2, tmp.BoundsRectV2);
      tk1 := GetTimeTick;
      source.ProjectionTo(tmp, source.BoundsRectV2, r2, CheckBox_bilinear_sampling.Checked, 1.0);
      tk1 := GetTimeTick - tk1;
      tmp.Scale(0.25);
      n := TDrawEngine.RebuildTextColor(Format('目标分辨率 %d*%d' + #13#10 + '串行方式投影耗时: %d 毫秒' + #13#10 +
        '分辨率越低,串行化的工作效率提升越明显' + #13#10 + '小图片使用并行投影并不会提速',
        [Image1.Width * 4, Image1.Height * 4, tk1]), tsPascal, '', '', '', '', '|color(0.5,1,0.5))|', '||', '', '', '', '');
      tmp.DrawEngine.BeginCaptureShadow(Vec2(2, 2), 0.9);
      tmp.DrawEngine.DrawText(n, 20, RectEdge(tmp.DrawEngine.ScreenRectV2, -10), DEColor(1, 1, 1), False, Vec2(0, 0), 5);
      tmp.DrawEngine.EndCaptureShadow;
      tmp.DrawEngine.Flush;
      TCompute.Sync(procedure
        begin
          MemoryBitmapToBitmap(tmp, Image1.Picture.Bitmap);
        end);
      disposeObject(tmp);

      TRasterVertex.Parallel := True;
      tmp := NewRaster();
      tmp.SetSize(Image2.Width * 4, Image2.Height * 4, RColorF(0, 0, 0));
      r2 := RectFit(source.BoundsRectV2, tmp.BoundsRectV2);
      tk2 := GetTimeTick;
      source.ProjectionTo(tmp, source.BoundsRectV2, r2, CheckBox_bilinear_sampling.Checked, 1.0);
      tk2 := GetTimeTick - tk2;
      tmp.Scale(0.25);
      n := TDrawEngine.RebuildTextColor(Format('目标分辨率 %d*%d' + #13#10 + '并行方式投影耗时: %d 毫秒 相比串行提速 %f 倍' + #13#10 +
        '分辨率越高,并行工作效率提升越明显' + #13#10 + '光栅引擎处理高分辨图片会自动化切换到并行模式',
        [Image2.Width * 4, Image2.Height * 4, tk2, tk1 / tk2]), tsPascal, '', '', '', '', '|color(0.5,1,0.5))|', '||', '', '', '', '');
      tmp.DrawEngine.BeginCaptureShadow(Vec2(2, 2), 0.9);
      tmp.DrawEngine.DrawText(n, 20, RectEdge(tmp.DrawEngine.ScreenRectV2, -10), DEColor(1, 1, 1), False, Vec2(0, 0), 5);
      tmp.DrawEngine.EndCaptureShadow;
      tmp.DrawEngine.Flush;
      TCompute.Sync(procedure
        begin
          MemoryBitmapToBitmap(tmp, Image2.Picture.Bitmap);
        end);
      disposeObject(tmp);
    end);
end;

procedure TParallelProjectionForm.DoStatus_Backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TParallelProjectionForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_Backcall);
  source := nil;
  TCompute.RunP_NP(procedure
    begin
      source := NewRasterFromFile(umlCombineFileName(TPath.GetLibraryPath, 'lena.bmp'));
      DoStatus('prepare source rasterization done.');
    end);
end;

procedure TParallelProjectionForm.FormResize(Sender: TObject);
begin
  Image1.Width := (ClientWidth - leftSplitter.Width) div 2;
end;

procedure TParallelProjectionForm.TimerTimer(Sender: TObject);
begin
  DoStatus;
end;

end.
