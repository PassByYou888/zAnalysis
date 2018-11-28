unit pyramidPictureCopmpareFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls,
  PascalStrings, MemoryRaster, Learn, LearnTypes, ZDBLocalManager, ZDBEngine,
  CoreClasses, DoStatusIO, ObjectData,
  UnicodeMixedLib, MemoryStream64, PyramidSpace,
  zDrawEngineInterface_SlowFMX, FMX.Layouts, FMX.ExtCtrls, FMX.Edit, FMX.EditBox, FMX.SpinBox;

type
  TForm1 = class(TForm)
    WriteDBButton: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    LogMemo: TMemo;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    MatchDBButton: TButton;
    OpenDialog2: TOpenDialog;
    TabItem2: TTabItem;
    ImageViewer1: TImageViewer;
    ImageViewer2: TImageViewer;
    Splitter1: TSplitter;
    Layout1: TLayout;
    SpinBox: TSpinBox;
    Label1: TLabel;
    Label2: TLabel;
    TabItem3: TTabItem;
    ImageViewer3: TImageViewer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MatchDBButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WriteDBButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ZDB: TZDBLocalManager;
    featureDB: TZDBStoreEngine;
    workerTh: Integer;
    procedure backcall_dostatus(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(ZDB);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHookM(self, backcall_dostatus);
  ZDB := TZDBLocalManager.Create;
  featureDB := ZDB.InitNewDB('feature');
  workerTh := 0;
end;

procedure TForm1.MatchDBButtonClick(Sender: TObject);
var
  w, h: TLInt;
  QueryFeature: TFeature;
  MatchedFeatureCount: TLInt;
  mr1, mr2: TMemoryRaster;
  successed: Boolean;
begin
  if not OpenDialog2.Execute then
      exit;

  AtomInc(workerTh);

  MatchedFeatureCount := Round(SpinBox.Value);

  mr1 := NewRasterFromFile(OpenDialog2.FileName);

  w := mr1.width;
  h := mr1.height;
  ComputeSamplerSize(w, h);
  mr1.Zoom(w, h);

  QueryFeature := TFeature.CreateWithRaster(mr1);
  QueryFeature.LinkRaster := mr1;
  QueryFeature.LinkRaster.DrawText(Format('origin picture feature: %d', [QueryFeature.Count]), 0, 0, 10, RasterColorF(1, 0.5, 0.5, 1));
  MemoryBitmapToBitmap(QueryFeature.LinkRaster, ImageViewer1.Bitmap);

  mr2 := NewRaster;
  mr2.SetSize(mr1.width, mr1.height, RasterColor(0, 0, 0));
  mr2.DrawText(Format('not matched', []), 0, 0, 10, RasterColorF(1, 0, 0, 1));
  MemoryBitmapToBitmap(mr2, ImageViewer2.Bitmap);
  DisposeObject(mr2);

  successed := False;

  featureDB.WaitQueryP(
    procedure(var qState: TQueryState)
    var
      f: TFeature;
      mi: TArrayMatchInfo;
      c: TLInt;
      mr3: TMemoryRaster;
    begin
      if qState.ID <> 999 then
          exit;

      f := TFeature.Create;
      f.LoadFromStream(qState.Cache);

      c := MatchFeature(QueryFeature, f, mi);

      if c > MatchedFeatureCount then
        begin
          MatchedFeatureCount := c;
          f.LinkRaster := NewRasterFromStream(qState.NextCache);
          f.LinkRaster.DrawText(Format('dest feature: %d matched: %d', [f.Count, c]), 0, 0, 10, RasterColorF(1, 0.5, 0.5, 1));
          MemoryBitmapToBitmap(f.LinkRaster, ImageViewer2.Bitmap);
          DisposeObject(f.LinkRaster);
          successed := True;
        end
      else
        begin
        end;
      DisposeObject(f);
      Application.ProcessMessages;
    end);

  DisposeObject(QueryFeature.LinkRaster);
  DisposeObject(QueryFeature);

  AtomDec(workerTh);

  if not successed then
      ShowMessage('匹配不成功');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  ZDB.Progress;
  DoStatus;
  WriteDBButton.Enabled := workerTh = 0;
  MatchDBButton.Enabled := WriteDBButton.Enabled;
end;

procedure TForm1.WriteDBButtonClick(Sender: TObject);
var
  i: Integer;
  p1: PPascalString;
begin
  if not OpenDialog1.Execute then
      exit;
  for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      while workerTh > 4 do
          Application.ProcessMessages;

      AtomInc(workerTh);

      new(p1);
      p1^ := OpenDialog1.Files[i];
      TComputeThread.RunP(p1, nil,
        procedure(Sender: TComputeThread)
        var
          w, h: TLInt;
          p2: PPascalString;
          mr: TMemoryRaster;
          ft: TFeature;
          ms1, ms2: TMemoryStream64;
        begin
          p2 := Sender.UserData;
          mr := NewRasterFromFile(p2^);
          w := mr.width;
          h := mr.height;
          ComputeSamplerSize(w, h);
          mr.Zoom(w, h);

          ft := TFeature.CreateWithRaster(mr);
          AtomDec(workerTh);

          ms1 := TMemoryStream64.Create;
          ft.SaveToStream(ms1);

          ms2 := TMemoryStream64.Create;
          mr.SaveToJpegLS3Stream(ms2);
          DisposeObject(mr);

          DoStatus('build %s to feature fc:%d f:%s p:%s',
            [umlGetFileName(p2^).text, ft.Count, umlSizeToStr(ms1.Size).text, umlSizeToStr(ms2.Size).text]);
          DisposeObject(ft);
          TThread.Synchronize(TThread.CurrentThread, procedure
            begin
              featureDB.AddData(ms1, 999); // feature
              featureDB.AddData(ms2, 888); // picture
            end);
          DisposeObject([ms1, ms2]);
        end,
        procedure(Sender: TComputeThread)
        begin
          Dispose(PPascalString(Sender.UserData));
        end);
    end;

end;

procedure TForm1.backcall_dostatus(AText: SystemString; const ID: Integer);
begin
  LogMemo.Lines.Add(AText);
  LogMemo.GoToTextEnd;
end;

end.
