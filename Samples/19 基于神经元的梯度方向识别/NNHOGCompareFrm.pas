unit NNHOGCompareFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.StdCtrls,

  FMX.Surfaces,

  CoreClasses, DoStatusIO, MemoryRaster, PascalStrings, ObjectDataManager, ItemStream,
  zDrawEngineInterface_SlowFMX,
  Learn, LearnTypes;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    GlobalLayout: TLayout;
    ListBox1: TListBox;
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure RefreshGRLib;
    procedure DoListItemClick(Sender: TObject);
  public
    { Public declarations }
    pack: TObjectDataManagerOfCache;
    mrList: TCoreClassListForObj;
    lr: TLearn;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  m: TLMatrix;
  t: TTimeTick;
begin
  if lr <> nil then
      DisposeObject(lr);

  t := GetTimeTick;
  lr := TLearn.CreateHOGClassifier(ltKDT);
  DoStatus('正在生成 %d 采样矩阵...', [lr.InLen]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.LMatrixSamplerWithHOG(TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [LearnTypes.CLearnString[lr.LearnType]]);

  // 深度训练时请将1改大，比如50
  // 如果使用了"ltLBFGS_MT, ltLM_MT"这些并行方式训练，大数值深度+n维度的数组，会在瞬间吃掉16G以上内存，多数情况下，这一过程只会持续几秒，根据数据体积而定
  // 为保证Demo的顺利运行，我们在这里使用单核训练，只消耗很小内存

  t := GetTimeTick;
  lr.TrainP(1,
    procedure(const LSender: TLearn; const state: Boolean)
    begin
      if state then
          DoStatus('神经网络训练完成...耗时%dms', [GetTimeTick - t])
      else
          DoStatus('神经网络训练失败!!!');
      DoStatus(lr.Info);
      GlobalLayout.Enabled := True;
    end);
end;

procedure TForm1.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  rs: TResourceStream;
  sr: TItemSearch;
  mr: TMemoryRaster;
  itmStream: TItemStream;
begin
  AddDoStatusHook(Self, DoStatusM);

  rs := TResourceStream.Create(hInstance, 'pngPack', RT_RCDATA);
  pack := TObjectDataManagerOfCache.CreateAsStream(rs, '', ObjectDataMarshal.ID, True, False, True);
  mrList := TCoreClassListForObj.Create;

  if pack.ItemFindFirst('/', '*.png', sr) then
    begin
      repeat
        itmStream := TItemStream.Create(pack, sr.HeaderPOS);
        mr := TMemoryRaster.Create;
        LoadMemoryBitmap(itmStream, mr);
        mrList.Add(mr);
        DisposeObject(itmStream);
      until not pack.ItemFindNext(sr);
    end;

  lr := nil;

  RefreshGRLib;

  Button1Click(Button1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  DeleteDoStatusHook(Self);
  for i := 0 to mrList.Count - 1 do
      DisposeObject(mrList[i]);
  DisposeObject(mrList);
  DisposeObject(pack);
  if lr <> nil then
      DisposeObject(lr);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
  m: TLMatrix;
  t: TTimeTick;
begin
  if lr <> nil then
      DisposeObject(lr);

  t := GetTimeTick;
  lr := TLearn.CreateHOGClassifier(ltLBFGS);
  DoStatus('正在生成 %d 采样矩阵...', [lr.InLen]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.LMatrixSamplerWithHOG(TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [LearnTypes.CLearnString[lr.LearnType]]);

  // 深度训练时请将1改大，比如50
  // 如果使用了"ltLBFGS_MT, ltLM_MT"这些并行方式训练，大数值深度+n维度的数组，会在瞬间吃掉16G以上内存，多数情况下，这一过程只会持续几秒，根据数据体积而定
  // 为保证Demo的顺利运行，我们在这里使用单核训练，只消耗很小内存

  t := GetTimeTick;
  lr.TrainP(1,
    procedure(const LSender: TLearn; const state: Boolean)
    begin
      if state then
          DoStatus('神经网络训练完成...耗时%dms', [GetTimeTick - t])
      else
          DoStatus('神经网络训练失败!!!');
      DoStatus(lr.Info);
      GlobalLayout.Enabled := True;
    end);
end;

procedure TForm1.DoListItemClick(Sender: TObject);
var
  m: TLMatrix;
  fin, fout: TLVec;
  fMax, fMin: TLFloat;
  i: Integer;
  pb: TProgressBar;
begin
  if lr = nil then
      exit;

  m := Learn.LMatrixSamplerWithHOG(TMemoryRaster(mrList[TListboxItem(Sender).Index]));
  fin := LVec(m, lr.InLen);
  if lr.process(@fin, @fout) then
    begin
      DoStatus('最接近的图片索引：%d', [LMaxVecIndex(fout)]);
      MemoryBitmapToBitmap(TMemoryRaster(mrList[LMaxVecIndex(fout)]), Image1.Bitmap);

      // 做归一化处理
      fMax := LMaxVec(fout);
      fMin := LMinVec(fout);
      for i := 0 to length(fout) - 1 do
        begin
          pb := TProgressBar(ListBox1.ListItems[i].TagObject);
          pb.Max := fMax;
          pb.Min := fMin;
          pb.Value := fout[i];
        end;
    end;
end;

procedure TForm1.RefreshGRLib;
var
  litm: TListboxItem;
  img: TImage;
  pb: TProgressBar;
  i: Integer;
begin
  ListBox1.Clear;

  ListBox1.BeginUpdate;
  for i := 0 to mrList.Count - 1 do
    begin
      litm := TListboxItem.Create(ListBox1);
      litm.Parent := ListBox1;
      litm.Height := ListBox1.Width / 3.0 - 6;
      litm.Margins.Rect := Rectf(1, 1, 1, 1);
      litm.OnClick := DoListItemClick;

      img := TImage.Create(litm);
      img.Parent := litm;
      img.Align := TAlignLayout.Client;
      img.HitTest := False;
      MemoryBitmapToBitmap(TMemoryRaster(mrList[i]), img.Bitmap);

      pb := TProgressBar.Create(img);
      pb.Parent := img;
      pb.Align := TAlignLayout.Client;
      pb.HitTest := False;
      pb.Opacity := 0.5;
      pb.Margins.Rect := Rectf(2, 2, 2, 2);
      pb.Max := 1;
      pb.Min := 0;
      pb.Value := 0;

      litm.TagObject := pb;
    end;
  ListBox1.EndUpdate;
end;

end.
