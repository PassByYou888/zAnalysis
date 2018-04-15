unit NNPicutreCompareFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.StdCtrls,

  FMX.Surfaces,

  CoreClasses, DoStatusIO, MemoryRaster, PascalStrings, ObjectDataManager, ItemStream,
  Learn;

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
    Button3: TButton;
    Button4: TButton;
    Splitter1: TSplitter;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    procedure RefreshGRLib;
    procedure DoListItemClick(Sender: TObject);
  public
    { Public declarations }
    pack  : TObjectDataManagerOfCache;
    mrList: TCoreClassListForObj;
    lr    : TLearn;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

  { 下列函数是从 zDrawEngine->FMX 接口拔出的 }
  { 因为zDrawEngine的体系有点巨大，懒于整理，不便开源 }
  {$REGION 'drawEngine'}
procedure MemoryBitmapToSurface(bmp: TMemoryRaster; Surface: TBitmapSurface); overload; inline;
procedure MemoryBitmapToSurface(bmp: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface); overload; inline;
procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; bmp: TMemoryRaster); inline;
procedure MemoryBitmapToBitmap(b: TMemoryRaster; bmp: TBitmap); overload;
procedure MemoryBitmapToBitmap(b: TMemoryRaster; sourRect: TRect; bmp: TBitmap); overload;
procedure BitmapToMemoryBitmap(bmp: TBitmap; b: TMemoryRaster);
procedure LoadMemoryBitmap(f: SystemString; b: TMemoryRaster); overload;
procedure LoadMemoryBitmap(stream: TCoreClassStream; b: TMemoryRaster); overload;
{$ENDREGION 'drawEngine'}

implementation

{$R *.fmx}
{$REGION 'drawEngine'}


procedure MemoryBitmapToSurface(bmp: TMemoryRaster; Surface: TBitmapSurface);
var
  X, Y: Integer;
  c   : TRasterColorEntry;
  dc  : TAlphaColor;
begin
  Surface.SetSize(bmp.Width, bmp.Height, TPixelFormat.RGBA);
  for Y := 0 to bmp.Height - 1 do
    for X := 0 to bmp.Width - 1 do
      begin

        {$IF Defined(ANDROID) or Defined(IOS) or Defined(OSX)}
        c.RGBA := RGBA2BGRA(bmp.Pixel[X, Y]);
        {$ELSE}
        c.RGBA := bmp.Pixel[X, Y];
        {$IFEND}
        TAlphaColorRec(dc).r := c.r;
        TAlphaColorRec(dc).g := c.g;
        TAlphaColorRec(dc).b := c.b;
        TAlphaColorRec(dc).a := c.a;
        Surface.Pixels[X, Y] := dc;
      end;
end;

procedure MemoryBitmapToSurface(bmp: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface);
var
  nb: TMemoryRaster;
begin
  nb := TMemoryRaster.Create;
  nb.DrawMode := dmBlend;
  nb.SetSize(sourRect.Width, sourRect.Height, RasterColor(0, 0, 0, 0));
  bmp.DrawTo(nb, 0, 0, sourRect);
  MemoryBitmapToSurface(nb, Surface);
  DisposeObject(nb);
end;

procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; bmp: TMemoryRaster);
var
  X, Y: Integer;
begin
  bmp.SetSize(Surface.Width, Surface.Height);
  for Y := 0 to Surface.Height - 1 do
    for X := 0 to Surface.Width - 1 do
      with TAlphaColorRec(Surface.Pixels[X, Y]) do
          bmp.Pixel[X, Y] := RasterColor(r, g, b, a)
end;

procedure MemoryBitmapToBitmap(b: TMemoryRaster; bmp: TBitmap);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  MemoryBitmapToSurface(b, Surface);
  bmp.Assign(Surface);
  DisposeObject(Surface);
end;

procedure MemoryBitmapToBitmap(b: TMemoryRaster; sourRect: TRect; bmp: TBitmap);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  MemoryBitmapToSurface(b, sourRect, Surface);
  bmp.Assign(Surface);
  DisposeObject(Surface);
end;

procedure BitmapToMemoryBitmap(bmp: TBitmap; b: TMemoryRaster);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  Surface.Assign(bmp);
  SurfaceToMemoryBitmap(Surface, b);
  DisposeObject(Surface);
end;

procedure LoadMemoryBitmap(f: SystemString; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if b.CanLoadFile(f) then
    begin
      b.LoadFromFile(f);
    end
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromFile(f, Surf, TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            SurfaceToMemoryBitmap(Surf, b);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure LoadMemoryBitmap(stream: TCoreClassStream; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if b.CanLoadStream(stream) then
    begin
      b.LoadFromStream(stream);
    end
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromStream(stream, Surf, TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            SurfaceToMemoryBitmap(Surf, b);
      finally
          DisposeObject(Surf);
      end;
    end;
end;
{$ENDREGION 'drawEngine'}


procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  m: TLMatrix;
  t: TTimeTick;
begin
  if lr <> nil then
      DisposeObject(lr);

  t := GetTimeTick;
  lr := TLearn.CreateClassifier(ltKDT, 100);
  DoStatus('正在生成 %d * %d 采样矩阵...', [Trunc(Sqrt(lr.InLen)), Trunc(Sqrt(lr.InLen))]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.MatrixSampler(True, Trunc(Sqrt(lr.InLen)), TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [Learn.CLearnString[lr.LearnType]]);

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
  rs       : TResourceStream;
  sr       : TItemSearch;
  mr       : TMemoryRaster;
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
  lr := TLearn.CreateClassifier(ltLBFGS, 100);
  DoStatus('正在生成 %d * %d 采样矩阵...', [Trunc(Sqrt(lr.InLen)), Trunc(Sqrt(lr.InLen))]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.MatrixSampler(True, Trunc(Sqrt(lr.InLen)), TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [Learn.CLearnString[lr.LearnType]]);

  // 深度训练时请将1改大，比如50
  // 如果使用了"ltLBFGS_MT, ltLM_MT"这些并行方式训练，大数值深度+n维度的数组，会在瞬间吃掉16G以上内存，多数情况下，这一过程只会持续几秒，根据数据体积而定
  // 为保证Demo的顺利运行，我们在这里使用单核训练，只消耗很小内存

  t := GetTimeTick;
  lr.TrainP(10,
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

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
  m: TLMatrix;
  t: TTimeTick;
begin
  if lr <> nil then
      DisposeObject(lr);

  t := GetTimeTick;
  lr := TLearn.CreateClassifier(ltLM, 100);
  DoStatus('正在生成 %d * %d 采样矩阵...', [Trunc(Sqrt(lr.InLen)), Trunc(Sqrt(lr.InLen))]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.MatrixSampler(True, Trunc(Sqrt(lr.InLen)), TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [Learn.CLearnString[lr.LearnType]]);

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

procedure TForm1.Button4Click(Sender: TObject);
var
  i: Integer;
  m: TLMatrix;
  t: TTimeTick;
begin
  if lr <> nil then
      DisposeObject(lr);

  t := GetTimeTick;
  lr := TLearn.CreateClassifier(ltForest, 100);
  DoStatus('正在生成 %d * %d 采样矩阵...', [Trunc(Sqrt(lr.InLen)), Trunc(Sqrt(lr.InLen))]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.MatrixSampler(True, Trunc(Sqrt(lr.InLen)), TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [Learn.CLearnString[lr.LearnType]]);

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

procedure TForm1.Button5Click(Sender: TObject);
var
  i: Integer;
  m: TLMatrix;
  t: TTimeTick;
begin
  if lr <> nil then
      DisposeObject(lr);

  t := GetTimeTick;
  lr := TLearn.CreateClassifier(ltLBFGS_Ensemble, 100);
  DoStatus('正在生成 %d * %d 采样矩阵...', [Trunc(Sqrt(lr.InLen)), Trunc(Sqrt(lr.InLen))]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.MatrixSampler(True, Trunc(Sqrt(lr.InLen)), TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [Learn.CLearnString[lr.LearnType]]);

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

procedure TForm1.Button6Click(Sender: TObject);
var
  i: Integer;
  m: TLMatrix;
  t: TTimeTick;
begin
  if lr <> nil then
      DisposeObject(lr);

  t := GetTimeTick;
  lr := TLearn.CreateClassifier(ltLM_Ensemble, 100);
  DoStatus('正在生成 %d * %d 采样矩阵...', [Trunc(Sqrt(lr.InLen)), Trunc(Sqrt(lr.InLen))]);
  for i := 0 to mrList.Count - 1 do
    begin
      m := Learn.MatrixSampler(True, Trunc(Sqrt(lr.InLen)), TMemoryRaster(mrList[i]));
      lr.AddMatrix(m, [i]);
    end;
  DoStatus('采样矩阵生成完成...耗时%dms', [GetTimeTick - t]);

  GlobalLayout.Enabled := False;
  DoStatus('正在使用 %s 训练神经网络...', [Learn.CLearnString[lr.LearnType]]);

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
  m         : TLMatrix;
  fin, fout : TLVec;
  fMax, fMin: TLFloat;
  i         : Integer;
  pb        : TProgressBar;
begin
  if lr = nil then
      exit;

  m := MatrixSampler(True, Trunc(Sqrt(lr.InLen)), TMemoryRaster(mrList[TListboxItem(Sender).Index]));
  fin := LVec(m, lr.InLen);
  if lr.process(@fin, @fout) then
    begin
      DoStatus('最接近的图片索引：%d', [MaxVecIndex(fout)]);
      MemoryBitmapToBitmap(TMemoryRaster(mrList[MaxVecIndex(fout)]), Image1.Bitmap);

      // 做归一化处理
      fMax := MaxVec(fout);
      fMin := MinVec(fout);
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
  img : TImage;
  pb  : TProgressBar;
  i   : Integer;
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
