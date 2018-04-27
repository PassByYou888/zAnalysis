unit LargepictureAnalysisFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.Surfaces, System.Threading,

  Learn, KDTree,
  MemoryRaster, ZDBEngine, MemoryStream64, PascalStrings, UnicodeMixedLib, DoStatusIO, CoreClasses;

type
  TForm1 = class(TForm)
    Button1: TButton;
    TabControl1: TTabControl;
    Button2: TButton;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Memo1: TMemo;
    ListBox1: TListBox;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    TabItem3: TTabItem;
    Image1: TImage;
    Label1: TLabel;
    Image2: TImage;
    Label2: TLabel;
    OpenDialog2: TOpenDialog;
    globalLayout1: TLayout;
    EnabledLibViewCheckBox: TCheckBox;
    Image3: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Image4: TImage;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure EnabledLibViewCheckBoxChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    lr: TLearn;
    ze: TDBStoreBase;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
    procedure RefreshGRLib;
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
  // TMemoryRaster 是高速光栅化图形库的类
  m64     : TMemoryStream64;
  storePos: Int64;
  i       : Integer;
  buff    : TLVec;
  mr      : TMemoryRaster;
begin
  if not OpenDialog1.Execute then
      exit;

  ProgressBar1.Visible := True;
  ProgressBar1.Max := OpenDialog1.Files.Count - 1;
  ProgressBar1.Min := 0;

  globalLayout1.Enabled := False;

  for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      try
        mr := TMemoryRaster.Create;
        LoadMemoryBitmap(OpenDialog1.Files[i], mr);

        // 将图片原始数据存储到zdb
        m64 := TMemoryStream64.Create;
        mr.SaveToStream(m64);
        storePos := ze.AddData(m64, $99);
        DisposeObject(m64);

        lr.AddMatrix(MatrixSampler(15, mr), [storePos]);

        ProgressBar1.Value := i;
        DoStatus('提取图片特征 %s ', [umlGetFileName(OpenDialog1.Files[i]).Text]);
        Application.ProcessMessages;
      finally
          DisposeObject(mr);
      end;
    end;

  // 在后台进行特征学习
  lr.TrainP(
    1,
    procedure(const LSender: TLearn; const state: Boolean)
    begin
      DoStatus('机器人学习完成，一共学习了 %d 张图片', [LSender.Count]);
      ProgressBar1.Visible := False;
      RefreshGRLib;
      globalLayout1.Enabled := True;
    end);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  // TMemoryRaster 是高速光栅化图形库的类
  mr1, mr2: TMemoryRaster;
  f_In    : TLVec;
  list    : TLIVec;
  i       : Integer;
  storePos: Int64;
begin
  if not OpenDialog2.Execute then
      exit;

  mr1 := TMemoryRaster.Create;
  LoadMemoryBitmap(OpenDialog2.FileName, mr1);

  MemoryBitmapToBitmap(mr1, Image1.Bitmap);

  f_In := LVec(MatrixSampler(True, 15, mr1), 15 * 15);

  if lr.Count > 0 then
    begin
      lr.SearchMemoryWithPearson(f_In, list);

      for i := 0 to length(list) - 1 do
        begin
          case i of
            0:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image2.Bitmap);
                DisposeObject(mr2);
              end;
            1:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image3.Bitmap);
                DisposeObject(mr2);
              end;
            2:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image4.Bitmap);
                DisposeObject(mr2);
              end;
          end;
        end;

      TabControl1.ActiveTab := TabItem3;
    end;

  DisposeObject(mr1);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  // TMemoryRaster 是高速光栅化图形库的类
  mr1, mr2: TMemoryRaster;
  f_In    : TLVec;
  list    : TLIVec;
  i       : Integer;
  storePos: Int64;
begin
  if not OpenDialog2.Execute then
      exit;

  mr1 := TMemoryRaster.Create;
  LoadMemoryBitmap(OpenDialog2.FileName, mr1);

  MemoryBitmapToBitmap(mr1, Image1.Bitmap);

  f_In := LVec(MatrixSampler(True, 15, mr1), 15 * 15);

  if lr.Count > 0 then
    begin
      lr.SearchMemoryWithSpearman(f_In, list);

      for i := 0 to length(list) - 1 do
        begin
          case i of
            0:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image2.Bitmap);
                DisposeObject(mr2);
              end;
            1:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image3.Bitmap);
                DisposeObject(mr2);
              end;
            2:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image4.Bitmap);
                DisposeObject(mr2);
              end;
          end;
        end;

      TabControl1.ActiveTab := TabItem3;
    end;
  DisposeObject(mr1);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  // TMemoryRaster 是高速光栅化图形库的类
  mr1, mr2: TMemoryRaster;
  f_In    : TLVec;
  list    : TLIVec;
  i       : Integer;
  storePos: Int64;
begin
  if not OpenDialog2.Execute then
      exit;

  mr1 := TMemoryRaster.Create;
  LoadMemoryBitmap(OpenDialog2.FileName, mr1);

  MemoryBitmapToBitmap(mr1, Image1.Bitmap);

  f_In := LVec(MatrixSampler(True, 15, mr1), 15 * 15);
  if lr.Count > 0 then
    begin
      lr.SearchMemoryWithDistance(f_In, list);

      for i := 0 to length(list) - 1 do
        begin
          case i of
            0:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image2.Bitmap);
                DisposeObject(mr2);
              end;
            1:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image3.Bitmap);
                DisposeObject(mr2);
              end;
            2:
              begin
                storePos := Round(lr[list[i]]^.m_out[0]);
                mr2 := TMemoryRaster.Create;
                mr2.LoadFromStream(ze.GetCacheStream(storePos));
                MemoryBitmapToBitmap(mr2, Image4.Bitmap);
                DisposeObject(mr2);
              end;
          end;
        end;

      TabControl1.ActiveTab := TabItem3;
    end;
  DisposeObject(mr1);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  // TMemoryRaster 是高速光栅化图形库的类
  m64     : TMemoryStream64;
  storePos: Int64;
  i       : Integer;
  buff    : TLVec;
  mr      : TMemoryRaster;
begin
  if not OpenDialog1.Execute then
      exit;

  ProgressBar1.Visible := True;
  ProgressBar1.Max := OpenDialog1.Files.Count - 1;
  ProgressBar1.Min := 0;

  globalLayout1.Enabled := False;

  for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      try
        mr := TMemoryRaster.Create;
        LoadMemoryBitmap(OpenDialog1.Files[i], mr);

        // 将图片原始数据存储到zdb
        m64 := TMemoryStream64.Create;
        mr.SaveToStream(m64);
        storePos := ze.AddData(m64, $99);
        DisposeObject(m64);

        lr.AddMatrix(MatrixSampler(True, 15, mr), [storePos]);

        ProgressBar1.Value := i;
        DoStatus('提取图片特征 %s ', [umlGetFileName(OpenDialog1.Files[i]).Text]);
        Application.ProcessMessages;
      finally
          DisposeObject(mr);
      end;
    end;

  // 在后台进行特征学习
  lr.TrainP(
    1,
    procedure(const LSender: TLearn; const state: Boolean)
    begin
      DoStatus('机器人学习完成，一共学习了 %d 张图片', [LSender.Count]);
      ProgressBar1.Visible := False;
      RefreshGRLib;
      globalLayout1.Enabled := True;
    end);
end;

procedure TForm1.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo2.Lines.Add(AText);
  Memo2.GoToTextEnd;
end;

procedure TForm1.EnabledLibViewCheckBoxChange(Sender: TObject);
begin
  RefreshGRLib;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 直观解释，每张图片50个双浮点特征数据，存储空间维50*double(8byte)
  // 在TLearn内部，50是表示50个维度
  lr := TLearn.CreateRegression(ltKDT, 15 * 15, 1);

  // CreateNewMemory方法是让zdb基于内存进行存储和工作

  ze := TDBStoreBase.CreateNew(umlCombineFileName(umlCurrentPath, '机器学习库.ox'));

  AddDoStatusHook(Self, DoStatusM);
  Test_KDTree(50);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DisposeObject([lr, ze]);
  DeleteDoStatusHook(Self);
end;

procedure TForm1.RefreshGRLib;
begin
  if ze.QueryThreadCount > 0 then
      exit;

  ListBox1.Clear;

  if not EnabledLibViewCheckBox.IsChecked then
      exit;

  ze.WaitQuery(True,
    procedure(var qState: TQueryState)
    var
      litm: TListBoxItem;
      img: TImage;
      mr: TMemoryRaster;
      stream: TStream;
    begin
      if qState.ID <> $99 then
          exit;
      litm := TListBoxItem.Create(ListBox1);
      litm.Parent := ListBox1;
      litm.Height := litm.Width - 5;
      litm.Margins.Rect := Rectf(1, 1, 1, 1);
      litm.Selectable := False;
      litm.HitTest := False;
      img := TImage.Create(litm);
      img.Parent := litm;
      img.Align := TAlignLayout.Client;
      img.HitTest := False;

      mr := TMemoryRaster.Create;
      mr.LoadFromStream(qState.DBEng.GetCacheStream(qState.storePos));
      MemoryBitmapToBitmap(mr, img.Bitmap);
      DisposeObject(mr);
    end);
  Invalidate;
end;

end.
