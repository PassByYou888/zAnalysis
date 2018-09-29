unit HOGLargepictureAnalysisFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.Surfaces, System.Threading,

  Learn, LearnTypes, KDTree,
  zDrawEngineInterface_SlowFMX,
  MemoryRaster, ZDBEngine, ZDBLocalManager, MemoryStream64,
  PascalStrings, UnicodeMixedLib, DoStatusIO, CoreClasses;

type
  TForm1 = class(TForm)
    Button1: TButton;
    TabControl1: TTabControl;
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
    Button4: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure EnabledLibViewCheckBoxChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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

implementation

{$R *.fmx}


procedure TForm1.Button1Click(Sender: TObject);
var
  m64: TMemoryStream64;
  storePos: Int64;
  i: Integer;
  buff: TLVec;
  mr: TMemoryRaster;
  m: TLMatrix;
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

        m := LMatrixSamplerWithHOG(mr);
        lr.AddMatrix(m, [storePos]);

        ProgressBar1.Value := i;
        DoStatus('提取图片特征 %s 矩阵长度 %d * %d = %d',
          [umlGetFileName(OpenDialog1.Files[i]).Text, length(m), length(m[0]), length(m) * length(m[0])]);
        Application.ProcessMessages;
      finally
          DisposeObject(mr);
      end;
    end;

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
  mr1, mr2: TMemoryRaster;
  f_In: TLVec;
  list: TLIVec;
  i: Integer;
  storePos: Int64;
begin
  if not OpenDialog2.Execute then
      exit;

  mr1 := TMemoryRaster.Create;
  LoadMemoryBitmap(OpenDialog2.FileName, mr1);
  f_In := LVec(LMatrixSamplerWithHOG(mr1));

  MemoryBitmapToBitmap(mr1, Image1.Bitmap);

  storePos := Round(lr.processFV(f_In));

  mr2 := TMemoryRaster.Create;
  mr2.LoadFromStream(ze.GetCacheStream(storePos));
  MemoryBitmapToBitmap(mr2, Image2.Bitmap);
  DisposeObject(mr2);

  Image3.Bitmap.Clear(TAlphaColorRec.Black);
  Image4.Bitmap.Clear(TAlphaColorRec.Black);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  mr1, mr2: TMemoryRaster;
  f_In: TLVec;
  list: TLIVec;
  i: Integer;
  storePos: Int64;
begin
  if not OpenDialog2.Execute then
      exit;

  mr1 := TMemoryRaster.Create;
  LoadMemoryBitmap(OpenDialog2.FileName, mr1);
  f_In := LVec(LMatrixSamplerWithHOG(mr1));

  MemoryBitmapToBitmap(mr1, Image1.Bitmap);

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
  lr := TLearn.CreateHOGRegression(ltKDT, 1);

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

  ze.WaitQueryP(True,
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
