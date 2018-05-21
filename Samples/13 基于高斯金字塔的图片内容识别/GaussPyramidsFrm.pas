unit GaussPyramidsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.StdCtrls,

  FMX.Surfaces,

  CoreClasses, DoStatusIO, MemoryRaster, PascalStrings, ObjectDataManager, ItemStream,
  UnicodeMixedLib, Learn, LearnTypes, PyramidSpace;

type
  TGaussPyramidsForm = class(TForm)
    TabControl1: TTabControl;
    Memo1: TMemo;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ft1, ft2: TFeature;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
  end;

var
  GaussPyramidsForm: TGaussPyramidsForm;
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
procedure SaveMemoryBitmap(f: SystemString; b: TMemoryRaster);
{$ENDREGION 'drawEngine'}

implementation

{$R *.fmx}
{$REGION 'drawEngine'}


procedure MemoryBitmapToSurface(bmp: TMemoryRaster; Surface: TBitmapSurface);
var
  X, Y: Integer;
  c: TRasterColorEntry;
  dc: TAlphaColor;
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

procedure SaveMemoryBitmap(f: SystemString; b: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['*.bmp'], f) then
      b.SaveToFile(f)
  else if umlMultipleMatch(['*.seq'], f) then
      b.SaveToZLibCompressFile(f)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(b, Surf);
        TBitmapCodecManager.SaveToFile(f, Surf, nil);
      finally
          DisposeObject(Surf);
      end;
    end;
end;
{$ENDREGION 'drawEngine'}


function _NewRasterFromFile(const fn: string): TMemoryRaster;
begin
  Result := NewRaster;
  LoadMemoryBitmap(fn, Result);
end;

procedure _SaveRaster(mr: TMemoryRaster; const fn: string);
begin
  SaveMemoryBitmap(fn, mr);
end;

{ TGaussPyramidsForm }

procedure TGaussPyramidsForm.Button1Click(Sender: TObject);
var
  mr: TMemoryRaster;
  dt: TTimeTick;
begin
  if not OpenDialog1.Execute then
      exit;
  DisposeObject(ft1);
  ft1 := nil;
  dt := GetTimeTick;
  ft1 := TFeature.CreateWithRaster(OpenDialog1.FileName);
  DoStatus('提取 %s 特征所花费的时间:%dms 特征数:%d ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt, ft1.Count]);

  dt := GetTimeTick;
  mr := ft1.CreateFeatureViewer(10, RasterColorF(1, 0, 0, 0.3));
  DoStatus('生成 %s 视图所花费的时间:%dms ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt]);

  MemoryBitmapToBitmap(mr, Image1.Bitmap);
  DisposeObject(mr);

  TabControl1.ActiveTab := TabItem1;
end;

procedure TGaussPyramidsForm.Button2Click(Sender: TObject);
var
  mr: TMemoryRaster;
  dt: TTimeTick;
begin
  if not OpenDialog1.Execute then
      exit;
  DisposeObject(ft2);
  ft2 := nil;
  dt := GetTimeTick;
  ft2 := TFeature.CreateWithRaster(OpenDialog1.FileName);
  DoStatus('提取 %s 特征所花费的时间:%dms 特征数:%d ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt, ft2.Count]);

  dt := GetTimeTick;
  mr := ft2.CreateFeatureViewer(10, RasterColorF(0, 0, 1, 0.3));
  DoStatus('生成 %s 视图所花费的时间:%dms ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt]);

  MemoryBitmapToBitmap(mr, Image2.Bitmap);
  DisposeObject(mr);
  TabControl1.ActiveTab := TabItem2;
end;

procedure TGaussPyramidsForm.Button3Click(Sender: TObject);
var
  mi: TArrayMatchInfo;
  f: TLFloat;
  mr: TMemoryRaster;
  dt: TTimeTick;
begin
  if ft1 = nil then
      exit;
  if ft2 = nil then
      exit;

  dt := GetTimeTick;
  f := MatchFeature(ft1, ft2, mi);
  DoStatus('分析特征相近度:%s 总共匹配了 %d 个相似特征', [FloatToStr(f), length(mi)]);
  DoStatus('分析特征所花费的时间:%dms ', [GetTimeTick - dt]);

  if length(mi) = 0 then
    begin
      DoStatus('没有相似特征');
      exit;
    end;

  dt := GetTimeTick;
  mr := BuildMatchInfoView(mi, 5, False);
  DoStatus('生成特征分析视图所花费的时间:%dms ', [GetTimeTick - dt]);

  if mr <> nil then
    begin
      MemoryBitmapToBitmap(mr, Image3.Bitmap);
      DisposeObject(mr);
    end
  else
      DoStatus('没有相似特征');

  TabControl1.ActiveTab := TabItem3;
end;

procedure TGaussPyramidsForm.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TGaussPyramidsForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusM);
  ft1 := nil;
  ft2 := nil;
end;

procedure TGaussPyramidsForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
end;

initialization

NewRasterFromFile := _NewRasterFromFile;
SaveRaster := _SaveRaster;

finalization

end.
