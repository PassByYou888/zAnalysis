unit RasterFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Surfaces, FMX.ListBox,

  CoreClasses, MemoryRaster, PascalStrings, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mr1, mr2: TMemoryRaster;
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
begin
  mr2.Assign(mr1);
  MemoryBitmapToBitmap(mr2, Image2.Bitmap);
  Label2.Text := Format('%s 输出 %d * %d', [ComboBox1.Items[ComboBox1.ItemIndex], mr2.Width, mr2.Height]);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  mr2.Reset;
  case ComboBox1.ItemIndex of
    0: mr2.ZoomFrom(mr1, mr1.Width div 20, mr1.Height div 20);
    1: mr2.FastBlurZoomFrom(mr1, mr1.Width div 20, mr1.Height div 20);
    2: mr2.GaussianBlurZoomFrom(mr1, mr1.Width div 20, mr1.Height div 20);
    3: mr2.GrayscaleBlurZoomFrom(mr1, mr1.Width div 20, mr1.Height div 20);
  end;
  MemoryBitmapToBitmap(mr2, Image2.Bitmap);
  Label2.Text := Format('%s 输出 %d * %d', [ComboBox1.Items[ComboBox1.ItemIndex], mr2.Width, mr2.Height]);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  mr2.Reset;
  case ComboBox1.ItemIndex of
    0: mr2.ZoomFrom(mr1, mr1.Width * 5, mr1.Height * 5);
    1: mr2.FastBlurZoomFrom(mr1, mr1.Width * 5, mr1.Height * 5);
    2: mr2.GaussianBlurZoomFrom(mr1, mr1.Width * 5, mr1.Height * 5);
    3: mr2.GrayscaleBlurZoomFrom(mr1, mr1.Width * 5, mr1.Height * 5);
  end;
  MemoryBitmapToBitmap(mr2, Image2.Bitmap);
  Label2.Text := Format('%s 输出 %d * %d', [ComboBox1.Items[ComboBox1.ItemIndex], mr2.Width, mr2.Height]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  rs:TResourceStream;
begin
  rs:=TResourceStream.Create(hInstance, 'demo', RT_RCDATA);
  image1.Bitmap.LoadFromStream(rs);
  disposeObject(rs);

  mr1 := TMemoryRaster.Create;
  mr2 := TMemoryRaster.Create;

  BitmapToMemoryBitmap(Image1.Bitmap, mr1);
  Label1.Text := Format('原图 %d * %d', [mr1.Width, mr1.Height]);

  Button1Click(Button1);
end;

end.
