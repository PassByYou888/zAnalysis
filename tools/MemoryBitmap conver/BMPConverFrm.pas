unit BMPConverFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Layouts, FMX.TabControl, FMX.Controls.Presentation,
  FMX.Objects, FMX.Colors, FMX.Ani, FMX.ListBox,
  FMX.Surfaces,

  MemoryRaster, CoreClasses, PascalStrings, UnicodeMixedLib;

type
  TBMPConverForm = class(TForm)
    converbmpButton: TButton;
    StyleBook1: TStyleBook;
    Layout1: TLayout;
    DestDirEdit: TEdit;
    Label1: TLabel;
    seldirEditButton: TEditButton;
    Rectangle: TRectangle;
    SameDirCheckBox: TCheckBox;
    AddFileButton: TButton;
    ClearButton: TButton;
    OpenDialog: TOpenDialog;
    ListBox: TListBox;
    converseqButton: TButton;
    procedure AddFileButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ListBoxChange(Sender: TObject);
    procedure converbmpButtonClick(Sender: TObject);
    procedure seldirEditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure converseqButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BMPConverForm: TBMPConverForm;

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


procedure TBMPConverForm.AddFileButtonClick(Sender: TObject);
var
  i  : Integer;
  itm: TListBoxItem;
begin
  OpenDialog.Filter := '*.*';
  if not OpenDialog.Execute then
      exit;
  ListBox.BeginUpdate;
  for i := 0 to OpenDialog.Files.Count - 1 do
    begin
      itm := TListBoxItem.Create(ListBox);
      itm.ItemData.Text := umlGetFileName(OpenDialog.Files[i]);
      itm.ItemData.Detail := umlGetFilePath(OpenDialog.Files[i]);
      itm.TagString := OpenDialog.Files[i];
      itm.StyleLookup := 'listboxitembottomdetail';
      itm.Height := 40;
      itm.Selectable := True;
      ListBox.AddObject(itm);
    end;
  ListBox.EndUpdate;
end;

procedure TBMPConverForm.ClearButtonClick(Sender: TObject);
begin
  ListBox.Clear;
end;

procedure TBMPConverForm.converbmpButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    f: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.bmp')
    else
      begin
        f := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, f), '.bmp');
      end;
  end;

var
  i  : Integer;
  itm: TListBoxItem;
  f  : string;
  b  : TMemoryRaster;
begin
  if ListBox.Count <= 0 then
      exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      f := itm.TagString;

      b := TMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToFile(GetDestFile(f));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(f)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converseqButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    f: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.seq')
    else
      begin
        f := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, f), '.seq');
      end;
  end;

var
  i  : Integer;
  itm: TListBoxItem;
  f  : string;
  b  : TSequenceMemoryRaster;
begin
  if ListBox.Count <= 0 then
      exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      f := itm.TagString;

      b := TSequenceMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToFile(GetDestFile(f));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(f)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.FormCreate(Sender: TObject);
begin
  DestDirEdit.Text := umlCurrentPath;
end;

procedure TBMPConverForm.seldirEditButtonClick(Sender: TObject);
var
  v: string;
begin
  v := DestDirEdit.Text;
  if SelectDirectory('output directory', '', v) then
      DestDirEdit.Text := v;
end;

procedure TBMPConverForm.ListBoxChange(Sender: TObject);
var
  b: TMemoryRaster;
begin
  if ListBox.Selected = nil then
      exit;

  b := TMemoryRaster.Create;
  LoadMemoryBitmap(ListBox.Selected.TagString, b);
  Rectangle.Fill.Kind := TBrushKind.Bitmap;
  MemoryBitmapToBitmap(b, Rectangle.Fill.Bitmap.Bitmap);
  DisposeObject(b);
end;

end.
