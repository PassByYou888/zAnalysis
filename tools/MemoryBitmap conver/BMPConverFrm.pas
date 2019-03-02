unit BMPConverFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Layouts, FMX.TabControl, FMX.Controls.Presentation,
  FMX.Objects, FMX.Colors, FMX.Ani, FMX.ListBox,

  zDrawEngineInterface_FMX, MemoryRaster, zDrawEngine,
  Geometry2DUnit, CoreClasses, UnicodeMixedLib, FMX.ExtCtrls;

type
  TBMPConverForm = class(TForm)
    converbmp32Button: TButton;
    StyleBook1: TStyleBook;
    Layout1: TLayout;
    DestDirEdit: TEdit;
    Label1: TLabel;
    seldirEditButton: TEditButton;
    AddFileButton: TButton;
    ClearButton: TButton;
    OpenDialog: TOpenDialog;
    ListBox: TListBox;
    converseqButton: TButton;
    converjlsButton: TButton;
    RadioButton_JLS8: TRadioButton;
    RadioButton_JLS24: TRadioButton;
    Image: TImageViewer;
    converyv12Button: TButton;
    converbmp24Button: TButton;
    SameDirCheckBox: TCheckBox;
    converHalfYUVButton: TButton;
    converQuartYUVButton: TButton;
    converJpegButton: TButton;
    RadioButton_Jpeg_RGBA: TRadioButton;
    RadioButton_Jpeg_RGB: TRadioButton;
    RadioButton_Jpeg_Gray: TRadioButton;
    Layout2: TLayout;
    Label2: TLabel;
    JpegQualilyEdit: TEdit;
    RadioButton_Jpeg_GrayA: TRadioButton;
    RadioButton_Jpeg_CMYK: TRadioButton;
    procedure AddFileButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure converbmp24ButtonClick(Sender: TObject);
    procedure ListBoxChange(Sender: TObject);
    procedure converbmp32ButtonClick(Sender: TObject);
    procedure converHalfYUVButtonClick(Sender: TObject);
    procedure seldirEditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure converseqButtonClick(Sender: TObject);
    procedure converjlsButtonClick(Sender: TObject);
    procedure converJpegButtonClick(Sender: TObject);
    procedure converQuartYUVButtonClick(Sender: TObject);
    procedure converyv12ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BMPConverForm: TBMPConverForm;

implementation

{$R *.fmx}


procedure TBMPConverForm.AddFileButtonClick(Sender: TObject);
var
  i: Integer;
  itm: TListBoxItem;
begin
  OpenDialog.Filter := '*.*';
  if not OpenDialog.Execute then
      Exit;
  ListBox.BeginUpdate;
  for i := 0 to OpenDialog.Files.Count - 1 do
    begin
      itm := TListBoxItem.Create(ListBox);
      itm.ItemData.Text := umlGetFileName(OpenDialog.Files[i]);
      itm.ItemData.detail := umlGetFilePath(OpenDialog.Files[i]);
      itm.TagString := OpenDialog.Files[i];
      itm.StyleLookup := 'listboxitembottomdetail';
      itm.height := 40;
      itm.Selectable := True;
      ListBox.AddObject(itm);
    end;
  ListBox.EndUpdate;
end;

procedure TBMPConverForm.ClearButtonClick(Sender: TObject);
begin
  ListBox.Clear;
end;

procedure TBMPConverForm.converbmp24ButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.bmp')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.bmp');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToBmp24File(GetDestFile(F));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converbmp32ButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.bmp')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.bmp');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToBmp32File(GetDestFile(F));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converHalfYUVButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.hyuv')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.hyuv');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToHalfYUVFile(GetDestFile(F));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converjlsButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.jls')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.jls');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TSequenceMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TSequenceMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);

      if RadioButton_JLS8.IsChecked then
          b.SaveToJpegLS1File(GetDestFile(F))
      else if RadioButton_JLS24.IsChecked then
          b.SaveToJpegLS3File(GetDestFile(F))
      else
          RaiseInfo('error.');

      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converJpegButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.jpg')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.jpg');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);

      if RadioButton_Jpeg_RGBA.IsChecked then
          b.SaveToJpegRGBAFile(GetDestFile(F), umlStrToInt(JpegQualilyEdit.Text, 90))
      else if RadioButton_Jpeg_RGB.IsChecked then
          b.SaveToJpegRGBFile(GetDestFile(F), umlStrToInt(JpegQualilyEdit.Text, 90))
      else if RadioButton_Jpeg_GrayA.IsChecked then
          b.SaveToJpegGrayAFile(GetDestFile(F), umlStrToInt(JpegQualilyEdit.Text, 90))
      else if RadioButton_Jpeg_Gray.IsChecked then
          b.SaveToJpegGrayFile(GetDestFile(F), umlStrToInt(JpegQualilyEdit.Text, 90))
      else if RadioButton_Jpeg_CMYK.IsChecked then
          b.SaveToJpegCMYKRGBFile(GetDestFile(F), umlStrToInt(JpegQualilyEdit.Text, 90));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converQuartYUVButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.qyuv')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.qyuv');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToQuartYUVFile(GetDestFile(F));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converseqButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.seq')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.seq');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TSequenceMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TSequenceMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToFile(GetDestFile(F));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
      DisposeObject(b);
    end;
  Caption := Format('all conver done!', []);
end;

procedure TBMPConverForm.converyv12ButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    F: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.yv12')
    else
      begin
        F := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, F), '.yv12');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  F: string;
  b: TMemoryRaster;
begin
  if ListBox.Count <= 0 then
      Exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      F := itm.TagString;

      b := TMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);
      b.SaveToYV12File(GetDestFile(F));
      Caption := Format('%s -> %s ok!', [umlGetFileName(itm.TagString).Text, umlGetFileName(GetDestFile(F)).Text]);
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
  b, bk: TMemoryRaster;
begin
  if ListBox.selected = nil then
      Exit;

  b := TMemoryRaster.Create;
  LoadMemoryBitmap(ListBox.selected.TagString, b);

  bk := TMemoryRaster.Create;
  bk.SetSize(b.width, b.height);
  FillBlackGrayBackgroundTexture(bk, 32);
  b.DrawTo(bk);

  bk.DrawText(Format('%s' + #10 + 'width: %d * height: %d' + #10 + 'size:%s', [umlGetFileName(ListBox.selected.TagString).Text,
    b.width, b.height, umlSizeToStr(umlGetFileSize(ListBox.selected.TagString)).Text]),
    0, 0, vec2(1.0, 0.0), -10, 0.9, 12, RasterColorF(1.0, 0.5, 0.5, 1));

  MemoryBitmapToBitmap(bk, Image.Bitmap);
  DisposeObject(b);
  DisposeObject(bk);
end;

end.
