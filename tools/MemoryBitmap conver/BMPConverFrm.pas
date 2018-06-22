unit BMPConverFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Layouts, FMX.TabControl, FMX.Controls.Presentation,
  FMX.Objects, FMX.Colors, FMX.Ani, FMX.ListBox,

  zDrawEngineInterface_FMX, MemoryRaster, Geometry2DUnit, CoreClasses, UnicodeMixedLib, FMX.ExtCtrls;

type
  TBMPConverForm = class(TForm)
    converbmpButton: TButton;
    StyleBook1: TStyleBook;
    Layout1: TLayout;
    DestDirEdit: TEdit;
    Label1: TLabel;
    seldirEditButton: TEditButton;
    SameDirCheckBox: TCheckBox;
    AddFileButton: TButton;
    ClearButton: TButton;
    OpenDialog: TOpenDialog;
    ListBox: TListBox;
    converseqButton: TButton;
    converjlsButton: TButton;
    RadioButton_JLS8: TRadioButton;
    RadioButton_JLS24: TRadioButton;
    RadioButton_JLS32: TRadioButton;
    Image: TImageViewer;
    procedure AddFileButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ListBoxChange(Sender: TObject);
    procedure converbmpButtonClick(Sender: TObject);
    procedure seldirEditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure converseqButtonClick(Sender: TObject);
    procedure converjlsButtonClick(Sender: TObject);
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
  i: Integer;
  itm: TListBoxItem;
  f: string;
  b: TMemoryRaster;
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

procedure TBMPConverForm.converjlsButtonClick(Sender: TObject);
  function GetDestFile(sour: string): string;
  var
    f: string;
  begin
    if SameDirCheckBox.IsChecked then
        Result := umlChangeFileExt(sour, '.jls')
    else
      begin
        f := umlGetFileName(sour);
        Result := umlChangeFileExt(umlCombineFileName(DestDirEdit.Text, f), '.jls');
      end;
  end;

var
  i: Integer;
  itm: TListBoxItem;
  f: string;
  b: TSequenceMemoryRaster;
begin
  if ListBox.Count <= 0 then
      exit;

  for i := 0 to ListBox.Count - 1 do
    begin
      itm := ListBox.ListItems[i];
      f := itm.TagString;

      b := TSequenceMemoryRaster.Create;
      LoadMemoryBitmap(itm.TagString, b);

      if RadioButton_JLS8.IsChecked then
          b.SaveToJpegLS1File(GetDestFile(f))
      else if RadioButton_JLS24.IsChecked then
          b.SaveToJpegLS3File(GetDestFile(f))
      else
          b.SaveToJpegAlphaFile(GetDestFile(f));

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
  i: Integer;
  itm: TListBoxItem;
  f: string;
  b: TSequenceMemoryRaster;
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
  b.DrawText(Format('%s' + #10 + 'width: %d * height: %d' + #10 + 'size:%s', [umlGetFileName(ListBox.Selected.TagString).Text,
    b.Width, b.Height, umlSizetoStr(umlGetFileSize(ListBox.Selected.TagString)).Text]),
    0, 0, Vec2(1.0, 0.0), -10, 0.9, 12, RasterColorF(1.0, 0.5, 0.5, 1));
  MemoryBitmapToBitmap(b, Image.Bitmap);
  DisposeObject(b);
end;

end.

