unit ImgFmtConverFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.FileCtrl,

  Vcl.Graphics,

  System.IOUtils,

  TextDataEngine, CoreClasses, ListEngine, Vcl.XPMan;

type
  TImgFmtConverForm = class(TForm)
    SourMemo: TMemo;
    Label1: TLabel;
    DestEdit: TLabeledEdit;
    BrowseButton: TButton;
    UsedSameDirCheckBox: TCheckBox;
    StateMemo: TMemo;
    ExeccuteConvertButton: TButton;
    AddFileButton: TButton;
    ClearButton: TButton;
    XPManifest1: TXPManifest;
    OpenPictureDialog: TOpenDialog;
    UsedImagingTechCheckBox: TCheckBox;
    UsedImagenTechCheckBox: TCheckBox;
    FullExtractCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClearButtonClick(Sender: TObject);
    procedure AddFileButtonClick(Sender: TObject);
    procedure UsedSameDirCheckBoxClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure ExeccuteConvertButtonClick(Sender: TObject);
    procedure UsedImagingTechCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IniConf: TSectionTextData;
  end;

var
  ImgFmtConverForm: TImgFmtConverForm;

implementation

{$R *.dfm}


uses UnicodeMixedLib,
  Vcl.Imaging.pngimage,
  ieview, imageenview, imageenio, hyiedefs, hyieutils, MemoryRaster,
  ImagingTypes,
  Imaging,
  ImagingUtility,
  MemoryStream64;

procedure TImgFmtConverForm.FormCreate(Sender: TObject);
var
  fn, n: string;
  i: Integer;
begin
  IniConf := TSectionTextData.Create;

  fn := TPath.Combine(TPath.GetLibraryPath, 'ImgFmtConvert.conf');

  if TFile.Exists(fn) then
      IniConf.LoadFromFile(fn);

  SourMemo.Lines.Assign(IniConf.Strings['Sources']);
  DestEdit.Text := IniConf.GetDefaultValue('Main', 'DestPath', TPath.GetPicturesPath);
  UsedSameDirCheckBox.Checked := IniConf.GetDefaultValue('Main', 'SamePath', UsedSameDirCheckBox.Checked);

  UsedSameDirCheckBoxClick(UsedSameDirCheckBox);

  n := '';
  for i := 0 to GetFileFormatCount - 1 do
    begin
      fn := GetFilterIndexExtension(i, False);
      if fn <> '' then
        begin
          if n <> '' then
              n := n + ';*.' + fn
          else
              n := '*.' + fn;
        end;
    end;

  StateMemo.Text := 'Imaging technology:' + #13#10 + WrapText(n, #13#10, [' ', '-', #9, ';'], 80) + #13#10#13#10 +
    'ImageEN technology:' + #13#10 + WrapText(GetAllSupportedFileExtensions(True, True, False), #13#10, [' ', '-', #9, ';'], 80) + #13#10#13#10;
end;

procedure TImgFmtConverForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  fn: string;
begin
  Action := caFree;

  IniConf.Strings['Sources'].Assign(SourMemo.Lines);
  IniConf.SetDefaultValue('Main', 'DestPath', DestEdit.Text);
  IniConf.SetDefaultValue('Main', 'SamePath', UsedSameDirCheckBox.Checked);

  fn := TPath.Combine(TPath.GetLibraryPath, 'ImgFmtConvert.conf');

  IniConf.SaveToFile(fn);
  DisposeObject(IniConf);
end;

procedure TImgFmtConverForm.ClearButtonClick(Sender: TObject);
begin
  SourMemo.Clear;
end;

procedure TImgFmtConverForm.AddFileButtonClick(Sender: TObject);
var
  i: Integer;
  hashList: THashVariantList;
  n, bk: string;
begin
  if not OpenPictureDialog.Execute then
      exit;

  SourMemo.Lines.BeginUpdate;
  hashList := THashVariantList.Create;
  hashList.hashList.SetHashBlockCount(8192);
  bk := Caption;
  Enabled := False;
  for i := 0 to OpenPictureDialog.Files.Count - 1 do
    begin
      n := OpenPictureDialog.Files[i];
      if not hashList.Exists(n) then
        begin
          SourMemo.Lines.Add(n);
          hashList[n] := 1;
        end
      else
          StateMemo.Lines.Add(Format('warning: same %s', [n]));

      Caption := Format('processed %s', [umlPercentageToStr(OpenPictureDialog.Files.Count - 1, i).Text]);
      Application.ProcessMessages;
    end;
  DisposeObject(hashList);
  SourMemo.Lines.EndUpdate;
  Caption := bk;
  Enabled := True;
end;

procedure TImgFmtConverForm.UsedImagingTechCheckBoxClick(Sender: TObject);
begin
  FullExtractCheckBox.Enabled := UsedImagingTechCheckBox.Checked;
end;

procedure TImgFmtConverForm.UsedSameDirCheckBoxClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    begin
      DestEdit.Enabled := False;
      BrowseButton.Enabled := False;
      DestEdit.Color := clBtnface;
    end
  else
    begin
      DestEdit.Enabled := True;
      BrowseButton.Enabled := True;
      DestEdit.Color := clWindow;
    end;
end;

procedure TImgFmtConverForm.BrowseButtonClick(Sender: TObject);
var
  d: string;
begin
  d := DestEdit.Text;
  if Vcl.FileCtrl.SelectDirectory('output directory', '', d) then
    begin
      DestEdit.Text := d;
    end;
end;

procedure TImgFmtConverForm.ExeccuteConvertButtonClick(Sender: TObject);
  function GetDestFile(n: string): string; overload;
  const
    ext = '.png';
  begin
    if UsedSameDirCheckBox.Checked then
        Result := ChangeFileExt(n, ext)
    else
        Result := TPath.Combine(DestEdit.Text, ChangeFileExt(ExtractFileName(n), ext));
  end;

  function GetDestFile(n: string; index: Integer): string; overload;
  const
    ext = '.png';
  var
    f, d, e: string;
  begin
    d := ExtractFilePath(n);
    f := umlGetFileName(n);
    e := ExtractFileExt(f);
    f := ChangeFileExt(f, '');
    f := Format('%s%.3d%s', [f, index, ext]);

    if UsedSameDirCheckBox.Checked then
        Result := d + ChangeFileExt(f, ext)
    else
        Result := TPath.Combine(DestEdit.Text, f);
  end;

var
  i, j: Integer;
  sour, dest: string;
  ImageEN: TImageEnView;
  x, y: Integer;
  seqbmp32: TSequenceMemoryRaster;
  bmp32: TMemoryRaster;
  png: Vcl.Imaging.pngimage.TPngImage;
  ms: TMemoryStream64;
  bmp: TBitmap;
  byteptr: Vcl.Imaging.pngimage.pByteArray;
  Images: Imaging.TDynImageDataArray;
  bk: string;
begin
  if not TDirectory.Exists(DestEdit.Text) then
    begin
      MessageDlg(Format('directory no exists: %s', [DestEdit.Text]), mtError, [mbYes], 0);
      exit;
    end;

  Enabled := False;
  bk := Caption;

  for i := 0 to SourMemo.Lines.Count - 1 do
    begin
      sour := SourMemo.Lines[i];
      if not umlFileExists(sour) then
          continue;
      dest := GetDestFile(sour);
      if (umlMultipleMatch(['*.png'], sour)) and (UsedSameDirCheckBox.Checked) then
          continue;

      if (umlMultipleMatch(['*.seq'], sour)) and (TSequenceMemoryRaster.CanLoadFile(sour)) then
        begin
          seqbmp32 := TSequenceMemoryRaster.Create;
          bmp32 := TMemoryRaster.Create;
          png := Vcl.Imaging.pngimage.TPngImage.Create;

          try
            seqbmp32.LoadFromFile(sour);
            bmp32.Assign(seqbmp32);

            ms := TMemoryStream64.Create;
            bmp32.SaveToStream(ms);
            ms.Position := 0;
            bmp := TBitmap.Create;
            bmp.LoadFromStream(ms);
            DisposeObject(ms);
            png.Assign(bmp);
            DisposeObject(bmp);

            png.CreateAlpha;
            for y := 0 to bmp32.Height - 1 do
              begin
                byteptr := png.AlphaScanline[y];
                for x := 0 to bmp32.Width - 1 do
                    byteptr^[x] := bmp32.PixelAlpha[x, y];
              end;
            png.SaveToFile(dest);
          except
          end;
          DisposeObject(seqbmp32);
          DisposeObject(bmp32);
          DisposeObject(png);
          StateMemo.Lines.Add(Format('core sequence bitmap tech: %s -> %s ok!', [ExtractFileName(sour), ExtractFileName(dest)]));
        end
      else if (umlMultipleMatch('*.bmp', sour)) and (TMemoryRaster.CanLoadFile(sour)) then
        begin
          bmp32 := TMemoryRaster.Create;
          png := TPngImage.Create;

          try
            bmp32.LoadFromFile(sour);
            ms := TMemoryStream64.Create;
            bmp32.SaveToStream(ms);
            ms.Position := 0;
            bmp := TBitmap.Create;
            bmp.LoadFromStream(ms);
            DisposeObject(ms);
            png.Assign(bmp);
            DisposeObject(bmp);

            png.CreateAlpha;
            for y := 0 to bmp32.Height - 1 do
              begin
                byteptr := png.AlphaScanline[y];
                for x := 0 to bmp32.Width - 1 do
                    byteptr^[x] := bmp32.PixelAlpha[x, y];
              end;
            png.SaveToFile(dest);
          except
          end;
          DisposeObject(bmp32);
          DisposeObject(png);
          StateMemo.Lines.Add(Format('core memory bitmap tech: %s -> %s ok!', [ExtractFileName(sour), ExtractFileName(dest)]));
        end
      else if (UsedImagingTechCheckBox.Checked) and (Imaging.IsFileFormatSupported(sour)) then
        begin
          Imaging.LoadMultiImageFromFile(sour, Images);
          if FullExtractCheckBox.Checked then
            begin
              for j := low(Images) to high(Images) do
                begin
                  if Length(Images) > 1 then
                      SaveImageToFile(GetDestFile(sour, j), Images[j])
                  else
                      SaveImageToFile(GetDestFile(sour), Images[j]);
                end;
            end
          else if Length(Images) > 0 then
            begin
              SaveImageToFile(GetDestFile(sour), Images[0]);
            end;
          StateMemo.Lines.Add(Format('imaging tech: %s -> %s ok!', [ExtractFileName(sour), ExtractFileName(dest)]));
        end
      else if (UsedImagenTechCheckBox.Checked) and (imageenio.IsKnownFormat(sour)) then
        begin
          ImageEN := TImageEnView.Create(nil);
          try
            ImageEN.IO.Params.PSD_LoadLayers := True;
            ImageEN.IO.LoadFromFile(sour);

            ImageEN.IO.SaveToFilePNG(dest);
          except
          end;
          DisposeObject(ImageEN);
          StateMemo.Lines.Add(Format('imageEN tech: %s -> %s ok!', [ExtractFileName(sour), ExtractFileName(dest)]));
        end;

      Caption := Format('processed %s', [umlPercentageToStr(SourMemo.Lines.Count - 1, i).Text]);
      Application.ProcessMessages;
    end;
  Enabled := True;
  Caption := bk;
end;

initialization

TPicture.RegisterFileFormat('SEQ', 'Sequence frame bitmap', TBitmap);

end.
