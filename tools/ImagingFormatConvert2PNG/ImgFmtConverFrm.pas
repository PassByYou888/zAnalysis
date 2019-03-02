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
  HashList: THashVariantList;
  n, BK: string;
begin
  if not OpenPictureDialog.Execute then
      Exit;

  SourMemo.Lines.BeginUpdate;
  HashList := THashVariantList.Create;
  HashList.HashList.SetHashBlockCount(8192);
  BK := Caption;
  Enabled := False;
  for i := 0 to OpenPictureDialog.Files.Count - 1 do
    begin
      n := OpenPictureDialog.Files[i];
      if not HashList.Exists(n) then
        begin
          SourMemo.Lines.Add(n);
          HashList[n] := 1;
        end
      else
          StateMemo.Lines.Add(Format('warning: same %s', [n]));

      Caption := Format('processed %s', [umlPercentageToStr(OpenPictureDialog.Files.Count - 1, i).Text]);
      Application.ProcessMessages;
    end;
  DisposeObject(HashList);
  SourMemo.Lines.EndUpdate;
  Caption := BK;
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
      DestEdit.COLOR := clBtnFace;
    end
  else
    begin
      DestEdit.Enabled := True;
      BrowseButton.Enabled := True;
      DestEdit.COLOR := clWindow;
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
        Result := TPath.Combine(DestEdit.Text, ChangeFileExt(ExtractFilename(n), ext));
  end;

  function GetDestFile(n: string; Index: Integer): string; overload;
  const
    ext = '.png';
  var
    F, d, E: string;
  begin
    d := ExtractFilePath(n);
    F := umlGetFileName(n);
    E := ExtractFileExt(F);
    F := ChangeFileExt(F, '');
    F := Format('%s%.3d%s', [F, Index, ext]);

    if UsedSameDirCheckBox.Checked then
        Result := d + ChangeFileExt(F, ext)
    else
        Result := TPath.Combine(DestEdit.Text, F);
  end;

var
  i, J: Integer;
  sour, dest: string;
  ImageEn: TImageEnView;
  X, Y: Integer;
  seqbmp32: TSequenceMemoryRaster;
  bmp32: TMemoryRaster;
  png: Vcl.Imaging.pngimage.TPngImage;
  ms: TMemoryStream64;
  bmp: TBitmap;
  byteptr: Vcl.Imaging.pngimage.PByteArray;
  images: Imaging.TDynImageDataArray;
  BK: string;
begin
  if not TDirectory.Exists(DestEdit.Text) then
    begin
      MessageDlg(Format('directory no exists: %s', [DestEdit.Text]), mtError, [mbYes], 0);
      Exit;
    end;

  Enabled := False;
  BK := Caption;

  for i := 0 to SourMemo.Lines.Count - 1 do
    begin
      sour := SourMemo.Lines[i];
      if not umlFileExists(sour) then
          Continue;
      dest := GetDestFile(sour);
      if (umlMultipleMatch(['*.png'], sour)) and (UsedSameDirCheckBox.Checked) then
          Continue;

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
            for Y := 0 to bmp32.height - 1 do
              begin
                byteptr := png.AlphaScanline[Y];
                for X := 0 to bmp32.width - 1 do
                    byteptr^[X] := bmp32.PixelAlpha[X, Y];
              end;
            png.SaveToFile(dest);
          except
          end;
          DisposeObject(seqbmp32);
          DisposeObject(bmp32);
          DisposeObject(png);
          StateMemo.Lines.Add(Format('core sequence bitmap tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]));
        end
      else if TMemoryRaster.CanLoadFile(sour) then
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
            for Y := 0 to bmp32.height - 1 do
              begin
                byteptr := png.AlphaScanline[Y];
                for X := 0 to bmp32.width - 1 do
                    byteptr^[X] := bmp32.PixelAlpha[X, Y];
              end;
            png.SaveToFile(dest);
          except
          end;
          DisposeObject(bmp32);
          DisposeObject(png);
          StateMemo.Lines.Add(Format('core memory bitmap tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]));
        end
      else if (UsedImagingTechCheckBox.Checked) and (Imaging.IsFileFormatSupported(sour)) then
        begin
          Imaging.LoadMultiImageFromFile(sour, images);
          if FullExtractCheckBox.Checked then
            begin
              for J := low(images) to high(images) do
                begin
                  if length(images) > 1 then
                      SaveImageToFile(GetDestFile(sour, J), images[J])
                  else
                      SaveImageToFile(GetDestFile(sour), images[J]);
                end;
            end
          else if length(images) > 0 then
            begin
              SaveImageToFile(GetDestFile(sour), images[0]);
            end;
          StateMemo.Lines.Add(Format('imaging tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]));
        end
      else if (UsedImagenTechCheckBox.Checked) and (imageenio.IsKnownFormat(sour)) then
        begin
          ImageEn := TImageEnView.Create(nil);
          try
            ImageEn.IO.Params.PSD_LoadLayers := True;
            ImageEn.IO.LoadFromFile(sour);

            ImageEn.IO.SaveToFilePNG(dest);
          except
          end;
          DisposeObject(ImageEn);
          StateMemo.Lines.Add(Format('imageEN tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]));
        end;

      Caption := Format('processed %s', [umlPercentageToStr(SourMemo.Lines.Count - 1, i).Text]);
      Application.ProcessMessages;
    end;
  Enabled := True;
  Caption := BK;
end;

initialization

TPicture.RegisterFileFormat('SEQ', 'Sequence frame bitmap', TBitmap);
TPicture.RegisterFileFormat('JLS', 'Sequence frame bitmap', TBitmap);

end.
