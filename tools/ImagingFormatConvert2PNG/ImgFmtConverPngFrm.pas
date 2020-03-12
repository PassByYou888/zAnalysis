unit ImgFmtConverPngFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtDlgs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.FileCtrl,

  Vcl.Graphics,

  System.IOUtils,

  TextDataEngine, CoreClasses, ListEngine, PascalStrings, DoStatusIO,
  UnicodeMixedLib, MemoryRaster, MemoryStream64;

type
  TImgFmtConverPngForm = class(TForm)
    SourMemo: TMemo;
    Label1: TLabel;
    DestEdit: TLabeledEdit;
    BrowseButton: TButton;
    UsedSameDirCheckBox: TCheckBox;
    StateMemo: TMemo;
    ExeccuteConvertButton: TButton;
    AddFileButton: TButton;
    ClearButton: TButton;
    UsedImagingTechCheckBox: TCheckBox;
    UsedImagenTechCheckBox: TCheckBox;
    FullExtractCheckBox: TCheckBox;
    OpenPictureDialog: TFileOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClearButtonClick(Sender: TObject);
    procedure AddFileButtonClick(Sender: TObject);
    procedure UsedSameDirCheckBoxClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure ExeccuteConvertButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IniConf: TSectionTextData;
    procedure DoStatus_BackCall(Text_: SystemString; const ID: Integer);
  end;

var
  ImgFmtConverPngForm: TImgFmtConverPngForm;

function FillParam(var errorNum: Integer): Boolean;
function ConverToPNG(inputFiles: TStrings; UsedSameDir, ImagingTech, ImageEnTech, FullExtract: Boolean; OutputDirectory: string; OnStep: TProc): Integer;

implementation

{$R *.dfm}


uses
  Vcl.Imaging.pngimage, GR32,
  ieview, imageenview, imageenio, hyiedefs, hyieutils,
  ImagingTypes,
  Imaging,
  ImagingUtility;

function FillParam(var errorNum: Integer): Boolean;
var
  i: Integer;
  n, tmp: U_String;
  inputFiles: TStrings;
  UsedSameDir, ImagingTech, ImageEnTech, FullExtract: Boolean;
  OutputDirectory: U_String;
begin
  Result := ParamCount > 0;
  errorNum := 0;
  if not Result then
      exit;

  inputFiles := TStringList.Create;
  UsedSameDir := False;
  ImagingTech := True;
  ImageEnTech := True;
  FullExtract := False;
  OutputDirectory := TPath.GetLibraryPath;

  for i := 1 to ParamCount do
    begin
      n := ParamStr(i);
      if umlMultipleMatch(['-i:*', '-input:*'], n) then
        begin
          tmp := umlTrimSpace(umlDeleteFirstStr(n, ':'));
          if umlFileExists(tmp) then
              inputFiles.Add(tmp);
        end;
      if umlMultipleMatch(['-iConf:*', '-ic:*', '-inputConfig:*', '-inputConf:*'], n) then
        begin
          tmp := umlTrimSpace(umlDeleteFirstStr(n, ':'));
          if umlFileExists(tmp) then
              inputFiles.LoadFromFile(tmp, TEncoding.UTF8);
        end;

      if umlMultipleMatch(['-SameDir', '-UsedSameDir'], n) then
        begin
          UsedSameDir := True;
        end;
      if umlMultipleMatch(['-NoSameDir', '-NoUsedSameDir'], n) then
        begin
          UsedSameDir := False;
        end;

      if umlMultipleMatch(['-ImagingTech', '-Imaging'], n) then
        begin
          ImagingTech := True;
        end;
      if umlMultipleMatch(['-NoImagingTech', '-NoImaging'], n) then
        begin
          ImagingTech := False;
        end;

      if umlMultipleMatch(['-ImageEnTech', '-ImageEn'], n) then
        begin
          ImageEnTech := True;
        end;
      if umlMultipleMatch(['-NoImageEnTech', '-NoImageEn'], n) then
        begin
          ImageEnTech := False;
        end;

      if umlMultipleMatch(['-FullExtract', '-Full'], n) then
        begin
          FullExtract := True;
        end;
      if umlMultipleMatch(['-NoFullExtract', '-NoFull'], n) then
        begin
          FullExtract := False;
        end;

      if umlMultipleMatch(['-o:*', '-output:*', '-OutputDirectory:*'], n) then
        begin
          tmp := umlTrimSpace(umlDeleteFirstStr(n, ':'));
          if tmp.Same('SameDir') then
            begin
              UsedSameDir := True;
            end
          else
            begin
              UsedSameDir := False;
              OutputDirectory := tmp;
              if not umlDirectoryExists(OutputDirectory) then
                  umlCreateDirectory(OutputDirectory);
            end;
        end;
    end;

  errorNum := ConverToPNG(inputFiles, UsedSameDir, ImagingTech, ImageEnTech, FullExtract, OutputDirectory, nil);
  disposeObject(inputFiles);
end;

function ConverToPNG(inputFiles: TStrings; UsedSameDir, ImagingTech, ImageEnTech, FullExtract: Boolean; OutputDirectory: string; OnStep: TProc): Integer;
  function GetDestFile(n: string): string; overload;
  const
    ext = '.png';
  begin
    if UsedSameDir then
        Result := ChangeFileExt(n, ext)
    else
        Result := TPath.Combine(OutputDirectory, ChangeFileExt(ExtractFilename(n), ext));
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

    if UsedSameDir then
        Result := d + ChangeFileExt(F, ext)
    else
        Result := TPath.Combine(OutputDirectory, F);
  end;

var
  i, J, K: Integer;
  sour, dest: string;
  ImageEn: TImageEnView;
  X, Y: Integer;
  SeqRaster: TSequenceMemoryRaster;
  raster: TMemoryRaster;
  bmp32: TBitmap32;
  png: Vcl.Imaging.pngimage.TPngImage;
  ms: TMemoryStream64;
  bmp: TBitmap;
  byteptr: Vcl.Imaging.pngimage.PByteArray;
  images: Imaging.TDynImageDataArray;
  BK: string;
begin
  Result := 0;
  if not TDirectory.Exists(OutputDirectory) then
    begin
      inc(Result);
      DoStatus('directory no exists: %s', [OutputDirectory]);
      exit;
    end;

  for i := 0 to inputFiles.Count - 1 do
    begin
      sour := inputFiles[i];
      if not umlFileExists(sour) then
          Continue;
      dest := GetDestFile(sour);
      if (umlMultipleMatch(['*.png'], sour)) and (UsedSameDir) then
          Continue;

      if (umlMultipleMatch(['*.seq'], sour)) and (TSequenceMemoryRaster.CanLoadFile(sour)) then
        begin
          SeqRaster := TSequenceMemoryRaster.Create;
          raster := TMemoryRaster.Create;
          png := Vcl.Imaging.pngimage.TPngImage.Create;

          try
            SeqRaster.LoadFromFile(sour);
            raster.Assign(SeqRaster);

            ms := TMemoryStream64.Create;
            raster.SaveToStream(ms);
            ms.Position := 0;
            bmp := TBitmap.Create;
            bmp.LoadFromStream(ms);
            disposeObject(ms);
            png.Assign(bmp);
            disposeObject(bmp);

            png.CreateAlpha;
            for Y := 0 to raster.height - 1 do
              begin
                byteptr := png.AlphaScanline[Y];
                for X := 0 to raster.width - 1 do
                    byteptr^[X] := raster.PixelAlpha[X, Y];
              end;
            png.SaveToFile(dest);
            DoStatus('core sequence bitmap tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]);
          except
              inc(Result);
          end;
          disposeObject(SeqRaster);
          disposeObject(raster);
          disposeObject(png);
        end
      else if TMemoryRaster.CanLoadFile(sour) then
        begin
          raster := TMemoryRaster.Create;
          png := TPngImage.Create;

          try
            raster.LoadFromFile(sour);
            ms := TMemoryStream64.Create;
            raster.SaveToStream(ms);
            ms.Position := 0;
            bmp := TBitmap.Create;
            bmp.LoadFromStream(ms);
            disposeObject(ms);
            png.Assign(bmp);
            disposeObject(bmp);

            png.CreateAlpha;
            for Y := 0 to raster.height - 1 do
              begin
                byteptr := png.AlphaScanline[Y];
                for X := 0 to raster.width - 1 do
                    byteptr^[X] := raster.PixelAlpha[X, Y];
              end;
            png.SaveToFile(dest);
            DoStatus('core memory bitmap tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]);
          except
              inc(Result);
          end;
          disposeObject(raster);
          disposeObject(png);
        end
      else if (ImagingTech) and (Imaging.IsFileFormatSupported(sour)) then
        begin
          try
            Imaging.LoadMultiImageFromFile(sour, images);
            if FullExtract then
              begin
                for J := low(images) to high(images) do
                  begin
                    if length(images) > 1 then
                        SaveImageToFile(GetDestFile(sour, J), images[J])
                    else
                        SaveImageToFile(GetDestFile(sour), images[J]);
                    DoStatus('imaging tech: %s -> %s ok!', [ExtractFilename(sour), GetDestFile(sour, J)]);
                    if Assigned(OnStep) then
                        OnStep();
                  end;
              end
            else if length(images) > 0 then
              begin
                SaveImageToFile(GetDestFile(sour), images[0]);
                DoStatus('imaging tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]);
              end;
          except
              inc(Result);
          end;
        end
      else if (ImageEnTech) and (imageenio.IsKnownFormat(sour)) then
        begin
          ImageEn := TImageEnView.Create(nil);
          try
            ImageEn.IO.Params.PSD_LoadLayers := True;
            ImageEn.IO.LoadFromFile(sour);

            if ImageEn.LayersCount > 1 then
              begin
                for J := 0 to ImageEn.LayersCount - 1 do
                  begin
                    bmp := TBitmap.Create;
                    ImageEn.Layers[J].Bitmap.CopyToTBitmap(bmp);
                    bmp32 := TBitmap32.Create();
                    bmp32.Assign(bmp);
                    disposeObject(bmp);

                    raster := TMemoryRaster.Create;
                    raster.SetSize(bmp32.width, bmp32.height);
                    for Y := 0 to bmp32.height - 1 do
                      for X := 0 to bmp32.width - 1 do
                        with TColor32Entry(bmp32.Pixel[X, Y]) do
                            raster[X, Y] := RColor(r, g, b, ImageEn.Layers[J].Bitmap.Alpha[X, Y]);

                    raster.SaveToPNGFile(GetDestFile(sour, J));
                    DoStatus('imageEN tech: %s -> %s ok!', [ExtractFilename(sour), GetDestFile(sour, J)]);
                    disposeObject(bmp32);
                    disposeObject(raster);
                    if Assigned(OnStep) then
                        OnStep();
                  end;
              end
            else
              begin
                ImageEn.IO.SaveToFilePNG(dest);
                DoStatus('imageEN tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]);
              end;
          except
              inc(Result);
          end;
          disposeObject(ImageEn);
        end;
      if Assigned(OnStep) then
          OnStep();
    end;
end;

procedure TImgFmtConverPngForm.FormCreate(Sender: TObject);
var
  fn, n: string;
  i: Integer;
begin
  AddDoStatusHook(Self, DoStatus_BackCall);
  IniConf := TSectionTextData.Create;

  fn := TPath.Combine(TPath.GetDocumentsPath, 'ImgFmtConvertPng.conf');

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

procedure TImgFmtConverPngForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  fn: string;
begin
  Action := caFree;

  IniConf.Strings['Sources'].Assign(SourMemo.Lines);
  IniConf.SetDefaultValue('Main', 'DestPath', DestEdit.Text);
  IniConf.SetDefaultValue('Main', 'SamePath', UsedSameDirCheckBox.Checked);

  fn := TPath.Combine(TPath.GetDocumentsPath, 'ImgFmtConvertPng.conf');

  IniConf.SaveToFile(fn);
  disposeObject(IniConf);
end;

procedure TImgFmtConverPngForm.ClearButtonClick(Sender: TObject);
begin
  SourMemo.Clear;
end;

procedure TImgFmtConverPngForm.AddFileButtonClick(Sender: TObject);
var
  i: Integer;
  HashList: THashVariantList;
  n, BK: string;
begin
  if not OpenPictureDialog.Execute then
      exit;

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
          DoStatus(Format('warning: same %s', [n]));

      Caption := Format('processed %s', [umlPercentageToStr(OpenPictureDialog.Files.Count - 1, i).Text]);
      Application.ProcessMessages;
    end;
  disposeObject(HashList);
  SourMemo.Lines.EndUpdate;
  Caption := BK;
  Enabled := True;
end;

procedure TImgFmtConverPngForm.UsedSameDirCheckBoxClick(Sender: TObject);
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

procedure TImgFmtConverPngForm.BrowseButtonClick(Sender: TObject);
var
  d: string;
begin
  d := DestEdit.Text;
  if Vcl.FileCtrl.SelectDirectory('output directory', '', d) then
    begin
      DestEdit.Text := d;
    end;
end;

procedure TImgFmtConverPngForm.ExeccuteConvertButtonClick(Sender: TObject);
begin
  Enabled := False;
  ConverToPNG(SourMemo.Lines,
    UsedSameDirCheckBox.Checked,
    UsedImagingTechCheckBox.Checked,
    UsedImagenTechCheckBox.Checked,
    FullExtractCheckBox.Checked,
    DestEdit.Text, procedure
    begin
      Application.ProcessMessages;
    end);
  Enabled := True;
end;

procedure TImgFmtConverPngForm.DoStatus_BackCall(Text_: SystemString; const ID: Integer);
begin
  StateMemo.Lines.Add(Text_);
end;

end.
