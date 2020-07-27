unit ImgFmtConverTIFFrm;

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
  TImgFmtConverTIFForm = class(TForm)
    SourMemo: TMemo;
    Label1: TLabel;
    DestEdit: TLabeledEdit;
    BrowseButton: TButton;
    UsedSameDirCheckBox: TCheckBox;
    StateMemo: TMemo;
    ExeccuteConvertButton: TButton;
    AddFileButton: TButton;
    ClearButton: TButton;
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
  ImgFmtConverTIFForm: TImgFmtConverTIFForm;

function FillParam(var errorNum: Integer): Boolean;
function ConverToTIFF(inputFiles: TStrings; UsedSameDir: Boolean; OutputDirectory: string; OnStep: TProc): Integer;

implementation

{$R *.dfm}

uses ieview, imageenview, imageenio, hyiedefs, hyieutils;

function FillParam(var errorNum: Integer): Boolean;
var
  i: Integer;
  n, tmp: U_String;
  inputFiles: TStrings;
  UsedSameDir: Boolean;
  OutputDirectory: U_String;
begin
  Result := ParamCount > 0;
  errorNum := 0;
  if not Result then
      exit;

  inputFiles := TStringList.Create;
  UsedSameDir := False;
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

  errorNum := ConverToTIFF(inputFiles, UsedSameDir, OutputDirectory, nil);
  disposeObject(inputFiles);
end;

function ConverToTIFF(inputFiles: TStrings; UsedSameDir: Boolean; OutputDirectory: string; OnStep: TProc): Integer;
  function GetDestFile(n: string): string; overload;
  const
    ext = '.tif';
  begin
    if UsedSameDir then
        Result := ChangeFileExt(n, ext)
    else
        Result := umlCombineFileName(OutputDirectory, ChangeFileExt(ExtractFilename(n), ext));
  end;

  function GetDestFile(n: string; Index: Integer): string; overload;
  const
    ext = '.tif';
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
        Result := umlCombineFileName(OutputDirectory, F);
  end;

var
  i, J: Integer;
  sour, dest: string;
  ImageEn: TImageEnView;
  raster: TMemoryRaster;
  m64: TMemoryStream64;
  BK: string;
begin
  Result := 0;
  if not umlDirectoryExists(OutputDirectory) then
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
      if (umlMultipleMatch(['*.tif', '*.tiff'], sour)) and (UsedSameDir) then
          Continue;

      if TMemoryRaster.CanLoadFile(sour) then
        begin
          try
            raster := NewRasterFromFile(sour);
            m64 := TMemoryStream64.Create;
            raster.SaveToBmp32Stream(m64);
            disposeObject(raster);
            m64.Position := 0;

            ImageEn := TImageEnView.Create(nil);
            ImageEn.IO.Params.PSD_LoadLayers := True;
            ImageEn.IO.LoadFromStreamBMP(m64);
            disposeObject(m64);
            ImageEn.IO.SaveToFileTIFF(dest);
            disposeObject(ImageEn);
            DoStatus('imageEN tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]);
          except
              inc(Result);
          end;
        end
      else if (imageenio.IsKnownFormat(sour)) then
        begin
          ImageEn := TImageEnView.Create(nil);
          try
            ImageEn.IO.Params.PSD_LoadLayers := True;
            ImageEn.IO.LoadFromFile(sour);
            ImageEn.IO.SaveToFileTIFF(dest);
            disposeObject(ImageEn);
            DoStatus('imageEN tech: %s -> %s ok!', [ExtractFilename(sour), ExtractFilename(dest)]);
          except
              inc(Result);
          end;
        end;
      if Assigned(OnStep) then
          OnStep();
    end;
end;

procedure TImgFmtConverTIFForm.FormCreate(Sender: TObject);
var
  fn: string;
begin
  AddDoStatusHook(Self, DoStatus_BackCall);
  IniConf := TSectionTextData.Create;

  fn := umlCombineFileName(TPath.GetDocumentsPath, 'ImgFmtConvertTif.conf');

  if TFile.Exists(fn) then
      IniConf.LoadFromFile(fn);

  SourMemo.Lines.Assign(IniConf.Strings['Sources']);
  DestEdit.Text := IniConf.GetDefaultValue('Main', 'DestPath', TPath.GetPicturesPath);
  UsedSameDirCheckBox.Checked := IniConf.GetDefaultValue('Main', 'SamePath', UsedSameDirCheckBox.Checked);

  UsedSameDirCheckBoxClick(UsedSameDirCheckBox);

  DoStatus(
    'ImageEN technology:' + #13#10 + WrapText(GetAllSupportedFileExtensions(True, True, False), #13#10, [' ', '-', #9, ';'], 80) + #13#10#13#10);
end;

procedure TImgFmtConverTIFForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  fn: string;
begin
  Action := caFree;

  IniConf.Strings['Sources'].Assign(SourMemo.Lines);
  IniConf.SetDefaultValue('Main', 'DestPath', DestEdit.Text);
  IniConf.SetDefaultValue('Main', 'SamePath', UsedSameDirCheckBox.Checked);

  fn := umlCombineFileName(TPath.GetDocumentsPath, 'ImgFmtConvertTif.conf');

  IniConf.SaveToFile(fn);
  disposeObject(IniConf);
end;

procedure TImgFmtConverTIFForm.ClearButtonClick(Sender: TObject);
begin
  SourMemo.Clear;
end;

procedure TImgFmtConverTIFForm.AddFileButtonClick(Sender: TObject);
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

procedure TImgFmtConverTIFForm.UsedSameDirCheckBoxClick(Sender: TObject);
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

procedure TImgFmtConverTIFForm.BrowseButtonClick(Sender: TObject);
var
  d: string;
begin
  d := DestEdit.Text;
  if Vcl.FileCtrl.SelectDirectory('output directory', '', d) then
    begin
      DestEdit.Text := d;
    end;
end;

procedure TImgFmtConverTIFForm.ExeccuteConvertButtonClick(Sender: TObject);
begin
  Enabled := False;
  ConverToTIFF(SourMemo.Lines, UsedSameDirCheckBox.Checked, DestEdit.Text, procedure
    begin
      Application.ProcessMessages;
    end);

  Enabled := True;
end;

procedure TImgFmtConverTIFForm.DoStatus_BackCall(Text_: SystemString; const ID: Integer);
begin
  StateMemo.Lines.Add(Text_);
end;

end.
