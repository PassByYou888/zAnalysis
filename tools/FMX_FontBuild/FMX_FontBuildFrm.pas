unit FMX_FontBuildFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.Edit, FMX.ScrollBox, FMX.Memo,
  System.Actions, FMX.ActnList, FMX.Menus,

  System.Math, System.Threading, System.IOUtils, System.Win.Registry,
  FMX.DialogService,

  Winapi.Messages, Winapi.Windows, Winapi.ShellAPI,

  CoreClasses, ListEngine,
  ObjectData, ObjectDataManager, ItemStream, zExpression,
  MemoryStream64, MemoryRaster, Geometry2DUnit, DoStatusIO, PascalStrings,
  UnicodeMixedLib, zDrawEngine, zDrawEngineInterface_SlowFMX;

type
  TFMX_FontBuildForm = class(TForm)
    LeftLayout: TLayout;
    fontListBox: TListBox;
    Layout2: TLayout;
    Label2: TLabel;
    fntSplitterL: TSplitter;
    clientLayout: TLayout;
    pb: TPaintBox;
    sysTimer: TTimer;
    BuildFontButton: TButton;
    FilterEdit: TEdit;
    ActionList_: TActionList;
    Action_BuildFont: TAction;
    fnListPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    Layout1: TLayout;
    Label1: TLabel;
    sizeEdit: TEdit;
    CheckBox_ASCII: TCheckBox;
    CheckBox_GBK: TCheckBox;
    CheckBox_FULL: TCheckBox;
    CheckBox_AA: TCheckBox;
    CheckBox_Bold: TCheckBox;
    procedure FilterEditChange(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure pbPaint(Sender: TObject; Canvas: TCanvas);
    procedure sysTimerTimer(Sender: TObject);
    procedure Action_BuildFontExecute(Sender: TObject);
  public
    dIntf: TDrawEngineInterface_FMX;
    dEng: TDrawEngine;
    previewText: U_String;
    selFont: U_String;
    bkTex: TRaster;
    sourFontList: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshFont(filter: U_String);
  end;

var
  FMX_FontBuildForm: TFMX_FontBuildForm;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; stdcall;
procedure GetSystemFonts(FontList: TStrings);
function WaitShellExecute(sCMD, sWorkPath: string; ShowStatus: Boolean): DWord;

implementation

{$R *.fmx}

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; stdcall;
var
  Temp: U_String;
begin
  Temp := LogFont.lfFaceName;
  if Temp.First <> '@' then
      umlAddNewStrTo(Temp, TStrings(Data^));
  Result := 1;
end;

procedure GetSystemFonts(FontList: TStrings);
var
  DC: HDC;
  LFont: TLogFont;
begin
  DC := GetDC(0);
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Winapi.Windows.LPARAM(@FontList), 0);
  ReleaseDC(0, DC);
end;

function WaitShellExecute(sCMD, sWorkPath: string; ShowStatus: Boolean): DWord;
const
  BuffSize = $FFFF;
var
  ExecCode: DWord;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  pi: TProcessInformation;
  WasOK: Boolean;

  buffer: array [0 .. BuffSize] of Byte;
  BytesRead: Cardinal;
  line, n: TPascalString;
begin
  with SA do
    begin
      nLength := sizeof(SA);
      bInheritHandle := True;
      lpSecurityDescriptor := nil;
    end;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);

  with SI do
    begin
      FillChar(SI, sizeof(SI), 0);
      CB := sizeof(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;

  WasOK := CreateProcess(nil, PWideChar(sCMD), nil, nil, True, 0, nil, PWideChar(sWorkPath), SI, pi);
  CloseHandle(StdOutPipeWrite);

  if WasOK then
    begin
      repeat
        Sleep(10);
        WasOK := ReadFile(StdOutPipeRead, buffer, BuffSize, BytesRead, nil);
        if (WasOK) and (BytesRead > 0) then
          begin
            buffer[BytesRead] := 0;
            OemToAnsi(@buffer, @buffer);
            line.Append(strPas(PAnsiChar(@buffer)));
            while line.Exists([#10, #13]) do
              begin
                n := umlGetFirstStr(line, #10#13);
                line := umlDeleteFirstStr(line, #10#13);
                if ShowStatus then
                    DoStatus(n);
              end;
          end;
      until (not WasOK) or (BytesRead = 0);
      try
        WaitForSingleObject(pi.hProcess, Infinite);
        GetExitCodeProcess(pi.hProcess, Result);
      finally
        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
        CloseHandle(StdOutPipeRead);
      end;
    end
  else
    begin
      ExecCode := 0;
    end;
end;

procedure TFMX_FontBuildForm.FilterEditChange(Sender: TObject);
begin
  RefreshFont(FilterEdit.Text);
end;

procedure TFMX_FontBuildForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  dIntf.SetSurface(Canvas, Sender);
  dEng.SetSize;
  dEng.ViewOptions := [];
  dEng.DrawTile(bkTex, bkTex.BoundsRectV2, 1.0);
  dEng.Flush;
end;

procedure TFMX_FontBuildForm.pbPaint(Sender: TObject; Canvas: TCanvas);
var
  i, j: Integer;
  arry: TArrayPascalString;
  r: TDERect;
  l: TPascalStringList;
begin
  Canvas.Font.Family := 'Tahoma';
  dIntf.SetSurface(Canvas, Sender);
  dEng.SetSize;
  dEng.ViewOptions := [];
  dEng.DrawText(TCompute.State, 14, RectEdge(dEng.ScreenRectV2, -50), DEColor(1, 1, 1), False);
  dEng.Flush;

  if fontListBox.Selected <> nil then
    begin
      Canvas.Font.Family := fontListBox.Selected.Text;

      l := TPascalStringList.Create;
      umlSeparatorText(previewText, l, #13#10);
      l.FillTo(arry);
      disposeObject(l);

      dEng.ClearTextCache;
      r := dEng.DrawTextPackingInScene(arry, DEColor(1, 1, 1), 72, 10, vec2(10, 10), True);
      dEng.CameraR := r;
      dEng.Flush;
    end;
end;

procedure TFMX_FontBuildForm.sysTimerTimer(Sender: TObject);
begin
  DoStatus();
  Invalidate;
end;

constructor TFMX_FontBuildForm.Create(AOwner: TComponent);
begin
  dIntf := TDrawEngineInterface_FMX.Create();
  dEng := TDrawEngine.Create;
  dEng.ViewOptions := [voEdge];
  dEng.DrawInterface := dIntf;

  inherited Create(AOwner);

  sourFontList := TStringList.Create;
  GetSystemFonts(sourFontList);

  RefreshFont(FilterEdit.Text);
  bkTex := NewRaster();
  bkTex.SetSize(128, 128);
  FillBlackGrayBackgroundTexture(bkTex, 16, RColorF(0.55,0.55,0.55), RColorF(0.5,0.5,0.5), RColorF(0.45,0.45,0.45));

  previewText := 'abcdefg'#13#10 +
    'hijklmn'#13#10 +
    'ooqrst'#13#10 +
    'uvwxyz'#13#10 +
    'ABCDEFG'#13#10 +
    'HIJKLMN'#13#10 +
    'OPQRST'#13#10 +
    'UVWXYZ'#13#10 +
    '0123456789'#13#10 +
    '|color(0.5,1,0.5)|abcdefg世界'#13#10 +
    '|color(0.5,1,0.5)|世界hijklmn'#13#10 +
    '|color(0.5,1,0.5)|ooqrst世界'#13#10 +
    '|color(0.5,1,0.5)|世界uvwxyz'#13#10 +
    '|color(0.5,1,0.5)|ABCDEFG世界'#13#10 +
    '|color(0.5,1,0.5)|世界HIJKLMN'#13#10 +
    '|color(0.5,1,0.5)|OPQRST世界'#13#10 +
    '|color(0.5,1,0.5)|世界UVWXYZ'#13#10 +
    '0123456789'#13#10 +
    '０１２３４５６７８９'#13#10 +
    '一二三四五六七八九十'#13#10 +
    '～！＠＃￥％……＆＊（）－＝'#13#10 +
    '‘’——＋【】｛｝、｜；：“”，。《》/？“”'#13#10 +
    '您好,世界'#13#10 +
    'こんにちは、|color(0.5,0.5,1)|世界'#13#10 +
    '안녕하세요'#13#10 +
    '||台湾 |color(0,1,0)|臺灣'#13#10 +
    '~!@#$%^&*()_+-=[]{}\|;'#39':",./<>?'#13#10 +
    '|color(0.5,0.5,0.5)|color text'#13#10 +
    '|color(1,0.5,0.5)|color text'#13#10 +
    '|color(1,0,0.5)|red color'#13#10 +
    '|color(1,0.5,0)|color text'#13#10 +
    '|color(0,1,0.2)|color text'#13#10 +
    '|color(0.5,1,0.5)|color text'#13#10;

end;

destructor TFMX_FontBuildForm.Destroy;
begin
  inherited Destroy;
end;

procedure TFMX_FontBuildForm.Action_BuildFontExecute(Sender: TObject);
var
  n:U_String;
  i: Integer;
  FMX_FONTConsoleBuild: U_String;
  Cmd_: TPascalString;
  p: PPascalString;
begin
  n:=umlCombineFileName(TPath.GetLibraryPath, 'FMX_FONTConsoleBuild.EXE');
  if not umlFileExists(n) then
      exit;

  FMX_FONTConsoleBuild := n;
  for i := 0 to fontListBox.Count - 1 do
    if fontListBox.ListItems[i].IsChecked then
      begin
        Cmd_ := Format('"%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s"',
          [FMX_FONTConsoleBuild.Text,
          fontListBox.ListItems[i].Text,
          sizeEdit.Text,
          umlGetFilePath(FMX_FONTConsoleBuild).Text,
          umlBoolToStr(CheckBox_AA.IsChecked).Text,
          umlBoolToStr(CheckBox_Bold.IsChecked).Text,
          umlBoolToStr(CheckBox_ASCII.IsChecked).Text,
          umlBoolToStr(CheckBox_GBK.IsChecked).Text,
          umlBoolToStr(CheckBox_FULL.IsChecked).Text]);

        new(p);
        p^ := Cmd_;
        TCompute.RunP(p, nil, procedure(thSender: TCompute)
          begin
            WaitShellExecute(PPascalString(thSender.UserData)^, umlGetFilePath(FMX_FONTConsoleBuild), True);
            Dispose(PPascalString(thSender.UserData));
          end);
      end;
end;

procedure TFMX_FontBuildForm.RefreshFont(filter: U_String);
var
  ns: TStrings;
  i: Integer;
begin
  ns := TStringList.Create;
  ns.Assign(sourFontList);
  i := 0;
  while i < ns.Count do
    if not umlSearchMatch(filter, ns[i]) then
        ns.Delete(i)
    else
        inc(i);
  fontListBox.Items.Assign(ns);
  disposeObject(ns);
end;

end.
