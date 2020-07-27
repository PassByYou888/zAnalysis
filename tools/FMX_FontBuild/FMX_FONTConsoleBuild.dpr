program FMX_FONTConsoleBuild;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils, Windows,

  CoreClasses, ListEngine,
  MemoryStream64, MemoryRaster, Geometry2DUnit, DoStatusIO, PascalStrings, UPascalStrings,
  FastGBK,
  UnicodeMixedLib, zDrawEngine, zDrawEngineInterface_SlowFMX, FMXCharacterMapBuilder;

function GenerateArrayBuff(const ASCII_, GBK_, FULL_: Boolean): TUArrayChar;
var
  c: USystemChar;
  i, j: Integer;
begin
  SetLength(Result, TFontRaster.C_MAXWORD + 1);
  j := 0;
  for i := 0 to TFontRaster.C_MAXWORD do
    begin
      c := USystemChar(i);
      if IfGBKChar(c, ASCII_, GBK_, FULL_) then
        begin
          Result[j] := c;
          inc(j);
        end;
    end;
  SetLength(Result, j);
end;

procedure Make(const fontName_: U_String; const font_size_: Integer; const SavePath_: U_String; const AA_, BOLD_, ASCII_, GBK_, FULL_: Boolean);
var
  fr: TFontRaster;
  n: U_String;
begin
  if not(ASCII_ or GBK_ or FULL_) then
      exit;

  n := PFormat('%s_%s%d%s%s%s.zFont',
    [fontName_.ReplaceChar(#32, '_').Text, if_(BOLD_, '(BOLD)', ''), font_size_, if_(ASCII_, '(ASCII)', ''), if_(GBK_, '(GBK)', ''), if_(FULL_, '(FULL)', '')]);

  DoStatus('font: %s', [fontName_.Text]);
  DoStatus('size: %d', [font_size_]);
  DoStatus('build state:' + if_(ASCII_, ' (ASCII)', '') + if_(BOLD_, ' (BOLD)', '') + if_(GBK_, ' (GBK)', '') + if_(FULL_, ' (FULL)', ''));

  if umlFileExists(umlCombineFileName(SavePath_, n)) then
      exit;
  fr := BuildFMXCharacterAsFontRaster(AA_, fontName_,
    font_size_, BOLD_, false, GenerateArrayBuff(ASCII_, GBK_, FULL_));
  fr.Build(fontName_ + if_(AA_, ' (AA)', '') + if_(BOLD_, '(BOLD)', ''), font_size_, true);
  fr.ClearFragRaster();
  fr.SaveToFile(umlCombineFileName(SavePath_, n));
  disposeObject(fr);
  DoStatus('build %s done', [n.Text]);
end;

procedure FillParam();
var
  fontName_: U_String;
  font_size_: Integer;
  SavePath_: U_String;
  AA_, BOLD_, ASCII_, GBK_, FULL_: Boolean;
  ErrCode: Integer;
begin
  ExitCode := 1;
  if ParamCount <> 8 then
      exit;

  try
    fontName_ := ParamStr(1);
    font_size_ := umlStrToInt(ParamStr(2), 36);
    SavePath_ := ParamStr(3);
    AA_ := umlStrToBool(ParamStr(4));
    BOLD_ := umlStrToBool(ParamStr(5));
    ASCII_ := umlStrToBool(ParamStr(6));
    GBK_ := umlStrToBool(ParamStr(7));
    FULL_ := umlStrToBool(ParamStr(7));
  except
      exit;
  end;

  try
    Make(fontName_, font_size_, SavePath_, AA_, BOLD_, ASCII_, GBK_, FULL_);
    ExitCode := 0;
  except
  end;
end;

begin
  FillParam;

end.
