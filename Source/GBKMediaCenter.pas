{ ****************************************************************************** }
{ * GBK media Data support, writen by QQ 600585@qq.com                         * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit GBKMediaCenter;

interface

{$I zDefine.inc}


uses DoStatusIO, CoreClasses, PascalStrings, UPascalStrings,
  MemoryStream64, ListEngine, TextDataEngine, UnicodeMixedLib;

{$REGION 'GBKMediaCenterDecl'}


var
  // gbk base dict
  CharDict, PYDict, s2tDict, t2hkDict, t2sDict, t2twDict: THashStringList;

  // word part
  WordPartDict: THashTextEngine;
  // will vec
  WillVecDict: THashTextEngine;
  // word vec
  WordVecDict: THashTextEngine;

  // emotion and rep dict
  BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict: THashList;

  // big key
  BigKeyDict: THashStringList;

  // big word
  BigWordDict: THashList;

  {$ENDREGION 'GBKMediaCenterDecl'}

function LoadAndMergeDict(const root: TPascalString): NativeInt;

implementation


type
  TDictStyle = (dsChar, dsPY, dsS2T, dsT2HK, dsT2S, dsT2TW,
    dsWordPart,
    dsWillVec,
    dsWordVec,
    dsBadEmotion, dsBadRep, dsGoodEmotion, dsGoodRep,
    dsBigKey, dsBigWord
    );

const
  cDictName: array [TDictStyle] of string = (
    ('键值词库-字符'),
    ('键值词库-拼音'),
    ('键值词库-简体转繁体'),
    ('键值词库-繁体转港繁体'),
    ('键值词库-繁体转简体'),
    ('键值词库-繁体转台湾体'),
    ('分块文本词库-词性'),
    ('分块文本词库-意志'),
    ('分块文本词库-度量'),
    ('文本词库-情感负向'),
    ('文本词库-回馈负向'),
    ('文本词库-情感正向'),
    ('文本词库-回馈正向'),
    ('大规模键值词库-分词库'),
    ('大规模文本词库-分词库')
    );

function GBKStorePath(const root: TPascalString; const ds: TDictStyle): TPascalString;
begin
  Result := umlCombinePath(root, cDictName[ds]);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashStringList): NativeInt; overload;
var
  fArry: umlStringDynArray;
  pArry: umlStringDynArray;
  i    : Integer;
  ori  : NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.Count;
        mergeTo.LoadFromFile(fArry[i]);
        inc(Result, mergeTo.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashList): NativeInt; overload;
var
  fArry: umlStringDynArray;
  pArry: umlStringDynArray;
  i, j : Integer;
  lst  : TListPascalString;
  ori  : NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.Count;

        lst := TListPascalString.Create;
        lst.LoadFromFile(fArry[i]);
        for j := 0 to lst.Count - 1 do
            mergeTo.Add(lst[j], nil);
        disposeObject(lst);

        inc(Result, mergeTo.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashTextEngine): NativeInt; overload;
var
  fArry: umlStringDynArray;
  pArry: umlStringDynArray;
  te   : THashTextEngine;
  i    : Integer;
  ori  : NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.TotalCount;

        te := THashTextEngine.Create;
        te.LoadFromFile(fArry[i]);
        mergeTo.Merge(te);
        disposeObject(te);

        inc(Result, mergeTo.TotalCount - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadAndMergeDict(const root: TPascalString): NativeInt;
const
  cAllDict = [dsChar, dsPY, dsS2T, dsT2HK, dsT2S, dsT2TW,
    dsWordPart,
    dsWillVec,
    dsWordVec,
    dsBadEmotion, dsBadRep, dsGoodEmotion, dsGoodRep,
    dsBigKey, dsBigWord];

var
  ds: TDictStyle;
  r : NativeInt;
  ph: TPascalString;
begin
  Result := 0;
  for ds in cAllDict do
    begin
      ph := GBKStorePath(root, ds);
      if not umlDirectoryExists(ph) then
          umlCreateDirectory(ph);

      r := 0;

      case ds of
        dsChar: r := LoadPath(ph, '*.txt', CharDict);
        dsPY: r := LoadPath(ph, '*.txt', PYDict);
        dsS2T: r := LoadPath(ph, '*.txt', s2tDict);
        dsT2HK: r := LoadPath(ph, '*.txt', t2hkDict);
        dsT2S: r := LoadPath(ph, '*.txt', t2sDict);
        dsT2TW: r := LoadPath(ph, '*.txt', t2twDict);
        dsWordPart: r := LoadPath(ph, '*.ini;*.txt', WordPartDict);
        dsWillVec: r := LoadPath(ph, '*.ini;*.txt', WillVecDict);
        dsWordVec: r := LoadPath(ph, '*.ini;*.txt', WordVecDict);
        dsBadEmotion: r := LoadPath(ph, '*.txt', BadEmotionDict);
        dsBadRep: r := LoadPath(ph, '*.txt', BadRepDict);
        dsGoodEmotion: r := LoadPath(ph, '*.txt', GoodEmotionDict);
        dsGoodRep: r := LoadPath(ph, '*.txt', GoodRepDict);
        dsBigKey: r := LoadPath(ph, '*.txt', BigKeyDict);
        dsBigWord: r := LoadPath(ph, '*.txt', BigWordDict);
      end;

      DoStatus('%s loaded %d ...', [cDictName[ds], r]);
      inc(Result, r);
    end;
end;

function GetGBKTextEngineDict(data: Pointer; siz, hashSiz: NativeInt): THashTextEngine;
var
  Output: TMemoryStream64;
begin
  Output := TMemoryStream64.Create;
  DecompressStream(data, siz, Output);
  Output.Position := 0;
  Result := THashTextEngine.Create(hashSiz);
  Result.LoadFromStream(Output);
  disposeObject(Output);
end;

function GetGBKHashStringDict(data: Pointer; siz, hashSiz: NativeInt): THashStringList;
var
  Output: TMemoryStream64;
begin
  Output := TMemoryStream64.Create;
  DecompressStream(data, siz, Output);
  Output.Position := 0;
  Result := THashStringList.Create(hashSiz);
  Result.LoadFromStream(Output);
  disposeObject(Output);
end;

function GetGBKHashDict(data: Pointer; siz, hashSiz: NativeInt): THashList;
var
  Output: TMemoryStream64;
  lst   : TListPascalString;
  i     : Integer;
begin
  Output := TMemoryStream64.Create;
  DecompressStream(data, siz, Output);
  Output.Position := 0;
  Result := THashList.Create(hashSiz);

  lst := TListPascalString.Create;
  lst.LoadFromStream(Output);
  disposeObject(Output);
  for i := 0 to lst.Count - 1 do
      Result.Add(lst[i], nil);
  disposeObject(lst);
end;

procedure InitGBKMedia;
{$I GBK_Dict.inc}
{$I GBKVec_Dict.inc}
{$I GBKWordPart_Dict.inc}
{$I GBKBig_MiniDict.inc}
begin
  // base gbk dict
  CharDict := GetGBKHashStringDict(@C_CharDictPackageBuffer[0], SizeOf(T_CharDict_PackageBuffer), 20000);
  PYDict := GetGBKHashStringDict(@C_PYDictPackageBuffer[0], SizeOf(T_PYDict_PackageBuffer), 20000);
  s2tDict := GetGBKHashStringDict(@C_s2tPackageBuffer[0], SizeOf(T_s2t_PackageBuffer), 20000);
  t2hkDict := GetGBKHashStringDict(@C_t2hkPackageBuffer[0], SizeOf(T_t2hk_PackageBuffer), 20000);
  t2sDict := GetGBKHashStringDict(@C_t2sPackageBuffer[0], SizeOf(T_t2s_PackageBuffer), 20000);
  t2twDict := GetGBKHashStringDict(@C_t2twPackageBuffer[0], SizeOf(T_t2tw_PackageBuffer), 20000);

  // word part dict
  WordPartDict := GetGBKTextEngineDict(@C_WordPartPackageBuffer[0], SizeOf(T_WordPart_PackageBuffer), 50000);

  // will vec dict
  WillVecDict := GetGBKTextEngineDict(@C_willVecDictPackageBuffer[0], SizeOf(T_willVecDict_PackageBuffer), 5000);

  // word vec dict
  WordVecDict := GetGBKTextEngineDict(@C_WordVecDictPackageBuffer[0], SizeOf(T_WordVecDict_PackageBuffer), 5000);

  // emotion and rep dict
  BadEmotionDict := GetGBKHashDict(@C_BadEmotionDictPackageBuffer[0], SizeOf(T_BadEmotionDict_PackageBuffer), 20000);
  BadRepDict := GetGBKHashDict(@C_BadRepDictPackageBuffer[0], SizeOf(T_BadRepDict_PackageBuffer), 20000);
  GoodEmotionDict := GetGBKHashDict(@C_GoodEmotionDictPackageBuffer[0], SizeOf(T_GoodEmotionDict_PackageBuffer), 20000);
  GoodRepDict := GetGBKHashDict(@C_GoodRepDictPackageBuffer[0], SizeOf(T_GoodRepDict_PackageBuffer), 20000);

  // big key
  BigKeyDict := GetGBKHashStringDict(@C_MiniKeyDictPackageBuffer[0], SizeOf(T_MiniKeyDict_PackageBuffer), 200 * 10000);

  // big word
  BigWordDict := GetGBKHashDict(@C_MiniDictPackageBuffer[0], SizeOf(T_MiniDict_PackageBuffer), 200 * 10000);
end;

procedure FreeGBKMedia;
begin
  disposeObject([WordPartDict]);
  disposeObject([WillVecDict, WordVecDict]);
  disposeObject([BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict]);
  disposeObject([CharDict, PYDict, s2tDict, t2hkDict, t2sDict, t2twDict]);
  disposeObject(BigKeyDict);
  disposeObject(BigWordDict);
end;

initialization

InitGBKMedia;

finalization

FreeGBKMedia;

end.
