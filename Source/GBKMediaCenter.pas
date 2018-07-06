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

{$INCLUDE zDefine.inc}


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
  bigKeyDict: THashStringList;

  // big word
  bigWordDict: THashList;

{$ENDREGION 'GBKMediaCenterDecl'}

function LoadAndMergeDict(const ROOT: TPascalString): nativeInt;

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

function GBKStorePath(const ROOT: TPascalString; const DS: TDictStyle): TPascalString;
begin
  Result := umlCombinePath(ROOT, cDictName[DS]);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashStringList): nativeInt; overload;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  i: Integer;
  ori: nativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      Exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.Count;
        mergeTo.LoadFromFile(fArry[i]);
        Inc(Result, mergeTo.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      Inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashList): nativeInt; overload;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  i, J: Integer;
  lst: TListPascalString;
  ori: nativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      Exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.Count;

        lst := TListPascalString.Create;
        lst.LoadFromFile(fArry[i]);
        for J := 0 to lst.Count - 1 do
            mergeTo.Add(lst[J], nil, True);
        DisposeObject(lst);

        Inc(Result, mergeTo.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      Inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashTextEngine): nativeInt; overload;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  te: THashTextEngine;
  i: Integer;
  ori: nativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      Exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.TotalCount;

        te := THashTextEngine.Create;
        te.LoadFromFile(fArry[i]);
        mergeTo.Merge(te);
        DisposeObject(te);

        Inc(Result, mergeTo.TotalCount - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      Inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadAndMergeDict(const ROOT: TPascalString): nativeInt;
const
  cAllDict = [dsChar, dsPY, dsS2T, dsT2HK, dsT2S, dsT2TW,
    dsWordPart,
    dsWillVec,
    dsWordVec,
    dsBadEmotion, dsBadRep, dsGoodEmotion, dsGoodRep,
    dsBigKey, dsBigWord];

var
  DS: TDictStyle;
  R: nativeInt;
  ph: TPascalString;
begin
  Result := 0;
  for DS in cAllDict do
    begin
      ph := GBKStorePath(ROOT, DS);
      if not umlDirectoryExists(ph) then
          umlCreateDirectory(ph);

      R := 0;

      case DS of
        dsChar: R := LoadPath(ph, '*.txt', CharDict);
        dsPY: R := LoadPath(ph, '*.txt', PYDict);
        dsS2T: R := LoadPath(ph, '*.txt', s2tDict);
        dsT2HK: R := LoadPath(ph, '*.txt', t2hkDict);
        dsT2S: R := LoadPath(ph, '*.txt', t2sDict);
        dsT2TW: R := LoadPath(ph, '*.txt', t2twDict);
        dsWordPart: R := LoadPath(ph, '*.ini;*.txt', WordPartDict);
        dsWillVec: R := LoadPath(ph, '*.ini;*.txt', WillVecDict);
        dsWordVec: R := LoadPath(ph, '*.ini;*.txt', WordVecDict);
        dsBadEmotion: R := LoadPath(ph, '*.txt', BadEmotionDict);
        dsBadRep: R := LoadPath(ph, '*.txt', BadRepDict);
        dsGoodEmotion: R := LoadPath(ph, '*.txt', GoodEmotionDict);
        dsGoodRep: R := LoadPath(ph, '*.txt', GoodRepDict);
        dsBigKey: R := LoadPath(ph, '*.txt', bigKeyDict);
        dsBigWord: R := LoadPath(ph, '*.txt', bigWordDict);
      end;

      DoStatus('%s loaded %d ...', [cDictName[DS], R]);
      Inc(Result, R);
    end;
end;

function GetGBKTextEngineDict(Data: Pointer; siz, hashSiz: nativeInt): THashTextEngine;
var
  output: TMemoryStream64;
begin
  output := TMemoryStream64.Create;
  DecompressStream(Data, siz, output);
  output.Position := 0;
  Result := THashTextEngine.Create(hashSiz);
  Result.LoadFromStream(output);
  DisposeObject(output);
end;

function GetGBKHashStringDict(Data: Pointer; siz, hashSiz: nativeInt): THashStringList;
var
  output: TMemoryStream64;
begin
  output := TMemoryStream64.Create;
  DecompressStream(Data, siz, output);
  output.Position := 0;
  Result := THashStringList.Create(hashSiz);
  Result.LoadFromStream(output);
  DisposeObject(output);
end;

function GetGBKHashDict(Data: Pointer; siz, hashSiz: nativeInt): THashList;
var
  output: TMemoryStream64;
  lst: TListPascalString;
  i: Integer;
begin
  output := TMemoryStream64.Create;
  DecompressStream(Data, siz, output);
  output.Position := 0;
  Result := THashList.Create(hashSiz);

  lst := TListPascalString.Create;
  lst.LoadFromStream(output);
  DisposeObject(output);
  for i := 0 to lst.Count - 1 do
      Result.Add(lst[i], nil, True);
  DisposeObject(lst);
end;

{$INCLUDE GBK_Dict.inc}
{$INCLUDE GBKVec_Dict.inc}
{$INCLUDE GBKWordPart_Dict.inc}
{$INCLUDE GBKBig_MiniDict.inc}

procedure InitGBKMedia;
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
  bigKeyDict := GetGBKHashStringDict(@C_MiniKeyDictPackageBuffer[0], SizeOf(T_MiniKeyDict_PackageBuffer), 200 * 10000);

  // big word
  bigWordDict := GetGBKHashDict(@C_miniDictPackageBuffer[0], SizeOf(T_miniDict_PackageBuffer), 200 * 10000);
end;

procedure FreeGBKMedia;
begin
  DisposeObject([WordPartDict]);
  DisposeObject([WillVecDict, WordVecDict]);
  DisposeObject([BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict]);
  DisposeObject([CharDict, PYDict, s2tDict, t2hkDict, t2sDict, t2twDict]);
  DisposeObject(bigKeyDict);
  DisposeObject(bigWordDict);
end;

initialization

InitGBKMedia;

finalization

FreeGBKMedia;

end. 
