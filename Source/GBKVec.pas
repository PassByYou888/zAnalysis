unit GBKVec;

interface

{$I zDefine.inc}


uses SysUtils, DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, TextDataEngine, UnicodeMixedLib;

implementation

uses GBK;

var
  WillVecDict, WordVecDict: THashTextEngine;

var
  BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict: THashStringList;

function Query_Table(const list: THashStringList; const s: TPascalString; var Successed: Boolean): TPascalString; inline;
begin
  Result := list.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

procedure InitGBK;
{$I GBKVec_Dict.inc}
  function GetGBKINIDict(data: Pointer; siz, hashSiz: NativeInt): THashTextEngine;
  var
    Output: TMemoryStream64;
    lst   : TListPascalString;
    n     : TPascalString;
    i     : Integer;
  begin
    Output := TMemoryStream64.Create;
    DecompressStream(data, siz, Output);
    Output.Position := 0;
    Result := THashTextEngine.Create(hashSiz);
    Result.LoadFromStream(Output);
    DisposeObject(Output);
  end;

  function GetGBKDict(data: Pointer; siz, hashSiz: NativeInt): THashStringList;
  var
    Output: TMemoryStream64;
    lst   : TListPascalString;
    n     : TPascalString;
    i     : Integer;
  begin
    Output := TMemoryStream64.Create;
    DecompressStream(data, siz, Output);
    Output.Position := 0;
    Result := THashStringList.Create(hashSiz);
    Result.LoadFromStream(Output);
    DisposeObject(Output);
  end;

begin
  WillVecDict := GetGBKINIDict(@C_willVecDictPackageBuffer[0], SizeOf(T_willVecDict_PackageBuffer), 4000);
  WordVecDict := GetGBKINIDict(@C_WordVecDictPackageBuffer[0], SizeOf(T_WordVecDict_PackageBuffer), 4000);
  BadEmotionDict := GetGBKDict(@C_BadEmotionDictPackageBuffer[0], SizeOf(T_BadEmotionDict_PackageBuffer), 20000);
  BadRepDict := GetGBKDict(@C_BadRepDictPackageBuffer[0], SizeOf(T_BadRepDict_PackageBuffer), 20000);
  GoodEmotionDict := GetGBKDict(@C_GoodEmotionDictPackageBuffer[0], SizeOf(T_GoodEmotionDict_PackageBuffer), 20000);
  GoodRepDict := GetGBKDict(@C_GoodRepDictPackageBuffer[0], SizeOf(T_GoodRepDict_PackageBuffer), 20000);
end;

procedure FreeGBK;
begin
  DisposeObject([WillVecDict, WordVecDict]);
  DisposeObject([BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict]);
end;

initialization

InitGBK;

finalization

FreeGBK;

end.
