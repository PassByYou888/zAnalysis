{ ****************************************************************************** }
{ * Core class library  written by QQ 600585@qq.com                            * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit GBK;

interface

{$I zDefine.inc}


uses SysUtils, DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, UnicodeMixedLib;

{ any text fixed }
function GBKString(const s: TPascalString): TPascalString;

{ Pinyin encoding conversion, support simplified port body }
function PY(const s: TPascalString; const multiPy: Boolean): TPascalString;

{ Simplified to Traditional }
function S2T(const s: TPascalString): TPascalString;

{ Simplified to Hongkong Traditional (built-in vocabulary conversion) }
function S2HK(const s: TPascalString): TPascalString;

{ Traditional to Simplified (built-in vocabulary conversion) }
function T2S(const s: TPascalString): TPascalString;

{ Simplified to Taiwan Traditional (built-in vocabulary conversion) }
function S2TW(const s: TPascalString): TPascalString;

implementation

uses FastGBK;

var
  CharDict, PYDict, s2tDict, t2hkDict, t2sDict, t2twDict: THashStringList;

function GBKChar(const c: SystemChar): SystemString;
begin
  Result := CharDict.GetDefaultValue(c, c);
end;

function GBKString(const s: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to s.Len do
      Result.Append(GBKChar(s[i]));
end;

function PY_Table(const s: TPascalString; var Successed: Boolean): TPascalString;
begin
  Result := PYDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if Successed then
      Result := Result.ReplaceChar(',', #32)
  else
      Result := s;
end;

function PY(const s: TPascalString; const multiPy: Boolean): TPascalString;
var
  ns, n2, n3: TPascalString;
  i, j      : Integer;
  Successed : Boolean;
begin
  ns := GBKString(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(PYDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.copy(i, j);
          n3 := PY_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3 + #32);
              inc(i, j);
              break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := PY_Table(ns[i], Successed);
          if Successed then
            begin
              if n2.Exists(#32) then
                begin
                  if multiPy then
                      Result.Append('(' + n2.ReplaceChar(#32, ',') + ')' + #32)
                  else
                      Result.Append(umlGetFirstStr(n2, #32) + #32);
                end
              else
                  Result.Append(n2 + #32);
            end
          else
            begin
              if (Result.Len > 1) and (Result.Last = #32) and (CharIn(ns[i], ' {}[]\|:";'#39'<>?,./~!@#$%^&*()-=_+')) then
                  Result.DeleteLast;

              Result.Append(FastPY(ns[i], multiPy));
            end;
          inc(i);
        end;
    end;
  if (Result.Len > 1) and (Result.Last = #32) then
      Result.DeleteLast;
end;

function S2T_Table(const s: TPascalString; var Successed: Boolean): TPascalString; inline;
begin
  Result := s2tDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2T(const s: TPascalString): TPascalString;
var
  ns, n2, n3: TPascalString;
  i, j      : Integer;
  Successed : Boolean;
begin
  ns := GBKString(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(s2tDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.copy(i, j);
          n3 := S2T_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := S2T_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;
end;

function t2HK_Table(const s: TPascalString; var Successed: Boolean): TPascalString; inline;
begin
  Result := t2hkDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2HK(const s: TPascalString): TPascalString;
var
  ns, n2, n3: TPascalString;
  i, j      : Integer;
  Successed : Boolean;
begin
  ns := S2T(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(t2hkDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.copy(i, j);
          n3 := t2HK_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := t2HK_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;
end;

function t2s_Table(const s: TPascalString; var Successed: Boolean): TPascalString; inline;
begin
  Result := t2sDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function T2S(const s: TPascalString): TPascalString;
var
  ns, n2, n3: TPascalString;
  i, j      : Integer;
  Successed : Boolean;
begin
  ns := s;
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(t2sDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.copy(i, j);
          n3 := t2s_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := t2s_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;

  Result := GBKString(Result);
end;

function t2tw_Table(const s: TPascalString; var Successed: Boolean): TPascalString; inline;
begin
  Result := t2twDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2TW(const s: TPascalString): TPascalString;
var
  ns, n2, n3: TPascalString;
  i, j      : Integer;
  Successed : Boolean;
begin
  ns := S2T(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(t2twDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.copy(i, j);
          n3 := t2tw_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := t2tw_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;
end;

procedure InitGBK;
{$I GBK_Dict.inc}
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
  CharDict := GetGBKDict(@C_CharDictPackageBuffer[0], SizeOf(T_CharDict_PackageBuffer), 20000);
  PYDict := GetGBKDict(@C_PYDictPackageBuffer[0], SizeOf(T_PYDict_PackageBuffer), 20000);
  s2tDict := GetGBKDict(@C_s2tPackageBuffer[0], SizeOf(T_s2t_PackageBuffer), 20000);
  t2hkDict := GetGBKDict(@C_t2hkPackageBuffer[0], SizeOf(T_t2hk_PackageBuffer), 20000);
  t2sDict := GetGBKDict(@C_t2sPackageBuffer[0], SizeOf(T_t2s_PackageBuffer), 20000);
  t2twDict := GetGBKDict(@C_t2twPackageBuffer[0], SizeOf(T_t2tw_PackageBuffer), 20000);
end;

procedure FreeGBK;
begin
  DisposeObject([CharDict, PYDict, s2tDict, t2hkDict, t2sDict, t2twDict]);
end;

initialization

InitGBK;

finalization

FreeGBK;

end.
