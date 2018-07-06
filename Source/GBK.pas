{ ****************************************************************************** }
{ * BASE GBK support,  written by QQ 600585@qq.com                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit GBK;

interface

{$INCLUDE zDefine.inc}


uses DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, UnicodeMixedLib,
  UPascalStrings;

{ any text fixed }
function GBKString(const s: TUPascalString): TUPascalString;

{ Pinyin encoding conversion, support simplified port body }
function Py(const s: TUPascalString; const multiPy: Boolean): TUPascalString;

{ Simplified to Traditional }
function S2T(const s: TUPascalString): TUPascalString;

{ Simplified to Hongkong Traditional (built-in vocabulary conversion) }
function S2HK(const s: TUPascalString): TUPascalString;

{ Traditional to Simplified (built-in vocabulary conversion) }
function T2S(const s: TUPascalString): TUPascalString;

{ Simplified to Taiwan Traditional (built-in vocabulary conversion) }
function S2TW(const s: TUPascalString): TUPascalString;

implementation

uses FastGBK, GBKMediaCenter;

function GBKChar(const C: TUPascalString): USystemString;
begin
  {$IFDEF FPC}
  Result := TUPascalString(CharDict.GetDefaultValue(TPascalString(C), TPascalString(C)));
  {$ELSE FPC}
  Result := CharDict.GetDefaultValue(C, C);
  {$ENDIF FPC}
end;

function GBKString(const s: TUPascalString): TUPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to s.Len do
      Result.Append(GBKChar(s[i]));
end;

function PY_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  Result := PYDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if Successed then
      Result := Result.ReplaceChar(',', #32)
  else
      Result := s;
end;

function Py(const s: TUPascalString; const multiPy: Boolean): TUPascalString;
var
  ns, N2, n3: TUPascalString;
  i, J      : Integer;
  Successed : Boolean;
begin
  ns := GBKString(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      J := umlMin(PYDict.HashList.MaxNameLen, ns.Len - i);
      while J > 1 do
        begin
          N2 := ns.Copy(i, J);
          n3 := PY_Table(N2, Successed);
          if Successed then
            begin
              Result.Append(n3 + #32);
              Inc(i, J);
              Break;
            end;
          Dec(J);
        end;

      if not Successed then
        begin
          N2 := PY_Table(ns[i], Successed);
          if Successed then
            begin
              if N2.Exists(#32) then
                begin
                  if multiPy then
                      Result.Append('(' + N2.ReplaceChar(#32, ',') + ')' + #32)
                  else
                      Result.Append(umlGetFirstStr(N2.Text, #32).Text + #32);
                end
              else
                  Result.Append(N2 + #32);
            end
          else
            begin
              if (Result.Len > 1) and (Result.Last = #32) and (CharIn(ns[i], ' {}[]\|:";'#39'<>?,./~!@#$%^&*()-=_+')) then
                  Result.DeleteLast;

              Result.Append(FastPY(ns[i], multiPy));
            end;
          Inc(i);
        end;
    end;
  if (Result.Len > 1) and (Result.Last = #32) then
      Result.DeleteLast;
end;

function S2T_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString; inline;
begin
  Result := s2tDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2T(const s: TUPascalString): TUPascalString;
var
  ns, N2, n3: TUPascalString;
  i, J      : Integer;
  Successed : Boolean;
begin
  ns := GBKString(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      J := umlMin(s2tDict.HashList.MaxNameLen, ns.Len - i);
      while J > 1 do
        begin
          N2 := ns.Copy(i, J);
          n3 := S2T_Table(N2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              Inc(i, J);
              Break;
            end;
          Dec(J);
        end;

      if not Successed then
        begin
          N2 := S2T_Table(ns[i], Successed);
          if Successed then
              Result.Append(N2)
          else
              Result.Append(ns[i]);
          Inc(i);
        end;
    end;
end;

function t2HK_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString; inline;
begin
  Result := t2hkDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2HK(const s: TUPascalString): TUPascalString;
var
  ns, N2, n3: TUPascalString;
  i, J      : Integer;
  Successed : Boolean;
begin
  ns := S2T(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      J := umlMin(t2hkDict.HashList.MaxNameLen, ns.Len - i);
      while J > 1 do
        begin
          N2 := ns.Copy(i, J);
          n3 := t2HK_Table(N2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              Inc(i, J);
              Break;
            end;
          Dec(J);
        end;

      if not Successed then
        begin
          N2 := t2HK_Table(ns[i], Successed);
          if Successed then
              Result.Append(N2)
          else
              Result.Append(ns[i]);
          Inc(i);
        end;
    end;
end;

function t2s_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString; inline;
begin
  Result := t2sDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function T2S(const s: TUPascalString): TUPascalString;
var
  ns, N2, n3: TUPascalString;
  i, J      : Integer;
  Successed : Boolean;
begin
  ns := s;
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      J := umlMin(t2sDict.HashList.MaxNameLen, ns.Len - i);
      while J > 1 do
        begin
          N2 := ns.Copy(i, J);
          n3 := t2s_Table(N2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              Inc(i, J);
              Break;
            end;
          Dec(J);
        end;

      if not Successed then
        begin
          N2 := t2s_Table(ns[i], Successed);
          if Successed then
              Result.Append(N2)
          else
              Result.Append(ns[i]);
          Inc(i);
        end;
    end;

  Result := GBKString(Result);
end;

function t2tw_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString; inline;
begin
  Result := t2twDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2TW(const s: TUPascalString): TUPascalString;
var
  ns, N2, n3: TUPascalString;
  i, J      : Integer;
  Successed : Boolean;
begin
  ns := S2T(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      J := umlMin(t2twDict.HashList.MaxNameLen, ns.Len - i);
      while J > 1 do
        begin
          N2 := ns.Copy(i, J);
          n3 := t2tw_Table(N2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              Inc(i, J);
              Break;
            end;
          Dec(J);
        end;

      if not Successed then
        begin
          N2 := t2tw_Table(ns[i], Successed);
          if Successed then
              Result.Append(N2)
          else
              Result.Append(ns[i]);
          Inc(i);
        end;
    end;
end;

end. 
