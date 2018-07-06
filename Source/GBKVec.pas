{ ****************************************************************************** }
{ * GBK Vector support,  written by QQ 600585@qq.com                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit GBKVec;

interface

{$INCLUDE zDefine.inc}


uses DoStatusIO, CoreClasses, PascalStrings, UPascalStrings, Variants,
  MemoryStream64, ListEngine, TextDataEngine, UnicodeMixedLib;

function WordPart(const s: TUPascalString; const Unidentified, Completed: TListPascalString): Integer; overload;
function WordPart(const s: TUPascalString): TPascalString; overload;
function WordPartN(const s: TUPascalString): TPascalString;
function WordPartD(const s: TUPascalString): TPascalString;

function WillVec(const s: TUPascalString): Integer;
function WordVec(const s: TUPascalString): Integer;

function BadEmotion(const s: TUPascalString): Integer;
function BadRep(const s: TUPascalString): Integer;
function GoodEmotion(const s: TUPascalString): Integer;
function GoodRep(const s: TUPascalString): Integer;

implementation

uses GBK, GBKMediaCenter;

function WordPart(const s: TUPascalString; const Unidentified, Completed: TListPascalString): Integer;
var
  ns, N2   : TUPascalString;
  i, J     : Integer;
  Successed: Boolean;
begin
  Completed.Clear;
  ns := GBKString(s);
  Result := 0;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      J := umlMin(WordPartDict.MaxSectionNameLen, ns.Len - i);
      while J > 1 do
        begin
          N2 := ns.Copy(i, J);
          Successed := WordPartDict.Exists(N2);
          if Successed then
            begin
              Completed.Add(N2.Text, WordPartDict.VariantList[N2.Text]);
              Inc(Result);
              Inc(i, J);
              Break;
            end;
          Dec(J);
        end;

      if not Successed then
        begin
          Successed := WordPartDict.Exists(ns[i]);
          if Successed then
            begin
              Completed.Add(ns[i], WordPartDict.VariantList[ns[i]]);
              Inc(Result);
            end
          else
            begin
              Unidentified.Add(ns[i]);
            end;
          Inc(i);
        end;
    end;
end;

function WordPart(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed   : TListPascalString;
  i           : Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.Len > 0 then
              Result.Append(',');
          Result.Append(Completed[i]);
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function WordPartN(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed   : TListPascalString;
  i           : Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.Len > 0 then
              Result.Append(' ');
          Result.Append(Completed[i].Text + '\' + VarToStr(THashVariantList(Completed.Objects[i]).GetDefaultValue('token', '')));
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function WordPartD(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed   : TListPascalString;
  i           : Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.Len > 0 then
              Result.Append(#13#10);
          Result.Append(Completed[i].Text + '(' + VarToStr(THashVariantList(Completed.Objects[i]).GetDefaultValue('desc', '')) + ')');
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function FullQuery_Table(const List: THashList; const s: TUPascalString): Integer; overload;
var
  ns, N2, n3: TUPascalString;
  i, J, L   : Integer;
  Successed : Boolean;
begin
  ns := GBKString(s);

  Result := 0;
  L := List.MaxNameLen;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      J := umlMin(L, ns.Len - i);
      while J > 1 do
        begin
          N2 := ns.Copy(i, J);
          Successed := List.Exists(N2);
          if Successed then
            begin
              Inc(Result);
              Inc(i, J);
              Break;
            end;
          Dec(J);
        end;

      if not Successed then
        begin
          Successed := List.Exists(ns[i]);
          if Successed then
              Inc(Result);
          Inc(i);
        end;
    end;
end;

function FullQuery_Table(const List: THashTextEngine; const s: TUPascalString): Integer; overload;
  function InternalQuery(const vl: THashVariantList; const ns: TUPascalString): Integer;
  var
    N2       : TUPascalString;
    i, J, L  : Integer;
    Successed: Boolean;
  begin
    Result := 0;
    L := vl.HashList.MaxNameLen;

    i := 1;
    while i <= ns.Len do
      begin
        Successed := False;
        J := umlMin(L, ns.Len - i);
        while J > 1 do
          begin
            N2 := ns.Copy(i, J);
            Successed := vl.Exists(N2);
            if Successed then
              begin
                Inc(Result);
                Inc(i, J);
                Break;
              end;
            Dec(J);
          end;

        if not Successed then
          begin
            Successed := vl.Exists(ns[i]);
            if Successed then
                Inc(Result);
            Inc(i);
          end;
      end;
  end;

var
  ns  : TUPascalString;
  i, R: Integer;
  pl  : TListPascalString;
begin
  ns := GBKString(s);
  Result := 0;
  pl := TListPascalString.Create;
  List.GetSectionList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      R := InternalQuery(List.VariantList[pl[i]], ns);
      Inc(Result, umlStrToInt(pl[i]) * R);
    end;
  DisposeObject(pl);
end;

function WillVec(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(WillVecDict, s);
end;

function WordVec(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(WordVecDict, s);
end;

function BadEmotion(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(BadEmotionDict, s);
end;

function BadRep(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(BadRepDict, s);
end;

function GoodEmotion(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(GoodEmotionDict, s);
end;

function GoodRep(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(GoodRepDict, s);
end;

end. 
