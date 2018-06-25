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

{$I zDefine.inc}


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
  ns, n2   : TUPascalString;
  i, j     : Integer;
  Successed: Boolean;
begin
  Completed.Clear;
  ns := GBKString(s);
  Result := 0;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(WordPartDict.MaxSectionNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.copy(i, j);
          Successed := WordPartDict.Exists(n2);
          if Successed then
            begin
              Completed.Add(n2.Text, WordPartDict.VariantList[n2.Text]);
              inc(Result);
              inc(i, j);
              break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := WordPartDict.Exists(ns[i]);
          if Successed then
            begin
              Completed.Add(ns[i], WordPartDict.VariantList[ns[i]]);
              inc(Result);
            end
          else
            begin
              Unidentified.Add(ns[i]);
            end;
          inc(i);
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
  disposeObject([Unidentified, Completed]);
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
  disposeObject([Unidentified, Completed]);
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
  disposeObject([Unidentified, Completed]);
end;

function FullQuery_Table(const list: THashList; const s: TUPascalString): Integer; overload;
var
  ns, n2, n3: TUPascalString;
  i, j, l   : Integer;
  Successed : Boolean;
begin
  ns := GBKString(s);

  Result := 0;
  l := list.MaxNameLen;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(l, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.copy(i, j);
          Successed := list.Exists(n2);
          if Successed then
            begin
              inc(Result);
              inc(i, j);
              break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := list.Exists(ns[i]);
          if Successed then
              inc(Result);
          inc(i);
        end;
    end;
end;

function FullQuery_Table(const list: THashTextEngine; const s: TUPascalString): Integer; overload;
  function InternalQuery(const vl: THashVariantList; const ns: TUPascalString): Integer;
  var
    n2       : TUPascalString;
    i, j, l  : Integer;
    Successed: Boolean;
  begin
    Result := 0;
    l := vl.HashList.MaxNameLen;

    i := 1;
    while i <= ns.Len do
      begin
        Successed := False;
        j := umlMin(l, ns.Len - i);
        while j > 1 do
          begin
            n2 := ns.copy(i, j);
            Successed := vl.Exists(n2);
            if Successed then
              begin
                inc(Result);
                inc(i, j);
                break;
              end;
            dec(j);
          end;

        if not Successed then
          begin
            Successed := vl.Exists(ns[i]);
            if Successed then
                inc(Result);
            inc(i);
          end;
      end;
  end;

var
  ns  : TUPascalString;
  i, r: Integer;
  pl  : TListPascalString;
begin
  ns := GBKString(s);
  Result := 0;
  pl := TListPascalString.Create;
  list.GetSectionList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      r := InternalQuery(list.VariantList[pl[i]], ns);
      inc(Result, umlStrToInt(pl[i]) * r);
    end;
  disposeObject(pl);
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
