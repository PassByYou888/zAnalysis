{ ****************************************************************************** }
{ * Core class library  written by QQ 600585@qq.com                            * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit FastGBK;

interface

{$INCLUDE zDefine.inc}


uses DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, UnicodeMixedLib,
  UPascalStrings;

{ Quick query characters using the GBK encoding table }
function FastGBKChar(const C: USystemChar): Boolean; overload; inline;
{ Quick query string using the GBK encoding table }
function FastGBKString(const s: TUPascalString): Boolean; overload; inline;
{ Fast translation of Pinyin with GBK encoding table (not supporting phonetic) }
function FastPY(const s: TUPascalString; const multiPy: Boolean): TUPascalString;

{ Using the GBK coding table to sort out the phonetic alphabet quickly }
procedure FastPYSort(const inverse: Boolean; const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr);

{ custom sort }
type
  TFastCompareFuncCall   = function(const v1, v2: PPascalString)  : ShortInt;
  TFastCompareFuncMethod = function(const v1, v2: PPascalString): ShortInt of object;

  {$IFNDEF FPC} TFastCompareFuncProc = reference to function(const v1, v2: PPascalString): ShortInt; {$ENDIF FPC}

procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncCall); overload;
procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncMethod); overload;
{$IFNDEF FPC} procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncProc); overload; {$ENDIF FPC}


var
  GBKCache: packed array [$FF .. $FFFF] of TUPascalString;

implementation

uses SysUtils;

function FastGBKChar(const C: USystemChar): Boolean;
var
  ID: Cardinal;
  n : TUPascalString;
begin
  ID := Ord(C);
  if (ID >= $FF) and (ID <= $FFFF) then
      n := GBKCache[ID]
  else
      n := '';
  Result := n.Len > 0;
end;

function FastGBKString(const s: TUPascalString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if s.Len = 0 then
      Exit;
  for i := 1 to s.Len do
    if not FastGBKChar(s[i]) then
        Exit;
  Result := True;
end;

function FastPY(const s: TUPascalString; const multiPy: Boolean): TUPascalString;
var
  n      : TUPascalString;
  C      : USystemChar;
  LastGBK: Boolean;
  ID     : Cardinal;
begin
  Result := '';
  LastGBK := False;
  for C in s.buff do
    begin
      ID := Ord(C);
      if (ID >= $FF) and (ID <= $FFFF) then
          n := GBKCache[ID]
      else
          n := '';

      if n.Len > 0 then
        begin
          if n.Exists(',') then
            begin
              if multiPy then
                  n.Text := '(' + n.Text + ')'
              else
                  n := umlGetFirstStr(n.Text, ',');
            end;

          if (Result.Len > 0) then
            begin
              if LastGBK then
                  Result.Append(#32);
            end;

          LastGBK := True;
        end
      else
        begin
          n := C;

          if LastGBK then
            begin
              Result.Append(#32);
              LastGBK := False;
            end;
        end;
      Result.Append(n);
    end;
  n := '';
end;

procedure FastPYSort(const inverse: Boolean; const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr);

  function cv(const A, b: Integer): Integer; inline;
  begin
    if A = b then
        Result := 0
    else if A < b then
        Result := -1
    else
        Result := 1;
  end;

  function WasWide(const T: PPascalString): Byte; inline;
  var
    C: USystemChar;
  begin
    for C in T^.buff do
      if FastGBKChar(C) then
          Exit(1);
    Result := 0;
  end;

  function CompText(t1, t2: PPascalString): Integer;
  var
    t3: PPascalString;
  begin
    if inverse then
      begin
        t3 := t1;
        t1 := t2;
        t2 := t3;
      end;

    Result := cv(WasWide(t1), WasWide(t2));
    if Result = 0 then
      begin
        Result := cv(t1^.Len, t2^.Len);
        if (Result = 0) and (t1^.Len > 0) then
            Result := CompareText(FastPY(t1^, False).Text, FastPY(t2^, False).Text);
      end;
  end;

  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, R: Integer);
  var
    i, J: Integer;
    p, T: PPascalString;
  begin
    repeat
      i := L;
      J := R;
      p := SortList[(L + R) shr 1];
      repeat
        while CompText(SortList[i], p) < 0 do
            Inc(i);
        while CompText(SortList[J], p) > 0 do
            Dec(J);
        if i <= J then
          begin
            if i <> J then
              begin
                T := SortList[i];
                SortList[i] := SortList[J];
                SortList[J] := T;
              end;
            Inc(i);
            Dec(J);
          end;
      until i > J;
      if L < J then
          QuickSortList(SortList, L, J);
      L := i;
    until i >= R;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;

procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncCall);
  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, R: Integer);
  var
    i, J: Integer;
    p, T: PPascalString;
  begin
    repeat
      i := L;
      J := R;
      p := SortList[(L + R) shr 1];
      repeat
        while OnCompare(SortList[i], p) < 0 do
            Inc(i);
        while OnCompare(SortList[J], p) > 0 do
            Dec(J);
        if i <= J then
          begin
            if i <> J then
              begin
                T := SortList[i];
                SortList[i] := SortList[J];
                SortList[J] := T;
              end;
            Inc(i);
            Dec(J);
          end;
      until i > J;
      if L < J then
          QuickSortList(SortList, L, J);
      L := i;
    until i >= R;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;

procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncMethod);
  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, R: Integer);
  var
    i, J: Integer;
    p, T: PPascalString;
  begin
    repeat
      i := L;
      J := R;
      p := SortList[(L + R) shr 1];
      repeat
        while OnCompare(SortList[i], p) < 0 do
            Inc(i);
        while OnCompare(SortList[J], p) > 0 do
            Dec(J);
        if i <= J then
          begin
            if i <> J then
              begin
                T := SortList[i];
                SortList[i] := SortList[J];
                SortList[J] := T;
              end;
            Inc(i);
            Dec(J);
          end;
      until i > J;
      if L < J then
          QuickSortList(SortList, L, J);
      L := i;
    until i >= R;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;

{$IFNDEF FPC}


procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncProc);
  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, R: Integer);
  var
    i, J: Integer;
    p, T: PPascalString;
  begin
    repeat
      i := L;
      J := R;
      p := SortList[(L + R) shr 1];
      repeat
        while OnCompare(SortList[i], p) < 0 do
            Inc(i);
        while OnCompare(SortList[J], p) > 0 do
            Dec(J);
        if i <= J then
          begin
            if i <> J then
              begin
                T := SortList[i];
                SortList[i] := SortList[J];
                SortList[J] := T;
              end;
            Inc(i);
            Dec(J);
          end;
      until i > J;
      if L < J then
          QuickSortList(SortList, L, J);
      L := i;
    until i >= R;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;
{$ENDIF FPC}

{$INCLUDE FastGBK_Dict.inc}

procedure InitGBK;
// gbk with unpack format(unicode)
// char=py1,py2,py3
var
  output: TMemoryStream64;
  lst   : TListPascalString;
  n     : TUPascalString;
  i     : Integer;
begin
  output := TMemoryStream64.Create;
  DecompressStream(@C_FastGBKPackageBuffer[0], 71628, output);
  output.Position := 0;
  lst := TListPascalString.Create;
  lst.LoadFromStream(output);

  for i := low(GBKCache) to high(GBKCache) do
      GBKCache[i] := '';

  for i := 0 to lst.Count - 1 do
    begin
      n := lst[i];
      GBKCache[Ord(TUPascalString(umlGetFirstStr(n.Text, '=')).First)] := umlDeleteFirstStr(n.Text, '=');
    end;
  DisposeObject([lst, output]);
end;

procedure FreeGBK;
var
  i: Integer;
begin
  for i := low(GBKCache) to high(GBKCache) do
      GBKCache[i] := '';
end;

initialization

InitGBK;

finalization

FreeGBK;

end. 
