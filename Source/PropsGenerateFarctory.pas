{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit PropsGenerateFarctory;

{$INCLUDE zDefine.inc}

interface

uses CoreRULE, BioDataModule, UnicodeMixedLib, CoreClasses, PascalStrings;

type
  TPropsDMGenerateTableData = record
    fixed: Boolean;
    ID: TDataID;
    StartSeed, OverSeed: Variant;
  end;

  PPropsDMGenerateTableData = ^TPropsDMGenerateTableData;

  TPropsDMGenerateTable = array of TPropsDMGenerateTableData;
  PPropsDMGenerateTable = ^TPropsDMGenerateTable;

  TPropsDBItem = record
    Name: U_String;
    Description: U_String;
    PortraitExpression: U_String;
    Weight: Double;
    PorpsRegName: U_String;

    // Factory id
    ID: Cardinal;

    DMGenerateTable: PPropsDMGenerateTable;

    procedure Init;
    procedure Free;
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
  end;

  PPropsDBItem = ^TPropsDBItem;

  TPropsFactory = class(TCoreClassPersistent)
  private
    FPropsDB: TCoreClassList;
    FFactoryIDSeed: Cardinal;

    function GetProps(index: Integer): PPropsDBItem;
    function GetPropOfID(ID: Cardinal): PPropsDBItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function NewProp: PPropsDBItem;
    procedure Delete(ID: Cardinal);
    property Props[index: Integer]: PPropsDBItem read GetProps; default;
    property PropOfID[ID: Cardinal]: PPropsDBItem read GetPropOfID;
    function MakeFactoryID: Cardinal;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(fn: SystemString);
    procedure LoadFromFile(fn: SystemString);
  end;

var
  Factory: TPropsFactory;

implementation

uses DataFrameEngine, MemoryStream64;

procedure TPropsDBItem.Init;
begin
  Name := '';
  Description := '';
  PortraitExpression := '';
  Weight := 0;
  PorpsRegName := '';
  ID := 0;
  DMGenerateTable := nil;
end;

procedure TPropsDBItem.Free;
begin
  if DMGenerateTable <> nil then
    begin
      Dispose(DMGenerateTable);
      DMGenerateTable := nil;
    end;
end;

procedure TPropsDBItem.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  i: Integer;
  p: PPropsDMGenerateTableData;
begin
  df := TDataFrameEngine.Create;
  df.WriteString(Name);
  df.WriteString(Description);
  df.WriteString(PortraitExpression);
  df.WriteDouble(Weight);
  df.WriteString(PorpsRegName);
  df.WriteCardinal(ID);

  df.WriteInteger(length(DMGenerateTable^));

  for i := low(DMGenerateTable^) to high(DMGenerateTable^) do
    begin
      p := @DMGenerateTable^[i];
      df.WriteBool(p^.fixed);
      df.WriteInteger(Integer(p^.ID));
      df.WriteVariant(p^.StartSeed);
      df.WriteVariant(p^.OverSeed);
    end;

  df.EncodeTo(stream);
  DisposeObject(df);
end;

procedure TPropsDBItem.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  c, i: Integer;
  p: PPropsDMGenerateTableData;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  Name := df.Reader.ReadString;
  Description := df.Reader.ReadString;
  PortraitExpression := df.Reader.ReadString;
  Weight := df.Reader.ReadDouble;
  PorpsRegName := df.Reader.ReadString;
  ID := df.Reader.ReadCardinal;

  c := df.Reader.ReadInteger;
  new(DMGenerateTable);
  SetLength(DMGenerateTable^, c);
  for i := 0 to c - 1 do
    begin
      p := @DMGenerateTable^[i];
      p^.fixed := df.Reader.ReadBool;
      p^.ID := TDataID(df.Reader.ReadInteger);
      p^.StartSeed := df.Reader.ReadVariant;
      p^.OverSeed := df.Reader.ReadVariant;
    end;

  DisposeObject(df);
end;

function TPropsFactory.GetProps(index: Integer): PPropsDBItem;
begin
  Result := FPropsDB[index];
end;

function TPropsFactory.GetPropOfID(ID: Cardinal): PPropsDBItem;
var
  i: Integer;
  p: PPropsDBItem;
begin
  Result := nil;
  for i := 0 to FPropsDB.Count - 1 do
    begin
      p := FPropsDB[i];
      if p^.ID = ID then
          Exit(p);
    end;
end;

constructor TPropsFactory.Create;
begin
  inherited Create;
  FPropsDB := TCoreClassList.Create;
  FFactoryIDSeed := 1;
end;

destructor TPropsFactory.Destroy;
begin
  Clear;
  DisposeObject(FPropsDB);
  inherited Destroy;
end;

procedure TPropsFactory.Clear;
var
  i: Integer;
  p: PPropsDBItem;
begin
  for i := 0 to FPropsDB.Count - 1 do
    begin
      p := FPropsDB[i];
      p^.Free;
      Dispose(p);
    end;
  FPropsDB.Clear;
end;

function TPropsFactory.Count: Integer;
begin
  Result := FPropsDB.Count;
end;

function TPropsFactory.NewProp: PPropsDBItem;
begin
  new(Result);
  Result^.Init;
  FPropsDB.Add(Result);
end;

procedure TPropsFactory.Delete(ID: Cardinal);
var
  i: Integer;
  p: PPropsDBItem;
begin
  i := 0;
  while i < FPropsDB.Count do
    begin
      p := FPropsDB[i];
      if p^.ID = ID then
        begin
          p^.Free;
          Dispose(p);
        end
      else
          inc(i);
    end;
end;

function TPropsFactory.MakeFactoryID: Cardinal;
begin
  Result := FFactoryIDSeed + 1;
  while GetPropOfID(Result) <> nil do
      inc(Result);
  FFactoryIDSeed := Result + 1;
end;

procedure TPropsFactory.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  i: Integer;
  p: PPropsDBItem;
  ms: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;

  df.WriteCardinal(FFactoryIDSeed);
  df.WriteInteger(Count);

  for i := 0 to Count - 1 do
    begin
      p := GetProps(i);
      ms := TMemoryStream64.Create;
      p^.SaveToStream(ms);
      ms.Position := 0;
      df.WriteStream(ms);
      DisposeObject(ms);
    end;

  df.EncodeAsZLib(stream);
  DisposeObject(df);
end;

procedure TPropsFactory.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  c, i: Integer;
  p: PPropsDBItem;
  ms: TMemoryStream64;
begin
  Clear;

  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  FFactoryIDSeed := df.Reader.ReadCardinal;
  c := df.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      p := NewProp;

      ms := TMemoryStream64.Create;
      df.Reader.ReadStream(ms);
      ms.Position := 0;
      p^.LoadFromStream(ms);
      DisposeObject(ms);
    end;

  DisposeObject(df);
end;

procedure TPropsFactory.SaveToFile(fn: SystemString);
var
  ms: TMemoryStream64;
begin
  ms := TMemoryStream64.Create;
  try
      SaveToStream(ms);
  except
  end;
  ms.SaveToFile(fn);
  DisposeObject(ms);
end;

procedure TPropsFactory.LoadFromFile(fn: SystemString);
var
  ms: TMemoryStream64;
begin
  ms := TMemoryStream64.Create;
  ms.LoadFromFile(fn);
  ms.Position := 0;
  try
      LoadFromStream(ms);
  except
  end;
  DisposeObject(ms);
end;

initialization

Factory := TPropsFactory.Create;

finalization

DisposeObject(Factory);

end. 
 
 
 
