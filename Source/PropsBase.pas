{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit PropsBase;

{$INCLUDE zDefine.inc}

interface

uses BaseRULEClasses, UnicodeMixedLib, NumberBase, SpellBase, CoreClasses,
  PascalStrings,
  Geometry2DUnit, BioDataModule, BioBase, PropsMaterialUnit;

type
  TPropsOwnerType = (potBattleRegion, potBag, potUnknow);

  TProps = class(TPropsBase)
  private
    FWidth: Integer;
    FHeight: Integer;
    FName: U_String;
    FDescription: U_String;
    FPortraitExpression: U_String;
    FWeight: Double;
    FOwner: TCoreClassPersistent;
    FRegionPosition: TVec2;
    FID: Integer;
    FOwnerType: TPropsOwnerType;
    FOwnerRegionID: Integer;
    FOwnerBagBioID: Integer;
  public
    constructor Create(AOwner: TCoreClassPersistent); virtual;
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream); virtual;
    procedure LoadFromStream(stream: TCoreClassStream); virtual;

    property width: Integer read FWidth write FWidth;
    property height: Integer read FHeight write FHeight;

    property Name: U_String read FName write FName;
    property Description: U_String read FDescription write FDescription;
    property PortraitExpression: U_String read FPortraitExpression write FPortraitExpression;

    // weight
    property Weight: Double read FWeight write FWeight;

    // owner：region,bag,container
    property Owner: TCoreClassPersistent read FOwner write FOwner;
    property RegionPosition: TVec2 read FRegionPosition write FRegionPosition;

    // props in world id
    property ID: Integer read FID write FID;

    property OwnerType: TPropsOwnerType read FOwnerType write FOwnerType;
    property OwnerRegionID: Integer read FOwnerRegionID write FOwnerRegionID;
    property OwnerBagBioID: Integer read FOwnerBagBioID write FOwnerBagBioID;
  end;

  TPropsClass = class of TProps;

  TRegProps = record
    Regname: U_String;
    PropsClass: TPropsClass;
  end;

  PRegProps = ^TRegProps;

var
  PropsRegistedContainer: TCoreClassList = nil;

function RegPropsClass(Regname: U_String; PropsClass: TPropsClass): Boolean;
function GetRegPropsClass(Regname: U_String): TPropsClass;
function GetPropsClassRegName(PropsClass: TClass): U_String;

implementation

uses CoreRULE, DoStatusIO, DataFrameEngine;

procedure InitPropsRegistedContainer;
begin
  if PropsRegistedContainer = nil then
      PropsRegistedContainer := TCoreClassList.Create;
end;

procedure FreePropsRegistedContainer;
var
  i: Integer;
  p: PRegProps;
begin
  for i := 0 to PropsRegistedContainer.Count - 1 do
    begin
      p := PropsRegistedContainer[i];
      Dispose(p);
    end;
  DisposeObject(PropsRegistedContainer);
  PropsRegistedContainer := nil;
end;

function ExistsRegPropsClass(Regname: U_String): Boolean;
var
  i: Integer;
  p: PRegProps;
begin
  InitPropsRegistedContainer;
  Result := True;
  for i := 0 to PropsRegistedContainer.Count - 1 do
    begin
      p := PropsRegistedContainer[i];
      if umlMultipleMatch(True, Regname, p^.Regname) then
          Exit;
    end;
  Result := False;
end;

function RegPropsClass(Regname: U_String; PropsClass: TPropsClass): Boolean;
var
  p: PRegProps;
begin
  InitPropsRegistedContainer;
  Result := ExistsRegPropsClass(Regname);
  if Result then
    begin
      raise CoreClassException.Create('Props templet already exists:' + Regname.Text);
      Exit;
    end;
  new(p);
  p^.Regname := Regname;
  p^.PropsClass := PropsClass;
  PropsRegistedContainer.Add(p);
end;

function GetRegPropsClass(Regname: U_String): TPropsClass;
var
  i: Integer;
  p: PRegProps;
begin
  InitPropsRegistedContainer;
  for i := 0 to PropsRegistedContainer.Count - 1 do
    begin
      p := PropsRegistedContainer[i];
      if umlMultipleMatch(True, Regname, p^.Regname) then
        begin
          Result := p^.PropsClass;
          Exit;
        end;
    end;
  Result := nil;
end;

function GetPropsClassRegName(PropsClass: TClass): U_String;
var
  i: Integer;
  p: PRegProps;
begin
  InitPropsRegistedContainer;
  for i := 0 to PropsRegistedContainer.Count - 1 do
    begin
      p := PropsRegistedContainer[i];
      if p^.PropsClass = PropsClass then
        begin
          Result := p^.Regname;
          Exit;
        end;
    end;
  Result.Text := '';
end;

constructor TProps.Create(AOwner: TCoreClassPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FWidth := 0;
  FHeight := 0;
  FName.Text := '';
  FDescription.Text := '';
  FPortraitExpression.Text := '';
  FWeight := 0;
  FOwner := AOwner;
  FRegionPosition := NULLPoint;
  FID := 0;
  FOwnerType := potUnknow;
  FOwnerRegionID := 0;
  FOwnerBagBioID := 0;
end;

destructor TProps.Destroy;
begin
  inherited Destroy;
end;

procedure TProps.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;

  df.WriteInteger(FWidth);
  df.WriteInteger(FHeight);
  df.WriteString(FName);
  df.WriteString(FDescription);
  df.WriteDouble(FWeight);
  df.WriteArraySingle.WriteArray(FRegionPosition);
  df.WriteInteger(FID);

  df.WriteInteger(Integer(FOwnerType));
  df.WriteInteger(FOwnerRegionID);
  df.WriteInteger(FOwnerBagBioID);

  df.EncodeTo(stream);
  DisposeObject(df);
end;

procedure TProps.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  FWidth := df.Reader.ReadInteger;
  FHeight := df.Reader.ReadInteger;
  FName := df.Reader.ReadString;
  FDescription := df.Reader.ReadString;
  FWeight := df.Reader.ReadDouble;
  with df.Reader.ReadArraySingle do
      FRegionPosition := PointMake(buffer[0], buffer[1]);
  FID := df.Reader.ReadInteger;

  FOwnerType := TPropsOwnerType(df.Reader.ReadInteger);
  FOwnerRegionID := df.Reader.ReadInteger;
  FOwnerBagBioID := df.Reader.ReadInteger;

  DisposeObject(df);
end;

initialization

InitPropsRegistedContainer;

finalization

FreePropsRegistedContainer;

end. 
 
