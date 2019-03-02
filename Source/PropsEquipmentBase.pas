{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit PropsEquipmentBase;

{$INCLUDE zDefine.inc}

interface

uses PropsBase, BioBase, CoreClasses, SpellBase, BioDataModule;

type
  TProps_Equipment = class;

  TAffixData = record
    dID: TDataID;
    sNumber: Variant;
  end;

  PAffixData = ^TAffixData;

  TRequirementLogicType = (rltGreaterOrEqual, rltGreater, rltLess, rltLessOrEqual, rltEqual);

  TRequirementData = record
    LogicType: TRequirementLogicType;
    dID: TDataID;
    Value: Variant;
  end;

  PRequirementData = ^TRequirementData;

  TProps_Equipment = class(TProps)
  private
    FAffixList: TCoreClassList;
    FRequirementList: TCoreClassList;

    FOwnerEquipmentSocket: TCoreClassPersistent;

    function GetRequirements(index: Integer): PRequirementData;
    function GetAffixs(index: Integer): PAffixData;
  public
    constructor Create(AOwner: TCoreClassPersistent); override;
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromStream(stream: TCoreClassStream); override;

    procedure ClearAffix;
    function AffixCount: Integer;
    property Affixs[index: Integer]: PAffixData read GetAffixs; default;
    procedure AddAffix(dID: TDataID; sNumber: Variant);
    function ExistsAffix(dID: TDataID): Boolean;

    procedure ClearRequirement;
    function RequirementCount: Integer;
    property Requirements[index: Integer]: PRequirementData read GetRequirements;
    procedure AddRequirement(t: TRequirementLogicType; dID: TDataID; Value: Variant);

    function CanInstall(Socket: TCoreClassPersistent): Boolean; virtual;
    procedure Install(Socket: TCoreClassPersistent); virtual;
    procedure UnInstall(Socket: TCoreClassPersistent); virtual;

    property OwnerEquipmentSocket: TCoreClassPersistent read FOwnerEquipmentSocket;
  end;

implementation

uses DataFrameEngine, MemoryStream64, CoreRULE;

function TProps_Equipment.GetAffixs(index: Integer): PAffixData;
begin
  Result := PAffixData(FAffixList[index]);
end;

function TProps_Equipment.GetRequirements(index: Integer): PRequirementData;
begin
  Result := PRequirementData(FRequirementList[index]);
end;

constructor TProps_Equipment.Create(AOwner: TCoreClassPersistent);
begin
  inherited Create(AOwner);
  FAffixList := TCoreClassList.Create;
  FRequirementList := TCoreClassList.Create;
  FOwnerEquipmentSocket := nil;
end;

destructor TProps_Equipment.Destroy;
begin
  ClearAffix;
  ClearRequirement;

  DisposeObject(FAffixList);
  DisposeObject(FRequirementList);
  inherited Destroy;
end;

procedure TProps_Equipment.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
  i: Integer;
  PA: PAffixData;
  pr: PRequirementData;
begin
  df := TDataFrameEngine.Create;

  ms := TMemoryStream64.Create;
  inherited SaveToStream(ms);
  ms.Position := 0;
  df.WriteStream(ms);
  DisposeObject(ms);

  //
  df.WriteInteger(AffixCount);
  for i := 0 to AffixCount - 1 do
    begin
      PA := Affixs[i];
      df.WriteInteger(Integer(PA^.dID));
      df.WriteVariant(PA^.sNumber);
    end;

  df.WriteInteger(RequirementCount);
  for i := 0 to RequirementCount - 1 do
    begin
      pr := Requirements[i];
      df.WriteInteger(Integer(pr^.LogicType));
      df.WriteInteger(Integer(pr^.dID));
      df.WriteVariant(pr^.Value);
    end;

  df.EncodeTo(stream);
  DisposeObject(df);
end;

procedure TProps_Equipment.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
  c, i: Integer;
  TA: TAffixData;
  tr: TRequirementData;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  ms := TMemoryStream64.Create;
  df.Reader.ReadStream(ms);
  ms.Position := 0;
  inherited LoadFromStream(stream);
  DisposeObject(ms);

  //
  ClearAffix;
  c := df.Reader.ReadInteger;
  for i := 0 to c - 1 do
    begin
      TA.dID := TDataID(df.Reader.ReadInteger);
      TA.sNumber := df.Reader.ReadVariant;
      AddAffix(TA.dID, TA.sNumber);
    end;

  ClearRequirement;
  c := df.Reader.ReadInteger;
  for i := 0 to c - 1 do
    begin
      tr.LogicType := TRequirementLogicType(df.Reader.ReadInteger);
      tr.dID := TDataID(df.Reader.ReadInteger);
      tr.Value := df.Reader.ReadVariant;
      AddRequirement(tr.LogicType, tr.dID, tr.Value);
    end;

  DisposeObject(df);
end;

procedure TProps_Equipment.ClearAffix;
var
  i: Integer;
  p: PAffixData;
begin
  for i := 0 to FAffixList.Count - 1 do
    begin
      p := PAffixData(FAffixList[i]);
      Dispose(p);
    end;
  FAffixList.Clear;
end;

function TProps_Equipment.AffixCount: Integer;
begin
  Result := FAffixList.Count;
end;

procedure TProps_Equipment.AddAffix(dID: TDataID; sNumber: Variant);
var
  p: PAffixData;
begin
  new(p);
  p^.dID := dID;
  p^.sNumber := sNumber;
  FAffixList.Add(p);
end;

function TProps_Equipment.ExistsAffix(dID: TDataID): Boolean;
var
  i: Integer;
  p: PAffixData;
begin
  Result := True;
  for i := 0 to FAffixList.Count - 1 do
    begin
      p := PAffixData(FAffixList[i]);
      if p^.dID = dID then
          Exit;
    end;
  Result := False;
end;

procedure TProps_Equipment.ClearRequirement;
var
  i: Integer;
  p: PRequirementData;
begin
  for i := 0 to FRequirementList.Count - 1 do
    begin
      p := PRequirementData(FRequirementList[i]);
      Dispose(p);
    end;
  FRequirementList.Clear;
end;

function TProps_Equipment.RequirementCount: Integer;
begin
  Result := FRequirementList.Count;
end;

procedure TProps_Equipment.AddRequirement(t: TRequirementLogicType; dID: TDataID; Value: Variant);
var
  p: PRequirementData;
begin
  new(p);
  p^.LogicType := t;
  p^.dID := dID;
  p^.Value := Value;
  FRequirementList.Add(p);
end;

function TProps_Equipment.CanInstall(Socket: TCoreClassPersistent): Boolean;
var
  b: TBio;
  esk: TBioEquipmentSocket;
  i: Integer;
  p: PRequirementData;
  d: BioDataModule.TDataItem;
  v: Variant;
  LV: Boolean;
begin
  Result := False;

  if not(Owner is TPropsBag) then
      Exit;
  if not(Socket is TBioEquipmentSocket) then
      Exit;

  esk := Socket as TBioEquipmentSocket;

  b := esk.Owner;

  LV := True;
  for i := 0 to FRequirementList.Count - 1 do
    begin
      p := PRequirementData(FRequirementList[i]);
      d := b.CoreData.GetDataOfID(p^.dID);
      v := d.FinalValue;
      case p^.LogicType of
        rltGreaterOrEqual: LV := LV and (v >= p^.Value);
        rltGreater: LV := LV and (v > p^.Value);
        rltLess: LV := LV and (v < p^.Value);
        rltLessOrEqual: LV := LV and (v <= p^.Value);
        rltEqual: LV := LV and (v >= p^.Value);
      end;
      if not LV then
          Break;
    end;

  Result := LV;
end;

procedure TProps_Equipment.Install(Socket: TCoreClassPersistent);
var
  b: TBio;
  esk: TBioEquipmentSocket;
  i: Integer;
  p: PAffixData;
begin
  if not(Owner is TPropsBag) then
      Exit;
  if not(Socket is TBioEquipmentSocket) then
      Exit;

  esk := Socket as TBioEquipmentSocket;

  b := esk.Owner;

  for i := 0 to FAffixList.Count - 1 do
    begin
      p := PAffixData(FAffixList[i]);
      b.CoreData.PostOverlapData(p^.dID, dipsEquipment, Self, p^.sNumber);
    end;

  FOwnerEquipmentSocket := esk;
end;

procedure TProps_Equipment.UnInstall(Socket: TCoreClassPersistent);
var
  b: TBio;
  esk: TBioEquipmentSocket;
begin
  if not(Owner is TPropsBag) then
      Exit;
  if not(Socket is TBioEquipmentSocket) then
      Exit;

  esk := Socket as TBioEquipmentSocket;

  b := esk.Owner;

  b.CoreData.DeletePostFlag(Self);

  FOwnerEquipmentSocket := nil;
end;

end. 
 
 
 
