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
unit CoreRULE;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, NumberBase, SpellBase, WorldBase, BaseRULEClasses,
  PropsBase, BioBase,

  Geometry2DUnit,

  PascalStrings, UnicodeMixedLib, ListEngine, CampRelationBase, zNavigationScene,
  zNavigationPass, zNavigationPoly, zNavigationPathFinding, MovementEngine,
  BattleCore, BioDataModule, PropsEquipmentBase, PropsMaterialUnit;

type
  TRULECore     = class;
  TPlayer       = class;
  TBio          = class;
  TRegion       = class;
  TBattleRegion = class;
  TMapRegion    = class;
  TLocation     = class;

  ILocationNotifyIntf = interface
    procedure BioIn(Sender: TLocation; b: TBio);
    procedure BioOut(Sender: TLocation; b: TBio);
  end;

  TLocation = class(TCoreClassPersistent)
  private
    FOwner: TBattleRegion;
    FBioList: TCoreClassListForObj;
    FPosition: TVec2;
    FRadius: TGeoFloat;
    FStepDelta: Double;
    FCurStepTimer: Double;
    FEnabled: Boolean;
    FName: U_String;
    FNotifyIntf: ILocationNotifyIntf;
    FNewBioPositionIdx: Integer;

    FUserListBoxItem: TCoreClassObject;

    procedure SetEnabled(const Value: Boolean);
  private
    procedure DoBioIn(b: TBio);
    procedure DoBioOut(b: TBio);

    procedure BioFreeNotifyProc(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
  public
    constructor Create(AOwner: TBattleRegion);
    destructor Destroy; override;

    procedure ProcessBio;
    procedure Progress(deltaTime: Double);
    procedure ProcessMovementBio(b: TBio);
    function ExistsBio(b: TBio): Boolean;
    procedure DeleteBio(b: TBio);

    procedure DoAllBioOut;

    function Count: Integer;
    function GetBio(index: Integer): TBio;
    property Bio[index: Integer]: TBio read GetBio; default;

    function NewBio(OwnerPlayer: TPlayer; BioRadius: TGeoFloat): TBio;

    property Owner: TBattleRegion read FOwner;
    property Position: TVec2 read FPosition write FPosition;
    property radius: TGeoFloat read FRadius write FRadius;
    property StepDelta: Double read FStepDelta write FStepDelta;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Name: U_String read FName write FName;

    property NotifyIntf: ILocationNotifyIntf read FNotifyIntf write FNotifyIntf;

    property UserListBoxItem: TCoreClassObject read FUserListBoxItem write FUserListBoxItem;
  end;

  TRegionBioCreateFormulaList = class;

  TRegionBioCreateFormula = class(TCoreClassPersistent)
  protected
    FOwner: TRegionBioCreateFormulaList;
  public
    constructor Create(AOwner: TRegionBioCreateFormulaList);
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); virtual;
  end;

  TRegionBioCreateFormulaList = class(TCoreClassPersistent)
  protected
    FOwner: TRegion;
  public
    constructor Create(AOwner: TRegion);
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream); virtual;
    procedure LoadFromStream(stream: TCoreClassStream); virtual;

    procedure Progress(deltaTime: Double);
  end;

  TRegionPath = array of Integer;

  TRegionConnectObject = class(TCoreClassPersistent)
  public
    Portrait: U_String;
    Position: TVec2;
    Region: TRegion;
  end;

  TRegion = class(TRegionBase)
  private
    FOwner: TRegion;
    FRULECore: TRULECore;
    FLevel: Integer;
    FID: Integer;

    FPortraitFile: U_String;

    FDescription: U_String;

    FName: U_String;
  protected
    FChildrenList: TCoreClassListForObj;
    FConnectList: TCoreClassListForObj;

    function GetChildren(index: Integer): TRegion;
    function GetConnect(index: Integer): TRegionConnectObject;
  protected
  public
    constructor Create(AOwner: TRegion; ACore: TRULECore); virtual;
    destructor Destroy; override;

    // children region
    function ChildrenCount: Integer;
    property Children[index: Integer]: TRegion read GetChildren;
    procedure ClearChildren;
    procedure AddChildren(r: TRegion);
    function GetChildrenFromID(AID: Integer; recursionSearch: Boolean): TRegion;
    function RootRegion: TRegion;
    function MakeRegionID: Integer;
    function RegionPath: TRegionPath;
    function GetRegionFromPath(const ph: TRegionPath): TRegion;

    // free region connect
    function ConnectCount: Integer;
    property Connect[index: Integer]: TRegionConnectObject read GetConnect; default;
    procedure ClearConnect;
    procedure AddConnect(connObj: TRegionConnectObject);

    // dont process children and connect
    procedure SaveToStream(stream: TCoreClassStream); virtual;
    procedure LoadFromStream(stream: TCoreClassStream); virtual;

    procedure Progress(deltaTime: Double); virtual;

    function FocusCanInRegion: Boolean; virtual;
    procedure FocusInRegion; virtual;
    function FocusCanOutRegion: Boolean; virtual;
    procedure FocusOutRegion; virtual;
  public
    property PortraitFile: U_String read FPortraitFile write FPortraitFile;

    property Description: U_String read FDescription write FDescription;
    property Name: U_String read FName write FName;
    property ID: Integer read FID;
    property level: Integer read FLevel;

    property Owner: TRegion read FOwner;
    property RULECore: TRULECore read FRULECore;
  end;

  TRegionClass = class of TRegion;

  TRegistedRegion = record
    RegionClass: TRegionClass;
    Name: U_String;
    Description: U_String;
  end;

  PRegistedRegion = ^TRegistedRegion;

  TBattleRegion = class(TRegion)
  protected
    FNavScene: TNavigationScene;

    FBattleBackground: U_String;
    FViewOffset: TVec2;
    FViewScale: TVec2;

    FBioList: TCoreClassListForObj;
    FLocationList: TCoreClassListForObj;
    FPropsList: TCoreClassListForObj;
    FBattleInterface: TBattleCore;

    function GetBios(index: Integer): TBio;
    function GetLocation(index: Integer): TLocation;
    function GetProps(index: Integer): TProps;

    procedure BioFreeNotifyProc(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
  public
    constructor Create(AOwner: TRegion; ACore: TRULECore); override;
    destructor Destroy; override;

    // bio
    function BioCount: Integer;
    property Bios[index: Integer]: TBio read GetBios;
    procedure ClearBio;
    function ExistsBio(b: TBio): Boolean;
    procedure AddBio(b: TBio);
    procedure RemoveBio(b: TBio);
    function GetBioFromID(AID: Integer): TBio;
    function GetRadiusBioList(Position: TVec2; radius: TGeoFloat; output: TCoreClassListForObj): Integer;

    // Location
    function LocationCount: Integer;
    property Location[index: Integer]: TLocation read GetLocation;
    procedure ClearLocation;
    function ExistsLocation(L: TLocation): Boolean;
    procedure AddLocation(L: TLocation);
    procedure RemoveLocation(L: TLocation);

    // Props
    function PropsCount: Integer;
    property Props[index: Integer]: TProps read GetProps;
    procedure ClearProps;
    function ExistsProps(L: TProps): Boolean;
    procedure AddProps(L: TProps);
    procedure RemoveProps(L: TProps);

    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromStream(stream: TCoreClassStream); override;

    procedure Progress(deltaTime: Double); override;

    function BioCanInRegion(b: TBio): Boolean; virtual;
    procedure BioInRegion(b: TBio); virtual;

    function BioCanOutRegion(b: TBio): Boolean; virtual;
    procedure BioOutRegion(b: TBio); virtual;
  public
    property NavScene: TNavigationScene read FNavScene;
    property BattleBackground: U_String read FBattleBackground write FBattleBackground;
    property ViewOffset: TVec2 read FViewOffset write FViewOffset;
    property ViewScale: TVec2 read FViewScale write FViewScale;

    property BattleInterface: TBattleCore read FBattleInterface write FBattleInterface;
  end;

  TMapRegion = class(TRegion)
  protected
    FMapBackground: U_String;
    FViewOffset: TVec2;
    FViewScale: TVec2;
  public
    constructor Create(AOwner: TRegion; ACore: TRULECore); override;
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromStream(stream: TCoreClassStream); override;
  public
    property MapBackground: U_String read FMapBackground write FMapBackground;
    property ViewOffset: TVec2 read FViewOffset write FViewOffset;
    property ViewScale: TVec2 read FViewScale write FViewScale;
  end;

  TRootRegion = class(TMapRegion)
  public
    constructor Create(AOwner: TRegion; ACore: TRULECore); override;
    destructor Destroy; override;

    procedure SaveAllToStream(stream: TCoreClassStream);
    class function LoadAllFromStream(stream: TCoreClassStream; ACore: TRULECore): TRootRegion;
  end;

  TPropsBag = class(TPropsBagBase)
  protected
    FItems: TCoreClassListForObj;
    FOwner: TBio;
  public
    constructor Create(AOwner: TBio);
    destructor Destroy; override;

    property Owner: TBio read FOwner write FOwner;
    procedure Clear;

    procedure Add(AProps: TProps);
    procedure Delete(AProps: TProps);

    property Items: TCoreClassListForObj read FItems;
  end;

  TBioEquipmentSocket = class(TCoreClassPersistent)
  protected
    FEquipment: TProps_Equipment;
    FOwner: TBio;

    procedure SetEquipment(const Value: TProps_Equipment);
  public
    constructor Create(AOwner: TBio);
    destructor Destroy; override;

    property Equipment: TProps_Equipment read FEquipment write SetEquipment;
    property Owner: TBio read FOwner;
  end;

  TBio = class(TBioBase)
  protected
    FOwner: TPlayer;
    FPropsBag: TPropsBag;
    FCoreData: TBioBaseData;
    FSpellContainer: TCoreClassListForObj;

    FPrimaryWeapon: TBioEquipmentSocket;
    FSecondaryWeapon: TBioEquipmentSocket;
    FArrmor: TBioEquipmentSocket;
    FBelt: TBioEquipmentSocket;
    FLeftRing: TBioEquipmentSocket;
    FRightRing: TBioEquipmentSocket;
    FGloves: TBioEquipmentSocket;
    FHelm: TBioEquipmentSocket;
    FAmulet: TBioEquipmentSocket;
    FBoots: TBioEquipmentSocket;
    FCloak: TBioEquipmentSocket;
    FBolts: TBioEquipmentSocket;
    FArrow: TBioEquipmentSocket;
  public
    constructor Create(AOwner: TPlayer; AOwnerRegion: TRegion; APosition: TVec2; AAngle, ARadius: TGeoFloat);
    destructor Destroy; override;

    procedure FreeSpells;

    property Owner: TPlayer read FOwner;
    property PropsBag: TPropsBag read FPropsBag;
    property CoreData: TBioBaseData read FCoreData;
    property SpellContainer: TCoreClassListForObj read FSpellContainer;

    property PrimaryWeapon: TBioEquipmentSocket read FPrimaryWeapon;
    property SecondaryWeapon: TBioEquipmentSocket read FSecondaryWeapon;
    property Arrmor: TBioEquipmentSocket read FArrmor;
    property Belt: TBioEquipmentSocket read FBelt;
    property LeftRing: TBioEquipmentSocket read FLeftRing;
    property RightRing: TBioEquipmentSocket read FRightRing;
    property Gloves: TBioEquipmentSocket read FGloves;
    property Helm: TBioEquipmentSocket read FHelm;
    property Amulet: TBioEquipmentSocket read FAmulet;
    property Boots: TBioEquipmentSocket read FBoots;
    property Cloak: TBioEquipmentSocket read FCloak;
    property Bolts: TBioEquipmentSocket read FBolts;
    property Arrow: TBioEquipmentSocket read FArrow;
  end;

  TPlayer = class(TCoreClassPersistent)
  private
    FOwner: TRULECore;
    FPlayerID: U_String;
    FCampID: U_String;
    FBioList: TCoreClassListForObj;

    function GetBio(index: Integer): TBio;
  public
    constructor Create(AOwner: TRULECore);
    destructor Destroy; override;

    procedure Delete;

    procedure Progress(deltaTime: Double);

    function NewBio(Region: TRegion; Position: TVec2; RollAngle, radius: TGeoFloat): TBio;
    function BioCount: Integer;
    function DeleteBio(b: TBio): Boolean;
    function ExistsBio(b: TBio): Boolean;
    procedure Clear;

    property Bios[index: Integer]: TBio read GetBio; default;

    property Owner: TRULECore read FOwner;
    property PlayerID: U_String read FPlayerID;
    property CampID: U_String read FCampID;
  end;

  TRULECore = class(TCoreClassPersistent)
  private
    // relation engine
    FRelation: TCampRelation;
    // player object list
    FPlayerHashList: THashObjectList;
    // player list;
    FPlayerList: TCoreClassListForObj;
    // camp list
    FCampList: TCoreClassStringList;
    // region root
    FRootRegion: TRootRegion;

    FLastMakeBioID: Integer;

    function GetPlayerOfID(Name: U_String): TPlayer;
    function GetPlayers(index: Integer): TPlayer;
  private
    procedure DoNewBioNotify(Bio: TBio);
    procedure DoPlayerUpdateNotify;
  private
    procedure ClearAllBio;
    procedure ClearAllPlayer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double);

    procedure InitBaseRelation;

    function NewPlayer(PlayerID, CampID: U_String): TPlayer;
    procedure DeletePlayer(PlayerID: U_String);
    function PlayerCount: Integer;
    procedure ClearPlayer;
    function ExistsPlayer(PlayerID: U_String): Boolean;

    property Players[index: Integer]: TPlayer read GetPlayers; default;
    property PlayerOfID[Name: U_String]: TPlayer read GetPlayerOfID;

    function GetBioOfID(bioID: Integer): TBio;
    function MakeBioID: Integer;

    procedure AddCamp(CampID: U_String; AroundRelation: TRelationType = rtAntagonize);

    function BioRelation(b1, b2: TBio): TRelationType;
    function PlayerRelation(p1, p2: TPlayer): TRelationType;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);

    property Relation: TCampRelation read FRelation;
    property RootRegion: TRootRegion read FRootRegion;
  end;

var
  RegistedRegionList: TCoreClassList = nil;

function GetRegistedRegionName(Region: TRegion): U_String;
function RegistedRegionExists(Name: U_String): Boolean;
function GetRegistedRegionClass(Regname: U_String): TRegionClass;
procedure RegistedRegion(RegionClass: TRegionClass; Name, Description: U_String);

implementation

uses DataFrameEngine, MemoryStream64, DoStatusIO;

procedure TLocation.SetEnabled(const Value: Boolean);
var
  i: Integer;
begin
  if FEnabled and (not Value) then
      DoAllBioOut;
  FEnabled := Value;
end;

procedure TLocation.DoBioIn(b: TBio);
begin
  if FNotifyIntf <> nil then
      FNotifyIntf.BioIn(Self, b);
end;

procedure TLocation.DoBioOut(b: TBio);
begin
  if FNotifyIntf <> nil then
      FNotifyIntf.BioOut(Self, b);
end;

procedure TLocation.BioFreeNotifyProc(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
var
  i: Integer;
begin
  i := 0;
  while i < FBioList.Count do
    begin
      if FBioList[i] = TriggerObject then
        begin
          FBioList.Delete(i);
        end
      else
          inc(i);
    end;
end;

constructor TLocation.Create(AOwner: TBattleRegion);
begin
  inherited Create;
  FOwner := AOwner;
  FBioList := TCoreClassListForObj.Create;
  FPosition := NULLPoint;
  FRadius := 100;
  FStepDelta := 2.0;
  FCurStepTimer := 0;
  FEnabled := True;
  FNotifyIntf := nil;
  FNewBioPositionIdx := 0;
  FUserListBoxItem := nil;
end;

destructor TLocation.Destroy;
var
  i: Integer;
begin
  for i := 0 to FBioList.Count - 1 do
    begin
      TBio(FBioList[i]).UnRegisterFreeBackcall(Self);
      DoBioOut(TBio(FBioList[i]));
    end;
  DisposeObject(FBioList);
  inherited Destroy;
end;

procedure TLocation.ProcessBio;

  procedure ProcessBioIn;
  var
    i: Integer;
    b: TBio;
  begin
    // imple in/out event trigger
    i := 0;
    while i < Owner.BioCount do
      begin
        b := Owner.Bios[i];
        if (not b.IsDie) and (CircleCollision(FPosition, b.Position, FRadius, b.radius)) then
          begin
            if not ExistsBio(b) then
              begin
                FBioList.Add(b);
{$IFDEF FPC}
                b.RegisterFreeBackcall(Self, @BioFreeNotifyProc);
{$ELSE}
                b.RegisterFreeBackcall(Self, BioFreeNotifyProc);
{$ENDIF}
                // execute in event
                DoBioIn(b);
              end;
          end;
        inc(i);
      end;
  end;

  procedure ProcessBioOut;
  var
    i: Integer;
    b: TBio;
  begin
    i := 0;
    while i < FBioList.Count do
      begin
        b := TBio(FBioList[i]);
        if (b.IsDie) or (not CircleCollision(FPosition, b.Position, FRadius, b.radius)) then
          begin
            FBioList.Delete(i);
            // execute out event
            b.UnRegisterFreeBackcall(Self);
            DoBioOut(b);
          end
        else
            inc(i);
      end;
  end;

begin
  ProcessBioOut;
  ProcessBioIn;
end;

procedure TLocation.Progress(deltaTime: Double);
var
  b: TBio;
begin
  if not FEnabled then
    begin
      DoAllBioOut;
      Exit;
    end;

  FCurStepTimer := FCurStepTimer + deltaTime;
  while FCurStepTimer > FStepDelta do
    begin
      FCurStepTimer := 0;
      ProcessBio;
    end;
end;

procedure TLocation.ProcessMovementBio(b: TBio);
begin
  if (FEnabled) and (not b.IsDie) and (CircleCollision(FPosition, b.Position, FRadius, b.radius)) then
    begin
      if not ExistsBio(b) then
        begin
          // execute in event
          FBioList.Add(b);
          DoBioIn(b);
{$IFDEF FPC}
          b.RegisterFreeBackcall(Self, @BioFreeNotifyProc);
{$ELSE}
          b.RegisterFreeBackcall(Self, BioFreeNotifyProc);
{$ENDIF}
        end;
    end
  else
    begin
      if ExistsBio(b) then
        begin
          // execute out event
          DoBioOut(b);
          DeleteBio(b);
          b.UnRegisterFreeBackcall(Self);
        end;
    end;
end;

function TLocation.ExistsBio(b: TBio): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FBioList.Count - 1 do
    if FBioList[i] = b then
        Exit;

  Result := False;
end;

procedure TLocation.DeleteBio(b: TBio);
var
  i: Integer;
begin
  i := 0;
  while i < FBioList.Count do
    begin
      if FBioList[i] = b then
          FBioList.Delete(i)
      else
          inc(i);
    end;
end;

function TLocation.Count: Integer;
begin
  Result := FBioList.Count;
end;

procedure TLocation.DoAllBioOut;
var
  b: TBio;
begin
  while FBioList.Count > 0 do
    begin
      b := TBio(FBioList[0]);
      FBioList.Delete(0);
      DoBioOut(b);
    end;
end;

function TLocation.GetBio(index: Integer): TBio;
begin
  Result := TBio(FBioList[index]);
end;

function TLocation.NewBio(OwnerPlayer: TPlayer; BioRadius: TGeoFloat): TBio;
const
  CirclePointCount = 100.0;
var
  pt: TVec2;
  a: TGeoFloat;
begin
  a := 360.0 / CirclePointCount * FNewBioPositionIdx;
  pt := PointRotation(FPosition, 1, a);
  inc(FNewBioPositionIdx);
  if FNewBioPositionIdx >= CirclePointCount then
      FNewBioPositionIdx := 0;
  Result := OwnerPlayer.NewBio(Owner, pt, a, BioRadius);
end;

constructor TRegionBioCreateFormula.Create(AOwner: TRegionBioCreateFormulaList);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TRegionBioCreateFormula.Destroy;
begin
  inherited Destroy;
end;

procedure TRegionBioCreateFormula.Progress(deltaTime: Double);
begin
end;

constructor TRegionBioCreateFormulaList.Create(AOwner: TRegion);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TRegionBioCreateFormulaList.Destroy;
begin
  inherited Destroy;
end;

procedure TRegionBioCreateFormulaList.SaveToStream(stream: TCoreClassStream);
begin

end;

procedure TRegionBioCreateFormulaList.LoadFromStream(stream: TCoreClassStream);
begin

end;

procedure TRegionBioCreateFormulaList.Progress(deltaTime: Double);
begin

end;

function TRegion.GetChildren(index: Integer): TRegion;
begin
  Result := FChildrenList[index] as TRegion;
end;

function TRegion.GetConnect(index: Integer): TRegionConnectObject;
begin
  Result := FConnectList[index] as TRegionConnectObject;
end;

constructor TRegion.Create(AOwner: TRegion; ACore: TRULECore);
begin
  inherited Create;
  FOwner := AOwner;
  FRULECore := ACore;

  if AOwner <> nil then
    begin
      FID := AOwner.MakeRegionID;
      FLevel := AOwner.FLevel + 1;
    end
  else
    begin
      FID := 0;
      FLevel := 0;
    end;

  FChildrenList := TCoreClassListForObj.Create;
  FConnectList := TCoreClassListForObj.Create;

  FPortraitFile.Text := '';
  FDescription.Text := '';
  FName.Text := '';
end;

destructor TRegion.Destroy;
begin
  ClearChildren;
  ClearConnect;

  DisposeObject(FChildrenList);
  DisposeObject(FConnectList);
  inherited;
end;

function TRegion.ChildrenCount: Integer;
begin
  Result := FChildrenList.Count;
end;

procedure TRegion.ClearChildren;
begin
  FChildrenList.Clear;
end;

procedure TRegion.AddChildren(r: TRegion);
begin
  FChildrenList.Add(r);
end;

function TRegion.GetChildrenFromID(AID: Integer; recursionSearch: Boolean): TRegion;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ChildrenCount - 1 do
    if Children[i].FID = AID then
      begin
        Result := Children[i];
        Exit;
      end;
  if recursionSearch then
    begin
      for i := 0 to ChildrenCount - 1 do
        begin
          Result := Children[i].GetChildrenFromID(AID, recursionSearch);
          if Result <> nil then
              Exit;
        end;
    end
end;

function TRegion.RootRegion: TRegion;
begin
  Result := Self;
  while Result.Owner <> nil do
      Result := Result.Owner;
end;

function TRegion.MakeRegionID: Integer;
var
  r: TRegion;
  n: TRegion;
begin
  r := RootRegion;
  n := r;
  Result := r.ID;

  while n <> nil do
    begin
      inc(Result);
      n := r.GetChildrenFromID(Result, True);
    end;
end;

function TRegion.RegionPath: TRegionPath;
var
  i: Integer;
  r: TRegion;
begin
  SetLength(Result, FLevel + 1);
  i := FLevel;
  r := Self;
  while r.Owner <> nil do
    begin
      Result[i] := r.ID;
      dec(i);
      r := r.Owner;
    end;
  Result[i] := r.ID;
end;

function TRegion.GetRegionFromPath(const ph: TRegionPath): TRegion;
var
  i, L: Integer;
begin
  i := low(ph);
  L := high(ph);
  Result := RootRegion;
  while (Result <> nil) and (i < L) do
    begin
      if Result.ID <> ph[i] then
          RaiseInfo('Region path failed!');
      inc(i);
      Result := Result.GetChildrenFromID(ph[i], False);
    end;
end;

function TRegion.ConnectCount: Integer;
begin
  Result := FConnectList.Count;
end;

procedure TRegion.ClearConnect;
var
  i: Integer;
begin
  for i := 0 to FConnectList.Count - 1 do
      DisposeObject(FConnectList[i]);
  FConnectList.Clear;
end;

procedure TRegion.AddConnect(connObj: TRegionConnectObject);
begin
  if connObj.Region = nil then
      RaiseInfo('connect region are nil');
  FConnectList.Add(connObj);
end;

procedure TRegion.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.WriteInteger(FID);

  df.WriteString(FPortraitFile);
  df.WriteString(FDescription);
  df.WriteString(FName);

  df.SaveToStream(stream);
  DisposeObject(df);
end;

procedure TRegion.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  FID := df.Reader.ReadInteger;

  FPortraitFile := df.Reader.ReadString;
  FDescription := df.Reader.ReadString;
  FName := df.Reader.ReadString;

  DisposeObject(df);
end;

procedure TRegion.Progress(deltaTime: Double);
var
  i: Integer;
begin
  for i := 0 to ChildrenCount - 1 do
      Children[i].Progress(deltaTime);
end;

function TRegion.FocusCanInRegion: Boolean;
begin
  Result := True;
end;

procedure TRegion.FocusInRegion;
begin
end;

function TRegion.FocusCanOutRegion: Boolean;
begin
  Result := True;
end;

procedure TRegion.FocusOutRegion;
begin
end;

function TBattleRegion.GetBios(index: Integer): TBio;
begin
  Result := FBioList[index] as TBio;
end;

function TBattleRegion.GetLocation(index: Integer): TLocation;
begin
  Result := FLocationList[index] as TLocation;
end;

function TBattleRegion.GetProps(index: Integer): TProps;
begin
  Result := FPropsList[index] as TProps;
end;

procedure TBattleRegion.BioFreeNotifyProc(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
var
  i: Integer;
begin
  i := 0;
  while i < FBioList.Count do
    begin
      if FBioList[i] = TriggerObject then
        begin
          FBioList.Delete(i);
        end
      else
          inc(i);
    end;
end;

constructor TBattleRegion.Create(AOwner: TRegion; ACore: TRULECore);
begin
  inherited Create(AOwner, ACore);

  FBioList := TCoreClassListForObj.Create;
  FLocationList := TCoreClassListForObj.Create;
  FPropsList := TCoreClassListForObj.Create;

  FNavScene := TNavigationScene.Create;

  FBattleBackground := '';
  FViewScale := PointMake(1, 1);
  FViewOffset := ZeroPoint;

  FBattleInterface := nil;
end;

destructor TBattleRegion.Destroy;
begin
  DisposeObject(FNavScene);

  ClearBio;
  ClearLocation;
  ClearProps;

  DisposeObject(FBioList);
  DisposeObject(FLocationList);
  DisposeObject(FPropsList);
  inherited Destroy;
end;

function TBattleRegion.BioCount: Integer;
begin
  Result := FBioList.Count;
end;

procedure TBattleRegion.ClearBio;
begin
  while BioCount > 0 do
      RemoveBio(Bios[0]);
end;

function TBattleRegion.ExistsBio(b: TBio): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FBioList.Count - 1 do
    if FBioList[i] = b then
        Exit;
  Result := False;
end;

procedure TBattleRegion.AddBio(b: TBio);
begin
  if not ExistsBio(b) then
    begin
      FBioList.Add(b);
{$IFDEF FPC}
      b.RegisterFreeBackcall(Self, @BioFreeNotifyProc);
{$ELSE}
      b.RegisterFreeBackcall(Self, BioFreeNotifyProc);
{$ENDIF}
    end;
end;

procedure TBattleRegion.RemoveBio(b: TBio);
var
  i: Integer;
begin
  i := 0;
  while i < FBioList.Count do
    begin
      if FBioList[i] = b then
        begin
          FBioList.Delete(i);
          b.UnRegisterFreeBackcall(Self);
        end
      else
          inc(i);
    end;

  for i := 0 to LocationCount - 1 do
      Location[i].DeleteBio(b);
end;

function TBattleRegion.GetBioFromID(AID: Integer): TBio;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to BioCount - 1 do
    if Bios[i].ID = AID then
      begin
        Result := Bios[i];
        Break;
      end;
end;

function TBattleRegion.GetRadiusBioList(Position: TVec2; radius: TGeoFloat; output: TCoreClassListForObj): Integer;
var
  i: Integer;
  b: TBio;
begin
  for i := 0 to BioCount - 1 do
    begin
      b := Bios[i];
      if (not b.IsDie) and (CircleCollision(Position, b.Position, radius, b.radius)) then
          output.Add(b);
    end;
  Result := output.Count;
end;

function TBattleRegion.LocationCount: Integer;
begin
  Result := FLocationList.Count;
end;

procedure TBattleRegion.ClearLocation;
begin
  FLocationList.Clear;
end;

function TBattleRegion.ExistsLocation(L: TLocation): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to LocationCount - 1 do
    if Location[i] = L then
        Exit;
  Result := False;
end;

procedure TBattleRegion.AddLocation(L: TLocation);
begin
  if not ExistsLocation(L) then
      FLocationList.Add(L);
end;

procedure TBattleRegion.RemoveLocation(L: TLocation);
var
  i: Integer;
begin
  i := 0;
  while i < FLocationList.Count do
    begin
      if FLocationList[i] = L then
        begin
          FLocationList.Delete(i);
        end
      else
          inc(i);
    end;
end;

function TBattleRegion.PropsCount: Integer;
begin
  Result := FPropsList.Count;
end;

procedure TBattleRegion.ClearProps;
begin
  FPropsList.Clear;
end;

function TBattleRegion.ExistsProps(L: TProps): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FPropsList.Count - 1 do
    if FPropsList[i] = L then
        Exit;
  Result := False;
end;

procedure TBattleRegion.AddProps(L: TProps);
begin
  FPropsList.Add(L);
end;

procedure TBattleRegion.RemoveProps(L: TProps);
var
  i: Integer;
begin
  i := 0;
  while i < FPropsList.Count do
    begin
      if FPropsList[i] = L then
        begin
          FPropsList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TBattleRegion.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;

  ms := TMemoryStream64.Create;
  inherited SaveToStream(ms);
  ms.Position := 0;
  df.WriteStream(ms);
  DisposeObject(ms);

  ms := TMemoryStream64.Create;
  FNavScene.SaveToStream(ms);
  ms.Position := 0;
  df.WriteStream(ms);
  DisposeObject(ms);

  df.WriteString(FBattleBackground);
  df.Write2DPoint(FViewScale);
  df.Write2DPoint(FViewOffset);

  df.SaveToStream(stream);
  DisposeObject(df);
end;

procedure TBattleRegion.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  ms := TMemoryStream64.Create;
  df.Reader.ReadStream(ms);
  ms.Position := 0;
  inherited LoadFromStream(ms);
  DisposeObject(ms);

  ms := TMemoryStream64.Create;
  df.Reader.ReadStream(ms);
  FNavScene.Reset;
  FNavScene.LoadFromStream(ms);
  DisposeObject(ms);

  FBattleBackground := df.Reader.ReadString;
  FViewScale := df.Reader.Read2DPoint;
  FViewOffset := df.Reader.Read2DPoint;

  DisposeObject(df);
end;

procedure TBattleRegion.Progress(deltaTime: Double);
var
  i: Integer;
begin
  inherited Progress(deltaTime);

  FNavScene.Progress(deltaTime);

  for i := 0 to LocationCount - 1 do
      Location[i].Progress(deltaTime);
end;

function TBattleRegion.BioCanInRegion(b: TBio): Boolean;
begin
  Result := not ExistsBio(b);
end;

procedure TBattleRegion.BioInRegion(b: TBio);
begin
  AddBio(b);
end;

function TBattleRegion.BioCanOutRegion(b: TBio): Boolean;
begin
  Result := ExistsBio(b);
end;

procedure TBattleRegion.BioOutRegion(b: TBio);
begin
  RemoveBio(b);
end;

constructor TMapRegion.Create(AOwner: TRegion; ACore: TRULECore);
begin
  inherited Create(AOwner, ACore);
  FMapBackground := '';

  FViewOffset := NULLPoint;
  FViewScale := MakePoint(1, 1);
end;

destructor TMapRegion.Destroy;
begin
  inherited Destroy;
end;

procedure TMapRegion.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;

  ms := TMemoryStream64.Create;
  inherited SaveToStream(ms);
  ms.Position := 0;
  df.WriteStream(ms);
  DisposeObject(ms);

  df.WriteString(FMapBackground);
  df.Write2DPoint(FViewOffset);
  df.Write2DPoint(FViewScale);

  df.SaveToStream(stream);
  DisposeObject(df);
end;

procedure TMapRegion.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  ms := TMemoryStream64.Create;
  df.Reader.ReadStream(ms);
  ms.Position := 0;
  inherited LoadFromStream(ms);
  DisposeObject(ms);

  FMapBackground := df.Reader.ReadString;
  FViewOffset := df.Reader.Read2DPoint;
  FViewScale := df.Reader.Read2DPoint;

  DisposeObject(df);
end;

constructor TRootRegion.Create(AOwner: TRegion; ACore: TRULECore);
begin
  inherited Create(AOwner, ACore);
end;

destructor TRootRegion.Destroy;
begin
  inherited Destroy;
end;

procedure TRootRegion.SaveAllToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;

  procedure WriteRegion(const r: TRegion);
  var
    i: Integer;
    ms: TMemoryStream64;
  begin
    // region header
    df.WriteString(r.ClassName);
    df.WriteString(GetRegistedRegionName(r));

    // region info
    ms := TMemoryStream64.Create;
    r.SaveToStream(ms);
    df.WriteStream(ms);
    DisposeObject(ms);

    // children
    df.WriteInteger(r.ChildrenCount);
    for i := 0 to r.ChildrenCount - 1 do
        WriteRegion(r.Children[i]);
  end;

  procedure WriteRegionConn(const r: TRegion);
  var
    i: Integer;
    ms: TMemoryStream64;
    connObj: TRegionConnectObject;
  begin
    // current region path
    df.WriteArrayInteger.WriteArray(r.RegionPath);

    // connect region
    df.WriteInteger(r.ConnectCount);
    for i := 0 to r.ConnectCount - 1 do
      begin
        connObj := r.Connect[i];
        df.WriteString(connObj.Portrait);
        df.Write2DPoint(connObj.Position);
        df.WriteArrayInteger.WriteArray(connObj.Region.RegionPath);
      end;

    // children
    df.WriteInteger(r.ChildrenCount);
    for i := 0 to r.ChildrenCount - 1 do
        WriteRegionConn(r.Children[i]);
  end;

begin
  df := TDataFrameEngine.Create;

  WriteRegion(Self);
  WriteRegionConn(Self);

  df.SaveToStream(stream);
  DisposeObject(df);
end;

class function TRootRegion.LoadAllFromStream(stream: TCoreClassStream; ACore: TRULECore): TRootRegion;
var
  df: TDataFrameEngine;

  function ReadRegion(CurrentParent: TRegion): TRegion;
  var
    i: Integer;
    cName, Regname: U_String;
    c: TRegionClass;

    ms: TMemoryStream64;
    childCnt: Integer;
  begin
    // region header
    cName := df.Reader.ReadString;
    Regname := df.Reader.ReadString;

    c := GetRegistedRegionClass(Regname);
    if c = nil then
        RaiseInfo('failed load region class "%s" regname:"%s"', [cName.Text, Regname.Text]);

    Result := c.Create(CurrentParent, ACore);
    if CurrentParent <> nil then
        CurrentParent.AddChildren(Result);

    // region info
    ms := TMemoryStream64.Create;
    df.Reader.ReadStream(ms);
    ms.Position := 0;
    Result.LoadFromStream(ms);
    DisposeObject(ms);

    // children
    childCnt := df.Reader.ReadInteger;
    for i := 0 to childCnt - 1 do
        ReadRegion(Result);
  end;

  procedure ReadRegionConnect(r: TRegion);
  var
    i, j: Integer;
    ph: TRegionPath;

    Current: TRegion;
    connCnt, childCnt: Integer;

    connObj: TRegionConnectObject;
  begin
    // current region path
    with df.Reader.ReadArrayInteger do
      begin
        SetLength(ph, Count);
        for i := 0 to Count - 1 do
            ph[i] := buffer[i];
      end;
    Current := r.GetRegionFromPath(ph);

    // connect region
    connCnt := df.Reader.ReadInteger;
    for i := 0 to connCnt - 1 do
      begin
        connObj := TRegionConnectObject.Create;

        connObj.Portrait := df.Reader.ReadString;
        connObj.Position := df.Reader.Read2DPoint;
        with df.Reader.ReadArrayInteger do
          begin
            SetLength(ph, Count);
            for j := 0 to Count - 1 do
                ph[j] := buffer[j];
          end;
        connObj.Region := Current.GetRegionFromPath(ph);
        Current.AddConnect(connObj);
      end;

    // children
    childCnt := df.Reader.ReadInteger;
    for i := 0 to childCnt - 1 do
        ReadRegionConnect(Current);
  end;

begin
  df := TDataFrameEngine.Create;
  df.LoadFromStream(stream);

  Result := ReadRegion(nil) as TRootRegion;
  ReadRegionConnect(Result);

  DisposeObject(df);
end;

constructor TPropsBag.Create(AOwner: TBio);
begin
  inherited Create;
  FOwner := AOwner;
  FItems := TCoreClassListForObj.Create;
end;

destructor TPropsBag.Destroy;
begin
  Clear;
  DisposeObject(FItems);
  inherited;
end;

procedure TPropsBag.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
      DisposeObject(FItems[i]);
  FItems.Clear;
end;

procedure TPropsBag.Add(AProps: TProps);
begin
  FItems.Add(AProps);
end;

procedure TPropsBag.Delete(AProps: TProps);
var
  i: Integer;
begin
  i := 0;
  while i < FItems.Count do
    begin
      if FItems[i] = AProps then
          FItems.Delete(i)
      else
          inc(i);
    end;
end;

procedure TBioEquipmentSocket.SetEquipment(const Value: TProps_Equipment);
begin
  if (Value <> nil) and (not Value.CanInstall(Self)) then
      Exit;

  Owner.CoreData.BeginUpdate;

  if FEquipment <> nil then
    begin
      FEquipment.UnInstall(Self);
      FEquipment := nil;
    end;

  FEquipment := Value;

  if FEquipment <> nil then
    begin
      FEquipment.Install(Self);
    end;

  Owner.CoreData.EndUpdate;
end;

constructor TBioEquipmentSocket.Create(AOwner: TBio);
begin
  inherited Create;
  FOwner := AOwner;
  FEquipment := nil;
end;

destructor TBioEquipmentSocket.Destroy;
begin
  inherited Destroy;
end;

constructor TBio.Create(AOwner: TPlayer; AOwnerRegion: TRegion; APosition: TVec2; AAngle, ARadius: TGeoFloat);
begin
  inherited Create(AOwnerRegion, APosition, AAngle, ARadius);
  FOwner := AOwner;
  FID := AOwner.FOwner.MakeBioID;
  FPropsBag := TPropsBag.Create(Self);
  FCoreData := TBioBaseData.Create(Self);
  FSpellContainer := TCoreClassListForObj.Create;

  FPrimaryWeapon := TBioEquipmentSocket.Create(Self);
  FSecondaryWeapon := TBioEquipmentSocket.Create(Self);
  FArrmor := TBioEquipmentSocket.Create(Self);
  FBelt := TBioEquipmentSocket.Create(Self);
  FLeftRing := TBioEquipmentSocket.Create(Self);
  FRightRing := TBioEquipmentSocket.Create(Self);
  FGloves := TBioEquipmentSocket.Create(Self);
  FHelm := TBioEquipmentSocket.Create(Self);
  FAmulet := TBioEquipmentSocket.Create(Self);
  FBoots := TBioEquipmentSocket.Create(Self);
  FCloak := TBioEquipmentSocket.Create(Self);
  FBolts := TBioEquipmentSocket.Create(Self);
  FArrow := TBioEquipmentSocket.Create(Self);
end;

destructor TBio.Destroy;
begin
  DisposeObject(FPrimaryWeapon);
  DisposeObject(FSecondaryWeapon);
  DisposeObject(FArrmor);
  DisposeObject(FBelt);
  DisposeObject(FLeftRing);
  DisposeObject(FRightRing);
  DisposeObject(FGloves);
  DisposeObject(FHelm);
  DisposeObject(FAmulet);
  DisposeObject(FBoots);
  DisposeObject(FCloak);
  DisposeObject(FBolts);
  DisposeObject(FArrow);

  FreeSpells;
  DisposeObject(FSpellContainer);
  DisposeObject(FPropsBag);
  DisposeObject(FCoreData);

  inherited Destroy;
end;

procedure TBio.FreeSpells;
var
  i: Integer;
begin
  for i := 0 to FSpellContainer.Count - 1 do
      DisposeObject(FSpellContainer[i]);
  FSpellContainer.Clear;
end;

function TPlayer.GetBio(index: Integer): TBio;
begin
  Result := TBio(FBioList[index]);
end;

constructor TPlayer.Create(AOwner: TRULECore);
begin
  inherited Create;
  FOwner := AOwner;
  FPlayerID := '';
  FCampID := '';
  FBioList := TCoreClassListForObj.Create;
end;

destructor TPlayer.Destroy;
begin
  DisposeObject(FBioList);
  inherited;
end;

procedure TPlayer.Delete;
begin
  Owner.DeletePlayer(FPlayerID);
  DisposeObject(Self);
end;

procedure TPlayer.Progress(deltaTime: Double);
var
  i: Integer;
begin
  for i := 0 to BioCount - 1 do
      Bios[i].Progress(deltaTime);
end;

function TPlayer.NewBio(Region: TRegion; Position: TVec2; RollAngle, radius: TGeoFloat): TBio;
begin
  Result := TBio.Create(Self, Region, Position, RollAngle, radius);
  FBioList.Add(Result);
  Owner.DoNewBioNotify(Result);
end;

function TPlayer.BioCount: Integer;
begin
  Result := FBioList.Count;
end;

function TPlayer.DeleteBio(b: TBio): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := 0;
  while i < FBioList.Count do
    begin
      if FBioList[i] = b then
        begin
          FBioList.Delete(i);
          Result := True;
        end
      else
          inc(i);
    end;
end;

function TPlayer.ExistsBio(b: TBio): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FBioList.Count - 1 do
    if FBioList[i] = b then
        Exit;
  Result := False;
end;

procedure TPlayer.Clear;
begin
  FBioList.Clear;
end;

function TRULECore.GetPlayerOfID(Name: U_String): TPlayer;
begin
  Result := TPlayer(FPlayerHashList[Name.Text]);
end;

function TRULECore.GetPlayers(index: Integer): TPlayer;
begin
  Result := TPlayer(FPlayerList[index]);
end;

procedure TRULECore.DoNewBioNotify(Bio: TBio);
begin
end;

procedure TRULECore.DoPlayerUpdateNotify;
begin
end;

procedure TRULECore.ClearAllBio;
  procedure _ProcessPlayerBio(p: TPlayer);
  var
    b: TBio;
  begin
    while p.BioCount > 0 do
      begin
        b := p.Bios[0];
        b.Delete;
      end;
  end;

  procedure _ProcessPlayer;
  var
    i: Integer;
  begin
    for i := 0 to PlayerCount - 1 do
        _ProcessPlayerBio(Players[i]);
  end;

begin
  _ProcessPlayer;
end;

procedure TRULECore.ClearAllPlayer;
var
  p: TPlayer;
begin
  while PlayerCount > 0 do
    begin
      p := Players[0];
      p.Delete;
    end;
end;

constructor TRULECore.Create;
begin
  inherited Create;
  // relation engine
  FRelation := TCampRelation.Create;
  // player object list
  FPlayerHashList := THashObjectList.Create(False);
  // player list;
  FPlayerList := TCoreClassListForObj.Create;
  // camp list
  FCampList := TCoreClassStringList.Create;
  // region root
  FRootRegion := TRootRegion.Create(nil, Self);
  FRootRegion.Name := 'Root';
  FRootRegion.Description := 'Root Region';

  FLastMakeBioID := 0;

  // init base relation
  InitBaseRelation;
end;

destructor TRULECore.Destroy;
begin
  ClearAllBio;
  ClearAllPlayer;

  DisposeObject(FRootRegion);
  DisposeObject(FRelation);
  DisposeObject(FPlayerHashList);
  DisposeObject(FPlayerList);
  DisposeObject(FCampList);

  inherited Destroy;
end;

procedure TRULECore.Progress(deltaTime: Double);
var
  i: Integer;
begin
  for i := 0 to PlayerCount - 1 do
      Players[i].Progress(deltaTime);

  FRootRegion.Progress(deltaTime);
end;

procedure TRULECore.InitBaseRelation;
begin
  FRelation.Clear;

  FRelation.DefaultType := rtAntagonize;

  FRelation.ChangeRelation('NPC', 'NPC', rtNeutrally, 1);
  FRelation.ChangeRelation('Monster', 'Monster', rtAlly, 1);
  FRelation.ChangeRelation('Monster', 'NPC', rtAntagonize, 1);
end;

function TRULECore.NewPlayer(PlayerID, CampID: U_String): TPlayer;
begin
  if (not umlExistsChar(PlayerID, '*?')) and (PlayerID.Text <> '') and (not FPlayerHashList.Exists(PlayerID.Text)) then
    begin
      Result := TPlayer.Create(Self);
      FPlayerHashList.Add(PlayerID.Text, Result);
      FPlayerList.Clear;
      FPlayerHashList.GetAsList(FPlayerList);
      AddCamp(CampID, rtAntagonize);
      Result.FPlayerID := PlayerID;
      Result.FCampID := CampID;
      DoPlayerUpdateNotify;
    end
  else
      Result := nil;
end;

procedure TRULECore.DeletePlayer(PlayerID: U_String);
  procedure _DeletePlayerBio(p: TPlayer);
  var
    b: TBio;
  begin
    while p.BioCount > 0 do
      begin
        b := p.Bios[0];
        b.Delete;
      end;
  end;

begin
  if ExistsPlayer(PlayerID) then
    begin
      _DeletePlayerBio(PlayerOfID[PlayerID]);
      FPlayerHashList.Delete(PlayerID.Text);
      FPlayerList.Clear;
      FPlayerHashList.GetAsList(FPlayerList);
      FRelation.Delete(PlayerID);
      umlDeleteStrings(PlayerID, FCampList, True);
      DoPlayerUpdateNotify;
    end;
end;

function TRULECore.PlayerCount: Integer;
begin
  Result := FPlayerList.Count;
end;

procedure TRULECore.ClearPlayer;
begin
  ClearAllBio;
  ClearAllPlayer;
  FPlayerHashList.Clear;
  FPlayerList.Clear;
end;

function TRULECore.ExistsPlayer(PlayerID: U_String): Boolean;
begin
  Result := FPlayerHashList.Exists(PlayerID.Text);
end;

function TRULECore.GetBioOfID(bioID: Integer): TBio;
  procedure ProcessBio(p: TPlayer);
  var
    i: Integer;
    b: TBio;
  begin
    for i := 0 to p.BioCount - 1 do
      begin
        b := p.Bios[i];
        if b.ID = bioID then
          begin
            Result := b;
            Exit;
          end;
      end;
  end;

  procedure ProcessPlayer;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to PlayerCount - 1 do
      begin
        ProcessBio(Players[i]);
        if Result <> nil then
            Exit;
      end;
  end;

begin
  ProcessPlayer;
end;

function TRULECore.MakeBioID: Integer;
begin
  Result := FLastMakeBioID + 1;
  while GetBioOfID(Result) <> nil do
      inc(Result);
end;

procedure TRULECore.AddCamp(CampID: U_String; AroundRelation: TRelationType);
var
  i: Integer;
begin
  if umlAddNewStrTo(CampID, FCampList, True) then
    begin
      for i := 0 to FCampList.Count - 1 do
        if not umlMultipleMatch(True, CampID, FCampList[i]) then
            FRelation.ChangeRelation(CampID, FCampList[i], AroundRelation, 1);
      FRelation.ChangeRelation(CampID, 'Monster', rtAntagonize, 1);
      FRelation.ChangeRelation(CampID, 'NPC', rtNeutrally, 1);
      FRelation.ChangeRelation(CampID, CampID, rtAlly, 1);
    end;
end;

function TRULECore.BioRelation(b1, b2: TBio): TRelationType;
begin
  Result := PlayerRelation(b1.FOwner, b2.FOwner);
end;

function TRULECore.PlayerRelation(p1, p2: TPlayer): TRelationType;
begin
  if p1 = p2 then
      Result := rtPlayer
  else
      Result := FRelation.GetRelationType(p1.CampID, p2.CampID);
end;

procedure TRULECore.LoadFromStream(stream: TCoreClassStream);
begin
  if FRootRegion <> nil then
    begin
      DisposeObject(FRootRegion);
      FRootRegion := nil;
    end;
  FRootRegion := TRootRegion.LoadAllFromStream(stream, Self);
end;

procedure TRULECore.SaveToStream(stream: TCoreClassStream);
begin
  FRootRegion.SaveAllToStream(stream);
end;

function GetRegistedRegionName(Region: TRegion): U_String;
var
  i: Integer;
begin
  for i := 0 to RegistedRegionList.Count - 1 do
    if PRegistedRegion(RegistedRegionList[i])^.RegionClass = Region.ClassType then
        Exit(PRegistedRegion(RegistedRegionList[i])^.Name);
  Result := '';
end;

function RegistedRegionExists(Name: U_String): Boolean;
var
  i: Integer;
begin
  for i := 0 to RegistedRegionList.Count - 1 do
    if PRegistedRegion(RegistedRegionList[i])^.Name.Same(Name) then
        Exit(True);
  Result := False;
end;

function GetRegistedRegionClass(Regname: U_String): TRegionClass;
var
  i: Integer;
begin
  for i := 0 to RegistedRegionList.Count - 1 do
    if PRegistedRegion(RegistedRegionList[i])^.Name.Same(Regname) then
        Exit(PRegistedRegion(RegistedRegionList[i])^.RegionClass);
  Result := nil;
end;

procedure RegistedRegion(RegionClass: TRegionClass; Name, Description: U_String);
var
  p: PRegistedRegion;
begin
  if RegistedRegionExists(Name) then
    begin
      DoStatus('region already registed:%s', [Name.Text]);
      Exit;
    end;
  new(p);
  p^.RegionClass := RegionClass;
  p^.Name := Name;
  p^.Description := Description;
  RegistedRegionList.Add(p);
end;

procedure _initialization;
begin
  RegistedRegionList := TCoreClassList.Create;
end;

procedure _finalization;
var
  i: Integer;
  p: PRegistedRegion;
begin
  for i := 0 to RegistedRegionList.Count - 1 do
    begin
      p := PRegistedRegion(RegistedRegionList[i]);
      Dispose(p);
    end;
  DisposeObject(RegistedRegionList);
  RegistedRegionList := nil;
end;

initialization

_initialization;
RegistedRegion(TMapRegion, 'MapRegion', 'big map region');
RegistedRegion(TBattleRegion, 'BattleRegion', 'battle region');
RegistedRegion(TRootRegion, 'rootRegion', 'root region');

finalization

_finalization;

end.
 
 

