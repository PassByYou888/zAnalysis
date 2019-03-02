{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit SpellBase;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, UnicodeMixedLib, ListEngine, PascalStrings,
  Geometry2DUnit, BioBase, CampRelationBase, DataFrameEngine;

type
  TSpellBase = class;

  TSpellMode = (
    // direct local
    smDirect,

    // bio target object
    smBio, smBioAlly, smBioAllyOrSelf, smBioNeutrally, smBioAntagonize, smBioSelf,

    // area target
    smArea, smAreaAlly, smAreaAllyOrSelf, smAreaNeutrally, smAreaAntagonize, smAreaSelf,

    // coordnate target
    smPoint,

    // direction point target
    smDirectionPoint,

    // Range halo spell mode
    smHalo);

  TSpellTriggerBase = class(TCoreClassObject)
  private
    FOwner: TSpellBase;
  public
    constructor Create(AOwner: TSpellBase);
    destructor Destroy; override;

    property Owner: TSpellBase read FOwner;
  end;

  TSpellTriggerObject = class(TSpellTriggerBase)
  private
    // TBioBase
    FTarget: TBioBase;
    FTargetOwnerPlayer: U_String;
    FTargetBioID: Integer;

    // trigger obejct=TBioBase
    procedure TargetFreeNotify(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
    procedure TargetDieNotify(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
    procedure SetTarget(const Value: TBioBase);
  public
    constructor Create(AOwner: TSpellBase);
    destructor Destroy; override;

    // TBioBase
    property Target: TBioBase read FTarget write SetTarget;
    property TargetOwnerPlayer: U_String read FTargetOwnerPlayer;
    property TargetBioID: Integer read FTargetBioID;
  end;

  TSpellTriggerArea = class(TSpellTriggerBase)
  private
    // TBioBase list
    FPosition: TVec2;
    FRadius: TGeoFloat;
  public
    constructor Create(AOwner: TSpellBase);
    destructor Destroy; override;

    // TBioBase list
    property Position: TVec2 read FPosition;
    property radius: TGeoFloat read FRadius;
  end;

  TSpellTriggerPoint = class(TSpellTriggerBase)
  private
    FTarget: TVec2;
  public
    constructor Create(AOwner: TSpellBase);
    destructor Destroy; override;

    property Target: TVec2 read FTarget write FTarget;
  end;

  TRULEBioSpellActionMessage = (bsanMovementIng, bsanStopRollAngleIng, bsanMovementOver, bsanMovementDone, bsanAnimationTrigger, bsanAnimationOver, bsanBreak, bsanEnd);

  TRULEBioSpellDelayActionMessageData = record
    NotifyType: TRULEBioSpellActionMessage;
    DelayTime: Double;
  end;

  PRULEBioSpellDelayActionMessageData = ^TRULEBioSpellDelayActionMessageData;

  TRULEBioSpellDelayFreeData = record
    FreeObjectIntf: TCoreClassObject;
    DelayTime: Double;
  end;

  PRULEBioSpellDelayFreeData = ^TRULEBioSpellDelayFreeData;

  TSpellBase = class(TCoreClassInterfacedObject, IBioActionBaseNotifyIntf)
  protected
    FOwner: TBioBase;

    // mode
    FMode: TSpellMode;

    // loop execute spell
    FLoopMode: Boolean;

    // distnace
    FLaunchDistance: TGeoFloat;

    // max launch angle
    FLaunchAngle: TGeoFloat;

    // second
    FCoolDownTime: Double;

    // spell animation switch flag
    FAnimationFlag: Variant;

    FTriggerTarget: TSpellTriggerBase;

    FObjectList: THashObjectList;
    FVariantList: THashVariantList;

    procedure SetMode(const Value: TSpellMode);

    function GetObjectList: THashObjectList;
    function GetVariantList: THashVariantList;
  protected
    // self state private
    FCurrentDeltaTime, FCurrentCoolDownTime: Double;
    FRunCount: Integer;
    FLoopModeWait: Boolean;
    FLastExecuteTime: Double;
    FCurrentBioAction: TBioActionBase;

    procedure InitSelfPrivateValue;
  protected
    FDelayActionMsgList: TCoreClassList;

    procedure InitDelayActionMsgSection;
    procedure FreeDelayActionMsgSection;

    procedure ClearDelayActionMsgList;
    procedure AddDelayActionMsg(Delay: Double; Notify: TRULEBioSpellActionMessage);
    procedure ProcessDelayActionMsg(deltaTime: Double);
  protected
    FDelayFreeDataList: TCoreClassList;

    procedure InitDelayFreeDataSection;
    procedure FreeDelayFreeDataSection;

    procedure ClearDelayFreeDataList;
    function ExistsDelayFreeData(ObjectIntf: TCoreClassObject): PRULEBioSpellDelayFreeData;
    procedure AddDelayFreeDataIntf(Delay: Double; ObjectIntf: TCoreClassObject);
    procedure ProcessDelayFreeData(deltaTime: Double);
  protected
    // notify intf
    procedure DoSpellInstall;
    procedure DoSpellUnInstall;
    procedure DoSpellExecuteStart;
    procedure DoSpellExecuteBreak;
    procedure DoSpellExecuteProgress(deltaTime: Double);
    procedure DoSpellExecute;
    procedure DoSpellExecuteOver;
  protected
    FActionActived: Boolean;

    procedure ActionDestroy(Action: TBioActionBase); virtual;
    function ActionExecuteBeforeCheck(Action: TBioActionBase): Boolean; virtual;
    function ActionExecuteBefore(Action: TBioActionBase): Boolean; virtual;
    procedure ActionProgress(Action: TBioActionBase; deltaTime: Double); virtual;
    procedure ActionExecuteBreak(Action: TBioActionBase); virtual;
    function ActionExecuteAfterCheck(Action: TBioActionBase): Boolean; virtual;
    function ActionExecuteAfter(Action: TBioActionBase): Boolean; virtual;
    procedure ActionExecuteAfterSuccess(Action: TBioActionBase); virtual;
    procedure ActionExecuteAfterFailed(Action: TBioActionBase); virtual;

    procedure InstallActionHook(BioAction: TBioActionBase); virtual;
    procedure UnInstallActionHook; virtual;
  public
    constructor Create(AOwner: TBioBase); virtual;
    destructor Destroy; override;

    // spell description
    class function Description: U_String; virtual;
    // spell name
    class function Name: U_String; virtual;
    // spell category
    class function Category: U_String; virtual;
    // spell caninstall
    class function CanInstall: Boolean; virtual;

    procedure AppendToDelayFreeList(Delay: Double; ObjectIntf: TCoreClassObject);

    procedure Progress(deltaTime: Double); virtual;

    // direct style
    function ExecuteSpell_Direct(Action: TBioActionBase): Boolean;
    // bio style
    function ExecuteSpell_ToBio(Action: TBioActionBase; Bio: TBioBase): Boolean;
    function ExecuteSpell_ToBioAlly(Action: TBioActionBase; Bio: TBioBase): Boolean;
    function ExecuteSpell_ToBioAllyOrSelf(Action: TBioActionBase; Bio: TBioBase): Boolean;
    function ExecuteSpell_ToBioNeutrally(Action: TBioActionBase; Bio: TBioBase): Boolean;
    function ExecuteSpell_ToBioAntagonize(Action: TBioActionBase; Bio: TBioBase): Boolean;
    function ExecuteSpell_ToBioSelf(Action: TBioActionBase; Bio: TBioBase): Boolean;
    // area style
    function ExecuteSpell_ToArea(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
    function ExecuteSpell_ToAreaAlly(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
    function ExecuteSpell_ToAreaAllyOrSelf(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
    function ExecuteSpell_ToAreaNeutrally(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
    function ExecuteSpell_ToAreaAntagonize(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
    function ExecuteSpell_ToAreaSelf(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
    // point style
    function ExecuteSpell_ToPoint(Action: TBioActionBase; ToPosition: TVec2): Boolean;
    // direction point style
    function ExecuteSpell_ToDirectionPoint(Action: TBioActionBase; ToPosition: TVec2): Boolean;

    // get area bio list
    function GetAreaSpellDestBioList(ToPosition: TVec2; radius: TGeoFloat; OutList: TCoreClassListForObj): Integer;

    // spell animation trigger execute
    procedure PostSpellExecuteNotify;
    // spell animation over
    procedure PostSpellExecuteOverNotify;
    // spell break
    procedure PostSpellBreak;
    // spell end
    procedure PostSpellEnd;
    // post spell process notify
    procedure PostSpellPreprocessNotify(Notify: TRULEBioSpellActionMessage);

    // spell animation trigger execute
    procedure DelayPostSpellExecuteNotify(Delay: Double);
    // spell animation over
    procedure DelayPostSpellExecuteOverNotify(Delay: Double);
    // spell break
    procedure DelayPostSpellBreak(Delay: Double);
    // spell end
    procedure DelayPostSpellEnd(Delay: Double);
    // post spell process notify
    procedure DelayPostSpellPreprocessNotify(Delay: Double; Notify: TRULEBioSpellActionMessage);

    // spell state
    function SpellIsReadyOk: Boolean;
    function SpellIsProcess: Boolean;
    function SpellCanExecuteOfPos(ToPosition: TVec2; ARatio: TGeoFloat = 1.0): Boolean;
    function SpellCanExecuteOfBio(ABio: TBioBase; ARatio: TGeoFloat): Boolean;
    function SpellCanExecuteOfTime: Boolean;
    function GetCooldownWaitTime: Double;

    function SpellCanExecuteOfCost: Boolean; virtual;
    procedure DoProcessCost;

    property Owner: TBioBase read FOwner;

    property Mode: TSpellMode read FMode write SetMode;

    // loop execute spell
    property LoopMode: Boolean read FLoopMode write FLoopMode;

    // distnace
    property LaunchDistance: TGeoFloat read FLaunchDistance write FLaunchDistance;

    // max launch angle
    property LaunchAngle: TGeoFloat read FLaunchAngle write FLaunchAngle;

    // second
    property CoolDownTime: Double read FCoolDownTime write FCoolDownTime;

    // spell animation switch flag
    property AnimationFlag: Variant read FAnimationFlag write FAnimationFlag;

    property TriggerTarget: TSpellTriggerBase read FTriggerTarget;

    property VariantList: THashVariantList read GetVariantList;
    property ObjectList: THashObjectList read GetObjectList;
  end;

  TSpellEffect = class(TCoreClassPersistent)
  public
    TriBeforeEffect: U_String;
    TriEffect: U_String;
    TriAfterEffect: U_String;

    TriSound: U_String;

    Bullet: U_String;
    BulletTailEffect: U_String;

    CollisionBeforeEffect: U_String;
    CollisionEffect: U_String;
    CollisionAfterEffect: U_String;

    CollisionSound: U_String;

    constructor Create;
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
  end;

  TSpellClass = class of TSpellBase;

  TRegSpell = record
    SpClass: TSpellClass;
  end;

  PRegSpell = ^TRegSpell;

var
  SpellRegistedContainer: TCoreClassList = nil;

function StrToSpellMode(s: SystemString): TSpellMode;
function SpellModeToStr(SM: TSpellMode): SystemString;

function ExistsRegSpellClass(SpClass: TSpellClass): Boolean; overload;
function ExistsRegSpellClass(Regname: U_String): Boolean; overload;
function RegSpellClass(SpClass: TSpellClass): Boolean;
function GetRegSpellClass(Regname: U_String): TSpellClass;
function GetSpellClassRegName(SpClass: TClass): U_String;

implementation

uses CoreRULE, DoStatusIO;

procedure InitSpellRegistedContainer;
begin
  if SpellRegistedContainer = nil then
      SpellRegistedContainer := TCoreClassList.Create;
end;

procedure FreeSpellRegistedContainer;
var
  i: Integer;
  p: PRegSpell;
begin
  for i := 0 to SpellRegistedContainer.Count - 1 do
    begin
      p := SpellRegistedContainer[i];
      Dispose(p);
    end;
  DisposeObject(SpellRegistedContainer);
  SpellRegistedContainer := nil;
end;

function ExistsRegSpellClass(SpClass: TSpellClass): Boolean;
var
  i: Integer;
  p: PRegSpell;
begin
  InitSpellRegistedContainer;
  Result := True;
  for i := 0 to SpellRegistedContainer.Count - 1 do
    begin
      p := SpellRegistedContainer[i];
      if p^.SpClass = SpClass then
          Exit;
    end;
  Result := False;
end;

function ExistsRegSpellClass(Regname: U_String): Boolean;
var
  i: Integer;
  p: PRegSpell;
begin
  InitSpellRegistedContainer;
  Result := True;
  for i := 0 to SpellRegistedContainer.Count - 1 do
    begin
      p := SpellRegistedContainer[i];
      if umlMultipleMatch(True, Regname, p^.SpClass.Name) then
          Exit;
    end;
  Result := False;
end;

function RegSpellClass(SpClass: TSpellClass): Boolean;
var
  p: PRegSpell;
begin
  InitSpellRegistedContainer;
  Result := (SpClass.Name.Len = 0) or (ExistsRegSpellClass(SpClass.Name));
  if Result then
    begin
      raise CoreClassException.Create('Spell already exists or empty "' + SpClass.Name.Text + '"');
      Exit;
    end;
  new(p);
  p^.SpClass := SpClass;
  SpellRegistedContainer.Add(p);
end;

function GetRegSpellClass(Regname: U_String): TSpellClass;
var
  i: Integer;
  p: PRegSpell;
begin
  InitSpellRegistedContainer;
  for i := 0 to SpellRegistedContainer.Count - 1 do
    begin
      p := SpellRegistedContainer[i];
      if umlMultipleMatch(True, Regname, p^.SpClass.Name) then
        begin
          Result := p^.SpClass;
          Exit;
        end;
    end;
  Result := nil;
end;

function GetSpellClassRegName(SpClass: TClass): U_String;
var
  i: Integer;
  p: PRegSpell;
begin
  InitSpellRegistedContainer;
  for i := 0 to SpellRegistedContainer.Count - 1 do
    begin
      p := SpellRegistedContainer[i];
      if p^.SpClass = SpClass then
        begin
          Result := p^.SpClass.Name;
          Exit;
        end;
    end;
  Result.Text := '';
end;

function StrToSpellMode(s: SystemString): TSpellMode;
begin
  if umlMultipleMatch('Direct', s) then
      Result := smDirect
  else if umlMultipleMatch('Bio', s) then
      Result := smBio
  else if umlMultipleMatch('BioAlly', s) then
      Result := smBioAlly
  else if umlMultipleMatch('BioAllyOrSelf', s) then
      Result := smBioAllyOrSelf
  else if umlMultipleMatch('BioNeutrally', s) then
      Result := smBioNeutrally
  else if umlMultipleMatch('BioAntagonize', s) then
      Result := smBioAntagonize
  else if umlMultipleMatch('BioSelf', s) then
      Result := smBioSelf
  else if umlMultipleMatch('Area', s) then
      Result := smArea
  else if umlMultipleMatch('AreaAlly', s) then
      Result := smAreaAlly
  else if umlMultipleMatch('AreaAllyOrSelf', s) then
      Result := smAreaAllyOrSelf
  else if umlMultipleMatch('AreaNeutrally', s) then
      Result := smAreaNeutrally
  else if umlMultipleMatch('AreaAntagonize', s) then
      Result := smAreaAntagonize
  else if umlMultipleMatch('AreaSelf', s) then
      Result := smAreaSelf
  else if umlMultipleMatch('Point', s) then
      Result := smPoint
  else if umlMultipleMatch('DirectionPoint', s) then
      Result := smDirectionPoint
  else if umlMultipleMatch('Halo', s) then
      Result := smHalo
  else
      Result := smDirect;
end;

function SpellModeToStr(SM: TSpellMode): SystemString;
begin
  case SM of

    // direct local
    smDirect:
      Result := 'Direct';

    // bio target object
    smBio:
      Result := 'Bio';
    smBioAlly:
      Result := 'BioAlly';
    smBioAllyOrSelf:
      Result := 'BioAllyOrSelf';
    smBioNeutrally:
      Result := 'BioNeutrally';
    smBioAntagonize:
      Result := 'BioAntagonize';
    smBioSelf:
      Result := 'BioSelf';

    // area target
    smArea:
      Result := 'Area';
    smAreaAlly:
      Result := 'AreaAlly';
    smAreaAllyOrSelf:
      Result := 'AreaAllyOrSelf';
    smAreaNeutrally:
      Result := 'AreaNeutrally';
    smAreaAntagonize:
      Result := 'AreaAntagonize';
    smAreaSelf:
      Result := 'AreaSelf';

    // coordnate target
    smPoint:
      Result := 'Point';

    // direction point
    smDirectionPoint:
      Result := 'DirectionPoint';

    // Range halo spell mode
    smHalo:
      Result := 'Halo';

    else
      Result := 'Direct';
  end;
end;

constructor TSpellTriggerBase.Create(AOwner: TSpellBase);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TSpellTriggerBase.Destroy;
begin
  inherited Destroy;
end;

procedure TSpellTriggerObject.TargetFreeNotify(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
begin
  if (Owner <> nil) and (Owner.FCurrentBioAction <> nil) then
      Owner.PostSpellEnd;
  Target := nil;
end;

procedure TSpellTriggerObject.TargetDieNotify(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
begin
  if (Owner <> nil) and (Owner.FCurrentBioAction <> nil) then
      Owner.PostSpellEnd;
  Target := nil;
end;

procedure TSpellTriggerObject.SetTarget(const Value: TBioBase);
begin
  if FTarget <> nil then
    begin
      FTarget.UnRegisterFreeBackcall(Self);
      FTarget.UnRegisterDieBackcall(Self);
    end;
  FTarget := Value;
  if FTarget <> nil then
    begin
{$IFDEF FPC}
      FTarget.RegisterFreeBackcall(Self, @TargetFreeNotify);
      FTarget.RegisterDieBackcall(Self, @TargetDieNotify);
{$ELSE}
      FTarget.RegisterFreeBackcall(Self, TargetFreeNotify);
      FTarget.RegisterDieBackcall(Self, TargetDieNotify);
{$ENDIF}
      FTargetOwnerPlayer := FTarget.PlayerID;
      FTargetBioID := FTarget.ID;
    end
  else
    begin
      FTargetOwnerPlayer := '';
      FTargetBioID := 0;
    end;
end;

constructor TSpellTriggerObject.Create(AOwner: TSpellBase);
begin
  inherited Create(AOwner);
  FTarget := nil;
  FTargetOwnerPlayer := '';
  FTargetBioID := 0;
end;

destructor TSpellTriggerObject.Destroy;
begin
  Target := nil;
  inherited Destroy;
end;

constructor TSpellTriggerArea.Create(AOwner: TSpellBase);
begin
  inherited Create(AOwner);
  FPosition := NULLPoint;
  FRadius := 1.0;
end;

destructor TSpellTriggerArea.Destroy;
begin
  inherited Destroy;
end;

constructor TSpellTriggerPoint.Create(AOwner: TSpellBase);
begin
  inherited Create(AOwner);
  FTarget := NULLPoint;
end;

destructor TSpellTriggerPoint.Destroy;
begin
  inherited Destroy;
end;

procedure TSpellBase.SetMode(const Value: TSpellMode);
begin
  if FMode <> Value then
    begin
      FMode := Value;

      case FMode of
        // direct local
        smDirect:
          begin
            if (not(FTriggerTarget is TSpellTriggerBase)) then
              begin
                if (FTriggerTarget <> nil) then
                  begin
                    DisposeObject(FTriggerTarget);
                    FTriggerTarget := nil;
                  end;
                FTriggerTarget := TSpellTriggerBase.Create(Self);
              end;
          end;

        // bio target object
        smBio, smBioAlly, smBioAllyOrSelf, smBioNeutrally, smBioAntagonize, smBioSelf:
          begin
            if (not(FTriggerTarget is TSpellTriggerObject)) then
              begin
                if (FTriggerTarget <> nil) then
                  begin
                    DisposeObject(FTriggerTarget);
                    FTriggerTarget := nil;
                  end;
                FTriggerTarget := TSpellTriggerObject.Create(Self);
              end;
          end;

        // area target
        smArea, smAreaAlly, smAreaAllyOrSelf, smAreaNeutrally, smAreaAntagonize, smAreaSelf:
          begin
            if (not(FTriggerTarget is TSpellTriggerArea)) then
              begin
                if (FTriggerTarget <> nil) then
                  begin
                    DisposeObject(FTriggerTarget);
                    FTriggerTarget := nil;
                  end;
                FTriggerTarget := TSpellTriggerArea.Create(Self);
              end;
          end;

        // coordnate target
        smPoint, smDirectionPoint:
          begin
            if (not(FTriggerTarget is TSpellTriggerPoint)) then
              begin
                if (FTriggerTarget <> nil) then
                  begin
                    DisposeObject(FTriggerTarget);
                    FTriggerTarget := nil;
                  end;
                FTriggerTarget := TSpellTriggerPoint.Create(Self);
              end;
          end;
        // coordnate target
        smHalo:
          begin
            if (FTriggerTarget <> nil) and (FTriggerTarget.ClassName <> TSpellTriggerBase.ClassName) then
              begin
                DisposeObject(FTriggerTarget);
                FTriggerTarget := nil;
              end;
            FTriggerTarget := TSpellTriggerBase.Create(Self);
          end;

        else
          Assert(False, 'invalid Spell Mode');
      end;
    end;
end;

function TSpellBase.GetObjectList: THashObjectList;
begin
  if FObjectList = nil then
      FObjectList := THashObjectList.Create(False);
  Result := FObjectList;
end;

function TSpellBase.GetVariantList: THashVariantList;
begin
  if FVariantList = nil then
      FVariantList := THashVariantList.Create;
  Result := FVariantList;
end;

procedure TSpellBase.InitSelfPrivateValue;
begin
  FCurrentDeltaTime := 0;
  FCurrentCoolDownTime := 0;
  FRunCount := 0;
  FLoopModeWait := False;
  FLastExecuteTime := 0;
  FCurrentBioAction := nil;
end;

procedure TSpellBase.InitDelayActionMsgSection;
begin
  FDelayActionMsgList := TCoreClassList.Create;
end;

procedure TSpellBase.FreeDelayActionMsgSection;
begin
  ClearDelayActionMsgList;
  DisposeObject(FDelayActionMsgList);
end;

procedure TSpellBase.ClearDelayActionMsgList;
var
  i: Integer;
begin
  for i := 0 to FDelayActionMsgList.Count - 1 do
    begin
      Dispose(PRULEBioSpellDelayActionMessageData(FDelayActionMsgList[i]));
    end;
  FDelayActionMsgList.Clear;
end;

procedure TSpellBase.AddDelayActionMsg(Delay: Double; Notify: TRULEBioSpellActionMessage);
var
  p: PRULEBioSpellDelayActionMessageData;
begin
  new(p);
  p^.NotifyType := Notify;
  p^.DelayTime := Delay;
  FDelayActionMsgList.Add(p);
end;

procedure TSpellBase.ProcessDelayActionMsg(deltaTime: Double);
var
  i: Integer;
  p: PRULEBioSpellDelayActionMessageData;
begin
  i := 0;
  while i < FDelayActionMsgList.Count do
    begin
      p := FDelayActionMsgList[i];
      p^.DelayTime := p^.DelayTime - deltaTime;
      if p^.DelayTime <= 0 then
        begin
          // post msg
          if FCurrentBioAction <> nil then
              PostSpellPreprocessNotify(p^.NotifyType);

          Dispose(p);
          FDelayActionMsgList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TSpellBase.InitDelayFreeDataSection;
begin
  FDelayFreeDataList := TCoreClassList.Create;
end;

procedure TSpellBase.FreeDelayFreeDataSection;
begin
  ClearDelayFreeDataList;
  DisposeObject(FDelayFreeDataList);
end;

procedure TSpellBase.ClearDelayFreeDataList;
var
  i: Integer;
  p: PRULEBioSpellDelayFreeData;
begin
  for i := 0 to FDelayFreeDataList.Count - 1 do
    begin
      p := FDelayFreeDataList[i];
      try
          DisposeObject(p^.FreeObjectIntf);
      except
      end;
      Dispose(p);
    end;
  FDelayFreeDataList.Clear;
end;

function TSpellBase.ExistsDelayFreeData(ObjectIntf: TCoreClassObject): PRULEBioSpellDelayFreeData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FDelayFreeDataList.Count - 1 do
    if PRULEBioSpellDelayFreeData(FDelayFreeDataList[i])^.FreeObjectIntf = ObjectIntf then
      begin
        Result := FDelayFreeDataList[i];
        Exit;
      end;
end;

procedure TSpellBase.AddDelayFreeDataIntf(Delay: Double; ObjectIntf: TCoreClassObject);
var
  p: PRULEBioSpellDelayFreeData;
begin
  p := ExistsDelayFreeData(ObjectIntf);
  if p = nil then
    begin
      new(p);
      p^.FreeObjectIntf := ObjectIntf;
      p^.DelayTime := Delay;
      FDelayFreeDataList.Add(p);
    end;
end;

procedure TSpellBase.ProcessDelayFreeData(deltaTime: Double);
var
  i: Integer;
  p: PRULEBioSpellDelayFreeData;
begin
  i := 0;
  while i < FDelayFreeDataList.Count do
    begin
      p := FDelayFreeDataList[i];
      p^.DelayTime := p^.DelayTime - deltaTime;
      if p^.DelayTime <= 0 then
        begin
          try
              DisposeObject(p^.FreeObjectIntf);
          except
          end;
          // execute free data
          Dispose(p);
          FDelayFreeDataList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TSpellBase.DoSpellInstall;
begin
end;

procedure TSpellBase.DoSpellUnInstall;
begin
end;

procedure TSpellBase.DoSpellExecuteStart;
begin
end;

procedure TSpellBase.DoSpellExecuteBreak;
begin
end;

procedure TSpellBase.DoSpellExecuteProgress(deltaTime: Double);
begin
end;

procedure TSpellBase.DoSpellExecute;
begin
end;

procedure TSpellBase.DoSpellExecuteOver;
begin
end;

procedure TSpellBase.ActionDestroy(Action: TBioActionBase);
begin
  if FTriggerTarget is TSpellTriggerObject then
      TSpellTriggerObject(FTriggerTarget).Target := nil;

  DoSpellExecuteBreak;
  UnInstallActionHook;
  FLoopModeWait := False;
  ClearDelayActionMsgList;
end;

function TSpellBase.ActionExecuteBeforeCheck(Action: TBioActionBase): Boolean;
begin
  Result := True;
end;

function TSpellBase.ActionExecuteBefore(Action: TBioActionBase): Boolean;
begin
  FLastExecuteTime := 0;
  FCurrentCoolDownTime := 0;
  Result := True;
end;

procedure TSpellBase.ActionProgress(Action: TBioActionBase; deltaTime: Double);
begin
  if FActionActived then
      DoSpellExecuteProgress(deltaTime);
end;

procedure TSpellBase.ActionExecuteBreak(Action: TBioActionBase);
var
  v: TVec2;
begin
  if FTriggerTarget is TSpellTriggerObject then
      TSpellTriggerObject(FTriggerTarget).Target := nil;

  v := Owner.Position;

  Owner.MovementStop;
  DoSpellExecuteBreak;
  UnInstallActionHook;
  FLoopModeWait := False;
  ClearDelayActionMsgList;
end;

function TSpellBase.ActionExecuteAfterCheck(Action: TBioActionBase): Boolean;
begin
  Result := True;
end;

function TSpellBase.ActionExecuteAfter(Action: TBioActionBase): Boolean;
begin
  Result := True;
end;

procedure TSpellBase.ActionExecuteAfterSuccess(Action: TBioActionBase);
begin
  if FTriggerTarget is TSpellTriggerObject then
      TSpellTriggerObject(FTriggerTarget).Target := nil;

  inc(FRunCount);
  UnInstallActionHook;
  FLoopModeWait := False;
  ClearDelayActionMsgList;
end;

procedure TSpellBase.ActionExecuteAfterFailed(Action: TBioActionBase);
begin
end;

procedure TSpellBase.InstallActionHook(BioAction: TBioActionBase);
begin
  Assert(FCurrentBioAction = nil, 'Current Bio Action Busy');
  FCurrentBioAction := BioAction;
  FCurrentBioAction.NotifyIntf := Self;
  FActionActived := False;
end;

procedure TSpellBase.UnInstallActionHook;
begin
  Assert(FCurrentBioAction <> nil, 'Current Bio Action Invalid');
  FCurrentBioAction := nil;
  FActionActived := False;
end;

constructor TSpellBase.Create(AOwner: TBioBase);
begin
  inherited Create;
  FOwner := AOwner;

  FMode := smHalo;

  FLoopMode := False;

  FLaunchDistance := 1;

  FLaunchAngle := 10;

  // second
  FCoolDownTime := 1;

  // spell animation switch flag
  FAnimationFlag := 0;

  FTriggerTarget := TSpellTriggerBase.Create(Self);

  FVariantList := nil;
  FObjectList := nil;

  InitSelfPrivateValue;
  InitDelayActionMsgSection;
  InitDelayFreeDataSection;
end;

destructor TSpellBase.Destroy;
begin
  if Owner <> nil then
      DoSpellUnInstall;
  FreeDelayActionMsgSection;
  FreeDelayFreeDataSection;
  if FVariantList <> nil then
      DisposeObject(FVariantList);
  if FObjectList <> nil then
      DisposeObject(FObjectList);
  if FTriggerTarget <> nil then
    begin
      DisposeObject(FTriggerTarget);
      FTriggerTarget := nil;
    end;
  inherited Destroy;
end;

class function TSpellBase.Description: U_String;
begin
  Result := '';
end;

class function TSpellBase.Name: U_String;
begin
  Result := '';
end;

class function TSpellBase.Category: U_String;
begin
  Result := '';
end;

// spell caninstall
class function TSpellBase.CanInstall: Boolean;
begin
  Result := True;
end;

procedure TSpellBase.AppendToDelayFreeList(Delay: Double; ObjectIntf: TCoreClassObject);
begin
  AddDelayFreeDataIntf(Delay, ObjectIntf);
end;

procedure TSpellBase.Progress(deltaTime: Double);
var
  v: TVec2;
begin
  FCurrentDeltaTime := deltaTime;
  FCurrentCoolDownTime := FCurrentCoolDownTime + FCurrentDeltaTime;

  if FLoopModeWait and SpellCanExecuteOfTime and SpellCanExecuteOfCost then
    begin
      FLoopModeWait := False;
      // FLastExecuteTime := 0;
      // FCurrentCoolDownTime := 0;
      inc(FRunCount);

      if FTriggerTarget is TSpellTriggerObject then
        begin
          if TSpellTriggerObject(FTriggerTarget).Target is TBioBase then
            begin
              v := TSpellTriggerObject(FTriggerTarget).Target.Position;
              if SpellCanExecuteOfBio(TSpellTriggerObject(FTriggerTarget).Target, 0.9) then
                begin
                  if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle then
                      PostSpellPreprocessNotify(bsanMovementDone)
                  else
                      Owner.RequestStopRollAngleTo(v);
                end
              else
                  PostSpellPreprocessNotify(bsanMovementOver);
            end
          else
              PostSpellPreprocessNotify(bsanMovementOver);
        end
      else if FTriggerTarget is TSpellTriggerArea then
        begin
          v := TSpellTriggerArea(FTriggerTarget).Position;
          if SpellCanExecuteOfPos(v, 1.0) then
            begin
              if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle then
                  PostSpellPreprocessNotify(bsanMovementDone)
              else
                  Owner.RequestStopRollAngleTo(v);
            end
          else
              PostSpellPreprocessNotify(bsanMovementOver);
        end
      else if FTriggerTarget is TSpellTriggerPoint then
        begin
          v := TSpellTriggerPoint(FTriggerTarget).Target;
          if SpellCanExecuteOfPos(v, 0.9) then
            begin
              if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle then
                  PostSpellPreprocessNotify(bsanMovementDone)
              else
                  Owner.RequestStopRollAngleTo(v);
            end
          else
              PostSpellPreprocessNotify(bsanMovementOver);
        end
      else
          PostSpellPreprocessNotify(bsanMovementDone);
    end;
  ProcessDelayActionMsg(deltaTime);
  ProcessDelayFreeData(deltaTime);
end;

function TSpellBase.ExecuteSpell_Direct(Action: TBioActionBase): Boolean;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smDirect then
      Exit;
  InstallActionHook(Action);
  Result := True;

  PostSpellPreprocessNotify(bsanMovementDone);
end;

function TSpellBase.ExecuteSpell_ToBio(Action: TBioActionBase; Bio: TBioBase): Boolean;
var
  ADestPos: TVec2;
begin
  Result := False;
  if Bio.IsDie then
      Exit;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smBio then
      Exit;
  TSpellTriggerObject(FTriggerTarget).Target := Bio;
  InstallActionHook(Action);
  Result := True;

  ADestPos := Bio.Position;

  if SpellCanExecuteOfPos(ADestPos) then
    begin
      if Owner.GetBio2DestAngleDistance(ADestPos) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ADestPos)
    end
  else
      Owner.RequestMovementTo(ADestPos);
end;

function TSpellBase.ExecuteSpell_ToBioAlly(Action: TBioActionBase; Bio: TBioBase): Boolean;
var
  ADestPos: TVec2;
begin
  Result := False;
  if Bio.IsDie then
      Exit;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smBioAlly then
      Exit;
  if Owner.GetRelation(Bio) <> rtAlly then
      Exit;
  TSpellTriggerObject(FTriggerTarget).Target := Bio;
  InstallActionHook(Action);
  Result := True;

  ADestPos := Bio.Position;

  if SpellCanExecuteOfPos(ADestPos) then
    begin
      if Owner.GetBio2DestAngleDistance(ADestPos) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ADestPos);
    end
  else
      Owner.RequestMovementTo(ADestPos);
end;

function TSpellBase.ExecuteSpell_ToBioAllyOrSelf(Action: TBioActionBase; Bio: TBioBase): Boolean;
var
  ADestPos: TVec2;
begin
  Result := False;
  if Bio.IsDie then
      Exit;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smBioAllyOrSelf then
      Exit;
  case Owner.GetRelation(Bio) of
    rtPlayer, rtAlly:
      begin
        TSpellTriggerObject(FTriggerTarget).Target := Bio;
        InstallActionHook(Action);
        Result := True;

        ADestPos := Bio.Position;

        if SpellCanExecuteOfPos(ADestPos) then
          begin
            if Owner.GetBio2DestAngleDistance(ADestPos) <= FLaunchAngle then
                PostSpellPreprocessNotify(bsanMovementDone)
            else
                Owner.RequestStopRollAngleTo(ADestPos);
          end
        else
            Owner.RequestMovementTo(ADestPos);
      end;
  end;
end;

function TSpellBase.ExecuteSpell_ToBioNeutrally(Action: TBioActionBase; Bio: TBioBase): Boolean;
var
  ADestPos: TVec2;
begin
  Result := False;
  if Bio.IsDie then
      Exit;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smBioNeutrally then
      Exit;
  case Owner.GetRelation(Bio) of
    rtNeutrally:
      begin
        TSpellTriggerObject(FTriggerTarget).Target := Bio;
        InstallActionHook(Action);
        Result := True;

        ADestPos := Bio.Position;

        if SpellCanExecuteOfPos(ADestPos) then
          begin
            if Owner.GetBio2DestAngleDistance(ADestPos) <= FLaunchAngle then
                PostSpellPreprocessNotify(bsanMovementDone)
            else
                Owner.RequestStopRollAngleTo(ADestPos);
          end
        else
            Owner.RequestMovementTo(ADestPos);
      end;
  end;
end;

function TSpellBase.ExecuteSpell_ToBioAntagonize(Action: TBioActionBase; Bio: TBioBase): Boolean;
var
  ADestPos: TVec2;
begin
  Result := False;
  if Bio.IsDie then
      Exit;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smBioAntagonize then
      Exit;
  case Owner.GetRelation(Bio) of
    rtAntagonize:
      begin
        TSpellTriggerObject(FTriggerTarget).Target := Bio;
        InstallActionHook(Action);
        Result := True;

        ADestPos := Bio.Position;

        if SpellCanExecuteOfPos(ADestPos) then
          begin
            if Owner.GetBio2DestAngleDistance(ADestPos) <= FLaunchAngle then
                PostSpellPreprocessNotify(bsanMovementDone)
            else
                Owner.RequestStopRollAngleTo(ADestPos)
          end
        else
            Owner.RequestMovementTo(ADestPos);
      end;
  end;
end;

function TSpellBase.ExecuteSpell_ToBioSelf(Action: TBioActionBase; Bio: TBioBase): Boolean;
var
  ADestPos: TVec2;
begin
  Result := False;
  if Bio.IsDie then
      Exit;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smBioSelf then
      Exit;
  case Owner.GetRelation(Bio) of
    rtPlayer:
      begin
        TSpellTriggerObject(FTriggerTarget).Target := Bio;
        InstallActionHook(Action);
        Result := True;

        ADestPos := Bio.Position;

        if SpellCanExecuteOfPos(ADestPos) then
          begin
            if Owner.GetBio2DestAngleDistance(ADestPos) <= FLaunchAngle then
                PostSpellPreprocessNotify(bsanMovementDone)
            else
                Owner.RequestStopRollAngleTo(ADestPos);
          end
        else
            Owner.RequestMovementTo(ADestPos);
      end;
  end;
end;

function TSpellBase.ExecuteSpell_ToArea(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
var
  sta: TSpellTriggerArea;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smArea then
      Exit;
  sta := TSpellTriggerArea(FTriggerTarget);
  sta.FPosition := ToPosition;
  sta.FRadius := radius;

  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition);
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.ExecuteSpell_ToAreaAlly(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
var
  sta: TSpellTriggerArea;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smAreaAlly then
      Exit;
  sta := TSpellTriggerArea(FTriggerTarget);
  sta.FPosition := ToPosition;
  sta.FRadius := radius;

  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition)
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.ExecuteSpell_ToAreaAllyOrSelf(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
var
  sta: TSpellTriggerArea;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smAreaAllyOrSelf then
      Exit;
  sta := TSpellTriggerArea(FTriggerTarget);
  sta.FPosition := ToPosition;
  sta.FRadius := radius;

  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition);
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.ExecuteSpell_ToAreaNeutrally(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
var
  sta: TSpellTriggerArea;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smAreaNeutrally then
      Exit;
  sta := TSpellTriggerArea(FTriggerTarget);
  sta.FPosition := ToPosition;
  sta.FRadius := radius;

  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition);
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.ExecuteSpell_ToAreaAntagonize(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
var
  sta: TSpellTriggerArea;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smAreaAntagonize then
      Exit;
  sta := TSpellTriggerArea(FTriggerTarget);
  sta.FPosition := ToPosition;
  sta.FRadius := radius;

  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition);
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.ExecuteSpell_ToAreaSelf(Action: TBioActionBase; ToPosition: TVec2; radius: TGeoFloat): Boolean;
var
  sta: TSpellTriggerArea;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smAreaSelf then
      Exit;
  sta := TSpellTriggerArea(FTriggerTarget);
  sta.FPosition := ToPosition;
  sta.FRadius := radius;

  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition);
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.ExecuteSpell_ToPoint(Action: TBioActionBase; ToPosition: TVec2): Boolean;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smPoint then
      Exit;
  TSpellTriggerPoint(FTriggerTarget).Target := ToPosition;
  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition);
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.ExecuteSpell_ToDirectionPoint(Action: TBioActionBase; ToPosition: TVec2): Boolean;
begin
  Result := False;
  if not SpellCanExecuteOfTime then
      Exit;
  if not SpellCanExecuteOfCost then
      Exit;
  if Mode <> smDirectionPoint then
      Exit;
  TSpellTriggerPoint(FTriggerTarget).Target := ToPosition;
  InstallActionHook(Action);
  Result := True;

  if SpellCanExecuteOfPos(ToPosition) then
    begin
      if Owner.GetBio2DestAngleDistance(ToPosition) <= FLaunchAngle then
          PostSpellPreprocessNotify(bsanMovementDone)
      else
          Owner.RequestStopRollAngleTo(ToPosition);
    end
  else
      Owner.RequestMovementTo(ToPosition);
end;

function TSpellBase.GetAreaSpellDestBioList(ToPosition: TVec2; radius: TGeoFloat; OutList: TCoreClassListForObj): Integer;
var
  i: Integer;
begin
  TBattleRegion(Owner.OwnerRegion).GetRadiusBioList(ToPosition, radius, OutList);

  case FMode of
    // bio target object
    smAreaAlly:
      begin
        i := 0;
        while i < OutList.Count do
          begin
            case Owner.GetRelation(TBioBase(OutList[i])) of
              rtAlly:
                inc(i);
              else
                OutList.Delete(i);
            end;
          end;
      end;
    smAreaAllyOrSelf:
      begin
        i := 0;
        while i < OutList.Count do
          begin
            case Owner.GetRelation(TBioBase(OutList[i])) of
              rtPlayer, rtAlly:
                inc(i);
              else
                OutList.Delete(i);
            end;
          end;
      end;
    smAreaNeutrally:
      begin
        i := 0;
        while i < OutList.Count do
          begin
            case Owner.GetRelation(TBioBase(OutList[i])) of
              rtNeutrally:
                inc(i);
              else
                OutList.Delete(i);
            end;
          end;
      end;
    smAreaAntagonize:
      begin
        i := 0;
        while i < OutList.Count do
          begin
            case Owner.GetRelation(TBioBase(OutList[i])) of
              rtNeutrally:
                inc(i);
              else
                OutList.Delete(i);
            end;
          end;
      end;
    smAreaSelf:
      begin
        i := 0;
        while i < OutList.Count do
          begin
            case Owner.GetRelation(TBioBase(OutList[i])) of
              rtPlayer:
                inc(i);
              else
                OutList.Delete(i);
            end;
          end;
      end;
  end;
  Result := OutList.Count;
end;

procedure TSpellBase.PostSpellExecuteNotify;
begin
  if FCurrentBioAction <> nil then
      PostSpellPreprocessNotify(bsanAnimationTrigger);
end;

procedure TSpellBase.PostSpellExecuteOverNotify;
begin
  if FCurrentBioAction <> nil then
      PostSpellPreprocessNotify(bsanAnimationOver);
end;

procedure TSpellBase.PostSpellBreak;
begin
  if FCurrentBioAction <> nil then
      PostSpellPreprocessNotify(bsanBreak);
end;

procedure TSpellBase.PostSpellEnd;
begin
  if FCurrentBioAction <> nil then
      PostSpellPreprocessNotify(bsanEnd);
end;

procedure TSpellBase.PostSpellPreprocessNotify(Notify: TRULEBioSpellActionMessage);
var
  v: TVec2;
begin
  Assert(FCurrentBioAction <> nil);
  case Notify of
    bsanMovementIng:
      begin
        if FTriggerTarget is TSpellTriggerObject then
          begin
            if TSpellTriggerObject(FTriggerTarget).Target is TBioBase then
              begin
                if SpellCanExecuteOfBio(TSpellTriggerObject(FTriggerTarget).Target, 0.9) then
                  begin
                    Owner.MovementStop;

                    v := TSpellTriggerObject(FTriggerTarget).Target.Position;
                    if Owner.GetBio2DestAngleDistance(v) > FLaunchAngle then
                        Owner.RequestStopRollAngleTo(v)
                    else
                        PostSpellPreprocessNotify(bsanMovementDone);
                  end;
              end
            else
              begin
                Owner.MovementStop;
                PostSpellPreprocessNotify(bsanMovementDone);
              end;
          end
        else if FTriggerTarget is TSpellTriggerArea then
          begin
            v := TSpellTriggerArea(FTriggerTarget).Position;
            if SpellCanExecuteOfPos(v, 1.0) then
              begin
                Owner.MovementStop;
                if Owner.GetBio2DestAngleDistance(v) > FLaunchAngle then
                    Owner.RequestStopRollAngleTo(v)
                else
                    PostSpellPreprocessNotify(bsanMovementDone);
              end;
          end
        else if FTriggerTarget is TSpellTriggerPoint then
          begin
            v := TSpellTriggerPoint(FTriggerTarget).Target;

            if SpellCanExecuteOfPos(v, 0.9) then
              begin
                Owner.MovementStop;
                if Owner.GetBio2DestAngleDistance(v) > FLaunchAngle then
                    Owner.RequestStopRollAngleTo(v)
                else
                    PostSpellPreprocessNotify(bsanMovementDone);
              end;
          end
        else
            Assert(False, 'Post movementIng');
      end;
    bsanStopRollAngleIng:
      begin
        if FTriggerTarget is TSpellTriggerObject then
          begin
            if TSpellTriggerObject(FTriggerTarget).Target is TBioBase then
              begin
                v := TSpellTriggerObject(FTriggerTarget).Target.Position;
                if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle * 0.9 then
                  begin
                    if SpellCanExecuteOfBio(TSpellTriggerObject(FTriggerTarget).Target, 0.9) then
                      begin
                        Owner.MovementStop;
                        PostSpellPreprocessNotify(bsanMovementDone);
                      end
                    else
                        Owner.RequestMovementTo(v);
                  end;
              end
            else
              begin
                Owner.MovementStop;
                PostSpellPreprocessNotify(bsanMovementDone);
              end;
          end
        else if FTriggerTarget is TSpellTriggerArea then
          begin
            v := TSpellTriggerArea(FTriggerTarget).Position;
            if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle * 0.9 then
              begin
                if SpellCanExecuteOfPos(v, 1.0) then
                  begin
                    Owner.MovementStop;
                    PostSpellPreprocessNotify(bsanMovementDone);
                  end
                else
                    Owner.RequestMovementTo(v);
              end;
          end
        else if FTriggerTarget is TSpellTriggerPoint then
          begin
            v := TSpellTriggerPoint(FTriggerTarget).Target;
            if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle * 0.9 then
              begin
                if SpellCanExecuteOfPos(v, 0.9) then
                  begin
                    Owner.MovementStop;
                    PostSpellPreprocessNotify(bsanMovementDone);
                  end
                else
                    Owner.RequestMovementTo(v);
              end;
          end
        else
            Assert(False, 'Post rollAngle');
      end;
    bsanMovementOver:
      begin
        if FTriggerTarget is TSpellTriggerObject then
          begin
            if TSpellTriggerObject(FTriggerTarget).Target is TBioBase then
              begin
                v := TSpellTriggerObject(FTriggerTarget).Target.Position;
                Owner.RequestMovementTo(v);
              end
            else
              begin
                Owner.MovementStop;
                PostSpellPreprocessNotify(bsanMovementDone);
              end;
          end
        else if FTriggerTarget is TSpellTriggerArea then
          begin
            v := TSpellTriggerArea(FTriggerTarget).Position;
            Owner.RequestMovementTo(v);
          end
        else if FTriggerTarget is TSpellTriggerPoint then
          begin
            v := TSpellTriggerPoint(FTriggerTarget).Target;
            Owner.RequestMovementTo(v);
          end
        else
            Assert(False, 'movement over');
      end;
    bsanMovementDone:
      begin
        // process cost
        DoProcessCost;

        DoSpellExecuteStart;

        if FTriggerTarget is TSpellTriggerObject then
          begin
            if TSpellTriggerObject(FTriggerTarget).Target is TBioBase then
              begin
                v := TSpellTriggerObject(FTriggerTarget).Target.Position;
              end
            else
              begin
                PostSpellPreprocessNotify(bsanEnd);
                Exit;
              end;
          end
        else if FTriggerTarget is TSpellTriggerArea then
          begin
            v := TSpellTriggerArea(FTriggerTarget).Position;
          end
        else if FTriggerTarget is TSpellTriggerPoint then
          begin
            v := TSpellTriggerPoint(FTriggerTarget).Target;
          end
        else
          begin
            PostSpellPreprocessNotify(bsanEnd);
            Exit;
          end;
      end;
    bsanAnimationTrigger:
      begin
        FLastExecuteTime := 0;
        FCurrentCoolDownTime := 0;
        DoSpellExecute;
        FActionActived := True;
      end;
    bsanAnimationOver:
      begin
        if FLoopMode then
          begin
            if SpellCanExecuteOfTime and SpellCanExecuteOfCost then
              begin
                DoSpellExecuteOver;
                if FTriggerTarget is TSpellTriggerObject then
                  begin
                    if TSpellTriggerObject(FTriggerTarget).Target is TBioBase then
                      begin
                        v := TSpellTriggerObject(FTriggerTarget).Target.Position;
                        if SpellCanExecuteOfBio(TSpellTriggerObject(FTriggerTarget).Target, 0.9) then
                          begin
                            if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle then
                                PostSpellPreprocessNotify(bsanMovementDone)
                            else
                                Owner.RequestStopRollAngleTo(v);
                          end
                        else
                            PostSpellPreprocessNotify(bsanMovementOver);
                      end
                    else
                      begin
                        PostSpellPreprocessNotify(bsanEnd);
                      end;
                  end
                else if FTriggerTarget is TSpellTriggerArea then
                  begin
                    v := TSpellTriggerArea(FTriggerTarget).Position;
                    if SpellCanExecuteOfPos(v, 1.0) then
                      begin
                        if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle then
                            PostSpellPreprocessNotify(bsanMovementDone)
                        else
                            Owner.RequestStopRollAngleTo(v);
                      end
                    else
                        PostSpellPreprocessNotify(bsanMovementOver);
                  end
                else if FTriggerTarget is TSpellTriggerPoint then
                  begin
                    v := TSpellTriggerPoint(FTriggerTarget).Target;
                    if SpellCanExecuteOfPos(v, 0.9) then
                      begin
                        if Owner.GetBio2DestAngleDistance(v) <= FLaunchAngle then
                            PostSpellPreprocessNotify(bsanMovementDone)
                        else
                            Owner.RequestStopRollAngleTo(v);
                      end
                    else
                        PostSpellPreprocessNotify(bsanMovementOver);
                  end
                else
                    PostSpellPreprocessNotify(bsanMovementDone);
              end
            else
              begin
                FLoopModeWait := True;
              end;
          end
        else
          begin
            DoSpellExecuteOver;
            FCurrentBioAction.ExecuteDone;
          end;
      end;
    bsanBreak:
      begin
        FCurrentBioAction.ExecuteBreak;
      end;
    bsanEnd:
      begin
        FCurrentBioAction.ExecuteDone;
      end;
    else
      Assert(False);
  end;
end;

procedure TSpellBase.DelayPostSpellExecuteNotify(Delay: Double);
begin
  DelayPostSpellPreprocessNotify(Delay, bsanAnimationTrigger);
end;

procedure TSpellBase.DelayPostSpellExecuteOverNotify(Delay: Double);
begin
  DelayPostSpellPreprocessNotify(Delay, bsanAnimationOver);
end;

procedure TSpellBase.DelayPostSpellBreak(Delay: Double);
begin
  DelayPostSpellPreprocessNotify(Delay, bsanBreak);
end;

procedure TSpellBase.DelayPostSpellEnd(Delay: Double);
begin
  DelayPostSpellPreprocessNotify(Delay, bsanEnd);
end;

procedure TSpellBase.DelayPostSpellPreprocessNotify(Delay: Double; Notify: TRULEBioSpellActionMessage);
begin
  AddDelayActionMsg(Delay, Notify);
end;

function TSpellBase.SpellIsReadyOk: Boolean;
begin
  Result := (FCurrentBioAction = nil) and SpellCanExecuteOfTime and SpellCanExecuteOfCost;
end;

function TSpellBase.SpellIsProcess: Boolean;
begin
  Result := FCurrentBioAction <> nil;
end;

function TSpellBase.SpellCanExecuteOfPos(ToPosition: TVec2; ARatio: TGeoFloat): Boolean;
var
  Bio: TBioBase;
begin
  case FMode of
    // bio target object
    smBio, smBioAlly, smBioAllyOrSelf, smBioNeutrally, smBioAntagonize, smBioSelf, smArea, smAreaAlly, smAreaAllyOrSelf, smAreaNeutrally, smAreaAntagonize, smAreaSelf, smPoint:
      begin
        Bio := Owner;
        Result := PointDistance(Bio.Position, ToPosition) * ARatio <= FLaunchDistance;
      end;
    else
      Result := True;
  end;
end;

function TSpellBase.SpellCanExecuteOfBio(ABio: TBioBase; ARatio: TGeoFloat): Boolean;
var
  SelfBio, DestBio: TBioBase;
begin
  case FMode of
    // bio target object
    smBio, smBioAlly, smBioAllyOrSelf, smBioNeutrally, smBioAntagonize, smBioSelf:
      begin
        SelfBio := Owner;
        DestBio := ABio;
        if FLaunchDistance <= 0 then
            Result := SelfBio.IsCollision(DestBio)
        else
            Result := PointDistance(SelfBio.Position, DestBio.Position) * ARatio <= FLaunchDistance;
      end;
    else
      Result := True;
  end;
end;

function TSpellBase.SpellCanExecuteOfTime: Boolean;
begin
  case FMode of
    smDirect,
      smBio, smBioAlly, smBioAllyOrSelf, smBioNeutrally, smBioAntagonize, smBioSelf,
      smArea, smAreaAlly, smAreaAllyOrSelf, smAreaNeutrally, smAreaAntagonize, smAreaSelf,
      smPoint, smDirectionPoint:
      Result := (FCoolDownTime <= 0) or ((FCurrentCoolDownTime - FLastExecuteTime) >= FCoolDownTime);
    else
      Result := True;
  end;
end;

function TSpellBase.GetCooldownWaitTime: Double;
begin
  case FMode of
    smDirect,
      smBio, smBioAlly, smBioAllyOrSelf, smBioNeutrally, smBioAntagonize, smBioSelf,
      smArea, smAreaAlly, smAreaAllyOrSelf, smAreaNeutrally, smAreaAntagonize, smAreaSelf,
      smPoint, smDirectionPoint:
      begin
        if (FCoolDownTime <= 0) or ((FCurrentCoolDownTime - FLastExecuteTime) >= FCoolDownTime) then
            Result := 0
        else
            Result := FCoolDownTime - (FCurrentCoolDownTime - FLastExecuteTime);
      end;
    else
      Result := 0;
  end;
end;

function TSpellBase.SpellCanExecuteOfCost: Boolean;
begin
  Result := True;
end;

procedure TSpellBase.DoProcessCost;
begin
end;

constructor TSpellEffect.Create;
begin
  inherited Create;
  TriBeforeEffect := '';
  TriEffect := '';
  TriAfterEffect := '';
  TriSound := '';
  Bullet := '';
  BulletTailEffect := '';
  CollisionBeforeEffect := '';
  CollisionEffect := '';
  CollisionAfterEffect := '';
  CollisionSound := '';
end;

destructor TSpellEffect.Destroy;
begin
  inherited Destroy;
end;

procedure TSpellEffect.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.LoadFromStream(stream);

  TriBeforeEffect := df.Reader.ReadString;
  TriEffect := df.Reader.ReadString;
  TriAfterEffect := df.Reader.ReadString;
  TriSound := df.Reader.ReadString;
  Bullet := df.Reader.ReadString;
  BulletTailEffect := df.Reader.ReadString;
  CollisionBeforeEffect := df.Reader.ReadString;
  CollisionEffect := df.Reader.ReadString;
  CollisionAfterEffect := df.Reader.ReadString;
  CollisionSound := df.Reader.ReadString;

  DisposeObject(df);
end;

procedure TSpellEffect.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;

  df.WriteString(TriBeforeEffect);
  df.WriteString(TriEffect);
  df.WriteString(TriAfterEffect);
  df.WriteString(TriSound);
  df.WriteString(Bullet);
  df.WriteString(BulletTailEffect);
  df.WriteString(CollisionBeforeEffect);
  df.WriteString(CollisionEffect);
  df.WriteString(CollisionAfterEffect);
  df.WriteString(CollisionSound);

  df.SaveToStream(stream);
  DisposeObject(df);
end;

initialization

InitSpellRegistedContainer;

finalization

FreeSpellRegistedContainer;

end. 
 
 
