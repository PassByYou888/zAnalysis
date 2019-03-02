{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit BioBase;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses,
  BaseRULEClasses,
  ListEngine,
  UnicodeMixedLib, Geometry2DUnit,
  zNavigationPass, CampRelationBase, MovementEngine;

type
  TBioBase = class;

  TBioActions    = class;
  TBioActionBase = class;

  TBioActionClass = class of TBioActionBase;

  IBioActionBaseNotifyIntf = interface
    procedure ActionDestroy(Action: TBioActionBase);
    function ActionExecuteBeforeCheck(Action: TBioActionBase): Boolean;
    function ActionExecuteBefore(Action: TBioActionBase): Boolean;
    procedure ActionProgress(Action: TBioActionBase; deltaTime: Double);
    procedure ActionExecuteBreak(Action: TBioActionBase);
    function ActionExecuteAfterCheck(Action: TBioActionBase): Boolean;
    function ActionExecuteAfter(Action: TBioActionBase): Boolean;
    procedure ActionExecuteAfterSuccess(Action: TBioActionBase);
    procedure ActionExecuteAfterFailed(Action: TBioActionBase);
  end;

  TBioActionExecuteState = (
    baesBeforeCheck,
    baesBeforeCheckFailed,
    baesBeforeCheckSuccess,
    baesBefore,
    baesBeforeFailed,
    baesBeforeSuccess,
    baesProgress,
    baesBreak,
    baesAfterCheck,
    baesAfterCheckFailed,
    baesAfterCheckSuccess,
    baesAfter,
    baesAfterFailed,
    baesAfterSuccess,
    baesEnd);

  TBioActionBase = class(TCoreClassObject)
  private
    FOwner: TBioActions;

    FActionTriggerObject: TCoreClassObject;
    FObjectValues: THashObjectList;
    FVariantValues: THashVariantList;

    FExecuteState: TBioActionExecuteState;
    FBreaked: Boolean;

    FNotifyIntf: IBioActionBaseNotifyIntf;
  private
    function GeTCoreClassObjectValues: THashObjectList;
    function GetVariantValues: THashVariantList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure FirstPlay; virtual;

    procedure ForceEnd; virtual;
    procedure ExecuteDone; virtual;
    procedure ExecuteBreak; virtual;

    function ExecuteIsEnd: Boolean; virtual;
    function ExecuteIsBreak: Boolean; virtual;

    procedure DoDestroy; virtual;
    function DoExecuteBeforeCheck: Boolean; virtual;
    function DoExecuteBefore: Boolean; virtual;
    procedure doProgress(deltaTime: Double); virtual;
    procedure DoExecuteBreak; virtual;
    function DoExecuteAfterCheck: Boolean; virtual;
    function DoExecuteAfter: Boolean; virtual;
    procedure DoExecuteAfterSuccess; virtual;
    procedure DoExecuteAfterFailed; virtual;

    property Owner: TBioActions read FOwner;

    property ExecuteState: TBioActionExecuteState read FExecuteState;
    property ActionTriggerObject: TCoreClassObject read FActionTriggerObject write FActionTriggerObject;

    property ObjectValues: THashObjectList read GeTCoreClassObjectValues;
    property VariantValues: THashVariantList read GetVariantValues;

    // step scheduler and manager
    procedure Progress(deltaTime: Double); virtual;
    property NotifyIntf: IBioActionBaseNotifyIntf read FNotifyIntf write FNotifyIntf;
  end;

  IBioActionsNotifyIntf = interface
    procedure SwitchAction(Actions: TBioActions; ACurrentAction, ANewAction: TBioActionBase);
    procedure AllActionPlayOver(Actions: TBioActions);
  end;

  TBioActions = class(TCoreClassObject)
  private
    FActionList: TCoreClassListForObj;
    FProcessPosition: Integer;
    FPlaying: Boolean;

    // TBioBase
    FOwner: TBioBase;

    FNotifyIntf: IBioActionsNotifyIntf;
  protected
    procedure DoSwitchAction(ACurrentAction, ANewAction: TBioActionBase);
    procedure DoAllActionPlayOver;

    function NextAction: Boolean;
    procedure ResetAction;

    function GetAction(Value: Integer): TBioActionBase;
  public
    constructor Create(AOwner: TBioBase);
    destructor Destroy; override;

    // action manager
    procedure Clear;
    function NewAction(AActionIntfClass: TBioActionClass): TBioActionBase; overload;
    function NewActionInsert(index: Integer; AActionIntfClass: TBioActionClass): TBioActionBase; overload;
    function NewActionInsertAfter(Value: TBioActionBase; AActionIntfClass: TBioActionClass): TBioActionBase; overload;
    function NewActionInsertBefore(Value: TBioActionBase; AActionIntfClass: TBioActionClass): TBioActionBase; overload;
    function ActionIndexOf(Value: TBioActionBase): Integer;
    function Count: Integer;
    function Delete(idx: Integer): Boolean;
    property Items[idx: Integer]: TBioActionBase read GetAction; default;

    // action scheduler
    function CurrentAction: TBioActionBase;
    function IsLastAction: Boolean;
    function PlayAction: Boolean;

    // TBioBase
    property Owner: TBioBase read FOwner;

    function Progress(deltaTime: Double): Boolean;

    property NotifyIntf: IBioActionsNotifyIntf read FNotifyIntf write FNotifyIntf;

    property ActionList: TCoreClassListForObj read FActionList;
  end;

  TBioBase = class(TCoreClassObject)
  protected
    FID: Integer;
    FOwnerRegion: TRegionBase;
    FDestroyBackcallList: TBackcalls;
    FDieBackcallList: TBackcalls;
    FResuscitationBackcallList: TBackcalls;
    FNavBio: TNavBio;
    FActions: TBioActions;

    function GetRadius: TGeoFloat;
    procedure SetRadius(const Value: TGeoFloat);

    function GetPosition: TVec2;
    procedure SetPosition(const Value: TVec2);

    function GetRollAngle: TGeoFloat;
    procedure SetRollAngle(const Value: TGeoFloat);

    function GetCampID: U_String;
    function GetPlayerID: U_String;
  public
    constructor Create(AOwnerRegion: TRegionBase; APosition: TVec2; AAngle, ARadius: TGeoFloat);
    destructor Destroy; override;

    // free call
    procedure RegisterFreeBackcall(AFlagObject: TCoreClassObject; ANotifyMethod: TBackcallNotifyMethod);
    procedure UnRegisterFreeBackcall(AFlagObject: TCoreClassObject);

    // die call
    procedure RegisterDieBackcall(AFlagObject: TObject; ANotifyMethod: TBackcallNotifyMethod);
    procedure UnRegisterDieBackcall(AFlagObject: TObject);

    // resuscitation call
    procedure RegisterResuscitationBackcall(AFlagObject: TObject; ANotifyMethod: TBackcallNotifyMethod);
    procedure UnRegisterResuscitationBackcall(AFlagObject: TObject);

    property ID: Integer read FID write FID;
    property OwnerRegion: TRegionBase read FOwnerRegion;

    procedure Progress(deltaTime: Double); virtual;

    procedure Delete;

    function IsDie: Boolean;

    function GetRelation(Bio: TBioBase): TRelationType;
    function GetBio2PositionAngle(DestPos: TVec2): TGeoFloat;
    function GetBio2DestAngleDistance(DestPos: TVec2): TGeoFloat;
    function IsCollision(DestBio: TBioBase): Boolean;

    function RequestMovementTo(ToPosition: TVec2): Boolean;
    procedure RequestStopRollAngleTo(ToPosition: TVec2);
    procedure MovementStop;

    property radius: TGeoFloat read GetRadius write SetRadius;
    property Position: TVec2 read GetPosition write SetPosition;
    property RollAngle: TGeoFloat read GetRollAngle write SetRollAngle;
    property CampID: U_String read GetCampID;
    property PlayerID: U_String read GetPlayerID;

    // external call of action
    procedure BreakAction;
    procedure Execute_MovementToPos(ToPosition: TVec2; Queue: Boolean);
    procedure Execute_TracertBio(DestBio: TBioBase; Queue: Boolean);

    // ai

  public
    // 种族

    // 职业

    // 特长

    // 专精

    // 技�?

    // 可穿戴装备：头，肩，身体，手腕，腰带，手套，脚，戒指，护身符，箭，矢，弹丸，斗篷

    // 物品

    // 历史战绩

    // 阵营
  end;

implementation

uses Variants, CoreRULE, WorldBase, BioActionImp, Geometry3DUnit;

function TBioActionBase.GeTCoreClassObjectValues: THashObjectList;
begin
  if FObjectValues = nil then
      FObjectValues := THashObjectList.Create(False);
  Result := FObjectValues;
end;

function TBioActionBase.GetVariantValues: THashVariantList;
begin
  if FVariantValues = nil then
      FVariantValues := THashVariantList.Create;
  Result := FVariantValues;
end;

constructor TBioActionBase.Create;
begin
  inherited Create;

  FOwner := nil;

  FActionTriggerObject := nil;

  FObjectValues := nil;
  FVariantValues := nil;

  FExecuteState := baesBeforeCheck;
  FBreaked := False;

  FNotifyIntf := nil;
end;

destructor TBioActionBase.Destroy;
begin
  DoDestroy;

  if FObjectValues <> nil then
      DisposeObject(FObjectValues);
  if FVariantValues <> nil then
      DisposeObject(FVariantValues);

  inherited Destroy;
end;

procedure TBioActionBase.FirstPlay;
begin

end;

procedure TBioActionBase.ForceEnd;
begin
  FExecuteState := baesEnd;
end;

procedure TBioActionBase.ExecuteDone;
begin
  // change executestate as after
  if (FExecuteState = baesProgress) then
      FExecuteState := baesAfterCheck;
end;

procedure TBioActionBase.ExecuteBreak;
begin
  // change executestate as break
  FExecuteState := baesBreak;
  FBreaked := True;
  DoExecuteBreak;
end;

function TBioActionBase.ExecuteIsEnd: Boolean;
begin
  Result := FExecuteState = baesEnd;
end;

function TBioActionBase.ExecuteIsBreak: Boolean;
begin
  Result := FBreaked;
end;

procedure TBioActionBase.DoDestroy;
begin
  if Assigned(FNotifyIntf) then
      FNotifyIntf.ActionDestroy(Self);
end;

function TBioActionBase.DoExecuteBeforeCheck: Boolean;
begin
  Result := False;
  if Assigned(FNotifyIntf) then
      Result := FNotifyIntf.ActionExecuteBeforeCheck(Self);
end;

function TBioActionBase.DoExecuteBefore: Boolean;
begin
  Result := False;
  if Assigned(FNotifyIntf) then
      Result := FNotifyIntf.ActionExecuteBefore(Self);
end;

procedure TBioActionBase.doProgress(deltaTime: Double);
begin
  if Assigned(FNotifyIntf) then
      FNotifyIntf.ActionProgress(Self, deltaTime);
end;

procedure TBioActionBase.DoExecuteBreak;
begin
  if Assigned(FNotifyIntf) then
      FNotifyIntf.ActionExecuteBreak(Self);
end;

function TBioActionBase.DoExecuteAfterCheck: Boolean;
begin
  Result := False;
  if Assigned(FNotifyIntf) then
      Result := FNotifyIntf.ActionExecuteAfterCheck(Self);
end;

function TBioActionBase.DoExecuteAfter: Boolean;
begin
  Result := False;
  if Assigned(FNotifyIntf) then
      Result := FNotifyIntf.ActionExecuteAfter(Self);
end;

procedure TBioActionBase.DoExecuteAfterSuccess;
begin
  if Assigned(FNotifyIntf) then
      FNotifyIntf.ActionExecuteAfterSuccess(Self);
end;

procedure TBioActionBase.DoExecuteAfterFailed;
begin
  if Assigned(FNotifyIntf) then
      FNotifyIntf.ActionExecuteAfterFailed(Self);
end;

procedure TBioActionBase.Progress(deltaTime: Double);
var
  BackValue: Boolean;
begin
  // scheduler and manager step
  if (FExecuteState = baesEnd) then
      Exit;

  case FExecuteState of
    baesBeforeCheck:
      begin
        BackValue := DoExecuteBeforeCheck;
        if BackValue then
            FExecuteState := baesBeforeCheckSuccess
        else
            FExecuteState := baesBeforeCheckFailed;
      end;
    baesBeforeCheckFailed:
      begin
        // state over
        FExecuteState := baesEnd;
      end;
    baesBeforeCheckSuccess:
      begin
        FExecuteState := baesBefore;
      end;
    baesBefore:
      begin
        BackValue := DoExecuteBefore;
        if BackValue then
            FExecuteState := baesBeforeSuccess
        else
            FExecuteState := baesBeforeFailed;
      end;
    baesBeforeFailed:
      begin
        // state over
        FExecuteState := baesEnd;
      end;
    baesBeforeSuccess:
      begin
        // state over
        FExecuteState := baesProgress;
      end;
    baesProgress:
      begin
        doProgress(deltaTime);
      end;
    baesBreak:
      begin
        FExecuteState := baesEnd;
      end;
    baesAfterCheck:
      begin
        BackValue := DoExecuteAfterCheck;
        if BackValue then
            FExecuteState := baesAfterCheckSuccess
        else
            FExecuteState := baesAfterCheckFailed;
      end;
    baesAfterCheckFailed:
      begin
        // state over
        FExecuteState := baesEnd;
      end;
    baesAfterCheckSuccess:
      begin
        FExecuteState := baesAfter;
      end;
    baesAfter:
      begin
        BackValue := DoExecuteAfter;
        if BackValue then
            FExecuteState := baesAfterSuccess
        else
            FExecuteState := baesAfterFailed;
      end;
    baesAfterFailed:
      begin
        FExecuteState := baesEnd;
        DoExecuteAfterFailed;
      end;
    baesAfterSuccess:
      begin
        FExecuteState := baesEnd;
        DoExecuteAfterSuccess;
      end;
    else
      begin
        // raise message
        raise CoreClassException.Create('invailied Action Execute State');
      end;
  end;
end;

procedure TBioActions.DoSwitchAction(ACurrentAction, ANewAction: TBioActionBase);
begin
  if Assigned(FNotifyIntf) then
      FNotifyIntf.SwitchAction(Self, ACurrentAction, ANewAction);
end;

procedure TBioActions.DoAllActionPlayOver;
begin
  if Assigned(FNotifyIntf) then
      FNotifyIntf.AllActionPlayOver(Self);
end;

function TBioActions.NextAction: Boolean;
begin
  if (FProcessPosition < 0) and (FActionList.Count > 0) then
      FProcessPosition := 0
  else if (FProcessPosition < FActionList.Count - 1) then
    begin
      FProcessPosition := FProcessPosition + 1;
      CurrentAction.FirstPlay;
      DoSwitchAction(nil, CurrentAction);
    end
  else
    begin
      FProcessPosition := -1;
      DoAllActionPlayOver;
    end;
  Result := FProcessPosition >= 0;
end;

procedure TBioActions.ResetAction;
begin
  // reset all action play state and clear
  FPlaying := False;
  FProcessPosition := -1;

  Clear;
end;

function TBioActions.GetAction(Value: Integer): TBioActionBase;
begin
  Result := TBioActionBase(FActionList[Value]);
end;

constructor TBioActions.Create(AOwner: TBioBase);
begin
  inherited Create;
  FActionList := TCoreClassListForObj.Create;
  FProcessPosition := -1;
  FPlaying := False;
  FOwner := AOwner;
  FNotifyIntf := nil;
end;

destructor TBioActions.Destroy;
begin
  Clear;
  DisposeObject(FActionList);
  inherited Destroy;
end;

procedure TBioActions.Clear;
var
  Action: TBioActionBase;
begin
  while FActionList.Count > 0 do
    begin
      Action := TBioActionBase(FActionList[0]);
      FActionList.Delete(0);
      try
          DisposeObject(Action);
      except
      end;
    end;
  FProcessPosition := -1;
  FPlaying := False;
end;

function TBioActions.NewAction(AActionIntfClass: TBioActionClass): TBioActionBase;
begin
  Result := AActionIntfClass.Create;
  Result.FOwner := Self;
  FActionList.Add(Result);
end;

function TBioActions.NewActionInsert(index: Integer; AActionIntfClass: TBioActionClass): TBioActionBase;
begin
  Result := AActionIntfClass.Create;
  Result.FOwner := Self;
  FActionList.Insert(index, Result);
end;

function TBioActions.NewActionInsertAfter(Value: TBioActionBase; AActionIntfClass: TBioActionClass): TBioActionBase;
begin
  Result := NewActionInsert(ActionIndexOf(Value), AActionIntfClass);
end;

function TBioActions.NewActionInsertBefore(Value: TBioActionBase; AActionIntfClass: TBioActionClass): TBioActionBase;
var
  idx: Integer;
begin
  idx := ActionIndexOf(Value);
  if idx = Count - 1 then
      Result := NewAction(AActionIntfClass)
  else
      Result := NewActionInsert(idx + 1, AActionIntfClass);
end;

function TBioActions.ActionIndexOf(Value: TBioActionBase): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if FActionList[i] = Value then
      begin
        Result := i;
        Exit;
      end;
end;

function TBioActions.Count: Integer;
begin
  Result := FActionList.Count;
end;

function TBioActions.Delete(idx: Integer): Boolean;
var
  Action: TBioActionBase;
begin
  Result := (idx >= 0) and (idx < FActionList.Count);
  if Result then
    begin
      Action := TBioActionBase(FActionList[idx]);
      FActionList.Delete(idx);
      try
      except
          DisposeObject(Action);
      end;
    end;
end;

function TBioActions.CurrentAction: TBioActionBase;
begin
  if (FProcessPosition >= 0) and (FProcessPosition < FActionList.Count) then
    begin
      Result := TBioActionBase(FActionList[FProcessPosition]);
    end
  else
    begin
      Result := nil;
    end;
end;

function TBioActions.IsLastAction: Boolean;
begin
  Result := (FActionList.Count > 0) and (FProcessPosition = FActionList.Count - 1);
end;

function TBioActions.PlayAction: Boolean;
begin
  Result := False;
  if Count = 0 then
      Exit;
  if FProcessPosition <> -1 then
      Exit;
  if FPlaying then
      Exit;
  FPlaying := True;
  FProcessPosition := 0;
  CurrentAction.FirstPlay;
  DoSwitchAction(nil, CurrentAction);
  Result := True;
end;

function TBioActions.Progress(deltaTime: Double): Boolean;
var
  a: TBioActionBase;
begin
  Result := False;
  if not FPlaying then
      Exit;
  a := CurrentAction;
  if (a <> nil) then
    begin
      a.Progress(deltaTime);
      if a.ExecuteIsBreak then
        begin
          FPlaying := False;
          FProcessPosition := -1;
          Clear;
        end
      else if a.ExecuteIsEnd then
        begin
          if not NextAction then
              ResetAction;
        end;
    end;
end;

function TBioBase.GetRadius: TGeoFloat;
begin
  Result := FNavBio.radius;
end;

procedure TBioBase.SetRadius(const Value: TGeoFloat);
var
  pt: TVec2;
  a: TGeoFloat;
begin
  pt := Position;
  a := RollAngle;
  DisposeObject(FNavBio);
  FNavBio := TBattleRegion(FOwnerRegion).NavScene.AddBio(pt, a, Value);
end;

function TBioBase.GetPosition: TVec2;
begin
  Result := FNavBio.Position;
end;

procedure TBioBase.SetPosition(const Value: TVec2);
begin
  FNavBio.Position := Value;
end;

function TBioBase.GetRollAngle: TGeoFloat;
begin
  Result := FNavBio.RollAngle;
end;

procedure TBioBase.SetRollAngle(const Value: TGeoFloat);
begin
  FNavBio.RollAngle := Value;
end;

function TBioBase.GetCampID: U_String;
var
  b: TBio;
begin
  b := Self as TBio;
  Result := b.Owner.CampID;
end;

function TBioBase.GetPlayerID: U_String;
var
  b: TBio;
begin
  b := Self as TBio;
  Result := b.Owner.PlayerID;
end;

constructor TBioBase.Create(AOwnerRegion: TRegionBase; APosition: TVec2; AAngle, ARadius: TGeoFloat);
begin
  inherited Create;

  Assert(AOwnerRegion is TBattleRegion);

  FOwnerRegion := AOwnerRegion;
  FDestroyBackcallList := TBackcalls.Create;
  FDestroyBackcallList.Owner := Self;

  FDieBackcallList := TBackcalls.Create;
  FDieBackcallList.Owner := Self;

  FResuscitationBackcallList := TBackcalls.Create;
  FResuscitationBackcallList.Owner := Self;

  FNavBio := TBattleRegion(FOwnerRegion).NavScene.AddBio(APosition, AAngle, ARadius);
  FActions := TBioActions.Create(Self);

  TBattleRegion(FOwnerRegion).BioInRegion(Self as TBio);
end;

destructor TBioBase.Destroy;
begin
  if FOwnerRegion is TBattleRegion then
      TBattleRegion(FOwnerRegion).BioOutRegion(Self as TBio);

  FDestroyBackcallList.ExecuteBackcall(Self, Null, Null, Null);
  DisposeObject(FDestroyBackcallList);

  DisposeObject(FDieBackcallList);
  DisposeObject(FResuscitationBackcallList);

  DisposeObject(FNavBio);
  DisposeObject(FActions);
  inherited Destroy;
end;

procedure TBioBase.RegisterFreeBackcall(AFlagObject: TCoreClassObject; ANotifyMethod: TBackcallNotifyMethod);
begin
  FDestroyBackcallList.RegisterBackcall(AFlagObject, ANotifyMethod);
end;

procedure TBioBase.UnRegisterFreeBackcall(AFlagObject: TCoreClassObject);
begin
  FDestroyBackcallList.UnRegisterBackcall(AFlagObject);
end;

procedure TBioBase.RegisterDieBackcall(AFlagObject: TObject; ANotifyMethod: TBackcallNotifyMethod);
begin
  FDieBackcallList.RegisterBackcall(AFlagObject, ANotifyMethod);
end;

procedure TBioBase.UnRegisterDieBackcall(AFlagObject: TObject);
begin
  FDieBackcallList.UnRegisterBackcall(AFlagObject);
end;

procedure TBioBase.RegisterResuscitationBackcall(AFlagObject: TObject; ANotifyMethod: TBackcallNotifyMethod);
begin
  FResuscitationBackcallList.RegisterBackcall(AFlagObject, ANotifyMethod);
end;

procedure TBioBase.UnRegisterResuscitationBackcall(AFlagObject: TObject);
begin
  FResuscitationBackcallList.UnRegisterBackcall(AFlagObject);
end;

procedure TBioBase.Progress(deltaTime: Double);
begin
  if FOwnerRegion = nil then
      Exit;

  FActions.Progress(deltaTime);
end;

procedure TBioBase.Delete;
var
  b: TBio;
begin
  b := Self as TBio;
  b.BreakAction;

  if b.FOwnerRegion <> nil then
      TBattleRegion(b.FOwnerRegion).RemoveBio(b);;

  b.Owner.DeleteBio(b);

  DisposeObject(b);
end;

function TBioBase.IsDie: Boolean;
begin
  Result := False;
end;

function TBioBase.GetRelation(Bio: TBioBase): TRelationType;
begin
  Result := TBio(Self).Owner.Owner.BioRelation(TBio(Self), TBio(Bio));
end;

function TBioBase.GetBio2PositionAngle(DestPos: TVec2): TGeoFloat;
begin
  Result := CalcAngle(Position, DestPos);
end;

function TBioBase.GetBio2DestAngleDistance(DestPos: TVec2): TGeoFloat;
begin
  Result := AngleDistance(RollAngle, GetBio2PositionAngle(DestPos));
end;

function TBioBase.IsCollision(DestBio: TBioBase): Boolean;
begin
  Result := CircleCollision(Position, DestBio.Position, radius, DestBio.radius);
end;

function TBioBase.RequestMovementTo(ToPosition: TVec2): Boolean;
begin
  Result := FNavBio.MovementTo(ToPosition);
end;

procedure TBioBase.RequestStopRollAngleTo(ToPosition: TVec2);
begin
  FNavBio.DirectionTo(ToPosition);
end;

procedure TBioBase.MovementStop;
begin
  FNavBio.stop;
end;

procedure TBioBase.BreakAction;
begin
  if FActions.CurrentAction <> nil then
      FActions.CurrentAction.ExecuteBreak;
end;

procedure TBioBase.Execute_MovementToPos(ToPosition: TVec2; Queue: Boolean);
var
  a: TBioMovement_Action;
begin
  if IsDie then
      Exit;

  if not Queue then
    begin
      BreakAction;
      FActions.Clear;
    end;

  // step 1, create new movement action
  a := TBioMovement_Action(FActions.NewAction(TBioMovement_Action));
  a.DestPos := ToPosition;
  // step 2, execute action
  FActions.PlayAction;
end;

procedure TBioBase.Execute_TracertBio(DestBio: TBioBase; Queue: Boolean);
var
  a: TBioTracert_Action;
begin
  if IsDie then
      Exit;

  if Self = DestBio then
      Exit;

  if not Queue then
    begin
      BreakAction;
      FActions.Clear;
    end;

  // step 1, create new movement action
  a := TBioTracert_Action(FActions.NewAction(TBioTracert_Action));
  a.DestBio := DestBio;
  // step 2, execute action
  FActions.PlayAction;
end;

end. 
 
 
 
