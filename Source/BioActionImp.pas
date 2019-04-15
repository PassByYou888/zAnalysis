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
unit BioActionImp;

{$INCLUDE zDefine.inc}

interface

uses Geometry2DUnit, CoreClasses, PascalStrings,
  BioBase, CoreRULE, WorldBase, ListEngine;

type
  TBioMovement_Action = class(TBioActionBase)
  private
    FDestPos: TVec2;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure FirstPlay; override;
    procedure ExecuteBreak; override;
    procedure ForceEnd; override;

    property DestPos: TVec2 read FDestPos write FDestPos;
  end;

  TBioTracert_Action = class(TBioActionBase)
  private
    FDestBio: TBioBase;
    FLastCoord: TVec2;

    procedure DestBioFreeNotify(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
    procedure SetDestBio(const Value: TBioBase);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure FirstPlay; override;
    procedure ExecuteBreak; override;

    property DestBio: TBioBase read FDestBio write SetDestBio;

    procedure Progress(deltaTime: Double); override;
  end;

implementation


constructor TBioMovement_Action.Create;
begin
  inherited Create;
  FDestPos := NULLPoint;
end;

destructor TBioMovement_Action.Destroy;
begin
  inherited Destroy;
end;

procedure TBioMovement_Action.FirstPlay;
begin
  inherited FirstPlay;
  Owner.Owner.RequestMovementTo(DestPos);
  ActionTriggerObject := Owner.Owner;
end;

procedure TBioMovement_Action.ExecuteBreak;
begin
  inherited ExecuteBreak;
  Owner.Owner.MovementStop;
end;

procedure TBioMovement_Action.ForceEnd;
begin
  inherited ForceEnd;
end;

procedure TBioTracert_Action.DestBioFreeNotify(Sender: TBackcalls; TriggerObject: TCoreClassObject; Param1, Param2, Param3: Variant);
begin
  DestBio := nil;
  Owner.Owner.MovementStop;
  ForceEnd;
end;

procedure TBioTracert_Action.SetDestBio(const Value: TBioBase);
begin
  if FDestBio is TBioBase then
      FDestBio.UnRegisterFreeBackcall(Self);
  FDestBio := Value;
  if FDestBio is TBioBase then
    begin
{$IFDEF FPC}
      FDestBio.RegisterFreeBackcall(Self, @DestBioFreeNotify);
{$ELSE}
      FDestBio.RegisterFreeBackcall(Self, DestBioFreeNotify);
{$ENDIF}
      FLastCoord := FDestBio.Position;
    end
  else
    begin
      FLastCoord := NULLPoint;
    end;
end;

constructor TBioTracert_Action.Create;
begin
  inherited Create;
  FDestBio := nil;
  FLastCoord := NULLPoint;
end;

destructor TBioTracert_Action.Destroy;
begin
  DestBio := nil;
  inherited Destroy;
end;

procedure TBioTracert_Action.FirstPlay;
begin
  inherited FirstPlay;
  Owner.Owner.RequestMovementTo(DestBio.Position);
  ActionTriggerObject := Owner.Owner;
  FLastCoord := FDestBio.Position;
end;

procedure TBioTracert_Action.ExecuteBreak;
begin
  inherited ExecuteBreak;
  Owner.Owner.MovementStop;
end;

procedure TBioTracert_Action.Progress(deltaTime: Double);
var
  dr: TGeoFloat;
begin
  inherited Progress(deltaTime);
  if DestBio <> nil then
    begin
      dr := DestBio.radius;
      if not CircleCollision(DestBio.Position, FLastCoord, dr * 2, dr * 2) then
        begin
          // removement
          Owner.Owner.RequestMovementTo(DestBio.Position);
          FLastCoord := FDestBio.Position;
        end;
    end;
end;

end. 
 
