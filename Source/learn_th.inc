type
  TLearn_th = class(TCoreClassThread)
  public
    source                : TLearn;
    TrainDepth            : TLInt;
    OnStateC              : TLearnState_Call;
    OnStateM              : TLearnState_Method;
    {$IFNDEF FPC} OnStateP: TLearnState_Proc; {$ENDIF}
    Successed             : Boolean;

    procedure SyncResultState;
    procedure Execute; override;
    constructor Create;
    destructor Destroy; override;
  end;

procedure TLearn_th.SyncResultState;
begin
  source.FTrainThreadRuning := False;
  if Assigned(OnStateC) then
      OnStateC(source, Successed);
  if Assigned(OnStateM) then
      OnStateM(source, Successed);
  {$IFNDEF FPC}
  if Assigned(OnStateP) then
      OnStateP(source, Successed);
  {$ENDIF FPC}
end;

procedure TLearn_th.Execute;
begin
  if source <> nil then
      Successed := source.Train(TrainDepth)
  else
      Successed := False;

  {$IFDEF FPC}
  Synchronize(@SyncResultState);
  {$ELSE FPC}
  Synchronize(SyncResultState);
  {$ENDIF FPC}
end;

constructor TLearn_th.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  source := nil;
  TrainDepth := 2;
  OnStateC := nil;
  OnStateM := nil;
  {$IFNDEF FPC} OnStateP := nil; {$ENDIF}
  Successed := False;
end;

destructor TLearn_th.Destroy;
begin
  inherited Destroy;
end;

