{ ****************************************************************************** }
{ * machine Learn support, writen by QQ 600585@qq.com                          * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit Learn;

interface

uses CoreClasses, UnicodeMixedLib, PascalStrings, KDTree;

{$I ZDefine.inc}


type
  TLearnFloat      = TKDTree.TKDTree_VecType;
  PLearnFloat      = TKDTree.PKDTree_VecType;
  TLearnFloatArray = TKDTree.TKDTree_Vec;
  PLearnFloatArray = TKDTree.PKDTree_Vec;

  TLearnType = (ltKDT, ltLM, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltLMEnsemble, ltLBFGSEnsemble);

  TLearn = class;

  TLearnState_Call               = procedure(const source: TLearn; const state: Boolean);
  TLearnState_Method             = procedure(const source: TLearn; const state: Boolean) of object;
  {$IFNDEF FPC} TLearnState_Proc = reference to procedure(const source: TLearn; const state: Boolean); {$ENDIF}

  TLearn = class(TCoreClassInterfacedObject)
  private
    FEnabledRandomNumber: Boolean;
    FInLen, FOutLen     : NativeInt;
    FDataIn, FDataOut   : TCoreClassList;
    FLearnType          : TLearnType;
    FLearnData          : Pointer;
    FInfo               : string;
    FIsTraining         : Boolean;
    FThreadRuning       : Boolean;

    procedure KDInput(const IndexFor: NativeInt; var source: TKDTree.TKDTree_Source; const Data: Pointer);
  public
    constructor Create(const lt: TLearnType; const AInLen, AOutLen: NativeInt); virtual;
    destructor Destroy; override;

    property EnabledRandomNumber: Boolean read FEnabledRandomNumber write FEnabledRandomNumber;

    procedure Clear;
    function Count: NativeInt;
    property InLen: NativeInt read FInLen;
    property OutLen: NativeInt read FOutLen;

    procedure AddMemory(const f_In, f_Out: TLearnFloatArray); overload;
    procedure AddMemory(const s_In, s_Out: string); overload;
    procedure AddMemory(const s: TPascalString); overload;
    function Train(const passcount: Cardinal): Boolean; overload;
    function Train: Boolean; overload;
    procedure TrainC(const passcount: Cardinal; const OnResult: TLearnState_Call);
    procedure TrainM(const passcount: Cardinal; const OnResult: TLearnState_Method);
    {$IFNDEF FPC} procedure TrainP(const passcount: Cardinal; const OnResult: TLearnState_Proc); {$ENDIF FPC}
    procedure WaitTrain;
    //
    // result ProcessOut
    function process(const ProcessIn: TLearnFloatArray; var ProcessOut: TLearnFloatArray): Boolean; overload;
    // result max value
    function processMax(const ProcessIn: TLearnFloatArray): TLearnFloat; overload;
    // result min value
    function processMin(const ProcessIn: TLearnFloatArray): TLearnFloat; overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    {$IFNDEF FPC}
    procedure SaveToJsonStream(stream: TCoreClassStream);
    procedure LoadFromJsonStream(stream: TCoreClassStream);
    {$ENDIF FPC}
    //
    //
    property Info: string read FInfo;
    property ThreadRuning: Boolean read FThreadRuning;
  end;

implementation

uses Math,
  {$IFDEF FPC}
  mtprocs,
  {$ELSE}
  Threading,
  {$ENDIF FPC}
  SyncObjs, DataFrameEngine, DoStatusIO;

{$INCLUDE Learn_Base.inc}
{$INCLUDE Learn_ablas.inc}
{$INCLUDE learn_trfac.inc}
{$INCLUDE learn_safesolve.inc}
{$INCLUDE learn_rcond.inc}
{$INCLUDE learn_matinv.inc}
{$INCLUDE learn_linmin.inc}
{$INCLUDE learn_lbfgs.inc}
{$INCLUDE learn_rotations.inc}
{$INCLUDE learn_ortfac.inc}
{$INCLUDE learn_bdsvd.inc}
{$INCLUDE learn_svd.inc}
{$INCLUDE learn_densesolver.inc}
{$INCLUDE learn_trainbase.inc}
{$INCLUDE learn_train.inc}
{$INCLUDE learn_trainEnsemble.inc}


type
  PLearnKDT = ^TLearnKDT;

  TLearnKDT = packed record
    kdt: TKDTree;
  end;

  TLearn_th = class(TCoreClassThread)
  public
    source                : TLearn;
    passcount             : NativeInt;
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
  if Assigned(OnStateC) then
      OnStateC(source, Successed);
  if Assigned(OnStateM) then
      OnStateM(source, Successed);
  {$IFNDEF FPC}
  if Assigned(OnStateP) then
      OnStateP(source, Successed);
  {$ENDIF FPC}
  source.FThreadRuning := False;
end;

procedure TLearn_th.Execute;
begin
  if source <> nil then
      Successed := source.Train(passcount)
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
  passcount := 1;
  OnStateC := nil;
  OnStateM := nil;
  {$IFNDEF FPC} OnStateP := nil; {$ENDIF}
  Successed := False;
end;

destructor TLearn_th.Destroy;
begin
  inherited Destroy;
end;

procedure TLearn.KDInput(const IndexFor: NativeInt; var source: TKDTree.TKDTree_Source; const Data: Pointer);
var
  i: Integer;
begin
  source.index := IndexFor;
  for i := 0 to FInLen - 1 do
      source.Buff[i] := PLearnFloatArray(FDataIn[IndexFor])^[i];
end;

constructor TLearn.Create(const lt: TLearnType; const AInLen, AOutLen: NativeInt);
var
  p_k: PLearnKDT;
  p_n: PMultiLayerPerceptron;
  p_e: PMLPEnsemble;
begin
  inherited Create;
  if AInLen <= 0 then
      raiseInfo('input need <= 0');
  if AOutLen <= 0 then
      raiseInfo('output need <= 0');

  FEnabledRandomNumber := False;

  FInLen := AInLen;
  FOutLen := AOutLen;
  FDataIn := TCoreClassList.Create;
  FDataOut := TCoreClassList.Create;
  FLearnType := lt;

  case lt of
    ltKDT:
      begin
        new(p_k);
        p_k^.kdt := TKDTree.Create(FInLen);
        FLearnData := p_k;
      end;
    ltLM, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod:
      begin
        new(p_n);
        MLPCreate0(FInLen, FOutLen, p_n^);
        FLearnData := p_n;
      end;
    ltLMEnsemble, ltLBFGSEnsemble:
      begin
        new(p_e);
        MLPECreate0(FInLen, FOutLen, 10, p_e^);
        FLearnData := p_e;
      end;
  end;

  FInfo := '';
  FIsTraining := False;
  FThreadRuning := False;
end;

destructor TLearn.Destroy;
var
  i: NativeInt;
begin
  WaitTrain;

  if FDataIn <> nil then
    begin
      for i := 0 to FDataIn.Count - 1 do
        begin
          SetLength(PLearnFloatArray(FDataIn[i])^, 0);
          Dispose(PLearnFloatArray(FDataIn[i]));
        end;
      DisposeObject(FDataIn);
      FDataIn := nil;
    end;

  if FDataOut <> nil then
    begin
      for i := 0 to FDataOut.Count - 1 do
        begin
          SetLength(PLearnFloatArray(FDataOut[i])^, 0);
          Dispose(PLearnFloatArray(FDataOut[i]));
        end;
      DisposeObject(FDataOut);
      FDataOut := nil;
    end;

  if FLearnData <> nil then
    begin
      case FLearnType of
        ltKDT:
          begin
            DisposeObject(PLearnKDT(FLearnData)^.kdt);
            Dispose(PLearnKDT(FLearnData));
            FLearnData := nil;
          end;
        ltLM, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod:
          begin
            Dispose(PMultiLayerPerceptron(FLearnData));
            FLearnData := nil;
          end;
        ltLMEnsemble, ltLBFGSEnsemble:
          begin
            Dispose(PMLPEnsemble(FLearnData));
            FLearnData := nil;
          end;
      end;
    end;

  FInLen := 0;
  FOutLen := 0;
  FInfo := '';
  inherited Destroy;
end;

procedure TLearn.Clear;
var
  i  : NativeInt;
  p_k: PLearnKDT;
  p_n: PMultiLayerPerceptron;
  p_e: PMLPEnsemble;
begin
  WaitTrain;

  if FDataIn <> nil then
    begin
      for i := 0 to FDataIn.Count - 1 do
        begin
          SetLength(PLearnFloatArray(FDataIn[i])^, 0);
          Dispose(PLearnFloatArray(FDataIn[i]));
        end;
      DisposeObject(FDataIn);
      FDataIn := nil;
    end;

  if FDataOut <> nil then
    begin
      for i := 0 to FDataOut.Count - 1 do
        begin
          SetLength(PLearnFloatArray(FDataOut[i])^, 0);
          Dispose(PLearnFloatArray(FDataOut[i]));
        end;
      DisposeObject(FDataOut);
      FDataOut := nil;
    end;

  if FLearnData <> nil then
    begin
      case FLearnType of
        ltKDT:
          begin
            DisposeObject(PLearnKDT(FLearnData)^.kdt);
            Dispose(PLearnKDT(FLearnData));
            FLearnData := nil;
          end;
        ltLM, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod:
          begin
            Dispose(PMultiLayerPerceptron(FLearnData));
            FLearnData := nil;
          end;
        ltLMEnsemble, ltLBFGSEnsemble:
          begin
            Dispose(PMLPEnsemble(FLearnData));
            FLearnData := nil;
          end;
      end;
    end;

  FDataIn := TCoreClassList.Create;
  FDataOut := TCoreClassList.Create;
  case FLearnType of
    ltKDT:
      begin
        new(p_k);
        p_k^.kdt := TKDTree.Create(FInLen);
        FLearnData := p_k;
      end;
    ltLM, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod:
      begin
        new(p_n);
        MLPCreate0(FInLen, FOutLen, p_n^);
        FLearnData := p_n;
      end;
    ltLMEnsemble, ltLBFGSEnsemble:
      begin
        new(p_e);
        MLPECreate0(FInLen, FOutLen, 10, p_e^);
        FLearnData := p_e;
      end;
  end;

  FInfo := '';
end;

function TLearn.Count: NativeInt;
begin
  Result := FDataIn.Count;
end;

procedure TLearn.AddMemory(const f_In, f_Out: TLearnFloatArray);
var
  p_in, p_out: PLearnFloatArray;
begin
  if FIsTraining or FThreadRuning then
      raiseInfo('wait Training');
  if Length(f_In) <> FInLen then
      raiseInfo('input length need = %d', [FInLen]);
  if Length(f_Out) <> FOutLen then
      raiseInfo('output length need = %d', [FOutLen]);

  new(p_in);
  SetLength(p_in^, FInLen);
  CopyPtr(@f_In[0], @(p_in^[0]), FInLen * SizeOf(TLearnFloat));

  new(p_out);
  SetLength(p_out^, FOutLen);
  CopyPtr(@f_Out[0], @(p_out^[0]), FOutLen * SizeOf(TLearnFloat));

  FDataIn.Add(p_in);
  FDataOut.Add(p_out);
end;

procedure TLearn.AddMemory(const s_In, s_Out: string);
var
  f_In, f_Out: TLearnFloatArray;
begin
  f_In := TKDTree.KDTreeVec(s_In);
  f_Out := TKDTree.KDTreeVec(s_Out);
  AddMemory(f_In, f_Out);
  SetLength(f_In, 0);
  SetLength(f_Out, 0);
end;

procedure TLearn.AddMemory(const s: TPascalString);
var
  s_In, s_Out: TPascalString;
begin
  s_In := umlGetFirstStr(s, '=');
  s_Out := umlGetLastStr(s, '=');
  AddMemory(s_In.Text, s_Out.Text);
end;

function TLearn.Train(const passcount: Cardinal): Boolean;
var
  p_k         : PLearnKDT;
  p_n         : PMultiLayerPerceptron;
  p_e         : PMLPEnsemble;
  Buff        : TLearnFloat2DArray;
  rInfo       : TLearnInteger;
  mlReport    : TMLPReport;
  IsTerminated: Boolean;
  eBest       : TLearnFloat;
  CVRep       : TMLPCVReport;
  bakseed     : Integer;

  procedure BuildInternalData;
  var
    i, j: NativeInt;
  begin
    SetLength(Buff, FDataIn.Count, FInLen + FOutLen);
    for i := 0 to FDataIn.Count - 1 do
      begin
        for j := 0 to FInLen - 1 do
            Buff[i][j] := PLearnFloatArray(FDataIn[i])^[j];
        for j := 0 to FOutLen - 1 do
            Buff[i][FInLen + j] := PLearnFloatArray(FDataOut[i])^[j];
      end;
  end;

  procedure FreeInternalData;
  begin
    SetLength(Buff, 0, 0);
  end;

begin
  Result := False;

  if FIsTraining then
    begin
      FInfo := 'wait Training';
      exit;
    end;

  if FDataIn.Count <> FDataOut.Count then
    begin
      FInfo := 'Training set mismatch';
      exit;
    end;
  if FDataIn.Count <= 0 then
    begin
      FInfo := 'Training set invailed';
      exit;
    end;

  FIsTraining := True;

  if not FEnabledRandomNumber then
    begin
      bakseed := RandSeed;
      RandSeed := 0;
    end;
  try
    case FLearnType of
      ltKDT:
        begin
          p_k := PLearnKDT(FLearnData);
          p_k^.kdt.Clear;
          {$IFDEF FPC}
          p_k^.kdt.BuildKDTreeM(FDataIn.Count, nil, @KDInput);
          {$ELSE FPC}
          p_k^.kdt.BuildKDTreeM(FDataIn.Count, nil, KDInput);
          {$ENDIF FPC}
          FInfo := 'task has been solved';
          Result := True;
        end;
      ltLM:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          MLPTrainLM(p_n^, Buff, Length(Buff), 0.001, passcount, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -9: FInfo := 'internal matrix inverse subroutine failed';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS(p_n^, Buff, Length(Buff), 0.001, passcount, 0.01, 0, rInfo, mlReport, @IsTerminated, eBest);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_MT:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS_MT(p_n^, Buff, Length(Buff), 0.001, passcount, 0.01, 0, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_MT_Mod:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS_MT_Mod(p_n^, Buff, Length(Buff), passcount, 0.01, 2.0, 0, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLMEnsemble:
        begin
          BuildInternalData;
          p_e := PMLPEnsemble(FLearnData);
          MLPEBaggingLM(p_e^, Buff, Length(Buff), 0.001, passcount, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGSEnsemble:
        begin
          BuildInternalData;
          p_e := PMLPEnsemble(FLearnData);
          MLPEBaggingLBFGS(p_e^, Buff, Length(Buff), 0.001, passcount, 0.01, 0, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
    end;
  finally
    FIsTraining := False;
    if not FEnabledRandomNumber then
        RandSeed := bakseed;
  end;
end;

function TLearn.Train: Boolean;
begin
  Result := Train(1);
end;

procedure TLearn.TrainC(const passcount: Cardinal; const OnResult: TLearnState_Call);
var
  th: TLearn_th;
begin
  WaitTrain;
  FThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.OnStateC := OnResult;
  th.passcount := passcount;
  th.Suspended := False;
end;

procedure TLearn.TrainM(const passcount: Cardinal; const OnResult: TLearnState_Method);
var
  th: TLearn_th;
begin
  WaitTrain;
  FThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.OnStateM := OnResult;
  th.passcount := passcount;
  th.Suspended := False;
end;

{$IFNDEF FPC}


procedure TLearn.TrainP(const passcount: Cardinal; const OnResult: TLearnState_Proc);
var
  th: TLearn_th;
begin
  WaitTrain;
  FThreadRuning := True;
  th := TLearn_th.Create;
  th.source := Self;
  th.OnStateP := OnResult;
  th.passcount := passcount;
  th.Suspended := False;
end;
{$ENDIF FPC}


procedure TLearn.WaitTrain;
begin
  while FThreadRuning do
      CheckThreadSynchronize(1);
end;

function TLearn.process(const ProcessIn: TLearnFloatArray; var ProcessOut: TLearnFloatArray): Boolean;
var
  p_kd_node: TKDTree.PKDTree_Node;
begin
  Result := False;
  if FIsTraining or FThreadRuning then
    begin
      FInfo := 'wait training';
      exit;
    end;
  if Length(ProcessIn) <> FInLen then
    begin
      FInfo := 'input length error';
      exit;
    end;

  case FLearnType of
    ltKDT:
      begin
        p_kd_node := PLearnKDT(FLearnData)^.kdt.Search(ProcessIn);
        if p_kd_node <> nil then
          begin
            ProcessOut := PLearnFloatArray(FDataOut[p_kd_node^.vec^.index])^;
            FInfo := 'successed';
            Result := True;
          end
        else
            FInfo := 'kdTree not inited';
      end;
    ltLM, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod:
      begin
        SetLength(ProcessOut, FOutLen);
        MLPProcess(PMultiLayerPerceptron(FLearnData)^, ProcessIn, ProcessOut);
        FInfo := 'successed';
        Result := True;
      end;
    ltLMEnsemble, ltLBFGSEnsemble:
      begin
        SetLength(ProcessOut, FOutLen);
        MLPEProcess(PMLPEnsemble(FLearnData)^, ProcessIn, ProcessOut);
        FInfo := 'successed';
        Result := True;
      end;
  end;
end;

function TLearn.processMax(const ProcessIn: TLearnFloatArray): TLearnFloat;
var
  ProcessOut: TLearnFloatArray;
  i         : NativeInt;
begin
  Result := 0;
  if not process(ProcessIn, ProcessOut) then
      exit;

  Result := ProcessOut[0];

  if FOutLen > 1 then
    for i := 1 to FOutLen - 1 do
      if ProcessOut[i] > Result then
          Result := ProcessOut[i];

  SetLength(ProcessOut, 0);
end;

function TLearn.processMin(const ProcessIn: TLearnFloatArray): TLearnFloat;
var
  ProcessOut: TLearnFloatArray;
  i         : NativeInt;
begin
  Result := 0;
  if not process(ProcessIn, ProcessOut) then
      exit;

  Result := ProcessOut[0];

  if FOutLen > 1 then
    for i := 1 to FOutLen - 1 do
      if ProcessOut[i] < Result then
          Result := ProcessOut[i];

  SetLength(ProcessOut, 0);
end;

procedure TLearn.SaveToStream(stream: TCoreClassStream);
var
  de  : TDataFrameEngine;
  ar  : TDataFrameArrayDouble;
  i, j: NativeInt;
begin
  de := TDataFrameEngine.Create;
  de.WriteInt64(FInLen);
  de.WriteInt64(FOutLen);
  de.WriteByte(Byte(FLearnType));

  ar := de.WriteArrayDouble;
  for i := 0 to FDataIn.Count - 1 do
    for j := 0 to FInLen - 1 do
        ar.Add(PLearnFloatArray(FDataIn[i])^[j]);

  ar := de.WriteArrayDouble;
  for i := 0 to FDataOut.Count - 1 do
    for j := 0 to FOutLen - 1 do
        ar.Add(PLearnFloatArray(FDataOut[i])^[j]);

  de.EncodeAsZLib(stream, False);
  DisposeObject(de);
end;

procedure TLearn.LoadFromStream(stream: TCoreClassStream);
var
  de  : TDataFrameEngine;
  ar  : TDataFrameArrayDouble;
  i, j: NativeInt;
  pf  : PLearnFloatArray;
begin
  Clear;

  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream, False);
  FInLen := de.Reader.ReadInt64;
  FOutLen := de.Reader.ReadInt64;
  FLearnType := TLearnType(de.Reader.ReadByte);

  ar := de.Reader.ReadArrayDouble;
  j := 0;
  for i := 0 to ar.Count - 1 do
    begin
      if j = 0 then
        begin
          new(pf);
          SetLength(pf^, FInLen);
          FDataIn.Add(pf);
        end;

      pf^[j] := ar[i];
      inc(j);
      if j = FInLen then
          j := 0;
    end;

  ar := de.Reader.ReadArrayDouble;
  j := 0;
  for i := 0 to ar.Count - 1 do
    begin
      if j = 0 then
        begin
          new(pf);
          SetLength(pf^, FOutLen);
          FDataOut.Add(pf);
        end;

      pf^[j] := ar[i];
      inc(j);
      if j = FOutLen then
          j := 0;
    end;

  DisposeObject(de);
  Train;
end;

{$IFNDEF FPC}


procedure TLearn.SaveToJsonStream(stream: TCoreClassStream);
var
  de  : TDataFrameEngine;
  ar  : TDataFrameArrayDouble;
  i, j: NativeInt;
begin
  de := TDataFrameEngine.Create;
  de.WriteInt64(FInLen);
  de.WriteInt64(FOutLen);
  de.WriteByte(Byte(FLearnType));

  ar := de.WriteArrayDouble;
  for i := 0 to FDataIn.Count - 1 do
    for j := 0 to FInLen - 1 do
        ar.Add(PLearnFloatArray(FDataIn[i])^[j]);

  ar := de.WriteArrayDouble;
  for i := 0 to FDataOut.Count - 1 do
    for j := 0 to FOutLen - 1 do
        ar.Add(PLearnFloatArray(FDataOut[i])^[j]);

  de.EncodeAsPublicJson(stream);
  DisposeObject(de);
end;

procedure TLearn.LoadFromJsonStream(stream: TCoreClassStream);
var
  de  : TDataFrameEngine;
  ar  : TDataFrameArrayDouble;
  i, j: NativeInt;
  pf  : PLearnFloatArray;
begin
  Clear;

  de := TDataFrameEngine.Create;
  de.DecodeFromJson(stream);
  FInLen := de.Reader.ReadInt64;
  FOutLen := de.Reader.ReadInt64;
  FLearnType := TLearnType(de.Reader.ReadByte);

  ar := de.Reader.ReadArrayDouble;
  j := 0;
  for i := 0 to ar.Count - 1 do
    begin
      if j = 0 then
        begin
          new(pf);
          SetLength(pf^, FInLen);
          FDataIn.Add(pf);
        end;

      pf^[j] := ar[i];
      inc(j);
      if j = FInLen then
          j := 0;
    end;

  ar := de.Reader.ReadArrayDouble;
  j := 0;
  for i := 0 to ar.Count - 1 do
    begin
      if j = 0 then
        begin
          new(pf);
          SetLength(pf^, FOutLen);
          FDataOut.Add(pf);
        end;

      pf^[j] := ar[i];
      inc(j);
      if j = FOutLen then
          j := 0;
    end;

  DisposeObject(de);
end;
{$ENDIF FPC}

initialization

end.
