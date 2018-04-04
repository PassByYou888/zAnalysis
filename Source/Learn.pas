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

uses CoreClasses, PascalStrings, KDTree;

{$I ZDefine.inc}


type
  TLearnFloat      = TKDTree.TKDTree_VecType;
  PLearnFloat      = TKDTree.PKDTree_VecType;
  TLearnFloatArray = TKDTree.TKDTree_Vec;
  PLearnFloatArray = TKDTree.PKDTree_Vec;

  TLearnType = (ltKDTree, ltLM, ltLBFGS, ltKFoldCVLBFGS, ltKFoldCVLM, ltLMEnsemble, ltLBFGSEnsemble, ltESEnsemble);

  PLearnSource = ^TLearnSource;

  TLearnSource = packed record
  private
    InLen, OutLen  : NativeInt;
    DataIn, DataOut: TCoreClassList;
    LearnType      : TLearnType;
    LearnData      : Pointer;

    procedure KDInput(const IndexFor: NativeInt; var source: TKDTree.TKDTree_Source; const Data: Pointer);
  public
    Info      : TPascalString;
    isTraining: Boolean;

    procedure AddMemory(const Ref_In, Ref_Out: TLearnFloatArray);
    function ExecuteTrain: Boolean;
    procedure process(const Ref_In: TLearnFloatArray; var ProcessOut: TLearnFloatArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
  end;

function BuildLearn(const lt: TLearnType; const InLen, OutLen: NativeInt): TLearnSource;
procedure FreeLearn(var source: TLearnSource);

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
  TLearnKDT = packed record
    kt: TKDTree;
  end;

  PLearnKDT = ^TLearnKDT;

procedure TLearnSource.KDInput(const IndexFor: NativeInt; var source: TKDTree.TKDTree_Source; const Data: Pointer);
var
  i: Integer;
begin
  source.index := IndexFor;
  for i := 0 to InLen - 1 do
      source.Buff[i] := PLearnFloatArray(DataIn[IndexFor])^[i];
end;

procedure TLearnSource.AddMemory(const Ref_In, Ref_Out: TLearnFloatArray);
var
  p_in, p_out: PLearnFloatArray;
begin
  if isTraining then
      raiseInfo('wait Training');
  if Length(Ref_In) <> InLen then
      raiseInfo('input length need = %d', [InLen]);
  if Length(Ref_Out) <> OutLen then
      raiseInfo('output length need = %d', [OutLen]);

  new(p_in);
  p_in^ := Ref_In;
  new(p_out);
  p_out^ := Ref_Out;

  DataIn.Add(p_in);
  DataOut.Add(p_out);
end;

function TLearnSource.ExecuteTrain: Boolean;
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

  procedure BuildInternalData;
  var
    i, j: NativeInt;
  begin
    SetLength(Buff, DataIn.Count, InLen + OutLen);
    for i := 0 to DataIn.Count - 1 do
      begin
        for j := 0 to InLen - 1 do
            Buff[i][j] := PLearnFloatArray(DataIn[i])^[j];
        for j := 0 to OutLen - 1 do
            Buff[i][InLen + j] := PLearnFloatArray(DataOut[i])^[j];
      end;
  end;

  procedure FreeInternalData;
  begin
    SetLength(Buff, 0, 0);
  end;

begin
  Result := False;

  if isTraining then
    begin
      Info := 'wait Training';
      exit;
    end;

  if DataIn.Count <> DataOut.Count then
    begin
      Info := 'Training set mismatch';
      exit;
    end;
  if DataIn.Count <= 0 then
    begin
      Info := 'Training set invailed';
      exit;
    end;

  isTraining := True;
  try
    case LearnType of
      ltKDTree:
        begin
          p_k := PLearnKDT(LearnData);
          p_k^.kt.Clear;
          p_k^.kt.BuildKDTreeM(DataIn.Count, nil, KDInput);
          Info := 'task has been solved';
          Result := True;
        end;
      ltLM:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(LearnData);
          MLPTrainLM(p_n^, Buff, Length(Buff), 0.001, 1, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: Info := 'task has been solved';
            -9: Info := 'internal matrix inverse subroutine failed';
            -2: Info := 'there is a point with class number outside of [0..NOut-1]';
            -1: Info := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else Info := 'unknow state';
          end;
          Result := Info = 2;
        end;
      ltLBFGS:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(LearnData);
          IsTerminated := False;
          MLPTrainLBFGS(p_n^, Buff, Length(Buff), 0.001, 1, 0.01, 0, rInfo, mlReport, @IsTerminated, eBest);
          FreeInternalData;
          case rInfo of
            2: Info := 'task has been solved';
            -8: Info := 'if both WStep=0 and MaxIts=0';
            -2: Info := 'there is a point with class number outside of [0..NOut-1]';
            -1: Info := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else Info := 'unknow state';
          end;
          Result := Info = 2;
        end;
      ltKFoldCVLBFGS:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(LearnData);
          MLPKFoldCVLBFGS(p_n^, Buff, Length(Buff), 0.001, 1, 0.01, 0, 10, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: Info := 'task has been solved';
            -8: Info := 'if both WStep=0 and MaxIts=0';
            -2: Info := 'there is a point with class number outside of [0..NOut-1]';
            -1: Info := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else Info := 'unknow state';
          end;
          Result := Info = 2;
        end;
      ltKFoldCVLM:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(LearnData);
          MLPKFoldCVLM(p_n^, Buff, Length(Buff), 0.001, 1, 10, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: Info := 'task has been solved';
            -9: Info := 'internal matrix inverse subroutine failed';
            -2: Info := 'there is a point with class number outside of [0..NOut-1]';
            -1: Info := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else Info := 'unknow state';
          end;
          Result := Info = 2;
        end;
      ltLMEnsemble:
        begin
          BuildInternalData;
          p_e := PMLPEnsemble(LearnData);
          MLPEBaggingLM(p_e^, Buff, Length(Buff), 0.001, 1, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: Info := 'task has been solved';
            -2: Info := 'there is a point with class number outside of [0..NClasses-1]';
            -1: Info := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else Info := 'unknow state';
          end;
          Result := Info = 2;
        end;
      ltLBFGSEnsemble:
        begin
          BuildInternalData;
          p_e := PMLPEnsemble(LearnData);
          MLPEBaggingLBFGS(p_e^, Buff, Length(Buff), 0.001, 1, 0.01, 0, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: Info := 'task has been solved';
            -8: Info := 'both WStep=0 and MaxIts=0';
            -2: Info := 'there is a point with class number outside of [0..NClasses-1]';
            -1: Info := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else Info := 'unknow state';
          end;
          Result := Info = 2;
        end;
      ltESEnsemble:
        begin
          BuildInternalData;
          p_e := PMLPEnsemble(LearnData);
          MLPETrainES(p_e^, Buff, Length(Buff), 0.001, 1, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            6: Info := 'task has been solved';
            -2: Info := 'there is a point with class number outside of [0..NClasses-1]';
            -1: Info := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else Info := 'unknow state';
          end;
          Result := Info = 6;
        end;
    end;
  finally
      isTraining := False;
  end;
end;

procedure TLearnSource.process(const Ref_In: TLearnFloatArray; var ProcessOut: TLearnFloatArray);
var
  p_kd_node: TKDTree.PKDTree_Node;
begin
  if isTraining then
    begin
      raiseInfo('wait training');
      exit;
    end;
  if Length(Ref_In) <> InLen then
      raiseInfo('input length need = %d', [InLen]);

  case LearnType of
    ltKDTree:
      begin
        p_kd_node := PLearnKDT(LearnData)^.kt.Search(Ref_In);
        ProcessOut := PLearnFloatArray(DataOut[p_kd_node^.vec.index])^;
      end;
    ltLM, ltLBFGS, ltKFoldCVLBFGS, ltKFoldCVLM:
      begin
        SetLength(ProcessOut, OutLen);
        MLPProcess(PMultiLayerPerceptron(LearnData)^, Ref_In, ProcessOut);
      end;
    ltLMEnsemble, ltLBFGSEnsemble, ltESEnsemble:
      begin
        MLPEProcess(PMLPEnsemble(LearnData)^, Ref_In, ProcessOut);
      end;
  end;
end;

procedure TLearnSource.SaveToStream(stream: TCoreClassStream);
var
  de  : TDataFrameEngine;
  ar  : TDataFrameArrayDouble;
  i, j: NativeInt;
begin
  if isTraining then
    begin
      raiseInfo('wait training');
      exit;
    end;
  de := TDataFrameEngine.Create;
  de.WriteInt64(InLen);
  de.WriteInt64(OutLen);
  de.WriteByte(Byte(LearnType));

  ar := de.WriteArrayDouble;
  for i := 0 to DataIn.Count - 1 do
    for j := 0 to InLen - 1 do
        ar.Add(PLearnFloatArray(DataIn[i])^[j]);

  ar := de.WriteArrayDouble;
  for i := 0 to DataOut.Count - 1 do
    for j := 0 to OutLen - 1 do
        ar.Add(PLearnFloatArray(DataOut[i])^[j]);

  de.EncodeAsZLib(stream, False);
  DisposeObject(de);
end;

procedure TLearnSource.LoadFromStream(stream: TCoreClassStream);
var
  de  : TDataFrameEngine;
  ar  : TDataFrameArrayDouble;
  i, j: NativeInt;
  pf  : PLearnFloatArray;
begin
  if isTraining then
    begin
      raiseInfo('wait training');
      exit;
    end;
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream, False);
  InLen := de.Reader.ReadInt64;
  OutLen := de.Reader.ReadInt64;
  LearnType := TLearnType(de.Reader.ReadByte);

  ar := de.Reader.ReadArrayDouble;
  j := 0;
  for i := 0 to ar.Count - 1 do
    begin
      if j = 0 then
        begin
          new(pf);
          SetLength(pf^, InLen);
          DataIn.Add(pf);
        end;

      pf^[j] := ar[i];
      inc(j);
      if j = InLen then
          j := 0;
    end;

  ar := de.Reader.ReadArrayDouble;
  j := 0;
  for i := 0 to ar.Count - 1 do
    begin
      if j = 0 then
        begin
          new(pf);
          SetLength(pf^, OutLen);
          DataOut.Add(pf);
        end;

      pf^[j] := ar[i];
      inc(j);
      if j = OutLen then
          j := 0;
    end;

  DisposeObject(de);
end;

function BuildLearn(const lt: TLearnType; const InLen, OutLen: NativeInt): TLearnSource;
var
  p_k: PLearnKDT;
  p_n: PMultiLayerPerceptron;
  p_e: PMLPEnsemble;
begin
  if InLen <= 0 then
      raiseInfo('input need <= 0');
  if OutLen <= 0 then
      raiseInfo('output need <= 0');

  Result.InLen := InLen;
  Result.OutLen := OutLen;
  Result.DataIn := TCoreClassList.Create;
  Result.DataOut := TCoreClassList.Create;
  Result.LearnType := lt;

  case lt of
    ltKDTree:
      begin
        new(p_k);
        p_k^.kt := TKDTree.Create(InLen);
        Result.LearnData := p_k;
      end;
    ltLM, ltLBFGS, ltKFoldCVLBFGS, ltKFoldCVLM:
      begin
        new(p_n);
        MLPCreate0(InLen, OutLen, p_n^);
        Result.LearnData := p_n;
      end;
    ltLMEnsemble, ltLBFGSEnsemble, ltESEnsemble:
      begin
        new(p_e);
        MLPECreate0(InLen, OutLen, 10, p_e^);
        Result.LearnData := p_e;
      end;
  end;

  Result.Info := 'successed';
  Result.isTraining := False;
end;

procedure FreeLearn(var source: TLearnSource);
var
  i: NativeInt;
begin
  if source.isTraining then
    begin
      raiseInfo('wait training');
      exit;
    end;

  if source.DataIn <> nil then
    begin
      for i := 0 to source.DataIn.Count - 1 do
        begin
          SetLength(PLearnFloatArray(source.DataIn[i])^, 0);
          Dispose(PLearnFloatArray(source.DataIn[i]));
        end;
      DisposeObject(source.DataIn);
      source.DataIn := nil;
    end;

  if source.DataOut <> nil then
    begin
      for i := 0 to source.DataOut.Count - 1 do
        begin
          SetLength(PLearnFloatArray(source.DataOut[i])^, 0);
          Dispose(PLearnFloatArray(source.DataOut[i]));
        end;
      DisposeObject(source.DataOut);
      source.DataOut := nil;
    end;

  if source.LearnData <> nil then
    begin
      case source.LearnType of
        ltKDTree:
          begin
            DisposeObject(PLearnKDT(source.LearnData)^.kt);
            Dispose(PLearnKDT(source.LearnData));
            source.LearnData := nil;
          end;
        ltLM, ltLBFGS, ltKFoldCVLBFGS, ltKFoldCVLM:
          begin
            Dispose(PMultiLayerPerceptron(source.LearnData));
            source.LearnData := nil;
          end;
        ltLMEnsemble, ltLBFGSEnsemble, ltESEnsemble:
          begin
            Dispose(PMLPEnsemble(source.LearnData));
            source.LearnData := nil;
          end;
      end;
    end;

  source.InLen := 0;
  source.OutLen := 0;
  source.Info := 'successed';
end;

initialization

end.
