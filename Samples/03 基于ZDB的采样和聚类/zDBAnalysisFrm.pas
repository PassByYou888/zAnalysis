unit zDBAnalysisFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  CoreClasses, ZDBEngine, Learn, KDTree, KM, ListEngine, DoStatusIO, PascalStrings, UnicodeMixedLib, JsonDataObjects,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    CreateRandDataButton: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TLabeledEdit;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CreateRandDataButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    ze                     : TDBStoreBase;
    lr1, lr2, lr3, lr4, lr5: TLearn;
  public
    { Public declarations }
    procedure DoStatusM(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.CreateRandDataButtonClick(Sender: TObject);
var
  i         : Integer;
  vl        : THashVariantList;
  OriginBuff: TLearnFloat2DArray;

  kBuff       : TKMFloat2DArray;
  KResultArray: TKMFloat2DArray;
  kResultIndex: TKMIntegerArray;
begin
  vl := THashVariantList.Create;

  vl['age'] := umlRandomRangeD(17.5, 20);
  vl['deposit'] := umlRandomRangeD(0.001, 0.05) * 10000;
  vl['consume'] := umlRandomRangeD(0.001, 0.03) * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 20;
  vl['deposit'] := 0.5 * 10000;
  vl['consume'] := 0.4 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 20.5;
  vl['deposit'] := 0.7 * 10000;
  vl['consume'] := 0.45 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 25;
  vl['deposit'] := 1.5 * 10000;
  vl['consume'] := 0.7 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 27;
  vl['deposit'] := 2.5 * 10000;
  vl['consume'] := 0.5 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 30;
  vl['deposit'] := 5.5 * 10000;
  vl['consume'] := 0.4 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 40;
  vl['deposit'] := 25 * 10000;
  vl['consume'] := 0.5 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 45;
  vl['deposit'] := 100 * 10000;
  vl['consume'] := 1.5 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 50;
  vl['deposit'] := 500 * 10000;
  vl['consume'] := 3.5 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 51;
  vl['deposit'] := 470 * 10000;
  vl['consume'] := 1.5 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 53;
  vl['deposit'] := 450 * 10000;
  vl['consume'] := 1.2 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 55;
  vl['deposit'] := 430 * 10000;
  vl['consume'] := 1.0 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  vl['age'] := 60;
  vl['deposit'] := 400 * 10000;
  vl['consume'] := 0.5 * 10000;
  vl['index'] := ze.Count + 1;
  ze.AddData(vl);

  DisposeObject(vl);
  DoStatus('数据库条目:%d', [ze.Count]);

  SetLength(OriginBuff, ze.Count, 4);

  ze.WaitQuery(procedure(var qState: TQueryState)
    begin
      OriginBuff[qState.index][0] := ze.vl[qState.StorePos]['age'];
      OriginBuff[qState.index][1] := ze.vl[qState.StorePos]['deposit'];
      OriginBuff[qState.index][2] := ze.vl[qState.StorePos]['consume'];
      OriginBuff[qState.index][3] := ze.vl[qState.StorePos]['index'];
    end);

  lr1.Clear;
  lr2.Clear;
  lr3.Clear;

  // 月消费聚类
  SetLength(kBuff, length(OriginBuff), 1);
  for i := 0 to length(kBuff) - 1 do
      kBuff[i][0] := OriginBuff[i][2];
  lr1.KMeans(kBuff, 1, 5, KResultArray, kResultIndex);
  for i := 0 to 5 - 1 do
      lr1.AddMemory([KResultArray[0, i]], [i, kResultIndex[i]]);
  lr1.Train_MT;

  // 存款与月消费
  SetLength(kBuff, length(OriginBuff), 2);
  for i := 0 to length(kBuff) - 1 do
    begin
      kBuff[i][0] := OriginBuff[i][1];
      kBuff[i][1] := OriginBuff[i][2];
    end;
  lr2.KMeans(kBuff, 2, 10, KResultArray, kResultIndex);
  for i := 0 to 10 - 1 do
      lr2.AddMemory([KResultArray[0, i], KResultArray[1, i]], [i, kResultIndex[i]]);
  lr2.Train_MT;

  // 综合
  SetLength(kBuff, length(OriginBuff), 3);
  for i := 0 to length(kBuff) - 1 do
    begin
      kBuff[i][0] := OriginBuff[i][0];
      kBuff[i][1] := OriginBuff[i][1];
      kBuff[i][2] := OriginBuff[i][2];
    end;
  lr3.KMeans(kBuff, 3, 5, KResultArray, kResultIndex);
  for i := 0 to 5 - 1 do
      lr3.AddMemory([KResultArray[0, i], KResultArray[1, i], KResultArray[2, i]], [i, kResultIndex[i]]);
  lr3.Train_MT;

  // 年龄段
  SetLength(kBuff, length(OriginBuff), 1);
  for i := 0 to length(kBuff) - 1 do
      kBuff[i][0] := OriginBuff[i][0];
  lr4.KMeans(kBuff, 1, 5, KResultArray, kResultIndex);
  for i := 0 to 5 - 1 do
      lr4.AddMemory([KResultArray[0, i]], [i, kResultIndex[i]]);
  lr4.Train_MT;

  // 推理学习
  for i := 0 to length(OriginBuff) - 1 do
      lr5.AddMemory([OriginBuff[i][0]], [OriginBuff[i][1], OriginBuff[i][2]]);
  lr5.TrainP(10, procedure(const Sender: TLearn; const state: Boolean)
    begin
      DoStatus('推理机器人学习完成');
    end);

  SetLength(OriginBuff, 0, 0);
  SetLength(KResultArray, 0, 0);
  SetLength(kResultIndex, 0);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  buff: array of TStringList;
  i, j: Integer;
begin
  SetLength(buff, lr1.Count);
  for i := 0 to length(buff) - 1 do
      buff[i] := TStringList.Create;

  ze.WaitQuery(procedure(var qState: TQueryState)
    var
      v, r: TLearnVector;
    begin
      v := [ze.vl[qState.StorePos]['consume']];
      r := [];
      lr1.process(@v, @r);
      buff[Round(r[0])].Add(Format('年龄:%d 每月消费 %f 万元', [

        Round(ze.vl[qState.StorePos]['age']),
        (Double(ze.vl[qState.StorePos]['consume']) * 0.0001)
        ]));
    end);

  j := 0;
  for i := 0 to length(buff) - 1 do
    if buff[i].Count > 0 then
      begin
        DoStatus('第 %d 个月消费聚类', [j + 1]);
        DoStatus(buff[i]);
        inc(j);
      end;

  for i := 0 to length(buff) - 1 do
      DisposeObject(buff[i]);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  buff: array of TStringList;
  i, j: Integer;
begin
  SetLength(buff, lr2.Count);
  for i := 0 to length(buff) - 1 do
      buff[i] := TStringList.Create;

  ze.WaitQuery(procedure(var qState: TQueryState)
    var
      v, r: TLearnVector;
    begin
      v := [ze.vl[qState.StorePos]['deposit'], ze.vl[qState.StorePos]['consume']];
      r := [];
      lr2.process(@v, @r);
      buff[Round(r[0])].Add(Format('年龄:%d 现 %f 万元存款，每月消费 %f 万元', [

        Round(ze.vl[qState.StorePos]['age']),
        (Double(ze.vl[qState.StorePos]['deposit']) * 0.0001),
        (Double(ze.vl[qState.StorePos]['consume']) * 0.0001)
        ]));
    end);

  j := 0;
  for i := 0 to length(buff) - 1 do
    if buff[i].Count > 0 then
      begin
        DoStatus('第 %d 个月存款与消费聚类', [j + 1]);
        DoStatus(buff[i]);
        inc(j);
      end;

  for i := 0 to length(buff) - 1 do
      DisposeObject(buff[i]);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  buff: array of TStringList;
  i, j: Integer;
begin
  SetLength(buff, lr4.Count);
  for i := 0 to length(buff) - 1 do
      buff[i] := TStringList.Create;

  ze.WaitQuery(procedure(var qState: TQueryState)
    var
      v, r: TLearnVector;
    begin
      v := [ze.vl[qState.StorePos]['age']];
      r := [];
      lr4.process(@v, @r);
      buff[Round(r[0])].Add(Format('年龄 %f 岁, 每月消费 %f 万元', [

        (Double(ze.vl[qState.StorePos]['age'])),
        (Double(ze.vl[qState.StorePos]['consume']) * 0.0001)
        ]));
    end);

  j := 0;
  for i := 0 to length(buff) - 1 do
    if buff[i].Count > 0 then
      begin
        DoStatus('第 %d 个年龄段聚类', [j + 1]);
        DoStatus(buff[i]);
        inc(j);
      end;

  for i := 0 to length(buff) - 1 do
      DisposeObject(buff[i]);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  buff: array of TStringList;
  i, j: Integer;
begin
  SetLength(buff, lr3.Count);
  for i := 0 to length(buff) - 1 do
      buff[i] := TStringList.Create;

  ze.WaitQuery(procedure(var qState: TQueryState)
    var
      v, r: TLearnVector;
    begin
      v := [ze.vl[qState.StorePos]['age'], ze.vl[qState.StorePos]['deposit'], ze.vl[qState.StorePos]['consume']];
      r := [];
      lr3.process(@v, @r);
      buff[Round(r[0])].Add(Format('年龄:%d 现 %f 万元存款，每月消费 %f 万元', [

        Round(ze.vl[qState.StorePos]['age']),
        (Double(ze.vl[qState.StorePos]['deposit']) * 0.0001),
        (Double(ze.vl[qState.StorePos]['consume']) * 0.0001)
        ]));
    end);

  j := 0;
  for i := 0 to length(buff) - 1 do
    if buff[i].Count > 0 then
      begin
        DoStatus('第 %d 个综合聚类', [j + 1]);
        DoStatus(buff[i]);
        inc(j);
      end;

  for i := 0 to length(buff) - 1 do
      DisposeObject(buff[i]);
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  v, r: TLearnVector;
begin
  v := [umlStrToFloat(Edit1.text, 0)];
  r := [];
  if lr5.process(@v, @r) then
      DoStatus('推理机器人计算结果 当 %f 岁，存款 %f 万，每月消费 %f 万', [
      v[0],
      r[0] * 0.0001,
      r[1] * 0.0001
      ]);
end;

procedure TForm1.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ze := TDBStoreBase.CreateNewMemory;
  AddDoStatusHook(Self, DoStatusM);

  // lr1          这是做消费能力统计的机器人
  // 参数 lt:     机器学习的方式，我们在这里使用ltKDT
  // ltKDT是k维线性空间的切割算法，它不能推理，它的作用是快速找到临近度最高的k维入口
  // 参数 AInLen: 输入的数组数据长度，lr1是学习消费能力，我们只输入1项数值
  // 参数 AOutLen:输出的数组数据长度，lr1是输出2个数值，第一是输出聚类索引，第二个是输出数据库索引
  lr1 := TLearn.Create(TLearnType.ltKDT, 1, 2);

  // lr2          这是做存款+消费能力统计的机器人
  // 参数 lt:     机器学习的方式，我们在这里使用ltKDT
  // ltKDT是k维线性空间的切割算法，它不能推理，它的作用是快速找到临近度最高的k维入口
  // 参数 AInLen: 输入的数组数据长度，lr2是学习存款和消费能力，我们输入2项数值
  // 参数 AOutLen:输出的数组数据长度，lr2是输出2个数值，第一是输出聚类索引，第二个是输出数据库索引
  lr2 := TLearn.Create(TLearnType.ltKDT, 2, 2);

  // lr3          这是做年龄+存款+消费能力统计的机器人
  // 参数 lt:     机器学习的方式，我们在这里使用ltKDT
  // ltKDT是k维线性空间的切割算法，它不能推理，它的作用是快速找到临近度最高的k维入口
  // 参数 AInLen: 输入的数组数据长度，lr3是学习年龄,存款,消费能力，我们输入3项数值
  // 参数 AOutLen:输出的数组数据长度，lr3是输出2个数值，第一是输出聚类索引，第二个是输出数据库索引
  lr3 := TLearn.Create(TLearnType.ltKDT, 3, 2);

  // lr3          这是做年龄归类统计的机器人
  // 参数 lt:     机器学习的方式，我们在这里使用ltKDT
  // ltKDT是k维线性空间的切割算法，它不能推理，它的作用是快速找到临近度最高的k维入口
  // 参数 AInLen: 输入的数组数据长度，lr4是学习年龄，我们输入1项数值
  // 参数 AOutLen:输出的数组数据长度，lr4是输出2个数值，第一是输出聚类索引，第二个是输出数据库索引
  lr4 := TLearn.Create(TLearnType.ltKDT, 1, 2);

  // lr5          这是做年龄推理的机器人，我们输入一项随意数值作为年龄，lr5会自动推理出这个年龄应该有的存款的消费能力
  // ltLBFGS_Ensemble_MT  是BFGS的神经网络集成，神经网络集成化算法是先将数据按极值归类后再学习(凡是后缀是_MT的，所有的学习都是并行化进行的，在HPC服务器上，它的效果非常好)
  // 神经网络集成的相关技术，请自行上网学习
  // lr5 与前面的k维线性空间学习方式不一样，它是将数据库内容全部进行记忆，然后再学习
  lr5 := TLearn.Create(TLearnType.ltLBFGS_Ensemble_MT, 1, 2);

  CreateRandDataButtonClick(CreateRandDataButton);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject(ze);
  DisposeObject([lr1, lr2, lr3, lr4]);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  CoreClasses.CheckThreadSynchronize(0);
end;

end.
