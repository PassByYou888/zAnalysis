program NNDecisionDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  Learn;

var
  lr: TLearn;
  n : TPascalString;
  lt: TLearnType;

begin
  // 这里演示了怎样使用神经网络进行数据分类
  for lt in [ltKDT, ltKM,
    ltForest, ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo, ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT] do
    begin
      // CreateClassifier2 是创建一个具有两层感知能力的神经网络学习层
      // CreateClassifier方法在任何时候，都是一个近整型输出值
      // 我们基于最速下降法LBFGS进行决策分类
      lr := TLearn.CreateClassifier2(lt, 5);

      // 1 1 1 1 1，这里的5个1表示5种数据维度，这里可以说任意数值
      // 0表示决代号，决策号是线性的
      lr.AddMemory('1 1 1 1 1=0');

      // 依次类推
      lr.AddMemory('1 2 1 2 1=1');

      // 依次类推
      lr.AddMemory('10 10 21 12 21=2');

      // 2 2 2 2 2，这里的5个2也表示5种数据维度，这里可以说任意数值
      // 3表示决代号，决策号是线性的
      lr.AddMemory('2 2 2 2 2=3');

      // 100是训练深度，在内核反复迭代训练的次数
      // 部分算法因为样本数量，在这一步会有不同延迟
      lr.Train(100);

      // 这里的输出是4个返回值，因为，我们只有0,1,2,3三种决策，依据0123的顺序，对n变量的输入值进行分别评估
      // 数值最大的值，表示最接近的决策id，当我们取得了决策id后，即可进行相应的子程序处理
      // 不同的训练方法会产生不同的权重
      n := '1 2 3 4 5';
      DoStatus('(%s) %s = %s', [Learn.CLearnString[lr.LearnType], n.Text, lr.process(n)]);
      DoStatus('(%s) 最优决策ID:%d', [Learn.CLearnString[lr.LearnType], lr.processMaxIndex(LVec(n, lr.InLen))]);

      // 这里的输出是4个返回值，因为，我们只有0,1,2,3三种决策，依据0123的顺序，对n变量的输入值进行分别评估
      // 数值最大的值，表示最接近的决策id，当我们取得了决策id后，即可进行相应的子程序处理
      // 不同的训练方法会产生不同的权重
      n := '10 10 21 12 21';
      DoStatus('(%s) %s = %s', [Learn.CLearnString[lr.LearnType], n.Text, lr.process(n)]);
      DoStatus('(%s) 最优决策ID:%d', [Learn.CLearnString[lr.LearnType], lr.processMaxIndex(LVec(n, lr.InLen))]);

      disposeObject(lr);
    end;

  DoStatus('NN决策演示结束');
  readln;

end.
