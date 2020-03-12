program Analysis_RandomForestDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils, CoreClasses, DoStatusIO, UnicodeMixedLib, Learn, LearnTypes;

var
  lr: TLearn;
  i: Integer;

function f1(const In_: TLVec): TLFloat;
begin
  Result := lr.ProcessFV(In_);
  DoStatusNoLn('input (%s = %f) ', [LVec(In_).Text, Result]);
end;

function f2(id: TLFloat; const Print: Boolean = True): string;
begin
  Result := format('machine decision %d', [round(id)]);

  if Print then
    begin
      DoStatusNoLn(Result);
      DoStatusNoLn;
    end;
end;

begin
  System.ReportMemoryLeaksOnShutdown := True;

  // 随机森林决策树回归模型
  // 随机森林决策模型在工作时需要逻辑决策回归，OutIn可以1或者更高数值，OutLen只能是1
  lr := TLearn.CreateRegression(TLearnType.ltForest, 2, 1);
  lr.AddMemory('0,0 = 1');
  lr.AddMemory('1,1 = 2');
  lr.AddMemory('1,0 = 3');
  lr.AddMemory('0,1 = 4');
  lr.AddMemory('4,5 = 5');
  lr.AddMemory('3,5 = 6');
  lr.AddMemory('5,3 = 7');
  lr.AddMemory('4,2 = 8');
  lr.AddMemory('1,2 = 9');
  lr.AddMemory('0,5 = 10');
  lr.Training();

  // 这里是我们已经学习过的内容，我们打印出来验证
  for i := 0 to lr.Count - 1 do
      f2(f1(lr[i]^.m_in));

  // 随机值 推理学习
  // 随机森林决策模型会严格的从已经学习到的Out值中去寻找和推理最佳符合条件
  // 随机森林适用于复杂的条件处理程序
  DoStatus('************************************************');
  for i := 1 to 10 do
      f2(f1([umlRandomRange(0, 5), umlRandomRange(0, 5)]));
  disposeObject(lr);

  DoStatus('************************************************');
  // 标签式决策
  // 森林决策树回归模型
  // 森林决策树回归模型在工作时需要逻辑决策回归，OutIn可以1或者更高数值，OutLen只能是1
  lr := TLearn.CreateRegression(TLearnType.ltForest, 2, 1);
  lr.AddMemory('0,0 = ' + f2(1, False));
  lr.AddMemory('1,1 = ' + f2(2, False));
  lr.AddMemory('1,0 = ' + f2(3, False));
  lr.AddMemory('0,1 = ' + f2(4, False));
  lr.AddMemory('4,5 = ' + f2(5, False));
  lr.AddMemory('3,5 = ' + f2(6, False));
  lr.AddMemory('5,3 = ' + f2(7, False));
  lr.AddMemory('4,2 = ' + f2(8, False));
  lr.AddMemory('1,2 = ' + f2(9, False));
  lr.AddMemory('0,5 = ' + f2(10, False));
  lr.Training();
  for i := 1 to 10 do
    begin
      DoStatusNoLn(lr.SearchToken([f1([umlRandomRange(0, 5), umlRandomRange(0, 5)])]));
      DoStatusNoLn;
    end;

  disposeObject(lr);
  readln;

end.
