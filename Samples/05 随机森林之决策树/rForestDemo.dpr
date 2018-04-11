program rForestDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, CoreClasses, DoStatusIO, UnicodeMixedLib, Learn;

// 这里是我们模拟在应用时的条件处理程序
procedure runF(id: TLearnFloat);
begin
  case Round(id) of
    1: dostatus('%f hey', [id]);
    2: dostatus('%f im robot', [id]);
    3: dostatus('%f nice to meet you', [id]);
    4: dostatus('%f haha', [id]);
    5: dostatus('%f lol', [id]);
    6: dostatus('%f hello world', [id]);
    7: dostatus('%f byebye', [id]);
    else raiseInfo('%f no memory', [id]);
  end;
end;

var
  lr: TLearn;
  i : Integer;
begin
  System.ReportMemoryLeaksOnShutdown := True;

  // 随机森林决策模型
  // 随机森林决策模型在工作时需要逻辑决策回归，OutIn可以1或者更高数值，OutLen只能是1
  lr := TLearn.Create(TLearnType.ltForest, 2, 1);
  lr.AddMemory('0,0 = 1');
  lr.AddMemory('1,1 = 2');
  lr.AddMemory('1,0 = 3');
  lr.AddMemory('0,1 = 4');
  lr.AddMemory('4,5 = 5');
  lr.AddMemory('3,5 = 6');
  lr.AddMemory('5,3 = 7');

  lr.Train;

  // 这里是我们已经学习过的内容，我们打印出来验证
  runF(lr.processRF([0, 0]));
  runF(lr.processRF([1, 1]));
  runF(lr.processRF([1, 0]));
  runF(lr.processRF([0, 1]));
  runF(lr.processRF([4, 5]));
  runF(lr.processRF([3, 5]));
  runF(lr.processRF([5, 3]));

  // 随机值 推理学习
  // 随机森林决策模型会严格的从已经学习到的Out值中去寻找和推理最佳符合条件
  // 随机森林适用于复杂的条件处理程序
  DoStatus('************************************************');
  for i := 1 to 10 do
    begin
      runF(lr.processRF([umlRandomRange(-100, 100), umlRandomRange(-100, 100)]));
      TCoreClassThread.Sleep(100);
    end;

  disposeObject(lr);
  readln;
end.
