program testKDTree;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  DoStatusIO,
  Learn, KDTree,
  FastKDTreeD, FastKDTreeE, FastKDTreeS, FastKDTreeC, FastKDTreeI8,
  FastKDTreeI16, FastKDTreeI32, FastKDTreeI64;

var
  i: integer;

begin
  KDTree.Test_KDTree(64);
  FastKDTreeI8.Test_All;
  FastKDTreeI16.Test_All;
  FastKDTreeI32.Test_All;
  FastKDTreeI64.Test_All;
  FastKDTreeS.Test_All;
  FastKDTreeD.Test_All;
  FastKDTreeE.Test_All;
  FastKDTreeC.Test_All;

  DoStatus('all test finished!');

  readln;

  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
