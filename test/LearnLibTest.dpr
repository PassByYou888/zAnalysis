program LearnLibTest;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  DoStatusIO,
  Learn;

begin
  LearnTest;

  DoStatus('all test finished!');

  readln;

  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
