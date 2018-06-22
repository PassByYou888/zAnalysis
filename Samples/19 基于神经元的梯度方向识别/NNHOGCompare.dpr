program NNHOGCompare;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  NNHOGCompareFrm in 'NNHOGCompareFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
