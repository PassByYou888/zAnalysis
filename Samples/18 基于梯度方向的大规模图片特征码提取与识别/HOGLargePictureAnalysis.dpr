program HOGLargePictureAnalysis;

uses
  System.StartUpCopy,
  FMX.Forms,
  HOGLargepictureAnalysisFrm in 'HOGLargepictureAnalysisFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
