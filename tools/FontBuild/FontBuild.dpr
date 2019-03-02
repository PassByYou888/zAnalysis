program FontBuild;

uses
  Vcl.Forms,
  FontBuildFrm in 'FontBuildFrm.pas' {FontBuildForm};

{$R *.res}


begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFontBuildForm, FontBuildForm);
  Application.Run;
end.
