program FMX_FontBuild;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX_FontBuildFrm in 'FMX_FontBuildFrm.pas' {FMX_FontBuildForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMX_FontBuildForm, FMX_FontBuildForm);
  Application.Run;
end.
