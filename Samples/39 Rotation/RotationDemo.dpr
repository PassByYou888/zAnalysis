program RotationDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  RotationFrm in 'RotationFrm.pas' {ApproximatePolygonForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TApproximatePolygonForm, ApproximatePolygonForm);
  Application.Run;
end.
