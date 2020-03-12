program ApproximatePolygon;

uses
  System.StartUpCopy,
  FMX.Forms,
  ApproximatePolygonFrm in 'ApproximatePolygonFrm.pas' {ApproximatePolygonForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TApproximatePolygonForm, ApproximatePolygonForm);
  Application.Run;
end.
