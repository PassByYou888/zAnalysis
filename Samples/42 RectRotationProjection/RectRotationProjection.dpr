program RectRotationProjection;

uses
  System.StartUpCopy,
  FMX.Forms,
  RectRotationProjectionFrm in 'RectRotationProjectionFrm.pas' {RectRotationProjectionForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRectRotationProjectionForm, RectRotationProjectionForm);
  Application.Run;
end.
