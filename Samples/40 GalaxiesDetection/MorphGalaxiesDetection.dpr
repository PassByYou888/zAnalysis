program MorphGalaxiesDetection;

uses
  System.StartUpCopy,
  FMX.Forms,
  MorphGalaxiesDetectionFrm in 'MorphGalaxiesDetectionFrm.pas' {MorphGalaxiesDetectionForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMorphGalaxiesDetectionForm, MorphGalaxiesDetectionForm);
  Application.Run;
end.
