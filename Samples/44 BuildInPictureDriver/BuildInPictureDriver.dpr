program BuildInPictureDriver;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  BuildInPictureDriverFrm in 'BuildInPictureDriverFrm.pas' {BuildInPictureDriverForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TBuildInPictureDriverForm, BuildInPictureDriverForm);
  Application.Run;
end.
