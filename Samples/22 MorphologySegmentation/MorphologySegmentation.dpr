program MorphologySegmentation;

uses
  System.StartUpCopy,
  FMX.Forms,
  MorphologySegmentationMainFrm in 'MorphologySegmentationMainFrm.pas' {MorphologySegmentationMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMorphologySegmentationMainForm, MorphologySegmentationMainForm);
  Application.Run;
end.
