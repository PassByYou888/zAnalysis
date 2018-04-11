program GUIAnalysis;

uses
  System.StartUpCopy,
  FMX.Forms,
  GUIAnalysisMainFrm in 'GUIAnalysisMainFrm.pas' {GUIAnalysisMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGUIAnalysisMainForm, GUIAnalysisMainForm);
  Application.Run;
end.
