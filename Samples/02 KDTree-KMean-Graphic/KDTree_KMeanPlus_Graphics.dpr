program KDTree_KMeanPlus_Graphics;

uses
  System.StartUpCopy,
  FMX.Forms,
  KDTree_KMeanPlus_GUIMainFrm in 'KDTree_KMeanPlus_GUIMainFrm.pas' {KDTree_KMeanPlus_GUIMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TKDTree_KMeanPlus_GUIMainForm, KDTree_KMeanPlus_GUIMainForm);
  Application.Run;
end.
