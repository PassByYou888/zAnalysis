program ParallelProjection;

uses
  Vcl.Forms,
  ParallelProjectionFrm in 'ParallelProjectionFrm.pas' {ParallelProjectionForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TParallelProjectionForm, ParallelProjectionForm);
  Application.Run;
end.
