program ImgFmtConver2PNG;

uses
  Vcl.Forms,
  ImgFmtConverFrm in 'ImgFmtConverFrm.pas' {ImgFmtConverForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TImgFmtConverForm, ImgFmtConverForm);
  Application.Run;
end.


