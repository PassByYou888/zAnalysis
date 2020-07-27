program ImgFmtConver2TIF;

{$APPTYPE CONSOLE}


uses
  Vcl.Forms,
  ImgFmtConverTIFFrm in 'ImgFmtConverTIFFrm.pas' {ImgFmtConverTIFForm} ,
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

var
  errorNo: Integer;

begin
  ExitCode := 1;
  if FillParam(errorNo) then
    begin
      if errorNo = 0 then
          ExitCode := 0;
      exit;
    end;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Title := 'image to .PNG';
  Application.CreateForm(TImgFmtConverTIFForm, ImgFmtConverTIFForm);
  Application.Run;

end.
