program BitmapConver;

uses
  System.StartUpCopy,
  FMX.Forms,
  BMPConverFrm in 'BMPConverFrm.pas' {BMPConverForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBMPConverForm, BMPConverForm);
  Application.Run;
end.
