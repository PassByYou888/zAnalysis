program MemoryRasterScale;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  RasterFrm in 'RasterFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
