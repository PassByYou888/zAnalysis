unit FontBuildFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  System.Math, Threading,
  MemoryStream64, MemoryRaster, Geometry2DUnit, DoStatusIO, PascalStrings, CoreClasses, GR32;

type
  TFontBuildForm = class(TForm)
    FontDialog: TFontDialog;
    BuildButton: TButton;
    SaveButton: TButton;
    Image: TImage;
    Memo: TMemo;
    SetFontButton: TButton;
    ProgressBar: TProgressBar;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    LoadButton: TButton;
    AATrackBar: TTrackBar;
    Label1: TLabel;
    gbkCheckBox: TCheckBox;
    SampleMemo: TMemo;
    ExportBMPButton: TButton;
    bmpSaveDialog: TSaveDialog;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure ExportBMPButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SampleMemoChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SetFontButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FR: TFontRaster;
    procedure DoStatus_Backcall(AText: SystemString; const ID: Integer);
  end;

var
  FontBuildForm: TFontBuildForm;

implementation

{$R *.dfm}


procedure TFontBuildForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(FR);
  DeleteDoStatusHook(Self);
end;

procedure TFontBuildForm.DoStatus_Backcall(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
  Application.ProcessMessages;
end;

procedure TFontBuildForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatus_Backcall);
  FR := TFontRaster.Create;
  SampleMemo.Font.Assign(FontDialog.Font);
  SampleMemo.ParentColor := True;
end;

function computeYHead(R: TMemoryRaster): Integer; inline;
var
  X, Y: Integer;
begin
  for X := 1 to R.width - 1 do
    begin
      for Y := R.height - 1 downto 0 do
        if R.PixelAlpha[X, Y] >= 200 then
            Break;
      if Y > 0 then
          Exit(X - 1);
    end;
  Exit(0);
end;

function computeYTail(R: TMemoryRaster; StartP: Integer): Integer; inline;
var
  X, Y: Integer;
begin
  for X := R.width - 1 downto 0 do
    begin
      for Y := R.height - 1 downto 0 do
        if R.PixelAlpha[X, Y] >= 200 then
            Break;
      if Y > 0 then
          Exit(X + 1);
    end;
  Exit(StartP);
end;

procedure TFontBuildForm.BuildButtonClick(Sender: TObject);
  procedure DoFor(pass: Integer);
  var
    Gr: TBitmap32;
    isiz: TSize;
    n, N2: TMemoryRaster;
    Bx, EX: Integer;
  begin
    Gr := TBitmap32.Create;
    Gr.Font.Assign(FontDialog.Font);
    isiz := Gr.TextExtentW(WideChar(pass));
    if (isiz.Cx > 0) and (isiz.Cy > 0) then
      begin
        Gr.SetSize(Max(Gr.width, isiz.Cx * 2), Max(Gr.height, isiz.Cy + 1));
        Gr.Clear(0);
      end;

    if (Gr.width > 0) and (Gr.height > 0) then
      begin
        Gr.RenderTextW(1, 1, WideChar(pass), AATrackBar.Position, Color32($FF, $FF, $FF, $FF));

        N2 := TMemoryRaster.Create;
        N2.DrawMode := MemoryRaster.TDrawMode.dmBlend;
        N2.SetWorkMemory(Gr.Bits, Gr.width, Gr.height);
        if pass = 32 then
          begin
            Bx := 1;
            EX := Gr.width;
          end
        else
          begin
            Bx := computeYHead(N2);
            EX := computeYTail(N2, Gr.width);
          end;
        if EX - Bx > 0 then
          begin
            n := TMemoryRaster.Create;
            n.SetSize(EX - Bx, Gr.height, RasterColor(0, 0, 0, 0));
            N2.DrawTo(n, 0, 0, Rect(Bx, 0, EX, Gr.height));
            LockObject(FR);
            FR.Add(WideChar(pass), n);
            UnLockObject(FR);
          end;
        DisposeObject(N2);
      end;
    DisposeObject(Gr);
    ProgressBar.Position := pass;
    Application.ProcessMessages;
  end;

var
  m64: TMemoryStream64;
  ngr: TBitmap32;
  raster: TMemoryRaster;
  i: Integer;
begin
  FR.Clear;
  DoStatus('raster Sampler...');

  BuildButton.Enabled := False;
  SaveButton.Enabled := False;
  SetFontButton.Enabled := False;
  LoadButton.Enabled := False;
  ExportBMPButton.Enabled := False;
  AATrackBar.Enabled := False;
  gbkCheckBox.Enabled := False;

  ProgressBar.Max := $FFFF;
  ProgressBar.Min := $0;

  for i := ProgressBar.Min to ProgressBar.Max do
    begin
      if (i <= $FF) then
          DoFor(i)
      else if (gbkCheckBox.Checked) then
          DoFor(i);
    end;

  ProgressBar.Position := 0;

  FR.Build(FontDialog.Font.Size);

  DoStatus('Activted Word:%d', [FR.ActivtedWord]);
  DoStatus('size: %d x %d ', [FR.width, FR.height]);

  BuildButton.Enabled := True;
  SaveButton.Enabled := True;
  SetFontButton.Enabled := True;
  LoadButton.Enabled := True;
  ExportBMPButton.Enabled := True;
  AATrackBar.Enabled := True;
  gbkCheckBox.Enabled := True;

  ngr := TBitmap32.Create;
  ngr.SetSize(Image.width, Image.height);
  ngr.Clear(Color32(0, 0, 0, 0));
  raster := TMemoryRaster.Create;
  raster.DrawMode := MemoryRaster.TDrawMode.dmBlend;
  raster.SetWorkMemory(ngr.Bits, ngr.width, ngr.height);
  FR.Draw(SampleMemo.Text, raster, vec2(0, 0), RasterColorF(1, 0, 0, 1));

  m64 := TMemoryStream64.Create;
  ngr.SaveToStream(m64);
  m64.Position := 0;
  Image.Picture.LoadFromStream(m64);
  DisposeObject(m64);
  DisposeObject(raster);
  DisposeObject(ngr);
end;

procedure TFontBuildForm.ExportBMPButtonClick(Sender: TObject);
var
  m64: TMemoryStream64;
begin
  if not bmpSaveDialog.Execute then
      Exit;
  m64 := TMemoryStream64.Create;
  FR.ExportRaster(m64, True);
  m64.SaveToFile(bmpSaveDialog.FileName);
  m64.Clear;

  DisposeObject(m64);
end;

procedure TFontBuildForm.LoadButtonClick(Sender: TObject);
var
  m64: TMemoryStream64;
  ngr: TBitmap32;
  raster: TMemoryRaster;
begin
  if not OpenDialog.Execute then
      Exit;
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(OpenDialog.FileName);
  m64.Position := 0;
  FR.LoadFromStream(m64);
  DisposeObject(m64);

  DoStatus('Activted Word:%d', [FR.ActivtedWord]);
  DoStatus('size: %d x %d ', [FR.width, FR.height]);

  ngr := TBitmap32.Create;
  ngr.SetSize(Image.width, Image.height);
  ngr.Clear(Color32(0, 0, 0, 0));
  raster := TMemoryRaster.Create;
  raster.DrawMode := MemoryRaster.TDrawMode.dmBlend;
  raster.SetWorkMemory(ngr.Bits, ngr.width, ngr.height);
  FR.Draw(SampleMemo.Text, raster, vec2(0, 0), RasterColorF(1, 0, 0, 1));

  m64 := TMemoryStream64.Create;
  ngr.SaveToStream(m64);
  m64.Position := 0;
  Image.Picture.LoadFromStream(m64);
  DisposeObject(m64);
  DisposeObject(raster);
  DisposeObject(ngr);
end;

procedure TFontBuildForm.SampleMemoChange(Sender: TObject);
var
  m64: TMemoryStream64;
  ngr: TBitmap32;
  raster: TMemoryRaster;
begin
  ngr := TBitmap32.Create;
  ngr.SetSize(Image.width, Image.height);
  ngr.Clear(Color32(0, 0, 0, 0));
  raster := TMemoryRaster.Create;
  raster.DrawMode := MemoryRaster.TDrawMode.dmBlend;
  raster.SetWorkMemory(ngr.Bits, ngr.width, ngr.height);
  FR.Draw(SampleMemo.Text, raster, vec2(0, 0), RasterColorF(1, 0, 0, 1));

  m64 := TMemoryStream64.Create;
  ngr.SaveToStream(m64);
  m64.Position := 0;
  Image.Picture.LoadFromStream(m64);
  DisposeObject(m64);
  DisposeObject(raster);
  DisposeObject(ngr);
end;

procedure TFontBuildForm.SaveButtonClick(Sender: TObject);
var
  m64: TMemoryStream64;
begin
  if not SaveDialog.Execute then
      Exit;
  m64 := TMemoryStream64.Create;
  FR.SaveToStream(m64);
  m64.SaveToFile(SaveDialog.FileName);
  DisposeObject(m64);
end;

procedure TFontBuildForm.SetFontButtonClick(Sender: TObject);
begin
  if not FontDialog.Execute then
      Exit;
  SampleMemo.Font.Assign(FontDialog.Font);
  SampleMemo.ParentColor := True;
end;

end. 
