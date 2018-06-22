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
    fr: TFontRaster;
    procedure DoStatus_Backcall(AText: SystemString; const ID: Integer);
  end;

var
  FontBuildForm: TFontBuildForm;

implementation

{$R *.dfm}


procedure TFontBuildForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(fr);
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
  fr := TFontRaster.Create;
  SampleMemo.Font.Assign(FontDialog.Font);
  SampleMemo.ParentColor := True;
end;

function computeYHead(r: TMemoryRaster): Integer; inline;
var
  x, y: Integer;
begin
  for x := 1 to r.Width - 1 do
    begin
      for y := r.Height - 1 downto 0 do
        if r.PixelAlpha[x, y] >= 200 then
            break;
      if y > 0 then
          exit(x - 1);
    end;
  exit(0);
end;

function computeYTail(r: TMemoryRaster; startP: Integer): Integer; inline;
var
  x, y: Integer;
begin
  for x := r.Width - 1 downto 0 do
    begin
      for y := r.Height - 1 downto 0 do
        if r.PixelAlpha[x, y] >= 200 then
            break;
      if y > 0 then
          exit(x + 1);
    end;
  exit(startP);
end;

procedure TFontBuildForm.BuildButtonClick(Sender: TObject);
  procedure dofor(pass: Integer);
  var
    gr: TBitmap32;
    isiz: TSize;
    n, n2: TMemoryRaster;
    bx, ex: Integer;
  begin
    gr := TBitmap32.Create;
    gr.Font.Assign(FontDialog.Font);
    isiz := gr.TextExtentW(WideChar(pass));
    if (isiz.cx > 0) and (isiz.cy > 0) then
      begin
        gr.SetSize(Max(gr.Width, isiz.cx * 2), Max(gr.Height, isiz.cy + 1));
        gr.Clear(0);
      end;

    if (gr.Width > 0) and (gr.Height > 0) then
      begin
        gr.RenderTextW(1, 1, WideChar(pass), AATrackBar.Position, Color32($FF, $FF, $FF, $FF));

        n2 := TMemoryRaster.Create;
        n2.DrawMode := MemoryRaster.TDrawMode.dmBlend;
        n2.SetWorkMemory(gr.Bits, gr.Width, gr.Height);
        if pass = 32 then
          begin
            bx := 1;
            ex := gr.Width;
          end
        else
          begin
            bx := computeYHead(n2);
            ex := computeYTail(n2, gr.Width);
          end;
        if ex - bx > 0 then
          begin
            n := TMemoryRaster.Create;
            n.SetSize(ex - bx, gr.Height, RasterColor(0, 0, 0, 0));
            n2.DrawTo(n, 0, 0, Rect(bx, 0, ex, gr.Height));
            lockobject(fr);
            fr.Add(WideChar(pass), n);
            unlockobject(fr);
          end;
        DisposeObject(n2);
      end;
    DisposeObject(gr);
    ProgressBar.Position := pass;
    Application.ProcessMessages;
  end;

var
  m64: TMemoryStream64;
  ngr: TBitmap32;
  raster: TMemoryRaster;
  i: Integer;
begin
  fr.Clear;
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
          dofor(i)
      else if (gbkCheckBox.Checked) then
          dofor(i);
    end;

  ProgressBar.Position := 0;

  fr.Build(FontDialog.Font.Size);

  DoStatus('Activted Word:%d', [fr.ActivtedWord]);
  DoStatus('size: %d x %d ', [fr.Width, fr.Height]);

  BuildButton.Enabled := True;
  SaveButton.Enabled := True;
  SetFontButton.Enabled := True;
  LoadButton.Enabled := True;
  ExportBMPButton.Enabled := True;
  AATrackBar.Enabled := True;
  gbkCheckBox.Enabled := True;

  ngr := TBitmap32.Create;
  ngr.SetSize(Image.Width, Image.Height);
  ngr.Clear(Color32(0, 0, 0, 0));
  raster := TMemoryRaster.Create;
  raster.DrawMode := MemoryRaster.TDrawMode.dmBlend;
  raster.SetWorkMemory(ngr.Bits, ngr.Width, ngr.Height);
  fr.Draw(SampleMemo.Text, raster, Vec2(0, 0), RasterColorF(1, 0, 0, 1));

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
      exit;
  m64 := TMemoryStream64.Create;
  fr.ExportRaster(m64, True);
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
      exit;
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(OpenDialog.FileName);
  m64.Position := 0;
  fr.LoadFromStream(m64);
  DisposeObject(m64);

  DoStatus('Activted Word:%d', [fr.ActivtedWord]);
  DoStatus('size: %d x %d ', [fr.Width, fr.Height]);

  ngr := TBitmap32.Create;
  ngr.SetSize(Image.Width, Image.Height);
  ngr.Clear(Color32(0, 0, 0, 0));
  raster := TMemoryRaster.Create;
  raster.DrawMode := MemoryRaster.TDrawMode.dmBlend;
  raster.SetWorkMemory(ngr.Bits, ngr.Width, ngr.Height);
  fr.Draw(SampleMemo.Text, raster, Vec2(0, 0), RasterColorF(1, 0, 0, 1));

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
  ngr.SetSize(Image.Width, Image.Height);
  ngr.Clear(Color32(0, 0, 0, 0));
  raster := TMemoryRaster.Create;
  raster.DrawMode := MemoryRaster.TDrawMode.dmBlend;
  raster.SetWorkMemory(ngr.Bits, ngr.Width, ngr.Height);
  fr.Draw(SampleMemo.Text, raster, Vec2(0, 0), RasterColorF(1, 0, 0, 1));

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
      exit;
  m64 := TMemoryStream64.Create;
  fr.SaveToStream(m64);
  m64.SaveToFile(SaveDialog.FileName);
  DisposeObject(m64);
end;

procedure TFontBuildForm.SetFontButtonClick(Sender: TObject);
begin
  if not FontDialog.Execute then
      exit;
  SampleMemo.Font.Assign(FontDialog.Font);
  SampleMemo.ParentColor := True;
end;

end.
