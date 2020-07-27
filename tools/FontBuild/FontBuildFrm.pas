unit FontBuildFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  System.Math, System.Threading,
  FastGBK, GBK,
  MemoryStream64, MemoryRaster, Geometry2DUnit, DoStatusIO, PascalStrings, UPascalStrings, CoreClasses, GR32, zDrawEngine;

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
    IncludeallCheckBox: TCheckBox;
    SampleMemo: TMemo;
    ExportBMPButton: TButton;
    bmpSaveDialog: TSaveDialog;
    IncludeGBKCheckBox: TCheckBox;
    Timer1: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure ExportBMPButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SampleMemoChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SetFontButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FR: TFontRaster;
    procedure DoStatus_Backcall(AText: SystemString; const ID: Integer);
    procedure UpdatePreview();
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

procedure TFontBuildForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatus_Backcall);
  FR := TFontRaster.Create;

  if ParamCount = 1 then
      FR.LoadFromFile(ParamStr(1))
  else
      FR.Assign(Wait_SystemFont_Init);

  DoStatus('Activted Word:%d', [FR.ActivtedWord]);
  DoStatus('size: %d x %d ', [FR.width, FR.height]);

  UpdatePreview;
end;

procedure TFontBuildForm.BuildButtonClick(Sender: TObject);

  function computeYHead(R: TMemoryRaster): Integer;
  var
    X, Y: Integer;
  begin
    for X := 1 to R.width - 1 do
      begin
        for Y := R.height - 1 downto 0 do
          if R.PixelAlpha[X, Y] > 0 then
              Break;
        if Y > 0 then
            Exit(Max(X - 1, 0));
      end;
    Exit(0);
  end;

  function computeYTail(R: TMemoryRaster; sp: Integer): Integer;
  var
    X, Y: Integer;
  begin
    for X := R.width - 1 downto 0 do
      begin
        for Y := R.height - 1 downto 0 do
          if R.PixelAlpha[X, Y] > 0 then
              Break;
        if Y > 0 then
            Exit(Min(X + 1, R.width));
      end;
    Exit(sp);
  end;

  procedure DoFor(index: Integer);
  var
    GR: TBitmap32;
    isiz: TSize;
    n, N2: TMemoryRaster;
    BX, EX: Integer;
  begin
    GR := TBitmap32.Create;
    GR.Font.Assign(FontDialog.Font);
    isiz := GR.TextExtentW(WideChar(index));
    if (isiz.Cx > 0) and (isiz.Cy > 0) then
      begin
        GR.SetSize(Max(GR.width, isiz.Cx * 2) + 10, Max(GR.height, isiz.Cy + 1) + 10);
        GR.Clear(0);
      end;

    if (GR.width > 0) and (GR.height > 0) then
      begin
        LockObject(FR);
        GR.RenderTextW(10, 10, WideChar(index), AATrackBar.Position, Color32($FF, $FF, $FF, $FF));
        UnlockObject(FR);

        N2 := TMemoryRaster.Create;
        N2.DrawMode := MemoryRaster.TDrawMode.dmBlend;
        N2.SetWorkMemory(GR.Bits, GR.width, GR.height);
        if index = 32 then
          begin
            BX := 1;
            EX := isiz.Cx;
          end
        else
          begin
            BX := computeYHead(N2);
            EX := computeYTail(N2, GR.width);
          end;
        if EX - BX > 0 then
          begin
            n := TMemoryRaster.Create;
            n.SetSize(EX - BX, GR.height, RasterColor(0, 0, 0, 0));
            N2.DrawTo(n, 0, 0, Rect(BX, 0, EX, GR.height));
            LockObject(FR);
            FR.Add(WideChar(index), n);
            UnlockObject(FR);
          end;
        DisposeObject(N2);
      end;
    DisposeObject(GR);
  end;

var
  pass: Integer;
begin
  FR.Clear;
  DoStatus('raster Sampler...');

  BuildButton.Enabled := False;
  SaveButton.Enabled := False;
  SetFontButton.Enabled := False;
  LoadButton.Enabled := False;
  ExportBMPButton.Enabled := False;
  AATrackBar.Enabled := False;
  IncludeallCheckBox.Enabled := False;
  IncludeGBKCheckBox.Enabled := False;

  ProgressBar.Max := $FFFF;
  ProgressBar.Min := $0;

  for pass := ProgressBar.Min to ProgressBar.Max do
    begin
      if IfGBKChar(USystemChar(pass), True, IncludeGBKCheckBox.Checked, IncludeallCheckBox.Checked) then
          DoFor(pass);
      ProgressBar.Position := pass;
      Application.ProcessMessages;
    end;

  ProgressBar.Position := 0;

  FR.Build(FontDialog.Font.Name, FontDialog.Font.Size, True);

  DoStatus('Activted Word:%d', [FR.ActivtedWord]);
  DoStatus('size: %d x %d ', [FR.width, FR.height]);

  BuildButton.Enabled := True;
  SaveButton.Enabled := True;
  SetFontButton.Enabled := True;
  LoadButton.Enabled := True;
  ExportBMPButton.Enabled := True;
  AATrackBar.Enabled := True;
  IncludeallCheckBox.Enabled := True;
  IncludeGBKCheckBox.Enabled := True;

  UpdatePreview;
end;

procedure TFontBuildForm.ExportBMPButtonClick(Sender: TObject);
var
  m64: TMemoryStream64;
begin
  if not bmpSaveDialog.Execute then
      Exit;
  m64 := TMemoryStream64.Create;
  FR.ExportRaster(m64, False);
  m64.SaveToFile(bmpSaveDialog.FileName);
  m64.Clear;

  DisposeObject(m64);
end;

procedure TFontBuildForm.LoadButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
      Exit;
  FR.LoadFromFile(OpenDialog.FileName);

  DoStatus('Activted Word:%d', [FR.ActivtedWord]);
  DoStatus('size: %d x %d ', [FR.width, FR.height]);

  UpdatePreview;
end;

procedure TFontBuildForm.SampleMemoChange(Sender: TObject);
begin
  UpdatePreview;
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
end;

procedure TFontBuildForm.Timer1Timer(Sender: TObject);
begin
  DoStatus();
end;

procedure TFontBuildForm.DoStatus_Backcall(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TFontBuildForm.UpdatePreview;
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
  // map memory for TBitmap32
  raster.SetWorkMemory(ngr.Bits, ngr.width, ngr.height);
  raster.Font := FR;
  raster.DrawEngine.DrawText(SampleMemo.Text, FR.FontSize, raster.DrawEngine.ScreenRect, DEColor(1, 1, 1), True);
  raster.DrawEngine.Flush;

  m64 := TMemoryStream64.Create;
  ngr.SaveToStream(m64);
  m64.Position := 0;
  Image.Picture.LoadFromStream(m64);
  DisposeObject(m64);
  DisposeObject(raster);
  DisposeObject(ngr);
end;

end.
