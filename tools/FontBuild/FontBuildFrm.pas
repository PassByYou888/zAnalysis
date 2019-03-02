unit FontBuildFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  System.Math, Threading, FastGBK,
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
    IncludeallCheckBox: TCheckBox;
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

function IsChineseText(const AStr: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
  const CheckAllChar: Boolean;
  const IncludeCharacters: Boolean = True;
  const IncludeRadicals: Boolean = False;
  const IncludePUAParts: Boolean = False;
  const IncludeStrokes: Boolean = False;
  const IncludePhoneticNotation: Boolean = False;
  const IncludeAllCharacters: Boolean = False): Boolean;
var
  I, F, C: Integer;
  IsHasNext,
    IsNoCheckNext,
    IsFound,
    IsAllTrue: Boolean;
  UTF32: DWORD;
begin
  Result := False;
  IsAllTrue := Length(AStr) <> 0;
{$IFDEF UNICODE}
  F := Low(AStr);
  C := High(AStr);
{$ELSE}
  F := 1;
  C := Length(AStr);
{$ENDIF}
  IsNoCheckNext := False;
  for I := F to C do
    begin
      IsHasNext := I < C;
      if IsNoCheckNext then
          Continue;
      IsNoCheckNext := False;
      if ((AStr[I] >= #$D800) and (AStr[I] <= #$DFFF)) and IsHasNext then
        begin
          UTF32 := (Cardinal(AStr[I]) and $000003FF) shl 10 or (Cardinal(AStr[I + 1]) and $000003FF) + $00010000;
          IsNoCheckNext := True;
        end
      else
          UTF32 := Ord(AStr[I]);
      // https://www.qqxiuzi.cn/zh/hanzi-unicode-bianma.php
      // ×Ö·û¼¯	×ÖÊý	Unicode ±àÂë
      // »ù±¾ºº×Ö	20902×Ö	4E00-9FA5
      // »ù±¾ºº×Ö²¹³ä	38×Ö	9FA6-9FCB
      // À©Õ¹A	6582×Ö	3400-4DB5
      // À©Õ¹B	42711×Ö	20000-2A6D6
      // À©Õ¹C	4149×Ö	2A700-2B734
      // À©Õ¹D	222×Ö	2B740-2B81D
      // ¿µÎõ²¿Ê×	214×Ö	2F00-2FD5
      // ²¿Ê×À©Õ¹	115×Ö	2E80-2EF3
      // ¼æÈÝºº×Ö	477×Ö	F900-FAD9
      // ¼æÈÝÀ©Õ¹	542×Ö	2F800-2FA1D
      // PUA(GBK)²¿¼þ	81×Ö	E815-E86F
      // ²¿¼þÀ©Õ¹	452×Ö	E400-E5E8
      // PUAÔö²¹	207×Ö	E600-E6CF
      // ºº×Ö±Ê»­	36×Ö	31C0-31E3
      // ºº×Ö½á¹¹	12×Ö	2FF0-2FFB
      // ººÓï×¢Òô	22×Ö	3105-3120
      // ×¢ÒôÀ©Õ¹	22×Ö	31A0-31BA
      // ©–	1×Ö	3007
      // http://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=UnicodeHexCode
      IsFound := False;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $4E00) and (UTF32 <= $9FA5);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $9FA6) and (UTF32 <= $9FCB);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $3400) and (UTF32 <= $4DB5);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $20000) and (UTF32 <= $2A6D6);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $2A700) and (UTF32 <= $2B734);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $2B740) and (UTF32 <= $2B81D);
        end;
      if (not IsFound) and IncludeRadicals then
        begin
          IsFound := (UTF32 >= $2F00) and (UTF32 <= $2FD5);
        end;
      if (not IsFound) and IncludeRadicals then
        begin
          IsFound := (UTF32 >= $2E80) and (UTF32 <= $2EF3);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $F900) and (UTF32 <= $FAD9);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 >= $2F800) and (UTF32 <= $2FA1D);
        end;
      if (not IsFound) and IncludePUAParts then
        begin
          IsFound := (UTF32 >= $E815) and (UTF32 <= $E86F);
        end;
      if (not IsFound) and IncludePUAParts then
        begin
          IsFound := (UTF32 >= $E400) and (UTF32 <= $E5E8);
        end;
      if (not IsFound) and IncludePUAParts then
        begin
          IsFound := (UTF32 >= $E600) and (UTF32 <= $E6CF);
        end;
      if (not IsFound) and IncludeStrokes then
        begin
          IsFound := (UTF32 >= $31C0) and (UTF32 <= $31E3);
        end;
      if (not IsFound) and IncludeStrokes then
        begin
          IsFound := (UTF32 >= $2FF0) and (UTF32 <= $2FFB);
        end;
      if (not IsFound) and IncludePhoneticNotation then
        begin
          IsFound := (UTF32 >= $3105) and (UTF32 <= $3120);
        end;
      if (not IsFound) and IncludePhoneticNotation then
        begin
          IsFound := (UTF32 >= $31A0) and (UTF32 <= $31BA);
        end;
      if (not IsFound) and IncludeCharacters then
        begin
          IsFound := (UTF32 = $3007)
        end;
      if (not IsFound) and IncludeAllCharacters then
        begin
          IsFound := (UTF32 >= $0391) and (UTF32 <= $FFE5);
        end;

      if IsFound then
        begin
          if not CheckAllChar then
            begin
              Result := True;
              Break;
            end;
        end
      else
          IsAllTrue := False;
    end;
  if CheckAllChar then
      Result := IsAllTrue;
end;

procedure TFontBuildForm.BuildButtonClick(Sender: TObject);
  procedure DoFor(index: Integer);
  var
    Gr: TBitmap32;
    isiz: TSize;
    n, N2: TMemoryRaster;
    Bx, EX: Integer;
  begin
    Gr := TBitmap32.Create;
    Gr.Font.Assign(FontDialog.Font);
    isiz := Gr.TextExtentW(WideChar(index));
    if (isiz.Cx > 0) and (isiz.Cy > 0) then
      begin
        Gr.SetSize(Max(Gr.width, isiz.Cx * 2), Max(Gr.height, isiz.Cy + 1));
        Gr.Clear(0);
      end;

    if (Gr.width > 0) and (Gr.height > 0) then
      begin
        LockObject(FR);
        Gr.RenderTextW(1, 1, WideChar(index), AATrackBar.Position, Color32($FF, $FF, $FF, $FF));
        UnlockObject(FR);

        N2 := TMemoryRaster.Create;
        N2.DrawMode := MemoryRaster.TDrawMode.dmBlend;
        N2.SetWorkMemory(Gr.Bits, Gr.width, Gr.height);
        if index = 32 then
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
            FR.Add(WideChar(index), n);
            UnlockObject(FR);
          end;
        DisposeObject(N2);
      end;
    DisposeObject(Gr);

    ProgressBar.Position := index;
    Application.ProcessMessages;
  end;

var
  m64: TMemoryStream64;
  ngr: TBitmap32;
  raster: TMemoryRaster;
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

  ProgressBar.Max := $FFFF;
  ProgressBar.Min := $0;

  for pass := ProgressBar.Min to ProgressBar.Max do
    begin
      if (pass <= $FF) then
          DoFor(pass)
      else if FastGBKChar(SystemChar(pass)) then
          DoFor(pass)
      else if CharIn(SystemChar(pass), '¡¤£±£²£³£´£µ£¶£·£¸£¹£°£­£½¡¾¡¿£Ü£»¡¯£¬¡£¡¢¡«£¡£À£££¤£¥¡­¡­£¦¡Á£¨£©¡ª¡ª£«£û£ý£ü£º¡°¡¶¡·£¿') then
          DoFor(pass)
      else if (IncludeallCheckBox.Checked) then
          DoFor(pass);
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
  IncludeallCheckBox.Enabled := True;

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
