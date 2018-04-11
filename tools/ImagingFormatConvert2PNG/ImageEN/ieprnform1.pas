(*
Copyright (c) 1998-2014 by Carlotta Calandra. All rights reserved.
Copyright (c) 2011-2014 by Xequte software.

This software comes without express or implied warranty.
In no case shall the author be liable for any damage or unwanted behavior of any
computer hardware and/or software.

Author grants you the right to include the component
in your application, whether COMMERCIAL, SHAREWARE, or FREEWARE.

ImageEn, IEvolution and ImageEn ActiveX may not be included in any
commercial, shareware or freeware libraries or components.

www.ImageEn.com
*)

(*
File version 1007
*)

unit ieprnform1;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEPRINTDIALOGS}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ImageEnView, ImageEnIO, ComCtrls,
  ExtCtrls, Menus, ieview, hyiedefs, hyieutils, imageenproc, ievect,
  ImgList;

type
  TfiePrnForm1 = class(TForm)
    PrinterSetupDialog1: TPrinterSetupDialog;
    ImageEnView1: TImageEnView;
    Panel1: TPanel;
    imlPositions: TImageList;
    grpSize: TGroupBox;
    grpMargins: TGroupBox;
    grpOther: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    lblPosition: TLabel;
    cmbPosition: TComboBox;
    Edit7: TEdit;
    UpDown7: TUpDown;
    lblGamma: TLabel;
    lblSize: TLabel;
    ComboBox1: TComboBox;
    edtWidth: TEdit;
    updWidth: TUpDown;
    edtHeight: TEdit;
    updHeight: TUpDown;
    lblByX: TLabel;
    btnPrint: TBitBtn;
    btnSetup: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnSetupClick(Sender: TObject);
    procedure cmbPositionDrawItem(Control: TWinControl; Index: Integer; Rect:
        TRect; State: TOwnerDrawState);
    procedure PrintPreview(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton12Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown4Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown7Click(Sender: TObject; Button: TUDBtnType);
    procedure updWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure updHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    procedure incdecmargins(text: TEdit; Button: TUDBtnType);
    function XMult: double;
  public
    { Public declarations }
    fDialogsMeasureUnit: TIEDialogsMeasureUnit;
    io: TImageEnIO;
    fTaskName: string;
    fPrintPreviewParams: TIOPrintPreviewParams;
    PrintAnnotations: boolean;
    activating: boolean;
    procedure UpdateLanguage();
    procedure LoadParameters;
    procedure SaveParameters;    

    {$IFDEF UNIT_TESTING}
    procedure SetNextLanguage;
    {$ENDIF}
  end;


implementation

uses printers, iesettings;


{$R *.DFM}


procedure TfiePrnForm1.btnSetupClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  PrintPreview(self);
end;

{$IFDEF UNIT_TESTING}
procedure tfiePrnForm1.SetNextLanguage;
begin
  Self.Tag := Self.Tag + 1;
  if Self.tag > ord(msArabic) then
    Self.Tag := 0;
  IEGlobalSettings().MsgLanguage := TMsgLanguage(Self.Tag);
  UpdateLanguage();
end;
{$ENDIF}

procedure tfiePrnForm1.LoadParameters;
begin
  // get parameters from fPrintPreviewParams
  Edit1.Text := FloatToStrF(fPrintPreviewParams.MarginTop, ffGeneral, 4, 4);
  Edit2.Text := FloatToStrF(fPrintPreviewParams.MarginLeft, ffGeneral, 4, 4);
  Edit3.Text := FloatToStrF(fPrintPreviewParams.MarginRight, ffGeneral, 4, 4);
  Edit4.Text := FloatToStrF(fPrintPreviewParams.MarginBottom, ffGeneral, 4, 4); 
  cmbPosition.ItemIndex := ord(fPrintPreviewParams.Position);
  ComboBox1.ItemIndex := integer(fPrintPreviewParams.Size);
  if fPrintPreviewParams.Size=psSpecifiedSize then
  begin
    edtWidth  .Enabled := true;
    edtHeight .Enabled := true;
    updWidth  .Enabled := true;
    updHeight .Enabled := true;
    lblByX    .Enabled := true;
  end;

  if fPrintPreviewParams.Width<=0 then
    edtWidth.Text := FloatToStrF( (io.IEBitmap.Width/IO.Params.DpiX)*xmult , ffGeneral, 4, 4)
  else
    edtWidth.Text := FloatToStrF(fPrintPreviewParams.Width, ffGeneral, 4, 4);

  if fPrintPreviewParams.Height<=0 then
    edtHeight.Text := FloatToStrF( (io.IEBitmap.Height/io.Params.DpiY)*xmult, ffGeneral, 4, 4)
  else
    edtHeight.Text := FloatToStrF(fPrintPreviewParams.Height, ffGeneral, 4, 4);

  Edit7.Text := FloatToStrF(fPrintPreviewParams.Gamma, ffGeneral, 4, 4);
end;

procedure tfiePrnForm1.SaveParameters;
begin
  // put parameters to fPrintPreviewParams
  fPrintPreviewParams.MarginTop    := IEStrToFloatDefS(Edit1.Text, 1);
  fPrintPreviewParams.MarginLeft   := IEStrToFloatDefS(Edit2.Text, 1);
  fPrintPreviewParams.MarginRight  := IEStrToFloatDefS(Edit3.Text, 1);
  fPrintPreviewParams.MarginBottom := IEStrToFloatDefS(Edit4.Text, 1);
  fPrintPreviewParams.Position     := TIOPrintPreviewPosition(cmbPosition.ItemIndex);
  fPrintPreviewParams.Size         := TIOPrintPreviewSize(ComboBox1.ItemIndex);

  if (fPrintPreviewParams.Size=psSpecifiedSize) then
  begin
    fPrintPreviewParams.Width := IEStrToFloatDefS(edtWidth.Text, 6);
    fPrintPreviewParams.Height := IEStrToFloatDefS(edtHeight.Text, 4);
  end
  else
  begin
    fPrintPreviewParams.Width := -1;
    fPrintPreviewParams.Height := -1;
  end;

  fPrintPreviewParams.Gamma := IEStrToFloatDefS(Edit7.Text, 1);
end;


procedure tfiePrnForm1.FormActivate(Sender: TObject);
begin
  Application.ProcessMessages; // first draws all controls (to avoid "Swiss Cheese")
  activating := True;

  if IEGlobalSettings().UseButtonGlyphsInDialogs = False then
  begin
    btnPrint.Glyph := nil;
    btnSetup.Glyph := nil;
    btnCancel.Glyph := nil;
  end;
  LoadParameters;
  if ComboBox1.ItemIndex < 0 then
    ComboBox1.ItemIndex := 1;
  activating := false;
  PrintPreview(Sender);
end;

procedure TfiePrnForm1.cmbPositionDrawItem(Control: TWinControl; Index:
    Integer; Rect: TRect; State: TOwnerDrawState);
var
  iGlyph : Integer;
  sText  : string;
begin
  iGlyph := Index;
  sText  := cmbPosition.Items[Index];
  IEDrawComboListBoxItem(TCombobox(Control), Rect, sText, imlPositions, iGlyph);
end;
function tfiePrnForm1.XMult: double;
begin
  if (fDialogsMeasureUnit = ieduCm) or (fDialogsMeasureUnit = ieduSelectableDefCm) then
    xmult := CM_per_Inch
  else
    xmult := 1;
end;

// preview or print

procedure tfiePrnForm1.PrintPreview(Sender: TObject);
var
  VerticalPos: TIEVerticalPos;
  HorizontalPos: TIEHorizontalPos;
  Size: TIESize;
  MarginLeft, MarginTop, MarginRight, MarginBottom, SpecWidth, SpecHeight, GammaCorrection: double;
  ie: TImageEnVect;
  lc: TCursor;
begin
  if activating then
    exit;
  if (Width < 100) or (Height < 100) then
    exit; // to disallow AV

  VerticalPos := ievpCENTER;
  HorizontalPos := iehpCENTER;
  if TIOPrintPreviewPosition(cmbPosition.ItemIndex) in [ppTopLeft, ppTop, ppTopRight] then
    VerticalPos := ievpTOP;
  if TIOPrintPreviewPosition(cmbPosition.ItemIndex) in [ppBottomLeft, ppBottom, ppBottomRight] then
    VerticalPos := ievpBOTTOM;
  if TIOPrintPreviewPosition(cmbPosition.ItemIndex) in [ppTopLeft, ppLeft, ppBottomLeft] then
    HorizontalPos := iehpLEFT;
  if TIOPrintPreviewPosition(cmbPosition.ItemIndex) in [ppTopRight, ppRight, ppBottomRight] then
    HorizontalPos := iehpRIGHT;

  Size := iesFITTOPAGE;
  case ComboBox1.ItemIndex of
    0: Size := iesNORMAL;
    1: Size := iesFITTOPAGE;
    2: Size := iesFITTOPAGESTRETCH;
    3: Size := iesFILLPAGE;
    4: Size := iesSPECIFIEDSIZE;
  end;
  MarginLeft := IEStrToFloatDefS(Edit2.Text, 0) / xmult;
  MarginTop := IEStrToFloatDefS(Edit1.Text, 0) / xmult;
  MarginRight := IEStrToFloatDefS(Edit3.Text, 0) / xmult;
  MarginBottom := IEStrToFloatDefS(Edit4.text, 0) / xmult;
  SpecWidth := IEStrToFloatDefS(edtWidth.Text, 1) / xmult; if SpecWidth=0 then SpecWidth := 0.001;
  SpecHeight := IEStrToFloatDefS(edtHeight.Text, 1) / xmult; if SpecHeight=0 then SpecHeight := 0.001;
  GammaCorrection := IEStrToFloatDefS(Edit7.Text, 1);
  lc := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if PrintAnnotations then
  begin

    if Sender = btnPrint then
    begin
      // print
      Printer.Title := fTaskName;
      Printer.BeginDoc;
      ie := TImageEnVect.Create(nil);
      ie.IEBitmap.Assign( io.IEBitmap );

      {$ifdef IEINCLUDEIMAGINGANNOT}
      if io.Params.ImagingAnnot.ObjectsCount>0 then
      begin
        io.Params.ImagingAnnot.CopyToTImageEnVect(ie);
        ie.IEBitmap.PixelFormat := ie24RGB;
        ie.CopyObjectsToBack(false);
        ie.RemoveAllObjects;
        ie.Update;
      end;
      {$endif}

      ie.IO.Params.DpiX := io.Params.DpiX;
      ie.IO.Params.DpiY := io.Params.DpiY;
      ie.IO.PrintingFilterOnSubsampling := io.PrintingFilterOnSubsampling;
      ie.io.PrintImage(Printer.Canvas, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, 
        Size, SpecWidth, SpecHeight, GammaCorrection);
      FreeAndNil(ie);
      Printer.EndDoc;
    end
    else
    begin
      // preview
      ie := TImageEnVect.Create(nil);
      ie.IEBitmap.Assign( io.IEBitmap );

      {$ifdef IEINCLUDEIMAGINGANNOT}
      if io.Params.ImagingAnnot.ObjectsCount>0 then
      begin
        io.Params.ImagingAnnot.CopyToTImageEnVect(ie);
        ie.IEBitmap.PixelFormat := ie24RGB;
        ie.CopyObjectsToBack(false);
        ie.RemoveAllObjects;
        ie.Update;
      end;
      {$endif}

      ie.IO.Params.DpiX := io.Params.DpiX;
      ie.IO.Params.DpiY := io.Params.DpiY;
      ie.IO.PrintingFilterOnSubsampling := io.PrintingFilterOnSubsampling;
      ie.io.PreviewPrintImage(ImageEnView1.Bitmap, ImageEnView1.Width, ImageEnView1.Height, Printer, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, 
        Size, SpecWidth, SpecHeight, GammaCorrection);
      ImageEnView1.Update;
      ImageEnView1.Fit;
      FreeAndNil(ie);
    end;

  end
  else
  begin

    if Sender = btnPrint then
    begin
      // print
      Printer.Title := fTaskName;
      Printer.BeginDoc;
      io.PrintImage(Printer.Canvas, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos,
        Size, SpecWidth, SpecHeight, GammaCorrection);
      Printer.EndDoc;
    end
    else
    begin
      // preview
      io.PreviewPrintImage(ImageEnView1.Bitmap, ImageEnView1.Width, ImageEnView1.Height, Printer, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos,
        Size, SpecWidth, SpecHeight, GammaCorrection);
      ImageEnView1.Update;
      ImageEnView1.Fit;
    end;

  end;
  Screen.Cursor := lc;
      
  if Sender = btnPrint then  
    ModalResult := mrOK;
end;

procedure tfiePrnForm1.ComboBox1Change(Sender: TObject);
var
  en: boolean;
begin
  en := ComboBox1.ItemIndex = 3; // true when Specified Sizes 
  edtWidth  .Enabled := en;
  edtHeight .Enabled := en;
  updWidth  .Enabled := en;
  updHeight .Enabled := en;
  lblByX    .Enabled := en;
  printpreview(Sender);
end;

procedure TfiePrnForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Save parameters if they print or close, but do not cancel
  If ModalResult <> mrCancel then
    SaveParameters;
end;

procedure tfiePrnForm1.SpeedButton12Click(Sender: TObject);
begin
  modalresult := mrCancel;
end;

procedure tfiePrnForm1.UpdateLanguage();
var
  bEnglish : Boolean;
begin
  bEnglish := (IEGlobalSettings().MsgLanguage = msEnglish) or
              ((IEGlobalSettings().MsgLanguage = msSystem) and (syslocale.PriLangID = LANG_ENGLISH));
  Caption := iemsg(IEMSG_PRINT);
  btnPrint .Caption := iemsg(IEMSG_PRINT);
  btnSetup .Caption := iemsg(IEMSG_PRINTSETUP) + '...';
  btnCancel.Caption := iemsg(IEMSG_CANCEL);
  Edit1.Hint := iemsg(IEMSG_TOPMARGIN);
  Edit2.Hint := iemsg(IEMSG_LEFTMARGIN);
  Edit3.Hint := iemsg(IEMSG_RIGHTMARGIN);
  Edit4.hint := iemsg(IEMSG_BOTTOMMARGIN);
  Label1.Caption := iemsg(IEMSG_TOP) + ':';
  Label2.Caption := iemsg(IEMSG_LEFT) + ':';
  Label3.Caption := iemsg(IEMSG_RIGHT) + ':';
  Label4.Caption := iemsg(IEMSG_BOTTOM) + ':';
  ComboBox1.Hint := iemsg(IEMSG_LOCATIONSIZE);
  edtWidth.Hint := iemsg(IEMSG_WIDTH);
  edtHeight.Hint := iemsg(IEMSG_HEIGHT);
  Edit7.hint := iemsg(IEMSG_GAMMACORRECTION);

  cmbPosition.Clear;
  cmbPosition.Items.Add(iemsg(IEMSG_TOPLEFT));
  cmbPosition.Items.Add(iemsg(IEMSG_TOPCENTER));
  cmbPosition.Items.Add(iemsg(IEMSG_TOPRIGHT));
  cmbPosition.Items.Add(iemsg(IEMSG_CENTERLEFT));
  cmbPosition.Items.Add(iemsg(IEMSG_CENTER));
  cmbPosition.Items.Add(iemsg(IEMSG_CENTERRIGHT));
  cmbPosition.Items.Add(iemsg(IEMSG_BOTTOMLEFT));
  cmbPosition.Items.Add(iemsg(IEMSG_BOTTOMCENTER));
  cmbPosition.Items.Add(iemsg(IEMSG_BOTTOMRIGHT));

  if not bEnglish then
  begin
    ComboBox1.Items[0] := iemsg(IEMSG_NORMAL);
    ComboBox1.Items[1] := iemsg(IEMSG_FITTOPAGE);
    ComboBox1.Items[2] := iemsg(IEMSG_STRETCHTOPAGE);
    ComboBox1.Items[3] := iemsg(IEMSG_SPECIFIEDSIZE);
  end;

  grpSize    .Caption := ' ' + iemsg(IEMSG_SIZE) + ' ';
  grpMargins .Caption := ' ' + iemsg(IEMSG_MARGINS) + ' (' + iemsg(IEMSG_INCHES) + ') ';
  grpOther   .Caption := ' ' + iemsg(IEMSG_OTHER) + ' ';

  lblGamma.Caption := iemsg(IEMSG_GAMMACORRECTION) + ':';
  lblPosition     .Caption := iemsg(IEMSG_POSITION) + ':';

  if bEnglish then
    lblSize.Caption := 'Print Size:'  // English text too short
  else
    lblSize.Caption := iemsg(IEMSG_SIZE) + ':';
end;

procedure TfiePrnForm1.incdecmargins(text: TEdit; Button: TUDBtnType);
begin
  case button of
    btNext:
      text.Text := FloatToStrF(dmax(IEStrToFloatDefS(text.Text, 0) + IEGlobalSettings().PrintDialogMarginsIncrement, IEGlobalSettings().PrintDialogMarginsMinValue), ffGeneral, 4, 4);
    btPrev:
      text.Text := FloatToStrF(dmax(IEStrToFloatDefS(text.Text, 0) - IEGlobalSettings().PrintDialogMarginsIncrement, IEGlobalSettings().PrintDialogMarginsMinValue), ffGeneral, 4,4);
  end;
end;

procedure TfiePrnForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit1, Button);
end;

procedure TfiePrnForm1.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit2, Button);
end;

procedure TfiePrnForm1.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit3, Button);
end;

procedure TfiePrnForm1.UpDown4Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit4, Button);
end;

procedure TfiePrnForm1.UpDown7Click(Sender: TObject; Button: TUDBtnType);
begin
  case button of
    btNext:
      Edit7.Text := FloatToStrF(dmax(IEStrToFloatDefS(Edit7.Text, 0) + 0.1, 0), ffGeneral, 4, 4);
    btPrev:
      Edit7.Text := FloatToStrF(dmax(IEStrToFloatDefS(Edit7.Text, 0) - 0.1, 0), ffGeneral, 4, 4);
  end;
end;

procedure TfiePrnForm1.updWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(edtWidth, Button);
end;

procedure TfiePrnForm1.updHeightClick(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(edtHeight, Button);
end;

procedure TfiePrnForm1.FormResize(Sender: TObject);
begin
  PrintPreview(self);
end;


{$ELSE} // {$ifdef IEINCLUDEPRINTDIALOGS}

interface
implementation

{$ENDIF}


end.
