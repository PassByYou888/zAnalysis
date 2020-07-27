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
File version 1008
*)

unit ieprnform2;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEPRINTDIALOGS}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ImageEnView, ImageEnIO, ieview, hyiedefs, hyieutils,
  ComCtrls, ImageEnProc, ievect, ExtCtrls, ImgList;

type
  TfiePrnForm2 = class(TForm)
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    grpOther: TGroupBox;
    Edit7: TEdit;
    lblGamma: TLabel;
    grpPreview: TGroupBox;
    ImageEnView1: TImageEnView;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    UpDown7: TUpDown;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Image1: TImage;
    grpSizeLocation: TGroupBox;
    cmbPosition: TComboBox;
    ComboBox1: TComboBox;
    lblSize: TLabel;
    lblPosition: TLabel;
    imlPositions: TImageList;
    Edit5: TEdit;
    UpDown5: TUpDown;
    Edit6: TEdit;
    UpDown6: TUpDown;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblWidthUnit: TLabel;
    lblHeightUnit: TLabel;
    lblMeasurement: TLabel;
    ComboBox2: TComboBox;
    pnlButtons: TPanel;
    btnSetup: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure ComboBox1Change(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure printpreview(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown4Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown7Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown5Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown6Click(Sender: TObject; Button: TUDBtnType);
    procedure btnSetupClick(Sender: TObject);
    procedure cmbPositionDrawItem(Control: TWinControl; Index: Integer; Rect:
        TRect; State: TOwnerDrawState);
    procedure ComboBox2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure incdecmargins(text: TEdit; Button: TUDBtnType);
    procedure SetLanguage_units;
    function XMult: double;
    Procedure UpdateDialogSize;
  public
    { Public declarations }
    io: TImageEnIO;
    fDialogsMeasureUnit: TIEDialogsMeasureUnit;
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


{$IFDEF UNIT_TESTING}
procedure tfiePrnForm2.SetNextLanguage;
begin
  Self.Tag := Self.Tag + 1;
  if Self.tag > ord(msArabic) then
    Self.Tag := 0;
  IEGlobalSettings().MsgLanguage := TMsgLanguage(Self.Tag);
  UpdateLanguage();
end;
{$ENDIF}

procedure tfiePrnForm2.LoadParameters;
begin
  // get parameters from fPrintPreviewParams
  Edit1.Text := FloatToStrF(fPrintPreviewParams.MarginTop, ffGeneral, 4, 4);
  Edit2.Text := FloatToStrF(fPrintPreviewParams.MarginLeft, ffGeneral, 4, 4);
  Edit3.Text := FloatToStrF(fPrintPreviewParams.MarginRight, ffGeneral, 4, 4);
  Edit4.Text := FloatToStrF(fPrintPreviewParams.MarginBottom, ffGeneral, 4, 4);
  cmbPosition.ItemIndex := ord(fPrintPreviewParams.Position);
  ComboBox1.ItemIndex := integer(fPrintPreviewParams.Size);
  if fPrintPreviewParams.Size = psSpecifiedSize then
  begin
    Edit5.Enabled := true;
    Edit6.Enabled := true;
    UpDown5.Enabled := true;
    UpDown6.Enabled := true;
    lblWidth      .Enabled := true;
    lblHeight     .Enabled := true;
    lblWidthUnit  .Enabled := true;
    lblHeightUnit .Enabled := true;
  end;

  if fPrintPreviewParams.Width > 0 then            
    Edit5.Text := FloatToStrF(fPrintPreviewParams.Width, ffGeneral, 4, 4)
  else 
  if IO.Params.DpiX > 0 then  
    Edit5.Text := FloatToStrF( (io.IEBitmap.Width/IO.Params.DpiX)*xmult , ffGeneral, 4, 4);

  if fPrintPreviewParams.Height > 0 then
    Edit6.Text := FloatToStrF(fPrintPreviewParams.Height, ffGeneral, 4, 4)
  else
    Edit6.Text := FloatToStrF( (io.IEBitmap.Height/io.Params.DpiY)*xmult, ffGeneral, 4, 4);

  Edit7.Text := FloatToStrF(fPrintPreviewParams.Gamma, ffGeneral, 4, 4);
end;

procedure tfiePrnForm2.SaveParameters;
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
    fPrintPreviewParams.Width := IEStrToFloatDefS(Edit5.Text, 6);
    fPrintPreviewParams.Height := IEStrToFloatDefS(Edit6.Text, 4);
  end
  else
  begin
    fPrintPreviewParams.Width := -1;
    fPrintPreviewParams.Height := -1;
  end;

  fPrintPreviewParams.Gamma := IEStrToFloatDefS(Edit7.Text, 1);
end;

procedure TfiePrnForm2.FormActivate(Sender: TObject);
var
  bCanSetMeasureUnits: Boolean;
begin
  Application.ProcessMessages; // first draws all controls (to avoid "Swiss Cheese")  
  activating := true;

  if IEGlobalSettings().UseButtonGlyphsInDialogs = False then
  begin
    btnOK.Glyph := nil;
    btnSetup.Glyph := nil;
    btnCancel.Glyph := nil;
  end;
  LoadParameters;
  if ComboBox1.ItemIndex<0 then
    ComboBox1.ItemIndex := 1;
  bCanSetMeasureUnits := (fDialogsMeasureUnit = ieduSelectableDefInches) or (fDialogsMeasureUnit = ieduSelectableDefCm);
  lblMeasurement .Visible := bCanSetMeasureUnits;
  ComboBox2      .Visible := bCanSetMeasureUnits;
  if (fDialogsMeasureUnit = ieduSelectableDefInches) then
    ComboBox2.ItemIndex := 0
  else
  if (fDialogsMeasureUnit = ieduSelectableDefCm) then
    ComboBox2.ItemIndex := 1;
  activating := false;
  UpdateDialogSize;
  printpreview(Sender);
end;

// Changes Size combobox

procedure TfiePrnForm2.ComboBox1Change(Sender: TObject);
var
  en: boolean;
begin
  en := ComboBox1.ItemIndex = 3; // true when Specified Sizes

  lblWidth      .Enabled := en;
  lblHeight     .Enabled := en;
  lblWidthUnit  .Enabled := en;
  lblHeightUnit .Enabled := en;
  Edit5.Enabled := en;
  Edit6.Enabled := en;
  UpDown5.Enabled := en;
  UpDown6.Enabled := en;

  printpreview(Sender);
end;

// OK (print)

procedure TfiePrnForm2.btnOKClick(Sender: TObject);
begin
  SaveParameters;
  printpreview(Sender);
end;


function tfiePrnForm2.XMult: double;
begin
  if (fDialogsMeasureUnit = ieduCm) or (fDialogsMeasureUnit = ieduSelectableDefCm) then
    xmult := CM_per_Inch
  else
    xmult := 1;
end;

// preview or print

procedure TfiePrnForm2.printpreview(Sender: TObject);
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
  SpecWidth := IEStrToFloatDefS(Edit5.Text, 1) / xmult; if SpecWidth=0 then SpecWidth := 0.001;
  SpecHeight := IEStrToFloatDefS(Edit6.Text, 1) / xmult; if SpecHeight=0 then SpecHeight := 0.001;
  GammaCorrection := IEStrToFloatDefS(Edit7.Text, 1);
  lc := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if PrintAnnotations then
  begin

    if Sender = btnOK then
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

    if Sender = btnOK then
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
end;

procedure tfiePrnForm2.SetLanguage_units;
begin
  if (fDialogsMeasureUnit = ieduInches) or (fDialogsMeasureUnit = ieduSelectableDefInches) then
  begin
    // inches
    GroupBox1.Caption := ' ' + iemsg(IEMSG_MARGINS) + ' (' + iemsg(IEMSG_INCHES) + ') ';
    lblWidthUnit .Caption := iemsg(IEMSG_INCHES);
    lblHeightUnit.Caption := iemsg(IEMSG_INCHES);
  end
  else
  begin
    // centimeters (Cm)
    GroupBox1.Caption := ' ' + iemsg(IEMSG_MARGINS) + ' (cm) ';
    lblWidthUnit .Caption := 'cm';
    lblHeightUnit.Caption := 'cm';
  end;
end;

procedure tfiePrnForm2.UpdateLanguage();
var
  bEnglish : Boolean;
begin
  bEnglish := (IEGlobalSettings().MsgLanguage = msEnglish) or
              ((IEGlobalSettings().MsgLanguage = msSystem) and (syslocale.PriLangID = LANG_ENGLISH));
  Caption := iemsg(IEMSG_PRINT);
  SetLanguage_units;
  Label1.Caption := iemsg(IEMSG_TOP) + ':';
  Label2.Caption := iemsg(IEMSG_LEFT) + ':';
  Label3.Caption := iemsg(IEMSG_RIGHT) + ':';
  Label4.Caption := iemsg(IEMSG_BOTTOM) + ':';
  grpPreview.Caption := ' ' + iemsg(IEMSG_PREVIEW) + ' ';
  ComboBox1.Items[0] := iemsg(IEMSG_NORMAL);
  ComboBox1.Items[1] := iemsg(IEMSG_FITTOPAGE);
  ComboBox1.Items[2] := iemsg(IEMSG_STRETCHTOPAGE);
  ComboBox1.Items[3] := iemsg(IEMSG_SPECIFIEDSIZE);
  btnOK.Caption := iemsg(IEMSG_OK);
  btnCancel.Caption := iemsg(IEMSG_CANCEL);
  btnSetup.Caption := iemsg(IEMSG_PRINTSETUP) + '...';
  ComboBox2.Items[0] := iemsg(IEMSG_INCHES);
  grpOther.Caption := ' ' + iemsg(IEMSG_OTHER) + ' ';
  lblGamma.Caption := iemsg(IEMSG_GAMMACORRECTION) + ':';
  lblWidth  .Caption := iemsg(IEMSG_WIDTH) + ':';
  lblHeight .Caption := iemsg(IEMSG_HEIGHT) + ':';
  lblMeasurement.Caption := iemsg(IEMSG_MEASUREUNITS) + ':';
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

  grpSizeLocation .Caption := ' ' + iemsg(IEMSG_LOCATIONSIZE) + ' ';
  lblPosition     .Caption := iemsg(IEMSG_POSITION) + ':';
  if bEnglish then
    lblSize.Caption := 'Print Size:'  // English text too short
  else
    lblSize.Caption := iemsg(IEMSG_SIZE) + ':';
end;

procedure TfiePrnForm2.incdecmargins(text: TEdit; Button: TUDBtnType);
begin
  case button of
    btNext:
      text.Text := FloatToStrF(dmax(IEStrToFloatDefS(text.Text, 0) + IEGlobalSettings().PrintDialogMarginsIncrement, IEGlobalSettings().PrintDialogMarginsMinValue), ffGeneral, 4, 4);
    btPrev:
      text.Text := FloatToStrF(dmax(IEStrToFloatDefS(text.Text, 0) - IEGlobalSettings().PrintDialogMarginsIncrement, IEGlobalSettings().PrintDialogMarginsMinValue), ffGeneral, 4, 4);
  end;
end;

procedure TfiePrnForm2.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit1, Button);
end;

procedure TfiePrnForm2.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit2, Button);
end;

procedure TfiePrnForm2.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit3, Button);
end;

procedure TfiePrnForm2.UpDown4Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit4, Button);
end;

procedure TfiePrnForm2.UpDown7Click(Sender: TObject; Button: TUDBtnType);
begin
  case button of
    btNext:
      Edit7.Text := FloatToStrF(dmax(IEStrToFloatDefS(Edit7.Text, 0) + 0.1, 0), ffGeneral, 4, 4);
    btPrev:
      Edit7.Text := FloatToStrF(dmax(IEStrToFloatDefS(Edit7.Text, 0) - 0.1, 0), ffGeneral, 4, 4);
  end;
end;

procedure TfiePrnForm2.UpDown5Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit5, Button);
end;

procedure TfiePrnForm2.UpDown6Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit6, Button);
end;

// setup

procedure TfiePrnForm2.btnSetupClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  printpreview(self);
end;

procedure TfiePrnForm2.cmbPositionDrawItem(Control: TWinControl; Index:
    Integer; Rect: TRect; State: TOwnerDrawState);
var
  iGlyph : Integer;
  sText  : string;
begin
  iGlyph := Index;
  sText  := cmbPosition.Items[Index];
  IEDrawComboListBoxItem(TCombobox(Control), Rect, sText, imlPositions, iGlyph);
end;

// units

procedure TfiePrnForm2.ComboBox2Change(Sender: TObject);
var
  pred: TIEDialogsMeasureUnit;
  xmult: double;
begin
  pred := fDialogsMeasureUnit;
  if ComboBox2.ItemIndex = 0 then
    fDialogsMeasureUnit := ieduSelectableDefInches
  else
    fDialogsMeasureUnit := ieduSelectableDefCm;
  if pred <> fDialogsMeasureUnit then
  begin
    if pred = ieduSelectableDefCm then
      xmult := 1 / CM_per_Inch
    else
      xmult := CM_per_Inch;
    Edit1.Text := FloatToStrF(IEStrToFloatDefS(Edit1.Text, 0) * xmult, ffGeneral, 4, 4);
    Edit2.Text := FloatToStrF(IEStrToFloatDefS(Edit2.Text, 0) * xmult, ffGeneral, 4, 4);
    Edit3.Text := FloatToStrF(IEStrToFloatDefS(Edit3.Text, 0) * xmult, ffGeneral, 4, 4);
    Edit4.Text := FloatToStrF(IEStrToFloatDefS(Edit4.Text, 0) * xmult, ffGeneral, 4, 4);
    Edit5.Text := FloatToStrF(IEStrToFloatDefS(Edit5.Text, 0) * xmult, ffGeneral, 4, 4);
    Edit6.Text := FloatToStrF(IEStrToFloatDefS(Edit6.Text, 0) * xmult, ffGeneral, 4, 4);
    SetLanguage_units;
  end;
end;

procedure TfiePrnForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Save parameters if they print or close, but do not cancel
  If ModalResult <> mrCancel then
    SaveParameters;
end;

procedure TfiePrnForm2.FormCreate(Sender: TObject);
begin
  case iegDialogsBackground of
    iedbPaper:
      begin
        ImageEnView1.Background:=clWhite;
        IECreateOSXBackgroundPaper(Image1.Picture.Bitmap,Image1.Width,Image1.Height);
        Image1.Update;
      end;
    iedbMetal: 
      begin
        ImageEnView1.Background := clSilver;
        IECreateOSXBackgroundMetal(Image1.Picture.Bitmap, Image1.Width, Image1.Height);
        Image1.Update;
      end;
  end;
end;


Procedure TfiePrnForm2.UpdateDialogSize;
var
  iBottomMargin: Integer;
begin
  iBottomMargin := lblMeasurement.Left; // Suitable approximation
  if ComboBox2.Visible then
    grpOther.Height := ComboBox2.Top + ComboBox2.Height + iBottomMargin
  else                     
    grpOther.Height := Edit7.Top + Edit7.Height + iBottomMargin;

  grpPreview.Height := grpOther.Top + grpOther.Height - grpPreview.Top;
  ImageEnView1.Height := grpPreview.Height - (ImageEnView1.Top + 6);

  pnlButtons.Top := grpPreview.Top + grpPreview.Height + 2;
  ClientHeight   := pnlButtons.Top + pnlButtons.height;
end;

{$ELSE} // {$ifdef IEINCLUDEPRINTDIALOGS}

interface
implementation

{$ENDIF}


end.


