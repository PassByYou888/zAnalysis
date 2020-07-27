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
File version 1009
*)

unit ieprnform3;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEMULTIVIEW}

{$IFDEF IEINCLUDEPRINTDIALOGS}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, hyiedefs, hyieutils, iemio,
  ExtCtrls, imageenview, ieview, iemview, imageenio, ComCtrls, StdCtrls,
  Buttons, imageenproc, Menus, ievect, ImgList;

type
  TfiePrnForm3 = class(TForm)
    Splitter1: TSplitter;
    Panel2: TPanel;
    PrinterSetupDialog1: TPrinterSetupDialog;
    ImageEnView1: TImageEnView;
    pnlLeft: TPanel;
    ImageEnMView1: TImageEnMView;
    PopupMenu1: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    imlPositions: TImageList;
    pnlPrintSelector: TPanel;
    cmbPrintSelector: TComboBox;
    btnPrint: TBitBtn;
    btnCancel: TBitBtn;
    btnSetup: TBitBtn;
    grpSize: TGroupBox;
    lblSize: TLabel;
    lblByX: TLabel;
    cmbPrintSize: TComboBox;
    edtWidth: TEdit;
    updWidth: TUpDown;
    edtHeight: TEdit;
    updHeight: TUpDown;
    grpMargins: TGroupBox;
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
    grpOther: TGroupBox;
    lblPosition: TLabel;
    lblGamma: TLabel;
    cmbPosition: TComboBox;
    Edit7: TEdit;
    UpDown7: TUpDown;
    edtThumbnailColumns: TEdit;
    updThumbnailColumns: TUpDown;
    lblThumbnailsByX: TLabel;
    edtThumbnailRows: TEdit;
    updThumbnailRows: TUpDown;
    grpThumbnailsOther: TGroupBox;
    lblThumbnailStyle: TLabel;
    lblThumbnailSpacing: TLabel;
    cmbThumbnailStyle: TComboBox;
    edtThumbnailSpacing: TEdit;
    updThumbnailSpacing: TUpDown;
    tmrUpdatePreview: TTimer;
    btnPrevious: TButton;
    btnNext: TButton;
    procedure FormActivate(Sender: TObject);
    procedure cmbPrintSizeChange(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown4Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown7Click(Sender: TObject; Button: TUDBtnType);
    procedure FormResize(Sender: TObject);
    procedure DelayedPrintPreview(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageEnMView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Delete1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnSetupClick(Sender: TObject);
    procedure cmbPositionDrawItem(Control: TWinControl; Index: Integer; Rect:
        TRect; State: TOwnerDrawState);
    procedure PrintPreview(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure updHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure updThumbnailColumnsClick(Sender: TObject; Button: TUDBtnType);
    procedure updThumbnailRowsClick(Sender: TObject; Button: TUDBtnType);
    procedure updThumbnailSpacingClick(Sender: TObject; Button: TUDBtnType);
    procedure updWidthClick(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
    srcview: TImageEnMView;
    ie: TImageEnVect;
    fActivating: boolean;
    fDialogCaption: string;
    fThumbnailPageIndex : Integer;  // the page of thumbnails that is presently being previewed fOriginalDlgWidth then
    fOriginalDlgWidth  : Integer;   // Remember the default size of this dialog
    fOriginalDlgHeight : Integer;   // Remember the default size of this dialog
    procedure incdecmargins(text: TEdit; Button: TUDBtnType);
    procedure ImageSelect;
    function GetImage(idx : Integer) : Boolean;
    procedure UpdatePrintSizeControls;
    procedure SetDialogCaption(value: string);
    procedure SetLanguage_units; 

  public
    { Public declarations }
    mio: TImageEnMIO;
    fTaskName: string;
    fDialogsMeasureUnit: TIEDialogsMeasureUnit;
    PrintAnnotations: boolean;
    fPrintPreviewParams: TIOPrintPreviewParams;
    procedure UpdateLanguage();
    procedure LoadParameters;
    procedure SaveParameters;

    {$IFDEF UNIT_TESTING}
    procedure SetNextLanguage;
    {$ENDIF}

    // Form caption
    property DialogCaption: string read fDialogCaption write SetDialogCaption;
  end;

implementation

uses printers, iesettings;

{$R *.DFM}


{$IFDEF UNIT_TESTING}
procedure tfiePrnForm3.SetNextLanguage;
begin
  Self.Tag := Self.Tag + 1;
  if Self.tag > ord(msArabic) then
    Self.Tag := 0;
  IEGlobalSettings().MsgLanguage := TMsgLanguage(Self.Tag);
  UpdateLanguage();
end;
{$ENDIF}


procedure tfiePrnForm3.SetLanguage_units;
// Matteo Barrese: 15/11/12
begin
  if (fDialogsMeasureUnit = ieduInches) or (fDialogsMeasureUnit = ieduSelectableDefInches) then
  begin
    // inches
    grpMargins.Caption := ' ' + iemsg(IEMSG_MARGINS) + ' (' + iemsg(IEMSG_INCHES) + ') ';
  end
  else
  begin
    // centimeters (Cm)
    grpMargins.Caption := ' ' + iemsg(IEMSG_MARGINS) + ' (cm) ';
  end;
end;


procedure tfiePrnForm3.UpdateLanguage();
var
  bEnglish : Boolean;
begin
  bEnglish := (IEGlobalSettings().MsgLanguage = msEnglish) or
              ((IEGlobalSettings().MsgLanguage = msSystem) and (syslocale.PriLangID = LANG_ENGLISH));
              
  Caption := iemsg(IEMSG_PRINT);
  Edit1.Hint := iemsg(IEMSG_TOPMARGIN);
  Edit2.Hint := iemsg(IEMSG_LEFTMARGIN);
  Edit3.Hint := iemsg(IEMSG_RIGHTMARGIN);
  Edit4.hint := iemsg(IEMSG_BOTTOMMARGIN);
  cmbPrintSize.Hint := iemsg(IEMSG_LOCATIONSIZE);
  Edit7.hint := iemsg(IEMSG_GAMMACORRECTION);
  Label1.Caption := iemsg(IEMSG_TOP) + ':';
  Label2.Caption := iemsg(IEMSG_LEFT) + ':';
  Label3.Caption := iemsg(IEMSG_RIGHT) + ':';
  Label4.Caption := iemsg(IEMSG_BOTTOM) + ':';

  SetLanguage_units;
  if not bEnglish then
  begin
    cmbPrintSize.Items[0] := iemsg(IEMSG_NORMAL);
    cmbPrintSize.Items[1] := iemsg(IEMSG_FITTOPAGE);
    cmbPrintSize.Items[2] := iemsg(IEMSG_STRETCHTOPAGE);
    cmbPrintSize.Items[3] := iemsg(IEMSG_SPECIFIEDSIZE);
    cmbPrintSize.Items[4] := iemsg(IEMSG_THUMBNAILS);
  end;


  cmbPrintSelector.Clear;
  cmbPrintSelector.Items.Add(iemsg(IEMSG_PRINTALL));
  cmbPrintSelector.Items.Add(iemsg(IEMSG_PRINTSELECTED));

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

  btnPrint .Caption := iemsg(IEMSG_PRINT);
  btnSetup .Caption := iemsg(IEMSG_PRINTSETUP);
  btnCancel.Caption := iemsg(IEMSG_CANCEL);

  grpSize    .Caption := ' ' + iemsg(IEMSG_SIZE) + ' ';
  grpOther   .Caption := ' ' + iemsg(IEMSG_OTHER) + ' ';

  lblGamma.Caption := iemsg(IEMSG_GAMMACORRECTION) + ':';
  lblPosition     .Caption := iemsg(IEMSG_POSITION) + ':';

  if bEnglish then
    lblSize.Caption := 'Print Size:'  // English text too short
  else
    lblSize.Caption := iemsg(IEMSG_SIZE) + ':';

  edtWidth                 .Hint := iemsg(IEMSG_WIDTH);
  edtHeight                .Hint := iemsg(IEMSG_HEIGHT);
  edtThumbnailColumns      .Hint := iemsg(IEMSG_COLUMNS);
  edtThumbnailRows         .Hint := iemsg(IEMSG_ROWS);
  cmbThumbnailStyle        .Hint := iemsg(IEMSG_STYLE);
  lblThumbnailStyle        .Caption := iemsg(IEMSG_STYLE) + ':';
  edtThumbnailSpacing      .Hint := iemsg(IEMSG_SPACING);
  lblThumbnailSpacing      .Caption := iemsg(IEMSG_SPACING) + ':';
  grpThumbnailsOther       .Caption := ' ' + iemsg(IEMSG_OTHER) + ' ';

  btnPrevious              .Caption := iemsg(IEMSG_PREVIOUS);
  btnNext                  .Caption := iemsg(IEMSG_NEXT);
end;

procedure tfiePrnForm3.LoadParameters;
const
  Minimum_Width  = 300;
  Minimum_Height = 200;
var
  X, Y: Integer;
begin
  // get parameters from fPrintPreviewParams
  Edit1.Text := FloatToStrF(fPrintPreviewParams.MarginTop, ffGeneral, 4, 4);
  Edit2.Text := FloatToStrF(fPrintPreviewParams.MarginLeft, ffGeneral, 4, 4);
  Edit3.Text := FloatToStrF(fPrintPreviewParams.MarginRight, ffGeneral, 4, 4);
  Edit4.Text := FloatToStrF(fPrintPreviewParams.MarginBottom, ffGeneral, 4, 4);  
  cmbPosition.ItemIndex := ord(fPrintPreviewParams.Position);

  if fPrintPreviewParams.PrintThumbnails then
    cmbPrintSize.ItemIndex := 4
  else
    cmbPrintSize.ItemIndex := integer(fPrintPreviewParams.Size);
  UpdatePrintSizeControls;

  if fPrintPreviewParams.Width>0 then
    edtWidth.Text := FloatToStrF(fPrintPreviewParams.Width, ffGeneral, 4, 4);

  if fPrintPreviewParams.Height>0 then
    EdtHeight.Text := FloatToStrF(fPrintPreviewParams.Height, ffGeneral, 4, 4);

  Edit7.Text := FloatToStrF(fPrintPreviewParams.Gamma, ffGeneral, 4, 4);

  edtThumbnailColumns.Text    := IntToStr(fPrintPreviewParams.ThumbnailColumns);
  edtThumbnailRows.Text       := IntToStr(fPrintPreviewParams.ThumbnailRows);
  cmbThumbnailStyle.ItemIndex := ord(fPrintPreviewParams.ThumbnailStyle);
  edtThumbnailSpacing.Text    := FloatToStrF(fPrintPreviewParams.ThumbnailSpacing, ffGeneral, 4, 4);

  if (fPrintPreviewParams.DlgWidth  >= Minimum_Width) and
     (fPrintPreviewParams.DlgHeight >= Minimum_Height) then
  begin
    ClientWidth  := fPrintPreviewParams.DlgWidth;
    ClientHeight := fPrintPreviewParams.DlgHeight;

    // Check bounds
    if Width > Screen.Width then
      Width := Screen.Width;
    if Height > Screen.Height then
      Height := Screen.Height;

    // Center it
    X := (Screen.Width - Width) div 2;
    Y := (Screen.Height - Height) div 2;

    if X < Screen.DesktopLeft then
      X := Screen.DesktopLeft;
    if Y < Screen.DesktopTop then
      Y := Screen.DesktopTop;
    SetBounds(X, Y, Width, Height);
  end;

  { Not exposed: ThumbnailShowText }
end;

procedure tfiePrnForm3.SaveParameters;
begin
  // put parameters to fPrintPreviewParams
  fPrintPreviewParams.MarginTop    := IEStrToFloatDefS(Edit1.Text, 1);
  fPrintPreviewParams.MarginLeft   := IEStrToFloatDefS(Edit2.Text, 1);
  fPrintPreviewParams.MarginRight  := IEStrToFloatDefS(Edit3.Text, 1);
  fPrintPreviewParams.MarginBottom := IEStrToFloatDefS(Edit4.Text, 1);
  fPrintPreviewParams.Position     := TIOPrintPreviewPosition(cmbPosition.ItemIndex);

  if cmbPrintSize.ItemIndex = 4 then // Thumbnails
    fPrintPreviewParams.PrintThumbnails := True
  else
  begin
    fPrintPreviewParams.PrintThumbnails := False;
    fPrintPreviewParams.Size := TIOPrintPreviewSize(cmbPrintSize.ItemIndex);
  end;

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
  
  fPrintPreviewParams.ThumbnailColumns := StrToIntDef(edtThumbnailColumns.Text, 4);
  fPrintPreviewParams.ThumbnailRows    := StrToIntDef(edtThumbnailRows.Text, 5);
  fPrintPreviewParams.ThumbnailStyle   := TIOPrintPreviewThumbnailStyle(cmbThumbnailStyle.ItemIndex);
  fPrintPreviewParams.ThumbnailSpacing := IEStrToFloatDefS(edtThumbnailSpacing.Text, 0.1);
  { Not exposed: ThumbnailShowText }
end;

procedure TfiePrnForm3.FormActivate(Sender: TObject);
var
  i, idx: integer;
begin
  Screen.Cursor := crHourglass;
  try
    Application.ProcessMessages; // first draws all controls (to avoid "Swiss Cheese")  
    ImageEnMView1.LockUpdate;
    fActivating := true; 

    if IEGlobalSettings().UseButtonGlyphsInDialogs = False then
    begin
      btnPrint.Glyph := nil;
      btnSetup.Glyph := nil;
      btnCancel.Glyph := nil;
    end;
    fOriginalDlgWidth  := ClientWidth;
    fOriginalDlgHeight := ClientHeight;
    LoadParameters;
    ImageEnMView1.FillThumbnail := false;  
    ImageEnMView1.SetModernStyling;   
    ImageEnMView1.SideGap := 6;
    ImageEnMView1.Background := clBtnFace;
    ImageEnMView1.GradientEndColor := clGray;
    ImageEnMView1.ThumbnailDisplayFilter := rfLanczos3;
    ImageEnMView1.ThumbnailsBorderColor := clSilver;
    
    // load ImageEnMView1
    srcview := mio.AttachedMView as TImageEnMView;

    for i := 0 to srcview.ImageCount - 1 do
    begin
      idx := ImageEnMView1.AppendImage;
      ImageEnMView1.SetIEBitmapEx(idx, srcview.GetTIEBitmap(i));
      srcview.ReleaseBitmap(idx, false);

      if srcview.ImageTopText[i].Caption <> '' then
        ImageEnMView1.ImageTopText[idx].Caption := srcview.ImageTopText[i].Caption
      else
      if srcview.ImageBottomText[i].Caption <> '' then
        ImageEnMView1.ImageTopText[idx].Caption := srcview.ImageBottomText[i].Caption
      else
      ImageEnMView1.ImageTopText[idx].Caption := 'Image ' + IntToStr(i + 1);
      if PrintAnnotations then
        ImageEnMView1.MIO.Params[idx].Assign( srcview.MIO.Params[i] );
    end;
    ImageEnMView1.SelectedImage := 0;

    // Get our selection from the source
    if srcview.MultiSelectedImagesCount > 0 then
      ImageEnMView1.CopySelection(srcview);

    cmbPrintSelector.ItemIndex := 0;

    fActivating := false;
    ImageSelect;        
    SetLanguage_units;
  finally
    ImageEnMView1.UnlockUpdate;
    Screen.Cursor := crDefault;
  end;
end;

// preview or print

procedure tfiePrnForm3.PrintPreview(Sender: TObject);
const
  Thumbnail_Box_Color = clGray;
var
  VerticalPos: TIEVerticalPos;
  HorizontalPos: TIEHorizontalPos;
  Size: TIESize;
  MarginLeft, MarginTop, MarginRight, MarginBottom, SpecWidth, SpecHeight, GammaCorrection: double;
  xmult: Double;
  i, idx: Integer;
  lc: TCursor;
  bPrintThumbnails : Boolean;
  bPrintAll : Boolean;
  iThumbColumns  : Integer;
  iThumbRows     : Integer;
  ThumbSpacing   : Double;
  ThumbStyle     : TIOPrintPreviewThumbnailStyle;
  bThumbShowBox  : Boolean;
  bThumbShadow   : Boolean;
  bThumbShowText : Boolean;
  iImageCount    : Integer;
  iPageCount     : Integer;
  iThumbsPerPage : Integer;
  bFirstImage: Boolean;
begin
  if fActivating then
    exit;
  if (ImageEnMView1.ImageCount = 0) or (Width < 100) or (Height < 100) then
    exit; // to disallow AV

  tmrUpdatePreview.Enabled := False;

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

  bPrintThumbnails := False;
  Size := iesFITTOPAGE;
  case cmbPrintSize.ItemIndex of
    0: Size := iesNORMAL;
    1: Size := iesFITTOPAGE;
    2: Size := iesFITTOPAGESTRETCH;
    3: Size := iesFILLPAGE;
    4: Size := iesSPECIFIEDSIZE;
    5: bPrintThumbnails := True;
  end;
  if (fDialogsMeasureUnit = ieduCm) or (fDialogsMeasureUnit = ieduSelectableDefCm) then
    xmult := CM_per_Inch
  else
    xmult := 1;
  MarginLeft := IEStrToFloatDefS(Edit2.Text, 0) / xmult;
  MarginTop := IEStrToFloatDefS(Edit1.Text, 0) / xmult;
  MarginRight := IEStrToFloatDefS(Edit3.Text, 0) / xmult;
  MarginBottom := IEStrToFloatDefS(Edit4.text, 0) / xmult;
  SpecWidth := IEStrToFloatDefS(edtWidth.Text, 1) / xmult;
  SpecHeight := IEStrToFloatDefS(edtHeight.Text, 1) / xmult;
  GammaCorrection := IEStrToFloatDefS(Edit7.Text, 1);

  iThumbColumns  := StrToIntDef(edtThumbnailColumns.Text, 4);
  iThumbRows     := StrToIntDef(edtThumbnailRows.Text, 5);
  ThumbSpacing   := IEStrToFloatDefS(edtThumbnailSpacing.Text, 1) / xmult;
  ThumbStyle     := TIOPrintPreviewThumbnailStyle(cmbThumbnailStyle.ItemIndex);
  bThumbShowBox  := ThumbStyle = ptBorder;
  bThumbShadow   := ThumbStyle = ptSoftShadow;
  bThumbShowText := fPrintPreviewParams.ThumbnailShowText;

  lc := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  bPrintAll := cmbPrintSelector.ItemIndex = 0;
  if Sender = btnPrint then
  begin
    // print
    Printer.Title := fTaskName;
    Printer.BeginDoc;
    if bPrintThumbnails then
    begin
      // Thumbnails
      // Use srcview so we get the best quality output
      if bPrintAll = False then
        srcview.CopySelection(ImageEnMView1);
      srcview.mio.PrintImages(iThumbColumns, iThumbRows, ThumbSpacing, ThumbSpacing, not bPrintAll, MarginLeft, MarginTop, MarginRight, MarginBottom, bThumbShowBox, bThumbShowText, bThumbShadow, Thumbnail_Box_Color);
    end
    else
    if bPrintAll then
    begin
      // print all
      bFirstImage := True;
      for i := 0 to srcview.ImageCount - 1 do
      begin
        if GetImage(i) then
        begin
          if bFirstImage = False then
            Printer.NewPage;
          bFirstImage := False;

          ie.io.Params.DpiX := mio.Params[i].DpiX;
          ie.io.Params.DpiY := mio.Params[i].DpiY;

          ie.io.PrintingFilterOnSubsampling := mio.PrintingFilterOnSubsampling;
          ie.io.PrintImage(Printer.Canvas, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
        end;
      end;
    end
    else
    begin
      // print selected
      bFirstImage := True;
      for i := 0 to ImageEnMView1.MultiSelectedImagesCount - 1 do
      begin
        idx := ImageEnMView1.MultiSelectedImages[i];
        if GetImage(idx) then
        begin
          if bFirstImage = False then
            Printer.NewPage;
          bFirstImage := False;

          ie.io.Params.DpiX := mio.Params[idx].DpiX;
          ie.io.Params.DpiY := mio.Params[idx].DpiY;

          ie.io.PrintingFilterOnSubsampling := mio.PrintingFilterOnSubsampling;
          ie.io.PrintImage(Printer.Canvas, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
        end;
      end;
    end;
    Printer.EndDoc;
    ImageSelect;
  end
  else
  begin
    if bPrintAll then
      iImageCount := srcview.ImageCount
    else
      iImageCount := ImageEnMView1.MultiSelectedImagesCount;

    // preview    
    if bPrintThumbnails then
    begin
      // Thumbnails
      iThumbsPerPage := StrToIntDef(edtThumbnailColumns.Text, 4) * StrToIntDef(edtThumbnailRows.Text, 5);
      iPageCount := iImageCount div iThumbsPerPage;
      if iImageCount mod iThumbsPerPage <> 0 then
        inc(iPageCount);

      if fThumbnailPageIndex < 0 then
        fThumbnailPageIndex := 0;
      if fThumbnailPageIndex > iPageCount - 1 then
        fThumbnailPageIndex := iPageCount - 1;

      btnPrevious.Enabled := fThumbnailPageIndex > 0;
      btnNext    .Enabled := fThumbnailPageIndex < iPageCount - 1;

      Caption := fDialogCaption + ' (' + format(iemsg(IEMSG_Page_X_of_X), [fThumbnailPageIndex + 1, iPageCount]) + ')';
      // Use ImageEnMView1 rather than srcview so it will be faster
      ImageEnMView1.MIO.PreviewPrintImages(ImageEnView1.Bitmap, ImageEnView1.Width, ImageEnView1.Height, Printer, iThumbColumns,
                                           iThumbRows, ThumbSpacing, ThumbSpacing, not bPrintAll, MarginLeft, MarginTop,
                                           MarginRight, MarginBottom, bThumbShowBox, bThumbShowText, bThumbShadow, Thumbnail_Box_Color, fThumbnailPageIndex);
    end
    else
    begin
      // Image
      if ie.IsEmpty2 then
        ImageEnView1.Blank
      else
      begin
        Caption := fDialogCaption + ' (' + format(iemsg(IEMSG_X_Pages), [iImageCount]) + ')';

        ie.io.Params.DpiX := mio.Params[ImageEnMView1.SelectedImage].DpiX;
        ie.io.Params.DpiY := mio.Params[ImageEnMView1.SelectedImage].DpiY;

        ie.io.PrintingFilterOnSubsampling := mio.PrintingFilterOnSubsampling;
        ie.io.PreviewPrintImage(ImageEnView1.Bitmap, ImageEnView1.Width, ImageEnView1.Height, Printer, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
      end;
    end;
    ImageEnView1.Update;
    ImageEnView1.Fit;
  end;
  Screen.Cursor := lc;   
  if Sender = btnPrint then
    ModalResult := mrOK;
end;

procedure TfiePrnForm3.incdecmargins(text: TEdit; Button: TUDBtnType);
begin
  case button of
    btNext:
      text.Text := FloatToStrF(dmax(IEStrToFloatDefS(text.Text, 0) + IEGlobalSettings().PrintDialogMarginsIncrement, IEGlobalSettings().PrintDialogMarginsMinValue), ffGeneral, 4, 4);
    btPrev:
      text.Text := FloatToStrF(dmax(IEStrToFloatDefS(text.Text, 0) - IEGlobalSettings().PrintDialogMarginsIncrement, IEGlobalSettings().PrintDialogMarginsMinValue), ffGeneral, 4, 4);
  end;
end;

procedure TfiePrnForm3.cmbPrintSizeChange(Sender: TObject);
begin
  UpdatePrintSizeControls;
  DelayedPrintPreview(Sender);
end;


procedure TfiePrnForm3.UpdatePrintSizeControls;
const
  Button_Margin = 8;
var
  bSpecifiedSize : Boolean;
  bThumbnails    : Boolean;
begin
  bSpecifiedSize := cmbPrintSize.ItemIndex = 3;
  edtWidth  .Enabled := bSpecifiedSize;
  updWidth  .Enabled := bSpecifiedSize;
  lblByX    .Enabled := bSpecifiedSize;
  edtHeight .Enabled := bSpecifiedSize;
  updHeight .Enabled := bSpecifiedSize;

  bThumbnails  := cmbPrintSize.ItemIndex = 4;
  edtThumbnailColumns .Visible := bThumbnails;
  updThumbnailColumns .Visible := bThumbnails;
  lblThumbnailsByX    .Visible := bThumbnails;
  edtThumbnailRows    .Visible := bThumbnails;
  updThumbnailRows    .Visible := bThumbnails;

  grpOther            .Visible := not bThumbnails;
  grpThumbnailsOther  .Visible := bThumbnails;

  btnPrevious         .Visible := bThumbnails;
  btnNext             .Visible := bThumbnails;

  btnPrevious         .Top     := ImageEnView1.Height - btnPrevious.Height - Button_Margin;
  btnNext             .Top     := btnPrevious         .Top;         
  btnPrevious         .Left    := Button_Margin;
  btnNext             .Left    := ImageEnView1.Width - btnPrevious.Width - Button_Margin;
end;


procedure TfiePrnForm3.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit1, Button);
end;

procedure TfiePrnForm3.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit2, Button);
end;

procedure TfiePrnForm3.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit3, Button);
end;

procedure TfiePrnForm3.UpDown4Click(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(Edit4, Button);
end;

procedure TfiePrnForm3.UpDown7Click(Sender: TObject; Button: TUDBtnType);
begin
  case button of
    btNext:
      Edit7.Text := FloatToStrF(dmax(IEStrToFloatDefS(Edit7.Text, 0) + 0.1, 0), ffGeneral, 4, 4);
    btPrev:
      Edit7.Text := FloatToStrF(dmax(IEStrToFloatDefS(Edit7.Text, 0) - 0.1, 0), ffGeneral, 4, 4);
  end;
end;

procedure TfiePrnForm3.FormResize(Sender: TObject);
begin
  // Fix repaint issue on dragging of splitter
  pnlPrintSelector.Invalidate;

  UpdatePrintSizeControls;
  DelayedPrintPreview(self);
end;


function TfiePrnForm3.GetImage(idx : Integer) : Boolean;
var
  bRetrieved: Boolean;
  bAllFiles: Boolean;
begin
  Result := True;

  bAllFiles := srcview.ClassName <> 'TImageEnFolderMView'; // Don't break existing method of TImageEnMView
  if (bAllFiles = False) and (IsKnownFormat(srcview.ImageFilename[idx]) = False) then
  begin
    ie.Blank;
    Result := False;
  end;

  // copy image
  bRetrieved := False;
  if (srcview.StoreType <> ietNormal) and FileExists(srcview.ImageFilename[idx]) then
  begin
    ie.io.LoadFromFile(srcview.ImageFilename[idx]);
    bRetrieved := not ie.io.Aborting;
  end;

  if not bRetrieved then
    srcview.CopyToIEBitmap(idx, ie.IEBitmap);

  {$ifdef IEINCLUDEIMAGINGANNOT}
  // copy annotations
  if PrintAnnotations then
  begin
    if srcview.MIO.Params[idx].ImagingAnnot.ObjectsCount > 0 then
    begin
      srcview.MIO.Params[idx].ImagingAnnot.CopyToTImageEnVect(ie);
      ie.IEBitmap.PixelFormat := ie24RGB;
      ie.CopyObjectsToBack(false);
      ie.RemoveAllObjects;
    end;
  end;
  {$endif}

  ie.Update;
end;

procedure TfiePrnForm3.ImageSelect;
var
  idx: integer;
begin
  idx := ImageEnMView1.SelectedImage;
  if idx > -1 then
    GetImage(idx);
  DelayedPrintPreview(self);
end;

procedure TfiePrnForm3.ImageEnMView1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ImageSelect;
end;

procedure TfiePrnForm3.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ie);
end;

procedure TfiePrnForm3.FormCreate(Sender: TObject);
begin
  ie := TImageEnVect.Create(nil);
  ie.Blank;

  // Move controls to display positions
  edtThumbnailColumns .Top := edtWidth  .Top;
  updThumbnailColumns .Top := updWidth  .Top;
  lblThumbnailsByX    .Top := lblByX    .Top;
  edtThumbnailRows    .Top := edtHeight .Top;
  updThumbnailRows    .Top := updHeight .Top;

  grpThumbnailsOther  .Left := grpOther .Left;
  grpThumbnailsOther  .Top := grpOther  .Top;
end;


const
  Zoom_Amount = 20;

procedure TfiePrnForm3.Delete1Click(Sender: TObject);
begin
  ImageEnMView1.ThumbWidth := ImageEnMView1.ThumbWidth - Zoom_Amount;
  ImageEnMView1.ThumbHeight := ImageEnMView1.ThumbHeight - Zoom_Amount;
end;

procedure TfiePrnForm3.Add1Click(Sender: TObject);
begin
  ImageEnMView1.ThumbWidth := ImageEnMView1.ThumbWidth + Zoom_Amount;
  ImageEnMView1.ThumbHeight := ImageEnMView1.ThumbHeight + Zoom_Amount;
end;

procedure TfiePrnForm3.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfiePrnForm3.btnNextClick(Sender: TObject);
begin
  Inc(fThumbnailPageIndex);
  DelayedPrintPreview(nil);
end;

procedure TfiePrnForm3.btnPreviousClick(Sender: TObject);
begin
  Dec(fThumbnailPageIndex);
  DelayedPrintPreview(nil);
end;

procedure TfiePrnForm3.btnSetupClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  DelayedPrintPreview(self);
end;


procedure TfiePrnForm3.cmbPositionDrawItem(Control: TWinControl; Index:
    Integer; Rect: TRect; State: TOwnerDrawState);
var
  iGlyph : Integer;
  sText  : string;
begin
  iGlyph := Index;
  sText  := cmbPosition.Items[Index];
  IEDrawComboListBoxItem(TCombobox(Control), Rect, sText, imlPositions, iGlyph);
end;

procedure TfiePrnForm3.DelayedPrintPreview(Sender: TObject);
begin
  // Reset Preview Timer
  tmrUpdatePreview.Enabled := False;
  tmrUpdatePreview.Enabled := True;
end;

procedure TfiePrnForm3.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Save parameters if they print or close, but do not cancel
  If ModalResult <> mrCancel then
    SaveParameters;

  // But always save dialog size       
  fPrintPreviewParams.DlgWidth  := -1;
  fPrintPreviewParams.DlgHeight := -1;
  if ClientWidth <> fOriginalDlgWidth then
    fPrintPreviewParams.DlgWidth := ClientWidth;
  if ClientHeight <> fOriginalDlgHeight then
    fPrintPreviewParams.DlgHeight := ClientHeight;
end;

procedure TfiePrnForm3.updHeightClick(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(edtHeight, Button);
end;

procedure TfiePrnForm3.updThumbnailColumnsClick(Sender: TObject; Button:
    TUDBtnType);
var
  iNewValue: Integer;
begin
  case button of
    btNext:
      iNewValue := StrToIntDef(edtThumbnailColumns.Text, 0) + 1;
    btPrev:
      iNewValue := StrToIntDef(edtThumbnailColumns.Text, 0) - 1;
    else
      iNewValue := 0; // avoid Delphi warning
  end;
  if iNewValue < 1 then
    iNewValue := 1;  
  edtThumbnailColumns.Text := IntToStr(iNewValue);
end;

procedure TfiePrnForm3.updThumbnailRowsClick(Sender: TObject; Button:
    TUDBtnType);      
var
  iNewValue: Integer;
begin
  case button of
    btNext:
      iNewValue := StrToIntDef(edtThumbnailRows.Text, 0) + 1;
    btPrev:
      iNewValue := StrToIntDef(edtThumbnailRows.Text, 0) - 1;
    else
      iNewValue := 0; // avoid Delphi warning
  end;
  if iNewValue < 1 then
    iNewValue := 1;
  edtThumbnailRows.Text := IntToStr(iNewValue);
end;

procedure TfiePrnForm3.updThumbnailSpacingClick(Sender: TObject; Button:
    TUDBtnType);
begin
  incdecmargins(edtThumbnailSpacing, Button);
end;

procedure TfiePrnForm3.updWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  incdecmargins(edtWidth, Button);
end;

      
procedure TfiePrnForm3.SetDialogCaption(value: string);
begin
  fDialogCaption := Value;
  Caption := Value;
end;



{$ELSE} // {$ifdef IEINCLUDEPRINTDIALOGS}

interface
implementation

{$ENDIF}

{$ELSE} // {$ifdef IEINCLUDEMULTIVIEW}

interface
implementation

{$ENDIF}





end.
