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
File version 1003
*)

unit iexAcquireForm;
// NPC: 18/11/11

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEIEXACQUIRE}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ImgList, hyiedefs;

type
  TiexAcquireForm = class(TForm)
    Image1: TImage;
    imlDevices: TImageList;
    pnlButtons: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblCaption: TLabel;
    lbxSources: TListBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbxSourcesClick(Sender: TObject);
    procedure lbxSourcesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
        State: TOwnerDrawState);
  private
    { Private declarations }
    Procedure UpdateDialogSize;
    function GetSelectedSource : string;
    procedure SetSelectedSource(const value : string);
  public
    { Public declarations }
    procedure UpdateLanguage();

    // Selected item in lbxSources as a raw device string
    property SelectedSource : string read GetSelectedSource write SetSelectedSource;

    {$IFDEF UNIT_TESTING}
    procedure SetNextLanguage;
    {$ENDIF}
  end;

implementation

{$R *.DFM}

uses
  iewords, hyieutils, iexAcquire, iesettings;




procedure TiexAcquireForm.btnOKClick(Sender: TObject);
begin
  if SelectedSource = '' then
    ModalResult := mrNone;
end;
           
{$IFDEF UNIT_TESTING}
procedure TiexAcquireForm.SetNextLanguage;
begin
  Self.Tag := Self.Tag + 1;
  if Self.tag > ord(msArabic) then
    Self.Tag := 0;
  IEGlobalSettings().MsgLanguage := TMsgLanguage(Self.Tag);
  UpdateLanguage();
end;
{$ENDIF}

procedure TiexAcquireForm.FormActivate(Sender: TObject);
begin
  Application.ProcessMessages; // first draws all controls (to avoid "Swiss Cheese")
  if IEGlobalSettings().UseButtonGlyphsInDialogs = False then
  begin
    btnOK.Glyph := nil;
    btnCancel.Glyph := nil;
  end;
  UpdateDialogSize;
  lbxSourcesClick(nil);
end;


procedure TiexAcquireForm.UpdateLanguage();
begin
  Caption := iemsg(IEMSG_SELECTACQUIREDEVICE);
  lblCaption.Caption := iemsg(IEMSG_SELECTANACQUISITIONDEVICE);
  btnOK.Caption := iemsg(IEMSG_OK);
  btnCancel.Caption := iemsg(IEMSG_CANCEL);
end;





procedure TiexAcquireForm.FormCreate(Sender: TObject);
begin
  case iegDialogsBackground of
    iedbPaper:
      begin
        IECreateOSXBackgroundPaper(Image1.Picture.Bitmap, Image1.Width, Image1.Height);
        Image1.Update;
      end;
    iedbMetal:
      begin
        IECreateOSXBackgroundMetal(Image1.Picture.Bitmap, Image1.Width, Image1.Height);
        Image1.Update;
      end;
  end;
end;

procedure TiexAcquireForm.FormResize(Sender: TObject);
begin
  UpdateDialogSize;
end;

procedure TiexAcquireForm.lbxSourcesClick(Sender: TObject);
begin
  btnOK.Enabled := SelectedSource <> '';
end;


Procedure TiexAcquireForm.UpdateDialogSize;
begin                                                  
  pnlButtons.Left   := ClientWidth - pnlButtons.Width;
  pnlButtons.Top    := ClientHeight - pnlButtons.Height;
  lbxSources.Width  := ClientWidth - (2 * lbxSources.Left);
  lbxSources.Height := pnlButtons.Top - lbxSources.Top - 5;
end;


function TiexAcquireForm.GetSelectedSource : string;
begin
  result := '';
  if lbxSources.ItemIndex > -1 then
    Result := lbxSources.Items[lbxSources.ItemIndex];
end;

procedure TiexAcquireForm.lbxSourcesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  Scanner_Glyph_Index = 0;
  Camera_Glyph_Index  = 1;
  Drive_Glyph_Index   = 2;
  Unknown_Glyph_Index = Scanner_Glyph_Index; // Just assume it is a scanner
begin
  DrawAcquireComboListBoxItem(Control, Rect, lbxSources.Items[Index], imlDevices,
                              Scanner_Glyph_Index, Camera_Glyph_Index, Drive_Glyph_Index, Unknown_Glyph_Index);

end;


procedure TiexAcquireForm.SetSelectedSource(const value : string);   
var
  iIndex: Integer;
begin
  iIndex := -1;
  if Value <> '' then
    iIndex := lbxSources.Items.IndexOf(value);
  lbxSources.ItemIndex := iIndex;
end;



{$ELSE} // {$ifdef IEINCLUDEIEXACQUIRE}

interface
implementation

{$ENDIF}


end.


