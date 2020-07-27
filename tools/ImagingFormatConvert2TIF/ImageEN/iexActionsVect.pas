{------------------------------------------------------------------------------)}
{                                                                              }
{  TActions for common TImageEnVect functions                                  }
{                                                                              }
{  Nigel Cross                                                                 }
{  Xequte Software                                                             }
{  nigel@xequte.com                                                            }
{  http://www.xequte.com                                                       }
{                                                                              }
{  © Xequte Software 2011-2014                                                 }
{                                                                              }
{------------------------------------------------------------------------------}


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


unit iexActionsVect;    

{$I ie.inc}
                  
{$IFDEF IEINCLUDEACTIONS}


interface

Uses
  ActnList, Classes, ieVect, ImageEnIO, imageenproc, iexActions, ImageEnView, hyiedefs;

Type

  TImageEnVectAction = class(TBaseImageEnViewAction)
  private
    function GetImageEnVect: TImageEnVect;
    procedure SetImageEnVect(const Value: TImageEnVect);    
    function ActiveImageEnVect : TImageEnVect;
    function HaveActiveImageEnVect : boolean;   
  protected
  public
  published
    property ImageEnVect: TImageEnVect read GetImageEnVect write SetImageEnVect;
  end;

  TImageEnVectAutoShrink = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectAutoStretch = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;  
    procedure UpdateLanguage();
  published
  end;
  
  TImageEnVectClear = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage();
  published
  end;
  
  TImageEnVectFit = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectFitToHeight = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectFitToWidth = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectZoomFullSize = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override; 
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectZoomIn = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectZoomOut = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;    

  TImageEnVectSetZoom = class(TImageEnVectAction, IIELanguageUpdatable)
  private
    fZoom : Integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
    property Zoom : Integer read fZoom write fZoom;
  end;

  TImageEnVectMouseInteract = class(TImageEnVectAction)
  private
    fMouseInteract : ImageEnView.TIEMouseInteractItems;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
  end;

  TImageEnVectMouseMovingScroll = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;  
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseScroll = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;  
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseSelect = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseSelectCircle = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;  
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseSelectLasso = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseSelectMagicWand = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;        
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseSelectPolygon = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;   
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseSelectZoom = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;  
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseZoom = class(TImageEnVectMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;    
    procedure UpdateLanguage();
  published
  end;  

  TImageEnVectMouseInteractVt = class(TImageEnVectAction)
  private
    fMouseInteractVt : TIEMouseInteractVtItems;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
  end;

  TImageEnVectMouseVtArea = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtLineLen = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;         
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutLine = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutBox = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;        
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutEllipse = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutBitmap = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;          
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutText = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtObjectSelect = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;       
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtDragLen = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;       
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutRuler = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutPolyLine = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutAngle = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;         
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutMemo = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;        
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtPutLineLabel = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtEditPolyline = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;        
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMouseVtUnStampMode = class(TImageEnVectMouseInteractVt, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;    
    procedure UpdateLanguage();
  published
  end;

  {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  TImageEnVectPromptToOpen = class(TImageEnVectAction, IIELanguageUpdatable)
  private    
    fDialogTitle : WideString;
    fDefaultFilter : TIOFileType;
    fLimitToFileType : TIOFileType;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;         
    procedure UpdateLanguage();
  published     
    property DialogTitle : WideString read fDialogTitle write fDialogTitle;
    property DefaultFilter : TIOFileType read fDefaultFilter write fDefaultFilter default -1;
    property LimitToFileType : TIOFileType read fLimitToFileType write fLimitToFileType default -1;
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  TImageEnVectPromptToSave = class(TImageEnVectAction, IIELanguageUpdatable)
  private       
    fDialogTitle : WideString;
    fDefaultFilter : TIOFileType;
    fLimitToFileType : TIOFileType;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage();
  published  
    property DialogTitle : WideString read fDialogTitle write fDialogTitle;
    property DefaultFilter : TIOFileType read fDefaultFilter write fDefaultFilter default -1;
    property LimitToFileType : TIOFileType read fLimitToFileType write fLimitToFileType default -1;
  end;
  {$ENDIF}
                   
  {$IFDEF IEINCLUDEDIALOGIO}
  TImageEnVectDoIOPreviews = class(TImageEnVectAction, IIELanguageUpdatable)
  private
    fPreviewParams : TPreviewParams;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
    property PreviewParams : TPreviewParams read fPreviewParams write fPreviewParams default [ppAll];
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEPRINTDIALOGS}
  TImageEnVectDoPrintPreviewDialog = class(TImageEnVectAction, IIELanguageUpdatable)
  private
    fDialogType : TIEDialogType;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
    property DialogType : TIEDialogType read fDialogType write fDialogType default iedtDialog;
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEPRINTDIALOGS}
  TImageEnVectPrintImageNormal = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage();
  published
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEPRINTDIALOGS}
  TImageEnVectPrintImageFitToPage = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;
  {$ENDIF}

  TImageEnVectObjectAction = class(TImageEnVectAction)
  private
    fRequireObjectSelection : Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
  published
  end;

  TImageEnVectObjCopyToClipboard = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectObjCutToClipboard = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectObjPasteFromClipboard = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
  end;   

  TImageEnVectCopyToClipboard = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectSelAllObjects = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;        
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectUnSelAllObjects = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;         
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectRotateObjectRight = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectRotateObject180 = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectRotateObjectLeft = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectCropImageToObjects = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMergeAllToBackground = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectMergeObjToBackground = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectObjBringToFront = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectObjBringForward = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectObjSendToBack = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;    
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectObjSendBackward = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;   
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectRemoveObject = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectRemoveAllObjects = class(TImageEnVectObjectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage();
  published
  end;

  TImageEnVectObjUndo = class(TImageEnVectAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;


implementation

uses
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  Printers,
  {$ENDIF}
  Forms, Windows, hyieutils, iewords
  {$IFDEF IEHASTYPES}
  , Types
  {$ENDIF}
  , iesettings
  ;

const
  CTRL_DELETE_SHORTCUT  = 16430;

{ TImageEnVectAutoShrink }

constructor TImageEnVectAutoShrink.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ImageIndex := 39;
  UpdateLanguage();
end;

procedure TImageEnVectAutoShrink.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AutoShrink);
  Hint := iemsg(IEMsg_DisplayLargeImagesAtTheWindowSize);
end;

procedure TImageEnVectAutoShrink.ExecuteTarget(Target: TObject);
var
  bAutoShrink: Boolean;
begin
  if HaveActiveImageEnVect then
  begin
    bAutoShrink := not ActiveImageEnVect.AutoShrink;
    ActiveImageEnVect.AutoShrink := bAutoShrink;
    if bAutoShrink then
      ActiveImageEnVect.Update
    else
      ActiveImageEnVect.Zoom := 100;
  end;
end;

procedure TImageEnVectAutoShrink.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if HaveActiveImageEnVect then
    Checked := ActiveImageEnVect.AutoShrink
  else
    Checked := False;
end;


{ TImageEnVectAutoStretch }

constructor TImageEnVectAutoStretch.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ImageIndex := 40;
  UpdateLanguage();
end;

procedure TImageEnVectAutoStretch.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AutoStretch);
  Hint := iemsg(IEMsg_DisplaySmallImagesAtTheWindowsize);
end;

procedure TImageEnVectAutoStretch.ExecuteTarget(Target: TObject);
var
  bAutoStretch: Boolean;
begin
  if HaveActiveImageEnVect then
  begin
    bAutoStretch := not ActiveImageEnVect.AutoStretch;
    ActiveImageEnVect.AutoStretch := bAutoStretch;
    if bAutoStretch then
      ActiveImageEnVect.Update
    else
      ActiveImageEnVect.Zoom := 100;
  end;
end;

procedure TImageEnVectAutoStretch.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if HaveActiveImageEnVect then
    Checked := ActiveImageEnVect.AutoStretch
  else
    Checked := False;
end;


{ TImageEnVectClear }

constructor TImageEnVectClear.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 25;
  UpdateLanguage();
end;

procedure TImageEnVectClear.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Clear);
  Hint := iemsg(IEMsg_ClearThisImage);
end;

procedure TImageEnVectClear.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
  begin
    ActiveImageEnVect.RemoveAllObjects;
    ActiveImageEnVect.Clear;
  end;
end;


{ TImageEnVectFit }

constructor TImageEnVectFit.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 41;
  UpdateLanguage();
end;

procedure TImageEnVectFit.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FitImage);
  Hint := iemsg(IEMsg_DisplayTheimageAtThesizeOfTheWindow);
end;

procedure TImageEnVectFit.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then   
  begin
    ActiveImageEnVect.AutoShrink  := False;
    ActiveImageEnVect.AutoStretch := False;
    ActiveImageEnVect.Fit;
  end;
end;


{ TImageEnVectFitToHeight }

constructor TImageEnVectFitToHeight.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 42;
  UpdateLanguage();
end;

procedure TImageEnVectFitToHeight.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FitImageToHeight);
  Hint := iemsg(IEMsg_DisplayTheimageAtTheheightOfTheWindow);
end;

procedure TImageEnVectFitToHeight.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then    
  begin
    ActiveImageEnVect.AutoShrink  := False;
    ActiveImageEnVect.AutoStretch := False;
    ActiveImageEnVect.FitToHeight;
  end;
end;


{ TImageEnVectFitToWidth }

constructor TImageEnVectFitToWidth.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 43;
  UpdateLanguage();
end;

procedure TImageEnVectFitToWidth.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FitImageToWidth);
  Hint := iemsg(IEMsg_DisplayTheimageAtThewidthOfTheWindow);
end;

procedure TImageEnVectFitToWidth.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then    
  begin
    ActiveImageEnVect.AutoShrink  := False;
    ActiveImageEnVect.AutoStretch := False;
    ActiveImageEnVect.FitToWidth;
  end;
end;


{ TImageEnVectZoomIn }

constructor  TImageEnVectZoomIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImageIndex := 22;
  UpdateLanguage();
end;

procedure TImageEnVectZoomIn.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomIn);
  Hint := iemsg(IEMsg_DisplayTheimageLarger);
end;

procedure  TImageEnVectZoomIn.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
  begin
    ActiveImageEnVect.AutoShrink  := False;
    ActiveImageEnVect.AutoStretch := False;
    ActiveImageEnVect.ZoomIn;
  end;
end;


{ TImageEnVectZoomOut }

constructor TImageEnVectZoomOut.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 23;
  UpdateLanguage();
end;

procedure TImageEnVectZoomOut.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomOut);
  Hint := iemsg(IEMsg_DisplayTheImageSmaller);
end;

procedure TImageEnVectZoomOut.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then 
  begin
    ActiveImageEnVect.AutoShrink  := False;
    ActiveImageEnVect.AutoStretch := False;
    ActiveImageEnVect.ZoomOut;
  end;
end;


{ TImageEnVectZoomFullSize }

constructor TImageEnVectZoomFullSize.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 24;
  UpdateLanguage();
end;

procedure TImageEnVectZoomFullSize.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomToFullSize);
  Hint := iemsg(IEMsg_DisplayTheImageAtFullSize);
end;

procedure TImageEnVectZoomFullSize.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
  begin
    ActiveImageEnVect.AutoShrink  := False;
    ActiveImageEnVect.AutoStretch := False;
    ActiveImageEnVect.Zoom := 100;
  end;
end;



{ TImageEnViewSetZoom }

constructor TImageEnVectSetZoom.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 96;
  fZoom      := 50; 
  UpdateLanguage();
end;

procedure TImageEnVectSetZoom.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomImage);
  Hint := iemsg(IEMsg_DisplayImageAtCustomZoom);
end;

procedure TImageEnVectSetZoom.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
  begin
    ActiveImageEnVect.AutoShrink  := False;
    ActiveImageEnVect.AutoStretch := False;
    ActiveImageEnVect.Zoom := fZoom;
  end;
end;


{ TImageEnVectMouseInteract }

constructor TImageEnVectMouseInteract.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
end;

procedure TImageEnVectMouseInteract.ExecuteTarget(Target: TObject);
var
  bInclude: Boolean;
begin
  if HaveActiveImageEnVect then
  begin
    bInclude := not (fMouseInteract in ActiveImageEnVect.MouseInteract);

    if bInclude then
      ActiveImageEnVect.MouseInteract := ActiveImageEnVect.MouseInteract + [fMouseInteract]
    else
      ActiveImageEnVect.MouseInteract := ActiveImageEnVect.MouseInteract - [fMouseInteract];

    // Options that affect other properties
    if bInclude then
    begin
      if fMouseInteract in [miZoom, miSelectZoom] then
      begin
        ActiveImageEnVect.AutoStretch := False;
        ActiveImageEnVect.AutoShrink  := False;
      end;
    end;
  end;
end;


procedure TImageEnVectMouseInteract.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if ActiveImageEnVect = nil then
    Checked := False
  else
    Checked := fMouseInteract in ActiveImageEnVect.MouseInteract;
end;


{ TImageEnVectMouseMovingScroll }

constructor TImageEnVectMouseMovingScroll.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miMovingScroll;
  ImageIndex := 49;
  UpdateLanguage();
end;

procedure TImageEnVectMouseMovingScroll.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ScrollToCursor);
  Hint := iemsg(IEMsg_MoveThemouseToscrollTheImage);
end;


{ TImageEnVectMouseScroll }

constructor TImageEnVectMouseScroll.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miScroll;
  ImageIndex := 52;
  UpdateLanguage();
end;

procedure TImageEnVectMouseScroll.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ScrollImage);
  Hint := iemsg(IEMsg_ClickTheImageAnddragTheMouseToscroll);
end;


{ TImageEnVectMouseSelect }

constructor TImageEnVectMouseSelect.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelect;
  ImageIndex := 53;
  UpdateLanguage();
end;

procedure TImageEnVectMouseSelect.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RectangularSelect);
  Hint := iemsg(IEMsg_SelectArectangularAreaOfYourImage);
end;


{ TImageEnVectMouseSelectCircle }

constructor TImageEnVectMouseSelectCircle.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectCircle;
  ImageIndex := 54;
  UpdateLanguage();
end;

procedure TImageEnVectMouseSelectCircle.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CircularSelect);
  Hint := iemsg(IEMsg_SelectAcircularAreaOfYourImage);
end;


{ TImageEnVectMouseSelectLasso }

constructor TImageEnVectMouseSelectLasso.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectLasso;
  ImageIndex := 55;
  UpdateLanguage();
end;

procedure TImageEnVectMouseSelectLasso.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_LassoSelect);
  Hint := iemsg(IEMsg_PerformAnIrregularSelectionOfYourImageByDraggingTheMouse);
end;


{ TImageEnVectMouseSelectMagicWand }

constructor TImageEnVectMouseSelectMagicWand.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectMagicWand;
  ImageIndex := 56;
  UpdateLanguage();
end;

procedure TImageEnVectMouseSelectMagicWand.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SelectByColor);
  Hint := iemsg(IEMsg_SelectAPortionOfYourImageOfASimilarColor);
end;


{ TImageEnVectMouseSelectPolygon }

constructor TImageEnVectMouseSelectPolygon.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectPolygon;
  ImageIndex := 57;
  UpdateLanguage();
end;

procedure TImageEnVectMouseSelectPolygon.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PolygonSelect);
  Hint := iemsg(IEMsg_PerformAnIrregularSelectionOfYourImageByClickingTheMouse);
end;


{ TImageEnVectMouseSelectZoom }

constructor TImageEnVectMouseSelectZoom.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectZoom;
  ImageIndex := 58;
  UpdateLanguage();
end;

procedure TImageEnVectMouseSelectZoom.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomToSelection);
  Hint := iemsg(IEMsg_SelectAnAreaOfTheImageTozoomInto);
end;


{ TImageEnVectMouseZoom }

constructor TImageEnVectMouseZoom.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miZoom;
  ImageIndex := 59;
  UpdateLanguage();
end;

procedure TImageEnVectMouseZoom.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Zoom);
  Hint := iemsg(IEMsg_LeftclickTheImageToZoomInRightclickToZoomOut);
end;


{ TImageEnVectAction }

function TImageEnVectAction.ActiveImageEnVect: TImageEnVect;
begin
  Result := nil;
  if HaveActiveImageEnVect then
    Result := TImageEnVect(ActiveImageEnView);
end;



function TImageEnVectAction.GetImageEnVect: TImageEnVect;
begin
  Result := TImageEnVect(fImageEnView);
end;

function TImageEnVectAction.HaveActiveImageEnVect: boolean;
begin
  Result := HaveActiveImageEnView
end;

procedure TImageEnVectAction.SetImageEnVect(const Value: TImageEnVect);
begin
  fImageEnView := Value;
end;


{ TImageEnVectPromptToOpen }

{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnVectPromptToOpen.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ShortCut := CTRL_O_SHORTCUT;
  ImageIndex := 7;       
  fDefaultFilter   := -1;
  fLimitToFileType := -1;
  UpdateLanguage();
end;

procedure TImageEnVectPromptToOpen.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Open);
  Hint := iemsg(IEMsg_LoadAnImageFromFile);
end;

procedure TImageEnVectPromptToOpen.ExecuteTarget(Target: TObject);
var
  sFilename: string;
  ex: string;
begin
  if HaveActiveImageEnIO then
  begin
    sFilename := ActiveImageEnIO.ExecuteOpenDialog('', '', false, 0, '', fDialogTitle, '', fDefaultFilter, fLimitToFileType);
    ex := IEExtractFileExtS(sFileName);
    if (ex = '.iev') then
      ActiveImageEnVect.LoadFromFileIEV(sFilename)
    else
    if (ex = '.all') then
      ActiveImageEnVect.LoadFromFileALL(sFilename)
    else
    if (ex = '.dxf') then
      ActiveImageEnVect.ImportDXF(sFilename)
    else
    if sFilename <> '' then
      ActiveImageEnIO.LoadFromFile(sFilename);
  end;
end;
{$ENDIF}


{ TImageEnVectPromptToSave }
                  
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnVectPromptToSave.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_ALT_S_SHORTCUT;
  ImageIndex := 9;     
  fDefaultFilter   := -1;
  fLimitToFileType := -1;
  UpdateLanguage();
end;

procedure TImageEnVectPromptToSave.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SaveAs);
  Hint := iemsg(IEMsg_SaveThisImageToANewFilename);
end;

procedure TImageEnVectPromptToSave.ExecuteTarget(Target: TObject);
var
  sFilename: string;
  ex: string;
  ABitmap: TIEBitmap;
begin
  if HaveActiveImageEnIO then
  begin                                   
    sFilename := ActiveImageEnIO.ExecuteSaveDialog('', '', false, 0, '', fDialogTitle, '', fDefaultFilter, fLimitToFileType);
    ex := IEExtractFileExtS(sFileName);
    if (ex = '.iev') then
      ActiveImageEnVect.SaveToFileIEV(sFilename)
    else
    if (ex = '.all') then
      ActiveImageEnVect.SaveToFileALL(sFilename)
    else
    if sFilename <> '' then
    begin
      ABitmap := TIEBitmap.create;
      ABitmap.Assign(ActiveImageEnVect.IEBitmap);
      ActiveImageEnVect.DrawObjectsToBitmap(ABitmap);
      try
        with TImageEnIO.CreateFromBitmap(ABitmap) do
        begin
          AssignParams(ActiveImageEnVect.IO.Params);
          SaveToFile(sFilename);
          Free;
        end;
      finally
        ABitmap.Free;
      end;
    end;
  end;
end;
{$ENDIF}


{ TImageEnVectDoIOPreviews }

{$IFDEF IEINCLUDEDIALOGIO}
constructor TImageEnVectDoIOPreviews.Create(AOwner: TComponent);
begin
  inherited;
  fPreviewParams := [ppAll];
  ImageIndex := 10;
  UpdateLanguage();
end;

procedure TImageEnVectDoIOPreviews.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SaveProperties);
  Hint := iemsg(IEMsg_SpecifyAdvancedPropertiesForThisImage);
end;

procedure TImageEnVectDoIOPreviews.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.DoPreviews(fPreviewParams);
end;
{$ENDIF}


{ TImageEnVectDoPrintPreviewDialog }
                            
{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnVectDoPrintPreviewDialog.Create(AOwner: TComponent);
begin
  inherited;
  fDialogType := iedtDialog;
  Shortcut := CTRL_P_SHORTCUT;
  ImageIndex := 27;
  UpdateLanguage();
end;

procedure TImageEnVectDoPrintPreviewDialog.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PrintPreview);
  Hint := iemsg(IEMsg_DisplayAPreviewOfThisImageForPrinting);
end;

procedure TImageEnVectDoPrintPreviewDialog.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    with TImageEnView.Create(nil) do
    begin
      IEBitmap.Assign(ActiveImageEnVect.IEBitmap);
      ActiveImageEnVect.DrawObjectsToBitmap(IEBitmap);
      IO.DoPrintPreviewDialog(fDialogType);
      Free();
    end;
end;
{$ENDIF}


{ TImageEnVectPrintImageNormal }
                                              
{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnVectPrintImageNormal.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 26;
  UpdateLanguage();
end;

procedure TImageEnVectPrintImageNormal.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Print);
  Hint := iemsg(IEMsg_PrintThisImageAtItsOriginalSize);
end;

procedure TImageEnVectPrintImageNormal.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    with TImageEnView.Create(nil) do
    begin
      IEBitmap.Assign(ActiveImageEnVect.IEBitmap);
      ActiveImageEnVect.DrawObjectsToBitmap(IEBitmap);
      IO.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesNORMAL);
      Free();
    end;
end;
{$ENDIF}


{ TImageEnVectPrintImageFitToPage }
                                                         
{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnVectPrintImageFitToPage.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 26;
  UpdateLanguage();
end;

procedure TImageEnVectPrintImageFitToPage.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PrintToPage);
  Hint := iemsg(IEMsg_PrintThisImageTofitThepage);
end;

procedure TImageEnVectPrintImageFitToPage.ExecuteTarget(Target: TObject);
begin  
  if HaveActiveImageEnVect then
    with TImageEnView.Create(nil) do
    begin
      IEBitmap.Assign(ActiveImageEnVect.IEBitmap);
      ActiveImageEnVect.DrawObjectsToBitmap(IEBitmap);
      IO.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE);
      Free();
    end;
end;
{$ENDIF}


{ TImageEnVectObjectAction }

constructor TImageEnVectObjectAction.Create(AOwner: TComponent);
begin
  inherited;
  fRequireObjectSelection := True;
end;

procedure TImageEnVectObjectAction.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled then
  begin
    if fRequireObjectSelection and HaveActiveImageEnVect then
      bEnabled := ActiveImageEnVect.SelObjectsCount > 0
    else
      bEnabled := ActiveImageEnVect.ObjectsCount > 0;
  end;
  Enabled := bEnabled;
end;

{ TImageEnVectObjCopyToClipboard }

constructor TImageEnVectObjCopyToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_C_SHORTCUT;
  ImageIndex := 1;
  UpdateLanguage();
end;

procedure TImageEnVectObjCopyToClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Copy);
  Hint := iemsg(IEMsg_CopyTheSelectedObjectToTheClipboard);
end;

procedure TImageEnVectObjCopyToClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.ObjCopyToClipboard;
end;

{ TImageEnVectObjCutToClipboard }

constructor TImageEnVectObjCutToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_X_SHORTCUT;
  ImageIndex := 0;
  UpdateLanguage();
end;

procedure TImageEnVectObjCutToClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Cut);
  Hint := iemsg(IEMsg_MoveTheselectedObjectToTheClipboard);
end;

procedure TImageEnVectObjCutToClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.ObjCutToClipboard;
end;

{ TImageEnVectObjPasteFromClipboard }

constructor TImageEnVectObjPasteFromClipboard.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  Shortcut := CTRL_V_SHORTCUT;
  ImageIndex := 2;
  UpdateLanguage();
end;

procedure TImageEnVectObjPasteFromClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Paste);
  Hint := iemsg(IEMsg_PasteObjectFromTheclipboard);
end;

procedure TImageEnVectObjPasteFromClipboard.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnVect then
    bEnabled := ActiveImageEnVect.ObjIsClipboardAvailable;
  Enabled := bEnabled;
end;

procedure TImageEnVectObjPasteFromClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.ObjPasteFromClipboard(-1, -1);
end;


{ TImageEnVectCopyToClipboard }

constructor TImageEnVectCopyToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 1;
  UpdateLanguage();
end;

procedure TImageEnVectCopyToClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CopyImage);
  Hint := iemsg(IEMsg_CopyImageToTheClipboard);
end;

procedure TImageEnVectCopyToClipboard.ExecuteTarget(Target: TObject);
var
  ABitmap: TIEBitmap;
begin
  if HaveActiveImageEnVect then
  begin
    ABitmap := TIEBitmap.create;
    ABitmap.Assign(ActiveImageEnVect.IEBitmap);
    ActiveImageEnVect.DrawObjectsToBitmap(ABitmap);
    try
      with TImageEnProc.CreateFromBitmap(ABitmap) do
      begin
        CopyToClipboard;
        Free;
      end;
    finally
      ABitmap.Free;
    end;
  end;
end;


{ TImageEnVectSelAllObjects }

constructor TImageEnVectSelAllObjects.Create(AOwner: TComponent);
begin
  inherited;
  fRequireObjectSelection := False;
  Shortcut := CTRL_A_SHORTCUT;
  ImageIndex := 11;
  UpdateLanguage();
end;

procedure TImageEnVectSelAllObjects.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SelectAll);
  Hint := iemsg(IEMsg_SelectAllObjectsOfImage);
end;

procedure TImageEnVectSelAllObjects.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.SelAllObjects;
end;


{ TImageEnVectUnSelAllObjects }

constructor TImageEnVectUnSelAllObjects.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 12;
  UpdateLanguage();
end;

procedure TImageEnVectUnSelAllObjects.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_DeselectAll);
  Hint := iemsg(IEMsg_DeselectAllObjects);
end;

procedure TImageEnVectUnSelAllObjects.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.UnSelAllObjects;
end;


{ TImageEnVectRotateObjectRight }

constructor TImageEnVectRotateObjectRight.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 15;
  UpdateLanguage();
end;

procedure TImageEnVectRotateObjectRight.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RotateRight);
  Hint := iemsg(IEMsg_RotateTheselectedObject90clockwise);
end;

procedure TImageEnVectRotateObjectRight.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.RotateObject(IEV_ALL_SELECTED_OBJECTS, 270, ierObject);
end;


{ TImageEnVectRotateObject180 }

constructor TImageEnVectRotateObject180.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 14;
  UpdateLanguage();
end;

procedure TImageEnVectRotateObject180.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Rotate180);
  Hint := iemsg(IEMsg_RotateTheselectedObject180clockwise);
end;

procedure TImageEnVectRotateObject180.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.RotateObject(IEV_ALL_SELECTED_OBJECTS, 180, ierObject);
end;


{ TImageEnVectRotateObjectLeft }

constructor TImageEnVectRotateObjectLeft.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 13;
  UpdateLanguage();
end;

procedure TImageEnVectRotateObjectLeft.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RotateLeft);
  Hint := iemsg(IEMsg_RotateTheselectedObject90counterclockwise);
end;

procedure TImageEnVectRotateObjectLeft.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.RotateObject(IEV_ALL_SELECTED_OBJECTS, 90, ierObject);
end;


{ TImageEnVectCropImageToObjects }

constructor TImageEnVectCropImageToObjects.Create(AOwner: TComponent);
begin
  inherited;
  fRequireObjectSelection := False;
  ImageIndex := 87;
  UpdateLanguage();
end;

procedure TImageEnVectCropImageToObjects.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CropImageToObjects);
  Hint := iemsg(IEMsg_MatchThesizeOfThebackgroundImageToallOfTheobjectsItContains);
end;

procedure TImageEnVectCropImageToObjects.ExecuteTarget(Target: TObject);
var
  rc: TRect;
begin
  if HaveActiveImageEnVect then
  begin
    rc := ActiveImageEnVect.ObjectsExtents;
    ActiveImageEnVect.Bitmap.Width := rc.Right;
    ActiveImageEnVect.Bitmap.Height := rc.Bottom;
    ActiveImageEnVect.Update;
  end;
end;


{ TImageEnVectMergeAllToBackground }

constructor TImageEnVectMergeAllToBackground.Create(AOwner: TComponent);
begin
  inherited;
  fRequireObjectSelection := False;
  ImageIndex := 66;
  UpdateLanguage();
end;

procedure TImageEnVectMergeAllToBackground.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_MergeAllToBackground);
  Hint := iemsg(IEMsg_MergeAllObjectsWithTheBackgroundLayer);
end;

procedure TImageEnVectMergeAllToBackground.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
  begin
    ActiveImageEnVect.CopyObjectsToBack;
    ActiveImageEnVect.RemoveAllObjects;
  end;
end;


{ TImageEnVectMergeObjToBackground }

constructor TImageEnVectMergeObjToBackground.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 65;
  UpdateLanguage();
end;

procedure TImageEnVectMergeObjToBackground.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_MergeToBackground);
  Hint := iemsg(IEMsg_MergeTheselectedObjectWithThebackgroundLayer);
end;

procedure TImageEnVectMergeObjToBackground.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
  begin
    ActiveImageEnVect.CopyObjectToBack(IEV_ALL_SELECTED_OBJECTS);
    ActiveImageEnVect.RemoveObject(IEV_ALL_SELECTED_OBJECTS);
  end;
end;


{ TImageEnVectObjBringToFront }

constructor TImageEnVectObjBringToFront.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 67;
  UpdateLanguage();
end;

procedure TImageEnVectObjBringToFront.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_BringToFront);
  Hint := iemsg(IEMsg_BringTheselectedObjectToThefrontOfAllOtherObjects);
end;

procedure TImageEnVectObjBringToFront.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnVect and (ActiveImageEnVect.SelObjectsCount = 1) then
    bEnabled := ActiveImageEnVect.GetIndexFromObj(ActiveImageEnVect.SelObjects[0]) < ActiveImageEnVect.ObjectsCount -1;
  Enabled := bEnabled;
end;

procedure TImageEnVectObjBringToFront.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.SetObjFrontOf(IEV_ALL_SELECTED_OBJECTS, -1);
end;


{ TImageEnVectObjBringForward }

constructor TImageEnVectObjBringForward.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 68;
  UpdateLanguage();
end;

procedure TImageEnVectObjBringForward.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_BringForward);
  Hint := iemsg(IEMsg_MoveTheselectedObjectForward);
end;

procedure TImageEnVectObjBringForward.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnVect and (ActiveImageEnVect.SelObjectsCount = 1) then
    bEnabled := ActiveImageEnVect.GetIndexFromObj(ActiveImageEnVect.SelObjects[0]) < ActiveImageEnVect.ObjectsCount -1;
  Enabled := bEnabled;
end;

procedure TImageEnVectObjBringForward.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.SetObjFrontOf(IEV_ALL_SELECTED_OBJECTS, -2);
end;


{ TImageEnVectObjSendToBack }

constructor TImageEnVectObjSendToBack.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 69;
  UpdateLanguage();
end;

procedure TImageEnVectObjSendToBack.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SendToBack);
  Hint := iemsg(IEMsg_PositionTheselectedObjectBehindAllOtherObjects);
end;

procedure TImageEnVectObjSendToBack.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnVect and (ActiveImageEnVect.SelObjectsCount = 1) then
    bEnabled := ActiveImageEnVect.GetIndexFromObj(ActiveImageEnVect.SelObjects[0]) > 0;
  Enabled := bEnabled;
end;

procedure TImageEnVectObjSendToBack.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.SetObjBackTo(IEV_ALL_SELECTED_OBJECTS, -1);
end;


{ TImageEnVectObjSendBackward }

constructor TImageEnVectObjSendBackward.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 70;
  UpdateLanguage();
end;

procedure TImageEnVectObjSendBackward.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SendBackward);
  Hint := iemsg(IEMsg_MoveTheSelectedObjectBackward);
end;

procedure TImageEnVectObjSendBackward.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnVect and (ActiveImageEnVect.SelObjectsCount = 1) then
    bEnabled := ActiveImageEnVect.GetIndexFromObj(ActiveImageEnVect.SelObjects[0]) > 0;
  Enabled := bEnabled;
end;

procedure TImageEnVectObjSendBackward.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.SetObjBackTo(IEV_ALL_SELECTED_OBJECTS, -2);
end;


{ TImageEnVectRemoveObject }

constructor TImageEnVectRemoveObject.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_DELETE_SHORTCUT;
  ImageIndex := 5;
  UpdateLanguage();
end;

procedure TImageEnVectRemoveObject.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Delete);
  Hint := iemsg(IEMsg_RemoveTheselectedObjectFromTheImage);
end;

procedure TImageEnVectRemoveObject.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.RemoveObject(IEV_ALL_SELECTED_OBJECTS);
end;


{ TImageEnVectRemoveAllObjects }

constructor TImageEnVectRemoveAllObjects.Create(AOwner: TComponent);
begin
  inherited;
  fRequireObjectSelection := False;
  ImageIndex := 6;
  UpdateLanguage();
end;

procedure TImageEnVectRemoveAllObjects.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_DeleteAll);
  Hint := iemsg(IEMsg_RemoveAllObjectsFromTheImage);
end;

procedure TImageEnVectRemoveAllObjects.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.RemoveAllObjects;
end;


{ TImageEnVectObjUndo }

constructor TImageEnVectObjUndo.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_Z_SHORTCUT;
  ImageIndex := 3;
  UpdateLanguage();
end;

procedure TImageEnVectObjUndo.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Undo);
  Hint := iemsg(IEMsg_UndoThelastEdit);
end;

procedure TImageEnVectObjUndo.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnVect then
    bEnabled := ActiveImageEnVect.ObjCanUndo;
  Enabled := bEnabled;
end;

procedure TImageEnVectObjUndo.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnVect then
    ActiveImageEnVect.ObjUndo;
end;


{ TImageEnVectMouseInteractVt }

constructor TImageEnVectMouseInteractVt.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
end;

procedure TImageEnVectMouseInteractVt.ExecuteTarget(Target: TObject);
var
  bInclude: Boolean;
begin
  if HaveActiveImageEnVect then
  begin
    bInclude := not (fMouseInteractVt in ActiveImageEnVect.MouseInteractVt);

    if bInclude then
      ActiveImageEnVect.MouseInteractVt := ActiveImageEnVect.MouseInteractVt + [fMouseInteractVt]
    else
      ActiveImageEnVect.MouseInteractVt := ActiveImageEnVect.MouseInteractVt - [fMouseInteractVt];
  end;
end;

procedure TImageEnVectMouseInteractVt.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if ActiveImageEnVect = nil then
    Checked := False
  else
    Checked := fMouseInteractVt in ActiveImageEnVect.MouseInteractVt;
end;


{ TImageEnVectMouseVtArea }

constructor TImageEnVectMouseVtArea.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miArea;
  ImageIndex := 71;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtArea.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_MeasureArea);
  Hint := iemsg(IEMsg_MeasureARectangularArea);
end;


{ TImageEnVectMouseVtLineLen }

constructor TImageEnVectMouseVtLineLen.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miLineLen;
  ImageIndex := 72;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtLineLen.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_MeasureLength);
  Hint := iemsg(IEMsg_MeasureTheDistanceBetweenTwoPoints);
end;


{ TImageEnVectMouseVtPutLine }

constructor TImageEnVectMouseVtPutLine.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutLine;
  ImageIndex := 73;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutLine.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertLine);
  Hint := iemsg(IEMsg_AddAlineObject);
end;


{ TImageEnVectMouseVtPutBox }

constructor TImageEnVectMouseVtPutBox.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutBox;
  ImageIndex := 74;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutBox.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertRectangle);
  Hint := iemsg(IEMsg_AddAnRectangularObject);
end;


{ TImageEnVectMouseVtPutEllipse }

constructor TImageEnVectMouseVtPutEllipse.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutEllipse;
  ImageIndex := 75;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutEllipse.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertEllipse);
  Hint := iemsg(IEMsg_AddAnEllipicalObject);
end;


{ TImageEnVectMouseVtPutBitmap }

constructor TImageEnVectMouseVtPutBitmap.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutBitmap;
  ImageIndex := 76;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutBitmap.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertImage);
  Hint := iemsg(IEMsg_AddAnImageObject);
end;


{ TImageEnVectMouseVtPutText }

constructor TImageEnVectMouseVtPutText.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutText;
  ImageIndex := 77;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutText.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertText);
  Hint := iemsg(IEMsg_AddAtextObject);
end;


{ TImageEnVectMouseVtObjectSelect }

constructor TImageEnVectMouseVtObjectSelect.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miObjectSelect;
  ImageIndex := 78;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtObjectSelect.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Select);
  Hint := iemsg(IEMsg_SelectOrResizeAnObject);
end;


{ TImageEnVectMouseVtDragLen }

constructor TImageEnVectMouseVtDragLen.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miDragLen;
  ImageIndex := 79;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtDragLen.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_MeasureDistance);
  Hint := iemsg(IEMsg_DynamicallyMeasureADistance);
end;


{ TImageEnVectMouseVtPutRuler }

constructor TImageEnVectMouseVtPutRuler.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutRuler;
  ImageIndex := 80;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutRuler.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertRuler);
  Hint := iemsg(IEMsg_AddArulerObject);
end;


{ TImageEnVectMouseVtPutPolyLine }

constructor TImageEnVectMouseVtPutPolyLine.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutPolyLine;
  ImageIndex := 81;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutPolyLine.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertPolyline);
  Hint := iemsg(IEMsg_FreehandPaintAnOpenPolygon);
end;


{ TImageEnVectMouseVtPutAngle }

constructor TImageEnVectMouseVtPutAngle.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutAngle;
  ImageIndex := 82;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutAngle.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertAngle);
  Hint := iemsg(IEMsg_AddAnAngleMeasurementObject);
end;


{ TImageEnVectMouseVtPutMemo }

constructor TImageEnVectMouseVtPutMemo.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutMemo;
  ImageIndex := 83;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutMemo.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertMemo);
  Hint := iemsg(IEMsg_AddAMultilineTextObject);
end;


{ TImageEnVectMouseVtPutLineLabel }

constructor TImageEnVectMouseVtPutLineLabel.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miPutLineLabel;
  ImageIndex := 84;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtPutLineLabel.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_InsertLabelWithLine);
  Hint := iemsg(IEMsg_AddANewlabelObjectWithALine);
end;


{ TImageEnVectMouseVtEditPolyline }

constructor TImageEnVectMouseVtEditPolyline.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miEditPolyline;
  ImageIndex := 85;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtEditPolyline.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_EditPolyline);
  Hint := iemsg(IEMsg_EditPointsOfAPolyline);
end;


{ TImageEnVectMouseVtUnStampMode }

constructor TImageEnVectMouseVtUnStampMode.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteractVt := miUnStampMode;
  ImageIndex := 86;
  UpdateLanguage();
end;

procedure TImageEnVectMouseVtUnStampMode.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Unstamp);
  Hint := iemsg(IEMsg_SingleClickingWillNotInsertANewObject);
end;



{$ELSE} // {$IFDEF IEINCLUDEACTIONS}}

interface
implementation

{$ENDIF}







{!!
<FS>TImageEnVect Actions

<FN>
ImageEn includes a large set of actions for ImageEnVect, <L TImageEnView Actions>ImageEnView</L> and <L TImageEnMView Actions>ImageEnMView</L> components to allow you to rapidly develop your UI.

<FM>To use actions:<FN>
1. Add a TActionList component to your form
2. Double-click your TActionList to open it
3. Select "New Standard Action"
4. Scroll down to the ImageEnVect actions, select the ones you require and click OK
5. Select your actions and set the <FB>ImageEnVect<FN> property to your <A TImageEnVect> component
6. Assign the actions to menu items and buttons

See the demo for more information: Demos\Other\Actions_Vect\VectActions.dpr

<FM>Notes:<FN>
- Your must set the <FB>ImageEnVect<FN> property of the actions     
- You can set <A TIEImageEnGlobalSettings.MsgLanguage> to localize the actions
- See the <L Actions ImageIndex List>list of the default ImageIndexes</L> if you are planning to add graphics to your actions

<FM>General Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnVectAutoShrink</C> <C>Auto-Shrink</C> <C>Display large images at the window size</C> <C><A TImageEnView.AutoShrink></C> <C> -</C> </R>
<R> <C>TImageEnVectAutoStretch</C> <C>Auto-Stretch</C> <C>Display small images at the window size</C> <C><A TImageEnView.AutoStretch></C> <C> -</C> </R>
<R> <C>TImageEnVectClear</C> <C>Clear</C> <C>Clear this image</C> <C><A TImageEnVect.RemoveAllObjects>, <A TImageEnVect.RemoveAllObjects></C> <C> -</C> </R>
<R> <C>TImageEnVectFit</C> <C>Fit Image</C> <C>Display the image at the size of the window</C> <C><A TImageEnView.Fit></C> <C> -</C> </R>
<R> <C>TImageEnVectFitToHeight</C> <C>Fit Image to Height</C> <C>Display the image at the height of the window</C> <C><A TImageEnView.FitToHeight></C> <C> -</C> </R>
<R> <C>TImageEnVectFitToWidth</C> <C>Fit Image to Width</C> <C>Display the image at the width of the window</C> <C><A TImageEnView.FitToWidth></C> <C> -</C> </R>
<R> <C>TImageEnVectZoomIn</C> <C>Zoom In</C> <C>Display the image larger</C> <C><A TImageEnView.ZoomIn></C> <C> -</C> </R>
<R> <C>TImageEnVectZoomOut</C> <C>Zoom Out</C> <C>Display the image smaller</C> <C><A TImageEnView.ZoomOut></C> <C> -</C> </R>
<R> <C>TImageEnVectZoomFullSize</C> <C>Zoom to Full Size</C> <C>Display the image at full size </C> <C><A TImageEnView.Zoom></C> <C> -</C> </R>
<R> <C>TImageEnVectSetZoom</C> <C>Zoom to x%</C> <C>Display the image at x% Zoom </C> <C><A TImageEnView.Zoom></C> <C>Zoom</C> </R>
<R> <C>TImageEnVectCopyToClipboard</C> <C>Copy Image</C> <C>Copy image to the clipboard</C> <C><A TImageEnProc.CopyToClipboard></C> <C> -</C> </R>
</TABLE>

<FM>IO Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnVectPromptToOpen</C> <C>Open</C> <C>Load an image from file</C> <C><A TImageEnIO.ExecuteOpenDialog>, <A TImageEnVect.LoadFromFileIEV>, <A TImageEnVect.LoadFromFileALL>, <A TImageEnVect.ImportDXF>, <A TImageEnIO.LoadFromFile></C> <C>DialogTitle, DefaultFilter, LimitToFileType</C> </R>
<R> <C>TImageEnVectPromptToSave</C> <C>Save as</C> <C>Save this image to a new filename</C> <C><A TImageEnIO.ExecuteSaveDialog>, <A TImageEnVect.SaveToFileIEV>, <A TImageEnVect.SaveToFileALL>, <A TImageEnIO.SaveToFile></C> <C>DialogTitle, DefaultFilter, LimitToFileType</C> </R>
<R> <C>TImageEnVectDoIOPreviews</C> <C>Save Properties</C> <C>Specify advanced properties for this image</C> <C><A TImageEnIO.DoPreviews></C> <C><L TPreviewParams>PreviewParams</L></C> </R>
<R> <C>TImageEnVectDoPrintPreviewDialog</C> <C>Print Preview</C> <C>Display a preview of this image for printing</C> <C><A TImageEnIO.DoPrintPreviewDialog></C> <C><L TIEDialogType>DialogType</L></C> </R>
<R> <C>TImageEnVectPrintImageNormal</C> <C>Print</C> <C>Print this image at its original size</C> <C><A TImageEnIO.PrintImage></C> <C> -</C> </R>
<R> <C>TImageEnVectPrintImageFitToPage</C> <C>Print to Page</C> <C>Print this image to fit the page</C> <C><A TImageEnIO.PrintImage></C> <C> -</C> </R>
</TABLE>

<FM>Object Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnVectObjCopyToClipboard</C> <C>Copy</C> <C>Copy the selected object to the clipboard</C> <C><A TImageEnVect.ObjCopyToClipboard></C> <C> -</C> </R>
<R> <C>TImageEnVectObjCutToClipboard</C> <C>Cut</C> <C>Move the selected object to the clipboard</C> <C><A TImageEnVect.ObjCutToClipboard></C> <C> -</C> </R>
<R> <C>TImageEnVectObjPasteFromClipboard</C> <C>Paste</C> <C>Paste object from the clipboard</C> <C><A TImageEnVect.ObjPasteFromClipboard></C> <C> -</C> </R>
<R> <C>TImageEnVectSelAllObjects</C> <C>Select All</C> <C>Select all objects of image</C> <C><A TImageEnVect.SelAllObjects></C> <C> -</C> </R>
<R> <C>TImageEnVectUnSelAllObjects</C> <C>Deselect All</C> <C>Deselect all objects</C> <C><A TImageEnVect.UnSelAllObjects></C> <C> -</C> </R>
<R> <C>TImageEnVectRotateObjectRight</C> <C>Rotate Right</C> <C>Rotate the selected object 90° clockwise</C> <C><A TImageEnVect.RotateObject></C> <C> -</C> </R>
<R> <C>TImageEnVectRotateObject180</C> <C>Rotate 180°</C> <C>Rotate the selected object 180° clockwise</C> <C><A TImageEnVect.RotateObject></C> <C> -</C> </R>
<R> <C>TImageEnVectRotateObjectLeft</C> <C>Rotate Left</C> <C>Rotate the selected object 90° counter-clockwise</C> <C><A TImageEnVect.RotateObject></C> <C> -</C> </R>
<R> <C>TImageEnVectCropImageToObjects</C> <C>Crop Image to Objects</C> <C>Resize the image to match all of the objects that it contains</C> <C><A TImageEnVect.ObjectsExtents></C> <C> -</C> </R>
<R> <C>TImageEnVectMergeAllToBackground</C> <C>Merge All to Background</C> <C>Merge all objects with the background layer</C> <C><A TImageEnVect.CopyObjectsToBack>, <A TImageEnVect.RemoveAllObjects></C> <C> -</C> </R>
<R> <C>TImageEnVectMergeObjToBackground</C> <C>Merge to Background</C> <C>Merge the selected object with the background layer</C> <C><A TImageEnVect.CopyObjectToBack>, <A TImageEnVect.RemoveObject></C> <C> -</C> </R>
<R> <C>TImageEnVectObjBringToFront</C> <C>Bring to Front</C> <C>Position the selected object in front of all other objects</C> <C><A TImageEnVect.SetObjFrontOf></C> <C> -</C> </R>
<R> <C>TImageEnVectObjBringForward</C> <C>Bring Forward</C> <C>Move the selected object forward</C> <C><A TImageEnVect.SetObjFrontOf>  </C> <C> -</C> </R>
<R> <C>TImageEnVectObjSendToBack</C> <C>Send to Back</C> <C>Position the selected object behind all other objects</C> <C><A TImageEnVect.SetObjBackTo></C> <C> -</C> </R>
<R> <C>TImageEnVectObjSendBackward</C> <C>Send Backwards</C> <C>Move the selected object backward</C> <C><A TImageEnVect.SetObjBackTo>    </C> <C> -</C> </R>
<R> <C>TImageEnVectRemoveObject</C> <C>Delete</C> <C>Remove the selected object from the image</C> <C><A TImageEnVect.RemoveObject></C> <C> -</C> </R>
<R> <C>TImageEnVectRemoveAllObjects</C> <C>Delete All</C> <C>Remove all objects from the image</C> <C><A TImageEnVect.RemoveAllObjects></C> <C> -</C> </R>
<R> <C>TImageEnVectObjUndo</C> <C>Undo</C> <C>Undo the last edit</C> <C><A TImageEnVect.ObjUndo></C> <C> -</C> </R>
</TABLE>

<FM>Mouse Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnVectMouseVtArea</C> <C>Measure Area</C> <C>Measure a rectangula area</C> <C><L TImageEnVect.MouseInteractVt>miArea</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtLineLen</C> <C>Measure Length</C> <C>Measure the distance between two points</C> <C><L TImageEnVect.MouseInteractVt>miLineLen</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutLine</C> <C>Insert Line</C> <C>Add a line object</C> <C><L TImageEnVect.MouseInteractVt>miPutLine</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutBox</C> <C>Insert Rectangle</C> <C>Add an rectangular object</C> <C><L TImageEnVect.MouseInteractVt>miPutBox</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutEllipse</C> <C>Insert Ellipse</C> <C>Add an ellipical object</C> <C><L TImageEnVect.MouseInteractVt>miPutEllipse</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutBitmap</C> <C>Insert Image</C> <C>Add an image object</C> <C><L TImageEnVect.MouseInteractVt>miPutBitmap</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutText</C> <C>Insert Text</C> <C>Add a text object</C> <C><L TImageEnVect.MouseInteractVt>miPutText</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtObjectSelect</C> <C>Select</C> <C>Select or resize an object </C> <C><L TImageEnVect.MouseInteractVt>miObjectSelect</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtDragLen</C> <C>Measure Distance</C> <C>Dynamically measure a distance</C> <C><L TImageEnVect.MouseInteractVt>miDragLen</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutRuler</C> <C>Insert Ruler</C> <C>Add a ruler object</C> <C><L TImageEnVect.MouseInteractVt>miPutRuler</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutPolyLine</C> <C>Insert Polyline</C> <C>Free-hand paint an open polygon</C> <C><L TImageEnVect.MouseInteractVt>miPutPolyLine</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutAngle</C> <C>Insert Angle</C> <C>Add an angle measurement object </C> <C><L TImageEnVect.MouseInteractVt>miPutAngle</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutMemo</C> <C>Insert Memo</C> <C>Add a multi line text object</C> <C><L TImageEnVect.MouseInteractVt>miPutMemo</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtPutLineLabel</C> <C>Insert Label with Line</C> <C>Add a new label object with a line</C> <C><L TImageEnVect.MouseInteractVt>miPutLineLabel</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtEditPolyline</C> <C>Edit Polyline</C> <C>Edit points of a polyline </C> <C><L TImageEnVect.MouseInteractVt>miEditPolyline</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseVtUnStampMode</C> <C>Unstamp</C> <C>Single-clicking will not insert a new object"</C> <C><L TImageEnVect.MouseInteractVt>miUnStampMode</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseMovingScroll</C> <C>Scroll to Cursor</C> <C>Move the mouse to scroll the image</C> <C><L TImageEnView.MouseInteract>miMovingScroll</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseScroll</C> <C>Scroll Image</C> <C>Click the image and drag the mouse to scroll</C> <C><L TImageEnView.MouseInteract>miScroll</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseSelect</C> <C>Rectangular Select</C> <C>Select a rectangular area of your image</C> <C><L TImageEnView.MouseInteract>miSelect</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseSelectCircle</C> <C>Circular Select</C> <C>Select a circular area of your image</C> <C><L TImageEnView.MouseInteract>miSelectCircle</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseSelectLasso</C> <C>Lasso Select</C> <C>Perform an irregular selection of your image by dragging the mouse</C> <C><L TImageEnView.MouseInteract>miSelectLasso</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseSelectMagicWand</C> <C>Select by Color</C> <C>Select a portion of your image of a similar color</C> <C><L TImageEnView.MouseInteract>miSelectMagicWand</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseSelectPolygon</C> <C>Polygon Select</C> <C>Perform an irregular selection of your image by clicking the mouse</C> <C><L TImageEnView.MouseInteract>miSelectPolygon</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseSelectZoom</C> <C>Zoom to Selection</C> <C>Select an area of the image to zoom into</C> <C><L TImageEnView.MouseInteract>miSelectZoom</L></C> <C> -</C> </R>
<R> <C>TImageEnVectMouseZoom</C> <C>Zoom</C> <C>Left-click the image to Zoom In. Right-click to Zoom Out</C> <C><L TImageEnView.MouseInteract>miZoom</L></C> <C> -</C> </R>
</TABLE>

!!}


end.


