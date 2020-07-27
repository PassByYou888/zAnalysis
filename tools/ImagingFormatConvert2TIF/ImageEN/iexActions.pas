{------------------------------------------------------------------------------}
{                                                                              }
{  TActions for common TImageEnView functions                                  }
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
File version 1004
*)


unit iexActions;   

{$I ie.inc}
  
{$IFDEF IEINCLUDEACTIONS}

interface

Uses
  ActnList, Classes, ImageEnView, ImageEnIO, hyiedefs,
  {$IFDEF IEINCLUDEIEXACQUIRE}
  iexAcquire,
  {$ENDIF}
  imageenproc;

  

Type
  TBaseImageEnViewAction = class(TAction)
  private
  protected
    function ActiveImageEnView : TImageEnView;
    function HaveActiveImageEnView : boolean;    
    function ActiveImageEnProc : TImageEnProc;
    function HaveActiveImageEnProc : boolean;  
    function ActiveImageEnIO : TImageEnIO;
    function HaveActiveImageEnIO : boolean;
    function HaveImage : boolean;       
    function HaveSelection : boolean;
  public               
    fImageEnView : TImageEnView;
    fRequireImage : Boolean;   
    fRequireSelection : Boolean;
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; Override;
    procedure UpdateTarget(Target: TObject); override;
    function BaseEnabled : Boolean;
  published
  end;

  TImageEnViewAction = class(TBaseImageEnViewAction)
  private
  protected
  public
  published
    property ImageEnView: TImageEnView read fImageEnView write fImageEnView;
  end;

  TImageEnViewAutoShrink = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewAutoStretch = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewBlank = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewDeSelect = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewFit = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewFitToHeight = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override; 
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewFitToWidth = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersAdd = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override; 
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersMergeAll = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;        
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersRemoveCurrent = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersCreateFromClipboard = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;          
                             
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  TImageEnViewLayersCreateFromFile = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage(); 
  published
  end;        
{$ENDIF}

  TImageEnViewLayersMoveSendToBack = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersMoveSendBackward = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersMoveBringForward = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersMoveBringToFront = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewLayersFixBorders = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewEnableAdjustOrientation = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewPlaying = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewPlayLoop = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;          

  TImageEnViewZoomFullSize = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewZoomIn = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewZoomOut = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage(); 
  published
  end;       

  TImageEnViewSetZoom = class(TImageEnViewAction, IIELanguageUpdatable)
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

  TImageEnViewMouseInteract = class(TBaseImageEnViewAction)
  private
    fMouseInteract : ImageEnView.TIEMouseInteractItems;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property ImageEnView: TImageEnView read fImageEnView write fImageEnView;
  end;

  TImageEnViewMouseMoveLayers = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseMovingScroll = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;  
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseResizeLayers = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;  
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseRotateLayers = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseScroll = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;    
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseSelect = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;      
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseSelectCircle = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;   
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseSelectLasso = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;    
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseSelectMagicWand = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseSelectPolygon = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;  
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseSelectZoom = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewMouseZoom = class(TImageEnViewMouseInteract, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;     
    procedure UpdateLanguage(); 
  published
  end;

  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnViewDoEffectPreviews = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnViewDoAdjustPreviews = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnViewPromptToRotate = class(TImageEnViewAction, IIELanguageUpdatable)
  private
    fAntiAliasMode : TIEAntialiasMode;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
    property AntiAliasMode : TIEAntialiasMode read fAntiAliasMode write fAntiAliasMode default ierFast;
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnViewPromptToResize = class(TImageEnViewAction, IIELanguageUpdatable)
  private
    fResampleFilter : TResampleFilter;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published              
    property ResampleFilter : TResampleFilter read fResampleFilter write fResampleFilter default rfFastLinear;
  end;
  {$ENDIF}

  TImageEnViewConvertToGray = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewHistAutoEqualize = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewNegative = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewRemoveRedEyes = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewSharpen = class(TImageEnViewAction, IIELanguageUpdatable)
  private
    fIntensity : integer;
    fNeighbourhood : integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage(); 
  published
    property Intensity : integer read fIntensity write fIntensity default 10;
    property Neighbourhood : integer read fNeighbourhood write fNeighbourhood default 4;
  end;
  
  TImageEnViewFlipHorizontal = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override; 
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewFlipVertical = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewRotateRight = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override; 
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewRotate180 = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewRotateLeft = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage(); 
  published       
  end;     

  TImageEnViewCrop = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewCopyToClipboard = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewPasteFromClipboard = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewSelCutToClip = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;
  
  TImageEnViewSelCopyToClip = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage(); 
  published
  end;

  TImageEnViewUndo = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override; 
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;    

  TImageEnViewRedo = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;

                      
  {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  TImageEnViewPromptToOpen = class(TImageEnViewAction, IIELanguageUpdatable)
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
  TImageEnViewSave = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateLanguage(); 
  published
  end;
  {$ENDIF}
                
  {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  TImageEnViewPromptToSave = class(TImageEnViewAction, IIELanguageUpdatable)
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
  TImageEnViewDoIOPreviews = class(TImageEnViewAction, IIELanguageUpdatable)
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
  TImageEnViewDoPrintPreviewDialog = class(TImageEnViewAction, IIELanguageUpdatable)
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
  TImageEnViewPrintImageNormal = class(TImageEnViewAction, IIELanguageUpdatable)
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
  TImageEnViewPrintImageFitToPage = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage(); 
  published
  end;
  {$ENDIF}


  {$IFDEF IEINCLUDEIEXACQUIRE}
  TImageEnViewSelectAcquireSource = class(TImageEnViewAction, IIELanguageUpdatable)
  private
    fApis : TIEAcquireApis;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
    property Apis : TIEAcquireApis read fApis write fApis default [ieaTwain, ieaWIA];
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEIEXACQUIRE}
  TImageEnViewAcquire = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateLanguage(); 
  published
  end;
  {$ENDIF}
    
  TImageEnViewSeekFirst = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;
    
  TImageEnViewSeekPrior = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;
    
  TImageEnViewSeekNext = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage(); 
  published
  end;
    
  TImageEnViewSeekLast = class(TImageEnViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;



const
  CTRL_A_SHORTCUT     = 16449;
  CTRL_C_SHORTCUT     = 16451;
  CTRL_O_SHORTCUT     = 16463;
  CTRL_P_SHORTCUT     = 16464;
  CTRL_S_SHORTCUT     = 16467;
  CTRL_V_SHORTCUT     = 16470;
  CTRL_X_SHORTCUT     = 16472;
  CTRL_Y_SHORTCUT     = 16473;
  CTRL_Z_SHORTCUT     = 16474;
  CTRL_ALT_S_SHORTCUT = 49235;

  
implementation

uses
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  Printers,
  {$ENDIF}
  Sysutils, Forms, iewords, iesettings, hyieutils;



constructor  TBaseImageEnViewAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRequireImage := True;
  fRequireSelection := False;
  Caption := 'ImageEn Action';  
  DisableIfNoHandler := false;
end;

function  TBaseImageEnViewAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

function TBaseImageEnViewAction.HaveActiveImageEnIO: boolean;
begin     
  Result := HaveActiveImageEnView; // It will always create a IO if it is required
end;

function TBaseImageEnViewAction.HaveActiveImageEnProc: boolean;
begin    
  Result := HaveActiveImageEnView; // It will always create a Proc if it is required
end;

function TBaseImageEnViewAction.HaveActiveImageEnView: boolean;
begin
  Result := ActiveImageEnView <> nil;
end;

// True if the active ImageEnView contains an image
function TBaseImageEnViewAction.HaveImage: boolean;
begin
  Result := HaveActiveImageEnView and
            (ActiveImageEnView.IsEmpty2 = False);
end;

function TBaseImageEnViewAction.HaveSelection: boolean;
begin
  Result := HaveActiveImageEnView and
            ActiveImageEnView.Selected;
end;

function TBaseImageEnViewAction.BaseEnabled: boolean;
begin
  Result := HaveActiveImageEnView;
  if Result and fRequireImage then
    Result := HaveImage;
  if Result and fRequireSelection then
    Result := HaveSelection;
end;

procedure TBaseImageEnViewAction.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
end;


function TBaseImageEnViewAction.ActiveImageEnIO: TImageEnIO;
begin
  Result := nil;
  if HaveActiveImageEnView then
    Result := ActiveImageEnView.IO;
end;                                  


function TBaseImageEnViewAction.ActiveImageEnProc: TImageEnProc;
begin
  Result := nil;
  if HaveActiveImageEnView then
    Result := ActiveImageEnView.Proc;
end;
                       
// Return the speciifed or selected ImageEnView
function TBaseImageEnViewAction.ActiveImageEnView: TImageEnView;
begin
  Result := fImageEnView;
  if (Result = nil) and Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TImageEnView) then
    Result := TImageEnView(Screen.ActiveControl);
end;


{ TImageEnViewAutoShrink }

constructor TImageEnViewAutoShrink.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ImageIndex := 39;
  UpdateLanguage();
end;

procedure TImageEnViewAutoShrink.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AutoShrink);
  Hint := iemsg(IEMsg_DisplayLargeImagesAtThewindowSize);
end;

procedure TImageEnViewAutoShrink.ExecuteTarget(Target: TObject);
var
  bAutoShrink: Boolean;
begin
  if HaveActiveImageEnView then
  begin
    bAutoShrink := not ActiveImageEnView.AutoShrink;
    ActiveImageEnView.AutoShrink := bAutoShrink;
    if bAutoShrink then
      ActiveImageEnView.Update
    else
      ActiveImageEnView.Zoom := 100;
  end;
end;

procedure TImageEnViewAutoShrink.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if HaveActiveImageEnView then
    Checked := ActiveImageEnView.AutoShrink
  else
    Checked := False;
end;


{ TImageEnViewAutoStretch }

constructor TImageEnViewAutoStretch.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ImageIndex := 40;
  UpdateLanguage();
end;

procedure TImageEnViewAutoStretch.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AutoStretch);
  Hint := iemsg(IEMsg_DisplaySmallImagesAtTheWindowSize);
end;

procedure TImageEnViewAutoStretch.ExecuteTarget(Target: TObject);
var
  bAutoStretch: Boolean;
begin
  if HaveActiveImageEnView then
  begin
    bAutoStretch := not ActiveImageEnView.AutoStretch;
    ActiveImageEnView.AutoStretch := bAutoStretch;
    if bAutoStretch then
      ActiveImageEnView.Update
    else
      ActiveImageEnView.Zoom := 100;
  end;
end;

procedure TImageEnViewAutoStretch.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if HaveActiveImageEnView then
    Checked := ActiveImageEnView.AutoStretch
  else
    Checked := False;
end;


{ TImageEnViewBlank }

constructor TImageEnViewBlank.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 25;
  UpdateLanguage();
end;

procedure TImageEnViewBlank.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Clear);
  Hint := iemsg(IEMsg_ClearThewindow);
end;

procedure TImageEnViewBlank.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.Blank;
end;


{ TImageEnViewDeSelect }

constructor TImageEnViewDeSelect.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  fRequireSelection := True;
  ImageIndex := 12;
  UpdateLanguage();
end;

procedure TImageEnViewDeSelect.UpdateLanguage();
begin 
  Caption := iemsg(IEMsg_Deselect);
  Hint := iemsg(IEMsg_ClearYourSelection);
end;

procedure TImageEnViewDeSelect.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.DeSelect;
end;


{ TImageEnViewFit }

constructor TImageEnViewFit.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 41;
  UpdateLanguage();
end;

procedure TImageEnViewFit.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FitImage);
  Hint := iemsg(IEMsg_DisplayTheImageAtTheSizeOfTheWindow);
end;

procedure TImageEnViewFit.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then   
  begin
    ActiveImageEnView.AutoShrink  := False;
    ActiveImageEnView.AutoStretch := False;
    ActiveImageEnView.Fit;
  end;
end;


{ TImageEnViewFitToHeight }

constructor TImageEnViewFitToHeight.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 42;
  UpdateLanguage();
end;

procedure TImageEnViewFitToHeight.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FitImageToHeight);
  Hint := iemsg(IEMsg_DisplayTheImageAtTheHeightOfTheWindow);
end;

procedure TImageEnViewFitToHeight.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then    
  begin
    ActiveImageEnView.AutoShrink  := False;
    ActiveImageEnView.AutoStretch := False;
    ActiveImageEnView.FitToHeight;
  end;
end;


{ TImageEnViewFitToWidth }

constructor TImageEnViewFitToWidth.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 43;
  UpdateLanguage();
end;

procedure TImageEnViewFitToWidth.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FitImageToWidth);
  Hint := iemsg(IEMsg_DisplayTheImageAtTheWidthOfTheWindow);
end;

procedure TImageEnViewFitToWidth.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then    
  begin
    ActiveImageEnView.AutoShrink  := False;
    ActiveImageEnView.AutoStretch := False;
    ActiveImageEnView.FitToWidth;
  end;
end;


{ TImageEnViewLayersAdd }

constructor TImageEnViewLayersAdd.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 44;
  fRequireImage := False;
  UpdateLanguage();
end;

procedure TImageEnViewLayersAdd.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AddLayer);
  Hint := iemsg(IEMsg_AddANewLayerTothisImage);
end;

procedure TImageEnViewLayersAdd.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.LayersAdd;
end;


{ TImageEnViewLayersMergeAll }

constructor TImageEnViewLayersMergeAll.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 46;
  UpdateLanguage();
end;

procedure TImageEnViewLayersMergeAll.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_MergeAllLayers);
  Hint := iemsg(IEMsg_MergesAllLayersOfTheImageIntoASingleOne);
end;

procedure TImageEnViewLayersMergeAll.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.LayersMergeAll;
end;

procedure TImageEnViewLayersMergeAll.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnView then
    bEnabled := ActiveImageEnView.LayersCount > 1;
  Enabled := bEnabled;
end;


{ TImageEnViewLayersRemoveCurrent }

constructor TImageEnViewLayersRemoveCurrent.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 45;
  UpdateLanguage();
end;

procedure TImageEnViewLayersRemoveCurrent.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RemoveLayer);
  Hint := iemsg(IEMsg_RemoveTheSelectedLayerFromTheImage);
end;

procedure TImageEnViewLayersRemoveCurrent.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView and (ActiveImageEnView.LayersCurrent > 0) then
    ActiveImageEnView.LayersRemove(ActiveImageEnView.LayersCurrent);
end;


procedure TImageEnViewLayersRemoveCurrent.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnView then
    bEnabled := ActiveImageEnView.LayersCurrent > 0;  // Cannot remove background layer  
  Enabled := bEnabled;
end;


{ TImageEnViewLayersCreateFromClipboard }

constructor TImageEnViewLayersCreateFromClipboard.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 89;
  fRequireImage := False;
  UpdateLanguage();
end;

procedure TImageEnViewLayersCreateFromClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PasteAsLayer);
  Hint := iemsg(IEMsg_PasteTheContentOfTheClipboardAsANewlayer);
end;

procedure TImageEnViewLayersCreateFromClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
  begin
    if ActiveImageEnProc.IsClipboardAvailable then
      ActiveImageEnView.LayersCreateFromClipboard;
  end;
end;

procedure TImageEnViewLayersCreateFromClipboard.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnProc then
    bEnabled := ActiveImageEnProc.IsClipboardAvailable;
  Enabled := bEnabled;
end;


{ TImageEnViewLayersCreateFromFile }

{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnViewLayersCreateFromFile.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 90;
  fRequireImage := False;
  UpdateLanguage();
end;

procedure TImageEnViewLayersCreateFromFile.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_LoadFileAsLayer);
  Hint := iemsg(IEMsg_PromptForAnImageFileToLoadAsANewLayer);
end;

procedure TImageEnViewLayersCreateFromFile.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnView then
  begin
    sFilename := ActiveImageEnIO.ExecuteOpenDialog('', '', false);
    if sFilename <> '' then
      ActiveImageEnView.LayersCreateFromFile(sFilename);
  end;
end;
{$ENDIF}


{ TImageEnViewLayersMoveSendToBack }

constructor TImageEnViewLayersMoveSendToBack.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 91;
  UpdateLanguage();
end;

procedure TImageEnViewLayersMoveSendToBack.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SendToBack);
  Hint := iemsg(IEMsg_PositionTheselectedLayerBehindAllOthers);
end;

procedure TImageEnViewLayersMoveSendToBack.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.LayersMove(ActiveImageEnView.LayersCurrent, IEN_Send_To_Back);
end;

procedure TImageEnViewLayersMoveSendToBack.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnView then
    bEnabled := ActiveImageEnView.LayersCurrent > 0;
  Enabled := bEnabled;
end;


{ TImageEnViewLayersMoveSendBackward }

constructor TImageEnViewLayersMoveSendBackward.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 92;
  UpdateLanguage();
end;

procedure TImageEnViewLayersMoveSendBackward.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SendBackward);
  Hint := iemsg(IEMsg_MoveTheSelectedLayerBackward);
end;

procedure TImageEnViewLayersMoveSendBackward.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.LayersMove(ActiveImageEnView.LayersCurrent, IEN_Send_Backward);
end;

procedure TImageEnViewLayersMoveSendBackward.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnView then
    bEnabled := ActiveImageEnView.LayersCurrent > 0;       
  Enabled := bEnabled;
end;



{ TImageEnViewLayersMoveBringToFront }

constructor TImageEnViewLayersMoveBringToFront.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 94;
  UpdateLanguage();
end;

procedure TImageEnViewLayersMoveBringToFront.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_BringToFront);
  Hint := iemsg(IEMsg_BringTheSelectedLayerToThefrontOfAllOthers);
end;

procedure TImageEnViewLayersMoveBringToFront.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.LayersMove(ActiveImageEnView.LayersCurrent, IEN_Bring_To_Front);
end;
      
procedure TImageEnViewLayersMoveBringToFront.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnView then
    bEnabled := ActiveImageEnView.LayersCurrent < ActiveImageEnView.LayersCount - 1; 
  Enabled := bEnabled;
end;




{ TImageEnViewLayersMoveBringForward }

constructor TImageEnViewLayersMoveBringForward.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 93;
  UpdateLanguage();
end;

procedure TImageEnViewLayersMoveBringForward.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_BringForward);
  Hint := iemsg(IEMsg_MoveTheSelectedLayerForward);
end;

procedure TImageEnViewLayersMoveBringForward.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.LayersMove(ActiveImageEnView.LayersCurrent, IEN_Bring_Forward);
end;

procedure TImageEnViewLayersMoveBringForward.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnView then
    bEnabled := ActiveImageEnView.LayersCurrent < ActiveImageEnView.LayersCount - 1;       
  Enabled := bEnabled;
end;



{ TImageEnViewLayersFixBorders }

constructor TImageEnViewLayersFixBorders.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 95;
  UpdateLanguage();
end;

procedure TImageEnViewLayersFixBorders.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CropTransparency);
  Hint := iemsg(IEMsg_RemoveTheTransparentBordersFromTheSelectedLayer);
end;

procedure TImageEnViewLayersFixBorders.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.LayersFixBorders(ActiveImageEnView.LayersCurrent);
end;


{ TImageEnViewEnableAdjustOrientation }

constructor TImageEnViewEnableAdjustOrientation.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ImageIndex := 88;
  UpdateLanguage();
end;

procedure TImageEnViewEnableAdjustOrientation.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AutoRotateDisplay);
  Hint := iemsg(IEMsg_AutomaticallyDisplayImageWithTheCorrectOrientation);
end;

procedure TImageEnViewEnableAdjustOrientation.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.Params.EnableAdjustOrientation := not ActiveImageEnIO.Params.EnableAdjustOrientation;
end;

procedure TImageEnViewEnableAdjustOrientation.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if Enabled and HaveActiveImageEnIO then
    Checked :=  ActiveImageEnIO.Params.EnableAdjustOrientation
  else
    Checked := False;
end;



{ TImageEnViewPlaying }

constructor TImageEnViewPlaying.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 35;
  UpdateLanguage();
end;

procedure TImageEnViewPlaying.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Animate);
  Hint := iemsg(IEMsg_DisplayTheanimationOfAGIFOrAVIFile);
end;

procedure TImageEnViewPlaying.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.Playing := not ActiveImageEnView.Playing;
end;

procedure TImageEnViewPlaying.UpdateTarget(Target: TObject);
var
  bCanAnimate: Boolean;
begin
  bCanAnimate := False;
  if BaseEnabled and HaveActiveImageEnView then
    bCanAnimate := (ActiveImageEnView.IO.Params.ImageCount > 1) and (ActiveImageEnView.IO.Params.FileType in
    {$ifdef IEINCLUDEDICOM}
    [ioAVI, ioGIF, ioDICOM]
    {$else}
    [ioAVI, ioGIF]
    {$endif});

  Enabled := bCanAnimate;

  if HaveActiveImageEnView and bCanAnimate then
    Checked := ActiveImageEnView.Playing
  else
    Checked := False;
end;


{ TImageEnViewPlayLoop }

constructor TImageEnViewPlayLoop.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 36;
  UpdateLanguage();
end;

procedure TImageEnViewPlayLoop.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_LoopPlayback);
  Hint := iemsg(IEMsg_ContinouslyLoopThePlaybackOfAGIFOrAVIFile);
end;

procedure TImageEnViewPlayLoop.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
    ActiveImageEnView.PlayLoop := not ActiveImageEnView.PlayLoop;
end;

procedure TImageEnViewPlayLoop.UpdateTarget(Target: TObject);
var
  bCanAnimate: Boolean;
begin
  bCanAnimate := False;
  if BaseEnabled and HaveActiveImageEnView then
    bCanAnimate := (ActiveImageEnView.IO.Params.ImageCount > 1) and (ActiveImageEnView.IO.Params.FileType in 
    {$ifdef IEINCLUDEDICOM}   
    [ioAVI, ioGIF, ioDICOM]
    {$else}
    [ioAVI, ioGIF]
    {$endif});

  Enabled := bCanAnimate;

  if HaveActiveImageEnView and bCanAnimate then
    Checked := ActiveImageEnView.PlayLoop
  else
    Checked := False;
end;


{ TImageEnViewZoomIn }

constructor  TImageEnViewZoomIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImageIndex := 22;
  UpdateLanguage();
end;

procedure TImageEnViewZoomIn.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomIn);
  Hint := iemsg(IEMsg_DisplayTheImageLarger);
end;

procedure  TImageEnViewZoomIn.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
  begin
    ActiveImageEnView.AutoShrink  := False;
    ActiveImageEnView.AutoStretch := False;
    ActiveImageEnView.ZoomIn;
  end;
end;


{ TImageEnViewZoomOut }

constructor TImageEnViewZoomOut.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 23;
  UpdateLanguage();
end;

procedure TImageEnViewZoomOut.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomOut);
  Hint := iemsg(IEMsg_DisplayTheImageSmaller);
end;

procedure TImageEnViewZoomOut.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then 
  begin
    ActiveImageEnView.AutoShrink  := False;
    ActiveImageEnView.AutoStretch := False;
    ActiveImageEnView.ZoomOut;
  end;
end;


{ TImageEnViewZoomFullSize }

constructor TImageEnViewZoomFullSize.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 24;
  UpdateLanguage();
end;

procedure TImageEnViewZoomFullSize.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomToFullSize);
  Hint := iemsg(IEMsg_DisplayTheimageAtFullSize);
end;

procedure TImageEnViewZoomFullSize.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
  begin
    ActiveImageEnView.AutoShrink  := False;
    ActiveImageEnView.AutoStretch := False;
    ActiveImageEnView.Zoom := 100;
  end;
end;


{ TImageEnViewSetZoom }

constructor TImageEnViewSetZoom.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 96;
  fZoom      := 50; 
  UpdateLanguage();
end;

procedure TImageEnViewSetZoom.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomImage);
  Hint := iemsg(IEMsg_DisplayImageAtCustomZoom);
end;

procedure TImageEnViewSetZoom.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnView then
  begin
    ActiveImageEnView.AutoShrink  := False;
    ActiveImageEnView.AutoStretch := False;
    ActiveImageEnView.Zoom := fZoom;
  end;
end;    


{ TImageEnViewMouseInteract }

constructor TImageEnViewMouseInteract.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
end;      


procedure TImageEnViewMouseInteract.ExecuteTarget(Target: TObject);
var
  bInclude: Boolean;
begin
  if HaveActiveImageEnView then
  begin
    bInclude := not (fMouseInteract in ActiveImageEnView.MouseInteract);

    // Incompatible options
    if bInclude and (fMouseInteract = miResizeLayers) then
      ActiveImageEnView.MouseInteract := ActiveImageEnView.MouseInteract - [miRotateLayers];

    if bInclude then
      ActiveImageEnView.MouseInteract := ActiveImageEnView.MouseInteract + [fMouseInteract]
    else
      ActiveImageEnView.MouseInteract := ActiveImageEnView.MouseInteract - [fMouseInteract];

    // Options that affect other properties
    if bInclude then
    begin
      if fMouseInteract in [miMoveLayers, miResizeLayers, miRotateLayers] then
        ActiveImageEnView.LayersSync := False
      else                   
      if fMouseInteract in [miZoom, miSelectZoom] then
      begin
        ActiveImageEnView.AutoStretch := False;
        ActiveImageEnView.AutoShrink  := False;
      end;
    end;
  end;
end;

procedure TImageEnViewMouseInteract.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if ActiveImageEnView = nil then
    Checked := False
  else
    Checked := fMouseInteract in ActiveImageEnView.MouseInteract;
end;


{ TImageEnViewMouseMoveLayers }

constructor TImageEnViewMouseMoveLayers.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miMoveLayers;
  ImageIndex := 48;
  UpdateLanguage();
end;

procedure TImageEnViewMouseMoveLayers.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_MoveLayers);
  Hint := iemsg(IEMsg_UseThemouseTomoveImageLayers);
end;


{ TImageEnViewMouseMovingScroll }

constructor TImageEnViewMouseMovingScroll.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miMovingScroll;
  ImageIndex := 49;
  UpdateLanguage();
end;

procedure TImageEnViewMouseMovingScroll.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ScrollToCursor);
  Hint := iemsg(IEMsg_MoveTheMouseToscrollTheimage);
end;


{ TImageEnViewMouseResizeLayers }

constructor TImageEnViewMouseResizeLayers.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miResizeLayers;
  ImageIndex := 50;
  UpdateLanguage();
end;

procedure TImageEnViewMouseResizeLayers.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ResizeLayers);
  Hint := iemsg(IEMsg_UseTheMouseToresizeImageLayers);
end;


{ TImageEnViewMouseRotateLayers }

constructor TImageEnViewMouseRotateLayers.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miRotateLayers;
  ImageIndex := 51;
  UpdateLanguage();
end;

procedure TImageEnViewMouseRotateLayers.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RotateLayers);
  Hint := iemsg(IEMsg_UseTheMouseTorotateImageLayers);
end;


{ TImageEnViewMouseScroll }

constructor TImageEnViewMouseScroll.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miScroll;
  ImageIndex := 52;
  UpdateLanguage();
end;

procedure TImageEnViewMouseScroll.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ScrollImage);
  Hint := iemsg(IEMsg_ClickTheImageAndDragTheMouseToScroll);
end;


{ TImageEnViewMouseSelect }

constructor TImageEnViewMouseSelect.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelect;
  ImageIndex := 53;
  UpdateLanguage();
end;

procedure TImageEnViewMouseSelect.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RectangularSelect);
  Hint := iemsg(IEMsg_SelectArectangularAreaOfYourImage);
end;


{ TImageEnViewMouseSelectCircle }

constructor TImageEnViewMouseSelectCircle.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectCircle;
  ImageIndex := 54;
  UpdateLanguage();
end;

procedure TImageEnViewMouseSelectCircle.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CircularSelect);
  Hint := iemsg(IEMsg_SelectAcircularAreaOfYourImage);
end;


{ TImageEnViewMouseSelectLasso }

constructor TImageEnViewMouseSelectLasso.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectLasso;
  ImageIndex := 55;
  UpdateLanguage();
end;

procedure TImageEnViewMouseSelectLasso.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_LassoSelect);
  Hint := iemsg(IEMsg_PerformAnIrregularSelectionOfYourImageByDraggingTheMouse);
end;


{ TImageEnViewMouseSelectMagicWand }

constructor TImageEnViewMouseSelectMagicWand.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectMagicWand;
  ImageIndex := 56;
  UpdateLanguage();
end;

procedure TImageEnViewMouseSelectMagicWand.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SelectByColor);
  Hint := iemsg(IEMsg_SelectAportionOfYourImageOfASimilarColor);
end;


{ TImageEnViewMouseSelectPolygon }

constructor TImageEnViewMouseSelectPolygon.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectPolygon;
  ImageIndex := 57;
  UpdateLanguage();
end;

procedure TImageEnViewMouseSelectPolygon.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PolygonSelect);
  Hint := iemsg(IEMsg_PerformAnIrregularSelectionOfYourImageByClickingTheMouse);
end;


{ TImageEnViewMouseSelectZoom }

constructor TImageEnViewMouseSelectZoom.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miSelectZoom;
  ImageIndex := 58;
  UpdateLanguage();
end;

procedure TImageEnViewMouseSelectZoom.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ZoomToSelection);
  Hint := iemsg(IEMsg_SelectAnAreaOfTheImageToZoomInto);
end;


{ TImageEnViewMouseZoom }

constructor TImageEnViewMouseZoom.Create(AOwner: TComponent);
begin
  inherited;
  fMouseInteract := miZoom;
  ImageIndex := 59;
  UpdateLanguage();
end;

procedure TImageEnViewMouseZoom.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Zoom);
  Hint := iemsg(IEMsg_LeftclickTheimageToZoomInRightclickToZoomOut);
end;


{ TImageEnViewDoAdjustPreviews }

{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnViewDoAdjustPreviews.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 28;
  UpdateLanguage();
end;

procedure TImageEnViewDoAdjustPreviews.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AdjustColors);
  Hint := iemsg(IEMsg_PerformColorEnhancementFunctionsOnTheImage);
end;

procedure TImageEnViewDoAdjustPreviews.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.DoPreviews(ppeColorAdjust);
end;
{$ENDIF}

{ TImageEnViewDoEffectPreviews }

{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnViewDoEffectPreviews.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 29;
  UpdateLanguage();
end;

procedure TImageEnViewDoEffectPreviews.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ImageEffects);
  Hint := iemsg(IEMsg_PerformEffectsOnTheimage);
end;

procedure TImageEnViewDoEffectPreviews.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.DoPreviews(ppeEffects - [peRotate, peResize]);
end;
{$ENDIF}

{ TImageEnViewPromptToRotate }

{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnViewPromptToRotate.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 97;     
  fAntiAliasMode := ierFast;
  UpdateLanguage();
end;

procedure TImageEnViewPromptToRotate.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CustomRotate);
  Hint := iemsg(IEMsg_RotateImageByACustomAngle);     
end;

procedure TImageEnViewPromptToRotate.ExecuteTarget(Target: TObject);
var
  WasAntiAlias : TIEAntialiasMode;
begin
  if HaveActiveImageEnProc then
  begin
    WasAntiAlias := IEGlobalSettings().DefaultRotateAntiAlias;
    try
      IEGlobalSettings().DefaultRotateAntiAlias := fAntiAliasMode;
      ActiveImageEnProc.DoPreviews([peRotate]);
    finally
      IEGlobalSettings().DefaultRotateAntiAlias := WasAntiAlias;
    end;
  end;
end;
{$ENDIF}

{ TImageEnViewPromptToResize }

{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnViewPromptToResize.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 98;   
  fResampleFilter := rfFastLinear;
  UpdateLanguage();
end;

procedure TImageEnViewPromptToResize.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ResizeImage) + '...';
  Hint := iemsg(IEMsg_SpecifyANewSizeForTheImage);
end;

procedure TImageEnViewPromptToResize.ExecuteTarget(Target: TObject);
var
  WasResampleFilter : TResampleFilter;
begin
  if HaveActiveImageEnProc then
  begin
    WasResampleFilter := IEGlobalSettings().DefaultResampleFilter;
    try
      IEGlobalSettings().DefaultResampleFilter := fResampleFilter;
      ActiveImageEnProc.DoPreviews([peResize]);
    finally
      IEGlobalSettings().DefaultResampleFilter := WasResampleFilter;
    end;
  end;
end;
{$ENDIF}

{ TImageEnViewConvertToGray }

constructor TImageEnViewConvertToGray.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 30;
  UpdateLanguage();
end;

procedure TImageEnViewConvertToGray.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ConvertToGray);
  Hint := iemsg(IEMsg_ReduceThecolorsOfTheimageTograyscale);
end;

procedure TImageEnViewConvertToGray.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.ConvertToGray;
end;


{ TImageEnViewHistAutoEqualize }

constructor TImageEnViewHistAutoEqualize.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 32;
  UpdateLanguage();
end;

procedure TImageEnViewHistAutoEqualize.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_AutoEqualizeColors);
  Hint := iemsg(IEMsg_EqualizesTheColorHistogramForTheSelectedRegion);
end;

procedure TImageEnViewHistAutoEqualize.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.HistAutoEqualize;
end;


{ TImageEnViewNegative }

constructor TImageEnViewNegative.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 31;
  UpdateLanguage();
end;

procedure TImageEnViewNegative.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Negative);
  Hint := iemsg(IEMsg_InvertThecolorsOfTheimage);
end;

procedure TImageEnViewNegative.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Negative;
end;


{ TImageEnViewRemoveRedEyes }

constructor TImageEnViewRemoveRedEyes.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 33;
  UpdateLanguage();
end;

procedure TImageEnViewRemoveRedEyes.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RemoveRedEyes);
  Hint := iemsg(IEMsg_RemoveTheRedeyeEffectFromTheSelection);
end;

procedure TImageEnViewRemoveRedEyes.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.RemoveRedEyes;
end;


{ TImageEnViewSharpen }

constructor TImageEnViewSharpen.Create(AOwner: TComponent);
begin
  inherited;
  fIntensity := 10;
  fNeighbourhood := 4;
  ImageIndex := 34;
  UpdateLanguage();
end;

procedure TImageEnViewSharpen.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Sharpen);
  Hint := iemsg(IEMsg_ApplyASharpeningFilterToTheImage);
end;

procedure TImageEnViewSharpen.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Sharpen(fIntensity, fNeighbourhood);
end;


{ TImageEnViewFlipHorizontal }

constructor TImageEnViewFlipHorizontal.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 16;
  UpdateLanguage();
end;

procedure TImageEnViewFlipHorizontal.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FlipHorizontal);
  Hint := iemsg(IEMsg_FlipTheimageFromTopToBottom);
end;

procedure TImageEnViewFlipHorizontal.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Flip(fdHorizontal);
end;


{ TImageEnViewFlipVertical }

constructor TImageEnViewFlipVertical.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 17;
  UpdateLanguage();
end;

procedure TImageEnViewFlipVertical.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FlipVertical);
  Hint := iemsg(IEMsg_FlipTheimageFromLeftToRight);
end;

procedure TImageEnViewFlipVertical.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Flip(fdVertical);
end;


{ TImageEnViewRotateRight }

constructor TImageEnViewRotateRight.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 15;
  UpdateLanguage();
end;

procedure TImageEnViewRotateRight.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RotateRight);
  Hint := iemsg(IEMsg_RotateTheImage90Clockwise);
end;

procedure TImageEnViewRotateRight.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Rotate(270, ierFast);
end;


{ TImageEnViewRotate180 }

constructor TImageEnViewRotate180.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 14;
  UpdateLanguage();
end;

procedure TImageEnViewRotate180.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Rotate180);
  Hint := iemsg(IEMsg_RotateTheImage180Clockwise);
end;

procedure TImageEnViewRotate180.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Rotate(180, ierFast);
end;


{ TImageEnViewRotateLeft }

constructor TImageEnViewRotateLeft.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 13;
  UpdateLanguage();
end;

procedure TImageEnViewRotateLeft.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_RotateLeft);
  Hint := iemsg(IEMsg_RotateTheImage90CounterClockwise);
end;

procedure TImageEnViewRotateLeft.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Rotate(90, ierFast);
end;


{ TImageEnViewCrop }

constructor TImageEnViewCrop.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := True;
  ImageIndex := 47;
  UpdateLanguage();
end;

procedure TImageEnViewCrop.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CropToSelection);
  Hint := iemsg(IEMsg_RemoveAllPartsOfTheImageOutsideTheCurrentSelection);
end;
                                                                                    
procedure TImageEnViewCrop.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc and HaveSelection then
  begin
    ActiveImageEnProc.Crop(ActiveImageEnView.SelectedRect.x,
                           ActiveImageEnView.SelectedRect.y,
                           ActiveImageEnView.SelectedRect.x + ActiveImageEnView.SelectedRect.Width,
                           ActiveImageEnView.SelectedRect.y + ActiveImageEnView.SelectedRect.Height );   
    if HaveActiveImageEnView then
      ActiveImageEnView.Deselect;
  end;
end;


{ TImageEnViewCopyToClipboard }

constructor TImageEnViewCopyToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_C_SHORTCUT;
  ImageIndex := 1;
  UpdateLanguage();
end;

procedure TImageEnViewCopyToClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Copy);
  Hint := iemsg(IEMsg_CopyTheCurrentImageOrSelectionToTheClipboard);
end;

procedure TImageEnViewCopyToClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
  begin
    if HaveSelection then
      ActiveImageEnProc.SelCopyToClip
    else
      ActiveImageEnProc.CopyToClipboard;
  end;
end;


{ TImageEnViewPasteFromClipboard }

constructor TImageEnViewPasteFromClipboard.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  Shortcut := CTRL_V_SHORTCUT;
  ImageIndex := 2;
  UpdateLanguage();
end;

procedure TImageEnViewPasteFromClipboard.UpdateLanguage();
begin              
  Caption := iemsg(IEMsg_Paste);
  Hint := iemsg(IEMsg_PasteAnImageFromTheClipboard);
end;

procedure TImageEnViewPasteFromClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
  begin
    if HaveSelection then
      ActiveImageEnProc.SelPasteFromClip(True, False)
    else
      ActiveImageEnProc.PasteFromClipboard;
  end;
end;


procedure TImageEnViewPasteFromClipboard.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnProc then
    bEnabled := ActiveImageEnProc.IsClipboardAvailable;
  Enabled := bEnabled;
end;

{ TImageEnViewSelCutToClip }

constructor TImageEnViewSelCutToClip.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := True;
  Shortcut := CTRL_X_SHORTCUT;
  ImageIndex := 0;       
  UpdateLanguage();
end;         

procedure TImageEnViewSelCutToClip.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CutSelection);
  Hint := iemsg(IEMsg_CopyTheCurrentSelectionToTheClipboardAndRemoveItFromTheImage);
end;

procedure TImageEnViewSelCutToClip.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc and HaveSelection then
    ActiveImageEnProc.SelCutToClip;
end;


{ TImageEnViewSelCopyToClip }

constructor TImageEnViewSelCopyToClip.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := True;
  ImageIndex := 1;
  UpdateLanguage();
end;

procedure TImageEnViewSelCopyToClip.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CopySelection);
  Hint := iemsg(IEMsg_CopyTheCurrentSelectionToTheclipboard);
end;

procedure TImageEnViewSelCopyToClip.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc and HaveSelection then
    ActiveImageEnProc.SelCopyToClip;
end;


{ TImageEnViewUndo }

constructor TImageEnViewUndo.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_Z_SHORTCUT;
  ImageIndex := 3;
  UpdateLanguage();
end;

procedure TImageEnViewUndo.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Undo);
  Hint := iemsg(IEMsg_UndoThelastAction);
end;

procedure TImageEnViewUndo.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Undo;
end;


procedure TImageEnViewUndo.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnProc then
    bEnabled := ActiveImageEnProc.CanUndo;
  Enabled := bEnabled;
end;

{ TImageEnViewRedo }

constructor TImageEnViewRedo.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_Y_SHORTCUT;
  ImageIndex := 4;
  UpdateLanguage();
end;

procedure TImageEnViewRedo.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Redo);
  Hint := iemsg(IEMsg_RedoTheLastActionThatWasUndone);
end;

procedure TImageEnViewRedo.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    ActiveImageEnProc.Redo;
end;

procedure TImageEnViewRedo.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnProc then
    bEnabled := ActiveImageEnProc.CanRedo;    
  Enabled := bEnabled;
end;


{ TImageEnViewPromptToOpen }

{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnViewPromptToOpen.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ShortCut := CTRL_O_SHORTCUT;
  ImageIndex := 7;      
  fDefaultFilter   := -1;
  fLimitToFileType := -1;
  UpdateLanguage();
end;

procedure TImageEnViewPromptToOpen.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Open);
  Hint := iemsg(IEMsg_LoadAnImageFromFile);
end;

procedure TImageEnViewPromptToOpen.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnIO then
  begin
    sFilename := ActiveImageEnIO.ExecuteOpenDialog('', '', false, 0, '', fDialogTitle, '', fDefaultFilter, fLimitToFileType);
    if sFilename <> '' then
      ActiveImageEnIO.LoadFromFile(sFilename);
  end;
end;
{$ENDIF}


{ TImageEnViewSave }
                   
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnViewSave.Create(AOwner: TComponent);
begin
  inherited;
  ShortCut := CTRL_S_SHORTCUT;
  ImageIndex := 8;
  UpdateLanguage();
end;

procedure TImageEnViewSave.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Save);
  Hint := iemsg(IEMsg_SaveChangesToThisImageToFile);
end;

procedure TImageEnViewSave.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnIO then
  begin
    sFilename := ActiveImageEnIO.Params.FileName;
    if sFilename = '' then    
      sFilename := ActiveImageEnIO.ExecuteSaveDialog('', '', false);
    if sFilename <> '' then
      ActiveImageEnIO.SaveToFile(sFilename);
  end;
end;
{$ENDIF}


{ TImageEnViewPromptToSave }
                          
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnViewPromptToSave.Create(AOwner: TComponent);
begin
  inherited;
  Shortcut := CTRL_ALT_S_SHORTCUT;
  ImageIndex := 9;    
  fDefaultFilter   := -1;
  fLimitToFileType := -1;
  UpdateLanguage();
end;

procedure TImageEnViewPromptToSave.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SaveAs);
  Hint := iemsg(IEMsg_SaveThisImageToANewFilename);
end;

procedure TImageEnViewPromptToSave.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnIO then
  begin
    sFilename := ActiveImageEnIO.ExecuteSaveDialog('', '', false, 0, '', fDialogTitle, '', fDefaultFilter, fLimitToFileType);
    if sFilename <> '' then
      ActiveImageEnIO.SaveToFile(sFilename);
  end;
end;
{$ENDIF}


{ TImageEnViewDoIOPreviews }

{$IFDEF IEINCLUDEDIALOGIO}
constructor TImageEnViewDoIOPreviews.Create(AOwner: TComponent);
begin
  inherited;
  fPreviewParams := [ppAll];
  ImageIndex := 10;
  UpdateLanguage();
end;

procedure TImageEnViewDoIOPreviews.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SaveProperties);
  Hint := iemsg(IEMsg_SpecifyAdvancedPropertiesForThisImage);
end;

procedure TImageEnViewDoIOPreviews.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.DoPreviews(fPreviewParams);
end;
{$ENDIF}


{ TImageEnViewDoPrintPreviewDialog }

{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnViewDoPrintPreviewDialog.Create(AOwner: TComponent);
begin
  inherited;
  fDialogType := iedtDialog;
  ShortCut := CTRL_P_SHORTCUT;
  ImageIndex := 27;
  UpdateLanguage();
end;

procedure TImageEnViewDoPrintPreviewDialog.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PrintPreview);
  Hint := iemsg(IEMsg_DisplayAPreviewOfThisImageForPrinting);
end;

procedure TImageEnViewDoPrintPreviewDialog.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.DoPrintPreviewDialog(fDialogType);
end;
{$ENDIF}


{ TImageEnViewPrintImageNormal }

{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnViewPrintImageNormal.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 26;
  UpdateLanguage();
end;

procedure TImageEnViewPrintImageNormal.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Print);
  Hint := iemsg(IEMsg_PrintThisImageAtItsOriginalSize);
end;

procedure TImageEnViewPrintImageNormal.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesNORMAL);
end;
{$ENDIF}


{ TImageEnViewPrintImageFitToPage }

{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnViewPrintImageFitToPage.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 26;
  UpdateLanguage();
end;

procedure TImageEnViewPrintImageFitToPage.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PrintToPage);
  Hint := iemsg(IEMsg_PrintThisImageToFitThePage);
end;

procedure TImageEnViewPrintImageFitToPage.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE);
end;
{$ENDIF}


{ TImageEnViewSelectAcquireSource }

{$IFDEF IEINCLUDEIEXACQUIRE}
constructor TImageEnViewSelectAcquireSource.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  fApis := [ieaTwain, ieaWIA];
  ImageIndex := 38;
  UpdateLanguage();
end;

procedure TImageEnViewSelectAcquireSource.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SelectAcquisitionSource);
  Hint := iemsg(IEMsg_SelectTheCameraOrScannerToAcquireImagesFrom);
end;

procedure TImageEnViewSelectAcquireSource.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.SelectAcquireSource(fApis);
end;
{$ENDIF}


{ TImageEnViewAcquire }

{$IFDEF IEINCLUDEIEXACQUIRE}
constructor TImageEnViewAcquire.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImage := False;
  ImageIndex := 37;
  UpdateLanguage();
end;

procedure TImageEnViewAcquire.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Acquire);
  Hint := iemsg(IEMsg_RetrieveAnImageFromACameraOrScanner);
end;

procedure TImageEnViewAcquire.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.Acquire;
end;
{$ENDIF}


{ TImageEnViewSeekFirst }

constructor TImageEnViewSeekFirst.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 18;
  UpdateLanguage();
end;

procedure TImageEnViewSeekFirst.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_FirstFrame);
  Hint := iemsg(IEMsg_DisplayTheFirstFrameOfThisImage);
end;

procedure TImageEnViewSeekFirst.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.Seek(ieioSeekFirst);
end;

procedure TImageEnViewSeekFirst.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnIO then
    bEnabled := (ActiveImageEnIO.Params.ImageCount > 1) and
                (ActiveImageEnIO.Params.ImageIndex > 0);
  Enabled := bEnabled;
end;


{ TImageEnViewSeekPrior }

constructor TImageEnViewSeekPrior.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 19;
  UpdateLanguage();
end;

procedure TImageEnViewSeekPrior.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PreviousFrame);
  Hint := iemsg(IEMsg_DisplayThePriorFrameOfThisImage);
end;

procedure TImageEnViewSeekPrior.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.Seek(ieioSeekPrior);
end;

procedure TImageEnViewSeekPrior.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnIO then
    bEnabled := (ActiveImageEnIO.Params.ImageCount > 1) and
                (ActiveImageEnIO.Params.ImageIndex > 0);
  Enabled := bEnabled;
end;            


{ TImageEnViewSeekNext }

constructor TImageEnViewSeekNext.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 20;
  UpdateLanguage();
end;

procedure TImageEnViewSeekNext.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_NextFrame);
  Hint := iemsg(IEMsg_DisplayTheNextFrameOfThisImage);
end;

procedure TImageEnViewSeekNext.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.Seek(ieioSeekNext);
end;

procedure TImageEnViewSeekNext.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnIO then
    bEnabled := (ActiveImageEnIO.Params.ImageCount > 1) and
                (ActiveImageEnIO.Params.ImageIndex < ActiveImageEnIO.Params.ImageCount - 1);
  Enabled := bEnabled;
end;


{ TImageEnViewSeekLast }

constructor TImageEnViewSeekLast.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 21;
  UpdateLanguage();
end;

procedure TImageEnViewSeekLast.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_LastFrame);
  Hint := iemsg(IEMsg_DisplayTheLastFrameOfThisImage);
end;

procedure TImageEnViewSeekLast.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnIO then
    ActiveImageEnIO.Seek(ieioSeekLast);
end;

procedure TImageEnViewSeekLast.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnIO then
    bEnabled := (ActiveImageEnIO.Params.ImageCount > 1) and
                (ActiveImageEnIO.Params.ImageIndex < ActiveImageEnIO.Params.ImageCount - 1);
  Enabled := bEnabled;  
end;


{$ELSE} // {$IFDEF IEINCLUDEACTIONS}}

interface
implementation

{$ENDIF}

{!!
<FS>TImageEnView Actions

<FN>
ImageEn includes a large set of actions for ImageEnView, <L TImageEnMView Actions>ImageEnMView</L> , <L TImageEnFolderMView Actions>ImageEnFolderMView</L> and <L TImageEnVect Actions>ImageEnVect</L> components to allow you to rapidly develop your UI.

<FM>To use actions:<FN>
1. Add a TActionList component to your form
2. Double-click your TActionList to open it
3. Select "New Standard Action"
4. Scroll down to the ImageEnView actions, select the ones you require and click OK
5. Select your actions and set the <FB>ImageEnView<FN> property to your <A TImageEnView> component
6. Assign the actions to menu items and buttons

See the demo for more information: Demos\Other\Actions\Actions.dpr

<FM>Notes:<FN>
- Your must set the <FB>ImageEnView<FN> property of the actions
- You can set <A TIEImageEnGlobalSettings.MsgLanguage> to localize the actions
- See the <L Actions ImageIndex List>list of the default ImageIndexes</L> if you are planning to add graphics to your actions

<FM>General Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnViewAutoShrink</C> <C>Auto-Shrink</C> <C>Display large images at the window size</C> <C><A TImageEnView.AutoShrink></C> <C> -</C> </R>
<R> <C>TImageEnViewAutoStretch</C> <C>Auto-Stretch</C> <C>Display small images at the window size</C> <C><A TImageEnView.AutoStretch></C> <C> -</C> </R>
<R> <C>TImageEnViewBlank</C> <C>Clear</C> <C>Clear the window</C> <C><A TImageEnView.Blank></C> <C> -</C> </R>
<R> <C>TImageEnViewDeSelect</C> <C>Deselect</C> <C>Clear your selection</C> <C><A TImageEnView.DeSelect></C> <C> -</C> </R>
<R> <C>TImageEnViewFit</C> <C>Fit Image</C> <C>Display the image at the size of the window</C> <C><A TImageEnView.Fit></C> <C> -</C> </R>
<R> <C>TImageEnViewFitToHeight</C> <C>Fit Image to Height</C> <C>Display the image at the height of the window</C> <C><A TImageEnView.FitToHeight></C> <C> -</C> </R>
<R> <C>TImageEnViewFitToWidth</C> <C>Fit Image to Width</C> <C>Display the image at the width of the window</C> <C><A TImageEnView.FitToWidth></C> <C> -</C> </R>
<R> <C>TImageEnViewEnableAdjustOrientation</C> <C>Auto-Rotate Display</C> <C>Automatically display image with the correct orientation</C> <C><A TIOParamsVals.EnableAdjustOrientation></C> <C> -</C> </R>
<R> <C>TImageEnViewPlaying</C> <C>Animate</C> <C>Display the animation of a GIF or AVI file</C> <C><A TImageEnView.Playing></C> <C> -</C> </R>
<R> <C>TImageEnViewPlayLoop</C> <C>Loop Playback</C> <C>Continously loop the playback of a GIF or AVI file</C> <C><A TImageEnView.PlayLoop>"</C> <C> -</C> </R>
<R> <C>TImageEnViewZoomIn</C> <C>Zoom In</C> <C>Display the image larger</C> <C><A TImageEnView.ZoomIn></C> <C> -</C> </R>
<R> <C>TImageEnViewZoomOut</C> <C>Zoom Out</C> <C>Display the image smaller</C> <C><A TImageEnView.ZoomOut></C> <C> -</C> </R>
<R> <C>TImageEnViewZoomFullSize</C> <C>Zoom to Full Size</C> <C>Display the image at full size </C> <C><A TImageEnView.Zoom></C> <C> -</C> </R>
<R> <C>TImageEnViewSetZoom</C> <C>Zoom to x%</C> <C>Display the image at x% Zoom </C> <C><A TImageEnView.Zoom></C> <C>Zoom</C> </R>
</TABLE>

<FM>Layer Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnViewLayersAdd</C> <C>Add Layer</C> <C>Add a new layer to this image</C> <C><A TImageEnView.LayersAdd></C> <C> -</C> </R>
<R> <C>TImageEnViewLayersMergeAll</C> <C>Merge All Layers</C> <C>Merges all layers of the image into a single one</C> <C><A TImageEnView.LayersMergeAll></C> <C> -</C> </R>
<R> <C>TImageEnViewLayersRemoveCurrent</C> <C>Remove Layer</C> <C>Remove the selected layer from the image</C> <C><A TImageEnView.LayersRemove></C> <C> -</C> </R>
<R> <C>TImageEnViewLayersCreateFromClipboard</C> <C>Paste as Layer</C> <C>Paste the content of the clipboard as a new layer</C> <C><A TImageEnView.LayersCreateFromClipboard></C> <C> -</C> </R>
<R> <C>TImageEnViewLayersCreateFromFile</C> <C>Load File as Layer...</C> <C>Prompts for an image file and loads it as a new layer</C> <C><A TImageEnView.LayersCreateFromFile></C> <C> -</C> </R>
<R> <C>TImageEnViewLayersMoveBringToFront</C> <C>Bring to Front</C> <C>Bring the selected layer to the front of all others</C> <C><A TImageEnView.LayersMove></C> <C> -</C> </R>
<R> <C>TImageEnViewLayersMoveBringForward</C> <C>Bring Forward</C> <C>Move the selected layer forward</C> <C><A TImageEnView.LayersMove>  </C> <C> -</C> </R>
<R> <C>TImageEnViewLayersMoveSendToBack</C> <C>Send to Back</C> <C>Position the selected layer behind all others</C> <C><A TImageEnView.LayersMove></C> <C> -</C> </R>
<R> <C>TImageEnViewLayersMoveSendBackward</C> <C>Send Backwards</C> <C>Move the selected layer backward</C> <C><A TImageEnView.LayersMove>    </C> <C> -</C> </R>
<R> <C>TImageEnViewLayersFixBorders</C> <C>Crop Transparency</C> <C>Remove the transparent borders from the selected layer</C> <C><A TImageEnView.LayersFixBorders>    </C> <C> -</C> </R>
</TABLE>

<FM>Mouse Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnViewMouseMoveLayers</C> <C>Move Layers</C> <C>Use the mouse to move image layers</C> <C><L TImageEnView.MouseInteract>miMoveLayers</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseMovingScroll</C> <C>Scroll to Cursor</C> <C>Move the mouse to scroll the image</C> <C><L TImageEnView.MouseInteract>miMovingScroll</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseResizeLayers</C> <C>Resize Layers</C> <C>Use the mouse to resize image layers</C> <C><L TImageEnView.MouseInteract>miResizeLayers</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseRotateLayers</C> <C>Rotate Layers</C> <C>Use the mouse to rotate image layers</C> <C><L TImageEnView.MouseInteract>miRotateLayers</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseScroll</C> <C>Scroll Image</C> <C>Click the image and drag the mouse to scroll</C> <C><L TImageEnView.MouseInteract>miScroll</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseSelect</C> <C>Rectangular Select</C> <C>Select a rectangular area of your image</C> <C><L TImageEnView.MouseInteract>miSelect</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseSelectCircle</C> <C>Circular Select</C> <C>Select a circular area of your image</C> <C><L TImageEnView.MouseInteract>miSelectCircle</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseSelectLasso</C> <C>Lasso Select</C> <C>Perform an irregular selection of your image by dragging the mouse</C> <C><L TImageEnView.MouseInteract>miSelectLasso</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseSelectMagicWand</C> <C>Select by Color</C> <C>Select a portion of your image of a similar color</C> <C><L TImageEnView.MouseInteract>miSelectMagicWand</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseSelectPolygon</C> <C>Polygon Select</C> <C>Perform an irregular selection of your image by clicking the mouse</C> <C><L TImageEnView.MouseInteract>miSelectPolygon</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseSelectZoom</C> <C>Zoom to Selection</C> <C>Select an area of the image to zoom into</C> <C><L TImageEnView.MouseInteract>miSelectZoom</L></C> <C> -</C> </R>
<R> <C>TImageEnViewMouseZoom</C> <C>Zoom</C> <C>Left-click the image to Zoom In. Right-click to Zoom Out</C> <C><L TImageEnView.MouseInteract>miZoom</L></C> <C> -</C> </R>
</TABLE>

<FM>Proc Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnViewDoAdjustPreviews</C> <C>Adjust Colors</C> <C>Peform color enhancement functions on the image</C> <C><A TImageEnProc.DoPreviews></C> <C> -</C> </R>
<R> <C>TImageEnViewDoEffectPreviews</C> <C>Image Effects</C> <C>Perform effects on the image</C> <C><A TImageEnProc.DoPreviews></C> <C> -</C> </R>
<R> <C>TImageEnViewPromptToResize</C> <C>Prompt to Resize</C> <C>Display the previews dialog to allow the user to resample (resize) the image</C> <C><A TImageEnProc.DoPreviews></C> <C><L TResampleFilter>ResampleFilter</L></C> </R>
<R> <C>TImageEnViewPromptToRotate</C> <C>Prompt to Rotate</C> <C>Display the previews dialog to allow the user to rotate the image to a custom angle</C> <C><A TImageEnProc.DoPreviews></C> <C><L TImageEnProc.Rotate>AntiAliasMode</L></C> </R>
<R> <C>TImageEnViewRotateRight</C> <C>Rotate Right</C> <C>Rotate the image 90° clockwise</C> <C><A TImageEnProc.Rotate></C> <C> -</C> </R>
<R> <C>TImageEnViewRotate180</C> <C>Rotate 180°</C> <C>Rotate the image 180° clockwise</C> <C><A TImageEnProc.Rotate></C> <C> -</C> </R>
<R> <C>TImageEnViewRotateLeft</C> <C>Rotate Left</C> <C>Rotate the image 90° counter-clockwise</C> <C><A TImageEnProc.Rotate></C> <C> -</C> </R>
<R> <C>TImageEnViewFlipHorizontal</C> <C>Flip Horizontal</C> <C>Flip the image from top to bottom</C> <C><A TImageEnProc.Flip></C> <C> -</C> </R>
<R> <C>TImageEnViewFlipVertical</C> <C>Flip Vertical</C> <C>Flip the image from left to right</C> <C><A TImageEnProc.Flip></C> <C> -</C> </R>
<R> <C>TImageEnViewCrop</C> <C>Crop to Selection</C> <C>Remove all parts of the image outside the current selection</C> <C><A TImageEnProc.Crop></C> <C> -</C> </R>
<R> <C>TImageEnViewCopyToClipboard</C> <C>Copy</C> <C>Copy the current image or selection to the clipboard</C> <C><A TImageEnProc.CopyToClipboard></C> <C> -</C> </R>
<R> <C>TImageEnViewPasteFromClipboard</C> <C>Paste</C> <C>Paste an image from the clipboard</C> <C><A TImageEnProc.PasteFromClipboard></C> <C> -</C> </R>
<R> <C>TImageEnViewSelCutToClip</C> <C>Cut Selection</C> <C>Copy the current selection to the clipboard and remove it from the image</C> <C><A TImageEnProc.SelCutToClip></C> <C> -</C> </R>
<R> <C>TImageEnViewSelCopyToClip</C> <C>Copy Selection</C> <C>Copy the current selection to the clipboard</C> <C><A TImageEnProc.SelCopyToClip></C> <C> -</C> </R>
<R> <C>TImageEnViewUndo</C> <C>Undo</C> <C>Undo the last action</C> <C><A TImageEnProc.Undo></C> <C> -</C> </R>
<R> <C>TImageEnViewRedo</C> <C>Redo</C> <C>Redo the last action that was undone</C> <C><A TImageEnProc.Redo></C> <C> -</C> </R>
<R> <C>TImageEnViewConvertToGray</C> <C>Convert to Gray</C> <C>Reduce the colors of the image to grayscale</C> <C><A TImageEnProc.ConvertToGray></C> <C> -</C> </R>
<R> <C>TImageEnViewHistAutoEqualize</C> <C>Auto Equalize Colors</C> <C>Equalizes the color histogram for the selected region</C> <C><A TImageEnProc.HistAutoEqualize></C> <C> -</C> </R>
<R> <C>TImageEnViewNegative</C> <C>Negative</C> <C>Invert the colors of the image</C> <C><A TImageEnProc.Negative></C> <C> -</C> </R>
<R> <C>TImageEnViewRemoveRedEyes</C> <C>Remove Red-Eyes</C> <C>Remove the red-eye effect from the selection</C> <C><A TImageEnProc.RemoveRedEyes></C> <C> -</C> </R>
<R> <C>TImageEnViewSharpen</C> <C>Sharpen</C> <C>Apply a sharpening filter to the image</C> <C><A TImageEnProc.Sharpen></C> <C><L TImageEnProc.Sharpen>Intensity</L>, <L TImageEnProc.Sharpen>Neighbourhood</L></C> </R>
</TABLE>

<FM>IO Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnViewPromptToOpen</C> <C>Open</C> <C>Load an image from file</C> <C><A TImageEnIO.ExecuteOpenDialog>, <A TImageEnIO.LoadFromFile></C> <C>DialogTitle, DefaultFilter, LimitToFileType</C> </R>
<R> <C>TImageEnViewSave</C> <C>Save</C> <C>Save changes to this image to file</C> <C><A TImageEnIO.SaveToFile></C> <C> -</C> </R>
<R> <C>TImageEnViewPromptToSave</C> <C>Save as</C> <C>Save this image to a new filename</C> <C><A TImageEnIO.ExecuteSaveDialog>, <A TImageEnIO.SaveToFile></C> <C>DialogTitle, DefaultFilter, LimitToFileType</C> </R>
<R> <C>TImageEnViewDoIOPreviews</C> <C>Save Properties</C> <C>Specify advanced properties for this image</C> <C><A TImageEnIO.DoPreviews></C> <C><L TPreviewParams>PreviewParams</L></C> </R>
<R> <C>TImageEnViewDoPrintPreviewDialog</C> <C>Print Preview</C> <C>Display a preview of this image for printing</C> <C><A TImageEnIO.DoPrintPreviewDialog></C> <C><L TIEDialogType>DialogType</L></C> </R>
<R> <C>TImageEnViewPrintImageNormal</C> <C>Print</C> <C>Print this image at its original size</C> <C><A TImageEnIO.PrintImage></C> <C> -</C> </R>
<R> <C>TImageEnViewPrintImageFitToPage</C> <C>Print to Page</C> <C>Print this image to fit the page</C> <C><A TImageEnIO.PrintImage></C> <C> -</C> </R>
<R> <C>TImageEnViewSelectAcquireSource</C> <C>Select Acquisition Source</C> <C>Select the camera or scanner to acquire images from</C> <C><A TImageEnIO.SelectAcquireSource></C> <C><L TIEAcquireApis>Apis</L></C> </R>
<R> <C>TImageEnViewAcquire</C> <C>Acquire</C> <C>Retrieve an image from a camera or scanner</C> <C><A TImageEnIO.Acquire></C> <C> -</C> </R>
<R> <C>TImageEnViewSeekFirst</C> <C>First Frame</C> <C>Display the first frame of this image</C> <C><A TImageEnIO.Seek></C> <C> -</C> </R>
<R> <C>TImageEnViewSeekPrior</C> <C>Previous Frame</C> <C>Display the prior frame of this image</C> <C><A TImageEnIO.Seek></C> <C> -</C> </R>
<R> <C>TImageEnViewSeekNext</C> <C>Next Frame</C> <C>Display the next frame of this image</C> <C><A TImageEnIO.Seek></C> <C> -</C> </R>
<R> <C>TImageEnViewSeekLast</C> <C>Last Frame</C> <C>Display the last frame of this image</C> <C><A TImageEnIO.Seek></C> <C> -</C> </R>
</TABLE>
!!}


{!!
<FS>Actions ImageIndex List

<FN>
The following is a list of the default ImageIndexes of <L Action Classes>ImageEn Actions</L>:

<TABLE>
<R> <H>ImageIndex</H> <H>Image</H> </R>
<R> <C>0</C> <C>Cut</C> </R>
<R> <C>1</C> <C>Copy</C> </R>
<R> <C>2</C> <C>Paste</C> </R>
<R> <C>3</C> <C>Undo</C> </R>
<R> <C>4</C> <C>Redo</C> </R>
<R> <C>5</C> <C>Delete</C> </R>
<R> <C>6</C> <C>Delete All</C> </R>
<R> <C>7</C> <C>Open</C> </R>
<R> <C>8</C> <C>Save</C> </R>
<R> <C>9</C> <C>Save As</C> </R>
<R> <C>10</C> <C>Save Properties</C> </R>
<R> <C>11</C> <C>Select All</C> </R>
<R> <C>12</C> <C>Deselect</C> </R>
<R> <C>13</C> <C>Rotate Left</C> </R>
<R> <C>14</C> <C>Rotate 180</C> </R>
<R> <C>15</C> <C>Rotate Right</C> </R>
<R> <C>16</C> <C>Flip Horizontal</C> </R>
<R> <C>17</C> <C>Flip Vertical</C> </R>
<R> <C>18</C> <C>Seek First</C> </R>
<R> <C>19</C> <C>Seek Prior</C> </R>
<R> <C>20</C> <C>Seek Next</C> </R>
<R> <C>21</C> <C>Seek Last</C> </R>
<R> <C>22</C> <C>Zoom In</C> </R>
<R> <C>23</C> <C>Zoom Out</C> </R>
<R> <C>24</C> <C>Zoom 100%</C> </R>
<R> <C>25</C> <C>Blank</C> </R>
<R> <C>26</C> <C>Print</C> </R>
<R> <C>27</C> <C>Print Preview</C> </R>
<R> <C>28</C> <C>Adjust Colors</C> </R>
<R> <C>29</C> <C>Image Effects</C> </R>
<R> <C>30</C> <C>Grayscale</C> </R>
<R> <C>31</C> <C>Negative</C> </R>
<R> <C>32</C> <C>Auto-Equalize Colors</C> </R>
<R> <C>33</C> <C>Remove Red Eyes</C> </R>
<R> <C>34</C> <C>Sharpen</C> </R>
<R> <C>35</C> <C>Playing</C> </R>
<R> <C>36</C> <C>Play Loop</C> </R>
<R> <C>37</C> <C>Aquire</C> </R>
<R> <C>38</C> <C>Select Acquire Source</C> </R>
<R> <C>39</C> <C>Auto-Shrink</C> </R>
<R> <C>40</C> <C>Auto-Stretch</C> </R>
<R> <C>41</C> <C>Fit</C> </R>
<R> <C>42</C> <C>Fit to Height</C> </R>
<R> <C>43</C> <C>Fit to Width</C> </R>
<R> <C>44</C> <C>Add Layer</C> </R>
<R> <C>45</C> <C>Remove Layer</C> </R>
<R> <C>46</C> <C>Merge All Layers</C> </R>
<R> <C>47</C> <C>Crop</C> </R>
<R> <C>48</C> <C>Mouse Move Layers</C> </R>
<R> <C>49</C> <C>Mouse Scroll to Cursor</C> </R>
<R> <C>50</C> <C>Mouse Resize Layers</C> </R>
<R> <C>51</C> <C>Mouse Rotate Layers</C> </R>
<R> <C>52</C> <C>Mouse Scroll</C> </R>
<R> <C>53</C> <C>Mouse Select</C> </R>
<R> <C>54</C> <C>Mouse Select Circle</C> </R>
<R> <C>55</C> <C>Mouse Select Lasso</C> </R>
<R> <C>56</C> <C>Mouse Select Magic Wand</C> </R>
<R> <C>57</C> <C>Mouse Select Polygon</C> </R>
<R> <C>58</C> <C>Mouse Select Zoom</C> </R>
<R> <C>59</C> <C>Mouse Zoom</C> </R>
<R> <C>88</C> <C>Auto-Rotate Display</C> </R>   
<R> <C>89</C> <C>Create layer from Clipboard</C> </R>
<R> <C>90</C> <C>Prompt for layer file</C> </R>
<R> <C>91</C> <C>Send layer to back</C> </R>
<R> <C>92</C> <C>Send layer backward</C> </R>
<R> <C>93</C> <C>Bring layer forward</C> </R>
<R> <C>94</C> <C>Bring layer to front</C> </R>
<R> <C>95</C> <C>Crop Transparency</C> </R>
<R> <C>96</C> <C>Set Zoom</C> </R>         
<R> <C>97</C> <C>Prompt to Rotate</C> </R>
<R> <C>98</C> <C>Prompt to Resize</C> </R>
</TABLE>

<FM>ImageEnMView Only<FN>
<TABLE>
<R> <H>ImageIndex</H> <H>Image</H> </R>
<R> <C>60</C> <C>Display Mode Single</C> </R>
<R> <C>61</C> <C>Prompt to Add Image</C> </R>
<R> <C>62</C> <C>Image Save Properties</C> </R>
<R> <C>63</C> <C>Print Selected Thumbs</C> </R>
<R> <C>64</C> <C>Print All Thumbs</C> </R>
</TABLE>      

<FM>ImageEnFolderMView Only<FN>
<TABLE>
<R> <H>ImageIndex</H> <H>Image</H> </R>
<R> <C>99</C> <C>Refresh Folder</C> </R>
<R> <C>100</C> <C>Open File</C> </R>
<R> <C>101</C> <C>Go Up</C> </R>
<R> <C>102</C> <C>Move Files</C> </R>
<R> <C>103</C> <C>Copy Files</C> </R>
<R> <C>104</C> <C>Delete Files</C> </R>
<R> <C>105</C> <C>Copy Files to Clipboard</C> </R>
<R> <C>106</C> <C>Paste Files from Clipboard</C> </R>  
<R> <C>107</C> <C>Rename File</C> </R>
<R> <C>108</C> <C>Prompt for Folder</C> </R>
<R> <C>109</C> <C>Create Folder</C> </R>
</TABLE>

<FM>ImageEnVect Only<FN>
<TABLE>
<R> <H>ImageIndex</H> <H>Image</H> </R>
<R> <C>65</C> <C>Merge Objects to Back</C> </R>
<R> <C>66</C> <C>Merge All to Back</C> </R>
<R> <C>67</C> <C>Bring Object to Front</C> </R>
<R> <C>68</C> <C>Bring Object Forward</C> </R>
<R> <C>69</C> <C>Send Object to Back</C> </R>
<R> <C>70</C> <C>Send Object Backward</C> </R>
<R> <C>71</C> <C>Mouse Measure Area</C> </R>
<R> <C>72</C> <C>Mouse Measure Length</C> </R>
<R> <C>73</C> <C>Mouse Insert Line</C> </R>
<R> <C>74</C> <C>Mouse Insert Rectangle</C> </R>
<R> <C>75</C> <C>Mouse Insert Ellipse</C> </R>
<R> <C>76</C> <C>Mouse Insert Image</C> </R>
<R> <C>77</C> <C>Mouse Insert Text</C> </R>
<R> <C>78</C> <C>Mouse Select Object</C> </R>
<R> <C>79</C> <C>Mouse Measure Distance</C> </R>
<R> <C>80</C> <C>Mouse Insert Ruler</C> </R>
<R> <C>81</C> <C>Mouse Insert Polyline</C> </R>
<R> <C>82</C> <C>Mouse Insert Angle</C> </R>
<R> <C>83</C> <C>Mouse Insert Memo</C> </R>
<R> <C>84</C> <C>Mouse Insert Line Label</C> </R>
<R> <C>85</C> <C>Mouse Edit Polyline</C> </R>
<R> <C>86</C> <C>Mouse Unstamp</C> </R>       
<R> <C>87</C> <C>Crop Image to Objects</C> </R>
</TABLE>
!!}

end.


