{------------------------------------------------------------------------------}
{                                                                              }
{  TActions for common TImageEnMView functions                                 }
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

unit iexActionsMulti;

{$I ie.inc}

{$IFDEF IEINCLUDEACTIONS}
  {$DEFINE INCLUDE_MULTIVIEWACTIONS}
{$ENDIF}
{$IFDEF IEINCLUDEMULTIVIEW}
  {$DEFINE INCLUDE_MULTIVIEWACTIONS}
{$ELSE}
  {$UNDEF INCLUDE_MULTIVIEWACTIONS}
{$ENDIF}

{$IFDEF INCLUDE_MULTIVIEWACTIONS}


interface

Uses
  ActnList, Classes, ieMIO, imageenproc, hyiedefs,
  {$IFDEF IEINCLUDEIEXACQUIRE}
  iexAcquire,
  {$ENDIF}
  IEMView, imageenio;

Type

{!!
<FS>TIEAutoSaveErrorEvent

<FM>Declaration<FC>
type TIEImageSaveErrorEvent = procedure(Sender: TObject; Filename: string; ErrorMsg: string) of object;

<FM>Description<FN>
If AutoSaveChanges has been enabled for a relevant TImageEnMViewAction then this event will be called when changes cannot be saved
!!}
  TIEAutoSaveErrorEvent = procedure(Sender: TObject; Filename: string; ErrorMsg: string) of object;



  TImageEnMViewAction = class(TAction)
  private
  protected
    function ActiveImageEnMView : TImageEnMView;
    function HaveActiveImageEnMView : boolean;    
    function ActiveImageEnProc : TImageEnProc;
    function HaveActiveImageEnProc : boolean;  
    function ActiveImageEnMIO : TImageEnMIO;
    function HaveActiveImageEnMIO : boolean;
    function HaveSelection : boolean;                
    procedure CheckSelection;
    function CurrentImageIndex : integer;
  public
    fImageEnMView : TImageEnMView;
    fRequireSelection : Boolean;
    fRequireImages : Boolean;
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; Override;
    procedure UpdateTarget(Target: TObject); override;
    function BaseEnabled : Boolean;
  published       
    property ImageEnMView: TImageEnMView read fImageEnMView write fImageEnMView;
  end;
  
  TImageEnMViewClear = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;      

  TImageEnMViewDeleteImage = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewDeselect = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;
  
  TImageEnMViewSelectAll = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewPlaying = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewPlayLoop = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewDisplayModeSingle = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewEnableAdjustOrientation = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewProcAction = class(TImageEnMViewAction)
  private
    fAutoSaveChanges : Boolean;
    fOnAutoSaveError : TIEAutoSaveErrorEvent;
    procedure DoAutoSaveChanges(iOverrideJpegQuality : Integer);
    procedure DoLosslessTranform(Transform: TIEJpegTransform);
    function CanUseLosslessTranform : boolean;
    function CurrentFilename : string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function CanAutoSaveChanges : boolean;
  published
    property AutoSaveChanges : Boolean read fAutoSaveChanges write fAutoSaveChanges default False;
    property OnAutoSaveError : TIEAutoSaveErrorEvent read fOnAutoSaveError write fOnAutoSaveError;
  end;


  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnMViewDoEffectPreviews = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private         
    fOverrideJpegQuality : Integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published         
    property OverrideJpegQuality : Integer read fOverrideJpegQuality write fOverrideJpegQuality default 0;
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnMViewDoAdjustPreviews = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private    
    fOverrideJpegQuality : Integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
    property OverrideJpegQuality : Integer read fOverrideJpegQuality write fOverrideJpegQuality default 0;
  end;
  {$ENDIF}     

  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnMViewPromptToRotate = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private
    fAntiAliasMode : TIEAntialiasMode; 
    fOverrideJpegQuality : Integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
    property AntiAliasMode : TIEAntialiasMode read fAntiAliasMode write fAntiAliasMode default ierFast;  
    property OverrideJpegQuality : Integer read fOverrideJpegQuality write fOverrideJpegQuality default 0;
  end;
  {$ENDIF}

  {$IFDEF IEINCLUDEDIALOGIP}
  TImageEnMViewPromptToResize = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private
    fResampleFilter : TResampleFilter; 
    fOverrideJpegQuality : Integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published              
    property ResampleFilter : TResampleFilter read fResampleFilter write fResampleFilter default rfFastLinear;      
    property OverrideJpegQuality : Integer read fOverrideJpegQuality write fOverrideJpegQuality default 0;
  end;
  {$ENDIF}

  TImageEnMViewFlipHorizontal = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;      
    procedure UpdateLanguage();
  published
  end;
  
  TImageEnMViewFlipVertical = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage();
  published
  end;
  
  TImageEnMViewRotateRight = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewRotate180 = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewRotateLeft = class(TImageEnMViewProcAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateLanguage();
  published
  end;     

  TImageEnMViewCutToClipboard = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage();
  published
  end;

  TImageEnMViewCopyToClipboard = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage();
  published
  end;
  
  TImageEnMViewPasteFromClipboard = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewPromptToOpen = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
    fMultiSelect : Boolean;
    fDialogTitle : WideString;
    fDefaultFilter : TIOFileType;
    fLimitToFileType : TIOFileType;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
    property MultiSelect : Boolean read fMultiSelect write fMultiSelect default false;
    property DialogTitle : WideString read fDialogTitle write fDialogTitle;
    property DefaultFilter : TIOFileType read fDefaultFilter write fDefaultFilter default -1;
    property LimitToFileType : TIOFileType read fLimitToFileType write fLimitToFileType default -1;
  end;
  {$ENDIF}
           
  {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  TImageEnMViewPromptToAdd = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewSave = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewPromptToSave = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewDoIOPreviews = class(TImageEnMViewAction, IIELanguageUpdatable)
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

  {$IFDEF IEINCLUDEDIALOGIO}
  TImageEnMViewDoIOPreviewsSelected = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewDoPrintPreviewDialog = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewPrintImageNormal = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewPrintImageFitToPage = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewPrintSelectedThumbnails = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
    fColumnCount : integer;
    fRowCount    : integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;        
    procedure UpdateLanguage();
  published
    property ColumnCount : integer read fColumnCount write fColumnCount default 4;
    property RowCount : integer read fRowCount write fRowCount default 6;
  end;
  {$ENDIF}
                                
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  TImageEnMViewPrintAllThumbnails = class(TImageEnMViewAction, IIELanguageUpdatable)
  private   
    fColumnCount : integer;
    fRowCount    : integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage();
  published
    property ColumnCount : integer read fColumnCount write fColumnCount default 4;
    property RowCount : integer read fRowCount write fRowCount default 6;
  end;
  {$ENDIF}
  
  {$IFDEF IEINCLUDEIEXACQUIRE}
  TImageEnMViewSelectAcquireSource = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  TImageEnMViewAcquire = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;       
    procedure UpdateLanguage();
  published
  end;
  {$ENDIF}

  TImageEnMViewSeekFirst = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;    
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;
    
  TImageEnMViewSeekPrior = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;
    
  TImageEnMViewSeekNext = class(TImageEnMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;   
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;
    
  TImageEnMViewSeekLast = class(TImageEnMViewAction, IIELanguageUpdatable)
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
  CTRL_A_SHORTCUT       = 16449;
  CTRL_C_SHORTCUT       = 16451;
  CTRL_O_SHORTCUT       = 16463;
  CTRL_P_SHORTCUT       = 16464;
  CTRL_S_SHORTCUT       = 16467;
  CTRL_V_SHORTCUT       = 16470;
  CTRL_X_SHORTCUT       = 16472;
  CTRL_Y_SHORTCUT       = 16473;
  CTRL_Z_SHORTCUT       = 16474;
  CTRL_ALT_S_SHORTCUT   = 49235;
  CTRL_DELETE_SHORTCUT  = 16430;
  

implementation

uses
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  Printers,
  {$ENDIF}
  Forms, hyieutils, SysUtils, iewords, iesettings;


constructor  TImageEnMViewAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRequireImages := True;
  fRequireSelection := True;
  Caption := 'ImageEn Action';
  DisableIfNoHandler := false;
end;                       

function TImageEnMViewAction.CurrentImageIndex: integer;
begin
  Result := -1;
  if HaveActiveImageEnMView then
  begin
    if ActiveImageEnMView.DisplayMode = mdSingle then
      Result := ActiveImageEnMView.VisibleFrame
    else
      Result := ActiveImageEnMView.SelectedImage;
  end;
end;

function  TImageEnMViewAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

function TImageEnMViewAction.HaveActiveImageEnMIO: boolean;
begin     
  Result := HaveActiveImageEnMView; // It will always create a IO if it is required
end;

function TImageEnMViewAction.HaveActiveImageEnProc: boolean;
begin    
  Result := HaveActiveImageEnMView; // It will always create a Proc if it is required
end;

function TImageEnMViewAction.HaveActiveImageEnMView: boolean;
begin
  Result := ActiveImageEnMView <> nil;
end;

function TImageEnMViewAction.HaveSelection: boolean;
begin
  Result := HaveActiveImageEnMView and
            (ActiveImageEnMView.SelectedImage > -1);
end;


procedure TImageEnMViewAction.CheckSelection;
begin
  // if we are in mdSingle mode then ensure the "Current Image" is the displayed mode
  if HaveActiveImageEnMView then
    if ActiveImageEnMView.DisplayMode = mdSingle then
      ActiveImageEnMView.SelectedImage := ActiveImageEnMView.VisibleFrame;
end;


function TImageEnMViewAction.BaseEnabled: boolean;
begin
  Result := HaveActiveImageEnMView;
  if Result and fRequireImages then
  begin
    Result := ActiveImageEnMView.ImageCount > 0;

    // fRequireSelection assumes fRequireImages
    if Result and fRequireSelection then
      Result := HaveSelection;
  end;
end;

procedure TImageEnMViewAction.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
end;


function TImageEnMViewAction.ActiveImageEnMIO: TImageEnMIO;
begin
  Result := nil;
  if HaveActiveImageEnMView then
    Result := ActiveImageEnMView.MIO;
end;                                  


function TImageEnMViewAction.ActiveImageEnProc: TImageEnProc;
begin
  Result := nil;
  if HaveActiveImageEnMView then
    Result := ActiveImageEnMView.Proc;
end;
                       
// Return the speciifed or selected ImageEnMView
function TImageEnMViewAction.ActiveImageEnMView: TImageEnMView;
begin
  Result := fImageEnMView;
  if (Result = nil) and Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TImageEnMView) then
    Result := TImageEnMView(Screen.ActiveControl);
end;

{ TImageEnMViewClear }

constructor TImageEnMViewClear.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ImageIndex := 25;
  UpdateLanguage();
end;

procedure TImageEnMViewClear.UpdateLanguage();
begin      
  Caption := iemsg(IEMsg_Clear);
  Hint := iemsg(IEMsg_ClearAllImages);
end;

procedure TImageEnMViewClear.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    ActiveImageEnMView.Clear;
end;


{ TImageEnMViewDeleteImage  }

constructor TImageEnMViewDeleteImage.Create(AOwner: TComponent);
begin
  inherited;
  ShortCut := CTRL_DELETE_SHORTCUT;
  ImageIndex := 5;
  UpdateLanguage();
end;

procedure TImageEnMViewDeleteImage.UpdateLanguage();
begin   
  Caption := iemsg(IEMsg_Delete);
  Hint := iemsg(IEMsg_RemoveTheSelectedImage);
end;

procedure TImageEnMViewDeleteImage.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    ActiveImageEnMView.DeleteSelectedImages;
end;


{ TImageEnMViewSelectAll }

constructor TImageEnMViewSelectAll.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ShortCut := CTRL_A_SHORTCUT;
  ImageIndex := 11;
  UpdateLanguage();
end;

procedure TImageEnMViewSelectAll.UpdateLanguage();
begin    
  Caption := iemsg(IEMsg_SelectAll);
  Hint := iemsg(IEMsg_SelectAllImagesInThegrid);
end;

procedure TImageEnMViewSelectAll.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    ActiveImageEnMView.SelectAll;
end;

procedure TImageEnMViewSelectAll.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnMView then
    bEnabled := ActiveImageEnMView.EnableMultiSelect and (ActiveImageEnMView.DisplayMode = mdGrid);
  Enabled := bEnabled;
end;


{ TImageEnMViewDeselect }

constructor TImageEnMViewDeselect.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 12;
  UpdateLanguage();
end;

procedure TImageEnMViewDeselect.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Deselect);
  Hint := iemsg(IEMsg_ClearYourSelection);
end;

procedure TImageEnMViewDeselect.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    ActiveImageEnMView.DeSelect;
end;

procedure TImageEnMViewDeselect.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnMView then
    bEnabled := ActiveImageEnMView.DisplayMode = mdGrid;
  Enabled := bEnabled;
end;


{ TImageEnMViewPlaying }

constructor TImageEnMViewPlaying.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ImageIndex := 35;
  UpdateLanguage();
end;

procedure TImageEnMViewPlaying.UpdateLanguage();
begin    
  Caption := iemsg(IEMsg_Animate);
  Hint := iemsg(IEMsg_PlaybackTheseFramesInSequence);
end;

procedure TImageEnMViewPlaying.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    ActiveImageEnMView.Playing := not ActiveImageEnMView.Playing;
end;

procedure TImageEnMViewPlaying.UpdateTarget(Target: TObject);
var
  bCanAnimate: Boolean;
begin
  bCanAnimate := False;
  if BaseEnabled and HaveActiveImageEnMView then
    bCanAnimate := ActiveImageEnMView.ImageCount > 1;

  Enabled := bCanAnimate;

  if HaveActiveImageEnMView and bCanAnimate then
    Checked := ActiveImageEnMView.Playing
  else
    Checked := False;
end;


{ TImageEnMViewPlayLoop }

constructor TImageEnMViewPlayLoop.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ImageIndex := 36;
  UpdateLanguage();
end;

procedure TImageEnMViewPlayLoop.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_LoopPlayback);
  Hint := iemsg(IEMsg_RestartPlaybackAfterItCompletes);
end;

procedure TImageEnMViewPlayLoop.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    ActiveImageEnMView.PlayLoop := not ActiveImageEnMView.PlayLoop;
end;

procedure TImageEnMViewPlayLoop.UpdateTarget(Target: TObject);
var
  bCanAnimate: Boolean;
begin
  bCanAnimate := False;
  if BaseEnabled and HaveActiveImageEnMView then
    bCanAnimate := ActiveImageEnMView.ImageCount > 1;

  Enabled := bCanAnimate;

  if HaveActiveImageEnMView and bCanAnimate then
    Checked := ActiveImageEnMView.PlayLoop
  else
    Checked := False;
end;



{ TImageEnMViewDisplayModeSingle }

constructor TImageEnMViewDisplayModeSingle.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ImageIndex := 60;
  UpdateLanguage();
end;

procedure TImageEnMViewDisplayModeSingle.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_SingleFrameOnly);
  Hint := iemsg(IEMsg_DisplayOnlyTheActiveFrame);
end;

procedure TImageEnMViewDisplayModeSingle.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
  begin
    if ActiveImageEnMView.DisplayMode = mdGrid then
    begin
      ActiveImageEnMView.VisibleFrame := ActiveImageEnMView.SelectedImage;
      ActiveImageEnMView.DisplayMode := mdSingle
    end
    else
    begin
      ActiveImageEnMView.SelectedImage := ActiveImageEnMView.VisibleFrame;
      ActiveImageEnMView.DisplayMode := mdGrid;
    end;
  end;
end;

procedure TImageEnMViewDisplayModeSingle.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if Enabled and HaveActiveImageEnMView then
    Checked :=  ActiveImageEnMView.DisplayMode = mdSingle
  else
    Checked := False;
end;



{ TImageEnMViewEnableAdjustOrientation }

constructor TImageEnMViewEnableAdjustOrientation.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ImageIndex := 88;
  UpdateLanguage();
end;

procedure TImageEnMViewEnableAdjustOrientation.UpdateLanguage();
begin                   
  Caption := iemsg(IEMsg_AutoRotateDisplay);
  Hint := iemsg(IEMsg_AutomaticallyDisplayImagesWithTheCorrectOrientation);
end;

procedure TImageEnMViewEnableAdjustOrientation.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    ActiveImageEnMView.EnableAdjustOrientation := not ActiveImageEnMView.EnableAdjustOrientation;
end;

procedure TImageEnMViewEnableAdjustOrientation.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
  if Enabled and HaveActiveImageEnMView then
    Checked :=  ActiveImageEnMView.EnableAdjustOrientation
  else
    Checked := False;
end;


{ TImageEnMViewProcAction }

function TImageEnMViewProcAction.CanAutoSaveChanges: boolean;
var
  sFilename: string;
begin
  Result := False;
  if HaveActiveImageEnMView then
  begin
    sFilename := CurrentFilename;
    Result := (sFilename <> '') and
              (pos(iemview.IEM_Path_Index_Delimiter, sFilename) = 0); // not one frame of a multiframe file
  end;
end;

function TImageEnMViewProcAction.CanUseLosslessTranform: boolean;
var
  ex: string;
begin
  ex := string(IEExtractFileExtW(CurrentFileName));
  Result := (ex = '.jpg') or (ex = '.jpeg') or (ex = '.jpe') or (ex='.jif');
end;

constructor TImageEnMViewProcAction.Create(AOwner: TComponent);
begin
  inherited;
  fAutoSaveChanges := False;
end;

function TImageEnMViewProcAction.CurrentFilename: string;
begin  
  Result := '';
  if HaveActiveImageEnMView and (CurrentImageIndex >= 0) then
    Result := ActiveImageEnMView.ImageFilename[CurrentImageIndex];
end;

procedure TImageEnMViewProcAction.DoAutoSaveChanges(iOverrideJpegQuality : Integer);
var
  ABitmap : TIEBitmap;
  iImageIndex: Integer;
  sFilename: string;
begin
  if CanAutoSaveChanges then
  try
    iImageIndex := CurrentImageIndex;
    sFilename := CurrentFilename;

    if pos(IEM_Path_Index_Delimiter, sFilename) > 0 then // is one frame of a multiframe file
      raise Exception.create('File is part of a multi-frame image set');

    try
      ABitmap := ActiveImageEnMView.GetTIEBitmap(iImageIndex);
      if iOverrideJpegQuality > 0 then
        ActiveImageEnMIO.Params[iImageIndex].JPEG_Quality := iOverrideJpegQuality;
      ABitmap.write(sFilename, ActiveImageEnMIO.Params[iImageIndex]);
    finally
      ActiveImageEnMView.ReleaseBitmap(0);
    end;

  except
    on E:Exception do
    begin
      if assigned(fOnAutoSaveError) then
        fOnAutoSaveError(ActiveImageEnMView, CurrentFilename, E.Message);
    end;
  end;
end;


procedure TImageEnMViewProcAction.DoLosslessTranform(Transform: TIEJpegTransform);
var
  sFilename: string;
begin   
  if CanAutoSaveChanges then
  try
    sFilename := CurrentFilename;
    if pos(IEM_Path_Index_Delimiter, sFilename) > 0 then // is one frame of a multiframe file
      raise Exception.create('File is part of a multi-frame image set');

    if JpegLosslessTransform2(sFileName, Transform, False, jcCopyAll, Rect(0, 0, 0, 0), True) = False then
      raise Exception.create('Unexpected error while performing lossless transformation');

  except
    on E:Exception do
    begin
      if assigned(fOnAutoSaveError) then
        fOnAutoSaveError(ActiveImageEnMView, CurrentFilename, E.Message);
    end;
  end;
end;


{ TImageEnMViewDoAdjustPreviews }

{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnMViewDoAdjustPreviews.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 28; 
  fOverrideJpegQuality := 0;
  UpdateLanguage();
end;

procedure TImageEnMViewDoAdjustPreviews.UpdateLanguage();
begin              
  Caption := iemsg(IEMsg_AdjustColors);
  Hint := iemsg(IEMsg_PerformColorEnhancementFunctionsOnTheSelectedImage);
end;

procedure TImageEnMViewDoAdjustPreviews.ExecuteTarget(Target: TObject);
begin
  CheckSelection;
  if HaveActiveImageEnProc then
  begin
    if ActiveImageEnProc.DoPreviews(ppeColorAdjust) and fAutoSaveChanges then
      DoAutoSaveChanges(fOverrideJpegQuality);
  end;
end;
{$ENDIF}


{ TImageEnMViewDoEffectPreviews }
                              
{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnMViewDoEffectPreviews.Create(AOwner: TComponent);
begin
  inherited;       
  fOverrideJpegQuality := 0;
  ImageIndex := 29;
  UpdateLanguage();
end;

procedure TImageEnMViewDoEffectPreviews.UpdateLanguage();
begin           
  Caption := iemsg(IEMsg_ImageEffects);
  Hint := iemsg(IEMsg_PerformEffectsOnTheSelectedImage);
end;

procedure TImageEnMViewDoEffectPreviews.ExecuteTarget(Target: TObject);
begin
  CheckSelection;
  if HaveActiveImageEnProc then
  begin
    if ActiveImageEnProc.DoPreviews(ppeEffects - [peRotate, peResize]) and fAutoSaveChanges then
      DoAutoSaveChanges(fOverrideJpegQuality);
  end;
end;
{$ENDIF}


{ TImageEnMViewPromptToRotate }

{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnMViewPromptToRotate.Create(AOwner: TComponent);
begin
  inherited;        
  fOverrideJpegQuality := 0;
  ImageIndex := 97;     
  fAntiAliasMode := ierFast;
  UpdateLanguage();
end;

procedure TImageEnMViewPromptToRotate.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CustomRotate);
  Hint := iemsg(IEMsg_RotateImageByACustomAngle);     
end;

procedure TImageEnMViewPromptToRotate.ExecuteTarget(Target: TObject);
var
  WasAntiAlias : TIEAntialiasMode;
begin
  if HaveActiveImageEnProc then
  begin
    WasAntiAlias := IEGlobalSettings().DefaultRotateAntiAlias;
    try
      IEGlobalSettings().DefaultRotateAntiAlias := fAntiAliasMode;
      if ActiveImageEnProc.DoPreviews([peRotate]) and fAutoSaveChanges then
        DoAutoSaveChanges(fOverrideJpegQuality);
    finally
      IEGlobalSettings().DefaultRotateAntiAlias := WasAntiAlias;
    end;
  end;
end;
{$ENDIF}

{ TImageEnMViewPromptToResize }

{$IFDEF IEINCLUDEDIALOGIP}
constructor TImageEnMViewPromptToResize.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 98;   
  fResampleFilter := rfFastLinear; 
  fOverrideJpegQuality := 0;
  UpdateLanguage();
end;

procedure TImageEnMViewPromptToResize.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ResizeImage) + '...';
  Hint := iemsg(IEMsg_SpecifyANewSizeForTheImage);
end;

procedure TImageEnMViewPromptToResize.ExecuteTarget(Target: TObject);
var
  WasResampleFilter : TResampleFilter;
begin
  if HaveActiveImageEnProc then
  begin
    WasResampleFilter := IEGlobalSettings().DefaultResampleFilter;
    try
      IEGlobalSettings().DefaultResampleFilter := fResampleFilter;
      if ActiveImageEnProc.DoPreviews([peResize]) and fAutoSaveChanges then
        DoAutoSaveChanges(fOverrideJpegQuality);
    finally
      IEGlobalSettings().DefaultResampleFilter := WasResampleFilter;
    end;
  end;
end;
{$ENDIF}


{ TImageEnMViewFlipHorizontal }

constructor TImageEnMViewFlipHorizontal.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 16;
  UpdateLanguage();
end;

procedure TImageEnMViewFlipHorizontal.UpdateLanguage();
begin   
  Caption := iemsg(IEMsg_FlipHorizontal);
  Hint := iemsg(IEMsg_FlipTheselectedImageFromTopToBottom);
end;

procedure TImageEnMViewFlipHorizontal.ExecuteTarget(Target: TObject);
begin                          
  CheckSelection;

  if HaveActiveImageEnProc then
    ActiveImageEnProc.Flip(fdHorizontal);

  if fAutoSaveChanges then
  begin
    if CanUseLosslessTranform then
      DoLosslessTranform(jtHorizFlip)
    else
      DoAutoSaveChanges(-1);
  end;
end;


{ TImageEnMViewFlipVertical }

constructor TImageEnMViewFlipVertical.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 17;
  UpdateLanguage();
end;

procedure TImageEnMViewFlipVertical.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_FlipVertical);
  Hint := iemsg(IEMsg_FlipTheselectedImageFromLeftToRight);
end;

procedure TImageEnMViewFlipVertical.ExecuteTarget(Target: TObject);
begin
  CheckSelection;

  if HaveActiveImageEnProc then
    ActiveImageEnProc.Flip(fdVertical);  

  if fAutoSaveChanges then
  begin
    if CanUseLosslessTranform then
      DoLosslessTranform(jtVertFlip)
    else
      DoAutoSaveChanges(-1);
  end;
end;


{ TImageEnMViewRotateRight }

constructor TImageEnMViewRotateRight.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 15;
  UpdateLanguage();
end;

procedure TImageEnMViewRotateRight.UpdateLanguage();
begin  
  Caption := iemsg(IEMsg_RotateRight);
  Hint := iemsg(IEMsg_RotateTheSelectedImage90Clockwise);
end;

procedure TImageEnMViewRotateRight.ExecuteTarget(Target: TObject);
begin                             
  CheckSelection;

  if HaveActiveImageEnProc then
    ActiveImageEnProc.Rotate(270, ierFast); // Antialias is not used for 90 deg. rotate anyway

  if fAutoSaveChanges then
  begin
    if CanUseLosslessTranform then
      DoLosslessTranform(jtRotate90)
    else
      DoAutoSaveChanges(-1);
  end;
end;


{ TImageEnMViewRotate180 }

constructor TImageEnMViewRotate180.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 14;
  UpdateLanguage();
end;

procedure TImageEnMViewRotate180.UpdateLanguage();
begin  
  Caption := iemsg(IEMsg_Rotate180);
  Hint := iemsg(IEMsg_RotateTheSelectedImage180Clockwise);
end;

procedure TImageEnMViewRotate180.ExecuteTarget(Target: TObject);
begin                  
  CheckSelection;

  if HaveActiveImageEnProc then
    ActiveImageEnProc.Rotate(180, ierFast); // Antialias is not used for 90 deg. rotate anyway
    
  if fAutoSaveChanges then
  begin
    if CanUseLosslessTranform then
      DoLosslessTranform(jtRotate180)
    else
      DoAutoSaveChanges(-1);
  end;
end;


{ TImageEnMViewRotateLeft }

constructor TImageEnMViewRotateLeft.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 13;
  UpdateLanguage();
end;

procedure TImageEnMViewRotateLeft.UpdateLanguage();
begin         
  Caption := iemsg(IEMsg_RotateLeft);
  Hint := iemsg(IEMsg_RotateTheselectedImage90Counterclockwise);
end;

procedure TImageEnMViewRotateLeft.ExecuteTarget(Target: TObject);
begin
  CheckSelection;

  if HaveActiveImageEnProc then
    ActiveImageEnProc.Rotate(90, ierFast); // Antialias is not used for 90 deg. rotate anyway
    
  if fAutoSaveChanges then
  begin
    if CanUseLosslessTranform then
      DoLosslessTranform(jtRotate270)
    else
      DoAutoSaveChanges(-1);
  end;
end;    


{ TImageEnMViewCutToClipboard }

constructor TImageEnMViewCutToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  ShortCut := CTRL_X_SHORTCUT;
  ImageIndex := 0;
  UpdateLanguage();
end;

procedure TImageEnMViewCutToClipboard.UpdateLanguage();
begin  
  Caption := iemsg(IEMsg_Cut);
  Hint := iemsg(IEMsg_CopyTheCurrentImageToTheClipboardAndRemoveIt);
end;

procedure TImageEnMViewCutToClipboard.ExecuteTarget(Target: TObject);
begin                        
  CheckSelection;
  if HaveActiveImageEnProc then
  begin
    ActiveImageEnProc.CopyToClipboard;

    if ActiveImageEnMView.SelectedImage > -1 then
      ActiveImageEnMView.DeleteImage(ActiveImageEnMView.SelectedImage); // Only the current image
  end;
end;


{ TImageEnMViewCopyToClipboard }

constructor TImageEnMViewCopyToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  ShortCut := CTRL_C_SHORTCUT;
  ImageIndex := 1;
  UpdateLanguage();
end;

procedure TImageEnMViewCopyToClipboard.UpdateLanguage();
begin        
  Caption := iemsg(IEMsg_Copy);
  Hint := iemsg(IEMsg_CopyTheCurrentImageToTheClipboard);
end;

procedure TImageEnMViewCopyToClipboard.ExecuteTarget(Target: TObject);
begin                        
  CheckSelection;
  if HaveActiveImageEnProc then
    ActiveImageEnProc.CopyToClipboard;
end;


{ TImageEnMViewPasteFromClipboard }

constructor TImageEnMViewPasteFromClipboard.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ShortCut := CTRL_V_SHORTCUT;
  ImageIndex := 2;
  UpdateLanguage();
end;

procedure TImageEnMViewPasteFromClipboard.UpdateLanguage();
begin            
  Caption := iemsg(IEMsg_Paste);
  Hint := iemsg(IEMsg_PasteAnImageFromTheClipboard);
end;

procedure TImageEnMViewPasteFromClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnProc then
    if ActiveImageEnProc.IsClipboardAvailable then
    begin
      ActiveImageEnMView.AppendImage(100, 100);
      ActiveImageEnProc.PasteFromClipboard;
    end;
end;



procedure TImageEnMViewPasteFromClipboard.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnProc then
    bEnabled := ActiveImageEnProc.IsClipboardAvailable;
  Enabled := bEnabled;
end;


{ TImageEnMViewPromptToOpen }
                                
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnMViewPromptToOpen.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ShortCut := CTRL_O_SHORTCUT;
  ImageIndex := 7;     
  fDefaultFilter   := -1;
  fLimitToFileType := -1;
  fMultiSelect     := False;
  UpdateLanguage();
end;

procedure TImageEnMViewPromptToOpen.UpdateLanguage();
begin         
  Caption := iemsg(IEMsg_Open);
  Hint := iemsg(IEMsg_LoadAnImageFromFile);
end;

procedure TImageEnMViewPromptToOpen.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnMIO then
  begin
    sFilename := ActiveImageEnMIO.ExecuteOpenDialog('', '', false, 0, '', fMultiSelect, fDialogTitle, '', fDefaultFilter, fLimitToFileType);
    if sFilename <> '' then
      ActiveImageEnMIO.LoadFromFile(sFilename);
  end;
end;
{$ENDIF}



{ TImageEnMViewPromptToAdd }
                                
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnMViewPromptToAdd.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ImageIndex := 61;
  UpdateLanguage();
end;

procedure TImageEnMViewPromptToAdd.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_AddFromFile);
  Hint := iemsg(IEMsg_AddAnImageToThegridFromFile);
end;

procedure TImageEnMViewPromptToAdd.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnMIO then
  begin
    sFilename := ActiveImageEnMIO.ExecuteOpenDialog('', '', false);
    if sFilename <> '' then
      ActiveImageEnMView.AppendImage(sFilename);
  end;
end;
{$ENDIF}



{ TImageEnMViewSave }
                         
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnMViewSave.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ShortCut := CTRL_S_SHORTCUT;
  ImageIndex := 8;
  UpdateLanguage();
end;

procedure TImageEnMViewSave.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Save);
  Hint := iemsg(IEMsg_SaveChangesToThisImageToFile);
end;

procedure TImageEnMViewSave.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnMIO then
  begin
    sFilename := ActiveImageEnMIO.LastFilename;
    if sFilename = '' then
      sFilename := ActiveImageEnMIO.ExecuteSaveDialog('', '', false);
    if sFilename <> '' then
      ActiveImageEnMIO.SaveToFile(sFilename);
  end;
end;
{$ENDIF}


{ TImageEnMViewPromptToSave }
                               
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
constructor TImageEnMViewPromptToSave.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ShortCut := CTRL_ALT_S_SHORTCUT;
  ImageIndex := 9;           
  fDefaultFilter   := -1;
  fLimitToFileType := -1;
  UpdateLanguage();
end;

procedure TImageEnMViewPromptToSave.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SaveAs);
  Hint := iemsg(IEMsg_SaveThisImageToANewFilename);
end;

procedure TImageEnMViewPromptToSave.ExecuteTarget(Target: TObject);
var
  sFilename: string;
begin
  if HaveActiveImageEnMIO then
  begin                            
    sFilename := ActiveImageEnMIO.ExecuteSaveDialog('', '', false, 0, '', fDialogTitle, '', fDefaultFilter, fLimitToFileType);
    if sFilename <> '' then
      ActiveImageEnMIO.SaveToFile(sFilename);
  end;
end;
{$ENDIF}


{ TImageEnMViewDoIOPreviews }

{$IFDEF IEINCLUDEDIALOGIO}
constructor TImageEnMViewDoIOPreviews.Create(AOwner: TComponent);
begin
  inherited;
  fPreviewParams := [ppAll];
  fRequireSelection := False;
  ImageIndex := 10;
  UpdateLanguage();
end;

procedure TImageEnMViewDoIOPreviews.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SaveProperties);
  Hint := iemsg(IEMsg_SpecifyAdvancedPropertiesForAllImages);
end;

procedure TImageEnMViewDoIOPreviews.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.DoPreviews(-1, fPreviewParams);
end;
{$ENDIF}


{ TImageEnMViewDoIOPreviews }

{$IFDEF IEINCLUDEDIALOGIO}
constructor TImageEnMViewDoIOPreviewsSelected.Create(AOwner: TComponent);
begin
  inherited;
  fPreviewParams := [ppAll];
  ImageIndex := 62;
  UpdateLanguage();
end;

procedure TImageEnMViewDoIOPreviewsSelected.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_ImageSaveProperties);
  Hint := iemsg(IEMsg_SpecifyAdvancedPropertiesForTheSelectedImage);
end;

procedure TImageEnMViewDoIOPreviewsSelected.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.DoPreviews(IEM_SELECTED_IMAGES, fPreviewParams);
end;
{$ENDIF}


{ TImageEnMViewDoPrintPreviewDialog }
                                                                 
{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnMViewDoPrintPreviewDialog.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ShortCut := CTRL_P_SHORTCUT;
  ImageIndex := 27;
  UpdateLanguage();
end;

procedure TImageEnMViewDoPrintPreviewDialog.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PrintPreview);
  Hint := iemsg(IEMsg_DisplayAPreviewOfYourPrinting);
end;

procedure TImageEnMViewDoPrintPreviewDialog.ExecuteTarget(Target: TObject);
begin
  CheckSelection;
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.DoPrintPreviewDialog;
end;
{$ENDIF}


{ TImageEnMViewPrintImageNormal }

{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnMViewPrintImageNormal.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 26;
  UpdateLanguage();
end;

procedure TImageEnMViewPrintImageNormal.UpdateLanguage();
begin              
  Caption := iemsg(IEMsg_Print);
  Hint := iemsg(IEMsg_PrintTheSelectedImageAtItsOriginalSize);
end;

procedure TImageEnMViewPrintImageNormal.ExecuteTarget(Target: TObject);
begin
  CheckSelection;
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.PrintImage(IEM_SELECTED_IMAGES, Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesNORMAL);
end;
{$ENDIF}


{ TImageEnMViewPrintImageFitToPage }
                                       
{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnMViewPrintImageFitToPage.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 26;
  UpdateLanguage();
end;

procedure TImageEnMViewPrintImageFitToPage.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_PrintToPage);
  Hint := iemsg(IEMsg_PrintTheSelectedImageToFitThePage);
end;

procedure TImageEnMViewPrintImageFitToPage.ExecuteTarget(Target: TObject);
begin
  CheckSelection;
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.PrintImage(IEM_SELECTED_IMAGES, Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE);
end;
{$ENDIF}

                         
{ TImageEnMViewPrintSelectedThumbnails }
                                                            
{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnMViewPrintSelectedThumbnails.Create(AOwner: TComponent);
begin
  inherited;
  fColumnCount := 4;
  fRowCount    := 6;
  ImageIndex := 63;
  UpdateLanguage();
end;

procedure TImageEnMViewPrintSelectedThumbnails.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_PrintThumbnails);
  Hint := iemsg(IEMsg_PrintTheSelectedImagesAsASheetOfThumbnails);
end;

procedure TImageEnMViewPrintSelectedThumbnails.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.PrintImages(fColumnCount, fRowCount, 0.5, 0.5, True)
end;
{$ENDIF}


{ TImageEnMViewPrintAllThumbnails }

{$IFDEF IEINCLUDEPRINTDIALOGS}
constructor TImageEnMViewPrintAllThumbnails.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  fColumnCount := 4;
  fRowCount    := 6;
  ImageIndex := 64;
  UpdateLanguage();
end;

procedure TImageEnMViewPrintAllThumbnails.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_PrintAllThumbnails);
  Hint := iemsg(IEMsg_PrintAllImagesOfTheGridAsASheetOfThumbnails);
end;

procedure TImageEnMViewPrintAllThumbnails.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.PrintImages(fColumnCount, fRowCount, 0.5, 0.5, True)
end;
{$ENDIF}


{ TImageEnMViewSelectAcquireSource }

{$IFDEF IEINCLUDEIEXACQUIRE}
constructor TImageEnMViewSelectAcquireSource.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  fApis := [ieaTwain, ieaWIA];
  ImageIndex := 38;
  UpdateLanguage();
end;

procedure TImageEnMViewSelectAcquireSource.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SelectAcquisitionSource);
  Hint := iemsg(IEMsg_SelectTheCameraOrScannerToAcquireImagesFrom);
end;

procedure TImageEnMViewSelectAcquireSource.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.SelectAcquireSource(fApis);
end;
{$ENDIF}


{ TImageEnMViewAcquire }

{$IFDEF IEINCLUDEIEXACQUIRE}
constructor TImageEnMViewAcquire.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ImageIndex := 39;
  UpdateLanguage();
end;

procedure TImageEnMViewAcquire.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_Acquire);
  Hint := iemsg(IEMsg_RetrieveImagesFromACameraOrScanner);
end;

procedure TImageEnMViewAcquire.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMIO then
    ActiveImageEnMIO.Acquire;
end;
{$ENDIF}


{ TImageEnMViewSeekFirst }

constructor TImageEnMViewSeekFirst.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ImageIndex := 18;
  UpdateLanguage();
end;

procedure TImageEnMViewSeekFirst.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_FirstFrame);
  Hint := iemsg(IEMsg_SelectTheFirstFrame);
end;

procedure TImageEnMViewSeekFirst.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
  begin
    if ActiveImageEnMView.DisplayMode = mdSingle then
      ActiveImageEnMView.Seek(ieioSeekFirst)
    else
      ActiveImageEnMView.SelectSeek(iskFirst);
  end;
end;

procedure TImageEnMViewSeekFirst.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnMView then
    bEnabled := (ActiveImageEnMView.ImageCount > 1) and
                (CurrentImageIndex > 0);
  Enabled := bEnabled;
end;


{ TImageEnMViewSeekPrior }

constructor TImageEnMViewSeekPrior.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ImageIndex := 19;
  UpdateLanguage();
end;

procedure TImageEnMViewSeekPrior.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_PreviousFrame);
  Hint := iemsg(IEMsg_SelectThePreviousFrame);
end;

procedure TImageEnMViewSeekPrior.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
  begin
    if ActiveImageEnMView.DisplayMode = mdSingle then
      ActiveImageEnMView.Seek(ieioSeekPrior)
    else
      ActiveImageEnMView.SelectSeek(iskLeft);
  end;
end;

procedure TImageEnMViewSeekPrior.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnMView then
    bEnabled := (ActiveImageEnMView.ImageCount > 1) and
                (CurrentImageIndex > 0);
  Enabled := bEnabled;
end;


{ TImageEnMViewSeekNext }

constructor TImageEnMViewSeekNext.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ImageIndex := 20;
  UpdateLanguage();
end;

procedure TImageEnMViewSeekNext.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_NextFrame);
  Hint := iemsg(IEMsg_SelectTheNextFrame);
end;

procedure TImageEnMViewSeekNext.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
    if ActiveImageEnMView.DisplayMode = mdSingle then
      ActiveImageEnMView.Seek(ieioSeekNext)
    else
      ActiveImageEnMView.SelectSeek(iskRight);
end;

procedure TImageEnMViewSeekNext.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnMView then
    bEnabled := (ActiveImageEnMView.ImageCount > 1) and
                (CurrentImageIndex < ActiveImageEnMView.ImageCount - 1);
  Enabled := bEnabled;
end;


{ TImageEnMViewSeekLast }

constructor TImageEnMViewSeekLast.Create(AOwner: TComponent);
begin
  inherited;
  fRequireSelection := False;
  ImageIndex := 21;
  UpdateLanguage();
end;

procedure TImageEnMViewSeekLast.UpdateLanguage();
begin       
  Caption := iemsg(IEMsg_LastFrame);
  Hint := iemsg(IEMsg_SelectTheLastFrame);
end;

procedure TImageEnMViewSeekLast.ExecuteTarget(Target: TObject);
begin
  if HaveActiveImageEnMView then
  begin
    if ActiveImageEnMView.DisplayMode = mdSingle then
      ActiveImageEnMView.Seek(ieioSeekLast)
    else
      ActiveImageEnMView.SelectSeek(iskLast);
  end;
end;

procedure TImageEnMViewSeekLast.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveImageEnMView then
    bEnabled := (ActiveImageEnMView.ImageCount > 1) and
                (CurrentImageIndex < ActiveImageEnMView.ImageCount - 1);
  Enabled := bEnabled;
end;



{$ELSE} // {$IFDEF INCLUDE_MULTIVIEWACTIONS}

interface
implementation

{$ENDIF}



{!!
<FS>TImageEnMView Actions

<FN>
ImageEn includes a large set of actions for ImageEnMView, <L TImageEnView Actions>ImageEnView</L> and <L TImageEnVect Actions>ImageEnVect</L> components to allow you to rapidly develop your UI.

<FM>To use actions:<FN>
1. Add a TActionList component to your form
2. Double-click your TActionList to open it
3. Select "New Standard Action"
4. Scroll down to the ImageEnMView actions, select the ones you require and click OK
5. Select your actions and set the <FB>ImageEnMView<FN> property to your <A TImageEnMView> component
6. Assign the actions to menu items and buttons

See the demo for more information: Demos\Other\Actions_MView\MViewActions.dpr

<FM>Notes:<FN>
- Your must set the <FB>ImageEnMView<FN> property of the actions     
- You can set <A TIEImageEnGlobalSettings.MsgLanguage> to localize the actions
- See the <L Actions ImageIndex List>list of the default ImageIndexes</L> if you are planning to add graphics to your actions

<FM>General Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnMViewClear</C> <C>Clear</C> <C>Clear all images</C> <C><A TImageEnMView.Clear></C> <C> -</C> </R>
<R> <C>TImageEnMViewSelectAll</C> <C>Select All</C> <C>Select all images in the grid</C> <C><A TImageEnMView.SelectAll></C> <C> -</C> </R>
<R> <C>TImageEnMViewDeselect</C> <C>Deselect</C> <C>Clear your selection</C> <C><A TImageEnMView.DeSelect></C> <C> -</C> </R>
<R> <C>TImageEnMViewPlaying</C> <C>Animate</C> <C>Playback these frames in sequence</C> <C><A TImageEnMView.Playing></C> <C> -</C> </R>
<R> <C>TImageEnMViewPlayLoop</C> <C>Loop Playback</C> <C>Restart playback after it completes</C> <C><A TImageEnMView.PlayLoop></C> <C> -</C> </R>
<R> <C>TImageEnMViewDisplayModeSingle</C> <C>Single Frame Only</C> <C>Display only the active frame</C> <C><A TImageEnMView.DisplayMode></C> <C> -</C> </R>
<R> <C>TImageEnMViewEnableAdjustOrientation</C> <C>Auto-Rotate Display</C> <C>Automatically display images with the correct orientation</C> <C><A TImageEnMView.EnableAdjustOrientation></C> <C> -</C> </R>
</TABLE>

<FM>Proc Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnMViewDoAdjustPreviews</C> <C>Adjust Colors</C> <C>Peform color enhancement functions on the selected image</C> <C><A TImageEnProc.DoPreviews></C> <C>AutoSaveChanges, OverrideJpegQuality, <L TIEAutoSaveErrorEvent>AutoSaveError</L></C> </R>
<R> <C>TImageEnMViewDoEffectPreviews</C> <C>Image Effects</C> <C>Perform effects on the selected image</C> <C><A TImageEnProc.DoPreviews></C> <C>AutoSaveChanges, OverrideJpegQuality, <L TIEAutoSaveErrorEvent>AutoSaveError</L></C> </R>
<R> <C>TImageEnMViewPromptToResize</C> <C>Prompt to Resize</C> <C>Display the previews dialog to allow the user to resample (resize) the image</C> <C><A TImageEnProc.DoPreviews></C> <C><L TResampleFilter>ResampleFilter</L>, AutoSaveChanges, OverrideJpegQuality, <L TIEAutoSaveErrorEvent>AutoSaveError</C> </R>
<R> <C>TImageEnMViewPromptToRotate</C> <C>Prompt to Rotate</C> <C>Display the previews dialog to allow the user to rotate the image to a custom angle</C> <C><A TImageEnProc.DoPreviews></C> <C><L TImageEnProc.Rotate>AntiAliasMode</L>, AutoSaveChanges, OverrideJpegQuality, <L TIEAutoSaveErrorEvent>AutoSaveError</C> </R>
<R> <C>TImageEnMViewFlipHorizontal</C> <C>Flip Horizontal</C> <C>Flip the selected image from top to bottom</C> <C><A TImageEnProc.Flip></C> <C>AutoSaveChanges, <L TIEAutoSaveErrorEvent>AutoSaveError</L></C> </R>
<R> <C>TImageEnMViewFlipVertical</C> <C>Flip Vertical</C> <C>Flip the selected image from left to right</C> <C><A TImageEnProc.Flip></C> <C>AutoSaveChanges, <L TIEAutoSaveErrorEvent>AutoSaveError</L></C> </R>
<R> <C>TImageEnMViewRotateRight</C> <C>Rotate Right</C> <C>Rotate the selected image 90° clockwise</C> <C><A TImageEnProc.Rotate></C> <C>AutoSaveChanges, <L TIEAutoSaveErrorEvent>AutoSaveError</L></C> </R>
<R> <C>TImageEnMViewRotate180</C> <C>Rotate 180°</C> <C>Rotate the selected image 180° clockwise</C> <C><A TImageEnProc.Rotate></C> <C>AutoSaveChanges, <L TIEAutoSaveErrorEvent>AutoSaveError</L></C> </R>
<R> <C>TImageEnMViewRotateLeft</C> <C>Rotate Left</C> <C>Rotate the selected image 90° counter-clockwise</C> <C><A TImageEnProc.Rotate></C> <C>AutoSaveChanges, <L TIEAutoSaveErrorEvent>AutoSaveError</L></C> </R>
<R> <C>TImageEnMViewCutToClipboard</C> <C>Cut</C> <C>Copy the current image to the clipboard and remove it</C> <C><A TImageEnProc.CopyToClipboard></C> <C> -</C> </R>
<R> <C>TImageEnMViewCopyToClipboard</C> <C>Copy</C> <C>Copy the current image to the clipboard</C> <C><A TImageEnProc.CopyToClipboard></C> <C> -</C> </R>
<R> <C>TImageEnMViewPasteFromClipboard</C> <C>Paste</C> <C>Paste an image from the clipboard</C> <C><A TImageEnProc.PasteFromClipboard></C> <C> -</C> </R>
</TABLE>
Note: if <FB>AutoSaveChanges<FN> is true then any editing actions applied to the thumbnail will be saved to the underlying file (<FB>without prompting<FN>). For rotation and flipping of JPEGs <L Lossless Jpeg Transformations>lossless functions</L> will be used where possible, otherwise JPEG's will be saved with the JPEG_Quality specified in the item's <A TImageEnMIO.Params> or can be overriden with <FC>OverrideJpegQuality<FN>. If an error is encountered an <L TIEAutoSaveErrorEvent>AutoSaveError event</L> will occur.

<FM>IO Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TImageEnMViewPromptToOpen</C> <C>Open</C> <C>Load an image from file</C> <C><A TImageEnMIO.ExecuteOpenDialog>, <A TImageEnMIO.LoadFromFile></C> <C>MultiSelect, DialogTitle, DefaultFilter, LimitToFileType</C> </R>
<R> <C>TImageEnMViewPromptToAdd</C> <C>Add from File</C> <C>Add an image to the grid from file</C> <C><A TImageEnMIO.ExecuteOpenDialog>, <A TImageEnMView.AppendImage></C> <C> -</C> </R>
<R> <C>TImageEnMViewSave</C> <C>Save</C> <C>Save changes to this image to file</C> <C><A TImageEnMIO.SaveToFile></C> <C> -</C> </R>
<R> <C>TImageEnMViewPromptToSave</C> <C>Save as</C> <C>Save this image to a new filename</C> <C><A TImageEnMIO.ExecuteSaveDialog>, <A TImageEnMIO.SaveToFile></C> <C>DialogTitle, DefaultFilter, LimitToFileType</C> </R>
<R> <C>TImageEnMViewDoIOPreviews</C> <C>Save Properties</C> <C>Specify advanced properties for all images</C> <C><A TImageEnMIO.DoPreviews></C> <C><L TPreviewParams>PreviewParams</L></C> </R>
<R> <C>TImageEnMViewDoIOPreviewsSelected</C> <C>Image Save Properties</C> <C>Specify advanced properties for the selected image</C> <C><A TImageEnMIO.DoPreviews></C> <C><L TPreviewParams>PreviewParams</L></C> </R>
<R> <C>TImageEnMViewDoPrintPreviewDialog</C> <C>Print Preview</C> <C>Display a preview of your printing</C> <C><A TImageEnMIO.DoPrintPreviewDialog></C> <C> -</C> </R>
<R> <C>TImageEnMViewPrintImageNormal</C> <C>Print</C> <C>Print the selected image at its original size</C> <C><A TImageEnMIO.PrintImage></C> <C> -</C> </R>
<R> <C>TImageEnMViewPrintImageFitToPage</C> <C>Print to Page</C> <C>Print the selected image to fit the page</C> <C><A TImageEnMIO.PrintImage></C> <C> -</C> </R>
<R> <C>TImageEnMViewPrintSelectedThumbnails</C> <C>Print Thumbnails</C> <C>Print the selected images as a sheet of thumbnails</C> <C><A TImageEnMIO.PrintImages></C> <C><L TImageEnMIO.PrintImages>ColumnCount</L>, <L TImageEnMIO.PrintImages>RowCount</L></C> </R>
<R> <C>TImageEnMViewPrintAllThumbnails</C> <C>Print All Thumbnails</C> <C>Print all images of the grid as a sheet of thumbnails</C> <C><A TImageEnMIO.PrintImages></C> <C><L TImageEnMIO.PrintImages>ColumnCount</L>, <L TImageEnMIO.PrintImages>RowCount</L></C> </R>
<R> <C>TImageEnMViewSelectAcquireSource</C> <C>Select Acquisition Source</C> <C>Select the camera or scanner to acquire images from</C> <C><A TImageEnMIO.SelectAcquireSource></C> <C><L TIEAcquireApis>Apis</L></C> </R>
<R> <C>TImageEnMViewAcquire</C> <C>Acquire</C> <C>Retrieve images from a camera or scanner</C> <C><A TImageEnMIO.Acquire></C> <C> -</C> </R>
<R> <C>TImageEnMViewSeekFirst</C> <C>First Frame</C> <C>Select the first frame</C> <C><A TImageEnMView.Seek>, <A TImageEnMView.SelectSeek></C> <C> -</C> </R>
<R> <C>TImageEnMViewSeekPrior</C> <C>Previous Frame</C> <C>Select the previous frame</C> <C><A TImageEnMView.Seek>, <A TImageEnMView.SelectSeek></C> <C> -</C> </R>
<R> <C>TImageEnMViewSeekNext</C> <C>Next Frame</C> <C>Select the next frame</C> <C><A TImageEnMView.Seek>, <A TImageEnMView.SelectSeek></C> <C> -</C> </R>
<R> <C>TImageEnMViewSeekLast</C> <C>Last Frame</C> <C>Select the last frame</C> <C><A TImageEnMView.Seek>, <A TImageEnMView.SelectSeek></C> <C> -</C> </R>
</TABLE>
!!}


end.


