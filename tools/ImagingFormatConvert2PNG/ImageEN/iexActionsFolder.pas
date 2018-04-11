{------------------------------------------------------------------------------}
{                                                                              }
{  TActions for common TImageEnFolderMView functions                           }
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
File version 1001
*)

unit iexActionsFolder;

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
  ActnList, Classes, ieMIO, imageenproc, hyiedefs, iemview, iexFolderMView, imageenio;

Type

  TIEFolderMViewAction = class(TAction)
  private
  protected
    function ActiveIEFolderMView : TImageEnFolderMView;
    function HaveActiveIEFolderMView : boolean;
    function HaveSelection : boolean;                
  public
    fIEFolderMView : TImageEnFolderMView;
    fRequireSelection : Boolean;
    fRequireImages : Boolean;
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; Override;
    procedure UpdateTarget(Target: TObject); override;      
    function BaseEnabled : Boolean;
  published       
    property ImageEnFolderMView: TImageEnFolderMView read fIEFolderMView write fIEFolderMView;
  end;

  TIEFolderMViewPromptForFolder = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;     
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;       

  TIEFolderMViewCreateNewFolder = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewRefreshFileList = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewExecuteSelectedFile = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewOpenParentFolder = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;
  
  TIEFolderMViewMoveSelectedFilesToFolder = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private    
    fLastMoveDirectory : string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewCopySelectedFilesToFolder = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
    fLastCopyDirectory : string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewDeleteSelectedFilesFromFolder = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewCopySelectedFilesToClipboard = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewCutSelectedFilesToClipboard = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;

  TIEFolderMViewPasteFilesFromClipboard = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure UpdateLanguage();
  published
  end;       

  TIEFolderMViewRenameSelectedFile = class(TIEFolderMViewAction, IIELanguageUpdatable)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;  
    procedure UpdateLanguage();
  published
  end;




implementation

uses
  Forms, hyieutils, SysUtils, iewords, iesettings, iexWindowsFunctions, 
  dialogs;

const
  CTRL_C_SHORTCUT       = 16451;
  CTRL_V_SHORTCUT       = 16470;
  CTRL_X_SHORTCUT       = 16472;
  CTRL_DELETE_SHORTCUT  = 16430;
  F5_SHORTCUT           = 116  ;
  F2_ShortCut           = 113  ;


constructor  TIEFolderMViewAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRequireImages := True;
  fRequireSelection := True;
  Caption := 'ImageEn Action';
  DisableIfNoHandler := false;
end;                       


function  TIEFolderMViewAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;


function TIEFolderMViewAction.HaveActiveIEFolderMView: boolean;
begin
  Result := ActiveIEFolderMView <> nil;
end;

function TIEFolderMViewAction.HaveSelection: boolean;
begin
  Result := HaveActiveIEFolderMView and
            (ActiveIEFolderMView.SelectedFilename <> '');
end;


function TIEFolderMViewAction.BaseEnabled : boolean;
begin
  Result := HaveActiveIEFolderMView and (ActiveIEFolderMView.Folder <> '');
  if Result and fRequireImages then
  begin
    Result := ActiveIEFolderMView.ImageCount > 0;

    // fRequireSelection assumes fRequireImages
    if Result and fRequireSelection then
      Result := HaveSelection;
  end;
end;


procedure TIEFolderMViewAction.UpdateTarget(Target: TObject);
begin
  Enabled := BaseEnabled;
end;

                       
// Return the speciifed or selected ImageEnMView
function TIEFolderMViewAction.ActiveIEFolderMView: TImageEnFolderMView;
begin
  Result := fIEFolderMView;
  if (Result = nil) and Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TImageEnFolderMView) then
    Result := TImageEnFolderMView(Screen.ActiveControl);
end;

{ TIEFolderMViewRefreshFileList }

constructor TIEFolderMViewRefreshFileList.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False; 
  Shortcut := F5_SHORTCUT;
  ImageIndex := 99;
  UpdateLanguage();
end;

procedure TIEFolderMViewRefreshFileList.UpdateLanguage();
begin      
  Caption := iemsg(IEMsg_Refresh);
  Hint := iemsg(IEMsg_RefreshTheFileListing);
end;

procedure TIEFolderMViewRefreshFileList.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView then
    ActiveIEFolderMView.RefreshFileList;
end;


{ TIEFolderMViewExecuteSelectedFile  }

constructor TIEFolderMViewExecuteSelectedFile.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 100;
  UpdateLanguage();
end;

procedure TIEFolderMViewExecuteSelectedFile.UpdateLanguage();
begin   
  Caption := iemsg(IEMsg_OpenFile);
  Hint := iemsg(IEMsg_OpenTheSelectedFileInTheDefaultViewer);
end;

procedure TIEFolderMViewExecuteSelectedFile.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView then
    ActiveIEFolderMView.ExecuteFile(IEF_CURRENT_FILE);
end;


{ TIEFolderMViewMoveSelectedFilesToFolder }

constructor TIEFolderMViewMoveSelectedFilesToFolder.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 102;
  UpdateLanguage();
end;

procedure TIEFolderMViewMoveSelectedFilesToFolder.UpdateLanguage();
begin    
  Caption := iemsg(IEMsg_MoveFiles);
  Hint := iemsg(IEMsg_MoveTheSelectedFilesToANewFolder);
end;

procedure TIEFolderMViewMoveSelectedFilesToFolder.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView = False then
    exit;

  if fLastMoveDirectory = '' then
    fLastMoveDirectory := ActiveIEFolderMView.Folder;
  if WindowsSelectDirectory(iemsg(IEMsg_WhereDoYouWantToMoveTheseFiles), fLastMoveDirectory, ActiveIEFolderMView) then
    ActiveIEFolderMView.MoveSelectedFilesToFolder(fLastMoveDirectory);
end;


{ TIEFolderMViewOpenParentFolder }

constructor TIEFolderMViewOpenParentFolder.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ImageIndex := 101;
  UpdateLanguage();
end;

procedure TIEFolderMViewOpenParentFolder.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_GoUp);
  Hint := iemsg(IEMsg_OpenTheParentOfTheCurrentFolder);
end;

procedure TIEFolderMViewOpenParentFolder.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView then
    ActiveIEFolderMView.OpenParentFolder;
end;

procedure TIEFolderMViewOpenParentFolder.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if bEnabled and HaveActiveIEFolderMView then
    bEnabled := ActiveIEFolderMView.CanOpenParentFolder;
  Enabled := bEnabled;
end;


{ TIEFolderMViewCopySelectedFilesToFolder }

constructor TIEFolderMViewCopySelectedFilesToFolder.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 103;
  UpdateLanguage();
end;

procedure TIEFolderMViewCopySelectedFilesToFolder.UpdateLanguage();
begin    
  Caption := iemsg(IEMsg_CopyFiles);
  Hint := iemsg(IEMsg_CopyTheSelectedFilesToANewFolder);
end;

procedure TIEFolderMViewCopySelectedFilesToFolder.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView = False then
    exit;

  if fLastCopyDirectory = '' then
    fLastCopyDirectory := ActiveIEFolderMView.Folder;
  if WindowsSelectDirectory(iemsg(IEMsg_WhereDoYouWantToCopyTheseFiles), fLastCopyDirectory, ActiveIEFolderMView) then
    ActiveIEFolderMView.CopySelectedFilesToFolder(fLastCopyDirectory);
end;


{ TIEFolderMViewDeleteSelectedFilesFromFolder }

constructor TIEFolderMViewDeleteSelectedFilesFromFolder.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 104;  
  Shortcut := CTRL_DELETE_SHORTCUT;
  UpdateLanguage();
end;

procedure TIEFolderMViewDeleteSelectedFilesFromFolder.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_DeleteFiles);
  Hint := iemsg(IEMsg_DeleteTheSelectedFilesFromTheFolder);
end;

procedure TIEFolderMViewDeleteSelectedFilesFromFolder.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView then
    ActiveIEFolderMView.DeleteSelectedFilesFromFolder;
end;



{ TIEFolderMViewCopySelectedFilesToClipboard }

constructor TIEFolderMViewCopySelectedFilesToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 105;
  Shortcut := Ctrl_C_SHORTCUT;
  UpdateLanguage();
end;

procedure TIEFolderMViewCopySelectedFilesToClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CopyFilesToClipboard);
  Hint := iemsg(IEMsg_CopyTheSelectedFilesToTheClipboard);
end;

procedure TIEFolderMViewCopySelectedFilesToClipboard.ExecuteTarget(Target: TObject);
begin
  try
    if HaveActiveIEFolderMView then
      ActiveIEFolderMView.CopySelectedFilesToClipboard;
  except
    MessageDlg('Unable to copy your selection to the clipboard.', mtError, [mbOK], 0);
  end;
end;




{ TIEFolderMViewCutSelectedFilesToClipboard }

constructor TIEFolderMViewCutSelectedFilesToClipboard.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 105;
  Shortcut := Ctrl_X_SHORTCUT;
  UpdateLanguage();
end;

procedure TIEFolderMViewCutSelectedFilesToClipboard.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_CutFilesToClipboard);
  Hint := iemsg(IEMsg_CutTheSelectedFilesToTheClipboard);
end;

procedure TIEFolderMViewCutSelectedFilesToClipboard.ExecuteTarget(Target: TObject);
begin
  try
    if HaveActiveIEFolderMView then
      ActiveIEFolderMView.CutSelectedFilesToClipboard;
  except
    MessageDlg('Unable to cut your selection to the clipboard.', mtError, [mbOK], 0);
  end;
end;



{ TIEFolderMViewPasteFilesFromClipboard }

constructor TIEFolderMViewPasteFilesFromClipboard.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;        
  ImageIndex := 106;
  Shortcut := Ctrl_V_SHORTCUT;
  UpdateLanguage();
end;

procedure TIEFolderMViewPasteFilesFromClipboard.UpdateLanguage();
begin                   
  Caption := iemsg(IEMsg_PasteFilesFromClipboard);
  Hint := iemsg(IEMsg_PasteFilesFromTheClipboardToThisFolder);
end;

procedure TIEFolderMViewPasteFilesFromClipboard.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView then
    ActiveIEFolderMView.PasteFilesFromClipboard;
end;

procedure TIEFolderMViewPasteFilesFromClipboard.UpdateTarget(Target: TObject);
var
  bEnabled: boolean;
begin
  bEnabled := BaseEnabled;
  if HaveActiveIEFolderMView then
    bEnabled :=  ActiveIEFolderMView.CanPasteFilesFromClipboard;
  Enabled := bEnabled;
end;



{ TIEFolderMViewRenameSelectedFile  }

constructor TIEFolderMViewRenameSelectedFile.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 107;
  UpdateLanguage();
end;

procedure TIEFolderMViewRenameSelectedFile.UpdateLanguage();
begin   
  Caption := iemsg(IEMsg_RenameFile) + '...';
  Hint := iemsg(IEMsg_SpecifyANewNameForTheSelectedFile);
end;

procedure TIEFolderMViewRenameSelectedFile.ExecuteTarget(Target: TObject);
var
  sNewName: string;
begin                     
  if HaveActiveIEFolderMView = False then
    exit;

  sNewName := InputBox(iemsg(IEMsg_RenameFile), Format(iemsg(IEMsg_SpecifyANewNameForX), [ExtractFilename(ActiveIEFolderMView.SelectedFilename)]), '');
  sNewName := Trim(sNewName);
  if sNewName = '' then
    exit;

  if ExtractFileExt(sNewName) = '' then
    sNewName := sNewName + ExtractFileExt(ActiveIEFolderMView.SelectedFilename);

  ActiveIEFolderMView.RenameFile(IEF_CURRENT_FILE, sNewName);
end;



{ TIEFolderMViewPromptForFolder }

constructor TIEFolderMViewPromptForFolder.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := 108;
  UpdateLanguage();
end;

procedure TIEFolderMViewPromptForFolder.UpdateLanguage();
begin
  Caption := iemsg(IEMsg_SelectFolder);
  Hint := iemsg(IEMsg_SelectAFolderToOpen);
end;

procedure TIEFolderMViewPromptForFolder.ExecuteTarget(Target: TObject);
begin
  if HaveActiveIEFolderMView then
    ActiveIEFolderMView.PromptForFolder;
end;

procedure TIEFolderMViewPromptForFolder.UpdateTarget(Target: TObject);
begin
  Enabled := HaveActiveIEFolderMView;
end;



{ TIEFolderMViewCreateNewFolder }

constructor TIEFolderMViewCreateNewFolder.Create(AOwner: TComponent);
begin
  inherited;
  fRequireImages := False;
  ImageIndex := 109;
  UpdateLanguage();
end;
                                  
procedure TIEFolderMViewCreateNewFolder.UpdateLanguage();
begin
  Caption := iemsg(IEMSG_CreateFolder) + '...';
  Hint := iemsg(IEMSG_CreateANewFolderAtTheCurrentLocation);
end;

procedure TIEFolderMViewCreateNewFolder.ExecuteTarget(Target: TObject);
var
  sNewName: string;
begin                     
  if (HaveActiveIEFolderMView = False) or (ActiveIEFolderMView.Folder = '') then
    exit;

  sNewName := InputBox(iemsg(IEMSG_CreateFolder), iemsg(IEMsg_SpecifyTheNameOfYourNewFolder), '');  
  sNewName := Trim(sNewName);
  if sNewName = '' then
    exit;

  ActiveIEFolderMView.CreateNewFolder(sNewName);
end;



{$ELSE} // {$IFDEF INCLUDE_MULTIVIEWACTIONS}

interface
implementation

{$ENDIF}


{!!
<FS>TImageEnFolderMView Actions

<FN>
When using a <A TImageEnFolderMView> you can use both the TImageEnFolderMView specific actions which follow, as well as all the actions of <L TImageEnMView Actions>TImageEnMView</L> to rapidly develop your UI.

<FM>To use actions:<FN>
1. Add a TActionList component to your form
2. Double-click your TActionList to open it
3. Select "New Standard Action"
4. Scroll down to the ImageEnMView actions, select the ones you require and click OK
5. Select your actions and set the <FB>IEFolderMView<FN> property to your <A TImageEnFolderMView> component
6. Assign the actions to menu items and buttons

See the demo for more information: Demos\Multi\FolderMView\FolderMView.dpr

<FM>Notes:<FN>
- Your must set the <FB>ImageEnMView<FN> property of the actions     
- You can set <A TIEImageEnGlobalSettings.MsgLanguage> to localize the actions
- See the <L Actions ImageIndex List>list of the default ImageIndexes</L> if you are planning to add graphics to your actions

<FM>Folder Actions<FN>
<TABLE>
<R> <H>Action</H> <H>Name</H> <H>Description</H> <H>Associated Method</H> <H>Extra Properties</H></R>
<R> <C>TIEFolderMViewPromptForFolder</C> <C>Select Folder</C> <C>Select a folder to open</C> <C><A TImageEnFolderMView.PromptForFolder></C> <C> -</C> </R>
<R> <C>TIEFolderMViewOpenParentFolder</C> <C>Go Up</C> <C>Open the parent of the current folder</C> <C><A TImageEnFolderMView.OpenParentFolder></C> <C> -</C> </R>
<R> <C>TIEFolderMViewRefreshFileList</C> <C>Refresh</C> <C>Refresh the file listing</C> <C><A TImageEnFolderMView.RefreshFileList></C> <C> -</C> </R>
<R> <C>TIEFolderMViewCreateNewFolder</C> <C>Create Folder</C> <C>Create a new folder at the current location</C> <C><A TImageEnFolderMView.CreateNewFolder></C> <C> -</C> </R>
<R> <C>TIEFolderMViewExecuteSelectedFile</C> <C>Open File</C> <C>Open the selected file in the default viewer</C> <C><A TImageEnFolderMView.ExecuteFile></C> <C> -</C> </R>
<R> <C>TIEFolderMViewMoveSelectedFilesToFolder</C> <C>Move Files</C> <C>Move the selected files to a new folder</C> <C><A TImageEnFolderMView.MoveSelectedFilesToFolder></C> <C> -</C> </R>
<R> <C>TIEFolderMViewCopySelectedFilesToFolder</C> <C>Copy Files</C> <C>Copy the selected files to a new folder</C> <C><A TImageEnFolderMView.CopySelectedFilesToFolder></C> <C> -</C> </R>
<R> <C>TIEFolderMViewDeleteSelectedFilesFromFolder</C> <C>Delete Files</C> <C>Delete the selected files from the folder</C> <C><A TImageEnFolderMView.DeleteSelectedFilesFromFolder></C> <C> -</C> </R>
<R> <C>TIEFolderMViewCopySelectedFilesToClipboard</C> <C>Copy Files to Clipboard</C> <C>Copy the selected files to the clipboard</C> <C><A TImageEnFolderMView.CopySelectedFilesToClipboard></C> <C> -</C> </R>
<R> <C>TIEFolderMViewPasteFilesFromClipboard</C> <C>Paste Files from Clipboard</C> <C>Paste files from the clipboard to this folder</C> <C><A TImageEnFolderMView.PasteFilesFromClipboard></C> <C> -</C> </R>
<R> <C>TIEFolderMViewRenameSelectedFile</C> <C>Rename File</C> <C>Specify a new name for the selected file</C> <C><A TImageEnFolderMView.RenameFile></C> <C> -</C> </R>
<R> <C>TIEFolderMViewPromptForFolder</C> <C>Select Folder</C> <C>Select a folder to open</C> <C><A TImageEnFolderMView.PromptForFolder></C> <C> -</C> </R>
<R> <C>TIEFolderMViewCreateNewFolder</C> <C>Create Folder</C> <C>Create a new folder at the current location</C> <C><A TImageEnFolderMView.CreateNewFolder></C> <C> -</C> </R>
</TABLE>

!!}

end.


