{------------------------------------------------------------------------------}
{                                                                              }
{  Windows File Functions                                                      }
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
File version 1002
*)

unit iexWindowsFunctions;


{$I ie.inc}


interface

uses
  Windows, Forms, ActiveX, ComObj, Controls, Classes, hyiedefs, Messages;

type
{!!
<FS>TIEFileDropEvent

<FM>Declaration<FC>
}
  TIEFileDropEvent = procedure(Sender: TObject; ssFiles : TStrings; dwEffect: Integer) of object;
{!!}


{!!
<FS>TIEFileDragDrop

<FM>Declaration<FC>
TIEFileDragDrop = class(TInterfacedObject, IDropTarget, IDropSource)
public
  property ActivateDropping : Boolean;
  property DropActions : <A TIEFileDragDropActions> ;
  constructor Create(Control: TWinControl; OnFileDrop : <A TIEFileDropEvent>);
  destructor Destroy; override;
  procedure InitiateDragging(ssFilenames : TStrings; DragActions: <A TIEFileDragDropActions>);
end;

<FM>Description<FN>
Class to provide support for dragging and dropping files from/to Windows Explorer (and other applications). Used by <A TImageEnFolderMView>.

See iexFolderMView.pas for example usage.
!!}        
  // CLASS TO HANDLE DRAGGING/DROPPING FILES TO/FROM WINDOWS EXPLORER
  TIEFileDragDrop = class(TInterfacedObject, IDropTarget, IDropSource)
  private
    fControl : TWinControl;
    fControlHandle : HWND;
    fDroppingActive : Boolean;
    fOnFileDrop : TIEFileDropEvent;
    fDropActions : TIEFileDragDropActions;

    procedure GetFileNames(const dataObj: IDataObject; Dest: TStrings);
    function GetDataObject(ssFiles: TStrings): IDataObject;
    function GetDragEffect(KeyState : Integer) : Integer;
    procedure SetDroppingActive(const Value: Boolean);

    // Supporting IDropTarget
    function DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;

    // Supporting IDropSource
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  public
    property ActivateDropping : Boolean read fDroppingActive write SetDroppingActive;
    property DropActions : TIEFileDragDropActions read fDropActions write fDropActions;
    constructor Create(Control: TWinControl; OnFileDrop : TIEFileDropEvent);
    destructor Destroy; override;
    procedure InitiateDragging(ssFilenames : TStrings; DragActions: TIEFileDragDropActions);
  end;



// CLASS TO MONITOR CHANGES TO A FOLDER
// Based on Cromis.DirectoryWatch
// http://www.cromis.net/blog/downloads/
// Created by Iztok Kacin, Cromis (iztok.kacin@gmail.com).
// Used by permission (10-7-13)
                          
const
  cShutdownTimeout = 3000;
  cFileWaitTimeout = 0;

type

{!!
<FS>TWatchOptions

<FM>Declaration<FC>
}
  // the filters that control when the watch is triggered
  TWatchOption = (woFileName, woFolderName, woAttributes, woSize, woLastWrite, woLastAccess, woCreation, woSecurity);
  TWatchOptions = set of TWatchOption;
{!!}

       

{!!
<FS>TWatchActions

<FM>Declaration<FC>
}
  // the filters that control when the watch is triggered
  // the actions that are the result of the watch being triggered
  TWatchAction = (waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew);
  TWatchActions = set of TWatchAction;
{!!}
                          

{!!
<FS>TFileChangeNotifyEvent

<FM>Declaration<FC>
}
  TFileChangeNotifyEvent = procedure(const Sender: TObject;
                                     const Action: TWatchAction;
                                     const FileName: string
                                     ) of Object; 
{!!}
                                     

{!!
<FS>TOnError

<FM>Declaration<FC>
}
  TOnError = procedure(const Sender: TObject;
                       const ErrorCode: Integer;
                       const ErrorMessage: string
                       ) of Object;    
{!!}


{!!
<FS>TIEFolderWatch

<FM>Declaration<FC>
  TIEFolderWatch = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function Running: Boolean;
    property WatchSubTree: Boolean;
    property WatchOptions: <A TWatchOptions>;
    property WatchActions: <A TWatchActions>;
    property BufferSize: Integer;
    property Path: string;
    property OnNotify: <A TFileChangeNotifyEvent>;
    property OnChange: TNotifyEvent;
    property OnError: <A TOnError>;
  end;


<FM>Description<FN>
Class that automatically notifies changes to files within a specified folder. Used by <A TImageEnFolderMView>.

See iexFolderMView.pas for example usage.


<FM>Attribution<FC>
Based on Cromis.DirectoryWatch by Iztok Kacin, Cromis (iztok.kacin@gmail.com).

<L http://www.cromis.net/blog/downloads/>www.cromis.net</L>
!!}
  TIEFolderWatch = class
  private
    FWatchOptions : TWatchOptions;
    FWatchActions : TWatchActions;
    FWatchSubTree : Boolean;
    FWatchThread  : TThread;
    FBufferSize   : Integer;
    FWndHandle    : HWND;
    FPath         : string;
    FAbortEvent   : THandle;
    FOnError      : TOnError;
    FOnChange     : TNotifyEvent;
    FOnNotify     : TFileChangeNotifyEvent;
    procedure WatchWndProc(var Msg: TMessage);
    procedure SetPath(const Value: string);
    procedure SetWatchOptions(const Value: TWatchOptions);
    procedure SetWatchActions(const Value: TWatchActions);
    procedure SetWatchSubTree(const Value: Boolean);
    procedure DeallocateHWnd(Wnd: HWND);
    function MakeFilter: Integer;
  protected
    procedure Change; virtual;
    procedure AllocWatchThread;
    procedure ReleaseWatchThread;
    procedure RestartWatchThread;
    procedure Notify(const Action: Integer;
                     const FileName: string
                     ); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function Running: Boolean;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
    property WatchOptions: TWatchOptions read FWatchOptions write SetWatchOptions;
    property WatchActions: TWatchActions read FWatchActions write SetWatchActions;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property Path: string read FPath write SetPath;
    // notification properties. Notify about internal and external changes
    property OnNotify: TFileChangeNotifyEvent read FOnNotify write FOnNotify;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnError: TOnError read FOnError write FOnError;
  end;

  // waits for the file to be ready (it is not in use anymore) or timeout occurs
  procedure WaitForFileReady(const FileName: string; const Timeout: Cardinal = cFileWaitTimeout);



  // Clipboard                           
  function CutFilesToClipboard(Handle : HWND; Filenames: TStrings): Boolean;
  function CopyFilesToClipboard(Handle : HWND; Filenames: TStrings): Boolean;
  function PasteFilesFromClipboard(Handle : HWND; ssFilenames: TStrings; out bMoveFiles : Boolean) : Boolean;
  function CanPasteFilesFromClipboard(Handle : HWND) : Boolean;
                                          
  procedure PopupSystemMenu(Handle: HWND; ssFileList: TStrings; x, y: integer);

  // Similar to FileCtrl.SelectDirectory
  function WindowsSelectDirectory(const Caption: string; var Directory: string;
                                  Parent: TWinControl = nil; bAllowNewFolder : Boolean = True): Boolean;

  function WindowsLocalAppDataFolder : string;
  function WindowsMyDocumentsFolder  : string;
  function WindowsMyPicturesFolder   : string;
  function WindowsMyMusicFolder      : string;
  function WindowsMyVideosFolder     : string;
  function WindowsDesktopFolder      : string;
  function WindowsProgramFilesFolder : String;

  procedure WindowsAddToRecentDocs(const Filename: string);


  {To allow multiple source files, replace spaces with #0}
  function WindowsCopy(Handle : HWnd; sFromFilename, sDestFolder: String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;
  function WindowsMove(Handle : HWnd; sFromFilename, sDestFolder: String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;
  function WindowsErase(Handle : HWnd; sFilename: String; bSendToRecycleBin, bShowConfirmation, bShowProgress: Boolean; bVerboseErrors : boolean = True): Boolean; overload;

  function WindowsCopy(Handle : HWnd; ssFiles : TStrings; const sDestFolder : String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;
  function WindowsMove(Handle : HWnd; ssFiles : TStrings; const sDestFolder : String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;
  function WindowsErase(Handle : HWnd; ssFiles : TStrings; bSendToRecycleBin, bShowConfirmation, bShowProgress: Boolean; bVerboseErrors : boolean = True): Boolean; overload;

  // sNewName must be a name only without a file path
  function WindowsRename(Handle : HWnd; const sFilename, sNewName: String;  bRenameOnCollision, bShowConfirmation : Boolean; bVerboseErrors : boolean = True; bCheckForOverwrite : Boolean = True): Boolean; 

  procedure WindowsLaunchFile(Handle: THandle; const sFilename: string);

  // CSIDL_... definitions in ShlObj
  // return a path such as CSIDL_LOCAL_APPDATA or CSIDL_BITBUCKET
  // fpMypictures = CSIDL_MYPICTURES
  // fpMyMusic = CSIDL_COMMON_MUSIC
  // Non-local app data CSIDL_APPDATA
  function GetWindowsSpecialFolder(const iCSIDL: Integer): string;

  {$IFNDEF Delphi6orNewer}
  function DirectoryExists(const Directory: string): Boolean;
  {$ENDIF}

implementation

uses
  {$IFNDEF Delphi6orNewer} FileCtrl, {$ENDIF}
  Sysutils,  ShlObj, shellapi, Dialogs, hyieutils, consts;


{$ifdef IESUPPORTDEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$endif}


{$IFNDEF Delphi2007orNewer}
const
  CSIDL_PROGRAM_FILES                 = $0026; { C:\Program Files }
  CSIDL_LOCAL_APPDATA                 = $001c; { <user name>\Local Settings\Application Data (non roaming) }
  CSIDL_MYPICTURES                    = $0027; { C:\Program Files\My Pictures }
  CSIDL_MYDOCUMENTS                   = $000c; { logical "My Documents" desktop icon }
  CSIDL_MYMUSIC                       = $000d; { "My Music" folder }
  CSIDL_MYVIDEO                       = $000e; { "My Video" folder }
  BIF_NONEWFOLDERBUTTON               = $00000200;
  BIF_NEWDIALOGSTYLE                  = $0040;
  BIF_SHAREABLE                       = $8000;

function DirectoryExists(const Directory: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}
          
type
  // Required by _PopupSystemMenu
  IShellCommandVerb = interface
    ['{7D2A7245-2376-4D33-8008-A130935A2E8B}']
    procedure ExecuteCommand(Verb: string; var Handled: boolean);
    procedure CommandCompleted(Verb: string; Succeeded: boolean);
  end;

  PArrayOfPItemIDList = ^TArrayOfPItemIDList;
  TArrayOfPItemIDList = array[0..0] of PItemIDList;

    
{!!
<FS>WindowsLaunchFile

<FM>Declaration<FC>
procedure WindowsLaunchFile(Handle: THandle; const sFilename: string);


<FM>Description<FN>
Calls ShellExecute to launch the specified file in the default application

<FM>Example<FC>
WindowsLaunchFile(Form1.Handle, IEFolderMView1.SelectedFilename);

!!}
procedure WindowsLaunchFile(Handle: THandle; const sFilename: string);
var
  ret: integer;
begin
  ret := ShellExecute(Handle, nil, PChar(sFileName), nil,  nil, SW_SHOWNORMAL);
  if ret <= 32 then
    raise Exception.create(SysErrorMessage(GetLastError));
end;


{!!
<FS>WindowsProgramFilesFolder

<FM>Declaration<FC>
function WindowsProgramFilesFolder : String;


<FM>Description<FN>
Returns the full path to "Program Files" (using <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx>CSIDL_PROGRAM_FILES</L>)

<FM>Example<FC>
IEFolderMView1.Folder := WindowsProgramFilesFolder;

!!}
function WindowsProgramFilesFolder : String;
begin
  Result := GetWindowsSpecialFolder(CSIDL_PROGRAM_FILES);
end;

{!!
<FS>GetWindowsSpecialFolder

<FM>Declaration<FC>
function GetWindowsSpecialFolder(const iCSIDL: Integer): string;


<FM>Description<FN>
Calls <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762204%28v=vs.85%29.aspx>SHGetSpecialFolderPath</L> to return a Windows special folder.

<FM>Example<FC>
// Display the contents of the Recycle Bin
IEFolderMView1.Folder := GetWindowsSpecialFolder(CSIDL_BITBUCKET);

!!}
// return a path such as CSIDL_LOCAL_APPDATA or CSIDL_BITBUCKET
function GetWindowsSpecialFolder(const iCSIDL: Integer): string;
var
  Path: array[0..1023] of Char;
begin
  Result := '';
  try
    if SHGetSpecialFolderPath(0, Path, iCSIDL, False) then
      Result := Path;
  except
    { ERROR }
  end;
end;

{!!
<FS>WindowsAddToRecentDocs

<FM>Declaration<FC>
procedure WindowsAddToRecentDocs(Const Filename: string);


<FM>Description<FN>
Adds a file to Windows' list of recent documents (displayed under the Start menu)

<FM>Example<FC>
WindowsAddToRecentDocs('C:\My File.jpg');

!!}
procedure WindowsAddToRecentDocs(Const Filename: string);
begin
  SHAddToRecentDocs(SHARD_PATH, @Filename[1]);
end;


function ShGetKnownFolderPath(const rfid: TGUID; dwFlags: DWord; hToken: THandle; out ppszPath: PWideChar): HResult;
type
  TShGetKnownFolderPath = function(const rfid: TGUID; dwFlags: DWord; hToken: THandle; out ppszPath: PWideChar): HResult; stdcall;
var
  Shell: HModule;
  Fn: TShGetKnownFolderPath;
begin
  Shell := LoadLibrary('shell32.dll');
  Win32Check(Shell <> 0);
  try
    @Fn := GetProcAddress(Shell, 'SHGetKnownFolderPath');
    Win32Check(Assigned(Fn));
    Result := Fn(rfid, dwFlags, hToken, ppszPath);
  finally
    FreeLibrary(Shell);
  end;
end;

function GetKnownFolderPath(const rfid: TGUID; dwFlags: DWord; hToken: THandle): WideString;
var
  buffer: PWideChar;
  ret: HResult;
begin
  ret := ShGetKnownFolderPath(rfid, dwFlags, hToken, buffer);
  OleCheck(ret);
  try
    Result := buffer;
  finally
    CoTaskMemFree(buffer);
  end;
end;


function PerformSHFileOperation(Handle : HWnd; iAction : UINT; sFromFilename, sDestFolder: String; bRenameOnCollision, bSendToRecycleBin, bShowConfirmation, bShowProgress, bVerboseErrors : boolean): Boolean;
var
  Struct : TSHFileOpStructA;
  bDelete: Boolean;
  bAborted : Boolean;
begin
  bAborted := False;
  bDelete := iAction = FO_Delete;
  sFromFilename := sFromFilename + #0#0;
  if not bDelete then
    sDestFolder := sDestFolder + #0#0;

  with Struct do
  begin
    wnd         := Handle;
    wFunc       := iAction;
    pFrom       := PAnsiChar(AnsiString(sFromFilename));
    if bDelete then
      pTo       := nil
    else
      pTo       := PAnsiChar(AnsiString(sDestFolder));

    If (bDelete = False) or bSendToRecycleBin then
      fFlags := FOF_ALLOWUNDO or FOF_FILESONLY
    else
      fFlags := 0 or FOF_FILESONLY;

    If bShowConfirmation = false then
      fFlags := fFlags or FOF_NOCONFIRMATION;

    if bShowProgress = False then
      fFlags := fFlags or FOF_SILENT;

    If (bDelete = False) and bRenameOnCollision then
      fFlags := fFlags or FOF_RENAMEONCOLLISION;

    if bVerboseErrors = false then
      fFlags := fFlags or FOF_NOERRORUI;

    fAnyOperationsAborted := bAborted;
    hNameMappings := nil;
    lpszProgressTitle := nil;
  end;
  result := (SHFileOperationA(Struct) = 0) and (not bAborted);
end;

function PerformSHFileOperation2(Handle : HWnd; iAction : UINT; ssFiles : TStrings; const sDestFolder : String; bRenameOnCollision, bSendToRecycleBin, bShowConfirmation, bShowProgress, bVerboseErrors : boolean): Boolean;
var
  sFromFilename : String;
  I: Integer;
  bDelete: Boolean;
begin
  Result := False;
  bDelete := iAction = FO_Delete;

  if (bDelete = False) and (sDestFolder = '') then
    exit;

  sFromFilename := '';
  for I := 0 to ssFiles.Count - 1 do
    sFromFilename := sFromFilename + ssFiles[I] + #0;

  if sFromFilename = '' then
    exit;

  // Remove final #0
  Delete(sFromFilename, Length(sFromFilename), 1);

  Result := PerformSHFileOperation(Handle, iAction, sFromFilename, sDestFolder, bRenameOnCollision, bSendToRecycleBin, bShowConfirmation, bShowProgress, bVerboseErrors);
end;



{!!
<FS>WindowsCopy

<FM>Declaration<FC>
function WindowsCopy(Handle : HWnd; sFromFilename, sDestFolder: String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean;
function WindowsCopy(Handle : HWnd; ssFiles : TStrings; const sDestFolder : String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;


<FM>Description<FN>

Uses <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762164%28v=vs.85%29.aspx>PerformSHFileOperation</L> to copy files to a folder.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>sFromFilename/ssFiles</C> <C>The file(s) to copy. If using sFromFilename the multiple files can be delimited using #0</C> </R>
<R> <C>sDestFolder</C> <C>The folder to copy files to</C> </R>
<R> <C>bRenameOnCollision</C> <C>Give the file being operated on a new name if a file with the target name already exists at the destination (pertains to <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_RENAMEONCOLLISION</L></C> </R>
<R> <C>bShowConfirmation</C> <C>If not specified then will automatically respond with "Yes to All" for any dialog box that is displayed (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOCONFIRMATION</L></C> </R>
<R> <C>bShowProgress</C> <C>Displays a progress dialog box (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_SILENT</L></C> </R>
<R> <C>bVerboseErrors</C> <C>Displays a dialog to the user if an error occurs (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOERRORUI</L></C> </R>
</TABLE>

Result is true unless the operation fails or is aborted.

<FM>Example<FC>
WindowsCopy(Form1.Handle, 'C:\My Image.jpg', 'C:\My Files', True, True, True);

!!}
function WindowsCopy(Handle : HWnd; sFromFilename, sDestFolder: String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean;
begin
  Result := PerformSHFileOperation(Handle, FO_Copy, sFromFilename, sDestFolder, bRenameOnCollision, True, bShowConfirmation, bShowProgress, bVerboseErrors);
end;

function WindowsCopy(Handle : HWnd; ssFiles : TStrings; const sDestFolder : String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;
begin
  Result := PerformSHFileOperation2(Handle, FO_Copy, ssFiles, sDestFolder, bRenameOnCollision, True, bShowConfirmation, bShowProgress, bVerboseErrors);
end;



{!!
<FS>WindowsMove

<FM>Declaration<FC>
function WindowsMove(Handle : HWnd; sFromFilename, sDestFolder: String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean;
function WindowsMove(Handle : HWnd; ssFiles : TStrings; const sDestFolder : String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;



<FM>Description<FN>
Uses <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762164%28v=vs.85%29.aspx>PerformSHFileOperation</L> to move files to a folder.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>sFromFilename/ssFiles</C> <C>The file(s) to move. If using sFromFilename the multiple files can be delimited using #0</C> </R>
<R> <C>sDestFolder</C> <C>The folder to move files to</C> </R>
<R> <C>bRenameOnCollision</C> <C>Give the file being operated on a new name if a file with the target name already exists at the destination (pertains to <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_RENAMEONCOLLISION</L></C> </R>
<R> <C>bShowConfirmation</C> <C>If not specified then will automatically respond with "Yes to All" for any dialog box that is displayed (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOCONFIRMATION</L></C> </R>
<R> <C>bShowProgress</C> <C>Displays a progress dialog box (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_SILENT</L></C> </R>
<R> <C>bVerboseErrors</C> <C>Displays a dialog to the user if an error occurs (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOERRORUI</L></C> </R>
</TABLE>

Result is true unless the operation fails or is aborted.

<FM>Example<FC>
WindowsMove(Form1.Handle, 'C:\My Image.jpg', 'C:\My Files', True, True, True);

!!}
function WindowsMove(Handle : HWnd; sFromFilename, sDestFolder: String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean;
begin
  Result := PerformSHFileOperation(Handle, FO_Move, sFromFilename, sDestFolder, bRenameOnCollision, True, bShowConfirmation, bShowProgress, bVerboseErrors);
end;

function WindowsMove(Handle : HWnd; ssFiles : TStrings; const sDestFolder : String; bRenameOnCollision, bShowConfirmation, bShowProgress: boolean; bVerboseErrors : boolean = True): Boolean; overload;
begin
  Result := PerformSHFileOperation2(Handle, FO_Move, ssFiles, sDestFolder, bRenameOnCollision, True, bShowConfirmation, bShowProgress, bVerboseErrors);
end;



{!!
<FS>WindowsErase

<FM>Declaration<FC>
function WindowsErase(Handle : HWnd; sFilename: String; bSendToRecycleBin, bShowConfirmation, bShowProgress: Boolean; bVerboseErrors : boolean = True): Boolean;
function WindowsErase(Handle : HWnd; ssFiles : TStrings; bSendToRecycleBin, bShowConfirmation, bShowProgress: Boolean; bVerboseErrors : boolean = True): Boolean; overload;



<FM>Description<FN>
Uses <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762164%28v=vs.85%29.aspx>PerformSHFileOperation</L> to delete files from a folder.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>sFilename/ssFiles</C> <C>The file(s) to delete. If using sFromFilename the multiple files can be delimited using #0</C> </R>
<R> <C>bSendToRecycleBin</C> <C>Moves the file to the Recycle Bin rather than permanently deleting it (pertains to <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_ALLOWUNDO</L></C> </R>
<R> <C>bShowConfirmation</C> <C>If not specified then will automatically respond with "Yes to All" for any dialog box that is displayed (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOCONFIRMATION</L></C> </R>
<R> <C>bShowProgress</C> <C>Displays a progress dialog box (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_SILENT</L></C> </R>
<R> <C>bVerboseErrors</C> <C>Displays a dialog to the user if an error occurs (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOERRORUI</L></C> </R>
</TABLE>

Result is true unless the operation fails or is aborted.

<FM>Example<FC>
WindowsErase(Form1.Handle, 'C:\My Image.jpg', True, True, True);

!!}
function WindowsErase(Handle : HWnd; sFilename: String; bSendToRecycleBin, bShowConfirmation, bShowProgress: Boolean; bVerboseErrors : boolean = True): Boolean;
begin
  Result := PerformSHFileOperation(Handle, FO_Delete, sFilename, '', False, bSendToRecycleBin, bShowConfirmation, bShowProgress, bVerboseErrors);
end;

function WindowsErase(Handle : HWnd; ssFiles : TStrings; bSendToRecycleBin, bShowConfirmation, bShowProgress: Boolean; bVerboseErrors : boolean = True): Boolean; overload;
begin
  Result := PerformSHFileOperation2(Handle, FO_Delete, ssFiles, '', False, bSendToRecycleBin, bShowConfirmation, bShowProgress, bVerboseErrors);
end;



{!!
<FS>WindowsRename

<FM>Declaration<FC>
function WindowsRename(Handle : HWnd; const sFilename, sNewName: String; bRenameOnCollision, bShowConfirmation : Boolean; bVerboseErrors : boolean = True; bCheckForOverwrite : Boolean = True): Boolean; overload;

<FM>Description<FN>
Uses <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762164%28v=vs.85%29.aspx>PerformSHFileOperation</L> to delete files from a folder.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>sFilename/ssFiles</C> <C>The file(s) to rename (only one can be specified</C> </R>
<R> <C>sNewName</C> <C>The new filename for the file (do not include path)</C> </R>
<R> <C>bRenameOnCollision</C> <C>Give the file being operated on a new name if a file with the target name already exists at the destination (pertains to <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_RENAMEONCOLLISION</L></C> </R>
<R> <C>bShowConfirmation</C> <C>If not specified then will automatically respond with "Yes to All" for any dialog box that is displayed (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOCONFIRMATION</L></C> </R>
<R> <C>bVerboseErrors</C> <C>Displays a dialog to the user if an error occurs (undefines <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb759795%28v=vs.85%29.aspx>FOF_NOERRORUI</L></C> </R>
</TABLE>

Result is true unless the operation fails or is aborted.

<FM>Example<FC>
WindowsRename(Form1.Handle, 'C:\My Image.jpg', 'My New Name.jpg', True, True);

!!}
// sNewName must be a name only without a file path
function WindowsRename(Handle : HWnd; const sFilename, sNewName: String; bRenameOnCollision, bShowConfirmation : Boolean; bVerboseErrors : boolean = True; bCheckForOverwrite : Boolean = True): Boolean; overload;
var
  sNewFilename: string;
begin
  Result := False;
  if (sFilename = '') or (sNewName = '') then
    exit;

  sNewFilename := IncludeTrailingBackSlash(ExtractFilepath(sFilename)) + sNewName;

  if bCheckForOverwrite and FileExists(sNewFilename) then
  begin
    Result := False;
    if bVerboseErrors then
      MessageDlg(format('A file already exists of the name, "%s"', [sNewName]), mtInformation, [mbOK], 0);
    exit;
  end;

  Result := PerformSHFileOperation(Handle, FO_Rename, sFilename, sNewFilename, bRenameOnCollision, True, bShowConfirmation, False, bVerboseErrors);
end;



{!!
<FS>WindowsLocalAppDataFolder

<FM>Declaration<FC>
function WindowsLocalAppDataFolder : string;


<FM>Description<FN>
Returns the full path to the "Local Apps" folder (using <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx>CSIDL_LOCAL_APPDATA</L>)

<FM>Example<FC>
IEFolderMView1.Folder := WindowsLocalAppDataFolder;

!!}
function WindowsLocalAppDataFolder : string;
begin
  Result := IncludeTrailingBackSlash(GetWindowsSpecialFolder(CSIDL_LOCAL_APPDATA));
end;




{!!
<FS>WindowsMyDocumentsFolder

<FM>Declaration<FC>
function WindowsMyDocumentsFolder: string;


<FM>Description<FN>
Returns the full path to the "My Documents" folder (using <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx>CSIDL_PERSONAL</L>)

<FM>Example<FC>
IEFolderMView1.Folder := WindowsMyDocumentsFolder;

!!}
function WindowsMyDocumentsFolder: string;
begin
  Result := IncludeTrailingBackSlash(GetWindowsSpecialFolder(CSIDL_PERSONAL));
end;



{!!
<FS>WindowsMyPicturesFolder

<FM>Declaration<FC>
function WindowsMyPicturesFolder: string;


<FM>Description<FN>
Returns the full path to the "My Pictures" folder (using <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx>CSIDL_MYPICTURES</L>)

<FM>Example<FC>
IEFolderMView1.Folder := WindowsMyPicturesFolder;

!!}
function WindowsMyPicturesFolder: string;
begin
  Result := IncludeTrailingBackSlash(GetWindowsSpecialFolder(CSIDL_MYPICTURES));
end;



{!!
<FS>WindowsMyMusicFolder

<FM>Declaration<FC>
function WindowsMyMusicFolder: string;


<FM>Description<FN>
Returns the full path to the "My Music" folder (using <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx>CSIDL_MYMUSIC</L>)

<FM>Example<FC>
IEFolderMView1.Folder := WindowsMyMusicFolder;

!!}
function WindowsMyMusicFolder: string;
begin
  Result := IncludeTrailingBackSlash(GetWindowsSpecialFolder(CSIDL_MYMUSIC));
end;



{!!
<FS>WindowsMyVideosFolder

<FM>Declaration<FC>
function WindowsMyVideosFolder: string;


<FM>Description<FN>
Returns the full path to the "My Videos" folder (using <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx>CSIDL_MYVIDEO</L>)

<FM>Example<FC>
IEFolderMView1.Folder := WindowsMyVideosFolder;

!!}
function WindowsMyVideosFolder: string;
begin
  Result := IncludeTrailingBackSlash(GetWindowsSpecialFolder(CSIDL_MYVIDEO));
end;




{!!
<FS>WindowsDesktopFolder

<FM>Declaration<FC>
function WindowsDesktopFolder: string;


<FM>Description<FN>
Returns the full path to the "Desktop" (using <L http://msdn.microsoft.com/en-us/library/windows/desktop/bb762494%28v=vs.85%29.aspx>CSIDL_DESKTOP</L>)

<FM>Example<FC>
IEFolderMView1.Folder := WindowsDesktopFolder;

!!}
function WindowsDesktopFolder: string;
begin
  Result := IncludeTrailingBackSlash(GetWindowsSpecialFolder(CSIDL_DESKTOP));
end;


   
{$IFDEF Delphi6orNewer}
type
  TSelectDirCallback = class(TObject)
  private
    FDirectory: string;
  protected
    function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer;
  public
    constructor Create(const ADirectory: string);
  end;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  Result := TSelectDirCallback(lpData).SelectDirCB(Wnd, uMsg, lParam, lpData);
end;
{$ENDIF}


{!!
<FS>WindowsSelectDirectory

<FM>Declaration<FC>
function WindowsSelectDirectory(const Caption: string; var Directory: string;
                                Parent: TWinControl = nil; bAllowNewFolder : Boolean = True): Boolean;

<FM>Description<FN>
Displays a dialog to the user which allows the to select a folder.

Note: Similar to FileCtrl.SelectDirectory

<FM>Example<FC>
sFolder := IEFolderMView1.Folder;
WindowsSelectDirectory('Specify Folder', sFolder);
  IEFolderMView1.Folder := sFolder;
!!}
{$IFNDEF Delphi6orNewer}
function WindowsSelectDirectory(const Caption: string; var Directory: string;
                                Parent: TWinControl = nil; bAllowNewFolder : Boolean = True): Boolean;
var
  Ops : TSelectDirOpts;
begin            
  if not DirectoryExists(Directory) then
    Directory := '';
  if bAllowNewFolder then
    Ops := [sdAllowCreate, sdPerformCreate, sdPrompt]
  else
    Ops := [];
  Result := FileCtrl.SelectDirectory(Directory, Ops, 0);
end;
{$ELSE}
function WindowsSelectDirectory(const Caption: string; var Directory: string;
                                Parent: TWinControl = nil; bAllowNewFolder : Boolean = True): Boolean;
const
  Display_Sharable_Resources = True;
  Validate_Directory         = True;
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  OldErrorMode: Cardinal;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
  CoInitResult: HRESULT;
  SelectDirCallback: TSelectDirCallback;
  Root: WideString;
begin
  Result := False;    
  Root := '';
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        if (Parent = nil) or not Parent.HandleAllocated then
          hwndOwner := Application.Handle
        else
          hwndOwner := Parent.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE or BIF_EDITBOX;
        if not bAllowNewFolder then
          ulFlags := ulFlags or BIF_NONEWFOLDERBUTTON;
        if Display_Sharable_Resources then
          ulFlags := ulFlags or BIF_SHAREABLE;
        if Validate_Directory then
          ulFlags := ulFlags or BIF_VALIDATE;
        lpfn := SelectDirCB;
      end;
      SelectDirCallback := TSelectDirCallback.Create(Directory);
      try
        BrowseInfo.lParam := Integer(SelectDirCallback);
        CoInitResult := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
        if CoInitResult = RPC_E_CHANGED_MODE then
          BrowseInfo.ulFlags := BrowseInfo.ulFlags and not BIF_NEWDIALOGSTYLE;
        try
          WindowList := DisableTaskWindows(0);
          OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
          try
            ItemIDList := ShBrowseForFolder(BrowseInfo);
          finally
            SetErrorMode(OldErrorMode);
            EnableTaskWindows(WindowList);
          end;
        finally
          CoUninitialize;
        end;
      finally
        SelectDirCallback.Free;
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;


{ TSelectDirCallback }

constructor TSelectDirCallback.Create(const ADirectory: string);
begin
  inherited Create;
  FDirectory := ADirectory;
end;

function TSelectDirCallback.SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer;
var
  Rect: TRect;
  Monitor: TMonitor;
begin
  Result := 0;
  if uMsg = BFFM_INITIALIZED then
  begin
    if Assigned(Application.MainForm) then
      Monitor := Screen.MonitorFromWindow(Application.MainForm.Handle)
    else
      Monitor := Screen.MonitorFromWindow(0);
    GetWindowRect(Wnd, Rect);
    SetWindowPos(Wnd, 0, (Monitor.Width - (Rect.Right - Rect.Left)) div 2,
      (Monitor.Height - (Rect.Bottom - Rect.Top)) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
    if FDirectory <> '' then
      SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), Windows.LPARAM(PChar(FDirectory)));
  end
  else
  if uMsg = BFFM_VALIDATEFAILED then
  begin
    MessageDlg(Format(iemsg(IEMsg_XIsNotAValidFolder), [PChar(lParam)]), mtError, [mbOK], 0);
    Result := 1;
  end;
end;
{$ENDIF}

        
const
  CF_PreferredDropEffect_Str = 'Preferred DropEffect'; // Windows Explorer Clipboard format string for specifying whether a file is moved or copied




{!!
<FS>CutFilesToClipboard

<FM>Declaration<FC>
function CutFilesToClipboard(Handle : HWND; Filenames: TStrings): Boolean;


<FM>Description<FN>
Adds a list of filenames to the clipboard and specifies that they should moved (same as selecting Ctrl+X in Windows Explorer). The filenames can then be pasted into other Windows applications such as Explorer using Ctrl+V. After pasting they will be removed from the source folder.

<FM>Example<FC>
CutFilesToClipboard(Form1.Handle, ssFilenames);

!!}
function CutFilesToClipboard(Handle : HWND; Filenames: TStrings): Boolean;
{$IFNDEF Delphi6orNewer}
type
  PCardinal     = ^Cardinal;
{$ENDIF}
var
  sFilenames: String;
  iIndex: Integer;
  hBuffer: HGLOBAL;
  pBuffer: PDropFiles;
  f: UINT;  
  d: pcardinal;
begin
  Result := False;
  if (Filenames = nil) and (Filenames.Count = 0) then
    Exit; 

  // Filenames are separated by #0 and end with a double #0#0
  sFilenames := '';
  for iIndex := 0 to Filenames.Count - 1 do
    sFilenames := sFilenames + ExcludeTrailingBackslash(Filenames.Strings[iIndex]) + #0;
  sFilenames := sFilenames + #0;

  if OpenClipboard(Handle) = False then
    Exit;

  EmptyClipboard();

  hBuffer := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, SizeOf(DROPFILES) + Length(sFilenames));
  try
    if hBuffer = 0 then
      exit;

    pBuffer := GlobalLock(hBuffer);

    // prepare the "DROPFILES" structure
    pBuffer^.pFiles := SizeOf(DROPFILES);
    pBuffer := Pointer(NativeInt(pBuffer) + SizeOf(DROPFILES));
    CopyMemory(pBuffer, PChar(sFilenames), Length(sFilenames));
    GlobalUnlock(hBuffer);
    SetClipboardData(CF_HDROP, hBuffer);

    // Preferred DropEffect
    f := RegisterClipboardFormat(PChar(CF_PreferredDropEffect_Str));
    hBuffer := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT, sizeof(dword));
    d := pcardinal(GlobalLock(hBuffer));
    d^ := DROPEFFECT_MOVE;
    SetClipboardData(f, hBuffer);
    GlobalUnlock(hBuffer);

    CloseClipboard;
    Result := True;
  except
    // free only if handle could not be passed to the clipboard
    GlobalFree(hBuffer);
    Raise;
  end;
end;




{!!
<FS>CopyFilesToClipboard

<FM>Declaration<FC>
function CopyFilesToClipboard(Handle : HWND; Filenames: TStrings): Boolean;


<FM>Description<FN>
Adds a list of filenames to the clipboard (same as selecting Ctrl+C in Windows Explorer). The filenames can then be pasted into other Windows applications such as Explorer using Ctrl+V.

<FM>Example<FC>
CopyFilesToClipboard(Form1.Handle, ssFilenames);

!!}
// Source: http://www.martinstoeckli.ch/delphi/
function CopyFilesToClipboard(Handle : HWND; Filenames: TStrings): Boolean;
var
  sFilenames: String;
  iIndex: Integer;
  hBuffer: HGLOBAL;
  pBuffer: PDropFiles;
begin
  Result := False;
  if (Filenames = nil) and (Filenames.Count = 0) then
    Exit;

  // Filenames are separated by #0 and end with a double #0#0
  sFilenames := '';
  for iIndex := 0 to Filenames.Count - 1 do
    sFilenames := sFilenames + ExcludeTrailingBackslash(Filenames.Strings[iIndex]) + #0;
  sFilenames := sFilenames + #0;

  // allocate memory with the size of the "DropFiles" structure plus the
  // length of the filename buffer.
  hBuffer := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, SizeOf(DROPFILES) + Length(sFilenames));
  try
    if hBuffer = 0 then
      exit;

    pBuffer := GlobalLock(hBuffer);
    try
      // prepare the "DROPFILES" structure
      pBuffer^.pFiles := SizeOf(DROPFILES);
      // behind the "DROPFILES" structure we place the filenames
      pBuffer := Pointer(NativeInt(pBuffer) + SizeOf(DROPFILES));
      CopyMemory(pBuffer, PChar(sFilenames), Length(sFilenames));
    finally
      GlobalUnlock(hBuffer);
    end;
        
    // copy buffer to the clipboard
    if OpenClipboard(Handle) = False then
      raise Exception.CreateRes(@SCannotOpenClipboard);
    try                 
      EmptyClipboard;          
      SetClipboardData(CF_HDROP, hBuffer);
    finally
      CloseClipboard;
    end;

    Result := True;
  except
    // free only if handle could not be passed to the clipboard
    GlobalFree(hBuffer);    
    Raise;
  end;
end;



procedure _FilenamesFromDropHandle(hDropHandle: HDROP; Dest : TStrings);
var
  szBuffer: PChar;
  iCount, iIndex: Integer;
  iLength: Integer;
begin  
  Dest.Clear;
  iCount := DragQueryFile(hDropHandle, $FFFFFFFF, nil, 0);
  for iIndex := 0 to iCount - 1 do
  begin
    iLength := DragQueryFile(hDropHandle, iIndex, nil, 0);
    szBuffer := StrAlloc(iLength + 1);
    try
      DragQueryFile(hDropHandle, iIndex, szBuffer, iLength + 1);
      Dest.Add(szBuffer);
    finally
      StrDispose(szBuffer);
    end;
  end;
end;




{!!
<FS>PasteFilesFromClipboard

<FM>Declaration<FC>
function PasteFilesFromClipboard(Handle : HWND; ssFilenames: TStrings; out bMoveFiles : Boolean) : Boolean;


<FM>Description<FN>
If there are files om the clipboard that have been copied from Windows Explorer or <A TImageEnFolderMView> then they will be assigned to <FC>ssFilenames<FN>.
<FC>bMoveFiles<FN> will be true if they are marked to be moved (i.e. Ctrl+X was used instead of Ctrl+V).
Result is true if there are files on the clipboard

<FM>Example<FC>
bHaveFiles := PasteFilesFromClipboard(Form1.Handle, ssFilenames, bMoveFiles);
if bHaveFiles then
  ShowMessage('No Files to paste')
else
if bMoveFiles  then
  ShowMessage('Files to Move:' + #13#10 + ssFilenames.Text)    
else
  ShowMessage('Files to Copy:' + #13#10 + ssFilenames.Text)

!!}
// Source: http://www.martinstoeckli.ch/delphi/
// Can pass nil to ssFilenames to test if there are files on the clipboard
function PasteFilesFromClipboard(Handle : HWND; ssFilenames: TStrings; out bMoveFiles : Boolean) : Boolean;
var
  hDropHandle: HDROP;
  ClipFormat, hn: Cardinal;
  szBuffer: array [0 .. 511] of Char;
  FormatID: string;
  pMem: Pointer;
  iDropEffect : Cardinal;
begin
  Result := False;
  bMoveFiles := False;
  if assigned(ssFilenames) then
    ssFilenames.Clear;

  if OpenClipboard(Handle) = False then
    Exit;

  try
    // does clipboard contain filenames?
    Result := IsClipboardFormatAvailable(CF_HDROP);
    if assigned(ssFilenames) and result then
    begin
      hDropHandle := GetClipboardData(CF_HDROP);
      _FilenamesFromDropHandle(hDropHandle, ssFilenames);
    end;

    ClipFormat := EnumClipboardFormats(0);
    while ClipFormat <> 0 do
    begin
      GetClipboardFormatName(ClipFormat, szBuffer, SizeOf(szBuffer));
      FormatID := string(szBuffer);

      if SameText(FormatID, CF_PreferredDropEffect_Str) then
      begin
        hn := GetClipboardData(ClipFormat);
        pMem := GlobalLock(hn);
        Move(pMem^, iDropEffect, 4);
        bMoveFiles := iDropEffect = DROPEFFECT_MOVE;
        GlobalUnlock(hn);
        Break;
      end;

      ClipFormat := EnumClipboardFormats(ClipFormat);
    end;

  finally
    CloseClipboard;
  end;
end;


{!!
<FS>CanPasteFilesFromClipboard

<FM>Declaration<FC>
function CanPasteFilesFromClipboard(Handle : HWND) : Boolean;


<FM>Description<FN>
Returns True if there are files on the clipboard that have been copied from Windows Explorer or <A TImageEnFolderMView> and can be pasted using <A PasteFilesFromClipboard>.

<FM>Example<FC>
btnPaste.Enabled := CanPasteFilesFromClipboard(Form1.Handle);

!!}
function CanPasteFilesFromClipboard(Handle : HWND) : Boolean;
var
  bDummy: Boolean;
begin
  try
    Result := PasteFilesFromClipboard(Handle, nil, bDummy);
  except
    Result := False;
  end;
end;



{!!
<FS>PopupSystemMenu

<FM>Declaration<FC>
procedure PopupSystemMenu(Handle: HWND; ssFileList: TStrings; x, y: integer);


<FM>Description<FN>
Displays the system menu for the specified files at position x,y. This menu is the same as that displayed when right-clicking a selection of files in Windows Explorer

<FM>Example<FC>
PopupSystemMenu(Form1.Handle, ssSelectedFiles, ClickPos.X, ClickPos.Y);

!!}
// Source: http://blog.issamsoft.com/index.php?q=en/node/65
procedure PopupSystemMenu(Handle: HWND; ssFileList: TStrings; x, y: integer);
var
  Root: IShellFolder;
  ShellParentFolder: IShellFolder;
  chEaten, dwAttributes: ULONG;
  FilePIDL, ParentFolderPIDL: PItemIDList;
  CM: IContextMenu;
  Menu: HMenu;
  Command: LongBool;
  ICM2: IContextMenu2;

  ICI: TCMInvokeCommandInfo;
  ICmd: integer;
  ZVerb: array[0..255] of AnsiChar;
  Verb: string;
  Handled: boolean;
  SCV: IShellCommandVerb;
  HR: HResult;
  P: TPoint;

  ItemIDListArray: PArrayOfPItemIDList;
  idx: Integer;
Begin
  if ssFileList.Count = 0 then
    Exit;

  OleCheck(SHGetDesktopFolder(Root));//Get the Desktop IShellFolder interface

  OleCheck(Root.ParseDisplayName(Handle, nil,
                                 PWideChar(WideString(ExtractFilePath(ssFileList[0]))),
                                 chEaten, ParentFolderPIDL, dwAttributes)); // Get the PItemIDList of the parent folder

  OleCheck(Root.BindToObject(ParentFolderPIDL, nil, IShellFolder, ShellParentFolder)); // Get the IShellFolder Interface  of the Parent Folder

  //allocate memory for the PItemIDList array
  ItemIDListArray := AllocMem(SizeOf(PItemIDList) * ssFileList.Count);
  try
    for idx := 0 to ssFileList.Count - 1 do
    Begin
      // Get the relative  PItemIDList of each file in the list
      OleCheck(ShellParentFolder.ParseDisplayName(Handle, nil,
               PWideChar(WideString(ExtractFileName(ssFileList[idx]))),
               chEaten, FilePIDL, dwAttributes));
      ItemIDListArray^[idx] := FilePIDL;
    End;
    // get the IContextMenu Interace for the file array
    ShellParentFolder.GetUIObjectOf(Handle, ssFileList.Count, ItemIDListArray^[0], IID_IContextMenu, nil, CM);
  finally
    FreeMem(ItemIDListArray);
  end;

  if CM = nil then
    Exit;

  P.X := X;
  P.Y := Y;

  Windows.ClientToScreen(Handle, P);

  Menu := CreatePopupMenu;
  try
    CM.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME);
    CM.QueryInterface(IID_IContextMenu2, ICM2); //To handle submenus.
    try
      Command := TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, p.X, p.Y, 0, Handle, nil);
    finally
      ICM2 := nil;
    end;

    if Command then
    begin
      ICmd := LongInt(Command) - 1;
      HR := CM.GetCommandString(ICmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb));
      Verb := string(StrPas(ZVerb)); // Verb := String(PAnsiString(@ZVerb[0])); <-- v5.0.6. Fails in 64bit
      Handled := False;
      if Supports(nil, IShellCommandVerb, SCV) then
      begin
        HR := 0;
        SCV.ExecuteCommand(Verb, Handled);
      end;

      if not Handled then
      begin
        FillChar(ICI, SizeOf(ICI), #0);
        with ICI do
        begin
          cbSize := SizeOf(ICI);
          hWND := 0;
          lpVerb := MakeIntResourceA(ICmd);
          nShow := SW_SHOWNORMAL;
        end;
        HR := CM.InvokeCommand(ICI);
      end;

      if Assigned(SCV) then
        SCV.CommandCompleted(Verb, HR = S_OK);
    end;
  finally
     DestroyMenu(Menu)
  end;
End;



{ TIEFileDragDrop }

constructor TIEFileDragDrop.Create(Control: TWinControl; OnFileDrop : TIEFileDropEvent);
begin
  inherited Create;
  fDroppingActive := False;
  fControl := Control;
  fOnFileDrop := OnFileDrop;
  fDropActions := [iedaCopy, iedaMove]; 
  fControlHandle := 0;
end;


destructor TIEFileDragDrop.Destroy;
begin
  if fControlHandle <> 0 then  
    RevokeDragDrop(fControlHandle);
  fControlHandle := 0;

  inherited;
end;


function TIEFileDragDrop.GetDragEffect(KeyState : Integer) : Integer;
var
  bIsShiftKeyPressed : Boolean;
begin
  if fDropActions = [iedaCopy, iedaMove] then
  begin
    bIsShiftKeyPressed := (KeyState and MK_SHIFT = MK_SHIFT);
    if bIsShiftKeyPressed then
      Result := DROPEFFECT_MOVE
   else
      Result := DROPEFFECT_COPY;
  end
  else
  if fDropActions = [iedaCopy] then
    Result := DROPEFFECT_COPY
  else
  if fDropActions = [iedaMove] then
    Result := DROPEFFECT_MOVE
  else
    Result := DROPEFFECT_NONE;
end;



function TIEFileDragDrop.DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  dwEffect := GetDragEffect(grfKeyState);
  result := S_OK;
end;

function TIEFileDragDrop.DragLeave: HResult;
begin    
  Result := S_OK;
end;

function TIEFileDragDrop.DragOver(grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin  
  dwEffect := GetDragEffect(grfKeyState);
  result := S_OK;
end;

function TIEFileDragDrop.Drop(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  ssFilenames: TStringList;
begin
  Result := S_OK;
  Try
    ssFilenames := TStringList.create;
    try                      
      dwEffect := GetDragEffect(grfKeyState);
      GetFilenames(dataObj, ssFilenames);
      fOnFileDrop(Self, ssFilenames, dwEffect);
    finally
      ssFilenames.free;
    end;     
  Except
    Application.HandleException(Self);
  End;
end;



procedure TIEFileDragDrop.GetFileNames(const dataObj: IDataObject; Dest: TStrings);
var
  formatetcIn: TFormatEtc;
  medium: TStgMedium;
  hDropHandle: HDROP;
begin
  Dest.Clear;
  formatetcIn.cfFormat := CF_HDROP;
  formatetcIn.ptd := nil;
  formatetcIn.dwAspect := DVASPECT_CONTENT;
  formatetcIn.lindex := -1;
  formatetcIn.tymed := TYMED_HGLOBAL;
  if dataObj.GetData(formatetcIn, medium)=S_OK then
  begin
    (* This cast needed because HDROP is incorrectly declared as Longint in ShellAPI.pas.  It should be declared as THandle
       which is an unsigned integer.  Without this fix the routine fails in top-down memory allocation scenarios. *)
    hDropHandle := HDROP(medium.hGlobal);
    _FilenamesFromDropHandle(hDropHandle, Dest);
  end;
end;


function TIEFileDragDrop.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

function TIEFileDragDrop.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Integer): HResult;
begin
  if fEscapePressed or ((MK_LBUTTON or MK_RBUTTON) = (grfKeyState and (MK_LBUTTON or MK_RBUTTON))) then
    Result := DRAGDROP_S_CANCEL
  else
  if grfKeyState and MK_LBUTTON = 0 then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;


procedure TIEFileDragDrop.SetDroppingActive(const Value: Boolean);
begin
  if fDroppingActive <> Value then
  begin
    fDroppingActive := Value;
    if fDroppingActive and (fControlHandle = 0) then
    begin
      fControlHandle := fControl.Handle;
      RegisterDragDrop(fControlHandle, Self);
    end;
  end;
end;


function TIEFileDragDrop.GetDataObject(ssFiles: TStrings): IDataObject;
var
  Malloc: IMalloc;
  Root: IShellFolder;
  p: PArrayOfPItemIDList;
  chEaten, dwAttributes: ULONG;
  i, iFileCount: Integer;
begin
  Result := nil;
  iFileCount := ssFiles.Count;
  if iFileCount = 0 then
    Exit;

  OleCheck(SHGetMalloc(Malloc));
  OleCheck(SHGetDesktopFolder(Root));
  p := AllocMem(SizeOf(PItemIDList) * iFileCount);
  try
    for i := 0 to iFileCount - 1 do
      try
        OleCheck(Root.ParseDisplayName(GetActiveWindow, nil, PWideChar(WideString(ssFiles[i])), chEaten, p^[i], dwAttributes));
      except
        // ERROR
      end;
    OleCheck(Root.GetUIObjectOf(GetActiveWindow, iFileCount, p^[0], IDataObject, nil, Pointer(Result)));
  finally
    for i := 0 to iFileCount - 1 do
    begin
      if p^[i] <> nil then
        Malloc.Free(p^[i]);
    end;
    FreeMem(p);
  end;
end;


procedure TIEFileDragDrop.InitiateDragging(ssFilenames : TStrings; DragActions: TIEFileDragDropActions);
var
  DataObject: IDataObject;
  dwEffect: Integer;
  dwOKEffects: Longint;
begin
  DataObject := GetDataObject(ssFilenames);
  if DataObject = nil then
    exit;

  dwEffect := DROPEFFECT_NONE;

  if DragActions = [iedaCopy] then
    dwOKEffects := DROPEFFECT_COPY
  else    
  if DragActions = [iedaMove] then
    dwOKEffects := DROPEFFECT_MOVE
  else       
    dwOKEffects := DROPEFFECT_COPY or DROPEFFECT_MOVE;

  DoDragDrop(DataObject, Self, dwOKEffects, dwEffect);
end;

         
{ TIEFolderWatch }


const
  FILE_NOTIFY_CHANGE_FILE_NAME   = $00000001;
  FILE_NOTIFY_CHANGE_DIR_NAME    = $00000002;
  FILE_NOTIFY_CHANGE_ATTRIBUTES  = $00000004;
  FILE_NOTIFY_CHANGE_SIZE        = $00000008;
  FILE_NOTIFY_CHANGE_LAST_WRITE  = $00000010;
  FILE_NOTIFY_CHANGE_LAST_ACCESS = $00000020;
  FILE_NOTIFY_CHANGE_CREATION    = $00000040;
  FILE_NOTIFY_CHANGE_SECURITY    = $00000100;



type
  PFILE_NOTIFY_INFORMATION = ^TFILE_NOTIFY_INFORMATION;
  TFILE_NOTIFY_INFORMATION = record
    NextEntryOffset : Cardinal;
    Action          : Cardinal;
    FileNameLength  : Cardinal;
    FileName        : array[0..MAX_PATH - 1] of WideChar;
  end;

const
  WM_FOLDERWATCH_ERROR    = WM_USER + 137;
  WM_FOLDERWATCH_NOTIFY   = WM_USER + 138;

  FILE_LIST_DIRECTORY  = $0001;

const
  // error messages
  cErrorInWatchThread = 'Error "%s" in watch thread. Error code: %d';
  cErrorCreateWatchError = 'Error trying to create file handle for "%s". Error code: %d';

type
  TIEFolderWatchThread = class(TThread)
  private
    FWatchSubTree : Boolean;
    FAbortEvent   : THandle;
    FChangeEvent  : THandle;
    FBufferSize   : Integer;
    FWndHandle    : HWND;
    FDirHandle    : THandle;
    FPath         : string;
    FIOResult     : Pointer;
    FFilter       : Integer;
    procedure SignalError(const ErrorMessage: string; ErrorCode: Cardinal = 0);
  protected
    procedure Execute; override;
  public
    constructor Create(const Path: string;
                       const WndHandle: HWND;
                       const BufferSize: Integer;
                       const AbortEvent: THandle;
                       const TypeFilter: Cardinal;
                       const aWatchSubTree: Boolean);
    destructor Destroy; override;
  end;


function _MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;
begin
  if ANow < AThen then
    Result := Trunc(MSecsPerDay * (AThen - ANow))
  else
    Result := Trunc(MSecsPerDay * (ANow - AThen));
end;

procedure WaitForFileReady(const FileName: string; const Timeout: Cardinal);
var
  hFile: THandle;
  StartTime: TDateTime;
begin
  StartTime := Now;
                                                                                           
  // wait to close
  while (_MilliSecondsBetween(Now, StartTime) < Timeout) or (Timeout = 0) do 
  begin
    hFile := CreateFile(PChar(FileName), GENERIC_READ, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    if hFile <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(hFile);
      Break;
    end;

    // wait for file
    Sleep(50);
  end;
end;

procedure TIEFolderWatchThread.Execute;
var
  NotifyData: PFILE_NOTIFY_INFORMATION;
  Events: array [0..1] of THandle;
  ErrorMessage: string;
  WaitResult: DWORD;
  NextEntry: Cardinal;
  FileName: PWideChar;
  Overlap: TOverlapped;
  ResSize: Cardinal;
begin
  FillChar(Overlap, SizeOf(TOverlapped), 0);
  Overlap.hEvent := FChangeEvent;

  // set the array of events
  Events[0] := FChangeEvent;
  Events[1] := FAbortEvent;

  while not Terminated do
  try
    if ReadDirectoryChangesW(FDirHandle, FIOResult, FBufferSize, FWatchSubtree, FFilter, @ResSize, @Overlap, nil) then
    begin
      WaitResult := WaitForMultipleObjects(Length(Events), @Events, FALSE, INFINITE);

      // check if we have terminated the thread
      if WaitResult <> WAIT_OBJECT_0 then
      begin
        Terminate;
        Exit;
      end;

      if WaitResult = WAIT_OBJECT_0 then
      begin
        if GetOverlappedResult(FDirHandle, Overlap, ResSize, False) then
        begin
          NotifyData := FIOResult;

          // check overflow
          if ResSize = 0 then
          begin
            ErrorMessage := SysErrorMessage(ERROR_NOTIFY_ENUM_DIR);
            SignalError(ErrorMessage, ERROR_NOTIFY_ENUM_DIR);
          end;

          repeat
            NextEntry := NotifyData^.NextEntryOffset;

            // get memory for filename and fill it with data
            GetMem(FileName, NotifyData^.FileNameLength + SizeOf(WideChar));
            Move(NotifyData^.FileName, Pointer(FileName)^, NotifyData^.FileNameLength);
            PWord(Cardinal(FileName) + NotifyData^.FileNameLength)^ := 0;

            // send the message about the filename information and advance to the next entry
            PostMessage(FWndHandle, WM_FOLDERWATCH_NOTIFY, NotifyData^.Action, LParam(FileName));
            PByte(NotifyData) := PByte(DWORD(NotifyData) + NextEntry);
          until (NextEntry = 0);
        end
        else
        begin
          ErrorMessage := SysErrorMessage(GetLastError);
          SignalError(ErrorMessage);
        end;
      end;
    end
    else
    begin
      ErrorMessage := SysErrorMessage(GetLastError);
      SignalError(ErrorMessage);
    end;
  except
    on E:Exception do
    begin
      ErrorMessage := E.Message;
      SignalError(ErrorMessage);
    end;
  end;
end;

procedure TIEFolderWatchThread.SignalError(const ErrorMessage: string; ErrorCode: Cardinal);
var
  ErrorMsg: PChar;
  MessageSize: Integer;
begin
  if ErrorCode = 0 then
    ErrorCode := GetLastError;

  // calculate the size of the error message buffer
  MessageSize := Length(ErrorMessage) * SizeOf(Char) + SizeOf(WideChar);

  GetMem(ErrorMsg, MessageSize);
  StrPCopy(ErrorMsg, ErrorMessage);
  PostMessage(FWndHandle, WM_FOLDERWATCH_ERROR, ErrorCode, LPARAM(ErrorMsg));
end;

constructor TIEFolderWatchThread.Create(const Path: string;
                                   const WndHandle: HWND;
                                   const BufferSize: Integer;
                                   const AbortEvent: THandle;
                                   const TypeFilter: Cardinal;
                                   const aWatchSubTree: Boolean);
begin
   //
   // Retrieve proc pointer, open directory to
   // watch and allocate buffer for notification data.
   // (note, it is done before calling inherited
   // create (that calls BeginThread) so any exception
   // will be still raised in caller's thread)
   //
   FDirHandle := CreateFile(PChar(Path),
                            FILE_LIST_DIRECTORY,
                            FILE_SHARE_READ OR
                            FILE_SHARE_DELETE OR
                            FILE_SHARE_WRITE,
                            nil, OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS OR
                            FILE_FLAG_OVERLAPPED,
                            0);

   if FDirHandle = INVALID_HANDLE_VALUE then
     raise Exception.CreateFmt(cErrorCreateWatchError, [Path, GetLastError]);

   FChangeEvent := CreateEvent(nil, FALSE, FALSE, nil);
   FAbortEvent := AbortEvent;

   // allocate the buffer memory
   FBufferSize := BufferSize * SizeOf(TFILE_NOTIFY_INFORMATION);
   GetMem(FIOResult, FBufferSize);

   FWatchSubTree := aWatchSubtree;
   FWndHandle := WndHandle;
   FPath := Path;
   FFilter := TypeFilter;

   inherited Create(False);
end;


destructor TIEFolderWatchThread.Destroy;
begin
   CloseHandle(FChangeEvent);

   if FDirHandle <> INVALID_HANDLE_VALUE  then
     CloseHandle(FDirHandle);
   if Assigned(FIOResult) then
     FreeMem(FIOResult);

   inherited Destroy;
end;


procedure TIEFolderWatch.AllocWatchThread;
begin
  if FWatchThread = nil then
  begin
    FAbortEvent := CreateEvent(nil, FALSE, FALSE, nil);
    FWatchThread := TIEFolderWatchThread.Create(Path,
                                                FWndHandle,
                                                FBufferSize,
                                                FAbortEvent,
                                                MakeFilter,
                                                WatchSubtree);
  end;
end;

procedure TIEFolderWatch.ReleaseWatchThread;
var
  AResult: Cardinal;
  ThreadHandle: THandle;
begin
  if FWatchThread <> nil then
  begin
    ThreadHandle := FWatchThread.Handle;
    // set and close event
    SetEvent(FAbortEvent);

    // wait and block until thread is finished
    AResult := WaitForSingleObject(ThreadHandle, cShutdownTimeout);

    // check if we timed out
    if AResult = WAIT_TIMEOUT then
      TerminateThread(ThreadHandle, 0);

    FreeAndNil(FWatchThread);
    CloseHandle(FAbortEvent);
  end;

end;

procedure TIEFolderWatch.RestartWatchThread;
begin
  Stop;
  Start;
end;

function TIEFolderWatch.Running: Boolean;
begin
  Result := FWatchThread <> nil;
end;

procedure TIEFolderWatch.DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));

  if Instance <> @DefWindowProc then
  begin
    { make sure we restore the default
      windows procedure before freeing memory }
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
  end;

  DestroyWindow(Wnd);
end;

destructor TIEFolderWatch.Destroy;
begin
  Stop;
  DeallocateHWnd(FWndHandle);

  inherited Destroy;
end;

constructor TIEFolderWatch.Create;
begin
   FWndHandle := AllocateHWnd(WatchWndProc);
   FWatchSubtree := True;
   FBufferSize := 32;

   // construct the default watch actions and options
   FWatchActions := [waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew];
   FWatchOptions := [woFileName, woFolderName, woAttributes, woSize, woLastWrite,
                     woLastAccess, woCreation, woSecurity];
end;



procedure TIEFolderWatch.SetWatchActions(const Value: TWatchActions);
begin
  if FWatchActions <> Value then
  begin
    FWatchActions := Value;

    if Running then
      RestartWatchThread;

    Change;
  end;
end;

procedure TIEFolderWatch.SetWatchOptions(const Value: TWatchOptions);
begin
  if FWatchOptions <> Value then
  begin
    FWatchOptions := Value;

    if Running then
      RestartWatchThread;

    Change;
  end;
end;

procedure TIEFolderWatch.WatchWndProc(var Msg : TMessage);
var
  ErrorCode: Cardinal;
  ErrorMessage: string;
begin
   case Msg.msg of
     WM_FOLDERWATCH_NOTIFY:
     //
     // Retrieve notify data and forward
     // the event to TIEFolderWatch's notify
     // handler. Free filename string (allocated
     // in WatchThread's notify handler.)
     //
     begin
        try
           Notify(Msg.wParam, WideCharToString(PWideChar(Msg.lParam)));
        finally
          if Msg.lParam <> 0 then
            FreeMem(Pointer(Msg.lParam));
        end;
     end;

     WM_FOLDERWATCH_ERROR:
     //
     // Disable dir watch and re-raise
     // exception on error
     //
     begin
        try
          ErrorMessage := StrPas(PChar(Msg.lParam));
          ErrorCode := Msg.WParam;

          if Assigned(FOnError) then
            FOnError(Self, ErrorCode, ErrorMessage);
        finally
          if Msg.lParam <> 0 then
            FreeMem(Pointer(Msg.lParam));
        end;
     end;
     //
     // pass all other messages down the line
     //
     else
     begin
       Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.wParam, Msg.lParam);
       Exit;
     end;
   end;
end;

function TIEFolderWatch.MakeFilter: Integer;
const
  FilterFlags: array [TWatchOption] of Integer = (FILE_NOTIFY_CHANGE_FILE_NAME,
                                                  FILE_NOTIFY_CHANGE_DIR_NAME,
                                                  FILE_NOTIFY_CHANGE_ATTRIBUTES,
                                                  FILE_NOTIFY_CHANGE_SIZE,
                                                  FILE_NOTIFY_CHANGE_LAST_WRITE,
                                                  FILE_NOTIFY_CHANGE_LAST_ACCESS,
                                                  FILE_NOTIFY_CHANGE_CREATION,
                                                  FILE_NOTIFY_CHANGE_SECURITY);
{$IFDEF Delphi2005orNewer}
var
  Flag: TWatchOption;
begin
  Result := 0;
  for Flag in FWatchOptions do
    Result := Result or FilterFlags[Flag];
end;   
{$ELSE}
begin
  Result := 0;
  if woFileName in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if woFolderName in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if woAttributes in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if woSize in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if woLastWrite in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if woLastAccess in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if woCreation in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_CREATION;
  if woSecurity in FWatchOptions then
    Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
end;
{$ENDIF}


procedure TIEFolderWatch.SetWatchSubTree(const Value : Boolean);
begin
  if Value <> FWatchSubtree then
  begin
    FWatchSubtree := Value;

    if Running then
      RestartWatchThread;

    Change;
  end;
end;


procedure TIEFolderWatch.Start;
begin
  if FPath = '' then
    raise Exception.Create('No Path!');

  if not Running then
  begin
    AllocWatchThread;
    Change;
  end;
end;

procedure TIEFolderWatch.Stop;
begin
  if Running then
  begin
    ReleaseWatchThread;
    Change;
  end;
end;

procedure TIEFolderWatch.SetPath(const Value: string);
begin
  if StrIComp(PChar(Trim(Value)), PChar(FPath)) <> 0 then
  begin
    FPath := Trim(Value);

    if Running then
      RestartWatchThread;

    Change;
  end;
end;

procedure TIEFolderWatch.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TIEFolderWatch.Notify(const Action: Integer; const FileName: string);
begin
  if Assigned(FOnNotify) then
    if TWatchAction(Action - 1) in FWatchActions then
     FOnNotify(Self, TWatchAction(Action - 1), IncludeTrailingBackSlash(FPath) + FileName);
end;


initialization
  OleInitialize(nil);

finalization
  OleUninitialize;



{!!
<FS>iexWindowsFunctions

<FN>iexWindowsFunctions.pas provides access to Windows API methods for file and folder management. It is mainly used for <A TImageEnFolderMView>.

<FM>CLASSES<FN>
<TABLE2>
<R> <C_IMG_CLASS> <C><A TIEFileDragDrop></C> </R>
<R> <C_IMG_CLASS> <C><A TIEFolderWatch></C> </R>
</TABLE>

<FM>FILE OPERATION FUNCTIONS<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsCopy></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsMove></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsErase></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsLaunchFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsRename></C> </R>
</TABLE>

<FM>CLIPBOARD FUNCTIONS<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A CutFilesToClipboard></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A CopyFilesToClipboard></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A PasteFilesFromClipboard></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A CanPasteFilesFromClipboard></C> </R>
</TABLE>

<FM>SPECIAL FOLDER FUNCTIONS<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsDesktopFolder></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsMyVideosFolder></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsMyMusicFolder></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsMyPicturesFolder></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsMyDocumentsFolder></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsLocalAppDataFolder></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsProgramFilesFolder></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A GetWindowsSpecialFolder></C> </R>
</TABLE>

<FM>OTHER FUNCTIONS<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsSelectDirectory></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WindowsAddToRecentDocs></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A PopupSystemMenu></C> </R>
</TABLE>

!!}


end.
