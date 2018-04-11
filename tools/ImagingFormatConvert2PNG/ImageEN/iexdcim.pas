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

unit iexDCIM;
// NPC: 15/11/11

{$R-}
{$Q-}

{$I ie.inc}

interface


uses
  Windows, Classes, Sysutils, Graphics,
  {$ifdef IEHASUITYPES} System.UITypes, {$endif}
  comctrls, hyieutils, hyiedefs;


{$IFDEF IEINCLUDEIEXACQUIRE}


type

{!!
<FS>TIEDcimAcquire

<FM>Description<FN>
TIEDcimAcquire class is used by <A TIEAcquireParams> to retrieve images from digital camera cards and connected camera devices (which appear as USB drives).

<FM>Properties<FN>  
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEDcimAcquire.AcquireFormats></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEDcimAcquire.Aborting></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEDcimAcquire.SelectedSource></C> </R>
</TABLE>

<FM>Methods<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEDcimAcquire.Create></C> </R>
<R> <C_IMG_METHOD> <C><A TIEDcimAcquire.Destroy></C> </R>
<R> <C_IMG_METHOD> <C><A TIEDcimAcquire.Acquire></C> </R>
<R> <C_IMG_METHOD> <C><A TIEDcimAcquire.CopyImages></C> </R>
<R> <C_IMG_METHOD> <C><A TIEDcimAcquire.GetListOfDcimSources></C> </R>
<R> <C_IMG_METHOD> <C><A TIEDcimAcquire.SetSelectedSource></C> </R>
</TABLE>
!!}


  TIEDcimAcquire = class
  private
    fOwner: TComponent;
    FAcquireFormats : string;
    FAborting : Boolean;
    FSelectedSource : string;
  public

{!!
<FS>TIEDcimAcquire.Create

<FM>Declaration<FC>
}
    constructor Create(Owner: TComponent);
{!!}


{!!
<FS>TIEDcimAcquire.Destroy

<FM>Declaration<FC>
}
    destructor Destroy; override;
{!!}

{!!
<FS>TIEDcimAcquire.AcquireFormats

<FM>Declaration<FC>
property AcquireFormats : String; (Read/Write)

<FM>Description<FN>
A list of extensions for file types that TIEDcimAcquire will retrieve when calling <A TIEDcimAcquire.Acquire> or <A TIEDcimAcquire.CopyImages>, e.g. '*.jpg;*.jpeg;*.bmp;' or '*.*'

If AcquireFormats is '' then all supported files types (including video types) are retrieved.
!!}         
    property AcquireFormats : string read FAcquireFormats write FAcquireFormats;

{!!
<FS>TIEDcimAcquire.Aborting

<FM>Declaration<FC>
property Aborting : Boolean; (Read/Write)

<FM>Description<FN>
Set to false to cancel the acquisition of images (e.g. during the OnProgress event)
!!}
    property Aborting : Boolean read FAborting write FAborting;

    function GetListOfDcimSources(ssDest : TStrings; bPathsOnly : Boolean) : Integer;

    // Single Image Acquisition
    function Acquire(DestBitmap : TIEBitmap; DestIOParamsVals: TObject = nil) : boolean; overload;

    // Muliple Image Acquisition
    function Acquire(OnGetImage: TIEMultiCallBack; OnProgress: TIEProgressEvent = nil) : boolean; overload;

    // Internal usage only
    function AcquireEx(bMultiple  : Boolean;
                       DestBitmap : TIEBitmap;
                       DestIOParamVals : TObject;
                       OnGetImage  : TIEMultiCallBack;
                       const sDestPath : string;
                       bDeleteFromSource : Boolean;
                       OnProgress: TIEProgressEvent) : Boolean;

    // Special Usage
    function CopyImages(const sDestPath : string; OnProgress: TIEProgressEvent = nil; bDeleteFromSource : Boolean = False) : boolean;

{!!
<FS>TIEDcimAcquire.SelectedSource

<FM>Declaration<FC>
property SelectedSource : string; (Read only)

<FM>Description<FN>
Return the path of the selected DCIM folder which will be used for subsequent calls to <A TIEDcimAcquire.Acquire> or <A TIEDcimAcquire.CopyImages>.

<FM>See Also<FN>
- <A TIEDcimAcquire.SetSelectedSource>
!!}
    property SelectedSource : string read FSelectedSource;

    function SetSelectedSource(sLocation : string) : boolean;

    function IsValidDcimLocation(sLocation : string) : boolean;
  end;


implementation

uses
  ImageEnIO, Dialogs, iexAcquire, IEMIO, iesettings;
  
const
  Digital_Camera_Folder_Name = 'DCIM';

constructor TIEDcimAcquire.Create(Owner: TComponent);
begin
  inherited Create;
  fOwner := Owner;
  FAborting := False;
end;

destructor TIEDcimAcquire.Destroy;
begin
  //
  inherited;
end;

          
{!!
<FS>TIEDcimAcquire.GetListOfDcimSources

<FM>Declaration<FC>
function GetListOfDcimSources(ssDest : TStrings; bPathsOnly : Boolean) : Integer;

<FM>Description<FN>
Locate all drives (camera cards or connected camera devices which appear as USB drives) containing DCIM folders and add them to ssDest.

if bPathsOnly is true then only the DCIM folders are added to the list, e.g. G:\DCIM, H:\DCIM, etc.

Otherwise a raw string is used which provides full device details in the format:

Device Name||Index of Location||API||Device Type

E.g. My Cool Scanner||3||TWN||SCN

The raw string can be decoded using <A StrToAcquireSource>.

Note: ssDest is NOT cleared before the fill!
!!}
function TIEDcimAcquire.GetListOfDcimSources(ssDest : TStrings; bPathsOnly : Boolean) : Integer;
var
  Drives : Array [0..MAX_PATH] of char;
  ADrive : PChar;   
  ErrorMode: word;
  sPath: string;
begin
    { turn off critical errors }
    ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
  try 
    if (FSelectedSource <> '') and (IEDirectoryExists(FSelectedSource) = False) then
      FSelectedSource := '';

    GetLogicalDriveStrings(MAX_PATH, @Drives[0]);
    ADrive := @Drives[0];
    while lstrlen(ADrive) <> 0 do
    begin
      ADrive[2] := #0;
      sPath := IncludeTrailingBackslash(string(ADrive) + '\' + Digital_Camera_Folder_Name);              
      if (ADrive <> 'A:') and (ADrive <> 'B:') and (GetDriveType(PChar(ADrive)) = DRIVE_REMOVABLE) and IEDirectoryExists(sPath) then
      begin
        If FSelectedSource = '' then
          FSelectedSource := sPath;
        if bPathsOnly then
          ssDest.add(sPath)
        else
          ssDest.add(AcquireSourceToStr(IEAcquireSource(ieaDCIM,
                                                        sPath,
                                                        AnsiString(format('Device on %s:\', [uppercase(ADrive[0])])),
                                                        ieadDrive)));
      end;

      // Move ahead to next drive (lstrlen + 2 because we've added a zero byte)
      ADrive := ADrive + lstrlen(ADrive) + 2;
    end;

    Result := ssDest.Count;
  finally
    { restore old error mode }
    SetErrorMode(ErrorMode);
  end;
end;



procedure RetrieveFileList(ssDest : TStrings; const sFolder, sExtensions : string; bIncludeSubFolders : boolean = True);

  function _ValidFile(dwFileAttributes : DWord): boolean;
  const
    INCLUDE_HIDDEN_FILES = False;
    INCLUDE_SYSTEM_FILES = False;
  begin
    result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY= 0) and
             (INCLUDE_HIDDEN_FILES or (dwFileAttributes and FILE_ATTRIBUTE_HIDDEN= 0)) and
             (INCLUDE_SYSTEM_FILES or (dwFileAttributes and FILE_ATTRIBUTE_SYSTEM= 0));
  end;

  procedure _ProcessFolder(sPath : string);
  const
    Period = '.';
    ALL_FILES_SPEC = '*.*';
  var
    ErrorMode  : word;
    FindData   : TWin32FindData;
    FindHandle : THandle;
    Done       : Boolean;
    NewDir     : array[0..MAX_PATH] of Char;
    ADir       : array[0..MAX_PATH] of Char;
    FileName   : array[0..MAX_PATH] of char;
    ScanDir    : array[0..MAX_PATH] of char;
  begin
    { turn off critical errors }
    ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
    try
      // Add backslash  to the currently scanned dir '\'
      lstrcpy(ADir, PChar(IncludeTrailingBackslash(sPath)));
      lstrcpy(ScanDir, ADir);
      lstrcat(ScanDir, ALL_FILES_SPEC);

      // Scan for files this in the current directory
      FindHandle := Windows.FindFirstFile(ScanDir, FindData);
      try
        Done := (FindHandle = INVALID_HANDLE_VALUE);
        while not Done do
        begin
          // FOLDER
          if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
             (FindData.cFileName[0] <> Period) and (bIncludeSubFolders) then
          begin
            lstrcpy(NewDir, ADir);
            lstrcat(NewDir, FindData.CFileName);
            _ProcessFolder(NewDir);
          end
          else
          // FILE
          if _ValidFile(FindData.dwFileAttributes) then
          begin
            lstrcpy(FileName, ADir);
            lstrcat(FileName, FindData.CFileName);
            if IEFilenameInExtensions(Filename, sExtensions) then
              ssDest.Add(Filename);
          end;
          Done := not Windows.FindNextFile(FindHandle, FindData);
        end;

      finally
        Windows.FindClose(FindHandle);
      end;

    finally
      SetErrorMode(ErrorMode);
    end;
  end;

begin
  ssDest.clear;
  _ProcessFolder(sFolder);
end;


 // result is true if an image was retrieved
function TIEDcimAcquire.AcquireEx(bMultiple  : Boolean;
                                  DestBitmap : TIEBitmap;
                                  DestIOParamVals : TObject;
                                  OnGetImage  : TIEMultiCallBack;
                                  const sDestPath : string;    
                                  bDeleteFromSource : Boolean;
                                  OnProgress: TIEProgressEvent) : Boolean;

  procedure ShowProgress(iPercent : Integer);
  begin
    if bMultiple and assigned(OnProgress) then
      OnProgress(Self, iPercent);
  end;

  function ProcessImage(const sFilename : string): boolean;
  var
    TempParams : TIOParamsVals;
  begin
    Result := False;        
    TempParams := TIOParamsVals.Create(nil);
    try
      if assigned(DestBitmap) then
      begin
        DestBitmap.Read(sFilename, TempParams);

        if Assigned(DestIOParamVals) and (DestIOParamVals is TIOParamsVals) then
          TIOParamsVals(DestIOParamVals).Assign(TempParams);
        if assigned(OnGetImage) then
          OnGetImage(DestBitmap, DestIOParamVals, TempParams.DpiX, TempParams.DpiY);
      end;

      if sDestPath <> '' then
      begin
        if Windows.CopyFile(Pchar(sFilename), pChar(IncludeTrailingBackslash(sDestPath) + ExtractFileName(sFilename)), FALSE) = False then
          raise Exception.Create('Unable to copy: ' + sFilename);
        if bDeleteFromSource then
          Windows.DeleteFile(Pchar(sFilename));
      end;
      result := true;
    finally
      FreeAndNil(TempParams);
    end;
  end;

var
  bImageFound: Boolean;
  ssFileList: TStringList;
  i: Integer;
  sAcquireFormats: string;
  bNeedCreateBitmap: Boolean;
begin
  result := False;
  bNeedCreateBitmap := (Assigned(DestBitmap) = False) and Assigned(OnGetImage);
  FAborting := False;
  bImageFound := False;
  ssFileList := TStringList.create;
  if bNeedCreateBitmap then
    DestBitmap := TIEBitmap.create;
  try
    try
      ShowProgress(0);

      // Get file types to retrieve
      sAcquireFormats := FAcquireFormats;
      if sAcquireFormats = '' then
        sAcquireFormats := GetAllSupportedFileExtensions(True, True);

      // Retrieve list of images
      RetrieveFileList(ssFileList, FSelectedSource, sAcquireFormats);
      if ssFileList.count = 0 then
      begin
        MessageDlg('No images were found on the specified device.', mtError, [mbok], 0);
        exit;
      end;

      for i := 0 to ssFileList.count - 1 do
      begin      
        ShowProgress(MulDiv(i, 100, ssFileList.count));
        if ProcessImage(ssFileList[i]) then
          bImageFound := True;
        if (bMultiple = False) and bImageFound then
          Break;
        if FAborting then
          Break;
      end;
      result := bImageFound;
    except
      on E:Exception do
      begin
        MessageDlg('Error encountered retrieving your images: ' + e.message, mtError, [mbok], 0);
        Result := False;
      end;
    end;
  finally
    ssFileList.free;   
    if bNeedCreateBitmap then
      DestBitmap.Free;
  end;
end;

{!!
<FS>TIEDcimAcquire.Acquire

<FM>Declaration<FC>
function Acquire(DestBitmap : <A TIEBitmap>; DestIOParamsVals: TObject = nil) : boolean; overload;
function Acquire(OnGetImage: <A TIEMultiCallBack>; OnProgress: <A TIEProgressEvent>  = nil) : boolean; overload;

<FM>Description<FN>
Retrieve files of the formats specified in <A TIEDcimAcquire.AcquireFormats> that are located in the folder specified by <A TIEDcimAcquire.SelectedSource> (and all sub-folders).

<FB>** Generally you should NOT call this method directly. Use <A TImageEnIO.Acquire> or <A TImageEnMIO.Acquire> Instead **<FN>

Acquiring a single image:   
function Acquire(DestBitmap : <A TIEBitmap>; DestIOParamsVals: TObject = nil) : boolean;
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>DestBitmap</C> <C>The <A TIEBitmap> which will be filled with the acquired image</C> </R>
<R> <C>DestIOParamVals</C> <C>A <A TIOParamsVals> object which will be filled with the parameters of the acquired image (optional)</C> </R>
</TABLE>

Acquiring multiple images:
function Acquire(OnGetImage: <A TIEMultiCallBack>; OnProgress: <A TIEProgressEvent>  = nil) : boolean;
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>ImageAcquireCallBack</C> <C>Event to call for every acquired image</C> </R>
<R> <C>OnProgress</C> <C>Event to display acquisition progress and allow <L TIEDcimAcquire.Aborting>aborting</L> (optional)</C> </R>
</TABLE>

<FM>Example<FC>
// Retrieve the first image from the first connected camera card
ssDcimDevices := TStringList.create;
GetListOfDcimSources(ssDcimDevices, False);
if ssDcimDevices.count > 0 then
begin
  SetSelectedSource(StrToAcquireSource(ssDcimDevices[0]));
  Acquire(ImageEnView1.IEBitmap);
  ImageEnView1.Update;
end;
ssDcimDevices.Free;

// Retrieve all images from the first connected camera card
// Assumes you have created an OnGetImage event that does something with the retrieved images
ssDcimDevices := TStringList.create;
GetListOfDcimSources(ssDcimDevices, False);
if ssDcimDevices.count > 0 then
begin
  SetSelectedSource(StrToAcquireSource(ssDcimDevices[0]));
  Acquire(OnGetImage);
end;
ssDcimDevices.Free;
!!}

// Single Image Acquisition
function TIEDcimAcquire.Acquire(DestBitmap : TIEBitmap; DestIOParamsVals: TObject = nil) : boolean;
begin
  Result := AcquireEx(False, DestBitmap, DestIOParamsVals, nil, '', False, nil);
end;

// Muliple Image Acquisition
function TIEDcimAcquire.Acquire(OnGetImage: TIEMultiCallBack; OnProgress: TIEProgressEvent = nil) : boolean;
var
  ABitmap : TIEBitmap;
begin
  ABitmap := TIEBitmap.Create;
  try
    Result := AcquireEx(True, ABitmap, nil, OnGetImage, '', False, OnProgress);
  finally
    FreeAndNil(ABitmap);
  end;
end;


{!!
<FS>TIEDcimAcquire.CopyImages

<FM>Declaration<FC>
function CopyImages(const sDestPath : string; OnProgress: TIEProgressEvent = nil) : boolean;

<FM>Description<FN>
Copy all files of the formats specified in <A TIEDcimAcquire.AcquireFormats> that are located in the folder specified by sDCIMPath (and all sub-folders) to the path, sDestPath.
If an OnProgress event is passed then a progress dialog can be displayed to the user and retrieval cancelled by setting <A TIEDcimAcquire.Aborting> to True.
If bDeleteFromSource is true then the images will be removed from the source after retrieval

<FB>Warning: Ensure sDestPath points to a valid path.  You should create a new folder for the retrieval as this procedure will OVERWRITE ALL EXISTING FILES WITHOUT WARNING<FN>

<FM>Example<FC>
// Copy all images from the first connected camera card
ssDcimDevices := TStringList.create;
GetListOfDcimSources(ssDcimDevices, False);
if ssDcimDevices.count > 0 then
begin
  CheckCreateNewFolder(sNewFolder);
  SetSelectedSource(StrToAcquireSource(ssDcimDevices[0]));
  Acquire(sNewFolder);
end;
ssDcimDevices.Free;
!!}
function TIEDcimAcquire.CopyImages(const sDestPath : string; OnProgress: TIEProgressEvent = nil; bDeleteFromSource : Boolean = False) : boolean;
var
  ABitmap : TIEBitmap;
begin
  ABitmap := TIEBitmap.Create;
  try
    Result := AcquireEx(True, ABitmap, nil, nil, sDestPath, bDeleteFromSource, OnProgress);
  finally
    FreeAndNil(ABitmap);
  end;
end;


{!!
<FS>TIEDcimAcquire.SetSelectedSource

<FM>Declaration<FC>
function SetSelectedSource(sLocation : string) : boolean;

<FM>Description<FN>
Set the selected DCIM folder which will be used for subsequent calls to <A TIEDcimAcquire.Acquire> or <A TIEDcimAcquire.CopyImages>.  Result is true if the folder exists.

sLocation can be one of the following:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>Blank</C> <C>The default device will be selected</C> </R>
<R> <C>Path</C> <C>The path of a DCIM folder, e.g. 'I:\DCIM\'</C> </R>
<R> <C>Drive Letter</C> <C>The letter of a connected camera card or device containing a DCIM folder, e.g. 'I'</C> </R>
<R> <C>Raw device string</C> <C>A string describing a device as returned by <A TIEDcimAcquire.GetListOfDcimSources></C> </R>
</TABLE>
        
<FM>See Also<FN>
- <A TIEDcimAcquire.SelectedSource>

<FM>Example<FC>
// Retrieve all images from the first connected camera card
if SetSelectedSource('') then
  Acquire(OnGetImage);

// Acquire from the camera card on H drive
if SetAcquireSource(ieaDCIM, 'H') then
  Acquire(OnGetImage);
!!}
function TIEDcimAcquire.SetSelectedSource(sLocation : string) : boolean;

  function _SetToDefaultSource : Boolean;
  var
    ssTemp: TStringList;
  begin
    FSelectedSource := '';
    ssTemp := TStringList.create;
    try
      // during fill of list FSelectedSource will bet set if items are found
      GetListOfDcimSources(ssTemp, True);
    finally
      sstemp.free;
    end;
    Result := FSelectedSource <> '';
  end;

  function _SetToPath(const sPath : string) : Boolean;
  var
    ErrorMode  : word;
  begin
    Result := False;
    try
      { turn off critical errors }
      ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
      try
        if IEDirectoryExists(sPath) then
        begin
          Result := True;
          FSelectedSource := sPath;
        end;
      finally
        SetErrorMode(ErrorMode);
      end;
    except
      // UNEXPECTED ERROR
    end;
  end;

begin
  if sLocation = '' then
  begin
    // Default Path
    Result := _SetToDefaultSource;
  end
  else
  if IsAcquireSourceStr(sLocation) then
  begin
    // Raw Device string
    Result := _SetToPath(StrToAcquireSource(sLocation).Location);
  end
  else
  if Length(sLocation) <= 3 then
  begin
    // Drive letter
    Result := _SetToPath(sLocation[1] + ':\' + Digital_Camera_Folder_Name);
  end
  else
  begin
    // Normal path  
    Result := _SetToPath(sLocation);
  end;

  // Make DCIM the API for subsequent calls to Acquire
  {$IFDEF IEINCLUDEMULTIVIEW}
  If Result and (fOwner is TImageEnMIO) then
    (fOwner as TImageEnMIO).AcquireParams.fSelectedSourceAPI := ieaDCIM
  else
  {$ENDIF}
  If Result and (fOwner is TImageEnIO) then
    (fOwner as TImageEnIO).AcquireParams.fSelectedSourceAPI := ieaDCIM
end;



function TIEDcimAcquire.IsValidDcimLocation(sLocation : string) : boolean;    
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := False;

  If Pos(Uppercase(Digital_Camera_Folder_Name), Uppercase(sLocation)) > 0 then
  begin
    Handle := FindFirstFile(PChar(ExcludeTrailingBackSlash(sLocation)), FindData);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      Result := (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0;
    end;
  end;
end;

{$ELSE} // IEINCLUDEIEXACQUIRE
implementation
{$ENDIF}

end.
