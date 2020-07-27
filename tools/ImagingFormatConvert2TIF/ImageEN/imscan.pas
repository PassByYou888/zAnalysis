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
File version 1011
*)

unit imscan;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEIEXACQUIRE}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ImageEnIO, ietwain, hyiedefs, hyieutils, dialogs;

type
  TIETWCloseCallBack = procedure of object;

function IETW_SelectImageSource(var SelectedSourceName: AnsiString; TwainShared: PIETwainShared; callwnd: HWND): boolean;
function IETW_Acquire(Bitmap: TIEBitmap; multi: boolean; MultiCallBack: TIEMultiCallBack; Params: TIETwainParams; IOParams: TIOParamsVals; var Progress: TProgressRec; TwainShared: PIETwainShared; callwnd: HWND; DoNativePixelFormat: boolean): boolean;
function IETW_GetSourceList(SList: TList; TwainShared: PIETwainShared; callwnd: HWND): boolean;
function IETW_GetCapabilities(Params: TIETwainParams; var Capabilities: TIETWSourceCaps; setcap: boolean; TwainShared: PIETwainShared; callwnd: HWND): boolean;
function IETW_GetDefaultSource(TwainShared: PIETwainShared; callwnd: HWND): AnsiString;
procedure IETW_FreeResources(TwainShared: PIETwainShared; callwnd: HWND);

// new implementation
function IETWAINAcquireOpen(CloseCallBack: TIETWCloseCallBack; MultiCallBack: TIEMultiCallBack; Params: TIETwainParams; TwainShared: PIETwainShared; IOParams: TIOParamsVals; parent: TWinControl; DoNativePixelFormat: boolean): pointer;
procedure IETWAINAcquireClose(var grec: pointer);

implementation

uses ImageEnProc, forms, iesettings;

{$R-}

type

  ErrorDetail = (
    ED_NONE,
    ED_START_TRIPLET_ERRS,
    ED_CAP_GET,       // MSG_GET triplet on a capability failed
    ED_CAP_SET,       // MSG_SET triplet on capability failed
    ED_DSM_FAILURE,   // TWAIN DSM returned TWRC_FAILURE
    ED_DS_FAILURE,    // source returned TWRC_FAILURE
    ED_END_TRIPLET_ERRS,
    ED_NOT_STATE_4,   // operation invoked in wrong state
    ED_NULL_HCON,     // MSG_GET returned a null container handle
    ED_BAD_HCON,      // MSG_GET returned an invalid/unlockable container handle
    ED_BAD_CONTYPE,   // returned container ConType is not valid.
    ED_BAD_ITEMTYPE,  // returned container ItemType is not valid.
    ED_CAP_GET_EMPTY, // returned container has 0 items.
    ED_CAP_SET_EMPTY  // trying to restrict a cap to empty set
    );

  TIEDPI = record
    xdpi: integer;
    ydpi: integer;
  end;
  PIEDPI = ^TIEDPI;

const
  {$ifdef WIN64}
  DSM_FILENAME:   AnsiString = 'System32\TWAINDSM.dll';
  {$else}
  DSM_FILENAME:   AnsiString = 'TWAIN_32.DLL';
  {$endif}
  DSM_ENTRYPOINT: AnsiString = 'DSM_Entry';

  TWAIN_PRESESSION = 1;     // source manager not loaded
  TWAIN_SM_LOADED = 2;      // source manager loaded
  TWAIN_SM_OPEN = 3;        // source manager open
  TWAIN_SOURCE_OPEN = 4;    // source open but not enabled
  TWAIN_SOURCE_ENABLED = 5; // source enabled to acquire
  TWAIN_TRANSFER_READY = 6; // image ready to transfer
  TWAIN_TRANSFERRING = 7;   // image in transit

  TWAIN_BW = $0001;       // 1-bit per pixel, B&W      (== TWPT_BW)
  TWAIN_GRAY = $0002;     // 1, 4, or 8-bit grayscale  (== TWPT_GRAY)
  TWAIN_RGB = $0004;      // 24-bit RGB color          (== TWPT_RGB)
  TWAIN_PALETTE = $0008;  // 1, 4, or 8-bit palette    (== TWPT_PALETTE)
  TWAIN_ANYTYPE = $0000;  // any of the above

type

  tgrec = record
    nState: integer;            // TWAIN state (per the standard)
    hDSMLib: THANDLE;           // handle of DSM
    DSM_Entry: TDSMEntryProc;   // entry point of Data Source Manager (TWAIN.DLL)
    hwndSM: HWND;
    rc: TW_INT16;               // result code
    AppId: TW_IDENTITY;
    SourceId: TW_IDENTITY;      // source identity structure
    twUI: TW_USERINTERFACE;
    nErrDetail: ErrorDetail;    // detailed error code
    nErrRC, nErrCC: word;       // result code and condition code for last error
    bHideUI: boolean;           // allow source u/i to be hidden
    pendingXfers: TW_PENDINGXFERS;
    gmulti: boolean;
    MultiCallBack: TIEMultiCallBack;
    fAborting: boolean;         // used only if bHideUI=true
    TWParams: TIETwainParams;
    IOParams: TIOParamsVals;
    NativePixelFormat: boolean;  // copy of IOParams.IsNativePixelFormat or TImageEnMIO.NativePixelFormat, you should read this value instead of IOParams.IsNativePixelFormat
    TransferMode: (tmNative, tmBuffered, tmFile);
    transferdone: boolean;      // true on transfer completed
    closedsok: boolean;         // true when MSG_CLOSEDSOK has been received
    breakmodalloop: boolean;
    fBitmap: TIEBitmap;         // bitmap to fill
    Progress: PProgressRec;
    PTwainShared: PIETwainShared;
    actwnd: HWND;
    callwnd: HWND;
    proxywnd: HWND;
    BWToInvert: boolean;        // the black/white image need to be inverted
    uionly: boolean;            // IETW_EnableSource will use MSG_ENABLEDSUIONLY instead of MSG_ENABLEDS
    // new implementation
    ProxyWin: TWinControl;
    modal: boolean;
    sending: boolean;
    fclosecallback: TIETWCloseCallBack;
    fWindowList: pointer;
  end;
  pgrec = ^tgrec;

function GetUINT16asInteger(var grec: tgrec; ilist: TIEIntegerList; cap: TW_UINT16): boolean; forward;

procedure IETW_EmptyMessageQueue(var grec: tgrec); forward;

procedure LogWrite(ss: AnsiString);
begin
  if iegTwainLogName <> '' then
  begin
    closefile(iegTwainLogFile);
    assignfile(iegTwainLogFile, string(iegTwainLogName));
    append(iegTwainLogFile);
    WriteLn(iegTwainLogFile, AnsiString(datetostr(date)) + ' ' + AnsiString(timetostr(time)) + ' : ' + ss);
    Flush(iegTwainLogFile);
  end;
end;

function ResultToStr(rsl: TW_UINT16): AnsiString;
begin
  case rsl of
    TWCC_BADCAP: Result := 'Capability not supported by Source or operation (get, set) is not supported on capability, or capability had dependencies on other capabilities and cannot be operated upon at this time';
    TWCC_BADDEST: Result := 'Unknown destination in DSM_Entry.';
    TWCC_BADPROTOCOL: Result := 'Unrecognized operation triplet.';
    TWCC_BADVALUE: Result := 'Data parameter out of supported range.';
    TWCC_BUMMER: Result := 'General failure. Unload Source immediately.';
    TWCC_CAPUNSUPPORTED: Result := 'Capability not supported by Source.';
    TWCC_CAPBADOPERATION: Result := 'Operation not supported on capability.';
    TWCC_CAPSEQERROR: Result := 'Capability has dependencies on other capabilities and cannot be operated upon at this time.';
    TWCC_DENIED: Result := 'File System operation is denied (file is protected).';
    TWCC_PAPERDOUBLEFEED: Result := 'Transfer failed because of a feeder error';
    TWCC_FILEEXISTS: Result := 'Operation failed because file already exists.';
    TWCC_FILENOTFOUND: Result := 'File not found.';
    TWCC_LOWMEMORY: Result := 'Not enough memory to complete operation.';
    TWCC_MAXCONNECTIONS: Result := 'Source is connected to maximum supported number of applications.';
    TWCC_NODS: Result := 'Source Manager unable to find the specified Source.';
    TWCC_NOTEMPTY: Result := 'Operation failed because directory is not empty.';
    TWCC_OPERATIONERROR: Result := 'Source or Source Manager reported an error to the user and handled the error; no application action required.';
    TWCC_PAPERJAM: Result := 'Transfer failed because of a feeder error';
    TWCC_SEQERROR: Result := 'Illegal operation for current Source Manager Source state.';
    TWCC_SUCCESS: Result := 'Operation worked.';
  else
    Result := 'Unknown Condition ' + IEIntToStr(rsl);
  end;
end;

function IETW_DS(var grec: tgrec; dg: TW_UINT32; dat: TW_UINT16; msg: TW_UINT16; pd: TW_MEMREF): boolean;
var
  FPUControlWord: Word;
begin
  with grec do
  begin
    rc := TWRC_FAILURE;
    if (@DSM_Entry <> nil) then
    begin
      try
        FPUControlWord := Get8087CW();
        try
          Set8087CW($133F);
          rc := DSM_Entry(@AppId, @SourceId, dg, dat, msg, pd);
        finally
          Set8087CW(FPUControlWord);
        end;
        if (rc <> TWRC_SUCCESS) and (dat <> DAT_EVENT) and assigned(TWParams) then
        begin
          TWParams.LastError    := rc;
          TWParams.LastErrorStr := ResultToStr(rc);
          LogWrite('IETW_DS : ' + TWParams.LastErrorStr);
        end;
      except
        on E:Exception do
          LogWrite('IETW_DS : Exception -> ' + AnsiString(E.Message));
      end;
    end;
    result := (rc = TWRC_SUCCESS);
  end;
end;


procedure Init_grec(var grec: tgrec);
begin
  grec.actwnd := windows.GetActiveWindow;
  grec.fAborting := false;
  grec.nState := 1;
  grec.TWParams := nil;
  grec.BWToInvert := false;
  grec.sending := false;
  grec.uionly := false;
  with grec.AppId do
  begin
    id := 0;
    with version do
    begin
      MajorNum := 1;
      MinorNum := 0;
      Language := TWLG_USERLOCALE;  // 3.0.3
      Country := TWCY_USA;
      Info := ' ' + #0;
    end;
    ProtocolMajor := TWON_PROTOCOLMAJOR;
    ProtocolMinor := TWON_PROTOCOLMINOR;
    SupportedGroups := DG_IMAGE or DG_CONTROL;
    Manufacturer := ' ' + #0;
    ProductFamily := ' ' + #0;
    ProductName := ' ' + #0;
  end;
  grec.fWindowList := nil;
  grec.NativePixelFormat := false;
end;

procedure GetCustomData(var grec: tgrec);
var
  customData: TW_CUSTOMDSDATA;
  dataPtr: pbyte;
begin
  FillChar(customData, sizeof(TW_CUSTOMDSDATA), 0);
  if IETW_DS(grec, DG_CONTROL, DAT_CUSTOMDSDATA, MSG_GET, @customData) and
     (customData.InfoLength>0) and
     (GlobalSize(customData.hData)>=customData.InfoLength) then
  begin
    dataPtr := GlobalLock(customData.hData);
    try
      grec.TWParams.SourceSettings.Clear;
      grec.TWParams.SourceSettings.Write(pbyte(dataPtr)^, customData.InfoLength);
    finally
      GlobalUnlock(customData.hData);
      GlobalFree(customData.hData);
    end;
  end;
end;

procedure SetCustomData(var grec: tgrec);
var
  customData: TW_CUSTOMDSDATA;
  dataSize: integer;
  dataPtr: pbyte;
begin
  dataSize := grec.TWParams.SourceSettings.Size;
  if dataSize>0 then
  begin
    customData.InfoLength := dataSize;
    customData.hData := GlobalAlloc(GHND, dataSize);
    dataPtr := GlobalLock(customData.hData);
    try
      CopyMemory(dataPtr, grec.TWParams.SourceSettings.Memory, dataSize);
      GlobalUnlock(customData.hData);
      IETW_DS(grec, DG_CONTROL, DAT_CUSTOMDSDATA, MSG_SET, @customData);
    finally
      GlobalFree(customData.hData);
    end;
  end;
end;

procedure Set_AppId(var grec: tgrec);
begin
  with grec.AppId do
  begin
    IEStrCopy(version.Info, PAnsiChar(grec.TWParams.AppVersionInfo)); // <?>
    version.Info[33] := #0;
    IEStrCopy(Manufacturer, PAnsiChar(grec.TWParams.AppManufacturer));
    Manufacturer[33] := #0;
    IEStrCopy(ProductFamily, PAnsiChar(grec.TWParams.AppProductFamily));
    ProductFamily[33] := #0;
    IEStrCopy(ProductName, PAnsiChar(grec.TWParams.AppProductName));
    ProductName[33] := #0;
    Version.Language := grec.TWParams.Language;  // 3.0.3
    Version.Country  := grec.TWParams.Country;   // 3.0.3
  end;
end;


function IETW_LoadSourceManager(var grec: tgrec): boolean;
var
  szSMDir: array[0..255] of AnsiChar;
  cc: integer;
begin
  with grec do
  begin
    LogWrite('IETW_LoadSourceManager');
    if (nState >= 2) then
    begin
      LogWrite('  IETW_LoadSourceManager : already loaded');
      result := TRUE; // DSM already loaded
      exit;
    end;
    if PTwainShared^.hDSMLib <> 0 then
    begin
      hDSMLib := PTwainShared^.hDSMLib;
      DSM_Entry := PTwainShared^.DSM_Entry;
      result := TRUE;
      nState := 2;
      LogWrite('  IETW_LoadSourceManager : already loaded');
      exit;
    end;
    GetWindowsDirectoryA(szSMDir, sizeof(szSMDir));
    cc := lstrlenA(@szSMDir);
    if (cc <> 0) and (szSMDir[cc - 1] <> ':') then
      lstrcatA(@szSMDir, '\');
    lstrcatA(@szSMDir, PAnsiChar(DSM_FILENAME));
    hDSMLib := 0;
    if IEFileExists(string(AnsiString(szSMDir))) then
      hDSMLib := LoadLibraryA(szSMDir);
    DSM_Entry := nil;
    if hDSMLib <> 0 then
    begin
      LogWrite('  IETW_LoadSourceManager : Load OK');
      DSM_Entry := TDSMEntryProc(GetProcAddress(hDSMLib, PAnsiChar(DSM_ENTRYPOINT)));
      if @DSM_Entry <> nil then
      begin
        nState := 2;
      end
      else
      begin
        FreeLibrary(hDSMLib);
        hDSMLib := 0;
      end
    end;
    result := (nState >= 2);

    PTwainShared^.hDSMLib := hDSMLib;
    PTwainShared^.DSM_Entry := DSM_Entry;
  end;
end;


// use Force=true to really unloadsourcemanager
function IETW_UnloadSourceManager(var grec: tgrec; force: boolean): boolean;
begin
  with grec do
  begin
    LogWrite('IETW_UnloadSourceManager');
    if force and (PTwainShared^.hDSMLib <> 0) then
    begin
      FreeLibrary(hDSMLib);
      hDSMLib := 0;
      DSM_Entry := nil;
      PTwainShared^.hDSMLib := 0;
      PTwainShared^.DSM_Entry := nil;
      nState := 1;
      LogWrite('  IETW_UnloadSourceManager : Unload OK');
    end
    else
    if (nState = 2) then
    begin
      if (hDSMLib <> 0) then
        hDSMLib := 0;
      DSM_Entry := nil;
      nState := 1;
      LogWrite('  IETW_UnloadSourceManager not Unloaded, for future uses');
    end;
    result := (nState = 1);
  end;
end;


function CreateProxyWindow(var grec: tgrec): HWND;
var
  mainwnd: HWND;
begin
  LogWrite('CreateProxyWindow');
  if grec.PTwainShared.hproxy <> 0 then
  begin
    result := grec.PTwainShared^.hproxy;
    LogWrite('  CreateProxyWindow : already created');
    exit;
  end;

  {$IFDEF OCXVERSION}
  mainwnd := HWND_DESKTOP;
  {$ELSE}
  mainwnd := grec.callwnd;
  if mainwnd = 0 then
    mainwnd := HWND_DESKTOP;
  {$ENDIF}

  {$WARNINGS OFF}
  // Here memory debuggers could show a memory leak: it is not true, if DestroyWindow is not called by ImageEn
  // it is called by parent window.
  result := CreateWindow('STATIC', 'Acquire Proxy', WS_POPUPWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
            CW_USEDEFAULT, CW_USEDEFAULT,
            mainwnd, 0, GetModuleHandle(nil), nil);
  if assigned(Application) then
    Application.ProcessMessages;
  grec.PTwainShared.hproxy := result;
  LogWrite('  CreateProxyWindow : created');
  {$WARNINGS ON}
end;


procedure DestroyProxyWindow(wnd: HWND; var grec: tgrec; force: boolean);
begin
  LogWrite('DestroyProxyWindow');
  if force then
  begin
    // the window could be destroyed by parent
    if IsWindow(grec.PTwainShared^.hproxy) then
      DestroyWindow(grec.PTwainShared.hproxy);
    grec.PTwainShared.hproxy := 0;
    LogWrite('  DestroyProxyWindow : destroyed');
  end
  else
  begin
    LogWrite('  DestroyProxyWindow : not destroyed, for future uses');
  end;
end;


function IETW_Mgr(var grec: tgrec; dg: TW_UINT32; dat: TW_UINT16; msg: TW_UINT16; pd: TW_MEMREF): boolean;
var
  FPUControlWord: Word;
begin
  with grec do
  begin
    rc := TWRC_FAILURE;
    if (@DSM_Entry <> nil) then
    begin
      try
        FPUControlWord := Get8087CW();
        try
          Set8087CW($133F);
          rc := DSM_Entry(@AppId, nil, dg, dat, msg, pd);
        finally
          Set8087CW(FPUControlWord);
        end;
        if (rc <> TWRC_SUCCESS) and assigned(TWParams) then
        begin
          TWParams.LastError := rc;
          TWParams.LastErrorStr := ResultToStr(rc);
          LogWrite('IETW_Mgr : ' + TWParams.LastErrorStr);
        end;
      except
      end;
    end;
    result := (rc = TWRC_SUCCESS);
  end;
end;


function IETW_OpenSourceManager(var grec: tgrec; hwnd: HWND): boolean;
begin
  with grec do
  begin
    LogWrite('IETW_OpenSourceManager');
    hwndSM := hwnd;
    if (IETW_Mgr(grec, DG_CONTROL, DAT_PARENT, MSG_OPENDSM, @hwndSM)) then
      nState := TWAIN_SM_OPEN;
    result := (nState >= TWAIN_SM_OPEN);
    if result then
      LogWrite('  IETW_OpenSourceManager : Ok')
    else
      LogWrite('  IETW_OpenSourceManager : FAILED!');
  end;
end;


function IETW_CloseSourceManager(var grec: tgrec; hwnd: HWND): boolean;
var
  hwnd32: TW_INT32;
begin
  with grec do
  begin
    LogWrite('IETW_CloseSourceManager');
    IETW_EmptyMessageQueue(grec);
    hwnd32 := hwnd;
    rc := TWRC_SUCCESS;
    if (IETW_Mgr(grec, DG_CONTROL, DAT_PARENT, MSG_CLOSEDSM, @hwnd32)) then
    begin
      nState := 2;
    end;
    result := (nState <= 2);
    if result then
      LogWrite('  IETW_CloseSourceManager : Ok')
    else
      LogWrite('  IETW_CloseSourceManager : FAILED!');
  end;
end;


function IETW_DisableSource(var grec: tgrec): boolean;
begin
  with grec do
  begin
    LogWrite('IETW_DisableSource');
    BreakModalLoop := true;
    if (nState = 5) then
    begin
      IETW_DS(grec, DG_CONTROL, DAT_USERINTERFACE, MSG_DISABLEDS, @twUI);
      nState := 4;
    end;
    {$IFDEF IETWAINTASKWINDOWS}
    if fWindowList <> nil then
      EnableTaskWindows(fWindowList);
    fWindowList := nil;
    {$ENDIF}
    IETW_EmptyMessageQueue(grec);
    result := (nState <= 4);
    if result then
      LogWrite('  IETW_DisableSource : Ok')
    else
      LogWrite('  IETW_DisableSource : FAILED!');
  end;
end;


function IETW_CloseSource(var grec: tgrec): boolean;
begin
  with grec do
  begin
    LogWrite('IETW_CloseSource');
    BreakModalLoop := true;
    rc := TWRC_SUCCESS;
    if (nState = 5) then
      IETW_DisableSource(grec);
    if (nState = 4) and (IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_CLOSEDS, @SourceId)) then
      nState := 3;
    result := (nState <= 3);
    if result then
      LogWrite('  IETW_CloseSource : Ok')
    else
      LogWrite('  IETW_CloseSource : FAILED!');
  end;
end;

// returns ProductName of selected source
function IETW_SelectImageSource(var SelectedSourceName: AnsiString; TwainShared: PIETwainShared; callwnd: HWND): boolean;
var
  NewSourceId: TW_IDENTITY;
  grec: tgrec;
  wnd: HWND;
begin
  result := false;

  if IEGlobalSettings().IsInsideTwain then
    exit;
  IEGlobalSettings().IsInsideTwain := true;

  try
    Init_grec(grec);
    grec.callwnd := callwnd;
    grec.PTwainShared := TwainShared;
    if IEGlobalSettings().ModelessSelectTwainSource then
      wnd := CreateProxyWindow(grec)
    else
      wnd := callwnd;
    grec.proxywnd := wnd;
    if IETW_LoadSourceManager(grec) then
    begin
      if IETW_OpenSourceManager(grec, wnd) then
      begin
        FillMemory(@NewSourceId, sizeof(NewSourceId), 0);
        IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_GETDEFAULT, @NewSourceId);
        // Post the Select Source dialog
        result := IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_USERSELECT, @NewSourceId);
        SelectedSourceName := NewSourceId.ProductName;
        IETW_CloseSourceManager(grec, wnd);
      end
      else
      begin
        if wnd <> callwnd then
          DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
        exit;
      end;
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
    end;
    if wnd <> callwnd then
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
  finally
    windows.SetActiveWindow(grec.actwnd);
    IEGlobalSettings().IsInsideTwain := false;
  end;
end;


procedure ClearError(var grec: tgrec);
begin
  grec.nErrDetail := ED_NONE;
end;


function IETW_GetResultCode(var grec: tgrec): word;
begin
  result := grec.rc;
end;


function IETW_GetConditionCode(var grec: tgrec): word;
var
  twStatus: TW_STATUS;
begin
  with grec do
  begin
    if nState >= 4 then
    begin
      // get source status if open
      IETW_DS(grec, DG_CONTROL, DAT_STATUS, MSG_GET, TW_MEMREF(@twStatus));
    end
    else
    if nState = 3 then
    begin
      // otherwise get source manager status
      IETW_Mgr(grec, DG_CONTROL, DAT_STATUS, MSG_GET, TW_MEMREF(@twStatus));
    end
    else
    begin
      // nothing open, not a good time to get condition code!
      result := TWCC_SEQERROR;
      exit;
    end;
    if rc = TWRC_SUCCESS then
    begin
      result := twStatus.ConditionCode;
      exit;
    end
    else
      result := TWCC_BUMMER;
  end;
end;


function RecordError(var grec: tgrec; ed: ErrorDetail): boolean;
begin
  with grec do
  begin
    if nErrDetail = ED_NONE then
    begin
      nErrDetail := ed;
      if (ed > ED_START_TRIPLET_ERRS) and (ed < ED_END_TRIPLET_ERRS) then
      begin
        nErrRC := IETW_GetResultCode(grec);
        nErrCC := IETW_GetConditionCode(grec);
      end
      else
      begin
        nErrRC := 0;
        nErrCC := 0;
      end;
    end;
    result := FALSE;
  end;
end;


function IETW_OpenSource(var grec: tgrec): boolean;
var
  src: pTW_IDENTITY;
  sn: AnsiString;
begin
  with grec do
  begin
    LogWrite('IETW_OpenSource');
    if (nState <> 3) then
    begin
      result := FALSE;
      LogWrite('  IETW_OpenSource : already loaded');
      exit;
    end;

    sn := TWParams.SourceName[TWParams.SelectedSource];

    if sn <> '' then
    begin
      // Find selected source by ProductName
      src := AllocMem(sizeof(TW_IDENTITY));
      try
        IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_GETFIRST, src);
        while rc <> TWRC_ENDOFLIST do
        begin
          if src^.ProductName = sn then
            break;
          ZeroMemory(src, sizeof(TW_IDENTITY));
          IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_GETNEXT, src);
        end;
        move(src^, SourceId, sizeof(TW_IDENTITY));
      finally
        FreeMem(src);
      end;
    end
    else
    begin
      // system default source
      SourceId.ProductName[0] := #0;
      SourceId.Id := 0;
    end;
    if (IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_OPENDS, @SourceId)) then
    begin
      nState := 4;
      SetCustomData(grec);
    end
    else
      RecordError(grec, ED_DSM_FAILURE);
    result := (nState = 4);
    if result then
      LogWrite('  IETW_OpenSource : Ok')
    else
      LogWrite('  IETW_OpenSource : FAILED!');
  end;
end;


function IETW_EnableSource(var grec: tgrec; hwnd: HWND): boolean;
begin
  with grec do
  begin
    LogWrite('IETW_EnableSource');
    if (nState <> 4) then
    begin
      result := FALSE;
      LogWrite('  IETW_EnableSource : already enabled');
      exit;
    end;
    twUI.ShowUI := not bHideUI;
    twUI.hParent := TW_HANDLE(hwnd);
    twUI.ModalUI := modal;
    {$IFDEF IETWAINTASKWINDOWS}
    fWindowList := DisableTaskWindows(0);
    {$ENDIF}
    LogWrite('  IETW_EnableSource : ShowUI=' + IEIntToStr(integer(twUI.ShowUI)) + ' hParent=' + IEIntToStr(twUI.hParent) + ' ModalUI=' + IEIntToStr(integer(twUI.ModalUI)));
    if uionly then
      IETW_DS(grec, DG_CONTROL, DAT_USERINTERFACE, MSG_ENABLEDSUIONLY, @twUI)
    else
      IETW_DS(grec, DG_CONTROL, DAT_USERINTERFACE, MSG_ENABLEDS, @twUI);
    if (rc = TWRC_FAILURE) or (rc=TWCC_NODS) then
    begin
      RecordError(grec, ED_DS_FAILURE);
    end
    else
      nState := 5;
    result := (nState = 5);
    if result then
      LogWrite('  IETW_EnableSource : Ok')
    else
      LogWrite('  IETW_EnableSource : FAILED!');
  end;
end;


function GetCapability(var grec: tgrec; var twCapability: TW_CAPABILITY; cap: TW_UINT16): boolean;
begin
  twCapability.Cap := cap;
  twCapability.ConType := TWON_DONTCARE16;
  twCapability.hContainer := 0;
  LogWrite('GetCapability : $' + IEIntToHex(cap, 4));
  IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_GET, @twCapability);
  result := grec.rc = TWRC_SUCCESS;
  if result then
    LogWrite('  GetCapability : Ok')
  else
    LogWrite('  GetCapability : FAILED!');
end;


function GetOneStringCapability(var grec: tgrec; outstr: AnsiString; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  ptr: PAnsiChar;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  pvalOneValue := GlobalLock(twCapability.hContainer);
  ptr := @(pvalOneValue^.Item);
  outstr := AnsiString(ptr);
  GlobalUnlock(twCapability.hContainer);
  GlobalFree(twCapability.hContainer);
end;


// Supported TW_ONEVALUE (current value)
function SetOneStringCapability(var grec: tgrec; value: AnsiString; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  ptr: PAnsiChar;
begin
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('SetOnStringCapability $' + IEIntToHex(cap, 4));
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  // write TW_ONEVALUE (current value only)
  value := IECopy(value, 1, 255);
  twCapability.ConType := TWON_ONEVALUE;
  twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE) + 256 - 4);
  try
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pvalOneValue^.ItemType := TWTY_STR255;
    ptr := @(pvalOneValue^.Item);
    IEStrCopy(ptr, PAnsiChar(value));
    GlobalUnLock(twCapability.hContainer);
    IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
    result := grec.rc = TWRC_SUCCESS;
  finally
    GlobalFree(twCapability.hContainer);
  end;
  if result then
    LogWrite('  SetOnStringCapability : Ok')
  else
    LogWrite('  SetOnStringCapability : FAILED!')
end;


function IETW_AbortAllPendingXfers(var grec: tgrec): boolean;
begin
  with grec do
  begin
    LogWrite('IETW_AbortAllPendingXfers');
    breakmodalloop := true;
    if (nState = 7) and IETW_DS(grec, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @pendingXfers) then
    begin
      if pendingXfers.Count <> 0 then
        nState := 6
      else
        nState := 5;
    end;
    if (nState = 6) and (IETW_DS(grec, DG_CONTROL, DAT_PENDINGXFERS, MSG_RESET, @pendingXfers)) then
    begin
      nState := 5;
    end;
    IETW_EmptyMessageQueue(grec);
    result := (nState <= 5);
    if result then
      LogWrite('  IETW_AbortAllPendingXfers : Ok')
    else
      LogWrite('  IETW_AbortAllPendingXfers : FAILED!')
  end;
end;


// supported 1bit(black/write), 8bit(grayscale), 24bit(truecolor)
procedure CopyBuffer(var grec: tgrec; Bitmap: TIEBitmap; const twImageInfo: TW_IMAGEINFO; const imxfer: TW_IMAGEMEMXFER; LockMemory: boolean);
var
  src, dst: pbyte; // source buffer
  sinc: integer;   // source row length DWORDed
  pb: pbyte;       // dest buffer
  row, col: integer;
  t1: integer;
  px: PRGB;
  pw, pxw: pword;
begin
{$WARNINGS OFF}
  LogWrite('CopyBuffer compression=' + IEIntToStr(imxfer.Compression) + ' BytesPerRow=' + IEIntToStr(imxfer.BytesPerRow) + ' Columns=' + IEIntToStr(imxfer.Columns) + ' Rows=' +
    IEIntToStr(imxfer.Rows) + ' XOffset=' + IEIntToStr(imxfer.XOffset) + ' YOffset=' + IEIntToStr(imxfer.YOffset) + ' BytesWritten=' + IEIntToStr(imxfer.BytesWritten));
  if LockMemory then
    src := GlobalLock(HGLOBAL(imxfer.Memory.TheMem)) // source data
  else
    src := imxfer.Memory.TheMem;
  sinc := imxfer.BytesPerRow;
  case twImageInfo.BitsPerPixel of
    48:
      // RGB 48 bit (16 bit per channel)
      if grec.NativePixelFormat then
        // native pixel format
        for row := 0 to imxfer.Rows - 1 do
        begin
          t1 := row + imxfer.YOffset;
          if t1 >= Bitmap.Height then
            break;
          dst := Bitmap.Scanline[t1];
          inc(dst, imxfer.XOffset * 6); // select column
          CopyMemory(dst, src, imxfer.Columns*6);
          inc(src, sinc);
        end
      else
        // convert to 24 bit
        for row := 0 to imxfer.Rows - 1 do
        begin
          t1 := row + imxfer.YOffset;
          if t1 >= Bitmap.Height then
            break;
          dst := Bitmap.Scanline[t1];
          inc(dst, imxfer.XOffset * 3); // select column
          px := PRGB(dst);
          pw := pword(src);
          for col := 0 to imxfer.Columns - 1 do
          begin
            px^.r := pw^ shr 8; inc(pw);
            px^.g := pw^ shr 8; inc(pw);
            px^.b := pw^ shr 8; inc(pw);
            inc(px);
          end;
          inc(src, sinc);
        end;
    24:
      // truecolor (24bit)
      for row := 0 to imxfer.Rows - 1 do
      begin
        t1 := row + imxfer.YOffset;
        if t1 >= Bitmap.Height then
          break;
        dst := Bitmap.Scanline[t1];
        inc(dst, imxfer.XOffset * 3); // select column
        _CopyBGR_RGB(PRGB(dst), PRGB(src), imxfer.Columns);
        inc(src, sinc);
      end;
    16:
      // 16 bit gray scale
      if grec.NativePixelFormat then
      begin
        // native pixel format
        for row := 0 to imxfer.Rows - 1 do
        begin
          t1 := row + imxfer.YOffset;
          if t1 >= Bitmap.Height then
            break;
          pxw := Bitmap.Scanline[t1];
          inc(pxw, imxfer.XOffset); // select column
          pw := pword(src);
          for col := 0 to imxfer.Columns - 1 do
          begin
            pxw^ := pw^;
            inc(pxw);
            inc(pw);
          end;
          inc(src, sinc);
        end;
      end
      else
        // convert to 24 bit
        for row := 0 to imxfer.Rows - 1 do
        begin
          t1 := row + imxfer.YOffset;
          if t1 >= Bitmap.Height then
            break;
          dst := Bitmap.Scanline[t1];
          inc(dst, imxfer.XOffset * 3); // select column
          pw := pword(src);
          px := PRGB(dst);
          for col := 0 to imxfer.Columns - 1 do
          begin
            with px^ do
            begin
              r := pw^ shr 8;
              g := r;
              b := r;
            end;
            inc(pw);
            inc(px);
          end;
          inc(src, sinc);
        end;
    8:
      // grayscale (8bit)
      if grec.NativePixelFormat then
      begin
        // native pixel format
        for row := 0 to imxfer.Rows - 1 do
        begin
          t1 := row + imxfer.YOffset;
          if t1 >= Bitmap.Height then
            break;
          dst := Bitmap.Scanline[t1];
          inc(dst, imxfer.XOffset); // select column
          pb := src;
          for col := 0 to imxfer.Columns - 1 do
          begin
            dst^ := pb^;
            inc(pb);
            inc(dst);
          end;
          inc(src, sinc);
        end;
      end
      else
        // convert to 24 bit
        for row := 0 to imxfer.Rows - 1 do
        begin
          t1 := row + imxfer.YOffset;
          if t1 >= Bitmap.Height then
            break;
          dst := Bitmap.Scanline[t1];
          inc(dst, imxfer.XOffset * 3); // select column
          pb := src;
          px := PRGB(dst);
          for col := 0 to imxfer.Columns - 1 do
          begin
            with px^ do
            begin
              r := pb^;
              g := pb^;
              b := pb^;
            end;
            inc(pb);
            inc(px);
          end;
          inc(src, sinc);
        end;
    1:
      begin
        // black/write (1bit)
        for row := 0 to imxfer.Rows - 1 do
        begin
          dst := Bitmap.Scanline[row + imxfer.YOffset];
          IECopyBits_large(dst, src, imxfer.XOffset, 0, imxfer.Columns, 2147483647);
          inc(src, sinc);
        end;
      end;
  end;
  if LockMemory then
    GlobalUnlock(HGLOBAL(imxfer.Memory.TheMem));
  LogWrite('CopyBuffer : Ok');
{$WARNINGS ON}
end;


function GetOneBool(var grec: tgrec; var Value: boolean; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  pbol: pTW_BOOL;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  if twCapability.ConType = TWON_ONEVALUE then
  begin
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pbol := @(pvalOneValue^.Item);
    Value := pbol^;
    GlobalUnlock(twCapability.hContainer);
  end
  else
    result := false;
  GlobalFree(twCapability.hContainer);
end;


function GetOneUINT16(var grec: tgrec; var Value: integer; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  puint16: pTW_UINT16;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  if twCapability.ConType = TWON_ONEVALUE then
  begin
    pvalOneValue := GlobalLock(twCapability.hContainer);
    puint16 := @(pvalOneValue^.Item);
    Value := puint16^;
    GlobalUnlock(twCapability.hContainer);
  end
  else
    result := false;
  GlobalFree(twCapability.hContainer);
end;


function GetOneINT16(var grec: tgrec; var Value: integer; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  pint16: pTW_INT16;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  if twCapability.ConType = TWON_ONEVALUE then
  begin
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pint16 := @(pvalOneValue^.Item);
    Value := pint16^;
    GlobalUnlock(twCapability.hContainer);
  end
  else
    result := false;
  GlobalFree(twCapability.hContainer);
end;


function GetOneINT32(var grec: tgrec; var Value: integer; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  pint32: pTW_INT32;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  if twCapability.ConType = TWON_ONEVALUE then
  begin
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pint32 := @(pvalOneValue^.Item);
    Value := pint32^;
    GlobalUnlock(twCapability.hContainer);
  end
  else
    result := false;
  GlobalFree(twCapability.hContainer);
end;


// Supported TW_ONEVALUE (current value)
function SetOneBoolCapability(var grec: tgrec; value: boolean; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
begin
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  // write TW_ONEVALUE (current value only)
  LogWrite('SetOnBoolCapability');
  twCapability.ConType := TWON_ONEVALUE;
  twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE));
  try
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pvalOneValue^.ItemType := TWTY_BOOL;
    pvalOneValue^.Item := ord(value);
    GlobalUnLock(twCapability.hContainer);
    IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
    result := grec.rc = TWRC_SUCCESS;
  finally
    GlobalFree(twCapability.hContainer);
  end;
  if result then
    LogWrite('  SetOnBoolCapability : Ok')
  else
    LogWrite('  SetOnBoolCapability : FAILED!');
end;


procedure settemppath(os: PAnsiChar);
var
  s: AnsiString;
begin
  s := AnsiString(IEGetTempFileName2+'.bmp');
  if length(s) > 254 then
    s := AnsiString(IEGlobalSettings().DefTEMPPATH) + 'imageentwain03.bmp';
  if length(s) > 254 then
    s := 'imageentwain03.bmp';
  IEStrCopy(os, PAnsiChar(s));
end;


procedure IETW_XferReady(var grec: tgrec; pmsg: PMSG);
var
  hNative: TW_UINT32;
  setupmemxfer: TW_SETUPMEMXFER;
  setupfilexfer: TW_SETUPFILEXFER;
  imxfer: TW_IMAGEMEMXFER;
  hbuff: THandle;
  twImageInfo: TW_IMAGEINFO;
  pimxfer: pTW_IMAGEMEMXFER;
  DelayImageInfo: boolean; // if true recall ImageInfo after loaded all buffers
  buffers: TList;
  ptr: pointer;
  i: integer;
  pixfor: TIEPixelFormat;
  io: TImageEnIO;

  function ImageInfo: boolean;
  begin
    LogWrite('IETW_XferReady.ImageInfo');
    DelayImageInfo := false;
    result := true;
    try
      with grec do
      begin
        if not IETW_DS(grec, DG_IMAGE, DAT_IMAGEINFO, MSG_GET, TW_MEMREF(@twImageInfo)) then
        begin
          IETW_AbortAllPendingXfers(grec);
          result := false;
          LogWrite('IETW_XferReady.ImageInfo : not available!');
          exit;
        end;
        if (TransferMode <> tmFile) and ((twImageInfo.PixelType > 2) or (twImageInfo.Planar <> false) or (twImageInfo.Compression <> 0)) then
          TransferMode := tmNative;
        case twImageInfo.BitsPerPixel of
          1..8:
            begin
              IOParams.BitsPerSample := twImageInfo.BitsPerPixel;
              IOParams.SamplesPerPixel := 1;
            end;
          24:
            begin
              IOParams.BitsPerSample := 8;
              IOParams.SamplesPerPixel := 3;
            end;
          48:
            begin
              IOParams.BitsPerSample := 8;
              IOParams.SamplesPerPixel := 4;
            end;
        end;
        IOParams.DpiX := round(twImageInfo.XResolution.Whole + twImageInfo.XResolution.Frac / 65536);
        IOParams.DpiY := round(twImageInfo.YResolution.Whole + twImageInfo.YResolution.Frac / 65536);
        IOParams.Width  := twImageInfo.ImageWidth;
        IOParams.Height := twImageInfo.ImageLength;
        IOParams.OriginalWidth  := twImageInfo.ImageWidth;
        IOParams.OriginalHeight := twImageInfo.ImageLength;
        IOParams.FreeColorMap;
        if (IOParams.Width < 0) or (IOParams.Height < 0) then
        begin
          DelayImageInfo := true;
          result := true;
          exit;
        end;
        if (IOParams.Width = 0) or (IOParams.Height = 0) then
        begin
          IETW_AbortAllPendingXfers(grec);
          result := false;
          exit;
        end;
        if NativePixelFormat then
        begin
          case twImageInfo.BitsPerPixel of
            1:  pixfor := ie1g;
            8:  pixfor := ie8g;
            16: pixfor := ie16g;
            24: pixfor := ie24RGB;
            48: pixfor := ie48RGB;
          end;
        end
        else
        begin
          if (IOParams.BitsPerSample = 1) and (IOParams.SamplesPerPixel = 1) then
            pixfor := ie1g
          else
            pixfor := ie24RGB;
        end;
        if (fBitmap.Width <> IOParams.Width) or (fBitmap.Height <> IOParams.Height) or (fBitmap.PixelFormat <> pixfor) then
          fBitmap.allocate(IOParams.Width, IOParams.Height, pixfor);
      end;
    except
      LogWrite('  IETW_XferReady.ImageInfo : exception!');
      if result then
      begin
        IETW_AbortAllPendingXfers(grec);
        result := false;
      end;
    end;
    LogWrite('  IETW_XferReady.ImageInfo : end');
  end;

begin
{$WARNINGS OFF}
  LogWrite('IETW_XferReady');
  with grec do
  begin

    if not ImageInfo then
    begin
      fAborting := true;
      LogWrite('IETW_XferReady : ABORTED, image info not available!');
      exit;
    end;
    //DelayImageInfo := true;  // uncomment to force undefined size (test only)
    case TransferMode of
      tmBuffered:
        begin
          ///// Buffered xfer
          LogWrite('  IETW_XferReady : buffered transfer mode');
          buffers := nil;
          if DelayImageInfo then
            buffers := TList.Create;
          if assigned(Progress) and (twImageInfo.ImageLength <> 0)then
            Progress.per1 := 100 / twImageInfo.ImageLength;
          if IETW_DS(grec, DG_CONTROL, DAT_SETUPMEMXFER, MSG_GET, @setupmemxfer) then
            LogWrite('  IETW_XferReady : DAT_SETUPMEMXFER Ok')
          else
            LogWrite('  IETW_XferReady : DAT_SETUPMEMXFER FAILED!');
          hbuff := GlobalAlloc(GPTR, setupmemxfer.Preferred);
          try
            with imxfer do
            begin
              Compression := TWON_DONTCARE16;
              BytesPerRow := TW_UINT32(TWON_DONTCARE32);
              Columns := TW_UINT32(TWON_DONTCARE32);
              Rows := TW_UINT32(TWON_DONTCARE32);
              XOffset := TW_UINT32(TWON_DONTCARE32);
              YOffset := TW_UINT32(TWON_DONTCARE32);
              BytesWritten := TW_UINT32(TWON_DONTCARE32);
              Memory.Length := setupmemxfer.Preferred;
              if TWParams.UseMemoryHandle then
              begin
                Memory.Flags := TWMF_APPOWNS or TWMF_HANDLE;
                Memory.TheMem := pointer(hbuff);
              end
              else
              begin
                Memory.Flags := TWMF_APPOWNS or TWMF_POINTER;
                Memory.TheMem := GlobalLock(hbuff);
              end;
            end;
            repeat
              if IETW_DS(grec, DG_IMAGE, DAT_IMAGEMEMXFER, MSG_GET, @imxfer) then
                LogWrite('  IETW_XferReady : DAT_IMAGEMEMXFER Ok')
              else
                LogWrite('  IETW_XferReady : DAT_IMAGEMEMXFER FAILED! (image terminated?)');
              case rc of
                TWRC_SUCCESS, TWRC_XFERDONE:
                  begin
                    if rc = TWRC_SUCCESS then
                      LogWrite('  IETW_XferReady : TWRC_SUCCESS begin');
                    if rc = TWRC_XFERDONE then
                      LogWrite('  IETW_XferReady : TWRC_XFERDONE begin');
                    if DelayImageInfo then
                    begin
                      new(pimxfer);
                      move(imxfer, pimxfer^, sizeof(TW_IMAGEMEMXFER));
                      getmem(pimxfer^.Memory.TheMem, imxfer.BytesWritten);
                      ptr := GlobalLock(HGLOBAL(imxfer.Memory.TheMem));
                      CopyMemory(pimxfer^.Memory.TheMem, ptr, imxfer.BytesWritten);
                      GlobalUnlock(HGLOBAL(imxfer.Memory.TheMem));
                      buffers.Add(pimxfer);
                    end
                    else
                      CopyBuffer(grec, fBitmap, twImageInfo, imxfer, true);
                    if rc = TWRC_XFERDONE then
                    begin
                      // CAP_CAPTION
                      //if not GetOneStringCapability(grec, IOParams.FileName, CAP_CAPTION) then  // 4.1.0 beta
                        IOParams.FileName := '';
                      //
                      nState := 7;
                      transferdone := true;
                      if DelayImageInfo then
                      begin
                        // get image info and copy buffers
                        if ImageInfo then
                        begin
                          for i := 0 to buffers.Count - 1 do
                          begin
                            pimxfer := buffers[i];
                            CopyBuffer(grec, fBitmap, twImageInfo, pimxfer^, false);
                          end;
                        end;
                        DelayImageInfo := true; // this because ImageInfo set it to False
                        // are there other transfers?
                        if (nState = 7) and IETW_DS(grec, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @pendingXfers) then
                        begin
                          if pendingXfers.Count <> 0 then
                            nState := 6
                          else
                            nState := 5;
                        end;
                      end;
                      //
                      break;
                    end;
                    LogWrite('  IETW_XferReady : TWRC_SUCCESS or TWRC_XFERDONE end');
                  end;
                TWRC_CANCEL:
                  begin
                    LogWrite('  IETW_XferReady : TWRC_CANCEL');
                    breakmodalloop := true;
                    nState := 7;
                    if bHideUI then
                      fAborting := true;
                    break;
                  end;
                TWRC_FAILURE:
                  begin
                    LogWrite('  IETW_XferReady : TWRC_FAILURE');
                    nState := 6;
                    if bHideUI then
                      fAborting := true;
                    break;
                  end;
              end;
              // OnProgress
              if assigned(Progress) then
              begin
                with Progress^ do
                  if assigned(fOnProgress) then
                    fOnProgress(Sender, trunc(per1 * (imxfer.YOffset + imxfer.Rows)));
                if Progress^.Aborting^ then
                begin
                  nState := 7;
                  if bHideUI then
                    fAborting := true;
                  break;
                end;
              end;
            until false;
          finally
            if not TWParams.UseMemoryHandle then
              GlobalUnlock(hbuff);
            GlobalFree(hbuff);
          end;
          if DelayImageInfo then
          begin
            for i := 0 to buffers.Count-1 do
            begin
              pimxfer := buffers[i];
              freemem(pimxfer^.Memory.TheMem);
              dispose(pimxfer);
            end;
            FreeAndNil(buffers);
          end;
        end;
      tmNative:
        begin
          ////// Native xfer
          LogWrite('  IETW_XferReady : Native transfer mode');
          IETW_DS(grec, DG_IMAGE, DAT_IMAGENATIVEXFER, MSG_GET, @hNative);
          case (rc) of
            TWRC_XFERDONE:
              begin
                // copy image
                LogWrite('  IETW_XferReady : TWRC_XFERDONE');
                if DelayImageInfo then
                begin
                  if ImageInfo() then
                    _CopyDIB2BitmapEx(hNative, fBitmap, nil, false);
                  DelayImageInfo := true;
                end
                else
                  _CopyDIB2BitmapEx(hNative, fBitmap, nil, false);
                GlobalFree(hNative);
                //
                nState := 7;
                transferdone := true;
              end;
            TWRC_CANCEL:
              begin
                LogWrite('  IETW_XferReady : TWRC_CANCEL');
                breakmodalloop := true;
                nState := 7;
                if bHideUI then
                  fAborting := true;
              end;
            TWRC_FAILURE:
              begin
                LogWrite('  IETW_XferReady : TWRC_FAILURE');
                nState := 6;
                if bHideUI then
                  fAborting := true;
              end;
          else
            nState := 6;
          end;
        end;
      tmFile:
        begin
          ////// File xfer
          LogWrite('  IETW_XferReady : File transfer mode');
          IETW_DS(grec, DG_CONTROL, DAT_SETUPFILEXFER, MSG_GET, @setupfilexfer);
          settemppath(@setupfilexfer.FileName[0]);
          if (setupfilexfer.Format = 1) or (setupfilexfer.Format = 3) or (setupfilexfer.Format = 5) or (setupfilexfer.Format = 6) or (setupfilexfer.Format > 7) then
            setupfilexfer.Format := TWFF_BMP;
          setupfilexfer.VRefNum := 0;
          IETW_DS(grec, DG_CONTROL, DAT_SETUPFILEXFER, MSG_SET, @setupfilexfer);
          IETW_DS(grec, DG_IMAGE, DAT_IMAGEFILEXFER, MSG_GET, nil);
          case (rc) of
            TWRC_XFERDONE:
              begin
                // copy image
                LogWrite('  IETW_XferReady : TWRC_XFERDONE');
                io := TImageEnIO.CreateFromBitmap(fBitmap);
                try
                  io.LoadFromFileFormat(setupfilexfer.FileName, FindFileFormat(setupfilexfer.FileName, false));
                finally
                  FreeAndNil(io);
                end;
                DeleteFile( setupfilexfer.FileName );
                //
                nState := 7;
                transferdone := true;
              end;
            TWRC_CANCEL:
              begin
                LogWrite('  IETW_XferReady : TWRC_CANCEL');
                breakmodalloop := true;
                nState := 7;
                if bHideUI then
                  fAborting := true;
              end;
            TWRC_FAILURE:
              begin
                LogWrite('  IETW_XferReady : TWRC_FAILURE');
                nState := 6;
                if bHideUI then
                  fAborting := true;
              end;
          else
            nState := 6;
          end;
        end;
    end;
    breakmodalloop := true;
    IETW_AbortAllPendingXfers(grec);
  end;
  LogWrite('  IETW_XferReady : end');
{$WARNINGS ON}
end;


procedure IETW_XferReadyMulti(var grec: tgrec; pmsg: PMSG);
var
  hNative: TW_UINT32;
  setupmemxfer: TW_SETUPMEMXFER;
  setupfilexfer: TW_SETUPFILEXFER;
  imxfer: TW_IMAGEMEMXFER;
  hbuff: THandle;
  twImageInfo: TW_IMAGEINFO;
  ofy: integer;
  ofy_set: boolean;
  pimxfer: pTW_IMAGEMEMXFER;
  DelayImageInfo: boolean; // if true recall ImageInfo after loaded all buffers
  buffers: TList;
  ptr: pointer;
  i: integer;
  fCaption: AnsiString;
  io: TImageEnIO;
  pixfor: TIEPixelFormat;
  ImDpiX, ImDpiY: integer;

  function ImageInfo: boolean;
  begin
    LogWrite('IETW_XferReadyMulti.ImageInfo');
    DelayImageInfo := false;
    result := true;
    try
      with grec do
      begin
        if not IETW_DS(grec, DG_IMAGE, DAT_IMAGEINFO, MSG_GET, TW_MEMREF(@twImageInfo)) then
        begin
          IETW_AbortAllPendingXfers(grec);
          result := false;
          LogWrite('IETW_XferReadyMulti.ImageInfo : not available!');
          exit;
        end;
        if (TransferMode <> tmFile) and ((twImageInfo.PixelType > 2) or (twImageInfo.Planar <> false) or (twImageInfo.Compression <> 0)) then
          TransferMode := tmNative;
        if (twImageInfo.ImageWidth < 0) or (twImageInfo.ImageLength < 0) then
        begin
          DelayImageInfo := true;
          result := true;
          exit;
        end;
        if (twImageInfo.ImageWidth <= 0) or (twImageInfo.ImageLength <= 0) then
        begin
          IETW_AbortAllPendingXfers(grec);
          result := false;
          exit;
        end;
        fBitmap := TIEBitmap.Create;
        if NativePixelFormat then
        begin
          case twImageInfo.BitsPerPixel of
            1:  pixfor := ie1g;
            8:  pixfor := ie8g;
            16: pixfor := ie16g;
            24: pixfor := ie24RGB;
            48: pixfor := ie48RGB;
          end;
        end
        else
        begin
          if twImageInfo.BitsPerPixel = 1 then
            pixfor := ie1g
          else
            pixfor := ie24RGB;
        end;
        fBitmap.Allocate(twImageInfo.ImageWidth, twImageInfo.ImageLength, pixfor)
      end;
    except
      LogWrite('  IETW_XferReadyMulti.ImageInfo : exception!');
      if result then
      begin
        IETW_AbortAllPendingXfers(grec);
        result := false;
      end;
    end;
    LogWrite('  IETW_XferReadyMulti.ImageInfo : end');
  end;

begin
{$WARNINGS OFF}
  LogWrite('IETW_XferReadyMulti');
  fCaption := '';
  with grec do
  begin
    repeat
      LogWrite('  IETW_XferReadyMulti : getting another image');
      if assigned(Progress) and Progress^.Aborting^ then
      begin
        LogWrite('IETW_XferReadyMulti, ending : Aborting = true!');
        nState := 5;  // 3.0.4
        IETW_AbortAllPendingXfers(grec);
        exit;
      end;
      if not ImageInfo or fAborting then
      begin
        LogWrite('IETW_XferReadyMulti, ending : ABORTED, image info not available!');
        nState := 5;  // 3.0.4
        fAborting := true;
        exit;
      end;
      //DelayImageInfo := true;  // uncomment for force undefined size (test only)
      case TransferMode of
        tmBuffered:
          begin
            ///// Buffered xfer
            LogWrite('  IETW_XferReadyMulti : buffered transfer mode');
            buffers := nil;
            if DelayImageInfo then
              buffers := TList.Create;
            if IETW_DS(grec, DG_CONTROL, DAT_SETUPMEMXFER, MSG_GET, @setupmemxfer) then
              LogWrite('  IETW_XferReadyMulti : DAT_SETUPMEMXFER Ok')
            else
              LogWrite('  IETW_XferReadyMulti : DAT_SETUPMEMXFER FAILED!');
            hbuff := GlobalAlloc(GPTR, setupmemxfer.Preferred);
            try
              with imxfer do
              begin
                Compression := TWON_DONTCARE16;
                BytesPerRow := TW_UINT32(TWON_DONTCARE32);
                Columns := TW_UINT32(TWON_DONTCARE32);
                Rows := TW_UINT32(TWON_DONTCARE32);
                XOffset := TW_UINT32(TWON_DONTCARE32);
                YOffset := TW_UINT32(TWON_DONTCARE32);
                BytesWritten := TW_UINT32(TWON_DONTCARE32);
                Memory.Length := setupmemxfer.Preferred;
                if TWParams.UseMemoryHandle then
                begin
                  Memory.Flags := TWMF_APPOWNS or TWMF_HANDLE;
                  Memory.TheMem := pointer(hbuff);
                end
                else
                begin
                  Memory.Flags := TWMF_APPOWNS or TWMF_POINTER;
                  Memory.TheMem := GlobalLock(hbuff);
                end;
              end;
              if assigned(Progress) and (twImageInfo.ImageLength <> 0) then
                Progress.per1 := 100 / twImageInfo.ImageLength;
              ofy_set := false;
              ofy := 0;
              repeat
                if IETW_DS(grec, DG_IMAGE, DAT_IMAGEMEMXFER, MSG_GET, @imxfer) then
                  LogWrite('  IETW_XferReadyMulti : DAT_IMAGEMEMXFER Ok')
                else
                  LogWrite('  IETW_XferReadyMulti : DAT_IMAGEMEMXFER FAILED! (image terminated?)');

                if not ofy_set then
                begin
                  ofy_set := true;
                  ofy := imxfer.YOffset;
                end;
                imxfer.YOffset := imxfer.YOffset - ofy;

                case rc of
                  TWRC_SUCCESS, TWRC_XFERDONE:
                    begin
                      if rc = TWRC_SUCCESS then
                        LogWrite('  IETW_XferReadyMulti : TWRC_SUCCESS begin');
                      if rc = TWRC_XFERDONE then
                        LogWrite('  IETW_XferReadyMulti : TWRC_XFERDONE begin');
                      if DelayImageInfo then
                      begin
                        new(pimxfer);
                        move(imxfer, pimxfer^, sizeof(TW_IMAGEMEMXFER));
                        getmem(pimxfer^.Memory.TheMem, imxfer.BytesWritten);
                        ptr := GlobalLock(HGLOBAL(imxfer.Memory.TheMem));
                        CopyMemory(pimxfer^.Memory.TheMem, ptr, imxfer.BytesWritten);
                        GlobalUnlock(HGLOBAL(imxfer.Memory.TheMem));
                        buffers.Add(pimxfer);
                      end
                      else
                        CopyBuffer(grec, fBitmap, twImageInfo, imxfer, true);
                      if rc = TWRC_XFERDONE then
                      begin
                        // CAP_CAPTION
                        if not GetOneStringCapability(grec, fCaption, CAP_CAPTION) then
                          fCaption := '';
                        //
                        transferdone := true;
                        nState := 7;
                        if DelayImageInfo then
                        begin
                          // get image info and copy buffers
                          if ImageInfo then
                          begin
                            for i := 0 to buffers.Count - 1 do
                            begin
                              pimxfer := buffers[i];
                              CopyBuffer(grec, fBitmap, twImageInfo, pimxfer^, false);
                            end;
                          end;
                          DelayImageInfo := true; // this because ImageInfo set it to False
                          // are there other images?
                          if (nState = 7) and IETW_DS(grec, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @pendingXfers) then
                          begin
                            if pendingXfers.Count <> 0 then
                              nState := 6
                            else
                              nState := 5;
                          end;
                        end;
                        break;
                      end;
                      LogWrite('  IETW_XferReadyMulti : TWRC_SUCCESS or TWRC_XFERDONE end');
                    end;
                  TWRC_CANCEL:
                    begin
                      LogWrite('  IETW_XferReadyMulti : TWRC_CANCEL');
                      breakmodalloop := true;
                      nState := 7;
                      if bHideUI then
                        fAborting := true;
                      break;
                    end;
                  TWRC_FAILURE:
                    begin
                      LogWrite('  IETW_XferReadyMulti : TWRC_FAILURE');
                      nState := 6;
                      if bHideUI then
                        fAborting := true;
                      // version 2.1.6-3
                      if assigned(Progress) then
                        Progress^.Aborting^ := true;
                      //
                      break;
                    end;
                end;
                // OnProgress
                if assigned(Progress) then
                  with Progress^ do
                    if assigned(fOnProgress) then
                      fOnProgress(Sender, trunc(per1 * (imxfer.YOffset + imxfer.Rows)));
              until false;
            finally
              if not TWParams.UseMemoryHandle then
                GlobalUnlock(hbuff);
              GlobalFree(hbuff);
            end;
            if DelayImageInfo then
            begin
              for i := 0 to buffers.Count-1 do
              begin
                pimxfer := buffers[i];
                freemem(pimxfer^.Memory.TheMem);
                dispose(pimxfer);
              end;
              FreeAndNil(buffers);
            end;
          end;
        tmNative:
          begin
            ////// Native xfer
            LogWrite('  IETW_XferReadyMulti : Native transfer mode');
            IETW_DS(grec, DG_IMAGE, DAT_IMAGENATIVEXFER, MSG_GET, @hNative);
            case (rc) of
              TWRC_XFERDONE:
                begin
                  // copy image
                  LogWrite('  IETW_XferReadyMulti : TWRC_XFERDONE');
                  if DelayImageInfo then
                  begin
                    if ImageInfo() then
                      _CopyDIB2BitmapEx(hNative, fBitmap, nil, false);
                    DelayImageInfo := true;
                  end
                  else
                    _CopyDIB2BitmapEx(hNative, fBitmap, nil, false);
                  GlobalFree(hNative);
                  //
                  nState := 7;
                  transferdone := true;
                end;
              TWRC_CANCEL:
                begin
                  LogWrite('  IETW_XferReadyMulti : TWRC_CANCEL');
                  breakmodalloop := true;
                  nState := 7;
                  if bHideUI then
                    fAborting := true;
                end;
              TWRC_FAILURE:
                begin
                  LogWrite('  IETW_XferReadyMulti : TWRC_FAILURE');
                  nState := 6;
                  if bHideUI then
                    fAborting := true;
                end;
            else
              nState := 6;
            end;
          end;
        tmFile:
          begin
            ////// File xfer
            LogWrite('  IETW_XferReadyMulti : File transfer mode');
            IETW_DS(grec, DG_CONTROL, DAT_SETUPFILEXFER, MSG_GET, @setupfilexfer);
            settemppath(@setupfilexfer.FileName[0]);
            if (setupfilexfer.Format = 1) or (setupfilexfer.Format = 3) or (setupfilexfer.Format = 5) or (setupfilexfer.Format = 6) or (setupfilexfer.Format > 7) then
              setupfilexfer.Format := TWFF_BMP;
            setupfilexfer.VRefNum := 0;
            IETW_DS(grec, DG_CONTROL, DAT_SETUPFILEXFER, MSG_SET, @setupfilexfer);
            IETW_DS(grec, DG_IMAGE, DAT_IMAGEFILEXFER, MSG_GET, nil);
            case (rc) of
              TWRC_XFERDONE:
                begin
                  // copy image
                  LogWrite('  IETW_XferReadyMulti : TWRC_XFERDONE');
                  io := TImageEnIO.CreateFromBitmap(fBitmap);
                  try
                    io.LoadFromFileFormat(setupfilexfer.FileName, FindFileFormat(setupfilexfer.FileName, false));
                  finally
                    FreeAndNil(io);
                  end;
                  DeleteFile( setupfilexfer.FileName );
                  nState := 7;
                  transferdone := true;
                end;
              TWRC_CANCEL:
                begin
                  LogWrite('  IETW_XferReadyMulti : TWRC_CANCEL');
                  breakmodalloop := true;
                  nState := 7;
                  if bHideUI then
                    fAborting := true;
                end;
              TWRC_FAILURE:
                begin
                  LogWrite('  IETW_XferReadyMulti : TWRC_FAILURE');
                  nState := 6;
                  if bHideUI then
                    fAborting := true;
                end;
            else
              nState := 6;
            end;
          end;
      end;

      if assigned(fBitmap) and (fBitmap.PixelFormat = ie1g) and grec.BWToInvert then
        _Negative1BitEx(fBitmap);
      ImDpiX := round(twImageInfo.XResolution.Whole + twImageInfo.XResolution.Frac / 65536);
      ImDpiY := round(twImageInfo.YResolution.Whole + twImageInfo.YResolution.Frac / 65536);
      if not fAborting then
        MultiCallBack(fBitmap, TObject(IOParams), ImDpiX, ImDpiY);
      FreeAndNil(fBitmap);
      if IOParams <> nil then
      begin
        case twImageInfo.BitsPerPixel of
          1..8:
            begin
              IOParams.BitsPerSample := twImageInfo.BitsPerPixel;
              IOParams.SamplesPerPixel := 1;
            end;
          24:
            begin
              IOParams.BitsPerSample := 8;
              IOParams.SamplesPerPixel := 3;
            end;
        end;
        IOParams.DpiX := ImDpiX;
        IOParams.DpiY := ImDpiY;
        IOParams.Width := twImageInfo.ImageWidth;
        IOParams.Height := twImageInfo.ImageLength;
        IOParams.OriginalWidth := twImageInfo.ImageWidth;
        IOParams.OriginalHeight := twImageInfo.ImageLength;
        IOParams.FreeColorMap;
        IOParams.FileName := fCaption;
      end;

      if (nState = 7) and IETW_DS(grec, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, @pendingXfers) then
      begin
        if pendingXfers.Count <> 0 then
          nState := 6
        else
          nState := 5;
      end;
    until nState <> 6;
    breakmodalloop := true;
    IETW_AbortAllPendingXfers(grec);
  end;
  LogWrite('  IETW_XferReadyMulti : end');
{$WARNINGS ON}
end;


// true msg processed
function IETW_MessageHook(var grec: tgrec; lpmsg: pMSG): boolean;
var
  bProcessed: boolean;
  twEvent: TW_EVENT;
  xmodal: boolean;
begin
  LogWrite('IETW_MessageHook');
  with grec do
  begin
    xmodal := modal; // grec.modal could not be more valid after ProxyWin.Free
    bProcessed := FALSE;
    if (nState >= 5) then
    begin
      // source enabled
      LogWrite('IETW_MessageHook : state>=5');
      twEvent.pEvent := TW_MEMREF(lpmsg);
      twEvent.TWMessage := MSG_NULL;

      IETW_DS(grec, DG_CONTROL, DAT_EVENT, MSG_PROCESSEVENT, @twEvent);
      LogWrite('IETW_MessageHook : event.msg=$' + IEIntToHex(twEvent.TWMessage, 4));
      bProcessed := (rc = TWRC_DSEVENT);
      case (twEvent.TWMessage) of
        MSG_XFERREADY:
          begin
            if not sending then
            begin
              sending := true;
              nState := 6;
              if gmulti then
                IETW_XferReadyMulti(grec, lpmsg)
              else
                IETW_XferReady(grec, lpmsg);
              if fAborting then
                IETW_DisableSource(grec);
              sending := false;
              LogWrite('  IETW_MessageHook : processed MSG_XFERREADY');
            end;
          end;
        MSG_CLOSEDSREQ:
          begin
            LogWrite('  IETW_MessageHook : processed MSG_CLOSEDSREQ');
            IETW_DisableSource(grec);
            if not xmodal then
              FreeAndNil(grec.ProxyWin);
          end;
        MSG_CLOSEDSOK:
          begin
            LogWrite('  IETW_MessageHook : processed MSG_CLOSEDSOK');
            closedsok := true;
            IETW_DisableSource(grec);
            GetCustomData(grec);
            if not xmodal then
              FreeAndNil(grec.ProxyWin);
          end;
        MSG_NULL:
          begin
            // no message returned from DS
            LogWrite('  IETW_MessageHook : MSG_NULL');
          end;
      end;
    end
    else
      LogWrite('IETW_MessageHook : state = ' + AnsiString(IntToStr(nState)));
    result := bProcessed;
  end;
  if xmodal then
    LogWrite('IETW_MessageHook : end');
end;


procedure IETW_EmptyMessageQueue(var grec: tgrec);
var
  msg: TMSG;
begin
  LogWrite('IETW_EmptyMessageQueue');
  with grec do
  begin
    while (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) do
    begin
      if (msg.message = WM_QUIT) then
      begin
        PostQuitMessage(msg.wParam);
        break;
      end;
      if (not IETW_MessageHook(grec, @msg)) then
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end;
  end;
  LogWrite('  IETW_EmptyMessageQueue : end');
end;


procedure IETW_ModalEventLoop(var grec: tgrec);
var
  msg: TMSG;
begin
  LogWrite('IETW_ModalEventLoop');
  with grec do
  begin
    BreakModalLoop := false;
    while (nState >= 5) and (not TransferDone) and (not BreakModalLoop) and (GetMessage(msg, 0, 0, 0)) do
    begin

      LogWrite('IETW_ModalEventLoop : event.msg=$' + IEIntToHex(msg.message, 4));

      if (not IETW_MessageHook(grec, @msg)) then
      begin
        TranslateMessage(msg);
        try
          DispatchMessage(msg);
        except
        end;
      end;

    end;
    BreakModalLoop := false;
  end;
  LogWrite('IETW_ModalEventLoop : end');
end;


function IETW_GetSourceList(SList: TList; TwainShared: PIETwainShared; callwnd: HWND): boolean;
var
  SourceId: pTW_IDENTITY;
  grec: tgrec;
  wnd: HWND;
begin
  result := false;

  if IEGlobalSettings().IsInsideTwain then
    exit;
  IEGlobalSettings().IsInsideTwain := true;

  try
    SList.Clear;
    Init_grec(grec);
    grec.callwnd := callwnd;
    grec.PTwainShared := TwainShared;
    wnd := CreateProxyWindow(grec);
    grec.proxywnd := wnd;
    if (IETW_LoadSourceManager(grec)) then
    begin
      if (IETW_OpenSourceManager(grec, wnd)) then
      begin
        SourceId := AllocMem(sizeof(TW_IDENTITY));
        try
          IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_GETFIRST, SourceId);
          while grec.rc <> TWRC_ENDOFLIST do
          begin
            if SourceId^.ProductName = '' then
              freemem(SourceId)
            else
              SList.Add(SourceId);
            SourceId := AllocMem(sizeof(TW_IDENTITY));
            IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_GETNEXT, SourceId);
          end;
        finally
          FreeMem(SourceId); // last not assigned
        end;
        IETW_CloseSourceManager(grec, wnd);
        result := true;
      end
      else
      begin
        DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
        exit;
      end;
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
    end
    else
    begin
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
  finally
    windows.SetActiveWindow(grec.actwnd);
    IEGlobalSettings().IsInsideTwain := false;
  end;
end;


procedure FloatToFix32(const floater: double; fix32: pTW_FIX32);
var
  s: double;
  value: TW_INT32;
begin
  try
    if floater < 0 then
      s := -0.5
    else
      s := 0.5;
    value := trunc(floater * 65536 + s);
    Fix32^.Whole := value shr 16;
    Fix32^.Frac := value and $0000FFFF;
  except
    Fix32^.Whole := 0;
    Fix32^.Frac := 0;
  end;
end;


procedure GetAcquireFrame(var grec: tgrec; var fAcquireFrame: TIEDRect);
var
  ImageLayout: TW_IMAGELAYOUT;
begin
  LogWrite('GetAcquireFrame');
  IETW_DS(grec, DG_IMAGE, DAT_IMAGELAYOUT, MSG_GET, @ImageLayout);
  if grec.rc = TWRC_SUCCESS then
  begin
    with ImageLayout.Frame do
    begin
      fAcquireFrame.Left := Left.Whole + Left.Frac / 65536;
      fAcquireFrame.Top := Top.Whole + Top.Frac / 65536;
      fAcquireFrame.Right := Right.Whole + Right.Frac / 65536;
      fAcquireFrame.Bottom := Bottom.Whole + BOttom.Frac / 65536;
    end;
    LogWrite('  GetAcquireFrame : ok');
  end
  else
  begin
    LogWrite('  GetAcquireFrame : FAILED!');
  end;
end;


procedure SetAcquireFrame(var grec: tgrec; const fLeft, fTop, fRight, fBottom: double);
var
  ImageLayout: TW_IMAGELAYOUT;
begin
  LogWrite('SetAcquireFrame');
  fillchar(ImageLayout, sizeof(TW_IMAGELAYOUT), 0);
  with ImageLayout.Frame do
  begin
    FloatToFIX32(fLeft, @Left);
    FloatToFIX32(fTop, @Top);
    FloatToFIX32(fRight, @Right);
    FloatToFIX32(fBottom, @Bottom);
  end;
  IETW_DS(grec, DG_IMAGE, DAT_IMAGELAYOUT, MSG_SET, @ImageLayout);
  LogWrite('  SetAcquireFrame : end');
end;


// get a Fix32 data (one, range, enum, array) and convert to double data
// free and realloc vlist
function GetFix32asDouble(var grec: tgrec; dlist: TIEDoubleList; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalEnum: pTW_ENUMERATION;
  pvalOneValue: pTW_ONEVALUE;
  pvalArray: pTW_ARRAY;
  pvalRange: pTW_RANGE;
  pfix: pTW_FIX32;
  q: integer;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  dlist.clear;
  LogWrite('GetFix32asDouble');
  case twCapability.ConType of
    TWON_ENUMERATION:
      begin
        pvalEnum := GlobalLock(twCapability.hContainer);
        dlist.Count := pvalEnum^.NumItems;
        if (dlist.Count > 0) then
        begin
          pfix := @(pvalEnum^.ItemList[0]);
          for q := 0 to dlist.Count - 1 do
          begin
            dlist[q] := pfix^.Whole + pfix^.Frac / 65536;
            inc(pfix);
          end;
          dlist.CurrentValue := dlist[pvalEnum^.CurrentIndex];
        end
        else
          result := false;
      end;
    TWON_ONEVALUE:
      begin
        pvalOneValue := GlobalLock(twCapability.hContainer);
        dlist.Count := 1;
        pfix := @(pvalOneValue^.Item);
        dlist[0] := pfix^.Whole + pfix^.Frac / 65536;
        if ((cap = ICAP_XRESOLUTION) or (cap = ICAP_YRESOLUTION)) and (dlist[0] < 0) then
          dlist[0] := 300; // workaround for some scanner that return invalid resolution values
        dlist.CurrentValue := dlist[0];
      end;
    TWON_ARRAY:
      begin
        pvalArray := GlobalLock(twCapability.hContainer);
        dlist.Count := pvalArray^.NumItems;
        pfix := @(pvalArray^.ItemList[0]);
        for q := 0 to dlist.Count - 1 do
        begin
          dlist[q] := pfix^.Whole + pfix^.Frac / 65536;
          inc(pfix);
        end;
      end;
    TWON_RANGE:
      begin
        pvalRange := GlobalLock(twCapability.hContainer);
        pfix := @(pvalRange^.MinValue);
        dlist.RangeMin := pfix^.Whole + pfix^.Frac / 65536;
        pfix := @(pvalRange^.MaxValue);
        dlist.RangeMax := pfix^.Whole + pfix^.Frac / 65536;
        pfix := @(pvalRange^.StepSize);
        dlist.RangeStep := pfix^.Whole + pfix^.Frac / 65536;
        pfix := @(pvalRange^.CurrentValue);
        dlist.CurrentValue := pfix^.Whole + pfix^.Frac / 65536;
      end;
  else
    result := false;
  end;
  GlobalUnlock(twCapability.hContainer);
  GlobalFree(twCapability.hContainer);
  if result then
    LogWrite('  GetFix32asDouble : ok')
  else
    LogWrite('  GetFix32asDouble : FAILED!');
end;


function GetOneFIX32asDouble(var grec: tgrec; var Value: double; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  pfix: pTW_FIX32;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('GetOnFIX32asDouble');
  if twCapability.ConType = TWON_ONEVALUE then
  begin
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pfix := @(pvalOneValue^.Item);
    Value := pfix^.Whole + pfix^.Frac / 65536;
    GlobalUnlock(twCapability.hContainer);
  end
  else
    result := false;
  GlobalFree(twCapability.hContainer);
  if result then
    LogWrite('  GetOneFIX32asDouble : ok')
  else
    LogWrite('  GetOneFIX32asDouble : FAILED!');
end;


// get a UINT16 data (one, range, enum, array) and convert to integer data
// free and realloc vlist
function GetUINT16asInteger(var grec: tgrec; ilist: TIEIntegerList; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalEnum: pTW_ENUMERATION;
  pvalOneValue: pTW_ONEVALUE;
  pvalArray: pTW_ARRAY;
  pvalRange: pTW_RANGE;
  v16: pTW_UINT16;
  q: integer;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  ilist.Clear;
  LogWrite('GetUINT16asInteger');
  case twCapability.ConType of
    TWON_ENUMERATION:
      begin
        pvalEnum := GlobalLock(twCapability.hContainer);
        ilist.Count := pvalEnum^.NumItems;
        if pvalEnum^.NumItems > 0 then  // 3.0.3
        begin
          v16 := @(pvalEnum^.ItemList[0]);
          for q := 0 to ilist.Count - 1 do
          begin
            ilist[q] := v16^;
            inc(v16);
          end;
          ilist.CurrentValue := ilist[pvalEnum^.CurrentIndex];
        end;
      end;
    TWON_ONEVALUE:
      begin
        pvalOneValue := GlobalLock(twCapability.hContainer);
        ilist.Count := 1;
        v16 := @(pvalOneValue^.Item);
        ilist[0] := v16^;
        ilist.CurrentValue := ilist[0];
      end;
    TWON_ARRAY:
      begin
        pvalArray := GlobalLock(twCapability.hContainer);
        ilist.Count := pvalArray^.NumItems;
        v16 := @(pvalArray^.ItemList[0]);
        for q := 0 to ilist.Count - 1 do
        begin
          ilist[q] := v16^;
          inc(v16);
        end;
      end;
    TWON_RANGE:
      begin
        pvalRange := GlobalLock(twCapability.hContainer);
        v16 := @(pvalRange^.MinValue);
        ilist.RangeMin := v16^;
        v16 := @(pvalRange^.MaxValue);
        ilist.RangeMax := v16^;
        v16 := @(pvalRange^.StepSize);
        ilist.RangeStep := v16^;
        v16 := @(pvalRange^.CurrentValue);
        ilist.CurrentValue := v16^;
      end;
  else
    result := false;
  end;
  GlobalUnlock(twCapability.hContainer);
  GlobalFree(twCapability.hContainer);
  if result then
    LogWrite('  GetUINT16asInteger : ok')
  else
    LogWrite('  GetUINT16asInteger : FAILED!');
end;


// Supported only TW_ENUMERATION (allowed values and curr.) and TW_ONEVALUE (current value)
function SetIntegerAsUINT16Capability(var grec: tgrec; ilist: TIEIntegerList; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalEnum: pTW_ENUMERATION;
  pvalOneValue: pTW_ONEVALUE;
  v16: pTW_UINT16;
  q: integer;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('SetIntegerAsUINT16Capability');
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  if ielItems in ilist.Changed then
  begin
    // write TW_ENUMERATION (allowed values and current value)
    twCapability.ConType := TWON_ENUMERATION;
    twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ENUMERATION) + sizeof(TW_UINT16) * ilist.Count);
    try
      pvalEnum := GlobalLock(twCapability.hContainer);
      pvalEnum^.ItemType := TWTY_UINT16;
      pvalEnum^.NumItems := ilist.Count;
      pvalEnum^.CurrentIndex := ilist.IndexOf(ilist.CurrentValue);
      v16 := @(pvalEnum^.ItemList[0]);
      for q := 0 to ilist.Count - 1 do
      begin
        v16^ := ilist[q];
        inc(v16);
      end;
      GlobalUnLock(twCapability.hContainer);
      IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
      result := grec.rc = TWRC_SUCCESS;
    finally
      GlobalFree(twCapability.hContainer);
    end;
  end;
  if ielCurrentValue in ilist.Changed then
  begin
    // write TW_ONEVALUE (current value only)
    twCapability.ConType := TWON_ONEVALUE;
    twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE));
    try
      pvalOneValue := GlobalLock(twCapability.hContainer);
      pvalOneValue^.ItemType := TWTY_UINT16;
      pvalOneValue^.Item := ilist.CurrentValue;
      GlobalUnLock(twCapability.hContainer);
      IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
      result := grec.rc = TWRC_SUCCESS;
    finally
      GlobalFree(twCapability.hContainer);
    end;
  end;
  if result then
    LogWrite('  SetIntegerAsUINT16Capability : ok')
  else
    LogWrite('  SetIntegerAsUINT16Capability : FAILED!');
end;


// Supported only TW_ENUMERATION (allowed values and curr.), TW_ONEVALUE (current value)
// TW_RANGE (allowed values and curr.)
function SetDoubleAsFIX32Capability(var grec: tgrec; dlist: TIEDoubleList; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalEnum: pTW_ENUMERATION;
  pvalOneValue: pTW_ONEVALUE;
  pvalRange: pTW_RANGE;
  pfix: pTW_FIX32;
  q: integer;
begin
  result := true;
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('SetDoubleAsFIX32Capability');
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  if ielItems in dlist.Changed then
  begin
    // write TW_ENUMERATION (allowed values and current value)
    twCapability.ConType := TWON_ENUMERATION;
    twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ENUMERATION) + sizeof(TW_FIX32) * dlist.Count);
    try
      pvalEnum := GlobalLock(twCapability.hContainer);
      pvalEnum^.ItemType := TWTY_FIX32;
      pvalEnum^.NumItems := dlist.Count;
      pvalEnum^.CurrentIndex := dlist.IndexOf(dlist.CurrentValue);
      pfix := @(pvalEnum^.ItemList[0]);
      for q := 0 to dlist.Count - 1 do
      begin
        FloatToFIX32(dlist[q], pfix);
        inc(pfix);
      end;
      GlobalUnLock(twCapability.hContainer);
      IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
      result := grec.rc = TWRC_SUCCESS;
    finally
      GlobalFree(twCapability.hContainer);
    end;
  end;
  if ielCurrentValue in dlist.Changed then
  begin
    // write TW_ONEVALUE (current value only)
    twCapability.ConType := TWON_ONEVALUE;
    twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE));
    try
      pvalOneValue := GlobalLock(twCapability.hContainer);
      pvalOneValue^.ItemType := TWTY_FIX32;
      FloatToFIX32(dlist.CurrentValue, @pvalOneValue^.Item);
      GlobalUnLock(twCapability.hContainer);
      IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
      result := grec.rc = TWRC_SUCCESS;
    finally
      GlobalFree(twCapability.hContainer);
    end;
  end;
  if ielRange in dlist.Changed then
  begin
    // write TW_RANGE (allowed values and current)
    twCapability.ConType := TWON_RANGE;
    twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_RANGE));
    try
      pvalRange := GlobalLock(twCapability.hContainer);
      pvalRange^.ItemType := TWTY_FIX32;
      FloatToFIX32(dlist.CurrentValue, @pvalRange^.CurrentValue);
      FloatToFIX32(dlist.RangeMin, @pvalRange^.MinValue);
      FloatToFIX32(dlist.RangeMax, @pvalRange^.MaxValue);
      FloatToFIX32(dlist.RangeStep, @pvalRange^.StepSize);
      GlobalUnLock(twCapability.hContainer);
      IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
      result := grec.rc = TWRC_SUCCESS;
    finally
      GlobalFree(twCapability.hContainer);
    end;
  end;
  if result then
    LogWrite('  SetDoubleAsFIX32Capability : ok')
  else
    LogWrite('  SetDoubleAsFIX32Capability : FAILED!');
end;


// Supported TW_ONEVALUE (current value)
function SetOneDoubleAsFIX32Capability(var grec: tgrec; value: double; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
begin
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('SetOneDoubleAsFIX32Capability');
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  // write TW_ONEVALUE (current value only)
  twCapability.ConType := TWON_ONEVALUE;
  twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE));
  try
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pvalOneValue^.ItemType := TWTY_FIX32;
    FloatToFIX32(value, @pvalOneValue^.Item);
    GlobalUnLock(twCapability.hContainer);
    IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
    result := grec.rc = TWRC_SUCCESS;
  finally
    GlobalFree(twCapability.hContainer);
  end;
  if result then
    LogWrite('  SetOneDoubleAsFIX32Capability : ok')
  else
    LogWrite('  SetOneDoubleAsFIX32Capability : FAILED!');
end;


// Supported TW_ONEVALUE (current value)
function SetOneUINT16Capability(var grec: tgrec; value: word; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  pv16: pTW_UINT16;
begin
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('SetOneUINT16Capability');
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  // write TW_ONEVALUE (current value only)
  twCapability.ConType := TWON_ONEVALUE;
  twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE));
  try
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pvalOneValue^.ItemType := TWTY_UINT16;
    pv16 := @(pvalOneValue^.Item);
    pv16^ := value;
    GlobalUnLock(twCapability.hContainer);
    IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
    result := grec.rc = TWRC_SUCCESS;
  finally
    GlobalFree(twCapability.hContainer);
  end;
  if result then
    LogWrite('  SetOneUINT16Capability : ok')
  else
    LogWrite('  SetOneUINT16Capability : FAILED!');
end;


// Supported TW_ONEVALUE (current value)
function SetOneINT16Capability(var grec: tgrec; value: smallint; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  pv16: pTW_INT16;
begin
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('SetOneINT16Capability');
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  // write TW_ONEVALUE (current value only)
  twCapability.ConType := TWON_ONEVALUE;
  twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE));
  try
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pvalOneValue^.ItemType := TWTY_INT16;
    pv16 := @(pvalOneValue^.Item);
    pv16^ := value;
    GlobalUnLock(twCapability.hContainer);
    IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
    result := grec.rc = TWRC_SUCCESS;
  finally
    GlobalFree(twCapability.hContainer);
  end;
  if result then
    LogWrite('  SetOneINT16Capability : ok')
  else
    LogWrite('  SetOneINT16Capability : FAILED!');
end;


function SetOneINT32Capability(var grec: tgrec; value: integer; cap: TW_UINT16): boolean;
var
  twCapability: TW_CAPABILITY;
  pvalOneValue: pTW_ONEVALUE;
  pv32: pTW_INT32;
begin
  if not GetCapability(grec, twCapability, cap) then
  begin
    result := false;
    exit;
  end;
  LogWrite('SetOneINT32Capability');
  GlobalFree(twCapability.hContainer);
  twCapability.Cap := cap;
  // write TW_ONEVALUE (current value only)
  twCapability.ConType := TWON_ONEVALUE;
  twCapability.hContainer := GlobalAlloc(GHND, sizeof(TW_ONEVALUE));
  try
    pvalOneValue := GlobalLock(twCapability.hContainer);
    pvalOneValue^.ItemType := TWTY_INT32;
    pv32 := @(pvalOneValue^.Item);
    pv32^ := value;
    GlobalUnLock(twCapability.hContainer);
    IETW_DS(grec, DG_CONTROL, DAT_CAPABILITY, MSG_SET, @twCapability);
    result := grec.rc = TWRC_SUCCESS;
  finally
    GlobalFree(twCapability.hContainer);
  end;
  if result then
    LogWrite('  SetOneUINT16Capability : ok')
  else
    LogWrite('  SetOneUINT16Capability : FAILED!');
end;


// Need source loaded
function IETW_SetCapabilities(var grec: tgrec): boolean;
var
  Units: TIEIntegerList;
  xbuf: boolean;
  xfer: TIEIntegerList;
  chunk: TIEIntegerList;
  itmp: TIEIntegerList;
begin
  LogWrite('IETW_SetCapabilities');
  if not grec.TWParams.CompatibilityMode then
  begin

    // 3.0.0: ICAP_PIXELTYPE and ICAP_BITDEPTH position
    // ICAP_PIXELTYPE
    // note: ImageEn buffer xfer supports BW(1bit), GRAYSCALE (8bit), RGB
    if grec.TWParams.PixelType.CurrentValue > 2 then
      grec.TWParams.PixelType.CurrentValue := 2; // force to RGB when type is PALETTE...CIEXYZ
    LogWrite('  IETW_SetCapabilities : ICAP_PIXELTYPE');
    xbuf := SetIntegerAsUINT16Capability(grec, grec.TWParams.PixelType, ICAP_PIXELTYPE);

    // ICAP_BITDEPTH
    if grec.TWParams.BitDepth.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_BITDEPTH');
      SetIntegerAsUINT16Capability(grec, grec.TWParams.BitDepth, ICAP_BITDEPTH);
    end;

    // set ICAPS_UNITS first
    LogWrite('  IETW_SetCapabilities : ICAP_UNITS');
    Units := TIEIntegerList.Create;
    try
      if GetUINT16asInteger(grec, Units, ICAP_UNITS) and (Units.CurrentValue <> 0) then
      begin
        Units.CurrentValue := 0;
        SetIntegerAsUINT16Capability(grec, Units, ICAP_UNITS);
      end;
    finally
      FreeAndNil(Units);
    end;

    // ICAP_AUTOBRIGHT
    if grec.TWParams.AutoBright then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_AUTOBRIGHT');
      SetOneBoolCapability(grec, grec.TWParams.AutoBright, ICAP_AUTOBRIGHT);
    end;

    // ICAP_BRIGHTNESS
    if grec.TWParams.Brightness.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_BRIGHTNESS');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.Brightness, ICAP_BRIGHTNESS);
    end;

    // ICAP_UNDEFINEDIMAGESIZE
    if grec.TWParams.UndefinedImageSize then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_UNDEFINEDIMAGESIZE');
      SetOneBoolCapability(grec, true, ICAP_UNDEFINEDIMAGESIZE);
    end;

    // ICAP_CONTRAST
    if grec.TWParams.Contrast.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_CONTRAST');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.Contrast, ICAP_CONTRAST);
    end;

    // ICAP_FILTER
    if grec.TWParams.Filter<>ietwUndefined then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_FILTER');
      SetOneUINT16Capability(grec, integer(grec.TWParams.Filter)-1, ICAP_FILTER);
    end;

    // ICAP_THRESHOLD
    if grec.TWParams.Threshold.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_THRESHOLD');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.Threshold, ICAP_THRESHOLD);
    end;

    // ICAP_ROTATION
    if grec.TWParams.Rotation.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_ROTATION');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.Rotation, ICAP_ROTATION);
    end;

    // ICAP_PIXELFLAVOR
    LogWrite('  IETW_SetCapabilities : ICAP_PIXELFLAVOR');
    SetOneUINT16Capability(grec, TWPF_CHOCOLATE, ICAP_PIXELFLAVOR);
    itmp := TIEIntegerList.Create;
    try
      if GetUINT16asInteger(grec, itmp, ICAP_PIXELFLAVOR) then
        grec.BWToInvert := itmp.CurrentValue = TWPF_VANILLA
      else
        grec.BWToInvert := false; // not supported capability, assume TWPF_CHOCOLATE
    finally
      FreeAndNil(itmp);
    end;

    // ICAP_COMPRESSION (2.3.1)
    LogWrite('  IETW_SetCapabilities : ICAP_COMPRESSION');
    SetOneUINT16Capability(grec, TWCP_NONE, ICAP_COMPRESSION);

    // ICAP_XRESOLUTION
    if grec.TWParams.XResolution.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_XRESOLUTION');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.XResolution, ICAP_XRESOLUTION);
    end;

    // ICAP_YRESOLUTION
    if grec.TWParams.YResolution.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_YRESOLUTION');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.YResolution, ICAP_YRESOLUTION);
    end;

    // ICAP_XSCALING
    if grec.TWParams.XScaling.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_XSCALING');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.XScaling, ICAP_XSCALING);
    end;

    // ICAP_YSCALING
    if grec.TWParams.YScaling.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_YSCALING');
      SetDoubleAsFIX32Capability(grec, grec.TWParams.YScaling, ICAP_YSCALING);
    end;

    // set Chunky mode
    chunk := TIEIntegerList.Create;
    try
      GetUINT16asInteger(grec, chunk, ICAP_PLANARCHUNKY);
      if chunk.CurrentValue <> TWPC_CHUNKY then
      begin
        if xbuf and (grec.TWParams.PixelType.CurrentValue = 2) then
        begin
          LogWrite('  IETW_SetCapabilities : TWPC_CHUNKY - ICAP_PLANARCHUNKY');
          xbuf := SetOneUINT16Capability(grec, TWPC_CHUNKY, ICAP_PLANARCHUNKY);
        end;
      end;
    finally
      FreeAndNil(chunk);
    end;

    // set "memory mode transfer"
    if (grec.TransferMode = tmBuffered) and xbuf then
    begin
      xfer := TIEIntegerList.Create;
      try
        GetUINT16asInteger(grec, xfer, ICAP_XFERMECH);
        if xfer.IndexOf(TWSX_MEMORY) < 0 then
          grec.TransferMode := tmNative // do not support memory transfer (buffered transfer)
        else
        begin
          LogWrite('  IETW_SetCapabilities : TWSX_MEMORY - ICAP_XFERMECH');
          if not SetOneUINT16Capability(grec, TWSX_MEMORY, ICAP_XFERMECH) then
            grec.TransferMode := tmNative
          else
          begin
            xfer.Clear;
            GetUINT16asInteger(grec, xfer, ICAP_XFERMECH);
            if xfer.CurrentValue <> TWSX_MEMORY then
              grec.TransferMode := tmNative;
          end;
        end;
      finally
        FreeAndNil(xfer);
      end;
    end
    else
    if grec.TransferMode <> tmFile then
      grec.TransferMode := tmNative;

    // CAP_FEEDERENABLED
    LogWrite('  IETW_SetCapabilities : CAP_FEEDERENABLED');
    SetOneBoolCapability(grec, grec.TWParams.FeederEnabled, CAP_FEEDERENABLED);

    // CAP_AUTOFEED
    if grec.TWParams.AutoFeed then
    begin
      LogWrite('  IETW_SetCapabilities : CAP_AUTOFEED');
      SetOneBoolCapability(grec, grec.TWParams.AutoFeed, CAP_AUTOFEED);
    end;

    // ICAP_AUTOMATICDESKEW
    if grec.TWParams.AutoDeskew then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_AUTOMATICDESKEW');
      SetOneBoolCapability(grec, grec.TWParams.AutoDeskew, ICAP_AUTOMATICDESKEW);
    end;

    // ICAP_AUTOMATICBORDERDETECTION
    if grec.TWParams.AutoBorderDetection then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_AUTOMATICBORDERDETECTION');
      SetOneBoolCapability(grec, grec.TWParams.AutoBorderDetection, ICAP_AUTOMATICBORDERDETECTION);
    end;

    // ICAP_AUTOMATICROTATE
    if grec.TWParams.AutoRotate then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_AUTOMATICROTATE');
      SetOneBoolCapability(grec, grec.TWParams.AutoRotate, ICAP_AUTOMATICROTATE);
    end;

    // ICAP_AUTODISCARDBLANKPAGES
    if grec.TWParams.AutoDiscardBlankPages<>-2 then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_AUTODISCARDBLANKPAGES');
      SetOneINT32Capability(grec, grec.TWParams.AutoDiscardBlankPages, ICAP_AUTODISCARDBLANKPAGES);
    end;

    // ICAP_HIGHLIGHT
    if grec.TWParams.Highlight<>-1 then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_HIGHLIGHT');
      SetOneDoubleAsFIX32Capability(grec, grec.TWParams.Highlight, ICAP_HIGHLIGHT);
    end;

    // ICAP_SHADOW
    if grec.TWParams.Shadow<>-1 then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_SHADOW');
      SetOneDoubleAsFIX32Capability(grec, grec.TWParams.Shadow, ICAP_SHADOW);
    end;

    // CAP_AUTOSCAN
    LogWrite('  IETW_SetCapabilities : CAP_AUTOSCAN');
    SetOneBoolCapability(grec, grec.TWParams.AutoScan, CAP_AUTOSCAN);

    // CAP_XFERCOUNT
    // -2 = don't care (scanner default), -1 = multiple images, >1 = one o more images
    if grec.TWParams.AcceptedImages<>-2 then
    begin
      LogWrite('  IETW_SetCapabilities : CAP_XFERCOUNT');
      SetOneINT16Capability(grec, grec.TWParams.AcceptedImages, CAP_XFERCOUNT);
    end;

    // before 2.2.3 we first call ICAP_ORIENTATION then ICAP_SUPPORTEDSIZE
    // ICAP_SUPPORTEDSIZES
    if grec.TWParams.StandardSize.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_SUPPORTEDSIZES');
      SetIntegerAsUINT16Capability(grec, grec.TWParams.StandardSize, ICAP_SUPPORTEDSIZES);
    end;
    // ICAP_ORIENTATION
    if grec.TWParams.Orientation.Changed <> [] then
    begin
      LogWrite('  IETW_SetCapabilities : ICAP_ORIENTATION');
      SetIntegerAsUINT16Capability(grec, grec.TWParams.Orientation, ICAP_ORIENTATION);
    end;

    // CAP_INDICATORS
    LogWrite('  IETW_SetCapabilities : CAP_INDICATORS');
    SetOneBoolCapability(grec, grec.TWParams.ProgressIndicators, CAP_INDICATORS);

    // CAP_DUPLEXENABLED
    if grec.TWParams.DuplexSupported then
    begin
      LogWrite('  IETW_SetCapabilities : CAP_DUPLEXENABLED');
      SetOneBoolCapability(grec, grec.TWParams.DuplexEnabled, CAP_DUPLEXENABLED);
    end;

    // SETACQUIREFRAME
    if grec.TWParams.AcquireFrameEnabled then
      with grec.TWParams do
      begin
        LogWrite('  IETW_SetCapabilities : SETACQUIREFRAME');
        SetAcquireFrame(grec, AcquireFrameLeft, AcquireFrameTop, AcquireFrameRight, AcquireFrameBottom);
      end;

  end
  else
  begin
    // compatibility mode
    grec.TransferMode := tmFile;
  end;
  result := true;
  LogWrite('  IETW_SetCapabilities : end');
end;


// Bitmap is nil for multipage acquisition
function IETW_Acquire(Bitmap: TIEBitmap; multi: boolean; MultiCallBack: TIEMultiCallBack; Params: TIETwainParams; IOParams: TIOParamsVals; var Progress: TProgressRec; TwainShared: PIETwainShared; callwnd: HWND; DoNativePixelFormat: boolean): boolean;
var
  grec: tgrec;
  wnd: HWND;
begin
  if not Params.CapabilitiesValid then
    Params.GetFromScanner;

  result := false;

  if IEGlobalSettings().IsInsideTwain then
    exit;
  IEGlobalSettings().IsInsideTwain := true;

  try

    Init_grec(grec);
    grec.NativePixelFormat := DoNativePixelFormat;
    grec.callwnd           := callwnd;
    grec.PTwainShared      := TwainShared;
    grec.modal             := true;
    with grec do
    begin
      transferdone := false;
      closedsok    := false;
      bHideUI      := not Params.VisibleDialog;
      uionly       := Params.ShowSettingsOnly;
      gmulti       := multi;
      TWParams     := Params;
    end;
    LogWrite('IETW_Acquire');
    grec.IOParams := IOParams;
    grec.fBitmap  := Bitmap;
    if Params.CompatibilityMode then
      Params.FileTransfer := true;
    if Params.FileTransfer then
      grec.TransferMode := tmFile
    else
    if Params.BufferedTransfer then
      grec.TransferMode := tmBuffered
    else
      grec.TransferMode := tmNative;
    grec.MultiCallBack := MultiCallBack;
    grec.Progress := @Progress;
    Set_AppId(grec);
    ClearError(grec); // clear error detail
    wnd := CreateProxyWindow(grec);
    grec.proxywnd := wnd;
    if (not IETW_LoadSourceManager(grec)) then
    begin
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    if (not IETW_OpenSourceManager(grec, wnd)) then
    begin
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    if (not IETW_OpenSource(grec)) then
    begin
      IETW_CloseSourceManager(grec, wnd);
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    if (not IETW_SetCapabilities(grec)) then
    begin
      IETW_CloseSource(grec);
      IETW_CloseSourceManager(grec, wnd);
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    if (not IETW_EnableSource(grec, wnd)) then
    begin
      IETW_CloseSource(grec);
      IETW_CloseSourceManager(grec, wnd);
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit
    end;
    EnableWindow(wnd, FALSE);
    // source is enabled, wait for transfer or source closed
    try
      IETW_ModalEventLoop(grec);
    finally
      EnableWindow(wnd, TRUE);
    end;
    // shut everything down in the right sequence
    // these routines do nothing if the corresponding 'open' failed
    IETW_DisableSource(grec);
    IETW_CloseSource(grec);
    IETW_CloseSourceManager(grec, wnd);
    IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
    DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
    // invert black/white image if necessary
    if assigned(Bitmap) and (Bitmap.PixelFormat = ie1g) and grec.BWToInvert then
      _Negative1BitEx(Bitmap);
    //
    result := grec.transferdone or (grec.closedsok and grec.uionly);
    LogWrite('  IETW_Acquire : end');
  finally
    if grec.fWindowList <> nil then
      EnableTaskWindows(grec.fWindowList);
    windows.SetActiveWindow(grec.actwnd);
    IEGlobalSettings().IsInsideTwain := false;
  end;
end;


// if setcap is True then set capabilities and doesn't change the "Changed" field of the lists
// return false if fails
function IETW_GetCapabilities(Params: TIETwainParams; var Capabilities: TIETWSourceCaps; setcap: boolean; TwainShared: PIETwainShared; callwnd: HWND): boolean;
var
  grec: tgrec;
  wnd: HWND;
  Units: TIEIntegerList;
  temp_i: integer;
  fXResolutionChanged: TIEListChanges;
  fYResolutionChanged: TIEListChanges;
  fXScalingChanged: TIEListChanges;
  fYScalingChanged: TIEListChanges;
  fPixelTypeChanged: TIEListChanges;
  fBitDepthChanged: TIEListChanges;
  fOrientationChanged: TIEListChanges;
  fContrastChanged: TIEListChanges;
  fBrightnessChanged: TIEListChanges;
  fStandardSizeChanged: TIEListChanges;
  fThresholdChanged: TIEListChanges;
  fRotationChanged: TIEListChanges;
begin
  result := false;

  if IEGlobalSettings().IsInsideTwain then
    exit;
  IEGlobalSettings().IsInsideTwain := true;

  try
    Init_grec(grec);
    grec.callwnd      := callwnd;
    grec.PTwainShared := TwainShared;
    grec.TWParams     := Params;
    if Params.CompatibilityMode then
      Params.FileTransfer := true;
    if Params.FileTransfer then
      grec.TransferMode := tmFile
    else
    if Params.BufferedTransfer then
      grec.TransferMode := tmBuffered
    else
      grec.TransferMode := tmNative;
    ClearError(grec); // clear error detail

    wnd := CreateProxyWindow(grec);
    grec.proxywnd := wnd;

    if (not IETW_LoadSourceManager(grec)) then
    begin
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    if (not IETW_OpenSourceManager(grec, wnd)) then
    begin
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    if (not IETW_OpenSource(grec)) then
    begin
      IETW_CloseSourceManager(grec, wnd);
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
      DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
      exit;
    end;
    if setcap then
      IETW_SetCapabilities(grec);
    with grec, Capabilities do
    begin
      fXResolutionChanged := fXResolution.Changed;
      fYResolutionChanged := fYResolution.Changed;
      fXScalingChanged := fXScaling.Changed;
      fYScalingChanged := fYScaling.Changed;
      fPixelTypeChanged := fPixelType.Changed;
      fBitDepthChanged := fBitDepth.Changed;
      fOrientationChanged := fOrientation.Changed;
      fContrastChanged := fContrast.Changed;
      fBrightnessChanged := fBrightness.Changed;
      fStandardSizeChanged := fStandardSize.Changed;
      fThresholdChanged := fThreshold.Changed;
      fRotationChanged := fRotation.Changed;
      if not Params.CompatibilityMode then
      begin
        // set ICAPS_UNITS first
        Units := TIEIntegerList.Create;
        try
          if GetUINT16asInteger(grec, Units, ICAP_UNITS) and (Units.CurrentValue <> 0) then
          begin
            Units.CurrentValue := 0;
            SetIntegerAsUINT16Capability(grec, Units, ICAP_UNITS);
          end;
        finally
          FreeAndNil(Units);
        end;
      end;
      // ICAP_XRESOLUTION
      if not GetFix32asDouble(grec, fXResolution, ICAP_XRESOLUTION) then
        fXResolution.Clear;
      // ICAP_YRESOLUTION
      if not GetFix32asDouble(grec, fYResolution, ICAP_YRESOLUTION) then
        fYResolution.Clear;
      if not Params.CompatibilityMode then
      begin
        // ICAP_XSCALING
        if not GetFix32asDouble(grec, fXScaling, ICAP_XSCALING) then
        begin
          fXScaling.Clear;
          fXScaling.Add(1);
        end;
        // ICAP_YSCALING
        if not GetFix32asDouble(grec, fYScaling, ICAP_YSCALING) then
        begin
          fYScaling.Clear;
          fYScaling.Add(1);
        end;
        // ICAP_CONTRAST
        if not GetFix32asDouble(grec, fContrast, ICAP_CONTRAST) then
          fContrast.Clear;
        // ICAP_BRIGHTNESS
        if not GetFix32asDouble(grec, fBrightness, ICAP_BRIGHTNESS) then
          fBrightness.Clear;
        // ICAP_THRESHOLD
        if not GetFix32asDouble(grec, fThreshold, ICAP_THRESHOLD) then
          fThreshold.Clear;
        // ICAP_ROTATION
        if not GetFix32asDouble(grec, fRotation, ICAP_ROTATION) then
          fRotation.Clear;
        // ICAP_PIXELTYPE
        if not GetUINT16asinteger(grec, fPixelType, ICAP_PIXELTYPE) then
          fPixelType.Clear;
        // ICAP_BITDEPTH
        if not GetUINT16asinteger(grec, fBitDepth, ICAP_BITDEPTH) then
          fBitDepth.Clear;
        // ICAP_GAMMA
        if not GetOneFIX32asDouble(grec, fGamma, ICAP_GAMMA) then
          fGamma := 2.2;
        // ICAP_PHYSICALHEIGHT
        if not GetOneFIX32asDouble(grec, fPhysicalHeight, ICAP_PHYSICALHEIGHT) then
          fPhysicalHeight := 0;
        // ICAP_PHYSICALWIDTH
        if not GetOneFIX32asDouble(grec, fPhysicalWidth, ICAP_PHYSICALWIDTH) then
          fPhysicalWidth := 0;
        // CAP_FEEDERENABLED
        if not GetOneBOOL(grec, fFeederEnabled, CAP_FEEDERENABLED) then
          fFeederEnabled := false;
        // CAP_AUTOFEED
        if not GetOneBOOL(grec, fAutoFeed, CAP_AUTOFEED) then
          fAutoFeed := false;

        // ICAP_AUTOMATICDESKEW
        if not GetOneBOOL(grec, fAutoDeskew, ICAP_AUTOMATICDESKEW) then
          fAutoDeskew := false;
        // ICAP_AUTOMATICBORDERDETECTION
        if not GetOneBOOL(grec, fAutoBorderDetection, ICAP_AUTOMATICBORDERDETECTION) then
          fAutoBorderDetection := false;
        // ICAP_AUTOBRIGHT
        if not GetOneBOOL(grec, fAutoBright, ICAP_AUTOBRIGHT) then
          fAutoBright := false;
        // ICAP_AUTOMATICROTATE
        if not GetOneBOOL(grec, fAutoRotate, ICAP_AUTOMATICROTATE) then
          fAutoRotate := false;
        // ICAP_AUTODISCARDBLANKPAGES
        if not GetOneINT32(grec, fAutoDiscardBlankPages, ICAP_AUTODISCARDBLANKPAGES) then
          fAutoDiscardBlankPages := -2;

        // ICAP_FILTER
        if not GetOneUINT16(grec, temp_i, ICAP_FILTER) then
          fFilter := ietwUndefined
        else
          fFilter := TIETWFilter(temp_i+1);

        // ICAP_HIGHLIGHT
        if not GetOneFIX32asDouble(grec, fHighlight, ICAP_HIGHLIGHT) then
          fHighlight := -1;

        // ICAP_SHADOW
        if not GetOneFIX32asDouble(grec, fShadow, ICAP_SHADOW) then
          fShadow := -1;

        // CAP_AUTOSCAN
        if not GetOneBOOL(grec, fAutoScan, CAP_AUTOSCAN) then
          fAutoScan := false;

        // CAP_DEVICEONLINE
        if not GetOneBOOL(grec, fDeviceOnline, CAP_DEVICEONLINE) then
          fDeviceOnline := false;

        // CAP_XFERCOUNT
        if not GetOneINT16(grec, temp_i, CAP_XFERCOUNT) then
          fAcceptedImages := -2
        else
          fAcceptedImages := temp_i;

        // CAP_FEEDERLOADED
        if not GetOneBOOL(grec, fFeederLoaded, CAP_FEEDERLOADED) then
          fFeederLoaded := false;
        // CAP_PAPERDETECTABLE
        if not GetOneBOOL(grec, fPaperDetectable, CAP_PAPERDETECTABLE) then
          fPaperDetectable := false;
        // CAP_DUPLEXENABLED
        if not GetOneBOOL(grec, fDuplexEnabled, CAP_DUPLEXENABLED) then
          fDuplexEnabled := false;
        // CAP_DUPLEX
        fDuplexSupported := GetOneUINT16(grec, temp_i, CAP_DUPLEX) and (temp_i<>0); // 3.0.3
        // ICAP_ORIENTATION
        if not GetUINT16asInteger(grec, fOrientation, ICAP_ORIENTATION) then
        begin
          fOrientation.Clear;
          fOrientation.Add(TWOR_PORTRAIT);
        end;
        // ICAP_SUPPORTEDSIZES (renamed as StandardSize)
        if not GetUINT16asInteger(grec, fStandardSize, ICAP_SUPPORTEDSIZES) then
          fStandardSize.Clear;
        // CAP_INDICATORS
        if not GetOneBOOL(grec, fIndicators, CAP_INDICATORS) then
          fIndicators := True;
        // Acquire Frame
        if fAcquireFrameEnabled then
          GetAcquireFrame(grec, fAcquireFrame);
        // clear Changed property of lists
        if not setcap then
        begin
          fXResolution.Changed := [];
          fYResolution.Changed := [];
          fXScaling.Changed := [];
          fYScaling.Changed := [];
          fPixelType.Changed := [];
          fBitDepth.Changed := [];
          fOrientation.Changed := [];
          fContrast.Changed := [];
          fBrightness.Changed := [];
          fStandardSize.Changed := [];
          fThreshold.Changed := [];
          fRotation.Changed := [];
        end
        else
        begin
          fXResolution.Changed := fXResolutionChanged;
          fYResolution.Changed := fYResolutionChanged;
          fXScaling.Changed := fXScalingChanged;
          fYScaling.Changed := fYScalingChanged;
          fPixelType.Changed := fPixelTypeChanged;
          fBitDepth.Changed := fBitDepthChanged;
          fOrientation.Changed := fOrientationCHanged;
          fContrast.Changed := fContrastChanged;
          fBrightness.Changed := fBrightnessChanged;
          fStandardSize.Changed := fStandardSizeChanged;
          fThreshold.Changed := fThresholdChanged;
          fRotation.Changed := fRotationChanged;
        end;
      end;
    end;

    IETW_CloseSource(grec);
    IETW_CloseSourceManager(grec, wnd);
    IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);

    DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
    result := true;
  finally
    windows.SetActiveWindow(grec.actwnd);
    IEGlobalSettings().IsInsideTwain := false;
  end;

end;


// returns ProductName of default source
function IETW_GetDefaultSource(TwainShared: PIETwainShared; callwnd: HWND): AnsiString;
var
  NewSourceId: TW_IDENTITY;
  grec: tgrec;
  wnd: HWND;
begin
  result := '';

  if IEGlobalSettings().IsInsideTwain then
    exit;
  IEGlobalSettings().IsInsideTwain := true;

  Init_grec(grec);
  try
    grec.callwnd := callwnd;
    grec.PTwainShared := TwainShared;
    wnd := CreateProxyWindow(grec);
    grec.proxywnd := wnd;
    if IETW_LoadSourceManager(grec) then
    begin
      if IETW_OpenSourceManager(grec, wnd) then
      begin
        fillmemory(@NewSourceId, sizeof(NewSourceId), 0);
        // Post the Select Source dialog
        if IETW_Mgr(grec, DG_CONTROL, DAT_IDENTITY, MSG_GETDEFAULT, @NewSourceId) then
          result := NewSourceId.ProductName;
        IETW_CloseSourceManager(grec, wnd);
      end
      else
      begin
        DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
        exit;
      end;
      IETW_UnloadSourceManager(grec, IEGlobalSettings().ReleaseTwainResources);
    end;
    DestroyProxyWindow(wnd, grec, IEGlobalSettings().ReleaseTwainResources);
  finally
    windows.SetActiveWindow(grec.actwnd);
    IEGlobalSettings().IsInsideTwain := false;
  end;
end;


// unload sourcemanager
procedure IETW_FreeResources(TwainShared: PIETwainShared; callwnd: HWND);
var
  grec: tgrec;
begin
  Init_grec(grec);
  grec.callwnd := callwnd;
  grec.PTwainShared := TwainShared;
  grec.hDSMLib := TwainShared.hDSMLib;
  grec.DSM_Entry := TwainShared.DSM_Entry;
  if grec.PTwainShared^.hproxy <> 0 then
    DestroyProxyWindow(0, grec, true);
  if grec.PTwainShared^.hDSMLib <> 0 then
    IETW_UnloadSourceManager(grec, true);
end;



////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
// New implementation
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////


procedure _IETWAINAcquireClose(var grec: pointer); forward;

type
  TIEProxyWin = class(TWinControl)
  public
    grec: pgrec;
    constructor NewCreate(AOwner: TComponent; xgrec: pgrec);
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
  end;


constructor TIEProxyWin.NewCreate(AOwner: TComponent; xgrec: pgrec);
begin
  inherited Create(AOwner);
  grec := xgrec;
end;


destructor TIEProxyWin.Destroy;
begin
  _IETWAINAcquireClose(pointer(grec));
  inherited;
end;


procedure TIEProxyWin.WndProc(var Message: TMessage);
var
  msg: TMSG;
begin
  msg.hwnd := handle;
  msg.message := Message.Msg;
  msg.wParam := Message.WParam;
  msg.lParam := Message.LParam;
  if not assigned(grec) then
    LogWrite('TIEProxyWin.WndProc. Msg=' + IEIntToStr(msg.message) + ' grec=' + IEIntToStr(NativeInt(grec)) + ' grec no assigned!')
  else
    LogWrite('TIEProxyWin.WndProc. Msg=' + IEIntToStr(msg.message) + ' grec=' + IEIntToStr(NativeInt(grec)) + ' state=' + IEIntToStr(grec^.nstate));
  if (not assigned(grec)) or (grec^.nState < 5) or (not IETW_MessageHook(grec^, @msg)) then
    inherited WndProc(Message);
end;


function IETWAINAcquireOpen(CloseCallBack: TIETWCloseCallBack; MultiCallBack: TIEMultiCallBack; Params: TIETwainParams; TwainShared: PIETwainShared; IOParams: TIOParamsVals; parent: TWinControl; DoNativePixelFormat: boolean): pointer;
var
  grec: pgrec;
begin
  new(grec);
  Init_grec(grec^);
  with grec^ do
  begin
    transferdone      := false;
    closedsok         := false;
    bHideUI           := not Params.VisibleDialog;
    TWParams          := Params;
    NativePixelFormat := DoNativePixelFormat;
  end;
  grec.PTwainShared := TwainShared;
  grec.modal := false;
  grec.IOParams := IOParams;
  if Params.FileTransfer then
    grec.TransferMode := tmFile
  else
  if Params.BufferedTransfer then
    grec.TransferMode := tmBuffered
  else
    grec.TransferMode := tmNative;
  grec.MultiCallBack := MultiCallBack;
  grec.Progress := nil;
  grec.gmulti := true;
  grec.fclosecallback := CloseCallBack;
  Set_AppId(grec^);
  ClearError(grec^); // clear error detail
  // create proxy window
  grec^.ProxyWin := TIEProxyWin.NewCreate(parent, grec);
  grec^.ProxyWin.Parent := parent;
  try
    if (not IETW_LoadSourceManager(grec^)) then
    begin
      FreeAndNil(grec.ProxyWin);
      //dispose(grec);  // already disposes by ProxyWin.free
      result := nil;
      exit;
    end;
    if (not IETW_OpenSourceManager(grec^, grec.ProxyWin.Handle)) then
    begin
      IETW_UnloadSourceManager(grec^, IEGlobalSettings().ReleaseTwainResources);
      FreeAndNil(grec.ProxyWin);
      dispose(grec);
      result := nil;
      exit;
    end;
    if (not IETW_OpenSource(grec^)) then
    begin
      IETW_CloseSourceManager(grec^, grec.ProxyWin.Handle);
      IETW_UnloadSourceManager(grec^, IEGlobalSettings().ReleaseTwainResources);
      FreeAndNil(grec.ProxyWin);
      //dispose(grec);  // already disposes by ProxyWin.free
      result := nil;
      exit;
    end;
    if (not IETW_SetCapabilities(grec^)) then
    begin
      IETW_CloseSource(grec^);
      IETW_CloseSourceManager(grec^, grec.ProxyWin.Handle);
      IETW_UnloadSourceManager(grec^, IEGlobalSettings().ReleaseTwainResources);
      FreeAndNil(grec.ProxyWin);
      //dispose(grec);  // already disposes by ProxyWin.free
      result := nil;
      exit;
    end;
    if (not IETW_EnableSource(grec^, grec.ProxyWin.Handle)) then
    begin
      IETW_CloseSource(grec^);
      IETW_CloseSourceManager(grec^, grec.ProxyWin.Handle);
      IETW_UnloadSourceManager(grec^, IEGlobalSettings().ReleaseTwainResources);
      FreeAndNil(grec.ProxyWin);
      //dispose(grec);  // already disposes by ProxyWin.free
      result := nil;
      exit
    end;
    result := grec;
    LogWrite('IETWAINAcquireOpen : result=  ' + IEIntToStr(integer(result)));
  finally
    //windows.SetActiveWindow(grec.actwnd);
  end;
end;


// important: ProxyWin will be destroyed by parent!!!
//             otherwise you can destroy it calling this function!
// called from external (then ProxyWin is not in destroying state)
procedure IETWAINAcquireClose(var grec: pointer);
begin
  LogWrite('IETWAINAcquireClose : begin');
  FreeAndNil(pgrec(grec)^.ProxyWin);
  LogWrite('IETWAINAcquireClose : end');
end;


// called from TIEProxyWin (ProxyWin must be in destroying state)
procedure _IETWAINAcquireClose(var grec: pointer);
var
  closecallback: TIETWCloseCallBack;
begin
  LogWrite('_IETWAINAcquireClose : begin');
  IETW_DisableSource(pgrec(grec)^);
  IETW_CloseSource(pgrec(grec)^);
  IETW_CloseSourceManager(pgrec(grec)^, pgrec(grec)^.hwndSM);
  IETW_UnloadSourceManager(pgrec(grec)^, true);
  closecallback := pgrec(grec)^.fclosecallback;
  dispose(grec);
  closecallback;
  grec := nil;
  LogWrite('_IETWAINAcquireClose : end');
end;


////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////




{$ELSE} // {$ifdef IEINCLUDEIEXACQUIRE}

interface
implementation

{$ENDIF}

end.
