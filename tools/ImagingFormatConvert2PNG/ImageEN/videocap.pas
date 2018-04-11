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

unit videocap;

{$R-}
{$Q-}

{$I ie.inc}

{$ifdef IESUPPORTDEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$endif}

{$IFDEF IEINCLUDEVIDEOCAPTURE}

interface

uses
  Windows, Messages, SysUtils, StdCtrls, Classes, Graphics, Controls, Forms, ImageEnView, ImageEnProc,
  hyiedefs, ievect, ieview, hyieutils;

type

  TVideoCapException = class(Exception);

{!!
<FS>TVCDisplayMode

<FM>Declaration<FC>
type TVCDisplayMode = (dmPreview, dmOverlay);

<FM>Description<FN>
dmPreview: copy video input into a temporary buffer, then display with GDI.
dmOverlay: bypass GDI by sending video input to video card directly.
!!}
  TVCDisplayMode = (dmPreview, dmOverlay);

{!!
<FS>TVideoFrameEvent

<FM>Declaration<FC>
type TVideoFrameEvent=procedure(Sender: TObject; Bitmap: <A TIEDibBitmap>) of object;

<FM>Description<FN>
The Bitmap parameter is the current frame.
!!}
  TVideoFrameEvent = procedure(Sender: TObject; Bitmap: TIEDibBitmap) of object;
  
{!!
<FS>TVideoFrameRawEvent

<FM>Declaration<FC>
TVideoFrameRawEvent = procedure(Sender: TObject; hDib: Thandle; pData: pointer) of object;

<FM>Description<FN>
hDib is an handle to TBITMAPINFO structure and pData is the pixels of the image.
!!}
  TVideoFrameRawEvent = procedure(Sender: TObject; hDib: Thandle; pData: pointer) of object;

{!!
<FS>TCAPDRIVERCAPS

<FM>Declaration<FC>
}
  TCAPDRIVERCAPS = record
    wDeviceIndex: integer;
    fHasOverlay: longbool;
    fHasDlgVideoSource: longbool;
    fHasDlgVideoFormat: longbool;
    fHasDlgVideoDisplay: longbool;
    fCaptureInitialized: longbool;
    fDriverSuppliesPalettes: longbool;
    hVideoIn: THandle;
    hVideoOut: THandle;
    hVideoExtIn: THandle;
    hVideoExtout: THandle;
  end;
{!!}

{!!
<FS>PCAPDRIVERCAPS

<FM>Declaration<FC>
type PCAPDRIVERCAPS = ^<A TCAPDRIVERCAPS>;
!!}
  PCAPDRIVERCAPS = ^TCAPDRIVERCAPS;

{!!
<FS>TVIDEOHDR

<FM>Declaration<FC>
}
  TVIDEOHDR = record
    lpData: pbyte;
    dwBufferLength: dword;
    dwBytesUsed: dword;
    dwTimeCaptured: dword;
    dwUser: dword;
    dwFlags: dword;
    dwReserved: array[0..3] of dword;
  end;
{!!}

{!!
<FS>PVIDEOHDR

<FM>Declaration<FC>
type PVIDEOHDR = ^<A TVIDEOHDR>;
!!}
  PVIDEOHDR = ^TVIDEOHDR;

  TCAPSTATUS = record
    uiImageWidth: dword;
    uiImageHeight: dword;
    fLiveWindow: longbool;
    fOverlayWindow: longbool;
    fScale: longbool;
    ptScroll: TPOINT;
    fUsingDefaultPalette: longbool;
    fAudioHardware: longbool;
    fCapFileExists: longbool;
    dwCurrentVideoFrame: dword;
    dwCurrentVideoFramesDropped: dword;
    dwCurrentWaveSamples: dword;
    dwCurrentTimeElapsedMS: dword;
    hPalCurrent: THandle;
    fCapturingNow: longbool;
    dwReturn: dword;
    wNumVideoAllocated: dword;
    wNumAudioAllocated: dword;
  end;
  PCAPSTATUS = ^TCAPSTATUS;

  PCAPTUREPARMS = ^TCAPTUREPARMS;
  TCAPTUREPARMS = record
    dwRequestMicroSecPerFrame: DWORD; // Requested capture rate
    fMakeUserHitOKToCapture: longbool; // Show "Hit OK to cap" dlg?

    //wPercentDropForError      : WORD;     // Give error msg if > (10%)
    wPercentDropForError: dword; // Give error msg if > (10%)

    fYield: longbool; // Capture via background task?
    dwIndexSize: DWORD; // Max index size in frames (32K)

    //wChunkGranularity         : WORD;     // Junk chunk granularity (2K)
    wChunkGranularity: dword; // Junk chunk granularity (2K)

    fUsingDOSMemory: longbool; // Use DOS buffers?

    //wNumVideoRequested        : WORD;     // # video buffers, If 0, autocalc
    wNumVideoRequested: dword; // # video buffers, If 0, autocalc

    fCaptureAudio: longbool; // Capture audio?

    //wNumAudioRequested        : WORD;     // # audio buffers, If 0, autocalc
    //vKeyAbort                 : WORD;     // Virtual key causing abort
    wNumAudioRequested: dword; // # audio buffers, If 0, autocalc
    vKeyAbort: dword; // Virtual key causing abort

    fAbortLeftMouse: longbool; // Abort on left mouse?
    fAbortRightMouse: longbool; // Abort on right mouse?
    fLimitEnabled: longbool; // Use wTimeLimit?

    //wTimeLimit                : WORD;     // Seconds to capture
    wTimeLimit: dword; // Seconds to capture

    fMCIControl: longbool; // Use MCI video source?
    fStepMCIDevice: longbool; // Step MCI device?
    dwMCIStartTime: DWORD; // Time to start in MS
    dwMCIStopTime: DWORD; // Time to stop in MS
    fStepCaptureAt2x: longbool; // Perform spatial averaging 2x

    //wStepCaptureAverageFrames : WORD;     // Temporal average n Frames
    wStepCaptureAverageFrames: dword; // Temporal average n Frames

    dwAudioBufferSize: DWORD; // Size of audio bufs (0 = default)
    fDisableWriteCache: longbool; // Attempt to disable write cache

    //AVStreamMaster            : WORD;     // Indicates whether the audio stream controls the clock when writing an AVI file.
    AVStreamMaster: dword; // Indicates whether the audio stream controls the clock when writing an AVI file.
  end;

  PWAVEFORMATEX = ^TWAVEFORMATEX;
  TWAVEFORMATEX = record
    wFormatTag: word;
    nChannels: word;
    nSamplesPerSec: dword;
    nAvgBytesPerSec: dword;
    nBlockAlign: word;
    wBitsPerSample: word;
    cbSize: word;
  end;

  //TcapVideoStreamCallback = function(hWnd: HWND; lpVHdr: PVIDEOHDR): LRESULT; stdcall;

{!!
<FS>TImageEnVideoView

<FM>Description<FN>
TImageEnVideoView is derived directly from <A TImageEnVect>. TImageEnVideoView has some properties and methods (zoom, scroll-bars...and, above all, bitmap field and vectorial object capability).

When you set <A TImageEnVideoView.ShowVideo> property to True the current image of TImageEnVideoView is overlapped from video input (stretched to current size of TImageEnVideoView component).

It is recommended that you use <A TImageEnView>.<A TImageEnView.IO>.<A TImageEnIO.DShowParams> to capture video from cameras, because it uses the more supported DirectShow API.

<FB>This component is deprecated.<FN>

<FM>Methods and Properties<FN>
- <A TImageEnVideoView.AudioBitsPerSample>
- <A TImageEnVideoView.AudioChannels>
- <A TImageEnVideoView.AudioFormat>
- <A TImageEnVideoView.AudioSamplesPerSec>
- <A TImageEnVideoView.DisplayMode>
- <A TImageEnVideoView.DoConfigureCompression>
- <A TImageEnVideoView.DoConfigureDisplay>
- <A TImageEnVideoView.DoConfigureFormat>
- <A TImageEnVideoView.DoConfigureSource>
- <A TImageEnVideoView.FitFreeze>
- <A TImageEnVideoView.Freeze>
- <A TImageEnVideoView.Frozen>
- <A TImageEnVideoView.GetVideoSize>
- <A TImageEnVideoView.HasDlgVideoDisplay>
- <A TImageEnVideoView.HasDlgVideoFormat>
- <A TImageEnVideoView.HasDlgVideoSource>
- <A TImageEnVideoView.HasOverlay>
- <A TImageEnVideoView.PreviewRate>
- <A TImageEnVideoView.RecAudio>
- <A TImageEnVideoView.RecFileName>
- <A TImageEnVideoView.RecFrameRate>
- <A TImageEnVideoView.RecMultitask>
- <A TImageEnVideoView.SaveFrame>
- <A TImageEnVideoView.ShowVideo>
- <A TImageEnVideoView.StartRecord>
- <A TImageEnVideoView.StopRecord>
- <A TImageEnVideoView.UnFreeze>
- <A TImageEnVideoView.VideoSource>
- <A TImageEnVideoView.VideoSourceList>
- <A TImageEnVideoView.WndCaptureHandle>

<FM>Events<FN>
- <A TImageEnVideoView.OnJob>
- <A TImageEnVideoView.OnVideoFrameRaw>
- <A TImageEnVideoView.OnVideoFrame>
!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TImageEnVideoView = class(TImageEnVect)
  private
    fShowVideo: boolean; // se true visualizza video input
    fFreeze: boolean; // se true l'immagine è statica e salvata in Bitmap
    fDisplayMode: TVCDisplayMode; // Preview/Overlay...
    fWndC: HWND; // Handle finestra Video Capture (0=da creare)
    fDrivers: TStringList; // driver disponibili
    fVideoSource: integer; // indice video source corrente
    fSScrollBars: TScrollStyle; // ombra di fScrollBars
    fPreviewRate: integer;
    fCallBackFrame: boolean; // Se True chiama attiva la callback CallBackFrameFunc
    fOnVideoFrame: TVideoFrameEvent;
    fOnVideoFrameRaw: TVideoFrameRawEvent;
    fhBitmapInfo: THandle; // Handle della Bitmapinfo riempita da FillBitmapInfo
    fBitmapInfoUp: boolean; // true se fhBitmapInfo è aggiornata (serve a FillBitmapInfo)
    fConnected: boolean; // true se connesso al driver
    fFitFreeze: boolean; // true adatta il freeze alla dimensione del componente
    fOnJob: TIEJobEvent;
    fHDrawDib: HDRAWDIB;
    fGrabFrame: boolean; // se true la callback acquisisce il frame in Bitmap
    //
    fRecFileName: AnsiString; // nome file destinazione
    fRecFrameRate: integer; // frames per second (dwRequestMicroSecPerFrame)
    fRecAudio: boolean; // true cattura audio (fCaptureAudio)
    fRecMultitask: boolean; // false disabilita multitasking (fYeld) [ESC=abort]
    fRecording: boolean; // true se in registrazione
    //
    fCreatingCaptureWindow: boolean;
  protected
    procedure SetShowVideo(v: boolean);
    procedure SetFreeze(v: boolean);
    procedure SetDisplayMode(v: TVCDisplayMode);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    function DriverConnect: boolean;
    function DriverConnectNE: boolean;
    procedure DriverDisconnect;
    procedure SetVideoSource(v: integer);
    procedure SetScrollBars(v: TScrollStyle); override;
    function GetHasDlgVideoSource: boolean;
    function GetHasDlgVideoFormat: boolean;
    function GetHasDlgVideoDisplay: boolean;
    function GetHasOverlay: boolean;
    procedure GetCaps(var fDriverCaps: TCAPDRIVERCAPS);
    procedure SetPreviewRate(v: integer);
    procedure SetCallBackFrame(v: boolean);
    procedure SetOnVideoFrame(v: TVideoFrameEvent);
    procedure SetOnVideoFrameRaw(v: TVideoFrameRawEvent);
    function FillBitmapInfo: boolean;
    procedure CreateCaptureWindow;
    procedure DestroyCaptureWindow;
    procedure DoJob(job: TIEJob; per: integer);
    procedure DecompRawFrame(OutBitmap: TIEDibBitmap; pix: pointer);
    function GetAudioFormat: word;
    procedure SetAudioFormat(v: word);
    function GetAudioChannels: word;
    procedure SetAudioChannels(v: word);
    function GetAudioSamplesPerSec: dword;
    procedure SetAudioSamplesPerSec(v: dword);
    function GetAudioBitsPerSample: word;
    procedure SetAudioBitsPerSample(v: word);
    procedure GetWaveFormat(var wf: TWAVEFORMATEX);
    procedure SetWaveFormat(var wf: TWAVEFORMATEX);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure FillDrivers;
    procedure Paint; override;
    procedure Select(x1, y1, x2, y2: integer; Op: TIESelOp); override;
    function DoConfigureSource: boolean;
    function DoConfigureFormat: boolean;
    function DoConfigureDisplay: boolean;
    function DoConfigureCompression: boolean;
    procedure Freeze;
    procedure UnFreeze;
    property Frozen: boolean read fFreeze write SetFreeze default false;
    procedure SaveFrame;

{!!
<FS>TImageEnVideoView.VideoSourceList

<FM>Declaration<FC>
property VideoSourceList: TStringList;

<FM>Description<FN>
This is the list of video capture (video source) drivers installed on the system. The format is: "Device_Name Device_Version". The index of the list corresponds to VideoSource property value.

Read-only
!!}
    property VideoSourceList: TStringList read fDrivers;

    property HasOverlay: boolean read GetHasOverlay;
    property HasDlgVideoSource: boolean read GetHasDlgVideoSource;
    property HasDlgVideoFormat: boolean read GetHasDlgVideoFormat;
    property HasDlgVideoDisplay: boolean read GetHasDlgVideoDisplay;
    function StartRecord: boolean;
    procedure StopRecord;

{!!
<FS>TImageEnVideoView.RecFileName

<FM>Declaration<FC>
property RecFileName: AnsiString

<FM>Description<FN>
RecFileName contains the file name (AVI file format) where to save the captured video input.
Default is 'Capture.avi'.

<FM>Example<FC>
ImageEnVideoView1.RecFileName := 'myfile.avi';
!!}
    property RecFileName: AnsiString read fRecFileName write fRecFileName;

{!!
<FS>TImageEnVideoView.RecFrameRate

<FM>Declaration<FC>
property RecFrameRate: integer;

<FM>Description<FN>
RecFrameRate is the number of frames per second captured on recording.
Default is 15.
!!}
    property RecFrameRate: integer read fRecFrameRate write fRecFrameRate;

{!!
<FS>TImageEnVideoView.RecAudio

<FM>Declaration<FC>
property RecAudio: boolean;

<FM>Description<FN>
Set RecAudio to True to capture audio input with video input.
Default is False.

!!}
    property RecAudio: boolean read fRecAudio write fRecAudio;

{!!
<FS>TImageEnVideoView.RecMultitask

<FM>Declaration<FC>
property RecMultitask: boolean;

<FM>Description<FN>
If RecMultitask is False the system is locked to wait the end of recording. To stop recording press ESC.
Default is True.

!!}
    property RecMultitask: boolean read fRecMultitask write fRecMultitask;

{!!
<FS>TImageEnVideoView.WndCaptureHandle

<FM>Declaration<FC>
property WndCaptureHandle: HWND;

<FM>Description<FN>
WndCaptureHandle is the handle of the video capture window. It is useful to send messages to Video for Windows system.

<FM>Example<FC>
// Activates Preview mode bypassing TImageEnVideoView component
SendMessage(ImageEnVideoView1.WndCaptureHandle, WM_CAP_SETPREVIEW, 1, 0);

// Activates Overlay mode bypassing TImageEnVideoView component
SendMessage(ImageEnVideoView1.WndCaptureHandle, WM_CAP_SETOVERLAY, 1, 0);
!!}
    property WndCaptureHandle: HWND read fWndC;

    function GetVideoSize: TRect;
    property AudioFormat: word read GetAudioFormat write SetAudioFormat;
    property AudioChannels: word read GetAudioChannels write SetAudioChannels;
    property AudioSamplesPerSec: dword read GetAudioSamplesPerSec write SetAudioSamplesPerSec;
    property AudioBitsPerSample: word read GetAudioBitsPerSample write SetAudioBitsPerSample;
  published

{!!
<FS>TImageEnVideoView.FitFreeze

<FM>Declaration<FC>
property FitFreeze: boolean;

<FM>Description<FN>
If True (default) the video input frozen and is adapted to the component size (uses a triangular filter if the video input is smaller than component, to improve quality), otherwise the freezed image is what you have selected with Format dialog.
!!}
    property FitFreeze: boolean read fFitFreeze write fFitFreeze default true;

    property DisplayMode: TVCDisplayMode read fDisplayMode write SetDisplayMode default dmPreview;
    property ShowVideo: boolean read fShowVideo write SetShowVideo default false;
    property VideoSource: integer read fVideoSource write SetVideoSource default 0;
    property PreviewRate: integer read fPreviewRate write SetPreviewRate default 60;
    property OnVideoFrame: TVideoFrameEvent read fOnVideoFrame write SetOnVideoFrame;
    property OnVideoFrameRaw: TVideoFrameRawEvent read fOnVideoFrameRaw write SetOnVideoFrameRaw;

{!!
<FS>TImageEnVideoView.OnJob

<FM>Declaration<FC>
property OnJob: <A TIEJobEvent>;

<FM>Description<FN>
The OnJob event is generated on video capture jobs, as connecting, video format negotiations...
Supported job of TImageEnVideoView have "iejVIDEOCAP_" prefix. See TIEJobEvent type for more details.

<FM>Example<FC>
procedure TForm1.ImageEnVideoView1Job(Sender: TObject; job: TIEJob; per: Integer);
begin
  case job of
    iejNOTHING: Label8.Caption := '';
    iejVIDEOCAP_CONNECTING: Label8.Caption := 'Connecting...';
    iejVIDEOCAP_TRYVIDEOFORMATS: Label8.Caption := 'Trying supported video formats...'+inttostr(per)+'%';
    iejVIDEOCAP_NEGOTIATINGVIDEOFORMAT: Label8.Caption := 'Negotiating video format...';
  end;
  Application.ProcessMessages;
end;
!!}
    property OnJob: TIEJobEvent read fOnJob write fOnJob;

  end {$ifdef IESUPPORTDEPRECATED} deprecated {$endif};

implementation

uses iesettings;

{$R-}

const

  DLL2 = 'AVICAP32.DLL';

  // VIDEOCAP CONSTS
  WM_CAP_START = WM_USER;
  WM_CAP_DRIVER_CONNECT = WM_CAP_START + 10;
  WM_CAP_SEQUENCE = WM_CAP_START + 62;
  WM_CAP_STOP = WM_CAP_START + 68;
  WM_CAP_FILE_SET_CAPTURE_FILE = WM_CAP_START + 20;
  WM_CAP_SETPREVIEW = WM_CAP_START + 50;
  WM_CAP_SETPREVIEWRATE = WM_CAP_START + 52;
  WM_CAP_SETOVERLAY = WM_CAP_START + 51;
  WM_CAP_SET_SCALE = WM_CAP_START + 53;
  WM_CAP_DRIVER_DISCONNECT = WM_CAP_START + 11;
  WM_CAP_GRAB_FRAME = WM_CAP_START + 60;
  WM_CAP_GRAB_FRAME_NOSTOP = WM_CAP_START + 61;
  WM_CAP_SET_CALLBACK_FRAME = WM_CAP_START + 5;
  WM_CAP_DLG_VIDEOFORMAT = WM_CAP_START + 41;
  WM_CAP_DLG_VIDEOSOURCE = WM_CAP_START + 42;
  WM_CAP_DLG_VIDEODISPLAY = WM_CAP_START + 43;
  WM_CAP_DRIVER_GET_CAPS = WM_CAP_START + 14;
  WM_CAP_GET_VIDEOFORMAT = WM_CAP_START + 44;
  WM_CAP_SET_VIDEOFORMAT = WM_CAP_START + 45;
  WM_CAP_DRIVER_GET_NAME = WM_CAP_START + 12;
  WM_CAP_SET_SEQUENCE_SETUP = WM_CAP_START + 64;
  WM_CAP_GET_SEQUENCE_SETUP = WM_CAP_START + 65;
  WM_CAP_DLG_VIDEOCOMPRESSION = WM_CAP_START + 46;
  WM_CAP_FILE_SAVEDIB = WM_CAP_START + 25;
  WM_CAP_EDIT_COPY = WM_CAP_START + 30;
  WM_CAP_SET_USER_DATA = WM_CAP_START + 9;
  WM_CAP_GET_USER_DATA = WM_CAP_START + 8;
  WM_CAP_SET_AUDIOFORMAT = WM_CAP_START + 35;
  WM_CAP_GET_AUDIOFORMAT = WM_CAP_START + 36;

  // AVICAP

function capCreateCaptureWindowA(lpszWindowName: PAnsiChar; dwStyle: dword; x, y, nWidth, nHeight: integer; hwndParent: HWND; nID: integer): HWND; stdcall; external DLL2;

function capGetDriverDescriptionA(wDriverIndex: integer; lpszName: PAnsiChar; cnName: integer; lpszVer: PAnsiChar; cbVer: integer): longbool; stdcall; external DLL2;

function CallBackFrameFunc(hWnd: HWND; lpVHdr: PVIDEOHDR): LRESULT; stdcall; forward;

/////////////////////////////////////////////////////////////////////////////////////

constructor TImageEnVideoView.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  fFitFreeze := true;
  fCallBackFrame := false;
  fDrivers := TStringList.Create;
  fPreviewRate := 60;
  fVideoSource := 0;
{$IFNDEF OCXVERSION}
  FillDrivers;
{$ENDIF}
  fWndC := 0;
  fShowVideo := false;
  fFreeze := false;
  fDisplayMode := dmPreview;
  fOnVideoFrame := nil;
  fhBitmapInfo := GlobalAlloc(GHND, sizeof(TBITMAPINFO) + sizeof(TRGBQUAD) * 256);
  fConnected := false;
  fBitmapInfoUp := false;
  fRecFileName := 'Capture.avi';
  fRecFrameRate := 15; // 15 frames per second (dwRequestMicroSecPerFrame=66667)
  fRecAudio := false;
  fRecMultitask := true;
  fRecording := false;
  fOnJob := nil;
  fHDrawDib := IEDrawDibOpen;
  fCreatingCaptureWindow := false;
end;

/////////////////////////////////////////////////////////////////////////////////////

destructor TImageEnVideoView.Destroy;
begin
  FreeAndNil(fDrivers);
  DestroyCaptureWindow;
  GlobalFree(fhBitmapInfo);
  IEDrawDibClose(fHDrawDib);
  //
  inherited;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.FillDrivers;
var
  DeviceName: array[0..79] of AnsiChar;
  DeviceVersion: array[0..79] of AnsiChar;
  q: integer;
begin
  fDrivers.Clear;
  for q := 0 to 9 do
  begin
    if capGetDriverDescriptionA(q, DeviceName, 80, DeviceVersion, 80) then
      fDrivers.Add(string(AnsiString(DeviceName) + ' ' + AnsiString(DeviceVersion)));
  end;
end;


{!!
<FS>TImageEnVideoView.ShowVideo

<FM>Declaration<FC>
property ShowVideo: boolean;

<FM>Description<FN>
Set ShowVideo to True to show the current video source input (see VideoSource property) and hide the current image of TImageEnVideoView component.
The scrollbars will be hidden.

You can set this property to True at design time and see the video input, but remember to set it to False when running the application, because only one video input is allowed at a time.
You can have multiple TImageEnVideoView component on your form but only one can have ShowVideo property set to True (if they use some video source).
!!}
// fWndC=0 allora SetShowVideo sarà richiamata da Paint
// - Se il driver è occupato genera l'eccezione TVideoCapException.
procedure TImageEnVideoView.SetShowVideo(v: boolean);
begin
  if v = fShowVideo then
    exit;
  if v then
  begin
    fFreeze := false;
    // hide scrollsbars if visible
    if fScrollBars <> ssNone then
      inherited SetScrollBars(ssNone);
    // VISUALIZZA VIDEO INPUT
    {$ifdef OCXVERSION}
    if (fWndC = 0) then
      CreateCaptureWindow;
    {$endif}
    if fWndC = 0 then
      exit;
    if not fConnected then
      if not DriverConnect then
        exit;
    SendMessage(fWndC, WM_CAP_SET_SCALE, 1, 0);
    SetDisplayMode(fDisplayMode);
    SetCallBackFrame(fCallBackFrame); // ribadisce...
    ShowWindow(fWndC, SW_SHOWNORMAL);
    UpdateWindow(fWndC);
    SendMessage(fWndC, WM_CAP_SET_USER_DATA, 0, LPARAM(pointer(self)));
    fShowVideo := true;
  end
  else
  begin
    // Hide video input
    SendMessage(fWndC, WM_CAP_SET_USER_DATA, 0, 0);
    DriverDisconnect;
    //SendMessage(fWndC, WM_CAP_SETPREVIEW, 0, 0);
//SendMessage(fWndC, WM_CAP_SETOVERLAY, 0, 0);
    ShowWindow(fWndC, SW_HIDE);
    fShowVideo := false;
    // if was present enables scrollbars
    if (fSScrollBars <> ssNone) and not (csDestroying in ComponentState) then
      inherited SetScrollBars(fSScrollBars);
  end;
end;


{!!
<FS>TImageEnVideoView.DisplayMode

<FM>Declaration<FC>
property DisplayMode: <A TVCDisplayMode>;

<FM>Description<FN>
Select Overlay (dmOverlay) or Preview (dmPreview) display mode.
The default is dmPreview.
!!}
// nota: eseguire sempre anche se v=fDisplaymode. (vedi SetShowVideo o SetFreeze)
// se fWndC=0 allora SetDisplayMode sarà richiamato da Paint
procedure TImageEnVideoView.SetDisplayMode(v: TVCDisplayMode);
begin
  fDisplayMode := v;
  if fConnected then
  begin
    Deselect;
    if not fFreeze then
    begin
      if fDisplayMode = dmPreview then
      begin
        // preview
        SendMessage(fWndC, WM_CAP_SETPREVIEWRATE, fPreviewRate, 0);
        SendMessage(fWndC, WM_CAP_SETPREVIEW, 1, 0);
      end
      else
      begin
        // overlay
        SendMessage(fWndC, WM_CAP_SETPREVIEW, 1, 0);
        SendMessage(fWndC, WM_CAP_SETOVERLAY, 1, 0);
      end;
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.Select(x1, y1, x2, y2: integer; Op: TIESelOp);
begin
  if (fFreeze) or (not fShowVideo) then
    inherited Select(x1, y1, x2, y2, Op);
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.WMSize(var Message: TWMSize);
begin
  inherited;
  //
  if fWndC <> 0 then
    MoveWindow(fWndC, 0, 0, ClientWidth, ClientHeight, true);
end;

/////////////////////////////////////////////////////////////////////////////////////
// Assegna fWndC
// nota: prima di chiamare questa funzione assicurarsi che fWndC sia ZERO

procedure TImageEnVideoView.CreateCaptureWindow;
begin
  if fCreatingCaptureWindow then
    exit;
  fCreatingCaptureWindow := true;
  fWndC := capCreateCaptureWindowA(PAnsiChar(AnsiString(name)), WS_CHILD, 0, 0, ClientWidth, ClientHeight, IEFindHandle(self), 0);
  if fShowVideo then
    SetShowVideo(true); // qui richiama anche SetDisplayMode
  fCreatingCaptureWindow := false;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnvideoView.DestroyCaptureWindow;
begin
  if fWndC <> 0 then
  begin
    SendMessage(fWndC, WM_CAP_SET_USER_DATA, 0, 0);
    ShowVideo := false;
    DestroyWindow(fWndC);
    fWndC := 0;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.Paint;
begin
  {$ifdef OCXVERSION}
  inherited Paint;
  {$else}
  if (fWndC = 0) then
    CreateCaptureWindow;
  if (not fShowVideo) then
    inherited Paint;
  {$endif}
end;

{!!
<FS>TImageEnVideoView.Freeze

<FM>Declaration<FC>
procedure Freeze;

<FM>Description<FN>
Sets Frozen property to True. When you set the Frozen property to True, the video input is locked and the current image is copied to Bitmap field (in 24 bit x pixel). To process/zoom/navigate the captured image, you must set the ShowVideo property to False.

The size of image is equal to size of TImageEnVideoView component (it is your responsibility to maintain the correct aspect ratio) if FitFreeze is True.

!!}
procedure TImageEnVideoView.Freeze;
begin
  Frozen := true;
end;

{!!
<FS>TImageEnVideoView.UnFreeze

<FM>Declaration<FC>
procedure UnFreeze;

<FM>Description<FN>
Sets Frozen property to False. Unlocks video input. The Bitmap field of TImageEnVideoView contains the last frozen image.

!!}
procedure TImageEnVideoView.UnFreeze;
begin
  Frozen := false;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.DriverDisconnect;
begin
  SendMessage(fWndC, WM_CAP_DRIVER_DISCONNECT, 0, 0);
  fConnected := false;
end;

{!!
<FS>TImageEnVideoView.VideoSource

<FM>Declaration<FC>
property VideoSource: integer;

<FM>Description<FN>
VideoSource property contains the index of the current video source (see VideoSourceList). The default is 0.
!!}
procedure TImageEnVideoView.SetVideoSource(v: integer);
begin
  fVideoSource := v;
  if fShowVideo then
  begin
    SetShowVideo(false);
    SetShowVideo(true);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
// In questo modo posso escludere le scrollbars e poi riattivarle in modo pulito, utilizzando
// fSScrollBars (ombra di fScrollBars).

procedure TImageEnVideoView.SetScrollBars(v: TScrollStyle);
begin
  fSScrollBars := v;
  inherited SetScrollBars(v);
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoView.DriverConnect: boolean;
begin
  DoJob(iejVIDEOCAP_CONNECTING, 0);
  result := SendMessage(fWndC, WM_CAP_DRIVER_CONNECT, fVideoSource, 0) <> 0;
  if result then
  begin
    fConnected := true;
    fBitmapInfoUp := false;
    FillBitmapInfo;
    DoJob(iejNOTHING, 0);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
// Come DriverConnect, ma rest. false se la connessione fallisce

function TImageEnVideoView.DriverConnectNE: boolean;
begin
  result := SendMessage(fWndC, WM_CAP_DRIVER_CONNECT, fVideoSource, 0) <> 0;
  fConnected := result;
end;

{!!
<FS>TImageEnVideoView.HasDlgVideoSource

<FM>Declaration<FC>
property HasDlgVideoSource: boolean;

<FM>Description<FN>
Returns True if the selected driver supports a Video Source Dialog.

Read-only

!!}
function TImageEnVideoView.GetHasDlgVideoSource: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasDlgVideoSource;
end;

{!!
<FS>TImageEnVideoView.HasDlgVideoFormat

<FM>Declaration<FC>
property HasDlgVideoFormat: boolean;

<FM>Description<FN>
Returns True if the selected driver supports a Video Format Dialog.

Read-only

!!}
function TImageEnVideoView.GetHasDlgVideoFormat: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasDlgVideoFormat;
end;

{!!
<FS>TImageEnVideoView.HasDlgVideoDisplay

<FM>Declaration<FC>
property HasDlgVideoDisplay: boolean;

<FM>Description<FN>
Returns True if the selected driver supports a Video Display Dialog.

Read-only

!!}
function TImageEnVideoView.GetHasDlgVideoDisplay: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasDlgVideoDisplay;
end;

{!!
<FS>TImageEnVideoView.HasOverlay

<FM>Declaration<FC>
property HasOverlay: boolean;

<FM>Description<FN>
Returns True if the selected driver supports Overlay display mode.

Read-only

!!}
function TImageEnVideoView.GetHasOverlay: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasOverlay;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.GetCaps(var fDriverCaps: TCAPDRIVERCAPS);
var
  lcon: boolean;
begin
  lcon := fConnected;
  if not fConnected then
    DriverConnect;
  SendMessage(fWndC, WM_CAP_DRIVER_GET_CAPS, sizeof(TCAPDRIVERCAPS), LPARAM(@fDriverCaps));
  if not lcon then
    DriverDisconnect;
end;

{!!
<FS>TImageEnVideoView.PreviewRate

<FM>Declaration<FC>
property PreviewRate: integer;

<FM>Description<FN>
Sets the interval (in milliseconds) between acquisition of successive frames. It is valid only if DisplayMode is dmPreview.
!!}
procedure TImageEnVideoView.SetPreviewRate(v: integer);
begin
  fPreviewRate := v;
  if fConnected then
    SendMessage(fWndC, WM_CAP_SETPREVIEWRATE, fPreviewRate, 0);
end;

{!!
<FS>TImageEnVideoView.StartRecord

<FM>Declaration<FC>
function StartRecord: boolean;

<FM>Description<FN>
Begin recording of the video input to AVI format. To select compression algorithm run the ConfigureCompression dialog.

StartRecord returns False if it fails, True if it’s successful.

!!}
function TImageEnVideoView.StartRecord: boolean;
var
  cp: TCAPTUREPARMS;
begin
  result := false;
  if fRecording then
    exit;
  SendMessage(fWndC, WM_CAP_GET_SEQUENCE_SETUP, sizeof(cp), LPARAM(@cp));
  cp.fYield := fRecMultitask;
  cp.fLimitEnabled := false;
  cp.fCaptureAudio := fRecAudio;
  cp.fAbortLeftMouse := false;
  cp.fAbortRightMouse := false;
  cp.dwRequestMicroSecPerFrame := round((1 / fRecFrameRate) * 1000000);
  SendMessage(fWndC, WM_CAP_SET_SEQUENCE_SETUP, sizeof(cp), LPARAM(@cp));
  if SendMessage(fWndC, WM_CAP_FILE_SET_CAPTURE_FILE, 0, LPARAM(PAnsiChar(fRecFileName))) = 0 then
    exit;
  if SendMessage(fWndC, WM_CAP_SEQUENCE, 0, 0) = 0 then
    exit;
  fRecording := true;
  result := true;
end;

{!!
<FS>TImageEnVideoView.StopRecord

<FM>Declaration<FC>
procedure StopRecord;

<FM>Description<FN>
Stops recording begun with StartRecord. After StopRecord completes, you can access the saved AVI file.

!!}
procedure TImageEnVideoView.StopRecord;
begin
  if not fRecording then
    exit;
  SendMessage(fWndC, WM_CAP_STOP, 0, 0);
  fRecording := false;
end;

{!!
<FS>TImageEnVideoView.DoConfigureSource

<FM>Declaration<FC>
function DoConfigureSource: boolean;

<FM>Description<FN>
Executes the Configure Source Dialog of the selected driver (see VideoSource property). If the the driver is busy or it has failed to open, the function returns False.

!!}
function TImageEnVideoView.DoConfigureSource: boolean;
var
  lcon: boolean;
begin
  lcon := fConnected;
  result := fConnected;
  if not fConnected then
    result := DriverConnectNE;
  if result then
  begin
    result := SendMessage(fWndC, WM_CAP_DLG_VIDEOSOURCE, 0, 0) <> 0;
    fBitmapInfoUp := false;
    FillBitmapInfo;
    if not lcon then
      DriverDisconnect
  end;
end;

{!!
<FS>TImageEnVideoView.DoConfigureFormat

<FM>Declaration<FC>
function DoConfigureFormat: boolean;

<FM>Description<FN>
Executes the Configure Format Dialog of the selected driver (see VideoSource property). If the the driver is busy or it has failed to open, the function returns False.

!!}
function TImageEnVideoView.DoConfigureFormat: boolean;
var
  lcon: boolean;
begin
  lcon := fConnected;
  result := fConnected;
  if not fConnected then
    result := DriverConnectNE;
  if result then
  begin
    result := SendMessage(fWndC, WM_CAP_DLG_VIDEOFORMAT, 0, 0) <> 0;
    fBitmapInfoUp := false;
    FillBitmapInfo;
    if not lcon then
      DriverDisconnect
  end;
end;

{!!
<FS>TImageEnVideoView.DoConfigureDisplay

<FM>Declaration<FC>
function DoConfigureDisplay: boolean;

<FM>Description<FN>
Executes the Configure Display Dialog of the selected driver (see VideoSource property).  If the the driver is busy or it has failed to open, the function returns False.

!!}
function TImageEnVideoView.DoConfigureDisplay: boolean;
var
  lcon: boolean;
begin
  lcon := fConnected;
  result := fConnected;
  if not fConnected then
    result := DriverConnectNE;
  if result then
  begin
    result := SendMessage(fWndC, WM_CAP_DLG_VIDEODISPLAY, 0, 0) <> 0;
    fBitmapInfoUp := false;
    FillBitmapInfo;
    if not lcon then
      DriverDisconnect
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoView.FillBitmapInfo: boolean;
var
  sz: integer;
  pt: PBITMAPINFO;
  lcon: boolean;
begin
  if not fBitmapInfoUp then
  begin
    lcon := fConnected;
    result := fConnected;
    if not fConnected then
      result := DriverConnectNE;
    if result then
    begin
      GlobalFree(fhBitmapInfo);
      sz := SendMessage(fWndC, WM_CAP_GET_VIDEOFORMAT, 0, 0); // get size
      fhBitmapInfo := GlobalAlloc(GHND, IMAX(sizeof(TBITMAPINFO) + sizeof(TRGBQUAD) * 256, sz));
      pt := GlobalLock(fhBitmapInfo);
      SendMessage(fWndC, WM_CAP_GET_VIDEOFORMAT, sz, LPARAM(pt));
      GlobalUnLock(fhBitmapInfo);
      if not lcon then
        DriverDisconnect;
    end;
    fBitmapInfoUp := true;
  end
  else
    result := true;
end;

{!!
<FS>TImageEnVideoView.GetVideoSize

<FM>Declaration<FC>
function GetVideoSize: TRect;

<FM>Description<FN>
Returns the rectangle of video input (as selected from ConfigureFormat dialog).

!!}
function TImageEnVideoView.GetVideoSize: TRect;
var
  pt: PBITMAPINFO;
begin
  if fWndC = 0 then
    CreateCaptureWindow;
  FillBitmapInfo;
  with result do
  begin
    Left := 0;
    Top := 0;
    pt := GlobalLock(fhBitmapInfo);
    Right := pt^.bmiHeader.biWidth - 1;
    Bottom := pt^.bmiHeader.biHeight - 1;
    GlobalUnLock(fhBitmapInfo);
  end;
end;

// Attiva/disattiva chiamata funzione CallBackFrameFunc()
procedure TImageEnVideoView.SetCallBackFrame(v: boolean);
begin
  fCallBackFrame := v;
  if fConnected then
  begin
    // attiva/disattiva "al volo"
    if v then
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_FRAME, 0, LPARAM(@CallBackFrameFunc))
    else
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_FRAME, 0, 0);
  end;
end;

{!!
<FS>TImageEnVideoView.OnVideoFrame

<FM>Declaration<FC>
property OnVideoFrame: <A TVideoFrameEvent>;

<FM>Description<FN>
This event is generated for each input frame.
If you handle this event, the performance of video input degrades. You haven’t to free this bitmap: ImageEn will free it.
!!}
procedure TImageEnVideoView.SetOnVideoFrame(v: TVideoFrameEvent);
begin
  fOnVideoFrame := v;
  SetCallBackFrame(assigned(fOnVideoFrame) or assigned(fOnVideoFrameRaw));
end;

{!!
<FS>TImageEnVideoView.OnVideoFrameRaw

<FM>Declaration<FC>
property OnVideoFrameRaw: <A TVideoFrameRawEvent>;

<FM>Description<FN>
This event is generated for each input frame (as OnVideoFrame). The TVideoFrameRawEvent function is defined in this way:
You can modify the pixels (pData) because this event is generated before video source shows the frame.
!!}
procedure TImageEnVideoView.SetOnVideoFrameRaw(v: TVideoFrameRawEvent);
begin
  fOnVideoFrameRaw := v;
  SetCallBackFrame(assigned(fOnVideoFrame) or assigned(fOnVideoFrameRaw));
end;

// Decompress raw frame
// OutBitmap has to be created
procedure TImageEnVideoView.DecompRawFrame(OutBitmap: TIEDibBitmap; pix: pointer);
var
  pbi: PBITMAPINFOHEADER;
begin
  pbi := GlobalLock(fhBitmapInfo);
  if pbi^.biBitCount = 1 then
    OutBitmap.AllocateBits(pbi^.biWidth, pbi^.biHeight, 1)
  else
    OutBitmap.AllocateBits(pbi^.biWidth, pbi^.biHeight, 24);
  IEDrawDibDraw(fHDrawDib, OutBitmap.HDC, 0, 0, OutBitmap.Width, OutBitmap.Height,
    pbi^, pix, 0, 0, OutBitmap.Width, OutBitmap.Height, 0);
  GlobalUnLock(fhBitmapInfo);
end;

/////////////////////////////////////////////////////////////////////////////////////
// callback frame function

function CallBackFrameFunc(hWnd: HWND; lpVHdr: PVIDEOHDR): LRESULT;
var
  xBitmap: TIEDibBitmap;
  pobj: pointer;
  obj: TImageEnVideoView;
begin
  result := 0;
  pobj := pointer(SendMessage(hWnd, WM_CAP_GET_USER_DATA, 0, 0));
  if assigned(pobj) then
  begin
    obj := pobj;
    with obj do
    begin
      if assigned(fOnVideoFrame) then
      begin
        xBitmap := TIEDibBitmap.Create;
        DecompRawFrame(xBitmap, lpVHdr^.lpData);
        fOnVideoFrame(obj, xBitmap);
        FreeAndNil(xBitmap);
      end;
      if assigned(fOnVideoFrameRaw) then
        fOnVideoFrameRaw(obj, fhBitmapInfo, lpVHdr^.lpData);
      if fGrabFrame then
      begin
        xBitmap := TIEDibBitmap.Create;
        DecompRawFrame(xBitmap, lpVHdr^.lpData);
        xBitmap.CopyToTBitmap(fBitmap);
        FreeAndNil(xBitmap);
        fGrabFrame := false;
      end;
    end;
  end;
end;

{!!
<FS>TImageEnVideoView.Frozen

<FM>Declaration<FC>
property Frozen: boolean;

<FM>Description<FN>
When you set the Frozen property to True, the video input is locked and the current image is copied to Bitmap field (in 24 bit x pixel). To process/zoom/navigate the image you must set ShowVideo property to False.

The size of image is equal to size of TImageEnVideoView component (is your responsibility to maintain the correct aspect ratio) if FitFreeze is True.

!!}
procedure TImageEnVideoView.SetFreeze(v: boolean);
var
  fImageEnProc: TImageEnProc;
begin
  fFreeze := v;
  if (fShowVideo) and (fConnected) then
  begin
    if fFreeze then
    begin
      fGrabFrame := true;
      SetCallBackFrame(true);
      SendMessage(fWndC, WM_CAP_SETPREVIEW, 1, 0);
      SendMessage(fWndC, WM_CAP_GRAB_FRAME, 0, 0);
      while fGrabFrame do
        ;
      SetCallBackFrame(assigned(fOnVideoFrame) or assigned(fOnVideoFrameRaw));
      if fFitFreeze then
      begin
        fImageEnProc := TImageEnProc.CreateFromBitmap(Bitmap);
        try
          fImageEnProc.AutoUndo := False;
          if (ClientWidth > Bitmap.Width) or (ClientHeight > Bitmap.Height) then
            fImageEnProc.Resample(ClientWidth, ClientHeight, IEGlobalSettings().DefaultResampleFilter)
          else
            fImageEnProc.Resample(ClientWidth, ClientHeight, rfNone);
        finally
          FreeAndNil(fImageEnProc);
        end;
      end;
      Update;
    end
    else
    begin
      SetDisplayMode(fDisplayMode);
    end;
  end;
end;

{!!
<FS>TImageEnVideoView.SaveFrame

<FM>Declaration<FC>
procedure SaveFrame;

<FM>Description<FN>
SaveFrame saves current frame without locking ( see freeze) the video input. Application can display the image in the component’s visual area by setting ShowVideo to False.

!!}
procedure TImageEnVideoView.SaveFrame;
var
  fImageEnProc: TImageEnProc;
begin
  if (fShowVideo) and (fConnected) then
  begin
    fGrabFrame := true;
    SetCallBackFrame(true);
    SendMessage(fWndC, WM_CAP_SETPREVIEW, 1, 0);
    SendMessage(fWndC, WM_CAP_GRAB_FRAME_NOSTOP, 0, 0);
    while fGrabFrame do
      ;
    SetCallBackFrame(assigned(fOnVideoFrame) or assigned(fOnVideoFrameRaw));
    if fFitFreeze then
    begin
      fImageEnProc := TImageEnProc.CreateFromBitmap(Bitmap);
      try
        fImageEnProc.AutoUndo := false;
        if (ClientWidth > Bitmap.Width) or (ClientHeight > Bitmap.Height) then
          fImageEnProc.Resample(ClientWidth, ClientHeight, IEGlobalSettings().DefaultResampleFilter)
        else
          fImageEnProc.Resample(ClientWidth, ClientHeight, rfNone);
      finally
        FreeAndNil(fImageEnProc);
      end;
    end;
    Update;
  end;
end;

{!!
<FS>TImageEnVideoView.DoConfigureCompression

<FM>Declaration<FC>
function DoConfigureCompression: boolean;

<FM>Description<FN>
Executes the Configure Compression Dialog of the selected driver (see VideoSource property). If the driver is busy (fails to open), the function returns False.

!!}
function TImageEnVideoView.DoConfigureCompression: boolean;
var
  lcon: boolean;
begin
  lcon := fConnected;
  result := fConnected;
  if not fConnected then
    result := DriverConnectNE;
  if result then
  begin
    result := SendMessage(fWndC, WM_CAP_DLG_VIDEOCOMPRESSION, 0, 0) <> 0;
    fBitmapInfoUp := false;
    FillBitmapInfo;
    if not lcon then
      DriverDisconnect
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.DoJob(job: TIEJob; per: integer);
begin
  if assigned(fOnJob) then
    fOnJob(self, job, per);
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.GetWaveFormat(var wf: TWAVEFORMATEX);
var
  lcon: boolean;
begin
  lcon := fConnected;
  if not fConnected then
    DriverConnect;
  SendMessage(fWndC, WM_CAP_GET_AUDIOFORMAT, sizeof(TWAVEFORMATEX), LPARAM(@wf));
  if not lcon then
    DriverDisconnect;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoView.SetWaveFormat(var wf: TWAVEFORMATEX);
var
  lcon: boolean;
begin
  lcon := fConnected;
  if not fConnected then
    DriverConnect;
  wf.nAvgBytesPerSec := 0;
  wf.cbSize := 0;
  SendMessage(fWndC, WM_CAP_SET_AUDIOFORMAT, sizeof(TWAVEFORMATEX), LPARAM(@wf));
  if not lcon then
    DriverDisconnect;
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoView.GetAudioFormat: word;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.wFormatTag
end;

{!!
<FS>TImageEnVideoView.AudioFormat

<FM>Declaration<FC>
property AudioFormat: word;

<FM>Description<FN>
<A TImageEnVideoView.AudioChannels>, <A TImageEnVideoView.AudioSamplesPerSec>, <A TImageEnVideoView.AudioBitsPerSample> and <A TImageEnVideoView.AudioFormat> properties allow the application to get/set the audio recording format.
AudioFormat specifies the compression audio format. Currently defined values are: 
$0000 : UNKNOWN
$0001 : PCM
$0002 : ADPCM
$0003 : IEEE_FLOAT
$0004 : VSELP
$0005 : IBM_CVSD
$0006 : ALAW
$0007 : MULAW
$0008 : DTS
$0010 : OKI_ADPCM
$0011 : DVI_ADPCM
$0012 : MEDIASPACE_ADPCM
$0013 : SIERRA_ADPCM
$0014 : G723_ADPCM
$0015 : DIGISTD
$0016 : DIGIFIX
$0017 : DIALOGIC_OKI_ADPCM
$0018 : MEDIAVISION_ADPCM
$0019 : CU_CODEC
$0020 : YAMAHA_ADPCM
$0021 : SONARC
$0022 : DSPGROUP_TRUESPEECH
$0023 : ECHOSC1
$0024 : AUDIOFILE_AF36
$0025 : APTX
$0026 : AUDIOFILE_AF10
$0027 : PROSODY_1612
$0028 : LRC
$0030 : DOLBY_AC2
$0031 : GSM610
$0032 : MSNAUDIO
$0033 : ANTEX_ADPCME
$0034 : CONTROL_RES_VQLPC
$0035 : DIGIREAL
$0036 : DIGIADPCM
$0037 : CONTROL_RES_CR10
$0038 : NMS_VBXADPCM
$0039 : CS_IMAADPCM
$003A : ECHOSC3
$003B : ROCKWELL_ADPCM
$003C : ROCKWELL_DIGITALK
$003D : XEBEC
$0040 : G721_ADPCM
$0041 : G728_CELP
$0042 : MSG723
$0050 : MPEG
$0052 : RT24
$0053 : PAC
$0055 : MPEGLAYER3
$0059 : LUCENT_G723
$0060 : CIRRUS
$0061 : ESPCM
$0062 : VOXWARE
$0063 : CANOPUS_ATRAC
$0064 : G726_ADPCM
$0065 : G722_ADPCM
$0067 : DSAT_DISPLAY
$0069 : VOXWARE_BYTE_ALIGNED
$0070 : VOXWARE_AC8
$0071 : VOXWARE_AC10
$0072 : VOXWARE_AC16
$0073 : VOXWARE_AC20
$0074 : VOXWARE_RT24
$0075 : VOXWARE_RT29
$0076 : VOXWARE_RT29HW
$0077 : VOXWARE_VR12
$0078 : VOXWARE_VR18
$0079 : VOXWARE_TQ40
$0080 : SOFTSOUND
$0081 : VOXWARE_TQ60
$0082 : MSRT24
$0083 : G729A
$0084 : MVI_MVI2
$0085 : DF_G726
$0086 : DF_GSM610
$0088 : ISIAUDIO
$0089 : ONLIVE
$0091 : SBC24
$0092 : DOLBY_AC3_SPDIF
$0093 : MEDIASONIC_G723
$0094 : PROSODY_8KBPS
$0094 : ZYXEL_ADPCM                
$0098 : PHILIPS_LPCBB
$0099 : PACKED                     
$00A0 : MALDEN_PHONYTALK           
$0100 : RHETOREX_ADPCM             
$0101 : IRAT
$0111 : VIVO_G723                  
$0112 : VIVO_SIREN                 
$0123 : DIGITAL_G723               
$0125 : SANYO_LD_ADPCM
$0130 : SIPROLAB_ACEPLNET          
$0131 : SIPROLAB_ACELP4800         
$0132 : SIPROLAB_ACELP8V3          
$0133 : SIPROLAB_G729              
$0134 : SIPROLAB_G729A             
$0135 : SIPROLAB_KELVIN            
$0140 : G726ADPCM                  
$0150 : QUALCOMM_PUREVOICE         
$0151 : QUALCOMM_HALFRATE
$0155 : TUBGSM
$0160 : MSAUDIO1                   
$0200 : CREATIVE_ADPCM             
$0202 : CREATIVE_FASTSPEECH8       
$0203 : CREATIVE_FASTSPEECH10      
$0210 : UHER_ADPCM                 
$0220 : QUARTERDECK
$0230 : ILINK_VC                   
$0240 : RAW_SPORT                  
$0250 : IPI_HSX                    
$0251 : IPI_RPELP                  
$0260 : CS2
$0270 : SONY_SCX                   
$0300 : FM_TOWNS_SND               
$0400 : BTV_DIGITAL                
$0450 : QDESIGN_MUSIC              
$0680 : VME_VMPCM                  
$0681 : TPC
$1000 : OLIGSM                     
$1001 : OLIADPCM                   
$1002 : OLICELP                    
$1003 : OLISBC                     
$1004 : OLIOPR                     
$1100 : LH_CODEC                   
$1400 : NORRIS
$1500 : SOUNDSPACE_MUSICOMPRESS
$2000 : DVM

!!}
procedure TImageEnVideoView.SetAudioFormat(v: word);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.wFormatTag := v;
  SetWaveFormat(wf);
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoView.GetAudioChannels: word;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.nChannels;
end;

{!!
<FS>TImageEnVideoView.AudioChannels

<FM>Declaration<FC>
property AudioChannels: word;

<FM>Description<FN>
<A TImageEnVideoView.AudioChannels>, <A TImageEnVideoView.AudioSamplesPerSec>, <A TImageEnVideoView.AudioBitsPerSample> and <A TImageEnVideoView.AudioFormat> properties allow the application to get/set the audio recording format.
AudioChannels specifies the number of channels in the waveform-audio data. Monaural data uses one channel and stereo data uses two channels.
!!}
procedure TImageEnVideoView.SetAudioChannels(v: word);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.nChannels := v;
  SetWaveFormat(wf);
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoView.GetAudioSamplesPerSec: dword;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.nSamplesPerSec;
end;

{!!
<FS>TImageEnVideoView.AudioSamplesPerSec

<FM>Declaration<FC>
property AudioSamplesPerSec: dword;

<FM>Description<FN>
<A TImageEnVideoView.AudioChannels>, <A TImageEnVideoView.AudioSamplesPerSec>, <A TImageEnVideoView.AudioBitsPerSample> and <A TImageEnVideoView.AudioFormat> properties allow the application to get/set the audio recording format.
AudioSamplesPerSec specifies the sampling rate, in samples per second (hertz), at which each channel should be played or recorded.
!!}
procedure TImageEnVideoView.SetAudioSamplesPerSec(v: dword);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.nSamplesPerSec := v;
  SetWaveFormat(wf);
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoView.GetAudioBitsPerSample: word;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.wBitsPerSample;
end;

{!!
<FS>TImageEnVideoView.AudioBitsPerSample

<FM>Declaration<FC>
property AudioBitsPerSample: word;

<FM>Description<FN>
<A TImageEnVideoView.AudioChannels>, <A TImageEnVideoView.AudioSamplesPerSec>, <A TImageEnVideoView.AudioBitsPerSample> and <A TImageEnVideoView.AudioFormat> properties allow the application to get/set the audio recording format.
AudioBitsPerSample specifies the bits per sample for the AudioFormat format type.  If a compression scheme cannot define a bits-per-sample value, this field is zero.
!!}
procedure TImageEnVideoView.SetAudioBitsPerSample(v: word);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.wBitsPerSample := v;
  SetWaveFormat(wf);
end;

{$ELSE} // IEINCLUDEVIDEOCAPTURE

interface
implementation

{$ENDIF}

end.


