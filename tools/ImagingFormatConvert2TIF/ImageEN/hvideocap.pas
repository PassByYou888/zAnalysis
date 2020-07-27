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
File version 1006
*)

unit hvideocap;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEVIDEOCAPTURE}

interface

uses
  Windows, Messages, SysUtils, StdCtrls, Classes, Graphics, Controls, Forms, ImageEnView,
  ImageEnProc, hyiedefs, videocap, ieview, hyieutils;

const
  VH_FRAMEMESSAGE = WM_USER + 5000;
  VH_DESTROYWINDOW = WM_USER + 5001;

type



{!!
<FS>TImageEnVideoCap

<FM>Description<FN>
TImageEnVideoCap is a non-visual component which can capture images from video cameras. It uses VFW (Video for Windows) to capture frames from a video source.

It is recommended that you now use <A TImageEnView>.<A TImageEnView.IO>.<A TImageEnIO.DShowParams> to capture video from cameras, because it uses the more widely supported DirectShow API.

<FB>This component is deprecated.<FN>

<FB>See also<FN>
- <A TImageEnVideoView>


<FM>Methods and Properties<FN>
  <A TImageEnVideoCap.AudioBitsPerSample>
  <A TImageEnVideoCap.AudioChannels>
  <A TImageEnVideoCap.AudioFormat>
  <A TImageEnVideoCap.AudioSamplesPerSec>
  <A TImageEnVideoCap.Capture>
  <A TImageEnVideoCap.DoConfigureCompression>
  <A TImageEnVideoCap.DoConfigureDisplay>
  <A TImageEnVideoCap.DoConfigureFormat>
  <A TImageEnVideoCap.DoConfigureSource>
  <A TImageEnVideoCap.GetVideoSize>
  <A TImageEnVideoCap.HasDlgVideoDisplay>
  <A TImageEnVideoCap.HasDlgVideoFormat>
  <A TImageEnVideoCap.HasDlgVideoSource>
  <A TImageEnVideoCap.HasOverlay>
  <A TImageEnVideoCap.RecAudio>
  <A TImageEnVideoCap.RecFileName>
  <A TImageEnVideoCap.RecFrameRate>
  <A TImageEnVideoCap.RecMultitask>
  <A TImageEnVideoCap.StartRecord>
  <A TImageEnVideoCap.StopRecord>
  <A TImageEnVideoCap.UseWindowsCodec>
  <A TImageEnVideoCap.VideoSource>
  <A TImageEnVideoCap.VideoSourceList>
  <A TImageEnVideoCap.WndCaptureHandle>


<FM>Events<FN>
  <A TImageEnVideoCap.OnJob>
  <A TImageEnVideoCap.OnVideoFrameRaw>
  <A TImageEnVideoCap.OnVideoFrame>
!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TImageEnVideoCap = class(TComponent)
  private
    fCapture: boolean;        // if true starts capture
    fWndC: HWND;              // Video Capture Window handle (0=to create)
    fDrivers: TStringList;    // available drivers
    fVideoSource: integer;    // index of current video capture source
    fCallBackFrame: boolean;  // If True activates CallBackFrameFunc callback
    fOnVideoFrame: TVideoFrameEvent;
    fOnVideoFrameRaw: TVideoFrameRawEvent;
    fhBitmapInfo: THandle;    // BitmapInfo Handle filled by FillBitmapInfo
    fBitmapInfoUp: boolean;   // true if fhBitmapInfo is updated (used in FillBitmapInfo)
    fConnected: boolean;      // true if connected to capture driver
    fOnJob: TIEJobEvent;
    fHDrawDib: HDRAWDIB;
    fBitmap: TIEDibBitmap;
    fPix: pointer;
    fDone: boolean;
    fDriverBusy: boolean;
    fEnding: boolean;
    fUseWindowsCodec: boolean;
    // recording support
    fRecFileName: String;   // destination file name
    fRecFrameRate: integer; // frames per second (dwRequestMicroSecPerFrame)
    fRecAudio: boolean;     // true enables audio capture (fCaptureAudio)
    fRecMultitask: boolean; // false disables multitasking (fYeld) [ESC=abort]
    fRecording: boolean;    // true if we are recording
    fWinHandle: HWND;
  protected
    procedure SetCapture(v: boolean);
    procedure DriverConnect;
    function DriverConnectNE: boolean;
    procedure DriverDisconnect;
    procedure FillDrivers;
    procedure SetVideoSource(v: integer);
    function GetHasDlgVideoSource: boolean;
    function GetHasDlgVideoFormat: boolean;
    function GetHasDlgVideoDisplay: boolean;
    function GetHasOverlay: boolean;
    procedure GetCaps(var fDriverCaps: TCAPDRIVERCAPS);
    procedure SetCallBackFrame(v: boolean);
    procedure SetOnVideoFrame(v: TVideoFrameEvent);
    procedure SetOnVideoFrameRaw(v: TVideoFrameRawEvent);
    function FillBitmapInfo: boolean;
    procedure CreateCaptureWindow;
    procedure DestroyCaptureWindow;
    procedure DoJob(job: TIEJob; per: integer);
    procedure AllocateWindow;
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
    property Capture: boolean read fCapture write SetCapture default false;
    function DoConfigureSource: boolean;
    function DoConfigureFormat: boolean;
    function DoConfigureDisplay: boolean;
    function DoConfigureCompression: boolean;

{!!
<FS>TImageEnVideoCap.VideoSourceList

<FM>Declaration<FC>
property VideoSourceList: TStringList;

<FM>Description<FN>
This is the list of video capture (video source) drivers installed on the system. The format is: "Device_Name Device_Version". The index of the list corresponds to VideoSource property value.

Read-only
!!}
    property VideoSourceList: TStringList read fDrivers;

{!!
<FS>TImageEnVideoCap.HasOverlay

<FM>Declaration<FC>
property HasOverlay: boolean;

<FM>Description<FN>
Returns True if the selected driver supports Overlay display mode.

Read-only

!!}
    property HasOverlay: boolean read GetHasOverlay;

{!!
<FS>TImageEnVideoCap.HasDlgVideoSource

<FM>Declaration<FC>
property HasDlgVideoSource: boolean;

<FM>Description<FN>
Returns True if the selected driver supports a Video Source Dialog.

Read-only

!!}
    property HasDlgVideoSource: boolean read GetHasDlgVideoSource;

{!!
<FS>TImageEnVideoCap.HasDlgVideoFormat

<FM>Declaration<FC>
property HasDlgVideoFormat: boolean;

<FM>Description<FN>
Returns True if the selected driver supports a Video Format Dialog.

Read-only

!!}
    property HasDlgVideoFormat: boolean read GetHasDlgVideoFormat;

{!!
<FS>TImageEnVideoCap.HasDlgVideoDisplay

<FM>Declaration<FC>
property HasDlgVideoDisplay: boolean;

<FM>Description<FN>
Returns True if the selected driver supports a Video Display Dialog.

Read-only

!!}
    property HasDlgVideoDisplay: boolean read GetHasDlgVideoDisplay;

    procedure StartRecord;
    procedure StopRecord;

{!!
<FS>TImageEnVideoCap.RecFileName

<FM>Declaration<FC>
property RecFileName: String

<FM>Description<FN>
RecFileName contains the file name (AVI file format) where to save the captured video input.
Default is 'Capture.avi'.
!!}
    property RecFileName: String read fRecFileName write fRecFileName;

{!!
<FS>TImageEnVideoCap.RecFrameRate

<FM>Declaration<FC>
property RecFrameRate: integer;

<FM>Description<FN>
RecFrameRate is the number of frames per second captured on recording.
Default is 15.

!!}
    property RecFrameRate: integer read fRecFrameRate write fRecFrameRate;

{!!
<FS>TImageEnVideoCap.RecAudio

<FM>Declaration<FC>
property RecAudio: boolean;

<FM>Description<FN>
Set RecAudio to True to capture audio input with video input.
Default is False.

!!}
    property RecAudio: boolean read fRecAudio write fRecAudio;

{!!
<FS>TImageEnVideoCap.RecMultitask

<FM>Declaration<FC>
property RecMultitask: boolean;

<FM>Description<FN>
If RecMultitask is False the system is locked to wait the end of recording. To stop recording press ESC.
Default is True.

!!}
    property RecMultitask: boolean read fRecMultitask write fRecMultitask;

{!!
<FS>TImageEnVideoCap.WndCaptureHandle

<FM>Declaration<FC>
property WndCaptureHandle: HWND;

<FM>Description<FN>
WndCaptureHandle is the handle of the video capture window. It is useful to send messages to Video for Windows system.

!!}
    property WndCaptureHandle: HWND read fWndC;

    property AudioFormat: word read GetAudioFormat write SetAudioFormat;
    property AudioChannels: word read GetAudioChannels write SetAudioChannels;
    property AudioSamplesPerSec: dword read GetAudioSamplesPerSec write SetAudioSamplesPerSec;
    property AudioBitsPerSample: word read GetAudioBitsPerSample write SetAudioBitsPerSample;
    // Video format
    function GetVideoSize: TRect;

{!!
<FS>TImageEnVideoCap.UseWindowsCodec

<FM>Declaration<FC>
property UseWindowsCodec: boolean;

<FM>Description<FN>
If UseWindowsCodec isTrue, TImageEnVideoCap uses Windows codec to render video stream, otherwise it uses internal codec. The default is True (Windows codecs).

Try setting this property to False if you have problems with video capture.
!!}
    property UseWindowsCodec: boolean read fUseWindowsCodec write fUseWindowsCodec;

  published

{!!
<FS>TImageEnVideoCap.VideoSource

<FM>Declaration<FC>
property VideoSource: integer;

<FM>Description<FN>
VideoSource property contains the index of the current video source (see VideoSourceList). The default is 0.
!!}
    property VideoSource: integer read fVideoSource write SetVideoSource default 0;

{!!
<FS>TImageEnVideoCap.OnVideoFrame

<FM>Declaration<FC>
property OnVideoFrame: <A TVideoFrameEvent>;

<FM>Description<FN>
This event is generated for each input frame.
If you handle this event, the performance of video input degrades. You haven’t to free this bitmap: ImageEn will free it.
!!}
    property OnVideoFrame: TVideoFrameEvent read fOnVideoFrame write SetOnVideoFrame;

{!!
<FS>TImageEnVideoCap.OnVideoFrameRaw

<FM>Declaration<FC>
property OnVideoFrameRaw: <A TVideoFrameRawEvent>;

<FM>Description<FN>
This event is generated for each input frame (as OnVideoFrame). The TVideoFrameRawEvent function is defined in this way:
You can modify the pixels (pData) because this event is generated before video source shows the frame.
!!}
    property OnVideoFrameRaw: TVideoFrameRawEvent read fOnVideoFrameRaw write SetOnVideoFrameRaw;

{!!
<FS>TImageEnVideoCap.OnJob

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

{$R-}

const

  DLL2 = 'AVICAP32.DLL';

  // VIDEOCAP CONSTS
  WM_CAP_START = WM_USER;
  WM_CAP_GET_STATUS = WM_CAP_START + 54;
  WM_CAP_SET_CALLBACK_STATUS = WM_CAP_START + 3;
  WM_CAP_DRIVER_CONNECT = WM_CAP_START + 10;
  WM_CAP_SEQUENCE = WM_CAP_START + 62;
  WM_CAP_STOP = WM_CAP_START + 69;
  WM_CAP_ABORT = WM_CAP_START + 68;
  WM_CAP_FILE_SET_CAPTURE_FILE = WM_CAP_START + 20;
  WM_CAP_SETPREVIEW = WM_CAP_START + 50;
  WM_CAP_SETPREVIEWRATE = WM_CAP_START + 52;
  WM_CAP_SETOVERLAY = WM_CAP_START + 51;
  WM_CAP_SET_SCALE = WM_CAP_START + 53;
  WM_CAP_DRIVER_DISCONNECT = WM_CAP_START + 11;
  WM_CAP_GRAB_FRAME = WM_CAP_START + 60;
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
  WM_CAP_SEQUENCE_NOFILE = WM_CAP_START + 63;
  WM_CAP_SET_CALLBACK_VIDEOSTREAM = WM_CAP_START + 6;
  WM_CAP_SET_CALLBACK_YIELD = WM_CAP_START + 4;
  WM_CAP_SET_CALLBACK_ERROR = WM_CAP_START + 2;
  WM_CAP_SET_AUDIOFORMAT = WM_CAP_START + 35;
  WM_CAP_GET_AUDIOFORMAT = WM_CAP_START + 36;

  IDS_CAP_BEGIN = 300;  // "Capture Start"
  IDS_CAP_END = 301;    // "Capture End"

  // AVICAP

function capCreateCaptureWindow(lpszWindowName: PChar; dwStyle: dword; x, y, nWidth, nHeight: integer; hwndParent: HWND; nID: integer): HWND; stdcall; external DLL2 name 'capCreateCaptureWindow' + IEDLLWNameExt;

function capGetDriverDescription(wDriverIndex: integer; lpszName: PChar; cnName: integer; lpszVer: PChar; cbVer: integer): longbool; stdcall; external DLL2 name 'capGetDriverDescription' + IEDLLWNameExt;

function CallBackFrameFunc(hWnd: HWND; lpVHdr: PVIDEOHDR): LRESULT; stdcall; forward;

function CallBackYeldFunc(hWnd: HWND): LRESULT; stdcall; forward;

function CallBackStatusFunc(hWnd: HWND; nID: integer; lpsz: PChar): LRESULT; stdcall; forward;

function capErrorCallback(hWnd: HWND; nID: integer; lpsz: PChar): LRESULT; stdcall; forward;

/////////////////////////////////////////////////////////////////////////////////////

constructor TImageEnVideoCap.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  //
  fUseWindowsCodec := true;
  fEnding := false;
  fCallBackFrame := false;
  fDrivers := TStringList.Create;
  fVideoSource := 0;
  FillDrivers;
  fWndC := 0;
  fCapture := false;
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
  fBitmap := TIEDibBitmap.create;
  fPix := nil;
  fDone := true;
  fDriverBusy := false;
  AllocateWindow;
end;

/////////////////////////////////////////////////////////////////////////////////////

destructor TImageEnVideoCap.Destroy;
begin
  if fCapture then
    SetCapture(false); // this calls also SetDisplayMode
  FreeAndNil(fDrivers);
  DestroyCaptureWindow;
  GlobalFree(fhBitmapInfo);
  IEDrawDibClose(fHDrawDib);
  FreeAndNil(fBitmap);
  PostMessage(fWinHandle, VH_DESTROYWINDOW, 0, 0);
  if fPix <> nil then
    freemem(fPix);

  inherited;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.FillDrivers;
var
  DeviceName: array[0..79] of Char;
  DeviceVersion: array[0..79] of Char;
  q: integer;
begin
  fDrivers.Clear;
  for q := 0 to 9 do
    if capGetDriverDescription(q, DeviceName, 80, DeviceVersion, 80) then
      fDrivers.Add(string(DeviceName) + ' ' + string(DeviceVersion));
end;

{!!
<FS>TImageEnVideoCap.Capture

<FM>Declaration<FC>
property Capture: boolean;

<FM>Description<FN>
Set Capture to True to activate video frames capture.  When Capture is True, video frames are sent to OnVideoFrame and OnVideoFrameRaw events.

!!}
procedure TImageEnVideoCap.SetCapture(v: boolean);
var
  cp: TCAPTUREPARMS;
begin
  if fWndC = 0 then
    CreateCaptureWindow;
  if v then
  begin
    // START VIDEO INPUT
    fEnding := false;
    fCapture := true;
    if fWndC <> 0 then
    begin
      if not fConnected then
        DriverConnect;
      SendMessage(fWndC, WM_CAP_SET_SCALE, 1, 0);
      SendMessage(fWndC, WM_CAP_SET_USER_DATA, 0, LPARAM(pointer(self)));
      //
      SendMessage(fWndC, WM_CAP_GET_SEQUENCE_SETUP, sizeof(cp), LPARAM(@cp));
      cp.fAbortLeftMouse := false;
      cp.fAbortRightMouse := false;
      cp.fLimitEnabled := false;
      cp.dwRequestMicroSecPerFrame := round((1 / fRecFrameRate) * 1000000);
      cp.wPercentDropForError := 100;
      if fRecording then
      begin
        cp.fYield := fRecMultitask;
        cp.fCaptureAudio := fRecAudio;
        SendMessage(fWndC, WM_CAP_SET_SEQUENCE_SETUP, sizeof(cp), LPARAM(@cp));
        if SendMessage(fWndC, WM_CAP_FILE_SET_CAPTURE_FILE, 0, LPARAM(PChar(fRecFileName))) = 0 then
          raise TVideoCapException.Create('Unable to create AVI file');
        SetCallBackFrame(fCallBackFrame);
        if SendMessage(fWndC, WM_CAP_SEQUENCE, 0, 0) = 0 then
          raise TVideoCapException.Create('Unable to start video recording');
      end
      else
      begin
        cp.fYield := true;
        cp.fCaptureAudio := false;
        //cp.wNumVideoRequested := 120;
        //cp.fStepCaptureAt2x := true;
        SendMessage(fWndC, WM_CAP_SET_SEQUENCE_SETUP, sizeof(cp), LPARAM(@cp));
        SetCallBackFrame(fCallBackFrame);
        SendMessage(fWndC, WM_CAP_SEQUENCE_NOFILE, 0, 0);
      end;
    end;
  end
  else
  begin
    // STOP VIDEO INPUT
    fEnding := true;
    SendMessage(fWndC, WM_CAP_STOP, 0, 0);
    SendMessage(fWndC, WM_CAP_SET_USER_DATA, 0, 0);
    DriverDisconnect;
    fCapture := false;
    DestroyCaptureWindow;
  end;
end;


// Assigns fWndC
// note: make sure that fWndC is 0 before call CreateCaptureWindow
procedure TImageEnVideoCap.CreateCaptureWindow;
begin
  fWndC := capCreateCaptureWindow(PChar(name), WS_CHILD, 0, 0, 50, 50, IEFindHandle(self), 0);
end;



procedure TImageEnVideoCap.DestroyCaptureWindow;
begin
  if fWndC <> 0 then
  begin
    SendMessage(fWndC, WM_CAP_SET_USER_DATA, 0, 0);
    DestroyWindow(fWndC);
    fWndC := 0;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.DriverDisconnect;
begin
  SendMessage(fWndC, WM_CAP_DRIVER_DISCONNECT, 0, 0);
  fConnected := false;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.SetVideoSource(v: integer);
begin
  fVideoSource := v;
  if fCapture then
  begin
    SetCapture(false);
    SetCapture(true);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.DriverConnect;
begin
  if fWndC = 0 then
    CreateCaptureWindow;
  DoJob(iejVIDEOCAP_CONNECTING, 0);
  if SendMessage(fWndC, WM_CAP_DRIVER_CONNECT, fVideoSource, 0) = 0 then
    raise TVideoCapException.Create('Unable to open video capture driver');
  fConnected := true;
  fBitmapInfoUp := false;
  FillBitmapInfo;
  DoJob(iejNOTHING, 0);
end;


// like DriverConnect, but returns false if fail to connect
function TImageEnVideoCap.DriverConnectNE: boolean;
begin
  if fWndC = 0 then
    CreateCaptureWindow;
  result := SendMessage(fWndC, WM_CAP_DRIVER_CONNECT, fVideoSource, 0) <> 0;
  fConnected := result;
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoCap.GetHasDlgVideoSource: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasDlgVideoSource;
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoCap.GetHasDlgVideoFormat: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasDlgVideoFormat;
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoCap.GetHasDlgVideoDisplay: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasDlgVideoDisplay;
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoCap.GetHasOverlay: boolean;
var
  fDriverCaps: TCAPDRIVERCAPS;
begin
  GetCaps(fDriverCaps);
  result := fDriverCaps.fHasOverlay;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.GetCaps(var fDriverCaps: TCAPDRIVERCAPS);
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
<FS>TImageEnVideoCap.StartRecord

<FM>Declaration<FC>
function StartRecord: boolean;

<FM>Description<FN>
Begin recording of the video input to AVI format. To select compression algorithm run the ConfigureCompression dialog.
StartRecord doesn't actually begins record, but enable recording when Capture becomes true.

StartRecord returns False if it fails, True if it’s successful.

!!}
procedure TImageEnVideoCap.StartRecord;
begin
  fRecording := true;
end;

{!!
<FS>TImageEnVideoCap.StopRecord

<FM>Declaration<FC>
procedure StopRecord;

<FM>Description<FN>
Stops recording begun with StartRecord.
!!}
procedure TImageEnVideoCap.StopRecord;
begin
  fRecording := false;
end;



{!!
<FS>TImageEnVideoCap.DoConfigureSource

<FM>Declaration<FC>
function DoConfigureSource: boolean;

<FM>Description<FN>
Executes the Configure Source Dialog of the selected driver (see VideoSource property). If the the driver is busy or it has failed to open, the function returns False.

!!}
function TImageEnVideoCap.DoConfigureSource: boolean;
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

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnVideoCap.DoConfigureFormat

<FM>Declaration<FC>
function DoConfigureFormat: boolean;

<FM>Description<FN>
Executes the Configure Format Dialog of the selected driver (see VideoSource property). If the the driver is busy or it has failed to open, the function returns False.

!!}
function TImageEnVideoCap.DoConfigureFormat: boolean;
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

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnVideoCap.DoConfigureDisplay

<FM>Declaration<FC>
function DoConfigureDisplay: boolean;

<FM>Description<FN>
Executes the Configure Display Dialog of the selected driver (see VideoSource property).  If the the driver is busy or it has failed to open, the function returns False.

!!}
function TImageEnVideoCap.DoConfigureDisplay: boolean;
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

function TImageEnVideoCap.FillBitmapInfo: boolean;
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
      if pt^.bmiHeader.biBitCount = 1 then
        fBitmap.AllocateBits(pt^.bmiHeader.biWidth, pt^.bmiHeader.biHeight, 1)
      else
        fBitmap.AllocateBits(pt^.bmiHeader.biWidth, pt^.bmiHeader.biHeight, 24);
      if fPix <> nil then
        freemem(fPix);
      getmem(fPix, pt^.bmiHeader.biSizeImage);
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
<FS>TImageEnVideoCap.GetVideoSize

<FM>Declaration<FC>
function GetVideoSize: TRect;

<FM>Description<FN>
Returns the rectangle of video input (as selected from ConfigureFormat dialog).

!!}
function TImageEnVideoCap.GetVideoSize: TRect;
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


// Enable/Disable calls to CallBackFrameFunc()
procedure TImageEnVideoCap.SetCallBackFrame(v: boolean);
begin
  fCallBackFrame := v;
  if fConnected then
  begin
    if v then
    begin
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0, LPARAM(@CallBackFrameFunc));
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_ERROR, 0, LPARAM(@capErrorCallback));
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_YIELD, 0, LPARAM(@CallBackYeldFunc));
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_STATUS, 0, LPARAM(@CallBackStatusFunc));
    end
    else
    begin
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0, 0);
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_ERROR, 0, 0);
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_YIELD, 0, 0);
      SendMessage(fWndC, WM_CAP_SET_CALLBACK_STATUS, 0, 0);
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.SetOnVideoFrame(v: TVideoFrameEvent);
begin
  fOnVideoFrame := v;
  SetCallBackFrame(assigned(fOnVideoFrame) or assigned(fOnVideoFrameRaw));
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.SetOnVideoFrameRaw(v: TVideoFrameRawEvent);
begin
  fOnVideoFrameRaw := v;
  SetCallBackFrame(assigned(fOnVideoFrame) or assigned(fOnVideoFrameRaw));
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnVideoCap.DoConfigureCompression

<FM>Declaration<FC>
function DoConfigureCompression: boolean;

<FM>Description<FN>
Executes the Configure Compression Dialog of the selected driver (see VideoSource property). If the driver is busy (fails to open), the function returns False.
!!}
function TImageEnVideoCap.DoConfigureCompression: boolean;
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

procedure TImageEnVideoCap.DoJob(job: TIEJob; per: integer);
begin
  if assigned(fOnJob) then
    fOnJob(self, job, per);
end;



(*
/* dwFlags field of VIDEOHDR */
#define VHDR_DONE      0x00000001  /* Done bit */
#define VHDR_PREPARED  0x00000002  /* Set if this header has been prepared */
#define VHDR_INQUEUE    0x00000004  /* Reserved for driver */
#define VHDR_KEYFRAME  0x00000008  /* Key Frame */
*)
function CallBackFrameFunc(hWnd: HWND; lpVHdr: PVIDEOHDR): LRESULT; {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
var
  pobj: pointer;
  obj: TImageEnVideoCap;
  i: pointer;
begin
  result := 0;
  if (lpVHdr^.dwFlags and 1 = 0) or (lpVHdr^.dwFlags and 2 = 0) then
    exit;
  if IESendMessageTimeOut(hWnd, WM_CAP_GET_USER_DATA, 0, 0, SMTO_ABORTIFHUNG or SMTO_BLOCK, 100, @i) <> 0 then
  begin
    pobj := pointer(i);
    if assigned(pobj) then
    begin
      obj := pobj;
      if (obj.fDone) and (not obj.fEnding) then
      begin
        obj.fDone := false;
        copymemory(obj.fpix, lpVHdr^.lpData, lpVHdr^.dwBufferLength);
        PostMessage(obj.fWinHandle, VH_FRAMEMESSAGE, 0, LPARAM(pobj));
      end;
    end;
  end;
end;

function capErrorCallback(hWnd: HWND; nID: integer; lpsz: PChar): LRESULT;
begin
  result := 0;
end;

function CallBackYeldFunc(hWnd: HWND): LRESULT;
begin
  result := 1;
end;

function CallBackStatusFunc(hWnd: HWND; nID: integer; lpsz: PChar): LRESULT; {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
var
  pobj: pointer;
  obj: TImageEnVideoCap;
  i: pointer;
begin
  if IESendMessageTimeOut(hWnd, WM_CAP_GET_USER_DATA, 0, 0, SMTO_ABORTIFHUNG or SMTO_BLOCK, 100, @i) <> 0 then
  begin
    pobj := pointer(i);
    if assigned(pobj) then
    begin
      obj := pobj;
      case nID of
        IDS_CAP_BEGIN:
          obj.fDriverBusy := true;
        IDS_CAP_END:
          obj.fDriverBusy := false;
      end;
    end;
  end;
  result := 0;
end;

function VideoCapWndProc(Window: HWND; Message, wParam, lParam: Longint): Longint; stdcall; {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
var
  pbi: PBITMAPINFOHEADER;
  obj: TImageEnVideoCap;
begin
  case Message of
    VH_FRAMEMESSAGE:
      begin
        obj := TImageEnVideoCap(lParam);
        with obj do
        begin
          if assigned(fOnVideoFrame) and (not fEnding) then
          begin
            pbi := GlobalLock(fhBitmapInfo);
            if fUseWindowsCodec then
              IEDrawDibDraw(fHDrawDib, fBitmap.HDC, 0, 0, pbi^.biWidth, pbi^.biHeight, pbi^, fPix, 0, 0, pbi^.biWidth, pbi^.biHeight, 0)
            else
              _CopyDIB2BitmapEx(THandle(pbi), fBitmap, fPix, false);
            GlobalUnLock(fhBitmapInfo);
            fOnVideoFrame(obj, fBitmap);
          end;
          if assigned(fOnVideoFrameRaw) and (not fEnding) then
            fOnVideoFrameRaw(obj, fhBitmapInfo, fpix);
          fDone := true;
          Result := 0;
        end;
      end;
    VH_DESTROYWINDOW:
      begin
        DestroyWindow(Window);
        Result := 0;
      end;
  else
    Result := DefWindowProc(Window, Message, wParam, lParam);
  end;
end;

var
  VideoCapWindowClass: TWndClass = (
    style:         0;
    lpfnWndProc:   nil;
    cbClsExtra:    0;
    cbWndExtra:    0;
    hInstance:     0;
    hIcon:         0;
    hCursor:       0;
    hbrBackground: 0;
    lpszMenuName:  nil;
    lpszClassName: 'TVideoCapWindow');

procedure TImageEnVideoCap.AllocateWindow;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  VideoCapWindowClass.lpfnWndProc := @VideoCapWndProc;

  VideoCapWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, VideoCapWindowClass.lpszClassName, TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @VideoCapWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(VideoCapWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(VideoCapWindowClass);
  end;
  fWinHandle := CreateWindow(VideoCapWindowClass.lpszClassName, '', 0, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnVideoCap.GetWaveFormat(var wf: TWAVEFORMATEX);
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

procedure TImageEnVideoCap.SetWaveFormat(var wf: TWAVEFORMATEX);
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

function TImageEnVideoCap.GetAudioFormat: word;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.wFormatTag
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnVideoCap.AudioFormat

<FM>Declaration<FC>
property AudioFormat: word;

<FM>Description<FN>
<A TImageEnVideoCap.AudioChannels>, <A TImageEnVideoCap.AudioSamplesPerSec>, <A TImageEnVideoCap.AudioBitsPerSample> and <A TImageEnVideoCap.AudioFormat> properties allow the application to get/set the audio recording format.
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
procedure TImageEnVideoCap.SetAudioFormat(v: word);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.wFormatTag := v;
  SetWaveFormat(wf);
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoCap.GetAudioChannels: word;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.nChannels;
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnVideoCap.AudioChannels

<FM>Declaration<FC>
property AudioChannels: word;

<FM>Description<FN>
<A TImageEnVideoCap.AudioChannels>, <A TImageEnVideoCap.AudioSamplesPerSec>, <A TImageEnVideoCap.AudioBitsPerSample> and <A TImageEnVideoCap.AudioFormat> properties allow the application to get/set the audio recording format.
AudioChannels specifies the number of channels in the waveform-audio data. Monaural data uses one channel and stereo data uses two channels.
!!}
procedure TImageEnVideoCap.SetAudioChannels(v: word);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.nChannels := v;
  SetWaveFormat(wf);
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoCap.GetAudioSamplesPerSec: dword;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.nSamplesPerSec;
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnVideoCap.AudioSamplesPerSec

<FM>Declaration<FC>
property AudioSamplesPerSec: dword;

<FM>Description<FN>
<A TImageEnVideoCap.AudioChannels>, <A TImageEnVideoCap.AudioSamplesPerSec>, <A TImageEnVideoCap.AudioBitsPerSample> and <A TImageEnVideoCap.AudioFormat> properties allow the application to get/set the audio recording format.
AudioSamplesPerSec specifies the sampling rate, in samples per second (hertz), at which each channel should be played or recorded.
!!}
procedure TImageEnVideoCap.SetAudioSamplesPerSec(v: dword);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.nSamplesPerSec := v;
  SetWaveFormat(wf);
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnVideoCap.GetAudioBitsPerSample: word;
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  result := wf.wBitsPerSample;
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnVideoCap.AudioBitsPerSample

<FM>Declaration<FC>
property AudioBitsPerSample: word;

<FM>Description<FN>
<A TImageEnVideoCap.AudioChannels>, <A TImageEnVideoCap.AudioSamplesPerSec>, <A TImageEnVideoCap.AudioBitsPerSample> and <A TImageEnVideoCap.AudioFormat> properties allow the application to get/set the audio recording format.
AudioBitsPerSample specifies the bits per sample for the AudioFormat format type.  If a compression scheme cannot define a bits-per-sample value, this field is zero.
!!}
procedure TImageEnVideoCap.SetAudioBitsPerSample(v: word);
var
  wf: TWAVEFORMATEX;
begin
  GetWaveFormat(wf);
  wf.wBitsPerSample := v;
  SetWaveFormat(wf);
end;

{$ELSE} // {$ifdef IEINCLUDEVIDEOCAPTURE}

interface
implementation

{$ENDIF}

end.





