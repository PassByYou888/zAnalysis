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
File version 1012
*)


unit iexAcquire;
// NPC: 15/11/11



{$R-}
{$Q-}

{$I ie.inc}


{$ifdef IEINCLUDEIEXACQUIRE}


interface

uses
  Windows, Classes,
  {$ifdef IEHASUITYPES} System.UITypes, {$endif}
  hyieutils, ieTwain, ieWIA, iexDCIM, hyiedefs, Controls;


type



{!!
<FS>TIEAcquireApi

<FM>Declaration<FC>
TIEAcquireApi = (ieaTwain, ieaWIA, ieaDCIM, ieaNone);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieaTwain</C> <C>Acquire from Twain device</C> </R>
<R> <C>ieaWIA</C> <C>Acquire from WIA (scanners or camera)</C> </R>
<R> <C>ieaDCIM</C> <C>Read from any attached device containing a DCIM folder (e.g. camera card in a slot or a USB connected camera)</C> </R>
<R> <C>ieaNone</C> <C>No valid source has been set</C> </R>
</TABLE>
!!}
  TIEAcquireApi = (ieaTwain, ieaWIA, ieaDCIM, ieaNone);

{!!
<FS>TIEAcquireApis

<FM>Declaration<FC>
TIEAcquireApis = Set of <A TIEAcquireApi>;
!!}
  TIEAcquireApis = Set of TIEAcquireApi;


{!!
<FS>TIEAcquireOrientation

<FM>Declaration<FC>
}
  TIEAcquireOrientation = (ieaoPortrait, ieaoLandscape, ieaoRotate90, ieaoRotate180);
{!!}

{!!
<FS>TIEAcquireRotation

<FM>Declaration<FC>
}
  TIEAcquireRotation = (iearNone, iear90CW, iear180CW, iear270CW);
{!!}  

{!!
<FS>TIEAcquirePixelType

<FM>Declaration<FC>
}
  TIEAcquirePixelType = (ieapMonochrome, ieap8BitGrayScale, ieap16BitGrayScale, ieapFullColor, ieapFullColor16, ieapOther);
{!!}

{!!
<FS>TIEAcquireDeviceType

<FM>Declaration<FC>
TIEAcquireDeviceType = (ieadScanner, ieadCamera, ieadDrive, ieadUnknown);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Device Type</H> </R>
<R> <C>ieadUnknown</C> <C>Could not be determined</C> </R>
<R> <C>ieadScanner</C> <C>Scanner</C> </R>
<R> <C>ieadCamera</C> <C>Camera</C> </R>
<R> <C>ieadDrive</C> <C>Camera card or connected device that appears as a removable drive and contains a DCIM folder</C> </R>
</TABLE>
!!}
TIEAcquireDeviceType = (ieadUnknown,
                        ieadScanner,
                        ieadCamera ,
                        ieadDrive  );

{!!
<FS>TIEAcquireSource

<FM>Declaration<FC>
TIEAcquireSource = Record
  Name       : AnsiString;
  Location   : Variant;
  Api        : <A TIEAcquireApi>;
  DeviceType : <A TIEAcquireDeviceType>;
end;

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>Api</C> <C>The <L TIEAcquireApi>method</L> used to connect to this device, e.g. Twain or WIA</C> </R>
<R> <C>Location</C> <C>Index of this device for <L TIEAcquireApi>ieaTwain</L> and <L TIEAcquireApi>ieaWIA</L> types.  Full path for <L TIEAcquireApi>ieaDCIM</L>.</C> </R>
<R> <C>Name</C> <C>The system name for this device</C> </R>
<R> <C>DeviceType</C> <C>The type of <L TIEAcquireDeviceType>device</L>, e.g. camera or scanner</C> </R>
</TABLE>
!!}
TIEAcquireSource = Record
  Api        : TIEAcquireApi;  
  Location   : Variant;       
  Name       : AnsiString;
  DeviceType : TIEAcquireDeviceType;
end;

  // Helper functions
  function IEAcquireSource(Api : TIEAcquireApi; Location : Variant; const sName : AnsiString; DeviceType : TIEAcquireDeviceType) : TIEAcquireSource;
  function AcquireSourceToStr(AcquireSource : TIEAcquireSource) : string;
  function StrToAcquireSource(const sRawDeviceStr : string) : TIEAcquireSource;
  function IsAcquireSourceStr(const value : string) : Boolean;

  // Custom draw the items of a combo or listbox (call from DrawItem event)
  procedure DrawAcquireComboListBoxItem(TheControl : TWinControl;
                                        CanvasRect : TRect;
                                        const sRawDeviceStr : string;
                                        AnImageList : TImageList = nil;
                                        iScannerGlyph : Short = -1;
                                        iCameraGlyph : Short = -1;
                                        iDriveGlyph : Short = -1;
                                        iUnknownGlyph : Short = -1);
  
type
{!!
<FS>TIEAcquireParams

<FM>Description<FN>
The TIEAcquireParams object provides a generic interface to the <L TIETwainParams>Twain</L>, <L TIEWia>WIA</L> and <L TIEDcimAcquire>DCIM Retrieval</L> interfaces of ImageEn. It allows you to use all available acquisition methods without writing separate code for Twain and WIA.


<IMG help_images\IEMView_Capture.gif>

<FM>General Method and Properties<FN>  
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AttachedTwainParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AttachedDCIMParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AttachedWIAParams></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAcquireParams.Create></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAcquireParams.Destroy></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.Aborting></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAcquireParams.Acquire></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAcquireParams.FillListWithSources></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.SelectedSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.SelectedSourceApi></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAcquireParams.SelectSource></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAcquireParams.SetSource></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAcquireParams.SetSourceByStr></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.VisibleDialog></C> </R>
</TABLE>

<FM>Device Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AcquireFrameEnabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AcquireFrameBottom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AcquireFrameLeft></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AcquireFrameRight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AcquireFrameTop></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.AutoFeed></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.BitDepth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.Brightness></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.Contrast></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.DuplexEnabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.DuplexSupported></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.FeederEnabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.FeederLoaded></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.Orientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.PaperDetectable></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.PhysicalHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.PhysicalWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.PixelType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.Rotation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.Threshold></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.XResolution></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAcquireParams.YResolution></C> </R>
</TABLE>

<FM>IEAcquire Helper Functions<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A StrToAcquireSource></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A AcquireSourceToStr></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A DrawAcquireComboListBoxItem></C> </R>
</TABLE>

!!}
  TIEAcquireParams = class
  private
    fOwner: TComponent;
    fTwainParams: TIETwainParams;
    fDcimParams : TIEDcimAcquire;
    fDuplex: boolean;
    fWiaParams: TIEWia;
    fWIAVisibleDialog : Boolean;
    fAborting : Boolean;
    function GetXResolution : Double;
    function GetYResolution : Double;  
    procedure SetXResolution(v : Double);
    procedure SetYResolution(v : Double);
    function GetContrast: Double;
    procedure SetContrast(v : Double);
    function GetBrightness: Double;
    procedure SetBrightness(v : Double);
    function GetThreshold: Double;
    procedure SetThreshold(v : Double);
    function GetRotation: TIEAcquireRotation;
    procedure SetRotation(v : TIEAcquireRotation);
    function GetPixelType: TIEAcquirePixelType;
    procedure SetPixelType(v : TIEAcquirePixelType);
    function GetBitDepth : Integer;
    procedure SetBitDepth(v : Integer);
    function GetPhysicalHeight: double;
    function GetPhysicalWidth: double;  
    function GetPhysicalHeightPixels: Integer;
    function GetPhysicalWidthPixels: Integer;
    function GetFeederEnabled: boolean;
    function GetAutoFeed: boolean;
    function GetOrientation: TIEAcquireOrientation;
    procedure SetOrientation(v : TIEAcquireOrientation);
    procedure SetFeederEnabled(v: boolean);
    procedure SetAutoFeed(v: boolean);
    function GetAcquireFrame(idx: integer): double;
    procedure SetAcquireFrame(idx: integer; v: double);
    procedure SetDuplexEnabled(v: boolean);
    function GetDuplexEnabled: boolean;
    procedure SetVisibleDialog(v: boolean);
    function GetVisibleDialog: boolean;
    procedure SetAcquireFrameEnabled(v: boolean);
    function GetAcquireFrameEnabled: boolean;
    function GetFeederLoaded: boolean;
    function GetDuplexSupported: boolean;
    function GetPaperDetectable: boolean;
    function GetSelectedSource: TIEAcquireSource;
    function AcquireEx(bMultiple  : Boolean;
                       DestBitmap : TIEBitmap;
                       DestIOParamVals : TObject;
                       OnGetImage  : TIEMultiCallBack;
                       OnProgress: TIEProgressEvent;
                       bNativePixelFormat : Boolean) : Boolean;
    function WIADevicePropertyHasFlag(PropId: DWord; Flag : Integer) : Boolean;
    procedure WIADevicePropertySetFlag(PropId: DWord; Flag : Integer; bSet : Boolean);
    function GetTwainSource(iIndex : Integer) : TIEAcquireSource;        
    function GetDcimSource(const sPath : string) : TIEAcquireSource;
    procedure CheckInitialize(bIncludeWIA : Boolean);

    function GetWiaSource(iIndex : Integer) : TIEAcquireSource;
    function GetSelectedSourceApi: TIEAcquireApi;
  public
    fSelectedSourceApi : TIEAcquireApi;

    constructor Create(Owner: TComponent);
    destructor Destroy; override;

    // Single Image Acquisition
    function Acquire(DestBitmap : TIEBitmap; DestIOParamVals : TObject; OnProgress: TIEProgressEvent = nil; bNativePixelFormat : Boolean = False) : Boolean; overload;

    // Muliple Image Acquisition
    function Acquire(OnGetImage  : TIEMultiCallBack; OnProgress: TIEProgressEvent = nil; bNativePixelFormat : Boolean = False) : Boolean; overload;

                                  
    property SelectedSource : TIEAcquireSource Read GetSelectedSource;


{!!
<FS>TIEAcquireParams.SelectedSourceApi

<FM>Declaration<FC>
property SelectedSourceApi : <A TIEAcquireApi>; (Read-only)

<FM>Description<FN>
Returns the acquisition source that is currently active due to selection by the user with <A TIEAcquireParams.SelectSource> or programatically using <A TIEAcquireParams.SetSource>.
A <A TIEAcquireSource> record is returned that provides meta information about the device (Selectedource.Name, SelectedSource.DeviceType) and technical details (SelectedSource.Api, SelectedSource.Location).
If no device is selected then SelectedSource.Api will be ieaNone.

<FB>** Generally you should NOT use this property directly. Use <L TImageEnIO.SelectedAcquireSource>TImageEnIO.SelectedAcquireSource</L> or <L TImageEnMIO.SelectedAcquireSource>TImageEnMIO.SelectedAcquireSource</L> Instead **<FN>

Note: This is the same as SelectedSource.API

<FM>See Also<FN>
- <A TIEAcquireParams.SelectedSource>

<FM>Examples<FC>
// Display the selected source
if AcquireParams.SelectedSource.Api = ieaNone then
  ShowMessage('No device is selected')
else
  ShowMessage('The selected device is ' + AcquireParams.SelectedSource.Name);

// Read and restore the selected source
var
  sDevice : string;
begin
  ..
  // Read the selected device
  sDevice := AcquireSourceToStr(AcquireParams.SelectedSource);
  ..
end;
    
var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ..
  // Restore the device selection
  ADevice := StrToAcquireSource(sDevice);
  AcquireParams.SetSource(ADevice.Api, ADevice.Location);
  ..
end;
!!}
    property SelectedSourceApi : TIEAcquireApi Read GetSelectedSourceApi;
                         
    function SelectSource(Apis : TIEAcquireApis = [ieaTwain, ieaWIA, ieaDCIM]): boolean;
    function SetSource(Api: TIEAcquireApi; Location : Variant) : boolean;
    function SetSourceByStr(const sRawDeviceStr : string) : boolean;

{!!
<FS>TIEAcquireParams.AttachedTwainParams

<FM>Declaration<FC>
property AttachedTwainParams : <A TIETwainParams>; (Read/Write)

<FM>Description<FN>
<A TIEAcquireParams> needs to be attached to a <A TIETwainParams> object in order to acquire from a <L TIEAcquireApi>Twain</L> source.

<FM>Example<FC>
AcquireParams := TIEAcquireParams.Create;
AcquireParams.AttachedTwainParams := fTwainParams;
AcquireParams.AttachedWIAParams   := fWIA;
AcquireParams.AttachedDCIMParams  := fDcimParams;
If AcquireParams.SelectSource([ieaTwain, ieaWIA, ieaDCIM]) then
  AcquireParams.Acquire;
!!}
    property AttachedTwainParams : TIETwainParams read fTwainParams write fTwainParams;


{!!
<FS>TIEAcquireParams.AttachedDCIMParams

<FM>Declaration<FC>
property AttachedDCIMParams : <A TIEDcimAcquire>; (Read/Write)

<FM>Description<FN>
<A TIEAcquireParams> needs to be attached to a <A TIEDcimAcquire> object in order to retrieve files from a <L TIEAcquireApi>DCIM device</L>.

<FM>Example<FC>
AcquireParams := TIEAcquireParams.Create;
AcquireParams.AttachedTwainParams := fTwainParams;
AcquireParams.AttachedWIAParams   := fWIA;
AcquireParams.AttachedDCIMParams  := fDcimParams;
If AcquireParams.SelectSource([ieaTwain, ieaWIA, ieaDCIM]) then
  AcquireParams.Acquire;
!!}
    property AttachedDCIMParams : TIEDcimAcquire read fDcimParams write fDcimParams;

{!!
<FS>TIEAcquireParams.AttachedWIAParams

<FM>Declaration<FC>
property AttachedWIAParams : <A TIEWia>; (Read/Write)

<FM>Description<FN>
<A TIEAcquireParams> needs to be attached to a <A TIEWia> object in order to retrieve files from a <L TIEAcquireApi>WIA source</L>.

<FM>Example<FC>
AcquireParams := TIEAcquireParams.Create;
AcquireParams.AttachedTwainParams := fTwainParams;
AcquireParams.AttachedWIAParams   := fWIA;
AcquireParams.AttachedDCIMParams  := fDcimParams;
If AcquireParams.SelectSource([ieaTwain, ieaWIA, ieaDCIM]) then
  AcquireParams.Acquire;
!!}
    property AttachedWIAParams : TIEWia read fWiaParams write fWiaParams;

    procedure FillListWithSources(ssDest : TStrings; Apis: TIEAcquireApis = [ieaTwain, ieaWIA, ieaDCIM]; bNameOnly : Boolean = False);

    function FindSourceByName(const sName : string) : TIEAcquireSource;

{!!
<FS>TIEAcquireParams.Aborting

<FM>Declaration<FC>
property Aborting : Boolean; (Read/Write)

<FM>Description<FN>
Set to false to cancel the acquisition of images (e.g. during the OnProgress event)
!!}
    property Aborting : Boolean read FAborting write FAborting;  
    property VisibleDialog: boolean read GetVisibleDialog write SetVisibleDialog;

    // Capabilities
    property XResolution: Double read GetXResolution write SetXResolution;
    property YResolution: Double read GetYResolution write SetYResolution;
    property PixelType: TIEAcquirePixelType read GetPixelType write SetPixelType;
    property BitDepth : Integer read GetBitDepth write SetBitDepth;
    property PhysicalHeight: double read GetPhysicalHeight;
    property PhysicalWidth: double read GetPhysicalWidth;    
    property PhysicalHeightPixels: Integer read GetPhysicalHeightPixels;
    property PhysicalWidthPixels: Integer read GetPhysicalWidthPixels;
    property FeederEnabled: boolean read GetFeederEnabled write SetFeederEnabled;
    property AutoFeed: boolean read GetAutoFeed write SetAutoFeed;
    property FeederLoaded: boolean read GetFeederLoaded;
    property PaperDetectable: boolean read GetPaperDetectable;
    property Orientation : TIEAcquireOrientation read GetOrientation write SetOrientation;
    property AcquireFrameLeft: double index 0 read GetAcquireFrame write SetAcquireFrame;
    property AcquireFrameTop: double index 1 read GetAcquireFrame write SetAcquireFrame;
    property AcquireFrameRight: double index 2 read GetAcquireFrame write SetAcquireFrame;
    property AcquireFrameBottom: double index 3 read GetAcquireFrame write SetAcquireFrame;
    property DuplexEnabled: boolean read GetDuplexEnabled write SetDuplexEnabled;
    property DuplexSupported: boolean read GetDuplexSupported;
    property AcquireFrameEnabled: boolean read GetAcquireFrameEnabled write SetAcquireFrameEnabled;
    property Contrast: Double read GetContrast write SetContrast;
    property Brightness : Double read GetBrightness write SetBrightness;
    property Threshold: Double read GetThreshold write SetThreshold;
    property Rotation: TIEAcquireRotation read GetRotation write SetRotation;
  end;

const
  Default_Device = -1;
  Error_Compiled_Without_WIA_Str = 'ImageEn compiled without WIA support';

implementation

uses
  {$IFDEF IENEWVARIANTS}
  Variants,
  {$ENDIF}
  SysUtils, imscan, ImageEnIO, iexAcquireForm, Dialogs, IEMIO;


procedure TIEAcquireParams.CheckInitialize(bIncludeWIA : Boolean);
begin
  // check all Attached*Params parameters have been set
  if fOwner is TImageEnIO then
    TImageEnIO(fOwner).InitializeAcquireSource(bIncludeWIA)
  else
  {$ifdef IEINCLUDEMULTIVIEW}
  if fOwner is TImageEnMIO then
    TImageEnMIO(fOwner).InitializeAcquireSource(bIncludeWIA);
  {$endif}
end;


function TIEAcquireParams.GetTwainSource(iIndex : Integer) : TIEAcquireSource;
var
  sName: string;
  ADeviceType : TIEAcquireDeviceType;
begin
  Result := IEAcquireSource(ieaNone, Default_Device, '', ieadUnknown);
  if iIndex < fTwainParams.SourceCount then
  begin
    sName := string(fTwainParams.SourceName[iIndex]);
    if Pos('SCAN', Uppercase(sName)) > 0 then
      ADeviceType := ieadScanner
    else
    if Pos('CAM', Uppercase(sName)) > 0 then
      ADeviceType := ieadCamera
    else
      ADeviceType := ieadUnknown;
    Result := IEAcquireSource(ieaTwain, iIndex, AnsiString(sName), ADeviceType);
  end;
end;


function TIEAcquireParams.GetDcimSource(const sPath : string) : TIEAcquireSource;
begin
  Result := IEAcquireSource(ieaNone, Default_Device, '', ieadUnknown);
  if sPath <> '' then
    Result := IEAcquireSource(ieaDCIM,
                              sPath,
                              AnsiString(format('Device on %s:\', [uppercase(sPath[1])])),
                              ieadDrive);
end;

function TIEAcquireParams.GetWiaSource(iIndex : Integer) : TIEAcquireSource;
var
  WiaDevice : TIEWiaDeviceInfo;
  ADeviceType : TIEAcquireDeviceType;
begin
  Result := IEAcquireSource(ieaNone, Default_Device, '', ieadUnknown);
  if iIndex < fWiaParams.DevicesInfoCount then
  Begin
    WiaDevice := fWiaParams.DevicesInfo[iIndex];
    case WiaDevice.DeviceType of
      iewScanner       : ADeviceType := ieadScanner;
      iewStreamingVideo,
      iewDigitalCamera : ADeviceType := ieadCamera;
      else               ADeviceType := ieadUnknown;
    end;
    Result := IEAcquireSource(ieaWIA, iIndex, AnsiString(WiaDevice.Name), ADeviceType);
  end;
end;



{!!
<FS>TIEAcquireParams.SelectedSource

<FM>Declaration<FC>
property SelectedSource: <A TIEAcquireSource>; (Read-only)

<FM>Description<FN>
Returns the acquisition source that is currently active due to selection by the user with <A TIEAcquireParams.SelectSource> or programatically using <A TIEAcquireParams.SetSource>.
A <A TIEAcquireSource> record is returned that provides meta information about the device (SelectedSource.Name, SelectedSource.DeviceType) and technical details (SelectedSource.Api, SelectedSource.Location).
If no device is selected then SelectedSource.Api will be ieaNone.

<FB>** Generally you should NOT use this property directly. Use <L TImageEnIO.SelectedAcquireSource>TImageEnIO.SelectedAcquireSource</L> or <L TImageEnMIO.SelectedAcquireSource>TImageEnMIO.SelectedAcquireSource</L> Instead **<FN>

<FM>See Also<FN>
- <A TIEAcquireParams.SelectSource>
- <A TIEAcquireParams.SetSource>

<FM>Examples<FC>
// Display the selected source
if AcquireParams.SelectedSource.Api = ieaNone then
  ShowMessage('No device is selected')
else
  ShowMessage('The selected device is ' + AcquireParams.SelectedSource.Name);

// Read and restore the selected source
var
  sDevice : string;
begin
  ..
  // Read the selected device
  sDevice := AcquireSourceToStr(AcquireParams.SelectedSource);
  ..
end;
    
var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ..
  // Restore the device selection
  ADevice := StrToAcquireSource(sDevice);
  AcquireParams.SetSource(ADevice.Api, ADevice.Location);
  ..
end;
!!}
function TIEAcquireParams.GetSelectedSource: TIEAcquireSource;
begin
  Result := IEAcquireSource(ieaNone, Default_Device, '', ieadUnknown);

  case SelectedSourceApi of

    ieaTwain : Result := GetTwainSource(fTwainParams.SelectedSource);

    ieaWIA   : Result := GetWiaSource(fWiaParams.ConnectedDeviceIndex);

    ieaDCIM  : Result := GetDcimSource(fDcimParams.SelectedSource);
  end;
end;


{!!
<FS>TIEAcquireParams.Acquire

<FM>Declaration<FC>
function Acquire(DestBitmap: <A TIEBitmap>; DestIOParamVals: TObject; OnProgress: <A TIEProgressEvent> = nil; bNativePixelFormat: Boolean = False): Boolean; overload;
function Acquire(OnGetImage: <A TIEMultiCallBack>; OnProgress: <A TIEProgressEvent> = nil; bNativePixelFormat: Boolean = False): Boolean; overload;

<FM>Description<FN>
Perform an image acquisition from the <A TIEAcquireParams.SelectedSource>, which may be a camera card, Twain or WIA device (<A TIEAcquireParams.SelectedSource> is set when the user chooses a source if you have called <A TImageEnIO.SelectAcquireSource> or manually set a source using  <A TImageEnIO.SetAcquireSource>).

<FB>** Generally you should NOT call this method directly. Use <A TImageEnIO.Acquire> or <A TImageEnMIO.Acquire> Instead **<FN>

Acquiring a single image:
function Acquire(DestBitmap : <A TIEBitmap>; DestIOParamVals : TObject; OnProgress: <A TIEProgressEvent> = nil; bNativePixelFormat : Boolean = False) : Boolean;
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>DestBitmap</C> <C>The <A TIEBitmap> which will be filled with the acquired image</C> </R>
<R> <C>DestIOParamVals</C> <C>A <A TIOParamsVals> object which will be filled with the parameters of the acquired image (optional)</C> </R>
<R> <C>OnProgress</C> <C>Event to display acquisition progress and allow <L TIEAcquireParams.Aborting>aborting</L> (optional)</C> </R>
<R> <C>bNativePixelFormat</C> <C>Set to true to disable conversion of paletted images, gray scale and all other formats to ImageEn native pixel formats (24 bit or 1 bit)</C> </R>
</TABLE>

Acquiring multiple images:
function Acquire(OnGetImage  : <A TIEMultiCallBack>; OnProgress: <A TIEProgressEvent> = nil; bNativePixelFormat : Boolean = False) : Boolean; overload;
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>OnGetImage</C> <C>Event to call for every acquired image</C> </R>
<R> <C>OnProgress</C> <C>Event to display acquisition progress and allow <L TIEAcquireParams.Aborting>aborting</L> (optional)</C> </R>
<R> <C>bNativePixelFormat</C> <C>Set to true to disable conversion of paletted images, gray scale and all other formats to ImageEn native pixel formats (24 bit or 1 bit)</C> </R>
</TABLE>
!!}
// Single Image Acquisition
function TIEAcquireParams.Acquire(DestBitmap: TIEBitmap; DestIOParamVals: TObject; OnProgress: TIEProgressEvent = nil; bNativePixelFormat: Boolean = False): Boolean;
begin
  result := AcquireEx(False, DestBitmap, DestIOParamVals, nil, OnProgress, bNativePixelFormat);
end;

// Muliple Image Acquisition
function TIEAcquireParams.Acquire(OnGetImage: TIEMultiCallBack; OnProgress: TIEProgressEvent = nil; bNativePixelFormat: Boolean = False): Boolean;
begin
  result := AcquireEx(True, nil, nil, OnGetImage, OnProgress, bNativePixelFormat);
end;

function TIEAcquireParams.AcquireEx(bMultiple  : Boolean;
                                    DestBitmap : TIEBitmap;
                                    DestIOParamVals : TObject;
                                    OnGetImage : TIEMultiCallBack;
                                    OnProgress : TIEProgressEvent;
                                    bNativePixelFormat : Boolean) : Boolean;
var
  Progress: TProgressRec;
  bNeedCreateBitmap: Boolean;
begin
  Result := False; 
  fAborting := False;
  CheckInitialize(SelectedSourceApi = ieaWIA);

  case SelectedSourceApi of

    ieaTwain:
      begin
        Progress.Aborting := @fAborting;
        Progress.fOnProgress := OnProgress;
        Progress.Sender := fOwner;

        if bMultiple = False then
        begin
          fTwainParams.LastError := 0;
          fTwainParams.LastErrorStr := '';
        end;
        result := IETW_Acquire(DestBitmap, bMultiple, OnGetImage, fTwainParams, TIOParamsVals(DestIOParamVals), Progress, @fTwainParams.TwainShared, IEFindHandle(fOwner), bNativePixelFormat);
        SetFocus(IEFindHandle(fOwner));
      end;

    ieaWIA:
      begin
        // SHOW ACQUIRE DIALOG
        If fWiaVisibleDialog then
          If fWiaParams.ShowAcquireDialog(true) = False then
            exit;

        bNeedCreateBitmap := not Assigned(DestBitmap);
        if bNeedCreateBitmap then
          DestBitmap := TIEBitmap.Create;
        try
          fWiaParams.ProcessingBitmap := DestBitmap;

          if assigned(OnGetImage) then
            fWIAParams.OnGetPage := OnGetImage;

          if bMultiple then
          begin
            // select feeder and duplex
            if fDuplex then
              fWIAParams.SetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_FEEDER or WIA_DUPLEX)
            else
              fWIAParams.SetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_FEEDER);
            fWIAParams.SetDeviceProperty(WIA_DPS_PAGES, 0); // unlimited number of pages
          end;
          result := fWIAParams.Transfer(nil, bMultiple);
        finally
          if bNeedCreateBitmap then
            FreeAndNil(DestBitmap);
        end;
      end;

    ieaDCIM:
      begin
        Result := fDCIMParams.AcquireEx(bMultiple, DestBitmap, DestIOParamVals, OnGetImage, '', False, OnProgress);
      end; 
  end;
end;



{!!
<FS>TIEAcquireParams.SelectSource

<FM>Declaration<FC>
function SelectSource(Apis: <A TIEAcquireApis> = [ieaTwain, ieaWIA, ieaDCIM]) : boolean;

<FM>Description<FN>
Prompts the user with a dialog to select a Twain, WIA or DCIM device.

<FB>** Generally you should NOT call this method directly. Use <A TImageEnIO.SelectAcquireSource> or <A TImageEnMIO.SelectAcquireSource> Instead **<FN>

Use Apis to specify which sources are available to the user. Any combination of the following can be used:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieaTwain</C> <C>Acquire from Twain device</C> </R>
<R> <C>ieaWIA</C> <C>Acquire using WIA (scanners or camera)</C> </R>
<R> <C>ieaDCIM</C> <C>Read from any attached device containing a DCIM folder (e.g. camera card in a slot or a USB connected camera)</C> </R>
</TABLE>

Note: If your location is only [ieTwain] or [ieWIA] then the default Twain/WIA selector is shown. Otherwise a custom device selector is used (Use <A TIEImageEnGlobalSettings.MsgLanguage> to control the language in the custom dialog).

Returns False if user press "Cancel" button.

<FM>Example<FC>
if AcquireParms.SelectSource([ieaTwain, ieaWIA, ieaDCIM]) then
  AcquireParms.Acquire;
!!}
function TIEAcquireParams.SelectSource(Apis: TIEAcquireApis = [ieaTwain, ieaWIA, ieaDCIM]): boolean;
var
  sn: AnsiString;
  AnAcquireForm : TiexAcquireForm;
begin
  result := false;
  if Apis = [] then
    Apis := [ieaTwain, ieaWIA, ieaDCIM];    
  CheckInitialize(ieaWIA in Apis);

  // TWAIN ONLY
  if Apis = [ieaTwain] then
  begin
    result := IETW_SelectImageSource(sn, @fTwainParams.TwainShared, IEFindHandle(fOwner));
    fTwainParams.SelectSourceByName(sn);
    if Result then
      fSelectedSourceApi := ieaTwain;
  end
  else
  // WIA ONLY
  if Apis = [ieaWIA] then
  begin
    result := fWIAParams.ConnectToUsingDialog;
    if Result then
      fSelectedSourceApi := ieaWIA;
  end
  else
  // MULTIPLE SOURCES
  begin
    AnAcquireForm := TiexAcquireForm.create(nil);
    try
      FillListWithSources(AnAcquireForm.lbxSources.Items, Apis, False);
      AnAcquireForm.SelectedSource := AcquireSourceToStr(SelectedSource);
      if AnAcquireForm.lbxSources.Items.count = 0 then
        MessageDlg('No acquisition devices could be located on your computer.', mtError, [mbOK], 0)
      else
      if AnAcquireForm.ShowModal = mrOK then
        Result := SetSourceByStr(AnAcquireForm.SelectedSource);
    finally
      AnAcquireForm.free;
    end;
  end;
end;



{!!
<FS>TIEAcquireParams.SetSource

<FM>Declaration<FC>
function SetSource(Api : <A TIEAcquireApi>; Location : Variant) : boolean;

<FM>Description<FN>
Programatically set the selected acquisition source by an API type and device. The selected device will be used for subsequent calls to <A TIEAcquireParams.Acquire>.
Result is false if the device cannot be selected (i.e. is not connected or does not exist).

The API can be one of the following:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieaTwain</C> <C>Acquire from Twain device</C> </R>
<R> <C>ieaWIA</C> <C>Acquire using WIA (scanners or camera)</C> </R>
<R> <C>ieaDCIM</C> <C>Read from any attached device containing a DCIM folder (e.g. camera card in a slot or a USB connected camera)</C> </R>
</TABLE>

Location can be one of the following:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>Default_Device</C> <C>The default device for that API type will be selected</C> </R>
<R> <C>Index</C> <C>An index of a device, e.g. 0 for the first device (ieaTwain/ieaWIA)</C> </R>
<R> <C>Name</C> <C>The name of a device, e.g. 'CanoScan FB620' (ieaTwain/ieaWIA)</C> </R>
<R> <C>Path</C> <C>The path of a DCIM folder, e.g. 'I:\DCIM\' (ieaDCIM)</C> </R>
<R> <C>Drive Letter</C> <C>The letter of a connected camera card or device containing a DCIM folder, e.g. 'I' (ieaDCIM)</C> </R>
<R> <C>ieaDCIM</C> <C>Read from any attached device containing a DCIM folder (e.g. camera card in a slot or a USB connected camera)</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEAcquireParams.Acquire>
- <A TIEAcquireParams.SelectedSource>

<FM>Examples<FC>
// Acquire from the default Twain device
if ImageEnMView1.MIO.SetAcquireSource(ieaTwain, Default_Device) then
  ImageEnMView1.MIO.Acquire;

// Select a Twain scanner by name
ImageEnMView1.MIO.SetAcquireSource(ieaTwain, 'CanoScan FB620');

// Select the second WIA device      
ImageEnMView1.MIO.SetAcquireSource(ieaWIA, 1);

// Acquire from the camera card on H drive
if ImageEnMView1.MIO.SetAcquireSource(ieaDCIM, 'H') then
  ImageEnMView1.MIO.Acquire;

// Read and restore a source
var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ..
  // Read the selected device
  sDevice := AcquireSourceToStr(ImageEnMView1.MIO.SelectedAcquireSource);
  ..
end;
    
var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ..
  // Restore the device selection
  ADevice := StrToAcquireSource(sDevice);
  ImageEnMView1.MIO.SetAcquireSource(ADevice.Api, ADevice.Location);
  ..
end;
!!}                       
function TIEAcquireParams.SetSource(Api: TIEAcquireApi; Location : Variant) : boolean;
var
  iIndex : Integer;
  sName  : String; 
begin
  Result := False;

  CheckInitialize(Api = ieaWIA);

  iIndex := Default_Device;
  sName  := '';
  if varType(Location) = varInteger then  
    iIndex := Location
  else
  {$IfDef UNICODE}
  if (varType(Location) = varUString) then
    sName := Location
  else
  {$EndIf}   
  if (varType(Location) = varString) then
    sName := Location;

  case Api of

    ieaTwain : begin
                 if sName <> '' then
                   // Select scanner by name
                   result := fTwainParams.SelectSourceByName(AnsiString(sName))
                 else
                 begin
                   // Select by index
                   if iIndex = Default_Device then
                     iIndex := fTwainParams.GetDefaultSource;
                   if iIndex > -1 then
                   try
                     fTwainParams.SelectedSource := iIndex;
                     Result := True;
                   except
                     // Device cannot be selected
                   end;
                 end;
               end;

    ieaWIA   : begin
                 if sName <> '' then
                   // Select scanner by name
                     result := fWIAParams.ConnectTo(sName)
                 else
                 begin
                   // Select by index
                   if iIndex = Default_Device then
                     iIndex := 0;
                     Result := fWIAParams.ConnectTo(iIndex);
                 end;
               end;

    ieaDCIM  : begin
                 // Select by path or drive letter
                 Result := fDcimParams.SetSelectedSource(sName);
               end;
  end;
  fSelectedSourceAPI := API;
end;




{!!
<FS>TIEAcquireParams.SetSourceByStr

<FM>Declaration<FC>
function SetSourceByStr(const sRawDeviceStr : string) : boolean;

<FM>Description<FN>
If you use <A TIEAcquireParams.FillListWithSources> from your own code then the returned devices will be list of strings formatted as follows:

Device Name||Index of Location||API||Device Type

E.g. My Cool Scanner||3||TWN||SCN

You can use SetSourceByStr to activate a source with these raw device strings.

<FM>See Also<FN>
- <A TIEAcquireParams.SetSource>
- <A StrToAcquireSource>

<FM>Example<FC>
procedure TForm1.btnAcquire(Sender: TObject);
begin
  // A listbox has been filled with devices using FillListWithSources
  if ImageEnMView1.MIO.AcquireParams.SetSourceByStr(Listbox1.Items[Listbox1.ItemIndex]) then
    ImageEnMView1.MIO.Acquire;
end;
!!}
function TIEAcquireParams.SetSourceByStr(const sRawDeviceStr : string) : boolean;
var
  ASource : TIEAcquireSource;
begin
  try
    ASource := StrToAcquireSource(sRawDeviceStr);
  except
    // Perhaps a device name has been passed?    
    ASource := FindSourceByName(sRawDeviceStr);
  end;
  Result := SetSource(ASource.Api, ASource.Location);
end;



{!!
<FS>TIEAcquireParams.FillListWithSources

<FM>Declaration<FC>
procedure FillListWithSources(ssDest : TStrings; Apis: <A TIEAcquireApis> = [ieaTwain, ieaWIA, ieaDCIM]; bNameOnly : Boolean = False);

<FM>Description<FN>
Add all acquisition sources to the specified list.  Specify which source types to support using the Apis parameter.

If bNameOnly is true then the list will only be filled with the names of the devices. If bNameOnly is false then the items will be formatted as follows:

Device Name||Index of Location||API||Device Type

E.g. My Cool Scanner||3||TWN||SCN

You can SetSourceByStr to activate a source with these raw device strings.

<FM>See Also<FN>
- <A TIEAcquireParams.SetSource>
- <A StrToAcquireSource>

<FM>Example<FC>
// Fill listbox with all available acquisition sources
FillListWithSources(Listbox1.Items, [ieaTwain, ieaWIA, ieaDCIM], False);

// Acquire from the selected source
if ImageEnMView1.MIO.AcquireParams.SetSourceByStr(Listbox1.Items[Listbox1.ItemIndex]) then
  ImageEnMView1.MIO.Acquire;
!!}
procedure TIEAcquireParams.FillListWithSources(ssDest : TStrings; Apis: TIEAcquireApis = [ieaTwain, ieaWIA, ieaDCIM]; bNameOnly : Boolean = False);
var
  i: Integer;
begin                             
  if Apis = [] then
    Apis := [ieaTwain, ieaWIA, ieaDCIM];
  CheckInitialize(ieaWIA in Apis);

  ssDest.Clear;

  // TWAIN SOURCES
  if ieaTwain in Apis then
  begin
    for i := 0 to fTwainParams.SourceCount - 1 do
    begin
      if bNameOnly then
        ssDest.Add(string(fTwainParams.SourceName[i]))
      else
        ssDest.Add(AcquireSourceToStr(GetTwainSource(i)));
    end;
  end;

  // WIA SOURCES
  if ieaWIA in Apis then
  begin
    for i := 0 to fWIAParams.DevicesInfoCount - 1 do
    begin
      if bNameOnly then
        ssDest.Add(fWIAParams.DevicesInfo[i].Name)
      else
        ssDest.Add(AcquireSourceToStr(GetWiaSource(i)));
    end;
  end;

  // DCIM Sources
  if ieaDCIM in Apis then
  begin
    fDcimParams.GetListOfDcimSources(ssDest, bNameOnly);
  end;

  // Don't sort if we only have a Twain or WIA source so the list aligns with their internal list
  if (Apis <> [ieaTwain]) and (Apis <> [ieaWIA]) then
    if ssDest is TStringList then
      (ssDest as TStringList).Sort();
end;



{!!
<FS>TIEAcquireParams.FindSourceByName

<FM>Declaration<FC>
procedure FindSourceByName(const sName : string) : <A TIEAcquireSource>;

<FM>Description<FN>
Pass the name of a Twain or WIA device and a TIEAcquireSource will be returned if it is valid.

<FM>See Also<FN>
- <A TIEAcquireParams.SetSource>
- <A StrToAcquireSource>

<FM>Example<FC>
ASource.FindSourceByName(edtTwainName.text);
if (ASource.API <> ieaNone) and ImageEnView1.IO.SetAcquireSource(ASource.API, ASource.Location) then
  ImageEnView1.MO.Acquire;

!!}
function TIEAcquireParams.FindSourceByName(const sName : string) : TIEAcquireSource;
var
  i: Integer;
begin
  Result := IEAcquireSource(ieaNone, Default_Device, '', ieadUnknown);

  // Catch passing of Raw Acquire Str
  if IsAcquireSourceStr(sName) then
  begin
    Result := StrToAcquireSource(sName);
    exit;
  end;

  if sName <> '' then
  begin
    // Try Twain Devices
    CheckInitialize(False);
    for i := 0 to fTwainParams.SourceCount - 1 do
      if SameText(sName, string(fTwainParams.SourceName[i])) then
      begin
        Result := GetTwainSource(i);
        exit;
      end;
                   
    // Try WIA Devices
    CheckInitialize(True);
    for i := 0 to fWIAParams.DevicesInfoCount - 1 do
      if SameText(sName, fWIAParams.DevicesInfo[i].Name) then
      begin
        Result := GetWiaSource(i);
        exit;
      end;
               
    // Try DCIM Paths
    if fDcimParams.IsValidDcimLocation(sName) then
      Result := GetDcimSource(sName);
  end;
end;


{!!
<FS>TIEAcquireParams.Create

<FM>Declaration<FC>
constructor Create(Owner: TComponent);

<FM>Example<FC>
AcquireParams := TIEAcquireParams.Create;
AcquireParams.AttachedTwainParams := fTwainParams;
AcquireParams.AttachedWIAParams   := fWIA;
AcquireParams.AttachedDCIMParams  := fDcimParams;
If AcquireParams.SetSelectedSource(ieaTwain, Default_Device) then
  AcquireParams.Acquire;
!!}
constructor TIEAcquireParams.Create(Owner: TComponent);
begin
  inherited Create;
  fOwner := Owner;
  fWIAVisibleDialog := True;
  fSelectedSourceApi := ieaTwain;  // Default to Twain
  fAborting := False;
end;

{!!
<FS>TIEAcquireParams.Destroy

<FM>Declaration<FC>
destructor TIEAcquireParams.Destroy;
!!}
destructor TIEAcquireParams.Destroy;
begin
  //
  inherited;
end;



{!!
<FS>TIEAcquireParams.VisibleDialog

<FM>Declaration<FC>
property VisibleDialog : boolean; (Read/Write)

<FM>Description<FN>
If VisibleDialog is True (default), the camera/scanner user interface is displayed when Acquire is called.

Note: Dialogs are never shown for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>Example<FC>
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams.VisibleDialog>
!!}
procedure TIEAcquireParams.SetVisibleDialog(v: boolean);
begin
  case SelectedSourceApi of
    ieaTwain : fTwainParams.VisibleDialog := v;
    ieaWIA   : fWIAVisibleDialog := v;
    { ieaDCIM : N/A }
  end;
end;


function TIEAcquireParams.GetVisibleDialog: boolean;
begin
  Result := False;
  case SelectedSourceApi of
    ieaTwain : Result := fTwainParams.VisibleDialog;
    ieaWIA   : Result := fWIAVisibleDialog;
    { ieaDCIM : N/A }
  end;
end;



{!!
<FS>TIEAcquireParams.XResolution

<FM>Declaration<FC>
property XResolution : Double; (Read/Write)

<FM>Description<FN>
XResolution is the DPI (Dots per Inch) in the X-axis.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>Example<FC>
// Acquire with 100 DPI (if supported by scanner)
ImageEnView1.IO.AcquireParams.YResolution := 100;
ImageEnView1.IO.AcquireParams.XResolution := 100;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams.XResolution>
- <L WIA item properties>WIA_IPS_XRES</L>
!!}
function TIEAcquireParams.GetXResolution: Double;
begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain : Result := fTwainParams.XResolution.CurrentValue;
    ieaWIA   : Result := fWiaParams.GetItemProperty(WIA_IPS_XRES);
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetXResolution(v : Double);
begin
  case SelectedSourceApi of
    ieaTwain : fTwainParams.XResolution.CurrentValue := v;
    ieaWIA   : fWiaParams.SetItemProperty(WIA_IPS_XRES, Trunc(v));
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.YResolution

<FM>Declaration<FC>
property YResolution: Double; (Read/Write)

<FM>Description<FN>
YResolution is the DPI (Dots per Inch) in the Y-axis.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>Example<FC>
// Acquire with 100 DPI (if supported by scanner)
ImageEnView1.IO.AcquireParams.YResolution := 100;
ImageEnView1.IO.AcquireParams.XResolution := 100;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams.YResolution>
- <L WIA item properties>WIA_IPS_YRES</L>
!!}
function TIEAcquireParams.GetYResolution: Double;
begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.YResolution.CurrentValue;
    ieaWIA    : Result := fWiaParams.GetItemProperty(WIA_IPS_YRES);
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetYResolution(v : Double);
begin
  case SelectedSourceApi of
    ieaTwain : fTwainParams.YResolution.CurrentValue := v;
    ieaWIA   : fWiaParams.SetItemProperty(WIA_IPS_YRES, Trunc(v));
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.Contrast

<FM>Declaration<FC>
property Contrast : Double; (Read/Write)

<FM>Description<FN>
Contrast value.  
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIETwainParams.Contrast>
- <L WIA item properties>WIA_IPS_CONTRAST</L>
!!}
function TIEAcquireParams.GetContrast: Double;
begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.Contrast.CurrentValue;
    ieaWIA    : Result := fWiaParams.GetItemProperty(WIA_IPS_CONTRAST);
    { ieaDCIM : N/A }
  end;
end;


procedure TIEAcquireParams.SetContrast(v : Double);
begin
  case SelectedSourceApi of
    ieaTwain  : fTwainParams.Contrast.CurrentValue := v;   
    ieaWIA    : fWiaParams.SetItemProperty(WIA_IPS_CONTRAST, Trunc(v));
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.Threshold

<FM>Declaration<FC>
property Threshold : Double; (Read/Write)

<FM>Description<FN>
Threshold specifies the dividing line between black and white. Allowed values: 0 - 255.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIETwainParams.Threshold>
- <L WIA item properties>WIA_IPS_THRESHOLD</L>
!!}
function TIEAcquireParams.GetThreshold: Double;
begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.Threshold.CurrentValue;
    ieaWIA    : Result := fWiaParams.GetItemProperty(WIA_IPS_THRESHOLD);
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetThreshold(v : Double);
begin
  case SelectedSourceApi of
    ieaTwain  : fTwainParams.Threshold.CurrentValue := v;
    ieaWIA    : fWiaParams.SetItemProperty(WIA_IPS_THRESHOLD, Trunc(v));
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.Rotation

<FM>Declaration<FC>
property Rotation : <A TIEAcquireRotation>; (Read/Write)

<FM>Description<FN>
Specifies how much the Source can/should rotate the scanned image data prior to transfer. 
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

Allowed values:    
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>iearNone<FN></C> <C>No rotation</C> </R>
<R> <C><FC>iear90CW<FN></C> <C>Rotate 90° clockwise</C> </R>
<R> <C><FC>iear180CW<FN></C> <C>Rotate 180°</C> </R>
<R> <C><FC>iear270CW<FN></C> <C>Rotate 270° clockwise</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIETwainParams.Rotation>
- <L WIA item properties>WIA_IPS_ROTATION</L>
!!}
function TIEAcquireParams.GetRotation : TIEAcquireRotation;
var
  iRotation: Integer;
begin
  result := iearNone;
  // Note: Twain rotation is clockwise, WIA is counter-clockwise
  case SelectedSourceApi of
    ieaTwain  : begin
                  iRotation := Trunc(fTwainParams.Rotation.CurrentValue);
                  if (iRotation = 90) or (iRotation = -270) then
                    Result := iear90CW
                  else
                  if (iRotation = -90) or (iRotation = 270) then
                    Result := iear270CW
                  else
                  if Abs(iRotation) = 180 then
                    Result := iear270CW
                  else
                    Result := iearNone
                end;
    ieaWIA    : case fWiaParams.GetItemProperty(WIA_IPS_ROTATION) of
                  WIA_ROT270           : Result := iear90CW;
                  WIA_ROT180           : Result := iear180CW;
                  WIA_LANDSCAPE        : Result := iear270CW;
                  else { WIA_PORTRAIT }  Result := iearNone;
                end;
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetRotation (v : TIEAcquireRotation);
begin           
  // Note: Twain rotation is clockwise, WIA is counter-clockwise
  case SelectedSourceApi of
    ieaTwain  : case v of
                  iearNone   : fTwainParams.Rotation.CurrentValue := 0;
                  iear90CW   : fTwainParams.Rotation.CurrentValue := 90;
                  iear180CW  : fTwainParams.Rotation.CurrentValue := 180;
                  iear270CW  : fTwainParams.Rotation.CurrentValue := 270;
                end;
    ieaWIA    : case v of
                  iearNone   : fWiaParams.SetItemProperty(WIA_IPS_ROTATION, WIA_PORTRAIT);
                  iear90CW   : fWiaParams.SetItemProperty(WIA_IPS_ROTATION, WIA_ROT270);
                  iear180CW  : fWiaParams.SetItemProperty(WIA_IPS_ROTATION, WIA_ROT180);
                  iear270CW  : fWiaParams.SetItemProperty(WIA_IPS_ROTATION, WIA_LANDSCAPE);
                end;
    { ieaDCIM : N/A }
  end;
end;



{!!
<FS>TIEAcquireParams.PaperDetectable

<FM>Declaration<FC>
property PaperDetectable: boolean; (Read-only)

<FM>Description<FN>
If PaperDetectable is True, the scanner is able to detect paper. 
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIETwainParams.PaperDetectable>
- <L WIA device properties>WIA_DETECT_FEED flag of WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES</L>
!!}
function TIEAcquireParams.GetPaperDetectable: boolean;
begin
  Result := False;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.PaperDetectable;
    ieaWIA    : Result := WIADevicePropertyHasFlag(WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES, WIA_DETECT_FEED);
    { ieaDCIM : N/A } 
  end;
end;


{!!
<FS>TIEAcquireParams.Brightness

<FM>Declaration<FC>
property Brightness : Double; (Read/Write)

<FM>Description<FN>
Brightness value.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIETwainParams.Brightness>
- <L WIA item properties>WIA_IPS_BRIGHTNESS</L>
!!}
function TIEAcquireParams.GetBrightness: Double;
begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.Brightness.CurrentValue;
    ieaWIA    : Result := fWiaParams.GetItemProperty(WIA_IPS_BRIGHTNESS);
    { ieaDCIM : N/A }
  end;
end;


procedure TIEAcquireParams.SetBrightness(v : Double);
begin
  case SelectedSourceApi of
    ieaTwain  : fTwainParams.Brightness.CurrentValue := v;  
    ieaWIA    : fWiaParams.SetItemProperty(WIA_IPS_BRIGHTNESS, Trunc(v));
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.PixelType

<FM>Declaration<FC>
property PixelType: <A TIEAcquirePixelType>; (Read/Write)

<FM>Description<FN>
PixelType is the type of pixel data that a scanner is capable of acquiring.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

Valid values are:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>ieapMonochrome<FN></C> <C>Black & white (1 bit)</C> </R>
<R> <C><FC>ieap8BitGrayScale<FN></C> <C>Gray scale (8 bit)</C> </R>
<R> <C><FC>ieap16BitGrayScale<FN></C> <C>Gray scale (16 bit)</C> </R>
<R> <C><FC>ieapFullColor<FN></C> <C>Full RGB (24 bit)</C> </R>
<R> <C><FC>ieapFullColor16<FN></C> <C>Full RGB (48 bit)</C> </R>
<R> <C><FC>ieapOther<FN></C> <C>Other pixel formats (that are not supported by ImageEn)</C> </R>
</TABLE>

These values can be assigned to PixelType.

<FM>Example<FC>
// Acquire a black/white (1bit) image
ImageEnView1.IO.AcquireParams.PixelType := ieapMonochrome;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

// Acquire a gray scale (8bit) image
ImageEnView1.IO.AcquireParams.PixelType.CurrentValue := ieap8BitGrayScale;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

// Acquire a full RGB (24bit) image
ImageEnView1.IO.AcquireParams.PixelType.CurrentValue := ieapFullColor;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams.PixelType>
- <L WIA item properties>WIA_IPA_BITS_PER_CHANNEL</L>
- <L WIA item properties>WIA_IPA_CHANNELS_PER_PIXEL</L>
!!}
function TIEAcquireParams.GetPixelType : TIEAcquirePixelType;
var
  iBitsPerChannel: Integer;
  iChannelPerPixel: Integer;
begin
  Result := ieapFullColor;
  case SelectedSourceApi of
    ieaTwain  : begin
                  if fTwainParams.PixelType.CurrentValue = Twain_PixelType_BW then
                    Result := ieapMonochrome
                  else
                  if (fTwainParams.PixelType.CurrentValue = Twain_PixelType_Grayscale) and (fTwainParams.BitDepth.CurrentValue = 8) then
                    Result := ieap8BitGrayScale
                  else
                  if (fTwainParams.PixelType.CurrentValue = Twain_PixelType_Grayscale) and (fTwainParams.BitDepth.CurrentValue = 16) then
                    Result := ieap16BitGrayScale
                  else
                  if (fTwainParams.PixelType.CurrentValue = Twain_PixelType_FullRGB) and (fTwainParams.BitDepth.CurrentValue = 8) then
                    Result := ieapFullColor
                  else
                  if (fTwainParams.PixelType.CurrentValue = Twain_PixelType_FullRGB) and (fTwainParams.BitDepth.CurrentValue = 16) then
                    Result := ieapFullColor16
                  else
                    Result := ieapOther;
                end;
    ieaWIA    : begin
                  iBitsPerChannel  := fWiaParams.GetItemProperty(WIA_IPA_BITS_PER_CHANNEL);
                  iChannelPerPixel := fWiaParams.GetItemProperty(WIA_IPA_CHANNELS_PER_PIXEL);
                  if (iBitsPerChannel = 1) and (iChannelPerPixel = 1) then
                    Result := ieapMonochrome
                  else
                  if (iBitsPerChannel = 8) and (iChannelPerPixel = 1) then
                    Result := ieap8BitGrayScale
                  else
                  if (iBitsPerChannel = 16) and (iChannelPerPixel = 1) then
                    Result := ieap16BitGrayScale
                  else
                  if (iBitsPerChannel = 8) and (iChannelPerPixel = 3) then
                    Result := ieapFullColor
                  else
                  if (iBitsPerChannel = 16) and (iChannelPerPixel = 3) then
                    Result := ieapFullColor16
                  else
                    Result := ieapOther;
                end;
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetPixelType(v : TIEAcquirePixelType);
begin
  case SelectedSourceApi of
    ieaTwain  : case v of
                  ieapMonochrome    : begin
                                        fTwainParams.PixelType.CurrentValue := Twain_PixelType_BW;
                                      end;
                  ieap8BitGrayScale : begin
                                        fTwainParams.PixelType.CurrentValue := Twain_PixelType_Grayscale;
                                        fTwainParams.BitDepth.CurrentValue  := 8;
                                      end;
                  ieap16BitGrayScale : begin
                                        fTwainParams.PixelType.CurrentValue := Twain_PixelType_Grayscale;
                                        fTwainParams.BitDepth.CurrentValue  := 16;
                                      end;
                  ieapFullColor     : begin
                                        fTwainParams.PixelType.CurrentValue := Twain_PixelType_FullRGB;
                                        fTwainParams.BitDepth.CurrentValue  := 8;
                                      end;
                  ieapFullColor16   : begin
                                        fTwainParams.PixelType.CurrentValue := Twain_PixelType_FullRGB;
                                        fTwainParams.BitDepth.CurrentValue  := 16;
                                      end;
                  ieapOther         : begin { Ignore } end;
                end;
    ieaWIA    : case v of
                  ieapMonochrome    : begin
                                        fWiaParams.SetItemProperty(WIA_IPA_BITS_PER_CHANNEL, 1);
                                        fWiaParams.SetItemProperty(WIA_IPA_CHANNELS_PER_PIXEL, 1);
                                      end;
                  ieap8BitGrayScale : begin
                                        fWiaParams.SetItemProperty(WIA_IPA_BITS_PER_CHANNEL, 8);
                                        fWiaParams.SetItemProperty(WIA_IPA_CHANNELS_PER_PIXEL, 1);
                                      end;
                  ieap16BitGrayScale : begin
                                        fWiaParams.SetItemProperty(WIA_IPA_BITS_PER_CHANNEL, 16);
                                        fWiaParams.SetItemProperty(WIA_IPA_CHANNELS_PER_PIXEL, 1);
                                      end;
                  ieapFullColor     : begin
                                        fWiaParams.SetItemProperty(WIA_IPA_BITS_PER_CHANNEL, 8);
                                        fWiaParams.SetItemProperty(WIA_IPA_CHANNELS_PER_PIXEL, 3);
                                      end;
                  ieapFullColor16   : begin
                                        fWiaParams.SetItemProperty(WIA_IPA_BITS_PER_CHANNEL, 16);
                                        fWiaParams.SetItemProperty(WIA_IPA_CHANNELS_PER_PIXEL, 3);
                                      end;
                  ieapOther         : begin { Ignore } end;
                end;
    { ieaDCIM : N/A }
  end;
end;


{!!
<FS>TIEAcquireParams.BitDepth

<FM>Declaration<FC>
property BitDepth : Integer; (Read/Write)

<FM>Description<FN>
Specifies the bit depth (bits per channel) of the image to scan.   
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>Example<FC>
// acquire 48 bit (RGB, 16 bit per pixel) native pixels
ImageEnView1.LegacyBitmap := false;
ImageEnView1.IO.NativePixelFormat := true;
ImageEnView1.IO.AcquireParams.BitDepth := 16;
ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams.BitDepth>
- <L WIA item properties>WIA_IPA_DEPTH</L>
!!}
function TIEAcquireParams.GetBitDepth: Integer;    
begin
  Result := 8;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.BitDepth.CurrentValue;
    ieaWIA    : Result := fWiaParams.GetItemProperty(WIA_IPA_DEPTH);
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetBitDepth(v : Integer);
begin
  case SelectedSourceApi of
    ieaTwain  : fTwainParams.BitDepth.CurrentValue := v;
    ieaWIA    : fWiaParams.SetItemProperty(WIA_IPA_DEPTH, v);
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.FeederLoaded

<FM>Declaration<FC>
property FeederLoaded: boolean; (Read-only)

<FM>Description<FN>
Use the FeederLoaded property to determine whether there are documents loaded in the Source's feeder. 
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>Example<FC>
// use of TImageEnIO (instead of TImageEnMIO) to acquire and save multi pages
while ImageEnView1.IO.AcquireParams.FeederLoaded do
begin
  ImageEnView1.IO.Acquire;
  ImageEnView1.IO.SaveToFile('page'+inttostr(count)+'.jpg');
  Inc( count );
end;
    
<FM>See Also<FN>
- <A TIETwainParams.FeederLoaded>
- <L WIA device properties>WIA_FEED_READY of WIA_DPS_DOCUMENT_HANDLING_STATUS</L>
!!}
function TIEAcquireParams.GetFeederLoaded: boolean;
begin
  Result := False;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.FeederLoaded;
    ieaWIA    : Result := WIADevicePropertyHasFlag(WIA_DPS_DOCUMENT_HANDLING_STATUS, WIA_FEED_READY);
    { ieaDCIM : N/A }
  end;
end;



{!!
<FS>TIEAcquireParams.FeederEnabled

<FM>Declaration<FC>
property FeederEnabled: boolean; (Read/Write)

<FM>Description<FN>
FeederEnabled enables the feed loader mechanism when present. Use this property only within <A TImageEnMIO> component to disable the feed loader.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>Example<FC>
// Acquires only one feed
ImageEnMView1.MIO.AcquireParams.FeederEnabled := False;
ImageEnMView1.MIO.Acquire;    

<FM>See Also<FN>
- <A TIETwainParams.FeederEnabled>
- <L WIA device properties>WIA_FEEDER of WIA_DPS_DOCUMENT_HANDLING_SELECT</L>
!!}
function TIEAcquireParams.GetFeederEnabled: boolean;
begin
  Result := False;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.FeederEnabled;
    ieaWIA    : Result := WIADevicePropertyHasFlag(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_FEEDER);
    { ieaDCIM : N/A }
  end;  
end;


procedure TIEAcquireParams.SetFeederEnabled(v: boolean);
begin
  case SelectedSourceApi of
    ieaTwain  : fTwainParams.FeederEnabled := v;
    ieaWIA    : WIADevicePropertySetFlag(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_FEEDER, v);
    { ieaDCIM : N/A }
  end;
end;


{!!
<FS>TIEAcquireParams.AutoFeed

<FM>Declaration<FC>
property AutoFeed : boolean; (Read/Write)

<FM>Description<FN>
If AutoFeed is true, the scanner will automatically feed the next page from the document feeder.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIETwainParams.AutoFeed>
- <L WIA device properties>WIA_AUTO_ADVANCE of WIA_DPS_DOCUMENT_HANDLING_SELECT</L>
!!}
function TIEAcquireParams.GetAutoFeed: boolean;
begin
  Result := False;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.AutoFeed;
    ieaWIA    : Result := WIADevicePropertyHasFlag(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_AUTO_ADVANCE);
    { ieaDCIM : N/A }
  end;
end;



procedure TIEAcquireParams.SetAutoFeed(v: boolean);
begin
  case SelectedSourceApi of
    ieaTwain : fTwainParams.AutoFeed := v;
    ieaWIA   : WIADevicePropertySetFlag(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_AUTO_ADVANCE, v);
    { ieaDCIM : N/A }
  end;
end;



{!!
<FS>TIEAcquireParams.DuplexEnabled

<FM>Declaration<FC>
property DuplexEnabled: boolean; (Read/Write)

<FM>Description<FN>
If DuplexEnabled is True, the scanner scans both sides of a paper; otherwise (default), the scanner will scan only one side.
Use this property only within <A TImageEnMIO> component to enable/disable duplex mode. 
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIETwainParams.DuplexEnabled>
- <L WIA device properties>WIA_DUPLEX of WIA_DPS_DOCUMENT_HANDLING_SELECT</L>
!!}
function TIEAcquireParams.GetDuplexEnabled: boolean;
begin                         
  Result := False;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.DuplexEnabled;
    ieaWIA    : Result := WIADevicePropertyHasFlag(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_DUPLEX);
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetDuplexEnabled(v: boolean);
begin
  fDuplex := v;
  case SelectedSourceApi of
    ieaTwain  : fTwainParams.DuplexEnabled := v;
    ieaWIA    : WIADevicePropertySetFlag(WIA_DPS_DOCUMENT_HANDLING_SELECT, WIA_DUPLEX, v);
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.Orientation

<FM>Declaration<FC>
property Orientation : <A TIEAcquireOrientation>; (Read/Write)

<FM>Description<FN>
Orientation defines the orientation of the output image. Not all scanners support this capability.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

Value is one of:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>ieaoPortrait<FN></C> <C>No rotation (Portrait)</C> </R>
<R> <C><FC>ieaoLandscape<FN></C> <C>Rotate 270° clockwise (Landscape)</C> </R>
<R> <C><FC>ieaoRotate90<FN></C> <C>Rotate 90° clockwise</C> </R>
<R> <C><FC>ieaoRotate180<FN></C> <C>Rotate 180°</C> </R>
</TABLE>


<FM>Example<FC>
// Acquire image in landscape orientation (if supported)
ImageEnView1.IO.AcquireParams.Orientation := ieaoLandscape;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams.Orientation>
- <L WIA item properties>WIA_IPS_ORIENTATION</L>
!!}
function TIEAcquireParams.GetOrientation: TIEAcquireOrientation;
var
  iOrientation: Integer;
begin
  // Note: Twain rotation is clockwise, WIA is counter-clockwise

  Result := ieaoPortrait;
  case SelectedSourceApi of
    ieaTwain  : begin
                  iOrientation := fTwainParams.Orientation.CurrentValue;
                  if iOrientation = Twain_Orientation_Rotate90 then
                    Result := ieaoRotate90
                  else
                  if iOrientation = Twain_Orientation_Rotate180 then
                    Result := ieaoRotate180
                  else
                  if iOrientation = Twain_Orientation_Rotate270 then
                    Result := ieaoLandscape
                  else
                    Result := ieaoPortrait;
                end;
    ieaWIA    : case fWiaParams.GetItemProperty(WIA_IPS_ORIENTATION) of
                  WIA_ROT270          : Result := ieaoRotate90;
                  WIA_ROT180          : Result := ieaoRotate180;
                  WIA_LANDSCAPE       : Result := ieaoLandscape;
                  else { WIA_PORTRAIT } Result := ieaoPortrait;
                end;
    { ieaDCIM : N/A }
  end;
end;


procedure TIEAcquireParams.SetOrientation(v : TIEAcquireOrientation);
begin
  // Note: Twain rotation is clockwise, WIA is counter-clockwise

  case SelectedSourceApi of
    ieaTwain  : case v of
                  ieaoRotate90   : fTwainParams.Orientation.CurrentValue := Twain_Orientation_Rotate90;
                  ieaoRotate180  : fTwainParams.Orientation.CurrentValue := Twain_Orientation_Rotate180;
                  ieaoLandscape  : fTwainParams.Orientation.CurrentValue := Twain_Orientation_Rotate270;
                  ieaoPortrait   : fTwainParams.Orientation.CurrentValue := Twain_Orientation_NoRotate;
                end;
    ieaWIA    : case v of
                  ieaoRotate90   : fWiaParams.SetItemProperty(WIA_IPS_ORIENTATION, WIA_ROT270   );
                  ieaoRotate180  : fWiaParams.SetItemProperty(WIA_IPS_ORIENTATION, WIA_ROT180   );
                  ieaoLandscape  : fWiaParams.SetItemProperty(WIA_IPS_ORIENTATION, WIA_LANDSCAPE);
                  ieaoPortrait   : fWiaParams.SetItemProperty(WIA_IPS_ORIENTATION, WIA_PORTRAIT );
                end;
    { ieaDCIM : N/A }
  end;
end;




{!!
<FS>TIEAcquireParams.AcquireFrameEnabled

<FM>Declaration<FC>
property AcquireFrameEnabled : boolean; (Read/Write)

<FM>Description<FN>
If AcquireFrameEnabled is True, it enables the properties <A TIEAcquireParams.AcquireFrameLeft>, <A TIEAcquireParams.AcquireFrameRight>, <A TIEAcquireParams.AcquireFrameTop> and <A TIEAcquireParams.AcquireFrameBottom>.

Note: This is only required for <L TIEAcquireParams.SelectedSourceApi>API Types</L> of ieaTwain.  For other types AcquireFrameEnabled is always true.   

<FM>See Also<FN>
- <A TIETwainParams.AcquireFrameEnabled>
!!}
function TIEAcquireParams.GetAcquireFrameEnabled: boolean;
begin
  Result := True;
  case SelectedSourceApi of
    ieaTwain  : Result := fTwainParams.AcquireFrameEnabled
    { ieaWIA  : N/A }
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetAcquireFrameEnabled(v: boolean);
begin
  case SelectedSourceApi of
    ieaTwain  : fTwainParams.AcquireFrameEnabled := v;
    { ieaWIA  : N/A }
    { ieaDCIM : N/A }
  end;
end;


{!!
<FS>TIEAcquireParams.AcquireFrameLeft

<FM>Declaration<FC>
property AcquireFrameLeft: double; (Read/Write)

<FM>Description<FN>
AcquireFrameLeft is the left of the rectangle to acquire measured in inches.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIEAcquireParams.AcquireFrameTop>
- <A TIEAcquireParams.AcquireFrameRight>
- <A TIEAcquireParams.AcquireFrameBottom>
- <A TIETwainParams.AcquireFrameLeft>
- <L WIA item properties>WIA_IPS_XPOS</L>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.AcquireParams.AcquireFrameTop    := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameLeft   := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameRight  := 7;
ImageEnView1.IO.AcquireParams.AcquireFrameBottom := 7;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;
!!}
{!!
<FS>TIEAcquireParams.AcquireFrameBottom

<FM>Declaration<FC>
property AcquireFrameBottom: double; (Read/Write)

<FM>Description<FN>
AcquireFrameBottom is the bottom of the rectangle to acquire measured in inches
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM).

<FM>See Also<FN>
- <A TIEAcquireParams.AcquireFrameLeft>
- <A TIEAcquireParams.AcquireFrameTop>
- <A TIEAcquireParams.AcquireFrameRight>
- <A TIETwainParams.AcquireFrameBottom>
- <L WIA item properties>WIA_IPS_YPOS/WIA_IPS_YEXTENT</L>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.AcquireParams.AcquireFrameTop    := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameLeft   := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameRight  := 7;
ImageEnView1.IO.AcquireParams.AcquireFrameBottom := 7;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire
!!}
{!!
<FS>TIEAcquireParams.AcquireFrameRight

<FM>Declaration<FC>
property AcquireFrameRight: double; (Read/Write)

<FM>Description<FN>
AcquireFrameRight is the right side of the rectangle to acquire measured in inches.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIEAcquireParams.AcquireFrameLeft>
- <A TIEAcquireParams.AcquireFrameTop>
- <A TIEAcquireParams.AcquireFrameBottom>
- <A TIETwainParams.AcquireFrameRight>
- <L WIA item properties>WIA_IPS_XPOS/WIA_IPS_XEXTENT</L>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.AcquireParams.AcquireFrameTop    := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameLeft   := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameRight  := 7;
ImageEnView1.IO.AcquireParams.AcquireFrameBottom := 7;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;
!!}
{!!
<FS>TIEAcquireParams.AcquireFrameTop

<FM>Declaration<FC>
property AcquireFrameTop: double; (Read/Write)

<FM>Description<FN>
AcquireFrameTop is the top of the rectangle to acquire measured in inches. 
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM).  With WIA ensure you set AcquireFrameLeft/AcquireFrameTop before AcquireFrameRight/AcquireFrameBottom

<FM>See Also<FN>
- <A TIEAcquireParams.AcquireFrameLeft>
- <A TIEAcquireParams.AcquireFrameRight>
- <A TIEAcquireParams.AcquireFrameBottom>
- <A TIETwainParams.AcquireFrameTop>
- <L WIA item properties>WIA_IPS_YPOS</L>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.AcquireParams.AcquireFrameTop    := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameLeft   := 2;
ImageEnView1.IO.AcquireParams.AcquireFrameRight  := 7;
ImageEnView1.IO.AcquireParams.AcquireFrameBottom := 7;
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;
!!}
function TIEAcquireParams.GetAcquireFrame(idx: integer): double;

  function _GetTwainAcquireFrame(idx: integer): double;
  begin
    result := 0;
    case idx of
      0 : result := fTwainParams.AcquireFrameLeft;
      1 : result := fTwainParams.AcquireFrameTop;
      2 : result := fTwainParams.AcquireFrameRight;
      3 : result := fTwainParams.AcquireFrameBottom;
    end;
  end;

  function _GetWIAAcquireFrame(idx: integer): double;
  begin
    Result := 0;
    case idx of
      0 : if GetXResolution <> 0 then
            result := fWiaParams.GetItemProperty(WIA_IPS_XPOS) / GetXResolution;
      1 : if GetYResolution <> 0 then
            result := fWiaParams.GetItemProperty(WIA_IPS_YPOS) / GetYResolution;
      2 : if GetXResolution <> 0 then
            result := (GetPhysicalWidthPixels - (fWiaParams.GetItemProperty(WIA_IPS_XPOS) + fWiaParams.GetItemProperty(WIA_IPS_XEXTENT))) / GetXResolution;    // May not match fTwainParams.AcquireFrameRight;
      3 : if GetYResolution <> 0 then
            result := (GetPhysicalHeightPixels - (fWiaParams.GetItemProperty(WIA_IPS_YPOS) + fWiaParams.GetItemProperty(WIA_IPS_YEXTENT))) / GetYResolution;   // May not match fTwainParams.AcquireFrameBottom;
    end;
  end;

begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain  : Result := _GetTwainAcquireFrame(idx);
    ieaWIA    : Result := _GetWIAAcquireFrame(idx);
    { ieaDCIM : N/A }
  end;
end;

procedure TIEAcquireParams.SetAcquireFrame(idx: integer; v: double);

  procedure _SetTwainAcquireFrame(idx: integer; v: double);
  begin
    case idx of
      0 : fTwainParams.AcquireFrameLeft   := v;
      1 : fTwainParams.AcquireFrameTop    := v;
      2 : fTwainParams.AcquireFrameRight  := v;
      3 : fTwainParams.AcquireFrameBottom := v;
    end;
  end;

  procedure _SetWIAAcquireFrame(idx: integer; v: double);
  begin
    case idx of
      0 : fWiaParams.SetItemProperty(WIA_IPS_XPOS, Trunc(v * GetXResolution));
      1 : fWiaParams.SetItemProperty(WIA_IPS_YPOS, Trunc(v * GetYResolution));
      2 : fWiaParams.SetItemProperty(WIA_IPS_XEXTENT, Trunc(GetPhysicalWidthPixels - fWiaParams.GetItemProperty(WIA_IPS_XPOS) - (v * GetXResolution)));
      3 : fWiaParams.SetItemProperty(WIA_IPS_YEXTENT, Trunc(GetPhysicalHeightPixels - fWiaParams.GetItemProperty(WIA_IPS_YPOS) - (v * GetYResolution)));
    end;
  end;

begin
  case SelectedSourceApi of
    ieaTwain  : _SetTwainAcquireFrame(Idx, v);
    ieaWIA    : _SetWIAAcquireFrame(Idx, v);
    { ieaDCIM : N/A }
  end;
end;


{!!
<FS>TIEAcquireParams.DuplexSupported

<FM>Declaration<FC>
property DuplexSupported: boolean; (Read-only)

<FM>Description<FN>
If DuplexSupported is True, the scanner can scans both sides of a paper; otherwise the scanner will scan only one side.
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>Example<FC>
// enables duplex if supported
If ImageEnMView1.MIO.AcquireParams.DuplexSupported then
  ImageEnMView1.MIO.AcquireParams.DuplexEnabled := True;
                   
<FM>See Also<FN>
- <A TIETwainParams.DuplexSupported>
- <L WIA device properties>WIA_DETECT_DUP_AVAIL of WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES</L>
!!}
function TIEAcquireParams.GetDuplexSupported: boolean;
begin                  
  Result := False;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.DuplexSupported;
    ieaWIA    : Result := WIADevicePropertyHasFlag(WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES, WIA_DETECT_DUP_AVAIL);
    { ieaDCIM : N/A }
  end;
end;

{!!
<FS>TIEAcquireParams.PhysicalHeight

<FM>Declaration<FC>
property PhysicalHeight: double; (Read-only)

<FM>Description<FN>
PhysicalHeight is the maximum physical height (Y-axis) the scanner can acquire (measured in inches). 
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)


<FM>See Also<FN>
- <A TIETwainParams.PhysicalHeight>
- <L WIA device properties>WIA_DPS_VERTICAL_BED_SIZE</L>
!!}
function TIEAcquireParams.GetPhysicalHeight : double;
begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.PhysicalHeight;
    ieaWIA    : Result := fWiaParams.GetDeviceProperty(WIA_DPS_VERTICAL_BED_SIZE) / 1000;
    { ieaDCIM : N/A }
  end;
end;

function TIEAcquireParams.GetPhysicalHeightPixels : Integer;
begin
  Result := Trunc(GetPhysicalHeight * GetYResolution);
end;

{!!
<FS>TIEAcquireParams.PhysicalWidth

<FM>Declaration<FC>
property PhysicalWidth: double; (Read-only)

<FM>Description<FN>
PhysicalWidth is the maximum physical width (X-axis) the scanner can acquire (measured in inches).
Note: Not supported for DCIM retrieval (i.e. when <A TImageEnIO.SelectedAcquireSource>.Api = ieaDCIM)

<FM>See Also<FN>
- <A TIETwainParams.PhysicalWidth>
- <L WIA device properties>WIA_DPS_HORIZONTAL_BED_SIZE</L>
!!}
function TIEAcquireParams.GetPhysicalWidth: double;
begin
  Result := 0;
  case SelectedSourceApi of
    ieaTwain  : result := fTwainParams.PhysicalWidth;
    ieaWIA    : Result := fWiaParams.GetDeviceProperty(WIA_DPS_HORIZONTAL_BED_SIZE) / 1000;
    { ieaDCIM : N/A }
  end;
end;   

function TIEAcquireParams.GetPhysicalWidthPixels : Integer;
begin
  Result := Trunc(GetPhysicalWidth * GetXResolution);
end;



function TIEAcquireParams.WIADevicePropertyHasFlag(PropId: DWord; Flag : Integer) : Boolean;
var
  iFlags: Integer;
begin
  iFlags := fWiaParams.GetDeviceProperty(PropId);
  Result := (iFlags AND Flag) = Flag;
end;

procedure TIEAcquireParams.WIADevicePropertySetFlag(PropId: DWord; Flag : Integer; bSet : Boolean);
var
  iFlags: Integer;
begin
  iFlags := fWiaParams.GetDeviceProperty(PropId);
  if bSet then
  begin
    iFlags := iFlags OR Flag;
    If Flag = WIA_FEEDER then
      iFlags := iFlags AND NOT WIA_FLATBED;
  end
  else
  begin
    iFlags := iFlags AND NOT Flag;
    If Flag = WIA_FEEDER then
      iFlags := iFlags OR WIA_FLATBED;
   end;
  fWiaParams.SetDeviceProperty(PropId, iFlags);
end;



function IEAcquireSource(Api : TIEAcquireApi; Location : Variant; const sName : AnsiString; DeviceType : TIEAcquireDeviceType) : TIEAcquireSource;
begin                            
  Result.Api        := Api;  
  Result.Location   := Location;
  Result.Name       := sName;     
  Result.DeviceType := DeviceType;
end;

const
  API_TYPE_TWAIN    = 'TWN';
  API_TYPE_WIA      = 'WIA';
  API_TYPE_DCIM     = 'DCM';
  API_TYPE_UNKNOWN  = '---';
  API_TYPE_LEN      = 3;

  DEVICE_TYPE_SCANNER  = 'SCN';
  DEVICE_TYPE_CAMERA   = 'CAM';
  DEVICE_TYPE_DRIVE    = 'DRV';
  DEVICE_TYPE_UNKNOWN  = '---';
  DEVICE_TYPE_LEN      = 3;

  Raw_String_Delimiter     = '||';
  Raw_String_Delimiter_Len = 2;

{!!
<FS>AcquireSourceToStr

<FM>Declaration<FC>
function AcquireSourceToStr(AcquireSource : <A TIEAcquireSource>) : string;

<FM>Description<FN>
Convert a <A TIEAcquireSource> to a string formatted as follows:

Device Name||Index of Location||API||Device Type

E.g. My Cool Scanner||3||TWN||SCN


<FM>Example<FC>
// Save the current acquisition source to the registry
var
  sDevice : string;
begin
  ..
  // Read the selected device
  sDevice := AcquireSourceToStr(ImageEnMView1.MIO.SelectedAcquireSource);
  WRegistry.WriteString('SelectedAcquireSource', sDevice);
  ..
end;

!!}
function AcquireSourceToStr(AcquireSource : TIEAcquireSource) : string;
var
  sAPI: string;
  sDevice: string;
  sLocation: string;
begin
  case AcquireSource.Api of
    ieaTwain  : sAPI := API_TYPE_TWAIN;
    ieaWIA    : sAPI := API_TYPE_WIA;
    ieaDCIM   : sAPI := API_TYPE_DCIM;
    else        sAPI := API_TYPE_UNKNOWN;
  end;

  case AcquireSource.DeviceType of
    ieadScanner  : sDevice := DEVICE_TYPE_SCANNER;
    ieadCamera   : sDevice := DEVICE_TYPE_CAMERA;
    ieadDrive    : sDevice := DEVICE_TYPE_DRIVE;
    else           sDevice := DEVICE_TYPE_UNKNOWN;
  end;

  sLocation := '';
  if varType(AcquireSource.Location) = varString then
    sLocation := AcquireSource.Location
  else
  if varType(AcquireSource.Location) = varInteger then
    sLocation := IntToStr(AcquireSource.Location);

  // Format example: My Cool Scanner||3||TWN||SCN
  Result := string(AcquireSource.Name) + Raw_String_Delimiter +
            sLocation + Raw_String_Delimiter +
            sAPI + Raw_String_Delimiter +
            sDevice;
end;


{!!
<FS>StrToAcquireSource

<FM>Declaration<FC>
function StrToAcquireSource(const sRawDeviceStr : string) : <A TIEAcquireSource>;

<FM>Description<FN>
If you use <A TIEAcquireParams.FillListWithSources> from your own code then the returned devices will be list of strings formatted as follows:

Device Name||Index of Location||API||Device Type

E.g. My Cool Scanner||3||TWN||SCN

You can use StrToAcquireSource to convert the raw string to a <A TIEAcquireSource> record.
!!}
function StrToAcquireSource(const sRawDeviceStr : string) : TIEAcquireSource;
var
  sAPI: string;
  sDevice: string;
  sRem: string;
  iPos: Integer;
  sName: string;
  sLocation: string;
  ADevice: TIEAcquireDeviceType;
begin
  Result := IEAcquireSource(ieaNone, Default_Device, '', ieadUnknown);
  if sRawDeviceStr <> '' then
  try
    // Format example: My Cool Scanner||3||TWN||SCN
    sDevice := Copy(sRawDeviceStr, Length(sRawDeviceStr) - API_TYPE_LEN + 1, API_TYPE_LEN);
    sAPI    := Copy(sRawDeviceStr, Length(sRawDeviceStr) - API_TYPE_LEN - Raw_String_Delimiter_Len - DEVICE_TYPE_LEN + 1, DEVICE_TYPE_LEN);
    sRem    := Copy(sRawDeviceStr, 1, Length(sRawDeviceStr) - API_TYPE_LEN - Raw_String_Delimiter_Len - DEVICE_TYPE_LEN - Raw_String_Delimiter_Len);
    iPos    := Pos(Raw_String_Delimiter, sRem);
    if iPos = 0 then
      Raise Exception.create('Invalid format');
    sName   := Copy(sRem, 1, iPos - 1);

    inc(iPos, Raw_String_Delimiter_Len);
    sLocation := Copy(sRem, iPos, Length(sRem) - iPos + 1);

    if SameText(sDevice, DEVICE_TYPE_SCANNER) then
      ADevice := ieadScanner
    else
    if SameText(sDevice, DEVICE_TYPE_CAMERA) then
      ADevice := ieadCamera
    else
    if SameText(sDevice, DEVICE_TYPE_DRIVE) then
      ADevice := ieadDrive
    else
      ADevice := ieadUnknown;

    if SameText(sAPI, API_TYPE_TWAIN) then
      Result := IEAcquireSource(ieaTwain, StrToInt(sLocation), AnsiString(sName), ADevice)
    else
    if SameText(sAPI, API_TYPE_WIA) then
      Result := IEAcquireSource(ieaWIA, StrToInt(sLocation), AnsiString(sName), ADevice)
    else
    if SameText(sAPI, API_TYPE_DCIM) then
      Result := IEAcquireSource(ieaDCIM, sLocation, AnsiString(sName), ADevice);
  except
    // MALFORMATTED STR
    Raise Exception.create('Invalid Raw Device String');
  end
end;


function IsAcquireSourceStr(const value : string) : Boolean;
var
  iPosLastDL: Integer;
  iPosMidDL: Integer;
begin
  iPosLastDL := Length(value) - DEVICE_TYPE_LEN - Raw_String_Delimiter_Len + 1;
  iPosMidDL  := iPosLastDL - API_TYPE_LEN  - Raw_String_Delimiter_Len;

  // Check it contains the mid and end delimiters
  Result := (Length(value) > 1 + Raw_String_Delimiter_Len + API_TYPE_LEN + Raw_String_Delimiter_Len + DEVICE_TYPE_LEN) AND
            SameText(Copy(Value, iPosLastDL, Raw_String_Delimiter_Len), Raw_String_Delimiter) AND
            SameText(Copy(Value, iPosMidDL, Raw_String_Delimiter_Len), Raw_String_Delimiter);
end;


{!!
<FS>DrawAcquireComboListBoxItem

<FM>Declaration<FC>
procedure DrawAcquireComboListBoxItem(TheControl : TWinControl;
                                      CanvasRect : TRect;
                                      const sRawDeviceStr : string;
                                      AnImageList : TImageList = nil;
                                      iScannerGlyph : Short = -1;
                                      iCameraGlyph : Short = -1;
                                      iDriveGlyph : Short = -1;
                                      iUnknownGlyph : Short = -1);

<FM>Description<FN>
If you use <A TIEAcquireParams.FillListWithSources> to fill a list or combobox you can use this method to custom draw the item from the controls OnDrawItem event.

TheControl, CanvasRect are passed from the event parameters.
sRawDeviceStr will be TheControl.Items[Index].
If you have a TImageList containing glyphs then pass it as AnImageList and the index of each relevant glyph as iScannerGlyph, iCameraGlyph, iDriveGlyph and iUnknownGlyph.

<FM>See Also<FN>
- <A TIEAcquireParams.FillListWithSources>

<FM>Example<FC>
procedure TMyForm.lbxSourcesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  Scanner_Glyph_Index = 0;
  Camera_Glyph_Index  = 1;
  Drive_Glyph_Index   = 2;
  Unknown_Glyph_Index = 3;
begin
  // lbxSources Style has been set to lbOwnerDrawFixed
  DrawAcquireComboListBoxItem(Control, Rect, lbxSources.Items[Index], imlDevices,
                              Scanner_Glyph_Index, Camera_Glyph_Index, Drive_Glyph_Index, Unknown_Glyph_Index);

end;
!!}
// Custom draw the items of a combo or listbox (call from DrawItem event)
procedure DrawAcquireComboListBoxItem(TheControl : TWinControl;
                                      CanvasRect : TRect;
                                      const sRawDeviceStr : string;
                                      AnImageList : TImageList = nil;
                                      iScannerGlyph : Short = -1;
                                      iCameraGlyph : Short = -1;
                                      iDriveGlyph : Short = -1;
                                      iUnknownGlyph : Short = -1);
var
  iGlyph: Integer;
  ASource : TIEAcquireSource;
begin
  try
  ASource := StrToAcquireSource(sRawDeviceStr);
  except
    // Invalid string
    ASource := IEAcquireSource(ieaNone, Default_Device, '', ieadUnknown);
  end;
  iGlyph := -1;
  if ASource.API <> ieaNone then
    case ASource.DeviceType of
      ieadScanner  : iGlyph := iScannerGlyph;
      ieadCamera   : iGlyph := iCameraGlyph;
      ieadDrive    : iGlyph := iDriveGlyph;
      else           iGlyph := iUnknownGlyph;
    end;
  IEDrawComboListBoxItem(TheControl, CanvasRect, string(ASource.Name), AnImageList, iGlyph);
end;


function TIEAcquireParams.GetSelectedSourceApi: TIEAcquireApi;
begin
  Result := fSelectedSourceApi;
  if Result <> ieaNone then
    CheckInitialize(Result = ieaWIA);
end;


{$else}   // IEINCLUDEIEXACQUIRE

interface

implementation

{$endif}  // IEINCLUDEIEXACQUIRE



end.





