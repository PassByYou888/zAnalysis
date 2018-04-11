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
File version 1037
*)

unit iemio;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEMULTIVIEW}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Printers,
  {$ifdef IEHASUITYPES} System.UITypes, {$endif}
  ImageEnView, ImageEnProc, ExtCtrls, hyiedefs, ImageEnIO, ieview,
  {$IFDEF IEINCLUDEIEXACQUIRE}
  ietwain, iexAcquire, iexDCIM, iewia,
  {$ENDIF}
  hyieutils, pcxfilter;


const
  IEM_SELECTED_IMAGES     = -9; // Quick reference to all selected images in the TImageEnMView
  IEM_ALL_IMAGES          = -7; // Quick reference to all images in the TImageEnMView


type

{!!
<FS>TImageEnMIO

<FM>Description<FN>
TImageEnMIO loads/saves and acquires multi-images. It must be attached to <A TImageEnMView>.
TImageEnMIO can load animated GIF, multi-image TIFF, AVI and acquire multi-page from Twain scanners.
It can also save animated GIF, multi-images TIFF and AVI.

Note: Users often attach a <A TImageEnIO> component to <A TImageEnMView> component. This is not correct. The TImageEnMIO component must be attached only to a <A TImageEnMView> component.

<FM>Methods and Properties<FN>
<FI>Generic Input/Output<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.Aborting></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.AllowMalformedPages></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.AutoAdjustDPI></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.AttachedMView></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.DefaultDitherMethod></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.DuplicateCompressionInfo></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.ExecuteOpenDialog></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.ExecuteSaveDialog></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.FilteredAdjustDPI></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromBuffer></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileAuto></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFiles></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromStreamFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromURL></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.NativePixelFormat></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.Params></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.ParamsCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.ProxyAddress></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.ProxyPassword></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.ProxyUser></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.Update></C> </R>
</TABLE>

<FI>Dialogs<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.DialogsMeasureUnit></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.DoPreviews></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.PreviewFont></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.PreviewFontEnabled></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.PreviewsParams></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMIO.SimplifiedParamsDialogs></C> </R>
</TABLE>

<FI>Image Acquisition (Twain/WIA)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.Acquire></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SelectAcquireSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.SelectedAcquireSource></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SetAcquireSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.AcquireParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.DCIMParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.TwainParams></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.TwainAcquireOpen></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.TwainAcquireClose></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.WIAParams></C> </R>
</TABLE>

<FI>Printing<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.DoPrintPreviewDialog></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.PrintImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.PrintImagePos></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.PrintImages></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.PrintImagesToFile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.PrintingFilterOnSubsampling></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMIO.PrintPreviewParams></C> </R>
</TABLE>

<FI>AVI Videos<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileAVI></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFileAVI></C> </R>
</TABLE>

<FI>DCX (Multipage PCX)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileDCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromStreamDCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFileDCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToStreamDCX></C> </R>
</TABLE>

<FI>GIF<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromStreamGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFileGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToStreamGIF></C> </R>
</TABLE>

<FI>Icons (ICO)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileICO></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromStreamICO></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFileICO></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToStreamICO></C> </R>
</TABLE>

<FI>TIFF<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromStreamTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFileTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToStreamTIFF></C> </R>
</TABLE>

<FI>DirectShow Media Files<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromMediaFile></C> </R>
</TABLE>

<FI>Adobe PDF<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFilePDF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToStreamPDF></C> </R>
</TABLE>

<FI>PostScript (PS)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFilePS></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToStreamPS></C> </R>
</TABLE>

<FI>DICOM Medical Imaging Format<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromFileDICOM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.LoadFromStreamDICOM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToFileDICOM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMIO.SaveToStreamDICOM></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TImageEnMIO.OnAcquireBitmap></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMIO.OnAcquireClose></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMIO.OnAfterAcquireBitmap></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMIO.OnDoPreviews></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMIO.OnFinishWork></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMIO.OnProgress></C> </R>
</TABLE>
!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TImageEnMIO = class(TComponent)
  private
    { Private declarations }
    fImageEnMView: TIEView;
    fImageEnMViewBitmapChangeHandle: pointer; // bitmap change handler (nil=none)
    fParams: TList; // list of TIOParamasVals
    fSimplifiedParamsDialogs: boolean;
    {$IFDEF IEINCLUDEIEXACQUIRE}
    fAcquireParams : TIEAcquireParams;
    fTwainParams: TIETwainParams;
    fDCIMParams : TIEDcimAcquire;
    fWIA: TIEWia;
    {$ENDIF}
    fPreviewsParams: TIOPreviewsParams;
    fPreviewFont: TFont;
    fPreviewFontEnabled: Boolean;
    fTwainNextToInsert: integer; // index of next image to insert (twain multipage acq.)
    fAutoAdjustDPI: boolean;
    fFilteredAdjustDPI: boolean;
    fDefaultDitherMethod: TIEDitherMethod;
    fResetPrinter: boolean;
    fDialogsMeasureUnit: TIEDialogsMeasureUnit;
    fNativePixelFormat: boolean;
    fPrintingFilterOnSubsampling: TResampleFilter;
    fOnDoPreviews: TIEDoPreviewsEvent;
    fLoadingFileName: string;  // used when a LoadFromFile calls LoadFromStream to fill Params.FileName
    fLastFilename : string; // the last file we loaded using LoadFromFile
    fAllowMalformedPages: boolean;
    // Twain modeless
    fgrec: pointer;
    // proxy settings
    fProxyAddress, fProxyUser, fProxyPassword: string;
    {$IFDEF IEINCLUDEPRINTDIALOGS}
    fPrintPreviewParams: TIOPrintPreviewParams;
    {$ENDIF}
    //
    procedure SetAttachedMView(v: TIEView);
    procedure RemoveIOParam(idx: integer);
    procedure InsertIOParam(idx: integer);
    procedure MoveIOParams(idx: integer; destination: integer);
    procedure SwapIOParams(idx1, idx2: integer);
    function GetParams(idx: integer): TIOParamsVals;
    procedure SetPreviewFont(f: TFont);
    procedure SetPreviewFontEnabled(Value: Boolean);
    function GetParamsCount: integer;
    procedure SetIOPreviewParams(v: TIOPreviewsParams);
    function GetIOPreviewParams: TIOPreviewsParams;
    procedure PrintImagesEx(PrtCanvas: TCanvas; dpix, dpiy: integer; pagewidth, pageheight: double; bPreview: Boolean; Columns: integer; Rows: integer; HorizSpace: double; VertSpace: double; PrintSelected: boolean; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; DrawBox: boolean; DrawText: boolean; DrawShadow: boolean; BoxColor: TColor = clBlack; iPageNo : Integer = 0);
    function GetImageEnVersion: string;
    procedure SetImageEnVersion(Value: string);
    procedure SetAborting(Value: Boolean);
  protected
    { Protected declarations }
    fAborting: boolean;
    fOnProgress: TIEProgressEvent;
    fOnAcquireBitmap: TIEAcquireBitmapEvent;
    fOnAcquireClose: TNotifyEvent;
    fOnAfterAcquireBitmap: TIEAfterAcquireBitmapEvent;
    fOnFinishWork: TNotifyEvent;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnBitmapChange(Sender: TObject; destroying: boolean);
    procedure DoFinishWork; virtual;
    procedure CheckDPI(p: TIOParamsVals);
    {$ifdef IEINCLUDEIEXACQUIRE}
    procedure TWMultiCallBack(Bitmap: TIEBitmap; var IOParams: TObject; ImDpiX, ImDpiY: integer); virtual;
    procedure TWCloseCallBack; virtual;
    function GetWIAParams: TIEWia; virtual;
    function WiaOnProgress(Percentage: integer): boolean;
    function GetSelectedAcquireSource : TIEAcquireSource;
    {$endif}
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Update;

{!!
<FS>TImageEnMIO.DefaultDitherMethod

<FM>Declaration<FC>
property DefaultDitherMethod: <A TIEDitherMethod>;

<FM>Description<FN>
DefaultDitherMethod specifies the default dithering method to apply when a color image needs to be converted to black/white.
ieThreshold is the default.
!!}
    property DefaultDitherMethod: TIEDitherMethod read fDefaultDitherMethod write fDefaultDitherMethod;

    // Other
    property Params[idx: integer]: TIOParamsVals read GetParams;
    property ParamsCount: integer read GetParamsCount;
    procedure DuplicateCompressionInfo;

    // The last file that was loaded using LoadFromFile, etc.
    property LastFilename : string read fLastFilename;

{!!
<FS>TImageEnMIO.AllowMalformedPages

<FM>Declaration<FC>
property AllowMalformedPages: boolean;

<FM>Description<FN>
If true malformed pages (for example loading a corrupted or out of standard TIFF), if possible, will be loaded anyway.
Default is False (stop loading).
!!}
    property AllowMalformedPages: boolean read fAllowMalformedPages write fAllowMalformedPages;

    {$IFDEF IEINCLUDEDIALOGIO}
    function DoPreviews(idx: integer; pp: TPreviewParams): boolean;
    {$ENDIF}

    {$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnMIO.AcquireParams

<FM>Declaration<FC>
property AcquireParams : <A TIEAcquireParams>;

<FM>Description<FN>
AcquireParams is a powerful interface that provides generic access to all image acquistion APIs, Twain, WIA and DCIM Retrieval. AcquireParams accesses <A TImageEnMIO.TwainParams>, <A TImageEnMIO.WIAParams> and <A TImageEnMIO.DCIMParams>.

If you have called <A TImageEnMIO.SelectAcquireSource> or <A TImageEnMIO.Acquire> then you can access common parameters for the device without having to know the <L TIEAcquireApi>API</L>

See the <FC>TIEAcquireParams<FN> object for more details.
!!}
    property AcquireParams: TIEAcquireParams read fAcquireParams;

{!!
<FS>TImageEnMIO.TwainParams

<FM>Declaration<FC>
property TwainParams: <A TIETwainParams>;

<FM>Description<FN>
Use the TwainParams property to control acquisition from Twain scanners and cameras. You can enable/disable the standard user interface, set pixeltype (Grayscale, RGB...), DPI, etc.

Note: Use TwainParams only when you need access to Twain specific parameters and functionality (when <A TImageEnMIO.SelectedAcquireSource>.Api is ieaTwain). For generic access to all image acquisitions sources (Twain, WIA, etc.) use <A TImageEnMIO.AcquireParams> instead.
                    
<FM>Example<FC>
// Acquire a black/white (1bit) image from the default Twain device
ImageEnMView1.MIO.TwainParams.PixelType.CurrentValue := 0;
If ImageEnMView1.MIO.SetSource(ieaTwain, Default_Device) then
ImageEnMView1.MIO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams>
- <A TImageEnMIO.AcquireParams>
- <A TImageEnMIO.SelectedAcquireSource>
!!}
    property TwainParams: TIETwainParams read fTwainParams;


{!!
<FS>TImageEnMIO.DCIMParams

<FM>Declaration<FC>
property DCIMParams: <A TIEDcimAcquire>;

<FM>Description<FN>
Use the DCIMParams property to handle retrieval of images from camera cards and connected camera devices (which appear as USB drives).

Note: Use DCIMParams only when you need access to DCIM specific functionality (when <A TImageEnMIO.SelectedAcquireSource>.Api is ieaDCIM). For generic access to all image acquisitions sources (Twain, WIA, etc.) use <A TImageEnMIO.AcquireParams> instead.
                    
<FM>See Also<FN>
- <A TIEDcimAcquire>
- <A TImageEnMIO.AcquireParams>
- <A TImageEnMIO.SelectedAcquireSource>
!!}
    property DCIMParams: TIEDcimAcquire read fDCIMParams;

    property WIAParams: TIEWia read GetWIAParams;

    {$ENDIF}


{!!
<FS>TImageEnMIO.Aborting

<FM>Declaration<FC>
property Aborting: boolean;

<FM>Description<FN>
Applications can abort save/load processing by assigning True to the Aborting property. On loading, the image will be truncated. On saving, the file will be closed and truncated (and will be unreadable).
You can also read the <A TImageEnMIO.Aborting> property to know when aborting is in progress.

<FM>Example<FC>
MyImageEnMView.LoadFromFileGIF('C:\MyGif.gif');
MyImageEnMView.Aborting then;

Which is the same as:
MyImageEnMView.LoadFromFileGIF('C:\MyGif.gif');
!!}
    property Aborting: boolean read fAborting write SetAborting;

    // GIF
    function LoadFromFileGIF(const FileName: string): Boolean;
    function LoadFromStreamGIF(Stream: TStream): Boolean;
    procedure SaveToFileGIF(const FileName: string; SelectedOnly: Boolean = False);
    procedure SaveToStreamGIF(Stream: TStream; SelectedOnly: Boolean = False);
    // DCX
    function LoadFromFileDCX(const FileName: string): Boolean;
    function LoadFromStreamDCX(Stream: TStream): Boolean;
    procedure SaveToFileDCX(const FileName: string; SelectedOnly: Boolean = False);
    procedure SaveToStreamDCX(Stream: TStream; SelectedOnly: Boolean = False);
    // DICOM
    {$ifdef IEINCLUDEDICOM}
    function LoadFromFileDICOM(const FileName: string): Boolean;
    function LoadFromStreamDICOM(Stream: TStream): Boolean;
    procedure SaveToFileDICOM(const FileName: string; SelectedOnly: Boolean = False);
    procedure SaveToStreamDICOM(Stream: TStream; SelectedOnly: Boolean = False);
    {$endif}
    // TIFF
    function LoadFromFileTIFF(const FileName: string): Boolean;
    function LoadFromStreamTIFF(Stream: TStream): Boolean;
    procedure SaveToFileTIFF(const FileName: string; SelectedOnly: Boolean = False);
    procedure SaveToStreamTIFF(Stream: TStream; SelectedOnly: Boolean = False);
    // AVI
    function LoadFromFileAVI(const FileName: string): Boolean;
    procedure SaveToFileAVI(const FileName: string; const Codec: AnsiString = ''; SelectedOnly: Boolean = False);
    // PostScript (PS)
    procedure SaveToFilePS(const FileName: string; SelectedOnly: Boolean = False);
    procedure SaveToStreamPS(Stream: TStream; SelectedOnly: Boolean = False);
    // Adobe PDF
    {$ifdef IEINCLUDEPDFWRITING}
    procedure SaveToFilePDF(const FileName: string; SelectedOnly: Boolean = False);
    procedure SaveToStreamPDF(Stream: TStream; SelectedOnly: Boolean = False);
    {$endif}
    // ICO
    function LoadFromFileICO(const FileName: string): Boolean;
    function LoadFromStreamICO(Stream: TStream): Boolean;
    procedure SaveToFileICO(const FileName: string; SelectedOnly: Boolean = False);
    procedure SaveToStreamICO(Stream: TStream; SelectedOnly: Boolean = False);
    // URL
    function LoadFromURL(URL: string): Boolean;
    // DirectShow media
    {$ifdef IEINCLUDEDIRECTSHOW}
    function LoadFromMediaFile(const FileName: string): Boolean;
    {$endif}
    // TWAIN
    {$IFDEF IEINCLUDEIEXACQUIRE}
    function Acquire : boolean;
    function SelectAcquireSource(Apis : TIEAcquireApis = [ieaTwain, ieaWIA, ieaDCIM]): boolean;
    function SetAcquireSource(Api: TIEAcquireApi; Location : Variant) : boolean;
    property SelectedAcquireSource : TIEAcquireSource read GetSelectedAcquireSource;

    function TwainAcquireOpen: boolean;
    procedure TwainAcquireClose;
    function AcquireOpen: boolean;
    procedure AcquireClose;
    // Internal use only
    procedure InitializeAcquireSource(bIncludeWIA : Boolean);
    {$ENDIF}
    // General
    procedure LoadFromBuffer(Buffer: pointer; BufferSize: integer; Format: TIOFileType = ioUnknown);
    function LoadFromFile(const FileName: string; bCheckUnknown: Boolean = False): Boolean;
    procedure LoadFromFiles(const FileName: string; AutoDetect: boolean=false; LoadWhenViewed: boolean=false);
    procedure SaveToFile(const FileName: string; SelectedOnly: Boolean = False);
    function LoadFromFileFormat(const FileName: string; FileFormat: TIOFileType): Boolean;
    function LoadFromFileAuto(const FileName: string): Boolean;
    function LoadFromStream(Stream: TStream): Boolean;
    function LoadFromStreamFormat(Stream: TStream; FileFormat: TIOFileType): Boolean;
{$IFDEF IEINCLUDEOPENSAVEDIALOGS}
    function ExecuteOpenDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                               FilterIndex: integer = 0; const ExtendedFilters : WideString = ''; MultiSelect : boolean = False;
                               const Title : WideString = ''; const Filter : WideString = ''; DefaultFilter : TIOFileType = -1;
                               LimitToFileType : TIOFileType = -1) : String; overload;
    function ExecuteOpenDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False; MultiSelect : boolean = False) : String; overload;
    function ExecuteSaveDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                               FilterIndex: integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                               const Filter : WideString = ''; DefaultFilter : TIOFileType = -1; LimitToFileType : TIOFileType = -1) : String; overload;
    function ExecuteSaveDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False) : String; overload;
{$ENDIF}
{$IFDEF IEINCLUDEPRINTDIALOGS}
    function DoPrintPreviewDialog(const TaskName: string = ''; PrintAnnotations: boolean=false; const Caption: string=''; ThumbnailPrinting: Boolean = False): boolean;
{$ENDIF}
    property ResetPrinter: boolean read fResetPrinter write fResetPrinter;

{!!
<FS>TImageEnMIO.PrintPreviewParams

<FM>Declaration<FC>
property PrintPreviewParams: <A TIOPrintPreviewParams>;

<FM>Description<FN>
This property allows you to set/get parameters of print preview dialog.

All measure units are specified by <A TImageEnMIO.DialogsMeasureUnit> property.
!!}
{$IFDEF IEINCLUDEPRINTDIALOGS}
    property PrintPreviewParams: TIOPrintPreviewParams read fPrintPreviewParams;
{$ENDIF}


{!!
<FS>TImageEnMIO.NativePixelFormat

<FM>Declaration<FC>
property NativePixelFormat: boolean;

<FM>Description<FN>
By setting this property to True, you disable the conversion of paletted images and gray scale to 24 bit.
By default, ImageEn converts all paletted images to 24 bit (true color). Only black/white images are stored in the original format with 1 bit per pixel.

Note that setting NativePixelFormat=True, you will not be able to execute some image processing operations.
!!}
    property NativePixelFormat: boolean read fNativePixelFormat write fNativePixelFormat;

{!!
<FS>TImageEnMIO.PrintingFilterOnSubsampling

<FM>Declaration<FC>
property PrintingFilterOnSubsampling: <A TResampleFilter>;

<FM>Description<FN>
Specifies a filter when the image needs to be printed and it must be resampled. Filtering enhances the image quality but slows processing.
!!}
    property PrintingFilterOnSubsampling: TResampleFilter read fPrintingFilterOnSubsampling write fPrintingFilterOnSubsampling;

{!!
<FS>TImageEnMIO.ProxyAddress

<FM>Declaration<FC>
property ProxyAddress: string;

<FM>Description<FN>
ProxyAddress specifies the proxy address and port when using <A TImageEnMIO.LoadFromURL> method. The syntax is: 'domain:port'.

<FM>Example<FC>
ImageEnMView.MIO.ProxyAddress := '10.2.7.2:8080';
ImageEnMView.MIO.LoadFromURL('http://www.imageen.com/image.gif');
!!}
    property ProxyAddress: string read fProxyAddress write fProxyAddress;

{!!
<FS>TImageEnMIO.ProxyUser

<FM>Declaration<FC>
property ProxyUser: string;

<FM>Description<FN>
ProxyUser specifies the proxy userid when using <A TImageEnMIO.LoadFromURL> method.

<FM>Example<FC>
ImageEnMView.MIO.ProxyAddress := '10.2.7.2:8080';
ImageEnMView.MIO.ProxyUser := 'testuser';
ImageEnMView.MIO.ProxyPassword := 'testpassword';
ImageEnMView.MIO.LoadFromURL('http://www.imageen.com/image.gif');
!!}
    property ProxyUser: string read fProxyUser write fProxyUser;

{!!
<FS>TImageEnMIO.ProxyPassword

<FM>Declaration<FC>
property ProxyPassword: string;

<FM>Description<FN>
ProxyPassword specifies the proxy password when using <A TImageEnMIO.LoadFromURL> method.

<FM>Example<FC>
ImageEnMView.MIO.ProxyAddress := '10.2.7.2:8080';
ImageEnMView.MIO.ProxyUser := 'testuser';
ImageEnMView.MIO.ProxyPassword := 'testpassword';
ImageEnMView.MIO.LoadFromURL('http://www.imageen.com/image.gif');
!!}
    property ProxyPassword: string read fProxyPassword write fProxyPassword;

    procedure PrintImagePos(ImageIndex: integer; PrtCanvas: TCanvas; x, y: double; Width, Height: double; GammaCorrection: double=1);
    procedure PrintImage(ImageIndex : integer; PrtCanvas : TCanvas = nil; MarginLeft : double = 1; MarginTop : double = 1; MarginRight : double = 1; MarginBottom : double = 1; VerticalPos : TIEVerticalPos = ievpCENTER; HorizontalPos : TIEHorizontalPos = iehpCENTER; Size : TIESize = iesFITTOPAGE; SpecWidth : double = 0; SpecHeight : double = 0; GammaCorrection : double = 1);
    procedure PrintImages(Columns: integer=2; Rows: integer=2; HorizSpace: double=0.5; VertSpace: double=0.5; PrintSelected: boolean=false; MarginLeft: double=0; MarginTop: double=0; MarginRight: double=0; MarginBottom: double=0; DrawBox: boolean=true; DrawText: boolean=true; DrawShadow: boolean=false; BoxColor: TColor = clBlack);
    procedure PreviewPrintImages(DestBitmap: TBitmap; MaxBitmapWidth, MaxBitmapHeight: integer; PrinterObj: TPrinter; Columns: integer; Rows: integer; HorizSpace: double; VertSpace: double; PrintSelected: boolean; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; DrawBox: boolean; DrawText: boolean; DrawShadow: boolean; BoxColor: TColor = clBlack; iPageNo : Integer = 0);
    procedure PrintImagesToFile(const sFilename : string; iJpegQuality : Integer; iImageWidth, iImageHeight: integer; iColumns : integer; iRows : integer; iHorzSpace : Integer = 6; iVertSpace : Integer = 6; bPrintSelectedOnly: Boolean = False; iHorzMargin : Integer = 12; iVertMargin : Integer = 12; bDrawBox : Boolean = False; bDrawText : Boolean = True; bDrawShadow : Boolean = True; BackgroundColor : TColor = clWhite; BoxColor: TColor = clBlack; iPageNo : Integer = -1);

  published
    { Published declarations }
    property AttachedMView: TIEView read fImageEnMView write SetAttachedMView;

{!!
<FS>TImageEnMIO.OnProgress

<FM>Declaration<FC>
property OnProgress: <A TIEProgressEvent>;

<FM>Description<FN>
OnProgress event is called upon input/output operations. If you are using it to update a progress bar then you can reset it in the <A TImageEnMView.OnFinishWork> event.
!!}
    property OnProgress: TIEProgressEvent read fOnProgress write fOnProgress;

{!!
<FS>TImageEnMIO.OnAcquireBitmap

<FM>Declaration<FC>
property OnAcquireBitmap: <A TIEAcquireBitmapEvent>;

<FM>Description<FN>
Occurs whenever the scanner acquires an image.

!!}
    property OnAcquireBitmap: TIEAcquireBitmapEvent read fOnAcquireBitmap write fOnAcquireBitmap;


{!!
<FS>TImageEnMIO.OnAcquireClose

<FM>Declaration<FC>
property OnAcquireClose: TNotifyEvent;

<FM>Description<FN>
Occurs when the user closes the acquire dialog, open using <A TImageEnMIO.TwainAcquireOpen>.
!!}
    property OnAcquireClose: TNotifyEvent read fOnAcquireClose write fOnAcquireClose;


{!!
<FS>TImageEnMIO.OnAfterAcquireBitmap

<FM>Declaration<FC>
property OnAfterAcquireBitmap: <A TIEAfterAcquireBitmapEvent>;

<FM>Description<FN>
OnAfterAcquireBitmap occurs just after an image acquired from scanner and hence added to the image list.
!!}
    property OnAfterAcquireBitmap: TIEAfterAcquireBitmapEvent read fOnAfterAcquireBitmap write fOnAfterAcquireBitmap;

    property PreviewsParams: TIOPreviewsParams read GetIOPreviewParams write SetIOPreviewParams default [];
    property PreviewFont: TFont read fPreviewFont write SetPreviewFont;
    property PreviewFontEnabled: Boolean read fPreviewFontEnabled write SetPreviewFontEnabled default false;

{!!
<FS>TImageEnMIO.AutoAdjustDPI

<FM>Declaration<FC>
property AutoAdjustDPI: boolean;

<FM>Description<FN>
When AutoAdjustDPI is True and last loaded/scanned image has horizontal DPI not equal to vertical DPI, ImageEn resizes the image making DPIX=DPIY.
The default is False.
!!}
    property AutoAdjustDPI: boolean read fAutoAdjustDPI write fAutoAdjustDPI default false;

{!!
<FS>TImageEnMIO.FilteredAdjustDPI

<FM>Declaration<FC>
property FilteredAdjustDPI: boolean;

<FM>Description<FN>
The FilteredAdjustDPI property is valid when <A TImageEnMIO.AutoAdjustDPI> is true. If set to True, ImageEn applies a resampling filter to the image to enhance quality.
It can slow down the loading process.
!!}
    property FilteredAdjustDPI: boolean read fFilteredAdjustDPI write fFilteredAdjustDPI default false;

{!!
<FS>TImageEnMIO.SimplifiedParamsDialogs

<FM>Declaration<FC>
property SimplifiedParamsDialogs: boolean;

<FM>Description<FN>
If the SimplifiedParamsDialogs property is True (the default), the 'Advanced' button of open/save dialogs will show a simplified set of parameters.
Warning: the default is True, to allow old style "advanced" dialogs set it to False.

<FM>Example<FC>
This shows the 'Advanced' dialog for TIFF, using SimplifiedParamsDialogs=True: 
<IMG help_images\68.bmp>

This show the 'Advanced' dialog for TIFF, using SimplifiedParamsDialogs=False: 
<IMG help_images\69.bmp>
!!}
    property SimplifiedParamsDialogs: boolean read fSimplifiedParamsDialogs write fSimplifiedParamsDialogs default true;

{!!
<FS>TImageEnMIO.OnFinishWork

<FM>Declaration<FC>
property OnFinishWork: TNotifyEvent;

<FM>Description<FN>
OnFinishWork occurs when an input/output task ends. It is useful in resetting progress bars, or to know when a thread ends in a asynchronous mode.
!!}
    property OnFinishWork: TNotifyEvent read fOnFinishWork write fOnFinishWork;

{!!
<FS>TImageEnMIO.DialogsMeasureUnit

<FM>Declaration<FC>
property DialogsMeasureUnit: <A TIEDialogsMeasureUnit>

<FM>Description<FN>
The DialogsMeasureUnit property specifies the measurement unit used in the print preview dialog (see <A TImageEnMIO.DoPrintPreviewDialog>).

<FM>Example<FC>
ImageEnView.MIO.DialogsMeasureUnit := ieduCm;
ImageEnView.MIO.DoPrintPreviewDialog('');
!!}
    property DialogsMeasureUnit: TIEDialogsMeasureUnit read fDialogsMeasureUnit write fDialogsMeasureUnit default ieduInches;

{!!
<FS>TImageEnMIO.OnDoPreviews

<FM>Declaration<FC>
property OnDoPreviews: <A TIEDoPreviewsEvent>;

<FM>Description<FN>
OnDoPreviews occurs just before DoPreviews is called or “Advanced” button is clicked (save dialog).
You can avoid to display the dialog setting Handled parameter True so to display a custom dialog.
!!}
    property OnDoPreviews: TIEDoPreviewsEvent read fOnDoPreviews write fOnDoPreviews;

    property ImageEnVersion: string read GetImageEnVersion write SetImageEnVersion stored false;
  end;


implementation

uses Dialogs, GIFFilter, TIFFilt, IEVfw, imscan, iopreviews, IEMView, IEOpenSaveDlg, bmpfilt, ieprnform3, ieds, iedicom, iewic, iesettings;

{$R-}



/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.AttachedMView

<FM>Declaration<FC>
property AttachedMView: <A TImageEnMView>;

<FM>Description<FN>
This property specifies the attached TImageEnMView component. You must set this property to use TImageEnMIO.
!!}
procedure TImageEnMIO.SetAttachedMView(v: TIEView);
begin
  if assigned(fImageEnMView) then
    fImageEnMView.RemoveBitmapChangeEvent(fImageEnMViewBitmapChangeHandle); // remove previous, if exists
  fImageEnMView := v;
  if assigned(fImageEnMView) then
  begin // fImageEnMView now could be "nil"
    fImageEnMView.FreeNotification(self);
    fImageEnMViewBitmapChangeHandle := fImageEnMView.RegisterBitmapChangeEvent(OnBitmapChange);
    // synchronize parameters count
    while ParamsCount < (fImageEnMview as TImageEnMView).ImageCount do
      InsertIOParam(ParamsCount);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure TImageEnMIO.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = fImageEnMView) and (Operation = opRemove) then
  begin
    fImageEnMView.RemoveBitmapChangeEvent(fImageEnMViewBitmapChangeHandle);
    fImageEnMView := nil;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

constructor TImageEnMIO.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  //
  fImageEnMViewBitmapChangeHandle := nil;
  fAborting := false;
  fImageEnMView := nil;
  fOnProgress := nil;
  fOnFinishWork := nil;
  fParams := TList.Create;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  fAcquireParams := TIEAcquireParams.Create(Self);
  fTwainParams := TIETwainParams.Create(Self);
  fDCIMParams  := TIEDcimAcquire.Create(Self);
  {$ENDIF}
  fPreviewsParams := [];
  fPreviewFont := TFont.Create;
  fPreviewFontEnabled := False;
  fAutoAdjustDPI := false;
  fFilteredAdjustDPI := false;
  fOnAcquireBitmap := nil;
  fOnAcquireClose := nil;
  fOnAfterAcquireBitmap := nil;
  fOnDoPreviews := nil;
  fgrec := nil;
  fDefaultDitherMethod := ieThreshold;
  SimplifiedParamsDialogs := true;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  fWIA := nil;
  {$ENDIF}
  fResetPrinter := true;
  fDialogsMeasureUnit := ieduInches;
  fNativePixelFormat := false;
  fPrintingFilterOnSubsampling := rfFastLinear;
  ProxyAddress := '';
  ProxyUser := '';
  ProxyPassword := '';
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  fPrintPreviewParams := TIOPrintPreviewParams.Create;
  {$ENDIF}
  fLoadingFileName := '';
  fLastFilename := '';
  fAllowMalformedPages := false;
end;

/////////////////////////////////////////////////////////////////////////////////////

destructor TImageEnMIO.Destroy;
begin
  if assigned(fImageEnMView) then
    fImageEnMView.RemoveBitmapChangeEvent(fImageEnMViewBitmapChangeHandle);
  while fParams.Count > 0 do
    RemoveIOParam( fParams.Count-1 );
  FreeAndNil(fParams);
  {$IFDEF IEINCLUDEIEXACQUIRE}
  FreeAndNil(fAcquireParams);
  FreeAndNil(fTwainParams);
  FreeAndNil(fDCIMParams);
  if assigned(fWia) then
    FreeAndNil(fWia);
  {$ENDIF}
  FreeAndNil(fPreviewFont);
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  FreeAndNil(fPrintPreviewParams);
  {$ENDIF}
  inherited;
end;

{!!
<FS>TImageEnMIO.Update

<FM>Declaration<FC>
procedure Update;

<FM>Description<FN>
Update calls the Update method of the attached <A TImageEnMView> component.
!!}
procedure TImageEnMIO.Update;
begin
  if assigned(fImageEnMView) then
    with fImageEnMView do
    begin
      Update;
      ImageChange;
    end;
end;


// called after registering using RegisterBitmapChangeEvent
// Realign I/O parameters with TImageEnMView
procedure TImageEnMIO.OnBitmapChange(Sender: TObject; destroying: boolean);
var
  lop, lidx, p1: integer;
begin
  if destroying then
    fImageEnMView := nil
  else
  if assigned(fImageEnMView) then
  begin
    lop := (fImageEnMView as TImageEnMView).GetLastOp;
    lidx := (fImageEnMView as TImageEnMView).GetLastOpIdx;
    p1 := (fImageEnMView as TImageEnMView).GetLastOpP1;
    case lop of
      1:
        // Inserting lidx
        InsertIOParam(lidx);
      2:
        // deleting lidx
        RemoveIOParam(lidx);
      3:
        // moving
        MoveIOParams(lidx, p1);
      4:
        // swap
        SwapIOParams(lidx, p1);
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.ParamsCount

<FM>Declaration<FC>
property ParamsCount: integer;

<FM>Description<FN>
ParamsCount is the number of elements in the Params property (alias number of images contained in <A TImageEnMView> attached object).

Read-only

!!}
function TImageEnMIO.GetParamsCount: integer;
begin
  result := fParams.Count;
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.LoadFromFileGIF

<FM>Declaration<FC>
function LoadFromFileGIF(const FileName: string): Boolean;

<FM>Description<FN>
Use LoadFromFileGIF to cause loading a GIF file.

<FC>FileName<FN> is the file name with extension.
Result will be false if the file is not GIF format (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

!!}
function TImageEnMIO.LoadFromFileGIF(const FileName: string) : Boolean;
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fLoadingFileName := FileName;
    Result := LoadFromStreamGIF(fs);
  finally
    fLoadingFileName := '';
    fLastFilename := Filename;
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.LoadFromStreamGIF

<FM>Declaration<FC>
function LoadFromStreamGIF(Stream: TStream): Boolean;

<FM>Description<FN>
LoadFromStreamGIF a GIF from a stream. Inserts the image from <A TImageEnMView.SelectedImage>.

The result will be false if an error is encountered, e.g. the file in the stream is not GIF format (<A TImageEnMIO.Aborting> will be true).

!!}
function TImageEnMIO.LoadFromStreamGIF(Stream: TStream): Boolean;
var
  bmp, xbmp, merged, prev: TIEBitmap;
  p1: int64;
  numi, idx: integer;
  Progress, Progress2: TProgressRec;
  Param: TIOParamsVals;
  ld, im: integer; // last delay
  tempAlphaChannel: TIEMask;
  dummy1: ppointerarray;
  dummy2, dummy3: pinteger;
  act: TIEGIFAction;
  backx, backy, backw, backh: integer;
  dx, dy: integer;
  OriginalPixelFormat: TIEPixelFormat;
begin
  Result := False;
  if not assigned(fImageEnMView) then
    exit;

  merged := nil;
  xbmp := nil;
  bmp := nil;
  prev := nil;

  try
    (fImageEnMView as TImageEnMView).LockPaint;
    fAborting := False;
    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    Progress2.fOnProgress := nil;
    Progress2.Sender := nil;
    Progress2.Aborting := @fAborting;
    p1 := Stream.Position;

    idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx = -1 then
      idx := (fImageEnMView as TImageEnMView).ImageCount;

    ld := 100; // 10ms default
    im := 0;
    merged := TIEBitmap.Create;
    merged.Location := ieMemory;
    act := ioGIF_None;
    backw := 0;
    backh := 0;
    backx := 0;
    backy := 0;
    repeat
      (fImageEnMView as TImageEnMView).InsertImageEx(idx);
      bmp := TIEBitmap.Create;
      try
        Param := Params[idx];
        Stream.position := p1;
        Param.GIF_ImageIndex := im;
        Param.IsNativePixelFormat := fNativePixelFormat;
        tempAlphaChannel := nil;
        ReadGIFStream(Stream, bmp, numi, Param, Progress2, False, tempAlphaChannel, false);
        if (bmp.Width = 0) or (bmp.Height = 0) then
          fAborting := true;
        OriginalPixelFormat := bmp.PixelFormat;
        dx := imax(Param.GIF_WinWidth, bmp.Width);
        dy := imax(Param.GIF_WinHeight, bmp.Height);
        if assigned(tempAlphaChannel) then
        begin
          bmp.AlphaChannel.CopyFromTIEMask(tempAlphaChannel);
          FreeAndNil(tempAlphaChannel);
        end;
        if fAutoAdjustDPI then
          xbmp := IEAdjustDPI(bmp, Param, fFilteredAdjustDPI)
        else
          xbmp := bmp;
      finally
        if bmp <> xbmp then
          FreeAndNil(bmp);
      end;
      bmp := xbmp;

      if numi > 1 then
      begin
        if Param.GIF_Action = ioGIF_RestorePrev then
        begin
          // saves current state
          if not assigned(prev) then
            prev := TIEBitmap.Create();
          prev.Assign(merged);
        end;
        if act = ioGIF_RestorePrev then
        begin
          // restore previous state
          if assigned(prev) then
            merged.Assign(prev);
        end;
        if act = ioGIF_DrawBackground then
        begin
          merged.FillRect(backx, backy, backx + backw - 1, backy + backh - 1, TRGB2TColor(param.GIF_Background));
          merged.AlphaChannel.FillRect(backx, backy, backx + backw - 1, backy + backh - 1, 0);  //*
        end;
        if (merged.Width = 0) then
        begin
          //merged.Allocate(dx, dy, bmp.PixelFormat);
          merged.Allocate(dx, dy, ie24RGB);
          merged.Fill(TRGB2TColor(param.GIF_Background));
          merged.AlphaChannel.Fill(0);//*
        end;
        if merged.PixelFormat<>ie24RGB then
          merged.PixelFormat := ie24RGB;
        if (dx > merged.Width) or (dy > merged.Height) then
          merged.Resize(dx, dy, TRGB2TColor(param.GIF_Background), 255, iehLeft, ievTop);
        dummy1 := nil;
        dummy2 := nil;
        dummy3 := nil;

        bmp.RenderToTIEBitmap(merged, dummy1, dummy2, dummy3, nil, Param.GIF_XPos, Param.GIF_YPos, bmp.Width, bmp.Height, 0, 0, bmp.Width, bmp.Height, true, false, 255, rfNone, true, ielNormal);

        bmp.MergeAlphaRectTo(merged, 0, 0, Param.GIF_XPos, Param.GIF_YPos, bmp.Width, bmp.Height);
        merged.AlphaChannel.Full := false;

        backw := bmp.Width;
        backh := bmp.Height;
        backx := Param.GIF_XPos;
        backy := Param.GIF_YPos;
        if bmp = xbmp then
          xbmp := nil;
        FreeAndNil(bmp);
        bmp := merged;
        act := param.GIF_Action; // act refers to the action of next image
        Param.GIF_Action := ioGIF_DrawBackground;
        Param.GIF_XPos := 0;
        Param.GIF_YPos := 0;
      end;

      Param.ImageIndex := idx;
      Param.FileType := ioGIF;
      Param.FileName := WideString(fLoadingFileName);
      if fAborting then
      begin
        (fImageEnMView as TImageEnMView).DeleteImage(idx);
        if bmp <> merged then
        begin
          if bmp = xbmp then
            xbmp := nil;
          FreeAndNil(bmp);
        end;
        break;
      end;
      if numi = 0 then
        (fImageEnMView as TImageEnMView).DeleteImage(idx)
      else
      begin
        Progress.per1 := 100 / numi;
        bmp.PixelFormat := originalPixelFormat;
        (fImageEnMView as TImageEnMView).SetIEBitmapEx(idx, bmp);
        (fImageEnMView as TImageEnMView).ImageBackground[idx] := TRGB2TColor(Param.GIF_Background);
        if Param.GIF_DelayTime > 0 then
        begin
          (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := Param.GIF_DelayTime * 10;
          ld := Param.GIF_DelayTime * 10;
        end
        else
          (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := ld;
      end;
      if bmp <> merged then
      begin
        if bmp = xbmp then
          xbmp := nil;
        FreeAndNil(bmp);
      end
      else
      begin
        if bmp = xbmp then
          xbmp := nil;
        bmp := nil;
      end;
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * im));
      if fAborting then
        break;
      inc(idx);
      inc(im);
    until im >= numi;
  finally
    if bmp = xbmp then
      xbmp := nil;
    if bmp = merged then
      bmp := nil;
    bmp.Free();
    xbmp.Free();
    merged.Free();
    prev.Free();
    Update;
    (fImageEnMView as TImageEnMView).UnLockPaint;
    DoFinishWork;      
  end;
  Result := Not fAborting;
end;

{!!
<FS>TImageEnMIO.SaveToFileGIF

<FM>Declaration<FC>
procedure SaveToFileGIF(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView as a GIF file (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnMIO.SaveToFileGIF(const FileName: string; SelectedOnly: Boolean = False);
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStreamGIF(fs, SelectedOnly);
  finally
    FreeAndNil(fs);
  end;
  _GIFMakeAnimate(FileName, 0, 0, 0);
end;

{!!
<FS>TImageEnMIO.SaveToStreamGIF

<FM>Declaration<FC>
procedure SaveToStreamGIF(Stream: TStream; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView to a stream in GIF format (or only those images selected if <FC>SelectedOnly<FN> = True).
!!}
procedure TImageEnMIO.SaveToStreamGIF(Stream: TStream; SelectedOnly: Boolean = False);
var
  p1: int64;
  Param: TIOParamsVals;
  Progress: TProgressRec;
  bmp: TIEBitmap;
  nullpr: TProgressRec;

  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    Param := Params[ImgIdx];
    Param.GIF_ImageIndex := FileIdx;
    Stream.position := p1;
    (fImageEnMView as TImageEnMView).CopyToIEBitmap(ImgIdx, bmp);
    if bmp.HasAlphaChannel then
    begin
      bmp.AlphaChannel.SyncFull;
      if bmp.AlphaChannel.Full then
        bmp.RemoveAlphaChannel;
    end;
    bmp.DefaultDitherMethod := fDefaultDitherMethod;
    if FileIdx = 0 then
      WriteGIFStream(Stream, bmp, Param, NullPr)
    else
      _InsertGIFImStream(Stream, bmp, Param, NullPr);
      
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i, imgCount: integer;
begin
  bmp := nil;
  try
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;

    if SelectedOnly then
      imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
    else
      imgCount := (fImageEnMView as TImageEnMView).ImageCount;
    if imgCount = 0 then
      exit;

    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    p1 := Stream.Position;
    Progress.per1 := 100 / imgCount;
    bmp := TIEBitmap.Create;

    i := 0;
    while not fAborting and (i < imgCount) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;

  finally
    FreeAndNil(bmp);
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnMIO.LoadFromFileTIFF

<FM>Declaration<FC>
function LoadFromFileTIFF(const FileName: string): Boolean;

<FM>Description<FN>
Use LoadFromFileTIFF to cause loading a TIFF file.

<FC>FileName<FN> is the file name with extension.
Result will be false if the file is not TIFF format (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

!!}
function TImageEnMIO.LoadFromFileTIFF(const FileName: string): Boolean;
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fLoadingFileName := FileName;
    Result := LoadFromStreamTIFF(fs);
  finally
    fLoadingFileName := '';
    fLastFilename := Filename;
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.LoadFromStreamTIFF

<FM>Declaration<FC>
function LoadFromStreamTIFF(Stream: TStream): Boolean;

<FM>Description<FN>
Load a TIFF from a stream.

The result will be false if an error is encountered, e.g. the file in the stream is not TIFF format (<A TImageEnMIO.Aborting> will be true).
!!}
// default delay is 100ms
function TImageEnMIO.LoadFromStreamTIFF(Stream: TStream): Boolean;
var
  p1: int64;
  xbmp, bmp: TIEBitmap;
  numi, idx: integer;
  Progress, Progress2: TProgressRec;
  Param: TIOParamsVals;
  im: integer;
  tempAlphaChannel: TIEMask;
begin
  Result := False;
  fAborting := False;
  if not assigned(fImageEnMView) then
    exit;
  try
    (fImageEnMView as TImageEnMView).LockPaint;
    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    Progress2.fOnProgress := nil;
    Progress2.Sender := nil;
    Progress2.Aborting := @fAborting;
    p1 := Stream.Position;

    idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx = -1 then
      idx := (fImageEnMView as TImageEnMView).ImageCount;

    im := 0;
    repeat
      (fImageEnMView as TImageEnMView).InsertImageEx(idx);
      bmp := TIEBitmap.Create;
      try
        Param := Params[idx];
        Stream.position := p1;
        Param.TIFF_ImageIndex := im;
        Param.IsNativePixelFormat := fNativePixelFormat;
        tempAlphaChannel := nil;
        TIFFReadStream(bmp, Stream, numi, Param, Progress2, false, tempAlphaChannel, true, false, false, false);
        CheckDPI(Param);
        if assigned(tempAlphaChannel) then
        begin
          bmp.AlphaChannel.CopyFromTIEMask(tempAlphaChannel);
          FreeAndNil(tempAlphaChannel);
        end;
        if fAutoAdjustDPI then
        begin
          xbmp := IEAdjustDPI(bmp, Param, fFilteredAdjustDPI);
          if bmp <> xbmp then
          begin
            FreeAndNil(bmp);
            bmp := xbmp
          end;
        end;
        
        Param.ImageIndex := idx;
        Param.FileType := ioTIFF;
        Param.FileName := WideString(fLoadingFileName);
        if fAborting and not fAllowMalformedPages then
        begin
          (fImageEnMView as TImageEnMView).DeleteImage(idx);
          break;
        end;
        if fAborting then
          fAborting := false;
        if numi = 0 then
          (fImageEnMView as TImageEnMView).DeleteImage(idx)
        else
        begin
          Progress.per1 := 100 / numi;
          (fImageEnMView as TImageEnMView).SetIEBitmapEx(idx, bmp);
          (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := 100;
        end;
      finally
        FreeAndNil(bmp);
      end;
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * im));
      if fAborting and not fAllowMalformedPages then
        break;
      inc(idx);
      inc(im);
    until im >= numi;
    Update;
  finally
    (fImageEnMView as TImageEnMView).UnLockPaint;
    DoFinishWork;
  end;
  Result := Not fAborting;
end;

{!!
<FS>TImageEnMIO.SaveToFileTIFF

<FM>Declaration<FC>
procedure SaveToFileTIFF(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView as a TIFF file (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnMIO.SaveToFileTIFF(const FileName: string; SelectedOnly: Boolean = False);
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStreamTIFF(fs, SelectedOnly);
  finally
    FreeAndNil(fs);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.SaveToStreamTIFF

<FM>Declaration<FC>
procedure SaveToStreamTIFF(Stream: TStream; SaveSelected: boolean=false);

<FM>Description<FN>
Save all images in the connected TImageEnMView to a stream in TIFF format (or only those images selected if <FC>SelectedOnly<FN> = True).
!!}
procedure TImageEnMIO.SaveToStreamTIFF(Stream: TStream; SelectedOnly: Boolean = False);
var
  p1: int64;
  Param: TIOParamsVals;
  Progress: TProgressRec;
  bmp: TIEBitmap;
  nullpr: TProgressRec;
  imgCount: integer;

  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    Param := Params[ImgIdx];
    Param.TIFF_ImageIndex := FileIdx;
    Stream.position := p1;
    (fImageEnMView as TImageEnMView).CopyToIEBitmap(ImgIdx, bmp);
    bmp.DefaultDitherMethod := fDefaultDitherMethod;
    if FileIdx = 0 then
      TIFFWriteStream(Stream, false, bmp, Param, NullPr)
    else
      TIFFWriteStream(Stream, true, bmp, Param, NullPr);
      
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i: integer;
begin
  bmp := nil;
  try
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;

    if SelectedOnly then
      imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
    else
      imgCount := (fImageEnMView as TImageEnMView).ImageCount;

    if imgCount = 0 then
      exit;

    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;

    bmp := TIEBitmap.Create;
    p1 := Stream.Position;
    Progress.per1 := 100 / imgCount;

    i := 0;
    while not fAborting and (i < imgCount) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;

  finally
    FreeAndNil(bmp);
    DoFinishWork;
  end;
end;



{!!
<FS>TImageEnMIO.LoadFromFileAVI

<FM>Declaration<FC>
function LoadFromFileAVI(const FileName: string): Boolean;

<FM>Description<FN>
Use LoadFromFileAVI to cause loading of an AVI file.

<FC>FileName<FN> is the file name with extension.
Result will be false if the file is not AVI format (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.
!!}
function TImageEnMIO.LoadFromFileAVI(const FileName: string): Boolean;
var
  avf: PAVIFILE;
  avs: PAVISTREAM;
  gf: PGETFRAME;
  pt: pointer;
  ln, idx, im: integer;
  Progress: TProgressRec;
  bmp: TIEDibBitmap;
  Param: TIOParamsVals;
  psi: TAVISTREAMINFO;
  dt: integer;
  bitcount: integer;
  trymediafile: boolean;
begin
  Result := False;
  trymediafile := false;
  try
    (fImageEnMView as TImageEnMView).LockPaint;
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;
    if not gAVIFILEinit then
    begin
      AVIFileInit;
      gAVIFILEinit := true;
    end;
    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    //
    if AVIFileOpen(avf, pchar(FileName), OF_READ, nil) <> 0 then
    begin
      fAborting := True;
      exit;
    end;
    if AVIFileGetStream(avf, avs, streamtypeVIDEO, 0) <> 0 then
    begin
      AVIFileRelease(avf);
      fAborting := True;
      trymediafile := true;
      exit;
    end;
    if AVIStreamInfo(avs, psi, sizeof(TAVISTREAMINFO)) <> 0 then
    begin
      AVIFileRelease(avf);
      AVIStreamRelease(avs);
      fAborting := True;
      trymediafile := true;
      exit;
    end;
    gf := AVIStreamGetFrameOpen(avs, nil);
    if gf = nil then
    begin
      AVIFileRelease(avf);
      AVIStreamRelease(avs);
      fAborting := True;
      trymediafile := true;
      exit;
    end;
    //
    ln := psi.dwLength;
    dt := trunc((1 / (psi.dwRate / psi.dwScale)) * 1000);
    Progress.per1 := 100 / ln;

    idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx=-1 then
      idx := (fImageEnMView as TImageEnMView).ImageCount;

    bmp := TIEDibBitmap.Create;
    for im := 0 to ln - 1 do
    begin
      pt := AVIStreamGetFrame(gf, im);
      if pt <> nil then
      begin
        (fImageEnMView as TImageEnMView).InsertImageEx(idx);
        Param := Params[idx];
        bitcount := _IECopyDIB2Bitmap2Ex(integer(pt), bmp, nil, true); // uses drawdibdraw
        (fImageEnMView as TImageEnMView).SetIEBitmapEx(idx, bmp);
        if im = 0 then
          (fImageEnMView as TImageEnMView).PrepareSpaceFor(bmp.Width, bmp.Height, BitCount, ln - 1);
        (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := dt;
        Param.GIF_DelayTime := dt;
        case BitCount of
          1:
            begin
              Param.BitsPerSample := 1;
              Param.SamplesPerPixel := 1;
            end;
          4:
            begin
              Param.BitsPerSample := 4;
              Param.SamplesPerPixel := 1;
            end;
          8:
            begin
              Param.BitsPerSample := 8;
              Param.SamplesPerPixel := 1;
            end;
          15:
            begin
              Param.BitsPerSample := 5;
              Param.SamplesPerPixel := 3;
            end;
          16, 24, 32:
            begin
              Param.BitsPerSample := 8;
              Param.SamplesPerPixel := 3;
            end;
        end;
        
        Param.ImageIndex := idx;
        Param.DpiX := IEGlobalSettings().DefaultDPIX;
        Param.DpiY := IEGlobalSettings().DefaultDPIY;
        Param.Width := bmp.Width;
        Param.Height := bmp.Height;
        Param.FileName := WideString(FileName);
        Param.FileType := ioAVI;
        Param.FreeColorMap;
        with Progress do
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * im));
      end
      else
        fAborting := true;
      if fAborting then
        break;
      inc(idx);
    end;
    FreeAndNil(bmp);
    AVIStreamGetFrameClose(gf);
    AVIStreamRelease(avs);
    AVIFileRelease(avf);
    Update;
  finally
    (fImageEnMView as TImageEnMView).UnLockPaint;
    if trymediafile then
    begin
      {$ifdef IEINCLUDEDIRECTSHOW}
      LoadFromMediaFile(FileName);
      {$endif}
    end
    else
      DoFinishWork;
  end;
  Result := Not fAborting;
end;
                        

{!!
<FS>TImageEnMIO.SaveToFileAVI

<FM>Declaration<FC>
procedure SaveToFileAVI(const FileName: string; const Codec: AnsiString = ''; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView as an AVI file (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Images in the <A TImageEnMView> must be of the same size.

<FC>codec<FN> specifies the compression codec to use (must be installed on system) as four characters length string. Example:
'cvid' : cinepak by Radius
'msvc' : Microsoft Video 1
'mp42' : Microsoft MPEG4 V2

More codecs are listed at <L http://www.fourcc.org>www.fourcc.org</L> or by searching for registered fourcc codes and wave formats on <L http://msdn.microsoft.com>MSDN</L>

If a codec is not specified, a dialog box appears to allow the user to select a compression.

!!}
procedure TImageEnMIO.SaveToFileAVI(const FileName: string; const Codec: AnsiString = ''; SelectedOnly: Boolean = False);
var
  io: TImageEnIO;
  Progress: TProgressRec;

  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    io.AttachedIEBitmap := (fImageEnMView as TImageEnMView).GetTIEBitmap(ImgIdx);
    io.SaveToAVI;
    (fImageEnMView as TImageEnMView).ReleaseBitmap(ImgIdx, false);
    
    with Progress do
     if assigned(fOnProgress) then
       fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i, imgCount: Integer;
begin
  if not assigned(fImageEnMView) then
    exit;

  if SelectedOnly then
    imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
  else
    imgCount := (fImageEnMView as TImageEnMView).ImageCount;
  if imgCount = 0 then
    exit;

  Progress.fOnProgress := fOnProgress;
  Progress.Sender := Self;
  Progress.per1 := 100 / imgCount;
  Progress.Aborting := @fAborting;
  io := TImageEnIO.Create(nil);
  try
    io.Params.Width := (fImageEnMView as TImageEnMView).ImageWidth[0];
    io.Params.Height := (fImageEnMView as TImageEnMView).ImageHeight[0];
    io.Params.BitsPerSample := Params[0].BitsPerSample;
    io.Params.SamplesPerPixel := Params[0].SamplesPerPixel;
    io.CreateAVIFile(FileName, 1.0 / (dmax((fImageEnMView as TImageEnMView).ImageDelayTime[0], 1.0) / 1000.0), codec);
    if io.Aborting then
      exit;

    i := 0;
    while not fAborting and (i < imgCount) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;

    io.CloseAVIFile;
  finally
    FreeAndNil(io);
  end;
end;


function MultiImageFilenameToFileType(const FileName: string) : TIOFileType;
var
  ex: string;
begin
  Result := ioUnknown;

  ex := IEExtractFileExtS(FileName);
  if (ex = '.gif') then
    Result := ioGIF
  else
  if (ex = '.tif') or (ex = '.tiff') or (ex = '.fax') or (ex = '.g3f') or (ex = '.g3n') then
    Result := ioTIFF
  else
  if (ex = '.avi') then
    Result := ioAVI
  else
  if (ex = '.ps') or (ex = '.eps') then
    Result := ioPS
  {$ifdef IEINCLUDEPDFWRITING}
  else
  if (ex = '.pdf') then
    Result := ioPDF
  {$endif}
  else
  if (ex = '.dcx') then
    Result := ioDCX
  else
  if (ex = '.ico') then
    Result := ioICO
  else
  if (ex = '.dcm') or (ex = '.dicom') or (ex = '.dic') or (ex = '.v2') then
    Result := ioDICOM;
end;


{!!
<FS>TImageEnMIO.LoadFromFile

<FM>Declaration<FC>
function LoadFromFile(const FileName: string; bCheckUnknown: Boolean = False): Boolean;

<FM>Description<FN>
LoadFromFile loads a multi-image file. It detects the file format from the file name extension.
The source can be also an URL, if it has the form 'http://'.

<FC>FileName<FN> is the file name with extension.
Result will be false if the file is not a recognized file type (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.
However, if you set <FC>bCheckUnknown<FN> to true, ImageEn will try to load the file even if it has an unknown or incorrect file extension.

<FM>Example<FC>
ImageEnMView1.MIO.LoadFromFile('C:\film.avi');

ImageEnMView1.MIO.LoadFromFile('C:\pages.tiff');

if ImageEnMView1.MIO.LoadFromFile(sFilename) = False then
  ShowMessage('This is not a supported file type');
!!}
function TImageEnMIO.LoadFromFile(const FileName: string; bCheckUnknown: Boolean = False): Boolean;
var
  ff, nf: TIOFileType;
begin                
  Result := False;
  if trim(FileName) = '' then
  begin
    fAborting := true;
    exit;
  end;

  if IEGetURLTypeW(FileName) <> ieurlUNKNOWN then
  begin
    Result := LoadFromURL(WideString(FileName));
    exit;
  end;

  ff := MultiImageFilenameToFileType(FileName);
  Result := LoadFromFileFormat(FileName, ff);

  if bCheckUnknown and (Result = False) then
  begin
    nf := FindFileFormat(FileName, false);
    if (nf <> ioUnknown) and (ff <> nf) then
      Result := LoadFromFileFormat(FileName, nf);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.SaveToFile

<FM>Declaration<FC>
procedure SaveToFile(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView to a multi-image format: GIF, TIFF, DCX, ICO, AVI, DICOM, PDF or PS. It detects the file's format from the extension.

<FC>FileName<FN> is the file name with extension.
Only the selected images are saved if <FC>SelectedOnly<FN> = True.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnMIO.SaveToFile(const FileName: string; SelectedOnly: Boolean = False);
begin
  if Trim(FileName) = '' then
  begin
    fAborting := true;
    exit;
  end;
  
  fAborting := False;

  case MultiImageFilenameToFileType(FileName) of
    ioGIF    : SaveToFileGIF(FileName, SelectedOnly);
    ioTIFF   : SaveToFileTIFF(FileName, SelectedOnly);
    ioAVI    : SaveToFileAVI(FileName, '', SelectedOnly);
    ioPS     : SaveToFilePS(FileName, SelectedOnly);
    {$ifdef IEINCLUDEPDFWRITING}
    ioPDF    : SaveToFilePDF(FileName, SelectedOnly);
    {$endif}
    ioDCX    : SaveToFileDCX(FileName, SelectedOnly);
    ioICO    : SaveToFileICO(FileName, SelectedOnly);
    ioDICOM  : SaveToFileDICOM(FileName, SelectedOnly);
  end;
end;


/////////////////////////////////////////////////////////////////////////////////////
{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnMIO.SelectAcquireSource

<FM>Declaration<FC>
function SelectAcquireSource(Apis : <A TIEAcquireApis> = [ieaTwain, ieaWIA, ieaDCIM]): boolean;

<FM>Description<FN>
Prompts the user with a dialog to select a Twain, WIA or DCIM device.  Use Apis to specify which sources are available to the user.

Any combination of the following can be used:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieaTwain</C> <C>Acquire from Twain device</C> </R>
<R> <C>ieaWIA</C> <C>Acquire using WIA (scanners or camera)</C> </R>
<R> <C>ieaDCIM</C> <C>Read from any attached device containing a DCIM folder (e.g. camera card in a slot or a USB connected camera)</C> </R>
</TABLE>

Note: If your location is only [ieTwain] or [ieWIA] then the default Twain/WIA selector is shown. Otherwise a custom device selector is used (Use <A TIEImageEnGlobalSettings.MsgLanguage> to control the language in the custom dialog).

Returns False if user press "Cancel" button.

<FM>Example<FC>
if ImageEn1.IO.SelectAcquireSource([ieaTwain, ieaWIA, ieaDCIM]) then
  ImageEn1.IO.Acquire;
!!}
function TImageEnMIO.SelectAcquireSource(Apis : TIEAcquireApis = [ieaTwain, ieaWIA, ieaDCIM]): boolean;    
// NPC: 16/11/11
begin                  
  Result := FAcquireParams.SelectSource(Apis);
end;

{!!
<FS>TImageEnMIO.SetAcquireSource

<FM>Declaration<FC>
function SetAcquireSource(Api : <A TIEAcquireApi>; Location : Variant) : boolean;

<FM>Description<FN>
Programatically set the selected acquisition source by an API type and device. The selected device will be used for subsequent calls to <A TImageEnMIO.Acquire>.
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
- <A TImageEnMIO.Acquire>
- <A TImageEnMIO.SelectedAcquireSource>

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
function TImageEnMIO.SetAcquireSource(Api: TIEAcquireApi; Location : Variant) : boolean; 
// NPC: 16/11/11
begin
  Result := FAcquireParams.SetSource(Api, Location);
end;


{!!
<FS>TImageEnMIO.SelectedAcquireSource

<FM>Declaration<FC>
property SelectedAcquireSource : <A TIEAcquireSource>; (Read only)

<FM>Description<FN>
Return the acquisition source that is currently active due to selection by the user with <A TImageEnMIO.SelectAcquireSource> or programatically using <A TImageEnMIO.SetAcquireSource>.
A <A TIEAcquireSource> record is returned that provides meta information about the device (SelectedAcquireSource.Name, SelectedAcquireSource.DeviceType) and technical details (SelectedAcquireSource.Api, SelectedAcquireSource.Location).
If no device is selected then SelectedAcquireSource.Api will be ieaNone.

<FM>See Also<FN>
- <A TImageEnMIO.SelectAcquireSource>
- <A TImageEnMIO.SetAcquireSource>

<FM>Examples<FC>
// Display the selected source
if ImageEnMView1.MIO.SelectedAcquireSource.Api = ieaNone then
  ShowMessage('No device is selected')
else
  ShowMessage('The selected device is ' + ImageEnMView1.MIO.SelectedAcquireSource.Name);

// Read and restore the selected source
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
function TImageEnMIO.GetSelectedAcquireSource : TIEAcquireSource; 
// NPC: 16/11/11
begin
  Result := FAcquireParams.SelectedSource;
end;


procedure TImageEnMIO.InitializeAcquireSource(bIncludeWIA : Boolean);
// NPC: 16/11/11
begin
  AcquireParams.AttachedTwainParams := fTwainParams;
  AcquireParams.AttachedDCIMParams := fDcimParams;
  if bIncludeWIA then
    AcquireParams.AttachedWIAParams := WIAParams;
end;


procedure TImageEnMIO.TWMultiCallBack(Bitmap: TIEBitmap; var IOParams: TObject; ImDpiX, ImDpiY: integer);
var
  bHandled: boolean; // Flag indicating whether the user has handled the acquired bitmap themselves
  bmp: TIEBitmap;
begin
  bHandled := false;
  if assigned(fOnAcquireBitmap) then
    fOnAcquireBitmap(Self, Bitmap, bHandled);
  if (bHandled = false) and assigned(fImageEnMView) then
  begin
    fTwainNextToInsert := imax(imin(fTwainNextToInsert, (fImageEnMView as TImageEnMView).ImageCount), 0);
    (fImageEnMView as TImageEnMView).InsertImageEx(fTwainNextToInsert);
    IOParams := Params[fTwainNextToInsert];
    if (ImDpiX<>0) and (ImDpiY<>0) then
    begin
      Params[fTwainNextToInsert].DpiX := ImDpiX;
      Params[fTwainNextToInsert].DpiY := ImDpiY;
    end;
    if fAutoAdjustDPI then
      bmp := IEAdjustDPI(Bitmap, IOParams as TIOParamsVals, fFilteredAdjustDPI)
    else
      bmp := Bitmap;
    (fImageEnMView as TImageEnMView).SetIEBitmapEx(fTwainNextToInsert, bmp);
    if bmp <> Bitmap then
      FreeAndNil(bmp);
    (fImageEnMView as TImageEnMView).ImageDelayTime[fTwainNextToInsert] := 100;
    if assigned(fOnAfterAcquireBitmap) then
      fOnAfterAcquireBitmap(Self, fTwainNextToInsert);
    inc(fTwainNextToInsert);
  end;
end;

{$endif}

/////////////////////////////////////////////////////////////////////////////////////
{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnMIO.Acquire

<FM>Declaration<FC>
function Acquire : boolean;

<FM>Description<FN>
Perform a multiple image acquisition from the <A TImageEnMIO.SelectedAcquireSource>, which may be a camera card, Twain or WIA device.
<A TImageEnMIO.SelectedAcquireSource> is set when the user chooses a source if you have called <A TImageEnMIO.SelectAcquireSource> or manually set a source using <A TImageEnMIO.SetAcquireSource>.
If no image is selected (see <A TImageEnMView.DeSelect>) then Acquire will append after the last image. 

Returns True if the acquisition succeeds.
               
<FM>Examples<FN>
// Prompt the user to choose a scanner source and then acquire
if ImageEnMView1.MIO.SelectAcquireSource([ieaTwain, ieaWIA, ieaDCIM]) then
  ImageEnMView1.MIO.Acquire;

// capture from the default WIA device
if ImageEnMView1.MIO.SetSource(ieaWIA, Default_Device) then
  ImageEnMView1.MIO.Acquire;

// select the second Twain device and capture
if ImageEnMView1.MIO.SetSource(ieaTwain, 1) then
  ImageEnMView1.MIO.Acquire;

// Capture from the Twain device named, CanoScan FB620
if ImageEnMView1.MIO.SetSource(ieaTwain, 'CanoScan FB620') then
  ImageEnMView1.MIO.Acquire;

// Retrieve from the camera card listed as H:\ drive
if ImageEnMView1.MIO.SetSource(ieaDCIM, 'H') then
  ImageEnMView1.MIO.Acquire;

// Capture without a dialog
ImageEnMView1.MIO.AcquireParams.VisibleDialog := False;
ImageEnMView1.MIO.Acquire;
!!}
function TImageEnMIO.Acquire : boolean;
begin
  // calling Acquire after TwainAcquireOpen!!!!
  if assigned(fgrec) then
  begin
    result := true; // there is already a scanner dialog open
    exit;
  end;

  try
    fAborting := False;
    if assigned(fImageEnMView) then
    begin
      if (fImageEnMView as TImageEnMView).SelectedImage >= 0 then
        fTwainNextToInsert := (fImageEnMView as TImageEnMView).SelectedImage
      else
        fTwainNextToInsert := (fImageEnMView as TImageEnMView).ImageCount;
    end
    else
      fTwainNextToInsert := 0;

    result := FAcquireParams.Acquire(TWMultiCallBack, fOnProgress, fNativePixelFormat);
    Update;
  except
    result := false;
  end;
end;
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////////////
// show the input/output parameters preview dialog
// return True if the user press OK
// if idx=-1 applies the some compression parameters to all images
{$IFDEF IEINCLUDEDIALOGIO}

{!!
<FS>TImageEnMIO.DoPreviews

<FM>Declaration<FC>
function DoPreviews(idx: integer; pp: <A TPreviewParams>): boolean;

<FM>Description<FN>
DoPreviews executes the Previews dialog for the image idx. This dialog gets/sets the parameters of image file formats.

pp is the set of the image format parameters to be displayed by the dialog.
If idx is -1, the dialogs act on all images.
if idx is IEM_SELECTED_IMAGES, the dialog acts on the current image (but not all selected images)

<FM>Example<FC>
ImageEnMView1.MIO.LoadFromFile('C:\myimage.gif');  // loads a GIF image
ImageEnMView1.MIO.DoPreviews(-1, [ppGIF]); // sets GIF parameters (background, transparency...) for ALL images
ImageEnMView1.MIO.SaveToFile('D:\newimage.gif'); // saves some image with new parameters

!!}
function TImageEnMIO.DoPreviews(idx: integer; pp: TPreviewParams): boolean;
var
  fIOPreviews: TfIOPreviews;
  fBitmap: TBitmap;
  Handled: boolean;
begin
  result := false;
  Handled := false;
  if assigned(fOnDoPreviews) then
    fOnDoPreviews(self, Handled);

  if idx = IEM_ALL_IMAGES then
    idx := -1 { just in case }
  else
  if idx = IEM_SELECTED_IMAGES then
  begin                
    if not assigned(fImageEnMView) then
      exit;
    if (fImageEnMView as TImageEnMView).DisplayMode = mdSingle then
      idx := (fImageEnMView as TImageEnMView).VisibleFrame
    else
      idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx < 0 then
      exit;
  end;

  if not Handled then
  begin
    if not assigned(fImageEnMView) then
      exit;
    if ((fImageEnMView as TImageEnMView).ImageCount > 0) and ((fImageEnMView as TImageEnMView).SelectedImage < 0) then
      (fImageEnMView as TImageEnMView).SelectedImage := 0;
    fBitmap := fImageEnMView.Bitmap;
    if not assigned(fBitmap) then
      exit;
    fIOPreviews := TfIOPreviews.Create(self);
    fIOPreviews.DefaultLockPreview := ioppDefaultLockPreview in PreviewsParams;
    fIOPreviews.btnApply.Visible := ioppApplyButton in PreviewsParams;
    fIOPreviews.fSimplified := fSimplifiedParamsDialogs;
    if fSimplifiedParamsDialogs then
    begin
      fIOPreviews.PageControl1.Height := trunc(IO_Preview_Page_Control_Short_Height / 96 * Screen.PixelsPerInch);
      fIOPreviews.Height := trunc(IO_Preview_Dialog_Short_Height / 96 * Screen.PixelsPerInch);
    end;
    fIOPreviews.fDefaultDitherMethod := fDefaultDitherMethod;
    if idx < 0 then
      fIOPreviews.fParams := Params[0]
    else
      fIOPreviews.fParams := Params[idx];
    //
    fIOPreviews.UpdateLanguage();

    if fPreviewFontEnabled then
      fIOPreviews.Font.Assign(fPreviewFont)
    else
      fIOPreviews.Font.Assign(IEGetDefaultDialogFont);

    //
    with fIOPreviews.ImageEn1 do
    begin
      IECopyBitmap(fBitmap, Bitmap);
      Update;
    end;
    if fIOPreviews.SetPreviewParams(pp) then
      result := fIOPreviews.ShowModal = mrOk
    else
      result := false;
    fIOPreviews.Release;
    // duplicate compression parameters
    if idx < 0 then
      DuplicateCompressionInfo;
    // reorder image indexes
    (*
    for q := 0 to ParamsCount - 1 do
    begin
      Params[q].TIFF_ImageIndex := q;
      Params[q].GIF_ImageIndex := q;
      Params[q].DCX_ImageIndex := q;
      Params[q].ICO_ImageIndex := idx;
    end;
    *)
    Update;
  end
  else
    result := true; // handled
end;
{$ENDIF}

{!!
<FS>TImageEnMIO.DuplicateCompressionInfo

<FM>Declaration<FC>
procedure DuplicateCompressionInfo;

<FM>Description<FN>
DuplicateCompressionInfo clones the compression information of page 0 to all pages.

!!}
procedure TImageEnMIO.DuplicateCompressionInfo;
var
  i: integer;
  tmp: TIOParamsVals;
begin
  tmp := TIOParamsVals.Create(nil);
  tmp.AssignCompressionInfo(Params[0]);
  for i := 1 to ParamsCount - 1 do
    Params[i].AssignCompressionInfo(tmp);
  FreeAndNil(tmp);
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.PreviewFont

<FM>Declaration<FC>
property PreviewFont: TFont;

<FM>Description<FN>
If <A TImageEnMIO.PreviewFontEnabled> is set to True then PreviewFont specifies the font used in the Previews dialog. Ensure the size of font matches the labels length.

<FM>Example<FC>
ImageEnMIO1.PreviewFont.Name := 'MS Times New Roman';
ImageEnMIO1.PreviewFontEnabled := True;
!!}
procedure TImageEnMIO.SetPreviewFont(f: TFont);
begin
  fPreviewFont.assign(f);
end;

/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.PreviewFontEnabled

<FM>Declaration<FC>
property PreviewFontEnabled: Boolean;

<FM>Description<FN>
If you set PreviewFontEnabled to True then you can use <A TImageEnMIO.PreviewFont> to specify a custom font for the Preview dialogs.

<FM>Example<FC>
ImageEnMIO1.PreviewFont.Name := 'MS Times New Roman';
ImageEnMIO1.PreviewFontEnabled := True;
!!}
procedure TImageEnMIO.SetPreviewFontEnabled(Value : Boolean);
begin
  fPreviewFontEnabled := Value;
end;

/////////////////////////////////////////////////////////////////////////////////////



{!!
<FS>TImageEnMIO.PreviewsParams

<FM>Declaration<FC>
property PreviewsParams: <A TIOPreviewsParams>;

<FM>Description<FN>
This property specifies the features that the input/output preview dialog will have.
!!}
procedure TImageEnMIO.SetIOPreviewParams(v: TIOPreviewsParams);
begin
  fPreviewsParams := v;
end;

/////////////////////////////////////////////////////////////////////////////////////

function TImageEnMIO.GetIOPreviewParams: TIOPreviewsParams;
begin
  result := fPreviewsParams;
end;


{!!
<FS>TImageEnMIO.ImageEnVersion

<FM>Declaration<FC>
property ImageEnVersion: string;

<FM>Description<FN>
This is a published property which returns the ImageEn version as string.

!!}
function TImageEnMIO.GetImageEnVersion: string;
begin
  result := IEMAINVERSION;
end;

procedure TImageEnMIO.SetImageEnVersion(Value: string);
begin
  // this is a read-only property, but it must be displayed in object inspector
end;


{$IFDEF IEINCLUDEIEXACQUIRE}

procedure TImageEnMIO.TWCloseCallBack;
begin
  fgrec := nil;
  if assigned(fOnAcquireClose) then
    fOnAcquireClose(self);
end;


{!!
<FS>TImageEnMIO.TwainAcquireOpen

<FM>Declaration<FC>
function TwainAcquireOpen : boolean;

<FM>Description<FN>
TwainAcquireOpen opens a connection to the selected scanner. It is useful to do a modeless acquisition.

TwainAcquireOpen returns False if fail.
Whenever ImageEn gets an image the <A TImageEnMIO.OnAcquireBitmap> and <A TImageEnMIO.OnAfterAcquireBitmap> events occur.

<FM>Example<FC>
ImageEnMIO.TwainAcquireOpen;
..
ImageEnMIO.TwainAcquireClose;
!!}
function TImageEnMIO.TwainAcquireOpen: boolean;
var
  tempio: TIOParamsVals;
begin
  if (not assigned(fgrec)) and assigned(fImageEnMView) then
  begin
    if assigned(fImageEnMView) then
    begin
      if (fImageEnMView as TImageEnMView).SelectedImage >= 0 then
        fTwainNextToInsert := (fImageEnMView as TImageEnMView).SelectedImage
      else
        fTwainNextToInsert := (fImageEnMView as TImageEnMView).ImageCount;
    end
    else
      fTwainNextToInsert := 0;
    fAborting := false;
    fTwainParams.FreeResources;
    tempio := TIOParamsVals.Create(nil);
    fgrec := IETWAINAcquireOpen(TWCloseCallBack, TWMultiCallBack, fTwainParams, @fTwainParams.TwainShared, tempio, fImageEnMView, fNativePixelFormat);
    FreeAndNil(tempio);
    result := fgrec <> nil;
  end
  else
    result := false;
end;

// Legacy interface to TwainAcquireOpen
function TImageEnMIO.AcquireOpen: boolean;
begin
  result := TwainAcquireOpen;
end;
{$ENDIF}

{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnMIO.TwainAcquireClose

<FM>Declaration<FC>
procedure TwainAcquireClose;

<FM>Description<FN>
TwainAcquireClose closes a connection opened with <A TImageEnMIO.TwainAcquireOpen>. It is useful to do a modeless acquisition.
Whenever ImageEn gets an image, the <A TImageEnMIO.OnAcquireBitmap> and and <A TImageEnMIO.OnAfterAcquireBitmap> event occur.

<FM>Example<FC>
ImageEnMIO.TwainAcquireOpen;
..
ImageEnMIO.TwainAcquireClose;
!!}
procedure TImageEnMIO.TwainAcquireClose;
begin
  if fgrec <> nil then
  begin
    IETWAINAcquireClose(fgrec);
    fgrec := nil;
  end;
  end;


// Legacy interface to TwainAcquireClose
procedure TImageEnMIO.AcquireClose; 
begin
  TwainAcquireClose;
end;
{$ENDIF}

procedure TImageEnMIO.DoFinishWork;
begin
  if assigned(fOnProgress) then
    fOnProgress(self, 100);
  if assigned(fOnFinishWork) then
    fOnFinishWork(self);
end;

{$IFDEF IEINCLUDEOPENSAVEDIALOGS}

{!!
<FS>TImageEnMIO.ExecuteOpenDialog

<FM>Declaration<FC>
function ExecuteOpenDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                           FilterIndex: integer = 0; const ExtendedFilters : WideString = ''; MultiSelect : boolean = False;
                           const Title : WideString = ''; const Filter : WideString = ''; DefaultFilter : <A TIOFileType> = -1;
                           LimitToFileType : <A TIOFileType> = -1) : String; overload;
function ExecuteOpenDialog(const Title : WideString; DefaultFilter : <A TIOFileType>; LimitToFileType : <A TIOFileType> = -1; AlwaysAnimate : boolean = False; MultiSelect : boolean = False) : String; overload;

<FM>Description<FN>
The ExecuteOpenDialog shows and executes the open dialog. It encapsulates the <A TOpenImageEnDialog> component.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>InitialDir</C> <C>Folder displayed on opening (leave as '' for no default)</C> </R>
<R> <C>InitialFileName</C> <C>Default file name with extension (leave as '' for no default)</C> </R>
<R> <C>AlwaysAnimate</C> <C>Enable to animate GIF and AVI (without user needing to click the play button). Default is False</C> </R>
<R> <C>FilterIndex</C> <C>The index of the default selected item in the filter (one-based). Default is 0.
Note: While this can change the first five items are generally:
  1: Common graphics formats
  2: All Graphics formats
  3: JPEG
  4: TIFF
  5: GIF

However, it is generally safer to use the <FC>DefaultFilter<FN> parameter instead</C> </R>
<R> <C>ExtendedFilters</C> <C>Any additional file formats to add to the filter (example: 'Fun Bitmap|*.fun;*.fan')</C> </R>
<R> <C>MultiSelect</C> <C>Allow selection of multiple files. The returned string will contain a list of filename separated by the "|" character (e.g. 'C:\one.jpg|C:\two.jpg')</C> </R>
<R> <C>Title</C> <C>The dialog title. If unspecified the Windows default title is used</C> </R>
<R> <C>Filter</C> <C>Override the default filter with a custom one (e.g. 'JPEG Bitmap (JPG)|*.jpg|CompuServe Bitmap (GIF)|*.gif')</C> </R>
<R> <C>DefaultFilter</C> <C>Specify the <L TIOFileType>file type</L> that is displayed by default. This setting overrides <FC>FilterIndex<FN>, but is ignored if you have specified <FC>InitialFileName<FN>. Default is -1</C> </R>
<R> <C>LimitToFileType</C> <C>Limits the filter to a specified <L TIOFileType>ImageEn file type</L>, plus "All Supported Types" and "All Files" (only relevant if Filter is not set)</C> </R>
</TABLE>

Returns a null string ('') if the user clicks Cancel.


<FM>Examples<FC>
// Prompt to load a file into an ImageEnMView
sFilename := ImageEnMView1.MIO.ExecuteOpenDialog;
if sFilename <> '' then
  ImageEnMView1.MIO.LoadFromFile(sFileName);

// Prompt to load a file, defaulting to AVI format (second overloaded method)
sFilename := ImageEnMView1.MIO.ExecuteOpenDialog('Select your video', ioAVI);
if sFilename <> '' then
  ImageEnMView1.MIO.LoadFromFile(sFileName);

// Prompt to load a file, forcing GIF format (second overloaded method)
sFilename := ImageEnMView1.MIO.ExecuteOpenDialog('Select an Image', -1, ioGIF);
if sFilename <> '' then
  ImageEnMView1.MIO.LoadFromFile(sFileName);
!!}      
function TImageEnMIO.ExecuteOpenDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False; MultiSelect : boolean = False) : String; 
begin
  Result := ExecuteOpenDialog('', '', AlwaysAnimate, 0, '', MultiSelect, Title, '', DefaultFilter, LimitToFileType);
end;

function TImageEnMIO.ExecuteOpenDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                                       FilterIndex: integer = 0; const ExtendedFilters : WideString = ''; MultiSelect : boolean = False;
                                       const Title : WideString = ''; const Filter : WideString = ''; DefaultFilter : TIOFileType = -1;
                                       LimitToFileType : TIOFileType = -1) : String;
var
  fOpenImageEnDialog: TOpenImageEnDialog;
  i: integer;
begin
  fOpenImageEnDialog := TOpenImageEnDialog.create(self);
  fOpenImageEnDialog.InitialDir := InitialDir;
  fOpenImageEnDialog.FileName := InitialFileName;
  fOpenImageEnDialog.AlwaysAnimate := AlwaysAnimate;
  fOpenImageEnDialog.PreviewBorderStyle := iepsSoftShadow;
  if Filter<>'' then
  begin
    fOpenImageEnDialog.AutoSetFilter := false;
    fOpenImageEnDialog.Filter := Filter;
  end;
  fOpenImageEnDialog.AutoSetFilterFileType := LimitToFileType;  
  fOpenImageEnDialog.FilterDefault := DefaultFilter;
  fOpenImageEnDialog.FilterIndex := FilterIndex;
  fOpenImageEnDialog.AutoAdjustDPI := AutoAdjustDPI;
  fOpenImageEnDialog.FilteredAdjustDPI := FilteredAdjustDPI;
  fOpenImageEnDialog.ExtendedFilters := ExtendedFilters;
  if MultiSelect then
    fOpenImageEnDialog.Options := fOpenImageEnDialog.Options+[ofAllowMultiSelect];
  if Title<>'' then
    fOpenImageEnDialog.Title := Title;
  result := '';
  if fOpenImageEnDialog.Execute then
  begin
    if MultiSelect then
      for i := 0 to fOpenImageEnDialog.Files.Count-1 do
      begin
        result := result+fOpenImageEnDialog.Files[i];
        if i<fOpenImageEnDialog.Files.Count-1 then
          result := result+'|';
      end
    else
      result := fOpenImageEnDialog.FileName;
  end;
  FreeAndNil(fOpenImageEnDialog);
end;

{$ENDIF}

{$IFDEF IEINCLUDEOPENSAVEDIALOGS}

{!!
<FS>TImageEnMIO.ExecuteSaveDialog

<FM>Declaration<FC>
function ExecuteSaveDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                           FilterIndex: integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                           const Filter : WideString = ''; DefaultFilter : <A TIOFileType> = -1; LimitToFileType : <A TIOFileType> = -1) : String; overload;
function ExecuteSaveDialog(const Title : WideString; DefaultFilter : <A TIOFileType>; LimitToFileType : <A TIOFileType> = -1; AlwaysAnimate : boolean = False) : String; overload;

<FM>Description<FN>
The ExecuteSaveDialog shows and executes the save dialog. It encapsulates the <A TSaveImageEnDialog> component.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>InitialDir</C> <C>Folder displayed on opening (leave as '' for no default)</C> </R>
<R> <C>InitialFileName</C> <C>Default file name with extension (leave as '' for no default)</C> </R>
<R> <C>AlwaysAnimate</C> <C>Enable to animate GIF and AVI (without user needing to click the play button). Default is False</C> </R>
<R> <C>FilterIndex</C> <C>The index of the default selected item in the filter (one-based). Default is 0.
Note: While this can change the first five items are generally:
  1: Common graphics formats
  2: All Graphics formats
  3: JPEG
  4: TIFF
  5: GIF

However, it is generally safer to use the <FC>DefaultFilter<FN> parameter instead</C> </R>
<R> <C>ExtendedFilters</C> <C>Any additional file formats to add to the filter (example: 'Fun Bitmap|*.fun;*.fan')</C> </R>
<R> <C>Title</C> <C>The dialog title. If unspecified the Windows default title is used</C> </R>
<R> <C>Filter</C> <C>Override the default filter with a custom one (e.g. 'JPEG Bitmap (JPG)|*.jpg|CompuServe Bitmap (GIF)|*.gif')</C> </R> 
<R> <C>DefaultFilter</C> <C>Specify the <L TIOFileType>file type</L> that is displayed by default. This setting overrides <FC>FilterIndex<FN>, but is ignored if you have specified <FC>InitialFileName<FN>. Default is -1</C> </R>
<R> <C>LimitToFileType</C> <C>Limits the filter to a specified <L TIOFileType>ImageEn file type. Default is -1<</L></C> </R>
</TABLE>

Returns a null string ('') if the user clicks Cancel.


<FM>Examples<FC>
// Prompt to save a file in an ImageEnMView
sFilename := ImageEnMView1.MIO.ExecuteSaveDialog;
if sFilename <> '' then
  ImageEnMView1.MIO.SaveToFile(sFileName);

// Prompt to save a file, defaulting to TIFF format (second overloaded method)
sFilename := ImageEnMView1.MIO.ExecuteSaveDialog('Save your File', ioTIFF);
if sFilename <> '' then
  ImageEnMView1.MIO.SaveToFile(sFileName);

// Prompt to save a file, forcing GIF format (second overloaded method)
sFilename := ImageEnMView1.MIO.ExecuteSaveDialog('Save your File', -1, ioGIF);
if sFilename <> '' then
  ImageEnMView1.MIO.SaveToFile(sFileName);
!!}       
function TImageEnMIO.ExecuteSaveDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False) : String; 
begin
  Result := ExecuteSaveDialog('', '', AlwaysAnimate, 0, '', Title, '', DefaultFilter, LimitToFileType);
end;

function TImageEnMIO.ExecuteSaveDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                                       FilterIndex: integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                                       const Filter : WideString = ''; DefaultFilter : TIOFileType = -1; LimitToFileType : TIOFileType = -1) : String;
var
  fSaveImageEnDialog: TSaveImageEnDialog;
begin
  fSaveImageEnDialog := TSaveImageEnDialog.create(self);
  fSaveImageEnDialog.InitialDir := InitialDir;
  fSaveImageEnDialog.FileName := TFileName(InitialFileName);
  fSaveImageEnDialog.AlwaysAnimate := AlwaysAnimate;
  fsaveImageEnDialog.AttachedImageEnIO := self;
  fSaveImageEnDialog.PreviewBorderStyle := iepsSoftShadow;
  if Filter<>'' then
  begin
    fSaveImageEnDialog.AutoSetFilter := false;
    fSaveImageEnDialog.Filter := Filter;
  end;
  fSaveImageEnDialog.AutoSetFilterFileType := LimitToFileType;  
  fSaveImageEnDialog.FilterDefault := DefaultFilter;
  fSaveImageEnDialog.FilterIndex := FilterIndex;
  fSaveImageEnDialog.AutoAdjustDPI := AutoAdjustDPI;
  fSaveImageEnDialog.FilteredAdjustDPI := FilteredAdjustDPI;
  fSaveImageEnDialog.ExtendedFilters := ExtendedFilters;
  if Title<>'' then
    fSaveImageEnDialog.Title := Title;
  if fSaveImageEnDialog.Execute then
    result := fSaveImageEnDialog.FileName
  else
    result := '';
  FreeAndNil(fSaveImageEnDialog);
end;

{$ENDIF}


{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnMIO.TwainParams

<FM>Declaration<FC>
property TwainParams: <A TIETwainParams>;

<FM>Description<FN>
Use the TwainParams property to control acquisition from Twain scanners and cameras. You can enable/disable standard user interface, set pixeltype (Grayscale, RGB...), DPI, etc.

Note: TwainParams is only relevant when <A TImageEnMIO.SelectedAcquireSource>.Api is ieaTwain.

<FM>See Also<FN>
- <A TIETwainParams>
- <A TImageEnMIO.AcquireParams>
- <A TImageEnMIO.SelectedAcquireSource>

<FM>Example<FC>
// Acquires a black/white (1bit) image from the default Twain device
ImageEnMView1.MIO.TwainParams.PixelType.CurrentValue := 0;
ImageEnMView1.MIO.TwainParams.VisibleDialog := False;
ImageEnMView1.MIO.SetSource(ieaTwain, Default_Device);
ImageEnMView1.MIO.Acquire;
!!}


{!!
<FS>TImageEnMIO.WIAParams

<FM>Declaration<FC>
property WIAParams: <A TIEWia>;

<FM>Description<FN>
WIAParams allows you to access parameters, show dialogs and control WIA devices.

Note: Use WIAParams only when you need access to WIA specific functionality (when <A TImageEnMIO.SelectedAcquireSource>.Api is ieaWIA). For generic access to all image acquisitions sources (Twain, WIA, etc.) use <A TImageEnMIO.AcquireParams> instead.


<FM>See Also<FN>
- <A TIEWia>
- <A TImageEnMIO.AcquireParams>
- <A TImageEnMIO.SelectedAcquireSource>


<FM>Example<FC>
// Acquires an image with a horizontal resolution of 150 dpi from the default WIA device
ImageEnMView1.MIO.SetItemProperty(WIA_IPS_XRES, 150);
if ImageEnMView1.MIO.SetSource(ieaWIA, Default_Device) then
  ImageEnMView1.MIO.Acquire;
!!}
function TImageEnMIO.GetWIAParams: TIEWia;
begin
  if not assigned(fWIA) then
  begin
    fWIA := TIEWia.Create(self);
    fWIA.OnProgress := WiaOnProgress;
  end;
  result := fWIA;
end;

function TImageEnMIO.WiaOnProgress(Percentage: integer): boolean;
begin
  if assigned(fOnProgress) then
    fOnProgress(self, Percentage);
  result := not fAborting;
end;

{$ENDIF}  // end of IEINCLUDEIEXACQUIRE

{!!
<FS>TImageEnMIO.SaveToStreamPS

<FM>Declaration<FC>
procedure SaveToStreamPS(Stream: TStream);

<FM>Description<FN>
Save all images in the connected TImageEnMView to a stream in PostScript format (or only those images selected if <FC>SelectedOnly<FN> = True).
!!}
procedure TImageEnMIO.SaveToStreamPS(Stream: TStream; SelectedOnly: Boolean = False);
var
  Param: TIOParamsVals;
  Progress: TProgressRec;
  bmp: TIEBitmap;
  nullpr: TProgressRec;
  han: pointer;
 
  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    Param := Params[ImgIdx];
    if FileIdx = 0 then
      han := IEPostScriptCreate(Stream, Param);
    (fImageEnMView as TImageEnMView).CopyToIEBitmap(ImgIdx, bmp);
    bmp.DefaultDitherMethod := fDefaultDitherMethod;
    IEPostScriptSave(han, Stream, bmp, Param, nullpr);
    
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i, imgCount: Integer;
begin
  try
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;

    if SelectedOnly then
      imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
    else
      imgCount := (fImageEnMView as TImageEnMView).ImageCount;
    if imgCount = 0 then
      exit;

    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    Progress.per1 := 100 / imgCount;
    bmp := TIEBitmap.Create;
    han := nil;

    i := 0;
    while not fAborting and (i < imgCount) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;

    IEPostScriptClose(han, Stream);
    FreeAndNil(bmp);
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnMIO.SaveToFilePS

<FM>Declaration<FC>
procedure SaveToFilePS(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Creates a multi-page PostScript file with all images in the connected TImageEnMView (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// load a multipage TIFF and save back to a multipage PostScript file
ImageEnMView.MIO.LoadFromFile('C:\multipage.tiff');
ImageEnMView.MIO.SaveToFilePS('D:\output.ps');
!!}
procedure TImageEnMIO.SaveToFilePS(const FileName: string; SelectedOnly: Boolean = False);
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStreamPS(fs, SelectedOnly);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.SaveToStreamPDF

<FM>Declaration<FC>
procedure SaveToStreamPDF(Stream : TStream; SelectedOnly: boolean = false);

<FM>Description<FN>
Creates a multipage Adobe PDF file with all images in the connected TImageEnMView (or only those images selected if <FC>SelectedOnly<FN> = True).

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnMIO.SaveToStreamPDF(Stream: TStream; SelectedOnly : boolean = false);
var
  Param: TIOParamsVals;
  Progress: TProgressRec;
  bmp: TIEBitmap;
  nullpr: TProgressRec;
  han: pointer;
  imgCount: integer;

  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    Param := Params[ImgIdx];
    if FileIdx = 0 then
      han := IEPDFCreate(Param);
    (fImageEnMView as TImageEnMView).CopyToIEBitmap(ImgIdx, bmp);
    bmp.DefaultDitherMethod := fDefaultDitherMethod;
    IEPDFSave(han, bmp, Param, nullpr);
    
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i: integer;
begin
  try
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;

    if SelectedOnly then
      imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
    else
      imgCount := (fImageEnMView as TImageEnMView).ImageCount;

    if imgCount = 0 then
      exit;
    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    Progress.per1 := 100 / imgCount;
    bmp := TIEBitmap.Create;
    han := nil;

    i := 0;
    while not fAborting and (i < imgCount) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;
    if imgCount > 0 then
      Param := Params[0];
    IEPDFClose(han, Stream, Param);
    FreeAndNil(bmp);
  finally
    DoFinishWork;
  end;
end;
{$endif}

{!!
<FS>TImageEnMIO.SaveToFilePDF

<FM>Declaration<FC>
procedure SaveToFilePDF(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Creates a multipage Adobe PDF file with all images in the connected TImageEnMView (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// load a multipage TIFF and save back to a multipage PDF file
ImageEnMView.MIO.LoadFromFile('C:\multipage.tiff');
ImageEnMView.MIO.SaveToFilePDF('D:\output.pdf');
!!}
{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnMIO.SaveToFilePDF(const FileName: string; SelectedOnly: Boolean = False);
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStreamPDF(fs, SelectedOnly);
  finally
    FreeAndNil(fs);
  end;
end;
{$endif}



{$IFDEF IEINCLUDEPRINTDIALOGS}

{!!
<FS>TImageEnMIO.DoPrintPreviewDialog

<FM>Declaration<FC>
function DoPrintPreviewDialog(const TaskName: string; PrintAnnotations: boolean; const Caption: string; ThumbnailPrinting: Boolean): boolean;

<FM>Description<FN>
Executes the multi-page print preview dialog. This function is like DoPrintPreviewDialog of <A TImageEnIO>, but allows working with multiple pages.
If <FC>ThumbnailPrinting<FN> is true then it defaults to printing of thumbnails, otherwise the default is read from <A TImageEnMIO.PrintPreviewParams>.
If <FC>PrintAnnotation<FN> is true and the image contains Imaging Annotations they will be printed out.
<FC>Caption<FN> specifies the caption of preview dialog.

Note: The language used in the dialog is controlled by <A TIEImageEnGlobalSettings.MsgLanguage>. The styling can also be adjusted using <A TIEImageEnGlobalSettings.UseButtonGlyphsInDialogs>

<FM>See Also<FN>
- <A TImageEnMIO.PrintPreviewParams>
- <A TImageEnMIO.DialogsMeasureUnit>

<FM>Example<FC>
ImageEnMView1.MIO.DoPrintPreviewDialog('');
<IMG help_images\74.bmp>
!!}
function TImageEnMIO.DoPrintPreviewDialog(const TaskName: string; PrintAnnotations: boolean; const Caption: string; ThumbnailPrinting: Boolean): boolean;
var
  fieprnform: tfieprnform3;
begin
  if fResetPrinter then
  try
    IEResetPrinter();
  except
     MessageDlg('The Print Preview could not be displayed because a printer has not been configured on this computer.', mtError, [mbOK], 0);
     result := false;
     exit;
  end;

  fieprnform := TfiePrnForm3.Create(self);
  try
    fieprnform.mio := self;

    if fPreviewFontEnabled then
      fieprnform.Font.Assign(fPreviewFont)
    else
      fieprnform.Font.Assign(IEGetDefaultDialogFont);

    fieprnform.fTaskName := TaskName;
    fieprnform.fDialogsMeasureUnit := fDialogsMeasureUnit;
    fieprnform.fPrintPreviewParams := fPrintPreviewParams;
    if ThumbnailPrinting then
      fieprnform.fPrintPreviewParams.PrintThumbnails := true;
    fieprnform.PrintAnnotations := PrintAnnotations;
    if Caption <> '' then
      fieprnform.DialogCaption := Caption
    else
      fieprnform.DialogCaption := iemsg(IEMSG_PRINT);

    fieprnform.UpdateLanguage();

    result := fieprnform.ShowModal = mrOk;
  finally
    fieprnform.Release;
  end;
end;
{$ENDIF}


{!!
<FS>TImageEnMIO.LoadFromFileICO

<FM>Declaration<FC>
function LoadFromFileICO(const FileName: string): Boolean;

<FM>Description<FN>
Load all images contained in a multi-image ICO file.

Result will be false if the file is not ICO format (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.
!!}
function TImageEnMIO.LoadFromFileICO(const FileName: string): Boolean;
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fLoadingFileName := FileName;
    Result := LoadFromStreamICO(fs);
  finally
    fLoadingFileName := '';
    fLastFilename := Filename;
    FreeAndNil(fs);
  end;
end;


{!!
<FS>TImageEnMIO.LoadFromStreamICO

<FM>Declaration<FC>
function LoadFromStreamICO(Stream: TStream): Boolean;

<FM>Description<FN>
Load all images contained in a multi-image ICO stream. The result will be false if an error is encountered, e.g. the file in the stream is not ICO format (<A TImageEnMIO.Aborting> will be true).

!!}
function TImageEnMIO.LoadFromStreamICO(Stream: TStream): Boolean;
var
  p1: int64;
  bmp: TIEBitmap;
  numi, idx: integer;
  Progress, Progress2: TProgressRec;
  Param: TIOParamsVals;
  im: integer;
  tempAlphaChannel: TIEMask;
begin
  Result := False;
  fAborting := False;
  if not assigned(fImageEnMView) then
    exit;
  try
    (fImageEnMView as TImageEnMView).LockPaint;
    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    Progress2.fOnProgress := nil;
    Progress2.Sender := nil;
    Progress2.Aborting := @fAborting;
    p1 := Stream.Position;

    idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx=-1 then
      idx := (fImageEnMView as TImageEnMView).ImageCount;

    im := 0;
    numi := _EnumICOImStream(Stream);
    repeat
      (fImageEnMView as TImageEnMView).InsertImageEx(idx);
      bmp := TIEBitmap.Create;
      try
        Param := Params[idx];
        Stream.position := p1;
        Param.ICO_ImageIndex := im;
        Param.IsNativePixelFormat := fNativePixelFormat;
        tempAlphaChannel := nil;
        ICOReadStream(Stream, bmp, Param, false, Progress2, tempAlphaChannel, false);
        if assigned(tempAlphaChannel) then
        begin
          bmp.AlphaChannel.CopyFromTIEMask(tempAlphaChannel);
          FreeAndNil(tempAlphaChannel);
        end;

        Param.ImageIndex := idx;
        Param.FileType := ioICO;
        Param.FileName := WideString(fLoadingFileName);
        if fAborting then
        begin
          (fImageEnMView as TImageEnMView).DeleteImage(idx);
          break;
        end;
        if numi = 0 then
          (fImageEnMView as TImageEnMView).DeleteImage(idx)
        else
        begin
          Progress.per1 := 100 / numi;
          (fImageEnMView as TImageEnMView).SetIEBitmapEx(idx, bmp);
          (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := 100;
        end;
      finally
        FreeAndNil(bmp);
      end;
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * im));
      if fAborting then
        break;
      inc(idx);
      inc(im);
    until im >= numi;
    Update;
  finally
    (fImageEnMView as TImageEnMView).UnLockPaint;
    DoFinishWork;
  end;
  Result := Not fAborting;
end;


procedure TImageEnMIO.CheckDPI(p: TIOParamsVals);
begin
  if p.DpiX < 2 then
    p.DpiX := IEGlobalSettings().DefaultDPIX;
  if p.DpiY < 2 then
    p.DpiY := IEGlobalSettings().DefaultDPIY;
end;

{!!
<FS>TImageEnMIO.LoadFromFileDCX

<FM>Declaration<FC>
function LoadFromFileDCX(const FileName: string): Boolean;

<FM>Description<FN>
Loads all images contained in a multi-image DCX file.

Result will be false if the file is not DCX format (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

!!}
function TImageEnMIO.LoadFromFileDCX(const FileName: string): Boolean;
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fLoadingFileName := FileName;
    Result := LoadFromStreamDCX(fs);
  finally
    fLoadingFileName := '';
    fLastFilename := Filename;
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.LoadFromStreamDCX

<FM>Declaration<FC>
function LoadFromStreamDCX(Stream: TStream): Boolean;

<FM>Description<FN>
Loads all images contained in a multi-image DCX stream.

The result will be false if an error is encountered, e.g. the file in the stream is not DCX format (<A TImageEnMIO.Aborting> will be true).

!!}
function TImageEnMIO.LoadFromStreamDCX(Stream: TStream): Boolean;
var
  p1: int64;
  xbmp, bmp: TIEBitmap;
  numi, idx: integer;
  Progress, Progress2: TProgressRec;
  Param: TIOParamsVals;
  im: integer;
begin             
  Result := False;
  fAborting := False;
  if not assigned(fImageEnMView) then
    exit;
  try
    (fImageEnMView as TImageEnMView).LockPaint;
    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    Progress2.fOnProgress := nil;
    Progress2.Sender := nil;
    Progress2.Aborting := @fAborting;
    p1 := Stream.Position;

    //idx := imax((fImageEnMView as TImageEnMView).SelectedImage, 0);
    idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx=-1 then
      idx := (fImageEnMView as TImageEnMView).ImageCount;

    numi := IEDCXCountStream(Stream);
    for im := 0 to numi-1 do
    begin
      (fImageEnMView as TImageEnMView).InsertImageEx(idx);
      bmp := TIEBitmap.Create;
      Param := Params[idx];
      Stream.position := p1;
      Param.DCX_ImageIndex := im;
      Param.IsNativePixelFormat := fNativePixelFormat;
      IEDCXReadStream(Stream, bmp, Param, Progress2, false);
      CheckDPI(Param);
      if fAutoAdjustDPI then
        xbmp := IEAdjustDPI(bmp, Param, fFilteredAdjustDPI)
      else
        xbmp := bmp;
      if bmp <> xbmp then
        FreeAndNil(bmp);
      bmp := xbmp;
      
      Param.ImageIndex := idx;
      Param.FileType := ioDCX;
      Param.FileName := WideString(fLoadingFileName);
      if fAborting then
      begin
        (fImageEnMView as TImageEnMView).DeleteImage(idx);
        FreeAndNil(bmp);
        break;
      end;
      Progress.per1 := 100 / numi;
      (fImageEnMView as TImageEnMView).SetIEBitmapEx(idx, bmp);
      (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := 100;
      //if xbmp=bmp then xbmp := nil;
      FreeAndNil(bmp);
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * im));
      if fAborting then
        break;
      inc(idx);
    end;
    Update;
  finally
    (fImageEnMView as TImageEnMView).UnLockPaint;
    DoFinishWork;
  end;
  Result := Not fAborting;
end;

{!!
<FS>TImageEnMIO.SaveToFileDCX

<FM>Declaration<FC>
procedure SaveToFileDCX(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView as a DCX file (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnMIO.SaveToFileDCX(const FileName: string; SelectedOnly: Boolean = False);
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStreamDCX(fs, SelectedOnly);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.SaveToStreamDCX

<FM>Declaration<FC>
procedure SaveToStreamDCX(Stream: TStream; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView to a stream in DCX format (or only those images selected if <FC>SelectedOnly<FN> = True).
!!}
procedure TImageEnMIO.SaveToStreamDCX(Stream: TStream; SelectedOnly: Boolean = False);
var
  p1: int64;
  Param: TIOParamsVals;
  Progress: TProgressRec;
  bmp: TIEBitmap;
  nullpr: TProgressRec;
       
  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    Param := Params[ImgIdx];
    Param.DCX_ImageIndex := FileIdx;
    Stream.position := p1;
    (fImageEnMView as TImageEnMView).CopyToIEBitmap(ImgIdx, bmp);
    bmp.DefaultDitherMethod := fDefaultDitherMethod;
    IEDCXInsertStream(Stream, bmp, Param, NullPr);
    
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i, imgCount: Integer;
begin
  try
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;

    if SelectedOnly then
      imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
    else
      imgCount := (fImageEnMView as TImageEnMView).ImageCount;
    if imgCount = 0 then
      exit;

    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    p1 := Stream.Position;
    Progress.per1 := 100 / imgCount;
    bmp := TIEBitmap.Create;

    i := 0;
    while not fAborting and (i < imgCount) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;

    FreeAndNil(bmp);
  finally
    DoFinishWork;
  end;
end;

{$ifdef IEINCLUDEDICOM}

{!!
<FS>TImageEnMIO.LoadFromFileDICOM

<FM>Declaration<FC>
function LoadFromFileDICOM(const FileName: string): Boolean;

<FM>Description<FN>
Loads a DICOM image or a multipage DICOM. This method is necessary when the DICOM file hasn't extension and hasn't a valid DICOM header, but you are sure that is a DICOM file.

Result will be false if the file is not DICOM format (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

Note: DICOM parameters are stored in <A TIOParamsVals.DICOM_Tags>.

<FM>Example<FC>
ImageEnMView1.MIO.LoadFromFileDICOM('heart-sequence');

!!}
function TImageEnMIO.LoadFromFileDICOM(const FileName: string): Boolean;
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fLoadingFileName := FileName;
    Result := LoadFromStreamDICOM(fs);
  finally
    fLoadingFileName := '';
    fLastFilename := Filename;
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.LoadFromStreamDICOM

<FM>Declaration<FC>
function TImageEnMIO.LoadFromStreamDICOM(Stream: TStream): Boolean;

<FM>Description<FN>
Loads a DICOM image or a sequence of images from stream.  The result will be false if an error is encountered, e.g. the file in the stream is not DICOM format (<A TImageEnMIO.Aborting> will be true).

Note: DICOM parameters are stored in <A TIOParamsVals.DICOM_Tags>.
!!}
function TImageEnMIO.LoadFromStreamDICOM(Stream: TStream): Boolean;
var
  p1: int64;
  xbmp, bmp: TIEBitmap;
  numi, idx: integer;
  Progress, Progress2: TProgressRec;
  Param: TIOParamsVals;
  im: integer;
  readContext: TDICOMReadContext;
begin
  Result := False;
  fAborting := False;
  if not assigned(fImageEnMView) then
    exit;

  readContext := nil;

  try
    (fImageEnMView as TImageEnMView).LockPaint;
    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    Progress2.fOnProgress := nil;
    Progress2.Sender := nil;
    Progress2.Aborting := @fAborting;
    p1 := Stream.Position;

    idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx = -1 then
      idx := (fImageEnMView as TImageEnMView).ImageCount;

    numi := IEDicomImageCount(Stream);
    for im := 0 to numi - 1 do
    begin
      (fImageEnMView as TImageEnMView).InsertImageEx(idx);
      Param := Params[idx];
      Stream.position := p1;
      Param.ImageIndex := im;
      Param.IsNativePixelFormat := fNativePixelFormat;
      bmp := TIEBitmap.Create();
      try
        if im = 0 then
        begin
          readContext := TDICOMReadContext.Create(Stream, Progress2);
          readContext.ReadHeader();
          readContext.ReadTags(Param);
        end;
        readContext.GetImage(bmp, Param, false);
      except
        bmp.Free();
      end;
      CheckDPI(Param);
      if fAutoAdjustDPI then
        xbmp := IEAdjustDPI(bmp, Param, fFilteredAdjustDPI)
      else
        xbmp := bmp;
      if bmp <> xbmp then
        FreeAndNil(bmp);
      bmp := xbmp;

      Param.ImageIndex := idx;
      Param.FileType := ioDICOM;
      Param.FileName := WideString(fLoadingFileName);
      if fAborting then
      begin
        (fImageEnMView as TImageEnMView).DeleteImage(idx);
        FreeAndNil(bmp);
        break;
      end;
      Progress.per1 := 100 / numi;
      (fImageEnMView as TImageEnMView).SetIEBitmapEx(idx, bmp);
      (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := 100;
      FreeAndNil(bmp);
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * im));
      if fAborting then
        break;
      inc(idx);
    end;
    Update;
  finally
    readContext.Free();
    (fImageEnMView as TImageEnMView).UnLockPaint();
    DoFinishWork();
  end;
  Result := Not fAborting;
end;

{!!
<FS>TImageEnMIO.SaveToFileDICOM

<FM>Declaration<FC>
procedure SaveToFileDICOM(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView as a Dicom file (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnMIO.SaveToFileDICOM(const FileName: string; SelectedOnly: Boolean = False);
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStreamDICOM(fs, SelectedOnly);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.SaveToStreamDICOM

<FM>Declaration<FC>
procedure SaveToStreamDICOM(Stream: TStream; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView to a stream in Dicom format (or only those images selected if <FC>SelectedOnly<FN> = True).
!!}
procedure TImageEnMIO.SaveToStreamDICOM(Stream: TStream; SelectedOnly: Boolean = False);
var
  Param: TIOParamsVals;
  Progress: TProgressRec;
  bmp: TIEBitmap;
  nullpr: TProgressRec;
  ParamBitsPerSample, ParamSamplesPerPixel, ParamWidth, ParamHeight: integer;
  context: pointer;
  
  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    Param := Params[ImgIdx];
    Param.ImageIndex := FileIdx;
    (fImageEnMView as TImageEnMView).CopyToIEBitmap(ImgIdx, bmp);
    bmp.DefaultDitherMethod := fDefaultDitherMethod;

    case bmp.PixelFormat of
      ie1g:
        begin
          ParamBitsPerSample   := 1;
          ParamSamplesPerPixel := 1;
        end;
      ie8g:
        begin
          ParamBitsPerSample   := 8;
          ParamSamplesPerPixel := 1;
        end;
      ie16g:
        begin
          ParamBitsPerSample   := 16;
          ParamSamplesPerPixel := 1;
        end;
      ie24RGB:
        begin
          ParamBitsPerSample   := 8;
          ParamSamplesPerPixel := 3;
        end;
      else
        raise EIEException.Create('DICOM saving: unsupported pixel format');
    end;
    ParamWidth  := bmp.Width;
    ParamHeight := bmp.Height;

    if FileIdx = 0 then
    begin
      Param.BitsPerSample   := ParamBitsPerSample;
      Param.SamplesPerPixel := ParamSamplesPerPixel;
      Param.Width  := ParamWidth;
      Param.Height := ParamHeight;
      context := IEDicomWrite_init(Stream, Param, (fImageEnMView as TImageEnMView).ImageCount, Progress);
    end
    else
    if (Param.BitsPerSample <> ParamBitsPerSample) or (Param.SamplesPerPixel <> ParamSamplesPerPixel) or (Param.Width <> ParamWidth) or (Param.Height <> ParamHeight) then
      raise EIEException.Create('DICOM saving: different images');

    IEDicomWrite_addImage(context, bmp);

    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i, imgCount: Integer;
begin
  ParamBitsPerSample   := 0;
  ParamSamplesPerPixel := 0;
  context := nil;
  bmp := nil;
  try
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;

    if SelectedOnly then
      imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
    else
      imgCount := (fImageEnMView as TImageEnMView).ImageCount;
    if imgCount = 0 then
      exit;

    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    Progress.per1 := 100 / imgCount;
    bmp := TIEBitmap.Create;

    i := 0;
    while not fAborting and (i < imgCount) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;

  finally
    IEDicomWrite_finalize(context);
    FreeAndNil(bmp);
    DoFinishWork;
  end;
end;

{$endif}


{$ifdef IEINCLUDEDIRECTSHOW}
{!!
<FS>TImageEnMIO.LoadFromMediaFile

<FM>Declaration<FC>
procedure LoadFromMediaFile(const FileName: string); dynamic;

<FM>Description<FN>
Loads a video using DirectShow. When LoadFromFileAVI fails, you should try this method.
Also use LoadFromMediaFile to load DirectShow supported files, like wmv, mpeg, avi.

!!}
function TImageEnMIO.LoadFromMediaFile(const FileName: string) : Boolean;
var
  dshow: TIEDirectShow;
  idx, i, l: integer;
  Progress: TProgressRec;
  Param: TIOParamsVals;
  bmp: TIEBitmap;
  rate: double;
  avgtime, ltime: int64;
  mul: integer;
begin   
  Result := False;
  if not assigned(fImageEnMView) then
    exit;
  fAborting := False;
  Progress.fOnProgress := fOnProgress;
  Progress.Sender := Self;
  Progress.Aborting := @fAborting;
  bmp := TIEBitmap.Create;
  dshow := TIEDirectShow.Create;
  try
    (fImageEnMView as TImageEnMView).LockPaint;

    dshow.FileInput := AnsiString(FileName);
    dshow.EnableSampleGrabber := true;
    dshow.Connect;
    dshow.Pause;

    dshow.TimeFormat := tfTime;
    ltime := dshow.Duration;
    if (ltime=0) then
    begin
      fAborting := true;
      exit;
    end;
    dshow.TimeFormat := tfFrame;
    l := dshow.Duration;
    avgtime := dshow.GetAverageTimePerFrame;

    idx := (fImageEnMView as TImageEnMView).SelectedImage;
    if idx=-1 then
      idx := (fImageEnMView as TImageEnMView).ImageCount;

    if l=ltime then
    begin
      l := l div avgtime;
      mul := avgtime;
    end
    else
      mul := 1;
    rate := (ltime/10000000)/l*100;
    Progress.per1 := 100 / l;
    for i := 0 to l-1 do
    begin
      (fImageEnMView as TImageEnMView).InsertImageEx(idx);
      Param := Params[idx];
      
      Param.ImageIndex := idx;
      Param.FileType := ioUnknown;
      Param.FileName := WideString(FileName);
      Param.IsNativePixelFormat := fNativePixelFormat;

      dshow.position := int64(i) * int64(mul);
      dshow.GetSample( bmp );

      (fImageEnMView as TImageEnMView).SetIEBitmapEx(idx, bmp);
      (fImageEnMView as TImageEnMView).ImageDelayTime[idx] := trunc(rate);

      if i = 0 then
        with (fImageEnMView as TImageEnMView) do
          if StoreType=ietNormal then
            PrepareSpaceFor(bmp.Width, bmp.Height, bmp.BitCount, l - 1)  // full size
          else
            PrepareSpaceFor(ThumbWidth, ThumbHeight, bmp.BitCount, l - 1); // thumbnails

      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * i));
      if fAborting then
        break;
      inc(idx);
    end;

    dshow.Disconnect;
    Update;
  finally
    fLastFilename := Filename;
    FreeAndNil(dshow);
    FreeAndNil(bmp);
    (fImageEnMView as TImageEnMView).UnLockPaint;
    DoFinishWork;
  end;
  Result := Not fAborting;
end;
{$endif}

{!!
<FS>TImageEnMIO.Params

<FM>Declaration<FC>
property Params[idx: integer]: <A TIOParamsVals>;

<FM>Description<FN>
This property contains a TIOParamsVals object for the idx image. This one contains some parameters (as bits per sample, type of compression, etc) of each image file format. The parameters are updated when you load files or streams. Also you can modify these parameters before saving images.

Read-only

!!}
function TImageEnMIO.GetParams(idx: integer): TIOParamsVals;
begin
  if (idx >= 0) and (idx < fParams.Count) then
    result := TIOParamsVals(fParams[idx])
  else
    result := nil;
end;

procedure TImageEnMIO.RemoveIOParam(idx: integer);
begin
  if idx<fParams.Count then
  begin
    TIOParamsVals(fParams[idx]).free;
    fParams.Delete(idx);
  end;
end;

procedure TImageEnMIO.InsertIOParam(idx: integer);
var
  iop: TIOParamsVals;
begin
  iop := TIOparamsVals.Create(nil);
  if fParams.Count > 0 then
  begin
    if idx >= fParams.Count then
      iop.AssignCompressionInfo(TIOParamsVals(fParams[idx - 1]))
    else
      iop.AssignCompressionInfo(TIOParamsVals(fParams[idx]));
  end;
  if idx < fParams.Count then
    // insert
    fParams.Insert(idx, iop)
  else
    // add
    fParams.Add(iop);
  // sort image indexes
  (*
  for q := idx to ParamsCount - 1 do
  begin
    Params[q].TIFF_ImageIndex := q;
    Params[q].GIF_ImageIndex := q;
    Params[q].DCX_ImageIndex := q;
  end;
  *)
end;

procedure TImageEnMIO.MoveIOParams(idx: integer; destination: integer);
begin
  if (idx >= 0) and (idx < fParams.Count) and (destination >= 0) and (destination <> idx) then
  begin
    if destination >= fParams.Count then
    begin
      fParams.Add(fParams[idx]);
      fParams.Delete(idx);
    end
    else
      fParams.Move(idx, destination);
  end;
end;

procedure TImageEnMIO.SwapIOParams(idx1, idx2: integer);
var
  tmp: pointer;
begin
  tmp := fParams[idx1];
  fParams[idx1] := fParams[idx2];
  fParams[idx2] := tmp;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// load from url


{!!
<FS>TImageEnMIO.LoadFromURL

<FM>Declaration<FC>
procedure LoadFromURL(URL: string);

<FM>Description<FN>
Loads a multipage file from the network using the HTTP or FTP protocol, specifing the URL.
This function doesn't support password authentication for HTTP, while it is necessary for FTP (also conntecting to anonymous server).

URL must have the syntax:
'http://domain[:port]/resource'
'https://domain[:port]/resource'
'ftp://user:password@domain[:port]/resource'

It is possible to set proxy parameters using <A TImageEnMIO.ProxyAddress>, <A TImageEnMIO.ProxyUser> and <A TImageEnMIO.ProxyPassword> properties.

<FM>Example<FC>
// load from standard port 80
ImageEnMView.MIO.LoadFromURL('http://www.imageen.com/image.gif');

// load from port 8080
ImageEnMView.MIO.LoadFromURL('http://www.imageen.com:8080/image.gif');

// load from FTP
ImageEnMView.MIO.LoadFromURL('ftp://space:shuttle@ftp.imageen.com/Pictures/test.jpg')

!!}
function TImageEnMIO.LoadFromURL(URL: string): Boolean;
var
  ms: TMemoryStream;
  ft: TIOFileType;
  tempf: string;
  fileext: string;
begin      
  Result := False;
  ms := TMemoryStream.Create();
  try
    if not IEGetFromURL(URL, ms, fProxyAddress, fProxyUser, fProxyPassword, fOnProgress, self, @fAborting, FileExt) then
    begin
      fAborting := true;
      DoFinishWork();
    end
    else
    begin
      fLoadingFileName := URL;
      ms.Position := 0;
      if not fAborting then
      begin
        ft := FindStreamFormat(ms);
        case ft of
          ioGIF   : Result := LoadFromStreamGIF(ms);
          ioTIFF  : Result := LoadFromStreamTIFF(ms);
          ioICO   : Result := LoadFromStreamICO(ms);
          ioDCX   : Result := LoadFromStreamDCX(ms);
          {$ifdef IEINCLUDEDICOM}
          ioDICOM : Result := LoadFromStreamDICOM(ms);
          {$endif}
          else
          begin
            // format not loadable from stream, need to save in a temporary file
            tempf := string(IEGetTempFileName2) + '.' + fileExt;
            ms.SaveToFile(tempf);
            FreeAndNil(ms);
            Result := LoadFromFile(tempf);
            DeleteFile(tempf);
          end;
        end;
      end;
    end;

  finally
    fLoadingFileName := '';
    if assigned(ms) then
      FreeAndNil(ms);
  end;
end;

// load from url
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMIO.LoadFromFileFormat

<FM>Declaration<FC>
function LoadFromFileFormat(const FileName: string; FileFormat: <A TIOFileType>): Boolean;

<FM>Description<FN>
Loads an image from file. LoadFromFileFormat recognizes the file format from FileFormat parameter.

FileName is the file name. Result will be false if the file is not a recognized file type (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

!!}
function TImageEnMIO.LoadFromFileFormat(const FileName: string; FileFormat: TIOFileType): Boolean;
var
  idx: Integer;
begin
  case FileFormat of
    ioGIF   : Result := LoadFromFileGIF(FileName);
    ioTIFF  : Result := LoadFromFileTIFF(FileName);
    ioAVI   : Result := LoadFromFileAVI(FileName);
    ioICO   : Result := LoadFromFileICO(FileName);
    ioDCX   : Result := LoadFromFileDCX(FileName);
    {$ifdef IEINCLUDEDICOM}
    ioDICOM : Result := LoadFromFileDICOM(FileName);
    {$endif}
    else      // NOT SUPPORTED MULTI-FRAME FORMAT
              begin        
                fAborting := False;

                // Fall back to loading as a single image if the extension is recognized
                if assigned(fImageEnMView) and IsKnownFormat(WideString(FileName), False) then
                begin
                  // try single image
                  try
                    idx := (fImageEnMView as TImageEnMView).SelectedImage;
                    if idx = -1 then
                      idx := (fImageEnMView as TImageEnMView).ImageCount;

                    (fImageEnMView as TImageEnMView).InsertImageEx(idx);
                    (fImageEnMView as TImageEnMView).SetImageFromFile(idx, WideString(FileName));
                  finally
                    DoFinishWork();
                  end;
                  Result := Not fAborting;
                end
                else
                begin
                  {$ifdef IEINCLUDEDIRECTSHOW}
                  Result := LoadFromMediaFile(FileName)
                  {$else}
                  Result := False;
                  fAborting := True;
                  {$endif}
                end;
              end;
  end;
end;

{!!
<FS>TImageEnMIO.LoadFromFileAuto

<FM>Declaration<FC>
function LoadFromFileAuto(const FileName: string): Boolean;

<FM>Description<FN>
Loads an image from file. To detect the file format it analyzes the file content, it does not use the filename extension.

Result will be false if the file is not a recognized file type (<A TImageEnMIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

<FM>Example<FC>
ImageEnMView1.MIO.LoadFromFileAuto('input.dat');

ImageEnMView1.MIO.LoadFromFileAuto('input.tif'); // a tiff or a RAW?

if ImageEnMView1.MIO.LoadFromFileAuto(sFilename) = False then
  ShowMessage('Not a supported file type!');

!!}
function TImageEnMIO.LoadFromFileAuto(const FileName: string): Boolean;
var
  ff: TIOFileType;
begin
  ff := FindFileFormat(FileName, false);
  if ff = ioUnknown then
    Result := LoadFromFile(FileName)
  else
    Result := LoadFromFileFormat(FileName, ff);
end;

{!!
<FS>TImageEnMIO.LoadFromStream

<FM>Declaration<FC>
function LoadFromStream(Stream: TStream): Boolean;

<FM>Description<FN>
Loads an image (or multiple image) from the specified stream. LoadFromStream tries to detect file format reading the image header. The result will be false if an error is encountered, e.g. the file in the stream is not a recognized format (<A TImageEnMIO.Aborting> will be true).
!!}
function TImageEnMIO.LoadFromStream(Stream: TStream): Boolean;
var
  sf: TIOFileType;
  lp: int64;
begin
  lp := Stream.Position;
  sf := FindStreamFormat(Stream);
  Stream.Position := lp;
  Result := LoadFromStreamFormat(Stream, sf);
end;

{!!
<FS>TImageEnMIO.LoadFromBuffer

<FM>Declaration<FC>
procedure LoadFromBuffer(Buffer: pointer; BufferSize: integer; Format: <A TIOFileType> = ioUnknown);

<FM>Description<FN>
Loads an image or a multipage image from the specified buffer.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Buffer<FN></C> <C>The buffer pointer.</C> </R>
<R> <C><FC>BufferSize<FN></C> <C>The buffer length in bytes.</C> </R>
<R> <C><FC>Format<FN></C> <C>Specifies the expected file format. If Format is ioUnknown, then try to find the format automatically.</C> </R>
</TABLE>


<FM>Example<FC>
ImageEnMView1.MIO.LoadFromBuffer(mybuffer, mybufferlength, ioJPEG);
!!}
procedure TImageEnMIO.LoadFromBuffer(Buffer: pointer; BufferSize: integer; Format: TIOFileType);
var
  stream: TIEMemStream;
begin
  stream := TIEMemStream.Create(Buffer, BufferSize);
  try
    if Format=ioUnknown then
      LoadFromStream(stream)
    else
      LoadFromStreamFormat(stream, Format);
  finally
    FreeAndNil(stream);
  end;
end;


{!!
<FS>TImageEnMIO.LoadFromStreamFormat

<FM>Declaration<FC>
function LoadFromStreamFormat(Stream: TStream; FileFormat: <A TIOFileType>): Boolean;

<FM>Description<FN>
Loads an image (or multiple image) from the specified stream assuming that the image has FileFormat format. The result will be false if an error is encountered, e.g. the file in the stream is not a recognized format (<A TImageEnMIO.Aborting> will be true).
!!}
function TImageEnMIO.LoadFromStreamFormat(Stream: TStream; FileFormat: TIOFileType): Boolean;
var
  idx: integer;
begin           
  Result := False;
  case FileFormat of
    ioGIF   : Result := LoadFromStreamGIF(Stream);
    ioTIFF  : Result := LoadFromStreamTIFF(Stream);
    //ioAVI : Result := LoadFromStreamAVI(Stream);  // not supported
    ioICO   : Result := LoadFromStreamICO(Stream);
    ioDCX   : Result := LoadFromStreamDCX(Stream);
    {$ifdef IEINCLUDEDICOM}
    ioDICOM : Result := LoadFromStreamDICOM(Stream);
    {$endif}
    else
    begin
      if assigned(fImageEnMView) and (FileFormat <> ioUnknown) then
      begin
        // try single image
        idx := (fImageEnMView as TImageEnMView).SelectedImage;
        if idx = -1 then
          idx := (fImageEnMView as TImageEnMView).ImageCount;
        (fImageEnMView as TImageEnMView).InsertImageEx(idx);
        (fImageEnMView as TImageEnMView).SetImageFromStream(idx, Stream);    
        Result := Not fAborting;
      end
    end;
  end;
end;

{!!
<FS>TImageEnMIO.LoadFromFiles

<FM>Declaration<FC>
procedure LoadFromFiles(const FileName: string; AutoDetect: boolean=false; LoadWhenViewed: boolean=false);

<FM>Description<FN>
Loads multiple files separated by '|' character.
If AutoDetect is true then ImageEn tries to detect file type from header, otherwise it looks only the file extension.
If LoadWhenViewed is true each file is actually loaded only when it needs to be displayed.

<FM>Examples<FC>
- loads three files
ImageEnMView1.MIO.LoadFromFiles('one.jpg|two.jpg|three.jpg');

- execute open dialog returns a list of selected files
filanames := ImageEnMView1.MIO.ExecuteOpenDialog('', '', true, 1, '', true);
ImageEnMView1.MIO.LoadFromFiles(filenames);
!!}
procedure TImageEnMIO.LoadFromFiles(const FileName: string; AutoDetect: boolean; LoadWhenViewed: boolean);
var
  sl: TStringList;
  p1, p2, idx: integer;
  mv: TImageEnMView;
begin
  sl := TStringList.Create;
  mv := nil;
  try

    p1 := 1; p2 := 1;
    while p2<=length(FileName) do
    begin
      if (FileName[p2]='|') then
      begin
        sl.Add( Trim(Copy(string(FileName), p1, p2-p1)) );
        p1 := p2+1;
      end;
      inc(p2);
    end;
    if (p1<>p2) then
      sl.Add( Trim(Copy(string(FileName), p1, p2-p1+1)) );

    if assigned(fImageEnMView) and (fImageEnMView is TImageEnMView) then
      mv := fImageEnMView as TImageEnMView;
    if assigned(mv) then
      mv.LockPaint;

    idx := 0;

    for p1 := 0 to sl.Count-1 do
    begin
      if assigned(mv) then
      begin
        idx := (fImageEnMView as TImageEnMView).SelectedImage;
        if idx=-1 then
          idx := (fImageEnMView as TImageEnMView).ImageCount;
      end;
      if AutoDetect then
        LoadFromFileAuto( sl[p1] )
      else
      if LoadWhenViewed and assigned(mv) then
      begin
        // note1: if the file contains multiple pages only the first one is loaded
        // note2: the image is added at the end of the image list
        idx := mv.AppendImage;
        mv.ImageFileName[idx] := sl[p1]+'::0';
      end
      else
        LoadFromFile( sl[p1] );
      if assigned(mv) then
        mv.ImageBottomText[idx].Caption := extractfilename(sl[p1]);
      if assigned(fOnProgress) then
        fOnProgress(self, trunc(p1 / sl.Count * 100));
    end;

  finally
    if assigned(mv) then
      mv.UnLockPaint;
    sl.free;
  end;
end;

{!!
<FS>TImageEnMIO.PrintImagePos

<FM>Declaration<FC>
procedure PrintImagePos(ImageIndex: integer; PrtCanvas: TCanvas; x, y: double; Width, Height: double; GammaCorrection: double);

<FM>Description<FN>
PrintImagePos prints the specified image, specifing absolute position and size.

PrtCanvas is the printing canvas. The application can set this parameter from Printer.Canvas.

ImageIndex is the page/image index (0=first page/image).
x, y is the top-left starting point, in inches.
Width, Height is the image size, in inches.
GammaCorrection is the gamma correction value.  Use a value of 1.0 to disable gamma correction.

<FM>Example<FC>
// print the image first page at the position 2, 2 and height 10 and width 10 (stretch the image)
Printer.BeginDoc;
ImageEnMView1.MIO.PrintImagePos(0, Printer.Canvas, 2, 2, 10, 10, 1);
Printer.EndDoc;
!!}
procedure TImageEnMIO.PrintImagePos(ImageIndex: integer; PrtCanvas: TCanvas; x, y: double; Width, Height: double; GammaCorrection: double);
var
  bmp: TIEBitmap;
  io: TImageEnIO;
begin
  bmp := (fImageEnMView as TImageEnMView).GetTIEBitmap(ImageIndex);
  io := TImageEnIO.CreateFromBitmap(bmp);
  io.Params.DpiX := Params[ImageIndex].DpiX;
  io.Params.DpiY := Params[ImageIndex].DpiY;
  try
    io.PrintImagePos(PrtCanvas, x, y, Width, Height, GammaCorrection);
  finally
    (fImageEnMView as TImageEnMView).ReleaseBitmap(ImageIndex, false);
    io.free;
  end;
end;

{!!
<FS>TImageEnMIO.PrintImage

<FM>Declaration<FC>
procedure PrintImage(ImageIndex : integer; PrtCanvas : TCanvas = nil; MarginLeft : double = 1; MarginTop : double = 1; MarginRight : double = 1; MarginBottom : double = 1; VerticalPos : TIEVerticalPos = ievpCENTER; HorizontalPos : TIEHorizontalPos = iehpCENTER; Size : TIESize = iesFITTOPAGE; SpecWidth : double = 0; SpecHeight : double = 0; GammaCorrection : double = 1);

<FM>Description<FN>
PrintImage prints the specified image by specifying margins, vertical position, horizonal position and size.

ImageIndex is the index of the page/image to print (0=first image).

<FC>IEM_SELECTED_IMAGES<FN> can be specified for <FC>ImageIndex<FN> to print all images that the user has selected in the TImageEnMView
<FC>IEM_ALL_IMAGES<FN> can be specified for <FC>hobj<ImageIndex> to print all images in the TImageEnMView

PrtCanvas is the printing canvas. The application can set this parameter from Printer.Canvas.
MarginLeft, MarginTop, MarginRight, MarginBottom are the page margins in inches. By specifying all zero values, no margins are used.
VerticalPos determines how the image vertically aligns within the page.
HorizontalPos determines how the image horizontally aligns within the page.
Size determines the size of the image.
SpecWidth, SpecHeight specify the absolute sizes of the image in inches. Valid only when Size=iesSPECIFIEDSIZE
GammaCorrection is the gamma correction value, use 1 to disable gamma correction.

<FM>Example<FC>
// print the first image in center of the page with original sizes
Printer.BeginDoc;
ImageEnMView1.MIO.PrintImage(0, Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesNORMAL, 0, 0, 1);
Printer.EndDoc;

// print the second image in center of the page stretch to the page (respecting the proportions)
Printer.BeginDoc;
ImageEnMView1.MIO.PrintImage(1, Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE, 0, 0, 1);
Printer.EndDoc;

!!}
procedure TImageEnMIO.PrintImage(ImageIndex: integer; PrtCanvas: TCanvas; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; VerticalPos: TIEVerticalPos; HorizontalPos: TIEHorizontalPos; Size: TIESize ; SpecWidth: double; SpecHeight: double; GammaCorrection: double);
var
  bPrintedPage: Boolean; 
  I: Integer;

  procedure _PrintImage(idx : Integer);
  var
    bmp: TIEBitmap;
    io: TImageEnIO;
  begin
    if bPrintedPage then
      Printer.NewPage;
    bmp := (fImageEnMView as TImageEnMView).GetTIEBitmap(idx);
    io := TImageEnIO.CreateFromBitmap(bmp);
    io.Params.DpiX := Params[idx].DpiX;
    io.Params.DpiY := Params[idx].DpiY;
    try
      io.PrintImage(PrtCanvas, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
    finally
     (fImageEnMView as TImageEnMView).ReleaseBitmap(idx, false);
      io.free;
    end;
    bPrintedPage := True;
  end;

begin                    
  bPrintedPage := False;
  if (ImageIndex = IEM_SELECTED_IMAGES) or (ImageIndex = IEM_ALL_IMAGES) then
  begin
    if (ImageIndex = IEM_SELECTED_IMAGES) and ((fImageEnMView as TImageEnMView).DisplayMode = mdSingle) then
      _PrintImage((fImageEnMView as TImageEnMView).VisibleFrame)
    else
    for I := 0 to (fImageEnMView as TImageEnMView).ImageCount - 1 do
    begin
      if (ImageIndex = IEM_ALL_IMAGES) or (fImageEnMView as TImageEnMView).IsSelected(I) then
        _PrintImage(I);
    end;
  end
  else
  begin
    _PrintImage(ImageIndex);
  end;
end;



procedure TImageEnMIO.PrintImagesEx(PrtCanvas: TCanvas; dpix, dpiy: integer; pagewidth, pageheight: double; bPreview: Boolean; Columns: integer; Rows: integer; HorizSpace: double; VertSpace: double; PrintSelected: boolean; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; DrawBox: boolean; DrawText: boolean; DrawShadow: boolean; BoxColor: TColor = clBlack; iPageNo : Integer = 0);
var
  bmp, tbmp, xbmp: TIEBitmap;
  io: TImageEnIO;
  proc: TImageEnProc;
  mview: TImageEnMView;
  i: integer;
  row, col: integer;
  ww, hh, t1: double;
  bx, by: double;
  bw, bh: double;
  z: double;
  h1, h2, h3: double;
  remaining: integer;
  iThumbCount: integer;
  iStartIndex: Integer;

  procedure PrtImg(index: integer);
  begin
    bmp := mview.GetTIEBitmap(index);
    try
      if DrawShadow then
      begin
        tbmp.Assign( bmp );
        tbmp.PixelFormat := ie24RGB;
        proc.AddSoftShadow(mview.SoftShadow.Radius, mview.SoftShadow.OffsetX, mview.SoftShadow.OffsetY, true, 0, mview.SoftShadow.Intensity);
        tbmp.RemoveAlphaChannel(true, clWhite);
        xbmp := tbmp;
      end
      else
        xbmp := bmp;

      io.AttachedIEBitmap := xbmp;

      bx := MarginLeft + col * (ww + HorizSpace);
      by := MarginTop + row * (hh + VertSpace);

      h1 := 0;
      h2 := 0;
      h3 := 0;

      if DrawText then
      begin
        if mview.ImageTopText[index].Caption <> '' then
        begin
          PrtCanvas.Font.Assign(mview.ImageTopText[index].Font);
          t1 := PrtCanvas.TextWidth(mview.ImageTopText[index].Caption);
          h1 := PrtCanvas.TextHeight(mview.ImageTopText[index].Caption);
          PrtCanvas.TextOut(trunc((bx+(ww-t1/dpix)/2)*dpix), trunc(by*dpiy), mview.ImageTopText[index].Caption);
        end;
        if mview.ImageInfoText[index].Caption <> '' then
        begin
          PrtCanvas.Font.Assign(mview.ImageInfoText[index].Font);
          t1 := PrtCanvas.TextWidth(mview.ImageInfoText[index].Caption);
          h2 := PrtCanvas.TextHeight(mview.ImageInfoText[index].Caption);
          PrtCanvas.TextOut(trunc((bx+(ww-t1/dpix)/2)*dpix), trunc((by+hh-h2*2/dpiy)*dpiy), mview.ImageInfoText[index].Caption);
        end;
        if mview.ImageBottomText[index].Caption <> '' then
        begin
          PrtCanvas.Font.Assign(mview.ImageBottomText[index].Font);
          t1 := PrtCanvas.TextWidth(mview.ImageBottomText[index].Caption);
          h3 := PrtCanvas.TextHeight(mview.ImageBottomText[index].Caption);
          PrtCanvas.TextOut(trunc((bx+(ww-t1/dpix)/2)*dpix), trunc((by+hh-h3/dpiy)*dpiy), mview.ImageBottomText[index].Caption);
        end;
      end;

      h1 := h1 / dpiy;
      h2 := h2 / dpiy;
      h3 := h3 / dpiy;
      z := dmin( ww/(xbmp.Width/dpix), (hh-h1-h2-h3)/(xbmp.Height/dpiy));
      bw := xbmp.Width/dpix*z;
      bh := xbmp.Height/dpiy*z;

      io.PrintImagePosEx(PrtCanvas, dpix, dpiy, bx+(ww-bw)/2, by+(hh-bh-h1-h2-h3)/2+h1, bw, bh, 1);

      if DrawBox then
      begin
        PrtCanvas.Pen.Color := BoxColor;
        PrtCanvas.Pen.Style := psSolid;
        PrtCanvas.Brush.Style := bsClear;
        PrtCanvas.Rectangle(trunc(bx*dpix), trunc(by*dpiy), trunc((bx+ww)*dpix), trunc((by+hh)*dpiy));
      end;

      dec(remaining);

      inc(col);
      if col = Columns then
      begin
        col := 0;
        inc(row);
        if row = Rows then
        begin
          row := 0;
          if (bPreview = False) and (remaining > 0) then
            Printer.NewPage;
        end;
      end;
    finally
      mview.ReleaseBitmap(index, false);
    end;
  end;

begin
  fAborting := false;

  mview := (fImageEnMView as TImageEnMView);
  try
    io   := TImageEnIO.Create(nil);
    tbmp := TIEBitmap.Create;
    proc := TImageEnProc.CreateFromBitmap(tbmp);

    proc.AutoUndo := false;

    row := 0;
    col := 0;
    ww  := (pagewidth-(MarginLeft+MarginRight+HorizSpace*(Columns-1))) / Columns;
    hh  := (pageheight-(MarginTop+MarginBottom+VertSpace*(Rows-1))) / Rows;

    iStartIndex := 0;
    if iPageNo > 0 then
      iStartIndex := iPageNo * Columns * Rows;

    if PrintSelected then
    begin
      // print only selected images
      iThumbCount := mview.MultiSelectedImagesCount - iStartIndex;
      if bPreview and (iThumbCount > Columns * Rows) then
        iThumbCount := Columns * Rows;
      remaining := iThumbCount;
      for i := iStartIndex to iStartIndex + iThumbCount - 1 do
      begin
        PrtImg( mview.MultiSelectedImages[i] );
        if assigned(fOnProgress) then
          fOnProgress(self, trunc(i / iThumbCount * 100));
        if fAborting then
          break;
      end
    end
    else
    begin
      // print all images
      iThumbCount := mview.ImageCount - iStartIndex;
      if bPreview and (iThumbCount > Columns * Rows) then
        iThumbCount := Columns * Rows;
      remaining := iThumbCount;
      for i := iStartIndex to iStartIndex + iThumbCount - 1 do
      begin
        PrtImg(i);
        if assigned(fOnProgress) then
          fOnProgress(self, trunc(i / iThumbCount * 100));
        if fAborting then
          break;
      end
    end;

  finally
    tbmp.Free;
    proc.Free;
    io.Free;
    DoFinishWork;
  end;
end;



{!!
<FS>TImageEnMIO.PrintImages

<FM>Declaration<FC>
procedure PrintImages(Columns: integer; Rows: integer; HorizSpace: double; VertSpace: double; PrintSelected: boolean; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; DrawBox: boolean; DrawText: boolean; DrawShadow: boolean; BoxColor: TColor = clBlack);

<FM>Description<FN>
PrintImages prints all images or only selected images.
You can specify number of columns and rows, spaces between images and margins. It is possible also to draw box around images, shadows and text.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Columns<FN></C> <C>Specifies how arrange images, specifying the number of columns.</C> </R>
<R> <C><FC>Rows<FN></C> <C>Specifies how arrange images, specifying the number of rows.</C> </R>
<R> <C><FC>HorizSpace<FN></C> <C>The horizontal space in inches between images.</C> </R>
<R> <C><FC>VertSpace<FN></C> <C>The vertical space in inches between images.</C> </R>
<R> <C><FC>PrintSelected<FN></C> <C>Set to true to print only selected images.</C> </R>
<R> <C><FC>MarginLeft<FN></C> <C>Page left margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>MarginTop<FN></C> <C>Page top margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>MarginRight<FN></C> <C>Page right margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>MarginBottom<FN></C> <C>Page bottom margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>DrawBox<FN></C> <C>Set to true to draw a box around the images (image space). The image is always stretched to maintain aspect ratio.</C> </R>
<R> <C><FC>DrawText<FN></C> <C>Set to true to draw text associated with every image.</C> </R>
<R> <C><FC>DrawShadow<FN></C> <C>Set to true to draw a shadow around the image.</C> </R>
<R> <C><FC>BoxColor<FN></C> <C>Specifies the color of the box around the image if DrawBox is True.</C> </R>
</TABLE>


<FM>Example<FC>
Printer.BeginDoc;
ImageEnMView1.MIO.PrintImages(6, 4);
Printer.EndDoc;
!!}
procedure TImageEnMIO.PrintImages(Columns: integer; Rows: integer; HorizSpace: double; VertSpace: double; PrintSelected: boolean; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; DrawBox: boolean; DrawText: boolean; DrawShadow: boolean; BoxColor: TColor);
var
  pagewidth, pageheight: double;
  dpix, dpiy: integer;
  PrtCanvas: TCanvas;
begin
  PrtCanvas := Printer.Canvas;
  dpix := GetDeviceCaps(PrtCanvas.Handle, LOGPIXELSX);
  dpiy := GetDeviceCaps(PrtCanvas.Handle, LOGPIXELSY);
  pagewidth  := GetDeviceCaps(PrtCanvas.Handle, 8) / dpix;
  pageheight := GetDeviceCaps(PrtCanvas.Handle, 10) / dpiy;
  PrintImagesEx(PrtCanvas, dpix, dpiy, pagewidth, pageheight, False, Columns, Rows, HorizSpace, VertSpace, PrintSelected, MarginLeft, MarginTop, MarginRight, MarginBottom, DrawBox, DrawText, DrawShadow, BoxColor);
end;


          
{!!
<FS>TImageEnMIO.PreviewPrintImages

<FM>Declaration<FC>
procedure PreviewPrintImages(DestBitmap: TBitmap; MaxBitmapWidth, MaxBitmapHeight: integer; PrinterObj: TPrinter; Columns: integer; Rows: integer; HorizSpace: double; VertSpace: double; PrintSelected: boolean; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; DrawBox: boolean; DrawText: boolean; DrawShadow: boolean; BoxColor: TColor = clBlack; iPageNo : Integer = 0);

<FM>Description<FN>
PreviewPrintImages displays a preview of thumbnail printing of all images or selected images.
You can specify number of columns and rows, spaces between images and margins. It is possible also to draw box around images, shadows and text.  A rectangle is also painted over the image showing the page margins.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>DestBitmap<FN></C> <C>Destination bitmap where to paint the preview. Resizes DestBitmap as needed.</C> </R>
<R> <C><FC>MaxBitmapWidth<FN></C> <C>Maximum destination bitmap width. Preview will be stretched to match this size.</C> </R>
<R> <C><FC>MaxBitmapHeight<FN></C> <C>Maximum destination bitmap height. Preview will be stretched to match this size.</C> </R>
<R> <C><FC>Printer<FN></C> <C>This is the Printer object. PreviewPrintImage need it to know printer settings as orientation or page sizes.</C> </R>
<R> <C><FC>Columns<FN></C> <C>Specifies how arrange images, specifying the number of columns.</C> </R>
<R> <C><FC>Rows<FN></C> <C>Specifies how arrange images, specifying the number of rows.</C> </R>
<R> <C><FC>HorizSpace<FN></C> <C>The horizontal space in inches between images.</C> </R>
<R> <C><FC>VertSpace<FN></C> <C>The vertical space in inches between images.</C> </R>
<R> <C><FC>PrintSelected<FN></C> <C>Set to true to print only selected images.</C> </R>
<R> <C><FC>MarginLeft<FN></C> <C>Page left margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>MarginTop<FN></C> <C>Page top margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>MarginRight<FN></C> <C>Page right margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>MarginBottom<FN></C> <C>Page bottom margin in inches. By specifying all zero values, no margins are used.</C> </R>
<R> <C><FC>DrawBox<FN></C> <C>Set to true to draw a box around the images (image space). Image is always stretched to maintain aspect ratio.</C> </R>
<R> <C><FC>DrawText<FN></C> <C>Set to true to draw text associated with every image.</C> </R>
<R> <C><FC>DrawShadow<FN></C> <C>Set to true to draw a shadow around the image.</C> </R>
<R> <C><FC>BoxColor<FN></C> <C>Specifies the color of the box around the image if DrawBox is True.</C> </R>
<R> <C><FC>iPageNo<FN></C> <C>The page of thumbnails to preview.  0 shows the first page, 1 the second, etc.</C> </R>
</TABLE>


<FM>Example<FC>
// Paint and display the preview of thumbnails of the ImageEnMView using a TImageEnView component
ImageEnMView1.MIO.PreviewPrintImages(ImageEnView2.Bitmap, ImageEnView2.Width, ImageEnView2.Height, Printer, 6, 4, ...);
!!}
procedure TImageEnMIO.PreviewPrintImages(DestBitmap: TBitmap; MaxBitmapWidth, MaxBitmapHeight: integer; PrinterObj: TPrinter; Columns: integer; Rows: integer; HorizSpace: double; VertSpace: double; PrintSelected: boolean; MarginLeft: double; MarginTop: double; MarginRight: double; MarginBottom: double; DrawBox: boolean; DrawText: boolean; DrawShadow: boolean; BoxColor: TColor = clBlack; iPageNo : Integer = 0);
const
  Can_Draw_Text_In_Preview = False;  // todo... Support preview of text
var
  Zoom, z1, z: double;
  x1, y1, x2, y2: integer;
  dpix, dpiy: integer;
  PageWidth, PageHeight: integer;
begin
  if PrinterObj = nil then
    PrinterObj := Printer;
  Zoom := (MaxBitmapWidth - 5) / (PrinterObj.PageWidth / 100);
  z1 := (MaxBitmapHeight - 5) / (PrinterObj.PageHeight / 100);
  if z1 < Zoom then
    Zoom := z1;
  z := Zoom / 100;
  PageWidth := trunc(PrinterObj.PageWidth * z);
  PageHeight := trunc(PrinterObj.PageHeight * z);
  dpix := trunc(GetDeviceCaps(PrinterObj.Handle, LOGPIXELSX) * z);
  dpiy := trunc(GetDeviceCaps(PrinterObj.Handle, LOGPIXELSY) * z);
  DestBitmap.Width := 1;
  DestBitmap.Height := 1;
  DestBitmap.PixelFormat := pf24bit;
  DestBitmap.Width := PageWidth;
  DestBitmap.Height := PageHeight;
  with DestBitmap.Canvas do
  begin
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    fillrect(rect(0, 0, destbitmap.width, destbitmap.height));
  end;
  if Can_Draw_Text_In_Preview = False then
    DrawText := False;
  PrintImagesEx(DestBitmap.Canvas, dpix, dpiy, PageWidth / dpix, PageHeight / dpiy, True, Columns, Rows, HorizSpace {* dpix}, VertSpace {* dpiy}, PrintSelected, MarginLeft {* dpix}, MarginTop {* dpiy}, MarginRight {* dpix}, MarginBottom {* dpiy}, DrawBox, DrawText, DrawShadow, BoxColor, iPageNo);
  with DestBitmap.Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Color := IEPrint_Preview_Margin_Color;
    Pen.Style := psDot;
    Pen.Width := 1;
    x1 := trunc(MarginLeft * dpix);
    y1 := trunc(MarginTop * dpiy);
    x2 := trunc(PageWidth - MarginRight * dpix);
    y2 := trunc(PageHeight - MarginBOttom * dpiy);
    Rectangle(x1, y1, x2, y2);
  end;
end;



       
{!!
<FS>TImageEnMIO.PrintImagesToFile

<FM>Declaration<FC>
procedure PrintImagesToFile(const sFilename : string; iJpegQuality : Integer; iImageWidth, iImageHeight: integer; iColumns : integer; iRows : integer; iHorzSpace : Integer = 6; iVertSpace : Integer = 6; bPrintSelectedOnly: Boolean = False; iHorzMargin : Integer = 12; iVertMargin : Integer = 12; bDrawBox : Boolean = False; bDrawText : Boolean = True; bDrawShadow : Boolean = True; BackgroundColor : TColor = clWhite; BoxColor: TColor = clBlack; iPageNo : Integer = -1);

<FM>Description<FN>
PrintImagesToFile displays a preview of thumbnail printing of all images or selected images.
You can specify number of columns and rows, spaces between images and margins. It is possible also to draw box around images, shadows and text.  A rectangle is also painted over the image showing the page margins.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>sFilename<FN></C> <C>The destination filename for these thumbnails. If all pages are being printed (see iPageNo) then this filename will be automatically adjusted in the format image.jpg, image_2.jpg, image_3.jpg, etc.</C> </R>
<R> <C><FC>iJpegQuality<FN></C> <C>If sFilename is a JPEG file then specify the output quality (e.g. 80)</C> </R>
<R> <C><FC>iImageWidth<FN></C> <C>The output width for the image</C> </R>
<R> <C><FC>iImageHeight<FN></C> <C>The output height for the image</C> </R>
<R> <C><FC>iColumns<FN></C> <C>Specifies how many thumbnails span across the page</C> </R>
<R> <C><FC>iRows<FN></C> <C>Specifies how many thumbnails span down the page</C> </R>
<R> <C><FC>iHorzSpace<FN></C> <C>The horizontal space in pixels between thumbnails</C> </R>
<R> <C><FC>iVertSpace<FN></C> <C>The vertical space in pixels between thumbnails</C> </R>
<R> <C><FC>bPrintSelectedOnly<FN></C> <C>Set to true to print only selected images. False to print all image in the TImageEnMView</C> </R>
<R> <C><FC>iHorzMargin<FN></C> <C>Page margin on left and right of the image (in pixels)</C> </R>
<R> <C><FC>iVertMargin<FN></C> <C>Page margin on top and bottom of the image (in pixels)</C> </R>
<R> <C><FC>bDrawBox<FN></C> <C>Set to true to draw a box around the images (image space). Image is always stretched to maintain aspect ratio.</C> </R>
<R> <C><FC>bDrawText<FN></C> <C>Set to true to draw text associated with every image.</C> </R>
<R> <C><FC>bDrawShadow<FN></C> <C>Set to true to draw a shadow around the image.</C> </R>
<R> <C><FC>BackgroundColor<FN></C> <C>Specifies the fill color of the image.</C> </R>
<R> <C><FC>BoxColor<FN></C> <C>Specifies the color of the box around the image if DrawBox is True.</C> </R>
<R> <C><FC>iPageNo<FN></C> <C>The page of thumbnails to output. If this is -1 (default) then all pages are output and the filename is automatically incremented</C> </R>
</TABLE>


<FM>Example<FC>
// Save image of thumbnails
ImageEnMView1.MIO.PrintImagesToFile('C:\SomeImage.jpg', Screen.Width, Screen.Height, 6, 4, ...);
!!}
procedure TImageEnMIO.PrintImagesToFile(const sFilename : string; iJpegQuality : Integer; iImageWidth, iImageHeight: integer; iColumns : integer; iRows : integer; iHorzSpace : Integer = 6; iVertSpace : Integer = 6; bPrintSelectedOnly: Boolean = False; iHorzMargin : Integer = 12; iVertMargin : Integer = 12; bDrawBox : Boolean = False; bDrawText : Boolean = True; bDrawShadow : Boolean = True; BackgroundColor : TColor = clWhite; BoxColor: TColor = clBlack; iPageNo : Integer = -1);
// NPC: 27/2/13

  procedure _PrintImagesToFile(sCurrFilename : string; iCurrPageNo : Integer);
  var
    ABitmap : TBitmap;
    AImageEnIO: TImageEnIO;
  begin
    ABitmap := TBitmap.create;
    ABitmap.Width  := iImageWidth;
    ABitmap.Height := iImageHeight;
    ABitmap.PixelFormat := pf24bit;
    ABitmap.Canvas.Brush.Color := BackgroundColor;
    ABitmap.Canvas.FillRect(Rect(0, 0, iImageWidth, iImageHeight));
    AImageEnIO := TImageEnIO.CreateFromBitmap(ABitmap);
    try
      PrintImagesEx(ABitmap.Canvas, 1, 1, iImageWidth, iImageHeight, True { as we are not printing },
                    iColumns, iRows, iHorzSpace, iVertSpace, bPrintSelectedOnly,
                    iHorzMargin, iVertMargin, iHorzMargin, iVertMargin,
                    bDrawBox, bDrawText, bDrawShadow, BoxColor, iCurrPageNo);
                    
      AImageEnIO.Params.JPEG_Quality := iJpegQuality;
      AImageEnIO.SaveToFile(sCurrFilename);
      if AImageEnIO.Aborting then
        raise EIEException.create(format('Unable to save to %s', [sCurrFilename]));
    finally
      AImageEnIO.Free;
      ABitmap.free;
    end;
end;

var
  iImageCount: Integer;
  iThumbsPerPage: integer;
  iPageCount: Integer;
  I: Integer;
  sCurrentFilename: string;
begin
  if bPrintSelectedOnly then
    iImageCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
  else
    iImageCount := (fImageEnMView as TImageEnMView).ImageCount;
  iThumbsPerPage := iColumns * iRows;
  iPageCount := iImageCount div iThumbsPerPage;
  if iImageCount mod iThumbsPerPage <> 0 then
    inc(iPageCount);

  if iPageNo > -1 then
    _PrintImagesToFile(sFilename, iPageNo)
  else
  for I := 0 to iPageCount - 1 do
  begin
    if I = 0 then
      sCurrentFilename := sFilename
    else
      sCurrentFilename := ChangeFileExt(sFilename, format('_%d%s', [I + 1, ExtractFileExt(sFilename)]));
    _PrintImagesToFile(sCurrentFilename, I);
    if assigned(fOnProgress) then
      fOnProgress(self, trunc(I / iPageCount * 100));
    if fAborting then
      break
  end;
  DoFinishWork();
end;



{!!
<FS>TImageEnMIO.SaveToFileICO

<FM>Declaration<FC>
procedure SaveToFileICO(const FileName: string; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView as an ICO file (or only those images selected if <FC>SelectedOnly<FN> = True). <FC>FileName<FN> is the file name with extension.

Note: If an internal save error is encountered <A TImageEnMIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnMIO.SaveToFileICO(const FileName: string; SelectedOnly: Boolean = False);
var
  fs: TFileStream;
begin
  fAborting := False;
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStreamICO(fs, SelectedOnly);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnMIO.SaveToStreamICO

<FM>Declaration<FC>
procedure SaveToStreamICO(Stream: TStream; SelectedOnly: Boolean = False);

<FM>Description<FN>
Save all images in the connected TImageEnMView to a stream in ICO format (or only those images selected if <FC>SelectedOnly<FN> = True).
!!}
procedure TImageEnMIO.SaveToStreamICO(Stream: TStream; SelectedOnly: Boolean = False);
var
  Progress: TProgressRec;
  nullpr: TProgressRec;
  ielist: array of TObject;
  ie: TImageEnView;

  procedure _SaveImg(ImgIdx, FileIdx: integer);
  begin
    ie := TImageEnView.Create(nil);
    ie.LegacyBitmap := false;
    ielist[FileIdx] := ie;
    (fImageEnMView as TImageEnMView).CopyToIEBitmap(ImgIdx, ie.IEBitmap);
    ie.IO.Params.Assign( Params[ImgIdx] );
    ie.IO.Params.ICO_Sizes[FileIdx].cx := ie.IEBitmap.Width;
    ie.IO.Params.ICO_Sizes[FileIdx].cy := ie.IEBitmap.Height;
    ie.IO.Params.ICO_ImageIndex := FileIdx;
    ie.IO.Params.ICO_Background := Params[ImgIdx].ICO_Background;
    ie.Update;

    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * FileIdx));
  end;

var
  i, imgCount: Integer;
begin
  try
    fAborting := False;
    if not assigned(fImageEnMView) then
      exit;

    if SelectedOnly then
      imgCount := (fImageEnMView as TImageEnMView).MultiSelectedImagesCount
    else
      imgCount := (fImageEnMView as TImageEnMView).ImageCount;
    if imgCount = 0 then
      exit;

    Progress.fOnProgress := fOnProgress;
    Progress.Sender := Self;
    Progress.Aborting := @fAborting;
    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    Progress.per1 := 100 / imgCount;
    SetLength(ielist, imgCount);

    i := 0;
    while not fAborting and (i < length(ielist)) do
    begin
      if SelectedOnly then
        _SaveImg((fImageEnMView as TImageEnMView).MultiSelectedImages[i], i)
      else
        _SaveImg(i, i);
      inc(i);
    end;

    ICOWriteStream2(Stream, ielist, Progress);
  finally
    for i := 0 to length(ielist) - 1 do
      TImageEnView(ielist[i]).Free;
    DoFinishWork;
  end;
end;


procedure TImageEnMIO.SetAborting(Value: Boolean);
begin
  fAborting := Value;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  fDCIMParams.Aborting := Value;    
  fAcquireParams.Aborting := Value;
  {$ENDIF}
end;

{$ELSE} // {$ifdef IEINCLUDEMULTIVIEW}

interface
implementation

{$ENDIF}

end.




