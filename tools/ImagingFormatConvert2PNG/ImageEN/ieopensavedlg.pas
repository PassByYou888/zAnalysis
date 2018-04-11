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
File version 1025
Doc revision 1004
*)

unit ieopensavedlg;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEOPENSAVEDIALOGS}

interface

uses
  Windows, Messages, SysUtils, {$ifndef FPC}CommDlg,{$endif} Classes, Graphics, Controls, Forms, comctrls,
  Clipbrd, stdctrls, buttons, extctrls, Dialogs, hyieutils, hyiedefs, ImageEnIO, ImageEnView,
  IEMView, IEMIO, ievect;

type


  TIEOpenFileNameEx = {packed }record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: pointer;
    lpstrCustomFilter: pointer;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: pointer;
    nMaxFile: DWORD;
    lpstrFileTitle: pointer;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: pointer;
    lpstrTitle: pointer;
    Flags: DWORD;
    nFileOffset: Word;
    nFileExtension: Word;
    lpstrDefExt: pointer;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
    lpTemplateName: pointer;
    pvReserved: Pointer;
    dwReserved: DWORD;
    FlagsEx: DWORD;
  end;
  PIEOpenFileNameEx = ^TIEOpenFileNameEx;


{!!
<FS>TIEFileDlgPreviewEvent

<FM>Declaration<FC>
TIEFileDlgPreviewEvent = procedure(Sender: TObject; Viewer: TObject; FileName: String; ParamsOnly: Boolean) of object;

<FM>Description<FN>
Occurs when the Open File Dialog needs to load an image for preview or get the parameters (file details) from an image.
<FC>Viewer<FN> will be a <A TImageEnVect> object for single-frame images or a <A TImageEnMView> object for multi-frame images or videos.
<FC>FileName<FN> contains the file name to load.
<FC>ParamsOnly<FN> is true when the Preview Checkbox is not checked, so the user doesn't want to display the image.
!!}
  TIEFileDlgPreviewEvent = procedure(Sender: TObject; Viewer: TObject; FileName: String; ParamsOnly: Boolean) of object;


{!!
<FS>TIEDBorderStyle

<FM>Declaration<FC>
TIEDBorderStyle = (iepsDefault, iepsCropped, iepsCropShadow, iepsSoftShadow);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iepsDefault</C> <C>3D border displayed around the preview box</C> </R>
<R> <C>iepsCropped</C> <C>3D border displayed around the image</C> </R>
<R> <C>iepsCropShadow</C> <C>Shadow is displayed around the image</C> </R>
<R> <C>iepsSoftShadow</C> <C>Soft (high quality) shadow is displayed around the image</C> </R>
</TABLE>
!!}
  TIEDBorderStyle = (iepsDefault, iepsCropped, iepsCropShadow, iepsSoftShadow);
  
{!!
<FS>TIECommonDialogView

<FM>Declaration<FC>
TIECommonDialogView = (iedvDefault, iedvSmallIcons, iedvMediumIcons, iedvLargeIcons, iedvExtraLargeIcons, iedvTiles, iedvList, iedvDetails);

<FM>Description<FN>
The default file view for the <A TOpenImageEnDialog> and <A TSaveImageEnDialog>.  iedvLargeIcons is the standard thumbnail view.  iedvDetails is the standard columnar report view.
Note: On Windows XP iedvLargeIcons and iedvExtraLargeIcons are treated as iedvMediumIcons
!!}
  TIECommonDialogView = (iedvDefault, iedvSmallIcons, iedvMediumIcons, iedvLargeIcons, iedvExtraLargeIcons, iedvTiles, iedvList, iedvDetails);



{!!
<FS>TIEPreviewSize

<FM>Declaration<FC>
TIEPreviewSize = (iespDefault, iespTall, iespLarge);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iespDefault</C> <C>Normal size preview window (smallest)</C> </R>
<R> <C>iespTall</C> <C>Preview is same width as iepsDefault, but taller</C> </R>
<R> <C>iespLarge</C> <C>Much larger preview window</C> </R>
</TABLE>
!!}
  TIEPreviewSize = (iespDefault, iespTall, iespLarge);

{!!
<FS>TIEShowFormats

<FM>Declaration<FC>
TIEShowFormats = (iesfImagesAndVideos, iesfImagesOnly, iesfVideosOnly, iesfAllOnly);

<FM>Description<FN>
Specifies the file types are listed in the drop down filter selector of the TOpenImageEnDialog. Also determines which formats are included in the "All Common Graphic Files" filter (which is the default filter).
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iesfImagesAndVideos</C> <C>All ImageEn supported image formats are included, AVI videos, and also WMV and MPEG videos if DirectShow is enabled</C> </R>
<R> <C>iesfImagesOnly</C> <C>All ImageEn supported image formats are included (no videos)</C> </R>
<R> <C>iesfVideosOnly</C> <C>AVI videos, and also WMV and MPEG videos if DirectShow is enabled (no images)</C> </R>
<R> <C>iesfAllOnly</C> <C>Does not show the "All Common Graphic Files" filter. Default is to show "All Files"</C> </R>
</TABLE>
!!}
  TIEShowFormats = (iesfImagesAndVideos, iesfImagesOnly, iesfVideosOnly, iesfAllOnly);



{!!
<FS>TIECommonDialog

<FM>Description<FN>
The common parent class of <A TOpenImageEnDialog> and <A TSaveImageEnDialog>.

<FM>Properties<FN>
- <A TIECommonDialog.FileView>
- <A TIECommonDialog.ExtendedDialog>
- <A TIECommonDialog.ShowPlacesBar>

!!}
  TIECommonDialog = class(TComponent)
  private
    FCtl3D: Boolean;
    FDefWndProc: Pointer;
    FHelpContext: THelpContext;
    FHandle: HWnd;
    FObjectInstance: Pointer;
    FTemplate: PWideChar;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    fShowPlacesBar: boolean;
    FFileView : TIECommonDialogView;
    fWatchTimer: TTimer;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMInitDialog(var Message: TWMInitDialog); message WM_INITDIALOG;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure MainWndProc(var Message: TMessage);
  protected
    fExtendedDialog: boolean;
    procedure DoClose; dynamic;
    procedure DoShow; dynamic;
    procedure WndProc(var Message: TMessage); virtual;
    function MessageHook(var Msg: TMessage): Boolean; virtual;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; virtual;
    function Execute: Boolean; virtual; abstract;
    property Template: PWideChar read FTemplate write FTemplate;
    function ExtractFilter(ss: pointer; idx: Integer): WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    property Handle: HWnd read FHandle;
  published
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;

{!!
<FS>TIECommonDialog.ShowPlacesBar

<FM>Declaration<FC>
property ShowPlacesBar: Boolean;

<FM>Description<FN>
Whether the standard Windows "Places" bar is shown.
!!}
    property ShowPlacesBar: boolean read fShowPlacesBar write fShowPlacesBar default true;

        

{!!
<FS>TIECommonDialog.FileView

<FM>Declaration<FC>
property FileView: <A TIECommonDialogView>;

<FM>Description<FN>
Overrides the default file view of the Open/Save dialogs, e.g. iedvLargeIcons forces thumbnails, whereas iedvDetails provides a columnar report view.
!!}
    property FileView : TIECommonDialogView read FFileView write FFileView default iedvDefault;



{!!
<FS>TIECommonDialog.ExtendedDialog

<FM>Declaration<FC>
property ExtendedDialog: Boolean; (Default: True)

<FM>Description<FN>
Extends the Open/Save dialog with extra features, such as file information.
!!}
    property ExtendedDialog: boolean read fExtendedDialog write fExtendedDialog default true;

  end;

{!!
<FS>TOpenImageEnDialog

<FM>Description<FN>
Displays a modal Windows dialog box for selecting and opening image files. It includes all properties, methods and events of the standard TOpenDialog.  It also includes a preview window which displays all ImageEn supported file formats. If the file is an animated GIF, AVI video or a multi page TIFF, all images will be shown in sequence: the "Play" button will be activated to animate the sequence.

Image information such as dimensions, colors, dpi, file type, compression and file-memory sizes is also shown.

<FM>See Also<FN>
- TOpenDialog
- <A TSaveImageEnDialog>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AlwaysAnimate></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AutoAdjustDPI></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AutoSetFilter></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AutoSetFilterFileType></C> </R>
<R> <C_IMG_METHOD> <C><A TOpenImageEnDialog.Execute></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIECommonDialog.ExtendedDialog></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.ExtendedFilters></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FileName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.FileNameW></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.Files></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.FilesW></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIECommonDialog.FileView></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.Filter></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FilteredAdjustDPI></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FilterDefault></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FilterIndex></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.GetExifThumbnail></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.InfoPanel></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.PicturePanel></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.PreviewBorderStyle></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.PreviewSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.SelectedFrame></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.ShowAllFrames></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.ShowFormats></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIECommonDialog.ShowPlacesBar></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.ZoomFilter></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TOpenImageEnDialog.OnCreateCustomControls></C> </R>
<R> <C_IMG_EVENT> <C><A TOpenImageEnDialog.OnDestroyCustomControls></C> </R>
<R> <C_IMG_EVENT> <C><A TOpenImageEnDialog.OnPreviewFile></C> </R>
</TABLE>

!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TOpenImageEnDialog = class(TIECommonDialog)
  private
    m_blnInSelectionChange: Boolean;
    FImageEnView: TImageEnVect;
{$IFDEF IEINCLUDEMULTIVIEW}
    fImageEnMView: TImageEnMView;
{$ENDIF}
    FPicturePanel: TPanel;
    fPicLabel1: TLabel;
    fPicLabel2: TLabel;
    fPicLabel3: TLabel;
    fInfoPanel: TPanel;
    fInfoLabel1: TLabel;
    fInfoLabel2: TLabel;
    FPreviewButton: TSpeedButton;
    FZoomComboBox: TComboBox;
    fProgressBar: TProgressBar;
{$IFDEF IEINCLUDEMULTIVIEW}
    fPlayButton: TSpeedButton;
{$ENDIF}
    fAdvancedButton: TButton;
    fPreviewCheck: TCheckBox;
    fFileSize: integer; // size of last file loaded
    fFrames: integer; // page count of last file loaded
    fSelType: WideString;
    fAutoSetFilter: boolean;
    fAutoSetFilterFileType : TIOFileType;
{$IFDEF IEINCLUDEMULTIVIEW}
    fAlwaysAnimate: boolean;
{$ENDIF}
    fAutoAdjustDPI: boolean;
    fFilteredAdjustDPI: boolean;
    fGetExifThumbnail : boolean;
    //
    FHistoryList: TStrings;
    FOptions: TOpenOptions;
    FFilter: string;
    FFilterIndex: Integer;
    FFilterDefault : TIOFileType;
    FCurrentFilterIndex: Integer;
    FInitialDir: string;
    FTitle: string;
    FDefaultExt: string;
    FFileNameW: WideString;
    FFiles: TStrings;
    FFilesW: TIEWideStrings;
    FFileEditStyle: TFileEditStyle;
    FOnSelectionChange: TNotifyEvent;
    FOnFolderChange: TNotifyEvent;
    FOnTypeChange: TNotifyEvent;
    FOnCanClose: TCloseQueryEvent;
    fPreviewBorderStyle: TIEDBorderStyle;
    fShowFormats : TIEShowFormats;
    fExtendedFilters: string;
    fShowAllFrames: boolean;
    fSelectedFrame: Integer;
    OpenFileNameExShadow: pointer;
    fOnCreateCustomControls: TNotifyEvent;
    fOnDestroyCustomControls: TNotifyEvent;
    fOnPreviewFile: TIEFileDlgPreviewEvent;  
    FPreviewIsFullSize : Boolean;
    FPreviewSize : TIEPreviewSize;
    function GetFileName: TFileName;
    function GetFileNameW: WideString;
    function GetLongFileName: WideString;
    function GetFileName2: WideString;
    function GetFilterIndex: Integer;
    procedure ReadFileEditStyle(Reader: TReader);
    procedure SetHistoryList(Value: TStrings);
    procedure SetInitialDir(const Value: string);
    procedure PreviewClick(Sender: TObject);
    procedure PreviewKeyPress(Sender: TObject; var Key: Char);
    procedure ZoomComboChange(Sender: TObject);
    procedure ImageEnIOProgress(Sender: TObject; per: integer);
    procedure ShowIOParams(params: TIOParamsVals);
    procedure DoCheckPreview(Sender: TObject);
    procedure SetPreviewBorderStyle(v: TIEDBorderStyle);
    procedure SetZoomFilter(v: TResampleFilter);
    function GetZoomFilter: TResampleFilter;
    function FileExtToFilterIndex(e : string; bExcludeCommon : Boolean = False) : integer;
    function FileTypeToFilterIndex(FileType : TIOFileType) : integer;
    {$IFDEF IEINCLUDEMULTIVIEW}
    procedure SetAlwaysAnimate(value: Boolean);
    {$endif}
    procedure SetFileName(value: TFileName);
    procedure SetPreviewSize(const v : TIEPreviewSize);
  protected
    fShowPreview: boolean; // if true show the preview of images
    //
    procedure PlayClick(Sender: TObject);
    function CanClose(var OpenFileName: TIEOpenFileNameEx): Boolean;
    function DoCanClose: Boolean; dynamic;
    function DoExecute(Func: Pointer): Bool;
    procedure DoSelectionChange; dynamic;
    procedure DoFolderChange; dynamic;
    procedure DoTypeChange; dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetFileNames(var OpenFileName: TIEOpenFileNameEx);
    function GetStaticRect: TRect; virtual;
    procedure WndProc(var Message: TMessage); override;
    //
    procedure SetZoom; virtual;
    procedure DoClose; override;
    procedure DoShow; override;
    procedure SetLang;
    procedure OnMViewSelect(Sender: TObject; idx: integer);
    procedure DoAllDisplayed(Sender: TObject);   
    function LoadPreview(bFullSize: Boolean) : Boolean;
    function NeedLoadPreviewFullSize: Boolean;
    function SelectedZoom : Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BuildStrFilter(LimitToType : TIOFileType = -1) : string;

{!!
<FS>TOpenImageEnDialog.Execute

<FM>Declaration<FC>
property SelectedFrame: Integer;

<FM>Description<FN>
Displays the ImageEn Open dialog, returning true if the user selects a file and clicks OK. If the user cancels the dialog, Execute returns <FC>false<FN>.

<FM>Example<FC>
// Browse for an image and display it in an ImageEnView
If OpenImageEnDialog1.Execute then
  ImageEnView1.LoadFromFile(OpenImageEnDialog1.FileName);

<FM>See Also<FN>
- <A TOpenImageEnDialog.Filename>
!!}
    function Execute: Boolean; override;
    property FileEditStyle: TFileEditStyle read FFileEditStyle write FFileEditStyle;

{!!
<FS>TOpenImageEnDialog.Files

<FM>Declaration<FC>
property Files: TStrings;

<FM>Description<FN>
A string list that contains all selected filenames (with a full directory path).

Notes:
- Set the <FC>ofAllowMultiSelect<FN> flag in <FC>Options<FN> to allow multiple selection
- If you require widestring support use <A TOpenImageEnDialog.FilesW>.


<FM>See Also<FN>
- <A TOpenImageEnDialog.Filename>
- <A TOpenImageEnDialog.FilesW>
!!}
    property Files: TStrings read FFiles;

{!!
<FS>TOpenImageEnDialog.FilesW

<FM>Declaration<FC>
property FilesW: TIEWideStrings;

<FM>Description<FN>
A widestring equivalent of <A TOpenImageEnDialog.Files>, which provides access to all selected filenames (with a full directory path).


<FM>See Also<FN>
- <A TOpenImageEnDialog.Files>
- <A TOpenImageEnDialog.FilenameW>
!!}
    property FilesW: TIEWideStrings read FFilesW;
    property HistoryList: TStrings read FHistoryList write SetHistoryList;

{!!
<FS>TOpenImageEnDialog.PicturePanel

<FM>Declaration<FC>
property PicturePanel: TPanel;

<FM>Description<FN>
The TPanel object that shows a preview of the image (it contains both a <A TImageEnMView> and a <A TImageEnVect> component).
This property can be used to <L TOpenImageEnDialog.OnCreateCustomControls>customize</L> the open/save dialogs with new controls.

<FM>See Also<FN>
- <A TOpenImageEnDialog.InfoPanel>
- <A TOpenImageEnDialog.OnCreateCustomControls>
!!}
    property PicturePanel: TPanel read fPicturePanel;

{!!
<FS>TOpenImageEnDialog.InfoPanel

<FM>Declaration<FC>
property InfoPanel: TPanel;

<FM>Description<FN>
The TPanel object that displays image information and additional controls. It contains labels for file detail, a button for Advanced Save settings and a preview checkbox.
This property can be used to <L TOpenImageEnDialog.OnCreateCustomControls>customize</L> the open/save dialogs with new controls.

<FM>See Also<FN>
- <A TOpenImageEnDialog.PicturePanel>
- <A TOpenImageEnDialog.OnCreateCustomControls>
!!}
    property InfoPanel: TPanel read fInfoPanel;

    property PreviewCheckBox: TCheckBox read fPreviewCheck;

{!!
<FS>TOpenImageEnDialog.ExtendedFilters

<FM>Declaration<FC>
property ExtendedFilters: String;

<FM>Description<FN>
Specifies additional file formats to include when <A TOpenImageEnDialog.AutoSetFilter> is True.

<FM>Example<FC>
// Add the "Fun" file type to the file formats list
OpenImageEnDialog1.ExtendedFilters := 'Fun Bitmap|*.fun;*.fan’;'
!!}
    property ExtendedFilters: string read fExtendedFilters write fExtendedFilters;

{!!
<FS>TOpenImageEnDialog.SelectedFrame

<FM>Declaration<FC>
property SelectedFrame: Integer;

<FM>Description<FN>
Returns the selected page (or frame) in the open dialog for multipage files like TIFF, GIF.

<FM>Example<FC>
// Prompt the user to select a TIFF file and load the page that they select
If OpenImageEnDialog1.Execute then
Begin
  ImageEnView1.IO.Params.TIFF_ImageIndex := OpenImageEnDialog1.SelectedFrame;
  ImageEnView1.IO.LoadFromFile( OpenImageEnDialog1.FileName );
End;
!!}
    property SelectedFrame: Integer read fSelectedFrame write fSelectedFrame;

{!!
<FS>TOpenImageEnDialog.FileNameW

<FM>Declaration<FC>
property FileNameW: WideString;

<FM>Description<FN>
Returns the name and complete directory path of the most recently selected file.

FileNameW is the same as <A TOpenImageEnDialog.Filename> but uses WideString instead of String.

<FM>See Also<FN>
- <A TOpenImageEnDialog.FilesW>
- <A TOpenImageEnDialog.Filename>
- <A TOpenImageEnDialog.Files>
!!}
    property FileNameW: WideString read GetFileNameW write FFileNameW;

  published
    property DefaultExt: string read FDefaultExt write FDefaultExt;

{!!
<FS>TOpenImageEnDialog.FileName

<FM>Declaration<FC>
property FileName : String;

<FM>Description<FN>
Returns the name and complete directory path of the most recently selected file.  The value of FileName is the same as the first item in the <A TOpenImageEnDialog.Files> property.

To specify a default filename appear for the dialog's edit box, assign it to this property before calling Execute.

If you require widestring support, use <A TOpenImageEnDialog.FilenameW>.

<FM>Example<FC>
// Select an image to load in an ImageEnView
If OpenImageEnDialog1.Execute then
  ImageEnView1.IO.LoadFromFile(OpenImageEnDialog1.FileName);

<FM>See Also<FN>
- <A TOpenImageEnDialog.FilenameW>
- <A TOpenImageEnDialog.Files>
!!}
    property FileName: TFileName read GetFileName write SetFileName;

{!!
<FS>TOpenImageEnDialog.Filter

<FM>Declaration<FC>
property Filter : string;

<FM>Description<FN>
The Open/Save dialog includes a drop-down list of file types under the edit box. When the user picks a file type from the list, only files of the selected type are displayed in the dialog.

If <A TOpenImageEnDialog.AutoSetFilter> is true then ImageEn will automatically fill the dialog with all supported file types that it supports (or a specific one if you use <A TOpenImageEnDialog.AutoSetFilterFileType>). If you wish to override the default handling, set <A TOpenImageEnDialog.AutoSetFilter> to false and specify your prefered filter.

<FM>Example<FC>
// Only allow the user to open GIF and BMP Files
OpenImageEnDialog1.Filter := 'Supported Images |*.gif;*.bmp|GIF Images (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp'

<FM>See Also<FN>
- <A TOpenImageEnDialog.AutoSetFilter>
- <A TOpenImageEnDialog.AutoSetFilterFileType>
- <A TOpenImageEnDialog.ExtendedFilters>
- <A TOpenImageEnDialog.FilterDefault>
- <A TOpenImageEnDialog.FilterIndex>
!!}
    property Filter: string read FFilter write FFilter;

{!!
<FS>TOpenImageEnDialog.FilterIndex

<FM>Declaration<FC>
property FilterIndex : Integer;

<FM>Description<FN>
Determines which of the file types in <A TOpenImageEnDialog.Filter> is selected by default when the dialog opens. Set FilterIndex to 1 to choose the first file type in the list as the default, or 2 to choose the second file type, etc.

Generally it is better to use <A TOpenImageEnDialog.FilterDefault> to choose a particular format such as JPEG or AVI.

Note: this setting is ignored if you have specified a filename or set a type for <A TOpenImageEnDialog.FilterDefault>.

<FM>See Also<FN>
- <A TOpenImageEnDialog.FilterDefault>
!!}
    property FilterIndex: Integer read GetFilterIndex write FFilterIndex default 1;


{!!
<FS>TOpenImageEnDialog.FilterDefault

<FM>Declaration<FC>
property FilterDefault : <A TIOFileType>;

<FM>Description<FN>
Specifies a file type which is the default selection for the <A TOpenImageEnDialog.Filter>, e.g. if you choose ioGIF then GIF will be the default save type.  This setting overrides <A TOpenImageEnDialog.FilterIndex>, but is ignored if you have specified a <A TOpenImageEnDialog.FileName>.

<FM>See Also<FN>
- <A TOpenImageEnDialog.FilterIndex>
- <A TOpenImageEnDialog.AutoSetFilter>
- <A TOpenImageEnDialog.AutoSetFilterFileType>
!!}
    property FilterDefault : TIOFileType read FFilterDefault write FFilterDefault default -1;
    property InitialDir: string read FInitialDir write SetInitialDir;
    property Options: TOpenOptions read FOptions write FOptions default [ofHideReadOnly];
    property Title: string read FTitle write FTitle;
    property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;

{!!
<FS>TOpenImageEnDialog.AutoSetFilter

<FM>Declaration<FC>
property AutoSetFilter : boolean;

<FM>Description<FN>
When AutoSetFilter is True (default), TOpenImageEnDialog/TSaveImageEnDialog ignores the <A TOpenImageEnDialog.Filter> property and fills it with all supported ImageEn file formats (with custom file formats) or a specific format if <A TOpenImageEnDialog.AutoSetFilterFileType> is used.
                 
<FM>Example<FC>
// Allow loading of all file types
OpenImageEnDialog1.AutoSetFilter := True;
OpenImageEnDialog1.AutoSetFilterFileType := -1;

// Only allow the user to save to GIF format
SaveImageEnDialog1.AutoSetFilter := True;
SaveImageEnDialog1.AutoSetFilterFileType := ioGIF;

<FM>See Also<FN>
- <A TOpenImageEnDialog.AutoSetFilterFileType>
- <A TOpenImageEnDialog.Filter>
!!}
    property AutoSetFilter: boolean read fAutoSetFilter write fAutoSetFilter default true;


{!!
<FS>TOpenImageEnDialog.AutoSetFilterFileType

<FM>Declaration<FC>
property AutoSetFilterFileType : <A TIOFileType>;

<FM>Description<FN>
If AutoSetFilter is enabled, then the value of AutoSetFilterFileType will specify what formats are assigned to <A TOpenImageEnDialog.Filter>.

By default (-1), all supported formats are added but you can specify an ImageEn type to limit the filter to that type.

Note: With <A TOpenImageEnDialog> the filter will always include a line item for "Supported Types" and "All Files" even if if AutoSetFilterFileType is used.

<FM>Example<FC>
// Allow loading of all file types
OpenImageEnDialog1.AutoSetFilter := True;
OpenImageEnDialog1.AutoSetFilterFileType := -1;

// Only allow the user to save to GIF format
SaveImageEnDialog1.AutoSetFilter := True;
SaveImageEnDialog1.AutoSetFilterFileType := ioGIF;

<FM>See Also<FN>
- <A TOpenImageEnDialog.AutoSetFilter>
- <A TOpenImageEnDialog.Filter>
!!}
    property AutoSetFilterFileType: TIOFileType read fAutoSetFilterFileType write fAutoSetFilterFileType default -1;


{$IFDEF IEINCLUDEMULTIVIEW}
    property AlwaysAnimate: boolean read fAlwaysAnimate write SetAlwaysAnimate default false;
{$ENDIF}
    property PreviewBorderStyle: TIEDBorderStyle read fPreviewBorderStyle write SetPreviewBorderStyle default iepsDefault;


{!!
<FS>TOpenImageEnDialog.AutoAdjustDPI

<FM>Declaration<FC>
property AutoAdjustDPI : Boolean;

<FM>Description<FN>
When True and the last loaded/scanned image has a horizontal DPI not equal to the vertical DPI, ImageEn will resize the image so that DPIX = DPIY.

The default is False.
!!}
    property AutoAdjustDPI: boolean read fAutoAdjustDPI write fAutoAdjustDPI default false;



{!!
<FS>TOpenImageEnDialog.GetExifThumbnail

<FM>Declaration<FC>
property GetExifThumbnail : Boolean;

<FM>Description<FN>
If enabled preview is sped up by loading the thumbnail for JPEG and Raw files (using <A TIOParamsVals.JPEG_GetExifThumbnail> and <A TIOParamsVals.RAW_GetExifThumbnail>).

The default is False.
!!}
    property GetExifThumbnail: boolean read fGetExifThumbnail write fGetExifThumbnail default false;



{!!
<FS>TOpenImageEnDialog.FilteredAdjustDPI

<FM>Declaration<FC>
property FilteredAdjustDPI : boolean;

<FM>Description<FN>
The FilteredAdjustDPI property is valid when <A TOpenImageEnDialog.AutoAdjustDPI> is True. If set, ImageEn applies a resampling filter to the image to enhance quality.
This can slow down the loading process.
!!}
    property FilteredAdjustDPI: boolean read fFilteredAdjustDPI write fFilteredAdjustDPI default false;

    property ZoomFilter: TResampleFilter read GetZoomFilter write SetZoomFilter default rfFastLinear;

{!!
<FS>TOpenImageEnDialog.ShowAllFrames

<FM>Declaration<FC>
property ShowAllFrames: Boolean;

<FM>Description<FN>
When previewing a multi-page file (TIFF, GIF or AVI) if ShowAllFrames is true, it shows and loads all frames, otherwise only the first is shown.
!!}
    property ShowAllFrames: boolean read fShowAllFrames write fShowAllFrames default true;


{!!
<FS>TOpenImageEnDialog.ShowFormats

<FM>Declaration<FC>
property ShowFormats: <A TIEShowFormats>; (Default: iesfImagesAndVideos)

<FM>Description<FN>
Specifies the file types listed in the drop down filter selector of the TOpenImageEnDialog. Also determines which formats are included in the "All Common Graphic Files" filter (which is the default filter).

!!}
    property ShowFormats: TIEShowFormats read fShowFormats write fShowFormats default iesfImagesAndVideos;

    property PreviewSize: TIEPreviewSize read fPreviewSize write SetPreviewSize default iespDefault;


{!!
<FS>TOpenImageEnDialog.OnCreateCustomControls

<FM>Declaration<FC>
property OnCreateCustomControls: TNotifyEvent;

<FM>Description<FN>
Occurs immediately before an open/save dialog is executed (shown).

This event is often used to add your own controls to the dialog. You should free custom controls in <A TOpenImageEnDialog.OnDestroyCustomControls> event.

<FM>Example<FC>
var
  MyCbx : TCheckBox;

procedure TForm1.OpenImageEnDialogCreateCustomControls(Sender: TObject);
var
  p: TWinControl;
begin
  p := (sender as TOpenImageEnDialog).InfoPanel;
  MyCbx := TCheckBox.Create(p);
  MyCbx.parent := p;
  MyCbx.Caption := 'Test';
  MyCbx.SetBounds(280, 0, 130, 23);
end;

procedure TForm1.OpenImageEnDialogDestroyCustomControls(Sender: TObject);
begin
  MyCbx.free;
end;
!!}
    property OnCreateCustomControls: TNotifyEvent read fOnCreateCustomControls write fOnCreateCustomControls;

{!!
<FS>TOpenImageEnDialog.OnDestroyCustomControls

<FM>Declaration<FC>
property OnDestroyCustomControls: TNotifyEvent;

<FM>Description<FN>
Occurs immediately after an open/save dialog is executed (shown).

It is generally used to destroy objects, that have been added using <A TOpenImageEnDialog.OnCreateCustomControls>.
       
<FM>See Also<FN>
- <L TOpenImageEnDialog.OnCreateCustomControls>OnCreateCustomControls example</L>
- <A TOpenImageEnDialog.InfoPanel>
- <A TOpenImageEnDialog.PicturePanel>

!!}
    property OnDestroyCustomControls: TNotifyEvent read fOnDestroyCustomControls write fOnDestroyCustomControls;

{!!
<FS>TOpenImageEnDialog.OnPreviewFile

<FM>Declaration<FC>
property OnPreviewFile: <A TIEFileDlgPreviewEvent>;

<FM>Description<FN>
Occurs whenever a file is selected and needs to be previewed.

You can change the method that is used to load images by handling this event.

<FM>Example<FC>
// this is the default behavior
procedure TForm1.OnPreviewFile(Sender: TObject; Viewer: TObject; FileName: String; ParamsOnly: Boolean;
begin
  if ParamsOnly then
    (Viewer as TImageEnView).IO.ParamsFromFile(FileName)
  else
  if Viewer is TImageEnView then
    (Viewer as TImageEnView).IO.LoadFromFileAuto(FileName)
  else
  if Viewer is TImageEnMView then
    (Viewer as TImageEnMView).LoadFromFileOnDemand(FileName);
end;
!!}
    property OnPreviewFile: TIEFileDlgPreviewEvent read fOnPreviewFile write fOnPreviewFile;

  end;

  
{!!
<FS>TIESaveDlgOpt

<FM>Declaration<FC>
type TIESaveDlgOpt = set of (sdShowPreview, sdShowAdvanced);

<FM>Description<FN>
Specifies further options for the Save dialog:
- sdShowPreview: Show a preview and information on the selected image
- sdShowAdvanced: Show an "Advanced" button to configure image properties
!!}
  TIESaveDlgOpt = set of (sdShowPreview, sdShowAdvanced);
{!!
<FS>TSaveImageEnDialog

<FM>Description<FN>
TSaveImageEnDialog is the same as the <A TOpenImageEnDialog>, but with an "Advanced" button to display and modify advanced specific file format parameters.

Also you can hide the image preview (using <A TSaveImageEnDialog.ExOptions>).

TSaveImageEnDialog should be attached to a <A TImageEnIO>, <A TImageEnMIO>, <A TImageEnView> or <A TImageEnMView> component to get/set <A TImageEnIO.Params> properties (when the user clicks the "Advanced" button). If it is not attached it will function as a standard save dialog with a preview, but without the ability to modify image properties.

<FM>See Also<FN>
- TSaveDialog
- <A TOpenImageEnDialog>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AlwaysAnimate></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TSaveImageEnDialog.AttachedImageEnIO></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AutoAdjustDPI></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AutoSetFilter></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.AutoSetFilterFileType></C> </R>
<R> <C_IMG_METHOD> <C><A TSaveImageEnDialog.Execute></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIECommonDialog.ExtendedDialog></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.ExtendedFilters></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TSaveImageEnDialog.ExOptions></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FileName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.FileNameW></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.Files></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.FilesW></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.Filter></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FilteredAdjustDPI></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FilterDefault></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.FilterIndex></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIECommonDialog.FileView></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.GetExifThumbnail></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.InfoPanel></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.PicturePanel></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.PreviewBorderStyle></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.PreviewSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TOpenImageEnDialog.SelectedFrame></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.ShowAllFrames></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.ShowFormats></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIECommonDialog.ShowPlacesBar></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TOpenImageEnDialog.ZoomFilter></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TOpenImageEnDialog.OnCreateCustomControls></C> </R>
<R> <C_IMG_EVENT> <C><A TOpenImageEnDialog.OnDestroyCustomControls></C> </R>
</TABLE>

!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TSaveImageEnDialog = class(TOpenImageEnDialog)
  private
    fExOptions: TIESaveDlgOpt;
    fAttachedImageEnIO: TComponent;
    procedure OnWatchTimer(Sender: TObject);
    procedure DoAdvanced(Sender: TObject);
    procedure SetAttachedImageEnIO(v: TComponent);
  protected
    procedure DoTypeChange; override;
    procedure SetFileNameExt; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetAdvancedType(var pp: TPreviewParams; var ft: TIOFileType);
    procedure EnableDisableAdvanced;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

{!!
<FS>TSaveImageEnDialog.Execute

<FM>Declaration<FC>
function Execute : Boolean;

<FM>Description<FN>
Displays the ImageEn Save dialog, returning true if the user selects a file and clicks Save. If the user cancels the save operation, Execute returns <FC>false<FN>.

Note: If you have not set <A TSaveImageEnDialog.AttachedImageEnIO> the user will be unable to set <L TSaveImageEnDialog.ExOptions>advanced parameters</L>.

<FM>Example<FC>
// Save the displayed image in an ImageEnView
SaveImageEnDialog1.AttachedImageEnIO := ImageEnView1;
If SaveImageEnDialog1.Execute then
  ImageEnView1.SaveToFile(SaveImageEnDialog1ImageEnDialog1.FileName);

<FM>See Also<FN>
- <A TOpenImageEnDialog.Filename>
- <A TSaveImageEnDialog.AttachedImageEnIO>
!!}
    function Execute : Boolean; override;
  published

{!!
<FS>TSaveImageEnDialog.ExOptions

<FM>Declaration<FC>
property ExOptions: <A TIESaveDlgOpt>;

<FM>Description<FN>
Options to hide/show the image preview and "Advanced" button.

<FM>Note<FN>: If the "Advanced" button does not display, ensure you have correctly set <A TSaveImageEnDialog.AttachedImageEnIO>.
!!}
    property ExOptions: TIESaveDlgOpt read fExOptions write fExOptions;

    property AttachedImageEnIO: TComponent read fAttachedImageEnIO write SetAttachedImageEnIO;
  end;



implementation


uses {$ifdef IEHASSTRUTILS}StrUtils, {$endif} dlgs, ieview, iewic, imageenproc, iesettings;

{$R-}

{$R IEOpenSaveDlg.res}

var
  CreationControl: TIECommonDialog = nil;
  HelpMsg:         Cardinal;
  HookCtl3D:       Boolean;


type

  TIENMHdr = record
    hwndFrom: HWND;
    idFrom:   pointer;
    code:     integer;     { NM_ code }
  end;

  TIEOFNotify = record
    hdr:     TIENMHdr;
    lpOFN:   PIEOpenFileNameEx;
    pszFile: pointer;
  end;
  PIEOFNotify = ^TIEOFNotify;



// in '*.jpg;*.jpeg' result= '.jpg'
function ExtractFirstExt(s: WideString): WideString;
var
  p1, p2: integer;
begin
  p1 := pos('.', s);
  p2 := pos(';', s);
  if p2 = 0 then
    p2 := length(s) + 1;
  result := copy(s, p1, p2 - p1);
end;

function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := 0;
  case Msg of
    WM_INITDIALOG:
      begin
        if HookCtl3D then
        begin
{$WARNINGS OFF}
          Subclass3DDlg(Wnd, CTL3D_ALL);
          SetAutoSubClass(True);
{$WARNINGS ON}
        end;
        IECenterWindow(Wnd);
        CreationControl.FHandle := Wnd;
        CreationControl.FDefWndProc := Pointer(SetWindowLong(Wnd, GWL_WNDPROC, NativeInt(CreationControl.FObjectInstance)));
        CallWindowProc(CreationControl.FObjectInstance, Wnd, Msg, WParam, LParam);
        CreationControl := nil;
      end;
    WM_DESTROY:
{$WARNINGS OFF}
      if HookCtl3D then
        SetAutoSubClass(False);
{$WARNINGS ON}
  end;
end;

constructor TIECommonDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCtl3D := True;
  fExtendedDialog := true;
  {$WARNINGS OFF}
  FObjectInstance := MakeObjectInstance(MainWndProc);
  {$WARNINGS ON}

  fShowPlacesBar := true;
  FFileView := iedvDefault;
  fWatchTimer := nil;
end;

destructor TIECommonDialog.Destroy;
begin
{$WARNINGS OFF}
  if FObjectInstance <> nil then
    FreeObjectInstance(FObjectInstance);
{$WARNINGS ON}
  inherited Destroy;
end;

function TIECommonDialog.MessageHook(var Msg: TMessage): Boolean;
begin
  Result := False;
  if (Msg.Msg = HelpMsg) and (FHelpContext <> 0) then
  begin
    if assigned(Application) then
      Application.HelpContext(FHelpContext);
    Result := True;
  end;
end;

procedure TIECommonDialog.DefaultHandler(var Message);
begin
  if FHandle <> 0 then
    with TMessage(Message) do
      Result := CallWindowProc(FDefWndProc, FHandle, Msg, WParam, LParam)
  else
    inherited DefaultHandler(Message);
end;

procedure TIECommonDialog.MainWndProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except
    if assigned(Application) then
      Application.HandleException(Self);
  end;
end;


procedure TIECommonDialog.WndProc(var Message: TMessage);
const
  // Command codes for SHELLDLL_DefView (Paul DiLascia, MSDN Magazine — March 2004)
  // W2K/XP
  XP_ODM_VIEW_ICONS    = $7029;
  XP_ODM_VIEW_LIST     = $702b;
  XP_ODM_VIEW_DETAIL   = $702c;
  XP_ODM_VIEW_THUMBS   = $702d;
  XP_ODM_VIEW_TILES    = $702e;

  // Vista/Win 7
  Vista_FVM_EXTRALARGE = $704d;
  Vista_FVM_TILES      = $704c;
  Vista_FVM_MEDIUMICON = $704e;
  Vista_FVM_LARGE      = $704f;
  Vista_FVM_SMALL      = $7050;
  Vista_FVM_LIST       = $7051;
  Vista_FVM_DETAILS    = $704b;
  Vista_FVM_CONTENT    = $7052; // Win 7 Only

  XP_View_Consts : array[TIECommonDialogView] of integer = (-1,                   // iedvDefault
                                                            XP_ODM_VIEW_ICONS,    // iedvSmallIcons
                                                            XP_ODM_VIEW_THUMBS,   // iedvMediumIcons
                                                            XP_ODM_VIEW_THUMBS,   // iedvLargeIcons
                                                            XP_ODM_VIEW_THUMBS,   // iedvExtraLargeIcons
                                                            XP_ODM_VIEW_TILES,    // iedvTiles
                                                            XP_ODM_VIEW_LIST,     // iedvList
                                                            XP_ODM_VIEW_DETAIL);  // iedvDetails

  Vista_View_Consts : array[TIECommonDialogView] of integer = (-1,                      // iedvDefault
                                                               Vista_FVM_SMALL,         // iedvSmallIcons
                                                               Vista_FVM_MEDIUMICON,    // iedvMediumIcons
                                                               Vista_FVM_LARGE,         // iedvLargeIcons
                                                               Vista_FVM_EXTRALARGE,    // iedvExtraLargeIcons
                                                               Vista_FVM_TILES,         // iedvTiles
                                                               Vista_FVM_LIST,          // iedvList
                                                               Vista_FVM_DETAILS);      // iedvDetails
  function GetView : Integer;
  begin
    Result := -1;
    if IEGlobalSettings().OpSys in [ieosWin2000, ieosWinXP, ieosWin2003] then
      Result := XP_View_Consts[FFileView]
    else    
    if IEIsWindowsVistaOrNewer then
      Result := Vista_View_Consts[FFileView];
  end;

var
  hList : THandle;                                                        
begin
  Dispatch(Message);

  // Assign default view
  if (FFileView <> iedvDefault) and
     (Message.Msg = WM_NOTIFY) and
     (POFNotify(Message.LParam)^.hdr.code = CDN_FOLDERCHANGE) then
  begin
    hList := FindWindowEx( GetParent(handle), 0, 'SHELLDLL_DefView', '');
    if (hList <> 0) and (GetView > -1) then
      SendMessage(hList, WM_COMMAND, GetView, 0);
  end;
end;

procedure TIECommonDialog.WMDestroy(var Message: TWMDestroy);
begin
  inherited;
  DoClose;
end;

procedure TIECommonDialog.WMInitDialog(var Message: TWMInitDialog);
begin
  { Called only by non-explorer style dialogs }
  DoShow;
  { Prevent any further processing }
  Message.Result := 0;
end;

procedure TIECommonDialog.WMNCDestroy(var Message: TWMNCDestroy);
begin
  inherited;
  FHandle := 0;
end;

function TIECommonDialog.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
type
  TDialogFunc = function(var DialogData): Bool; stdcall;
var
  {$IFNDEF OCXVERSION}
  ActiveWindow: HWnd;
  {$ENDIF}
  WindowList: Pointer;
  {$ifdef IEHASFOCUSSTATE}
  FocusState: TFocusState;
  {$endif}
  FPUControlWord: Word;
begin
  {$IFNDEF OCXVERSION}
  ActiveWindow := GetActiveWindow;
  {$ENDIF}
  WindowList := DisableTaskWindows(0);
  {$ifdef IEHASFOCUSSTATE}
  FocusState := SaveFocusState;
  {$endif}    
  try
    if assigned(Application) then
      Application.HookMainWindow(MessageHook);
    FPUControlWord := Get8087CW();
    try
      CreationControl := Self;
      Result := TDialogFunc(DialogFunc)(DialogData);
    finally
      Set8087CW(FPUControlWord);
      if assigned(Application) then
        Application.UnhookMainWindow(MessageHook);
    end;
  finally
    EnableTaskWindows(WindowList);
    {$IFNDEF OCXVERSION}
    SetActiveWindow(ActiveWindow);
    {$ENDIF}
    {$ifdef IEHASFOCUSSTATE}
    RestoreFocusState(FocusState);
    {$endif}
  end;
end;

procedure TIECommonDialog.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TOpenImageEnDialog.DoClose;
begin
  inherited DoClose;
{$IFDEF IEINCLUDEMULTIVIEW}
  fImageEnMView.Playing := false;
  fImageEnMView.clear;
{$ENDIF}
  fImageEnView.Blank;
  if assigned(Application) then
    Application.HideHint;
end;

procedure TIECommonDialog.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function ExplorerHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    CreationControl.FHandle := Wnd;
    CreationControl.FDefWndProc := Pointer(SetWindowLong(Wnd, GWL_WNDPROC, NativeInt(CreationControl.FObjectInstance)));
    CallWindowProc(CreationControl.FObjectInstance, Wnd, Msg, WParam, LParam);
    CreationControl := nil;
  end
  else
  if (Msg = WM_NOTIFY) and (PIEOFNotify(LParam)^.hdr.code = CDN_INITDONE) then
    IECenterWindow(GetWindowLong(Wnd, GWL_HWNDPARENT));
end;
                                                                    
function TOpenImageEnDialog.BuildStrFilter(LimitToType : TIOFileType = -1) : string;
var
  q, w2: integer;
  sCommon, sLimitToFormat, sCommonFilter : string;
  c: char;
  w, cc : Integer;
  s1, s2, ex, anex : string;
  bIsSaveDialog: Boolean;

  function _TrimRightSemi(const s : string) : string;
  begin
    Result := s;
    if (Length(Result) > 0) and (Result[Length(Result)] = ';') then
      SetLength(Result, Length(Result) - 1);
  end;

begin
  IEUpdateGIFStatus;
  result := '';          
  sCommon := '';
  sLimitToFormat := '';
  bIsSaveDialog := self is TSaveImageEnDialog;

  if not (fShowFormats = iesfVideosOnly) then
    for q := 0 to IEGlobalSettings().FileFormats.Count - 1 do
      with TIEFileFormatInfo(IEGlobalSettings().FileFormats[q]) do
      begin
        if (Extensions<>'') and ((bIsSaveDialog and (@WriteFunction <> nil)) or ((not bIsSaveDialog) and (@ReadFunction <> nil))) then
        begin
          ex := '';
          cc := IEFileFormatGetExtCount(FileType);
          for w := 0 to cc - 1 do
          begin
            anex := '*.' + LowerCase(IEFileFormatGetExt(FileType, w));
            ex := ex + anex + ';';
            sCommon := sCommon + anex + ';';
          end;

          // Remove trailing semi colon
          ex := _TrimRightSemi(ex);

          // add the new item
          // Make JPEG the default type
          if FileType = ioJPEG then
            result := '|' + FullName + ' (' + ex + ')|' + ex + result
          else
            result := result + '|' + FullName + ' (' + ex + ')|' + ex;

          if FileType = LimitToType then
            sLimitToFormat := FullName + ' (' + ex + ')|' + ex;
        end;
    end;

  // Video files
  if not (fShowFormats = iesfImagesOnly) then
  begin
    sCommon := sCommon + '*.avi;';
    result := result + '|' + iemsg(IEMSG_VIDEOFORWINDOWS) + ' (*.avi)|*.avi';
    if LimitToType = ioAVI then
      sLimitToFormat := iemsg(IEMSG_VIDEOFORWINDOWS) + ' (*.avi)|*.avi';
    {$ifdef IEINCLUDEDIRECTSHOW}
    if not bIsSaveDialog then
    begin
      // MPEG
      sCommon := sCommon + '*.mpeg;*.mpg;';
      result := result + '|MPEG (*.mpeg;*.mpg)|*.mpeg;*.mpg';
      if LimitToType = ioMPEG then
        sLimitToFormat := 'MPEG (*.mpeg;*.mpg)|*.mpeg;*.mpg';

      // WMV  
      sCommon := sCommon + '*.wmv;';
      result := result + '|Windows Media Video (*.wmv)|*.wmv';
      if LimitToType = ioWMV then
        sLimitToFormat := 'Windows Media Video (*.wmv)|*.wmv';
    end;
    {$endif}
  end;

  // Extended filters (example: 'Fun Bitmap|*.fun;*.fan|etc...'
  s1 := ''; // to result
  s2 := ''; // to all graphics
  q := 1;
  w2 := -1;
  while q <= length(fExtendedFilters) do
  begin
    c := fExtendedFilters[q];
    if w2 = -1 then
    begin
      s1 := s1 + c;
      if c = '|' then
        w2 := q;
    end
    else
    begin
      if (c = '|') or (q = length(fExtendedFilters)) then
      begin
        w2 := -1;
        s1 := s1 + c;
        if c <> '|' then
          s2 := s2 + c;
        if q < length(fExtendedFilters) then
          s2 := s2 + ';';
      end
      else
      begin
        s1 := s1 + c;
        s2 := s2 + c;
      end;
    end;
    inc(q);
  end;
  s1 := Trim(s1);
  s2 := Trim(s2);
  if s1 <> '' then
    result := result + '|' + s1;   
  if (s1 <> '') and (sLimitToFormat <> '') then
    sLimitToFormat := sLimitToFormat + '|' + s1;
  if s2 <> '' then
    sCommon := sCommon + s2 + ';';

  if bIsSaveDialog then
  begin
    if sLimitToFormat <> '' then
      Result := sLimitToFormat
    else
      result := Copy(result, 2, length(result))
  end
  else
  begin
    if fShowFormats = iesfAllOnly then
      sCommonFilter := ''
    else
      sCommonFilter := iemsg(IEMSG_ALLCOMMONGRAPHICFILES) + '|' + _TrimRightSemi(sCommon) + '|';
    if sLimitToFormat <> '' then
      Result := sLimitToFormat + '|' +                   // Users desired filter
                sCommonFilter +                          // ImageEn Supported Files
                iemsg(IEMSG_ALLFILES) + ' (*.*)|*.*'     // All Files
    else
      result := sCommonFilter +                          // ImageEn Supported Files
                iemsg(IEMSG_ALLFILES) + ' (*.*)|*.*' +   // All Files
                result;                                  // Individual filters
  end;
end;

procedure TOpenImageEnDialog.SetLang;
begin
  FPreviewButton.Hint := iemsg(IEMSG_PREVIEW);
  fPreviewCheck.Caption := iemsg(IEMSG_PREVIEW);
  fAdvancedButton.Caption := iemsg(IEMSG_ADVANCED) + '...';
{$IFDEF IEINCLUDEMULTIVIEW}
  FPlayButton.Hint := iemsg(IEMSG_ANIMATE);
{$ENDIF}
end;

const
  ORIGThumbHeight = 109+80;
  ORIGThumbWidth  = 100+75;
  ORIGHorizBorder = 4;
  ORIGVertBOrder = 4;

constructor TOpenImageEnDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOnPreviewFile := nil;
  fOnCreateCustomControls := nil;
  fOnDestroyCustomControls := nil;
  IEUpdateGIFStatus;
  fSelectedFrame := 0;
  fShowFormats := iesfImagesAndVideos;
  fShowAllFrames := true;
  fPreviewBorderStyle := iepsDefault;
  {$IFDEF IEINCLUDEMULTIVIEW}
  fAlwaysAnimate := false;
  {$ENDIF}
  fSelType := '';
  FHistoryList := TStringList.Create;
  FOptions := [ofHideReadOnly];
  FFiles := TStringList.Create;
  FFilesW := TIEWideStrings.Create;
  FFilterIndex := 1;
  FFilterDefault := -1;
  FFileEditStyle := fsEdit;
  fAutoAdjustDPI := false;
  fGetExifThumbnail := false;
  fFilteredAdjustDPI := false;
  fPreviewSize := iespDefault;
  fExtendedFilters := '';
  //
  fShowPreview := true;
  fAutoSetFilter := true;
  fAutoSetFilterFileType := -1;
  Filter := BuildStrFilter;
  //
  FInfoPanel := TPanel.Create(Self);
  with FInfoPanel do
  begin
    Name := 'InfoPanel';
    Caption := '';
    Ctl3D := True;
    SetBounds(204, 5, 169, 200);
    BevelOuter := bvNone;
    BorderWidth := 6;
    TabOrder := 1;
  end;
  FInfoLabel1 := TLabel.Create(Self);
  with fInfoLabel1 do
  begin
    Name := 'InfoLabel1';
    Parent := FInfoPanel;
    Caption := '';
    SetBounds(6, 6, 157, 23);
    AutoSize := true;
  end;
  FInfoLabel2 := TLabel.Create(Self);
  with fInfoLabel2 do
  begin
    Name := 'InfoLabel2';
    Parent := FInfoPanel;
    Caption := '';
    SetBounds(6, 29, 157, 23);
    AutoSize := true;
  end;
  fAdvancedButton := TButton.Create(Self);
  with FAdvancedButton do
  begin
    Name := 'AdvancedButton';
    Parent := FInfoPanel;
    if fShowPlacesBar then
      SetBounds(474, 0, 75, 23)
    else
      SetBounds(333, 0, 75, 23);
    Enabled := true;
    visible := false;
  end;
  fPreviewCheck := TCheckBox.Create(Self);
  with fPreviewCheck do
  begin
    Name := 'PreviewCheck';
    Parent := FInfoPanel;
    SetBounds(195, 0, 130, 23);
    Enabled := true;
    checked := true;
    visible := false;
    OnClick := DoCheckPreview;
  end;
  //
  FPicturePanel := TPanel.Create(Self);
  with FPicturePanel do
  begin
    Name := 'PicturePanel';
    align := alLeft;
    Caption := '';
    Ctl3D := True;
    SetBounds(204, 5, 233, 200);  // Also see SetPreviewSize
    BevelOuter := bvNone;
    BorderWidth := 6;
    TabOrder := 1;
  end;
  FPicLabel1 := TLabel.Create(Self);
  with fPicLabel1 do
  begin
    Name := 'PicLabel1';
    Parent := FPicturePanel;
    Caption := '';
    SetBounds(6, 180 + 80, 227, 23);  // Also see SetPreviewSize
    AutoSize := true;
  end;
  FPicLabel2 := TLabel.Create(Self);
  with fPicLabel2 do
  begin
    Name := 'PicLabel2';
    Parent := FPicturePanel;
    Caption := '';
    SetBounds(6, 203 + 80, 227, 23); // Also see SetPreviewSize
    AutoSize := true;
  end;
  FPicLabel3 := TLabel.Create(Self);
  with fPicLabel3 do
  begin
    Name := 'PicLabel3';
    Parent := FPicturePanel;
    Caption := '';
    SetBounds(6, 226 + 80, 227, 23); // Also see SetPreviewSize
    AutoSize := true;
  end;
  FZoomComboBox := TComboBox.Create(Self);
  with FZoomComboBox do
  begin
    Name := 'ZoomCombo';
    Parent := FPicturePanel;
    Style := csDropDownList;
    SetBounds(6, 5, 50+20, 21);
    Enabled := True;
    Hint := 'Zoom';
    ParentShowHint := False;
    ShowHint := True;
    OnChange := ZoomComboChange;
  end;
  FPreviewButton := TSpeedButton.Create(Self);
  with FPreviewButton do
  begin
    Name := 'PreviewButton';
    Parent := FPicturePanel;    
    SetBounds(85, 5, 23, 22);
    Flat := True;
    Enabled := False;
    Glyph.LoadFromResourceName(HInstance, 'IDB_PREVIEW');
    OnClick := PreviewClick;
    ShowHint := true;
  end;
  {$IFDEF IEINCLUDEMULTIVIEW}
  fPlayButton := TSpeedButton.Create(Self);
  with FPlayButton do
  begin
    Name := 'PlayButton';
    Parent := FPicturePanel;
    SetBounds(60+20+30, 5, 23, 22);
    Flat := true;
    Enabled := false;
    groupindex := 1;
    allowallup := true;
    Glyph.LoadFromResourceName(HInstance, 'IDB_PLAY');
    onclick := playclick;
    visible := true;
    ShowHint := true;
  end;
  {$ENDIF}
  FImageEnView := TImageEnVect.Create(Self);
  with FImageEnView do
  begin
    LegacyBitmap := false;
    Name := 'ImageEnView';
    SetBounds(6, 36, 133 + 100, 139 + 78);  // Also see SetPreviewSize
    TabOrder := 0;
    Cursor := crDefault;
    Ctl3d := true;
    OnDblClick := PreviewClick;
    MouseInteract := [miScroll];
    Background := IEGlobalSettings().PreviewImageBackgroundColor;
    BackgroundStyle := IEGlobalSettings().PreviewImageBackgroundStyle;
    BorderStyle := bsSingle;
    Center := true;
    ZoomFilter := IEGlobalSettings().DefaultResampleFilter;
    DelayZoomFilter := false;
    EnableAlphaChannel := true;
    SetChessboardStyle(12, bsSolid);
    OnProgress := ImageEnIOProgress;
    IO.Params.OutputICCProfile.Clear;  // this disables ICC loading
  end;
  //
{$IFDEF IEINCLUDEMULTIVIEW}
  FImageEnMView := TImageEnMView.Create(Self);
  with FImageEnMView do
  begin
    Name := 'ImageEnMView';
    SetBounds(6, 36, 133 + 100, 139 + 78);  // Also see SetPreviewSize
    TabOrder := 0;
    Cursor := crDefault;
    BorderStyle := bsSingle;
    Ctl3d := true;
    MouseInteract := [mmiScroll, mmiSelect];
    ScrollBars := ssHorizontal;
    KeyInteract := [mkiMoveSelected];
    ThumbHeight := ORIGThumbHeight;
    ThumbWidth := ORIGThumbWidth;
    HorizBorder := ORIGHorizBorder;
    VertBorder := ORIGVertBorder;
    Visible := false;
    background := IEGlobalSettings().PreviewImageBackgroundColor;
    EnableAlphaChannel := true;
    BackgroundStyle := IEGlobalSettings().PreviewImageBackgroundStyle;
    FillThumbnail := false;
    Style := iemsFlat;
    SetChessboardStyle(12, bsSolid);
    OnImageSelect := OnMViewSelect;
    OnProgress := ImageEnIOProgress;
    OnAllDisplayed := DoAllDisplayed;
    SoftShadow.Enabled := true;
  end;
{$ENDIF}
  fProgressBar := TProgressBar.Create(Self);
  with fProgressBar do
  begin
    Name := 'ProgressBar';
    SetBounds(0, 55+35, 127+100, 19);
    min := 0;
    max := 100;
    Visible := false;
  end;
  SetLang;
end;

procedure TOpenImageEnDialog.OnMViewSelect(Sender: TObject; idx: integer);
begin
  {$IFDEF IEINCLUDEMULTIVIEW}
  fSelectedFrame := idx;
  if fSelectedFrame<0 then
    fSelectedFrame := 0;

  if idx<fImageEnMView.MIO.ParamsCount then
    ShowIOParams(fImageEnMView.MIO.Params[idx]);
  {$ENDIF}
end;

destructor TOpenImageEnDialog.Destroy;
begin
  FreeAndNil(FZoomComboBox);
  FreeAndNil(FPreviewButton);
{$IFDEF IEINCLUDEMULTIVIEW}
  FreeAndNil(fPlayButton);
{$ENDIF}
  FreeAndNil(fProgressBar);
  FreeAndNil(fPicLabel1);
  FreeAndNil(fPicLabel2);
  FreeAndNil(fPicLabel3);
  FreeAndNil(fInfolabel1);
  FreeAndNil(fInfoLabel2);
  FreeAndNil(fAdvancedButton);
  FreeAndNil(fPreviewCheck);
{$IFDEF IEINCLUDEMULTIVIEW}
  FreeAndNil(fImageEnMView);
{$ENDIF}
  FreeAndNil(FImageEnView);
  FreeAndNil(fInfoPanel);
  FreeAndNil(FPicturePanel);
  //
  FreeAndNil(FFiles);
  FreeAndNil(FFilesW);
  FreeAndNil(FHistoryList);
  inherited Destroy;
end;

function TOpenImageEnDialog.CanClose(var OpenFileName: TIEOpenFileNameEx): Boolean;
begin
  GetFileNames(OpenFileName);
  Result := DoCanClose;
  FFiles.Clear;
  FFilesW.Clear;
end;

function wstrlen(s: PWideChar): Integer;
begin
  result := 0;
  while s^ <> #0 do
  begin
    inc(s);
    inc(result);
  end;
end;

function TIECommonDialog.ExtractFilter(ss: pointer; idx: integer): WideString;
var
  sa: PAnsiChar;
  sw: PWideChar;
begin
  if IEGlobalSettings().UnicodeOS then
  begin
    sw := ss;
    while idx > 1 do
    begin
      inc(sw, wstrlen(sw) + 1);
      inc(sw, wstrlen(sw) + 1);
      dec(idx);
    end;
    inc(sw, wstrlen(sw) + 1);
    result := WideString(sw);
  end
  else
  begin
    sa := ss;
    while idx > 1 do
    begin
      inc(sa, IEStrLen(sa) + 1);
      inc(sa, IEStrLen(sa) + 1);
      dec(idx);
    end;
    inc(sa, IEStrLen(sa) + 1);
    result := WideString(AnsiString(sa));
  end;
end;


procedure TOpenImageEnDialog.WndProc(var Message: TMessage);
var
  Index, q: integer;
  ss: WideString;
begin
  Message.Result := 0;
  if (Message.Msg = WM_INITDIALOG) and not (ofOldStyleDialog in Options) then
    exit
  else
  if (Message.Msg = WM_NOTIFY) then
    case (PIEOFNotify(Message.LParam)^.hdr.code) of
      CDN_FILEOK:
        begin
          if not CanClose(PIEOFNotify(Message.LParam)^.lpOFN^) then
          begin
            Message.Result := 1;
            SetWindowLong(Handle, DWL_MSGRESULT, Message.Result);
            Exit;
          end;
        end;
      CDN_INITDONE:
        begin
          fSelType := ExtractFilter(PIEOFNotify(Message.LParam)^.lpOFN^.lpstrFilter, FCurrentFilterIndex);
          if assigned(fWatchTimer) then
            fWatchTimer.Enabled := True;
          DoShow;
        end;
      CDN_SELCHANGE: DoSelectionChange;
      CDN_FOLDERCHANGE: DoFolderChange;
      CDN_TYPECHANGE:
        begin
          Index := PIEOFNotify(Message.LParam)^.lpOFN^.nFilterIndex;
          fSelType := ExtractFilter(PIEOFNotify(Message.LParam)^.lpOFN^.lpstrFilter, Index);
          ss := fSelType;
          Delete(ss, 1, 1);
          q := pos(';', ss);
          Delete(ss, q, length(ss));
          if IEGlobalSettings().UnicodeOS then
            PIEOFNotify(Message.LParam)^.lpOFN^.lpstrDefExt := PWideChar(ss)
          else
            PIEOFNotify(Message.LParam)^.lpOFN^.lpstrDefExt := PAnsiChar(AnsiString(ss));

          if Index <> FCurrentFilterIndex then
          begin
            FCurrentFilterIndex := Index;
            DoTypeChange;
          end;
        end;
    end;
  inherited WndProc(Message);
end;

function TOpenImageEnDialog.DoCanClose: Boolean;
begin
  Result := True;
  if Assigned(FOnCanClose) then
    FOnCanClose(Self, Result);
end;

procedure TOpenImageEnDialog.DoFolderChange;
begin
  if Assigned(FOnFolderChange) then
    FOnFolderChange(Self);
end;

procedure TOpenImageEnDialog.DoTypeChange;
begin
  if Assigned(FOnTypeChange) then
    FOnTypeChange(Self);
end;

procedure TOpenImageEnDialog.ReadFileEditStyle(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TOpenImageEnDialog.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('FileEditStyle', ReadFileEditStyle, nil, False);
end;

function TOpenImageEnDialog.FileExtToFilterIndex(e : string; bExcludeCommon : Boolean = False) : integer;  
// NPC: 27/10/11

  {$IFNDEF IEHASPOSEX}
  function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
  var
    I, X: Integer;
    Len, LenSubStr: Integer;
  begin
    if Offset = 1 then
      Result := Pos(SubStr, S)
    else
    begin
      I := Offset;
      LenSubStr := Length(SubStr);
      Len := Length(S) - LenSubStr + 1;
      while I <= Len do
      begin
        if S[I] = SubStr[1] then
        begin
          X := 1;
          while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
            Inc(X);
          if (X = LenSubStr) then
          begin
            Result := I;
            exit;
          end;
        end;
        Inc(I);
      end;
      Result := 0;
    end;
  end;
  {$ENDIF}

var
  q, q2, w, z, i : integer;
begin             
  e := LowerCase(e);
  q := Pos(e, Filter);

  // Exclude "Common File Types which is shown in the open dialog   
  if (self is TSaveImageEnDialog) = False then
  begin
    q2 := q;
    for i := 1 to 2 do
      q2 := PosEx(e, Filter, q2 + 1);
    if q2 > 0 then
      q := q2;
  end;

  z := 0;
  for w := 1 to q do
    if Filter[w] = '|' then
      inc(z);
  result := (z div 2)+1;
end;
    

function TOpenImageEnDialog.FileTypeToFilterIndex(FileType : TIOFileType) : integer;
// NPC: 27/10/11
var
  e : string;
begin
  Result := 0;
  e := IEFileFormatGetExt(FileType, 0);
  if e <> '' then
    Result := FileExtToFilterIndex('.' + e, True);
end;

function WideStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;


function TOpenImageEnDialog.DoExecute(Func: Pointer): Bool;
const
  MultiSelectBufferSize = High(Word) - 16;
  OpenOptions: array[TOpenOption] of DWORD = (
    OFN_READONLY, OFN_OVERWRITEPROMPT, OFN_HIDEREADONLY,
    OFN_NOCHANGEDIR, OFN_SHOWHELP, OFN_NOVALIDATE, OFN_ALLOWMULTISELECT,
    OFN_EXTENSIONDIFFERENT, OFN_PATHMUSTEXIST, OFN_FILEMUSTEXIST,
    OFN_CREATEPROMPT, OFN_SHAREAWARE, OFN_NOREADONLYRETURN,
    OFN_NOTESTFILECREATE, OFN_NONETWORKBUTTON, OFN_NOLONGNAMES,
    OFN_EXPLORER, OFN_NODEREFERENCELINKS,
    OFN_ENABLEINCLUDENOTIFY, OFN_ENABLESIZING
    {$ifdef IEHASOPENOPTIONSEX}
    , OFN_DONTADDTORECENT, OFN_FORCESHOWHIDDEN
    {$endif}
    );

  function AllocFilterStrW(const S: WideString): WideString;
  var
    P: PWideChar;
  begin
    Result := '';
    if S <> '' then
    begin
      Result := S + #0;
      P := WideStrScan(PWideChar(Result), '|');
      while P <> nil do
      begin
        P^ := #0;
        Inc(P);
        P := WideStrScan(P, '|');
      end;
    end;
  end;

  function AllocFilterStrA(const S: AnsiString): AnsiString;
  var
    P: PAnsiChar;
  begin
    Result := '';
    if S <> '' then
    begin
      Result := S + #0;
      P := IEAnsiStrScan(PAnsiChar(Result), '|');
      while P <> nil do
      begin
        P^ := #0;
        Inc(P);
        P := IEAnsiStrScan(P, '|');
      end;
    end;
  end;

var
  Option: TOpenOption;
  OpenFilename: TIEOpenFileNameEx;
  TempFilterW, TempFilenameW, TempExt: WideString;
  TempFilterA, TempFilenameA: AnsiString;
  q: integer;
  OpenFileNameEx: TIEOpenFileNameEx;
begin
{$IFDEF IEINCLUDEMULTIVIEW}
  fPlayButton.visible := not fAlwaysAnimate;
{$ENDIF}
  fSelectedFrame := 0;
  FFiles.Clear;
  FFilesW.Clear;
  FillChar(OpenFileName, SizeOf(OpenFileName), 0);
  OpenFileNameExShadow := @OpenFileName;
  with OpenFilename do
  begin
{$IFDEF IEFIXOSDIALOGS}
    if (Win32MajorVersion >= 5) and (Win32Platform = VER_PLATFORM_WIN32_NT) or { Win2k }((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (Win32MajorVersion >= 4) and (Win32MinorVersion >= 90)) then { WinME }
      lStructSize := SizeOf(TOpenFilenameW)
    else
      lStructSize := SizeOf(TOpenFilenameW) - (SizeOf(DWORD) shl 1) - SizeOf(Pointer); { subtract size of added fields }
{$ELSE}
    lStructSize := SizeOf(TOpenFilenameW);
{$ENDIF}
    hInstance := SysInit.HInstance;

    if ofAllowMultiSelect in FOptions then
      nMaxFile := MultiSelectBufferSize
    else
      nMaxFile := MAX_PATH;

    if FileNameW <> '' then  
      FFilterIndex := FileExtToFilterIndex(IEExtractFileExtS(FileNameW))
    else
    if FFilterDefault > ioUnknown then
      FFilterIndex := FileTypeToFilterIndex(FFilterDefault);
    if FFilterIndex = 0 then
      FFilterIndex := 1;
    nFilterIndex := FFilterIndex;
    FCurrentFilterIndex := FFilterIndex;

    if IEGlobalSettings().UnicodeOS then
    begin
      // lpstrFilter
      TempFilterW := AllocFilterStrW(FFilter);
      lpstrFilter := PWideChar(TempFilterW);
      // lpstrFile
      SetLength(TempFilenameW, nMaxFile + 2);
      FillChar(TempFilenameW[1], sizeof(WideChar)*(nMaxFile+2), 0);
      lpstrFile := PWideChar(TempFilenameW);
      CopyMemory(@TempFilenameW[1], @FFileNameW[1], imin(length(FFileNameW), nMaxFile)*sizeof(WideChar));
      // lpstrInitialDir
      lpstrInitialDir := PWideChar(WideString(FInitialDir));
      // lpstrTitle
      lpstrTitle := PWideChar(WideString(FTitle));
    end
    else
    begin
      // lpstrFilter
      TempFilterA := AllocFilterStrA(AnsiString(FFilter));
      lpstrFilter := PAnsiChar(TempFilterA);
      // lpstrFile
      SetLength(TempFilenameA, nMaxFile + 1);
      FillChar(TempFilenameA[1], sizeof(AnsiChar)*(nMaxFile+1), 0);
      lpstrFile := PAnsiChar(TempFilenameA);
      CopyMemory(@TempFilenameA[1], @AnsiString(FFileNameW)[1], imin(length(FFileNameW), nMaxFile)*sizeof(AnsiChar));
      // lpstrInitialDir
      lpstrInitialDir := PAnsiChar(AnsiString(FInitialDir));
      // lpstrTitle
      lpstrTitle := PAnsiChar(AnsiString(FTitle));
    end;

    Flags := OFN_ENABLEHOOK;
    for Option := Low(Option) to High(Option) do
      if Option in FOptions then
        Flags := Flags or OpenOptions[Option];
    if NewStyleControls then
      Flags := Flags xor OFN_EXPLORER
    else
      Flags := Flags and not OFN_EXPLORER;

    TempExt := FDefaultExt;
    if (TempExt = '') and (Flags and OFN_EXPLORER = 0) then
    begin
      TempExt := IEExtractFileExtW(FFilenameW);
      Delete(TempExt, 1, 1);
    end;
    if TempExt = '' then
    begin
      TempExt := ExtractFilter(lpstrFilter, FCurrentFilterIndex);
      Delete(TempExt, 1, 1);
      q := pos(';', TempExt);
      Delete(TempExt, q, length(TempExt));
    end;
    if IEGlobalSettings().UnicodeOS then
      lpstrDefExt := PWideChar(TempExt)
    else
      lpstrDefExt := PAnsiChar(AnsiString(TempExt));

    if (ofOldStyleDialog in Options) or not NewStyleControls then
      lpfnHook := DialogHook
    else
      lpfnHook := ExplorerHook;

    if Template <> nil then
    begin
      Flags := Flags or OFN_ENABLETEMPLATE;
      if IEGlobalSettings().UnicodeOS then
        lpTemplateName := Template
      else
        lpTemplateName := PAnsiChar(AnsiString(WideString(Template)));
    end;

    {$ifndef OCXVERSION}
    if assigned(Application) then
      {$ifdef IEHASACTIVEFORMHANDLE}
      hWndOwner := Application.ActiveFormHandle
      {$else}
      hWndOwner := Application.Handle
      {$endif}
    else
      hWndOwner := IEFindHandle(Owner);
    {$else}
    hWndOwner := IEFindHandle(Owner);
    {$endif}

    if IEGlobalSettings().OpSys <> ieosWin98 then
    begin
      Move(OpenFileName, OpenFileNameEx, sizeof(OpenFileName));
      if fShowPlacesBar then
        OpenFileNameEx.FlagsEx := 0
      else
        OpenFileNameEx.FlagsEx := 1;
      OpenFileNameEx.lStructSize := sizeof(TIEOpenFileNameEx);
      Result := TaskModalDialog(Func, OpenFileNameEx);
      Move(OpenFileNameEx, OpenFileName, SizeOf(OpenFileName));
    end
    else
      Result := TaskModalDialog(Func, OpenFileName);

    if Result then
    begin
      GetFileNames(OpenFilename);
      if (Flags and OFN_EXTENSIONDIFFERENT) <> 0 then
        Include(FOptions, ofExtensionDifferent)
      else
        Exclude(FOptions, ofExtensionDifferent);
      if (Flags and OFN_READONLY) <> 0 then
        Include(FOptions, ofReadOnly)
      else
        Exclude(FOptions, ofReadOnly);
      FFilterIndex := nFilterIndex;
    end;
  end;
end;

function WideStrEnd(s: PWideChar): PWideChar;
begin
  result := s;
  while result^ <> #0 do
    inc(result);
end;

function AnsiStrEnd(s: PAnsiChar): PAnsiChar;
begin
  result := s;
  while result^ <> #0 do
    inc(result);
end;

procedure TOpenImageEnDialog.GetFileNames(var OpenFileName: TIEOpenFileNameEx);
var
  SeparatorW: WideChar;
  SeparatorA: AnsiChar;

  function ExtractFileNameW(P: PWideChar; var S: WideString): PWideChar;
  begin
    Result := WideStrScan(P, SeparatorW);
    if Result = nil then
    begin
      S := WideString(P);
      Result := WideStrEnd(P);
    end
    else
    begin
      SetString(S, P, Result - P);
      Inc(Result);
    end;
  end;

  procedure ExtractFileNamesW(P: PWideChar);
  var
    DirName, FileName: WideString;
  begin
    P := ExtractFileNameW(P, DirName);
    P := ExtractFileNameW(P, FileName);
    if FileName = '' then
    begin
      FFiles.Add(DirName);
      FFilesW.Add(DirName);
    end
    else
    begin
      if AnsiLastChar(DirName)^ <> '\' then
        DirName := DirName + '\';
      repeat
        if (FileName[1] <> '\') and ((Length(FileName) <= 3) or
          (FileName[2] <> ':') or (FileName[3] <> '\')) then
          FileName := DirName + FileName;
        FFiles.Add(FileName);
        FFilesW.Add(FileName);
        P := ExtractFileNameW(P, FileName);
      until FileName = '';
    end;
  end;

  function ExtractFileNameA(P: PAnsiChar; var S: AnsiString): PAnsiChar;
  begin
    Result := IEAnsiStrScan(P, SeparatorA);
    if Result = nil then
    begin
      S := AnsiString(P);
      Result := AnsiStrEnd(P);
    end
    else
    begin
      SetString(S, P, Result - P);
      Inc(Result);
    end;
  end;

  procedure ExtractFileNamesA(P: PAnsiChar);
  var
    DirName, FileName: AnsiString;
  begin
    P := ExtractFileNameA(P, DirName);
    P := ExtractFileNameA(P, FileName);
    if FileName = '' then
    begin
      FFiles.Add(string(DirName));
      FFilesW.Add(WideString(DirName));
    end
    else
    begin
      if AnsiLastChar(string(DirName))^ <> '\' then
        DirName := DirName + '\';
      repeat
        if (FileName[1] <> '\') and ((Length(FileName) <= 3) or
          (FileName[2] <> ':') or (FileName[3] <> '\')) then
          FileName := DirName + FileName;
        FFiles.Add(string(FileName));
        FFilesW.Add(WideString(FileName));
        P := ExtractFileNameA(P, FileName);
      until FileName = '';
    end;
  end;

var
  sa: AnsiString;
begin
  SeparatorA := #0;
  SeparatorW := #0;
  if (ofAllowMultiSelect in FOptions) and ((ofOldStyleDialog in FOptions) or not NewStyleControls) then
  begin
    SeparatorA := ' ';
    SeparatorW := ' ';
  end;
  with OpenFileName do
  begin
    if ofAllowMultiSelect in FOptions then
    begin
      if IEGlobalSettings().UnicodeOS then
        ExtractFileNamesW(lpstrFile)
      else
        ExtractFileNamesA(lpstrFile);
      FFileNameW := FFilesW[0];
    end
    else
    begin
      if IEGlobalSettings().UnicodeOS then
        ExtractFileNameW(lpstrFile, FFileNameW)
      else
      begin
        ExtractFileNameA(lpstrFile, sa);
        FFileNameW := WideString(sa);
      end;
      FFiles.Add(FFileNameW);
      FFilesW.Add(FFileNameW);
    end;
  end;
end;

function TOpenImageEnDialog.GetStaticRect: TRect;
begin
  if FHandle <> 0 then
  begin
    if not (ofOldStyleDialog in Options) then
    begin
      GetWindowRect(GetDlgItem(FHandle, stc32), Result);
      MapWindowPoints(0, FHandle, Result, 2);
    end
    else
      GetClientRect(FHandle, Result)
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TOpenImageEnDialog.GetLongFileName: WideString;
var
  PathW: PWideChar;
  PathA: PAnsiChar;
begin
  if NewStyleControls and (FHandle <> 0) then
  begin
    if IEGlobalSettings().UnicodeOS then
    begin
      getmem(PathW, 65536 * SizeOf(WideChar));
      try
        GetDlgItemTextW(GetParent(FHandle), cmb13, PathW, 65535);
        Result := IEExtractFilePathW(GetFileName) + WideString(PathW);
      finally
        freemem(PathW);
      end;
    end
    else
    begin
      getmem(PathA, 65536 * SizeOf(AnsiChar));
      try
        GetDlgItemTextA(GetParent(FHandle), cmb13, PathA, 65535);
        Result := IEExtractFilePathW(GetFileName) + WideString(AnsiString(PathA));
      finally
        freemem(PathA);
      end;
    end;
  end
  else
    Result := FFileNameW;
end;

function TOpenImageEnDialog.GetFileName: TFileName;
begin
  result := TFileName(GetFileNameW);
end;

function TOpenImageEnDialog.GetFileNameW: WideString;
var
  PathW: array[0..MAX_PATH] of WideChar;
  PathA: array[0..MAX_PATH] of AnsiChar;
begin
  if NewStyleControls and (FHandle <> 0) then
  begin
    if IEGlobalSettings().UnicodeOS then
    begin
      SendMessageW(GetParent(FHandle), CDM_GETFILEPATH, Length(PathW), NativeInt(@PathW));
      Result := WideString(PathW);
    end
    else
    begin
      SendMessageA(GetParent(FHandle), CDM_GETFILEPATH, Length(PathA), NativeInt(@PathA));
      Result := WideString(AnsiString(PathA));
    end;
  end
  else
    Result := FFileNameW;
end;


// only file name
function TOpenImageEnDialog.GetFileName2: WideString;
var
  PathW: array[0..MAX_PATH] of WideChar;
  PathA: array[0..MAX_PATH] of AnsiChar;
begin
  if NewStyleControls and (FHandle <> 0) then
  begin
    if IEGlobalSettings().UnicodeOS then
    begin
      SendMessageW(GetParent(FHandle), CDM_GETSPEC, Length(PathW), NativeInt(@PathW));
      Result := WideString(PathW);
    end
    else
    begin
      SendMessageA(GetParent(FHandle), CDM_GETSPEC, Length(PathA), NativeInt(@PathA));
      Result := WideString(AnsiString(PathA));
    end;
  end
  else
    Result := FFileNameW;
end;


function TOpenImageEnDialog.GetFilterIndex: Integer;
begin
  if FHandle <> 0 then
    Result := FCurrentFilterIndex
  else
    Result := FFilterIndex;
end;


procedure TOpenImageEnDialog.SetHistoryList(Value: TStrings);
begin
  FHistoryList.Assign(Value);
end;


procedure TOpenImageEnDialog.SetInitialDir(const Value: string);
var
  L: Integer;
begin
  L := Length(Value);
  if (L > 1) and IsPathDelimiter(Value, L) and not IsDelimiter(':', Value, L - 1) then
    Dec(L);
  FInitialDir := Copy(Value, 1, L);
end;


function TOpenImageEnDialog.Execute: Boolean;
begin
  if assigned(fOnCreateCustomControls) then
    fOnCreateCustomControls(self);
  try
    if fPreviewSize = iespLarge then
      Template := 'IDD_IETEMPLATE2'
    else
      Template := 'IDD_IETEMPLATE';
    SetLang;
    fPreviewCheck.visible := true;
    if fAutoSetFilter then
      Filter := BuildStrFilter(fAutoSetFilterFileType);
    if IEGlobalSettings().UnicodeOS then
      Result := DoExecute(@GetOpenFileNameW)
    else
      Result := DoExecute(@GetOpenFileNameA);
  finally
    if assigned(fOnDestroyCustomControls) then
      fOnDestroyCustomControls(self);
    FPicturePanel.ParentWindow := 0;
    fInfoPanel.ParentWindow := 0;
  end;
end;


function GetFirstFileName(s: WideString): WideString;
var
  path: WideString;
  names: WideString;
  cur: WideString;
  quot: Boolean;
  i: Integer;
begin
  path := IEExtractFilePathW(s);
  names := IEExtractFileNameW(s) + ' ';
  if names[1]<>#34 then
    // single file name, fail
    result := ''
  else
  begin
    // multiple file names
    cur := '';
    quot := false;
    for i := 1 to length(names) do
    begin
      if names[i] = #34 then
        quot := not quot
      else
      if (names[i] = #32) and not quot then
        break
      else
        cur := cur + names[i];
    end;
    result := path + cur;
  end;
end;


function TOpenImageEnDialog.NeedLoadPreviewFullSize: Boolean;
const
  Maximum_Thumbnail_Load_Size = 10;
begin
  Result := SelectedZoom >= Maximum_Thumbnail_Load_Size;
end;


function TOpenImageEnDialog.LoadPreview(bFullSize: Boolean) : Boolean;  

  function ValidFile(const FileName: WideString): Boolean;
  begin
    if IEGlobalSettings().UnicodeOS then
      result := GetFileAttributesW(PWideChar(FileName)) <> $FFFFFFFF
    else
      result := GetFileAttributesA(PAnsiChar(AnsiString(FileName))) <> $FFFFFFFF;
  end;

var
  sFilename: WideString;
  CursorBak: TCursor;
  ff: TIOFileType;
  additionalMultipageExts: TStringList;
  ss: WideString;
begin
  if ofAllowMultiSelect in Options then
  begin
    sFilename := GetFirstFileName(GetLongFileName); // first file name is the last selected
    if sFilename = '' then
      sFilename := FileNameW;
  end
  else
  begin
    sFilename := FileNameW;
  end;

  // Valid picture?
  Result := IEFileExistsW(sFilename) and ValidFile(sFilename);

  if Result = False then
    exit;
       
  FPreviewIsFullSize := True;
  try
    fFileSize := IEGetFileSize(sFilename); // fFileSize is assignable only here (!?)
    if not fPreviewCheck.Checked then
    begin
      // do not show the image
      if assigned(fOnPreviewFile) then
        fOnPreviewFile(self, fImageEnView, sFilename, true)
      else
        fImageEnView.IO.ParamsFromFile(WideString(sFilename));

      fFrames := 1;
      if fImageEnView.IO.Params.FileType <> ioUnknown then
        ShowIOParams(fImageEnView.IO.Params)
      else
        ShowIOParams(nil);
    end
    else
    begin
      // show the image
      CursorBak := Screen.Cursor;
      try
        Screen.Cursor := crHourGlass;
        fImageEnView.Background := IEGlobalSettings().PreviewImageBackgroundColor;
        fImageEnView.blank;
        fImageEnView.Paint;
        {$IFDEF IEINCLUDEMULTIVIEW}
        fImageEnMView.background := IEGlobalSettings().PreviewImageBackgroundColor;
        fImageEnMView.Clear;
        fImageEnMView.Paint;
        fPlayButton.down := false;
        {$ENDIF}
        ss := IEExtractFileExtS(sFilename);
        ff := FindFileFormat(WideString(sFilename), false);
  {$IFDEF IEINCLUDEMULTIVIEW}
        additionalMultipageExts := TStringList.Create;
        additionalMultipageExts.CommaText := LowerCase(IEGlobalSettings().PreviewAdditionalMultipageExts);
        if ( (ff=ioAVI) or
            ((ff=ioGIF) and (@IEFileFormatGetInfo(ioGIF).ReadFunction <> nil) and (EnumGIFIm(WideString(sFilename)) > 1)) or
            ((ff=IOTIFF) and (EnumTIFFIm(WideString(sFilename)) > 1)) or
             (ff=ioDCX) or
            (additionalMultipageExts.IndexOf( copy(ss, 2, length(ss)) ) > -1)
            {$ifdef IEINCLUDEDICOM}
            or ((ff=ioDICOM) and (IEGetFileFramesCount(WideString(sFilename))>1))
            {$endif}
            {$ifdef IEINCLUDEWIC}
            or ((ff=ioHDP) and IEWICAvailable() and (IEGetFileFramesCount(WideString(sFilename))>1))
            {$endif}
           )
          and fShowAllFrames then
        begin
          // animated images
          fImageEnMView.ThumbWidth := ORIGThumbWidth;
          fImageEnMView.ThumbHeight := ORIGThumbHeight;
          fImageEnMView.HorizBorder := ORIGHorizBorder;
          fImageEnMView.VertBorder := ORIGVertBorder;
          fPlayButton.enabled := true;
          FZoomComboBox.enabled := false;
          fImageEnMView.show;
          fImageEnView.hide;
          with fProgressBar do
          begin
            Parent := fImageEnMView;
            position := 0;
            show;
            repaint;
          end;
          fImageEnMView.MIO.AutoAdjustDPI := fAutoAdjustDPI;
          fImageEnMView.MIO.FilteredAdjustDPI := fFilteredAdjustDPI;

          if assigned(fOnPreviewFile) then
            fOnPreviewFile(self, fImageEnMView, sFilename, false)
          else
            fImageEnMView.LoadFromFileOnDemand(WideString(sFilename));

          fFrames := fImageEnMView.MIO.ParamsCount;
          fImageEnMView.SelectSeek(iskFirst);
          FPreviewButton.Enabled := True;
          fZoomComboBox.Enabled := False;
          if fFrames>0 then
            ShowIOParams(fImageEnMView.MIO.Params[0]);
          if fAlwaysAnimate and ((ss = '.avi') or ((ss = '.gif') and (@IEFileFormatGetInfo(ioGIF).ReadFunction <> nil))) then
          begin
            fPlayButton.down := True;
            PlayClick(self);
          end;
        end
        else
        begin
          // static images
          fPlayButton.enabled := false;
  {$ENDIF}
          FZoomComboBox.enabled := true;
          fImageEnView.Show;
          {$IFDEF IEINCLUDEMULTIVIEW}
          fImageEnMView.hide;
          {$ENDIF}
          with fProgressBar do
          begin
            Parent := fImageEnView;
            position := 0;
            show;
            repaint;
          end;
          if bFullSize then
          begin
            fImageEnView.IO.Params.Width  := Screen.Width;
            fImageEnView.IO.Params.Height := Screen.Height;
            fImageEnView.IO.Params.JPEG_Scale := ioJPEG_AUTOCALC;
            {$ifdef IEINCLUDERAWFORMATS}
            fImageEnView.IO.Params.RAW_HalfSize := true;        // Keep as true.  Raw files will invariably be huge
            fImageEnView.IO.Params.RAW_GetExifThumbnail := False;
            {$endif}
            fImageEnView.IO.Params.JPEG_GetExifThumbnail := False;
          end
          else
          begin
            // Scaled or embedded thumbnail size     
            fImageEnView.IO.Params.Width  := FImageEnView.Width;
            fImageEnView.IO.Params.Height := fImageEnView.Height;
            fImageEnView.IO.Params.JPEG_Scale := ioJPEG_AUTOCALC;
            {$ifdef IEINCLUDERAWFORMATS}
            fImageEnView.IO.Params.RAW_HalfSize := true;
            fImageEnView.IO.Params.RAW_GetExifThumbnail := fGetExifThumbnail;
            {$endif}
            fImageEnView.IO.Params.JPEG_GetExifThumbnail := fGetExifThumbnail;
            FPreviewIsFullSize := False;
          end;
          fImageEnView.IO.AutoAdjustDPI := fAutoAdjustDPI;
          fImageEnView.IO.FilteredAdjustDPI := fFilteredAdjustDPI;
          fImageEnView.RemoveAllObjects;
          fImageEnView.LayersClear;

          if assigned(fOnPreviewFile) then
            fOnPreviewFile(self, fImageEnView, sFilename, false)
          else
            fImageEnView.IO.LoadFromFileAuto(WideString(sFilename));

          if fImageEnView.IO.Params.FileType = ioAVI then
            fFrames := fImageEnView.IO.Params.AVI_FrameCount
          else
          if fImageEnView.IO.Params.FileType = ioGIF then
            fFrames := EnumGIFIm(WideString(sFilename))
          else
          if fImageEnView.IO.Params.FileType = ioTIFF then
            fFrames := EnumTIFFIm(WideString(sFilename))
          else
          if fImageEnView.IO.Params.FileType = ioICO then
            fFrames := EnumICOIm(WideString(sFilename))
          else
          if fImageEnView.IO.Params.FileType = ioDCX then
            fFrames := EnumDCXIm(WideString(sFilename))
          {$ifdef IEINCLUDEDICOM}
          else
          if fImageEnView.IO.Params.FileType = ioDICOM then
            fFrames := IEGetFileFramesCount(WideString(sFilename))
          {$endif}
          {$ifdef IEINCLUDEWIC}
          else
          if fImageEnView.IO.Params.FileType = ioHDP then
            fFrames := IEGetFileFramesCount(WideString(sFilename))
          {$endif}
          else
            fFrames := 1;

          // Minimize reloading if we launch a preview
          if ((fImageEnView.IO.Params.FileType = ioRawDLLPlugIn) or (fImageEnView.IO.Params.FileType in [ioJPEG, ioRaw])) = False then
            FPreviewIsFullSize := True;

          ShowIOParams(fImageEnView.IO.Params);
          FPreviewButton.Enabled := True;
          fZoomComboBox.Enabled := true;
  {$IFDEF IEINCLUDEMULTIVIEW}
        end;
        additionalMultipageExts.Free;
  {$ENDIF}
        fProgressBar.hide;
        SetZoom;
      finally
        Screen.Cursor := CursorBak;
      end;
    end; // if fPreviewCheck
  except
    Result := False;
  end;
end;

procedure TOpenImageEnDialog.DoSelectionChange;
var
  bLoadedPreview: Boolean;
begin
  if m_blnInSelectionChange then
    Exit;
  if (not fShowPreview) then
  begin
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
    exit;
  end;

  m_blnInSelectionChange := True;
  try
    bLoadedPreview := LoadPreview(NeedLoadPreviewFullSize);

    if not bLoadedPreview then
    begin
      FInfoLabel1.Caption := '';
      fInfoLabel2.Caption := '';
      FPreviewButton.Enabled := False;
      fZoomComboBox.Enabled := false;
      FImageEnView.blank;
      FPicLabel1.Caption := '';
      FPicLabel2.Caption := '';
      FPicLabel3.Caption := '';
      {$IFDEF IEINCLUDEMULTIVIEW}
      fImageEnMView.Clear;
      fPlayButton.Enabled := false;
      {$ENDIF}
    end;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  finally
    m_blnInSelectionChange := False;
  end;
end;


procedure TOpenImageEnDialog.PreviewClick(Sender: TObject);
var
  PreviewForm: TForm;
begin
  PreviewForm := TForm.Create(Self);
  with PreviewForm do
  try
    Name := 'PreviewForm';
    Caption := Self.FileNameW;
    BorderStyle := bsSizeToolWin;
    KeyPreview := True;
    Position := poScreenCenter;
    OnKeyPress := PreviewKeyPress;
    with TImageEnVect.Create(PreviewForm) do
    begin
      Name := 'ImageEnView';
      Parent := PreviewForm;
      Align := alClient;
      Cursor := crDefault;
      MouseInteract := [miScroll, miZoom];
      if fImageEnView.visible then
      begin
        if FPreviewIsFullSize = False then
          LoadPreview(True);
        Assign(FImageEnView)
      end
      {$IFDEF IEINCLUDEMULTIVIEW}
      else
      if fImageEnMView.visible then
        Assign(fImageEnMView.Bitmap)
      {$ENDIF};

      Background := IEGlobalSettings().PreviewImageBackgroundColor;
      backgroundstyle := IEGlobalSettings().PreviewImageBackgroundStyle;
      EnableAlphaChannel := True;
      Center := True;
      ScrollBars := ssBoth;
      if (Bitmap.Width > 100) then
      begin
        if (Bitmap.Width < Screen.Width) then
          PreviewForm.ClientWidth := Bitmap.Width
        else
          PreviewForm.Width := Round(Screen.Width * 0.8);
      end
      else
        PreviewForm.Width := 100;
      if (Bitmap.Height > 100) then
      begin
        if (Bitmap.Height < Screen.Height) then
          PreviewForm.ClientHeight := Bitmap.Height
        else
          PreviewForm.Height := Round(Screen.Height * 0.8);
      end
      else
        PreviewForm.Height := 100;
    end;
    ShowModal;
  finally
    Free;
  end;
end;


procedure TOpenImageEnDialog.PlayClick(Sender: TObject);
begin
  {$IFDEF IEINCLUDEMULTIVIEW}
  fImageEnMView.Playing := fPlayButton.down;
  fPreviewButton.Enabled := not fPlayButton.down;
  //fZoomComboBox.Enabled := not fPlayButton.down;

  if fImageEnMView.Playing then
  begin
    fImageEnMView.ThumbWidth  := fImageEnMView.Width - IEGlobalSettings().EdgeX * 2;
    fImageEnMView.ThumbHeight := fImageEnMView.Height - IEGlobalSettings().EdgeY * 2;
    fImageEnMView.HorizBorder := 0;
    fImageEnMView.VertBorder  := 0;
  end;
  {$ENDIF}
end;


procedure TOpenImageEnDialog.PreviewKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    TForm(Sender).Close;
end;


procedure TOpenImageEnDialog.ZoomComboChange(Sender: TObject);
begin
  SetZoom;
end;


procedure TOpenImageEnDialog.ImageEnIOProgress(Sender: TObject;
  per: integer);
begin
  fProgressBar.position := per;
end;

function ConcatAndIns(const Value1, Value2: string; const sDelimiter: string = ','): string;
begin
  if (Value1 <> '') and (Value2 <> '') then
    Result := Value1 + sDelimiter + Value2
  else
    Result := Value1 + Value2;
end;

// if params=nil then empties fields
procedure TOpenImageEnDialog.ShowIOParams(params: TIOParamsVals);
const
  SIXTEEN_MILLION_COLORS = 16777216;
  INCLUDE_COLOR_BIT_COUNT = False;

  function _ColorCountToStr(iColors : Integer) : string;
  var
    bEnglish: Boolean;
  begin
    bEnglish := (IEGlobalSettings().MsgLanguage = msEnglish) or ((IEGlobalSettings().MsgLanguage = msSystem) and (syslocale.PriLangID = LANG_ENGLISH));
    if bEnglish and (iColors = SIXTEEN_MILLION_COLORS) then
      result := '16M'
    else
      result := string(IEIntToFormattedStr(iColors));
  end;

var
  ss: string;
  mdim, bitcount, ww, hh: integer;
  sSize: string;
begin
  if assigned(Params) then
  begin
    with Params do
    begin
      ww := OriginalWidth;
      hh := OriginalHeight;
      // Width X Height Pixel (frames)
      if (ww>0) and (hh>0) then
        sSize := IntToStr(ww) + ' x ' + IntToStr(hh) + ' '+iemsg(IEMSG_PIXELS)
      else
        sSize := '';
      if fFrames > 1 then
        sSize := ConcatAndIns(sSize, IntToStr(fFrames) + ' ' + iemsg(IEMSG_FRAMES), ', ');

      // xxx colors
      if (SamplesPerPixel = 4) and (BitsPerSample = 8) then
        ss := _ColorCountToStr(SIXTEEN_MILLION_COLORS)
      else
      if (SamplesPerPixel = 3) and (BitsPerSample = 16) then
        ss := '48 bit'
      else
        ss := _ColorCountToStr(1 shl (SamplesPerPixel * BitsPerSample));  
      ss := ss + ' ' + iemsg(IEMSG_COLORS);

      if INCLUDE_COLOR_BIT_COUNT then
      begin
        ss := ss + ' (' + IntToStr(SamplesPerPixel * BitsPerSample) + ' '+iemsg(IEMSG_BIT)+')';
      end;
      fPicLabel2.caption := ConcatAndIns(sSize, ss, ', ');
      // file size
      ss := string(IEBytesToStr(fFileSize, 1024)) + ' (' + iemsg(IEMSG_FILE) + ')';
      // memory size
      // solo pf1bit e pf24bit sono attualmente supportati
      if (SamplesPerPixel = 1) and (BitsPerSample = 1) then
        bitcount := 1
      else
        bitcount := 24;

      mdim := (((ww * BitCount) + 31) div 32) * 4 * hh; // only one frames is calculated
      ss := ss + ', ' + string(IEBytesToStr(mdim, 1024)) + ' (' + iemsg(IEMSG_MEM) + ')';

      if fShowPreview then
        fInfoLabel2.Caption := ''
      else
        fInfoLabel2.Caption := ss;
      fPicLabel3.caption := ss;

      // compression
      ss := FileTypeStr;
      if fShowPreview then
        fInfoLabel1.Caption := ''
      else
        fInfoLabel1.Caption := ss;
      fPicLabel1.caption := ss;
    end;
  end
  else
  begin
    fPicLabel1.caption := '';
    fPicLabel3.caption := '';
    fPicLabel2.caption := '';
    fInfoLabel2.Caption := '';
    fInfoLabel1.Caption := '';
  end;
end;


// Result is -1 if Zoom box is set to Autofit, otherwise a zoom level, e.g. 100
function TOpenImageEnDialog.SelectedZoom : Integer;
var
  intItemIndex: integer;
begin
  Result := -1;
  if not Assigned(FZoomComboBox) then
    Exit;
  if not Assigned(FImageEnView) then
    Exit;
  intItemIndex := FZoomComboBox.ItemIndex;
  if (intItemIndex < 0) then
    Exit;
  if Assigned(FZoomComboBox.Items.Objects[intItemIndex]) then
    Result := Integer(FZoomComboBox.Items.Objects[intItemIndex])
end;


procedure TOpenImageEnDialog.SetZoom;
begin
  if (Assigned(FZoomComboBox) = False) or (Assigned(FImageEnView) = False) then
    Exit;

  if SelectedZoom > 0 then
  begin
    fImageEnView.LockPaint;
    if (FPreviewIsFullSize = False) and NeedLoadPreviewFullSize then
      LoadPreview(True);
    fImageEnView.AutoFit := false;
    FImageEnView.Zoom := SelectedZoom;
    fImageEnView.UnlockPaint;
  end
  else
  begin
    fImageEnView.AutoFit := true;
    fImageEnView.Update;
  end;
end;


procedure TOpenImageEnDialog.DoShow;
var
  ClientRect, PreviewRect, StaticRect: TRect;
begin
  GetClientRect(handle, ClientRect);
  StaticRect := GetStaticRect();
  if fShowPreview then
  begin
    // PicturePanel
    PreviewRect := ClientRect;
    PreviewRect.Left := StaticRect.Right;
    PreviewRect.Top := StaticRect.Top;
    PreviewRect.Bottom := imax(StaticRect.Bottom, StaticRect.Top + fPicLabel3.BoundsRect.Bottom);
    FPicturePanel.BoundsRect := PreviewRect;
    FPicturePanel.ParentWindow := handle;
    {$IFDEF IEINCLUDEMULTIVIEW}
    fImageEnMView.ParentWindow := fpicturepanel.handle;
    {$ENDIF}
    fImageEnView.ParentWindow := fPicturepanel.handle;
    with FZoomComboBox, Items do
    begin
      AddObject(iemsg(IEMSG_FIT), nil);
      AddObject('25%', TObject(25));
      AddObject('50%', TObject(50));
      AddObject('75%', TObject(75));
      AddObject('100%', TObject(100));
      AddObject('125%', TObject(125));
      AddObject('150%', TObject(150));
      AddObject('175%', TObject(175));
      AddObject('200%', TObject(200));
      AddObject('300%', TObject(300));
      AddObject('400%', TObject(400));
      ItemIndex := 0;
    end;
    FImageEnView.Background := IEGlobalSettings().PreviewImageBackgroundColor;
    FImageEnView.Center := True;
    {$IFDEF IEINCLUDEMULTIVIEW}
    fPlayButton.Down := false;
    fImageEnMView.Playing := false;
    fPlayButton.Enabled := false;
    {$ENDIF}
  end;
  // Info panel
  if fExtendedDialog then
  begin
    fInfoPanel.Top    := StaticRect.Bottom;
    fInfoPanel.Left   := StaticRect.Left;
    fInfoPanel.Width  := Clientrect.Right;
    fInfoPanel.Height := ClientRect.Bottom;
    fInfoPanel.ParentWindow := handle;
  end
  else
  begin
    FPicLabel1.Visible := false;
    FPicLabel2.Visible := false;
    FPicLabel3.Visible := false;
    FZoomComboBox.Visible := false;
  end;
  inherited DoShow;
end;
              

function TSaveImageEnDialog.Execute: Boolean;
var
  fBackParams: TIOParamsVals;
begin
  fBackParams := TIOParamsVals.Create(nil);
  try
    if assigned(fOnCreateCustomControls) then
      fOnCreateCustomControls(self);

    fShowPreview := sdShowPreview in fExOptions;
      if fShowPreview = False then
        Template := 'IDD_IETEMPLATE1'
      else
      if fPreviewSize = iespLarge then        
        Template := 'IDD_IETEMPLATE2'
      else
        Template := 'IDD_IETEMPLATE';
    if fAutoSetFilter then
      Filter := BuildStrFilter(fAutoSetFilterFileType);
    fAdvancedButton.OnClick := DoAdvanced;
    {$IFDEF IEINCLUDEDIALOGIO}
    fAdvancedButton.visible := Assigned(fAttachedImageEnIO) and (sdShowAdvanced in fExOptions);
    {$ENDIF}
    SetLang;
    if fShowPlacesBar then
      fAdvancedButton.SetBounds(474, 0, 75, 23)
    else
      fAdvancedButton.SetBounds(333, 0, 75, 23);
    fPreviewCheck.visible := sdShowPreview in fExOptions;
    // save Params in fBackParams
    if fAttachedImageEnIO is TImageEnIO then
      fBackParams.Assign((fAttachedImageEnIO as TImageEnIO).Params)
    {$IFDEF IEINCLUDEMULTIVIEW}
    else
    if fAttachedImageEnIO is TImageEnMIO then
    with fAttachedImageEnIO as TImageEnMIO do
    begin
      if ParamsCount > 0 then
        fBackParams.Assign(Params[0])
      else
      begin
        result := false;
        exit;
      end;
    end
    else
    if fAttachedImageEnIO is TImageEnMView then
    with (fAttachedImageEnIO as TImageEnMView).MIO do
    begin
      if ParamsCount > 0 then
        fBackParams.Assign(Params[0])
      else
      begin
        result := false;
        exit;
      end;
    end
    {$ENDIF}
    else
    if fAttachedImageEnIO is TImageEnView then
      fBackparams.Assign((fAttachedImageEnIO as TImageEnView).IO.Params);
    //
    EnableDisableAdvanced;
    if IEGlobalSettings().UnicodeOS then
      Result := DoExecute(@GetSaveFileNameW)
    else
      Result := DoExecute(@GetSaveFileNameA);
    if result = false then
    begin
      // restore Params
      if fAttachedImageEnIO is TImageEnIO then
        (fAttachedImageEnIO as TIMageEnIO).Params.Assign(fBackParams)
    {$IFDEF IEINCLUDEMULTIVIEW}
      else
      if fAttachedImageEnIO is TImageEnMIO then
        (fAttachedImageEnIO as TImageEnMIO).Params[0].Assign(fBackParams)
      else
      if fAttachedImageEnIO is TImageEnMView then
        (fAttachedImageEnIO as TImageEnMView).MIO.Params[0].Assign(fBackParams)
    {$ENDIF}
      else
      if fAttachedImageEnIO is TImageEnView then
        (fAttachedImageEnIO as TImageEnView).IO.Params.Assign(fBackParams);
    end;
    // add the extension to the file name (if there isn't)
    if (FileNameW <> '') and (IEExtractFileExtW(FileNameW) = '') then
      FileNameW := FileNameW + WideString(ExtractFirstExt(fSelType));

  finally
    if assigned(fOnDestroyCustomControls) then
      fOnDestroyCustomControls(self);
    FreeAndNil(fBackParams);
    FPicturePanel.ParentWindow := 0;
    fInfoPanel.ParentWindow := 0;
  end;
end;


constructor TSaveImageEnDialog.Create(AOwner: TComponent);
var
  sFilter: string;
  fi: TIEFileFormatInfo;
begin
  inherited;
  fShowPreview := true;
  sFilter := 'JPEG Bitmap (JPG)|*.jpg';
  fi := IEFileFormatGetInfo(ioGIF);
  if assigned(fi) and (@fi.WriteFunction <> nil) then
    sFilter := sFilter + '|CompuServe Bitmap (GIF)|*.gif';
  sFilter := sFilter + '|TIFF Bitmap (TIF)|*.tif';
  sFilter := sFilter + '|PaintBrush (PCX)|*.pcx';
  sFilter := sFilter + '|Portable Network Graphics (PNG)|*.png';
  sFilter := sFilter + '|Windows Bitmap (BMP)|*.bmp';
  sFilter := sFilter + '|OS/2 Bitmap (BMP)|*.bmp';
  sFilter := sFilter + '|Targa Bitmap (TGA)|*.tga';
  sFilter := sFilter + '|Portable PixMap (PXM)|*.pxm';
  sFilter := sFilter + '|Portable PixMap (PPM)|*.ppm';
  sFilter := sFilter + '|Portable GreyMap (PGM)|*.pgm';
  sFilter := sFilter + '|Portable Bitmap (PBM)|*.pbm';
  sFilter := sFilter + '|JPEG2000 (JP2)|*.jp2';
  sFilter := sFilter + '|JPEG2000 Code Stream (J2K)|*.j2k';
  sFilter := sFilter + '|Multipage PCX (DCX)|*.dcx';
  Filter := sFilter;
  //
  fExOptions := [sdShowPreview, sdShowAdvanced];
  fAttachedImageEnIO := nil;
  fWatchTimer := TTimer.Create(self);
  fWatchTimer.Enabled := false;
  fWatchTimer.Interval := 100;
  fWatchTimer.OnTimer := OnWatchTimer;
end;

destructor TSaveImageEnDialog.Destroy;
begin
  FreeAndNil(fWatchTimer);
  inherited;
end;

{!!
<FS>TSaveImageEnDialog.AttachedImageEnIO

<FM>Declaration<FC>
property AttachedImageEnIO: TComponent;

<FM>Description<FN>
You can attach the dialog to a <A TImageEnIO>, <A TImageEnMIO>, <A TImageEnMView> or <A TImageEnView>. When it is attached the user can click the <L TSaveImageEnDialog.ExOptions>"Advanced" button</L> in the save dialog to read/write the <A TImageEnIO.Params> (which are only updated if the user clicks "OK").

If no ImageEn component is attached to the TSaveImageEnDialog it will function as a standard save dialog with an image preview, but without an "Advanced" button.
!!}
procedure TSaveImageEnDialog.SetAttachedImageEnIO(v: TComponent);
begin
  if assigned(v) and
    ((v is TImageEnIO){$IFDEF IEINCLUDEMULTIVIEW} or (v is TImageEnMIO) or (v is TImageEnMView){$ENDIF} or (v is TImageEnView)) then
  begin
    fAttachedImageEnIO := v;
    fAttachedImageEnIO.FreeNotification(self);
  end;
end;

procedure TSaveImageEnDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = fAttachedImageEnIO) and (Operation = opRemove) then
  begin
    fAttachedImageEnIO := nil;
  end;
end;

procedure TSaveImageEnDialog.EnableDisableAdvanced;
var
  pp: TPreviewParams;
  ft: TIOFileType;
begin
  GetAdvancedType(pp, ft);
  if fAttachedImageEnIO is TImageEnIO then
    (fAttachedImageEnIO as TImageEnIO).Params.FileType := ft;
  fAdvancedButton.Enabled := pp<>[];
end;

procedure TSaveImageEnDialog.GetAdvancedType(var pp: TPreviewParams; var ft: TIOFileType);

  function ConvertExt(e: string; var ft: TIOFileType): TPreviewParams;
  var
    fi: TIEFileFormatInfo;
  begin
    fi := IEFileFormatGetInfo2(e);
    if fi <> nil then
    begin
      result := fi.DialogPage;
      ft := fi.FileType;
    end
    else
    begin
      result := [];
      ft := 0;
    end;
  end;
var
  e: WideString;
begin
  e := IEExtractFileExtW(FileNameW);
  if e <> '' then
    pp := ConvertExt(e, ft)
  else
    pp := ConvertExt(DefaultExt, ft);
  if (pp = []) and  (ft=0) and (fSelType<>'') then
    pp := ConvertExt(IEExtractFileExtW(fSelType), ft);
end;

procedure TSaveImageEnDialog.DoAdvanced(Sender: TObject);
{$IFDEF IEINCLUDEDIALOGIO}
var
  pp: TPreviewParams;
  ft: TIOFileType;
begin
  if assigned(fAttachedImageEnIO) = False then
    exit;

  GetAdvancedType(pp, ft);
  if pp <> [] then
  begin
    if fAttachedImageEnIO is TImageEnIO then
    begin
      (fAttachedImageEnIO as TImageEnIO).Params.FileType := ft;
      (fAttachedImageEnIO as TImageEnIO).DoPreviews(pp);
    end
    else
    {$IFDEF IEINCLUDEMULTIVIEW}
    if fAttachedImageEnIO is TImageEnMIO then
      (fAttachedImageEnIO as TImageEnMIO).DoPreviews(-1, pp)
    else
    if fAttachedImageEnIO is TImageEnMView then
      (fAttachedImageEnIO as TImageEnMView).MIO.DoPreviews(-1, pp)
    else
    {$ENDIF}
    if fAttachedImageEnIO is TImageEnView then
    begin
      (fAttachedImageEnIO as TImageEnView).IO.Params.FileType := ft;
      (fAttachedImageEnIO as TImageEnView).IO.DoPreviews(pp);
    end;
  end;
end;
{$ELSE} // {$ifdef IEINCLUDEDIALOGIO}
begin
end;
{$ENDIF}



procedure TSaveImageEnDialog.SetFileNameExt;
var
  q: integer;
  s: WideString;
begin
  s := IEExtractFileNameW(GetFileName2);
  q := pos('.', s);
  if q > 0 then
    s := copy(s, 1, q - 1);
  s := s + ExtractFirstExt(fSelType);
  if IEGlobalSettings().UnicodeOS then
    SendMessageW(GetParent(FHandle), CDM_SETCONTROLTEXT, edt1, LPARAM(PWideChar(s)))
  else
    SendMessageA(GetParent(FHandle), CDM_SETCONTROLTEXT, edt1, LPARAM(PAnsiChar(AnsiString(s))));
end;


procedure TSaveImageEnDialog.DoTypeChange;
begin
  SetFileNameExt;
  EnableDisableAdvanced;
  inherited;
end;


procedure TOpenImageEnDialog.DoCheckPreview(Sender: TObject);
begin
  if fPreviewCheck.Checked then
  begin
    // reload image
    DoSelectionChange;
  end
  else
  begin
    // empty fields
    ShowIOParams(nil);
    fImageEnView.blank;
    fImageEnView.background := IEGlobalSettings().PreviewImageBackgroundColor;
    fImageEnView.Zoom := 100;
    {$IFDEF IEINCLUDEMULTIVIEW}
    fImageEnMView.Clear;
    fImageEnMView.background := IEGlobalSettings().PreviewImageBackgroundColor;
    fImageEnMView.Hide;
    {$ENDIF}
    FPreviewButton.Enabled := False;
    fZoomComboBox.Enabled := false;
  end;
end;


{!!
<FS>TOpenImageEnDialog.PreviewBorderStyle

<FM>Declaration<FC>
property PreviewBorderStyle: <A TIEDBorderStyle>;

<FM>Description<FN>
Specifies the border style for the preview box that is used for single frame images.
!!}
procedure TOpenImageEnDialog.SetPreviewBorderStyle(v: TIEDBorderStyle);
begin
  fPreviewBorderStyle := v;
  with FImageEnView do
    case fPreviewBorderStyle of
      iepsDefault:
        begin
          BackgroundStyle := iebsSolid;
          BorderStyle := bsSingle;
        end;
      iepsCropped:
        begin
          BackgroundStyle := iebsCropped;
          BorderStyle := bsNone;
        end;
      iepsCropShadow:
        begin
          BackgroundStyle := iebsCropShadow;
          BorderStyle := bsNone;
        end;
      iepsSoftShadow:
        begin
          BackgroundStyle := iebsSoftShadow;
          BorderStyle := bsNone;
        end;
    end;
end;

procedure TOpenImageEnDialog.SetZoomFilter(v: TResampleFilter);
begin
  FImageEnView.ZoomFilter := v;
end;

{!!
<FS>TOpenImageEnDialog.ZoomFilter

<FM>Declaration<FC>
property ZoomFilter: <A TResampleFilter>;

<FM>Description<FN>
Specifies the filter to apply when displaying an image preview (to improve its quality).

!!}
function TOpenImageEnDialog.GetZoomFilter: TResampleFilter;
begin
  result := FImageEnView.ZoomFilter;
end;

{$IFDEF IEINCLUDEMULTIVIEW}
{!!
<FS>TOpenImageEnDialog.AlwaysAnimate

<FM>Declaration<FC>
property AlwaysAnimate: Boolean;

<FM>Description<FN>
When enabled, GIF and AVI files will be automatically animated/played in the preview window.
!!}
procedure TOpenImageEnDialog.SetAlwaysAnimate(value: Boolean);
begin
  fAlwaysAnimate := value;
  if fAlwaysAnimate then
    ShowAllFrames := true;
end;
{$endif}


      
{!!
<FS>TOpenImageEnDialog.PreviewSize

<FM>Declaration<FC>
property PreviewSize : <A TIEPreviewSize>;

<FM>Description<FN>
Specifies the size of the image preview that is shown on the right of the dialog.
!!}
procedure TOpenImageEnDialog.SetPreviewSize(const v : TIEPreviewSize);
// Xequte: 19/11/12
const
  Height_Increase = 66;
  Width_Increase  = 148;
var
  iExtraHeight : integer;
  AAlignment : TAlignment;
  iExtraWidth: Integer;
begin
  if fPreviewSize = v then
    exit;
  fPreviewSize := v;

  // Default
  iExtraHeight := 0;
  iExtraWidth  := 0;
  AAlignment := taLeftJustify;

  // Tall or Large
  if v <> iespDefault then
  begin
    iExtraHeight := Height_Increase;
    AAlignment := taRightJustify;
  end;

  // Large
  if v = iespLarge then
  begin
    iExtraWidth := Width_Increase;
  end;


  // Note: must align TOpenImageEnDialog.Create
  fPicturePanel.SetBounds(204, 5, 233 + iExtraWidth, 200);
  fPicLabel1.SetBounds(6, 180 + 80 + iExtraHeight, 227 + iExtraWidth, 23);
  fPicLabel1.Alignment := AAlignment;
  fPicLabel2.SetBounds(6, 203 + 80 + iExtraHeight, 227 + iExtraWidth, 23);
  fPicLabel2.Alignment := AAlignment;
  fPicLabel3.SetBounds(6, 226 + 80 + iExtraHeight, 227 + iExtraWidth, 23);
  fPicLabel3.Alignment := AAlignment;
  fImageEnView.SetBounds(6, 36, 133 + 100 + iExtraWidth, 139 + 78 + iExtraHeight);
  {$IFDEF IEINCLUDEMULTIVIEW}
  fImageEnMView.SetBounds(6, 36, 133 + 100 + iExtraWidth, 139 + 78 + iExtraHeight);
  {$ENDIF}
end;



procedure TSaveImageEnDialog.OnWatchTimer(Sender: TObject);
begin
  EnableDisableAdvanced;
end;

procedure TOpenImageEnDialog.DoAllDisplayed(Sender: TObject);
begin
  {$IFDEF IEINCLUDEMULTIVIEW}
  if fImageEnMView.SelectedImage>-1 then
    ShowIOParams(fImageEnMView.MIO.Params[fImageEnMView.SelectedImage]);
  {$endif}
end;

procedure TOpenImageEnDialog.SetFileName(value: TFileName);
begin
  FileNameW := WideString(value);
end;


{$ELSE} // {$ifdef IEINCLUDEOPENSAVEDIALOGS}

interface
implementation

{$ENDIF}



end.




