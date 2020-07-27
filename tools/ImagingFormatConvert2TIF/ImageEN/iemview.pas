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
File version 1062
Doc revision 1004
*)

unit iemview;

{$R-}
{$Q-}

{$I ie.inc}

{$ifdef IESUPPORTDEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$endif}

{$IFDEF IEINCLUDEMULTIVIEW}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$ifdef IEHASTYPES} Types, {$endif}
  {$ifdef IEHASUITYPES} System.UITypes, {$endif}
  ExtCtrls, Clipbrd, stdctrls, hyieutils, hyiedefs, ImageEnView, ImageEnProc, ImageEnIO, ieview, iemio, iepresetim, ievect,
  ieanimation, iexTransitions;

type


{!!
<FS>TIEImageAddEvent

<FM>Declaration<FC>
type TIEImageAddEvent = procedure(Sender : TObject;
                                  idx : integer;
                                  const sFilename : string;
                                  bFolder : boolean;
                                  bHiddenFile : boolean;
                                  iFileSizeBytes : Int64;
                                  CreateDate : TDateTime;
                                  EditDate : TDateTime;
                                  var bAllow : Boolean) of object;

<FM>Description<FN>
Occurs whenever an image is added to a <A TImageEnFolderMView> or when using <L TImageEnMView.FillFromDirectory>TImageEnMView.FillFromDirectory</L>.

Set bAllow to skip the addition of certain files.

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>Sender</C> <C>The <A TImageEnFolderMView>  or <ATImageEnMView></C> </R>
<R> <C>idx</C> <C>The new position for this file</C> </R>
<R> <C>sFilename</C> <C>The full path of the file or folder</C> </R>
<R> <C>bFolder</C> <C>True for folders, false for files</C> </R>
<R> <C>bHiddenFile</C> <C>True for files marked hidden in file properties</C> </R>
<R> <C>iFileSizeBytes</C> <C>Size of the file in bytes</C> </R>
<R> <C>CreateDate</C> <C>Date the file was created</C> </R>
<R> <C>EditDate</C> <C>Date the file was last modified</C> </R>
<R> <C>bAllow</C> <C>Defaults to false. Set to true to skip the addition of this file</C> </R>
</TABLE>

<FM>Example<FC>
procedure TfMain.ImageEnMView1ImageAdd(Sender : TObject; idx : integer; const sFilename : string; bFolder : boolean; bHiddenFile : boolean; iFileSizeBytes : Int64; CreateDate : TDateTime; EditDate : TDateTime; var bAllow : Boolean): TObject; var bAllow: Boolean);
const
  ONE_MB = 1024 * 1024;
begin
  // Don't add files larger than one MB
  if iFileSizeBytes > ONE_MB then
    bAllow := False;
end;
!!}
  TIEImageAddEvent = procedure(Sender : TObject;
                               idx : integer;
                               const sFilename : string;
                               bFolder : boolean;
                               bHiddenFile : boolean;
                               iFileSizeBytes : Int64;
                               CreateDate : TDateTime;
                               EditDate : TDateTime;
                               var bAllow : Boolean) of object;

{!!
<FS>TIEImageSelectEvent

<FM>Declaration<FC>
type TIEImageSelectEvent = procedure(Sender: TObject; idx: integer) of object;

<FM>Description<FN>
Occurs whenever the user selects or deselects an image
<FC>idx<FN> is the index of the selected image.
!!}
  TIEImageSelectEvent = procedure(Sender: TObject; idx: integer) of object;

{!!
<FS>TIESelectionChangingEvent

<FM>Declaration<FC>
type TIESelectionChangingEvent = procedure(Sender: TObject; var bAllow: Boolean) of object;

<FM>Description<FN>
Occurs prior to a change in the selected frame/thumbnail due to user action (i.e. mouse or keyboard).

This can be used to prevent changing of a selection (e.g. due to the current selection not being saved).

<FM>Example<FC>
procedure TfMain.ImageEnMView1SelectionChanging(Sender: TObject; var bAllow: Boolean);
begin
  // Don't allow changing of the selection if the user cancels saving of the selected image
  if fSelectedImageChanged and (PromptToSaveSelectedImageChanges = False) then
    bAllow := False;
end;
!!}
  TIESelectionChangingEvent = procedure(Sender: TObject; var bAllow: Boolean) of object;


{!!
<FS>TIECheckboxClickEvent

<FM>Declaration<FC>
type TIECheckboxClickEvent = procedure(Sender: TObject; idx: integer; var bChecked : Boolean) of object;

<FM>Description<FN>
Occurs whenever a user clicks a checkbox.

<FC>idx<FN> is the index of the clicked image.
<FC>bChecked<FN> specifies the new status of the image. You can override it, e.g. set it to false if the image cannot be checked

Note: Don't read <A TImageEnMView.CheckedCount> in this event which will not yet be valid. Use OnClick or OnMouseUp.

<FM>Example<FC>
procedure TfMain.ImageEnMView1CheckboxClick(Sender: TObject; idx: integer; var bChecked : Boolean);
begin
  // Only allow JPEG images to be checked
  if bChecked and (IEFileIsOfFormat(ImageEnMView1.ImageFilename[idx], ioJPEG) = False) then
  begin
    MessageBeep(MB_ICONEXCLAMATION);
    bChecked := False;
  end;
end;

<FM>See Also<FN>
- <A TImageEnMView.Checkboxes>
- <A TImageEnMView.CheckedCount>
!!}
  TIECheckboxClickEvent  = procedure(Sender: TObject; idx: integer; var bChecked : Boolean) of object;


{!!
<FS>TIEImageIDRequestEvent

<FM>Declaration<FC>
type TIEImageIDRequestEvent = procedure(Sender: TObject; Index, ID: integer; var Bitmap: TBitmap) of object;

<FM>Description<FN>
Occurs when an image is required if you have specified a value for the <A TImageEnMView.ImageID> property.

<FC>Index<FN> is the index of the image being displayed (i.e. 0 for first image in the grid, 1 for second, etc.)
<FC>ID<FN> is the value you have specified in <A TImageEnMView.ImageID> property.
<FC>Bitmap<FN> is the image to display. The bitmap is copied in <A TImageEnMView>, and then automatically freed.
!!}
  TIEImageIDRequestEvent = procedure(Sender: TObject; Index, ID: integer; var Bitmap: TBitmap) of object;

{!!
<FS>TIEImageIDRequestExEvent

<FM>Declaration<FC>
type TIEImageIDRequestExEvent = procedure(Sender: TObject; Index, ID: integer; var Bitmap: <A TIEBitmap>) of object;

<FM>Description<FN>                           
Occurs when an image is required if you have specified a value for the <A TImageEnMView.ImageID> property.

<FC>Index<FN> is the index of the image being displayed (i.e. 0 for first image in the grid, 1 for second, etc.)
<FC>ID<FN> is the value you have specified in <A TImageEnMView.ImageID> property
<FC>Bitmap<FN> is the image to display. The bitmap is copied in <A TImageEnMView>, and then automatically freed.
!!}
  TIEImageIDRequestExEvent = procedure(Sender: TObject; Index, ID: integer; var Bitmap: TIEBitmap) of object;

{!!
<FS>TIEMProgressEvent

<FM>Declaration<FC>
TIEMProgressEvent = procedure(Sender: TObject; per: integer; idx: integer) of object;

<FM>Description<FN>
Occurs during <A TImageEnMView.PaintTo>.

<FC>per<FN> is the percentage of the progress (i.e. 100 indicates painting has finished).
<FC>idx<FN> is the index of the image currently being drawn.
!!}
  TIEMProgressEvent = procedure(Sender: TObject; per: integer; idx: integer) of object;

{!!
<FS>TIEWrongImageEvent

<FM>Declaration<FC>
TIEWrongImageEvent = procedure(Sender: TObject; OutBitmap: <A TIEBitmap>; idx: integer; var Handled: boolean) of object;

<FM>Description<FN>
Used by <A TImageEnMView.OnWrongImage> whenever TImageEnMView cannot load the image specified in <A TImageEnMView.ImageFileName> property, for instance when the file is corrupted or of an unrecognized format.

<FC>idx<FN> specifies the image index.
If you change the <FC>OutBitmap<FN>, also set <FC>Handled<FN> to True.
If <FC>Handled<FN> is false, <A TImageEnMView> replaces the bitmap with a question mark image.
!!}
  TIEWrongImageEvent = procedure(Sender: TObject; OutBitmap: TIEBitmap; idx: integer; var Handled: boolean) of object;

{!!
<FS>TIEImageDrawEvent

<FM>Declaration<FC>
TIEImageDrawEvent = procedure(Sender: TObject; idx: integer; Left, Top: integer; Canvas: TCanvas) of object;

<FM>Description<FN>
<FC>idx<FN> is the index of the image being painted
<FC>Left<FN> is the X coordinate of the top-left corner of the thumbnail
<FC>Top<FN> is the Y coordinate of the top-left corner of the thumbnail.
<FC>Canvas<FN> is the canvas to draw to.
!!}
  TIEImageDrawEvent = procedure(Sender: TObject; idx: integer; Left, Top: integer; Canvas: TCanvas) of object;

{!!
<FS>TIEImageDraw2Event

<FM>Declaration<FC>
TIEImageDraw2Event = procedure(Sender: TObject; idx: integer; Left, Top: integer; ImageRect: TRect; Canvas: TCanvas) of object;

<FM>Description<FN>
<FC>idx<FN> is the index of the image being painted
<FC>Left<FN> is the X coordinate of the top-left corner of the thumbnail
<FC>Top<FN> is the Y coordinate of the top-left corner of the thumbnail.
<FC>ImageRect<FN> is the actual image rectangle.
<FC>Canvas<FN> is the canvas to draw to.
!!}
  TIEImageDraw2Event = procedure(Sender: TObject; idx: integer; Left, Top: integer; ImageRect: TRect; Canvas: TCanvas) of object;


{!!
<FS>TIEImageDrawEventEx

<FM>Declaration<FC>
}
  TIEImageDrawEventEx = procedure(Sender: TObject; idx: integer; Left, Top: integer; Dest: TBitmap; var ThumbRect: TRect) of object;
{!!}



{!!
<FS>TIEImageAtPosEvent

<FM>Declaration<FC>
TIEImageAtPosEvent = procedure(Sender: TObject; var idx: integer; x, y: Integer) of object;

<FM>Description<FN>
Used by the <A TImageEnMView.OnImageAtPos> event to check if the specified coordinates are inside a thumbnail.
<FC>idx<FN> contains the proposed image thumbnail index. You can change this value or set it to -1 for no thumbnail.

!!}
  TIEImageAtPosEvent = procedure(Sender: TObject; var idx: integer; x, y: Integer) of object;


{!!
<FS>TIECreateImageEvent

<FM>Declaration<FC>
TIECreateImageEvent = procedure(Sender: TObject; idx: integer) of object;

<FM>Description<FN>
Used by <A TImageEnMView.OnCreateImage> whenever (immediately after) a new image is created.
!!}
  TIECreateImageEvent = procedure(Sender: TObject; idx: integer) of object;


{!!
<FS>TIEDestroyImageEvent

<FM>Declaration<FC>
TIEDestroyImageEvent = procedure(Sender: TObject; idx: integer) of object;

<FM>Description<FN>
Used by <A TImageEnMView.OnDestroyImage> whenever (immediately before) an image is destroyed.
!!}
  TIEDestroyImageEvent = procedure(Sender: TObject; idx: integer) of object;


{!!
<FS>TIEProcessStreamEvent

<FM>Declaration<FC>
TIEProcessStreamEvent = procedure(Sender: TObject; Stream: TStream) of object;

<FM>Description<FN>
Used when loading/saving snapshots to specify extra parameters.
!!}
  TIEProcessStreamEvent = procedure(Sender: TObject; Stream: TStream; Version : Byte) of object;

{!!
<FS>TIEImageEnMViewSortCompare

<FM>Declaration<FC>
TIEImageEnMViewSortCompare = function(Item1, Item2: integer): Integer;

<FM>Description<FN>
A comparison function that indicates how the items are to be ordered.

Return a value below 0 if Item1 is less than Item2
Return 0 if they are equal
Return a value above 0 if Item1 is greater than Item2
!!}
  TIEImageEnMViewSortCompare = function(Item1, Item2: integer): Integer;

{!!
<FS>TIEImageEnMViewSortCompareEx

<FM>Declaration<FC>
TIEImageEnMViewSortCompareEx = function(Item1, Item2: integer): Integer of object;

<FM>Description<FN>
A comparison function that indicates how the items are to be ordered.

Return a value below 0 if Item1 is less than Item2
Return 0 if they are equal
Return a value above 0 if Item1 is greater than Item2
!!}
  TIEImageEnMViewSortCompareEx = function(Item1, Item2: integer): Integer of object;

{!!
<FS>TIEImageEnMViewSortBy

<FM>Declaration<FC>
TIEImageEnMViewSortBy = (iesbFilename, iesbTopText, iesbBottomText, iesbInfoText, iesbImageSize, iesbFilenameWithoutPath, iesbFileExtension, iesbFileSize, iesbCreateDate, iesbEditDate, iesbFileType, iesbNone);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iesbFilename</C> <C>Sorts by the full file path (using <ATImageEnMView.ImageFileName>)</C> </R>
<R> <C>iesbTopText</C> <C>Alphabetical sort on <ATImageEnMView.ImageTopText></C> </R>
<R> <C>iesbBottomText</C> <C>Alphabetical sort on <ATImageEnMView.ImageBottomText></C> </R>
<R> <C>iesbInfoText</C> <C>Alphabetical sort on <ATImageEnMView.ImageInfoText></C> </R>
<R> <C>iesbImageSize</C> <C>Sorting by the image dimensions (<ATImageEnMView.ImageOriginalWidth> * <ATImageEnMView.ImageOriginalHeight>)</C> </R>
<R> <C>iesbFilenameWithoutPath</C> <C>Sorts only on the name portion of the file (i.e. excluding the file path)</C> </R>
<R> <C>iesbFileExtension</C> <C>Sorts by the extension of the file (e.g. .jpeg or .tiff)</C> </R>
<R> <C>iesbFileSize</C> <C>Sorting by the size of the files on disk</C> </R>
<R> <C>iesbCreateDate</C> <C>Sorting by the date that the files were created</C> </R>
<R> <C>iesbEditDate</C> <C>Sorting by the date that the files were last modified</C> </R>
<R> <C>iesbFileType</C> <C>Sorting by the file type</C> </R>
<R> <C>iesbCustom</C> <C><A TImageEnFolderMView> only: Sorts using the event, <A TImageEnFolderMView.OnCustomSortCompare> (has no effect in TImageEnMView)</C> </R>
<R> <C>iesbNone</C> <C>No sorting</C> </R>
</TABLE>

!!}
  TIEImageEnMViewSortBy = (iesbFilename, iesbTopText, iesbBottomText, iesbInfoText, iesbImageSize, iesbFilenameWithoutPath, iesbFileExtension, iesbFileSize, iesbCreateDate, iesbEditDate, iesbFileType, iesbCustom, iesbNone);


{!!
<FS>TIEImageEnMViewIconSize

<FM>Declaration<FC>
TIEImageEnMViewIconSize = (ieicStandardSize, ieicDoubleSize, ieicStretchHD, ieicStretchAll);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieicStandardSize</C> <C>Displays a 32x32 icon in the centre of the thumbnail box</C> </R>
<R> <C>ieicDoubleSize</C> <C>Stretches the icon to 64x64 and displays it in the centre of the thumbnail box</C> </R>
<R> <C>ieicStretchHD</C> <C>Under Windows Vista and newer, high quality icons are stretched to the display size, Low quality icons are shown at double size (ieicDoubleSize). On XP and older versions of Windows it functions the same as _icoDoubleSize.</C> </R>
<R> <C>ieicStretchAll</C> <C>Under Windows Vista and newer, all icons (low and high quality) are stretched to the display size. On XP and older versions of Windows it functions the same as _icoDoubleSize.</C> </R>
</TABLE>

!!}
  TIEImageEnMViewIconSize = (ieicStandardSize, ieicDoubleSize, ieicStretchHD, ieicStretchAll);


{!!
<FS>TIEMCheckboxType

<FM>Declaration<FC>
TIEMCheckboxType = (iecbNone, iecbShowOnHover, iecbAlways);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iecbNone</C> <C>Checkboxes are not shown/supported</C> </R>
<R> <C>iecbShowOnHover</C> <C>Checkboxes are shown when a thumbnail is selected, checked, or when hovered over with the mouse</C> </R>
<R> <C>iecbAlways</C> <C>Checkboxes are always shown on thumbnails</C> </R>
</TABLE>

!!}
  TIEMCheckboxType = (iecbNone, iecbShowOnHover, iecbAlways);

{!!
<FS>TIEMCheckboxPos

<FM>Declaration<FC>
TIEMCheckboxPos = (iecpTopLeft, iecpTopRight, iecpBottomLeft, iecpBottomRight);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iecpTopLeft</C> <C>Checkboxes display in the top left of the thumbnail box</C> </R>
<R> <C>iecpTopRight</C> <C>Checkboxes display in the top right of the thumbnail box</C> </R>
<R> <C>iecpBottomLeft</C> <C>Checkboxes display in the bottom left of the thumbnail box</C> </R>
<R> <C>iecpBottomRight</C> <C>Checkboxes display in the bottom right of the thumbnail box</C> </R>
</TABLE>

!!}
  TIEMCheckboxPos = (iecpTopLeft, iecpTopRight, iecpBottomLeft, iecpBottomRight);

{!!
<FS>TIEImageEnMViewDefaultText

<FM>Declaration<FC>
type TIEImageEnMViewDefaultText = (iedtNone, iedtFilename, iedtFilenameNoExt, iedtFilePath, iedtImageDimensions, iedtImageDimAndSize, iedtFileSize, iedtFileCreateDate, iedtFileCreateDateTime, iedtFileCreateDateAndSize, iedtFileEditDate, iedtFileEditDateTime, iedtFileEditDateAndSize, iedtFileType)

<FM>Description<FN>
Possible default values for <A TImageEnMView.ImageTopText>, <A TImageEnMView.ImageInfoText> and <A TImageEnMView.ImageBottomText>

<TABLE>
<R> <H>Value</H> <H>Uses Field</H> <H>Displays</H>  <H>Example</H> </R>
<R> <C>iedtNone</C> <C></C> <C>Caption will be blank, by default</C> <C></C> </R>
<R> <C>iedtFilename</C> <C>IEM_Filename</C> <C>Name of the file (without path)</C> <C>'MyImage.jpg'</C> </R>
<R> <C>iedtFilenameNoExt</C> <C>IEM_FilenameNoExt</C> <C>Name of the file without its extension (or path)</C> <C>'MyImage'</C> </R>
<R> <C>iedtFilePath</C> <C>IEM_FilePath</C> <C>Path of the file</C> <C>'C:\Data\My Pictures\'</C> </R>
<R> <C>iedtImageDimensions</C> <C>IEM_ImageDimensions</C> <C>Dimensions and color depth of images (Nothing is shown for non-images)</C> <C>'1200 x 800 x 24bit'</C> </R>
<R> <C>iedtImageDimAndSize</C> <C>IEM_ImageDimAndSize</C> <C>Dimensions of images with the file size</C> <C>'1200 x 800, 3,900 KB'</C> </R>
<R> <C>iedtFileSize</C> <C>IEM_FileSize</C> <C>Size of the file on disk</C> <C>'3,900 KB'</C> </R>
<R> <C>iedtFileCreateDate</C> <C>IEM_FileCreateDate</C> <C>The date that the file was created</C> <C>'7/5/13'</C> </R>
<R> <C>iedtFileCreateDateTime</C> <C>IEM_FileCreateDateTime</C> <C>The date and time that the file was created</C> <C>'7/5/13 8:03am'</C> </R>
<R> <C>iedtFileCreateDateAndSize</C> <C>IEM_FileCreateDateAndSize</C> <C>The create date of the file and its size</C> <C>'7/5/13, 3,900 KB'</C> </R>
<R> <C>iedtFileEditDate</C> <C>IEM_FileEditDate</C> <C>The date that the file was last edited</C> <C>'7/5/13'</C> </R>
<R> <C>iedtFileEditDateTime</C> <C>IEM_FileEditDateTime</C> <C>The date and time that the file was last edited</C> <C>'7/5/13 8:03am'</C> </R>
<R> <C>iedtFileEditDateAndSize</C> <C>IEM_FileEditDateAndSize</C> <C>The last edit date of the file and its size</C> <C>'7/5/13, 3,900 KB'</C> </R>
<R> <C>iedtFileType</C> <C>IEM_FileType</C> <C>The ImageEn type for this file</C> <C>'JPEG Image'</C> </R>
</TABLE>

<FM>Example<FC>
// Display the filename and dimensions for each image in C:\Images
IEFolderMView1.DefaultImageInfoText := iedtImageDimensions;
IEFolderMView1.DefaultImageBottomText := iedtFilename;        
IEFolderMView1.RefreshFileList;
end;
!!}
  TIEImageEnMViewDefaultText = (iedtNone                  ,
                                iedtFilename              ,
                                iedtFilenameNoExt         ,
                                iedtFilePath              ,
                                iedtImageDimensions       ,
                                iedtImageDimAndSize       ,
                                iedtFileSize              ,
                                iedtFileCreateDate        ,
                                iedtFileCreateDateTime    ,
                                iedtFileCreateDateAndSize ,
                                iedtFileEditDate          ,
                                iedtFileEditDateTime      ,
                                iedtFileEditDateAndSize   ,
                                iedtFileType);



{!!
<FS>TIEStoreType

<FM>Declaration<FC>
type TIEStoreType = (ietNormal, ietThumb, ietFastThumb);

<FM>Description<FN>
Specifies how images are loaded.

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ietNormal</C> <C>The full bitmap is kept in memory</C> </R>
<R> <C>ietThumb</C> <C>A sub-sampled copy of the bitmap is kept in memory (of the size specified by <A TImageEnMView.ThumbWidth> and <A TImageEnMView.ThumbHeight>).</C> </R>
<R> <C>ietFastThumb</C> <C>Only the cached image of the display thumbnail is held in memory. If the thumbnail is cleared from the cache it will need to be reloaded</C> </R>
</TABLE>


<FM>ietThumb vs ietFastThumb<FN>
When using ietFastThumb, creation of the thumbnail is delayed until it is shown on screen. Prior to this a full size image of each frame may be held in memory. For this reason ietFastThumb should not be used unless thumbnail frames are being loaded on demand (or there is not a large amount of off-screen frames).

ietFastThumb can improve the quality of thumbnails, particularly when using <A TImageEnMView.ImageInfoText> and <A TImageEnMView.ThumbnailClipping>. It cannot be used if you have disabled <L TImageEnMView.EnableImageCaching>image caching</L>.

If you are using ietFastThumb then you should significantly increase the <A TImageEnMView.ImageCacheSize> to something larger, such as 200.

!!}
  TIEStoreType = (
    // the image will be full loaded
    ietNormal,
    // the image will loaded as thumbnail (see ThumbWidth, ThumbHeight))
    ietThumb,
    // Thumbnail creation is delayed until it is drawn, then it is stored in CacheImage and Image is cleared
    ietFastThumb
    );



{!!
<FS>TIESourceType

<FM>Declaration<FC>
type TIESourceType = (iestDefault, iestFileIcon, iestFolderIcon, iestCustomImage);

<FM>Description<FN>
Returns the method used to fill the content of a frame/thumbnail.

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iestDefault</C> <C>The frame is a supported image (or video) type which was loaded normally</C> </R>
<R> <C>iestFileIcon</C> <C>The frame is for a file or unsupported image type and was filled by loading the system icon for that file type</C> </R>
<R> <C>iestFolderIcon</C> <C>The frame displays a system folder. The system icon for folders will be displayed</C> </R>
<R> <C>iestCustomImage</C> <C>The frame is for an unsupported image type and was filled using <A TImageEnMView.OnWrongImage></C> </R>
</TABLE>


!!}
  TIESourceType = (
    // Normal loading
    iestDefault,
    // Unsupported type: System icon loaded
    iestFileIcon,
    // Unsupported type: a folder, so the icon will be loaded
    iestFolderIcon,
    // Unsupported type: Filled with image using OnWrongImage
    iestCustomImage
    );


{!!
<FS>TIEFolderImageType

<FM>Declaration<FC>
type TIEFolderImageType = (ieftSupportedImage, ieftFile, ieftFolder);

<FM>Description<FN>
Returns the type of file for a frame/thumbnail.

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieftSupportedImage</C> <C>The frame is a supported image (or video) type</C> </R>
<R> <C>ieftFile</C> <C>The frame is for an unknown file type</C> </R>
<R> <C>ieftFolder</C> <C>The frame is a system folder</C> </R>
</TABLE>


!!}
  TIEFolderImageType = (ieftSupportedImage, ieftFile, ieftFolder);


{!!
<FS>TIEMTextPos

<FM>Declaration<FC>
}
  TIEMTextPos = (iemtpTop, iemtpBottom, iemtpInfo);
{!!}


{!!
<FS>TIEGetTextEvent

<FM>Declaration<FC>
type TIEGetTextEvent = procedure(Sender: TObject; Index: integer; Position : <A TIEMTextPos>; var Text: WideString) of object;

<FM>Description<FN>
Occurs before text is output when drawing a thumbnail allowing you to insert or modify the displayed text (by specifying a new value for <FC>Text<FN>).

<TABLE>
<R> <H>Position</H> <H>Description</H> </R>
<R> <C>iemtpTop</C> <C>Overrides the text specified for <A TImageEnMView.ImageTopText></C> </R>
<R> <C>iemtpInfo</C> <C>Overrides the text specified for <A TImageEnMView.ImageInfoText></C> </R>
<R> <C>iemtpBottom</C> <C>Overrides the text specified for <A TImageEnMView.ImageBottomText></C> </R>
</TABLE>


Note:
- Ensure that you have set <A TImageEnMView.UpperGap>/<A TImageEnMView.BottomGap> to allow space for the text
- Setting <FC>Text<FN> only modifies the text that is displayed, not the value in ImageTopText/ImageInfoText/ImageBottomText


<FM>Example<FN>
// Note: In form create we set IEFolderMView.UpperGap := 20;

// Display the file index above the frame
procedure TForm1.IEFolderMViewGetText(Sender: TObject; Index: Integer; Position: TIEMTextPos; var Text: WideString);
begin
  if Position = iemtpTop then
    Text := 'File #' + IntToStr(Index + 1);
end;

!!}
  TIEGetTextEvent = procedure(Sender: TObject; Index: integer; Position : TIEMTextPos; var Text: WideString) of object;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMText

{!!
<FS>TIEMText

<FM>Description<FN>
THe TIEMText object specifies how text is drawn inside a thumbnail.
Three <A TImageEnMView> properties contains this object: <A TImageEnMView.ImageTopText>, <A TImageEnMView.ImageBottomText>, <A TImageEnMView.ImageInfoText>.

<FM>Properties<FN>
- <A TIEMText.Caption>
- <A TIEMText.Font>
- <A TIEMText.Background>
- <A TIEMText.BackgroundStyle>
- <A TIEMText.TruncSide>
!!}
  TIEMText = class
  private
    fCaption: WideString;
    fFont: TFont;
    fBackground: TColor;
    fBackgroundStyle: TBrushStyle;
    fOwner: TComponent;
    fPos: TIEMTextPos;
    fTruncSide: TIEMTruncSide;
    procedure SetCaption(value: WideString);
  public
    constructor Create(Owner: TComponent; Position: TIEMTextPos);
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    function LoadFromStream(Stream: TStream): Boolean;

{!!
<FS>TIEMText.Caption

<FM>Declaration<FC>
property Caption: WideString;

<FM>Description<FN>
The text to output.

You can optionally specify the following constants:
<TABLE>
<R> <H>Value</H> <H>Displays</H>  <H>Example</H> </R>
<R> <C>IEM_Filename</C> <C>Name of the file</C> <C>'MyImage.jpg'</C> </R>             
<R> <C>IEM_FilenameNoExt</C> <C>Name of the file without its extension (or path)</C> <C>'MyImage'</C> </R>
<R> <C>IEM_FilePath</C> <C>Path of the file</C> <C>'C:\Data\My Pictures\'</C> </R>
<R> <C>IEM_ImageDimensions</C> <C>Dimensions and color depth of images (Nothing is shown for non-images)</C> <C>'1200 x 800 x 24bit'</C> </R>
<R> <C>IEM_ImageDimAndSize</C> <C>Dimensions of images with the file size</C> <C>'1200 x 800, 3,900 KB'</C> </R>
<R> <C>IEM_FileSize</C> <C>Size of the file on disk</C> <C>'3,900 KB'</C> </R>
<R> <C>IEM_FileCreateDate</C> <C>The date that the file was created</C> <C>'7/5/13'</C> </R>
<R> <C>IEM_FileCreateDateTime</C> <C>The date and time that the file was created</C> <C>'7/5/13 8:03am'</C> </R>
<R> <C>IEM_FileCreateDateAndSize</C> <C>The create date of the file and its size</C> <C>'7/5/13, 3,900 KB'</C> </R>
<R> <C>IEM_FileEditDate</C> <C>The date that the file was last edited</C> <C>'7/5/13'</C> </R>
<R> <C>IEM_FileEditDateTime</C> <C>The date and time that the file was last edited</C> <C>'7/5/13 8:03am'</C> </R>
<R> <C>IEM_FileEditDateAndSize</C> <C>The last edit date of the file and its size</C> <C>'7/5/13, 3,900 KB'</C> </R>
<R> <C>IEM_FileType</C> <C>The ImageEn type for this file</C> <C>'JPEG Image'</C> </R>
</TABLE>       

<FM>Example<FC>
// Display the filename and dimensions for each image
for I := 0 to ImageEnMView1.ImageCount - 1 do
begin
  ImageInfoText[I].Caption := IEM_ImageDimensions;
  ImageBottomText[I].Caption := IEM_Filename;
end;
!!}
    property Caption: WideString read fCaption write SetCaption;

{!!
<FS>TIEMText.Font

<FM>Declaration<FC>
property Font: TFont;

<FM>Description<FN>
The font of the text.

<FM>See Also<FN>
- <A TImageEnMView.DefaultTopTextFont>
- <A TImageEnMView.DefaultInfoTextFont>
- <A TImageEnMView.DefaultBottomTextFont>

!!}
    property Font: TFont read fFont;

{!!
<FS>TIEMText.Background

<FM>Declaration<FC>
property Background: TColor;

<FM>Description<FN>
The background color of the text.

<FM>See Also<FN>
- <A TIEMText.BackgroundStyle>
!!}
    property Background: TColor read fBackground write fBackground;

{!!
<FS>TIEMText.BackgroundStyle

<FM>Declaration<FC>
property BackgroundStyle: TBrushStyle;

<FM>Description<FN>
The background style of the text. You can override the default style with <A TImageEnMView.DefaultTextBackgroundStyle> 

<FM>See Also<FN>
- <A TIEMText.Background>
!!}
    property BackgroundStyle: TBrushStyle read fBackgroundStyle write fBackgroundStyle;

{!!
<FS>TIEMText.TruncSide

<FM>Declaration<FC>
property TruncSide: <A TIEMTruncSide>;

<FM>Description<FN>
Specifies the side to truncate the text if it is too wide to be displayed.
!!}
    property TruncSide: TIEMTruncSide read fTruncSide write fTruncSide;

  end;

// TIEMText
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



  // this structure contains single image info (except bitmaps)
  TIEImageInfo = record
    // the parent TImageEnMView object
    parent: TObject; // the TImageEnMView parent object
    // image contained in fImageList[]
    // if image is nil the image is not handled by component
    image: pointer;
    // image contained in fCacheList[]
    // if nil we need to repaint the image
    cacheImage: pointer;
    // top/left position
    X, Y: integer;
    // row/col position (calculated by UpdateCoords)
    row, col: integer;
    // image background color
    Background: TColor;
    // File name to load ('' = none)
    Filename: pwchar;
    // Image ID (-1=none).
    ID: integer;
    // Delay time, display time for this image (in millisecs)
    DTime: Double;
    // Texts info
    TopText: TIEMText;
    BottomText: TIEMText;
    InfoText: TIEMText;
    // last painted internal rect (this the internal image when there is a shadow)
    internalrect: TRect;
    // user tag
    tag: Integer;
    // user pointer
    userPointer: pointer;
    // Generally iesDefault. For unsupported images will be iesSystemIcon or iesCustomImage
    SourceType : TIESourceType;

    // Checked status of image
    Checked : Boolean;

    // File data
    CreateDate    : TDateTime;
    EditDate      : TDateTime;
    FileSize      : Int64;
    FileTypeIndex : Integer;  // Used only for sorting
  end;

  PIEImageInfo = ^TIEImageInfo;


{!!
<FS>TIEMThumbnailOptionsEx

<FM>Declaration<FC>
type TIEMThumbnailOptionsEx = set of (ietxCenterThumbnailColumn, ietxShowIconForUnknownFormat, ietxShowIconWhileLoading, ietxShowShadowForIcons, ietxShowShadowForFolders);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ietxCenterThumbnailColumn</C> <C>If enabled and one or more columns are displayed (i.e. <A TImageEnMView.GridWidth> <> 0) then the thumbnails will be centered horizontally in the view (but not vertically centered).
If enabled and only one row is displayed (i.e. <A TImageEnMView.GridWidth> = 0) then the thumbnails will be centered vertically in the view (but not horizontally centered).
When not enabled the the top left thumbnail will always be at a distance of <A TImageEnMView.HorizBorder> x <A TImageEnMView.VertBorder> from the corner.</C> </R>
<R> <C>ietxShowIconForUnknownFormat</C> <C>If enabled, then for any ImageFileName of an unknown format ImageEn will load the default icon for that file type (retrieved from Windows using the ShGetFileInfo Shell API function). Otherwise a question mark is displayed.
Note: Don't disable this option if you are showing folders</C> </R>
<R> <C>ietxShowIconWhileLoading</C> <C>If enabled, then the default icon for each file type is displayed while the content is loaded (retrieved from Windows using the ShGetFileInfo Shell API function). Otherwise a blank thumbnail is displayed.</C> </R>
<R> <C>ietxShowShadowForIcons</C> <C>If <A TImageEnMView.SoftShadow> is enabled, then this option determines whether the shadow is displayed for file icons</C> </R>
<R> <C>ietxShowShadowForFolders</C> <C>If <A TImageEnMView.SoftShadow> is enabled, then this option determines whether the shadow is displayed for folder icons</C> </R>
</TABLE>

<FM>Example 1<FC>
// Use icons for unknown files
ImageEnMView1.ThumbnailOptionsEx := ImageEnMView1.ThumbnailOptionsEx + [ietxShowIconForUnknownFormat];

// Display all files (not only image files)
ImageEnMView1.FillFromDirectory('C:\', -1, true);

<FM>Example 2<FC>
// Do not show shadows for any icon types
ImageEnMView1.ThumbnailOptionsEx := ImageEnMView1.ThumbnailOptionsEx - [ietxShowShadowForIcons, ietxShowShadowForFolders];

// Redraw the display
ImageEnMView1.Update;

<FM>Example 3<FC>
// Create a single column of centered thumbnails    
ImageEnMView1.GridWidth := 1;
ImageEnMView1.ThumbnailOptionsEx := ImageEnMView1.ThumbnailOptionsEx + [ietxCenterThumbnailColumn];
ImageEnMView1.Update;

<FM>See Also<FN>
- <A TImageEnMView.GridWidth>
- <A TImageEnMView.SoftShadow>
- <A TImageEnMView.IconSize>   
!!}     

  TIEMThumbnailOptionsExItems = (ietxCenterThumbnailColumn, ietxShowIconForUnknownFormat, ietxShowIconWhileLoading, ietxShowShadowForIcons, ietxShowShadowForFolders);
  TIEMThumbnailOptionsEx = set of TIEMThumbnailOptionsExItems;


  // automatic interactions with mouse
{!!
<FS>TIEMMouseInteractItems

<FM>Declaration<FC>
}
  TIEMMouseInteractItems = (
    mmiScroll, // Hand-scroll. Mouse drag scrolls images.
    mmiSelect  // Images selection. Mouse click select images.
    );
{!!}



{!!
<FS>TIEMMouseInteract

<FM>Declaration<FC>
type TIEMMouseInteract = set of <A TIEMMouseInteractItems>;

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>mmiScroll</C> <C>Dragging the mouse will scroll the images</C> </R>
<R> <C>mmiSelect</C> <C>Clicking the mouse will select images</C> </R>
</TABLE>

!!}
  TIEMMouseInteract = set of TIEMMouseInteractItems;

{!!
<FS>TIEMKeyInteractItems

<FM>Declaration<FC>
}
  // automatic interaction with keyboard
  TIEMKeyInteractItems = (
    mkiMoveSelected // move selected item (up, down, left, right)
    );
{!!}

{!!
<FS>TIEMKeyInteract

<FM>Declaration<FC>
TIEMKeyInteract = set of <A TIEMKeyInteractItems>;
!!}
  TIEMKeyInteract = set of TIEMKeyInteractItems;

{!!
<FS>TIEMDisplayMode

<FM>Declaration<FC>
type TIEMDisplayMode = (mdGrid, mdSingle);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>mdGrid</C> <C>Thumbnail view: Images are shown in a grid (of <A TImageEnMView.GridWidth> columns)</C> </R>
<R> <C>mdSingle</C> <C>Frame view: Only one image is shown a a time (specified by <A TImageEnMView.VisibleFrame> property)</C> </R>
</TABLE>

!!}
  TIEMDisplayMode = (
    mdGrid,   // grid (active fGridWidth property)
    mdSingle  // single frame
    );

{!!
<FS>TIESeek

<FM>Declaration<FC>
TIESeek = (iskLeft, iskRight, iskUp, iskDown, iskFirst, iskLast, iskPagDown, iskPagUp);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iskLeft</C> <C>Move to the left (by one column)</C> </R>
<R> <C>iskRight</C> <C>Move to the right (by one column)</C> </R>
<R> <C>iskUp</C> <C>Move up (by one row)</C> </R>
<R> <C>iskDown</C> <C>Move down (by one row)</C> </R>
<R> <C>iskFirst</C> <C>move to first image</C> </R>
<R> <C>iskLast</C> <C>move to the last image</C> </R>
<R> <C>iskPagDown</C> <C>move to the next page</C> </R>
<R> <C>iskPagUp</C> <C>move the the previous page</C> </R>
</TABLE>
!!}
  TIESeek = (iskLeft, iskRight, iskUp, iskDown, iskFirst, iskLast, iskPagDown, iskPagUp);

{!!
<FS>TIEMStyle

<FM>Declaration<FC>
TIEMStyle = (iemsFlat, iemsACD);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iemsFlat</C> <C>Flat style</C></R>
<R> <C>iemsACD</C> <C>3D style (default)</C></R>
</TABLE>
!!}
  TIEMStyle = (iemsFlat, iemsACD);


{!!
<FS>TIEMultiSelectionOptions

<FM>Declaration<FC>
TIEMultiSelectionOptions = set of (iemoRegion, iemoSelectOnMouseUp, iemoLeaveOneSelected, iemoRegionOnShift, iemoOptimizeForDragging, iemoSelectOnRightClick);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iemoRegion</C> <C>Select only items inside selection rectangle</C> </R>
<R> <C>iemoSelectOnMouseUp</C> <C>By default, ImageEn selects an image on mouse down. If iemoSelectOnMouseUp is specified, ImageEn delays selection until mouse up</C> </R>
<R> <C>iemoLeaveOneSelected</C> <C>If specified, at least one item remains selected. Note: works only when the user deselects multiple images</C> </R>
<R> <C>iemoRegionOnShift</C> <C>If specified, pressing <FC>Shift<FN> only selects items inside rectangle</C> </R>
<R> <C>iemoOptimizeForDragging</C> <C>Makes the behavior closer to Windows Explorer when selecting a region. If you click outside a selection it creates a region, if you click on an existing selection it does nothing. This is particularly useful if you need to support dragging</C> </R>
<R> <C>iemoSelectOnRightClick</C> <C>Makes the behavior closer to Windows Explorer. When right-clicking outside the selection the new frame is selected. By default this is not specified, meaning that right-clicking has no effect on selection</C> </R>
</TABLE>
!!}
  TIEMultiSelectionOptions = set of (iemoRegion, iemoSelectOnMouseUp, iemoLeaveOneSelected, iemoRegionOnShift, iemoOptimizeForDragging, iemoSelectOnRightClick);



{!!
<FS>TIEPlayFrameEvent

<FM>Declaration<FC>
}
TIEPlayFrameEvent = procedure(Sender: TObject; FrameIndex: integer; var bShowFrame: Boolean) of object;
{!!}


  TImageEnMView = class;

  TIEStarter = class(TThread)
  public
    mview: TImageEnMView;
    resumeEvent: THandle;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;



  // CLASSES TO HANDLE CACHING OF ICONS
  // NPC: 10/6/14
  TCachedIconType = (citFullDraw, citIconOnly);
  TIECachedIcon = class
    private
    public
      Bmp : TIEBitmap;
      Ext : string;
      constructor Create(aBmp: TIEBitmap; const sExt: string);
      destructor Destroy(); override;
      function MatchesExt(const sExt : string) : Boolean;
  end;

  TIECachedIconList = class
    private
      fOwner                  : TImageEnMView;
      fMaxIconCount           : Integer;
      fDataList               : TList;
      fOwnerSoftShadowEnabled : Boolean;
      fOwnerSoftShadowRadius  : Double;

      procedure ClearLastItem();
      function LookUpExt(const sExt : string) : Integer;
      function GetCacheName(aType : TCachedIconType; const sFilename : string; bIsFolder : Boolean) : string;
    public
      constructor Create(Owner : TImageEnMView; iMaxIconCount : Integer);
      destructor Destroy(); override;
      procedure Clear();
      function RetrieveFromCache(aType : TCachedIconType; const sFilename : string; bIsFolder : Boolean; var Dest : TIEBitmap; bWantCopy : Boolean): boolean;
      procedure SaveToCache(Image : TIEBitmap; aType : TCachedIconType; const sFilename : string; bIsFolder : Boolean);
  end;




{!!
<FS>TIEAnimationTextEvent

<FM>Declaration<FC>
}
TIEAnimationTextEvent = procedure(Sender: TObject; imageIndex: integer; var text: WideString) of object;
{!!}


{!!
<FS>TIEAfterEvent

<FM>Declaration<FC>
TIEAfterEventEvent = procedure(Sender: TObject; Event: <A TIEAfterEvent>) of object;

<FM>Description<FN>
}
TIEAfterEvent = (ieaeMouseDown, ieaeMouseUp, ieaeKeyDown, ieaeKeyUp, ieaePaint);
{!!}


{!!
<FS>TIEAfterEventEvent

<FM>Declaration<FC>
TIEAfterEventEvent = procedure(Sender: TObject; Event: <A TIEAfterEvent>) of object;

<FM>Description<FN>
Occurs after ImageEn has processed an event.
!!}
TIEAfterEventEvent = procedure(Sender: TObject; Event: TIEAfterEvent) of object;



{!!
<FS>TIEMViewerGestures

<FM>Description<FN>
TImageEnMView supports native Windows 7 and Windows 8 gestures, allowing pan and zoom.

This class contains the properties to configure the gestures.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEMViewerGestures.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMViewerGestures.Pan></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMViewerGestures.Zoom></C> </R>
</TABLE>
!!}
TIEMViewerGestures = class
  private
    fPan: TIEGesturePanOptions;
    fZoom: TIEGestureZoomOptions;

    function GetEnabled(): boolean;

  public
    constructor Create();
    destructor Destroy(); override;

{!!
<FS>TIEMViewerGestures.Enabled

<FM>Declaration<FC>
property Enabled: boolean; (Read-only)

<FM>Description<FN>
Returns true whenever at least one gesture is enabled.

!!}
    property Enabled: boolean read GetEnabled;

{!!
<FS>TIEMViewerGestures.Pan

<FM>Declaration<FC>
property Pan: <A TIEGesturePanOptions>;

<FM>Description<FN>
Allows enabling and configuration of the Pan (image scrolling) gesture. The Windows gesture to "Pan with intertia" is performed on the action of "dragging 1 or 2 fingers".
!!}
    property Pan: TIEGesturePanOptions read fPan;

{!!
<FS>TIEMViewerGestures.Zoom

<FM>Declaration<FC>
property Zoom: <A TIEGestureZoomOptions>;

<FM>Description<FN>
Allows enabling and configuration of the Zoom gesture. The Windows gesture of "Zoom" is performed on the action of "moving two fingers apart/towards each other".
!!}
    property Zoom: TIEGestureZoomOptions read fZoom;
end;



{!!
<FS>TImageEnMView

<FM>Description<FN>
The TImageEnMView component is similar to a <A TImageEnView>, but is designed for multiple images:
- Multiple images can be displayed in a grid (similar to a thumbnail view in Windows Explorer)
- A single frame of a multiple-frame image (such as a GIF, AVI or TIFF) can be displayed. This can also be animated.

The images can be stored as full images, as thumbnails (a sub-resampled image of the original), loaded when displayed (you need only specify the file name), or upon request (an event is generated whenever an image needs to be shown).
Thumbnails for video formats are retrieved from Windows Explorer. This can be disabled or expanded by setting <A TIEImageEnGlobalSettings.MViewExplorerThumbnailExts>.

For rapid UI development a full set of <L TImageEnMView Actions>actions</L> is also available.

You can also specify a custom function whenever an image is shown (i.e., to paint the image index near the image).

Note: Do not attach a <A TImageEnIO> component to TImageEnMView. Only the <A TImageEnMIO> component can be attached to a TImageEnMView component, or you can use embedded <A TImageEnMView.MIO> object.


 <IMG help_images\IEMView_Thumbnails.gif> <IMG help_images\IEMView_Multiframe.gif>

<FM>Example<FC>
// In FormCreate()
with ImageEnMView1 do
begin
  // Update to the most modern styling
  SetModernStyling(False);

  ThumbWidth   := 140;          // Better size for thumbnails
  ThumbHeight  := 150;

  GridWidth    := -1;           // Auto-column widths
  BorderStyle  := bsNone;       // Normally don't require a 3D border

  StoreType    := ietThumb;     // Don't need to store whole image in memory, just a sub-sampled thumbnail
end;

// Now display the images of a folder in the grid
ImageEnMView1.FillFromDirectory('C:\MyImages\', -1, False, '', False, '', True);

<FM>Methods and Properties<FN>
<FI>Display<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.Animation></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.Background></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.BackgroundStyle></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CenterFrame></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.ClearImageCache></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.DisplayImageAt></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.DisplayMode></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.EnableAlphaChannel></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.FlatScrollBars></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.GradientEndColor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.GridWidth> (Column count)</C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.LockPaint></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.LockPaintCount></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.LockUpdate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.LockUpdateCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MaximumViewX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MaximumViewY></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetChessboardStyle></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetPresetThumbnailFrame>  </C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetModernStyling></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetViewXY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SoftShadow></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.Style></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UnLockPaint></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UnLockUpdate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ViewX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ViewY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.VisibleFrame></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.WallPaper></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.WallPaperStyle></C> </R>
</TABLE>

<FI>Frame Editing<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMView.AppendImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.AppendSplit></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.Clear></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CreateMorphingSequence></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.DeleteImage>      </C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageCount></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.InsertImageEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.InsertImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.InsertTransitionFrames></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.InsertTransitionFramesEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.MoveImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.RemoveBlankPages></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.Sort></C> </R>
</TABLE>

<FI>Image Access<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.Bitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CopyToIEBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetTIEBitmap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.IEBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.PrepareSpaceFor></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.ReleaseBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetIEBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetImageEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetImageRect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UpdateImage></C> </R>
</TABLE>

<FI>Image Information<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageBitCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageCol></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageCreateDate>   </C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageEditDate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageFileName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageFileSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageID></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageOriginalHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageOriginalWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageRow></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageTag></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageUserPointer></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageY></C> </R>
</TABLE>

<FI>Image Text<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.DefaultBottomTextFont></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.DefaultInfoTextFont></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.DefaultTextBackgroundStyle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.DefaultTopTextFont></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageBottomText></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageInfoText></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageTopText></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectedFontColor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ShowText></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.TextMargin></C> </R>
</TABLE>

<FI>Thumbnail Appearance<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.BottomGap></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.DrawImageBackground></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.EnableResamplingOnMinor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.HorizBorder></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.IconSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.FillThumbnail></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageBackground></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetThumbnailSize></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.SideGap></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbHeight>             </C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ThumbnailClipping></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ThumbnailDisplayFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ThumbnailFrameRect></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ThumbnailFrameSelected></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ThumbnailFrame></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ThumbnailResampleFilter></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailsBackground></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailsBackgroundSelected></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailsBackgroundStyle></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailsBorderColor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailsBorderWidth></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailsInternalBorderColor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailsInternalBorder></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbnailOptionsEx></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ThumbsRounded></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThumbWidth></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.UpperGap>      </C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.VertBorder></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.Zoom></C> </R>
</TABLE>

<FI>Input/Output<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.EnableAdjustOrientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.EnableImageCaching></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.EnableLoadEXIFThumbnails></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.FillFromDirectory></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetImageToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetImageToStream></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ImageCacheSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageCacheUseDisk></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.JobsRunning></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.JobsWaiting></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.LoadFromFileOnDemand></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.LoadSnapshot></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.LookAhead></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MaintainInvisibleImages></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MIO></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.ReloadImage></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.RemoveCorrupted></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SaveSnapshot></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.Seek></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetImageFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetImageFromStream></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ThreadPoolSize></C> </R>
</TABLE>

<FI>Selections<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMView.BeginSelectImages></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CenterSelected></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.CheckThumbBoundsOnSelect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.DeleteSelectedImages></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.DeSelect></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.EnableMultiSelect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.EndSelectImages></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.IsSelected></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.MoveSelectedImagesTo></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MultiSelectedImages></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MultiSelectedImagesCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MultiSelectedImagesList></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MultiSelecting></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.MultiSelectionOptions></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.MultiSelectSortList></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SelectAll></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectedImage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectedImageAlwaysVisible></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectionAntialiased></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.SelectionColor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.SelectionWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectionWidthNoFocus></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SelectSeek></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.TrackMouseSelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UnSelectImage></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.VisibleSelection></C> </R>
</TABLE>

<FI>Checkboxes<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.Checkboxes></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.Checked></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CheckedCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.CheckboxPos></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetCheckboxParams></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CheckAll></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UncheckAll></C> </R>
</TABLE>

<FI>User Interaction<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.Gestures></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetImageVisibility></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.HScrollBarParams></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.ImageAtGridPos></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.ImageAtPos></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.InsertingPoint></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.IsVisible></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.KeyInteract></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.MouseInteract></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MouseWheelParams></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ScrollBars></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ScrollBarsAlwaysVisible></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.VScrollBarParams></C> </R>
</TABLE>

<FI>Animations and Transitions<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageDelayTime></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.Playing></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.PlayLoop></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.TransitionDuration></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.TransitionEffect></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.TransitionRunning></C> </R>
</TABLE>

<FI>Other<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CalcGridHeight></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CalcGridWidth></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.IEBeginDrag></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.IEEndDrag></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.ImageEnVersion></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.Proc></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.StoreType></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.Update></C> </R>
</TABLE>

<FI>Internal<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetLastOp></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetLastOpIdx>   </C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UpdateCoords></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnAcquireBitmap></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnAfterEvent></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnAllDisplayed></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnAnimationText></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnBeforeImageDrawEx></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnBeforeImageDraw></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnCheckboxClick></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnCreateImage></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnDestroyImage></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnDrawProgress></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnFinishWork></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnGetText></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageAdd></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageAtPos></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageDeselect></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageDraw></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageDraw2></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageIDRequestEx></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageIDRequest></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageEnGesture></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageSelect></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnIOProgress></C> </R>
<R> <C_IMG_EVENT> <C><A TIEView.OnMouseEnter></C> </R>
<R> <C_IMG_EVENT> <C><A TIEView.OnMouseLeave></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnPlayFrame></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnProgress></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnSelectionChanging></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnViewChange></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnWrongImage></C> </R>
</TABLE>

!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TImageEnMView = class(TIEView)
  private
    /////////////////////////
    // P R I V A T E
    fMDown: boolean;
    fBackBuffer: TIEBitmap;
    fHSVX1, fHSVY1: integer;            // view in mouse down
    fHoverCheckLastIdx : Integer;       // for drawing of hover checkboxes
    fHoverCheckLastPos : TPoint;        // Only show checkbox on reasonable mouse movement
    fScrollBars: TScrollStyle;
    fRXScroll, fRYScroll: Double;
    fViewX, fViewY: integer;
    fImageList: TIEVirtualImageList;
    fCacheList: TIEVirtualImageList;
    fIconList : TIECachedIconList;
    fStoreType: TIEStoreType;           // how to store images
    fThumbWidth, fThumbHeight: integer; // thumbsnails size
    fZoom: double;                      // affects ThumbWidth and ThumbHeight
    fHorizBorder: integer;              // horizontal border
    fVertBorder: integer;               // vertical border
    fVWidth, fVHeight: integer;         // virtual space size (Updated by UpdateCoords)
    fOnViewChange: TViewChangeEvent;
    fOnImageAtPos: TIEImageAtPosEvent;
    fOnCreateImage: TIECreateImageEvent;
    fOnDestroyImage: TIEDestroyImageEvent;
    fOnDrawProgress: TIEMProgressEvent;
    fOnWrongImage: TIEWrongImageEvent;
    fHDrawDib: HDRAWDIB;                // to draw on display
    fOnImageIDRequest: TIEImageIDRequestEvent;
    fOnImageIDRequestEx: TIEImageIDRequestExEvent;
    fOnImageDraw: TIEImageDrawEvent;
    fOnImageDraw2: TIEImageDraw2Event;
    fOnIOProgress: TIEProgressEvent;
    fBottomGap: integer;
    fUpperGap: integer;
    fSideGap : Integer;    
    fModernStyling : Boolean;           // Set to true if SetModernStyling
    fTextMargin : Integer;
    fDisplayMode: TIEMDisplayMode;      // display mode
    fGridWidth: integer;                // number of horizontal images (1=all vertical, other is a grid)
    fCurrentGridWidth : Integer;        // Number of horizontal lines as displayed
    fHSX1, fHSY1: integer;              // mouse down coordinates
    fHSIDX, fLHSIDX: integer;
    fHSIDXSelected : Boolean;           // whether the cell we clicked on was already selected
    fSelectIdxOnMouseUp : integer;      // Delay selection till mouse up so that it does not affect our dragging
    FLastRegionIdx: integer;            
    fPriorHSIDX: Integer;
    fLastMouseMoveX, fLastMouseMoveY: integer;
    fImageEnIO: TImageEnIO;             // to load images
    fLockPaint: integer;                // 0=paint unlocked
    fLockUpdate: integer;               // 0=update unlocked
    fRemoveCorrupted: boolean;          // works only when ImageFileName[] contains valid names
    fDrawImageBackground: boolean;      // true=draw image background  false=draw component background
    fScrollBarsAlwaysVisible: boolean;  // true if the scrollbars are always visible
    fVScrollBarParams: TIEScrollBarParams;
    fHScrollBarParams: TIEScrollBarParams;
    fMouseWheelParams: TIEMouseWheelParams;
    fThumbnailResampleFilter: TResampleFilter;
    fThumbnailDisplayFilter: TResampleFilter;
    fThumbnailClipping : Integer;
    fDestroying: boolean;               // component is destroying
    fStyle: TIEMStyle;
    fDoubleClicking: boolean;
    fThumbnailsBackground: TColor;
    fThumbnailsBackgroundSelected: TColor;
    fThumbnailsBorderWidth: integer;
    fThumbnailsBorderColor: TColor;
    fThumbnailsInternalBorder: boolean;
    fThumbnailsInternalBorderColor: TColor;
    fUpdating: boolean;
    fEnableResamplingOnMinor: boolean;   // Enable resampling when the image has width and height < of thumbnail width and height
    fIconSize : TIEImageEnMViewIconSize;
    fThumbsRounded: Integer;
    fEnableAdjustOrientation: Boolean;
    fEnableLoadEXIFThumbnails: Boolean;  // if we need thumbnails, allow to use EXIF thumbnails
    // when true (default) the image will be resized to the thumbnail sizes
    fEnableAlphaChannel: boolean;
    fBackgroundStyle: TIEBackgroundStyle;
    fThumbnailsBackgroundStyle: TIEBackgroundStyle;
    fFillThumbnail: boolean;
    fCurrentCompare: TIEImageEnMViewSortCompare;
    fCurrentCompareEx: TIEImageEnMViewSortCompareEx;
    fCurrentOrderBy: TIEImageEnMViewSortBy;
    fCurrentAscending: boolean;
    fCurrentCaseSensitive : Boolean;

    // Multithread
    fThreadPoolSize: integer;           // maximum threads count (0=disable multithread)
    fThreadPoolIO: TList;               // list of TImageEnIO objects (maximum size is fThreadPoolSize)
    fThreadRequests: TList;             // list of integers, the indexes of the image to load (no maximum size)
    fThreadStarter: TIEStarter;         // starter main thread
    fLookAheadList: TList;               // list of lookaheaded images (index of)
    // Wall paper
    fWallPaper: TPicture;
    fWallPaperStyle: TIEWallPaperStyle;
    // Selections
    fSelectedItem: integer;             // selected image index (-1 none)
    fVisibleSelection: boolean;
    fSelectionWidth: integer;           // selection width with focus
    fSelectionWidthNoFocus: integer;    // selection width without focus
    fSelectionAntialiased: Boolean;
    fSelectionColor: TColor;            // selection color
    fOnImageSelect: TIEImageSelectEvent;
    fOnImageDeselect: TIEImageSelectEvent;
    fOnSelectionChanging : TIESelectionChangingEvent;
    fOnImageAdd : TIEImageAddEvent;
    fOnGetText: TIEGetTextEvent;
    fMouseInteract: TIEMMouseInteract;
    fThumbnailOptionsEx : TIEMThumbnailOptionsEx;
    fKeyInteract: TIEMKeyInteract;
    fSelectedBitmap: TIEBitmap;
    fImageCacheSize: integer;           // stored in fImageList.MaxImagesInMemory
    fImageCacheUseDisk: boolean;        // stored in fImageList.UseDisk
    fMultiSelecting: boolean;
    fEnableMultiSelect: boolean;
    fHaveMultiselected: boolean;        // last mouseMove has selected images
    fSelectInclusive: boolean;          // when true reselecting an image doesn't unselect it
    fMultiSelectionOptions: TIEMultiSelectionOptions;       
    fSelectImages: boolean;             // if true we are inside BeginSelectImages and EndSelectImages
    fChangedSel: boolean;               // true if the selected is changed
    fSelectedFontColor : TColor;        // text color for selected cells

    // Play
    fPlaying: boolean;                  // true=play actived
    fPlayTimer: integer;                // timer for playback of frames (0=not allocated)
    fPlayLoop: boolean;                 // when True executes in loop
    fTimerInProgress: boolean;
    fFrame: integer;                    // current frame on single image mode
    fSaveDM: TIEMDisplayMode;           // displaymode before the animation
    fSaveSel: integer;                  // SelectedImage before the play
    // Following three fields are used by TImageEnMIO to get it updated on added or removed images.
    fLastImOp: integer;                 // last operation of insert(1)/delete(2)/move(3)/swap(4) (0=no op)
    fLastImIdx: integer;                // index of processed image by fLastImOp
    fLastImP1: Integer;                 // param 1
    // transition effects
    fTransition: TIETransitionEffects;  // effect engine
    fTransitionEffect: TIETransitionType; // transition type
    fTransitionDuration: integer;       // transition duration ms
    //
    fOnProgress: TIEProgressEvent;
    fOnBeforeImageDraw: TIEImageDrawEvent;
    fOnBeforeImageDrawEx: TIEImageDrawEventEx;
    fEnableImageCaching: boolean;
    fSoftShadow: TIEVSoftShadow;
    fChessboardSize: integer;
    fChessboardBrushStyle: TBrushStyle;
    fGradientEndColor: TColor;
    fShowText: boolean;
    fSetUpperGap, fSetBottomGap: integer;
    fFlatScrollBars: Boolean;
    fThumbnailFrame: TIEBitmap;
    fThumbnailFrameSelected: TIEBitmap;
    fThumbnailFrameRect: TRect;
    fMultiOnDemands: TList;              // list of TImageEnIO for on demand multi page
    fMaintainInvisibleImages: Integer;  // how much invisible images maintain when they are loaded on demand (-1 = maintain all)
    fLookAhead: Integer;
    fOnAllDisplayed: TNotifyEvent;       // when all images are displayed
    fAllDisplayed: Boolean;
    fUserAction: Boolean;               // if true user has made an action with mouse or keyboard, events fire
    fOnFinishWork: TNotifyEvent;
    fOnAcquireBitmap: TIEAcquireBitmapEvent;
    fOnPlayFrame: TIEPlayFrameEvent;
    fThreadCS: TRTLCriticalSection;     // critical section
    fDefaultBottomTextFont: TFont;
    fDefaultTopTextFont: TFont;
    fDefaultInfoTextFont: TFont;
    fDefaultTextBackgroundStyle: TBrushStyle; 
    fTrackMouseSelection: Boolean;
    fDrawMouseSelection: Boolean;
    fCheckThumbBoundsOnSelect: Boolean;
    fSelectedImageAlwaysVisible : Boolean;
    fOnAfterEvent: TIEAfterEventEvent;              
    fFileTypeList: TStringList;         // Used only when sorting by iesbFileType so we do not need to store file types for all files

    // animations
    fAnimation: TIEAnimation;
    fAnimationTimer: TTimer;
    fAnimationDraggingSlider: Boolean;
    fOnAnimationText: TIEAnimationTextEvent;

    // Automatic scrolling when dragging
    FDragScrollTimer : TTimer;
    FDragScrollCount : Integer;

    // Checkbox support
    fCheckboxes : TIEMCheckboxType;             // Show checkboxes?
    fCheckboxPos : TIEMCheckboxPos;             // Corner that checkbox is shown
    fOnCheckboxClick : TIECheckboxClickEvent;
    fCheckedCount : Integer;                    // Cache checked count
    fCheckedBitmap : TBitmap;                   // images to draw checkboxes
    fUncheckedBitmap : TBitmap;
    fCheckboxMargins : TPoint;                  // Position of checkbox from edges

    // Gestures
    fGestures: TIEMViewerGestures;
    fOnImageEnGesture: TIEImageEnGestureEvent;
    fGestureStartX: integer;
    fGestureStartY: integer;
    fGestureBaseViewX: integer;
    fGestureBaseViewY: integer;
    fGestureBaseZoom: double;
    fGestureStartValue: integer;

    // Automatic scrolling when dragging
    procedure InitializeDragScrollTimer;
    procedure TerminateDragScrollTimer;
    procedure DragScrollTimer(Sender: TObject);
                                          
    function GetHintStr(idx : Integer) : string;
    procedure GetMaxViewXY(var mx, my: integer);
    procedure SetViewX(v: integer);
    procedure SetViewY(v: integer);
    function GetImageX(idx: integer): integer;
    function GetImageY(idx: integer): integer;
    function GetImageCol(idx: integer): integer;
    function GetImageRow(idx: integer): integer;
    procedure SetThumbWidth(v: integer);
    procedure SetThumbHeight(v: integer);
    function GetThumbWidth(): integer;
    function GetThumbHeight(): integer;
    procedure SetZoom(value: double);
    function GetImageCount: integer;
    procedure SetImageFileName(idx: integer; v: WideString);
    function GetImageFileName(idx: integer): WideString;
    procedure SetImageID(idx, v: integer);
    function GetImageID(idx: integer): integer;
    procedure SetImageTag(idx, v: integer);
    function GetImageTag(idx: integer): integer;
    procedure SetImageUserPointer(idx: Integer; v: pointer);
    function GetImageUserPointer(idx: Integer): pointer;
    procedure SetHorizBorder(v: integer);
    function _GetHorizMargin : integer;
    procedure SetVertBorder(v: integer);
    function _GetVertMargin : integer;
    procedure SetSelectedImageAlwaysVisible(v: boolean);
    function DeleteImageNU(idx: integer; bBatchProcessing : Boolean = False): boolean;
    procedure DeleteAllImages();
    procedure SetCheckboxes(v: TIEMCheckboxType);
    procedure SetCheckboxPos(v: TIEMCheckboxPos);
    function GetChecked(index: integer): Boolean;
    procedure SetChecked(index: integer; v : Boolean);
    procedure SetVisibleSelection(v: boolean);
    procedure SetSelectionWidth(v: integer);
    procedure SetSelectionWidthNoFocus(v: integer);
    procedure SetSelectionAntialiased(v: Boolean);
    procedure SetSelectionColor(v: TColor);
    procedure SetSelectedItem(v: integer);
    procedure SetBottomGap(v: integer);
    procedure SetUpperGap(v: integer);   
    procedure SetSideGap(v: integer);
    procedure SetTextMargin(v: integer);
    procedure SetImageBackground(idx: integer; v: TColor);
    function GetImageBackground(idx: integer): TColor;
    procedure SetImageDelayTime(idx: integer; v: Double);
    function GetImageDelayTime(idx: integer): Double;
    function ObtainImageNow(idx: integer): boolean;
    function ObtainImageThreaded(idx: integer; priority: Integer): boolean;
    procedure SetDisplayMode(v: TIEMDisplayMode);
    procedure SetGridWidth(v: integer);
    procedure SetPlaying(v: boolean);
    procedure PlayFrame;
    procedure SetSelectedItemNU(v: integer);
    procedure DeselectNU;
    procedure SetVisibleFrame(v: integer);
    function GetMouseInteract: TIEMMouseInteract;
    function GetKeyInteract: TIEMKeyInteract;
    procedure SetRemoveCorrupted(v: boolean);
    procedure SetDrawImageBackground(v: boolean);
    function GetScrollBarsAlwaysVisible: boolean;
    procedure SetScrollBarsAlwaysVisible(v: boolean);
    procedure SetImageCacheSize(v: integer);
    procedure SetImageCacheUseDisk(v: boolean);
    function GetTransitionRunning: boolean;
    function GetImageTopText(idx: integer): TIEMText;
    function GetImageBottomText(idx: integer): TIEMText;
    function GetImageInfoText(idx: integer): TIEMText;
    procedure SetStyle(value: TIEMStyle);
    procedure SetEnableMultiSelect(Value: boolean);
    function GetMultiSelectedImages(index: integer): integer;
    function GetMultiSelectedImagesCount: integer;
    function GetMultiSelectedImagesList(): TIEArrayOfInteger;
    procedure SetThumbnailsBorderWidth(Value: integer);
    procedure SetThumbnailsBorderColor(Value: TColor);
    procedure SetThumbnailsInternalBorder(Value: boolean);
    procedure SetThumbnailsInternalBorderColor(Value: TColor);
    procedure SetEnableResamplingOnMinor(Value: boolean);      
    procedure SetIconSize(Value: TIEImageEnMViewIconSize);
    procedure DrawImage(DestBitmap: TBitmap; info: PIEImageInfo; IsSelected: boolean; Index: integer);
    procedure ThreadFinish(Sender: TObject);
    function GetImageBitCount(idx: integer): integer;
    function GetMaximumViewX: integer;
    function GetMaximumViewY: integer;
    procedure SetEnableImageCaching(v: boolean);
    function SetImageFromStreamOrFile(idx: integer; Stream: TStream; const FileName: WideString; SourceImageIndex: Integer): boolean;
    procedure SetEnableAlphaChannel(v: boolean);
    procedure SetBackgroundStyle(v: TIEBackgroundStyle);
    procedure SetThumbnailsBackgroundStyle(v: TIEBackgroundStyle);
    procedure SetGradientEndColor(Value: TColor);
    procedure SetFillThumbnail(Value: boolean);
    procedure SetShowText(Value: boolean);
    procedure SetThumbnailClipping(Value: Integer);
    {$ifdef IEINCLUDEFLATSB}
    procedure SetFlatScrollBars(Value: Boolean);
    {$endif}
    function GetJobsRunning: Integer;
    function GetJobsWaiting: Integer;
    function SortCompareFunction(index1, index2: Integer): Integer;
    function SortCompareBy(index1, index2: Integer): Integer;
    function GetOnDemandIO(const filename: WideString; var FrameIndex: Integer): TImageEnIO;
    procedure ClearOnDemandIOList;
    procedure LoadMultiOnDemand(io: TImageEnIO; frameindex: Integer; var dt: Double);
    function IsOnDemand(info: PIEImageInfo): Boolean;
    function IsLookAhead(idx: Integer): Boolean;
    procedure SetOnFinishWork(v: TNotifyEvent); virtual;
    function GetOnFinishWork: TNotifyEvent; virtual;
    procedure SetOnAcquireBitmap(v: TIEAcquireBitmapEvent); virtual;
    function GetOnAcquireBitmap: TIEAcquireBitmapEvent; virtual;
    function GetImageEnVersion: String;
    procedure SetImageEnVersion(Value: String);
    procedure AbortImageLoading(idx: Integer);
    procedure Sort2(Compare: TIEImageEnMViewSortCompare; CompareEx: TIEImageEnMViewSortCompareEx);
    function ImageAtPosWithCheckEvent(x, y: integer; checkBounds: Boolean; bRelativeToWindow: boolean = true): Integer;
    procedure SetAnimation(value: TIEAnimation);
    procedure AnimGetImageInfo(Sender: TObject; imageIndex: Integer; isVisible: Boolean; var ImageWidth: Integer; var ImageHeight: Integer; var text: WideString);
    procedure AnimGetImage(Sender: TObject; imageIndex: Integer; var image: TIEBitmap; var text: WideString);
    procedure AnimReleaseImage(Sender: TObject; imageIndex: Integer; var image: TIEBitmap);
    procedure AnimPaintTo(dest: TIEBitmap);
    procedure AnimTimer(Sender: TObject);
    procedure CheckSelectedImageIsVisible;
    procedure DoAfterEvent(e: TIEAfterEvent);
    procedure _InsertImage(idx: integer; bSelectIt : Boolean);
    procedure ProcessUndoRedo(Sender : TObject; bIsUndo : Boolean; Source : TIEUndoSource; UndoObj : TObject; iIndex : Integer; var bHandled : Boolean);
    function CheckSelectionChangingAllowed : Boolean;
    procedure WMGestureNotify(var Msg: TIEWMGestureNotify); message IEWM_GESTURENOTIFY;
    procedure WMEnabled(var Msg: TMessage); message WM_ENABLE;
    procedure WMGesture(var Msg: TMessage); message IEWM_GESTURE;
    procedure DoImageEnGestureEvent(const GInfo: TIEGESTUREINFO; var Handled: boolean); virtual;
    function PerformZoomSnap(Value: double): double;
  protected
    ///////////////////////
    // P R O T E C T E D
    //

    // encapsulated components
    fImageEnMIO: TImageEnMIO;
    fImageEnProc: TImageEnProc;

    // selections
    fMultiSelectedImages: TList; // array of selected images (pointer=integer=index of the selected image)

    fImageInfo: TList;                  // contains TIEImageInfo structures
    fShowThumbnailHint : Boolean;     
    fDragging: Boolean;

    function GetImageEnMIO: TImageEnMIO; virtual;
    function GetImageEnProc: TImageEnProc; virtual;
    procedure SetScrollBars(v: TScrollStyle); virtual;
    procedure SetStoreType(v : TIEStoreType);
    procedure IEMAutoScroll(var Message: TMessage); message IEM_AUTOSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CNKEYUP(var Message: TMessage); message CN_KEYUP;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure ViewChange(c: integer); virtual;
    function PaletteChanged(Foreground: Boolean): Boolean; override;
    procedure SetBackGround(cl: TColor); override;
    function GetFBitmap: TBitmap; override;
    function GetIEBitmap: TIEBitmap; override;
    procedure SetThumbnailOptionsEx(v : TIEMThumbnailOptionsEx);
    procedure SetMouseInteract(v: TIEMMouseInteract); virtual;
    procedure SetKeyInteract(v: TIEMKeyInteract); virtual;
    function GetImageWidth(idx: integer): integer;
    function GetImageHeight(idx: integer): integer;
    function GetImageOriginalWidth(idx: integer): integer;
    function GetImageOriginalHeight(idx: integer): integer;
    function GetImageCreateDate(idx: integer): TDateTime;
    function GetImageFileSize(idx: integer): Int64;
    function GetImageEditDate(idx: integer): TDateTime;
    function GetImageFileType(idx: integer): WideString;
    function GetImageType(idx: integer): TIEFolderImageType;
    procedure SetImageOriginalWidth(idx: integer; Value: integer);
    procedure SetImageOriginalHeight(idx: integer; Value: integer);
    procedure SetImageCreateDate(idx: integer; Value: TDateTime);
    procedure SetImageFileSize(idx: integer; Value:  Int64);
    procedure SetImageEditDate(idx: integer; Value: TDateTime);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SelectAtPos(X, Y: integer; Shift: TShiftState);
    procedure SetWallPaper(Value: TPicture);
    procedure SetWallPaperStyle(Value: TIEWallPaperStyle);
    function GetHasAlphaChannel: boolean; override;
    function GetAlphaChannel: TIEBitmap; override;
    procedure SetOnProgress(v: TIEProgressEvent); virtual;
    function GetOnProgress: TIEProgressEvent; virtual;
    procedure ClearThreadsAndRequests; virtual;
    procedure ClearCache;
    procedure DoWrongImage(OutBitmap: TIEBitmap; idx: integer; var ASourceType : TIESourceType); virtual;
    procedure DoImageSelect(idx: integer); virtual;
    procedure DoImageDeselect(idx: integer); virtual;
    {$ifdef IEDOTNETVERSION}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$endif}
    procedure SwapImages(idx1, idx2: Integer);
    function IsRequested(idx: Integer): Integer;
    procedure PaintBackgroundTo(DestBitmap: TBitmap);
    function ImageAtPosEx(x, y: integer; checkBounds: Boolean = true; bRelativeToWindow: boolean = true): Integer;
    function ReplaceIEMConsts(const ws : WideString; idx : Integer) : WideString;
    procedure GetFileDetailsForImage(idx: integer);
    procedure DrawCheckbox(ACanvas : TCanvas; Index : Integer; ThumbRect : TRect; IsSelected : Boolean; bRelativeToView : Boolean = False); overload; 
    procedure DrawCheckbox(ACanvas : TCanvas; Index : Integer; IsSelected : Boolean; bRelativeToView : Boolean = False); overload;
    function CheckboxAtPos(X, Y : Integer) : Integer;
    procedure ClickCheckboxAtPos(X, Y : Integer);
    function ThumbToCheckboxRect(ThumbRect : TRect; bRelativeToView : Boolean = False) : TRect;
    procedure _UpdateImage(idx: integer; bInvalidateOnly : Boolean);
  public
    /////////////////////
    // P U B L I C
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    // display
    procedure Update; override;
    procedure UpdateEx(bUpdateCache : Boolean; bRespositionSelection : Boolean = False);
    procedure PaintTo(DestBitmap: TBitmap); virtual;
    procedure PaintToCanvas(DestCanvas: TCanvas); virtual;
    procedure Paint; override;
    property ClientWidth;
    property ClientHeight;
    property ViewX: integer read fViewX write SetViewX;
    property ViewY: integer read fViewY write SetViewY;
    property Zoom: double read fZoom write SetZoom;
    property MaximumViewX: integer read GetMaximumViewX;
    property MaximumViewY: integer read GetMaximumViewY;
    procedure SetViewXY(x, y: integer);
    procedure CenterSelected;
    procedure CenterFrame;
    procedure LockPaint; override;
    procedure LockUpdate; virtual;
    function UnLockPaint: Integer; override;
    function UnLockUpdate: Integer; virtual;  
    function UnLockUpdateEx: integer;
    function NPUnLockPaint: integer; override;

{!!
<FS>TImageEnMView.LockPaintCount

<FM>Declaration<FC>
property LockPaintCount: Integer;

<FM>Description<FN>
Returns the lock painting state. A value of 0 means no locking. A value greater than zero means locking is in place (i.e. painting is disabled).

Calling <A TImageEnMView.LockPaint> increments <A TImageEnMView.LockPaintCount>, <A TImageEnMView.UnLockPaint> decrements it.
!!}
    property LockPaintCount: integer read fLockPaint;

{!!
<FS>TImageEnMView.LockUpdateCount

<FM>Declaration<FC>
property LockUpdateCount: Integer;

<FM>Description<FN>
Returns the lock updating state. A value of 0 means no locking. A value greater than zero means locking is in place (i.e. updating is disabled).

Calling <A TImageEnMView.LockUpdate> increments <A TImageEnMView.LockUpdateCount>, <A TImageEnMView.UnLockUpdate> decrements it.
!!}
    property LockUpdateCount: integer read fLockUpdate;

{!!
<FS>TImageEnMView.SoftShadow

<FM>Declaration<FC>
property SoftShadow: <A TIEVSoftShadow>;

<FM>Description<FN>
Paints a soft shadow beneath the thumbnails.

Note: You will need to enable the <L TImageEnMView.EnableAlphaChannel>alpha channel</L>.

<IMG help_images\71.bmp>

<FM>Example<FC>
ImageEnMView.EnableAlphaChannel := True;
ImageEnMView.SoftShadow.Enabled := True;
!!}
    property SoftShadow: TIEVSoftShadow read fSoftShadow;

    procedure SetChessboardStyle(Size: integer; BrushStyle: TBrushStyle);

    property GradientEndColor: TColor read fGradientEndColor write SetGradientEndColor;

    property FillThumbnail: boolean read fFillThumbnail write SetFillThumbnail;

    procedure SetModernStyling(bAutoGridWidth : Boolean = False; iThumbWidth : Integer = 0; iThumbHeight : Integer = 0);


{!!
<FS>TImageEnMView.ThumbsRounded

<FM>Declaration<FC>
property ThumbsRounded: Integer;

<FM>Description<FN>
If value greater than zero is specified for ThumbsRounded then the image is drawn with rounded corners.

Smaller values provide the maximum roundness, while large value produce minimal roundness.

<FM>Example<FC>
ImageEnMView1.ThumbsRounded := 5;
!!}
    property ThumbsRounded: Integer read fThumbsRounded write fThumbsRounded;

    procedure SetPresetThumbnailFrame(PresetIndex: Integer; UnSelectedColor: TColor; SelectedColor: TColor);

{!!
<FS>TImageEnMView.ThumbnailFrame

<FM>Declaration<FC>
property ThumbnailFrame: <A TIEBitmap>;

<FM>Description<FN>
Specifies a bitmap to display under the thumbnail.

<FM>Examples<FC>
ImageEnMView1.ThumbnailFrame := ImageEnViewUnSelected.IEBitmap;
ImageEnMView1.ThumbnailFrameSelected := ImageEnViewSelected.IEBitmap;
ImageEnMView1.ThumbnailFrameRect := Rect(10, 10, 50, 50);
!!}
    property ThumbnailFrame: TIEBitmap read fThumbnailFrame write fThumbnailFrame;

{!!
<FS>TImageEnMView.ThumbnailFrameSelected

<FM>Declaration<FC>
property ThumbnailFrameSelected: <A TIEBitmap>;

<FM>Description<FN>
Specifies a bitmap to display under the thumbnail when it is selected.

<FM>Examples<FC>
ImageEnMView1.ThumbnailFrame := ImageEnViewUnSelected.IEBitmap;
ImageEnMView1.ThumbnailFrameSelected := ImageEnViewSelected.IEBitmap;
ImageEnMView1.ThumbnailFrameRect := Rect(10, 10, 50, 50);
!!}
    property ThumbnailFrameSelected: TIEBitmap read fThumbnailFrameSelected write fThumbnailFrameSelected;

{!!
<FS>TImageEnMView.ThumbnailFrameRect

<FM>Declaration<FC>
property ThumbnailFrameRect: TRect;

<FM>Description<FN>
Using <A TImageEnMView.ThumbnailFrameSelected> and <A TImageEnMView.ThumbnailFrame>, this property specifies where the image (the thumbnail) will be drawn.

<FM>Examples<FC>
ImageEnMView1.ThumbnailFrame := ImageEnViewUnSelected.IEBitmap;
ImageEnMView1.ThumbnailFrameSelected := ImageEnViewSelected.IEBitmap;
ImageEnMView1.ThumbnailFrameRect := Rect(10, 10, 50, 50);
!!}
    property ThumbnailFrameRect: TRect read fThumbnailFrameRect write fThumbnailFrameRect;

    // others
    property MouseCapture;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function GetLastOp: integer;
    function GetLastOpIdx: integer;
    function GetLastOpP1: integer;
    property ScrollBarsAlwaysVisible: boolean read GetScrollBarsAlwaysVisible write SetScrollBarsAlwaysVisible;   // NB: No default. Always store!
    function RemoveBlankPages(Tolerance: Double = 0; Complete: boolean = true; LeftToRight: boolean = true ): Integer;
    function CalcGridWidth(): Integer;
    function CalcGridHeight(): Integer;

{!!
<FS>TImageEnMView.VScrollBarParams

<FM>Declaration<FC>
property VScrollBarParams: <A TIEScrollBarParams>;

<FM>Description<FN>
VScrollBarParams allows an application to customize the behavior of the vertical scroll bar. This includes tracking (display refresh on mouse dragging), up/down buttons scroll distance and pagedown/up scroll distance.

<FM>Example<FC>
// disable tracking
ImageEnMView1.VScrollBarParams.Tracking := False;
!!}
    property VScrollBarParams: TIEScrollBarParams read fVScrollBarParams;

{!!
<FS>TImageEnMView.HScrollBarParams

<FM>Declaration<FC>
property HScrollBarParams: <A TIEScrollBarParams>;

<FM>Description<FN>
HScrollBarParams allows an application to customize the behavior of the horizontal scroll bar. This includes tracking (display refresh on mouse dragging), up/down buttons scroll distance and pagedown/up scroll distance.

<FM>Example<FC>
// disable tracking
ImageEnMView1.HScrollBarParams.Tracking := False;
!!}
    property HScrollBarParams: TIEScrollBarParams read fHScrollBarParams;


{!!
<FS>TImageEnMView.MouseWheelParams

<FM>Declaration<FC>
property MouseWheelParams: <A TIEMouseWheelParams>;

<FM>Description<FN>
Properties to customize the behavior of the mouse wheel.

<FM>Examples<FC>
// For iemwVScroll used with iemwPercentage, TImageEnMView assumes a theoretical grid of 12.5 thumbnails high
// So the default of 8% will scroll one thumbnail with each wheel click
// Whereas 16% would scroll the height of two thumbnails
ImageEnMView1.MouseWheelParams.Action := iemwVScroll;
ImageEnMView1.MouseWheelParams.Variation := iemwPercentage;
ImageEnMView1.MouseWheelParams.value := 4; // half a thumbnail

// Mouse wheel will navigate to the next or previous image
ImageEnMView1.MouseWheelParams.Action := iemwNavigate;

<FM>Demo<FN>
\Other\MouseWheel\MouseWheelParams.dpr
!!}
    property MouseWheelParams: TIEMouseWheelParams read fMouseWheelParams;

    procedure RemoveAlphaChannel(Merge: boolean); override;
    procedure CallBitmapChangeEvents; override;
    procedure FillFromDirectory(const Directory: WideString; Limit : integer = -1; AllowUnknownFormats : boolean = false; const ExcludeExtensions : WideString = '';
                                DetectFileFormat : boolean = false; const FilterMask : WideString = ''; IncludeVideoFiles : Boolean = False;
                                LoadOnDemand : boolean = true;
                                DefaultTopText : TIEImageEnMViewDefaultText = iedtNone;
                                DefaultInfoText : TIEImageEnMViewDefaultText = iedtNone;
                                DefaultBottomText : TIEImageEnMViewDefaultText = iedtFilename;
                                bShowHiddenFiles : Boolean = False;
                                bShowFolders : Boolean = False);

{!!
<FS>TImageEnMView.EnableAdjustOrientation

<FM>Declaration<FC>
property EnableAdjustOrientation: Boolean;

<FM>Description<FN>
When this property is True all images which have orientation information (typically jpeg with EXIF data) will be automatically rotated for display (actual file is not modified).

Orientation information is often found in digital photos from high-end cameras. ImageEn uses the data found in <A TIOParamsVals.EXIF_Orientation> or <A TIOParamsVals.TIFF_Orientation> to determine the orientation.

!!}
    property EnableAdjustOrientation: Boolean read fEnableAdjustOrientation write fEnableAdjustOrientation;

{!!
<FS>TImageEnMView.MaintainInvisibleImages

<FM>Declaration<FC>
property MaintainInvisibleImages: Integer;

<FM>Description<FN>
Specifies the number of images to maintain when they are no longer visible (scrolled out of view). The default value is 15.

Specify -1 to maintain all images in memory.
Specify 0 to discard all images when they are not visible.

Note: This property only applies when images are loaded on demand, e.g. by setting <A TImageEnMView.ImageFileName>, <A TImageEnMView.FillFromDirectory> or <A TImageEnMView.LoadFromFileOnDemand>.
!!}
    property MaintainInvisibleImages: Integer read fMaintainInvisibleImages write fMaintainInvisibleImages;

    procedure LoadFromFileOnDemand(const FileName: WideString; Append: Boolean = false);

{!!
<FS>TImageEnMView.EnableLoadEXIFThumbnails

<FM>Declaration<FC>
property EnableLoadEXIFThumbnails : Boolean;

<FM>Description<FN>
Images from digital camera often contain a small thumnbail in their <L TIOParamsVals.EXIF_Bitmap>Exif data</L>. When this option is enabled, ImageEn will use the EXIF thumbnail for display rather than loading and sub-sampling the full size image. This speeds up loading of large images.

By default, EnableLoadEXIFThumbnails is true.
!!}
    property EnableLoadEXIFThumbnails: Boolean read fEnableLoadEXIFThumbnails write fEnableLoadEXIFThumbnails;

    procedure CreateMorphingSequence(Source: TImageEnVect; Target: TImageEnVect; FramesCount: Integer);
    // multithreads
    property JobsRunning: Integer read GetJobsRunning;
    property JobsWaiting: Integer read GetJobsWaiting;

{!!
<FS>TImageEnMView.LookAhead

<FM>Declaration<FC>
property LookAhead: Integer;

<FM>Description<FN>
Specifies the number of images to pre-load (load before they are required for display). Default is 0.

Note: This property only applies when images are loaded on demand, e.g. by setting <A TImageEnMView.ImageFileName>, <A TImageEnMView.FillFromDirectory> or <A TImageEnMView.LoadFromFileOnDemand>.
!!}
    property LookAhead: Integer read fLookAhead write fLookAhead;

    // cache
    procedure ClearImageCache(idx: integer);
    property EnableImageCaching: boolean read fEnableImageCaching write SetEnableImageCaching default true;
    // images
    property ImageWidth[idx: integer]: integer read GetImageWidth;
    property ImageHeight[idx: integer]: integer read GetImageHeight;
    property ImageOriginalWidth[idx: integer]: integer read GetImageOriginalWidth write SetImageOriginalWidth;
    property ImageOriginalHeight[idx: integer]: integer read GetImageOriginalHeight write SetImageOriginalHeight;
    property ImageBitCount[idx: integer]: integer read GetImageBitCount;
    property ImageX[idx: integer]: integer read GetImageX;
    property ImageY[idx: integer]: integer read GetImageY;
    property ImageRow[idx: integer]: integer read GetImageRow;
    property ImageCol[idx: integer]: integer read GetImageCol;
    property ImageFileName[idx: integer]: WideString read GetImageFileName write SetImageFileName;
    property ImageID[idx: integer]: integer read GetImageID write SetImageID;
    property ImageTag[idx: integer]: integer read GetImageTag write SetImageTag;
    property ImageUserPointer[idx: Integer]: pointer read GetImageUserPointer write SetImageUserPointer;
    property ImageBackground[idx: integer]: TColor read GetImageBackground write SetImageBackground;
    property ImageDelayTime[idx: integer]: Double read GetImageDelayTime write SetimageDelayTime;
    property ImageCreateDate[idx: Integer]: TDateTime read GetImageCreateDate write SetImageCreateDate;
    property ImageEditDate[idx: integer]: TDateTime read GetImageEditDate write SetImageEditDate;
    property ImageFileSize[idx: integer]: Int64 read GetImageFileSize write SetImageFileSize;

{!!
<FS>TImageEnMView.ImageTopText

<FM>Declaration<FC>
property ImageTopText[idx: Integer]: <A TIEMText>;

<FM>Description<FN>
Use <A TImageEnMView.ImageTopText>, <A TImageEnMView.ImageBottomText> and <A TImageEnMView.ImageInfoText> to specify text that is drawn inside the thumbnail of index, <FC>idx<FN>.

The <A TIEMText> object defines the text and its formatting.

<A TImageEnMView.BottomGap> and <A TImageEnMView.UpperGap> are adjusted to fit the text.

<FM>Example<FC>
ImageEnMView1.ImageTopText[idx].Caption    := 'Top text';
ImageEnMView1.ImageInfoText[idx].Caption   := 'Info text';
ImageEnMView1.ImageBottomText[idx].Caption := 'Bottom text';

<IMG help_images\58.bmp>


ImageEnMView1.ImageTopText[idx].Caption       := 'Top text';
ImageEnMView1.ImageInfoText[idx].Caption      := 'Info text';
ImageEnMView1.ImageBottomText[idx].Caption    := 'Bottom text';
ImageEnMView1.ImageBottomText[idx].Background := clYellow;
ImageEnMView1.ImageBottomText[idx].Font.Color := clBlue;

<IMG help_images\59.bmp>
!!}
    property ImageTopText[idx: integer]: TIEMText read GetImageTopText;

{!!
<FS>TImageEnMView.ImageBottomText

<FM>Declaration<FC>
property ImageBottomText[idx: Integer]: <A TIEMText>;

<FM>Description<FN>
Use <A TImageEnMView.ImageTopText>, <A TImageEnMView.ImageBottomText> and <A TImageEnMView.ImageInfoText> to specify text that is drawn inside the thumbnail of index, <FC>idx<FN>.

The <A TIEMText> object defines the text and its formatting.

<A TImageEnMView.BottomGap> and <A TImageEnMView.UpperGap> are adjusted to fit the text.

<FM>Example<FC>
ImageEnMView1.ImageTopText[idx].Caption    := 'Top text';
ImageEnMView1.ImageInfoText[idx].Caption   := 'Info text';
ImageEnMView1.ImageBottomText[idx].Caption := 'Bottom text';

<IMG help_images\58.bmp>


ImageEnMView1.ImageTopText[idx].Caption       := 'Top text';
ImageEnMView1.ImageInfoText[idx].Caption      := 'Info text';
ImageEnMView1.ImageBottomText[idx].Caption    := 'Bottom text';
ImageEnMView1.ImageBottomText[idx].Background := clYellow;
ImageEnMView1.ImageBottomText[idx].Font.Color := clBlue;

<IMG help_images\59.bmp>
!!}
    property ImageBottomText[idx: integer]: TIEMText read GetImageBottomText;

{!!
<FS>TImageEnMView.ImageInfoText

<FM>Declaration<FC>
property ImageInfoText[idx: Integer]: <A TIEMText>;

<FM>Description<FN>
Use <A TImageEnMView.ImageTopText>, <A TImageEnMView.ImageBottomText> and <A TImageEnMView.ImageInfoText> to specify text that is drawn inside the thumbnail of index, <FC>idx<FN>.

The <A TIEMText> object defines the text and its formatting.

<A TImageEnMView.BottomGap> and <A TImageEnMView.UpperGap> are adjusted to fit the text.

<FM>Example<FC>
ImageEnMView1.ImageTopText[idx].Caption    := 'Top text';
ImageEnMView1.ImageInfoText[idx].Caption   := 'Info text';
ImageEnMView1.ImageBottomText[idx].Caption := 'Bottom text';

<IMG help_images\58.bmp>


ImageEnMView1.ImageTopText[idx].Caption       := 'Top text';
ImageEnMView1.ImageInfoText[idx].Caption      := 'Info text';
ImageEnMView1.ImageBottomText[idx].Caption    := 'Bottom text';
ImageEnMView1.ImageBottomText[idx].Background := clYellow;
ImageEnMView1.ImageBottomText[idx].Font.Color := clBlue;

<IMG help_images\59.bmp>
!!}
    property ImageInfoText[idx: integer]: TIEMText read GetImageInfoText;


{!!
<FS>TImageEnMView.DefaultBottomTextFont

<FM>Declaration<FC>
property DefaultBottomTextFont: TFont;

<FM>Description<FN>
If you are using <A TImageEnMView.ImageBottomText> to add text to your thumbnails, then a default font can be specifed for all thumbnails with this property.

<FM>Example<FC>
ImageEnMView1.DefaultBottomTextFont.Height := 14;
ImageEnMView1.DefaultBottomTextFont.Name := 'Arial';
ImageEnMView1.FillFromDirectory('C:\Pictures');

<FM>See Also<FN>
- <A TImageEnMView.DefaultInfoTextFont>
- <A TImageEnMView.DefaultTopTextFont>
!!}
    property DefaultBottomTextFont: TFont read fDefaultBottomTextFont;

{!!
<FS>TImageEnMView.DefaultTopTextFont

<FM>Declaration<FC>
property DefaultBottomTextFont: TFont;

<FM>Description<FN>
If you are using <A TImageEnMView.ImageTopText> to add text to your thumbnails, then a default font can be specifed for all thumbnails with this property.

<FM>See Also<FN>
- <A TImageEnMView.DefaultBottomTextFont>
- <A TImageEnMView.DefaultInfoTextFont>
!!}
    property DefaultTopTextFont: TFont read fDefaultTopTextFont;

{!!
<FS>TImageEnMView.DefaultInfoTextFont

<FM>Declaration<FC>
property DefaultInfoTextFont: TFont;

<FM>Description<FN>
If you are using <A TImageEnMView.ImageInfoText> to add text to your thumbnails, then a default font can be specifed for all thumbnails with this property.

<FM>See Also<FN>
- <A TImageEnMView.DefaultBottomTextFont>
- <A TImageEnMView.DefaultTopTextFont>
!!}
    property DefaultInfoTextFont: TFont read fDefaultInfoTextFont;


{!!
<FS>TImageEnMView.DefaultTextBackgroundStyle

<FM>Declaration<FC>
property DefaultTextBackgroundStyle: TBrushStyle;

<FM>Description<FN>
If you are adding text to your thumbnails, then you can override the default background style with this property.

<FM>Example<FC>
// Ensure all thumbnails do not have a solid background
ImageEnMView1.DefaultTextBackgroundStyle := bsClear;
ImageEnMView1.FillFromDirectory('C:\Pictures');   

<FM>See Also<FN>
- <A TImageEnMView.ImageTopText>
- <A TImageEnMView.ImageInfoText>
- <A TImageEnMView.ImageBottomText>
!!}
    property DefaultTextBackgroundStyle: TBrushStyle read fDefaultTextBackgroundStyle write fDefaultTextBackgroundStyle;

    property ShowText: boolean read fShowText write SetShowText;
    procedure UpdateImage(idx: integer);

    procedure InsertImage(Idx : integer); overload;
    procedure InsertImage(Idx : integer; Stream : TStream); overload;
    procedure InsertImage(Idx : integer; Bitmap : TIEBitmap); overload;
    procedure InsertImage(Idx : integer; Bitmap : TBitmap); overload;
    procedure InsertImage(Idx : integer; Width, Height : integer; PixelFormat : TIEPixelFormat = ie24RGB); overload;
    procedure InsertImage(Idx : integer; const FileName : string); overload;
    procedure InsertImage(Idx : integer; const FileName : string;
                          LoadOnDemand : boolean;
                          DefaultTopText : TIEImageEnMViewDefaultText = iedtNone;
                          DefaultInfoText : TIEImageEnMViewDefaultText = iedtNone;
                          DefaultBottomText : TIEImageEnMViewDefaultText = iedtFilename;
                          bSelectIt : Boolean = true); overload;

    procedure InsertImageEx(idx: integer);

    procedure InsertTransitionFrames(Idx : integer; iFrameCount : Integer; Effect : TIETransitionType; iWidth : Integer = -1; iHeight : Integer = -1; 
                                     BackgroundColor : TColor = -1; ResamplingFilter: TResampleFilter = rfFastLinear);
    procedure InsertTransitionFramesEx(Idx : integer; iFrameCount : Integer; Effect : TIETransitionType;
                                       StartRect, EndRect : TRect; RectMaintainAspectRatio : boolean = True;
                                       iWidth : Integer = -1; iHeight : Integer = -1; bStretchSmall : Boolean = False;
                                       BackgroundColor : TColor = -1; ResamplingFilter: TResampleFilter = rfFastLinear;
                                       Timing : TIETransitionTiming = iettLinear);

    procedure MoveImage(idx: integer; destination: integer);
    procedure MoveSelectedImagesTo(beforeImage: Integer);
    procedure Sort(Compare: TIEImageEnMViewSortCompare); overload;
    procedure Sort(Compare: TIEImageEnMViewSortCompareEx); overload;
    procedure Sort(OrderBy: TIEImageEnMViewSortBy; Ascending: boolean = true; CaseSensitive: boolean = true); overload;
    function AppendImage(): integer; overload;
    function AppendImage(Stream: TStream): integer; overload;
    function AppendImage(Bitmap: TIEBitmap): integer; overload;
    function AppendImage(Bitmap : TBitmap): integer; overload;
    function AppendImage(Width, Height: Integer; PixelFormat: TIEPixelFormat = ie24RGB): Integer; overload;
    function AppendImage(const FileName: String): integer; overload;
    function AppendImage(const FileName: String;
                         LoadOnDemand : boolean;
                         DefaultTopText : TIEImageEnMViewDefaultText = iedtNone;
                         DefaultInfoText : TIEImageEnMViewDefaultText = iedtNone;
                         DefaultBottomText : TIEImageEnMViewDefaultText = iedtFilename;
                         bSelectIt : Boolean = true): integer; overload;

    {$ifdef IEINCLUDEDEPRECATEDMETHODS}
    function AppendImage2(Width, Height: Integer; PixelFormat: TIEPixelFormat=ie24RGB): Integer; {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
    {$endif}
    function AppendSplit(SourceGrid: TIEBitmap; cellWidth: Integer; cellHeight: Integer; maxCount: Integer = 0): Integer;
    procedure DeleteImage(idx: integer);
    procedure DeleteSelectedImages;
    property ImageCount: integer read GetImageCount;
    procedure UpdateCoords;
    procedure SetImage(idx: integer; srcImage: TBitmap); overload;
    procedure SetImage(idx: Integer; width, height: Integer; PixelFormat: TIEPixelFormat); overload;
    procedure SetImageEx(idx: integer; srcImage: TBitmap);
    procedure SetIEBitmapEx(idx: integer; srcImage: TIEBaseBitmap);
    procedure SetIEBitmap(idx: integer; srcImage: TIEBaseBitmap);
    function SetImageFromFile(idx: integer; const FileName: WideString; SourceImageIndex: Integer = 0): boolean;
    function SetImageFromStream(idx: integer; Stream: TStream; SourceImageIndex: Integer = 0): boolean;
    procedure GetImageToFile(idx: Integer; const FileName: WideString);
    procedure GetImageToStream(idx: Integer; Stream: TStream; ImageFormat: TIOFileType);
    procedure SetImageRect(idx: integer; srcImage: TBitmap; x1, y1, x2, y2: integer); overload;
    procedure SetImageRect(idx: integer; srcImage: TIEBitmap; x1, y1, x2, y2: integer); overload;
    procedure Clear;
    function GetBitmap(idx: integer): TBitmap;
    procedure ReleaseBitmap(idx: integer; saveChanges: Boolean = true);
    function GetTIEBitmap(idx: integer): TIEBitmap;
    function GetImageVisibility(idx: integer): integer;
    function ImageAtPos(x, y: integer; checkBounds: Boolean = true): integer;
    function ImageAtGridPos(row, col: integer): integer;
    function InsertingPoint(x, y: integer): integer;

    procedure SetThumbnailSize(width, height: Integer);

{!!
<FS>TImageEnMView.ThumbnailResampleFilter

<FM>Declaration<FC>
property ThumbnailResampleFilter: <A TResampleFilter>;

<FM>Description<FN>
Specifies the filter to use when thumbnails are generated (when assigning images to a TImageEnMView and <A TImageEnMView.StoreType> = ietThumb). Filters improve the quality of the thumbnail, but can slow down the application.
A value of <FC>rfNone<FN> provides no quality enhancement. Filters such as <FC>rfLanczos3<FN> provide excellent quality, but are slower. The default value is <FC>rfFastLinear<FN> which improves quality with negligible speed impact.

Note: Unlike <A TImageEnMView.ThumbnailDisplayFilter>, ThumbnailResampleFilter is only used once for each image (when it first assigned/loaded)

<FM>Example<FC>
// insert image 1.jpg and 2.jpg. Only 1.jpg will be filtered.
ImageEnMView1.ThumbnailResampleFilter := rfBSpline;
Idx := ImageEnMView1.AppendImage;
ImageEnMView1.SetImageFromFile('1.jpg');

ImageEnMView1.ThumbnailResampleFilter := rfNone;
Idx := ImageEnMView1.AppendImage;
ImageEnMView1.SetImageFromFile('2.jpg');
!!}
    property ThumbnailResampleFilter: TResampleFilter read fThumbnailResampleFilter write fThumbnailResampleFilter;

    property ThumbnailClipping : Integer read fThumbnailClipping write SetThumbnailClipping default 0;

{!!
<FS>TImageEnMView.ThumbnailDisplayFilter

<FM>Declaration<FC>
property ThumbnailDisplayFilter: <A TResampleFilter>;

<FM>Description<FN>
ThumbnailDisplayFilter specifies a filter to apply when an image (thumbnail) need to be resized for display. Filters improve the quality of the thumbnail, but can slow down the application.
A value of <FC>rfNone<FN> provides no quality enhancement. Filters such as <FC>rfLanczos3<FN> provide excellent quality, but are slower. A typical value is <FC>rfFastLinear<FN> which improves quality with negligible speed impact.

Notes:
- Unlike <A TImageEnMView.ThumbnailResampleFilter>, ThumbnailDisplayFilter is used every time the image needs to be displayed
- For black/white images <FC>rfFastLinear<FN> is always used.

<FM>Example<FC>
// Use high quality thumbnails
ImageEnMView1.ThumbnailDisplayFilter := rfLanczos3;
ImageEnMView1.Repaint;
!!}
    property ThumbnailDisplayFilter: TResampleFilter read fThumbnailDisplayFilter write fThumbnailDisplayFilter;

    property EnableResamplingOnMinor: boolean read fEnableResamplingOnMinor write SetEnableResamplingOnMinor;

    property IconSize : TIEImageEnMViewIconSize read fIconSize write SetIconSize;

    procedure CopyToIEBitmap(idx: integer; bmp: TIEBitmap);
    function IsVisible(idx: integer): boolean;
    procedure ReloadImage(idx: Integer);
    // allocations
    procedure PrepareSpaceFor(Width, Height: integer; Bitcount: integer; ImageCount: integer);
    property ImageCacheUseDisk: Boolean read fImageCacheUseDisk write SetImageCacheUseDisk;
    // selection
    property SelectedImage: integer read fSelectedItem write SetSelectedItem;
    property SelectedImageAlwaysVisible: Boolean read fSelectedImageAlwaysVisible write SetSelectedImageAlwaysVisible default true;
    procedure DeSelect;
    procedure SelectSeek(pos: TIESeek);
    procedure CopySelection(SourceMView: TImageEnMView);


{!!
<FS>TImageEnMView.CheckThumbBoundsOnSelect

<FM>Declaration<FC>
property CheckThumbBoundsOnSelect: Boolean;

<FM>Description<FN>
When enabled, ImageEn checks the thumbnail bounding rectangle before selecting it. The default is False, which allows more flexible selections.

Note: When using multi-selection, CheckThumbBoundsOnSelect must be set to false (the default)
!!}
    property CheckThumbBoundsOnSelect: Boolean read fCheckThumbBoundsOnSelect write fCheckThumbBoundsOnSelect;

{!!
<FS>TImageEnMView.TrackMouseSelection

<FM>Declaration<FC>
property TrackMouseSelection: Boolean;

<FM>Description<FN>
If True a semi-transparent rectangle is shown during mouse selection. Default is False.

<FM>Demo<FN>
Multi\Multiview
!!}
    property TrackMouseSelection: Boolean read fTrackMouseSelection write fTrackMouseSelection;

{!!
<FS>TImageEnMView.MultiSelecting

<FM>Declaration<FC>
property MultiSelecting: Boolean;

<FM>Description<FN>
Set MultiSelecting to True to simulate a CTRL key press. It allows the user to select multiple images with mouse or arrow keys without pressing the CTRL key.
Also, MultiSelecting can be used to select more than one image using the <A TImageEnMView.SelectedImage> property.

Note: To allow multi-selection, the <A TImageEnMView.EnableMultiSelect> property must be True.

<FM>Example<FC>
// select images 0 and 1 (assumes that you have set ImageEnMView1.EnableMultiSelect := True at design time)
ImageEnMView1.Deselect;
ImageEnMView1.MultiSelecting := True;
ImageEnMView1.SelectedImage  := 0;
ImageEnMView1.SelectedImage  := 1;
ImageEnMView1.MultiSelecting := False;
!!}
    property MultiSelecting: boolean read fMultiSelecting write fMultiSelecting;

    property MultiSelectedImages[index: integer]: integer read GetMultiSelectedImages;
    property MultiSelectedImagesCount: integer read GetMultiSelectedImagesCount;
    property MultiSelectedImagesList: TIEArrayOfInteger read GetMultiSelectedImagesList;

    procedure MultiSelectSortList;
    procedure UnSelectImage(idx: integer);
    procedure ToggleSelectImage(idx: integer);
    procedure SelectImage(idx: integer);
    procedure SelectAll;
    procedure BeginSelectImages;
    procedure EndSelectImages;
    function IsSelected(idx: integer): boolean;
    procedure DisplayImageAt(idx: Integer; x, y: Integer);

{!!
<FS>TImageEnMView.SelectedFontColor

<FM>Declaration<FC>
property SelectedFontColor: TColor; (Default: clNone)

<FM>Description<FN>
By default (when SelectedFontColor = clNone) the color of the text on both selected and unselected thumbnails is the same (as specified by <A TImageEnMView.ImageBottomText>, <A TImageEnMView.ImageInfoText> and <A TImageEnMView.ImageTopText>).

If you specify a color for SelectedFontColor it will be used for selected cells.

<FM>See Also<FN>
- <A TImageEnMView.ThumbnailsBackgroundSelected>
!!}
    property SelectedFontColor: TColor read fSelectedFontColor write fSelectedFontColor;

    // play
    property Playing: boolean read fPlaying write SetPlaying;

{!!
<FS>TImageEnMView.PlayLoop

<FM>Declaration<FC>
property PlayLoop: boolean;

<FM>Description<FN>
Set PlayLoop to True to continuously loop playback of animated GIF and AVI files (when <A TImageEnMView.Playing> is enabled).

!!}
    property PlayLoop: boolean read fPlayLoop write fPlayLoop;

    property VisibleFrame: integer read fFrame write SetVisibleFrame;
    //
    property TransitionRunning: boolean read GetTransitionRunning;

    // encapsulated components

{!!
<FS>TImageEnMView.MIO

<FM>Declaration<FC>
property MIO: <A TImageEnMIO>;

<FM>Description<FN>
The MIO property encapsulates the <A TImageEnMIO> component inside TImageEnMView (it is created automatically the first time that it is used).

<FM>Example<FC>
ImageEnMView1.MIO.LoadFromFile('C:\film.avi');

ImageEnMView1.MIO.Acquire;
!!}
    property MIO: TImageEnMIO read GetImageEnMIO;

{!!
<FS>TImageEnMView.Proc

<FM>Declaration<FC>
property Proc: <A TImageEnProc>;

<FM>Description<FN>
The Proc property encapsulates the <A TImageEnProc> component inside TImageEnMView (it is created automatically the first time that it is used).

<FM>Example<FC>
ImageEnMView1.Proc.Negative;  // reverse colors of the selected image
!!}
    property Proc: TImageEnProc read GetImageEnProc;

    // drag&drop
    procedure IEBeginDrag(Immediate: Boolean; Threshold: Integer=-1);
    procedure IEEndDrag;
    property SelectionWidthNoFocus: integer read fSelectionWidthNoFocus write SetSelectionWidthNoFocus;
    property SelectionAntialiased: Boolean read fSelectionAntialiased write SetSelectionAntialiased;

    // input&output
    procedure SaveSnapshot(Stream: TStream; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False); overload; virtual;
    procedure SaveSnapshot(FileName: WideString; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False); overload; virtual;
    procedure SaveSnapshotEx(Stream: TStream; SaveCache: Boolean; Compressed: Boolean; SaveParams: Boolean; GetExtraParams: TIEProcessStreamEvent);
    function LoadSnapshot(Stream: TStream): Boolean; overload; virtual;
    function LoadSnapshot(FileName: WideString): Boolean; overload; virtual;
    function LoadSnapshotEx(Stream: TStream; SetExtraParams: TIEProcessStreamEvent): Boolean;

    // animations
    property Animation: TIEAnimation read fAnimation write SetAnimation;

    function Seek(Destination: TIEIOSeekDestination) : integer;

    // Checkboxes
    property CheckboxPos : TIEMCheckboxPos read fCheckboxPos write SetCheckboxPos;
    function CheckedCount : Integer;
    property Checked[index: integer]: Boolean read GetChecked write SetChecked;
    procedure SetCheckboxParams(iHorzMargin, iVertMargin : Integer; CustomCheckedImage : TBitmap = nil; CustomUncheckedImage : TBitmap = nil);
    procedure CheckAll;
    procedure UncheckAll;

    // Gestures
{!!
<FS>TImageEnMView.Gestures

<FM>Declaration<FC>
property Gestures: <A TIEMViewerGestures>;

<FM>Description<FN>
TImageEnMView supports native Windows 7 and Windows 8 gestures, allowing pan and zoom.
ImageEn automatically handles the Windows gesture events, so adding gesture support to your applications requires only that you enable the relevant Gesture property.

<FM>Example<FC>
// enable Pan (scroll) and Zoom gestures
ImageEnMView1.Gestures.Pan.Enabled := True;
ImageEnMView1.Gestures.Zoom.Enabled := True;
!!}
    property Gestures: TIEMViewerGestures read fGestures;  // not: cannot stay in published section without an property editor

  published
    ///////////////////////
    // P U B L I S H E D
    property ScrollBars: TScrollStyle read fScrollBars write SetScrollBars default ssBoth;

    property StoreType: TIEStoreType read fStoreType write SetStoreType;       // NB: No default. Always store!

    property ThumbWidth: integer read GetThumbWidth write SetThumbWidth;       // NB: No default. Always store!
    property ThumbHeight: integer read GetThumbHeight write SetThumbHeight;    // NB: No default. Always store!
    property HorizBorder: integer read fHorizBorder write SetHorizBorder;      // NB: No default. Always store!
    property VertBorder: integer read fVertBorder write SetVertBorder;         // NB: No default. Always store!
    property BottomGap: integer read fBottomGap write SetBottomGap default 0;
    property UpperGap: integer read fUpperGap write SetUpperGap default 0;    
    property SideGap: integer read fSideGap write SetSideGap default 0;
    property TextMargin: integer read fTextMargin write SetTextMargin;

    property ThumbnailOptionsEx : TIEMThumbnailOptionsEx read fThumbnailOptionsEx write SetThumbnailOptionsEx default [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading];

{!!
<FS>TImageEnMView.OnViewChange

<FM>Declaration<FC>
property OnViewChange: <A TViewChangeEvent>;

<FM>Description<FN>
Occurs when the <A TImageEnMView.ViewX> or <A TImageEnMView.ViewY> properties change.
!!}
    property OnViewChange: TViewChangeEvent read fOnViewChange write fOnViewChange;


{!!
<FS>TImageEnMView.OnImageAtPos

<FM>Declaration<FC>
property OnImageAtPos: <A TIEImageAtPosEvent>;

<FM>Description<FN>
Occurs whenever it is necessary to check if specified coordinates are inside a thumbnail.

!!}
    property OnImageAtPos: TIEImageAtPosEvent read fOnImageAtPos write fOnImageAtPos;


{!!
<FS>TImageEnMView.OnCreateImage

<FM>Declaration<FC>
property OnCreateImage: <A TIECreateImageEvent>;

<FM>Description<FN>
Occurs whenever (immediately after) a new image is created (i.e whenever a new image is added to the TImageEnMView).
!!}
    property OnCreateImage: TIECreateImageEvent read fOnCreateImage write fOnCreateImage;


{!!
<FS>TImageEnMView.OnDestroyImage

<FM>Declaration<FC>
property OnDestroyImage: <A TIEDestroyImageEvent>;

<FM>Description<FN>
Occurs whenever (immediately before) an image is destroyed (i.e whenever an image is removed from the TImageEnMView).
!!}
    property OnDestroyImage: TIEDestroyImageEvent read fOnDestroyImage write fOnDestroyImage;


{!!
<FS>TImageEnMView.OnImageIDRequest

<FM>Declaration<FC>
property OnImageIDRequest: <A TIEImageIDRequestEvent>;

<FM>Description<FN>
Occurs whenever an image is required, if you have specified a value for its <A TImageEnMView.ImageID> property.

<FC>ID<FN> is the value you have specified in <A TImageEnMView.ImageID> property;
<FC>Bitmap<FN> is the image to display. It is a TBitmap (use <A TImageEnMView.OnImageIDRequestEx> if you require a <A TIEBitmap>). The bitmap is copied in TImageEnMView, and then automatically freed.

<FM>Example<FN>
procedure TMyForm.ImageEnMViewOnImageIDRequest(Sender: TObject; ID: integer; var Bitmap: TBitmap);
begin
  // Retrieve the image from a TImageList
  Bitmap := TBitmap.create;
  ImageList1.GetBitmap(ID, Bitmap);
end;

!!}
    property OnImageIDRequest: TIEImageIDRequestEvent read fOnImageIDRequest write fOnImageIDRequest;

{!!
<FS>TImageEnMView.OnImageIDRequestEx

<FM>Declaration<FC>
property OnImageIDRequestEx: <A TIEImageIDRequestExEvent>;

<FM>Description<FN>            
This event is called when an image is required, if you have inserted a value in the <A TImageEnMView.ImageID> property.   

<FC>ID<FN> is the value you have specified in <A TImageEnMView.ImageID> property;
<FC>Bitmap<FN> is the image to display. It is a <A TIEBitmap> (use <A TImageEnMView.OnImageIDRequest> if you require a TBitmap).  The bitmap is copied in TImageEnMView, and then automatically freed.

<FM>Example<FN>
procedure TMyForm.ImageEnMViewOnImageIDRequestEx(Sender: TObject; ID: integer; var Bitmap: TIEBitmap);
begin
  // Create the image dynamically
  Bitmap := TIEBitmap.create;
  GenerateChartOfID(ID, Bitmap);
end;

!!}
    property OnImageIDRequestEx: TIEImageIDRequestExEvent read fOnImageIDRequestEx write fOnImageIDRequestEx;

{!!
<FS>TImageEnMView.OnBeforeImageDraw

<FM>Declaration<FC>
property OnBeforeImageDraw: <A TIEImageDrawEvent>;

<FM>Description<FN>
Occurs immediately before an image is drawn. It can be used to prepare parameters prior to drawing the image (e.g. <A TImageEnMView.ImageTopText>, <A TImageEnMView.ImageBottomText> and <A TImageEnMView.ImageInfoText>).
!!}
    property OnBeforeImageDraw: TIEImageDrawEvent read fOnBeforeImageDraw write fOnBeforeImageDraw;

{!!
<FS>TImageEnMView.OnBeforeImageDrawEx

<FM>Declaration<FC>
property OnBeforeImageDrawEx: <A TIEImageDrawEventEx>;

<FM>Description<FN>

Occurs immediately before the image of index, <FC>idx<FN>, is painted.
<FC>Left<FN>, <FC>Top<FN> are the top-left coordinates where the image will be drawn (i.e. destination bitmap coordinates).
<FC>Dest<FN> is the destination bitmap (containing the canvas where the image must be drawn).
<FC>ThumbRect<FN> specifies the destination rectangle for the image. You can change this parameter to control where the image is drawn.

Note: When this event is assigned, the selection is not shown.

See the Multi\CustomThumbs demo for more info.
!!}
    property OnBeforeImageDrawEx: TIEImageDrawEventEx read fOnBeforeImageDrawEx write fOnBeforeImageDrawEx;

{!!
<FS>TImageEnMView.OnImageDraw

<FM>Declaration<FC>
property OnImageDraw: <A TIEImageDrawEvent>;

<FM>Description<FN>
Occurs whenever an image is painted.

<FM>Example<FC>
// Display the image index and sizes on bottom of the thumbnail
// Ensure you have set the BottomGap property
procedure TForm1.ImageEnMView1ImageDraw(Sender: TObject; idx: Integer; Left, Top: Integer; Canvas: TCanvas);
begin
  with canvas do
  begin
    Font.Height := 15;
    Font.Color := clWhite;
    TextOut(Left, Top + imageenmview1.ThumbHeight - imageenmview1.bottomgap + 2, IntToStr(idx));
    TextOut(Left, Top, IntToStr(imageenmview1.ImageWidth[idx]) + 'x' + IntToStr(imageenmview1.ImageHeight[idx]));
  end;
end;
!!}
    property OnImageDraw: TIEImageDrawEvent read fOnImageDraw write fOnImageDraw;

{!!
<FS>TImageEnMView.OnImageDraw2

<FM>Declaration<FC>
property OnImageDraw2: <A TIEImageDraw2Event>;

<FM>Description<FN>
Occurs whenever an image is painted. Same as <A TImageEnMView.OnImageDraw> but includes <FC>ImageRect<FN> to return the thumbnail rectangle.
!!}
    property OnImageDraw2: TIEImageDraw2Event read fOnImageDraw2 write fOnImageDraw2;

{!!
<FS>TImageEnMView.OnImageSelect

<FM>Declaration<FC>
property OnImageSelect: <A TIEImageSelectEvent>;

<FM>Description<FN>
Occurs whenever an image is selected.
!!}
    property OnImageSelect: TIEImageSelectEvent read fOnImageSelect write fOnImageSelect;

{!!
<FS>TImageEnMView.OnImageAdd

<FM>Declaration<FC>
property OnImageAdd: <A TIEImageAddEvent>;

<FM>Description<FN>
Occurs whenever an image is added to a <A TImageEnFolderMView> or when using <L TImageEnMView.FillFromDirectory>TImageEnMView.FillFromDirectory</L>.
!!}
    property OnImageAdd: TIEImageAddEvent read fOnImageAdd write fOnImageAdd;


{!!
<FS>TImageEnMView.OnGetText

<FM>Declaration<FC>
property OnGetText: <A TIEGetTextEvent>;

<FM>Description<FN>
Occurs before text is output when drawing a thumbnail allowing you to insert or modify the displayed text.

Note: Ensure that you have set <A TImageEnMView.UpperGap>/<A TImageEnMView.BottomGap> to allow space for the text


<FM>Example<FN>
// Note: In form create we set IEFolderMView.UpperGap := 20;

// Display the file index above the frame
procedure TForm1.IEFolderMViewGetText(Sender: TObject; Index: Integer; Position: TIEMTextPos; var Text: WideString);
begin
  if Position = iemtpTop then
    Text := 'File #' + IntToStr(Index + 1);
end;     

!!}
    property OnGetText: TIEGetTextEvent read fOnGetText write fOnGetText;



{!!
<FS>TImageEnMView.OnImageDeselect

<FM>Declaration<FC>
property OnImageDeselect: <A TIEImageSelectEvent>;

<FM>Description<FN>
Occurs whenever the user removes the selection from an image.

Note: <A TImageEnMView.EnableMultiSelect> must be true.
!!}
    property OnImageDeselect: TIEImageSelectEvent read fOnImageDeselect write fOnImageDeselect;


{!!
<FS>TImageEnMView.OnSelectionChanging

<FM>Declaration<FC>
property OnSelectionChanging: <A TIESelectionChangingEvent>;

<FM>Description<FN>
Occurs prior to a change in the selected frame/thumbnail due to user action (i.e. mouse or keyboard).

This can be used to prevent changing of a selection (e.g. due to the current selection not being saved).
!!}
    property OnSelectionChanging: TIESelectionChangingEvent read fOnSelectionChanging write fOnSelectionChanging;




{!!
<FS>TImageEnMView.OnIOProgress

<FM>Declaration<FC>
property OnIOProgress: <A TIEProgressEvent>;

<FM>Description<FN>
Occurs during input/output operations to advise the progress of the operation.
!!}
    property OnIOProgress: TIEProgressEvent read fOnIOProgress write fOnIOProgress;

{!!
<FS>TImageEnMView.OnDrawProgress

<FM>Declaration<FC>
property OnDrawProgress: <A TIEMProgressEvent>;

<FM>Description<FN>
Occurs during painting for each image drawn.

<FM>Example<FC>
procedure TForm1.ImageEnMView1DrawProgress(Sender: TObject; per, idx: Integer);
begin
  ProgressBar1.Position := per;
end;
!!}
    property OnDrawProgress: TIEMProgressEvent read fOnDrawProgress write fOnDrawProgress;

{!!
<FS>TImageEnMView.OnWrongImage

<FM>Declaration<FC>
property OnWrongImage: <A TIEWrongImageEvent>;

<FM>Description<FN>
Occurs whenever TImageEnMView cannot load the image specified by the <A TImageEnMView.ImageFileName> property, for instance when the file is corrupt or of an unrecognized format.

You can specify an alternative bitmap to display by changing the OutBitmap property.

<FM>Example<FC>
Procedure MyForm1OnWrongImage(Sender: TObject; OutBitmap: TIEBitmap; idx: Integer; var Handled: Boolean);
Var
   io: TImageEnIO;
begin
   io := TImageEnIO.CreateFromBitmap(OutBitmap);
   io.LoadFromFile('error_image.bmp');
   io.Free;
   Handled := True;
end;
!!}
    property OnWrongImage: TIEWrongImageEvent read fOnWrongImage write fOnWrongImage;

          
{!!
<FS>TImageEnMView.OnCheckboxClick

<FM>Declaration<FC>
property OnCheckboxClick: <A TIECheckboxClickEvent>;

<FM>Description<FN>
Occurs whenever a user clicks a checkbox.

<FC>idx<FN> is the index of the clicked image.
<FC>bChecked<FN> specifies the new status of the image. You can override it, e.g. set it to false if the image cannot be checked

Note: Don't read <A TImageEnMView.CheckedCount> in this event which will not yet be valid. Use OnClick or OnMouseUp.

<FM>Example<FC>
procedure TfMain.ImageEnMView1CheckboxClick(Sender: TObject; idx: integer; var bChecked : Boolean);
begin
  // Only allow JPEG images to be checked   
  if bChecked and (IEFileIsOfFormat(ImageEnMView1.ImageFilename[idx], ioJPEG) = False) then
  begin
    MessageBeep(MB_ICONEXCLAMATION);
    bChecked := False;
  end;
end;

<FM>See also<FN>
- <A TImageEnMView.Checkboxes>
- <A TImageEnMView.CheckedCount>
!!}
    property OnCheckboxClick: TIECheckboxClickEvent read fOnCheckboxClick write fOnCheckboxClick;

    property Checkboxes : TIEMCheckboxType read fCheckboxes write SetCheckboxes default iecbNone;

    property VisibleSelection: boolean read fVisibleSelection write SetVisibleSelection default true;
    property MouseInteract: TIEMMouseInteract read GetMouseInteract write SetMouseInteract default [mmiSelect];
    property KeyInteract: TIEMKeyInteract read GetKeyInteract write SetKeyInteract default [mkiMoveSelected];
    property DisplayMode: TIEMDisplayMode read fDisplayMode write SetDisplayMode default mdGrid;
    property GridWidth: integer read fGridWidth write SetGridWidth;    // NB: No default. Always store!
    property SelectionWidth: integer read fSelectionWidth write SetSelectionWidth default 2;
    property SelectionColor: TColor read fSelectionColor write SetSelectionColor default $00FFA0A0;
    property RemoveCorrupted: boolean read fRemoveCorrupted write SetRemoveCorrupted default false;
    property DrawImageBackground: boolean read fDrawImageBackground write SetDrawImageBackground default false;
    property ImageCacheSize: integer read fImageCacheSize write SetImageCacheSize default 10;
{!!
<FS>TImageEnMView.TransitionEffect

<FM>Declaration<FC>
property TransitionEffect: <A TIETransitionType>;

<FM>Description<FN>
Specifies the effect to apply when the application changes the currently displayed frame.

Note: <A TImageEnMView.DisplayMode> must be <FC>mdSingle<FN>. To change current frame, use the <A TImageEnMView.VisibleFrame> property.

<FM>Example<FC>
// Design time properties...
ImageEnMView1.DisplayMode := mdSingle;
ImageEnMView1.TransitionEffect := iettCrossDissolve;
ImageEnMView1.TransitionDuration := 1500;

// Display next frame using cross dissolve effect
ImageEnMView1.VisibleFrame := ImageEnMView1.VisibleFrame + 1;       

<FM>See Also<FN>
- <A TImageEnMView.TransitionDuration>
!!}
    property TransitionEffect: TIETransitionType read fTransitionEffect write fTransitionEffect default iettNone;

{!!
<FS>TImageEnMView.TransitionDuration

<FM>Declaration<FC>
property TransitionDuration: Integer;

<FM>Description<FN>
Specifies the duration of the transition in milliseconds.

<FM>Example<FC>
// Design time properties...
ImageEnMView1.DisplayMode := mdSingle;
ImageEnMView1.TransitionEffect := iettCrossDissolve;
ImageEnMView1.TransitionDuration := 1500;

// Display next frame using cross dissolve effect
ImageEnMView1.VisibleFrame := ImageEnMView1.VisibleFrame + 1;

<FM>See Also<FN>
- <A TImageEnMView.TransitionEffect>
!!}
    property TransitionDuration: integer read fTransitionDuration write fTransitionDuration default 1000;

    property Style: TIEMStyle read fStyle write SetStyle; // NB: No default. Always store!

{!!
<FS>TImageEnMView.ThumbnailsBackground

<FM>Declaration<FC>
property ThumbnailsBackground: TColor;

<FM>Description<FN>
Specifies the background color of the thumbnails when they are NOT selected. Use <A TImageEnMView.ThumbnailsBackgroundSelected> to set the color when selected.

Note: Not used if <A TImageEnMView.DrawImageBackground> is enabled
!!}
    property ThumbnailsBackground: TColor read fThumbnailsBackground write fThumbnailsBackground;  // NB: No default. Always store!
                                  
{!!
<FS>TImageEnMView.ThumbnailsBackgroundSelected

<FM>Declaration<FC>
property ThumbnailsBackgroundSelected: TColor;

<FM>Description<FN>
Specifies the background color of the thumbnails when they are selected. Use <A TImageEnMView.ThumbnailsBackground> to set the color when NOT selected.

Note: Not used if <A TImageEnMView.DrawImageBackground> is enabled
!!}
    property ThumbnailsBackgroundSelected: TColor read fThumbnailsBackgroundSelected write fThumbnailsBackgroundSelected;  // NB: No default. Always store!



    property EnableMultiSelect: boolean read fEnableMultiSelect write SetEnableMultiSelect default false;

{!!
<FS>TImageEnMView.MultiSelectionOptions

<FM>Declaration<FC>
property MultiSelectionOptions: <A TIEMultiSelectionOptions>;

<FM>Description<FN>
Controls the behaviour of selection.

<FM>Example<FC>
// If you do not specify iemoRegion the entire row is selected:
ImageEnMView1.MultiSelectionOptions := [];
<IMG help_images\64.bmp>

// By specifying iemoRegion only the specified columns are selected:
ImageEnMView1.MultiSelectionOptions := [iemoRegion];
<IMG help_images\65.bmp>
!!}
    property MultiSelectionOptions: TIEMultiSelectionOptions read fMultiSelectionOptions write fMultiSelectionOptions;  // NB: No default

    property ThumbnailsBorderWidth: integer read fThumbnailsBorderWidth write SetThumbnailsBorderWidth;  // NB: No default
    property ThumbnailsBorderColor: TColor read fThumbnailsBorderColor write SetThumbnailsBorderColor default clBtnFace;
    property ThumbnailsInternalBorder: boolean read fThumbnailsInternalBorder write SetThumbnailsInternalBorder default false;
    property ThumbnailsInternalBorderColor: TColor read fThumbnailsInternalBorderColor write SetThumbnailsInternalBorderColor default clBlack;
    property WallPaper: TPicture read fWallPaper write SetWallPaper;
    property WallPaperStyle: TIEWallPaperStyle read fWallPaperStyle write SetWallPaperStyle default iewoNormal;
    property OnProgress: TIEProgressEvent read GetOnProgress write SetOnProgress;

{!!
<FS>TImageEnMView.ThreadPoolSize

<FM>Declaration<FC>
property ThreadPoolSize: Integer;

<FM>Description<FN>
Specifies how many threads can be created to load images (to load images in the background). If ThreadPoolSize is set to 0 all images are loaded in the main thread.
!!}
    property ThreadPoolSize: integer read fThreadPoolSize write fThreadPoolSize default 5;

    property EnableAlphaChannel: boolean read fEnableAlphaChannel write SetEnableAlphaChannel default true;
    property BackgroundStyle: TIEBackgroundStyle read fBackgroundStyle write SetBackgroundStyle default iebsSolid;
    property ThumbnailsBackgroundStyle: TIEBackgroundStyle read fThumbnailsBackgroundStyle write SetThumbnailsBackgroundStyle default iebsSolid;

    property OnAcquireBitmap: TIEAcquireBitmapEvent read GetOnAcquireBitmap write SetOnAcquireBitmap;

{!!
<FS>TImageEnMView.OnAllDisplayed

<FM>Declaration<FC>
property OnAllDisplayed: TNotifyEvent;

<FM>Description<FN>
Occurs when all images have been loaded and displayed.
!!}
    property OnAllDisplayed: TNotifyEvent read fOnAllDisplayed write fOnAllDisplayed;

    property OnFinishWork: TNotifyEvent read GetOnFinishWork write SetOnFinishWork;



{!!
<FS>TImageEnMView.OnPlayFrame

<FM>Declaration<FC>
property OnPlayFrame: <A TIEPlayFrameEvent>;

<FM>Description<FN>
Occurs whenever a frame is displayed. You can set <FC>bShowFrame<FN> to False to skip playback of specific frames.

See also: <A TImageEnMView.Playing>.
   
<FM>Example<FC>
// We have two TImageEnMViews showing the same set of images: iemFileList shows thumbnails of all images. iemDisplay shows a slideshow. A button displays a slideshow of all checked images in iemFileList
procedure TMainForm.iemDisplayPlayFrame(Sender: TObject; FrameIndex: integer; var bShowFrame: Boolean);
begin
  if iemFileList.Checked[FrameIndex] = False then
    bShowFrame := False;
end;
!!}
    property OnPlayFrame: TIEPlayFrameEvent read fOnPlayFrame write fOnPlayFrame;

{!!
<FS>TImageEnMView.OnAnimationText

<FM>Declaration<FC>
property OnAnimationText: <A TIEAnimationTextEvent>;

<FM>Description<FN>
Occurs whenever text needs to be displayed during an animation.

<FM>See Also<FN>
- <A TImageEnMView.Animation>
!!}
    property OnAnimationText: TIEAnimationTextEvent read fOnAnimationText write fOnAnimationText;

{!!
<FS>TImageEnMView.OnAfterEvent

<FM>Declaration<FC>
property OnAfterEvent: <A TIEAfterEventEvent>;

<FM>Description<FN>
Occurs immediately after ImageEn has processed an event.

See <A TIEAfterEvent> for a list of handled events.
!!}
    property OnAfterEvent: TIEAfterEventEvent read fOnAfterEvent write fOnAfterEvent;

{!!
<FS>TImageEnMView.OnImageEnGesture

<FM>Declaration<FC>
property OnImageEnGesture: <A TIEImageEnGestureEvent>;

<FM>Description<FN>
Occurs whenever a gesture is handled by ImageEn.
!!}
    property OnImageEnGesture: TIEImageEnGestureEvent read fOnImageEnGesture write fOnImageEnGesture;

    property ImageEnVersion: String read GetImageEnVersion write SetImageEnVersion stored false;
    {$ifdef IEINCLUDEFLATSB}
    property FlatScrollBars: Boolean read fFlatScrollBars write SetFlatScrollBars default False;
    {$endif}
{$IFDEF IESUPPORTANCHORS}
    property Anchors;
{$ENDIF}
{$IFDEF IEMOUSEWHEELEVENTS}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$ENDIF}
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnContextPopup;
    {$ifdef IEHASONGESTURE}
    property OnGesture;
    {$endif}
  end;


const
  IEM_Path_Index_Delimiter   = '::'; // Delimits a filename from its image index. See ImageFilename[]

  // Possible consts for captions that are replaced at paint time
  IEM_Filename               = '$IEM_FILENAME$';
  IEM_FilenameNoExt          = '$IEM_FILENAMENOEXT$';
  IEM_FilePath               = '$IEM_FILEPATH$';
  IEM_ImageDimensions        = '$IEM_IMAGE_DIMENSIONS$';
  IEM_ImageDimAndSize        = '$IEM_IMAGE_DIM_AND_SIZE$';
  IEM_FileSize               = '$IEM_FILESIZE$';
  IEM_FileCreateDate         = '$IEM_FILECREATEDATE$';
  IEM_FileCreateDateTime     = '$IEM_FILECREATEDATETIME$';
  IEM_FileCreateDateAndSize  = '$IEM_FILECREATEDATE_AND_SIZE$';
  IEM_FileEditDate           = '$IEM_FILEEDITDATE$';
  IEM_FileEditDateTime       = '$IEM_FILEEDITDATETIME$';
  IEM_FileEditDateAndSize    = '$IEM_FILEEDITDATE_AND_SIZE$';
  IEM_FileType               = '$IEM_FILETYPE$';

  Max_Icon_Images_To_Cache = 50;    // How many icons should be cached in memory?

  Thumbnail_Background_When_Disabled = cl3DLight;
  Thumbnail_Border_When_Disabled     = clBtnShadow;
  Text_Color_When_Disabled           = clGrayText;


implementation

uses
  math, 
{$IFDEF Delphi7orNewer}  // From Delphi 7
  uxTheme, { For checkbox drawing }
{$EndIf}
{$ifdef IEINCLUDEFLATSB}
  flatsb,
{$endif}
{$IFDEF THEMED_BORDERS}
  Themes,
{$ENDIF}
{$ifdef IEUSEVCLZLIB}zlib, {$else}iezlib, {$endif}
{$ifdef IEVISION}
  ievision,
{$endif}
{$IFDEF VIDEO_THUMBNAILS}
  iexShellThumbnails,
{$ENDIF}
  iegdiplus, bmpfilt, iesettings, ShellApi;

{$R-}


{$IFDEF VIDEO_THUMBNAILS}
function OleInitialize(pwReserved: Pointer): HResult; stdcall; external 'ole32.dll' name 'OleInitialize';
procedure OleUninitialize; stdcall; external 'ole32.dll' name 'OleUninitialize';
{$ENDIF}


constructor TImageEnMView.Create(Owner: TComponent);
begin
  InitializeCriticalSection(fThreadCS);
  fBackBuffer := TIEBitmap.Create;
  fBackBuffer.Location := ieTBitmap;
  fImageEnMIO := nil;
  fImageEnProc := nil;
  inherited Create(Owner);

  {$ifdef IEVISION}
  if (csDesigning in ComponentState) then
  begin
    // design mode, unload ievision
    IEFinalize_ievision();
  end;
  {$endif}

  IEGDIPLoadLibrary();
  fUpdating := false;
  fMultiSelectedImages := TList.Create;
  fDestroying := false;
  fScrollBarsAlwaysVisible := false;
  fRXScroll := 1;
  fRYScroll := 1;
  fHoverCheckLastIdx := -1;
  fHoverCheckLastPos.X := -1;
  fScrollBars := ssBoth;
  Height := 90;
  Width := 180;
  fImageInfo := TList.Create;
  fStoreType := ietNormal;
  fThumbWidth := 100;
  fThumbHeight := 100;
  fZoom := 100.0;
  fHorizBorder := 4;
  fVertBorder := 4;
  fVWidth := 0;
  fVHeight := 0;
  fOnViewChange := nil;
  fOnImageIDRequest := nil;
  fOnImageIDRequestEx := nil;
  fOnImageDraw := nil;
  fOnImageDraw2 := nil;
  fOnImageSelect := nil;
  fOnImageAdd := nil;
  fOnImageDeselect := nil;
  fOnSelectionChanging := nil;
  fOnBeforeImageDrawEx := nil;
  fOnDrawProgress := nil;
  fOnWrongImage := nil;
  fOnCreateImage := nil;
  fOnDestroyImage := nil;
  fOnCheckboxClick := nil;
  fOnImageEnGesture := nil;
  fHDrawDib := IEDrawDibOpen;
  fImageEnIO := TImageEnIO.Create(self);
  fSelectedItem := -1;
  fVisibleSelection := true;
  fSelectionWidth := 2;
  fSelectionWidthNoFocus := 1;
  fSelectionAntialiased := true;
  fSelectionColor := $00FFA0A0;
  fBottomGap := 0;
  fUpperGap := 0;
  fSideGap := 0;
  fTextMargin := 0;
  fModernStyling := False;
  fMouseInteract := [mmiSelect];
  fKeyInteract := [mkiMoveSelected];
  fDisplayMode := mdGrid;
  fGridWidth := 0;
  fCurrentGridWidth := 0;
  fPlayTimer := 0;
  fPlayLoop := true;
  fLastImOp := 0;
  fLastImIdx := 0;
  fLastImP1 := 0;
  fTimerInProgress := false;
  fFrame := 0;
  fLockPaint := 0;
  fLockUpdate := 0;
  fOnIOProgress := nil;
  fOnImageAtPos := nil;
  fRemoveCorrupted := false;
  fDrawImageBackground := false;
  fThumbnailResampleFilter := rfFastLinear;
  fThumbnailDisplayFilter := rfNone;
  fThumbnailClipping := 0;
  fVScrollBarParams := TIEScrollBarParams.Create;
  fHScrollBarParams := TIEScrollBarParams.Create;
  fMouseWheelParams := TIEMouseWheelParams.Create(iemwVScroll);
  fImageCacheSize := 10;
  fImageCacheUseDisk := not (csDesigning in ComponentState);
  fImageList := TIEVirtualImageList.Create('ILIST', fImageCacheUseDisk);
  fImageList.MaxImagesInMemory := fImageCacheSize;
  fCacheList := TIEVirtualImageList.Create('ICACHE', fImageCacheUseDisk);
  fIconList := TIECachedIconList.create(Self, Max_Icon_Images_To_Cache);
  fTransition := TIETransitionEffects.Create(self);
  fTransitionEffect := iettNone;
  fTransitionDuration := 1000;
  fStyle := iemsACD;
  fDoubleClicking := false;
  fThumbnailsBackground := clBtnFace;
  fThumbnailsBackgroundSelected := clBtnFace;
  fMultiSelecting := false;
  fEnableMultiSelect := false;
  fSelectInclusive := false;
  fMultiSelectionOptions := [];
  fThumbnailsBorderWidth := 0;
  fThumbnailsBorderColor := clBtnFace;
  fThumbnailsInternalBorderColor := clBlack;
  fWallPaper := TPicture.Create;
  fWallPaperStyle := iewoNormal;
  fEnableResamplingOnMinor := true;
  fIconSize := ieicStretchHD;
  fOnProgress := nil;
  fOnBeforeImageDraw := nil;
  fThreadPoolSize := 5;
  fThreadPoolIO := TList.Create;
  fThreadRequests := TList.Create;
  fLookAheadList := TList.Create;
  fThreadStarter := TIEStarter.Create;
  fThreadStarter.mview := self;
  fSelectImages := false;
  fEnableImageCaching := true;
  fHaveMultiselected := false;
  fSoftShadow := TIEVSoftShadow.Create;
  fSoftShadow.Enabled := false;
  fSoftShadow.Radius := 3;
  fSoftShadow.OffsetX := 3;
  fSoftShadow.OffsetY := 3;
  fSoftShadow.Intensity := 100;
  fSoftShadow.ShadowColor := CreateRGB(0, 0, 0);
  fEnableAlphaChannel := true;
  fBackgroundStyle := iebsSolid;
  fThumbnailsBackgroundStyle := iebsSolid;
  fChessboardSize := 16;
  fChessboardBrushStyle := bsSolid;
  fGradientEndColor := clBlue;
  fFillThumbnail := true;
  fMDown := false;
  fShowText := true;
  fSetUpperGap := 0;
  fSetBottomGap := 0;
  fCurrentCompare := nil;
  fCurrentCompareEx := nil;
  fChangedSel := false;
  fThumbsRounded := 0;
  fFlatScrollBars := false;
  fThumbnailFrame := nil;
  fThumbnailFrameSelected := nil;
  fThumbnailFrameRect := Rect(0, 0, 0, 0);
  fDragging := false;
  fEnableAdjustOrientation := false;
  fMultiOnDemands := TList.Create;
  fMaintainInvisibleImages := 15;
  fLookAhead := 0;
  fOnAllDisplayed := nil;
  fAllDisplayed := false;
  fUserAction := false;
  fEnableLoadEXIFThumbnails := true;
  fOnFinishWork := nil;
  fOnAcquireBitmap := nil;
  fSelectedFontColor := Graphics.clNone;
  fOnPlayFrame := nil;
  fOnAnimationText := nil;
  fOnAfterEvent := nil;
  fDefaultBottomTextFont := TFont.Create;
  fDefaultTopTextFont := TFont.Create;
  fDefaultInfoTextFont := TFont.Create;
  fDefaultTextBackgroundStyle := bsSolid;
  fAnimation := nil;
  fAnimationDraggingSlider := false;
  fAnimationTimer := TTimer.Create(self);
  fLastMouseMoveX := -1;
  fLastMouseMoveY := -1;
  fTrackMouseSelection := false;
  fDrawMouseSelection := false;
  fCheckThumbBoundsOnSelect := false;
  fSelectedImageAlwaysVisible := True;
  fThumbnailOptionsEx := [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading];
  fCheckboxes := iecbNone;
  fCheckedCount := -1;
  fCheckedBitmap := nil;
  fUncheckedBitmap := nil;
  fCheckboxPos := iecpTopLeft;
  fCheckboxMargins := Point(4, 4);
  fGestures := TIEMViewerGestures.Create();
end;

procedure TImageEnMView.ClearThreadsAndRequests;
var
  i: Integer;
begin
  EnterCriticalSection(fThreadCS);
  try
    fLookAheadList.Clear;
    fThreadRequests.Clear;
    for i := 0 to fThreadPoolIO.Count - 1 do
      if TImageEnIO(fThreadPoolIO[i]).Tag > -1 then
      begin
        TImageEnIO(fThreadPoolIO[i]).Tag := -2;
        TImageEnIO(fThreadPoolIO[i]).Aborting := true;
      end;
  finally
    LeaveCriticalSection(fThreadCS);
  end;
  // wait up to 1 second
  for i := 1 to 10 do
    if fThreadPoolIO.Count > 0 then
      sleep(100);
end;

procedure TImageEnMView.AbortImageLoading(idx: Integer);
var
  i: Integer;
begin
  if assigned(fThreadRequests) and assigned(fLookAheadList) and assigned(fThreadPoolIO) then
  begin
    EnterCriticalSection(fThreadCS);
    try
      fThreadRequests.Remove(pointer(idx));
      fLookAheadList.Remove(pointer(idx));
      for i := 0 to fThreadPoolIO.Count - 1 do
        if TImageEnIO(fThreadPoolIO[i]).Tag = idx then
        begin
          TImageEnIO(fThreadPoolIO[i]).Tag := -2;
          TImageEnIO(fThreadPoolIO[i]).Aborting := true;
          break;
        end;
    finally
      LeaveCriticalSection(fThreadCS);
    end;
  end;
end;

// note: the timer object is not destroyed because the win32 should destroy it.
destructor TImageEnMView.Destroy;
var
  toDestroy: TIEBitmap;
begin
  fDestroying := true;
  DeselectNU;
  // threads
  ClearThreadsAndRequests;
  while fThreadPoolIO.Count > 0 do
  begin
    toDestroy := TImageEnIO(fThreadPoolIO[0]).IEBitmap;
    TImageEnIO(fThreadPoolIO[0]).free;
    fThreadPoolIO[0] := nil;
    FreeAndNil(toDestroy);
    fThreadPoolIO.Delete(0);
  end;

  fthreadstarter.Terminate;
  Windows.SetEvent(fThreadStarter.resumeEvent);

  FreeAndNil(fThreadStarter);
  FreeAndNil(fThreadRequests);
  FreeAndNil(fThreadPoolIO);
  //
  ClearOnDemandIOList;
  FreeAndNil(fMultiOnDemands);
  //
  if assigned(fImageEnMIO) then
    FreeAndNil(fImageEnMIO);
  if assigned(fImageEnProc) then
    FreeAndNil(fImageEnProc);
  // remove all objects
  DeleteAllImages();
  //
  FreeAndNil(fWallPaper);
  FreeAndNil(fTransition);
  Deselect;
  FreeAndNil(fImageList);
  FreeAndNil(fCacheList);
  FreeAndNil(fIconList);
  FreeAndNil(fImageInfo);
  IEDrawDibClose(fHDrawDib);
  FreeAndNil(fImageEnIO);
  FreeAndNil(fMultiSelectedImages);
  if assigned(fThumbnailFrame) then
    FreeAndNil(fThumbnailFrame);
  if assigned(fThumbnailFrameSelected) then
    FreeAndNil(fThumbnailFrameSelected);
  //
  if assigned(FDragScrollTimer) then
    FreeAndNil(FDragScrollTimer);

  FreeAndNil(fVScrollBarParams);
  FreeAndNil(fHScrollBarParams);
  FreeAndNil(fMouseWheelParams);
  FreeAndNil(fSoftShadow);
  FreeAndNil(fLookAheadlist);
  FreeAndNil(fDefaultBottomTextFont);
  FreeAndNil(fDefaultTopTextFont);
  FreeAndNil(fDefaultInfoTextFont);

  FreeAndNil(fCheckedBitmap);
  FreeAndNil(fUncheckedBitmap);

  FreeAndNil(fGestures);

  FreeAndNil(fAnimationTimer);
  if assigned(fAnimation) then
    FreeAndNil(fAnimation);

  FreeAndNil(fBackBuffer);
  DeleteCriticalSection(fThreadCS);

  IEGDIPUnLoadLibrary();

  inherited;
end;

{!!
<FS>TImageEnMView.ScrollBars

<FM>Declaration<FC>
property ScrollBars: TScrollType;

<FM>Description<FN>
Specifies whether the TImageEnView control displays scroll bars.

Note: Scrollbars are never displayed if they are not required (unless <A TImageEnMView.ScrollBarsAlwaysVisible> is set to true)

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>ssNone<FN></C>No scroll bars are shown</C> </R>
<R> <C><FC>ssHorizontal<FN></C> <C>A single scroll bar is shown along the bottom edge when needed</C> </R>
<R> <C><FC>ssVertical<FN></C> <C>A single scroll bar is shown along the right edge when needed</C> </R>
<R> <C><FC>ssBoth<FN></C> <C>Scroll bars are shown on both the bottom and right edges when needed</C> </R>
</TABLE>
!!}
procedure TImageEnMView.SetScrollBars(v: TScrollStyle);
begin
  fScrollBars := v;
  if ((GetParentForm(self) = nil) and (ParentWindow = 0)) or (not HandleAllocated) then
    exit;
  if (fScrollBars <> ssVertical) and (fScrollBars <> ssBoth) then
    IEShowScrollBar(handle, SB_VERT, false, fFlatScrollBars);
  if (fScrollBars <> ssHorizontal) and (fScrollBars <> ssBoth) then
    IEShowScrollBar(handle, SB_HORZ, false, fFlatScrollBars);
  UpdateEx(false);
end;



{!!
<FS>TImageEnMView.StoreType

<FM>Declaration<FC>
property StoreType: <A TIEStoreType>;

<FM>Description<FN>
Specifies how image used by the TImageEnMView component are stored in memory (as full size images or thumbnails at their display size)
!!}
procedure TImageEnMView.SetStoreType(v : TIEStoreType);
begin
  fStoreType := v;
  if fStoreType = ietFastThumb then
  begin
    fEnableImageCaching := True;
    if fThumbnailDisplayFilter = rfNone then
      fThumbnailDisplayFilter := rfFastLinear;
  end;
end;



procedure TImageEnMView.WMSize(var Message: TWMSize);
begin
  inherited;

  // Reset grid size if we have zoomed
  fCurrentGridWidth := fGridWidth;
  if fZoom <> 100 then
    fCurrentGridWidth := CalcGridWidth();

  UpdateEx(false);
end;


procedure TImageEnMView.IEMAutoScroll(var Message: TMessage);
var
  iX, iY: integer;
begin
  try
    if HiByte(GetAsyncKeyState(VK_LBUTTON)) <> 0 then
    begin
      // Mouse still down
      iX := ScreenToClient(Mouse.CursorPos).X;
      iY := ScreenToClient(Mouse.CursorPos).Y;
      MouseMove([], iX, iY);
    end;
  except
    // Unexpected mouse error
  end;
end;

procedure TImageEnMView.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TImageEnMView.WMMouseWheel(var Message: TMessage);
var
  zDelta, nPos: integer;
  dir: integer;
begin
  inherited;
  if fMouseWheelParams.Action = iemwNone then
    exit;

  fUserAction := true;
  try
    zDelta := smallint($FFFF and (Message.wParam shr 16));
    if zDelta > 0 then
      dir := -1
    else
      dir := 1;
    if fMouseWheelParams.InvertDirection then
      dir := -1 * dir;

    case fMouseWheelParams.Action of
      iemwVScroll:
        begin
          case fMouseWheelParams.Variation of
            iemwAbsolute: // pixel based
              if fCurrentGridWidth = 0 then
              begin
                // horizontal
                nPos := fViewX + dir * fMouseWheelParams.Value;
                SetViewX(nPos);
              end
              else
              begin
                // vertical
                nPos := fViewY + dir * fMouseWheelParams.Value;
                SetViewY(nPos);
              end;
            iemwPercentage: // thumbnail based

              // NOTE: DEFAULT VALUE OF fMouseWheelParams is Value=8/Variation=iemwPercentage
              // For compatibility with TImageEnView and previous versions of TImageEnMView
              // We will work on a theoretical grid of 12.5 thumbnails high, so "8% of window"
              // will always equate to one thumbnail height regardless of thumbnail size

              if fCurrentGridWidth = 0 then
              begin
                // Horizontal scroll
                nPos := fViewX + dir * Trunc((ThumbWidth + fHorizBorder) / 8 * fMouseWheelParams.Value);
                SetViewX(nPos);
              end
              else
              begin
                // Vertical scroll
                nPos := fViewY + dir * Trunc((ThumbHeight + fVertBorder) / 8 * fMouseWheelParams.Value);
                SetViewY(nPos);
              end;
          end;
          ViewChange(0);
        end;
      iemwZoom:
        begin
          case fMouseWheelParams.Variation of
            iemwAbsolute:
              Zoom := fZoom + dir * fMouseWheelParams.Value;
            iemwPercentage:
              Zoom := fZoom + imax(round(fZoom * fMouseWheelParams.Value / 100), 1) * dir;
          end;
        end;
      iemwNavigate:
        begin
          if dir < 0 then
            Seek(ieioSeekPrior)
          else
            Seek(ieioSeekNext);
        end;
    end;

  finally // user action in ViewChange could raise exceptions
    fUserAction := false;
  end;
end;

procedure TImageEnMView.WMVScroll(var Message: TMessage);
var
  nPos: integer;
  mx, my: integer;
begin
  inherited;
  fUserAction := true;
  try
    case Message.WParamLo of
      SB_THUMBPOSITION, SB_THUMBTRACK:
        begin
          if (not fVScrollBarParams.Tracking) and (Message.WParamLo = SB_THUMBTRACK) then
            exit;
          nPos := trunc(Message.WParamHi * fRYScroll);
        end;
      SB_BOTTOM:
        begin
          GetMaxViewXY(mx, my);
          nPos := my;
        end;
      SB_TOP:
        nPos := 0;
      SB_LINEDOWN:
        if fVScrollBarParams.LineStep = -1 then
          nPos := fViewY + ThumbHeight + fVertBorder
        else
          nPos := fViewY + fVScrollBarParams.LineStep;
      SB_LINEUP:
        if fVScrollBarParams.LineStep = -1 then
          nPos := fViewY - ThumbHeight - fVertBorder
        else
          nPos := fViewY - fVScrollBarParams.LineStep;
      SB_PAGEDOWN:
        if fVScrollBarParams.PageStep = -1 then
          nPos := fViewY + ClientHeight
        else
          nPos := fViewY + fVScrollBarParams.PageStep;
      SB_PAGEUP:
        if fVScrollBarParams.PageStep = -1 then
          nPos := fViewY - ClientHeight
        else
          nPos := fViewY - fVScrollBarParams.PageStep;
    else
      nPos := fViewY;
    end;
    SetViewY(nPos);
    ViewChange(0);
  finally
    fUserAction := false;
  end;
end;

procedure TImageEnMView.WMHScroll(var Message: TMessage);
var
  nPos: integer;
  mx, my: integer;
begin
  inherited;
  fUserAction := true;
  try
    case Message.WParamLo of
      SB_THUMBPOSITION, SB_THUMBTRACK:
        begin
          if (not fHScrollBarParams.Tracking) and (Message.WParamLo = SB_THUMBTRACK) then
            exit;
          nPos := trunc(Message.WParamHi * fRXScroll);
        end;
      SB_BOTTOM:
        begin
          GetMaxViewXY(mx, my);
          nPos := mx;
        end;
      SB_TOP:
        nPos := 0;
      SB_LINEDOWN:
        if fHScrollBarParams.LineStep = -1 then
          nPos := fViewX + ThumbWidth + fHorizBorder
        else
          nPos := fViewX + fVScrollBarParams.LineStep;
      SB_LINEUP:
        if fVScrollBarParams.LineStep = -1 then
          nPos := fViewX - ThumbWidth - fHorizBorder
        else
          nPos := fViewX - fVScrollBarParams.LineStep;
      SB_PAGEDOWN:
        if fHScrollBarParams.PageStep = -1 then
          nPos := fViewX + ClientWidth
        else
          nPos := fViewX + fVScrollBarParams.PageStep;
      SB_PAGEUP:
        if fVScrollBarParams.PageStep = -1 then
          nPos := fViewX - ClientWidth
        else
          nPos := fViewX - fVScrollBarParams.PageStep;
    else
      nPos := fViewX;
    end;
    SetViewX(nPos);
    ViewChange(0);
  finally
    fUserAction := false;
  end;
end;

Procedure _SetLoadParams(MView : TImageEnMView; Params : TIOParamsVals);
const
  SMALLEST_RAW_IMAGE_WIDTH  = 1800;
  SMALLEST_RAW_IMAGE_HEIGHT = 1400;
  Quality_Scaler            = 3;     // Improve quality of thumbnails when resampled
var
  iMaxX, iMaxY : Integer;
begin
  if mview.fStoreType in [ietThumb, ietFastThumb] then
  begin
    Params.JPEG_Scale := IOJPEG_AUTOCALC;
    Params.JPEG_GetExifThumbnail := (mview.ThumbWidth <= 200) and (mview.ThumbHeight <= 200) and mview.fEnableLoadEXIFThumbnails;
    Params.JPEG_DCTMethod := ioJPEG_IFAST;

    iMaxX := MulDiv(mview.ThumbWidth, 100 + mView.ThumbnailClipping, 100);
    iMaxY := MulDiv(mview.ThumbHeight, 100 + mView.ThumbnailClipping, 100);

    if mview.fStoreType = ietFastThumb then
    begin
      iMaxX := iMaxX * Quality_Scaler;
      iMaxY := iMaxY * Quality_Scaler;
    end;

    Params.Width  := iMaxX;
    Params.Height := iMaxY;
    {$ifdef IEINCLUDERAWFORMATS}
    Params.RAW_GetExifThumbnail := (mview.ThumbWidth <= 200) and (mview.ThumbHeight <= 200) and mview.fEnableLoadEXIFThumbnails;
    Params.RAW_HalfSize := (mview.ThumbWidth <= SMALLEST_RAW_IMAGE_WIDTH) and (mview.ThumbHeight <= SMALLEST_RAW_IMAGE_HEIGHT);
    {$endif}
  end
  else
    Params.JPEG_Scale := IOJPEG_FULLSIZE;
  Params.EnableAdjustOrientation := mview.fEnableAdjustOrientation;
end;

// double click
procedure TImageEnMView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  fDoubleClicking := true;
  inherited;
end;

procedure TImageEnMView.Assign(Source: TPersistent);
begin
  //
end;

procedure TImageEnMView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure TImageEnMView.GetMaxViewXY(var mx, my: integer);
begin
  mx := imax(fVWidth - ClientWidth, 0);
  my := imax(fVHeight - ClientHeight, 0);
end;

{!!
<FS>TImageEnMView.ViewX

<FM>Declaration<FC>
property ViewX: integer;

<FM>Description<FN>
ViewX is the first column visible at the top-left corner of the component (for example, if the user has scrolled horizontally by two columns then it will return 2). It is only relevant when <A TImageEnMView.GridWidth> has a value greater than zero (otherwise it always returns 0).

You can set ViewX to simulate horizontal scrollbar movement.

See also: <A TImageEnMView.ViewY>.
!!}
procedure TImageEnMView.SetViewX(v: integer);
var
  max_x, max_y: integer;
begin
  if v = fViewX then
    exit;
  GetMaxViewXY(max_x, max_y);
  fViewX := ilimit(v, 0, max_x);
  invalidate;
  if (fScrollBars = ssHorizontal) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_HORZ, trunc(fViewX / fRXScroll), true, fFlatScrollBars);
end;

{!!
<FS>TImageEnMView.ViewY

<FM>Declaration<FC>
property ViewY: integer;

<FM>Description<FN>
ViewY is the first row visible at the top-left corner of the component (for example, if the user has scrolled vertically by two columns then it will return 2). It is not relevant if <A TImageEnMView.GridWidth> = 0 (in which case it always returns 0).

You can set ViewY to simulate vertical scrollbar movement.

See also: <A TImageEnMView.ViewX>.
!!}
procedure TImageEnMView.SetViewY(v: integer);
var
  max_x, max_y: integer;
begin
  if v = fViewY then
    exit;
  GetMaxViewXY(max_x, max_y);
  fViewY := ilimit(v, 0, max_y);
  invalidate;
  if (fScrollBars = ssVertical) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_VERT, trunc(fViewY / fRYScroll), true, fFlatScrollBars);
end;

{!!
<FS>TImageEnMView.SetViewXY

<FM>Declaration<FC>
procedure SetViewXY(x, y: integer);

<FM>Description<FN>
Sets <A TImageEnMView.ViewX> and <A TImageEnMView.ViewY> simultaneously.
!!}
procedure TImageEnMView.SetViewXY(x, y: integer);
var
  max_x, max_y: integer;
begin
  if Parent = nil then
    exit;
  if (x = fViewX) and (y = fViewY) then
    exit;
  GetMaxViewXY(max_x, max_y);
  fViewX := ilimit(x, 0, max_x);
  fViewY := ilimit(y, 0, max_y);
  invalidate;
  if (fScrollBars = ssHorizontal) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_HORZ, trunc(fViewX / fRXScroll), true, fFlatScrollBars);
  if (fScrollBars = ssVertical) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_VERT, trunc(fViewY / fRYScroll), true, fFlatScrollBars);
end;

procedure TImageEnMView.PaintToCanvas(DestCanvas: TCanvas);
var
  iStartX, iStartY: integer;
begin
  if fLockPaint = 0 then
  begin
    if (not HandleAllocated) or (ClientWidth=0) or (ClientHeight=0) then  // 3.0.0
      exit;
    if (fBackBuffer.Width <> ClientWidth) or (fBackBuffer.Height <> ClientHeight) then
      fBackBuffer.Allocate(ClientWidth, ClientHeight, ie24RGB);

    if fLockUpdate = 0 then
    begin
      if assigned(fAnimation) then
        AnimPaintTo(fBackBuffer)
      else
      begin
        PaintTo(fBackBuffer.VclBitmap);
        if fTrackMouseSelection and fDrawMouseSelection then
          with TIECanvas.Create(fBackBuffer.Canvas) do
          begin
            Brush.Color := $85664F;
            Brush.Style := bsSolid;
            Brush.Transparency := 128;
            Pen.Color := Brush.Color;
            Pen.Width := 1;
            Pen.Style := psSolid;
            Pen.Transparency := 255;
            iStartX := fHSVX1 + fHSX1 - ViewX;
            iStartY := fHSVY1 + fHSY1 - ViewY;
            Rectangle(iStartX, iStartY, fLastMouseMoveX, fLastMouseMoveY);
            Free();
          end;
      end;
    end;

    if (IEGlobalSettings().SystemColors < 24) and not IEGlobalSettings().IsRemoteSession then
    begin
      // dithering needed (for example display with 16bit depth need dithering)
      SetStretchBltMode(DestCanvas.Handle, HALFTONE);
      StretchBlt(DestCanvas.Handle, 0, 0, fBackBuffer.Width, fBackBuffer.Height, fBackBuffer.Canvas.Handle, 0, 0, fBackBuffer.Width, fBackBuffer.Height, SRCCOPY);
    end
    else
    begin
      // no dithering needed (fastest way)
      BitBlt(DestCanvas.handle, 0, 0, fBackBuffer.Width, fBackBuffer.Height, fBackBuffer.Canvas.Handle, 0, 0, SRCCOPY);
    end;

  end;
end;

procedure TImageEnMView.Paint;
begin
  PaintToCanvas(Canvas);
  DoAfterEvent(ieaePaint);
end;

{!!
<FS>TImageEnMView.ImageWidth

<FM>Declaration<FC>
property ImageWidth[idx: Integer];

<FM>Description<FN>
ImageWidth and <A TImageEnMView.ImageHeight> return the dimensions of the image, <FC>idx<FN>.
             
<FM>Example<FC>
// Display the image size below the thumbnail
// Ensure you have set the BottomGap property
procedure TForm1.ImageEnMView1ImageDraw(Sender: TObject; idx: Integer; Left, Top: Integer; Canvas: TCanvas);
begin
  Canvas.Font.Height := 15;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(Left,
                 Top + imageenmview1.ThumbHeight - imageenmview1.bottomgap + 2,
                 IntToStr(imageenmview1.ImageWidth[idx]) + 'x' + IntToStr(imageenmview1.ImageHeight[idx]));
end;
!!}
function TImageEnMView.GetImageWidth(idx: integer): integer;
begin
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      if image <> nil then
        result := fImageList.GetImageWidth(image);
end;

{!!
<FS>TImageEnMView.ImageHeight

<FM>Declaration<FC>
property ImageHeight[idx: Integer];

<FM>Description<FN>                          
<A TImageEnMView.ImageWidth> and ImageHeight return the dimensions of the image, <FC>idx<FN>.

<FM>Example<FC>
// Display the image size below the thumbnail
// Ensure you have set the BottomGap property
procedure TForm1.ImageEnMView1ImageDraw(Sender: TObject; idx: Integer; Left, Top: Integer; Canvas: TCanvas);
begin
  Canvas.Font.Height := 15;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(Left,
                 Top + imageenmview1.ThumbHeight - imageenmview1.bottomgap + 2,
                 IntToStr(imageenmview1.ImageWidth[idx]) + 'x' + IntToStr(imageenmview1.ImageHeight[idx]));
end;
!!}
function TImageEnMView.GetImageHeight(idx: integer): integer;
begin
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      if image <> nil then
        result := fImageList.GetImageHeight(image);
end;

{!!
<FS>TImageEnMView.ImageOriginalWidth

<FM>Declaration<FC>
property ImageOriginalWidth[idx: Integer]: Integer;

<FM>Description<FN>
Use ImageOriginalWidth to read or set the original width of the image, <FC>idx<FN>.

This is useful only when images are stored as thumbnails (<A TImageEnMView.StoreType> = ietThumb/ietFastThumb) and you need to know the original image size.

See also: <A TImageEnMView.ImageOriginalHeight>.
!!}
function TImageEnMView.GetImageOriginalWidth(idx: integer): integer;
begin
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      if image <> nil then
        result := fImageList.GetImageOriginalWidth(image)
      else
      if cacheImage <> nil then
        result := fCacheList.GetImageOriginalWidth(cacheImage);
    end;
end;

{!!
<FS>TImageEnMView.ImageOriginalHeight

<FM>Declaration<FC>
property ImageOriginalHeight[idx: Integer]: Integer;

<FM>Description<FN>          
Use ImageOriginalHeight to read or set the original height of the image, <FC>idx<FN>.

This is useful only when images are stored as thumbnails (<A TImageEnMView.StoreType> = ietThumb/ietFastThumb) and you need to know the original image size.

See also: <A TImageEnMView.ImageOriginalWidth>.
!!}
function TImageEnMView.GetImageOriginalHeight(idx: integer): integer;
begin
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      if image <> nil then
        result := fImageList.GetImageOriginalHeight(image)
      else
      if cacheImage <> nil then
        result := fCacheList.GetImageOriginalHeight(cacheImage);
   end;
end;

procedure TImageEnMView.SetImageOriginalWidth(idx: integer; Value: integer);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      if image <> nil then
        fImageList.SetImageOriginalWidth(image, Value);
end;

procedure TImageEnMView.SetImageOriginalHeight(idx: integer; Value: integer);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      if image <> nil then
        fImageList.SetImageOriginalHeight(image, Value);
end;

// Retrieve the size and dates of the image at idx, from the file system
procedure TImageEnMView.GetFileDetailsForImage(idx: integer); 
var                     
  ii: PIEImageInfo;
  iFileSizeBytes: Int64;
  dtCreateDate: TDateTime;
  dtModifiedDate: TDateTime;
begin                                               
  // Assumes we have already checked index is valid!
  ii := PIEImageInfo(fImageInfo[idx]);

  if ii^.FileSize = -1 then
    exit; // we have already checked this

  if assigned(ii^.Filename) and IEGetFileDetails(ii^.Filename, iFileSizeBytes, dtCreateDate, dtModifiedDate) then
  begin
    ii^.FileSize    := iFileSizeBytes;
    ii^.CreateDate  := dtCreateDate;
    ii^.EditDate    := dtModifiedDate;
  end
  else
  begin
    ii^.FileSize    := -1;
    ii^.CreateDate  := 0;
    ii^.EditDate    := 0;
  end;
end;




{!!
<FS>TImageEnMView.ImageCreateDate

<FM>Declaration<FC>
property ImageCreateDate[idx: Integer]: TDateTime;

<FM>Description<FN>
Returns the creation date for the file at index, idx. If the file is a JPEG then this will be read from the EXIF data for greater accuracy. Otherwise it is read from the file system.

Result is 0 if this frame does not exist as a file on disk and has no exif data.

See also: <A TImageEnMView.ImageEditDate>.
!!}
function TImageEnMView.GetImageCreateDate(idx: integer): TDateTime;
begin        
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    result := PIEImageInfo(fImageInfo[idx])^.CreateDate;
    if Result = 0 then
    begin
      GetFileDetailsForImage(idx);
      result := PIEImageInfo(fImageInfo[idx])^.CreateDate;
    end;
  end;
end;

procedure TImageEnMView.SetImageCreateDate(idx: integer; Value: TDateTime);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    PIEImageInfo(fImageInfo[idx])^.CreateDate := Value;
end;


{!!
<FS>TImageEnMView.ImageEditDate

<FM>Declaration<FC>
property ImageEditDate[idx: Integer]: TDateTime;

<FM>Description<FN>
Returns the last modification date for the file at index, idx, which is read from the file system.

Result is 0 if this frame does not exist as a file on disk.

See also: <A TImageEnMView.ImageCreateDate>.
!!}
function TImageEnMView.GetImageEditDate(idx: integer): TDateTime;
begin
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    result := PIEImageInfo(fImageInfo[idx])^.EditDate;
    if Result = 0 then
    begin
      GetFileDetailsForImage(idx);
      result := PIEImageInfo(fImageInfo[idx])^.EditDate;
    end;
  end;
end;

procedure TImageEnMView.SetImageEditDate(idx: integer; Value: TDateTime);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    PIEImageInfo(fImageInfo[idx])^.EditDate := Value;
end;

{!!
<FS>TImageEnMView.ImageFileSize

<FM>Declaration<FC>
property ImageFileSize[idx: Integer]: Int64;

<FM>Description<FN>
Returns the file size (in Bytes) for the file at index, idx. Result is -1 if this frame does not exist as a file on disk.

!!}
function TImageEnMView.GetImageFileSize(idx: integer): Int64;
begin
  Result := -1;
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    result := PIEImageInfo(fImageInfo[idx])^.FileSize;
    if Result = -1 then
    begin
      GetFileDetailsForImage(idx);
      result := PIEImageInfo(fImageInfo[idx])^.FileSize;
    end;
  end;
end;


function TImageEnMView.GetImageFileType(idx: integer): WideString;

  function IEGetFileFileType(const sFilename : String): String;
  var
    shfi: TShFileInfo;
  begin
    try
      ShGetFileInfo(PChar(sFilename), 0, shfi, SizeOf(TShFileInfo), SHGFI_TYPENAME);
      Result := shfi.szTypeName;
    except
      Result := '';
    end;
  end;

var
  sFilename: string;
begin
  Result := '';
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    sFilename := PIEImageInfo(fImageInfo[idx])^.FileName;
    if sFilename <> '' then
      result := IEGetFileFileType(sFilename);
  end;
end;





function TImageEnMView.GetImageType(idx: integer): TIEFolderImageType;
begin
  Result := ieftSupportedImage;
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    if PIEImageInfo(fImageInfo[idx])^.SourceType = iestFolderIcon then
      Result := ieftFolder
    else
    if IsKnownFormat(PIEImageInfo(fImageInfo[idx])^.Filename) then     
      Result := ieftSupportedImage
    else
      Result := ieftFile;
  end;
end;

procedure TImageEnMView.SetImageFileSize(idx: integer; Value: Int64);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    PIEImageInfo(fImageInfo[idx])^.FileSize := Value;
end;


{!!
<FS>TImageEnMView.ImageBitCount

<FM>Declaration<FC>
property ImageBitCount[idx: Integer]: Integer;

<FM>Description<FN>
Returns the bit count of the image, <FC>idx<FN>. It can be:
1 : Black/white image
24 : True color image
!!}
function TImageEnMView.GetImageBitCount(idx: integer): integer;
begin
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      if image <> nil then
        result := fImageList.GetImageBitCount(image)
      else
      if cacheImage <> nil then
        result := fCacheList.GetImageBitCount(cacheImage);
    end;    
end;

{!!
<FS>TImageEnMView.ImageY

<FM>Declaration<FC>
property ImageY[idx: Integer]: integer;

<FM>Description<FN>
Returns the Y position (in pixels) where image <FC>idx<FN> will be shown.

Read-only

!!}
function TImageEnMView.GetImageY(idx: integer): integer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.Y
  else
    result := 0;
end;

{!!
<FS>TImageEnMView.ImageX

<FM>Declaration<FC>
property ImageX[idx: Integer]: integer;

<FM>Description<FN>
Returns the X position (in pixels) where image <FC>idx<FN> will be shown.

Read-only

!!}
function TImageEnMView.GetImageX(idx: integer): integer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.X
  else
    result := 0;
end;

{!!
<FS>TImageEnMView.ImageRow

<FM>Declaration<FC>
property ImageRow[idx: Integer]: integer; (Read-only)

<FM>Description<FN>
Returns the row where image <FC>idx<FN> will be shown. ImageRow[] returns 0 for the first image, 1 for an image on the second row, etc.

!!}
function TImageEnMView.GetImageRow(idx: integer): integer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.row
  else
    result := 0;
end;

{!!
<FS>TImageEnMView.ImageCol

<FM>Declaration<FC>
property ImageCol[idx: Integer]: integer; (Read-only)

<FM>Description<FN>
ImageCol returns the column where is the image <FC>idx<FN> will be shown. ImageCol[] returns 0 for the first image, 1 for an image on the second column, etc.

!!}
function TImageEnMView.GetImageCol(idx: integer): integer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.col
  else
    result := 0;
end;

// notes: set ID=-1

procedure TImageEnMView.SetImageFileName(idx: integer; v: WideString);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      if assigned(Filename) then
        freemem(Filename);
      getmem(Filename, (length(v) + 1) * SizeOf(WideChar) );
      iewstrcopy(Filename, pwchar(v));
      ID := -1;
    end;
end;

{!!
<FS>TImageEnMView.ImageFileName

<FM>Declaration<FC>
property ImageFileName[idx: Integer]: WideString;

<FM>Description<FN>
Specifies the file from where TImageEnMView will load the image for index, <FC>idx<FN> (loading is delayed until it needs to be shown).

A full file path must be specified. Optionally an image index can be specified for multi-page files in the form: "FullFilePath::ImageIndex"

<FM>Example 1<FC>
// Load image1.jpg as thumbnail 0 and image2.jpg as thumbnail 1
ImageEnView1.ImageFileName[0] := 'C:\image1.jpg';
ImageEnView1.ImageFileName[1] := 'C:\image2.jpg';

<FM>Example 2<FC>
// Load frame 0 and 1 from input.mpeg
ImageEnView1.ImageFileName[0] := 'C:\input.mpeg::0';
ImageEnView1.ImageFileName[1] := 'C:\input.mpeg::1';

<FM>Example 3<FC>
This example inserts two images. The first is loaded from 'one.tif' and the second from 'two.tif'.
The images are loaded only when these are shown (otherwise they are freed).

ImageEnMView1.InsertImage(0);
ImageEnMView1.ImageFileName[0] := 'C:\one.tif';
ImageEnMView1.InsertImage(1);
ImageEnMView1.ImageFileName[1] := 'C:\two.tif';

<FM>See Also<FN>
- <A TImageEnMView.LoadFromFileOnDemand>
!!}
function TImageEnMView.GetImageFileName(idx: integer): WideString;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.Filename
  else
    result := '';
end;



// set name=''
procedure TImageEnMView.SetImageID(idx, v: integer);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      ID := v;
      freemem(Filename);
      Filename := nil;
    end;
end;

procedure TImageEnMView.SetImageTag(idx, v: integer);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      Tag := v;
end;

{!!
<FS>TImageEnMView.ImageTag

<FM>Declaration<FC>
property ImageTag[idx: Integer]: integer;

<FM>Description<FN>
Associates a numeric integer value with the image, <FC>idx<FN>.

Note:
- This property is not used by TImageEnMView in any way and is provided for custom use (similar to the Tag property of components).
- The value is not loaded/saved from file and streams, or copied/pasted to clipboard.
!!}
function TImageEnMView.GetImageTag(idx: integer): integer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.Tag
  else
    result := -1;
end;



procedure TImageEnMView.SetImageUserPointer(idx: Integer; v: pointer);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      userPointer := v;
end;

{!!
<FS>TImageEnMView.ImageUserPointer

<FM>Declaration<FC>
property ImageUserPointer[idx: Integer]: pointer;

<FM>Description<FN>
ImageUserPointer associates a pointer with image, <FC>idx<FN>.

Note:
- This property is not used by TImageEnMView in any way and is provided for custom use.
- The value is not loaded/saved from file and streams, or copied/pasted to clipboard.
!!}
function TImageEnMView.GetImageUserPointer(idx: Integer): pointer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.userPointer
  else
    result := nil;
end;


{!!
<FS>TImageEnMView.ImageID

<FM>Declaration<FC>
property ImageID[idx: Integer]: integer;

<FM>Description<FN>
ImageID associates a numeric ID with image, <FC>idx<FN>. It is mainly used to allow dynamic loading of content into a TImageEnMView.

This ID is returned in the <A TImageEnMView.OnImageIDRequest> and <A TImageEnMView.OnImageIDRequestEx> events.

Note: <A TImageEnMView.ImageFilename> is not valid when using <FC>ImageID<FN>.


<FM>Example<FN>
// Display all bitmaps in a TImageList in a TImageEnView
procedure TMyForm.FormShow(Sender: TObject);
var
  I : Integer;
begin           
  ImageEnMView1.Clear;

  // Add an ID for every bitmap in a TImageList
  for I := 0 to ImageList1.Count - 1 do
  begin
    ImageEnMView1.InsertImage(I);
    ImageEnMView1.ImageID[I] := I;
  end;
end;

procedure TMyForm.ImageEnMViewOnImageIDRequest(Sender: TObject; ID: integer; var Bitmap: TBitmap);
begin
  // Retrieve the image from a TImageList
  Bitmap := TBitmap.create;
  ImageList1.GetBitmap(ID, Bitmap);
end;
!!}
function TImageEnMView.GetImageID(idx: integer): integer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.ID
  else
    result := -1;
end;

procedure TImageEnMView.SetImageBackground(idx: integer; v: TColor);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    PIEImageInfo(fImageInfo[idx])^.Background := v;
    ClearImageCache(idx);
    UpdateEx(false);
  end;
end;

{!!
<FS>TImageEnMView.ImageBackground

<FM>Declaration<FC>
property ImageBackground: TColor;

<FM>Description<FN>
Specifies the color of the area between images.

!!}
function TImageEnMView.GetImageBackground(idx: integer): TColor;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.Background
  else
    result := 0;
end;

procedure TImageEnMView.SetImageDelayTime(idx: integer; v: Double);
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    PIEImageInfo(fImageInfo[idx])^.DTime := v;
end;

{!!
<FS>TImageEnMView.ImageDelayTime

<FM>Declaration<FC>
property ImageDelayTime[idx: Integer]: Double;

<FM>Description<FN>
ImageDelayTime is the time in milliseconds the image, <FC>idx<FN>, will be shown during playback (<A TImageEnMView.Playing> = True).

!!}
function TImageEnMView.GetImageDelayTime(idx: integer): Double;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[idx])^.DTime
  else
    result := 0.0;
end;

{!!
<FS>TImageEnMView.ThumbWidth

<FM>Declaration<FC>
property ThumbWidth: integer;

<FM>Description<FN>
Specifies the width of the thumbnails displayed in the TImageEnView.

Note: This is the width of the thumbnail frame. The thumbnail image itself will be ThumbWidth less 2 x <A TImageEnMView.SideGap>.

<FM>See Also<FN>
- <A TImageEnMView.ThumbHeight>
- <A TImageEnMView.SetThumbnailSize>
!!}
procedure TImageEnMView.SetThumbWidth(v: integer);
begin
  fThumbWidth := v;
  ClearCache;
  Update;
end;


function TImageEnMView.GetThumbWidth(): integer;
begin
  result := trunc(fThumbWidth * fZoom / 100.0);
end;


{!!
<FS>TImageEnMView.ThumbHeight

<FM>Declaration<FC>
property ThumbHeight: integer;

<FM>Description<FN>          
Specifies the height of the thumbnails displayed in the TImageEnView.

Note: This is the height of the thumbnail frame. The thumbnail image itself will be ThumbHeight less <A TImageEnMView.UpperGap> and <A TImageEnMView.BottomGap>.

<FM>See Also<FN>
- <A TImageEnMView.ThumbWidth>
- <A TImageEnMView.SetThumbnailSize>
!!}
procedure TImageEnMView.SetThumbHeight(v: integer);
begin
  fThumbHeight := v;
  ClearCache;
  Update;
end;


function TImageEnMView.GetThumbHeight(): integer;
begin
  result := trunc(fThumbHeight * fZoom / 100.0);
end;



{!!
<FS>TImageEnMView.SetThumbnailSize

<FM>Declaration<FC>
procedure SetThumbnailSize(width, height: Integer);

<FM>Description<FN>
Sets <A TImageEnMView.ThumbWidth> and <A TImageEnMView.ThumbHeight> in one step.

<FM>See Also<FN>
- <A TImageEnMView.ThumbWidth>
- <A TImageEnMView.ThumbHeight>
!!}
procedure TImageEnMView.SetThumbnailSize(width, height: Integer);
begin
  fThumbWidth  := width;
  fThumbHeight := height;
  ClearCache;
  Update;
end;


{!!
<FS>TImageEnMView.Zoom

<FM>Declaration<FC>
property Zoom: Double;

<FM>Description<FN>
Enlarge or reduce the size of the thumbnail display.

Note: This property is only recomended for handling of temporary display enlargement. It is better to <L TImageEnMView.ThumbWidth>set the size of the thumbnails</L>.
Specifies the horizontal distance between images.
              
<FM>See Also<FN>
- <A TImageEnMView.ThumbWidth>
- <A TImageEnMView.ThumbHeight>
!!}
procedure TImageEnMView.SetZoom(value: double);
begin
  fCurrentGridWidth := fGridWidth;
  if Value <> 100 then
    fCurrentGridWidth := CalcGridWidth();
  SetViewXY(round(ViewX / (fZoom / 100.0) * (value / 100.0)),
            round(ViewY / (fZoom / 100.0) * (value / 100.0)));
  fZoom := value;
  ClearCache();
  Update();
end;


{!!
<FS>TImageEnMView.ImageCount

<FM>Declaration<FC>
property ImageCount: integer; (Read-only)

<FM>Description<FN>
Returns the number of images stored in the TImageEnMView component.
            
<FM>Example<FC>          
// Display the filename and dimensions for each image
for I := 0 to ImageEnMView1.ImageCount - 1 do
begin
  ImageInfoText[I].Caption := IEM_ImageDimensions;
  ImageBottomText[I].Caption := IEM_Filename;
end;
!!}
function TImageEnMView.GetImageCount: integer;
begin
  result := fImageInfo.Count;
end;

{!!
<FS>TImageEnMView.HorizBorder

<FM>Declaration<FC>
property HorizBorder: integer;

<FM>Description<FN>
Specifies the horizontal distance between images.

See also: <A TImageEnMView.VertBorder>.
!!}
procedure TImageEnMView.SetHorizBorder(v: integer);
begin
  fHorizBorder := v;
  Update;
end;

function TImageEnMView._GetHorizMargin : integer;
var
  iColCount: Integer;
begin
  Result := 0;
  if (fDisplayMode = mdGrid) and (ietxCenterThumbnailColumn in fThumbnailOptionsEx) then
  begin
    if fCurrentGridWidth = 1 then
      Result := (ClientWidth - ThumbWidth) div 2
    else
    if fCurrentGridWidth <> 0 then
    begin
      iColCount := fCurrentGridWidth;
      if iColCount = -1 then
        iColCount := CalcGridWidth();
      Result := imax(0, (ClientWidth - (iColCount * (ThumbWidth + fHorizBorder))) div 2);
    end;
  end;
end;



{!!
<FS>TImageEnMView.VertBorder

<FM>Declaration<FC>
property VertBorder: integer;

<FM>Description<FN>
Specifies the vertical distance between images.

See also: <A TImageEnMView.HorizBorder>.
!!}
procedure TImageEnMView.SetVertBorder(v: integer);
begin
  fVertBorder := v;
  Update;
end;

function TImageEnMView._GetVertMargin : integer;
begin
  Result := 0;
  // Center vertically if we have only a single row
  if (fDisplayMode = mdGrid) and (ietxCenterThumbnailColumn in fThumbnailOptionsEx) and (fCurrentGridWidth = 0) then
    Result := (ClientHeight - ThumbHeight) div 2;
end;



{!!
<FS>TImageEnMView.UpdateCoords

<FM>Declaration<FC>
procedure UpdateCoords;

<FM>Description<FN>
For internal use only.
!!}
// recalc x, y image positions
// update fVWidth and fVHeight
procedure TImageEnMView.UpdateCoords;
var
  q, xx, yy, gw: integer;
begin
  gw := CalcGridWidth();
  xx := 0;
  yy := 0;
  fVWidth := 0;
  fVHeight := 0;
  for q := 0 to fImageInfo.Count - 1 do
  begin
    with PIEImageInfo(fImageInfo[q])^ do
    begin
      x := xx * (ThumbWidth + fHorizBorder);
      y := yy * (ThumbHeight + fVertBorder);
      if fDisplayMode = mdGrid then
      begin
        inc(x, imax(_GetHorizMargin, fHorizBorder));
        inc(y, imax(_GetVertMargin, fVertBorder));  
      end;
      row := yy;
      col := xx;
      if x > fVWidth then
        fVWidth := x;
      if y > fVHeight then
        fVHeight := y;
    end;
    // calculates next position
    inc(xx);
    if (fDisplayMode = mdSingle) or (xx = gw) then
    begin
      // horizontal limit, clear column, go to next row
      xx := 0;
      inc(yy);
      if fDisplayMode = mdSingle then
      begin
        // vertical limit, go back to top-left side
        yy := 0;
        xx := 0;
      end;
    end;
  end;
  inc(fVWidth, ThumbWidth + fHorizBorder);
  inc(fVHeight, ThumbHeight + fVertBorder);
end;


{!!
<FS>TImageEnMView.InsertImage

<FM>Declaration<FC>
procedure InsertImage(idx: integer);
procedure InsertImage(Idx : integer; Stream : TStream);
procedure InsertImage(Idx : integer; Bitmap : <A TIEBitmap>);
procedure InsertImage(Idx : integer; Bitmap : TBitmap);
procedure InsertImage(Idx : integer; Width, Height : integer; PixelFormat : <A TIEPixelFormat> = ie24RGB);  
procedure InsertImage(Idx : integer; const FileName : string);
procedure InsertImage(const FileName: String;
                      LoadOnDemand : boolean;
                      DefaultTopText : <A TIEImageEnMViewDefaultText> = iedtNone;
                      DefaultInfoText : <A TIEImageEnMViewDefaultText> = iedtNone;
                      DefaultBottomText : <A TIEImageEnMViewDefaultText> = iedtFilename;
                      bSelectIt : Boolean = true);

<FM>Description<FN>
Inserts or appends a new image in position, <FC>idx<FN> (0 is the first).

Note: First overload, InsertImage(idx: integer), doesn't create the bitmap.   

The parameters for the final overload are as follows:  
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Filename<FN></C> <C>A file or folder available on the system, or an HTTP or FTP link to a remote file</C> </R>
<R> <C><FC>LoadOnDemand<FN></C> <C>If True, the image will not be loaded from disk until it is displayed (i.e. when it is scrolled into view)</C> </R>
<R> <C><FC>DefaultTopText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageTopText></C> </R>
<R> <C><FC>DefaultInfoText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageInfoText></C> </R>
<R> <C><FC>DefaultBottomText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageBottomText> (defaults to the filename)</C> </R>
<R> <C><FC>bSelectIt<FN></C> <C>The new image will be selected in the control</C> </R>
</TABLE>

<FM>Examples<FC>
ImageEnView1.IO.LoadFromFile('C:\000.tif');
ImageEnMView1.InsertImage(0);
ImageEnMView1.SetImage(0, ImageEnView1.Bitmap);

// Which is the same as...
ImageEnMView1.InsertImage(0, 'C:\000.tif');

// Insert 256 x 256 bitmap
ImageEnMView1.InsertImage(0, 256, 256, ie24RGB);

// Insert a file from the web
ImageEnMView1.InsertImage(0, 'http://www.imageen.com/graphics/imageen.gif');
!!}
procedure TImageEnMView.InsertImage(idx: integer);
begin
  _InsertImage(idx, True);
end;


procedure TImageEnMView._InsertImage(idx: integer; bSelectIt : Boolean);
var
  newinfo: PIEImageInfo;
begin
  SetPlaying(false);
  getmem(newinfo, SizeOf(TIEImageInfo));
  newinfo^.Filename := nil;
  newinfo^.ID := -1;
  newinfo^.tag := 0;
  newinfo^.userPointer := nil;
  newinfo^.Background := fBackground;
  newinfo^.DTime := 0.0;
  newinfo^.image := nil;
  newinfo^.TopText := TIEMText.Create(self, iemtpTop);
  newinfo^.BottomText := TIEMText.Create(self, iemtpBottom);
  newinfo^.InfoText := TIEMText.Create(self, iemtpInfo);
  newinfo^.cacheImage := nil;
  newinfo^.parent := self;
  newinfo^.SourceType := iestDefault;
  newinfo^.Checked := False;
  newinfo^.CreateDate := 0;
  newinfo^.FileSize := 0;
  newinfo^.EditDate := 0;

  if idx < fImageInfo.Count then
  begin
    // insert
    fImageInfo.Insert(idx, newinfo); // info
    if bSelectIt or (fSelectedItem < 0) then
      SetSelectedItemNU(idx);
  end
  else
  begin
    // append
    fImageInfo.Add(newinfo); // info
    if bSelectIt or (fSelectedItem < 0) then
      SetSelectedItemNU(fImageInfo.Count - 1);
    fPriorHSIDX := fSelectedItem;
  end;
  if assigned(fOnCreateImage) then
    fOnCreateImage(self, fSelectedItem);
  if (fLockPaint = 0) and assigned(fAnimation) then
    fAnimation.InsertImage(fSelectedItem);
  fLastImOp := 1; // insert...
  fLastImIdx := idx;  // ...image
  fCheckedCount := -1;
  CallBitmapChangeEvents;
  UpdateEx(false, fSelectedImageAlwaysVisible);
end;

procedure TImageEnMView.InsertImage(Idx : integer; Stream : TStream);    
// NPC: 24/10/11
begin
  InsertImage(Idx);
  SetImageFromStream(Idx, Stream);
  if assigned(fAnimation) then
    fAnimation.RestartAnimation;
end;

procedure TImageEnMView.InsertImage(Idx : integer; Bitmap : TIEBitmap);
// NPC: 24/10/11
begin
  InsertImage(Idx);
  SetIEBitmap(Idx, Bitmap);
  if assigned(fAnimation) then
    fAnimation.RestartAnimation;
end;

procedure TImageEnMView.InsertImage(Idx : integer; Bitmap : TBitmap);
// NPC: 24/10/11
begin
  InsertImage(Idx);
  SetImage(Idx, Bitmap);
  if assigned(fAnimation) then
    fAnimation.RestartAnimation;
end;

procedure TImageEnMView.InsertImage(Idx : integer; Width, Height : integer; PixelFormat : TIEPixelFormat);
// NPC: 24/10/11
var
  temp: TIEBitmap;
begin
  InsertImage(Idx);
  temp := TIEBitmap.Create;
  try
    temp.Allocate(Width, Height, PixelFormat);
    temp.Fill(clBlack);
    SetIEBitmap(Idx, temp);
    if assigned(fAnimation) then
      fAnimation.RestartAnimation;
  finally
    temp.free;
  end;
end;    

procedure TImageEnMView.InsertImage(Idx : integer; const FileName : string);
// NPC: 24/10/11
begin
  InsertImage(Idx);
  SetImageFromFile(Idx, FileName);
  if assigned(fAnimation) then
    fAnimation.RestartAnimation;
end;


function StripShortcutExt(const sFilename : string) : string;
const
  Windows_Shortcut_File_Ext = '.LNK';
var
  iPos: Integer;
begin
  Result := sFilename;
  iPos := Pos(Windows_Shortcut_File_Ext, uppercase(Result));
  if (iPos > 0) and (iPos = Length(Result) - Length(Windows_Shortcut_File_Ext) + 1) then
    SetLength(Result, iPos - 1);
end;


procedure TImageEnMView.InsertImage(Idx : integer; const FileName : string;
                                    LoadOnDemand : boolean;
                                    DefaultTopText : TIEImageEnMViewDefaultText = iedtNone;
                                    DefaultInfoText : TIEImageEnMViewDefaultText = iedtNone;
                                    DefaultBottomText : TIEImageEnMViewDefaultText = iedtFilename;
                                    bSelectIt : Boolean = true);

  function _GetCaption(DefaultText : TIEImageEnMViewDefaultText) : string;
  begin
    Result := '';
    case DefaultText of
      iedtFilename              : Result := StripShortcutExt(IEExtractFileNameW(FileName));
      iedtFilenameNoExt         : Result := IEM_FilenameNoExt        ;
      iedtFilePath              : Result := IEM_FilePath             ;
      iedtImageDimensions       : Result := IEM_ImageDimensions      ;
      iedtImageDimAndSize       : Result := IEM_ImageDimAndSize      ;
      iedtFileSize              : Result := IEM_FileSize             ;
      iedtFileCreateDate        : Result := IEM_FileCreateDate       ;
      iedtFileCreateDateTime    : Result := IEM_FileCreateDateTime   ;
      iedtFileCreateDateAndSize : Result := IEM_FileCreateDateAndSize;
      iedtFileEditDate          : Result := IEM_FileEditDate         ;
      iedtFileEditDateTime      : Result := IEM_FileEditDateTime     ;
      iedtFileEditDateAndSize   : Result := IEM_FileEditDateAndSize  ;
      iedtFileType              : Result := IEM_FileType             ;
    end;
  end;

begin        
  _InsertImage(Idx, bSelectIt);
  if LoadOnDemand then
    ImageFileName[idx] := FileName
  else
    SetImageFromFile(Idx, FileName);

  ImageTopText[idx].Caption    := _GetCaption(DefaultTopText);
  ImageInfoText[idx].Caption   := _GetCaption(DefaultInfoText);
  ImageBottomText[idx].Caption := _GetCaption(DefaultBottomText);

  if assigned(fAnimation) then
    fAnimation.RestartAnimation;
end;




{!!
<FS>TImageEnMView.InsertImageEx

<FM>Declaration<FC>
procedure InsertImageEx(idx: integer);

<FM>Description<FN>
InsertImageEx inserts a new image in position, <FC>idx<FN> (0 is the first).

Unlike <A TImageEnMView.InsertImage>, InsertImageEx doesn't create the bitmap and doesn't make the image selected.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('000.tif');
ImageEnMView1.InsertImageEx(0);
ImageEnMView1.SetImage(0, ImageEnView1.Bitmap);

<FM>See Also<FN>
- <A TImageEnMView.InsertImage>
- <A TImageEnMView.InsertImageEx>
- <A TImageEnMView.DeleteImage>
- <A TImageEnMView.AppendImage>

!!}
// Insert a new image
// idx is the index where to insert the new image
// If idx is equal to ImageCount it is added at the end
// The bitmap (Image[]) is created as "nil"
// The bitmap position is choised from fHorizImages and fVertImages values
// calls Update
// The image is not selected
// disable player
procedure TImageEnMView.InsertImageEx(idx: integer);
var
  newinfo: PIEImageInfo;
begin
  SetPlaying(false);
  getmem(newinfo, SizeOf(TIEImageInfo));
  newinfo^.Filename := nil;
  newinfo^.ID := -1;
  newinfo^.tag := 0;
  newinfo^.userPointer := nil;
  newinfo^.Background := fBackground;
  newinfo^.DTime := 0.0;
  newinfo^.image := nil;
  newinfo^.TopText := TIEMText.Create(Self, iemtpTop);
  newinfo^.BottomText := TIEMText.Create(Self, iemtpBottom);
  newinfo^.InfoText := TIEMText.Create(Self, iemtpInfo);
  newinfo^.cacheImage := nil;
  newinfo^.parent := self;
  newinfo^.SourceType := iestDefault;  
  newinfo^.Checked := False;
  newinfo^.CreateDate := 0;
  newinfo^.FileSize := 0;
  newinfo^.EditDate := 0;
  
  if idx < fImageInfo.Count then
  begin
    // insert
    fImageInfo.Insert(idx, newinfo); // info
  end
  else
  begin
    // append
    fImageInfo.Add(newinfo); // info
  end;
  if assigned(fOnCreateImage) then
    fOnCreateImage(self, idx);
  if assigned(fAnimation) then
    fAnimation.InsertImage(idx);
  fLastImOp := 1; // insert...
  fLastImIdx := idx; //...image    
  fCheckedCount := -1;
  CallBitmapChangeEvents;
  UpdateEx(false);
end;




{!!
<FS>TImageEnMView.InsertTransitionFrames

<FM>Declaration<FC>
procedure InsertTransitionFrames(Idx : integer; iFrameCount : integer; Effect : <A TIETransitionType>; iWidth : Integer = -1; iHeight : Integer = -1; BackgroundColor : TColor = -1; ResamplingFilter: <A TResampleFilter> = rfFastLinear);

<FM>Description<FN>
Create a series of transition frames from the image at <FC>Idx - 1<FN> to the image at <FC>Idx<FN> (and insert them at position <FC>Idx<FN>).  If <FC>Idx<FN> = 0 then the transition is from a blank frame to the first image. If <FC>Idx<FN> = ImageCount then the transition is from the last image to a blank frame.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Idx<FN></C> <C>The insertion position</C> </R>
<R> <C><FC>iFrameCount<FN></C> <C>The number of frames to insert</C> </R>
<R> <C><FC>Effect<FN></C> <C>The desired <L TIETransitionType>transition effect</L></C> </R>
<R> <C><FC>iWidth, iHeight<FN></C> <C>The size to create the transition bitmaps.  If either of these are -1 then the size will be the larger of the two images in each dimension.  Aspect Ratios will be maintained and any non-image area will be filled with <FC>BackgroundColor<FN>.</C> </R>
<R> <C><FC>BackgroundColor<FN></C> <C>The color that will be used for blank frames or non-image area (if -1 then <A TImageEnMView.Background> is used)</C> </R>
<R> <C><FC>ResamplingFilter<FN></C> <C>The <L TResampleFilter>algorithm</L> that is used to improve quality when resizing images</C> </R>
</TABLE>

Note: Use <A TImageEnMView.InsertTransitionFramesEx> if you need to create frames for an iettPanZoom transition

<FM>Example<FC>
// Create ten frames that use a cross dissolve transition from image 5 to image 6
ImageEnMView1.InsertTransitionFrames(6, 10, iettCrossDissolve);

<FM>See Also<FN>
- <A TImageEnMView.InsertTransitionFramesEx>
!!}
procedure TImageEnMView.InsertTransitionFrames(Idx : integer; iFrameCount : integer; Effect : TIETransitionType; iWidth : Integer = -1; iHeight : Integer = -1;
                                               BackgroundColor : TColor = -1; ResamplingFilter: TResampleFilter = rfFastLinear); 
// NPC: 24/10/11
var
  ARect : TRect;
begin
  ARect := Rect(0, 0, 0, 0);
  InsertTransitionFramesEx(Idx, iFrameCount, Effect, ARect, ARect, True, iWidth, iHeight, False, BackgroundColor, ResamplingFilter);
end;

{!!
<FS>TImageEnMView.InsertTransitionFramesEx

<FM>Declaration<FC>
procedure InsertTransitionFramesEx(Idx : Integer; iFrameCount : Integer; Effect : <A TIETransitionType>;
                                   StartRect, EndRect : TRect; RectMaintainAspectRatio : boolean = True;
                                   iWidth : Integer = -1; iHeight : Integer = -1; bStretchSmall : Boolean = False;
                                   BackgroundColor : TColor = -1; ResamplingFilter : <A TResampleFilter>;
                                   Timing : <A TIETransitionTiming> = iettLinear);


<FM>Description<FN>
This is an extended version of <A TImageEnMView.InsertTransitionFrames> that includes more parameters and is primarily used when you need to create a series of frames that show a Pan Zoom from <FC>StartRect<FN> to <FC>EndRect<FN> for the image specified at <FC>Idx - 1<FN>.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Idx<FN></C> <C>The insertion position</C> </R>
<R> <C><FC>iFrameCount<FN></C> <C>The number of frames to insert</C> </R>
<R> <C><FC>Effect<FN></C> <C>The desired <L TIETransitionType>transition effect</L></C> </R>
<R> <C><FC>StartRect<FN></C> <C>When using an iettPanZoom effect this is the portion of the image that is shown at the start</C> </R>
<R> <C><FC>EndRect<FN></C> <C>When using an iettPanZoom effect this is the portion of the image that is shown at the end</C> </R>
<R> <C><FC>RectMaintainAspectRatio<FN></C> <C>ImageEn will ensure that the starting and ending rects are automatically adjusted to ensure the resultant image has the correct aspect ratio (iettPanZoom only)</C> </R>
<R> <C><FC>iWidth, iHeight<FN></C> <C>The size to create the transition bitmaps.  If either of these are -1 then the size will be the larger of the two images in each dimension.  Aspect Ratios will be maintained and any non-image area will be filled with <FC>BackgroundColor<FN>.</C> </R>
<R> <C><FC>bStretchSmall<FN></C> <C>If the images are smaller than the transition bitmap size (<FC>iWidth<FN> x <FC>iHeight<FN>) should they be stretched to fit (which can lead to distortion).</C> </R>
<R> <C><FC>BackgroundColor<FN></C> <C>The color that will be used for blank frames or non-image area (if -1 then <A TImageEnMView.Background> is used)</C> </R>
<R> <C><FC>ResamplingFilter<FN></C> <C>The <L TResampleFilter>algorithm</L> that is used to improve quality when resizing images</C> </R>
<R> <C><FC>Timing<FN></C> <C>The <L TIETransitionTiming>rate</L> at which the transition progresses</C> </R>
</TABLE>

<FM>Example<FC>
// Create ten Pan Zoom frames for the image at index 5

// Top Left corner of image
StartingRect := Rect(0, 0, ImageEnMView1.ImageWidth[5] div 4, ImageEnMView1.ImageHeight[5] div 4);

// Bottom right corner of image
EndingRect := Rect(MulDiv(ImageEnMView1.ImageWidth[5], 3, 4), MulDiv(ImageEnMView1.ImageHeight[5], 3, 4), ImageEnMView1.ImageWidth[5], ImageEnMView1.ImageHeight[5]);

// Create frames
ImageEnMView1.InsertTransitionFramesEx(5, 10, iettPanZoom, StartRect, EndRect);

<FM>See Also<FN>
- <A TImageEnMView.InsertTransitionFrames>

!!}
procedure TImageEnMView.InsertTransitionFramesEx(Idx : integer; iFrameCount : Integer; Effect : TIETransitionType;
                                                 StartRect, EndRect : TRect; RectMaintainAspectRatio : boolean = True;
                                                 iWidth : Integer = -1; iHeight : Integer = -1; bStretchSmall : Boolean = False;
                                                 BackgroundColor : TColor = -1; ResamplingFilter: TResampleFilter = rfFastLinear;
                                                 Timing : TIETransitionTiming = iettLinear);
// NPC: 24/10/11
var
  StartImage    : TIEBitmap;
  EndImage      : TIEBitmap;
  ATransBitmap  : TBitmap; 
  iSteps        : Integer;
  EachStep      : Single;
  TransLevel    : Single;
  I             : Integer;
  BColor        : TColor;
  bWantFrame0   : Boolean;
  bWantFrame100 : Boolean;
begin
  if iFrameCount < 1 then
    exit;

  if (Idx < 0) or (Idx > ImageCount) then
    raise Exception.create('Invalid Index');
    
  if (Idx = 0) and (Effect = iettPanZoom) then
    raise Exception.create('Invalid index for iettPanZoom');
    
  // We don't want 0% or 100% for normal transitions as they will be identical to the before and after frames
  bWantFrame0   := False;
  bWantFrame100 := False;

  LockPaint;
  StartImage   := TIEBitmap.create;
  EndImage     := TIEBitmap.create;
  ATransBitmap := TBitmap.create;
  try
    if BackgroundColor = -1 then
      BColor := Background
    else
      BColor := BackgroundColor;

    If Idx = 0 then
    begin
      // Starting image is blank, Ending image is index 0
      CopyToIEBitmap(idx, EndImage);
      if (iWidth < 0) or (iHeight < 0) then
        StartImage.Allocate(EndImage.Width, EndImage.Height)
      else
        StartImage.Allocate(iWidth, iHeight);
      StartImage.Fill(BColor);

      // We want the initial blank (0%) Frame
      bWantFrame0   := True;
    end
    else
    if Idx = ImageCount then
    begin
      // Starting image is last frame, ending image is blank 
      // Starting image is blank, Ending image is index 0
      CopyToIEBitmap(idx - 1, StartImage);
      if (iWidth < 0) or (iHeight < 0) then
        EndImage.Allocate(StartImage.Width, StartImage.Height)
      else
        EndImage.Allocate(iWidth, iHeight);
      EndImage.Fill(BColor); 

      // We want the ending blank (100%) Frame
      bWantFrame100 := True;
    end
    else
    begin
      // Both images are valid   
      CopyToIEBitmap(idx - 1, StartImage);
      CopyToIEBitmap(idx, EndImage);    
    end;

    if Effect = iettPanZoom then
    begin
      // We want both the start and end frames (i.e. the rects that the user specified)
      bWantFrame0   := True;
      bWantFrame100 := True;
    end;

    // Prepare the bitmaps
    Proc.PrepareTransitionBitmapsEx(StartImage.VclBitmap, EndImage.VclBitmap, Effect,
                                    StartRect, EndRect, RectMaintainAspectRatio,
                                    iWidth, iHeight, bStretchSmall, BColor, ResamplingFilter, Timing);

    for I := 1 to iFrameCount do
    begin
      iSteps   := (iFrameCount + 1) - (Integer(bWantFrame0) + Integer(bWantFrame100));
      EachStep := 100 / iSteps;
      if bWantFrame0 then
        TransLevel := (I - 1) * EachStep
      else
        TransLevel := I * EachStep;
      Proc.CreateTransitionBitmap(TransLevel, ATransBitmap);
      InsertImage(idx, ATransBitmap);
      Inc(idx);
    end;

  finally
    ATransBitmap.Free;
    StartImage.free;
    EndImage  .free;    
    UnLockPaint;
    Update;
  end;
end;




{!!
<FS>TImageEnMView.AppendImage

<FM>Declaration<FC>
function AppendImage: Integer;
function AppendImage(Stream: TStream): integer;
function AppendImage(Bitmap: <A TIEBitmap>): integer;
function AppendImage(Bitmap : TBitmap): integer;
function AppendImage(Width, Height: Integer; PixelFormat: <A TIEPixelFormat> = ie24RGB): Integer;    
function AppendImage(const FileName: String): integer;
function AppendImage(const FileName: String;
                     LoadOnDemand : boolean;
                     DefaultTopText : <A TIEImageEnMViewDefaultText> = iedtNone;
                     DefaultInfoText : <A TIEImageEnMViewDefaultText> = iedtNone;
                     DefaultBottomText : <A TIEImageEnMViewDefaultText> = iedtFilename;
                     bSelectIt : Boolean = true): integer;

<FM>Description<FN>
AppendImage appends a new image at last position in the list and returns the new image position.

Note: First overload of AppendImage doesn't create the bitmap. Others load image from a file or a stream.

The parameters for the final overload are as follows:
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Filename<FN></C> <C>A file or folder available on the system, or an HTTP or FTP link to a remote file</C> </R>
<R> <C><FC>LoadOnDemand<FN></C> <C>If True, the image will not be loaded from disk until it is displayed (i.e. when it is scrolled into view)</C> </R>
<R> <C><FC>DefaultTopText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageTopText></C> </R>
<R> <C><FC>DefaultInfoText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageInfoText></C> </R>
<R> <C><FC>DefaultBottomText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageBottomText> (defaults to the filename)</C> </R>
<R> <C><FC>bSelectIt<FN></C> <C>The new image will be selected in the control</C> </R>
</TABLE>

<FM>Examples<FC>
ImageEnView1.IO.LoadFromFile('C:\000.tif');
idx := ImageEnMView1.AppendImage;
ImageEnMView1.SetImage(idx, ImageEnView1.Bitmap);

// Which is the same as...
ImageEnMView1.AppendImage('C:\000.tif');

// Append 256 x 256 bitmap
ImageEnMView1.AppendImage(256, 256, ie24RGB);

// Append a file from the web
ImageEnMView1.AppendImage('http://www.imageen.com/graphics/imageen.gif');
!!}
function TImageEnMView.AppendImage(): integer;
begin
  result := fImageInfo.Count;
  InsertImage(fImageInfo.Count);
end;

function TImageEnMView.AppendImage(Stream: TStream): integer;
begin
  result := fImageInfo.Count;
  InsertImage(Result, Stream);
end;

function TImageEnMView.AppendImage(Bitmap: TIEBitmap): integer;
begin
  result := fImageInfo.Count;
  InsertImage(Result, Bitmap);
end;

function TImageEnMView.AppendImage(Bitmap : TBitmap): integer;
begin
  result := fImageInfo.Count;
  InsertImage(Result, Bitmap);
end;

function TImageEnMView.AppendImage(Width, Height: Integer; PixelFormat: TIEPixelFormat): Integer;
begin
  result := fImageInfo.Count;
  InsertImage(Result, Width, Height, PixelFormat);
end;

function TImageEnMView.AppendImage(const FileName: String): integer;
begin
  result := fImageInfo.Count;
  InsertImage(result, FileName);
end;                       


function TImageEnMView.AppendImage(const FileName: String;
                                   LoadOnDemand : boolean;
                                   DefaultTopText : TIEImageEnMViewDefaultText = iedtNone;
                                   DefaultInfoText : TIEImageEnMViewDefaultText = iedtNone;
                                   DefaultBottomText : TIEImageEnMViewDefaultText = iedtFilename;
                                   bSelectIt : Boolean = true): integer; 
begin
  result := fImageInfo.Count;
  InsertImage(result, FileName, LoadOnDemand, DefaultTopText, DefaultInfoText, DefaultBottomText, bSelectIt);
end;



{$ifdef IEINCLUDEDEPRECATEDMETHODS}
{$ifdef IESUPPORTDEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$endif}
function TImageEnMView.AppendImage2(Width, Height: Integer; PixelFormat: TIEPixelFormat): Integer; {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
begin
  result := AppendImage(Width, Height, PixelFormat);
end;
{$ifdef IESUPPORTDEPRECATED} {$WARN SYMBOL_DEPRECATED ON} {$endif}
{$endif}


{!!
<FS>TImageEnMView.AppendSplit

<FM>Declaration<FC>
function AppendSplit(SourceGrid: <A TIEBitmap>; cellWidth: Integer; cellHeight: Integer; maxCount: Integer = 0): Integer;

<FM>Description<FN>
Splits the source image into cells of the specified size and adds each cell to the TImageEnMView

Result is the count of added images.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>SourceGrid<FN></C> <C>Source bitmap containing cells to split.</C> </R>
<R> <C><FC>cellWidth<FN></C> <C>Width of a cell.</C> </R>
<R> <C><FC>cellHeight<FN></C> <C>Height of a cell.</C> </R>
<R> <C><FC>maxCount<FN></C> <C>Maximum number of cells to add. 0 = all suitable cells.</C> </R>
</TABLE>
!!}
function TImageEnMView.AppendSplit(SourceGrid: TIEBitmap; cellWidth: Integer; cellHeight: Integer; maxCount: Integer): Integer;
var
  x, y: Integer;
begin
  result := 0;
  y := 0;
  while y < SourceGrid.Height do
  begin
    x := 0;
    while x < SourceGrid.Width do
    begin
      SetImageRect(AppendImage, SourceGrid, x, y, x+cellWidth-1, y+cellHeight-1);
      inc(result);
      if (maxCount>0) and (maxCount=result) then
        exit;
      inc(x, cellWidth);
    end;
    inc(y, cellHeight);
  end;
end;


// Delete image idx
// doesn't call Update
// Return true if image correctly deleted
// Disable playing
// use bBatchProcessing = True if this is being called by DeleteAllImages
function TImageEnMView.DeleteImageNU(idx: integer; bBatchProcessing : Boolean = False): boolean;
var
  psel: integer;
begin            
  result := false;
  SetPlaying(false);
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    AbortImageLoading(idx);

    psel := fSelectedItem;
    if bBatchProcessing = False then
      DeselectNU;

    if assigned(fOnDestroyImage) then
      fOnDestroyImage(self, idx);

    if PIEImageInfo(fImageInfo[idx])^.image <> nil then
    begin
      fImageList.Delete(PIEImageInfo(fImageInfo[idx])^.image);
      ClearImageCache(idx);
    end;
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      // free file name
      if assigned(Filename) then
        freemem(Filename);
      FreeAndNil(TopText);
      FreeAndNil(BottomText);
      FreeAndNil(InfoText);
    end;

    // free imageinfo
    freemem(fImageInfo[idx]);
    fImageInfo.Delete(idx);

    if bBatchProcessing = False then
    begin
      if (psel = idx) and (idx >= fImageInfo.Count) then
        SetSelectedItemNU(fImageInfo.Count - 1)
      else
      if psel > idx then
        SetSelectedItemNU(psel - 1)
      else
        SetSelectedItemNU(psel);

      if assigned(fAnimation) then
        fAnimation.DeleteImage(idx);
    end;

    fLastImOp  := 2;    // delete...
    fLastImIdx := idx;  //...image     
    fCheckedCount := -1;
    CallBitmapChangeEvents;
    result := true;
  end;
end;


procedure TImageEnMView.DeleteAllImages();
var
  idx: integer;
begin
  SetPlaying(false);
  DeselectNU();
  for idx := fImageInfo.Count - 1 downto 0 do
    DeleteImageNU(idx, True);
  fPriorHSIDX := 0;
end;

function ComparePointers(Item1, Item2: Pointer): Integer;
begin
  result := NativeInt(Item1) - NativeInt(Item2);
end;

{!!
<FS>TImageEnMView.DeleteSelectedImages

<FM>Declaration<FC>
procedure DeleteSelectedImages;

<FM>Description<FN>
Removes all selected images.
!!}
procedure TImageEnMView.DeleteSelectedImages;
var
  q: integer;
  cp: array of integer;
begin
  if DisplayMode = mdSingle then
    DeleteImageNU(VisibleFrame)
  else
  if EnableMultiSelect = False then    
    DeleteImageNU(SelectedImage)
  else
  begin
    fMultiSelectedImages.Sort(@ComparePointers);
    SetLength(cp, fMultiSelectedImages.Count);
    for q := 0 to fMultiSelectedImages.Count - 1 do
      cp[q] := integer(fMultiSelectedImages[q]);
    for q := length(cp) - 1 downto 0 do
      DeleteImageNU(cp[q]);
  end;
  UpdateEx(false, fSelectedImageAlwaysVisible);
end;


{!!
<FS>TImageEnMView.DeleteImage

<FM>Declaration<FC>
procedure DeleteImage(idx: integer);

<FM>Description<FN>
Deletes image, <FC>idx<FN> (freeing its associated bitmap).
!!}
procedure TImageEnMView.DeleteImage(idx: integer);
begin
  if DeleteImageNU(idx) then
    UpdateEx(false, fSelectedImageAlwaysVisible);
end;

// This method is called whenever the zoom or viewx/y changes.
procedure TImageEnMView.ViewChange(c: integer);
begin
  if assigned(fOnViewChange) and fUserAction then
    fOnViewChange(self, c);
end;

{!!
<FS>TImageEnMView.Update

<FM>Declaration<FC>
procedure Update;

<FM>Description<FN>
Updates the display to show the current content and properties.
!!}
procedure TImageEnMView.Update;
begin
  if assigned(fAnimation) then
    fAnimation.ImageCount := ImageCount;
  UpdateEx(true);
end;

procedure TImageEnMView.UpdateEx(bUpdateCache: Boolean; bRespositionSelection : Boolean = False);
var
  max_x, max_y, i: integer;
  lClientWidth, lClientHeight: integer;
  bb: boolean;
begin
  if (fLockPaint > 0) or (fLockUpdate > 0) then
    exit;
  if fUpdating then
    exit;
  if fDestroying then
    exit;
  if not HandleAllocated then // 3.0.0
    exit;
{$IFDEF IEFIXUPDATE}
  if (ComponentState <> []) and (ComponentState <> [csDesigning]) and (ComponentState <> [csFreeNotification]) then
    exit;
{$ELSE}
  if (ComponentState <> []) and (ComponentState <> [csDesigning]) then
    exit;
{$ENDIF}
  if (GetParentForm(self) = nil) and (ParentWindow = 0) then
    exit;

  if not (csDesigning in ComponentState) then
  begin
    fUpdating := true;
    if bUpdateCache then
      ClearCache;
    UpdateCoords;
    for i := 0 to 8 do
    begin
      lClientWidth := ClientWidth;
      lClientHeight := ClientHeight;
      bb := false;
      GetMaxViewXY(max_x, max_y);
      if fViewX > max_x then
      begin
        fViewX := max_x;
        bb := true;
      end;
      if fViewY > max_y then
      begin
        fViewY := max_y;
        bb := true;
      end;
      if bb then
        ViewChange(0);
      try
        if ((fScrollBars = ssHorizontal) or (fScrollBars = ssBoth)) and not assigned(fAnimation) then
        begin
          if (max_x > 0) then
          begin
            fRXScroll := (max_x + ClientWidth - 1) / 65536;
            IESetScrollBar(Handle, SB_HORZ, 0, 65535, trunc(ClientWidth / fRXScroll), trunc(fViewX / fRXScroll), true, fFlatScrollBars);
            IEEnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_HORZ, true, fFlatScrollBars);
          end
          else
          if fScrollBarsAlwaysVisible then
          begin
            IEEnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_HORZ, true, fFlatScrollBars);
          end
          else
            IEShowScrollBar(handle, SB_HORZ, false, fFlatScrollBars);
        end;
        if (fScrollBars = ssVertical) or (fScrollBars = ssBoth) and not assigned(fAnimation) then
        begin
          if (max_y > 0) then
          begin
            fRYScroll := (max_y + ClientHeight - 1) / 65536;
            IESetScrollBar(Handle, SB_VERT, 0, 65535, trunc(ClientHeight / fRYScroll), trunc(fViewY / fRYScroll), true, fFlatScrollBars);
            IEEnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_VERT, true, fFlatScrollBars);
          end
          else
          if fScrollBarsAlwaysVisible then
          begin
            IEEnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_VERT, true, fFlatScrollBars);
          end
          else
            IEShowScrollBar(handle, SB_VERT, false, fFlatScrollBars);
        end;
      except
      end;
      if (lClientWidth = ClientWidth) and (lClientHeight = ClientHeight) then
        break; // exit from for loop (no other adjustments necessary)
    end;

    if assigned(fAnimation) then
    begin
      fAnimation.ViewWidth := ClientWidth;
      fAnimation.ViewHeight := ClientHeight;
      fAnimation.CurrentImage := SelectedImage;
    end;

    CallBitmapChangeEvents;
    fUpdating := false;
  end;
  if bRespositionSelection then
    CheckSelectedImageIsVisible;
  invalidate;
  redrawwindow(handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

function TImageEnMView.PaletteChanged(Foreground: Boolean): Boolean;
begin
{$IFNDEF OCXVERSION}
  if assigned(application) and assigned(application.mainform) and assigned(application.mainform.canvas) then
  begin
    if IEDrawDibRealize(fHDrawDib, application.mainform.canvas.handle, false) > 0 then
      invalidate;
  end
  else
    invalidate;
{$ELSE}
  invalidate;
{$ENDIF}
  result := true;
end;


{!!
<FS>TImageEnMView.Background

<FM>Declaration<FC>
property Background: TColor;

<FM>Description<FN>
Specifies the background color of the component.
!!}
procedure TImageEnMView.SetBackGround(cl: TColor);
begin
  inherited SetBackGround(cl);
  Update;
end;


{!!
<FS>TImageEnMView.SetImageEx

<FM>Declaration<FC>
procedure SetImageEx(idx: Integer; srcImage: TBitmap);

<FM>Description<FN>
Sets the image assigned to index, <FC>idx<FN>. The <FC>srcImage<FN> bitmap is copied internally; therefore you can free <FC>srcImage<FN> after calling SetImageEx.

SetImageEx doesn't call <A TImageEnMView.Update> (the only difference from <A TImageEnMView.SetImage>).

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('000.tif');
ImageEnMView1.InsertImageEx(0);
ImageEnMView1.SetImageEx(0, ImageEnView1.Bitmap);
ImageEnMView1.Update;   // not needed for SetImage()

!!}
procedure TImageEnMView.SetImageEx(idx: integer; srcImage: TBitmap);
var
  tbmp: TIEBitmap;
begin
  if srcImage <> nil then
  begin
    tbmp := TIEBitmap.Create;
    try
      tbmp.EncapsulateTBitmap(srcImage, true);
      SetIEBitmapEx(idx, tbmp);
    finally
      FreeAndNil(tbmp);
    end;
  end;
end;

{!!
<FS>TImageEnMView.SetIEBitmap

<FM>Declaration<FC>
procedure SetIEBitmap(idx: Integer; srcImage: <A TIEBaseBitmap>);

<FM>Description<FN>
Sets the image assigned to index, <FC>idx<FN>. The <FC>srcImage<FN> bitmap is copied internally, therefore you can free <FC>srcImage<FN> after calling SetIEBitmap.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('000.tif');
ImageEnMView1.InsertImageEx(0);
ImageEnMView1.SetIEBitmap(0, ImageEnView1.IEBitmap);
!!}
procedure TImageEnMView.SetIEBitmap(idx: integer; srcImage: TIEBaseBitmap);
begin
  if srcImage <> nil then
  begin
    SetIEBitmapEx(idx, srcImage);
    ClearImageCache(idx);
    UpdateEx(false);
  end;
end;


// SetIEBitmapEx creates a new copy of "srcImage" (then srcImage can be freed)
// Doesn't call Update
procedure TImageEnMView.SetIEBitmapEx(idx: integer; srcImage: TIEBaseBitmap);
var
  bmp: TIEBitmap;
  ww, hh: integer;
  wDummy, hDummy : integer;
begin
  if srcImage <> nil then
  begin
    if (idx > -1) and (idx < fImageInfo.Count) then
    begin
      if PIEImageInfo(fImageInfo[idx])^.image <> nil then
      begin
        fImageList.Delete(PIEImageInfo(fImageInfo[idx])^.image);
        ClearImageCache(idx);
      end;

      if (fStoreType = ietThumb) and (fEnableResamplingOnMinor or (srcImage.Width > ThumbWidth) or (srcImage.Height > ThumbHeight)) then
      begin
        if (srcImage.Width = 0) or (srcImage.Height = 0) then
        begin
          ww := ThumbWidth;
          hh := ThumbHeight;
        end
        else
        begin
          IEGetFitResampleSizeWithAutoCrop(srcImage.width, srcImage.height, ThumbWidth, ThumbHeight, wDummy, hDummy, fThumbnailClipping, ww, hh);
        end;

        if (srcImage.Width > ww) or (srcImage.Height > hh) then
        begin
          bmp := TIEBitmap.Create;
          try
            bmp.allocate(ww, hh, ie24RGB);

            if (srcImage is TIEBitmap) and (srcImage as TIEBitmap).HasAlphaChannel then
              _IESetAlpha0Color(srcImage as TIEBitmap, CreateRGB(128, 128, 128));
            if srcImage.pixelformat = ie1g then
            begin
              _SubResample1bitFilteredEx(srcImage, 0, 0, srcImage.width - 1, srcImage.height - 1, bmp)
            end
            else
            begin
              if srcImage.PixelFormat <> ie24RGB then
              begin
                bmp.PixelFormat := srcImage.PixelFormat;
                _IEBmpStretchEx(srcImage, bmp, nil, nil);
              end
              else
              if (fThumbnailResampleFilter = rfNone) then
              begin
                bmp.PixelFormat := srcImage.PixelFormat;
                _IEBmpStretchEx(srcImage, bmp, nil, nil);
              end
              else
                _ResampleEx(srcImage, bmp, nil, fThumbnailResampleFilter, nil, nil);
            end;     

            if (srcImage is TIEBitmap) and (srcImage as TIEBitmap).HasAlphaChannel then
            begin
              if fThumbnailResampleFilter = rfNone then
                _Resampleie8g((srcImage as TIEBitmap).AlphaChannel, bmp.AlphaChannel, rfFastLinear)
              else
                _Resampleie8g((srcImage as TIEBitmap).AlphaChannel, bmp.AlphaChannel, fThumbnailResampleFilter);
              bmp.AlphaChannel.Full := (srcImage as TIEBitmap).AlphaChannel.Full;
            end;

            PIEImageInfo(fImageInfo[idx])^.image := fImageList.AddIEBitmap(bmp);
          finally
            FreeAndNil(bmp);
          end;
        end
        else
          PIEImageInfo(fImageInfo[idx])^.image := fImageList.AddIEBitmap(srcImage);

      end
      else
      begin
        PIEImageInfo(fImageInfo[idx])^.image := fImageList.AddIEBitmap(srcImage);
      end;
      fImageList.SetImageOriginalWidth(PIEImageInfo(fImageInfo[idx])^.image, srcImage.Width);
      fImageList.SetImageOriginalHeight(PIEImageInfo(fImageInfo[idx])^.image, srcImage.Height);
    end;

    if idx = fSelectedItem then
    begin
      fSelectedBitmap := nil;
      //fSelectedBitmap := GetBitmap(fSelectedItem);
      CallBitmapChangeEvents;
    end;
  end;
end;

{!!
<FS>TImageEnMView.SetImage

<FM>Declaration<FC>
procedure SetImage(idx: Integer; srcImage: TBitmap);
procedure SetImage(idx: Integer; width, height: Integer; PixelFormat: <A TIEPixelFormat>);

<FM>Description<FN>
Sets the image assigned to index, <FC>idx<FN>. The <FC>srcImage<FN> bitmap is copied internally; therefore you can free <FC>srcImage<FN> after calling SetImage.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('000.tif');
ImageEnMView1.InsertImageEx(0);
ImageEnMView1.SetImage(0, ImageEnView1.Bitmap);
!!}
procedure TImageEnMView.SetImage(idx: Integer; srcImage: TBitmap);
begin
  SetImageEx(idx, srcImage);
  ClearImageCache(idx);
  UpdateEx(false);
end;

procedure TImageEnMView.SetImage(idx: Integer; width, height: Integer; PixelFormat: TIEPixelFormat);
var
  temp: TIEBitmap;
begin
  temp := TIEBitmap.Create(Width, Height, PixelFormat);
  try
    SetIEBitmap(idx, temp);
  finally
    temp.free;
  end;
end;


{!!
<FS>TImageEnMView.SetImageRect

<FM>Declaration<FC>
procedure SetImageRect(idx: Integer; srcImage: TBitmap; x1, y1, x2, y2: Integer);
procedure SetImageRect(idx: Integer; srcImage: <A TIEBitmap>; x1, y1, x2, y2: Integer);

<FM>Description<FN>
SetImageRect sets the image assigned to index, <FC>idx<FN>.

The rectangle <FC>x1, y1, x2, y2<FN> of <FC>srcImage<FN> bitmap is copied internally. After calling SetImageRect you can free the <FC>srcImage<FN> bitmap.
!!}
// creates a new copy of "srcImage" (then srcImage can be freed)
procedure TImageEnMView.SetImageRect(idx: integer; srcImage: TBitmap; x1, y1, x2, y2: integer);
begin
  if idx < fImageInfo.Count then
  begin
    x1 := imin(srcImage.width - 1, x1);
    y1 := imin(srcImage.height - 1, y1);
    x2 := imin(srcImage.width - 1, x2);
    y2 := imin(srcImage.height - 1, y2);
    if PIEImageInfo(fImageInfo[idx])^.image <> nil then
    begin
      fImageList.Delete(PIEImageInfo(fImageInfo[idx])^.image);
      ClearImageCache(idx);
    end;
    PIEImageInfo(fImageInfo[idx])^.image := fImageList.AddBitmapRect(srcImage, x1, y1, x2 - x1 + 1, y2 - y1 + 1);
    fImageList.SetImageOriginalWidth(PIEImageInfo(fImageInfo[idx])^.image, srcImage.Width);
    fImageList.SetImageOriginalHeight(PIEImageInfo(fImageInfo[idx])^.image, srcImage.Height);
    if idx = fSelectedItem then
    begin
      //fSelectedBitmap := GetBitmap(fSelectedItem);
      fSelectedBitmap := nil;
      CallBitmapChangeEvents;
    end;
    ClearImageCache(idx);
    UpdateEx(false);
  end;
end;

// creates a new copy of "srcImage" (then srcImage can be freed)
procedure TImageEnMView.SetImageRect(idx: integer; srcImage: TIEBitmap; x1, y1, x2, y2: integer);
var
  temp: TIEBitmap;
begin
  temp := TIEBitmap.Create;
  try
    temp.Allocate(x2-x1+1, y2-y1+1, srcImage.PixelFormat);
    srcImage.CopyRectTo(temp, x1, y1, 0, 0, x2-x1+1, y2-y1+1);
    if srcImage.HasAlphaChannel then
    begin
      srcImage.AlphaChannel.CopyRectTo(temp.AlphaChannel, x1, y1, 0, 0, x2-x1+1, y2-y1+1);
      temp.AlphaChannel.SyncFull;
    end;
    SetIEBitmap(idx, temp);
  finally
    temp.free;
  end;
end;



{!!
<FS>TImageEnMView.ImageAtPos

<FM>Declaration<FC>
function ImageAtPos(x, y: Integer; checkBounds: Boolean = true): Integer;

<FM>Description<FN>
Use ImageAtPos to determine which image exists at the specified location within the control.
The <FC>x, y<FN> parameters specify the position in "client coordinates".
If <FC>checkBounds<FN> is True, thumbnail bounds are checked (Setting to False is useful for finding the nearest thumbnail).

If there is no thumbnail at the specified position, ImageAtPos returns -1.


<FM>Example<FC>
// Display the file name in the status bar for the image under the cursor
procedure TForm1.ImageEnMView1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  // Note: ImageFileName[] will return '' if -1 is passed
  StatusBar1.SimpleText := ImageEnMView1.ImageFileName[ ImageEnMView1.ImageAtPos(x, y) ];
end;

!!}
// returns image index at x, y position
// -1 no image
// Friendly wrapper for ImageAtPosEx
function TImageEnMView.ImageAtPos(x, y: integer; checkBounds: Boolean): Integer;
begin
  Result := ImageAtPosEx(x, y, checkBounds, True);
end;


// ImageAtPos with bRelativeToWindow
function TImageEnMView.ImageAtPosEx(x, y: integer; checkBounds: Boolean; bRelativeToWindow : Boolean): integer;
var
  ix, iy: integer;
  x1, y1, x2, y2: integer;
  info: PIEImageInfo;
begin
  if fDisplayMode = mdSingle then
  begin
    // SINGLE VIEW
    result := fFrame;
  end
  else
  if assigned(fAnimation) then
  begin
    // ANIMATION
    Result := fAnimation.FindImageAt(x, y);
  end
  else
  begin
    // GRID

    if bRelativeToWindow then
    begin
      ix := imin( (x + fViewX - _GetHorizMargin) div (fHorizBorder + ThumbWidth), CalcGridWidth()-1 );
      iy := imin( (y + fViewY - _GetVertMargin) div (fVertBorder + ThumbHeight), CalcGridHeight()-1 );
    end
    else
    begin
      ix := imin( (x - _GetHorizMargin) div (fHorizBorder + ThumbWidth), CalcGridWidth()-1 );          
      iy := imin( (y - _GetVertMargin) div (fVertBorder + ThumbHeight), CalcGridHeight()-1 );
    end;

    if fCurrentGridWidth = 0 then
      iy := 0;

    result := ImageAtGridPos(iy, ix);

    if result >= ImageCount then
    begin
      if iemoRegion in fMultiSelectionOptions then
        result := ImageCount-1
      else
        result := -1;
    end;

    if checkBounds and (result >= 0) then
    begin
      // verify if inside the thumbnail rectangle
      info := PIEImageInfo(fImageInfo[result]);
      if bRelativeToWindow then
      begin
        x1 := info^.X - fViewX;
        y1 := info^.Y - fViewY;
      end
      else     
      begin
        x1 := info^.X;
        y1 := info^.Y;
      end;
      x2 := x1 + ThumbWidth - 1;
      y2 := y1 + ThumbHeight;
      if not IEPointInRect(x, y, x1, y1, x2, y2) then
        result := -1;
    end;
  end;
end;


function TImageEnMView.ImageAtPosWithCheckEvent(x, y: integer; checkBounds: Boolean; bRelativeToWindow : Boolean): integer;
begin
  result := ImageAtPosEx(x, y, checkBounds, bRelativeToWindow);
  if assigned(fOnImageAtPos) then
    fOnImageAtPos(self, result, x, y);
end;


{!!
<FS>TImageEnMView.ImageAtGridPos

<FM>Declaration<FC>
function ImageAtGridPos(row, col: Integer): Integer;

<FM>Description<FN>
ImageAtGridPos returns the index of the image at row, col position, e.g. ImageAtGridPos(0, 0) would return the top-left image (i.e. 0).
!!}
function TImageEnMView.ImageAtGridPos(row, col: integer): integer;
begin
  result := row * CalcGridWidth() + col;
end;


{!!
<FS>TImageEnMView.InsertingPoint

<FM>Declaration<FC>
function InsertingPoint(x, y: Integer): Integer;

<FM>Description<FN>
Returns the index of the image before or after the <FC>x, y<FN> position. It is useful when an image needs to be inserted at a particular cursor position.

<FM>Example<FC>
// this drag/drop event copy all selected images of ImageEnMView1 to ImageEnMView2, starting at X, Y mouse position
procedure TForm1.ImageEnMView2DragDrop(Sender, Source: TObject; X, Y: Integer);
var
   i: integer;
   idx, im: integer;
   tmpbmp: TBitmap;
begin
   im := ImageEnMView2.InsertingPoint(X, Y);
   for i := 0 to ImageEnMView1.MultiSelectedImagesCount-1 do
   begin
      idx := ImageEnMView1.MultiSelectedImages[i];
      tmpbmp := ImageEnMView1.GetBitmap( idx );
      ImageEnMView2.InsertImage(im);
      ImageEnMView2.SetImage(im, tmpbmp);
      inc(im);
      ImageEnMView1.ReleaseBitmap( idx );
   end;
end;
!!}
// return =fImageInfo.Count if after last thumbnail
function TImageEnMView.InsertingPoint(x, y: integer): integer;
var
  ix, iy, gw: integer;
begin
  ix := (x + fViewX - _GetHorizMargin) div (fHorizBorder + ThumbWidth);
  iy := (y + fViewY - _GetVertMargin) div (fVertBorder + ThumbHeight);   
  gw := CalcGridWidth();
  if (gw > 0) and (ix > gw) then
    ix := gw;
  result := imin(iy * gw + ix, fImageInfo.Count); // not fImageInfo.Count-1 !!
end;

procedure TImageEnMView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  sidx: Integer;
  lSelectInclusive: boolean;
  lMultiSelecting: boolean;
  idx: Integer;
begin
  inherited;
  fUserAction := true;
  try
    if fDoubleClicking then
    begin
      fDoubleClicking := false;
      exit;
    end;
    {$IFDEF OCXVERSION}
    SetFocus();
    {$ELSE}
    if CanFocus then
      Windows.SetFocus(Handle);
    {$ENDIF}
    fHSX1 := X;
    fHSY1 := Y;
    fHSVX1 := ViewX;
    fHSVY1 := ViewY;
    fHSIDX := ImageAtPosWithCheckEvent(x, y, fCheckThumbBoundsOnSelect);
    fHSIDXSelected := IsSelected(ImageAtPosWithCheckEvent(x, y, True));
    fSelectIdxOnMouseUp := -1;
    if fHSIDXSelected and ((ssCtrl in Shift) = False) and ((ssShift in Shift) = False) then 
      fSelectIdxOnMouseUp := fHSIDX;
    fLHSIDX := fHSIDX;
    FLastRegionIdx := ImageAtPosEx(x, y, True, False);
    fHaveMultiselected := false;
    fMDown := true;
    fChangedSel := false;

    if (assigned(fAnimation) = False) and (iemoSelectOnRightClick in fMultiSelectionOptions) and (Button = mbRight) then
    begin
      // Make functionality much closer to Windows Explorer
      idx := ImageAtPosWithCheckEvent(x, y, True);
      if (idx >= 0) and (IsSelected(idx) = False) then
      begin
        Button := mbLeft;
        Shift := [];
      end;
    end;

    if assigned(fAnimation) and (Button = mbLeft) then
    begin
      // animation mode

      // show an image clicking on it
      sidx := fAnimation.FindImageAt(X, Y);
      if sidx > -1 then
      begin
        fAnimation.CurrentImage := sidx;
        SelectedImage := fAnimation.CurrentImage;
      end;

      // start scrollbar slider dragging
      if fAnimation.ShowScrollbar then
      begin
        if fAnimation.IsInsideScrollbarSlider(X, Y) then
          fAnimationDraggingSlider := true
        // directly move to the clicked scrollbar position
        else
        if fAnimation.IsInsideScrollbar(X, Y) then
        begin
          fAnimation.MoveScrollbarSliderTo(X);
          SelectedImage := fAnimation.CurrentImage;
        end;
      end;

    end
    else
    if ((iemoSelectOnMouseUp in fMultiSelectionOptions) = False) and (Button = mbLeft) and (CheckboxAtPos(X, Y) > -1) then
    begin
      ClickCheckboxAtPos(X, Y);
    end
    else
    if not (iemoSelectOnMouseUp in fMultiSelectionOptions) and (Button = mbLeft) and (mmiSelect in fMouseInteract) then
    begin
      // select on mouse down
      sidx := ImageAtPosWithCheckEvent(x, y, fCheckThumbBoundsOnSelect);

      if CheckSelectionChangingAllowed = False then
      begin
        // Do nothing
      end
      else
      if (ssShift in Shift) and fEnableMultiSelect then
      begin
        sidx := fPriorHSIDX;
        if sidx < 0 then
          sidx := 0;
        lSelectInclusive := fSelectInclusive;
        lMultiSelecting := fMultiSelecting;
        fMultiSelecting := false;
        fSelectInclusive := true;
        DeselectNU;
        SetSelectedItemNU(sidx);
        fMultiSelecting := lMultiSelecting;
        SelectAtPos(X, Y, [ssShift]);
        fSelectInclusive := lSelectInclusive;
        fHaveMultiselected := true;
      end
      else
      if not IsSelected(sidx) then
      begin
        fPriorHSIDX := sidx;
        SelectAtPos(X, Y, Shift);
      end
      else
      begin
        fPriorHSIDX := sidx;
        if (ssCtrl in Shift) and (iemoOptimizeForDragging in fMultiSelectionOptions) and (MultiSelectedImagesCount > 1) then
          UnSelectImage(sidx);
      end;
    end;

  finally
    fUserAction := false;
    DoAfterEvent(ieaeMouseDown);
  end;
end;

function TImageEnMView.GetHintStr(idx : Integer) : string;

  procedure _AddLine(ACaption : TMsgLanguageWords; sText : string);
  begin
    sText := ReplaceIEMConsts(sText, idx);
    if sText <> '' then
      Result := Result + #13#10 + iemsg(ACaption) + ': ' + sText;
  end;

var
  sFilename: WideString;
begin
  Result := '';
  
  sFilename := ImageFilename[idx];
  if sFilename = '' then
    exit;  // invalid file

  if GetImageType(idx) = ieftFolder then
  begin        
    Result := iemsg(IEMSG_Folder) + ': ' + sFilename;
  end
  else
  begin
    Result := iemsg(IEMSG_FILE) + ': ' + IEExtractFileNameW(sFilename);
    _AddLine(IEMSG_Folder, IEM_FilePath);
    _AddLine(IEMSG_Dimensions, IEM_ImageDimensions);
    _AddLine(IEMSG_Type, IEM_FileType); 
    _AddLine(IEMSG_SIZE, IEM_FileSize);
  end;

  _AddLine(IEMSG_Created, IEM_FileCreateDate);
  _AddLine(IEMSG_Modified, IEM_FileEditDate);
end;


procedure TImageEnMView.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  Scroll_Area_Size   = 20;
  Auto_Scroll_Amount = 10;          
  Mouse_Move_Threshold  = 10;
  Proportional_Scroll_Divisor = 3;  
  Drag_Scroll_Area   = 20;
var
  lSelectInclusive: boolean;
  lMultiSelecting: boolean;
  idx: integer;
  iNewX, iNewY: integer;
  iRgnIdx: integer;
  AutoScrollProp: integer;
  sCurrentHint: string;
  bCanSelect: Boolean;
begin
  inherited;                                 
  sCurrentHint := Hint;
  fUserAction := true;
  fLastMouseMoveX := X;
  fLastMouseMoveY := Y;
  try
    if assigned(fAnimation) then
    begin
      // animation mode
      if fAnimation.ShowScrollbar then
      begin
        // change mouse cursor (hand when inside the scrollbar slider, default otherwise)
        if fAnimation.IsInsideScrollbarSlider(X, Y) then
          Cursor := crHandPoint
        else
          Cursor := crDefault;

        // dragging scrollbar slider
        if MouseCapture and fAnimationDraggingSlider then
        begin
          fAnimation.MoveScrollbarSliderTo(X);
          SelectedImage := fAnimation.CurrentImage;
        end;
      end;
    end
    else
    begin     
      bCanSelect := (fHSIDXSelected = False) or not (iemoOptimizeForDragging in fMultiSelectionOptions);
      
      // default mode
      if MouseCapture then
      begin
        if (mmiSelect in fMouseInteract) and fEnableMultiSelect then
        begin
          idx     := ImageAtPosEx(fViewX + x, fViewY + y, False, False);
          iRgnIdx := ImageAtPosEx(fViewX + x, fViewY + y, True, False);

          if bCanSelect and (CheckSelectionChangingAllowed = False) then
            bCanSelect := False;

          if bCanSelect then
            if ((idx <> fLHSIDX) and (idx <> -1)) or
               ((iemoRegion in fMultiSelectionOptions) and (iRgnIdx <> FLastRegionIdx)) then
            begin     
              fLHSIDX        := idx;
              FLastRegionIdx := iRgnIdx;
              idx            := fHSIDX;
              if idx < 0 then
                idx := 0;

              lSelectInclusive := fSelectInclusive;
              lMultiSelecting  := fMultiSelecting;
              fMultiSelecting  := (ssCtrl in Shift);
              fSelectInclusive := true;

              if not fMultiSelecting then
                DeselectNU();
              SetSelectedItemNU(idx);
              fMultiSelecting := lMultiSelecting;
              SelectAtPos(X, Y, [ssShift]);
              fSelectInclusive   := lSelectInclusive;
              fHaveMultiselected := true;
            end;

          // Automatically scroll selection
          if (mmiScroll in fMouseInteract) = False then
          begin
            iNewX := fViewX;
            iNewY := fViewY;

            AutoScrollProp := Auto_Scroll_Amount;

            if Y < 0 then
              AutoScrollProp := imax(Abs(Y) div Proportional_Scroll_Divisor, AutoScrollProp)
            else
              AutoScrollProp := imax(imax(0, Y-ClientHeight) div 2, AutoScrollProp);
            if X < 0 then
              AutoScrollProp := imax(Abs(X) div Proportional_Scroll_Divisor, AutoScrollProp)
            else
              AutoScrollProp := imax(imax(0, X-ClientWidth) div 2, AutoScrollProp);


            if (Abs(fHSY1 - Y) > Mouse_Move_Threshold) and (Y < Scroll_Area_Size) and (fViewY > 0) then
              Dec(iNewY, AutoScrollProp)
            else
            if (Abs(fHSY1 - Y) > Mouse_Move_Threshold) and (Y > ClientHeight - Scroll_Area_Size) and (ClientHeight < fVHeight) and (fViewY < fVHeight) then
              Inc(iNewY, AutoScrollProp);

            if (Abs(fHSX1 - X) > Mouse_Move_Threshold) and (X < Scroll_Area_Size) and (fViewX > 0) then
              Dec(iNewX, AutoScrollProp)
            else
            if (Abs(fHSX1 - X) > Mouse_Move_Threshold) and (X > ClientWidth - Scroll_Area_Size) and (ClientWidth < fVWidth) and (fViewX < fVWidth) then
              Inc(iNewX, AutoScrollProp);

            if (iNewX <> fViewX) or (iNewY <> fViewY) then
            begin
              SetViewXY(iNewX, iNewY);
              ViewChange(0);
              PostMessage(Handle, IEM_AUTOSCROLL, 0, 0);

              if fTrackMouseSelection = False then
                Paint();
            end;
          end;

          if fTrackMouseSelection and bCanSelect then
          begin
            fDrawMouseSelection := true;
            Paint();
          end;
        end;
        if mmiScroll in fMouseInteract then
        begin
          if ((X - fHSx1)<>0) or ((Y - fHSy1)<>0) then
          begin
            SetViewXY(fHSVX1 - (X - fHSx1), fHSVY1 - (Y - fHSy1));
            ViewChange(0);
          end;
        end;
      end    // If MouseCapture...
      else
      if fShowThumbnailHint and (DisplayMode <> mdSingle) then
      begin
        idx := ImageAtPosEx(fViewX + x, fViewY + y, True, False);
        sCurrentHint := GetHintStr(idx);
      end;

      // DRAW CHECKBOX IF HOVERING OVER THE MOUSE
      if fHoverCheckLastPos.X = -1 then
        fHoverCheckLastPos := Point(X, Y);
      if (fCheckboxes = iecbShowOnHover) and
         (fLockPaint <= 0) and
         ((abs(fHoverCheckLastPos.X - X) > Mouse_Move_Threshold) or (abs(fHoverCheckLastPos.Y - Y) > Mouse_Move_Threshold)) then
      begin
        idx := ImageAtPosEx(fViewX + x, fViewY + y, True, False);
        if idx <> fHoverCheckLastIdx then
        begin
          if fHoverCheckLastIdx > -1 then
            _UpdateImage(fHoverCheckLastIdx, True);
          fHoverCheckLastIdx := idx;
          DrawCheckbox(Canvas, idx, True, True);
        end;
      end;

    end;

    if (sCurrentHint <> Hint) then
    begin
      Application.CancelHint;
      Hint := sCurrentHint;
    end;

  finally
    fUserAction := false;
  end;
end;

procedure TImageEnMView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
  bCanSelect: Boolean;
begin
  inherited;
  fUserAction := true;
  try
    if fTrackMouseSelection then
    begin
      fDrawMouseSelection := false;
      Paint();
    end;

    if not fMDown then
      exit;
    fMDown := false;

    if fDragging then
      exit;
    if fPlaying then
      exit;

    if (assigned(fAnimation) = False) and (iemoSelectOnRightClick in fMultiSelectionOptions) and (Button = mbRight) then
    begin
      // Make functionality much closer to Windows Explorer
      idx := ImageAtPosWithCheckEvent(x, y, True);
      if (idx >= 0) and (IsSelected(idx) = False) then
      begin
        Button := mbLeft;
        Shift := [];
      end;
    end;

    if Button = mbLeft then
    begin
      if assigned(fAnimation) then
      begin
        // animation mode
        fAnimationDraggingSlider := false;
      end
      else
      begin
        if (iemoSelectOnMouseUp in fMultiSelectionOptions) and (CheckboxAtPos(X, Y) > -1) then
        begin
          ClickCheckboxAtPos(X, Y);
        end
        else
        if (iemoSelectOnMouseUp in fMultiSelectionOptions) and (not fHaveMultiselected) and (mmiSelect in fMouseInteract) then
        begin
          // we select on "mouse up"
          if CheckSelectionChangingAllowed then
            SelectAtPos(X, Y, Shift);
        end;

        if not (iemoSelectOnMouseUp in fMultiSelectionOptions) and (fChangedSel = False) and (CheckboxAtPos(X, Y) = -1) then
        begin    
          bCanSelect := (fHSIDXSelected = False) or not (iemoOptimizeForDragging in fMultiSelectionOptions);

          if bCanSelect and (CheckSelectionChangingAllowed = False) then
          begin
            // Do nothing
          end
          else
          // when we select on "mouse down", there is one case where we still need to select on mouse up
          if bCanSelect then
            SelectAtPos(X, Y, Shift)
          else
          if fSelectIdxOnMouseUp > -1 then
          begin
            DeselectNU;
            SelectedImage := fSelectIdxOnMouseUp;
          end;
        end;
        if (mmiScroll in fMouseInteract) then
          SetViewXY(fHSVX1 - (X - fHSx1), fHSVY1 - (Y - fHSy1));
      end;
    end;
  finally
    fUserAction := false;
    DoAfterEvent(ieaeMouseUp);
  end;
end;

procedure TImageEnMView.SelectAtPos(X, Y: integer; Shift: TShiftState);
var
  idx, q, col, row: integer;
  lMultiSelecting: boolean;
  row1, row2, col1, col2: integer;
  x1, y1, x2, y2: integer;
  incx, incy: integer;
begin
  // find the image where user has clicked (put in fSelectedItem)
  lMultiSelecting := fMultiSelecting;
  if (ssCtrl in Shift) or (ssShift in Shift) then
    fMultiSelecting := true;

  if (iemoRegion in fMultiSelectionOptions) and ((fHSX1 <> X) or (fHSY1 <> Y)) then
  begin
    if not fMultiSelecting then
      DeselectNU();

    x1 := imin(fHSVX1 + fHSX1, ViewX + X);
    y1 := imin(fHSVY1 + fHSY1, ViewY + Y);
    x2 := imax(fHSVX1 + fHSX1, ViewX + X);
    y2 := imax(fHSVY1 + fHSY1, ViewY + Y);

    // Check minimum bounds
    x1 := imax(x1, 0);
    y1 := imax(y1, 0);


    row := y1;
    while row <= y2 do
    begin
      incy := 0;
      col := x1;
      while col <= x2 do
      begin
        q := ImageAtPosWithCheckEvent(col, row, True, False);
        if (q <> -1) and (fMultiSelectedImages.IndexOf(pointer(q)) = -1) then
          SetSelectedItemNU(q);
        incx := 0;
        if q <> -1 then
        begin
          incx := ImageX[q] + ThumbWidth + fHorizBorder;
          incy := ImageY[q] + ThumbHeight + fVertBorder;
        end;
        col := imax(col+1, incx);
      end;
      row := imax(row+1, incy);
    end;
    if fVisibleSelection then
      UpdateEx(false);
    CallBitmapChangeEvents;
  end
  else
  begin
    idx := ImageAtPosWithCheckEvent(x, y, fCheckThumbBoundsOnSelect);
    if (idx >= 0) then
    begin
      if fEnableMultiSelect and ((idx <> fSelectedItem) or fMultiSelecting) then
      begin
        if ssShift in Shift then
        begin
          // SHIFT pressed, select range
          if iemoRegionOnShift in fMultiSelectionOptions then
          begin
            row1 := ImageRow[fSelectedItem];
            row2 := ImageRow[idx];
            col1 := ImageCol[fSelectedItem];
            col2 := ImageCol[idx];
            if row1 > row2 then
              iswap(row1, row2);
            if col1 > col2 then
              iswap(col1, col2);
            for row := row1 to row2 do
              for col := col1 to col2 do
              begin
                q := ImageAtGridPos(row, col);
                if (q <> fSelectedItem) and (q <> idx) and (fMultiSelectedImages.IndexOf(pointer(q)) = -1) then
                  SetSelectedItemNU(q);
              end;
          end
          else
          begin
            if fSelectedItem < idx then
            begin
              for q := fSelectedItem + 1 to idx - 1 do
                if fMultiSelectedImages.IndexOf(pointer(q)) = -1 then
                  SetSelectedItemNU(q);
            end
            else
            begin
              for q := fSelectedItem - 1 downto idx + 1 do
                if fMultiSelectedImages.IndexOf(pointer(q)) = -1 then
                  SetSelectedItemNU(q);
            end;
          end;
        end;
      end;
      SetSelectedItemNU(idx);
      if fVisibleSelection then
        UpdateEx(false);
      CallBitmapChangeEvents;
    end;
  end;
  fMultiSelecting := lMultiSelecting;
end;

{!!
<FS>TImageEnMView.VisibleSelection

<FM>Declaration<FC>
property VisibleSelection: boolean;

<FM>Description<FN>
When enabled the selected frame is marked. Otherwise selected and unselected frames look identical.

!!}
procedure TImageEnMView.SetVisibleSelection(v: boolean);
begin
  fVisibleSelection := v;
  Update;
end;


{!!
<FS>TImageEnMView.Checkboxes

<FM>Declaration<FC>
property Checkboxes: <A TIEMCheckboxType>;

<FM>Description<FN>
Displays a checkbox in each thumbnail box to provide an alternative method for users to select files (as opposed to <L TImageEnMView.EnableMultiSelect>multiple selection</L>).

Notes:
- Use <A TImageEnMView.CheckedCount> to return the number of checked items
- Read/write the checked status of each file using <A TImageEnMView.Checked>
- Set checked status en masse using <A TImageEnMView.CheckAll> or <A TImageEnMView.UncheckAll>
- Change the position and style of checkboxes with <A TImageEnMView.CheckBoxPos> and <A TImageEnMView.SetCheckboxParams>
- Access the event, <A TImageEnMView.OnCheckboxClick>, to determine when the user clicks a checkbox

<FM>Example<FC>
// Copy all checked files to D:\Dest\
for I := 0 to ImageEnMView1.ImageCount - 1 do
  if ImageEnMView1.Checked[I] then
    WindowsCopy(Handle, ImageEnMView1.ImageFilename[I], 'D:\Dest\', True, True);
!!}
procedure TImageEnMView.SetCheckboxes(v: TIEMCheckboxType);
// NPC: 25/09/13
begin
  if fCheckboxes <> v then
  begin
    fCheckboxes := v;
    Invalidate;
  end;
end;

{!!
<FS>TImageEnMView.CheckboxPos

<FM>Declaration<FC>
property CheckboxPos : <A TIEMCheckboxPos>;

<FM>Description<FN>
Specify the corner of the thumbnail where the <L TImageEnMView.Checkboxes>checkbox</L> is displayed.

<FM>See Also<FN>
- <A TImageEnMView.SetCheckboxParams>

<FM>Example<FC>
// Show checkboxes on bottom left when the user hovers over the thumbnail
ImageEnMView1.Checkboxes  := iecbShowOnHover;
ImageEnMView1.CheckboxPos := iecpBottomLeft;
!!}
procedure TImageEnMView.SetCheckboxPos(v: TIEMCheckboxPos);
// NPC: 23/06/14
begin
  if fCheckboxPos <> v then
  begin
    fCheckboxPos := v;
    Invalidate;
  end;
end;


{!!
<FS>TImageEnMView.CheckedCount

<FM>Declaration<FC>
function CheckedCount : Integer;

<FM>Description<FN>
Returns the number of items that have been checked (if <A TImageEnMView.Checkboxes> have been enabled).

<FM>Example<FC>
// Note: Use OnClick, not OnCheckboxClick if need to read CheckedCount
procedure TfMain.ImageEnMView1Click(Sender: TObject);
begin
  // Display the current count of checked items in the status bar
  Statusbar1.SimpleText := 'Checked: ' + IntToStr(ImageEnMView1.CheckedCount);
end;

<FM>See Also<FN>
- <A TImageEnMView.Checked>
!!}
function TImageEnMView.CheckedCount : Integer;
// NPC: 25/09/13
var
  i: Integer;
begin
  if fCheckedCount < 0 then
  begin
    fCheckedCount := 0;
    for i := 0 to ImageCount - 1 do
      if Checked[i] then
        inc(fCheckedCount);
  end; 
  Result := fCheckedCount;
end;
                           
{!!
<FS>TImageEnMView.Checked

<FM>Declaration<FC>
property Checked[index : Integer] : Boolean;

<FM>Description<FN>
Sets the checked status for the item of the specified index (if <A TImageEnMView.Checkboxes> have been enabled).

<FM>Example<FC>
// Copy all checked files to D:\Dest\
for I := 0 to ImageEnMView1.ImageCount - 1 do
  if ImageEnMView1.Checked[I] then
    WindowsCopy(Handle, ImageEnMView1.ImageFilename[I], 'D:\Dest\', True, True);

<FM>See Also<FN>
- <A TImageEnMView.CheckedCount>
!!}
function TImageEnMView.GetChecked(index: integer): Boolean;
// NPC: 25/09/13
begin
  if (index >= 0) and (index < fImageInfo.Count) then
    result := PIEImageInfo(fImageInfo[index])^.Checked
  else
    result := False;
end;

procedure TImageEnMView.SetChecked(index: integer; v : Boolean);
// NPC: 25/09/13
var
  bOldChecked: Boolean;
begin
  if (index >= 0) and (index < fImageInfo.Count) then
  begin
    bOldChecked := PIEImageInfo(fImageInfo[index])^.Checked;
    if bOldChecked <> v then
    begin
      PIEImageInfo(fImageInfo[index])^.Checked := v;
      if fCheckedCount > -1 then
      begin
        if v then
          inc(fCheckedCount)
        else
          dec(fCheckedCount);
      end;
      DrawCheckbox(Canvas, Index, True, True);
    end;
  end;  
end;
         

{!!
<FS>TImageEnMView.SetCheckboxParams

<FM>Declaration<FC>
procedure SetCheckboxParams(iHorzMargin, iVertMargin : Integer; CustomCheckedImage : TBitmap = nil; CustomUncheckedImage : TBitmap = nil);

<FM>Description<FN>
Specifies the position and style of checkboxes (if <A TImageEnMView.Checkboxes> have been enabled).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>iHorzMargin<FN></C> <C>The position from the left (iecpTopLeft or iecpBottomLeft) or right (iecpTopRight or iecpBottomRight) of the thumbnail where the checkbox is positioned. Default: 4</C> </R>
<R> <C><FC>iVertMargin<FN></C> <C>The position from the top (iecpTopLeft or iecpTopRight) or bottom (iecpBottomLeft or iecpBottomRight) of the thumbnail where the checkbox is positioned. Default: 4</C> </R>
<R> <C><FC>CustomCheckedImage<FN></C> <C>If specified, it is the image that will be shown as the "checked" box. If <FC>nil<FN> the Windows standard checkbox image is used</C> </R>
<R> <C><FC>CustomUncheckedImage<FN></C> <C>If specified, it is the image that will be shown as the "unchecked" box. If <FC>nil<FN> the Windows standard checkbox image is used</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TImageEnMView.CheckboxPos>

<FM>Example<FC>
// Load custom checkbox images from file and position them at the very top left
CheckBmp := TBitmap.create;
UncheckBmp := TBitmap.create;
try
  CheckBmp.LoadFromFile('C:\Images\Checked.bmp');
  UncheckBmp.LoadFromFile('C:\Images\Unchecked.bmp');
  ImageEnMView1.SetCheckboxParams(0, 0, CheckBmp, UncheckBmp);
  ImageEnMView1.CheckboxPos := iecpTopLeft;
  ImageEnMView1.Checkboxes := iecbAlways;
finally
  CheckBmp.free;
  UncheckBmp.free;
end;
!!}
procedure TImageEnMView.SetCheckboxParams(iHorzMargin, iVertMargin : Integer; CustomCheckedImage : TBitmap = nil; CustomUncheckedImage : TBitmap = nil);
// NPC: 25/09/13
begin
  fCheckboxMargins := Point(iHorzMargin, iVertMargin);

  if Assigned(CustomCheckedImage) = False then
    FreeAndNil(fCheckedBitmap)  // default thumbnail will be assigned at draw
  else
  begin
    if fCheckedBitmap = nil then
      fCheckedBitmap := TBitmap.create;
    fCheckedBitmap.assign(CustomCheckedImage)
  end;

  if Assigned(CustomUncheckedImage) = False then
    FreeAndNil(fUncheckedBitmap)  // default thumbnail will be assigned at draw
  else
  begin
    if fUncheckedBitmap = nil then
      fUncheckedBitmap := TBitmap.create;
    fUncheckedBitmap.assign(CustomUncheckedImage)
  end;

  Invalidate;
end;

{!!
<FS>TImageEnMView.CheckAll

<FM>Declaration<FC>
procedure CheckAll;

<FM>Description<FN>
Marks the checkbox status for all files to "Checked" (if <A TImageEnMView.Checkboxes> have been enabled).
!!}
procedure TImageEnMView.CheckAll;
var
  I: Integer;
begin
  LockPaint;
  try
    for I := 0 to ImageCount - 1 do
      Checked[i] := True;
  finally
    NPUnLockPaint;
    Invalidate;
  end;
end;


       
{!!
<FS>TImageEnMView.UncheckAll

<FM>Declaration<FC>
procedure UncheckAll;

<FM>Description<FN>
Marks the checkbox status for all files to "Unchecked" (if <A TImageEnMView.Checkboxes> have been enabled).
!!}
procedure TImageEnMView.UncheckAll;
var
  I: Integer;
begin
  LockPaint;
  try
    for I := 0 to ImageCount - 1 do
      Checked[i] := False;
  finally
    NPUnLockPaint;
    Invalidate;
  end;
end;



function TImageEnMView.ThumbToCheckboxRect(ThumbRect : TRect; bRelativeToView : Boolean = False) : TRect; 
var
  x1, y1 : integer;
begin
  if fCheckboxPos in [iecpTopLeft, iecpBottomLeft] then
    // Left Aligned
    x1 := ThumbRect.Left + fCheckboxMargins.X
  else
    // Right aligned
    x1 := ThumbRect.Right - fCheckedBitmap.Width - fCheckboxMargins.X;
                         
  if fCheckboxPos in [iecpTopLeft, iecpTopRight] then
    // Top Aligned
    y1 := ThumbRect.Top + fCheckboxMargins.Y
  else
    // Bottom aligned
    y1 := ThumbRect.Bottom - fCheckedBitmap.Height - fCheckboxMargins.Y;
    
  if bRelativeToView then
  begin
    x1 := x1 - ViewX;
    y1 := y1 - ViewY;
  end;

  Result := Rect(x1, y1, x1 + fCheckedBitmap.Width, y1 + fCheckedBitmap.Height);
end;




procedure DrawCheckBoxToBitmap(Handle: THandle; DestBitmap: TBitmap; bChecked: Boolean);
var        
{$IFDEF Delphi7orNewer}  // From Delphi 7
  h: HTHEME;
{$EndIf}
  r: TRect;
  s: TSize;
begin
{$IFDEF Delphi7orNewer}  // From Delphi 7
  if UseThemes then
  begin
    h := OpenThemeData(Handle, 'BUTTON');
    if h <> 0 then
    try
      GetThemePartSize(h,
                       DestBitmap.Canvas.Handle,
                       BP_CHECKBOX,
                       CBS_CHECKEDNORMAL,
                       nil,
                       TS_DRAW,
                       s);

      DestBitmap.Width  := s.cx;
      DestBitmap.Height := s.cy;
      r := Rect(0, 0, s.cx, s.cy);

      if bChecked then        
        DrawThemeBackground(h,
                            DestBitmap.Canvas.Handle,
                            BP_CHECKBOX,
                            CBS_CHECKEDNORMAL,
                            r,
                            nil)
      else
        DrawThemeBackground(h,
                            DestBitmap.Canvas.Handle,
                            BP_CHECKBOX,
                            CBS_UNCHECKEDNORMAL,
                            r,
                            nil);
    finally
      CloseThemeData(h);
    end;
  end
  else  
{$EndIf}
  begin       
    s.cx := GetSystemMetrics(SM_CXMENUCHECK);
    s.cy := GetSystemMetrics(SM_CYMENUCHECK);
    DestBitmap.Width  := s.cx;
    DestBitmap.Height := s.cy;
    r := Rect(0, 0, s.cx, s.cy);
    if bChecked then        
      DrawFrameControl(DestBitmap.Canvas.Handle,
                       r,
                       DFC_BUTTON,
                       DFCS_CHECKED)
    else
      DrawFrameControl(DestBitmap.Canvas.Handle,
                       r,
                       DFC_BUTTON,
                       DFCS_BUTTONCHECK);
  end;
end;


procedure TImageEnMView.DrawCheckbox(ACanvas : TCanvas; Index : Integer; IsSelected : Boolean; bRelativeToView : Boolean = False);
var
  ThumbRect: TRect;
begin
  if Index > -1 then
  begin
    ThumbRect := Rect(ImageX[index], ImageY[index], ImageX[index] + ThumbWidth, ImageY[index] + ThumbHeight);
    DrawCheckbox(Canvas, Index, ThumbRect, IsSelected, True);
  end;
end;

procedure TImageEnMView.DrawCheckbox(ACanvas : TCanvas; Index : Integer; ThumbRect : TRect; IsSelected : Boolean; bRelativeToView : Boolean = False);
var
  CbxRect: TRect;
begin
  if (fCheckboxes = iecbNone) or (fLockPaint > 0) then
    exit;

  if fCheckedBitmap = nil then
  begin
    fCheckedBitmap := TBitmap.create;
    DrawCheckBoxToBitmap(Handle, fCheckedBitmap, True);
  end;

  if fUncheckedBitmap = nil then
  begin
    fUncheckedBitmap := TBitmap.create;
    DrawCheckBoxToBitmap(Handle, fUncheckedBitmap, False);
  end;

  CbxRect := ThumbToCheckboxRect(ThumbRect, bRelativeToView);
  if bRelativeToView then
  begin
    // check if off-screen
    if (CbxRect.Right < 0) or (CbxRect.Left > ClientWidth) or
       (CbxRect.Bottom < 0) or (CbxRect.Top > ClientHeight) then
      exit;
  end;

  if Checked[index] then
    ACanvas.Draw(CbxRect.Left, CbxRect.Top, fCheckedBitmap)
  else
  if IsSelected or (fCheckboxes = iecbAlways) then
    ACanvas.Draw(CbxRect.Left, CbxRect.Top, fUncheckedBitmap);
end;


// If x, y is within a checkbox of a thumbnail its index is returned. Otherwise -1
function TImageEnMView.CheckboxAtPos(X, Y : Integer) : Integer;
var
  ThumbRect, CbxRect: TRect;
  idx: Integer;
begin
  Result := -1;

  if (fCheckboxes = iecbNone) or (fCheckedBitmap = nil) then
    exit;

  idx := ImageAtPos(X, Y);
  if idx > -1 then
  begin
    ThumbRect := Rect(ImageX[idx], ImageY[idx], ImageX[idx] + ThumbWidth, ImageY[idx] + ThumbHeight);
    CbxRect := ThumbToCheckboxRect(ThumbRect);

    if IEPointInRect(ViewX + x, ViewY + y, CbxRect) then
      Result := idx
  end;
end;

procedure TImageEnMView.ClickCheckboxAtPos(X, Y : Integer);
var
  idx: Integer;
  bNewChecked: Boolean;
begin
  idx := CheckboxAtPos(X, Y);
  if idx > -1 then
  begin
    bNewChecked := NOT Checked[idx];
    if assigned(fOnCheckboxClick) then
      OnCheckboxClick(Self, idx, bNewChecked);
    Checked[idx] := bNewChecked;
  end;
end;



{!!
<FS>TImageEnMView.SelectionWidth

<FM>Declaration<FC>
property SelectionWidth: integer;

<FM>Description<FN>
Specifies the width (in pixels) of the border around selected frames.

Note: This value must be less than <A TImageEnMView.HorizBorder> and <A TImageEnMView.VertBorder>.

<FM>See Also<FN>
- <A TImageEnMView.SelectionColor>
- <A TImageEnMView.SelectionAntialiased>
!!}
procedure TImageEnMView.SetSelectionWidth(v: integer);
begin
  fSelectionWidth := v;
  Update;
end;

{!!
<FS>TImageEnMView.SelectionAntialiased

<FM>Declaration<FC>
property SelectionAntialiased: boolean;

<FM>Description<FN>
When enabled, the border around selected frames is drawn with antialiasing (to look smoother)   

<FM>See Also<FN>
- <A TImageEnMView.SelectionWidth>
- <A TImageEnMView.SelectionColor>
!!}
procedure TImageEnMView.SetSelectionAntialiased(v: Boolean);
begin
  fSelectionAntialiased := v;
  Update;
end;

{!!
<FS>TImageEnMView.SelectionWidthNoFocus

<FM>Declaration<FC>
property SelectionWidthNoFocus: integer;

<FM>Description<FN>
Specifies the width of the border around selected frames when the component doesn't have focus.

The default value is 1.
!!}
procedure TImageEnMView.SetSelectionWidthNoFocus(v: integer);
begin
  fSelectionWidthNoFocus := v;
  Update;
end;

{!!
<FS>TImageEnMView.SelectionColor

<FM>Declaration<FC>
property SelectionColor: TColor;

<FM>Description<FN>
Specifies the color of the selection.

<FM>See Also<FN>
- <A TImageEnMView.SelectionWidth>
- <A TImageEnMView.SelectionAntialiased>
!!}
procedure TImageEnMView.SetSelectionColor(v: TColor);
begin
  fSelectionColor := v;
  Update;
end;

{!!
<FS>TImageEnMView.BeginSelectImages

<FM>Declaration<FC>
procedure BeginSelectImages;

<FM>Description<FN>
Call BeginSelectImages and <A TImageEnMView.EndSelectImages> to select multiple images without refreshing the component's state.

Generally this will speed up the selection process.

<FM>Example<FC>
// select the first 100 images
ImageEnMView1.BeginSelectImages;
for i := 0 to 99 do
  ImageEnMView1.SelectedImage := i;
ImageEnMView1.EndSelectImages;

!!}
procedure TImageEnMView.BeginSelectImages;
begin
  DeselectNU;
  fSelectImages := true;
  fMultiSelecting := true;
end;

{!!
<FS>TImageEnMView.EndSelectImages

<FM>Declaration<FC>
procedure EndSelectImages;

<FM>Description<FN>
Call <A TImageEnMView.BeginSelectImages> and EndSelectImages to select multiple images without refreshing the component's state.

Generally this will speed up the selection process.

<FM>Example<FC>
// select the first 100 images
ImageEnMView1.BeginSelectImages;
for i := 0 to 99 do
  ImageEnMView1.SelectedImage := i;
ImageEnMView1.EndSelectImages;

!!}
procedure TImageEnMView.EndSelectImages;
begin
  fSelectImages := false;
  fMultiSelecting := false;
  if fMultiSelectedImages.Count > 0 then
  begin
    fSelectedItem := integer(fMultiSelectedImages[fMultiSelectedImages.Count - 1]); // select last selected image
    fSelectedBitmap := nil;
  end;
  CallBitmapChangeEvents;
  UpdateEx(false);
end;

{!!
<FS>TImageEnMView.SelectedImage

<FM>Declaration<FC>
property SelectedImage: integer;

<FM>Description<FN>
Get or set the currently selected image (which will be drawn with a <L TImageEnMView.SelectionColor>colored border</L>.

You can get the bitmap of selected image using <A TImageEnMView.IEBitmap> or <A TImageEnMView.Bitmap>.

<FM>See Also<FN>
- <A TImageEnMView.SelectedImageAlwaysVisible>
!!}
procedure TImageEnMView.SetSelectedItem(v: integer);
begin
  if fPlaying then
    exit;
  if (not fMultiSelecting) and (v = fSelectedItem) then
    exit;
  if (v < fImageInfo.Count) and (v >= 0) then
  begin
    SetSelectedItemNU(v);
    fPriorHSIDX := v;
    if not fSelectImages then
      UpdateEx(false, fSelectedImageAlwaysVisible);
  end;
end;
      



{!!
<FS>TImageEnMView.SelectedImageAlwaysVisible

<FM>Declaration<FC>
property SelectedImageAlwaysVisible: Boolean;  (Read/Write)

<FM>Description<FN>
Ensures that the selected cell is always visible when set using <A TImageEnMView.SelectedImage> or it changes due to <L TImageEnMView.AppendImage>appending</L> or <L TImageEnMView.DeleteImage>deleting</L> images.

<FM>See Also<FN>
- <A TImageEnMView.SelectedImage>
!!}
procedure TImageEnMView.SetSelectedImageAlwaysVisible(v: boolean);
begin
  fSelectedImageAlwaysVisible := v;
  if fSelectedImageAlwaysVisible then
    CheckSelectedImageIsVisible;
end;


// doesn't call Update
// indexes which aren't inside bounds are ignored
procedure TImageEnMView.SetSelectedItemNU(v: integer);
var
  i, q: integer;
begin
  fChangedSel := false;
  if fPlaying then
    exit;
  if (v < fImageInfo.Count) and (v >= 0) then
  begin
    if fSelectedItem >= 0 then
    begin
      if fSelectedBitmap <> nil then
        fImageList.ReleaseBitmap(fSelectedBitmap, true);
      //ClearImageCache(fSelectedItem); // ....  removed in 2.1.9
    end;
    if fEnableMultiSelect then
    begin
      if not fMultiSelecting then
      begin
        for i := 0 to fMultiSelectedImages.Count-1 do
          DoImageDeselect( integer(fMultiSelectedImages[i]) );
        fMultiSelectedImages.clear;
        fChangedSel := true;
      end
      else
      begin
        if not fSelectImages then
        begin
          q := fMultiSelectedImages.IndexOf(pointer(v));
          if (q > -1) and (not (iemoLeaveOneSelected in fMultiSelectionOptions) or (fMultiSelectedImages.Count > 1))then
          begin
            // item already selected, unselect when fMultiSelecting is True
            DoImageDeselect( v );
            fMultiSelectedImages.Delete(q);
            fChangedSel := true;
            if not fSelectInclusive then
            begin
              fSelectedItem := -1;
              EXIT; // EXIT POINT!!
            end;
          end;
        end;
      end;
      fMultiSelectedImages.Add(pointer(v));
      fChangedSel := true;
    end;
    if fSelectImages then
    begin
      // inside BeginSelectImages...EndSelectimages the SelectedItem doesn't change (also fSelectedBitmap doesn't change)
      fSelectedItem := -1;
      fChangedSel := true;
    end
    else
    begin
      fSelectedItem := v;
      fChangedSel := true;
      //fSelectedBitmap := GetBitmap(fSelectedItem);
      fSelectedBitmap := nil;
      CallBitmapChangeEvents;
      DoImageSelect( fSelectedItem );
    end;
  end;
end;

{!!
<FS>TImageEnMView.DeSelect

<FM>Declaration<FC>
procedure DeSelect;

<FM>Description<FN>
Remove selection from all selected images.
!!}
procedure TImageEnMView.DeSelect;
begin
  DeselectNU;
  UpdateEx(false);
end;

// doesn't call Update
procedure TImageEnMView.DeselectNU;
var
  i: Integer;
begin
  if fSelectedItem >= 0 then
  begin
    fImageList.ReleaseBitmapByImage(PIEImageInfo(fImageInfo[fSelectedItem])^.image, true);
  end;
  if fEnableMultiSelect then
  begin
    for i := 0 to fMultiSelectedImages.Count-1 do
      DoImageDeselect( integer(fMultiSelectedImages[i]) );
    fMultiSelectedImages.Clear();
  end
  else
    DoImageDeselect( fSelectedItem );
  fSelectedItem := -1;
end;

{!!
<FS>TImageEnMView.UnSelectImage

<FM>Declaration<FC>
procedure UnSelectImage(idx: Integer);

<FM>Description<FN>
Remove selection from the specified image (if multiselection is enabled).

!!}
procedure TImageEnMView.UnSelectImage(idx: integer);
begin
  if (idx >= 0) and (idx < ImageCount) then
  begin
    if idx = fSelectedItem then
    begin
      fImageList.ReleaseBitmapByImage(PIEImageInfo(fImageInfo[fSelectedItem])^.image, true);
      ClearImageCache(fSelectedItem);
      fMultiSelectedImages.Remove(pointer(idx));
      fSelectedItem := -1;
    end
    else
    begin
      fMultiSelectedImages.Remove(pointer(idx));
    end;
    DoImageDeselect( fSelectedItem );
  end;
  UpdateEx(false);
end;


// Select image without affecting existing selection
procedure TImageEnMView.SelectImage(idx: integer); 
begin
  if (idx >= 0) and (idx < ImageCount) then
  begin
    fSelectImages := true;
    fMultiSelecting := true;

    SelectedImage := idx;

    fSelectImages := false;
    fMultiSelecting := false;
    fSelectedItem := idx;
    fSelectedBitmap := nil;
    CallBitmapChangeEvents;
  end;
  UpdateEx(false);
end;

// Selects an image if not selected, otherwise deselects. Does not change selection status of other files
procedure TImageEnMView.ToggleSelectImage(idx: integer);
begin
  if (idx >= 0) and (idx < ImageCount) then
  begin     
    if IsSelected(idx) then
      UnSelectImage(idx)
    else
      SelectImage(idx);
  end;
end;


{!!
<FS>TImageEnMView.CopyToIEBitmap

<FM>Declaration<FC>
procedure CopyToIEBitmap(idx: Integer; bmp: <A TIEBitmap>);

<FM>Description<FN>
Copies the specified image, <FC>idx<FN>, to the destination <A TIEBitmap> object.

Note: If <A TImageEnMView.StoreType> is not ietNormal then the returned bmp may not be full resolution

!!}
procedure TImageEnMView.CopyToIEBitmap(idx: integer; bmp: TIEBitmap);
begin
  EnterCriticalSection(fThreadCS);
  try
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      // transform hbi to TBitmap object
      if image = nil then
        ObtainImageNow(idx);
      if image <> nil then
      begin
        // image present
        fImageList.CopyToIEBitmap(image, bmp);
      end;
    end;
  finally
    LeaveCriticalSection(fThreadCS);
  end;
end;

{!!
<FS>TImageEnMView.GetBitmap

<FM>Declaration<FC>
function GetBitmap(idx: Integer): TBitmap;

<FM>Description<FN>
Creates a TBitmap object from the image at index, <FC>idx<FN>.
Any changes you make to the bitmap will be visible after you call the <A TImageEnMView.Update> method.
You will need to call <A TImageEnMView.ReleaseBitmap> to free the TBitmap object.

See also: <A TImageEnMView.GetTIEBitmap>.

<FM>Example<FC>
// Save the fifth image to file
bmp := ImageEnMView1.GetBitmap(4);
bmp.SaveToFile('alfa.bmp');
ImageEnMView1.ReleaseBitmap(4);
!!}
function TImageEnMView.GetBitmap(idx: integer): TBitmap;
begin
  EnterCriticalSection(fThreadCS);
  result := nil;
  try
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      // transform hbi to TBitmap object
      if image = nil then
        ObtainImageNow(idx);
      if image <> nil then
      begin
        // image present
        result := fImageList.GetBitmap(image).VclBitmap;
      end;
    end;
  finally
    LeaveCriticalSection(fThreadCS);
  end;
end;

{!!
<FS>TImageEnMView.GetTIEBitmap

<FM>Declaration<FC>
function GetTIEBitmap(idx: Integer): <A TIEBitmap>;

<FM>Description<FN>         
Creates a <A TIEBitmap> object from the image at index, <FC>idx<FN>.
Any changes you make to the bitmap will be visible after you call the <A TImageEnMView.Update> method.
You will need to call <A TImageEnMView.ReleaseBitmap> to free the <A TIEBitmap> object.

<FM>Example<FC>          
// Save the fifth image to file
bmp := ImageEnMView1.GetTIEBitmap(4); // Note: bmp must be TIEBitmap type
bmp.Write('D:\alfa.png');
ImageEnMView1.ReleaseBitmap(4);

!!}
function TImageEnMView.GetTIEBitmap(idx: integer): TIEBitmap;
begin
  EnterCriticalSection(fThreadCS);
  try
    result := nil;
    with PIEImageInfo(fImageInfo[idx])^ do
    begin
      // transform hbi to TBitmap object
      if image = nil then
        ObtainImageNow(idx);
      if image <> nil then
      begin
        // image present
        result := fImageList.GetBitmap(image);
      end;
    end;
  finally
    LeaveCriticalSection(fThreadCS);
  end;
end;

{!!
<FS>TImageEnMView.ReleaseBitmap

<FM>Declaration<FC>
procedure ReleaseBitmap(idx: Integer; saveChanges: Boolean = true);

<FM>Description<FN>
ReleaseBitmap releases the bitmap created with <A TImageEnMView.GetBitmap> or <A TImageEnMView.GetTIEBitmap> method.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>idx<FN></C> <C>The image index to release.</C> </R>
<R> <C><FC>saveChanges<FN></C> <C>If true (default) the bitmap will be written in the cache.</C> </R>
</TABLE>

!!}
procedure TImageEnMView.ReleaseBitmap(idx: Integer; saveChanges: Boolean);
begin
  fImageList.ReleaseBitmapByImage(PIEImageInfo(fImageInfo[idx])^.image, saveChanges);
  ClearImageCache(idx);
end;

procedure TImageEnMView.ClearOnDemandIOList;
var
  i: Integer;
begin
  for i := 0 to fMultiOnDemands.Count-1 do
  begin
    TImageEnIO(fMultiOnDemands[i]).Free;
    fMultiOnDemands[i] := nil;
  end;
  fMultiOnDemands.Clear;
end;

function TImageEnMView.GetOnDemandIO(const filename: WideString; var FrameIndex: Integer): TImageEnIO;
var
  p: Integer;
  realname: WideString;
begin
  result := nil;
  p := Pos(WideString(IEM_Path_Index_Delimiter), filename);  // here the '::' must exist
  realname := copy(filename, 1, p-1);
  FrameIndex := StrToIntDef(Copy(filename, p+2, length(filename)), 0);
  // search for already created on the same file
  for p := 0 to fMultiOnDemands.Count-1 do
    if TImageEnIO(fMultiOnDemands[p]).Params.FileName=realname then
    begin
      result := TImageEnIO(fMultiOnDemands[p]);
      break;
    end;
  if result=nil then
  begin
    // create a new one
    result := TImageEnIO.Create(self);
    result.Params.FileName := realname;
    fMultiOnDemands.Add( result );
  end;
end;

procedure TImageEnMView.LoadMultiOnDemand(io: TImageEnIO; frameindex: Integer; var dt: Double);
var
  FInfo : TIEFileFormatInfo;
  FType : TIOFileType;
  FileName: WideString;
begin
  FileName := io.Params.FileName; // it is important that we don't pass io.Params.FileName as parameter

  FType := ioUnknown;
  FInfo := IEFileFormatGetInfo2(string(IEExtractFileExtW(FileName)));
  if assigned(FInfo) then
    FType := FInfo.FileType;
  if FType = ioUnknown then
   FType := FindFileFormat(FileName);

  dt := 0.0;
  if (FType <> ioUnknown) and (FType <> ioAVI) and (FType <> ioWMV) and (FType <> ioMPEG) then
  begin
    io.Params.GIF_ImageIndex  := frameindex;
    io.Params.TIFF_ImageIndex := frameindex;
    io.Params.DCX_ImageIndex  := frameindex;
    io.LoadFromFileAuto(FileName);
    if FType = ioGIF then
      dt := io.Params.GIF_DelayTime * 10;
  end
  else
  begin
    {$ifdef IEINCLUDEDIRECTSHOW}
      if not io.IsOpenMediaFile then
        io.OpenMediaFile(FileName);
      io.LoadFromMediaFile(frameindex);
      dt := io.Params.MEDIAFILE_FrameDelayTime * 10;
    {$else}
      if FType = ioAVI then
      begin
        if not io.IsOpenAVI then
          io.OpenAVIFile(FileName);
        io.LoadFromAVI(frameindex);
        dt := io.Params.AVI_FrameDelayTime;
      end;
    {$endif}
  end;
end;

function TImageEnMView.IsOnDemand(info: PIEImageInfo): Boolean;
begin
  result := ((info^.ID > -1) and (assigned(fOnImageIDRequest) or assigned(fOnImageIDRequestEx))) or assigned(info^.Filename);
end;


// Retrieve a thumbnail for a video format from Windows
function LoadVideoThumbnailFromExplorer(AIEBitmap: TIEBitmap; sFilename: string; iThumbWidth, iThumbHeight : integer) : Boolean;
// Xequte 16/11/12
var
  ABitmap : TBitmap;
begin
  Result := False;
  {$IFDEF VIDEO_THUMBNAILS}
  ABitmap := TBitmap.create;
  OleInitialize(nil);
  try
    if ExtractExplorerThumbnail(sFilename, ABitmap, iThumbWidth, iThumbHeight) then
    begin
      AIEBitmap.CopyFromTBitmap(ABitmap);
      if AIEBitmap.Width > 10 then
        Result := True;
    end;
  finally
    OleUninitialize();
    ABitmap.free;
  end;
  {$ENDIF}
end;



// Make sure that at index "idx" there is a valid image (load from FileName or request it using ID).
// Returns true if the image load is ok, false otherwise
function TImageEnMView.ObtainImageNow(idx: integer): boolean;
var
  info: PIEImageInfo;
  bmp: TBitmap;
  iebmp: TIEBitmap;
  io: TImageEnIO;
  multiondemand: Boolean;
  frameindex: Integer; // used reading on demand multi pages
  dt: Double;
  ASource: TIESourceType;
begin
  EnterCriticalSection(fThreadCS);
  try
    result := true;
    bmp := nil;
    info := PIEImageInfo(fImageInfo[idx]);
    ASource := info^.SourceType;
    if (info^.ID > -1) and assigned(fOnImageIDRequest) then
    begin
      // request by ID
      bmp := nil;
      fOnImageIDRequest(self, idx, info^.ID, bmp);
      SetImageEx(idx, bmp);
    end
    else
    if (info^.ID > -1) and assigned(fOnImageIDRequestEx) then
    begin
      // request by ID
      iebmp := nil;
      fOnImageIDRequestEx(self, idx, info^.ID, iebmp);
      SetIEBitmapEx(idx, iebmp);
    end
    else
    if assigned(info^.Filename) then
    begin
      // load from 'Name'
      multiondemand := Pos(IEM_Path_Index_Delimiter, info^.Filename) > 0; // this is the special syntax '::' to load multipages on demand, use ObtainImageNow
      if multiondemand then
        io := GetOnDemandIO(info^.Filename, frameindex)
      else
        io := fImageEnIO;

      iebmp := TIEBitmap.Create;
      try
        io.Background := info^.Background;
        io.AttachedIEBitmap := iebmp;
        io.OnProgress := fOnIOProgress;
        _SetLoadParams(Self, io.Params);
        if assigned(fImageEnMIO) then
          io.AutoAdjustDPI := fImageEnMIO.AutoAdjustDPI;
          
        try
          if multiondemand then
          begin
            // Has user aborted loading
            if MIO.Aborting then
              io.Aborting := true
            else
              LoadMultiOnDemand(io, frameindex, dt);
            info^.DTime := dt;
          end
          else
          if (IEExtractFileExtW(info^.Filename) = '.emf') or (IEExtractFileExtW(info^.Filename) = '.wmf') then
            io.ImportMetafile(info^.Filename, ThumbWidth, -1, true)
          else
          {$IFDEF VIDEO_THUMBNAILS}
          if UseThumbnailFromExplorer(info^.Filename) then
            io.Aborting := not LoadVideoThumbnailFromExplorer(iebmp, info^.Filename, ThumbWidth, ThumbHeight)
          else
          {$ENDIF}
          begin
            io.LoadFromFileAuto(info^.Filename);
            if (iebmp.Width  < 2) and
               (iebmp.Height < 2) then
              io.Aborting := true;
          end;
        except
          io.Aborting := true;
        end;

        if io.Aborting then
        begin
          DoWrongImage(iebmp, idx, ASource);
          result := false;
        end;

        // updates params of encapsulated TImageEnMIO object
        GetImageEnMIO.Params[idx].Assign(io.Params); // GetImageEnMIO creates TImageEnMIO if it doesn't exist

        // set the image
        info^.Background := io.Background;
        info^.SourceType := ASource;
        SetIEBitmapEx(idx, iebmp);
        ImageOriginalWidth[idx]  := io.Params.OriginalWidth;
        ImageOriginalHeight[idx] := io.Params.OriginalHeight;
        if io.Params.EXIF_DateTimeOriginal2 > 0 then
          ImageCreateDate[idx] := io.Params.EXIF_DateTimeOriginal2;  

      finally
        io.AttachedIEBitmap := nil;
        FreeAndNil(iebmp);
      end;

    end;
  finally
    LeaveCriticalSection(fThreadCS);
  end;
end;

// remove available threads or threads that want to load a no more visible image
procedure FreeUseLessThreads(mview: TImageEnMView);
var
  i: integer;
begin
  i := 0;
  with mview do
    while i < fThreadPoolIO.Count do
    begin
      if TImageEnIO(fThreadPoolIO[i]).Tag = -1 then
      begin
        // finished thread
        TImageEnIO(fThreadPoolIO[i]).IEBitmap.Free;
        TImageEnIO(fThreadPoolIO[i]).IEBitmap := nil;
        TImageEnIO(fThreadPoolIO[i]).AttachedIEBitmap := nil;
        TImageEnIO(fThreadPoolIO[i]).Free;
        fThreadPoolIO.delete(i);
      end
      else
      if (TImageEnIO(fThreadPoolIO[i]).Tag >= 0) and (not IsVisible(TImageEnIO(fThreadPoolIO[i]).Tag)) and (not IsLookAhead(TImageEnIO(fThreadPoolIO[i]).Tag)) then
      begin
        // invisible image
        TImageEnIO(fThreadPoolIO[i]).Tag := -2; // -2 controlled abort
        TImageEnIO(fThreadPoolIO[i]).Aborting := true;
        inc(i);
      end
      else
        inc(i);
    end;
end;

procedure FreeInvisibleRequests(mview: TImageEnMView);
var
  i: integer;
begin
  i := 0;
  with mview do
    if MaintainInvisibleImages<>-1 then  // 3.0.4
      while i < fThreadRequests.Count do
      begin
        if (not IsVisible(integer(fThreadRequests[i]))) and (not IsLookAhead(integer(fThreadRequests[i]))) then
          fThreadRequests.delete(i)
        else
          inc(i);
      end;
end;

// -1 = not found, >-1 item index in fThreadRequests
function TImageEnMView.IsRequested(idx: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  EnterCriticalSection(fThreadCS);
  try
    for i := 0 to fThreadRequests.Count - 1 do
      if integer(fThreadRequests[i]) = idx then
      begin
        result := i;
        break;
      end;
  finally
    LeaveCriticalSection(fThreadCS);
  end;
end;

// return false if info^.ID > -1 and assigned OnImageIDRequest. Works only with info^.Filename 
// priority: -1 = lowest priority, 0..inf priority position
function TImageEnMView.ObtainImageThreaded(idx: integer; priority: Integer): boolean;
var
  info: PIEImageInfo;
  i: integer;
begin
  info := PIEImageInfo(fImageInfo[idx]);

  if assigned(info^.Filename) and (Pos(IEM_Path_Index_Delimiter, info^.Filename) > 0) then
  begin
    // this is the special syntax '::' to load multipages on demand, use ObtainImageNow
    result := false;
    exit;
  end;

  try
    EnterCriticalSection(fThreadCS);

    if info^.image <> nil then
    begin
      result := true;
      exit;
    end;

    // free no more visible requests
    FreeInvisibleRequests(self);

    if priority>-1 then
      priority := imax(0, imin(priority, fThreadRequests.Count-1));

    // check if already requested
    i := IsRequested(idx);
    if i > -1 then
    begin
      // already requested
      if priority>-1 then
        fThreadRequests.Move(i, priority);
      result := true;
      exit;
    end;

    // check if already processing it
    for i := 0 to fThreadPoolIO.Count - 1 do
      if TImageEnIO(fThreadPoolIO[i]).Tag = idx then
      begin
        // already in progress
        result := true;
        exit;
      end;

    // free available threads
    FreeUseLessThreads(self);

    result := false;
    if (info^.ID > -1) and assigned(fOnImageIDRequest) then
    begin
      // request by ID, it is better to use ObtainImageNow
      exit;
    end
    else
    if (info^.ID > -1) and assigned(fOnImageIDRequestEx) then
    begin
      // request by ID, it is better to use ObtainImageNow
      exit;
    end
    else
    if assigned(info^.Filename) then
    begin
      // Load from 'Name'.
      // Add this request to the requests list.
      if priority > -1 then
        fThreadRequests.Insert(priority, pointer(idx)) // 3.0.4
      else
        fThreadRequests.Add(pointer(idx));
      result := true;
      exit;
    end;
    
  finally
    Windows.SetEvent(fThreadStarter.resumeEvent);
    LeaveCriticalSection(fThreadCS);
  end;
end;

procedure _DoubleIconSize(ABitmap: TIEBitmap);
begin     
  _IEResampleIEBitmap2(ABitmap, rfNearest, ABitmap.Width * 2, ABitmap.Width * 2, nil, nil);  
end;



procedure TImageEnMView.DoWrongImage(OutBitmap: TIEBitmap; idx: integer; var ASourceType : TIESourceType);
var
  OBitmap: TIEDibBitmap;
  cv: TCanvas;
  Handled: boolean;
  ww, hh: integer;
begin
  Handled := false;

  if (ASourceType <> iestFolderIcon) and assigned(fOnWrongImage) then
  begin
    OutBitmap.Allocate(ThumbWidth, ThumbHeight, ie24RGB);
    fOnWrongImage(self, OutBitmap, idx, Handled);
    if Handled then
      ASourceType := iestCustomImage;
  end;
  if not Handled then
  begin
    // Note: For performance reasons ASourceType may not be iestFolderIcon even for folders
    if ASourceType <> iestFolderIcon then
      ASourceType := iestFileIcon;
    if (ASourceType = iestFolderIcon) or (ietxShowIconForUnknownFormat in fThumbnailOptionsEx) then
    begin
      // display shell icon
      if fIconList.RetrieveFromCache(citIconOnly, ImageFilename[idx], ASourceType = iestFolderIcon, OutBitmap, True) then
        begin { OutBitmap now valid } end
      else
      {$IFDEF VIDEO_THUMBNAILS}
      if IEIsWindowsVistaOrNewer and (fIconSize in [ieicStretchHD, ieicStretchAll]) then
      begin
        IEGetJumboFileIcon(ImageFileName[idx], OutBitmap);
        if fEnableImageCaching then
          fIconList.SaveToCache(OutBitmap, citIconOnly, ImageFilename[idx], ASourceType = iestFolderIcon);
      end
      else
      {$ENDIF}
      begin
        IEGetFileIcon(ImageFileName[idx], OutBitmap);
        if fIconSize <> ieicStandardSize then
          _DoubleIconSize(OutBitmap);
        if fEnableImageCaching then
          fIconList.SaveToCache(OutBitmap, citIconOnly, ImageFilename[idx], ASourceType = iestFolderIcon);
      end;
    end
    else
    begin
      // display a question mark
      OBitmap := TIEDibBitmap.Create;
      OBitmap.AllocateBits(ThumbWidth, ThumbHeight, 24);
      cv := TCanvas.Create;
      cv.Handle := OBitmap.HDC;
      cv.Brush.Color := clWhite;
      cv.Brush.Style := bsSolid;
      cv.FillRect(rect(0, 0, OBitmap.Width, OBitmap.Height));
      cv.Font.Name := 'Arial';
      cv.Font.Size := ThumbHeight;
      cv.Font.Style := [fsBold];
      ww := cv.TextWidth('?');
      hh := cv.TextHeight('?');
      cv.Font.Color := clGray;
      cv.Brush.Style := bsClear;
      cv.TextOut((integer(OBitmap.Width) - ww) div 2 + 3, (integer(OBitmap.Height) - hh) div 2 + 3, '?');
      cv.Font.Color := clBlue;
      cv.TextOut((integer(OBitmap.Width) - ww) div 2, (integer(OBitmap.Height) - hh) div 2, '?');
      OutBitmap.CopyFromTDibBitmap(OBitmap);
      FreeAndNil(cv);
      FreeAndNil(OBitmap);
    end;
  end;
end;


procedure TImageEnMView.ThreadFinish(Sender: TObject);
var
  io: TImageEnIO;
  idx, ww, hh: integer;
  bmp: TIEBitmap;
  wDummy, hDummy : Integer;
  ASourceType : TIESourceType;
begin          
  io := Sender as TImageEnIO;
  ASourceType := iestDefault;

  // prepare the thumbnail in this thread (instead of in SetIEBitmapEx that must be in monothread mode)
  if (csDestroying in ComponentState) or (csDestroying in io.ComponentState) or (io.IEBitmap = nil) then
  begin
    EnterCriticalSection(fThreadCS);
    try
      io.Tag := -1;
      Windows.SetEvent(fThreadStarter.resumeEvent);
    finally
      LeaveCriticalSection(fThreadCS);
    end;
    exit;
  end;
  if io.Aborting then
  begin
    EnterCriticalSection(fThreadCS);
    try
      DoWrongImage(io.IEBitmap, io.Tag, ASourceType);
    finally
      LeaveCriticalSection(fThreadCS);
    end;
  end;
  if (fStoreType = ietThumb) and (fEnableResamplingOnMinor or (io.IEBitmap.Width > ThumbWidth) or (io.IEBitmap.Height > ThumbHeight)) then
  begin
    if (io.IEBitmap.width = 0) or (io.IEBitmap.height = 0) then
    begin
      ww := ThumbWidth;
      hh := ThumbHeight;
    end
    else
    begin
      IEGetFitResampleSizeWithAutoCrop(io.IEBitmap.width, io.IEBitmap.height, ThumbWidth, ThumbHeight, wDummy, hDummy, fThumbnailClipping, ww, hh);
    end;

    if (io.IEBitmap.Width > ww) or (io.IEBitmap.Height > hh) then
    begin
      bmp := TIEBitmap.Create;
      bmp.allocate(ww, hh, ie24RGB);
      if io.IEBitmap.PixelFormat = ie1g then
      begin
        _SubResample1bitFilteredEx(io.IEBitmap, 0, 0, io.IEBitmap.width - 1, io.IEBitmap.height - 1, bmp)
      end
      else
      begin
        if (io.IEBitmap.PixelFormat <> ie24RGB) and  (fThumbnailResampleFilter <> rfNone) then
          io.IEBitmap.PixelFormat := ie24RGB;
        if fThumbnailResampleFilter = rfNone then
        begin
          bmp.PixelFormat := io.IEBitmap.PixelFormat; // _IEBmpStretchEx supports multiple pixelformats, but input and output must have the same pixelformat
          _IEBmpStretchEx(io.IEBitmap, bmp, nil, nil);
        end
        else
          _ResampleEx(io.IEBitmap, bmp, nil, fThumbnailResampleFilter, nil, nil)
      end;
      if io.IEBitmap.HasAlphaChannel then
      begin
        if fThumbnailResampleFilter = rfNone then
          _Resampleie8g(io.IEBitmap.AlphaChannel, bmp.AlphaChannel, rfFastLinear)
        else
          _Resampleie8g(io.IEBitmap.AlphaChannel, bmp.AlphaChannel, fThumbnailResampleFilter);
        bmp.AlphaChannel.Full := io.IEBitmap.AlphaChannel.Full;
      end;
    end
    else
      bmp := io.IEBitmap;
  end
  else
    bmp := io.IEBitmap;

  EnterCriticalSection(fThreadCS);
  try
    idx := io.Tag;
    if idx >= 0 then
    begin
      GetImageEnMIO.Params[idx].Assign(io.Params);
      PIEImageInfo(fImageInfo[idx])^.Background := io.Background;
      if PIEImageInfo(fImageInfo[idx])^.SourceType <> iestFolderIcon then
        PIEImageInfo(fImageInfo[idx])^.SourceType := ASourceType;
      SetIEBitmapEx(idx, bmp);
      ImageOriginalWidth[idx]  := io.Params.OriginalWidth;
      ImageOriginalHeight[idx] := io.Params.OriginalHeight;    
      if io.Params.EXIF_DateTimeOriginal2 > 0 then
        ImageCreateDate[idx] := io.Params.EXIF_DateTimeOriginal2;
    end;
    if assigned(bmp) and (bmp <> io.IEBitmap) then
      FreeAndNil(bmp);
    io.Tag := -1;
    Windows.SetEvent(fThreadStarter.resumeEvent);
  finally
    LeaveCriticalSection(fThreadCS);
    invalidate();
  end;
end;


constructor TIEStarter.Create;
begin
  resumeEvent := Windows.CreateEvent(nil, false, false, nil);
  inherited Create(false); // create Not suspended
end;

destructor TIEStarter.Destroy;
begin
  inherited;
  Windows.CloseHandle(resumeEvent);
end;

procedure TIEStarter.Execute;
var
  info: PIEImageInfo;
  bmp: TIEBitmap;
  io: TImageEnIO;
  idx: integer;
begin
  Windows.WaitForSingleObject(resumeEvent, INFINITE);
  while not Terminated do
  begin
    EnterCriticalSection(mview.fThreadCS);
    try
      FreeUseLessThreads(mview);
      FreeInvisibleRequests(mview);
      while (not Terminated) and (mview.fThreadRequests.Count > 0) and (mview.fThreadPoolIO.Count < mview.fThreadPoolSize) do
      begin
        idx := integer(mview.fThreadRequests[0]);
        bmp := TIEBitmap.Create;
        io := TImageEnIO.Create(nil);
        io.Tag := idx;
        mview.fThreadRequests.Delete(0);
        mview.fThreadPoolIO.add(io);
        io.AttachedIEBitmap := bmp;
        info := PIEImageInfo(mview.fImageInfo[idx]);
        io.Background := info^.Background;     
        _SetLoadParams(mview, io.Params);
        io.OnFinishWork := mview.ThreadFinish;
        io.AsyncMode := true;
        if (IEExtractFileExtW(info^.Filename) = '.emf') or (IEExtractFileExtW(info^.Filename) = '.wmf') then
          io.ImportMetafile(info^.Filename, mview.ThumbWidth, -1, true)
        else
        {$IFDEF VIDEO_THUMBNAILS}
        if UseThumbnailFromExplorer(info^.Filename) then
        begin
          io.Aborting := not LoadVideoThumbnailFromExplorer(bmp, info^.Filename, mview.ThumbWidth, mview.ThumbHeight);
          mview.ThreadFinish(io);
        end
        else
        {$ENDIF}
          io.LoadFromFileAuto(info^.Filename); // LoadFromFile cannot throw exceptions, but just add a new thread
      end;
    finally
      LeaveCriticalSection(mview.fThreadCS);
    end;
    if (not Terminated) and (mview.fThreadRequests.Count = 0) and (mview.fThreadPoolIO.Count=0) then
      Windows.WaitForSingleObject(resumeEvent, INFINITE);
    sleep(0);  // let other threads to execute
  end;
end;


{!!
<FS>TImageEnMView.TextMargin

<FM>Declaration<FC>
property TextMargin : integer;

<FM>Description<FN>
Specifies the offset from the border that the text appears:
- Left and right side for all text types
- From the top for <A TImageEnMView.ImageTopText>
- From the bottom for <A TImageEnMView.ImageBottomText>

Note: Setting TextMargin will automatically adjust <A TImageEnMView.UpperGap> and <A TImageEnMView.BottomGap>

<FM>See Also<FN>
- <A TImageEnMView.UpperGap>
- <A TImageEnMView.BottomGap>

!!}
procedure TImageEnMView.SetTextMargin(v: integer);
begin
  fTextMargin := v;
  Update;
end;


{!!
<FS>TImageEnMView.BottomGap

<FM>Declaration<FC>
property BottomGap: integer;

<FM>Description<FN>
Specifies the distance between the thumbnail image and its bottom border.

You can use BottomGap to reserve space for painting, e.g. custom text (with <A TImageEnMView.OnImageDraw>).

<FM>See Also<FN>
- <A TImageEnMView.UpperGap>  
- <A TImageEnMView.SideGap>

!!}
procedure TImageEnMView.SetBottomGap(v: integer);
begin
  fBottomGap := v;
  fSetBottomGap := v;
  fIconList.Clear();
  Update;
end;

{!!
<FS>TImageEnMView.UpperGap

<FM>Declaration<FC>
property UpperGap: integer;

<FM>Description<FN>
Specifies the distance between the thumbnail image and its top border.

You can use UpperGap to reserve space for painting, e.g. custom text (with <A TImageEnMView.OnImageDraw>).

<FM>See Also<FN>
- <A TImageEnMView.BottomGap>
- <A TImageEnMView.SideGap>
!!}
procedure TImageEnMView.SetUpperGap(v: integer);
begin
  fUpperGap := v;
  fSetUpperGap := v;
  fIconList.Clear();
  Update;
end;


{!!
<FS>TImageEnMView.SideGap

<FM>Declaration<FC>
property SideGap: integer;

<FM>Description<FN>
Specifies the distance between the thumbnail image and the borders on the left and right.

Note: Generally you will not need to use SideGap if you have enabled <L TImageEnMView.SoftShadow>soft shadows</L> which already include their own spacing.

<FM>See Also<FN>
- <A TImageEnMView.UpperGap>
- <A TImageEnMView.BottomGap>
!!}
procedure TImageEnMView.SetSideGap(v: integer);
begin
  fSideGap := v;
  fIconList.Clear();
  Update;
end;



{!!
<FS>TImageEnMView.Bitmap

<FM>Declaration<FC>
property Bitmap: TBitmap;

<FM>Description<FN>
The currently selected image as a TBitmap object.

<FM>See Also<FN>
- <A TImageEnMView.IEBitmap>
- <A TImageEnMView.SelectedImage>
!!}
function TImageEnMView.GetFBitmap: TBitmap;
begin
  result := nil;
  if fSelectedItem >= 0 then
  begin
    if fSelectedBitmap = nil then
      fSelectedBitmap := GetTIEBitmap(fSelectedItem);
    if assigned(fSelectedBitmap) then
      result := fSelectedBitmap.VclBitmap;
  end;
end;

{!!
<FS>TImageEnMView.IEBitmap

<FM>Declaration<FC>
property Bitmap: <A TIEBitmap>;

<FM>Description<FN>
The currently selected image as <A TIEBitmap> object.

<FM>See Also<FN>
- <A TImageEnMView.Bitmap>
- <A TImageEnMView.SelectedImage>
!!}
function TImageEnMView.GetIEBitmap: TIEBitmap;
begin
  if fSelectedItem >= 0 then
  begin
    if fSelectedBitmap = nil then
      fSelectedBitmap := GetTIEBitmap(fSelectedItem);
    result := fSelectedBitmap;
  end
  else
    result := nil;
end;

procedure TImageEnMView.SetMouseInteract(v: TIEMMouseInteract);
begin
  if v <> fMouseInteract then
    fMouseInteract := v;
end;

procedure TImageEnMView.SetKeyInteract(v: TIEMKeyInteract);
begin
  if v <> fKeyInteract then
    fKeyInteract := v;
end;



{!!
<FS>TImageEnMView.ThumbnailOptionsEx

<FM>Declaration<FC>
property ThumbnailOptionsEx: <A TIEMThumbnailOptionsEx>;

<FM>Description<FN>
Controls some <L TIEMThumbnailOptionsEx>advanced options</L> for the display of icons and thumbnails.
Default: [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading];

Note: You will need to update the display after changing ThumbnailOptionsEx, e.g. by calling ImageEnMView1.<A TImageEnMView.Update>;
!!}

procedure TImageEnMView.SetThumbnailOptionsEx(v: TIEMThumbnailOptionsEx);
begin
  fThumbnailOptionsEx := v;
end;

function TImageEnMView.ReplaceIEMConsts(const ws : WideString; idx : Integer) : WideString;


  function ConcatAndIns(const Value1, Value2: string; const sDelimiter: string = ', '): string;
  begin
    if (Value1 <> '') and (Value2 <> '') then
      Result := Value1 + sDelimiter + Value2
    else
      Result := Value1 + Value2;
  end;

  function _FileSize : string;
  begin
    Result := '';
    if ImageFileSize[idx] > 0 then
      Result := String(IEBytesToStr2(ImageFileSize[idx]));
  end;

  function _FileCreateDate(bIncludeTime : Boolean = False) : string;
  var
    ADateTime: TDateTime;
  begin
    ADateTime := ImageCreateDate[idx];
    if ADateTime < 1 then
      Result := ''
    else
    if bIncludeTime then
      Result := DateTimeToStr(ADateTime)
    else
      Result := DateToStr(ADateTime)
  end;    

  function _FileEditDate(bIncludeTime : Boolean = False) : string;   
  var
    ADateTime: TDateTime;
  begin
    ADateTime := ImageEditDate[idx];
    if ADateTime < 1 then
      Result := ''
    else
    if bIncludeTime then
      Result := DateTimeToStr(ADateTime) 
    else
      Result := DateToStr(ADateTime)
  end;

  function _ExtractJustName(sFilename : Widestring): Widestring;
  var
    iExtLen: integer;
  begin
    Result := IEExtractFileNameW(sFilename);
    iExtLen := length(IEExtractFileExtW(sFilename));
    if iExtLen > 0 then
      Delete(result, Length(result) - iExtLen + 1, iExtLen);
  end;

var
  sDims: string;
begin
  Result := ws;
  if (result = '') or (result[1] <> '$') then
    exit;

  if SameText(ws, IEM_Filename) then
    Result := StripShortcutExt(IEExtractFileNameW(ImageFilename[idx]))
  else
  if SameText(ws, IEM_FilenameNoExt) then
    Result := _ExtractJustName(ImageFilename[idx])
  else
  if SameText(ws, IEM_FilePath) then
    Result := IEExtractFilePathW(ImageFilename[idx])
  else
  if SameText(ws, IEM_ImageDimensions) then
  begin
    if ImageOriginalWidth[idx] > 0 then
      Result := IntToStr(ImageOriginalWidth[idx]) + ' x ' +
                IntToStr(ImageOriginalHeight[idx]) + ' x ' +
                IntToStr(ImageBitCount[idx]) + 'bit'
    else
      Result := '';
  end
  else
  if SameText(ws, IEM_ImageDimAndSize) then
  begin
    sDims := '';
    if ImageOriginalWidth[idx] > 0 then
      sDims := IntToStr(ImageOriginalWidth[idx]) + ' x ' + IntToStr(ImageOriginalHeight[idx]);
    Result :=  ConcatAndIns(sDims, _FileSize, ', ');
  end
  else
  if SameText(ws, IEM_FileSize) then
    Result := _FileSize
  else
  if SameText(ws, IEM_FileCreateDate) then
    Result := _FileCreateDate
  else
  if SameText(ws, IEM_FileCreateDateTime) then
    Result := _FileCreateDate(True)
  else
  if SameText(ws, IEM_FileCreateDateAndSize) then
    Result := ConcatAndIns(_FileCreateDate, _FileSize, ', ')
  else
  if SameText(ws, IEM_FileEditDate) then
    Result := _FileEditDate
  else
  if SameText(ws, IEM_FileEditDateTime) then
    Result := _FileEditDate(True)
  else
  if SameText(ws, IEM_FileEditDateAndSize) then
    Result := ConcatAndIns(_FileEditDate, _FileSize, ', ')
  else
  if SameText(ws, IEM_FileType) then
    Result := GetImageFileType(idx);
end;






// info^.image can be nil
procedure TImageEnMView.DrawImage(DestBitmap: TBitmap; info: PIEImageInfo; IsSelected: boolean; Index: integer);
const
  Text_Horz_Spacing = 2;
var
  w, ww, hh, iw, ih, sw, t1: integer;
  x1, y1, cx1, cy1: integer;
  ImHeight, ImWidth: integer;
  pix, alpha: pbyte;
  tw: integer;
  ith: integer;   // infotext height
  bth: integer;   // bottomtext height
  iebmp, ietmp: TIEBitmap;
  DestBitmapScanline: ppointerarray;
  XLUT, YLUT: pinteger;
  filt: TResampleFilter;
  wsTop, wsInfo, wsBottom : WideString;
  cl: TColor;
  ActUpperGap, ActBottomGap: integer;
  ActThumbWidth, ActThumbHeight: integer;
  ThumbRect: TRect;
  ox, oy: Integer;
  iec: TIECanvas;
  iSoftShadowSize : Integer;
  iFinalww, iFinalhh : Integer;
  iThumbClipPercent: Integer;
  bStretchSmallImages : Boolean;
  bDoubleIconSize: Boolean;
  bAddSoftShadow: Boolean;
begin
  try
    EnterCriticalSection(fThreadCS);
    if fShowText then
    begin
      ActUpperGap := fUpperGap;
      ActBottomGap := fBottomGap;
    end
    else
    begin
      ActUpperGap  := fSetUpperGap;
      ActBottomGap := fSetBottomGap;
    end;
    ActThumbWidth  := ThumbWidth;
    ActThumbHeight := ThumbHeight;
    x1 := info^.X - fViewX;
    y1 := info^.Y - fViewY;

    if assigned(fOnBeforeImageDraw) then
      fOnBeforeImageDraw(self, Index, x1, y1, DestBitmap.Canvas);

    if assigned(fThumbnailFrame) and assigned(fThumbnailFrameSelected) then
    begin
      ThumbRect := fThumbnailFrameRect;
      if IsSelected then
        fThumbnailFrameSelected.RenderToTBitmapEx(DestBitmap, x1, y1, ThumbWidth, ThumbHeight, 0, 0, fThumbnailFrameSelected.Width, fThumbnailFrameSelected.Height, 255, rfNone, ielNormal)
      else
        fThumbnailFrame.RenderToTBitmapEx(DestBitmap, x1, y1, ThumbWidth, ThumbHeight, 0, 0, fThumbnailFrame.Width, fThumbnailFrame.Height, 255, rfNone, ielNormal);
      ActThumbWidth  := ThumbRect.Right - ThumbRect.Left;
      ActThumbHeight := ThumbRect.Bottom - ThumbRect.Top;
      inc(x1, ThumbRect.Left);
      inc(y1, ThumbRect.Top);
    end;

    if assigned(fOnBeforeImageDrawEx) then
    begin
      ThumbRect.Left := 0;
      ThumbRect.Top  := 0;
      ThumbRect.Right  := ActThumbWidth;
      ThumbRect.Bottom := ActThumbHeight;
      fOnBeforeImageDrawEx(self, Index, x1, y1, DestBitmap, ThumbRect);
      ActThumbWidth  := ThumbRect.Right - ThumbRect.Left;
      ActThumbHeight := ThumbRect.Bottom - ThumbRect.Top;
      inc(x1, ThumbRect.Left);
      inc(y1, ThumbRect.Top);
    end;

    // Now in info^.hbi there is the image to paint (or nil if it isn't possible to obtain the image)
    if fShowText and (info^.InfoText.Caption <> '') then
    begin
      DestBitmap.Canvas.Font.Assign(info^.InfoText.Font);
      ith := IETextHeightW(DestBitmap.Canvas, info^.InfoText.Caption);
    end
    else
      ith := 0;
    ImHeight := ActThumbHeight - ActBottomGap - ActUpperGap - ith;
    ImWidth  := ActThumbWidth - 2 * fSideGap;
    cx1 := 0;
    cy1 := 0;
    ww := 0;
    hh := 0;

    if fFillThumbnail then
    begin
      // Image is background
      if fDrawImageBackground then
        cl := info^.Background
      else
      // Normal (Unselected)
      if IsSelected = False then
        cl := fThumbnailsBackground
      else
      // Selected
      if Enabled then
        cl := fThumbnailsBackgroundSelected
      else
      // Control Disabled
        cl := Thumbnail_Background_When_Disabled;

      IEDrawBackground([], DestBitmap.Canvas, DestBitmap, fThumbnailsBackgroundStyle, cl, x1, y1, ActThumbWidth, ActThumbHeight, x1, y1, x1 + ActThumbWidth, y1 + ActThumbHeight, fChessboardSize, fChessboardBrushStyle, fGradientEndColor, nil, iewoNormal);
    end;

    // paint the image
    if (info^.cacheImage = nil) and (info^.image <> nil) then
    begin
      // cache empty
      iw := fImageList.GetImageWidth(info^.image);
      ih := fImageList.GetImageHeight(info^.image);
      if (iw < 1) or (ih < 1) then
        pix := nil
      else
        pix := fImageList.GetImageBits(info^.image);
      alpha := fImageList.GetAlphaBits(info^.image);

      iSoftShadowSize := 0;
      bAddSoftShadow := False;
      if fSoftShadow.Enabled then
        case info^.SourceType of
          iestFolderIcon : bAddSoftShadow := ietxShowShadowForFolders in fThumbnailOptionsEx;
          iestFileIcon   : bAddSoftShadow := ietxShowShadowForIcons in fThumbnailOptionsEx;
          else             bAddSoftShadow := True;
        end;
      if bAddSoftShadow then
        iSoftShadowSize := IESoftShadowSize(fSoftShadow.Radius, fSoftShadow.OffsetX, fSoftShadow.OffsetY);

      iThumbClipPercent := 0;
      if (fThumbnailClipping > 0) and (info^.SourceType in [iestDefault, iestCustomImage]) then
        iThumbClipPercent := fThumbnailClipping;

      IEGetFitResampleSizeWithAutoCrop(iw, ih, ImWidth - iSoftShadowSize, ImHeight - iSoftShadowSize, iFinalww, iFinalhh,
                                       iThumbClipPercent, ww, hh);

      bStretchSmallImages := False;
      bDoubleIconSize := False;
      if (info^.SourceType in [iestFileIcon, iestFolderIcon]) = False then
        bStretchSmallImages := not fEnableResamplingOnMinor
      else
      if IEIsWindowsVistaOrNewer and (fIconSize in [ieicStretchHD, ieicStretchAll]) then
      begin
        if ((iw > 32) and (ih > 32))              // HD Icon?
          or (fIconSize = ieicStretchAll) then    // Always stretch
          bStretchSmallImages := True
        else
          bDoubleIconSize := True;
      end;

      if (bStretchSmallImages = False) and  (iw < ImWidth - iSoftShadowSize) and (ih < ImHeight - iSoftShadowSize) then
      begin
        ww := iw;
        hh := ih;
        iFinalww := iw;
        iFinalhh := ih;
      end;
      cx1 := x1 + (abs(ActThumbWidth - iFinalww) - iSoftShadowSize) div 2;
      cy1 := y1 + ActUpperGap + (abs(ImHeight - iFinalhh) - iSoftShadowSize) div 2;

      if pix <> nil then
      begin
        ietmp := TIEBitmap.Create;
        ietmp.EncapsulateMemory(pix, iw, ih, fImageList.GetImagePixelFormat(info^.image), false);
        if alpha <> nil then
        begin
          ietmp.AlphaChannel.EncapsulateMemory(alpha, iw, ih, ie8g, true);
          ietmp.AlphaChannel.Full := false;
        end;
        if ietmp.PixelFormat = ie8p then
          CopyMemory(ietmp.PaletteBuffer, fImageList.GetImagePalette(info^.image), 256*SizeOf(TRGB));

        iebmp := TIEBitmap.Create;
        iebmp.Location := ieMemory;

        if (ietmp.PixelFormat = ie1g) and ((ww < iw) or (hh < ih)) then
        begin
          filt := rfFastLinear;
          iebmp.Allocate(ww, hh, ie24RGB);
        end
        else
        begin
          filt := fThumbnailDisplayFilter;
          iebmp.Allocate(ww, hh, ietmp.PixelFormat);
        end;

        _IEResampleIEBitmap(ietmp, iebmp, filt, nil, nil);

        if (iFinalww <> iebmp.Width) or (iFinalhh <> iebmp.Height) then
        begin
          iebmp.Resize(iFinalww, iFinalhh, 0, 255, iehCenter, ievCenter);
          ww := iebmp.Width;
          hh := iebmp.Height;
        end
        else
        if bDoubleIconSize then  
        begin
          _DoubleIconSize(iebmp);
          ww := iebmp.Width;
          hh := iebmp.Height;
        end;

        if fThumbsRounded > 0 then
        begin
          t1 := imin(ActThumbWidth, ActThumbHeight) div fThumbsRounded;
          _IERoundImage(iebmp, t1, t1, nil, nil);
        end;

        // draw shadow
        if iSoftShadowSize > 0 then
        begin
          // deflate ww and hh to correct thumb size
          ww := ww + iSoftShadowSize;
          hh := hh + iSoftShadowSize;

          with info^.internalrect do
          begin
            ox := (ww - iebmp.Width) div 2;
            oy := (hh - iebmp.Height) div 2;
            Left   := ox;
            Top    := oy;
            Right  := trunc(iebmp.Width + ox) + 1;
            Bottom := trunc(iebmp.Height + oy) + 1;    
          end;

          // APPLY SHADOW
          _IEAddSoftShadow(iebmp, fSoftShadow.Radius, fSoftShadow.OffsetX, fSoftShadow.OffsetY, fSoftShadow.Intensity, true, fSoftShadow.ShadowColor, nil, nil);
        end
        else
        begin
          info^.internalrect := Rect(0, 0, ww, hh);
        end;

        DestBitmapScanline := nil;
        XLut := nil;
        YLut := nil;
        iebmp.RenderToTBitmap(DestBitmap, DestBitmapScanline, XLut, YLut, nil, cx1, cy1, ww, hh, 0, 0, iebmp.Width, iebmp.Height, fEnableAlphaChannel, false, 255, filt, true, ielNormal);
        if fEnableImageCaching then
        begin
          info^.cacheImage := fCacheList.AddIEBitmap(iebmp);
          fCacheList.SetImageOriginalWidth(info^.cacheImage,  fImageList.GetImageOriginalWidth(info^.image));
          fCacheList.SetImageOriginalHeight(info^.cacheImage, fImageList.GetImageOriginalHeight(info^.image));

          if StoreType = ietFastThumb then
          begin
            fImageList.Delete(info^.image);
            info^.image := nil;
          end;
        end;
        FreeAndNil(ietmp);
        FreeAndNil(iebmp);
      end;
    end
    else
    if (info^.cacheImage <> nil) then
    begin
      // use cached image
      ww := fCacheList.GetImageWidth(info^.cacheImage);
      hh := fCacheList.GetImageHeight(info^.cacheImage);
      cx1 := x1 + abs(ActThumbWidth - ww) div 2;
      cy1 := y1 + ActUpperGap + abs(ImHeight - hh) div 2;
      pix := fCacheList.GetImageBits(info^.cacheImage);
      alpha := fCacheList.GetAlphaBits(info^.cacheImage);
      ietmp := TIEBitmap.Create;

      ietmp.EncapsulateMemory(pix, ww, hh, fCacheList.GetImagePixelFormat(info^.cacheImage), false);

      if ietmp.PixelFormat = ie8p then
        CopyMemory(ietmp.PaletteBuffer, fCacheList.GetImagePalette(info^.cacheImage), 256*SizeOf(TRGB));

      if alpha <> nil then
      begin
        ietmp.AlphaChannel.EncapsulateMemory(alpha, ww, hh, ie8g, true);
        ietmp.AlphaChannel.Full := false;
      end;
      DestBitmapScanline := nil;
      XLut := nil;
      YLut := nil;
      ietmp.RenderToTBitmap(DestBitmap, DestBitmapScanline, XLut, YLut, nil, cx1, cy1, ww, hh, 0, 0, ietmp.Width, ietmp.Height, true, false, 255, rfNone, true, ielNormal);
      FreeAndNil(ietmp);
    end
    else
    if ietxShowIconWhileLoading in fThumbnailOptionsEx then
    begin
      // display shell icon while we are loading

      if fIconList.RetrieveFromCache(citFullDraw, ImageFilename[Index], info^.SourceType = iestFolderIcon, ietmp, False) then
      begin
        // Ietmp now points to a valid icon image (DON'T FREE)
        cx1 := x1 + abs(ActThumbWidth - ietmp.width) div 2;
        cy1 := y1 + ActUpperGap + abs(ImHeight - ietmp.height) div 2;
        ww := ietmp.Width;
        hh := ietmp.Height;

        DestBitmapScanline := nil;
        XLut := nil;
        YLut := nil;
        ietmp.RenderToTBitmap(DestBitmap, DestBitmapScanline, XLut, YLut, nil, cx1, cy1, ww, hh, 0, 0, ietmp.Width, ietmp.Height, true, false, 255, rfNone, true, ielNormal);
      end
      else
      begin
        ietmp := TIEBitmap.Create;

        // Create icon image
        if info^.SourceType = iestFolderIcon then
          bStretchSmallImages := IEIsWindowsVistaOrNewer and (fIconSize in [ieicStretchHD, ieicStretchAll])
        else
          bStretchSmallImages := IEIsWindowsVistaOrNewer and (fIconSize = ieicStretchAll);
        IEGetFileIcon(ImageFilename[Index], ietmp);
        if (bStretchSmallImages = False) and (fIconSize <> ieicStandardSize) then
          _DoubleIconSize(ietmp);

        iSoftShadowSize := 0;
        if fSoftShadow.Enabled and (info^.SourceType <> iestFolderIcon) then
          iSoftShadowSize := IESoftShadowSize(fSoftShadow.Radius, fSoftShadow.OffsetX, fSoftShadow.OffsetY);
                                                                                                        
        IEGetFitResampleSize(ietmp.Width, ietmp.Height, ImWidth - iSoftShadowSize, ImHeight - iSoftShadowSize, ww, hh);
        if (bStretchSmallImages = False) and  ((ietmp.Width < ImWidth - iSoftShadowSize) or (ietmp.Height < ImHeight - iSoftShadowSize)) then
        begin
          ww := ietmp.Width;
          hh := ietmp.Height;
        end;
        _IEResampleIEBitmap2(ietmp, fThumbnailDisplayFilter, ww, hh, nil, nil);

        cx1 := x1 + (abs(ActThumbWidth - ww) - iSoftShadowSize) div 2;
        cy1 := y1 + ActUpperGap + (abs(ImHeight - hh) - iSoftShadowSize) div 2;

        if iSoftShadowSize > 0 then
        begin
          // deflate ww and hh to correct thumb size
          ww := ww + iSoftShadowSize;
          hh := hh + iSoftShadowSize;

          // APPLY SHADOW
          _IEAddSoftShadow(ietmp, fSoftShadow.Radius, fSoftShadow.OffsetX, fSoftShadow.OffsetY, fSoftShadow.Intensity, true, fSoftShadow.ShadowColor, nil, nil);
        end;

        // Now save it to our cache for next time
        if fEnableImageCaching then
          fIconList.SaveToCache(ietmp, citFullDraw, ImageFilename[Index], info^.SourceType = iestFolderIcon);

        DestBitmapScanline := nil;
        XLut := nil;
        YLut := nil;
        ietmp.RenderToTBitmap(DestBitmap, DestBitmapScanline, XLut, YLut, nil, cx1, cy1, ww, hh, 0, 0, ietmp.Width, ietmp.Height, true, false, 255, rfNone, true, ielNormal);
        FreeAndNil(ietmp);
      end;
    end;

    if (info^.image<>nil) or (info^.cacheImage<>nil) then
    begin
      if fThumbnailsInternalBorder and not (info^.SourceType in [iestFileIcon, iestFolderIcon]) then
      begin
        DestBitmap.Canvas.Pen.Color := fThumbnailsInternalBorderColor;
        DestBitmap.Canvas.Pen.Width := 1;
        DestBitmap.Canvas.Pen.Style := psSolid;
        DestBitmap.Canvas.Brush.Style := bsClear;
        with info^.internalrect do
          DestBitmap.Canvas.Rectangle(cx1 + Left, cy1 + Top, cx1 + Right, cy1 + Bottom);
      end;

      // draw sides background
      if fFillThumbnail then
        with DestBitmap.canvas do
        begin
          // Image is background
          if fDrawImageBackground then
            Brush.Color := info^.Background
          else
          // Normal (Unselected)
          if IsSelected = False then
            Brush.Color := fThumbnailsBackground
          else
          // Selected
          if Enabled then
            Brush.Color := fThumbnailsBackgroundSelected
          else
          // Control Disabled
            Brush.Color := Thumbnail_Background_When_Disabled;

          Brush.Style := bsSolid;
          FillRect(rect(x1, y1, cx1, y1 + ActThumbHeight));                        // left
          FillRect(rect(cx1 + ww, y1, x1 + ActThumbWidth, y1 + ActThumbHeight));   // right
          FillRect(rect(cx1, y1, cx1 + ww, cy1));                                  // up
          FillRect(rect(cx1, cy1 + hh, cx1 + ww, y1 + ActThumbHeight));            // bottom
        end;
    end;

    // draw top, bottom, info texts
    bth := 0;
    wsTop := '';
    wsInfo := '';
    wsBottom := '';
    if fShowText then
    begin
      // TOP TEXT
      wsTop := ReplaceIEMConsts(info^.TopText.Caption, Index);
      if assigned(fOnGetText) then
        fOnGetText(Self, Index, iemtpTop, wsTop);
      if wsTop <> '' then
      begin
        DestBitmap.Canvas.Font.Assign(info^.TopText.Font);
        if Enabled = False then
          DestBitmap.Canvas.Font.Color := Text_Color_When_Disabled
        else
        if IsSelected and (fSelectedFontColor <> Graphics.clNone) then
          DestBitmap.Canvas.Font.Color := fSelectedFontColor;
        DestBitmap.Canvas.Brush.Style := info^.TopText.BackgroundStyle;
        if info^.TopText.BackgroundStyle <> bsClear then
          DestBitmap.Canvas.Brush.Color := info^.TopText.Background;

        wsTop := IETruncateStr(wsTop, info^.TopText.TruncSide, DestBitmap.Canvas, ActThumbWidth - 2 * fTextMargin - Text_Horz_Spacing);
        if (fStyle = iemsFlat) and (info^.TopText.BackgroundStyle<>bsSolid) then
          DestBitmap.Canvas.FillRect(rect(x1, y1, x1 + ActThumbWidth, y1 + IETextHeightW(DestBitmap.Canvas, wsTop)));
        tw := IETextWidthW(DestBitmap.Canvas, wsTop);
        TextOutW(DestBitmap.Canvas.Handle, x1 + (ActThumbWidth - tw) div 2, y1 +  fTextMargin, @wsTop[1], length(wsTop));
      end;

      // INFO TEXT
      wsInfo := ReplaceIEMConsts(info^.InfoText.Caption, Index);
      if assigned(fOnGetText) then
        fOnGetText(Self, Index, iemtpInfo, wsInfo);
      if wsInfo <> '' then
      begin
        DestBitmap.Canvas.Font.Assign(info^.InfoText.Font); 
        if Enabled = False then
          DestBitmap.Canvas.Font.Color := Text_Color_When_Disabled
        else
        if IsSelected and (fSelectedFontColor <> Graphics.clNone) then
          DestBitmap.Canvas.Font.Color := fSelectedFontColor;
        DestBitmap.Canvas.Brush.Style := info^.InfoText.BackgroundStyle;
        if info^.InfoText.BackgroundStyle <> bsClear then
          DestBitmap.Canvas.Brush.Color := info^.InfoText.Background;

        wsInfo := IETruncateStr(wsInfo, info^.InfoText.TruncSide, DestBitmap.Canvas, ActThumbWidth - 2 * fTextMargin - Text_Horz_Spacing);
        if (fStyle = iemsFlat) and (info^.InfoText.BackgroundStyle=bsSolid) then
          DestBitmap.Canvas.FillRect(rect(x1, y1 + ActThumbHeight - ActBottomGap - ith, x1 + ActThumbWidth, y1 + ActThumbHeight - ActBottomGap));
        tw := IETextWidthW(DestBitmap.Canvas, wsInfo);
        TextOutW(DestBitmap.Canvas.Handle, x1 + (ActThumbWidth - tw) div 2, y1 + ActThumbHeight - ActBottomGap - ith, @wsInfo[1], length(wsInfo));
      end;

      // BOTTOM
      wsBottom := ReplaceIEMConsts(info^.BottomText.Caption, Index);
      if assigned(fOnGetText) then
        fOnGetText(Self, Index, iemtpBottom, wsBottom);
      if wsBottom <> '' then
      begin
        DestBitmap.Canvas.Font.Assign(info^.BottomText.Font);
        if Enabled = False then
          DestBitmap.Canvas.Font.Color := Text_Color_When_Disabled
        else
        if IsSelected and (fSelectedFontColor <> Graphics.clNone) then
          DestBitmap.Canvas.Font.Color := fSelectedFontColor;
        DestBitmap.Canvas.Brush.Style := info^.BottomText.BackgroundStyle;
        if info^.BottomText.BackgroundStyle <> bsClear then
          DestBitmap.Canvas.Brush.Color := info^.BottomText.Background;

        wsBottom := IETruncateStr(wsBottom, info^.BottomText.TruncSide, DestBitmap.Canvas, ActThumbWidth - 2 * fTextMargin - Text_Horz_Spacing);
        bth := IETextHeightW(DestBitmap.Canvas, wsBottom);
        if info^.BottomText.BackgroundStyle=bsSolid then
          DestBitmap.Canvas.FillRect(rect(x1, y1 + ActThumbHeight - bth - 1, x1 + ActThumbWidth, y1 + ActThumbHeight));
        tw := IETextWidthW(DestBitmap.Canvas, wsBottom);
        TextOutW(DestBitmap.Canvas.Handle, 1 + x1 + (ActThumbWidth - tw - 2) div 2, y1 + ActThumbHeight - bth - 1 - fTextMargin, @wsBottom[1], length(wsBottom));
        inc(bth, 3);
      end;    

    end; // end of fShowText

    // iemsACD style
    if fStyle = iemsACD then
    begin
      IEDraw3DRect(DestBitmap.Canvas, cx1, cy1, cx1 + ww - 1, cy1 + hh - 1, clGray, clWhite); // around the image
      if (ActBottomGap + ActUpperGap > 0) or (fShowText and (wsInfo <> '')) then
        IEDraw3DRect(DestBitmap.Canvas, x1, y1, x1 + ActThumbWidth - 1, y1 + ActThumbHeight - 1 - bth, clWhite, clBlack); // around entire thumbnail
      if fShowText and (wsBottom <> '') then
        IEDraw3DRect(DestBitmap.Canvas, x1, y1 + ActThumbHeight - bth - 1 + 3, x1 + ActThumbWidth - 1, y1 + ActThumbHeight - 1, clGray, clWhite); // around bottomtext
    end;

    // Draw Checkbox
    DrawCheckbox(DestBitmap.Canvas, Index, Rect(x1, y1, x1 + ActThumbWidth, y1 + ActThumbHeight), IsSelected);

    // call OnImageDraw
    if assigned(fOnImageDraw) then
      fOnImageDraw(self, Index, x1, y1, DestBitmap.Canvas);

    // call OnImageDraw2
    if assigned(fOnImageDraw2) then
      fOnImageDraw2(self, Index, x1, y1, Rect(cx1, cy1, cx1+ww, cy1+hh), DestBitmap.Canvas);

    // thumbnail border
    if fModernStyling = False then
    begin
      DestBitmap.Canvas.Pen.Width := 1;
      if Enabled then
        DestBitmap.Canvas.Pen.Color := fThumbnailsBorderColor
      else
        DestBitmap.Canvas.Pen.Color := Thumbnail_Border_When_Disabled;
      DestBitmap.Canvas.Brush.Style := bsClear;
      sw := fThumbnailsBorderWidth;
      if fThumbsRounded <> 0 then
      begin
        t1 := imin(ActThumbWidth, ActThumbHeight) div fThumbsRounded;
        for w := 1 to sw do
          DestBitmap.Canvas.RoundRect(x1 - w, y1 - w, x1 + ActThumbWidth + w, y1 + ActThumbHeight + w, t1 , t1);
      end
      else
      begin
        for w := 1 to sw do
          DestBitmap.Canvas.Rectangle(x1 - w, y1 - w, x1 + ActThumbWidth + w, y1 + ActThumbHeight + w);
      end;
    end;

    // selection
    if (fModernStyling or IsSelected) {$ifndef OCXVERSION}and (not assigned(fOnBeforeImageDrawEx)){$endif} and ((fThumbnailFrame=nil) or (fThumbnailFrameSelected=nil)) then
    begin
      iec := TIECanvas.Create(DestBitmap.Canvas, true, true);
      if IsSelected = False then
        sw := fThumbnailsBorderWidth
      else
      if Focused then
        sw := fSelectionWidth
      else
        sw := fSelectionWidthNoFocus;

      // Normal (Unselected)
      if IsSelected = False then
        iec.Pen.Color := fThumbnailsBorderColor
      else
      // Selected
      if Enabled then
        iec.Pen.Color := fSelectionColor
      else
      // Control is disabled
        iec.Pen.Color := Thumbnail_Border_When_Disabled;
      iec.Brush.Style := bsClear;

      if fSelectionAntialiased then
      begin
        iec.Pen.Width := sw;
        if sw > 0 then        
          iec.RoundRect(x1 - sw div 2, y1 - sw div 2, x1 + ActThumbWidth + sw div 2, y1 + ActThumbHeight + sw div 2, 4, 4);
      end
      else
      begin
        iec.Pen.Width := 1;
        for w := 1 to sw do
          iec.Rectangle(x1 - w, y1 - w, x1 + ActThumbWidth + w, y1 + ActThumbHeight + w);
      end;
      iec.Free;
    end;

  finally
    LeaveCriticalSection(fThreadCS);
  end;
end;

{!!
<FS>TImageEnMView.IsVisible

<FM>Declaration<FC>
function IsVisible(idx: Integer): Boolean;

<FM>Description<FN>
Returns True if image, <FC>idx<FN>, is currently visible (i.e. not scrolled out of view).

<FM>See Also<FN>
- <A TImageEnMView.GetImageVisibility>

!!}
function TImageEnMView.IsVisible(idx: integer): boolean;
var
  x1, y1: integer;
  info: PIEImageInfo;
begin
  info := PIEImageInfo(fImageInfo[idx]);
  x1 := info^.X - fViewX;
  y1 := info^.Y - fViewY;
  result := _RectXRect(0, 0, width - 1, height - 1, x1, y1, x1 + ThumbWidth - 1, y1 + ThumbHeight - 1);
end;

function TImageEnMView.IsLookAhead(idx: Integer): Boolean;
begin
  result := fLookAheadList.IndexOf(pointer(idx))>-1;
end;


procedure TImageEnMView.PaintBackgroundTo(DestBitmap: TBitmap);
var
  x1, y1: integer;
begin
  // draw global background
  IEDrawBackground([], DestBitmap.Canvas, DestBitmap, fBackgroundStyle, fBackground, 0, 0, ClientWidth, ClientHeight, 0, 0, 0, 0, fChessboardSize, fChessboardBrushStyle, fGradientEndColor, nil, iewoNormal);
  // draw wallpaper
  if assigned(fWallPaper.Graphic) then
  begin
    case fWallPaperStyle of
      iewoNormal:
        begin
          DestBitmap.Canvas.Draw(0, 0, fWallPaper.Graphic);
        end;
      iewoStretch:
        begin
          DestBitmap.Canvas.StretchDraw(rect(0, 0, ClientWidth, ClientHeight), fWallPaper.Graphic);
        end;
      iewoTile:
        begin
          if (fWallPaper.Graphic.Width>0) and (fWallPaper.Graphic.Height>0) then
          begin
            x1 := 0;
            y1 := 0;
            while (y1 < ClientHeight) do
            begin
              DestBitmap.Canvas.Draw(x1, y1, fWallPaper.Graphic);
              inc(x1, fWallPaper.Graphic.Width);
              if x1 >= ClientWidth then
              begin
                x1 := 0;
                inc(y1, fWallPaper.Graphic.Height);
              end;
            end;
          end;
        end;
    end;
  end;
end;


{!!
<FS>TImageEnMView.PaintTo

<FM>Declaration<FC>
procedure PaintTo(Canvas: TBitmap); virtual;     

<FM>Description<FN>
Output the current view to a bitmap
!!}
procedure TImageEnMView.PaintTo(DestBitmap: TBitmap);
var
  q, ne, nn, e: integer;
  info: PIEImageInfo;
  reloop: boolean;
  xsel: boolean; // true if display as selected
  firstvisible, lastvisible: Integer;
  oldalldisplayed: Boolean;
  displayPriority: Integer;
begin
  PaintBackgroundTo(DestBitmap);

  displayPriority := 0;

  repeat
    reloop := false;

    if (fDisplayMode = mdSingle) and (fImageInfo.Count > 0) then
      ne := 1 // one image at the time
    else
      ne := fImageInfo.Count;

    if fPlaying or (fDisplayMode = mdSingle) then
    begin
      if fFrame >= fImageInfo.Count then
        fFrame := fImageInfo.Count - 1;
      if fFrame < 0 then
        fFrame := 0;
      q := fFrame;
    end
    else
      q := 0;

    oldalldisplayed := fAllDisplayed;
    fAllDisplayed := true;
    firstvisible := -1;
    lastvisible := fImageInfo.Count-1;

    for nn := 0 to ne - 1 do
    begin

      if assigned(fOnDrawProgress) and (ne > 1) then
        fOnDrawProgress(Self, trunc(nn / ne * 100), q);
      info := PIEImageInfo(fImageInfo[q]);

      // check that the image is visible
      if IsVisible(q) then
      begin
        if firstvisible=-1 then
          firstvisible := q;
        // try to obtain the image if not available
        if (info^.image = nil) and (info^.cacheImage = nil) then
        begin
          oldalldisplayed := false;
          if (fSelectedItem = q) or (fThreadPoolSize = 0) or (ObtainImageThreaded(q, displayPriority) = false) then
          begin
            if (not ObtainImageNow(q)) and fRemoveCorrupted then
            begin
              // remove corrupted image and re-loop
              DeleteImageNU(nn);
              reloop := true;
              break;
            end;
          end;
        end;
        //
        if fSelectedItem <> q then
        begin
          xsel := false;
          if fEnableMultiSelect then
          begin
            e := fMultiSelectedImages.IndexOf(pointer(q));
            xsel := e > -1;
          end;
        end
        else
          xsel := true;
        xsel := xsel and (fVisibleSelection) and (fDisplayMode = mdGrid);

        if (info^.image = nil) and (info^.cacheImage = nil) then
          fAllDisplayed := false;

        DrawImage(DestBitmap, info, xsel, q);

        inc(displayPriority);

      end
      else
      begin
        if (firstvisible<>-1) and (lastvisible=fImageInfo.Count-1) then
          lastvisible := q-1;
      end;

      inc(q);
      if q = fImageInfo.Count then
        q := 0;
    end;

  until not reloop;

  // lookahead
  if (fLookAhead>0) and (fThreadPoolSize>0) then
  begin
    if (lastvisible+1<fImageinfo.Count) then
    begin
      nn := fLookAhead;
      fLookAheadList.Clear;
      if lastvisible+nn>=fImageInfo.Count then
        nn := fImageInfo.Count-1-lastvisible;
      for q := lastvisible+1 to lastvisible+nn do
      begin
        info := PIEImageInfo(fImageInfo[q]);
        if (info^.image=nil) and (info^.cacheImage=nil) then
        begin
          fLookAheadList.Add(pointer(q));
          ObtainImageThreaded(q, -1);
        end;
      end;
    end;
  end;

  // discard invisible images
  if fMaintainInvisibleImages >- 1 then
  begin
    nn := imax(fLookAhead, fMaintainInvisibleImages);
    for q := 0 to fImageInfo.Count-1 do
      if (q < firstvisible - nn) or (q > lastvisible + nn) then
      begin
        info := PIEImageInfo(fImageInfo[q]);
        if IsOnDemand(info) and (info^.image <> nil) then
        begin
          fImageList.Delete(info^.image);
          info^.image := nil;
        end;
      end;
  end;

  // draw done
  if assigned(fOnDrawProgress) and (ne > 1) then
    fOnDrawProgress(Self, 100, 0);

  if fAllDisplayed and not oldalldisplayed and assigned(fOnAllDisplayed) then
    fOnAllDisplayed(self);
end;

{!!
<FS>TImageEnMView.ReloadImage

<FM>Declaration<FC>
procedure ReloadImage(idx: Integer);

<FM>Description<FN>
Reloads the image at index, <FC>idx<FN>. Works only images loaded on demand (i.e. where you have set <A TImageEnMView.ImageFileName> or <A TImageEnMView.ImageID>).
!!}
procedure TImageEnMView.ReloadImage(idx: Integer);
var
  info: PIEImageInfo;
begin
  info := PIEImageInfo(fImageInfo[idx]);
  if IsOnDemand(info) and (info^.image<>nil) then
  begin
    fImageList.Delete(info^.image);
    info^.image := nil;
    if info^.cacheImage <> nil then
    begin
      fcacheList.Delete(info^.cacheImage);
      info^.cacheImage := nil;
    end;
  end;
  UpdateEx(false);
end;

{!!
<FS>TImageEnMView.DisplayMode

<FM>Declaration<FC>
property DisplayMode: <A TIEMDisplayMode>;

<FM>Description<FN>
<TABLE>
<R> <H>View</H> <H>Method</H> </R>
<R> <C>Frame View</C> <C>To display a single frame of a multi-frame image (e.g. TIFF or AVI) set DisplayMode to mdSingle</C> </R>
<R> <C>Thumbnail View</C> <C>To display a vertically scrollable thumbnail grid set DisplayMode to mdGrid, and <A TImageEnMView.GridWidth> to -1</C> </R>
<R> <C>Single Row</C> <C>Set DisplayMode to mdGrid, and <A TImageEnMView.GridWidth> to 0</C> </R>
<R> <C>Single Column</C> <C>Set DisplayMode to mdGrid, and <A TImageEnMView.GridWidth> to 1</C> </R>
</TABLE>

Note: Animation display modes are also available using <A TImageEnMView.Animation>

!!}
procedure TImageEnMView.SetDisplayMode(v: TIEMDisplayMode);
begin
  if fShowThumbnailHint then
    Hint := '';
  if fPlaying then
    fSaveDM := v
  else
  begin
    fDisplayMode := v;
    Update;
  end;
end;

{!!
<FS>TImageEnMView.GridWidth

<FM>Declaration<FC>
property GridWidth: integer;

<FM>Description<FN>
GridWidth is the number of images per row. This property is active only when <A TImageEnMView.DisplayMode> is mdGrid.

Valid GridWidth values are:

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>-1</C> <C>Automatically adjust column count to match the component width (standard usage)</C> </R>
<R> <C>0</C> <C>Only one row</C> </R>
<R> <C>>0</C> <C>GridWidth images per row, e.g. set to 1 for a single vertical column</C> </R>
</TABLE>

You can center the thumbnail column(s) using <A TImageEnMView.ThumbnailOptionsEx>

<FM>See Also<FN>
- <A TImageEnMView.DisplayMode>

!!}
procedure TImageEnMView.SetGridWidth(v: integer);
begin
  fGridWidth := v;
  fCurrentGridWidth := v;
  Update;
end;


                            

{!!
<FS>TImageEnMView.Playing

<FM>Declaration<FC>
property Playing: boolean;

<FM>Description<FN>
Enable Playing to display each image consecutively for its <A TImageEnMView.ImageDelayTime> time.
During playback, <A TImageEnMView.DisplayMode> property is set to mdSingle and the <A TImageEnMView.Deselect> method is called.
If <A TImageEnMView.PlayLoop> is enabled then the the animation will replay continuously

!!}
procedure TImageEnMView.SetPlaying(v: boolean);
begin
  if v = fPlaying then
    exit;
  if v then
  begin
    fSaveDM := fDisplayMode;
    fSaveSel := fSelectedItem;
    Deselect;
  end;
  fPlaying := v;
  PlayFrame;
  if not fPlaying then
  begin
    SetDisplayMode(fSaveDM);
    SetSelectedItem(fSaveSel);
  end;
end;



{!!
<FS>TImageEnMView.CenterFrame

<FM>Declaration<FC>
procedure CenterFrame;

<FM>Description<FN>
Scrolls the view (by calling <A TImageEnMView.SetViewXY>) to display the current frame at a central position in the view.

Note: This method uses <A TImageEnMView.VisibleFrame> to determine the current frame, whereas <A TImageEnMView.CenterSelected> uses <A TImageEnMView.SelectedImage>.
!!}
// show the image indexed by VisibleFrame, put it at the center (when applicable)
procedure TImageEnMView.CenterFrame;
var
  info: PIEImageInfo;
  x, y: integer;
begin
  if fSelectedItem >= 0 then
  begin
    info := PIEImageInfo(fImageInfo[fFrame]);
    X := info^.X - ((ClientWidth - ThumbWidth) div 2);
    Y := info^.Y - ((ClientHeight - ThumbHeight) div 2);
    SetViewXY(X, Y);
  end;
end;

procedure TImageEnMView.PlayFrame;
var
  info: PIEImageInfo;
  rr: TRect;
  iInterval: Integer;
  bShowFrame : Boolean;
begin
  if fTimerInProgress then
    exit;
  if csDestroying in ComponentState then
    exit;

  bShowFrame := True;
  fTimerInProgress := true;
  // remove timer
  if fPlayTimer <> 0 then
  begin
    KillTimer(self.handle, 1);
    fPlayTimer := 0;
  end;

  if fPlaying then
  begin
    if fDisplayMode <> mdSingle then
    begin
      fDisplayMode := mdSingle;
      Update;
    end;
    if fFrame >= fImageInfo.Count then
      fFrame := fImageInfo.Count - 1;
    if fFrame < 0 then
      exit;
    info := PIEImageInfo(fImageInfo[fFrame]);
    // show current frame
    if assigned(fOnPlayFrame) then
      fOnPlayFrame(self, fFrame, bShowFrame);

    if bShowFrame then
    begin
      Paint;
      rr := rect(0, 0, clientwidth, clientheight);
      ValidateRect(self.handle, @rr); // cancel invalidate executed by CenterSelected
    end;

    // another loop
    // prepare for next frame
    if fFrame = fImageInfo.Count - 1 then
    begin
      fFrame := 0;
      CallBitmapChangeEvents;
      if not fPlayLoop then
      begin
        fPlaying := false;
        fTimerInProgress := false;
        exit; // EXIT!
      end;
    end
    else
      inc(fFrame);
               
    if bShowFrame = False then
    begin
      // Skip this frame
      PostMessage(Self.Handle, WM_TIMER, 0, 0);
    end
    else
    begin
      // run timer
      iInterval := round(info^.DTime);
      if (iInterval < 1) and (IEExtractFileExtW(info^.Filename) = '.gif') then
        iInterval := Default_GIF_Animation_Delay_MS
      else
      if iInterval < 10 then
        iInterval := 10;   // compatibility with pre-v5.0.7 versions
      fPlayTimer := SetTimer(self.handle, 1, iInterval, nil);
    end;
  end;
  fTimerInProgress := false;
end;

procedure TImageEnMView.WMTimer(var Message: TWMTimer);
begin
  PlayFrame;
end;


{!!
<FS>TImageEnMView.GetLastOp

<FM>Declaration<FC>
function GetLastOp: integer;

<FM>Description<FN>
Undocumented. INTERNAL USE ONLY!
!!}
// look at fLastImOp
function TImageEnMView.GetLastOp: integer;
begin
  result := fLastImOp;
end;



{!!
<FS>TImageEnMView.GetLastOpIdx

<FM>Declaration<FC>
function GetLastOpIdx: integer;

<FM>Description<FN>
Undocumented. INTERNAL USE ONLY!
!!}
// look at fLastImIdx
function TImageEnMView.GetLastOpIdx: integer;
begin
  result := fLastImIdx;
end;

// look at fLastImP1
function TImageEnMView.GetLastOpP1: integer;
begin
  result := fLastImP1;
end;

// erase fLastImOp (in old versions this was done by GetLastOp)
procedure TImageEnMView.CallBitmapChangeEvents;
begin
  inherited;
  fLastImOp := 0;
end;


{!!
<FS>TImageEnMView.Clear

<FM>Declaration<FC>
procedure Clear;

<FM>Description<FN>
Removes all images from the TImageEnMView and releases any associated memory.
!!}
// recreate temp file (you can change the DefTEMPPATH and call Clear to make changes active)
procedure TImageEnMView.Clear;
begin
  Deselect;
  ClearThreadsAndRequests;
  DeleteAllImages();
  FreeAndNil(fImageList);
  fImageList := TIEVirtualImageList.Create('ILIST', fImageCacheUseDisk);
  fImageList.MaxImagesInMemory := fImageCacheSize;
  ClearOnDemandIOList;
  FreeAndNil(fCacheList);
  fCacheList := TIEVirtualImageList.Create('ICACHE', fImageCacheUseDisk);
  fIconList.Clear();
  Update;
end;

{!!
<FS>TImageEnMView.LockPaint

<FM>Declaration<FC>
procedure LockPaint;

<FM>Description<FN>
Increments the <L TImageEnMView.LockPaintCount>lock paint counter</L>. While <A TImageEnMView.LockPaintCount> is greater than zero all painting is disabled.

Use <A TImageEnMView.UnLockPaint> to unlock.

<FM>Example<FC>
// Disable painting of component
ImageEnMView1.LockPaint;
try
  ... Perform activities, e.g. appending many files
finally
  // Re-enable painting and refresh view
  ImageEnMView1.UnlockPaint;
end;

!!}
// increases fLockPaint
procedure TImageEnMView.LockPaint;
begin
  inc(fLockPaint);
end;

{!!
<FS>TImageEnMView.UnlockPaint

<FM>Declaration<FC>
function UnlockPaint: integer;

<FM>Description<FN>
Decrement the <L TImageEnMView.LockPaintCount>lock paint counter</L> (use after calling <A TImageEnMView.LockPaint>).

If the lock count is zero, then <A TImageEnMView.Update> is called to refresh the view.

Returns the lock count.

<FM>Example<FC>
// Disable painting of component
ImageEnMView1.LockPaint;
try
  ... Perform activities, e.g. appending many files
finally
  // Re-enable painting and refresh view
  ImageEnMView1.UnlockPaint;
end;
!!}
// decreases fLockPaint
// ret. current value (after the decrement)
function TImageEnMView.UnLockPaint: integer;
begin
  if fLockPaint > 0 then
    dec(fLockPaint);
  if fLockPaint = 0 then
  begin
    if assigned(fAnimation) then
      fAnimation.ImageCount := ImageCount;
    UpdateEx(true, fSelectedImageAlwaysVisible);
  end;
  result := fLockPaint;
end;

// Decreases fLockPaint
// ret current value (after the decrement)
// doesn't call Update if fLockpaint=0
function TImageEnMView.NPUnLockPaint: integer;
begin
  if fLockPaint > 0 then
    dec(fLockPaint);
  result := fLockPaint;
end;

{!!
<FS>TImageEnMView.LockUpdate

<FM>Declaration<FC>
procedure LockUpdate;

<FM>Description<FN>
Increments the <L TImageEnMView.LockUpdateCount>lock update counter</L>. While <A TImageEnMView.LockUpdateCount> is greater than zero all component updating is disabled.

Use <A TImageEnMView.UnLockUpdate> to unlock.

<FM>Example<FC>
// Disable updating of component
ImageEnMView1.LockUpdate;
try
  ... Perform activities, e.g. appending many files
finally
  // Re-enable Updating and refresh view
  ImageEnMView1.UnlockUpdate;
end;



The LockUpdate method increases the lock counter's value.
Use <A TImageEnMView.UnLockUpdate> to unlock.
!!}
// increases fLockUpdate
procedure TImageEnMView.LockUpdate;
begin
  inc(fLockUpdate);
end;

{!!
<FS>TImageEnMView.UnLockUpdate

<FM>Declaration<FC>
function UnlockUpdate: integer;

<FM>Description<FN>
Decrement the <L TImageEnMView.LockUpdateCount>lock update counter</L> (use after calling <A TImageEnMView.LockUpdate>).

If the lock count is zero, then <A TImageEnMView.Update> is called to refresh the view.

Returns the lock count.

<FM>Example<FC>
// Disable updating of component
ImageEnMView1.LockUpdate;
try
  ... Perform activities, e.g. appending many files
finally
  // Re-enable updating and call update
  ImageEnMView1.UnlockUpdate;
end;

!!}
// decreases fLockUpdate
// ret. current value (after the decrement)
function TImageEnMView.UnLockUpdate: integer;
begin
  Result := UnLockUpdateEx;
end;


function TImageEnMView.UnLockUpdateEx: integer;
begin
  if fLockUpdate > 0 then
    dec(fLockUpdate);
  if fLockUpdate = 0 then
  begin        
    if assigned(fAnimation) then
      fAnimation.ImageCount := ImageCount;
    UpdateEx(true, fSelectedImageAlwaysVisible);
  end;
  result := fLockUpdate;
end;


{!!
<FS>TImageEnMView.GetImageVisibility

<FM>Declaration<FC>
function GetImageVisibility(idx: Integer): Integer;

<FM>Description<FN>
Returns 0 if image, <FC>idx<FN>, is not visible, 1 if it's partially visible, or 2 if it's fully visible (i.e. not scrolled out of view).
       
<FM>See Also<FN>
- <A TImageEnMView.IsVisible>
!!}
function TImageEnMView.GetImageVisibility(idx: integer): integer;
var
  x1, y1: integer;
  info: PIEImageInfo;
begin
  result := 0;
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    info := PIEImageInfo(fImageInfo[idx]);
    x1 := info^.X - fViewX;
    y1 := info^.Y - fViewY;
    result := _RectPRect(0, 0, clientwidth - 1, clientheight - 1, x1, y1, x1 + ThumbWidth - 1, y1 + ThumbHeight - 1);
  end;
end;

{!!
<FS>TImageEnMView.UpdateImage

<FM>Declaration<FC>
procedure UpdateImage(idx: Integer);

<FM>Description<FN>
Redraw the image, <FC>idx<FN>.

Whenever an image (Bitmap) within a TImageEnMView changes, you must update the component with <A TImageEnMView.Update> or <A TImageEnMView.UpdateImage>.

<FM>Example<FC>
// Draw a rectangle on the fourth image
var
  bmp: TBitmap;
begin
  bmp := ImageEnMView1.GetBitmap(3);
  bmp.canvas.rectangle(0, 0, 10, 10);
  ImageEnMView1.ReleaseBitmap(3);
  UpdateImage(3);
end;
!!}
procedure TImageEnMView.UpdateImage(idx: integer);
begin
  _UpdateImage(idx, False);
end;

procedure TImageEnMView._UpdateImage(idx: integer; bInvalidateOnly : Boolean);
var
  rc: TRect;
  x1, y1: integer;
  info: PIEImageInfo;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) then
  begin
    if bInvalidateOnly = False then
    begin
      ClearImageCache(idx);
      UpdateEx(false);
    end;
    ValidateRect(self.handle, nil);

    if assigned(fAnimation) then
      fAnimation.RestartAnimation();

    info := PIEImageInfo(fImageInfo[idx]);
    x1 := info^.X - fViewX;
    y1 := info^.Y - fViewY;
    rc := Rect(x1, y1, x1 + ThumbWidth, y1 + ThumbHeight);
    InvalidateRect(self.handle, @rc, false);
  end;
end;

procedure TImageEnMView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  case msg.CharCode of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    VK_PRIOR, VK_NEXT, VK_HOME, VK_END:
      msg.Result := 1;
  end;
end;

{!!
<FS>TImageEnMView.CenterSelected

<FM>Declaration<FC>
procedure CenterSelected;

<FM>Description<FN>   
Scrolls the view (by calling <A TImageEnMView.SetViewXY>) to display the currently selected image at a central position in the view.

Note: This method uses <A TImageEnMView.SelectedImage> to determine the selection, whereas <A TImageEnMView.CenterFrame> uses <A TImageEnMView.VisibleFrame>.

<FM>Example<FC>
ImageEnMView1.SelectedImage := 10;
ImageEnMView1.CenterSelected;
!!}
procedure TImageEnMView.CenterSelected;
var
  info: PIEImageInfo;
  x, y: integer;
begin
  if fSelectedItem >= 0 then
  begin
    info := PIEImageInfo(fImageInfo[fSelectedItem]);
    X := info^.X - ((ClientWidth - ThumbWidth) div 2);
    Y := info^.Y - ((ClientHeight - ThumbHeight) div 2);
    SetViewXY(X, Y);
  end;
end;

{!!
<FS>TImageEnMView.SelectSeek

<FM>Declaration<FC>
procedure SelectSeek(pos: <A TIESeek>);

<FM>Description<FN>
Moves the selection by the method specified by <FC>pos<FN>. SelectSeek will scroll the view, where necessary, to ensure the newly selected image is visible.

<FM>Example<FC>
// This code loads "film.avi" and select the first image
ImageEnMView1.MIO.LoadFromFile('film.avi');
ImageEnMView1.SelectSeek(iskFirst);

!!}
procedure TImageEnMView.SelectSeek(pos: TIESeek);
var
  info: PIEImageInfo;
  gw, gh: integer;
begin
  if fImageInfo.Count = 0 then
    exit;
    
  fHoverCheckLastPos.X := -1;
  fHoverCheckLastIdx   := -1;

  gw := CalcGridWidth();
  gh := (ClientHeight - fVertBorder) div (ThumbHeight + fVertBorder);

  case pos of
    iskLeft:
      if fSelectedItem >= 0 then
      begin
        SetSelectedItem(fSelectedItem - 1);
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
        begin
          info := PIEImageInfo(fImageInfo[fSelectedItem]);
          SetViewXY(info^.X - fHorizBorder, info^.Y - fVertBorder);
        end;
      end;
    iskRight:
      if fSelectedItem >= -1 then
      begin
        SetSelectedItem(fSelectedItem + 1);
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
        begin
          info := PIEImageInfo(fImageInfo[fSelectedItem]);
          SetViewXY(info^.X - clientwidth + ThumbWidth + fHorizBorder, info^.Y - clientheight + ThumbHeight + fVertBorder);
        end;
      end;
    iskUp:
      if fSelectedItem >= 0 then
      begin
        if fCurrentGridWidth = 0 then
          // one row of infinite columns
          SetSelectedItem(fSelectedItem - 1)
        else
        begin
          // more rows of "gw" columns
          if fSelectedItem - gw >= 0 then
            SetSelectedItem(fSelectedItem - gw);
        end;
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
        begin
          info := PIEImageInfo(fImageInfo[fSelectedItem]);
          SetViewY(info^.Y - fVertBorder);
          if GetImageVisibility(fSelectedItem) <> 2 then
            SetViewX(info^.X - fHorizBorder);
        end;
      end;
    iskDown:
      if fSelectedItem >= -1 then
      begin
        if fCurrentGridWidth = 0 then
          // one row of infinite columns
          SetSelectedItem(fSelectedItem + 1)
        else
        begin
          // more row of gw columns
          if fSelectedItem + gw < fImageInfo.Count then
            SetSelectedItem(fSelectedItem + gw);
        end;
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
        begin
          info := PIEImageInfo(fImageInfo[fSelectedItem]);
          SetViewY(info^.Y - clientheight + ThumbHeight + fVertBorder);
          if GetImageVisibility(fSelectedItem) <> 2 then
            SetViewX(info^.X - clientwidth + ThumbWidth + fHorizBorder);
        end;
      end;
    iskFirst:
      begin
        SetSelectedItem(0);
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
          SetViewXY(0, 0);
      end;
    iskLast:
      begin
        SetSelectedItem(fImageInfo.Count - 1);
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
        begin
          info := PIEImageInfo(fImageInfo[fSelectedItem]);
          SetViewY(info^.Y - clientheight + ThumbHeight + fVertBorder);
          if GetImageVisibility(fSelectedItem) <> 2 then
            SetViewX(info^.X - clientwidth + ThumbWidth + fHorizBorder);
        end;
      end;
    iskPagDown:
      if fSelectedItem >= -1 then
      begin
        SetSelectedItem( imin(fSelectedItem + gw * gh , fImageInfo.Count-1) );
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
        begin
          info := PIEImageInfo(fImageInfo[fSelectedItem]);
          SetViewY(info^.Y - clientheight + ThumbHeight + fVertBorder);
          if GetImageVisibility(fSelectedItem) <> 2 then
            SetViewX(info^.X - clientwidth + ThumbWidth + fHorizBorder);
        end;
      end;
    iskPagUp:
      if fSelectedItem >= 0 then
      begin
        SetSelectedItem( imax(fSelectedItem - gw * gh , 0) );
        if (fSelectedItem >= 0) and (GetImageVisibility(fSelectedItem) <> 2) then
        begin
          info := PIEImageInfo(fImageInfo[fSelectedItem]);
          SetViewY(info^.Y - fVertBorder);
          if GetImageVisibility(fSelectedItem) <> 2 then
            SetViewX(info^.X - fHorizBorder);
        end;
      end;
  end;
end;

procedure TImageEnMView.KeyDown(var Key: Word; Shift: TShiftState);
var
  lMultiSelecting: boolean;
  lSelectInclusive: boolean;
begin
  inherited;
  fUserAction := true;
  try
    if fPlaying then
      exit;
    if mkiMoveSelected in fKeyInteract then
    begin

      if assigned(fAnimation) then
      begin
        // in animation behavior
        if (Key = VK_LEFT) or (Key = VK_UP) then
          SelectedImage := SelectedImage - 1
        else
        if (Key = VK_RIGHT) or (Key = VK_DOWN) then
          SelectedImage := SelectedImage + 1
      end
      else 
      if CheckSelectionChangingAllowed then
      begin
        // default behavior
        lMultiSelecting := fMultiSelecting;
        lSelectInclusive := fSelectInclusive;
        if fEnableMultiSelect and ((ssCtrl in Shift) or (ssShift in Shift)) then
        begin
          fMultiSelecting := true;
          fSelectInclusive := true;
        end;
        try
          case Key of
            VK_LEFT:
              begin
                SelectSeek(iskLeft);
                ViewChange(0);
              end;
            VK_RIGHT:
              begin
                SelectSeek(iskRight);
                ViewChange(0);
              end;
            VK_UP:
              begin
                SelectSeek(iskUp);
                ViewChange(0);
              end;
            VK_DOWN:
              begin
                SelectSeek(iskDown);
                ViewChange(0);
              end;
            VK_PRIOR:
              begin
                SelectSeek(iskPagUp);
                ViewChange(0);
              end;
            VK_NEXT:
              begin
                SelectSeek(iskPagDown);
                ViewChange(0);
              end;
            VK_HOME:
              begin
                SelectSeek(iskFirst);
                ViewChange(0);
              end;
            VK_END:
              begin
                SelectSeek(iskLast);
                ViewChange(0);
              end;
          else
            exit;
          end;
        finally
          fMultiSelecting := lMultiSelecting;
          fSelectInclusive := lSelectInclusive;
        end;
        if fEnableMultiSelect and (Shift = []) then
          fPriorHSIDX := fSelectedItem;
        UpdateEx(false);
      end;
    end;

  finally
    fUserAction := false;
    DoAfterEvent(ieaeKeyDown);
  end;
end;

procedure TImageEnMView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  DoAfterEvent(ieaeKeyUp);
end;

procedure TImageEnMView.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  invalidate;
end;

procedure TImageEnMView.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  invalidate;
end;


procedure TImageEnMView.WMNCPaint(var Message: TMessage);  
{$IFDEF THEMED_BORDERS}
var
  R: TRect;
  Details: TThemedElementDetails;
  DC: HDC;
  XEdge, YEdge: Integer;
{$ENDIF}
begin
  inherited;

{$IFDEF THEMED_BORDERS}
  if ThemeServices.ThemesEnabled and (Ctl3D = False) then
  begin
    R := Rect(0, 0, Width, Height);
    DC := GetWindowDC(Handle);
    XEdge := IEGlobalSettings().BorderX;
    YEdge := IEGlobalSettings().BorderY;
    ExcludeClipRect(DC, XEdge, YEdge, Width - XEdge, Height - YEdge);
    try
      Details := ThemeServices.GetElementDetails((teEditTextNormal));
      ThemeServices.DrawParentBackground(Handle, DC, @Details, True);
      ThemeServices.DrawElement(DC, Details, R);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
{$ENDIF}
end;


{!!
<FS>TImageEnMView.MouseInteract

<FM>Declaration<FC>
property MouseInteract: <A TIEMMouseInteract>;

<FM>Description<FN>
Specifies the effect of mouse actions upon the component.
!!}
function TImageEnMView.GetMouseInteract: TIEMMouseInteract;
begin
  result := fMouseInteract;
end;

{!!
<FS>TImageEnMView.KeyInteract

<FM>Declaration<FC>
property KeyInteract: <A TIEMKeyInteract>

<FM>Description<FN>
Specifies which keyboard activities the component will handle automatically.
!!}
function TImageEnMView.GetKeyInteract: TIEMKeyInteract;
begin
  result := fKeyInteract;
end;

{!!
<FS>TImageEnMView.RemoveCorrupted

<FM>Declaration<FC>
property RemoveCorrupted: boolean;

<FM>Description<FN>
If enabled, TImageEnMView will automatically remove any corrupted (unreadable) images from the grid.
!!}
procedure TImageEnMView.SetRemoveCorrupted(v: boolean);
begin
  fRemoveCorrupted := v;
  Update;
end;

{!!
<FS>TImageEnMView.DrawImageBackground

<FM>Declaration<FC>
property DrawImageBackground: boolean;

<FM>Description<FN>
If enabled, the background color of the thumbnail will be set to that of the current image. Otherwise it is set to <A TImageEnMView.ThumbnailsBackground>.

!!}
procedure TImageEnMView.SetDrawImageBackground(v: boolean);
begin
  fDrawImageBackground := v;
  Update;
end;

{!!
<FS>TImageEnMView.ScrollBarsAlwaysVisible

<FM>Declaration<FC>
property ScrollBarsAlwaysVisible: Boolean;

<FM>Description<FN>
When enabled, the scrollbars specified by <A TImageEnMView.ScrollBars> will always be displayed even when not required.

!!}
function TImageEnMView.GetScrollBarsAlwaysVisible: boolean;
begin
  result := fScrollBarsAlwaysVisible;
end;

procedure TImageEnMView.SetScrollBarsAlwaysVisible(v: boolean);
begin
  fScrollBarsAlwaysVisible := v;
  Update;
end;

{!!
<FS>TImageEnMView.SetImageFromFile

<FM>Declaration<FC>
function SetImageFromFile(idx: Integer; const FileName: WideString; SourceImageIndex: Integer = 0): Boolean;

<FM>Description<FN>
Loads an image from a file and assigns it to the TImageEnMView at index, <FC>idx<FN>.
Use SourceImageIndex to specify the image index if the source file is a multi-frame file (such as a TIFF or AVI).

<FM>Example<FC>
idx := ImageEnMView1.AppendImage;
ImageEnMView1.SetImageFromFile(idx, 'D:\myfile.jpg');

!!}
function TImageEnMView.SetImageFromFile(idx: integer; const FileName: WideString; SourceImageIndex: Integer): boolean;
var
  ms: TMemoryStream;
  FileExt: string;
begin
  if IEGetURLTypeW(FileName) <> ieurlUNKNOWN then
  begin
    // LOAD FROM URL
    ms := TMemoryStream.Create;
    try
      Result := IEGetFromURL(FileName, ms, GetImageEnMIO.ProxyAddress, GetImageEnMIO.ProxyUser, GetImageEnMIO.ProxyPassword, nil, nil, @GetImageEnMIO.Aborting, FileExt);
      if Result then
      begin
        ms.Position := 0;
        Result := SetImageFromStreamOrFile(Idx, ms, '', SourceImageIndex);
      end;
    finally
      FreeAndNil(ms);
    end;
  end
  else
  begin
    // LOAD FROM FILE
    result := SetImageFromStreamOrFile(idx, nil, FileName, SourceImageIndex);
  end;
  
  if assigned(fAnimation) then
    fAnimation.RestartAnimation();
end;

{!!
<FS>TImageEnMView.SetImageFromStream

<FM>Declaration<FC>
function SetImageFromStream(idx: Integer; Stream: TStream; SourceImageIndex: Integer = 0): Boolean;

<FM>Description<FN>      
Loads an image from a stream and assigns it to the TImageEnMView at index, <FC>idx<FN>.
Use SourceImageIndex to specify the image index if the source file is a multi-frame file (such as a TIFF or AVI).

<FM>Example<FC>
idx := ImageEnMView1.AppendImage;
ImageEnMView1.SetImageFromStream(idx, stream);

!!}
function TImageEnMView.SetImageFromStream(idx: integer; Stream: TStream; SourceImageIndex: Integer): boolean;
begin
  result := SetImageFromStreamOrFile(idx, Stream, '', SourceImageIndex);
  if assigned(fAnimation) then
    fAnimation.RestartAnimation();
end;

function TImageEnMView.SetImageFromStreamOrFile(idx: integer; Stream: TStream; const FileName: WideString; SourceImageIndex: Integer): boolean;
var
  bmp: TIEBitmap;
  info: PIEImageInfo;
  ASourceType : TIESourceType;
begin
  if idx >= fImageInfo.Count then
  begin
    result := false;
    exit;
  end;
  result := true;
  info := PIEImageInfo(fImageInfo[idx]);  
  ASourceType := info^.SourceType;
  bmp := TIEBitmap.Create;
  fImageEnIO.Background := info^.Background;
  fImageEnIO.attachediebitmap := bmp;
  fImageEnIO.OnProgress := fOnIOProgress;
  fImageEnIO.AutoAdjustDPI := MIO.AutoAdjustDPI;  // 3.0.2
  fImageEnIO.Params.ImageIndex := SourceImageIndex; // 3.0.2
  _SetLoadParams(Self, fImageEnIO.Params);
  try
    if Stream <> nil then
      fImageEnIO.LoadFromStream(Stream)
    else
    {$IFDEF VIDEO_THUMBNAILS}
    if (fStoreType in [ietThumb, ietFastThumb]) and UseThumbnailFromExplorer(FileName) then
      fImageEnIO.LoadThumbnailFromExplorer(FileName, ThumbWidth, ThumbHeight)
    else
    {$ENDIF}
      fImageEnIO.LoadFromFileAuto(FileName);
  except
    fImageEnIO.Aborting := true;
  end;
  //
  if fImageEnIO.Aborting then
  begin
    DoWrongImage(bmp, idx, ASourceType);
    result := false;
  end;
  // updates params of encapsulated TImageEnMIO object
  GetImageEnMIO.Params[idx].Assign(fImageEnIO.Params); // GetImageEnMIO creates TImageEnMIO if it doesn't exist
  // set the image
  info^.Background := fImageEnIO.Background;     
  info^.SourceType := ASourceType;
  SetIEBitmapEx(idx, bmp);
  ImageOriginalWidth[idx]  := fImageEnIO.Params.OriginalWidth;
  ImageOriginalHeight[idx] := fImageEnIO.Params.OriginalHeight;
  if fImageEnIO.Params.EXIF_DateTimeOriginal2 > 0 then
    ImageCreateDate[idx] := fImageEnIO.Params.EXIF_DateTimeOriginal2;  
  //
  fImageEnIO.attachediebitmap := nil;
  FreeAndNil(bmp);
  ClearImageCache(idx);
  UpdateEx(false);
end;

{!!
<FS>TImageEnMView.GetImageToFile

<FM>Declaration<FC>
procedure GetImageToFile(idx: Integer; const FileName: WideString);

<FM>Description<FN>
Saves the image at index, <FC>idx<FN>, to file. The file format is determined by the file extension.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>idx<FN></C> <C>The image index (0=first image)</C> </R>
<R> <C><FC>FileName<FN></C> <C>The destination path and file name</C> </R>
</TABLE>


<FM>Example<FC>
// Separate the pages of a multipage TIFF
ImageEnMView1.MIO.LoadFromFile('multipage.tif');
ImageEnMView1.GetImageToFile(0, 'page1.tif');
ImageEnMView1.GetImageToFile(1, 'page2.tif');

// Save the first image to a JPEG file (at 80% quality)
ImageEnMView1.MIO.Params[0].JPEG_Quality := 80;
ImageEnMView1.GetImageToFile(0, 'D:\MyImage.jpg');
!!}
procedure TImageEnMView.GetImageToFile(idx: Integer; const FileName: WideString);
var
  bmp: TIEBitmap;
begin
  fImageEnIO.Params.Assign( GetImageEnMIO.Params[idx] );
  bmp := GetTIEBitmap(idx);
  try
    fImageEnIO.AttachedIEBitmap := bmp;
    fImageEnIO.SaveToFile(FileName);
  finally
    ReleaseBitmap(idx, false);
  end;
end;

{!!
<FS>TImageEnMView.GetImageToStream

<FM>Declaration<FC>
procedure GetImageToStream(idx: Integer; Stream: TStream; ImageFormat: <A TIOFileType>);

<FM>Description<FN>
Saves the image at index, <FC>idx<FN>, to a stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>idx<FN></C> <C>The image index (Where 0 is the first image)</C> </R>
<R> <C><FC>Stream<FN></C> <C>The destination stream</C> </R>
<R> <C><FC>ImageFormat<FN></C> <C>The output file format (e.g. ioTiff or ioJpeg)</C> </R>
</TABLE>        

<FM>Example<FC>
// Save the first image to a stream in JPEG format (at 80% quality)
ImageEnMView1.MIO.Params[0].JPEG_Quality := 80;
ImageEnMView1.GetImageToFile(0, MyStream, ioJPEG);

!!}
procedure TImageEnMView.GetImageToStream(idx: Integer; Stream: TStream; ImageFormat: TIOFileType);
var
  bmp: TIEBitmap;
begin
  fImageEnIO.Params.Assign( GetImageEnMIO.Params[idx] );
  bmp := GetTIEBitmap(idx);
  try
    fImageEnIO.AttachedIEBitmap := bmp;
    fImageEnIO.SaveToStream(Stream, ImageFormat);
  finally
    ReleaseBitmap(idx, false);
  end;
end;


{!!
<FS>TImageEnMView.PrepareSpaceFor

<FM>Declaration<FC>
procedure PrepareSpaceFor(Width, Height: Integer; Bitcount: Integer; ImageCount: Integer);

<FM>Description<FN>
Allocates enough space within the temporary file for <FC>ImageCount><FN> images of size <FC>Width<FN> * <FC>Height<FN> * <FC>BitCount<FN>.

Use this method to improve performance only when you plan to add many images of the same size.

!!}
procedure TImageEnMView.PrepareSpaceFor(Width, Height: integer; Bitcount: integer; ImageCount: integer);
begin
  fImageList.PrepareSpaceFor(Width, Height, Bitcount, ImageCount);
end;

{!!
<FS>TImageEnMView.ImageCacheSize

<FM>Declaration<FC>
property ImageCacheSize: Integer;

<FM>Description<FN>
Specifies the number of images to be stored in memory, rather than in a memory mapped file. For example, if you know that a TImageEnMView will only contain 20 images then the ImageCacheSize could be set to 20 to disable all memory mapping.

The default value is 10.
!!}
procedure TImageEnMView.SetImageCacheSize(v: integer);
begin
  fImageCacheSize := v;
  fImageList.MaxImagesInMemory := fImageCacheSize;
end;

{!!
<FS>TImageEnMView.ImageCacheUseDisk

<FM>Declaration<FC>
property ImageCacheUseDisk: Boolean;

<FM>Description<FN>
When enabled (default), a disk file is used to cache the images and view. Otherwise only system memory is used.

Disabling this option is useful if you have low disk space or don't want ImageEn to write to disk.

Warning: Setting this property will also call <A TImageEnMView.Clear>
!!}
procedure TImageEnMView.SetImageCacheUseDisk(v: boolean);
begin
  if fImageCacheUseDisk <> v then
  begin
    fImageCacheUseDisk := v;
    Clear;
  end;
end;

{!!
<FS>TImageEnMView.VisibleFrame

<FM>Declaration<FC>
property VisibleFrame: Integer;

<FM>Description<FN>
Specifies the visible image when <A TImageEnMView.DisplayMode> is mdSingle or <A TImageEnMView.Playing> is <FC>True<FN>.
!!}
procedure TImageEnMView.SetVisibleFrame(v: integer);
begin
  if (v = fFrame) or (v < 0) or (v >= fImageInfo.Count) then
    exit;
  if fTransitionEffect <> iettNone then
  begin
    fTransition.Transition := fTransitionEffect;
    fTransition.Duration := fTransitionDuration;
    fTransition.SetSizes(ThumbWidth, ThumbHeight);
    PaintTo(fTransition.SourceShot);
    fFrame := v;
    PaintTo(fTransition.TargetShot);
    fTransition.Run(true);
  end
  else
  begin
    fFrame := v;
    UpdateEx(false);
  end;
end;

{!!
<FS>TImageEnMView.TransitionRunning

<FM>Declaration<FC>
property TransitionRunning: Boolean;

<FM>Description<FN>
Returns <FC>true<FN> when a transition is running.

<FM>Example<FC>
// Design time properties...
ImageEnMView1.DisplayMode := mdSingle;
ImageEnMView1.TransitionEffect := iettCrossDissolve;
ImageEnMView1.TransitionDuration := 1500;

// display next frame using cross dissolve
ImageEnMView1.VisibleFrame := ImageEnMView1.VisibleFrame + 1;

// wait for the transition to end
While ImageEnMView1.TransitionRunning do
  Application.processmessages;
ShowMessage('transition done!');

!!}
function TImageEnMView.GetTransitionRunning: boolean;
begin
  result := fTransition.Running;
end;

procedure TImageEnMView.RemoveAlphaChannel(Merge: boolean);
begin
  // nothing
end;

// return nil

function TImageEnMView.GetAlphaChannel: TIEBitmap;
begin
  result := nil;
end;

function TImageEnMView.GetHasAlphaChannel: boolean;
begin
  result := false;
end;

function TImageEnMView.GetImageTopText(idx: integer): TIEMText;
begin
  result := nil;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      result := TopText;
end;

function TImageEnMView.GetImageBottomText(idx: integer): TIEMText;
begin
  result := nil;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      result := BottomText;
end;

function TImageEnMView.GetImageInfoText(idx: integer): TIEMText;
begin
  result := nil;
  if (idx >= 0) and (idx < fImageInfo.Count) then
    with PIEImageInfo(fImageInfo[idx])^ do
      result := InfoText;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMText

constructor TIEMText.Create(Owner: TComponent; Position: TIEMTextPos);
var
  iem: TImageEnMView;
begin
  inherited Create;

  fCaption   := '';
  fFont      := TFont.Create();
  fOwner     := Owner;
  fPos       := Position;
  fTruncSide := iemtsLeft;
  if fOwner is TImageEnMView then
  begin
    iem := (fOwner as TImageEnMView);
    fBackground := iem.fBackground;
    case Position of
      iemtpTop    : fFont.Assign(iem.fDefaultTopTextFont);
      iemtpBottom : fFont.Assign(iem.fDefaultBottomTextFont);
      iemtpInfo   : fFont.Assign(iem.fDefaultInfoTextFont);
    end;
    fBackgroundStyle := iem.fDefaultTextBackgroundStyle;
  end
  else
  begin
    fBackground := clBtnFace;      
    fBackgroundStyle := bsSolid;
  end;
end;

destructor TIEMText.Destroy;
begin
  FreeAndNil(fFont);

  inherited;
end;

procedure TIEMText.SetCaption(value: WideString);
var
  iem: TImageEnMView;
  h: integer;
begin
  if fOwner is TImageEnMView then
  begin
    iem := (fOwner as TImageEnMView);
    {$ifndef IEDOTNETVERSION}
    if assigned(iem.Parent) then
    {$endif}
    begin
      // adjust top or bottom gap if needed
      iem.Canvas.Font.Assign(fFont);
      h := IETextHeightW(iem.Canvas, value);
      if (fPos = iemtpTop) and (iem.fUpperGap < (h + 2)) then
        iem.fUpperGap := h + 2 + iem.fTextMargin;
      if (fPos = iemtpBottom) and (iem.fBottomGap < (h + 2)) then
      begin
        if iem.fStyle = iemsACD then
          iem.fBottomGap := h + 4 + iem.fTextMargin
        else
          iem.fBottomGap := h + 2 + iem.fTextMargin;
      end;
    end;
  end;
  fCaption := value;
end;

procedure TIEMText.SaveToStream(Stream: TStream);
var
  ver: byte;
  f_charset: TFontCharset;
  f_color: TColor;
  f_pitch: TFontPitch;
  f_style: TFontStyles;
  i32: Integer;
begin
  // version
  ver := 1; 
  Stream.Write(ver, 1);
  // caption
  IESaveStringToStreamW(Stream, fCaption);
  // font - charset
  f_charset := fFont.Charset; 
  Stream.Write(f_charset, SizeOf(TFontCharset));
  // font - color
  f_color := fFont.Color; 
  Stream.Write(f_color, SizeOf(TColor));
  // font - height
  i32 := fFont.Height; 
  Stream.Write(i32, SizeOf(integer));
  // font - name
  IESaveStringToStream(Stream, AnsiString(fFont.Name));
  // font - pitch
  f_pitch := fFont.Pitch; 
  Stream.Write(f_pitch, SizeOf(TFontPitch));
  // font - style
  f_style := fFont.Style; 
  Stream.Write(f_style, SizeOf(TFontStyles));
  // background
  Stream.Write(fBackground, SizeOf(TColor));
  // background style
  Stream.Write(fBackgroundStyle, SizeOf(TBrushStyle));
  // pos
  Stream.Write(fPos, SizeOf(TIEMTextPos));
  // trunc side
  Stream.Write(fTruncSide, SizeOf(TIEMTruncSide));
end;

function TIEMText.LoadFromStream(Stream: TStream): Boolean;
var
  ver: byte;
  f_charset: TFontCharset;
  f_color: TColor;
  f_pitch: TFontPitch;
  f_style: TFontStyles;
  i32: Integer;
  s: AnsiString;
begin
  // version
  Stream.Read(ver, 1);
  // caption
  IELoadStringFromStreamW(Stream, fCaption);
  // font - charset
  Stream.Read(f_charset, SizeOf(TFontCharset));
  fFont.Charset := f_charset;
  // font - color
  Stream.Read(f_color, SizeOf(TColor));
  fFont.Color := f_color;
  // font - height
  Stream.Read(i32, SizeOf(integer));
  fFont.Height := i32;
  // font - name
  IELoadStringFromStream(Stream, s);
  fFont.Name := string(s);
  // font - pitch
  Stream.Read(f_pitch, SizeOf(TFontPitch));
  fFont.Pitch := f_pitch;
  // font - style
  Stream.Read(f_style, SizeOf(TFontStyles));
  fFont.Style := f_style;
  // background
  Stream.Read(fBackground, SizeOf(TColor));
  // background style
  Stream.Read(fBackgroundStyle, SizeOf(TBrushStyle));
  // pos
  Stream.Read(fPos, SizeOf(TIEMTextPos));
  // trunc side
  Stream.Read(fTruncSide, SizeOf(TIEMTruncSide));

  result := true;
end;

// end of TIEMText
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnMView.Style

<FM>Declaration<FC>
property Style: <A TIEMStyle>;

<FM>Description<FN>
Specifies the thumbnail style, either flat or 3D.

See also: <A TImageEnMView.SetModernStyling>

<FM>Example<FC>
ImageEnMView1.Style := iemsFlat;
<IMG help_images\60.bmp>

ImageEnMView1.Style := iemsACD;
<IMG help_images\61.bmp>
!!}
procedure TImageEnMView.SetStyle(value: TIEMStyle);
begin
  if fStyle <> value then
  begin
    fStyle := value;
    Update;
  end;
end;


{!!
<FS>TImageEnMView.EnableMultiSelect

<FM>Declaration<FC>
property EnableMultiSelect: Boolean;

<FM>Description<FN>
Enable to allow the user to select multiple images. The default is <FC>false<FN> (i.e. multi-selection is disabled).

<FM>Example<FC>
// select images 0 and 1 (assumes you have set at design time ImageEnMView1.EnableMultiSelect := True)
ImageEnMView1.Deselect;
ImageEnMView1.MultiSelecting := True;
ImageEnMView1.SelectedImage := 0;
ImageEnMView1.SelectedImage := 1;
ImageEnMView1.MultiSelecting := False;

<FM>See Also<FN>
- <A TImageEnMView.MultiSelectedImagesCount>
- <A TImageEnMView.MultiSelectedImages>
!!}
procedure TImageEnMView.SetEnableMultiSelect(Value: boolean);
begin
  if fEnableMultiSelect <> Value then
  begin
    fEnableMultiSelect := Value;
    Update;
  end;
end;

{!!
<FS>TImageEnMView.ThumbnailsBorderWidth

<FM>Declaration<FC>
property ThumbnailsBorderWidth: Integer;

<FM>Description<FN>
Specifies the width (in pixels) of the thumbnail border. Default is 0.

This value should be less than <A TImageEnMView.ThumbWidth> and <A TImageEnMView.ThumbHeight>. Specify the color of the border using <A TImageEnMView.ThumbnailsBorderColor>.

<FM>Example<FC>
// Draw a thin green border around all thumbnails
ImageEnMView1.ThumbnailsBorderWidth := 1;
ImageEnMView1.ThumbnailsBorderColor := clGreen;
!!}
procedure TImageEnMView.SetThumbnailsBorderWidth(Value: integer);
begin
  if fThumbnailsBorderWidth <> Value then
  begin
    fThumbnailsBorderWidth := Value;
    Update;
  end;
end;

{!!
<FS>TImageEnMView.ThumbnailsBorderColor

<FM>Declaration<FC>
property ThumbnailsBorderColor: TColor;

<FM>Description<FN>
Specifies the color of the thumbnail border.

<FM>Example<FC>
// draw a thin green border to all thumbnails
ImageEnMView1.ThumbnailsBorderWidth := 1;
ImageEnMView1.ThumbnailsBorderColor := clGreen;

<FM>See Also<FN>
- <A TImageEnMView.ThumbnailsBorderWidth>
!!}
procedure TImageEnMView.SetThumbnailsBorderColor(Value: TColor);
begin
  if fThumbnailsBorderColor <> Value then
  begin
    fThumbnailsBorderColor := Value;
    Update;
  end;
end;

{!!
<FS>TImageEnMView.ThumbnailsInternalBorder

<FM>Declaration<FC>
property ThumbnailsInternalBorder: boolean;

<FM>Description<FN>
If enabled, a border is drawn around the thumbnail interior. Specify the color using <A TImageEnMView.ThumbnailsInternalBorderColor>.
!!}
procedure TImageEnMView.SetThumbnailsInternalBorder(Value: boolean);
begin
  if fThumbnailsInternalBorder <> Value then
  begin
    fThumbnailsInternalBorder := Value;
    Update;
  end;
end;

{!!
<FS>TImageEnMView.ThumbnailsInternalBorderColor

<FM>Declaration<FC>
property ThumbnailsInternalBorderColor: TColor;

<FM>Description<FN>
Specifies the border color when <A TImageEnMView.ThumbnailsInternalBorder> is True.   

!!}
procedure TImageEnMView.SetThumbnailsInternalBorderColor(Value: TColor);
begin
  if fThumbnailsInternalBorderColor <> Value then
  begin
    fThumbnailsInternalBorderColor := Value;
    Update;
  end;
end;


{!!
<FS>TImageEnMView.MultiSelectedImages

<FM>Declaration<FC>
property MultiSelectedImages[index: Integer]: Integer;

<FM>Description<FN>
Returns the index of all selected images, where MultiSelectedImages[0] returns the index of the first selected image, MultiSelectedImages[1] returns the second, etc.

Notes:
- Use <A TImageEnMView.MultiSelectedImagesCount> to know how many images are selected
- The index returned by MultiSelectedImages is not in any particular order unless <A TImageEnMView.MultiSelectSortList> is called
- This property is valid even if <L TImageEnMView.EnableMultiSelect>multiple selection</L> is not enabled (i.e. returning <A TImageEnMView.SelectedImage>).

<FM>Example 1<FC>
// replaces all selected images with 'new.jpg'
for i := 0 to ImageEnMView1.MultiSelectedImagesCount - 1 do
begin
  iSelIndex := ImageEnMView1.MultiSelectedImages[ i ];
  ImageEnMView1.SetImageFromFile(iSelIndex , 'new.jpg');
end;

<FM>Example 2<FC>
// Get filenames of all selected files

// Sort them by image index
ImageEnMView1.MultiSelectSortList;
lbxFilenames.clear;

for i := 0 to ImageEnMView1.MultiSelectedImagesCount - 1 do
begin
  iSelIndex := ImageEnMView1.MultiSelectedImages[ i ];
  lbxFilenames.Items.Add(ImageEnMView1.ImageFileName[iSelIndex]);
end;

<FM>See Also<FN>
- <A TImageEnMView.MultiSelectSortList>
- <A TImageEnMView.IsSelected>

!!}
function TImageEnMView.GetMultiSelectedImages(index: integer): integer;
begin
  result := -1;
  if EnableMultiSelect and (index >= 0) and (index < fMultiSelectedImages.Count) then
    result := integer(fMultiSelectedImages[index])
  else
  if (EnableMultiSelect = False) and (index = 0) and (fSelectedItem > -1) then
    Result := fSelectedItem;
end;

{!!
<FS>TImageEnMView.MultiSelectedImagesCount

<FM>Declaration<FC>
property MultiSelectedImagesCount: Integer;

<FM>Description<FN>
MultiSelectedImagesCount returns the number of selected images. Selected indexes are returned by <A TImageEnMView.MultiSelectedImages>.

Note: This property is valid even if <L TImageEnMView.EnableMultiSelect>multiple selection</L> is not enabled (i.e. returning either 0 or 1).

<FM>Example<FC>
// replaces all selected images with 'new.jpg'
for i := 0 to ImageEnMView1.MultiSelectedImagesCount - 1 do
begin
  iSelIndex := ImageEnMView1.MultiSelectedImages[ i ];
  ImageEnMView1.SetImageFromFile(iSelIndex , 'new.jpg');
end;

!!}
function TImageEnMView.GetMultiSelectedImagesCount: integer;
begin
  Result := 0;
  if EnableMultiSelect then
    result := fMultiSelectedImages.Count
  else
  if fSelectedItem > -1 then
    Result := 1;
end;


{!!
<FS>TImageEnMView.MultiSelectedImagesList

<FM>Declaration<FC>
property MultiSelectedImagesList: <A TIEArrayOfInteger>;

<FM>Description<FN>
Returns an array of integers representing the indexes of all selected images. Changing the content of this list doesn't change the actual selected images.

Notes:
- The index returned by MultiSelectedImagesList is not in any particular order unless <A TImageEnMView.MultiSelectSortList> is called
- This property is valid even if <L TImageEnMView.EnableMultiSelect>multiple selection</L> is not enabled (i.e. returning <A TImageEnMView.SelectedImage>).
!!}
// returns a readonly list of selected images
function TImageEnMView.GetMultiSelectedImagesList(): TIEArrayOfInteger;
var
  i: integer;
begin
  SetLength(result, MultiSelectedImagesCount);
  for i := 0 to length(result) - 1 do
    result[i] := MultiSelectedImages[i];
end;


{!!
<FS>TImageEnMView.WallPaper

<FM>Declaration<FC>
property WallPaper: TPicture;

<FM>Description<FN>
Sets a background image that appears behind the thumbnails. Use <A TImageEnMView.WallPaperStyle> to specify how the wallpaper is painted.

<FM>Example<FN>
// Tile a bitmap over the background
ImageEnMView1.WallPaperStyle := iewoTile
ImageEnMView1.WallPaper.LoadFromFile('D:\MyWallpaper.bmp');
!!}
procedure TImageEnMView.SetWallPaper(Value: TPicture);
begin
  fWallPaper.Assign(Value);
  Update;
end;

{!!
<FS>TImageEnMView.WallPaperStyle

<FM>Declaration<FC>
property WallPaperStyle: <A TIEWallPaperStyle>;

<FM>Description<FN>
Determines how the wallpaper (specified by <A TImageEnMView.WallPaper>) is painted.

<FM>Example<FN>
// Tile a bitmap over the background
ImageEnMView1.WallPaperStyle := iewoTile
ImageEnMView1.WallPaper.LoadFromFile('D:\MyWallpaper.bmp');
!!}
procedure TImageEnMView.SetWallPaperStyle(Value: TIEWallPaperStyle);
begin
  if Value <> fWallPaperStyle then
  begin
    fWallPaperStyle := Value;
    Update;
  end;
end;

{!!
<FS>TImageEnMView.SelectAll

<FM>Declaration<FC>
procedure SelectAll;

<FM>Description<FN>
Selects all images (if <A TImageEnMView.EnableMultiSelect> is true).
!!}
procedure TImageEnMView.SelectAll;
var
  q: integer;
  lMultiSelecting: boolean;
begin
  if fEnableMultiSelect then
  begin        
    DeselectNU;
    lMultiSelecting := fMultiSelecting;
    fMultiSelecting := true;
    for q := 0 to fImageInfo.Count - 2 do
      fMultiSelectedImages.Add(pointer(q));
    SetSelectedItemNU(fImageInfo.Count - 1); // last item also is current selected item (own Bitmap object)
    fMultiSelecting := lMultiSelecting;
    UpdateEx(false);
  end;
end;

{!!
<FS>TImageEnMView.EnableResamplingOnMinor

<FM>Declaration<FC>
property EnableResamplingOnMinor: Boolean;

<FM>Description<FN>
If enabled (default), small images are resampled (enlarged) to the thumbnail size, otherwise images smaller than the specified thumbnail dimension are shown at their original size (i.e. only images larger than thumbnail size will be resampled/resized).
!!}
procedure TImageEnMView.SetEnableResamplingOnMinor(Value: boolean);
begin
  fEnableResamplingOnMinor := Value;
  Update;
end;
        
{!!
<FS>TImageEnMView.IconSize

<FM>Declaration<FC>
property IconSize : <A TIEImageEnMViewIconSize>;

<FM>Description<FN>
For non-image types a system icon is displayed. The default setting is _icoHDStretch which means that on Windows Vista and newer versions, a high quality icon is stretched to the display size. On XP and older versions of Windows it automatically downgrades to _icoDoubleSize.
!!}
procedure TImageEnMView.SetIconSize(Value: TIEImageEnMViewIconSize);
begin
  if fIconSize <> Value then
  begin
    fIconSize := Value;
    ClearCache;
    Update;
  end;
end;


function TImageEnMView.GetImageEnMIO: TImageEnMIO;
begin
  if not assigned(fImageEnMIO) then
  begin
    fImageEnMIO := TImageEnMIO.Create(self);
    fImageEnMIO.AttachedMView := self;
    fImageEnMIO.OnProgress := fOnProgress;
    fImageEnMIO.OnFinishWork := fOnFinishWork;
    fImageEnMIO.OnAcquireBitmap := fOnAcquireBitmap;
  end;
  result := fImageEnMIO;
end;

function TImageEnMView.GetImageEnProc: TImageEnProc;
begin
  if fSelectedItem = -1 then
    SelectedImage := 0;

  if not assigned(fImageEnProc) then
  begin
    fImageEnProc := TImageEnProc.Create(self);
    fImageEnProc.AttachedImageEn := self;
    fImageEnProc.OnProgress := fOnProgress;
    fImageEnProc.OnFinishWork := fOnFinishWork;
    fImageEnProc.fOnUndoRedoEvent := ProcessUndoRedo;
  end;                     

  fImageEnProc.fMViewIndex := fSelectedItem;
  result := fImageEnProc;

  if not assigned(fSelectedBitmap) then
    fImageEnProc.AttachedImageEn := self; // refresh bitmap if fSelectedBitmap=nil
end;


function TImageEnMView.CheckSelectionChangingAllowed : Boolean;
begin
  Result := True;
  if (fDestroying = False) and assigned(fOnSelectionChanging) then
    fOnSelectionChanging(self, Result);
end;

procedure TImageEnMView.ProcessUndoRedo(Sender : TObject; bIsUndo : Boolean; Source : TIEUndoSource; UndoObj : TObject; iIndex : Integer; var bHandled : Boolean);
var
  bmp: TIEBitmap;
begin
  if (Source <> ieuImage) or (iIndex < 0) then
    exit;

  bHandled := True;
  if iIndex = fSelectedItem then
    IEBitmap.assign(TIEBitmap(UndoObj))
  else
  begin
    bmp := GetTIEBitmap(iIndex);
    bmp.assign(TIEBitmap(UndoObj));
    ReleaseBitmap(iIndex);
  end;
  Update;
end;

{!!
<FS>TImageEnMView.OnAcquireBitmap

<FM>Declaration<FC>
property OnAcquireBitmap: <A TIEAcquireBitmapEvent>;

<FM>Description<FN>
Occurs whenever a new bitmap is acquired during a multi-page Twain acquisition.

!!}
function TImageEnMView.GetOnAcquireBitmap: TIEAcquireBitmapEvent;
begin
  result := fOnAcquireBitmap;
end;

procedure TImageEnMView.SetOnAcquireBitmap(v: TIEAcquireBitmapEvent);
begin
  fOnAcquireBitmap := v;
  if assigned(fImageEnIO) then
    fImageEnIO.OnAcquireBitmap := v;
end;

{!!
<FS>TImageEnMView.OnFinishWork

<FM>Declaration<FC>
property OnFinishWork: TNotifyEvent;

<FM>Description<FN>
Occurs whenever an image processing or input/output task terminates.

It is always called after <A TImageEnMView.OnProgress> so can be used to reset your progress bar.
!!}

function TImageEnMView.GetOnFinishWork: TNotifyEvent;
begin
  result := fOnFinishWork;
end;

procedure TImageEnMView.SetOnFinishWork(v: TNotifyEvent);
begin
  fOnFinishWork := v;
  if assigned(fImageEnMIO) then
    fImageEnMIO.OnFinishWork := v;
  if assigned(fImageEnProc) then
    fImageEnProc.OnFinishWork := v;
end;


procedure TImageEnMView.SetOnProgress(v: TIEProgressEvent);
begin
  fOnProgress := v;
  if assigned(fImageEnMIO) then
    fImageEnMIO.OnProgress := v;
  if assigned(fImageEnProc) then
    fImageEnProc.OnProgress := v;
end;

{!!
<FS>TImageEnMView.OnProgress

<FM>Declaration<FC>
property OnProgress: <A TIEProgressEvent>;

<FM>Description<FN>
Occurs during image processing or input/output operations. It is useful to display progress to the user of the current task.

You can use <A TImageEnMView.OnFinishWork> to reset the progress when the task has completed.

!!}
function TImageEnMView.GetOnProgress: TIEProgressEvent;
begin
  result := fOnProgress;
end;

{!!
<FS>TImageEnMView.MaximumViewX

<FM>Declaration<FC>
property MaximumViewX: Integer;

<FM>Description<FN>
Returns the maximum value that you can specify for <A TImageEnMView.ViewX> (naturally "MinimumViewX" is zero).

See also: <A TImageEnMView.MaximumViewY>.

!!}
function TImageEnMView.GetMaximumViewX: integer;
begin
  result := imax(fVWidth - ClientWidth, 0);
end;

{!!
<FS>TImageEnMView.MaximumViewY

<FM>Declaration<FC>
property MaximumViewY: Integer;

<FM>Description<FN>
Returns the maximum value that you can specify for <A TImageEnMView.ViewY> (naturally "MinimumViewY" is zero).

See also: <A TImageEnMView.MaximumViewX>.
!!}
function TImageEnMView.GetMaximumViewY: integer;
begin
  result := imax(fVHeight - ClientHeight, 0);
end;

{!!
<FS>TImageEnMView.ClearImageCache

<FM>Declaration<FC>
procedure ClearImageCache(idx: Integer);

<FM>Description<FN>
Clears the cache for image, <FC>idx<FN>.

Image caching allows you to speed up the thumbnail painting by saving the view of each image to an internal cache. You should call this method only if you have refreshing problems.

See also: <A TImageEnMView.EnableImageCaching>.

!!}
procedure TImageEnMView.ClearImageCache(idx: integer);
var
  info: PIEImageInfo;
begin
  info := PIEImageInfo(fImageInfo[idx]);
  if info^.cacheImage <> nil then
  begin
    fcacheList.Delete(info^.cacheImage);
    info^.cacheImage := nil;
  end;
end;

procedure TImageEnMView.ClearCache;
var
  i: integer;
begin
  for i := 0 to fImageInfo.Count - 1 do
    ClearImageCache(i);
  fIconList.Clear();
end;


{!!
<FS>TImageEnMView.EnableImageCaching

<FM>Declaration<FC>
property EnableImageCaching: Boolean;

<FM>Description<FN>
Speeds up thumbnail painting by saving the view of each image to an internal cache. This is enabled by default.

Set to false to disable image caching.

See also: <A TImageEnMView.ClearImageCache>.
!!}
procedure TImageEnMView.SetEnableImageCaching(v: boolean);
begin
  fEnableImageCaching := v;
  if not fEnableImageCaching then
  begin
    if fStoreType = ietFastThumb then
      fStoreType := ietThumb;
    ClearCache;
    update;
  end;
end;

{!!
<FS>TImageEnMView.IsSelected

<FM>Declaration<FC>
function IsSelected(idx: Integer): Boolean;

<FM>Description<FN>
IsSelected returns true if the image, <FC>idx<FN>, is currently selected.

<FM>Example<FC>
// replaces all selected images with 'new.jpg'
for idx := 0 to ImageEnMView1.ImageCount - 1 do
  if ImageEnMView1.IsSelected(idx) then
    ImageEnMView1.SetImageFromFile(idx, 'new.jpg');
end;

<FM>See Also<FN>
- <A TImageEnMView.MultiSelectedImages>

!!}
function TImageEnMView.IsSelected(idx: integer): boolean;
begin
  result := (fSelectedItem=idx) or (fMultiSelectedImages.IndexOf(pointer(idx)) > -1);
end;

{!!
<FS>TImageEnMView.MultiSelectSortList

<FM>Declaration<FC>
procedure MultiSelectSortList;

<FM>Description<FN>
Sorts the selected items list (<A TImageEnMView.MultiSelectedImages>) by image index.

<FM>Example<FC>
// Get filenames of all selected files in the order they are displayed
ImageEnMView1.MultiSelectSortList;
lbxFilenames.clear;
for i := 0 to ImageEnMView1.MultiSelectedImagesCount - 1 do
begin
  iSelIndex := ImageEnMView1.MultiSelectedImages[ i ];
  lbxFilenames.Items.Add(ImageFileName[iSelIndex]);
end;

!!}
procedure TImageEnMView.MultiSelectSortList;
begin
  fMultiSelectedImages.Sort(@ComparePointers);
end;

{!!
<FS>TImageEnMView.EnableAlphaChannel

<FM>Declaration<FC>
property EnableAlphaChannel: Boolean;

<FM>Description<FN>
Set EnableAlphaChannel to True to enable the alpha channel of thumbnails (e.g. for the display of <L TImageEnMView.SoftShadow>soft shadows</L>).
!!}
procedure TImageEnMView.SetEnableAlphaChannel(v: boolean);
begin
  fEnableAlphaChannel := v;
  Update;
end;

{!!
<FS>TImageEnMView.BackgroundStyle

<FM>Declaration<FC>
property BackgroundStyle: <A TIEBackgroundStyle>;

<FM>Description<FN>
Specifies the style of the background (the component region behind the thumbnails).

<FM>Example<FC>
// Small chessboard background
ImageEnMView1.BackgroundStyle := iebsChessboard;
ImageEnMView1.SetChessboardStyle(5, bsSolid);

// Solid white background
ImageEnMView1.BackgroundStyle := iebsSolid;
ImageEnMView1.Background := clWhite;

// Navy -> Black gradient background
ImageEnMView1.BackgroundStyle := iebsGradient;
ImageEnMView1.Background := clNavy;
ImageEnMView1.GradientEndColor := clBlack;

!!}
procedure TImageEnMView.SetBackgroundStyle(v: TIEBackgroundStyle);
begin
  fBackgroundStyle := v;
  Update;
end;

{!!
<FS>TImageEnMView.ThumbnailsBackgroundStyle

<FM>Declaration<FC>
property ThumbnailsBackgroundStyle: <A TIEBackgroundStyle>

<FM>Description<FN>
Specifies the style of the thumbnail background (the region of the thumbnail that does not contain the image).

Note: Only applicable when <A TImageEnMView.EnableAlphaChannel> is true and the image has an alpha channel to make it transparent.

!!}
procedure TImageEnMView.SetThumbnailsBackgroundStyle(v: TIEBackgroundStyle);
begin
  fThumbnailsBackgroundStyle := v;
  Update;
end;

{!!
<FS>TImageEnMView.SetChessboardStyle

<FM>Declaration<FC>
procedure SetChessboardStyle(Size: Integer; BrushStyle: TBrushStyle);

<FM>Description<FN>
Specifies the size and brush of the chessboard background (when <A TImageEnMView.BackgroundStyle> is set to iebsChessboard).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Size<FN></C> <C>Specifies the box size (default 16)</C> </R>
<R> <C><FC>BrushStyle<FN></C> <C>Specifies the brush style of the boxes (default bsSolid)</C> </R>
</TABLE>

<FM>Example<FC>
// Set small chessboard background
ImageEnMView1.BackgroundStyle := iebsChessboard;
ImageEnMView1.SetChessboardStyle(5, bsSolid);

!!}
procedure TImageEnMView.SetChessboardStyle(Size: integer; BrushStyle: TBrushStyle);
begin
  fChessboardSize := Size;
  fChessboardBrushStyle := BrushStyle;
end;
                                     

{!!
<FS>TImageEnMView.SetModernStyling

<FM>Declaration<FC>
procedure SetModernStyling(bAutoGridWidth : Boolean = False; iThumbWidth : Integer = 0; iThumbHeight : Integer = 0);

<FM>Description<FN>
Call SetModernStyling in FormCreate to update the control with the settings which closely match the current Windows styling.

if bAutoGridWidth is enabled then <A TImageEnMView.GridWidth> is set to -1. You can also optionally specify default values for <A TImageEnMView.GridWidth> and <A TImageEnMView.GridWidth>.

The current implementation of SetModernStyling makes the following changes:
<FC>
  <A TImageEnMView.Style>                        := iemsFlat;
  <A TImageEnMView.Background>                   := clWindow;
  <A TImageEnMView.ThumbnailsBackground>         := clWindow;
  <A TImageEnMView.ThumbnailsBackgroundSelected> := $00FCEADA;
  <A TImageEnMView.SelectionColor>               := $00CEA27D;
  <A TImageEnMView.TextMargin>                   := 2;
  <A TImageEnMView.HorizBorder>                  := 8;
  <A TImageEnMView.VertBorder>                   := 8;
  <A TImageEnMView.DefaultTextBackgroundStyle>   := bsClear;
  <A TImageEnMView.ThumbnailDisplayFilter>       := rfFastLinear;
  <A TImageEnMView.SelectedImageAlwaysVisible>   := True;
  <A TImageEnMView.ThumbnailsBorderWidth>        := 1;
  <L TIEVSoftShadow.Radius>SoftShadow.Radius</L>            := 2;
  <L TIEVSoftShadow.OffsetX>SoftShadow.OffsetX</L>           := 2;
  <L TIEVSoftShadow.OffsetY>SoftShadow.OffsetY</L>           := 2;
  <L TIEVSoftShadow.Intensity>SoftShadow.Intensity</L>           := 70;
  <L TIEVSoftShadow.Enabled>SoftShadow.Enabled</L>           := True;
<FN>       

<FM>Example<FC>
procedure TForm1.FormCreate(Sender: TObject);
begin
  ImageEnMView1.SetModernStyling();
end;

!!}
procedure TImageEnMView.SetModernStyling(bAutoGridWidth : Boolean = False; iThumbWidth : Integer = 0; iThumbHeight : Integer = 0);
const
  Explorer_Selection_Background_Color = $00FCEADA;
  Explorer_Selection_Border_Color     = $00CEA27D;
begin
  fModernStyling                := True;      
  fStyle                        := iemsFlat;
  SoftShadow.Radius             := 2;
  SoftShadow.OffsetX            := 2;
  SoftShadow.OffsetY            := 2;   
  SoftShadow.Intensity          := 70;
  SoftShadow.Enabled            := True;
  fBackground                   := clWindow;
  fThumbnailsBackground         := clWindow;
  fThumbnailsBackgroundSelected := Explorer_Selection_Background_Color;
  fSelectionColor               := Explorer_Selection_Border_Color;    
  fTextMargin                   := 2;
  fHorizBorder                  := 8;
  fVertBorder                   := 8;
  fThumbnailsBorderWidth        := 1;
  fThumbnailsBorderColor        := clBtnFace;
  fDefaultTextBackgroundStyle   := bsClear;
  fThumbnailsInternalBorder     := False;
  if fThumbnailDisplayFilter = rfNone then
    fThumbnailDisplayFilter := rfFastLinear;
  fSelectedImageAlwaysVisible   := True;
  if iThumbWidth > 0 then
  begin
    ThumbWidth := iThumbWidth;
    ClearCache;
  end;
  if iThumbHeight > 0 then
  begin
    ThumbHeight := iThumbHeight;
    ClearCache;
  end;
  if bAutoGridWidth then
  begin
    fGridWidth := -1;
    fCurrentGridWidth := -1;
  end;
end;
            


{!!
<FS>TImageEnMView.GradientEndColor

<FM>Declaration<FC>
property GradientEndColor: TColor

<FM>Description<FN>
Specifies the ending color of the gradient when <A TImageEnMView.BackgroundStyle> is iebsGradient. <A TImageEnMView.Background> specifies the starting color.

<FM>Example<FC>
// Set Navy -> Black gradient background
ImageEnMView1.BackgroundStyle := iebsGradient;
ImageEnMView1.Background := clNavy;
ImageEnMView1.GradientEndColor := clBlack;

!!}
procedure TImageEnMView.SetGradientEndColor(Value: TColor);
begin
  fGradientEndColor := Value;
  Update;
end;

{!!
<FS>TImageEnMView.FillThumbnail

<FM>Declaration<FC>
property FillThumbnail: Boolean;

<FM>Description<FN>
When enabled (default), the thumbnail background is filled with the background color.

See also: <A TImageEnMView.ThumbnailsBackgroundStyle>
!!}
procedure TImageEnMView.SetFillThumbnail(Value: boolean);
begin
  fFillThumbnail := Value;
  Update;
end;


{!!
<FS>TImageEnMView.ShowText

<FM>Declaration<FC>
property ShowText: Boolean;

<FM>Description<FN>
If enabled (default), the <A TImageEnMView.ImageTopText>, <A TImageEnMView.ImageBottomText> and <A TImageEnMView.ImageInfoText> are displayed.
!!}
procedure TImageEnMView.SetShowText(Value: boolean);
begin
  fShowText := Value;
  Update;
end;



{!!
<FS>TImageEnMView.ThumbnailClipping

<FM>Declaration<FC>
property ThumbnailClipping: Integer;

<FM>Description<FN>
Automatically clips the sides of the thumbnail when displaying it so that it more closely matches the size of the grid cell, for example, if you have a tall/portrait image the top and bottom would not be shown so that the thumbnail is as wide as the display area.

ThumbnailClipping is a percentage so the valid range is 0 to 100.

Possible values
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>0</C> <C>The entire image will be displayed as a thumbnail pointer.</C> </R>
<R> <C>1 - 99</C> <C>The percentage of permissible clipping. E.g. 10 would mean that up 10% of the image can be clipped.</C> </R>
<R> <C>100</C> <C>The maximum clipping will be performed so that the thumbnail has no border space (and all thumbnails will be displayed at an identical size)</C> </R>
</TABLE>
!!}

procedure TImageEnMView.SetThumbnailClipping(Value: Integer);
begin
  if fThumbnailClipping <> Value then
  begin
    fThumbnailClipping := Value;
    ClearCache;
    Update;
  end;
end;


{!!
<FS>TImageEnMView.FillFromDirectory

<FM>Declaration<FC>
procedure FillFromDirectory(const Directory: WideString; Limit : integer = -1; AllowUnknownFormats : boolean = false; const ExcludeExtensions : WideString = '';
                            DetectFileFormat : boolean = false; const FilterMask : WideString = ''; IncludeVideoFiles : Boolean = False;
                            LoadOnDemand : boolean = true;
                            DefaultTopText : <A TIEImageEnMViewDefaultText> = iedtNone;
                            DefaultInfoText : <A TIEImageEnMViewDefaultText> = iedtNone;
                            DefaultBottomText : <A TIEImageEnMViewDefaultText> = iedtFilename;
                            bShowHiddenFiles : Boolean = False;
                            bShowFolders : Boolean = False);

<FM>Description<FN>
Fills the ImageEnMView with files from the specified <FC>Directory<FN>. For each file <A TImageEnMView.ImageFileName> will be set with the full path and <A TImageEnMView.ImageBottomText> with the filename.
You can cancel the insertion of files by setting MIO.Aborting := True;

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Directory<FN></C> <C>The folder to search for files</C> </R>
<R> <C><FC>Limit<FN></C> <C>The maximum number of images to load. Use -1 to retrieve all files</C> </R>
<R> <C><FC>AllowUnknownFormats<FN></C> <C>If false (default) only known and supported file formats are loaded. Otherwise all files are loaded</C> </R>
<R> <C><FC>ExcludeExtensions<FN></C> <C>A comma separated list of file extensions to skip (e.g. 'lyr,all,iev')</C> </R>
<R> <C><FC>DetectFileFormat<FN></C> <C>If true then the image type is detected by reading the header (which can be slow). Otherwise ImageEn only checks the file extension</C> </R>
<R> <C><FC>FilterMask<FN></C> <C>Limits the fill to file extensions found in a comma separated list (e.g. 'jpg,jpeg,jpe'). Specify an empty string to return all supported extensions</C> </R>
<R> <C><FC>bIncludeVideoFiles<FN></C> <C>If AllowUnknownFormats is false then video files are excluded by default.  Set to true to include supported video file types such as AVI and MPEG. Thumbnails for video files will be retrieved from Windows Explorer if the format is specified in <A TIEImageEnGlobalSettings.MViewExplorerThumbnailExts></C> </R>
<R> <C><FC>LoadOnDemand<FN></C> <C>If True (Default), images are only loaded as they are displayed (i.e. not until they are scrolled into view). Set to false to load all images immediately</C> </R>
<R> <C><FC>DefaultTopText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageTopText></C> </R>
<R> <C><FC>DefaultInfoText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageInfoText></C> </R>
<R> <C><FC>DefaultBottomText<FN></C> <C>Specify <L TIEImageEnMViewDefaultText>the text</L> that is applied to <ATImageEnMView.ImageBottomText> (defaults to the filename)</C> </R>
<R> <C><FC>bShowHiddenFiles<FN></C> <C>Enable to include hidden and system files (default is false)</C> </R>
<R> <C><FC>bShowFolders<FN></C> <C>Enable to include folders (default is false)</C> </R>
</TABLE>


Note: More functionality for the display of thumbnails of file folders is available with <A TImageEnFolderMView>.


<FM>Example<FC>
ImageEnMView1.Clear;
ImageEnMView1.StoreType := ietFastThumb;
ImageEnMView1.FillFromDirectory('C:\images');
!!}
procedure TImageEnMView.FillFromDirectory(const Directory: WideString; Limit : integer = -1; AllowUnknownFormats : boolean = false; const ExcludeExtensions : WideString = '';
                                          DetectFileFormat : boolean = false; const FilterMask : WideString = ''; IncludeVideoFiles : Boolean = False;
                                          LoadOnDemand : boolean = true;
                                          DefaultTopText : TIEImageEnMViewDefaultText = iedtNone;
                                          DefaultInfoText : TIEImageEnMViewDefaultText = iedtNone;
                                          DefaultBottomText : TIEImageEnMViewDefaultText = iedtFilename;
                                          bShowHiddenFiles : Boolean = False;
                                          bShowFolders : Boolean = False);
var
  l, idx: Integer;
  fpath, fname: WideString;
  count: Integer;
  sep: WideString;
  excList: TStringList;
  mskList: TStringList;
  dir: TIEDirContent;
  ext: WideString;
  lAnimation: TIEAnimation;
  bInclude: Boolean;
  bAllowAddition: Boolean;
begin
  lAnimation := Animation;
  fAnimation := nil;
  LockPaint();
  dir := nil;
  excList := TStringList.Create;
  mskList := TStringList.Create;
  MIO.Aborting := False;
  try
    excList.CommaText := LowerCase(ExcludeExtensions);
    mskList.CommaText := LowerCase(FilterMask);
    l := length(Directory);
    if (l=0) or (Directory[l]='\') then
      sep := ''
    else
      sep := '\';
    dir := TIEDirContent.Create(Directory + sep + '*.*');
    count := 0;
    while dir.GetItem(fname, True, bShowFolders, bShowHiddenFiles) do
    begin
      fpath := Directory + sep + fname;
      ext := IEExtractFileExtW(fname, false);
      if dir.IsFolder then
        bInclude := True
      else
        bInclude := (AllowUnknownFormats or (DetectFileFormat and (FindFileFormat(fpath) <> ioUnknown)) or IsKnownFormat(fpath, IncludeVideoFiles)) and
                    (excList.IndexOf(ext) = -1) and
                    ((mskList.Count = 0) or (mskList.IndexOf(ext) > -1));

      if bInclude  then
      begin
        if (Limit>-1) and (count = limit) then
          break
        else
        if MIO.Aborting then
          break;

        bAllowAddition := True;
        if assigned(fOnImageAdd) then
          fOnImageAdd(self,
                      fImageInfo.Count,
                      fpath,
                      dir.IsFolder,
                      dir.IsHiddenFile,
                      dir.FileSizeBytes,
                      dir.CreateDate,
                      dir.EditDate,
                      bAllowAddition);

        if bAllowAddition then
        begin
          idx := AppendImage(fpath, LoadOnDemand, DefaultTopText, DefaultInfoText, DefaultBottomText, False);

          ImageFileSize[idx]   := dir.FileSizeBytes;
          ImageCreateDate[idx] := dir.CreateDate;
          ImageEditDate[idx]   := dir.EditDate;
          If dir.IsFolder then
            PIEImageInfo(fImageInfo[idx])^.SourceType := iestFolderIcon;

          inc(count);
        end;
      end;
    end;

    // Legacy support: select last added item
    SetSelectedItemNU(fImageInfo.Count - 1);
  finally
    dir.Free;
    mskList.Free;
    excList.Free;
    fAnimation := lAnimation;
    UnLockPaint();
    Update;
  end;
end;


{!!
<FS>TImageEnMView.MoveImage

<FM>Declaration<FC>
procedure MoveImage(idx: Integer; destination: Integer);

<FM>Description<FN>
Moves an image from the index position <FC>idx<FN> to the <FC>destination<FN> position.

If the destination index is greater than or equal to the image count, the image is moved to the position after the last image.

<FM>Example<FC>
// Exchange first and second images
ImageEnMView1.MoveImage(0, 1);

// Move first image to the end of the grid
ImageEnMView1.MoveImage(0, ImageEnMView1.ImageCount);

!!}
// move image idx to destination index
procedure TImageEnMView.MoveImage(idx: integer; destination: integer);
var
  psel: integer;
  mm: TList;
  i, p: Integer;
begin
  if (idx >= 0) and (idx < fImageInfo.Count) and (destination >= 0) and (destination <> idx) then
  begin
    SetPlaying(false);

    psel := fSelectedItem;
    mm := TList.Create;

    try
      for i := 0 to fMultiSelectedImages.Count-1 do
      begin
        p := integer( fMultiSelectedImages[i] );
        if p = idx then
          p := destination
        else
        begin
          if p > idx then dec(p);
          if p >= destination then inc(p);
        end;
        mm.Add( pointer(p) );
      end;

      if destination >= fImageInfo.Count then
      begin
        fImageInfo.Add(fImageInfo[idx]);
        fImageInfo.Delete(idx);
      end
      else
        fImageInfo.Move(idx, destination);

      fLastImOp := 3; // move...
      fLastImIdx := idx; //...image idx
      fLastImP1 := destination;
      CallBitmapChangeEvents;

      if (mm.Count > 0) and ((idx = psel) or (fMultiSelectedImages.Count > 0)) then
      begin
        fSelectedItem := integer(mm[0]);
        for i := 0 to fMultiSelectedImages.Count-1 do
          fMultiSelectedImages[i] := mm[i];
      end
      else
      begin
        DeselectNU;
        SetSelectedItemNU(psel);
      end;

      UpdateEx(false);

    finally
      mm.free;
    end;
  end;
end;

{!!
<FS>TImageEnMView.MoveSelectedImagesTo

<FM>Declaration<FC>
procedure MoveSelectedImagesTo(beforeImage: Integer);

<FM>Description<FN>
Moves all selected images to the postion prior to <FC>beforeImage<FN>.

To move images after last image set beforeImage to <A TImageEnMView.ImageCount>.

<FM>Example<FC>
// Move all selected images to the start of the grid
ImageEnMView1.MoveSelectedImagesTo(0);

// Move all selected images to the end of the grid
ImageEnMView1.MoveSelectedImagesTo(ImageEnMView1.ImageCount);
!!}
// beforeImage can be 0 up to imagecount
procedure TImageEnMView.MoveSelectedImagesTo(beforeImage: Integer);
var
  mm: TList;
  i: Integer;
  sn: Boolean;
begin
  if (beforeImage < 0) or (beforeImage > fImageInfo.Count) then
    exit;

  SetPlaying(false);

  sn := (fSelectedItem>-1) and (fMultiSelectedImages.Count=0);
  if sn then
    fMultiSelectedImages.Add(pointer(fSelectedItem));

  fLastImOp := 3; // move...
  fLastImIdx := fSelectedItem; //...image idx
  fLastImP1 := beforeImage;

  mm := TList.Create;

  try

    mm.Count := fImageInfo.Count;
    for i := 0 to fImageInfo.Count-1 do
      mm[i] := fImageInfo[i];

    for i := 0 to fMultiSelectedImages.Count-1 do
    begin
      fImageInfo[ fImageInfo.IndexOf( mm[MultiSelectedImages[i]] ) ] := pointer(-1); // mark as to remove
      fImageInfo.Insert( beforeImage, mm[MultiSelectedImages[i]] );
      inc(beforeImage);
    end;
    for i := fImageInfo.Count-1 downto 0 do
      if fImageInfo[i]=pointer(-1) then
        fImageInfo.Delete(i);
    for i := 0 to fMultiSelectedImages.Count-1 do
      fMultiSelectedImages[i] := pointer(fImageInfo.IndexOf( mm[MultiSelectedImages[i]] ));

    CallBitmapChangeEvents;

    fSelectedItem := MultiSelectedImages[0];

    if sn then
      fMultiSelectedImages.Clear;

    UpdateEx(false);

  finally
    mm.free;
  end;

end;

// no image must be selected
// doesn't restore selected image
// doesn't update
procedure TImageEnMView.SwapImages(idx1, idx2: Integer);
var
  tmp: pointer;
begin
  tmp := fImageInfo[idx1];
  fImageInfo[idx1] := fImageInfo[idx2];
  fImageInfo[idx2] := tmp;

  fLastImOp := 4; // swap...
  fLastImIdx := idx1;
  fLastImP1 := idx2;
  CallBitmapChangeEvents;
end;

function TImageEnMView.SortCompareFunction(index1, index2: Integer): Integer;
begin
  if assigned(fCurrentCompare) then
    result := fCurrentCompare(index1, index2)
  else
    result := fCurrentCompareEx(index1, index2)
end;

{!!
<FS>TImageEnMView.Sort

<FM>Declaration<FC>
procedure Sort(Compare: <A TIEImageEnMViewSortCompare>);
procedure Sort(Compare: <A TIEImageEnMViewSortCompareEx>);
procedure Sort(OrderBy: <A TIEImageEnMViewSortBy>; Ascending: boolean = true; CaseSensitive: boolean = true);

<FM>Description<FN>
Sorts all images in the TImageEnMView by property (filename, dimensions, etc) or using a custom comparison function.

<FM>Example 1<FC>
// sort by filename
ImageEnMView1.Sort( iesbFilename );

<FM>Example 2<FC>
// sort by filename even if files come from different folders
ImageEnMView1.Sort( iesbFilenameWithoutPath );

<FM>Example 3<FC>
// custom sort function (by DPI)
function XCompareDPI(i1, i2: Integer): Integer;
var
  s1, s2: Integer;
begin       
  s1 := Form1.ImageEnMView1.Params[i1].DPI;
  s2 := Form1.ImageEnMView1.Params[i2].DPI;

  if s1 < s2 then
    result := -1
  else
  if s1 > s2 then
    result := 1
  else
    result := 0;
end;

// Sort By DPI
procedure TForm1.Button1Click(Sender: TObject);
begin
  ImageEnMView1.Sort( XCompareDPI );
end;

!!}
procedure TImageEnMView.Sort(Compare: TIEImageEnMViewSortCompare);
begin
  Sort2(Compare, nil);
end;

procedure TImageEnMView.Sort(Compare: TIEImageEnMViewSortCompareEx);
begin
  Sort2(nil, Compare);
end;

function TImageEnMView.SortCompareBy(index1, index2: Integer): Integer;

  function _DoCompareStr(const S1, S2: string): Integer;
  begin
    if fCurrentCaseSensitive then
      Result := CompareStr(s1, s2)
    else                          
      Result := CompareText(s1, s2)
  end;

var
  s1, s2 : integer;
  u1, u2 : Int64;
  d1, d2 : TDateTime;
  bIndex1IsFolder, bIndex2IsFolder : Boolean;
begin
  bIndex1IsFolder := PIEImageInfo(fImageInfo[index1])^.SourceType = iestFolderIcon;
  bIndex2IsFolder := PIEImageInfo(fImageInfo[index2])^.SourceType = iestFolderIcon;
  
  // Folders always appear before files
  if bIndex1IsFolder <> bIndex2IsFolder then
  begin
    if bIndex1IsFolder then
      Result := -1
    else
      Result := 1;
  end
  else
  // Both files are folders, sort by name, if the sort type is not supported
  if bIndex1IsFolder and (fCurrentOrderBy in [iesbImageSize, iesbFilenameWithoutPath, iesbFileExtension, iesbFileSize, iesbFileType]) then
  begin
    result := _DoCompareStr(ImageFileName[index1], ImageFileName[index2]);
  end
  else
  begin
    // Normal sorting
    case fCurrentOrderBy of
      iesbFilename:
        result := _DoCompareStr(ImageFileName[index1], ImageFileName[index2]);
      iesbTopText:
        result := _DoCompareStr(ReplaceIEMConsts(ImageTopText[index1].Caption, index1), ReplaceIEMConsts(ImageTopText[index2].Caption, index2));
      iesbBottomText:
        result := _DoCompareStr(ReplaceIEMConsts(ImageBottomText[index1].Caption, index1), ReplaceIEMConsts(ImageBottomText[index2].Caption, index2));
      iesbInfoText:
        result := _DoCompareStr(ReplaceIEMConsts(ImageInfoText[index1].Caption, index1), ReplaceIEMConsts(ImageInfoText[index2].Caption, index2));
      iesbImageSize:
        begin
          s1 := ImageOriginalWidth[index1] * ImageOriginalHeight[index1];
          s2 := ImageOriginalWidth[index2] * ImageOriginalHeight[index2];
          if s1 < s2 then
            result := -1
          else
          if s1 > s2 then
            result := 1
          else
            result := 0;
        end;
      iesbFilenameWithoutPath:
        result := _DoCompareStr(ExtractFilename(ImageFileName[index1]), ExtractFilename(ImageFileName[index2]));
      iesbFileExtension:
        result := CompareText(ExtractFileExt(ImageFileName[index1]), ExtractFileExt(ImageFileName[index2]));
      iesbFileSize :
        begin
          u1 := ImageFileSize[index1];
          u2 := ImageFileSize[index2];
          if u1 < u2 then
            result := -1
          else
          if u1 > u2 then
            result := 1
          else
            result := 0;
        end;
      iesbCreateDate :
        begin
          d1 := ImageCreateDate[index1];
          d2 := ImageCreateDate[index2];
          if d1 < d2 then
            result := -1
          else
          if d1 > d2 then
            result := 1
          else
            result := 0;
        end;
      iesbEditDate :
        begin
          d1 := ImageEditDate[index1];
          d2 := ImageEditDate[index2];
          if d1 < d2 then
            result := -1
          else
          if d1 > d2 then
            result := 1
          else
            result := 0;
        end;
      iesbFileType :
        result := CompareText(fFileTypeList[PIEImageInfo(fImageInfo[index1])^.FileTypeIndex], fFileTypeList[PIEImageInfo(fImageInfo[index2])^.FileTypeIndex]);
      else
        result := 0;  // impossible
    end;

    // if files are identical then sort by name
    if (Result = 0) and (fCurrentOrderBy in [iesbImageSize, iesbFileExtension, iesbFileSize, iesbCreateDate, iesbEditDate, iesbFileType]) then
      Result := _DoCompareStr(ImageFileName[index1], ImageFileName[index2]);
  end;    

  if not fCurrentAscending then
    result := -result;
end;

procedure TImageEnMView.Sort(OrderBy: TIEImageEnMViewSortBy; Ascending: boolean = true; CaseSensitive: boolean = true);
var
  I: Integer;
  sFileType: string;
begin
  if OrderBy = iesbFileType then
  begin
    fFileTypeList := TStringList.create;
    for I := 0 to ImageCount - 1 do
    begin
      sFileType := GetImageFileType(I);
      fFileTypeList.Add(sFileType);
      PIEImageInfo(fImageInfo[I])^.FileTypeIndex := fFileTypeList.Count - 1;
    end;
  end;

  fCurrentOrderBy := OrderBy;
  fCurrentAscending := Ascending;
  fCurrentCaseSensitive := CaseSensitive;
  if (OrderBy <> iesbNone) and (OrderBy <> iesbCustom) then
    Sort2(nil, SortCompareBy);
  
  if OrderBy = iesbFileType then
    FreeAndNil(fFileTypeList);
end;

procedure TImageEnMView.Sort2(Compare: TIEImageEnMViewSortCompare; CompareEx: TIEImageEnMViewSortCompareEx);
var
  psel: integer;
begin
  if fImageInfo.Count > 0 then
  begin
    SetPlaying(false);
    psel := fSelectedItem;
    DeselectNU;
    fCurrentCompare := Compare;
    fCurrentCompareEx := CompareEx;
    try
      EnterCriticalSection(fThreadCS);  
      IEQuickSort(fImageInfo.Count, SortCompareFunction, SwapImages);
    finally
      LeaveCriticalSection(fThreadCS);
    end;
    SetSelectedItemNU(psel);
    UpdateEx(false);
  end;
end;

procedure TImageEnMView.DoImageSelect(idx: integer);
begin
  if assigned(fOnImageSelect) and fUserAction then
    fOnImageSelect(self, idx);
end;

procedure TImageEnMView.DoImageDeselect(idx: integer);
begin
  if not fDestroying and assigned(fOnImageDeselect) and fUserAction then
    fOnImageDeselect(self, idx);
end;


{$ifdef IEINCLUDEFLATSB}
{!!
<FS>TImageEnMView.FlatScrollBars

<FM>Declaration<FC>
property FlatScrollBars: Boolean;

<FM>Description<FN>
Specifies whether the component's scrollbars are flat.
!!}
procedure TImageEnMView.SetFlatScrollBars(Value: Boolean);
begin
  if Value<>fFlatScrollBars then
  begin
    fFlatScrollBars := Value;
    if fFlatScrollBars then
    begin
      // Why calls to IESetScrollRange? Please ask Microsoft's programmers!
      IESetScrollRange(Handle, SB_HORZ, 0, 65535, false, false); 
      IESetScrollRange(Handle, SB_VERT, 0, 65535, false, false); 
      InitializeFlatSB(Handle);
      IEShowScrollBar(handle, SB_HORZ, false, true); 
      IEShowScrollBar(handle, SB_VERT, false, true);
    end
    else
      UninitializeFlatSB(Handle);
  end;
end;
{$endif}

{!!
<FS>TImageEnMView.SetPresetThumbnailFrame

<FM>Declaration<FC>
procedure SetPresetThumbnailFrame(PresetIndex: Integer; UnSelectedColor: TColor; SelectedColor: TColor);

<FM>Description<FN>
Applies a background style to the thumbnails from an internal set
                           
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>PresetIndex</C> <C>The style to use (0: Photo, 1: Film, 2: Album, 3: Picture Frame)</C> </R>
<R> <C>UnSelectedColor</C> <C>Color modification used when frame is unselected</C> </R>
<R> <C>SelectedColor</C> <C>Color modification used when frame is selected</C> </R>
</TABLE>

<FM>Examples<FC>
ImageEnMView1.SetPresetThumbnailFrame(0, clWhite, clGreen);
<IMG help_images\75.bmp>

ImageEnMView1.SetPresetThumbnailFrame(2, clWhite, clGreen);
<IMG help_images\76.bmp>

<FM>See demo<FN>
Multi\Thumbnails2

!!}
procedure TImageEnMView.SetPresetThumbnailFrame(PresetIndex: Integer; UnSelectedColor: TColor; SelectedColor: TColor);
var
  io: TImageEnIO;
  proc: TImageEnProc;
  rgb: TRGB;
begin
  if (PresetIndex>=0) and (PresetIndex<iegPresetImages.Count) then
  begin
    EnableAlphaChannel := true;
    FillThumbnail := false;
    SelectionWidth := 0;
    ShowText := false;
    Style := iemsFlat;
    DrawImageBackground := false;

    if assigned(fThumbnailFrame) then
      FreeAndNil(fThumbnailFrame);
    if assigned(fThumbnailFrameSelected) then
      FreeAndNil(fThumbnailFrameSelected);

    fThumbnailFrame := TIEBitmap.Create;
    fThumbnailFrameSelected := TIEBitmap.Create;

    io := TImageEnIO.CreateFromBitmap(fThumbnailFrame);
    try
      with TIEPresetImage(iegPresetImages[PresetIndex]) do
      begin
        io.LoadFromBuffer( Data, Size, FileFormat );
        fThumbnailFrameRect := ThumbRect;
      end;
    finally
      FreeAndNil(io);
    end;

    fThumbnailFrameSelected.Assign( fThumbnailFrame );

    proc := TImageEnProc.CreateFromBitmap(fThumbnailFrameSelected);
    proc.AutoUndo := false;

    // set selected color
    rgb := TColor2TRGB(SelectedColor);
    proc.IntensityRGBall(rgb.r-255, rgb.g-255, rgb.b-255);

    // set unselected color
    proc.AttachedIEBitmap := fThumbnailFrame;
    rgb := TColor2TRGB(UnSelectedColor);
    proc.IntensityRGBall(rgb.r-255, rgb.g-255, rgb.b-255);

    FreeAndNil(proc);

    ThumbWidth  := fThumbnailFrame.Width;
    ThumbHeight := fThumbnailFrame.Height;

    Update;
  end;
end;

{$ifdef IEDOTNETVERSION}
procedure TImageEnMView.WMContextMenu(var Message: TWMContextMenu);
begin
  // just to remove Delphi default behavior
end;
{$endif}

{!!
<FS>TImageEnMView.IEBeginDrag

<FM>Declaration<FC>
procedure IEBeginDrag(Immediate: Boolean; Threshold: Integer = -1);

<FM>Description<FN>
ImageEn's implementation of BeginDrag. You must use this for Drag/Drop operations in TImageEnMView

<FM>Demo<FN>
DragDrop\TImageEnMView_DragDrop

<FM>See Also<FN>
- <A TImageEnMView.IEEndDrag>
- <L TImageEnMView.MultiSelectionOptions>iemoOptimizeForDragging</L>

!!}
procedure TImageEnMView.IEBeginDrag(Immediate: Boolean; Threshold: Integer);
begin
  // Why this? Because BeginDrag calls MouseUp, but "Dragging" of VCL is still not True and MouseUp must know we are dragging
  fDragging := true;
  BeginDrag(immediate, Threshold);
  fDragging := Dragging;  // get the official dragging value
  InitializeDragScrollTimer;
end;

{!!
<FS>TImageEnMView.IEEndDrag

<FM>Declaration<FC>
procedure IEEndDrag;

<FM>Description<FN>
ImageEn's implementation of EndDrag. You must use this for Drag/Drop operations in TImageEnMView

<FM>Demo<FN>
DragDrop\TImageEnMView_DragDrop

<FM>See Also<FN>
- <A TImageEnMView.IEBeginDrag>
!!}
procedure TImageEnMView.IEEndDrag;
begin
  EndDrag(true);
  fDragging := false;
  TerminateDragScrollTimer;
end;


// Handle automatic scrolling when we drag to the edges
procedure TImageEnMView.InitializeDragScrollTimer;
begin         
  if assigned(FDragScrollTimer) = False then
    FDragScrollTimer := TTimer.create(Self);
  FDragScrollCount := 0;
  FDragScrollTimer.Interval := 250;
  FDragScrollTimer.Enabled := True;
  FDragScrollTimer.OnTimer := DragScrollTimer;
end;

// End automatic scrolling when dragging completes
procedure TImageEnMView.TerminateDragScrollTimer;
begin
  if assigned(FDragScrollTimer) then
    FDragScrollTimer.Enabled := False;
end;

// Handle automatic scrolling when we drag to the edges
procedure TImageEnMView.DragScrollTimer(Sender: TObject);
const
  Scrollable_Area = 25;
  Max_Speed_Up_Dragging_Pos = 50;
var
  Pos: TPoint;
  iLineStep: Integer;
  bHaveScrolled: Boolean;
  bHaveVertScrolling: Boolean;
  bHaveHorzScrolling: Boolean;
begin
  if fDragging = False then
  begin
    TerminateDragScrollTimer;
    exit;
  end;

  GetCursorPos(Pos);
  Pos := ScreenToClient(Pos);

  bHaveScrolled := False;
  bHaveVertScrolling := (fCurrentGridWidth <> 0) and (fVHeight > ClientHeight);
  bHaveHorzScrolling := (fCurrentGridWidth <> -1) and (fVWidth > ClientWidth);

  // Vertical scrolling
  if bHaveVertScrolling and (Pos.X > 0) and (Pos.X < Width) then
  begin
    if fVScrollBarParams.LineStep = -1 then
      iLineStep := ThumbHeight - fVertBorder
    else
      iLineStep := fVScrollBarParams.LineStep;

    if Pos.Y < Scrollable_Area then
    begin
      SetViewY(fViewY - iLineStep);
      bHaveScrolled := True;
    end
    else
    if Pos.Y > Height - Scrollable_Area then
    begin
      SetViewY(fViewY + iLineStep);
      bHaveScrolled := True;
    end;
  end
  else
  // Horizontal scrolling
  if bHaveHorzScrolling and (Pos.Y > 0) and (Pos.Y < Height) then
  begin
    if fHScrollBarParams.LineStep = -1 then
      iLineStep := ThumbWidth - fHorizBorder
    else
      iLineStep := fHScrollBarParams.LineStep;

    if Pos.X < Scrollable_Area then
    begin
      SetViewX(fViewX - iLineStep);
      bHaveScrolled := True;
    end
    else
    if Pos.X > Width - Scrollable_Area then
    begin
      SetViewX(fViewX + iLineStep);
      bHaveScrolled := True;
    end;
  end;

  if bHaveScrolled then
  begin
    ViewChange(0);
    if FDragScrollCount < Max_Speed_Up_Dragging_Pos then
    begin
      inc(FDragScrollCount);
      if FDragScrollCount mod 5 = 0 then
        FDragScrollTimer.Interval := MulDiv(FDragScrollTimer.Interval, 3, 4);  // speed up the scrolling by reducing the timer interval
    end;
  end
  else
  begin
    InitializeDragScrollTimer;
    exit;
  end;
end;

{!!
<FS>TImageEnMView.JobsRunning

<FM>Declaration<FC>
property JobsRunning: Integer;

<FM>Description<FN>
Returns the number of active threads which are loading thumbnails.

<FM>Demo<FN>
Multi/Thumbnails

!!}
function TImageEnMView.GetJobsRunning: Integer;
begin
  result := fThreadPoolIO.Count;
end;

{!!
<FS>TImageEnMView.JobsWaiting

<FM>Declaration<FC>
property JobsWaiting: Integer;

<FM>Description<FN>
Returns the number of images/thumbnails waiting to be loaded.

<FM>Demo<FN>
Multi/Thumbnails

!!}
function TImageEnMView.GetJobsWaiting: Integer;
begin
  result := fThreadRequests.Count;
end;

{!!
<FS>TImageEnMView.LoadFromFileOnDemand

<FM>Declaration<FC>
procedure LoadFromFileOnDemand(const FileName: WideString; Append: Boolean = false);

<FM>Description<FN>
Fills TImageEnMView with all frames of a multiple image file (AVI, MPEG, TIFF, etc.) by filling <A TImageEnMView.ImageFileName> with filenames and all indexes of its frames (in the format 'FileName::0', 'FileName::1', etc.).

In this way loading of individual frames will be delayed until they are required (i.e. frames are only loaded when they appear on-screen). It is mainly used for loading the frames of files with numerous frames (such as videos).

When Append is false, the existing content of the TImageEnMView is cleared.

Note: You can set TImageEnMView.MIO.Aborting := True to abort the loading of an OnDemand sequence

<FM>Example<FC>
ImageEnMView1.LoadFromFileOnDemand('C:\input.mpeg');

If input.mpeg has 1000 frames, then we will now have:
ImageEnMView1.ImageFileName[0] = 'C:\input.mpeg::0'
ImageEnMView1.ImageFileName[1] = 'C:\input.mpeg::1'
...
ImageEnMView1.ImageFileName[999] = 'C:\input.mpeg::999'

<FM>Demo<FN>
InputOutput/LargeVideos

!!}
procedure TImageEnMView.LoadFromFileOnDemand(const FileName: WideString; Append: Boolean);
var
  FramesCount, i, ofs: Integer;
  lAnimation: TIEAnimation;
begin
  lAnimation := Animation;
  fAnimation := nil;
  LockPaint();   
  MIO.Aborting := False;
  try
    if Append then
    begin
      ofs := ImageCount;
    end
    else
    begin
      ofs := 0;
      Clear();
    end;
    FramesCount := IEGetFileFramesCount(FileName);
    for i := 0 to FramesCount-1 do
    begin
      InsertImageEx( ofs + i );
      ImageFileName[ ofs + i ] := filename + IEM_Path_Index_Delimiter + IntToStr(i);
    end;
  finally
    fAnimation := lAnimation;
    UnLockPaint();
  end;
end;

function CreateLinesArray(vect: TImageEnVect): PIELineArray;
var
  i, hobj: Integer;
  r: TRect;
begin
  getmem(result, SizeOf(TIELine) * vect.ObjectsCount);
  for i := 0 to vect.ObjectsCount-1 do
  begin
    hobj := vect.GetObjFromIndex(i);
    vect.GetObjRect(hobj, r);
    result[i].P.x := r.Left;
    result[i].P.y := r.Top;
    result[i].Q.x := r.Right;
    result[i].Q.y := r.Bottom;
  end;
end;

{!!
<FS>TImageEnMView.CreateMorphingSequence

<FM>Declaration<FC>
procedure CreateMorphingSequence(Source: <A TImageEnVect>; Target: <A TImageEnVect>; FramesCount: Integer);

<FM>Description<FN>
Creates a sequence of frames which are the transformation of the <FC>Source<FN> image to <FC>Target<FN> image. FramesCount specifies the number of frames to create.

Notes:
- The <A TIEBitmap.PixelFormat> of both images must be ie24RGB (true color)
- The dimension of the images must be identical
- The Source and Target must contain iekLINE objects to describe the transformation. The number of line objects must be identical and applied in the same order on both TImageEnVect components

<FM>Demo<FN>
Other/Morphing

!!}
procedure TImageEnMView.CreateMorphingSequence(Source: TImageEnVect; Target: TImageEnVect; FramesCount: Integer);
var
  source_lines, dest_lines: PIELineArray;
  outimages_src, outimages_trg: TList;
  outcount, i: Integer;
  ie: TImageEnView;
begin
  if (Source.ObjectsCount=0) or (Target.ObjectsCount=0) or (Source.ObjectsCount<>Target.ObjectsCount) then
    exit;
  if (Source.IEBitmap.PixelFormat<>ie24RGB) or (Target.IEBitmap.PixelFormat<>ie24RGB) then
    exit;

  source_lines := nil;
  dest_lines := nil;
  outimages_src := nil;
  outimages_trg := nil;
  outcount := 0;

  try
    source_lines := CreateLinesArray(Source);
    dest_lines := CreateLinesArray(Target);
    outimages_src := TList.Create;
    outimages_trg := TList.Create;

    IEFields_warp( Source.IEBitmap, source_lines, dest_lines, Source.ObjectsCount, FramesCount, outimages_src);
    IEFields_warp( Target.IEBitmap, dest_lines, source_lines, Target.ObjectsCount, FramesCount, outimages_trg);

    SetIEBitmapEx( AppendImage , Source.IEBitmap);

    ie := TImageEnView.Create(nil);
    try
      outcount := outimages_src.Count;
      for i := 0 to outcount-1 do
      begin
        ie.IEBitmap.Assign(outimages_src[i]);
        ie.Update;
        ie.Proc.MergeIEBitmap(outimages_trg[outcount-1-i], trunc((outcount-i)/outcount*100));
        SetIEBitmapEx( AppendImage , ie.IEBitmap );
      end;
    finally
      FreeAndNil(ie);
    end;

    SetIEBitmapEx( AppendImage , Target.IEBitmap);

  finally
    for i := 0 to outcount-1 do
      TIEBitmap(outimages_src[i]).Free;
    FreeAndNil(outimages_src);
    for i := 0 to outcount-1 do
      TIEBitmap(outimages_trg[i]).Free;
    FreeAndNil(outimages_trg);

    freemem(dest_lines);
    freemem(source_lines);
  end;

  Update;
end;

{!!
<FS>TImageEnMView.ImageEnVersion

<FM>Declaration<FC>
property ImageEnVersion: String;

<FM>Description<FN>
Returns the ImageEn version as a string.

!!}
function TImageEnMView.GetImageEnVersion: String;
begin
  result := IEMAINVERSION;
end;

procedure TImageEnMView.SetImageEnVersion(Value: String);
begin
  // this is a read-only property, but it must be displayed in object inspector
end;

procedure TImageEnMView.SaveSnapshot(FileName: WideString; SaveCache: Boolean; Compressed: Boolean; SaveParams: Boolean);
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmCreate);
  try
    SaveSnapshot(fs, SaveCache, Compressed, SaveParams);
  finally
    fs.Free;
  end;
end;

function TImageEnMView.LoadSnapshot(FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := LoadSnapshot(fs);
  finally
    fs.Free;
  end;
end;

{!!
<FS>TImageEnMView.SaveSnapshot

<FM>Declaration<FC>                                                                                                          
procedure SaveSnapshot(FileName: WideString; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False);
procedure SaveSnapshot(Stream: TStream; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False);

<FM>Description<FN>
Saves the image, cache data, text and dimensions of all frames to the specified stream or file.

This can be used to create caching mechanism, like Windows .db files, to load quickly an entire directory of images.

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>  
<R> <C>Filename/Stream</C> <C>Destination for snapshot</C> </R>
<R> <C>SaveCache</C> <C>If enabled (default), the image cache is also saved. This will speed up display but require more disk space</C> </R>
<R> <C>Compressed</C> <C>If enabled, an LZ compression algorithm is used to reduce disk space (though saving will be slower)</C> </R>
<R> <C>SaveParams</C> <C>Enable to save the <L TImageEnMIO.Params>input/output parameters</L> for all images</C> </R>
</TABLE>

You can reload a saved snapshot using <A TImageEnMView.LoadSnapshot>.
!!}
procedure TImageEnMView.SaveSnapshot(Stream: TStream; SaveCache: Boolean; Compressed: Boolean; SaveParams: Boolean);
begin
  SaveSnapshotEx(Stream, SaveCache, Compressed, SaveParams, nil);
end;

      
// versions:
//    2: 2.3.2
//    3: 3.0.0
//    4: 3.0.5
//    5: 4.0.3
//    6: 4.1.1-B
//    7: 5.0.1
procedure TImageEnMView.SaveSnapshotEx(Stream: TStream; SaveCache: Boolean; Compressed: Boolean; SaveParams: Boolean; GetExtraParams: TIEProcessStreamEvent);
const
  Snapshot_Save_Version = 7;
var
  ver: byte;
  i, i32: Integer;
  ii: PIEImageInfo;
  LZStream: TZCompressionStream;
begin
  // magick
  IESaveStringToStream(Stream, 'MVIEWSNAPSHOT');
  // version
  ver := Snapshot_Save_Version;
  Stream.Write(ver, SizeOf(byte));

  Stream.Write(Compressed, SizeOf(boolean));

  if Compressed then
  begin
    LZStream := TZCompressionStream.Create(Stream, zcDefault, 15);
    Stream := LZStream;
  end
  else
    LZStream := nil;

  try
    // fThumbWidth and fThumbHeight
    Stream.Write(fThumbWidth, SizeOf(integer));
    Stream.Write(fThumbHeight, SizeOf(integer));

    // fUpperGap and fBottomGap
    Stream.Write(fUpperGap, SizeOf(integer));
    Stream.Write(fBottomGap, SizeOf(integer));

    // SideGap and TextMargin
    Stream.Write(fSideGap, SizeOf(integer));
    Stream.Write(fTextMargin, SizeOf(integer));

    // StoreType
    Stream.Write(fStoreType, SizeOf(TIEStoreType));

    // Parameters for descended components, such as TImageEnFolderMView
    if assigned(GetExtraParams) then
      GetExtraParams(Self, Stream, Snapshot_Save_Version);

    // images
    fImageList.SaveToStream(Stream);
    // caches
    Stream.Write(SaveCache, SizeOf(boolean));
    if SaveCache then
      fCacheList.SaveToStream(Stream);
    // images count
    i32 := fImageInfo.Count; Stream.Write(i32, SizeOf(integer));
    // info
    for i := 0 to fImageInfo.Count-1 do
    begin
      ii := PIEImageInfo(fImageInfo[i]);
      // index of images
      i32 := fImageList.FindImageIndex( ii^.image );
      Stream.Write(i32, SizeOf(integer));
      // index of caches
      if SaveCache then
      begin
        i32 := fCacheList.FindImageIndex( ii^.cacheImage );
        Stream.Write(i32, SizeOf(integer));
      end;
      // background
      Stream.Write( ii^.Background, SizeOf(TColor) );
      // name
      IESaveStringToStreamW(Stream, ii^.Filename);
      // ID
      Stream.Write( ii^.ID, SizeOf(integer) );
      // DTime
      Stream.Write( ii^.DTime, SizeOf(double) );
      // tag
      Stream.Write( ii^.tag, SizeOf(integer) );
      // text
      ii^.TopText.SaveToStream(Stream);
      ii^.InfoText.SaveToStream(Stream);
      ii^.BottomText.SaveToStream(Stream);
      // Checked            
      Stream.Write( ii^.Checked, SizeOf(Boolean) );
      // I/O params (only of MIO embedded object)
      Stream.Write(SaveParams, SizeOf(boolean));
      if SaveParams then
        MIO.Params[i].SaveToStream(Stream);
    end;

  finally
    if Compressed then
      LZStream.Free;
  end;
end;

{!!
<FS>TImageEnMView.LoadSnapshot

<FM>Declaration<FC>                            
function LoadSnapshot(FileName: WideString): Boolean;
function LoadSnapshot(Stream: TStream): Boolean;

<FM>Description<FN>                 
Loads the image, cache data, text and dimensions of frames that have been save to a stream or file using <A TImageEnMView.SaveSnapshot>.

This can be used to create caching mechanism, like Windows .db files, to load quickly an entire directory of images.

!!}
function TImageEnMView.LoadSnapshot(Stream: TStream): Boolean;
begin
  result := LoadSnapshotEx(Stream, nil);
end;

function TImageEnMView.LoadSnapshotEx(Stream: TStream; SetExtraParams: TIEProcessStreamEvent): Boolean;
var
  magick: AnsiString;
  ver: byte;
  i, i32, a32, itemp: Integer;
  ii: PIEImageInfo;
  s: AnsiString;
  ws: WideString;
  LoadCache: Boolean;
  LoadParams: Boolean;
  Compressed: Boolean;
  LZStream: TZDecompressionStream;
begin
  result := false;
  // magick
  IELoadStringFromStream(Stream, magick);
  if magick<>'MVIEWSNAPSHOT' then
    exit;
  // version
  Stream.Read(ver, SizeOf(byte));

  Clear;

  Stream.Read(Compressed, SizeOf(boolean));
  if Compressed then
  begin
    LZStream := TZDecompressionStream.Create(Stream);
    Stream := LZStream;
  end
  else
    LZStream := nil;

  try
    // fThumbWidth and fThumbHeight
    Stream.Read(fThumbWidth, SizeOf(integer));
    Stream.Read(fThumbHeight, SizeOf(integer));

    if ver >= 3 then
    begin
      // fUpperGap and fBottomGap
      Stream.Read(fUpperGap, SizeOf(integer));
      Stream.Read(fBottomGap, SizeOf(integer));
    end;

    if ver >= 7 then
    begin
      // SideGap and TextMargin
      Stream.Read(fSideGap, SizeOf(integer));
      Stream.Read(fTextMargin, SizeOf(integer));
    end;

    if ver >= 4 then
      Stream.Read(fStoreType, SizeOf(TIEStoreType));

    // Parameters for descended components, such as TImageEnFolderMView
    if assigned(SetExtraParams) then
      SetExtraParams(Self, Stream, ver);

    // images
    result := fImageList.LoadFromStream(Stream);
    if not result then exit;
    // caches
    Stream.Read(LoadCache, SizeOf(boolean));
    if LoadCache then
      result := fCacheList.LoadFromStream(Stream);
    if not result then exit;
    // images count
    Stream.Read(i32, SizeOf(integer));
    // info
    for i := 0 to i32-1 do
    begin
      getmem(ii, SizeOf(TIEImageInfo));
      ii^.parent := self;
      fImageInfo.Add(ii);
      // image
      Stream.Read(a32, SizeOf(integer));
      ii^.image := fImageList.GetImageFromIndex(a32);
      // cache
      ii^.cacheImage := nil;
      if LoadCache then
      begin
        Stream.Read(a32, SizeOf(integer));
        ii^.cacheImage := fCacheList.GetImageFromIndex(a32)
      end;
      // background
      Stream.Read( ii^.Background, SizeOf(TColor) );
      // name
      if ver = 1 then
      begin
        IELoadStringFromStream(Stream, s);
        ws := WideString(s);
      end
      else
        IELoadStringFromStreamW(Stream, ws);
      if length(ws) > 0 then
      begin
        getmem(ii^.Filename, (length(ws) + 1) * SizeOf(WideChar));
        iewstrcopy(ii^.Filename, pwchar(ws));
      end
      else
        ii^.Filename := nil;
      // ID
      Stream.Read( ii^.ID, SizeOf(integer) );
      // DTime
      if ver >= 5 then
        Stream.Read( ii^.DTime, SizeOf(double) )
      else
      begin
        Stream.Read( itemp, SizeOf(integer) );
        ii^.DTime := itemp;
      end;
      // tag
      if ver >= 6 then
        Stream.Read( ii^.tag, SizeOf(integer) );
      // text
      ii^.TopText := TIEMText.Create(self, iemtpTop);
      ii^.BottomText := TIEMText.Create(self, iemtpBottom);
      ii^.InfoText := TIEMText.Create(self, iemtpInfo);
      ii^.TopText.LoadFromStream(Stream);
      ii^.InfoText.LoadFromStream(Stream);
      ii^.BottomText.LoadFromStream(Stream);

      // Checked
      if ver >= 7 then
        Stream.Read( ii^.Checked, SizeOf(Boolean) );

      // I/O params (only of MIO embedded object)
      if ver>=4 then
      begin
        Stream.Read(LoadParams, SizeOf(boolean));
        if LoadParams then
          MIO.Params[i].LoadFromStream(Stream);
      end;

      fLastImOp := 1; // insert...
      fLastImIdx := i; //...image
      fCheckedCount := -1;
      CallBitmapChangeEvents;
    end;

  finally
    if Compressed then
      LZStream.Free;
  end;

  if LoadCache then
    UpdateEx(false)
  else
    UpdateEx(true);

  result := true;
end;


{!!
<FS>TImageEnMView.RemoveBlankPages

<FM>Declaration<FC>
function RemoveBlankPages(Tolerance: Double; Complete: boolean; LeftToRight: boolean): Integer;

<FM>Description<FN>
Locate images in the TImageEnMView of a single color (i.e. blank images) and remove them.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Tolerance<FN></C> <C>Determines how tolerant to be in checking color variance (in the range 0.0 to 1.0). For example, if tolerance is 0.1 then 10% of pixels can be of different color and the image would still be considered "blank"</C> </R>
<R> <C><FC>Complete<FN></C> <C>If true all images are checked. Otherwise the check stops once the first non blank image has been found and removed.</C> </R>
<R> <C><FC>LeftToRight<FN></C> <C>If true the scan starts at the first image and proceeds to the last (otherwise it proceeds in reverse order)</C> </R>
</TABLE>

Returns the number of removed pages.

<FM>Example<FC>
// remove last blank pages
ImageEnMView1.RemoveBlankPages(0.0, false, false);
!!}
function TImageEnMView.RemoveBlankPages(Tolerance: Double; Complete: boolean; LeftToRight: boolean): Integer;
var
  proc: TImageEnProc;
  i: Integer;
  domValue: Double;
  domColor: TRGB;
begin
  result := 0;
  proc := TImageEnProc.Create(nil);
  proc.AutoUndo := false;
  try

    if LeftToRight then
      i := 0
    else
      i := ImageCount-1;
    while (LeftToRight and (i<ImageCount)) or (not LeftToRight and (i>=0)) do
    begin
      proc.AttachedIEBitmap := GetTIEBitmap(i);
      try
        domValue := proc.GetDominantColor(domColor) / 100;   // 3.0.3
      finally
        ReleaseBitmap(i, false);
      end;
      if 1-domValue < tolerance then
      begin
        DeleteImage(i);
        inc(result);
        if not LeftToRight then
          dec(i);
      end
      else
      begin
        if not Complete then
          // stop when a non-blank occurs
          break;
        if LeftToRight then
          inc(i)
        else
          dec(i);
      end;
    end;

  finally
    proc.Free;
  end;

end;


{!!
<FS>TImageEnMView.DisplayImageAt

<FM>Declaration<FC>
procedure DisplayImageAt(idx: Integer; x, y: Integer);

<FM>Description<FN>
Scrolls the control to display image, <FC>idx<FN>, at specified position in the current view.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>idx<FN></C> <C>Index of the image to display</C> </R>
<R> <C><FC>x<FN></C> <C>Horizontal offset in client area</C> </R>
<R> <C><FC>y<FN></C> <C>Vertical offset in client area</C> </R>
</TABLE>

<FM>Example<FC>
// make then tenth image visible at 0, 0 (Top-left of control)
ImageEnMView1.DisplayImageAt(9, 0, 0);
!!}
procedure TImageEnMView.DisplayImageAt(idx: Integer; x, y: Integer);
var
  newViewX, newViewY: Integer;
  image: PIEImageInfo;
begin
  image := PIEImageInfo(fImageInfo[idx]);
  newViewX := image^.X + x;
  newViewY := image^.Y + y;
  SetViewXY(newViewX, newViewY);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// animations supports

{!!
<FS>TImageEnMView.Animation

<FM>Declaration<FC>
property Animation: <A TIEAnimation>;

<FM>Description<FN>
Uses a special animated mode to display your images. Two internal animations are available:
- <A TIEHorizontalFlow>: Coverflow style animation
- <A TIECircularFlow>: Circular flow animation

Setting Animation to nil (default) disables animation.

<FM>Example<FC>
ImageEnMView1.Animation := TIEHorizontalFlow.Create();

<FM>Demo<FN>
\Display\MviewFlow\MviewFlow.dpr     

<FM>See Also<FN>
- <A TImageEnMView.OnAnimationText>
!!}
procedure TImageEnMView.SetAnimation(value: TIEAnimation);
begin
  if assigned(fAnimation) then
  begin
    fAnimationTimer.Enabled := false;
    FreeAndNil(fAnimation);
  end;
  fAnimation := value;
  if assigned(fAnimation) then
  begin
    // hide scroll bars
    IEShowScrollBar(handle, SB_VERT, false, fFlatScrollBars);
    IEShowScrollBar(handle, SB_HORZ, false, fFlatScrollBars);
    UpdateEx(false);

    fAnimation.SetupEvents(AnimGetImage, AnimReleaseImage, AnimGetImageInfo);
    fAnimation.ViewWidth := ClientWidth;
    fAnimation.ViewHeight := ClientHeight;
    fAnimation.ImageCount := ImageCount;

    fAnimationTimer.Interval := 10;
    fAnimationTimer.OnTimer := AnimTimer;
    fAnimationTimer.Enabled := true;
  end;
  Update();
end;

procedure TImageEnMView.AnimGetImageInfo(Sender: TObject; imageIndex: Integer; isVisible: Boolean; var ImageWidth: Integer; var ImageHeight: Integer; var text: WideString);
var
  bmp: TIEBitmap;
begin
  ImageWidth  := self.ImageWidth[imageIndex];
  ImageHeight := self.ImageHeight[imageIndex];
  if (ImageWidth = 0) or (ImageHeight = 0) then
  begin
    // return thumb size (if still not loaded)
    ImageWidth  := self.ThumbWidth;
    ImageHeight := self.ThumbHeight;
    if isVisible then
    begin
      // return actual image size
      bmp := GetTIEBitmap(imageIndex);
      if bmp <> nil then
      begin
        ImageWidth  := bmp.Width;
        ImageHeight := bmp.Height;
        ReleaseBitmap(imageIndex, false);
      end;
    end;
  end;
  if assigned(fOnAnimationText) then
    fOnAnimationText(self, imageIndex, text)
  else
    text := IEExtractFileNameW( ImageFileName[imageIndex] );
end;

procedure TImageEnMView.AnimGetImage(Sender: TObject; imageIndex: Integer; var image: TIEBitmap; var text: WideString);
begin
  image := GetTIEBitmap(imageIndex);
  text := IEExtractFileNameW( ImageFileName[imageIndex] );
end;

procedure TImageEnMView.AnimReleaseImage(Sender: TObject; imageIndex: Integer; var image: TIEBitmap);
begin
  ReleaseBitmap(imageIndex, false);
end;

procedure TImageEnMView.AnimPaintTo(dest: TIEBitmap);
begin
  if fAnimation.NeedRefresh then
  begin
    PaintBackgroundTo(dest.VclBitmap);
    fAnimation.Display(dest);
  end;
end;

procedure TImageEnMView.AnimTimer(Sender: TObject);
begin
  Paint();
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


// Copy selection from one TImageEnMView to another (assumes multiselction and that both have same image set)
procedure TImageEnMView.CopySelection(SourceMView : TImageEnMView);
var
  q: integer;
  lMultiSelecting: boolean;
begin
  If ImageCount <> SourceMView.ImageCount then
    exit;

  if fEnableMultiSelect then
  begin
    DeselectNU; 
    lMultiSelecting := fMultiSelecting;
    fMultiSelecting := true;
    for q := 0 to SourceMView.ImageCount - 1 do
      if SourceMView.IsSelected(q) then
        SetSelectedItemNU(q);
    fMultiSelecting := lMultiSelecting;
    UpdateEx(false);
  end;
end;



{!!
<FS>TImageEnMView.CalcGridWidth

<FM>Declaration<FC>
function CalcGridWidth(): Integer;

<FM>Description<FN>
Returns the number of columns displayed in the grid. This is mainly used when the column count is automatically calculated (i.e. when <A TImageEnMView.GridWidth> = -1).
!!}
function TImageEnMView.CalcGridWidth(): Integer;
begin
  case fCurrentGridWidth of
    -1:
      begin
        result := (ClientWidth - fHorizBorder) div (ThumbWidth + fHorizBorder);
        if (result < 1) and (ImageCount > 0) then
          result := 1;
      end;
     0:
      result := ImageCount;
    else
      result := fCurrentGridWidth;
  end;
end;


{!!
<FS>TImageEnMView.CalcGridHeight

<FM>Declaration<FC>
function CalcGridHeight(): Integer;

<FM>Description<FN>
Returns the number of rows displayed in the grid.
!!}
function TImageEnMView.CalcGridHeight(): Integer;
var
  gw: integer;
begin
  gw := CalcGridWidth();
  if (ImageCount > 0) and (gw <> 0) then
    result := IECeil(ImageCount / gw)
  else
    result := 0;
end;

procedure TImageEnMView.CheckSelectedImageIsVisible;
// NPC: 23/11/11
var
  info: PIEImageInfo;
  iViewX, iViewY : integer;
begin
  if (fLockPaint > 0) or (fLockUpdate > 0) then
    exit;

  if assigned(Parent) and (fSelectedItem >= 0) and (fMultiSelecting = False) then
  begin
    iViewX := fViewX;
    iViewY := fViewY;
    info := PIEImageInfo(fImageInfo[fSelectedItem]);
              
    if info^.Col = 0 then
      iViewX := 0
    else
    if info^.X < iViewX then
      iViewX := info^.X
    else
    if info^.X + ThumbWidth > iViewX + ClientWidth then
      iViewX := info^.X - ClientWidth + ThumbWidth;
            
    if info^.Row = 0 then
      iViewY := 0
    else
    if info^.Y < iViewY then
      iViewY := info^.Y
    else
    if info^.Y + ThumbHeight > iViewY + ClientHeight then
      iViewY := info^.Y - ClientHeight + ThumbHeight;

    SetViewXY(iViewX, iViewY);
  end;
end;



{!!
<FS>TImageEnMView.Seek

<FM>Declaration<FC>
function Seek(Page : <A TImageEnIO.TIEIOSeekDestination>) : integer;

<FM>Description<FN>
When <A TImageEnMView.DisplayMode> is mdSingle then Seek sets <A TImageEnMView.VisibleFrame> to display the first/previous/next/last image.

Otherwise it sets <A TImageEnMView.SelectedImage> to move the selection to the first/previous/next/last image.

Returns the new page index.

<FM>Example<FC>
// Display a single image
ImageEnMView1.DisplayMode := mdSingle;
..
// Advance to the next image
ImageEnMView1.IO.Seek(ieioSeekNext);
!!}
function TImageEnMView.Seek(Destination : TIEIOSeekDestination): Integer;
// NPC: 23/11/11
var
  iIndex: Integer;
begin
  Result := -1;
  if fImageInfo.Count = 0 then
    exit;

  if fDisplayMode = mdSingle then
    iIndex := VisibleFrame
  else
    iIndex := SelectedImage;

  result := 0;
  case Destination of
    ieioSeekFirst:
      result := 0;
    ieioSeekPrior:
      result := imax(0, iIndex - 1);
    ieioSeekNext:
      result := imin(fImageInfo.Count - 1, iIndex + 1);
    ieioSeekLast:
      result := fImageInfo.Count - 1;
  end;

  if fDisplayMode = mdSingle then
    VisibleFrame  := Result
  else
  begin
    SelectedImage := Result;
    CheckSelectedImageIsVisible;
  end;
end;


procedure TImageEnMView.DoAfterEvent(e: TIEAfterEvent);
begin
  if assigned(fOnAfterEvent) then
    fOnAfterEvent(self, e);
end;

procedure TImageEnMView.CNKEYUP(var Message: TMessage);
begin
  inherited;
  case Message.wParam of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END:
      DoAfterEvent(ieaeKeyUp);  // because no KeyUp event is generated on special keys
  end;
end;

constructor TIECachedIcon.Create(aBmp: TIEBitmap; const sExt: string);
begin
  inherited Create();
  Bmp := TIEBitmap.Create(aBmp);
  Ext := Uppercase(sExt);
end;

destructor TIECachedIcon.Destroy();
begin
  Bmp.Free();
  inherited;
end;   

function TIECachedIcon.MatchesExt(const sExt : string) : Boolean;
begin
  Result := Uppercase(sExt) = Ext;
end;


constructor TIECachedIconList.Create(Owner : TImageEnMView; iMaxIconCount : Integer);
begin
  inherited Create;
  fDataList := TList.create;
  fOwner := Owner;
  fMaxIconCount := iMaxIconCount;
end;

destructor TIECachedIconList.Destroy();
begin
  Clear();
  fDataList.free();
  inherited;
end;

procedure TIECachedIconList.ClearLastItem;
var
  Idx: Integer;
begin
  Idx := fDataList.Count - 1;
  TIECachedIcon(fDataList[Idx]).Free();
  fDataList.Delete(Idx);
end;


procedure TIECachedIconList.Clear();
var
  i: integer;
begin
  for i := 0 to fDataList.Count - 1 do
    TIECachedIcon(fDataList[i]).Free();
  fDataList.Clear();
end;


function TIECachedIconList.GetCacheName(aType : TCachedIconType; const sFilename : string; bIsFolder : Boolean) : string;
const
  _Folder_Item = '~FOLDER';
begin
  if bIsFolder then
    Result := _Folder_Item
  else
    Result := ExtractFileExt(sFilename);

  if Result <> '' then
  begin
    if aType = citIconOnly then
      Result := Result + '~I'
    else
      Result := Result + '~F';
  end;
end;


function TIECachedIconList.LookUpExt(const sExt : string) : Integer;
var
  i: integer;
begin
  Result := -1;

  // CHECK CACHE VALIDITY
  // Most property changes will automatically reset the icon cache, but not changes to the soft shadow, so ensure properties still match
  if (fOwner.SoftShadow.Enabled <> fOwnerSoftShadowEnabled) or
     (fOwner.SoftShadow.Radius <> fOwnerSoftShadowRadius) then
  begin
    fOwnerSoftShadowEnabled := fOwner.SoftShadow.Enabled;
    fOwnerSoftShadowRadius  := fOwner.SoftShadow.Radius;
    Clear();
    exit;
  end;

  // Locate the extension in our list 
  for i := 0 to fDataList.Count - 1 do
    if TIECachedIcon(fDataList[i]).MatchesExt(sExt) then
    begin
      Result := i;
      Break;
    end;
end;


// bWantCopy = True: Dest is pointer to Bitmap in list (don't create or free). bWantCopy = False: Copy of bitmap is made, must be created and freed
function TIECachedIconList.RetrieveFromCache(aType : TCachedIconType; const sFilename : string; bIsFolder : Boolean; var Dest : TIEBitmap; bWantCopy : Boolean): boolean;
const
  File_Extensions_With_Custom_Icons = '*.exe;*.lnk;*.ico;';
var
  Idx: Integer;
  sExt: string;
begin                               
  Result := False;

  // Is it a file type with a changeable icon?
  if IEFilenameInExtensions( sFilename, File_Extensions_With_Custom_Icons ) then
    exit;

  sExt := GetCacheName(aType, sFilename, bIsFolder);
  Idx := LookUpExt(sExt);
  if Idx >= 0 then
  begin
    if bWantCopy then
      Dest. Assign( TIECachedIcon(fDataList[Idx]).Bmp )
    else
      Dest := TIECachedIcon(fDataList[Idx]).Bmp;
    Result := True;
    
    // Promote the item
    if Idx <> 0 then
      fDataList.Move(Idx, 0);
  end;  
end;

procedure TIECachedIconList.SaveToCache(Image : TIEBitmap; aType : TCachedIconType; const sFilename : string; bIsFolder : Boolean);
var
  aItem : TIECachedIcon;
  Idx: Integer;
  sExt: string;
begin
  sExt := GetCacheName(aType, sFilename, bIsFolder);
  if (sExt = '') or (Image.Width < 2) then
    exit;

  Idx := LookUpExt(sExt);
  if Idx = -1 then
  begin  
    aItem := TIECachedIcon.Create(Image, sExt);
    fDataList.Insert(0, aItem);
  end
  else
  begin
    // Promote the item
    if Idx <> 0 then
      fDataList.Move(Idx, 0);
  end;

  if (fMaxIconCount > 0) and (fDataList.Count > fMaxIconCount) then
    ClearLastItem();
end;

procedure TImageEnMView.WMEnabled(var Msg: TMessage);
begin
  Invalidate;
  inherited;
end;

procedure TImageEnMView.WMGestureNotify(var Msg: TIEWMGestureNotify);
var
  c: integer;
  gc: array of TIEGESTURECONFIG;
begin
  inherited;
  if fGestures.Enabled and IEHasGestures() then
  begin
    IEUnregisterTouchWindow(Handle);
    if fGestures.fPan.Enabled then
    begin
      c := length(gc);
      SetLength(gc, c + 1);
      gc[c].dwID    := IEGID_PAN;
      gc[c].dwWant  := IEGC_PAN or IEGC_PAN_WITH_SINGLE_FINGER_VERTICALLY or IEGC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY or IEGC_PAN_WITH_INERTIA;
      gc[c].dwBlock := IEGC_PAN_WITH_GUTTER;
    end;
    if fGestures.fZoom.Enabled then
    begin
      c := length(gc);
      SetLength(gc, c + 1);
      gc[c].dwID    := IEGID_ZOOM;
      gc[c].dwWant  := IEGC_ALLGESTURES;
      gc[c].dwBlock := 0;
    end;
    IESetGestureConfig(Handle, 0, length(gc), @gc[0], sizeof(TIEGESTURECONFIG));
  end;
end;


procedure TImageEnMView.DoImageEnGestureEvent(const GInfo: TIEGESTUREINFO; var Handled: boolean);
var
  Flags: TIEGestureFlags;
  ID: TIEGestureID;
begin
  if assigned(fOnImageEnGesture) then
  begin
    Flags := [];
    if (IEGF_BEGIN and GInfo.dwFlags) <> 0 then
      Flags := Flags + [iegfBegin];
    if (IEGF_INERTIA and GInfo.dwFlags) <> 0 then
      Flags := Flags + [iegfInertia];
    if (IEGF_END and GInfo.dwFlags) <> 0 then
      Flags := Flags + [iegfEnd];
    ID := TIEGestureID(GInfo.dwID - 1);
    fOnImageEnGesture(self, Flags, ID, GInfo.ptsLocation,
                      GInfo.ullArguments and $FFFFFFFF,
                      (GInfo.ullArguments shr 32) and $FFFF,
                      (GInfo.ullArguments shr 48) and $FFFF,
                      (GInfo.ullArguments shr 32),
                      Handled);
  end;
end;


function TImageEnMView.PerformZoomSnap(Value: double): double;
begin
  result := Value;
  if fGestures.fZoom.SnapValues then
  begin
    if abs(result - 100.0) <= fGestures.fZoom.SnapDelta then
      result := 100;
  end;
end;


procedure TImageEnMView.WMGesture(var Msg: TMessage);
var
  gInfo: TIEGESTUREINFO;
  value: integer;
  v: double;
  isBegin: boolean;
  isInertia: boolean;
  handled: boolean;
begin
  Msg.Result := 1;  // not handled
  if fGestures.Enabled and IEHasGestures() then
  begin
    FillChar(gInfo, sizeof(gInfo), 0);
    gInfo.cbSize := sizeof(gInfo);
    if IEGetGestureInfo(Msg.LParam, @gInfo) then
    begin
      try
        handled := false;
        DoImageEnGestureEvent(gInfo, handled);

        if not handled then
        begin

          value     := gInfo.ullArguments and $FFFFFFFF;
          isBegin   := (IEGF_BEGIN and gInfo.dwFlags) <> 0;
          isInertia := (IEGF_INERTIA and gInfo.dwFlags) <> 0;

          case gInfo.dwID of
            IEGID_ZOOM:
              begin
                // begin zoom
                if isBegin then
                begin
                  fGestureStartValue := value;
                  fGestureBaseZoom   := Zoom;
                end
                // perform zoom
                else
                if fGestures.fZoom.Enabled and (not isInertia or fGestures.fZoom.Inertia) then
                begin
                  v := fGestureBaseZoom * (value / fGestureStartValue);
                  v := dmax(v, fGestures.fZoom.Min);
                  v := dmin(v, fGestures.fZoom.Max);
                  v := v * fGestures.fZoom.Multiplier;
                  Zoom := PerformZoomSnap(v);
                  Msg.Result := 0;  // handled
                end
              end;

            IEGID_PAN:
              begin
                // begin pan
                if isBegin then
                begin
                  fGestureStartX := gInfo.ptsLocation.x;
                  fGestureStartY := gInfo.ptsLocation.y;
                  fGestureBaseViewX := ViewX;
                  fGestureBaseViewY := ViewY;
                end
                // perform pan
                else

                if fGestures.fPan.Enabled and (not isInertia or fGestures.fPan.Inertia) then
                begin
                  v := fGestureBaseViewX + (fGestureStartX - gInfo.ptsLocation.x);
                  v := dmax(v, fGestures.fPan.BoundingBox.Left);
                  v := dmin(v, fGestures.fPan.BoundingBox.Right);
                  v := v * fGestures.fPan.Multiplier;
                  ViewX := trunc(v);

                  v := fGestureBaseViewY + (fGestureStartY - gInfo.ptsLocation.y);
                  v := dmax(v, fGestures.fPan.BoundingBox.Top);
                  v := dmin(v, fGestures.fPan.BoundingBox.Bottom);
                  v := v * fGestures.fPan.Multiplier;
                  ViewY := trunc(v);

                  Msg.Result := 0;  // handled
                end;
              end;

          end;
        end
        else
        begin
          // handled by user (in OnImageEnGesture event)
          Msg.Result := 0;  // handled
        end;

      finally
        if Msg.Result = 0 then
          IECloseGestureInfoHandle(Msg.LParam);
      end;
    end
  end;
  if Msg.Result <> 0 then
    inherited;
end;


constructor TIEMViewerGestures.Create();
begin
  inherited;
  fPan  := TIEGesturePanOptions.Create();
  fZoom := TIEGestureZoomOptions.Create();

  // Pan defaults
  fPan.Enabled := false;
  fPan.Inertia := true;
  fPan.BoundingBox := Rect(Low(integer), Low(integer), High(integer), High(integer));
  fPan.Multiplier := 1.0;
  fPan.SnapValues := false;
  fPan.SnapDelta := 0.0;

  // Zoom defaults
  fZoom.Enabled := false;
  fZoom.Inertia := true;
  fZoom.Min := 1.0;
  fZoom.Max := 8000.0;
  fZoom.Multiplier := 1.0;
  fZoom.SnapValues := true;
  fZoom.SnapDelta := 5.0;
end;


destructor TIEMViewerGestures.Destroy();
begin
  fPan.Free();
  fZoom.Free();
  inherited;
end;


function TIEMViewerGestures.GetEnabled(): boolean;
begin
  result := fPan.Enabled or fZoom.Enabled;
end;





{$ELSE} // {$ifdef IEINCLUDEMULTIVIEW}

interface
implementation

{$ENDIF}


end.


