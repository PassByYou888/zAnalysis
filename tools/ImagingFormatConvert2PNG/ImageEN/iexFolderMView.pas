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
KNOWN ISSUES:
- Link overlay does not appear for shortcut files
*)




(*
File version 1008
Doc revision 1004
*)

unit iexFolderMView;

{$R-}
{$Q-}

{$I ie.inc}


{$IFDEF IEINCLUDEMULTIVIEW}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, stdctrls, dialogs, 
  {$ifdef IEHASTYPES} Types, {$endif}
  {$ifdef IEHASUITYPES} System.UITypes, {$endif}
  ieview, hyieutils, hyiedefs, ImageEnView, iemview, ieanimation, ImageEnIO, iemio, ImageEnProc,
  ShlObj, ActiveX, iexWindowsFunctions;


const
  IEF_CURRENT_FILE = -999;

  IEF_Desktop_Folder      = 'IEF_DESKTOP_FOLDER';
  IEF_Root_Directory      = 'IEF_ROOT_DIRECTORY';
  IEF_MyDocuments_Folder  = 'IEF_MYDOCUMENTS_FOLDER';
  IEF_MyPictures_Folder   = 'IEF_MYPICTURES_FOLDER';
  IEF_MyVideos_Folder     = 'IEF_MYVIDEOS_FOLDER';


type  

{!!
<FS>TIEFolderFileTypes

<FM>Declaration<FC>
TIEFolderFileTypes = (iefAllImages, iefAllImagesAndVideos, iefAllFiles, iefCustom);

<FM>Description<FN>
Specifies which files types to retrieve
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iefAllImages</C> <C>Retrieves all supported image types of ImageEn</C> </R>
<R> <C>iefAllImagesAndVideos</C> <C>Retrieves all supported image and video types of ImageEn</C> </R>
<R> <C>iefAllFiles</C> <C>Retrieves all files</C> </R>
<R> <C>iefCustom</C> <C>The file types to retrieve are specified by <A TImageEnFolderMView.FileTypesMask></C> </R>
</TABLE>

!!}
  TIEFolderFileTypes = (iefAllImages, iefAllImagesAndVideos, iefAllFiles, iefCustom);


{!!
<FS>TIEFileOperationOptions

<FM>Declaration<FC>
TIEFileOperationOptions = set of (ieioShowConfirmation, ieioShowProgress, ieioVerboseErrors, ieioSendToRecycleBin, ieioRenameOnCollision);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieioShowConfirmation</C> <C>A confirmation dialog is displayed to confirm file operations. Otherwise it automatically responds with "Yes to All" for any dialog box</C> </R>
<R> <C>ieioShowProgress</C> <C>Displays the standard Windows progress dialog. Otherwise it is silent</C> </R>
<R> <C>ieioVerboseErrors</C> <C>Displays an error message if a failure occurs. Otherwise failures are not reported</C> </R>
<R> <C>ieioSendToRecycleBin</C> <C>Files are moved to the Recycle Bin rather than deleted</C> </R>
<R> <C>ieioRenameOnCollision</C> <C>If a file already exists in the destination folder when moving or copying a new name will be given to the current file</C> </R>
</TABLE>

Default: [ieioShowConfirmation, ieioShowProgress, ieioVerboseErrors, ieioSendToRecycleBin]

!!}
  TIEFileOperationOptions = set of (ieioShowConfirmation, ieioShowProgress, ieioVerboseErrors, ieioSendToRecycleBin, ieioRenameOnCollision);
   

{!!
<FS>TIEFolderInteractOptions

<FM>Declaration<FC>
TIEFolderInteractOptions = set of (ieboOpenFoldersOnDblClick, ieboLaunchImagesOnDblClick, ieboLaunchFilesOnDblClick, ieboEnableFolderShortcuts, ieboEnableFileShortcuts);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieboOpenFoldersOnDblClick</C> <C>If <A TImageEnFolderMView.ShowFolders> has been enabled to show folders, then ieboOpenFoldersOnDblClick allows the user to double click or press enter to open a folder</C> </R>
<R> <C>ieboLaunchImagesOnDblClick</C> <C>If a user double clicks a supported image type (or selects one and clicks the enter key) then it will be launched to the default windows handle for that file type</C> </R>
<R> <C>ieboLaunchFilesOnDblClick</C> <C>If a user double clicks an unknown file type (or selects one and clicks the enter key) then it will be launched to the default windows handle for that file type</C> </R>
<R> <C>ieboEnableFolderShortcuts</C> <C>If <A TImageEnFolderMView.ShowFolders> has been enabled to show folders, then Backspace or Alt+Up will open the parent folder. F5 will refresh the file listing</C> </R>
<R> <C>ieboEnableFileShortcuts</C> <C>Clicking Ctrl+X/C/V will Cut, Copy or Paste files to/from Windows Explorer. Clicking Delete will move files to the Recycle Bin. Shift+Delete will permanently delete files</C> </R>
</TABLE>

Default: [ieboOpenFoldersOnDblClick, ieboEnableFolderShortcuts]

!!}
  TIEFolderInteractOptions = set of (ieboOpenFoldersOnDblClick, ieboLaunchImagesOnDblClick, ieboLaunchFilesOnDblClick, ieboEnableFolderShortcuts, ieboEnableFileShortcuts);


{!!
<FS>TIEFolderChangeEvent

<FM>Declaration<FC>
TIEFolderChangeEvent = procedure(Sender: TObject; var sNewFolder: String; var bAllow: boolean) of object;

<FM>Description<FN>
Used by <A TImageEnFolderMView.OnFolderChange> whenever the value of <A TImageEnFolderMView.Folder> changes.

<FC>sNewFolder<FN> specifies the new folder. Set bAllow to false to prevent the folder being opened.
!!}
  TIEFolderChangeEvent = procedure(Sender: TObject; var sNewFolder: String; var bAllow: boolean) of object;


{!!
<FS>TIEDefaultFolder

<FM>Declaration<FC>
TIEDefaultFolder = (iedfNone, iedfDesktop, iedfRootDir, iedfMyDocuments, iedfMyPictures, iedfMyVideos, iedfSpecified);
   
<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>      
<R> <C>iedfNone</C> <C>The control is blank on start-up</C> </R>
<R> <C>iedfDesktop</C> <C>The Windows Desktop is shown</C> </R>
<R> <C>iedfRootDir</C> <C>The drive containing Windows is shown (usually C:\)</C> </R>
<R> <C>iedfMyDocuments</C> <C>The user's "Documents" folder is shown</C> </R>
<R> <C>iedfMyPictures</C> <C>The user's "Pictures" folder is shown</C> </R>
<R> <C>iedfMyVideos</C> <C>The user's "Videos" folder is shown</C> </R>    
<R> <C>iedfSpecified</C> <C>The path specified in <A TImageEnFolderMView.Folder> is shown</C> </R>
</TABLE>
!!}
  TIEDefaultFolder = (iedfNone, iedfDesktop, iedfRootDir, iedfMyDocuments, iedfMyPictures, iedfMyVideos, iedfSpecified);


{!!
<FS>TImageEnFolderMView

<FM>Description<FN>
The TImageEnFolderMView component is a descendent of <A TImageEnMView>, but is designed specifically for displaying the content of file folders.

While much of TImageEnFolderMView's functionality can be replicated in TImageEnMView using <A TImageEnMView.FillFromDirectory>, there are a number of features that are only available in this control:
- <L TImageEnFolderMView.AutoDragFiles>Dragging</L> and <L TImageEnFolderMView.AutoDropFiles>dropping</L> files from Windows
- <L TImageEnFolderMView.CutSelectedFilesToClipboard>Cut</L>, <L TImageEnFolderMView.CopySelectedFilesToClipboard>copy</L> and <L TImageEnFolderMView.CanPasteFilesFromClipboard>paste</L> files from Windows Explorer
- Display file information when <L TImageEnFolderMView.ShowThumbnailHint>hovering over frames</L>
- <L TImageEnFolderMView.AutoRefresh>Automatic refreshing</L> when folder contents change
- Display of the <L TImageEnFolderMView.PopupMenuUseSystem>standard folder popup menu</L>
- File functions to move, copy, rename and delete files
- In-built file navigation, file shortcuts and <L TImageEnFolderMView.FolderInteract>other functions</L>

To use TImageEnFolderMView, specify a <A TImageEnFolderMView.Folder> and the <L TImageEnFolderMView.FileTypes>file types</L> that you wish to display. For more control you can specify your own <L TImageEnFolderMView.FileTypesMask>file mask</L>, <L TImageEnFolderMView.ExclusionMask>exclusion mask</L> and <L TImageEnFolderMView.SortOrder>sort order</L>.


For rapid UI development TImageEnFolderMView provides a set of <L TImageEnFolderMView Actions>actions</L>, plus you can use all <L TImageEnMView Actions>actions of TImageEnMView</L>.


<IMG help_images\IEFolderMView_Component.gif>

<FM>Example<FC>
// Set up
with IEFolderMView1 do
begin
  BorderStyle  := bsNone;      // Normally don't require a 3D border

  ThumbWidth   := 140;         // Choose a good size for your thumbnails
  ThumbHeight  := 150;

  // Default is rfFastLinear for speed, but rfLanczos3 will give much better quality
  ThumbnailDisplayFilter := rfLanczos3;
end;

// Now display the images of a folder in the grid
IEFolderMView1.Folder := 'C:\MyImages\';       
IEFolderMView1.FileTypes := iefAllImages;   // Images only
IEFolderMView1.SortOrder := iesbImageSize;  // Sort by image dimensions

<FM>Methods and Properties<FN>
<FI>Folder Display<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnFolderMView.AutoRefresh> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.DefaultFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnFolderMView.DetectFileFormat> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.ExclusionMask> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnFolderMView.FileLimit> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.FileTypes> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.FileTypesMask> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.Folder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.PopupMenuUseSystem> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.PromptForFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.RefreshFileList> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.RefreshSorting> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.SetFolderEx> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.SetSortOrderEx> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.SortAscending> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.SortCaseSensitive> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.SortOrder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.ShowFolders> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.ShowHiddenFiles> <IMG help_images\Star.gif></C> </R>
</TABLE>

<FI>File Operations<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.AutoDragFiles> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.AutoDropFiles> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.CreateNewFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.ExecuteFile> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.CopyFilesToCurrentFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.CopySelectedFilesToClipboard> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.CopySelectedFilesToFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.CutSelectedFilesToClipboard> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.DeleteSelectedFilesFromFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.FileOperationOptions> <IMG help_images\Star.gif>    </C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.FolderInteract> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.MoveFilesToCurrentFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.MoveSelectedFilesToFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.OpenFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.OpenParentFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.CanOpenParentFolder> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.PasteFilesFromClipboard> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.CanPasteFilesFromClipboard> <IMG help_images\Star.gif></C> </R>  
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.PopupSystemMenu> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.RenameFile> <IMG help_images\Star.gif></C> </R>
</TABLE>

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
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.LockUpdate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnFolderMView.LockUpdateCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MaximumViewX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MaximumViewY></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetChessboardStyle></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetPresetThumbnailFrame>  </C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetModernStyling></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SetViewXY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SoftShadow></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.Style></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UnLockPaint></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.UnLockUpdate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ViewX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ViewY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.VisibleFrame></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.WallPaper></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.WallPaperStyle></C> </R>
</TABLE>

<FI>Frame Editing<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.AppendFile> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.AppendImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.AppendSplit></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.Clear></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.CreateMorphingSequence></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.DeleteImage>      </C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageCount></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.InsertFile> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.InsertImageEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.InsertImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.InsertTransitionFrames></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.InsertTransitionFramesEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.MoveImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.RemoveBlankPages></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.RemoveFile> <IMG help_images\Star.gif></C> </R>
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
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.FilenameToIndex> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageBitCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageCol></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageCreateDate>   </C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageEditDate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageFileName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageFileSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnFolderMView.ImageFileType> <IMG help_images\Star.gif></C> </R>
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
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.DefaultBottomText> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.DefaultBottomTextFont></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.DefaultInfoText> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.DefaultInfoTextFont></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.DefaultTopText> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.DefaultTopTextFont></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageBottomText></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageInfoText></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.ImageTopText></C> </R>
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
<R> <C_IMG_PUBLISHED> <C><A TImageEnFolderMView.ShowThumbnailHint> <IMG help_images\Star.gif></C> </R>
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
</TABLE>

<FI>Input/Output<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.EnableAdjustOrientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.EnableImageCaching></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.EnableLoadEXIFThumbnails></C> </R>
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
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MultiSelectedimagesCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.MultiSelecting></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.MultiSelectionOptions></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.MultiSelectSortList></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SelectAll></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectedImage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectedImageAlwaysVisible></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnFolderMView.SelectedFilename> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.SelectedFilenames> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectionAntialiased></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.SelectionColor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.SelectionWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.SelectionWidthNoFocus></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.SelectSeek></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnMView.TrackMouseSelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UnSelectImage></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnMView.VisibleSelection></C> </R>
</TABLE>

<FI>User Interaction<FN>
<TABLE2>
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
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.FillFromDirectory></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetLastOp></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.GetLastOpIdx>   </C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnFolderMView.Sort></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnMView.UpdateCoords></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnAfterEvent></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnAllDisplayed></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnAnimationText></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnBeforeImageDrawEx></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnBeforeImageDraw></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnCreateImage></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnFolderMView.OnCustomSortCompare> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnDestroyImage></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnDrawProgress></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnFinishWork></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnFolderMView.OnFolderChange> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnGetText></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageAdd></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageAtPos></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageDeselect></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageDraw></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageDraw2></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageIDRequestEx></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnMView.OnImageIDRequest></C> </R>
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


<IMG help_images\Star.gif> Unique to TImageEnFolderMView

!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TImageEnFolderMView = class(TImageEnMView)
  private
    /////////////////////////
    // P R I V A T E           

    fSortOrder : TIEImageEnMViewSortBy;
    fSortAscending : Boolean;
    fSortCaseSensitive : Boolean;
    fFileTypes : TIEFolderFileTypes;
    fPopupMenuUseSystem : Boolean;
    fFileLimit : Integer;
    fFileTypesMask : string;
    fFolder : string;
    fExclusionMask : string;
    fAutoRefresh : Boolean;
    fNeedRefreshFileList : Boolean;
    fNeedRefreshSorting : Boolean;
    fDefaultTopText : TIEImageEnMViewDefaultText;
    fDefaultInfoText : TIEImageEnMViewDefaultText;
    fDefaultBottomText : TIEImageEnMViewDefaultText;
    fInitialized : Boolean; // True after the window is created and we perform our first fill
    fShowHiddenFiles : Boolean;
    fShowFolders : Boolean;
    fDetectFileFormat : Boolean;
    fLoadOnDemand : Boolean;
    fClickedFrameIndex : Integer; // The index of hte frame that was last clicked
    fOnFolderChange : TIEFolderChangeEvent;
    fAutoDragFiles  : TIEFileDragDropActions;
    fOnCustomSortCompare : TIEImageEnMViewSortCompareEx;
    fAutoDropFiles  : TIEFileDragDropActions;
    fDragDrop       : TIEFileDragDrop;
    fMouseClickPos  : TPoint;   
    fFolderMonitor  : TIEFolderWatch;
    fDefaultFolder  : TIEDefaultFolder;
    fFolderInteract : TIEFolderInteractOptions;
    fFileOperationOptions : TIEFileOperationOptions;
    fCurrentFolderIndex : Integer;
    
    procedure SetExclusionMask(const Value: string);
    procedure SetFileTypes(const Value: TIEFolderFileTypes);
    procedure SetFileTypesMask(const Value: string);
    procedure SetFolder(const Value: string);  
    procedure _SetFolder(const Value: string; bIsFirstCall : Boolean);
    function GetFolder : string;
    procedure SetSortAscending(const Value: Boolean);
    procedure SetSortCaseSensitive(const Value: Boolean);
    procedure SetAutoRefresh(const Value: Boolean);
    procedure SetSortOrder(const Value: TIEImageEnMViewSortBy);
    procedure CheckForPendingRefresh;
    function GetSelectedFilename: WideString;
    procedure SetSelectedFilename(const Value: WideString);
    procedure RefreshFileListEx(bNewFolder : Boolean);
    procedure SetFolderInteract(const Value: TIEFolderInteractOptions);
    procedure SetFileOperationOptions(const Value: TIEFileOperationOptions);
    procedure SetShowThumbnailHint(const Value: Boolean);
    function GetImageFileType(idx: integer): WideString;
    function GetImageType(idx: integer): TIEFolderImageType;
    function GetShowThumbnailHint: Boolean;  
    procedure HandleDblClick(idx : Integer);
    procedure SetAutoDragFiles(const Value: TIEFileDragDropActions);
    procedure SetAutoDropFiles(const Value: TIEFileDragDropActions);    
    procedure DropFiles(Sender: TObject; ssFiles : TStrings; dwEffect: Integer);    
    procedure StartMonitoring;
    procedure StopMonitoring;
    procedure OnFolderMonitorNotify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
    procedure CheckIndexForConst(var idx : Integer);
    function DeleteSelectedFilesFromFolderEx(bForcePermanentDelete : Boolean) : Boolean;
    procedure SetShowFolders(const Value: Boolean);
    procedure SetShowHiddenFiles(const Value: Boolean);
    procedure SetDefaultFolder(const Value: TIEDefaultFolder);
    procedure _GetFillFromDirectoryParams(out bAllowUnknownFormats : Boolean; out bIncludeVideoFiles : Boolean; out sFilterMask : string);  
    procedure CheckIfFilesInFolder(ssFilenames : TStrings);
    procedure CheckIfFilesDeletedFromFolder(ssFilenames : TStrings);  
    procedure SetLoadSnapshotParameters(Sender: TObject; Stream: TStream; Version : Byte);
    procedure GetSaveSnapshotParameters(Sender: TObject; Stream: TStream; Version : Byte);
    function GetFirstFolder : string;
    function GetNextFolder : string;

  protected
    ///////////////////////
    // P R O T E C T E D
    //            
    procedure CreateWnd; override;

  public
    /////////////////////
    // P U B L I C
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    
    procedure DblClick; override;                      
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;  
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    // display
    procedure LockUpdate; override;
    function UnLockUpdate: Integer; override;
    
    function AppendFile(const sFilename : string; bSelectIt : Boolean = True; bCheckFileType : Boolean = True) : integer;
    function InsertFile(idx : Integer; const sFilename : string; bSelectIt : Boolean = True; bCheckFileType : Boolean = True) : Boolean;
    procedure RemoveFile(const sFilename : string);

    property SelectedFilename : WideString read GetSelectedFilename write SetSelectedFilename;
    function SelectedFilenames(Dest : TStrings): Integer;

    property ImageType[idx: integer]: TIEFolderImageType read GetImageType;

    property AutoRefresh : Boolean read fAutoRefresh write SetAutoRefresh;


    function MoveSelectedFilesToFolder(const sDestFolder : string) : Boolean;
    function CopySelectedFilesToFolder(const sDestFolder : string) : Boolean;
    function MoveFilesToCurrentFolder(ssFiles : TStrings) : Boolean;
    function CopyFilesToCurrentFolder(ssFiles : TStrings) : Boolean;
    function DeleteSelectedFilesFromFolder : Boolean;
    function CopySelectedFilesToClipboard : Integer;
    function CutSelectedFilesToClipboard : Integer;
    function PasteFilesFromClipboard : Boolean;
    function CanPasteFilesFromClipboard : Boolean;

    procedure PopupSystemMenu(X, Y: Integer);

{!!
<FS>TImageEnFolderMView.ImageFileType

<FM>Declaration<FC>
property ImageFileType[idx: Integer]: WideString;

<FM>Description<FN>
Returns the Window's file type for the file at index, idx.

!!}
    property ImageFileType[idx: integer]: WideString read GetImageFileType;

{!!
<FS>TImageEnFolderMView.FileLimit

<FM>Declaration<FC>
property FileLimit: Integer;

<FM>Description<FN>
Specifies the maximum number of images to retrieve from a folder. If this is -1 (default) then all images are retrieved.

Note: Changing this property will not affect the current content. The change will occur next time the <L TImageEnFolderMView.Folder>folder</L> is refreshed.
!!}
    property FileLimit: Integer read fFileLimit write fFileLimit;

{!!
<FS>TImageEnFolderMView.DetectFileFormat

<FM>Declaration<FC>
property DetectFileFormat : Boolean; (Default: False)

<FM>Description<FN>
If enabled, rather than using the file extension (e.g. ".JPEG") to determine the file format, the component will read the file header (which will be much slower)

Note: Changing this property will not affect the current content. The change will occur next time the <L TImageEnFolderMView.Folder>folder</L> is refreshed.
!!}
    property DetectFileFormat : Boolean read fDetectFileFormat write fDetectFileFormat;



{!!
<FS>TImageEnFolderMView.LoadOnDemand

<FM>Declaration<FC>
property LoadOnDemand : Boolean; (Default: True)

<FM>Description<FN>
If enabled, images are only loaded as they are displayed (i.e. not until they are scrolled into view). Set to false to load all images immediately (which will be much slower)

Note: Changing this property will not affect the current content. The change will occur next time the <L TImageEnFolderMView.Folder>folder</L> is refreshed.
!!}
    // Note: Not exposed in unit help index
    property LoadOnDemand: Boolean read fLoadOnDemand write fLoadOnDemand;

    procedure RefreshFileList;
    procedure RefreshSorting;

    procedure SetFolderEx(const sFolder: string; xFileTypes: TIEFolderFileTypes; const sFileTypesMask: string = ''; const sExclusionMask: string = '');
    procedure SetSortOrderEx(xSortOrder: TIEImageEnMViewSortBy; bSortAscending: Boolean = True; bSortCaseSensitive: Boolean = False);

    function OpenFolder(idx : Integer = IEF_CURRENT_FILE) : boolean;
    function OpenParentFolder : boolean;
    function CanOpenParentFolder : boolean;
    function ExecuteFile(idx : Integer = IEF_CURRENT_FILE) : boolean;
    function RenameFile(idx : Integer; const sNewName : string): boolean;
    function PromptForFolder : boolean;
    function CreateNewFolder(const sFolderName : string) : Boolean;

    procedure SaveSnapshot(Stream: TStream; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False); overload; override;
    procedure SaveSnapshot(FileName: WideString; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False); overload; override;
    function LoadSnapshot(Stream: TStream): Boolean; overload; override;
    function LoadSnapshot(FileName: WideString): Boolean; overload; override;

  published
    ///////////////////////
    // P U B L I S H E D

    property SortOrder: TIEImageEnMViewSortBy read fSortOrder write SetSortOrder default iesbNone;
    property SortAscending: Boolean read fSortAscending write SetSortAscending default True;
    property SortCaseSensitive: Boolean read fSortCaseSensitive write SetSortCaseSensitive default False;
    property FileTypes: TIEFolderFileTypes read fFileTypes write SetFileTypes default iefAllImages;
    property FileTypesMask: string read fFileTypesMask write SetFileTypesMask;
    property Folder : string read GetFolder write SetFolder;
    property ExclusionMask: string read fExclusionMask write SetExclusionMask;
    property FolderInteract: TIEFolderInteractOptions read fFolderInteract write SetFolderInteract default [ieboOpenFoldersOnDblClick, ieboEnableFolderShortcuts];
    property FileOperationOptions: TIEFileOperationOptions read fFileOperationOptions write SetFileOperationOptions default [ieioShowConfirmation, ieioShowProgress, ieioVerboseErrors, ieioSendToRecycleBin];
    property ShowThumbnailHint: Boolean read GetShowThumbnailHint write SetShowThumbnailHint default False;
    property AutoDragFiles  : TIEFileDragDropActions read fAutoDragFiles write SetAutoDragFiles;
    property AutoDropFiles  : TIEFileDragDropActions read fAutoDropFiles write SetAutoDropFiles;
                            
    property ShowHiddenFiles: Boolean read fShowHiddenFiles write SetShowHiddenFiles default False;
    property ShowFolders: Boolean read fShowFolders write SetShowFolders default True;


{!!
<FS>TImageEnFolderMView.DefaultTopText

<FM>Declaration<FC>
property DefaultTopText: <A TIEImageEnMViewDefaultText>;

<FM>Description<FN>
Specifies a default value for <A TImageEnMView.ImageTopText>, which will appear above the thumbnail.

Default is iedtNone.

Note: Changing this property will not affect the current content. The change will occur next time the <L TImageEnFolderMView.Folder>folder</L> is refreshed.

<FM>Example<FC>
// Display the filename and details for each image in C:\Images
IEFolderMView1.DefaultImageTopText := iedtFilename;
IEFolderMView1.DefaultImageInfoText := iedtImageDimensions;
IEFolderMView1.DefaultImageBottomText := iedtFileEditDateAndSize;
IEFolderMView1.RefreshFileList;
!!}
    property DefaultTopText : TIEImageEnMViewDefaultText read fDefaultTopText write fDefaultTopText Default iedtNone;

{!!
<FS>TImageEnFolderMView.DefaultInfoText

<FM>Declaration<FC>
property DefaultInfoText: <A TIEImageEnMViewDefaultText>;

<FM>Description<FN>
Specifies a default value for <A TImageEnMView.ImageInfoText>, which will appear directly below the thumbnail.

Default is iedtNone.

Note: Changing this property will not affect the current content. The change will occur next time the <L TImageEnFolderMView.Folder>folder</L> is refreshed.

<FM>Example<FC>
// Display the filename and details for each image in C:\Images
IEFolderMView1.DefaultImageTopText := iedtFilename;
IEFolderMView1.DefaultImageInfoText := iedtImageDimensions;
IEFolderMView1.DefaultImageBottomText := iedtFileEditDateAndSize;
IEFolderMView1.RefreshFileList;
!!}
    property DefaultInfoText : TIEImageEnMViewDefaultText read fDefaultInfoText write fDefaultInfoText Default iedtNone;

{!!
<FS>TImageEnFolderMView.DefaultBottomText

<FM>Declaration<FC>
property DefaultBottomText: <A TIEImageEnMViewDefaultText>;

<FM>Description<FN>
Specifies a default value for <A TImageEnMView.ImageBottomText>, which will appear below the thumbnail.

Default is iedtFilename.

Note: Changing this property will not affect the current content. The change will occur next time the <L TImageEnFolderMView.Folder>folder</L> is refreshed.

<FM>Example<FC>
// Display the filename and details for each image in C:\Images
IEFolderMView1.DefaultImageTopText := iedtFilename;
IEFolderMView1.DefaultImageInfoText := iedtImageDimensions;
IEFolderMView1.DefaultImageBottomText := iedtFileEditDateAndSize;
IEFolderMView1.RefreshFileList;
!!}
    property DefaultBottomText : TIEImageEnMViewDefaultText read fDefaultBottomText write fDefaultBottomText Default iedtFilename;

{!!
<FS>TImageEnFolderMView.PopupMenuUseSystem

<FM>Declaration<FC>
property PopupMenuUseSystem: Boolean;

<FM>Description<FN>
If enabled (Default) and a PopUpMenu has not been specified then a Windows Explorer popup menu will be shown when right-clicking a file.
                        
Note: If <A TImageEnFolderMView.AutoRefresh> is not enabled, the control may not update the display of files that are moved or renamed

<FM>See Also<FN>
- <A TImageEnFolderMView.PopupSystemMenu>

!!}
    property PopupMenuUseSystem: Boolean read fPopupMenuUseSystem write fPopupMenuUseSystem default True;

    property DefaultFolder: TIEDefaultFolder read fDefaultFolder write SetDefaultFolder default iedfNone;

{!!
<FS>TImageEnFolderMView.OnFolderChange

<FM>Declaration<FC>
property OnFolderChange: <A TIEFolderChangeEvent>;

<FM>Description<FN>
Occurs whenever the value of <A TImageEnFolderMView.Folder> is being changed.
!!}
    property OnFolderChange : TIEFolderChangeEvent read fOnFolderChange write fOnFolderChange;


{!!
<FS>TImageEnFolderMView.OnCustomSortCompare

<FM>Declaration<FC>
property OnCustomSortCompare: <A TIEImageEnMViewSortCompareEx>;

<FM>Description<FN>
Occurs whenever the content needs to be sorted if <A TImageEnFolderMView.SortOrder> is set to iesbCustom.    

<FM>Example<FC>
// custom sort function (by DPI)
function TMyForm.IEFolderMView1CustomSortCompare(i1, i2: Integer): Integer;
var
  s1, s2: Integer;
begin
  s1 := IEFolderMView1.Params[i1].DPI;
  s2 := IEFolderMView1.Params[i2].DPI;

  if s1 < s2 then
    result := -1
  else
  if s1 > s2 then
    result := 1
  else
    result := 0;
end;

// Sort By File Size
procedure TForm1.btnCustomSortClick(Sender: TObject);
begin
  IEFolderMView1.SortOrder := iesbCustom;
end;


!!}
    property OnCustomSortCompare : TIEImageEnMViewSortCompareEx read fOnCustomSortCompare write fOnCustomSortCompare;



    function FilenameToIndex(const sFilename : string) : integer;
  end;

const
  IEM_Folder_Delimiter   = '|'; // Delimits multiple folders

implementation

uses
  iesettings, ComObj, ShellApi;

{$R-}

constructor TImageEnFolderMView.Create(Owner: TComponent);
begin
  inherited;
  fInitialized := False;

  fDragDrop := TIEFileDragDrop.Create(Self, DropFiles);  

  // CHANGED FROM TImageEnMView behaviour
  StoreType               := ietFastThumb;
  ImageCacheSize          := 100;
  LookAhead               := 20;
  TrackMouseSelection     := True;    
  MultiSelectionOptions := [iemoRegion, iemoSelectOnRightClick, iemoOptimizeForDragging];
  SetModernStyling(True, 130, 130);

  // Defaults
  fSortOrder            := iesbNone;
  fSortAscending        := True;
  fSortCaseSensitive    := False;
  fFileTypes            := iefAllImages;
  fPopupMenuUseSystem   := True;
  fFileLimit            := -1;
  fShowHiddenFiles      := False;
  fShowFolders          := True;
  fDetectFileFormat     := False;
  fLoadOnDemand         := True;
  fFolderInteract      := [ieboOpenFoldersOnDblClick, ieboEnableFolderShortcuts];
  fFileOperationOptions := [ieioShowConfirmation, ieioShowProgress, ieioVerboseErrors, ieioSendToRecycleBin];

  fDefaultTopText       := iedtNone;
  fDefaultInfoText      := iedtNone;
  fDefaultBottomText    := iedtFilename;
                             
  fShowThumbnailHint    := False;
  fNeedRefreshFileList  := True;
  fNeedRefreshSorting   := True;
  fAutoRefresh          := False;

  fAutoDragFiles        := [];
  fAutoDropFiles        := [];

  fDefaultFolder        := iedfNone;
end;



function DefaultFolderToStr(const Value: TIEDefaultFolder; const sDefaultFolder : string) : string;
begin
  Result := '';
  if Result = '' then
    case Value of
      iedfDesktop     : Result := IEF_Desktop_Folder;
      iedfRootDir     : Result := IEF_Root_Directory;
      iedfMyDocuments : Result := IEF_MyDocuments_Folder;
      iedfMyPictures  : Result := IEF_MyPictures_Folder;
      iedfMyVideos    : Result := IEF_MyVideos_Folder;
      iedfSpecified   : Result := sDefaultFolder;
    end;
end;


procedure TImageEnFolderMView.CreateWnd;
begin
  inherited;
  if fInitialized = False then
  begin
    fInitialized := True;
    _SetFolder(DefaultFolderToStr(fDefaultFolder, fFolder), True);
    if fAutoDropFiles <> [] then
      fDragDrop.ActivateDropping := True;
  end;
end;

procedure TImageEnFolderMView.DblClick;
begin
  inherited;
  HandleDblClick(fClickedFrameIndex);
end;


// Use GetFirstFolder and GetNextFolder to iterate through all the items specified in the Folder properties (as there may be multiple delimited by IEM_Folder_Delimiter)
function TImageEnFolderMView.GetFirstFolder : string;
begin
  fCurrentFolderIndex := 1;
  Result := GetNextFolder;
end;

function TImageEnFolderMView.GetNextFolder : string;

  procedure _GetNextFolder;
  var
    sPosCheck: string;
    iNextPos: Integer;
  begin
    // Because older versions of Delphi do not support PosEx()
    sPosCheck := Copy(fFolder, fCurrentFolderIndex, Length(fFolder) - fCurrentFolderIndex + 1);
    iNextPos := Pos(IEM_Folder_Delimiter, sPosCheck);
    if iNextPos = 0 then
      iNextPos := Length(sPosCheck) + 1;
    Result := Copy(fFolder, fCurrentFolderIndex, iNextPos - 1);
    fCurrentFolderIndex := fCurrentFolderIndex + iNextPos;
  end;

begin
  Result := '';
  While (Result = '') and (Length(fFolder) > fCurrentFolderIndex) do
    _GetNextFolder;
end;

    
procedure TImageEnFolderMView.HandleDblClick(idx : Integer);
begin
  if idx < 0 then
    exit;

  case ImageType[idx] of
    ieftFolder          : if ieboOpenFoldersOnDblClick in fFolderInteract then
                            OpenFolder(idx);
    ieftSupportedImage  : if ieboLaunchImagesOnDblClick in fFolderInteract then
                            ExecuteFile(idx);
    ieftFile            : if ieboLaunchFilesOnDblClick in fFolderInteract then
                            ExecuteFile(idx);
  end;
end;

destructor TImageEnFolderMView.Destroy;
begin
  fDragDrop := nil; // don't free
  FreeAndNil(fFolderMonitor);
  inherited;
end;



{!!
<FS>TImageEnFolderMView.AppendImage

Note: It is better to use <A TImageEnFolderMView.AppendFile> than <A TImageEnMView.AppendImage> as it provides better support for TImageEnFolderMView properties.

<FM>See Also<FN>
- <A TImageEnFolderMView.AppendFile>
- <A TImageEnMView.AppendImage>

!!}


{!!
<FS>TImageEnFolderMView.AppendFile

<FM>Declaration<FC>
function AppendFile(const sFilename : string; bSelectIt : Boolean = True; bCheckFileType : Boolean = True) : integer;

<FM>Description<FN>
AppendFile adds a new file at last position in the grid and returns the new image position. If <FC>bSelectIt<FN> is enabled the new file will be selected.
If <FC>bCheckFileType<FN> is enabled then the file is only added if it is permitted by folder parameters: <A TImageEnFolderMView.FileTypes>, <A TImageEnFolderMView.FileTypesMask>, <A TImageEnFolderMView.ExclusionMask>, <A TImageEnFolderMView.ShowFolders> and <A TImageEnFolderMView.ShowHiddenFiles>

Result is -1 if the file does not exist.

Note: Unlike the <A TImageEnMView.AppendImage> methods, the added file will be loaded on demand and filled with the details and properties of TImageEnFolderMView

<FM>Example<FC>
IEFolderMView1.Add('D:\MyNewImage.jpg');

!!}
function TImageEnFolderMView.AppendFile(const sFilename : string; bSelectIt : Boolean = True; bCheckFileType : Boolean = True) : integer;
begin
  result := fImageInfo.Count;
  If InsertFile(Result, sFilename, bSelectIt, bCheckFileType) = False then
    Result := -1;
end;


{!!
<FS>TImageEnFolderMView.InsertImage

Note: It is better to use <A TImageEnFolderMView.InsertFile> than <A TImageEnMView.InsertImage> as it provides better support for TImageEnFolderMView properties.

<FM>See Also<FN>
- <A TImageEnFolderMView.InsertFile>
- <A TImageEnMView.InsertImage>

!!}


{!!
<FS>TImageEnFolderMView.InsertImageEx

Note: It is better to use <A TImageEnFolderMView.InsertFile> than <A TImageEnMView.InsertImageEx> as it provides better support for TImageEnFolderMView properties.

<FM>See Also<FN>
- <A TImageEnFolderMView.InsertFile>
- <A TImageEnMView.InsertImageEx>

!!}


{!!
<FS>TImageEnFolderMView.InsertFile

<FM>Declaration<FC>
function InsertFile(idx : Integer; const sFilename : string; bSelectIt : Boolean = True; bCheckFileType : Boolean = True) : Boolean;

<FM>Description<FN>
InsertFile adds a new file at the specified position in the grid. If <FC>bSelectIt<FN> is enabled the new file will be selected.
If <FC>bCheckFileType<FN> is enabled then the file is only added if it is permitted by folder parameters: <A TImageEnFolderMView.FileTypes>, <A TImageEnFolderMView.FileTypesMask>, <A TImageEnFolderMView.ExclusionMask>, <A TImageEnFolderMView.ShowFolders> and <A TImageEnFolderMView.ShowHiddenFiles>

Result is False if the file does not exist.

Note: Unlike the <A TImageEnMView.InsertImage> methods, the added file will be loaded on demand and filled with the details and properties of TImageEnFolderMView

<FM>Example<FC>
// Add file to the start of the list
IEFolderMView1.Add(0, 'D:\MyNewImage.jpg');

!!}
function TImageEnFolderMView.InsertFile(idx : Integer; const sFilename : string; bSelectIt : Boolean = True; bCheckFileType : Boolean = True) : Boolean;

  function _FileExtInExtensions(sFileExt : String; const sExtensions : String) : Boolean;
  begin
    sFileExt := Lowercase(sFileExt);
    result := (sFileExt <> '') and (sExtensions <> '') and
              (pos(sFileExt + ',', Lowercase(sExtensions + ',')) > 0);
  end;

var
  dir : TIEDirContent;
  fname : WideString;
  bAllowUnknownFormats: Boolean;
  bIncludeVideoFiles: Boolean;
  sFilterMask: string;
  bAllow: Boolean;
  ext: string;
begin
  Result := False;
  // Need to use ExcludeTrailingBackSlash() so we support folders
  dir := TIEDirContent.Create(ExcludeTrailingBackSlash(sFilename));
  try
    if dir.GetItem(fname, True, True, True) then
    begin
      bAllow := True;
      if bCheckFileType then
      begin
        if dir.IsFolder then
          bAllow := fShowFolders
        else
        begin
          _GetFillFromDirectoryParams(bAllowUnknownFormats, bIncludeVideoFiles, sFilterMask);
          ext := IEExtractFileExtW(sFilename, false);
          bAllow := (bAllowUnknownFormats or (fDetectFileFormat and (FindFileFormat(sFilename) <> ioUnknown)) or IsKnownFormat(sFilename, bIncludeVideoFiles)) and
                    (_FileExtInExtensions(ext, fExclusionMask) = False) and
                    ((sFilterMask = '') or _FileExtInExtensions(ext, sFilterMask));
        end;
        if (fShowHiddenFiles = False) and dir.IsHiddenFile then
          bAllow := False;
      end;

      if bAllow then
      begin
        InsertImage(idx, sFilename, fLoadOnDemand, DefaultTopText, DefaultInfoText, DefaultBottomText, bSelectIt);

        ImageFileSize[idx]   := dir.FileSizeBytes;
        ImageCreateDate[idx] := dir.CreateDate;
        ImageEditDate[idx]   := dir.EditDate;
        If dir.IsFolder then
          PIEImageInfo(fImageInfo[idx])^.SourceType := iestFolderIcon;
        Result := True;
      end;
     end;
  finally
    dir.Free;
  end;
end;


{!!
<FS>TImageEnFolderMView.RemoveFile

<FM>Declaration<FC>
procedure RemoveFile(const sFilename : string);

<FM>Description<FN>
Removes the file <FC>sFilename<FN> from the control.

<FM>Example<FC>
IEFolderMView1.RemoveFile('D:\MyOldmage.jpg');

!!}
procedure TImageEnFolderMView.RemoveFile(const sFilename : string);
var
  idx: integer;
begin
  idx := FilenameToIndex(sFilename);
  DeleteImage(idx);
end;



function TImageEnFolderMView.GetImageFileType(idx: integer): WideString;
begin
  Result := Inherited GetImageFileType(idx);
end;


{!!
<FS>TImageEnFolderMView.FilenameToIndex

<FM>Declaration<FC>
function FilenameToIndex(const sFilename : string) : integer;

<FM>Description<FN>
Returns the index of the specified filename or -1 if the file does not exist in the list.

<FC>sFilename<FN> must be a full path (file folder and name).  

<FM>Example<FC>
// Set caption of "Image.jpg" to "Cool Image"
idx := IEFolderMView.FilenameToIndex('C:\My Images\Image.jpg');
if idx >=0 then
  IEFolderMView.ImageInfoText[idx].Caption := 'Cool Image';

!!}
function TImageEnFolderMView.FilenameToIndex(const sFilename : string) : integer;
var
  I: Integer;
begin
  Result := -1;
  if sFilename = '' then
    exit;

  for I := 0 to ImageCount - 1 do
  begin
    if SameText(ImageFilename[I], sFilename) then
    begin
      Result := I;
      Break;
    end;
  end;
end;



{!!
<FS>TImageEnMView.ImageType

<FM>Declaration<FC>
property ImageType[idx: Integer]: <A TIEFolderImageType>;

<FM>Description<FN>
Determines the type of file at at index, <FC>idx<FN>.
           
<FM>Example<FC>
case IEFolderMView1.ImageType[idx] of
  ieftFolder          : Caption := 'Folder';
  ieftSupportedImage  : Caption := 'Image/Video';
  ieftFile            : Caption := 'File';
end;

!!}

function TImageEnFolderMView.GetImageType(idx: integer): TIEFolderImageType;
begin
  Result := Inherited GetImageType(idx);
end;

function TImageEnFolderMView.GetSelectedFilename: WideString;
var
  iSel: Integer;
begin
  Result := '';

  if DisplayMode = mdSingle then
    iSel := VisibleFrame
  else
    iSel := SelectedImage;

  if iSel >= 0 then
    Result := ImageFilename[iSel];
end;      

function TImageEnFolderMView.GetShowThumbnailHint: Boolean;
begin 
  Result := fShowThumbnailHint;
end;

procedure TImageEnFolderMView.KeyUp(var Key: Word; Shift: TShiftState);
const
  VK_C = 67;
  VK_V = 86;
  VK_X = 88;
begin
  inherited;
  if ieboEnableFolderShortcuts in fFolderInteract then
  begin
    if Key = VK_RETURN then
    begin
      HandleDblClick(SelectedImage);
      Key := 0;
    end
    else
    if (Key = VK_Back) or  // Strictly speaking this should be "View the previous folder"
       ((Key = VK_RIGHT) and (Shift = [ssAlt])) then
    begin
      OpenParentFolder;
      Key := 0;
    end
    else
    if (Key = VK_F5) then
    begin
      RefreshFileList;
      Key := 0;
    end;
  end;

  if ieboEnableFileShortcuts in fFolderInteract then
  begin
    case Key of                                         
      VK_DELETE : begin
                    DeleteSelectedFilesFromFolderEx(ssShift in Shift);
                    Key := 0;
                  end;
      VK_C      : if ssCtrl in Shift then
                  begin
                    CopySelectedFilesToClipboard;
                    Key := 0;
                  end;
      VK_X      : if ssCtrl in Shift then
                  begin
                    CutSelectedFilesToClipboard;
                    Key := 0;
                  end;   
      VK_V      : if ssCtrl in Shift then
                  begin
                    PasteFilesFromClipboard;
                    Key := 0;
                  end;
    end;
  end;
end;

{!!
<FS>TImageEnFolderMView.RefreshFileList

<FM>Declaration<FC>
procedure RefreshFileList;

<FM>Description<FN>
Refills the control with files found in the folder <A TImageEnFolderMView.Folder>.

This can be useful if you have not enabled <A TImageEnFolderMView.AutoRefresh>.

!!}
procedure TImageEnFolderMView.RefreshFileList;
begin
  RefreshFileListEx(False);
end;

procedure TImageEnFolderMView._GetFillFromDirectoryParams(out bAllowUnknownFormats : Boolean; out bIncludeVideoFiles : Boolean; out sFilterMask : string);
begin
  case fFileTypes of

    iefAllImages :
      begin
        bAllowUnknownFormats := False;
        bIncludeVideoFiles   := False;
        sFilterMask          := '';
      end;

    iefAllImagesAndVideos :
      begin
        bAllowUnknownFormats := False;
        bIncludeVideoFiles   := True;
        sFilterMask          := '';
      end;

    iefAllFiles :
      begin
        bAllowUnknownFormats := True;
        bIncludeVideoFiles   := True;
        sFilterMask          := '';
      end;

    else { iefCustom }
      begin
        bAllowUnknownFormats := True;
        bIncludeVideoFiles   := True;
        sFilterMask          := fFileTypesMask;
      end;
  end;
end;


procedure TImageEnFolderMView.RefreshFileListEx(bNewFolder : Boolean);
var
  bAllowUnknownFormats: Boolean;
  bIncludeVideoFiles: Boolean;
  sFilterMask: string;
  sSelectedFilename: Widestring;
  sFolder: string;
begin
  fNeedRefreshFileList := False;
  LockUpdate;                                                 
  try
    ClearCache;
    StopMonitoring;

    if bNewFolder then
      sSelectedFilename := ''
    else
      sSelectedFilename := SelectedFilename;

    Clear;
    if (fFolder = '') or (csDesigning in ComponentState) then
      exit;                                    

    _GetFillFromDirectoryParams(bAllowUnknownFormats, bIncludeVideoFiles, sFilterMask);

    MIO.Aborting := False;

    sFolder := GetFirstFolder;
    while (sFolder <> '') and (MIO.Aborting = False) do
    begin
      FillFromDirectory(sFolder, fFileLimit, bAllowUnknownFormats, fExclusionMask, fDetectFileFormat,
                        sFilterMask, bIncludeVideoFiles, fLoadOnDemand,
                        fDefaultTopText, fDefaultInfoText, fDefaultBottomText,
                        fShowHiddenFiles, fShowFolders);
      sFolder := GetNextFolder;
    end;

    if MIO.Aborting = False then
      RefreshSorting;

    SelectedFilename := sSelectedFilename;
  finally
    UnlockUpdate;
  end;

  if fAutoRefresh then  
    StartMonitoring;
end;


{!!
<FS>TImageEnFolderMView.RefreshSorting

<FM>Declaration<FC>
procedure RefreshSorting;

<FM>Description<FN>
Reapplies the current sort settings (<A TImageEnFolderMView.SortOrder>, <A TImageEnFolderMView.SortAscending> and <A TImageEnFolderMView.SortCaseSensitive>).

Note: TImageEnFolderMView will automatically apply sorting when refreshing files and setting sort properties, but this method may be used if files are added due to <L TImageEnFolderMView.AutoRefresh>folder monitoring</L>

!!}
procedure TImageEnFolderMView.RefreshSorting;
var
  sSelectedFilename: string;
begin
  fNeedRefreshSorting := False;
  if not (csDesigning in ComponentState) then
  begin
    LockUpdate;
    try
      sSelectedFilename := SelectedFilename;
      if (fSortOrder = iesbCustom) and assigned(fOnCustomSortCompare) then
        Sort(fOnCustomSortCompare)
      else
        Sort(fSortOrder, fSortAscending, fSortCaseSensitive);
      SelectedFilename := sSelectedFilename;
    finally
      UnlockUpdate;
    end;
  end;
end;



{!!
<FS>TImageEnFolderMView.SetFolderEx

<FM>Declaration<FC>
procedure SetFolderEx(const sFolder: string; xFileTypes: TIEFolderFileTypes; const sFileTypesMask: string = ''; const sExclusionMask: string = '');

<FM>Description<FN>
Specifies the folder and filtering options in one step (to avoid premature updating).

It is the same as making the following calls:
  <A TImageEnFolderMView.LockUpdate>
  try
    <A TImageEnFolderMView.Folder>
    <A TImageEnFolderMView.FileTypes>
    <A TImageEnFolderMView.FileTypesMask>
    <A TImageEnFolderMView.ExclusionMask>
  finally
    <A TImageEnFolderMView.UnLockUpdate>
  end;


<FM>Example<FC>       
// Retrieve all JPEG images in C:\Images\
IEFolderMView1.SetFolderEx('C:\Images\', iefCustom, 'jpg,jpeg,jpe');

// This is the same as calling:
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.Folder := 'C:\Images\';
  IEFolderMView1.FileTypes := iefCustom;
  IEFolderMView1.FileTypesMask := 'jpg,jpeg,jpe';
finally
  IEFolderMView1.UnlockUpdate;
end;

!!}
procedure TImageEnFolderMView.SetFolderEx(const sFolder: string; xFileTypes: TIEFolderFileTypes; const sFileTypesMask: string = ''; const sExclusionMask: string = '');
begin
  LockUpdate;
  try
    Folder        := sFolder;
    FileTypes     := xFileTypes;
    FileTypesMask := sFileTypesMask;
    ExclusionMask := sExclusionMask;
    RefreshFileList;
  finally
    UnlockUpdate;
  end;
end;



{!!
<FS>TImageEnFolderMView.SetSortOrderEx

<FM>Declaration<FC>
procedure SetSortOrderEx(xSortOrder: TIEImageEnMViewSortBy; bSortAscending: Boolean = True; bSortCaseSensitive: Boolean = False);

<FM>Description<FN>
Specifies both sort options in one step (to avoid premature updating).

It is the same as making the following calls:
  <A TImageEnFolderMView.LockUpdate>
  try
    <A TImageEnFolderMView.SortOrder>
    <A TImageEnFolderMView.SortAscending>
    <A TImageEnFolderMView.SortCaseSensitive>
  finally
    <A TImageEnFolderMView.UnLockUpdate>
  end;

!!}
procedure TImageEnFolderMView.SetSortOrderEx(xSortOrder: TIEImageEnMViewSortBy; bSortAscending: Boolean = True; bSortCaseSensitive: Boolean = False);
begin
  LockUpdate;
  try
    SortOrder         := xSortOrder;
    SortAscending     := bSortAscending;
    SortCaseSensitive := bSortCaseSensitive;
    RefreshSorting;
  finally
    UnlockUpdate;
  end;
end;


// Replace IEF_CURRENT_FILE if specified
procedure TImageEnFolderMView.CheckIndexForConst(var idx : Integer);
begin
  if idx = IEF_CURRENT_FILE then
  begin
    if DisplayMode = mdSingle then
      idx := VisibleFrame
    else
      idx := SelectedImage;
  end;
end;

{!!
<FS>TImageEnFolderMView.ExecuteFile

<FM>Declaration<FC>
function ExecuteFile(idx : Integer = IEF_CURRENT_FILE) : boolean;

<FM>Description<FN>
Executes the file at idx using the standard windows functionality.

Note: No check is made whether the file type is supported by ImageEn or not.

Result is true unless an error was encountered.

If you specify IEF_CURRENT_FILE for <FC>idx<FN> then currently selected folder will be opened.
       
<FM>Example<FC>
case IEFolderMView1.ImageType[idx] of
  ieftFolder          : IEFolderMView1.OpenFolder(idx);
  ieftSupportedImage  : DisplayImage(idx);
  ieftFile            : IEFolderMView1.ExecuteFile(idx);
end;
!!}
function TImageEnFolderMView.ExecuteFile(idx : Integer = IEF_CURRENT_FILE) : boolean;
var
  sFilename: WideString;
begin
  CheckIndexForConst(idx);

  sFilename := ImageFilename[idx];
  Result := sFilename <> '';
  if Result then
  try
    WindowsLaunchFile(Handle, sFilename);
  except
    on E:Exception do
    begin
      Result := False;
      MessageDlg(e.message, mtError, [mbOK], 0);
    end;
  end;
end;



{!!
<FS>TImageEnFolderMView.RenameFile

<FM>Declaration<FC>
function RenameFile(idx : Integer; const sNewName : string) : boolean;

<FM>Description<FN>
Renames the file at <FC>idx<FN> to <FC>sNewName<FN> using the standard windows functionality.

Note: <FC>sNewName<FN> must be a name only, without a folder path

Result is true unless an error was encountered.

If you specify IEF_CURRENT_FILE for <FC>idx<FN> then currently selected file will be renamed.

<FM>Example<FC>
// Rename current file to "Selected.jpg"
RenameFile(IEF_CURRENT_FILE, 'Selected.jpg');

!!}
function TImageEnFolderMView.RenameFile(idx : Integer; const sNewName : string): boolean;
var
  sFilename: WideString;
begin                     
  CheckIndexForConst(idx);
  sFilename := ImageFilename[idx];
  Result := (sFilename <> '') and (sNewName <> '');
  if Result then
    Result := WindowsRename(Handle, sFilename, sNewName, False, True); 
  if Result and (fAutoRefresh = False) then
  begin
    DeleteImage(Idx);
    AppendFile(IncludeTrailingBackSlash(ExtractFilePath(sFilename)) + sNewName);
  end;
end;




{!!
<FS>TImageEnFolderMView.OpenFolder

<FM>Declaration<FC>
function OpenFolder(idx : Integer = IEF_CURRENT_FILE) : boolean;

<FM>Description<FN>
If the frame at idx is a file folder, then it will be opened (i.e. <A TImageEnFolderMView.Folder> will change to that location)

Result is true if the frame at idx is a folder.

If you specify IEF_CURRENT_FILE for <FC>idx<FN> then currently selected folder will be opened.

!!}
function TImageEnFolderMView.OpenFolder(idx : Integer = IEF_CURRENT_FILE) : boolean;
begin
  CheckIndexForConst(idx);
  Result := ImageType[idx] = ieftFolder;
  if Result then
    Folder := ImageFilename[idx];
end;




{!!
<FS>TImageEnFolderMView.PromptForFolder

<FM>Declaration<FC>
function PromptForFolder : boolean;

<FM>Description<FN>
Display a folder selection dialog and allows the user to select a folder. The grid will be then set to that <A TImageEnFolderMView.Folder>.

Result is true if a folder was selected.

!!}
function TImageEnFolderMView.PromptForFolder : boolean;
var
  sNewFolder: string;                  
begin
  sNewFolder := GetFirstFolder;
  Result := WindowsSelectDirectory(iemsg(IEMSG_SelectAFolderToOpen), sNewFolder, Self);
  if Result then
    Folder := sNewFolder;
end;

// Returns the parents of sFolder or '' if there is no parent
function GetParentFolder(const sFolder : String) : String;
begin
  Result := ExtractFilePath(ExcludeTrailingBackslash(sFolder));
  if SameText(ExcludeTrailingBackslash(sFolder), ExcludeTrailingBackslash(Result)) then
    Result := '';
end;

{!!
<FS>TImageEnFolderMView.CreateNewFolder

<FM>Declaration<FC>
function CreateNewFolder(const sFolderName : string) : Boolean;

<FM>Description<FN>
Creates a new folder within the current <A TImageEnFolderMView.Folder> of the name <FC>sFolderName<FN>.

Note: <FC>sFolderName<FN> must NOT be a full path (just a new name)

<FM>Example<FC>
// Create a new folder of the name, "New Folder"
IEFolderMView1.CreateNewFolder('New Folder');
!!}
function TImageEnFolderMView.CreateNewFolder(const sFolderName : string) : Boolean;
var
  sNewFolder: string;
begin
  Result := fFolder <> '';
  sNewFolder := IncludeTrailingBackSlash(GetFirstFolder) + sFolderName;
  if Result then
    Result := CreateDir(sNewFolder);
  if (Result = False) and (ieioVerboseErrors in fFileOperationOptions) then
    MessageDlg(format('Unable to create "%s"', [sFolderName]) + #13#10 + #13#10 + SysErrorMessage(GetLastError), mtError, [mbOK], 0);
  if Result and (fAutoRefresh = False) then
    AppendFile(sNewFolder, True);
end;






{!!
<FS>TImageEnFolderMView.OpenParentFolder

<FM>Declaration<FC>
function OpenParentFolder : boolean;

<FM>Description<FN>
Changes <A TImageEnFolderMView.Folder> to the parent of the current folder

Result is true if there is a parent folder.

See also: <A TImageEnFolderMView.CanOpenParentFolder>

Note: If <FC>ieboEnableFolderShortcuts<FN> is specified for <A TImageEnFolderMView.FolderInteract> then the control will automatically Go Up if the user clicks Backspace or Alt+Up

!!}
function TImageEnFolderMView.OpenParentFolder : boolean;
var
  sParentFolder: string;
begin
  sParentFolder := GetParentFolder(GetFirstFolder);
  Result := sParentFolder <> '';
  if Result then
    Folder := sParentFolder;
end;


{!!
<FS>TImageEnFolderMView.CanOpenParentFolder

<FM>Declaration<FC>
function CanOpenParentFolder : boolean;

<FM>Description<FN>
Returns true if there is a folder above the current <A TImageEnFolderMView.Folder> that can be accessed (e.g. using <A TImageEnFolderMView.OpenParentFolder>)

<FM>Example<FC>
// Enable our "Go Up" button
btnGoUp.Enabled := IEFolderMView1.CanOpenParentFolder;

!!}
function TImageEnFolderMView.CanOpenParentFolder : boolean;
var
  sParentFolder: string;
begin
  sParentFolder := GetParentFolder(GetFirstFolder);
  Result := sParentFolder <> '';
end;

{!!
<FS>TImageEnFolderMView.LockUpdate

<FM>Declaration<FC>
procedure LockUpdate;

<FM>Description<FN>
Increments the <L TImageEnFolderMView.LockUpdateCount>lock update counter</L>. While <A TImageEnFolderMView.LockUpdateCount> is greater than zero all component updating is disabled.

Use <A TImageEnFolderMView.UnLockUpdate> to unlock.

<FM>Example<FC>
// Disable updating of component
IEFolderMView1.LockUpdate;
try
  ... Perform activities, e.g. appending many files
finally
  // Re-enable Updating and refresh view
  IEFolderMView1.UnlockUpdate;
end;

!!}
procedure TImageEnFolderMView.LockUpdate;
begin
  inherited;
end;

procedure TImageEnFolderMView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fClickedFrameIndex := ImageAtPosEx(x, y, True);
  if (Button = mbRight) and fPopupMenuUseSystem and (PopupMenu = nil) then
    PopupSystemMenu(X, Y);
end;    

procedure TImageEnFolderMView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;                
  fDragging := False;
  fMouseClickPos := point(X, Y);
end;


procedure TImageEnFolderMView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ssFilenames: TStringList;
begin   
  inherited;

  if (fAutoDragFiles <> []) and
     (csLButtonDown in ControlState) and
     (ImageAtPosEx(fMouseClickPos.x, fMouseClickPos.y, True) >= 0) and // actually clicked on the image
     ((Abs(X - fMouseClickPos.x) >= Mouse.DragThreshold) or
      (Abs(Y - fMouseClickPos.y) >= Mouse.DragThreshold)) then
  begin
    fDragging := True; // so mouse up does not occur
    Perform(WM_LBUTTONUP, 0, MakeLong(X, Y));
    ssFilenames := TStringList.create;
    try
      SelectedFilenames(ssFilenames);
      fDragDrop.InitiateDragging(ssFilenames, fAutoDragFiles);
    finally
      ssFilenames.free;
    end;
  end;

end;

{!!
<FS>TImageEnFolderMView.PopupSystemMenu

<FM>Declaration<FC>
procedure PopupSystemMenu(X, Y: Integer);

<FM>Description<FN>
Displays the system menu for the specified files at position x,y. This menu is the same as that displayed when right-clicking a selection of files in Windows Explorer.

Note: If <A TImageEnFolderMView.AutoRefresh> is not enabled, the control may not update the display of files that are moved or renamed

<FM>Example<FC>
PopupSystemMenu(Form1.Handle, ssSelectedFiles, ClickPos.X, ClickPos.Y);

!!}
procedure TImageEnFolderMView.PopupSystemMenu(X, Y: Integer);
var
  ssFilenames : TStringList;
begin
  ssFilenames := TStringList.create;
  try
    SelectedFilenames(ssFilenames); 
    iexWindowsFunctions.PopupSystemMenu(Handle, ssFilenames, X, Y);

    if fAutoRefresh = False then
      CheckIfFilesDeletedFromFolder(ssFilenames);
  finally
    ssFilenames.free;
  end;
end;


{!!
<FS>TImageEnFolderMView.UnLockUpdate

<FM>Declaration<FC>
function UnlockUpdate: integer;

<FM>Description<FN>
Decrement the <L TImageEnFolderMView.LockUpdateCount>lock update counter</L> (use after calling <A TImageEnFolderMView.LockUpdate>).

If the lock count is zero, then <A TImageEnMView.Update> is called to refresh the view.

Returns the lock count.

<FM>Example<FC>
// Disable updating of component while setting properties
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.Folder := 'C:\Images\';
  IEFolderMView1.FileTypes := iefCustom;
  IEFolderMView1.FileTypesMask := 'jpg,jpeg,jpe';
  IEFolderMView1.SortOrder := iesbFilename;
  IEFolderMView1.SortAscending := True;
finally
  // Re-enable updating and call update
  IEFolderMView1.UnlockUpdate;
end;

!!}
// decreases fLockUpdate
// ret. current value (after the decrement)
function TImageEnFolderMView.UnLockUpdate: integer;
begin
  // If this call will unlock the update (set it to 0) then refresh our list
  if fInitialized and (LockUpdateCount = 1) then
    CheckForPendingRefresh;

  Result := UnlockUpdateEx;
end;

{!!
<FS>TImageEnFolderMView.LockUpdateCount

<FM>Declaration<FC>
property LockUpdateCount: Integer;

<FM>Description<FN>
Returns the lock updating state. A value of 0 means no locking. A value greater than zero means locking is in place (i.e. updating is disabled).

Calling <A TImageEnFolderMView.LockUpdate> increments <A TImageEnFolderMView.LockUpdateCount>, <A TImageEnFolderMView.UnLockUpdate> decrements it.
!!}


{!!
<FS>TImageEnFolderMView.FileTypes

<FM>Declaration<FC>
property FileTypes: <A TIEFolderFileTypes>;

<FM>Description<FN>
The file types that TImageEnFolderMView will retrieve from your <A TImageEnFolderMView.Folder>.

If FileTypes is iefCustom then use <A TImageEnFolderMView.FileTypesMask> to specify your file types.

<FM>See Also<FN>
- <A TImageEnFolderMView.Folder>
- <A TImageEnFolderMView.FileTypesMask>
- <A TImageEnFolderMView.ExclusionMask>
- <A TImageEnFolderMView.SetFolderEx>

!!}
procedure TImageEnFolderMView.SetFileTypes(const Value: TIEFolderFileTypes);
begin
  if fFileTypes <> Value then
  begin
    StopMonitoring;
    fFileTypes := Value;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshFileList
    else
      fNeedRefreshFileList := True;
  end;
end;



{!!
<FS>TImageEnFolderMView.ShowFolders

<FM>Declaration<FC>
property ShowFolders: Boolean;

<FM>Description<FN>
Enable (default) to display folders with your files. When folders are displayed they can be <L TImageEnFolderMView.FolderInteract>opened by mouse or keyboard click</L>

See Also: <A TImageEnFolderMView.ShowHiddenFiles>
                                                 

<FM>Example<FC>
// Show folders
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.Folder := 'C:\';
  IEFolderMView1.ShowFolders := True;
finally
  // Re-enable updating and refresh content
  IEFolderMView1.UnlockUpdate;
end;
!!}
procedure TImageEnFolderMView.SetShowFolders(const Value: Boolean);
begin
  if fShowFolders <> Value then
  begin
    StopMonitoring;
    fShowFolders := Value;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshFileList
    else
      fNeedRefreshFileList := True;
  end;
end;


{!!
<FS>TImageEnFolderMView.ShowHiddenFiles

<FM>Declaration<FC>
property ShowHiddenFiles: Boolean;

<FM>Description<FN>
Enable to display system and hidden files (default is false).

See Also: <A TImageEnFolderMView.ShowFolders>

<FM>Example<FC>
// Show hidden files
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.Folder := 'C:\';
  IEFolderMView1.ShowFolders := True;
  IEFolderMView1.ShowHiddenFiles := True;
finally
  // Re-enable updating and refresh content
  IEFolderMView1.UnlockUpdate;
end;

!!}
procedure TImageEnFolderMView.SetShowHiddenFiles(const Value: Boolean);
begin
  if fShowHiddenFiles <> Value then
  begin
    StopMonitoring;
    fShowHiddenFiles := Value;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshFileList
    else
      fNeedRefreshFileList := True;
  end;
end;



{!!
<FS>TImageEnFolderMView.DefaultFolder

<FM>Declaration<FC>
property DefaultFolder : <A TIEDefaultFolder>;

<FM>Description<FN>
The initial value for <A TImageEnFolderMView.Folder>.

<FM>Example 1<FC>
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Default to the "My Pictures" folder
  IEFolderMView1.DefaultFolder := iedfMyPictures;
end;

<FM>Example 2<FC>
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Default to a custom folder
  IEFolderMView1.DefaultFolder := iedfSpecified;   // Note: This line is optional at run-time
  IEFolderMView1.Folder := 'D:\My Custom Folder';
end;
!!}
procedure TImageEnFolderMView.SetDefaultFolder(const Value: TIEDefaultFolder);
begin
  fDefaultFolder := Value;

  // Display normalized default folder path in the Object Inspector
  if csDesigning in ComponentState then
    _SetFolder(DefaultFolderToStr(fDefaultFolder, fFolder), True);
end;


{!!
<FS>TImageEnFolderMView.FileTypesMask

<FM>Declaration<FC>
property FileTypesMask: string;

<FM>Description<FN>
Specifies the file types to retrieve from your <A TImageEnFolderMView.Folder>. File types must be specified as a comma-separated list of extensions (e.g. 'jpg,jpeg,jpe'). If an empty string is specified all extensions will be returned.

Note: Property is ignored unless FileTypes has been set to iefCustom

<FM>Example<FC>
// Retrieve only JPEG images
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.Folder := 'C:\Images\';
  IEFolderMView1.FileTypes := iefCustom;
  IEFolderMView1.FileTypesMask := 'jpg,jpeg,jpe';
finally
  // Re-enable updating and call update
  IEFolderMView1.UnlockUpdate;
end;

<FM>See Also<FN>
- <A TImageEnFolderMView.Folder>
- <A TImageEnFolderMView.FileTypes>
- <A TImageEnFolderMView.ExclusionMask>
- <A TImageEnFolderMView.SetFolderEx>

!!}
procedure TImageEnFolderMView.SetFileTypesMask(const Value: string);
begin
  if fFileTypesMask <> Value then
  begin
    fFileTypesMask := Value;
    if fFileTypes = iefCustom then
    begin
      StopMonitoring;
      if fInitialized and (LockUpdateCount = 0) then
        RefreshFileList
      else
        fNeedRefreshFileList := True;
    end;
  end;
end;

{!!
<FS>TImageEnFolderMView.Folder

<FM>Declaration<FC>
property Folder: string;

<FM>Description<FN>
Specifies the folder to search for files with which we fill the control. You can specify multiple folders by delimiting them with IEM_Folder_Delimiter (which is a bar: |).

The type of files to be retrieved is specified by <A TImageEnFolderMView.FileTypes> and <A TImageEnFolderMView.FileTypesMask>. You can exclude particular types with <A TImageEnFolderMView.ExclusionMask>.

To update all folder properties at once, use <A TImageEnFolderMView.SetFolderEx>.

The following constants can be specified in place of a folder path:
<TABLE>
<R> <H>Constant</H> <H>Description</H> </R>
<R> <C>IEF_Desktop_Folder</C> <C>Windows Desktop</C> </R>
<R> <C>IEF_Root_Directory</C> <C>The drive containing Windows (usually C:\)</C> </R>
<R> <C>IEF_MyDocuments_Folder</C> <C>The user's "Documents" folder</C> </R>
<R> <C>IEF_MyPictures_Folder</C> <C>The user's "Pictures" folder</C> </R>
<R> <C>IEF_MyVideos_Folder</C> <C>The user's "Videos" folder</C> </R>
</TABLE>


Note: The value of <FC>Folder<FN> at start-up is defined by <A TImageEnFolderMView.DefaultFolder>. If <FC>DefaultFolder<FN> is not iedfSpecified then any value you have set for <FC>Folder<FN> at design time is ignored.


<FM>Example 1<FC>
// Retrieve only JPEG images
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.Folder := 'C:\Images\';
  IEFolderMView1.FileTypes := iefCustom;
  IEFolderMView1.FileTypesMask := 'jpg,jpeg,jpe';
finally
  // Re-enable updating and refresh content
  IEFolderMView1.UnlockUpdate;
end;

// This is the same as calling:
IEFolderMView1.SetFolderEx('C:\Images\', iefCustom, 'jpg,jpeg,jpe');

<FM>Example 2<FC>
// Display "My Pictures" folder
IEFolderMView1.Folder := IEF_MyPictures_Folder;
            
<FM>Example 3<FC>
// Display content of all drives on system by delimiting them with IEM_Folder_Delimiter
procedure TForm1.btnShowAllDrives(Sender: TObject);
var
  vDrivesSize : Cardinal;
  vDrives     : array[0..128] of Char;
  vDrive      : PChar;
  sDrives: string;
begin
  sDrives := '';
  vDrivesSize := GetLogicalDriveStrings(SizeOf(vDrives), vDrives);
  if vDrivesSize > 0 then
  begin
    vDrive := vDrives;
    while vDrive^ <> #0 do
    begin
      sDrives := sDrives + StrPas(vDrive) + IEM_Folder_Delimiter;
      Inc(vDrive, SizeOf(vDrive));
    end;
  end;
  IEFolderMView.Folder := sDrives;
end;
!!}


procedure TImageEnFolderMView.SetFolder(const Value: string);
begin
  _SetFolder(Value, False);
end;



procedure TImageEnFolderMView._SetFolder(const Value: string; bIsFirstCall : Boolean);

  function _FolderConstToPath(const aConst : string) : string;
  {$IFDEF Delphi2006orOlder}
  const
    CSIDL_WINDOWS = $0024; { "C:\Windows" }
  {$ENDIF}
  begin
    Result := aConst;

    if SameText(aConst, IEF_Desktop_Folder) then
      Result := WindowsDesktopFolder
    else
    if SameText(aConst, IEF_Root_Directory) then
      Result := IncludeTrailingBackslash(ExtractFileDrive(GetWindowsSpecialFolder(CSIDL_WINDOWS)))
    else
    if SameText(aConst, IEF_MyDocuments_Folder) then
      Result := WindowsMyDocumentsFolder
    else
    if SameText(aConst, IEF_MyPictures_Folder) then
      Result := WindowsMyPicturesFolder
    else
    if SameText(aConst, IEF_MyVideos_Folder) then
      Result := WindowsMyVideosFolder;
  end;

var
  sNewFolder: string;
  bAllow: Boolean;
begin
  sNewFolder := _FolderConstToPath(Value);
  if bIsFirstCall or (fFolder <> sNewFolder) then
  begin
    if (bIsFirstCall = False) and (not (csLoading in ComponentState)) then
    begin
      if sNewFolder <> '' then
        fDefaultFolder := iedfSpecified
      else
      if fDefaultFolder = iedfSpecified then
        fDefaultFolder := iedfNone;
    end;

    StopMonitoring;
    bAllow := True;
    if (sNewFolder <> '') and assigned(fOnFolderChange) then
      fOnFolderChange(Self, sNewFolder, bAllow);
    if bAllow = False then
      exit;

    fFolder := sNewFolder;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshFileListEx(True)
    else
      fNeedRefreshFileList := True;
  end;
end;

  
function TImageEnFolderMView.GetFolder : string;
begin
  if (csWriting in ComponentState) and (fDefaultFolder <> iedfSpecified) then
    Result := ''
  else
    Result := fFolder;
end;    
  


{!!
<FS>TImageEnFolderMView.FillFromDirectory

DO NOT USE for TImageEnFolderMView! Use <A TImageEnFolderMView.Folder> and associated properties instead.

See also: <A TImageEnMView.FillFromDirectory>

!!}

{!!
<FS>TImageEnFolderMView.SortAscending

<FM>Declaration<FC>
property SortAscending: Boolean;

<FM>Description<FN>
Whether the sort order specified by <A TImageEnFolderMView.SortOrder> is ascending or descending.

Note: you can set both sort properties at once, using <A TImageEnFolderMView.SetSortOrderEx>.


<FM>Example<FC>
// Sort by file name reverse alphabetically
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.SortOrder := iesbFilename;
  IEFolderMView1.SortAscending := False;
finally
  IEFolderMView1.UnlockUpdate;
end;

// Which is the same as:
IEFolderMView1.SetSortOrderEx(iesbFilename, False);
!!}

procedure TImageEnFolderMView.SetSortAscending(const Value: Boolean);
begin
  if fSortAscending <> Value then
  begin
    fSortAscending := Value;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshSorting
    else
      fNeedRefreshSorting := True;
  end;
end;

{!!
<FS>TImageEnFolderMView.SortCaseSensitive

<FM>Declaration<FC>
property SortCaseSensitive: Boolean;

<FM>Description<FN>
Whether text comparisons when sorting are case sensitive or not.

See also: <A TImageEnFolderMView.SetSortOrderEx>
!!}
procedure TImageEnFolderMView.SetSortCaseSensitive(const Value: Boolean);
begin
  if fSortCaseSensitive <> Value then
  begin
    fSortCaseSensitive := Value;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshSorting
    else
      fNeedRefreshSorting := True;
  end;
end;


{!!
<FS>TImageEnFolderMView.ShowThumbnailHint

<FM>Declaration<FC>
property ShowThumbnailHint : Boolean;

<FM>Description<FN>
Automatically fills the control's <FC>Hint<FN> property with full fill details of the thumbnail under the cursor.

!!}

procedure TImageEnFolderMView.SetShowThumbnailHint(const Value: Boolean);
begin
  if fShowThumbnailHint <> Value then
  begin
    fShowThumbnailHint := Value;
    if fShowThumbnailHint then
      ShowHint := True
    else
      Hint := '';
  end;
end;



{!!
<FS>TImageEnFolderMView.FileOperationOptions

<FM>Declaration<FC>
property FileOperationOptions : <A TIEFileOperationOptions>;

<FM>Description<FN>
Configure settings for when moving, copying or deleting files.

<FM>Affected Methods<FC>
- <A TImageEnFolderMView.CopyFilesToCurrentFolder>
- <A TImageEnFolderMView.CopySelectedFilesToClipboard>
- <A TImageEnFolderMView.CutSelectedFilesToClipboard>
- <A TImageEnFolderMView.DeleteSelectedFilesFromFolder>
- <A TImageEnFolderMView.MoveFilesToCurrentFolder>
- <A TImageEnFolderMView.MoveSelectedFilesToFolder>
- <A TImageEnFolderMView.PasteFilesFromClipboard>
                    
!!}
procedure TImageEnFolderMView.SetFileOperationOptions(const Value: TIEFileOperationOptions);
begin
  if fFileOperationOptions <> Value then
    fFileOperationOptions := Value;
end;
    
{!!
<FS>TImageEnFolderMView.FolderInteract

<FM>Declaration<FC>
property FolderInteract : <A TIEFolderInteractOptions>;

<FM>Description<FN>
Configure the behaviour when when interacting with the control.
                    
!!}
procedure TImageEnFolderMView.SetFolderInteract(const Value: TIEFolderInteractOptions);
begin
  if fFolderInteract <> Value then
    fFolderInteract := Value;
end;

{!!
<FS>TImageEnFolderMView.SelectedFilename

<FM>Declaration<FC>
property SelectedFilename: WideString;

<FM>Description<FN>
Returns the full path of the currently selected frame (using <A TImageEnMView.SelectedImage>) or '' if nothing is selected.

You can set SelectedFilename to make it the selected filename. If the filename is not found, the first item will be selected

See also: <A TImageEnFolderMView.SelectedFilenames>

<FM>Example<FN>
// Show preview of image in ImageEnView when a file is selected
procedure TForm1.IEFolderMView1ImageSelect(Sender: TObject);
begin
  if IEFolderMView1.SelectedFilename = '' then
    ImageEnView1.Blank
  else
    ImageEnView1.IO.LoadFromFile(IEFolderMView1.SelectedFilename);
end;
!!}
procedure TImageEnFolderMView.SetSelectedFilename(const Value: WideString);
var
  iIndex: Integer;
begin
  if ImageCount = 0 then
    exit;
  iIndex := FilenameToIndex(Value);
  if iIndex >= 0 then
    SelectedImage := iIndex
  else                   
    SelectedImage := 0; // Default to the first image
end;

    
{!!
<FS>TImageEnFolderMView.SelectedFilenames

<FM>Declaration<FC>
function SelectedFilenames(Dest : TStrings): Integer;

<FM>Description<FN>
Adds the filenames of all selected files to the specified strings object. Result is the count of selected files.

See also: <A TImageEnFolderMView.SelectedFilename>

<FM>Example<FN>
// Add all selected filenames to a listbox
IEFolderMView1.SelectedFilenames(Listbox1.Items);
!!}
function TImageEnFolderMView.SelectedFilenames(Dest : TStrings): Integer;
var
  i: Integer;
  iSelIndex: Integer;
begin
  Dest.Clear;

  if (DisplayMode = mdSingle) or (EnableMultiSelect = False) then
  begin
    if SelectedFilename <> '' then
      Dest.Add(SelectedFilename);
  end
  else
  begin
    // Sort by index
    MultiSelectSortList;

    // Get filenames
    for i := 0 to MultiSelectedImagesCount-1 do
    begin
      iSelIndex := MultiSelectedImages[ i ];
      Dest.Add( ImageFileName[ iSelIndex ] );
    end;
  end;

  Result := Dest.Count;
end;



{!!
<FS>TImageEnFolderMView.SortOrder

<FM>Declaration<FC>
property SortOrder: <A TIEImageEnMViewSortBy>;

<FM>Description<FN>
The order in which your files are displayed. You can reverse the sort order by setting <A TImageEnFolderMView.SortAscending> to false.

Note: you can set both sort properties at once, using <A TImageEnFolderMView.SetSortOrderEx>.

<FM>Example 1<FC>
// Sort by filename
IEFolderMView1.SortOrder := iesbFilename;

<FM>Example 2<FC>
// Sort by filename in reverse alphabetical order
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.SortOrder := iesbFilename;
  IEFolderMView1.SortAscending := False;
finally
  IEFolderMView1.UnlockUpdate;
end;

// Which is the same as:
IEFolderMView1.SetSortOrderEx(iesbFilename, False);
!!}
procedure TImageEnFolderMView.SetSortOrder(const Value: TIEImageEnMViewSortBy);
begin
  if fSortOrder <> Value then
  begin
    fSortOrder := Value;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshSorting
    else
      fNeedRefreshSorting := True;
  end;
end;


{!!
<FS>TImageEnFolderMView.Sort

<FM>Declaration<FC>
procedure Sort(Compare: <A TIEImageEnMViewSortCompare>);
procedure Sort(Compare: <A TIEImageEnMViewSortCompareEx>);
procedure Sort(OrderBy: <A TIEImageEnMViewSortBy>; Ascending: boolean = true; CaseSenstive : boolean = true);

<FM>Description<FN>
Sorts all images in the TImageEnFolderMView by property (filename, dimensions, etc) or using a custom comparison function.
                                                                                                                                  
DO NOT USE for TImageEnFolderMView! Use properties <A TImageEnFolderMView.SortOrder> and <A TImageEnFolderMView.SortAscending> instead.

!!}




{!!
<FS>TImageEnFolderMView.ExclusionMask

<FM>Declaration<FC>
property ExclusionMask: String;

<FM>Description<FN>
Specify any file types that you do not want to be added to the grid. Must be a comma-separated list of file extensions (e.g. 'lyr,all,iev')

Note: These types will be excluded regardless of your setting for <A TImageEnFolderMView.FileTypes>

<FM>Example<FC>
// Retrieve all images except JPEGs
IEFolderMView1.LockUpdate;
try
  IEFolderMView1.Folder := 'C:\Images\';
  IEFolderMView1.FileTypes := iefAllImages;
  IEFolderMView1.ExclusionMask := 'jpg,jpeg,jpe';
finally
  // Re-enable updating and refresh file list
  IEFolderMView1.UnlockUpdate;
end;
!!}

procedure TImageEnFolderMView.SetExclusionMask(const Value: string);
begin
  if fExclusionMask <> Value then
  begin
    StopMonitoring;
    fExclusionMask := Value;
    if fInitialized and (LockUpdateCount = 0) then
      RefreshFileList
    else
      fNeedRefreshFileList := True;
  end;
end;

{!!
<FS>TImageEnFolderMView.AutoDragFiles

<FM>Declaration<FC>
property AutoDragFiles : <A TIEFileDragDropActions>;

<FM>Description<FN>
Whether files can be dragged from the grid to other Windows applications, such as Explorer. Default is [] meaning that drag drop does not occur automatically.
  
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>[]</C> <C>Dragging of files is not automatically initiated due to user action</C> </R>
<R> <C>[iedpMove]</C> <C>A user can drag files to another application to move them</C> </R>
<R> <C>[iedpCopy]</C> <C>A user can drag files to another application to copy them</C> </R>
<R> <C>[iedpCopy, iedpMove]</C> <C>A user can drag files to another application to move or copy them (the desination application will determine which action takes place. Generally this will be copying, or moving if the Shift key is pressed</C> </R>
</TABLE>
!!}
procedure TImageEnFolderMView.SetAutoDragFiles(const Value: TIEFileDragDropActions);
begin
  fAutoDragFiles := Value;
end;

{!!
<FS>TImageEnFolderMView.AutoDropFiles

<FM>Declaration<FC>
property AutoDropFiles : Set of <A TIEFileDragDropActions>;

<FM>Description<FN>
Whether files can be dropped onto the grid from other Windows applications, such as Explorer. Default is [].

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>[]</C> <C>Dropping of files is disabled</C> </R>
<R> <C>[iedpMove]</C> <C>Files that are dropped onto the control will be moved to the current <A TImageEnFolderMView.Folder></C> </R>
<R> <C>[iedpCopy]</C> <C>Files that are dropped onto the control will be copied to the current <A TImageEnFolderMView.Folder></C> </R>
<R> <C>[iedpCopy, iedpMove]</C> <C>Files that are dropped onto the control can be moved or copied to the current <A TImageEnFolderMView.Folder>. If the Shift key is pressed, they will be moved, otherwise they will be copied.</C> </R>
</TABLE>

!!}
procedure TImageEnFolderMView.SetAutoDropFiles(const Value: TIEFileDragDropActions);
var
  bChangedDragDrop: Boolean;
begin                              
  bChangedDragDrop := (Value = []) <> (fAutoDropFiles = []);
  fAutoDropFiles := Value;       
  fDragDrop.DropActions := fAutoDropFiles;
  if fInitialized and bChangedDragDrop then
    fDragDrop.ActivateDropping := fAutoDropFiles <> [];
end;

      

{!!
<FS>TImageEnFolderMView.AutoRefresh

<FM>Declaration<FC>
property AutoRefresh : Boolean;

<FM>Description<FN>
When enabled the current <A TImageEnFolderMView.Folder> will be monitored for changes and new files that appear automatically appended and deleted files will be automatically removed

!!}
procedure TImageEnFolderMView.SetAutoRefresh(const Value: Boolean);
begin
  if Value <> fAutoRefresh then
  begin
    fAutoRefresh := Value;
    if fAutoRefresh then
      StartMonitoring
    else
      StopMonitoring;
  end;
end;

// Enable monitoring for changes in the current folder
procedure TImageEnFolderMView.StartMonitoring;
begin
  if fFolder = '' then
    exit;

  if not assigned(fFolderMonitor) then
  begin
    fFolderMonitor := TIEFolderWatch.Create;
    fFolderMonitor.OnNotify := OnFolderMonitorNotify;
    fFolderMonitor.WatchSubTree := False;
    fFolderMonitor.WatchOptions := [woFileName, woFolderName];
    fFolderMonitor.WatchActions := [waAdded, waRemoved, waRenamedOld, waRenamedNew];
  end;

  fFolderMonitor.Path := GetFirstFolder;
  fFolderMonitor.Start;
end;


// Disable monitoring for changes in the current folder
procedure TImageEnFolderMView.StopMonitoring;
begin
  if assigned(fFolderMonitor) then
    fFolderMonitor.Stop;
end;


procedure TImageEnFolderMView.OnFolderMonitorNotify(const Sender: TObject; const Action: TWatchAction; const FileName: string);
begin
  case Action of
    waAdded, waRenamedNew   : AppendFile(Filename, False, True); 
    waRemoved, waRenamedOld : RemoveFile(Filename);
  end;
end;


procedure TImageEnFolderMView.CheckForPendingRefresh;
begin
  if fNeedRefreshFileList then
    RefreshFileList
  else
  if fNeedRefreshSorting then
    RefreshSorting;
end;

{!!
<FS>TImageEnFolderMView.MoveFilesToCurrentFolder

<FM>Declaration<FC>
function MoveFilesToCurrentFolder(ssFilenames : TStrings) : Boolean;

<FM>Description<FN>
Moves the specified files to the current <A TImageEnFolderMView.Folder> and refreshes the grid.

Result is False if the action failed or was aborted.

Progress, confirmation and other options are controlled by <A TImageEnFolderMView.FileOperationOptions>.

!!}
function TImageEnFolderMView.MoveFilesToCurrentFolder(ssFiles : TStrings) : Boolean;
begin
  Result := False;
  if (fFolder = '') or (ssFiles.Count = 0) then
    exit;

  Result := WindowsMove(Handle,
                        ssFiles,
                        GetFirstFolder,
                        ieioRenameOnCollision in fFileOperationOptions,
                        ieioShowConfirmation in fFileOperationOptions,
                        ieioShowProgress in fFileOperationOptions,
                        ieioVerboseErrors in fFileOperationOptions);

  if fAutoRefresh = False then
  begin
    if ieioRenameOnCollision in fFileOperationOptions then
      RefreshFileList
    else
      // Add new files in folder
      CheckIfFilesInFolder(ssFiles);
  end;
end;


{!!
<FS>TImageEnFolderMView.CopyFilesToCurrentFolder

<FM>Declaration<FC>
function CopyFilesToCurrentFolder(ssFilenames : TStrings) : Boolean;

<FM>Description<FN>
Copies the specified files to the current <A TImageEnFolderMView.Folder> and refreshes the grid.

Result is False if the action failed or was aborted.

Progress, confirmation and other options are controlled by <A TImageEnFolderMView.FileOperationOptions>.

!!}
function TImageEnFolderMView.CopyFilesToCurrentFolder(ssFiles : TStrings) : Boolean;
begin
  Result := False;
  if (fFolder = '') or (ssFiles.Count = 0) then
    exit;

  Result := WindowsCopy(Handle,
                        ssFiles,
                        fFolder,
                        ieioRenameOnCollision in fFileOperationOptions,
                        ieioShowConfirmation in fFileOperationOptions,
                        ieioShowProgress in fFileOperationOptions,
                        ieioVerboseErrors in fFileOperationOptions);

  if fAutoRefresh = False then
  begin
    if ieioRenameOnCollision in fFileOperationOptions then
      RefreshFileList
    else
      // Add new files in folder
      CheckIfFilesInFolder(ssFiles);
  end;
end;


{!!
<FS>TImageEnFolderMView.MoveSelectedFilesToFolder

<FM>Declaration<FC>
function MoveSelectedFilesToFolder(const sDestFolder : string) : Boolean;

<FM>Description<FN>
Moves the selected files from the current folder to <FC>sDestFolder<FN> (and removes them from the grid)

Result is False if the action failed or was aborted.

Progress, confirmation and other options are controlled by <A TImageEnFolderMView.FileOperationOptions>.

!!}
function TImageEnFolderMView.MoveSelectedFilesToFolder(const sDestFolder : string) : Boolean;
var
  ssFiles: TStringList;
begin                     
  Result := False;
  if fFolder = '' then
    exit;

  ssFiles := TStringList.create;
  try
    if SelectedFilenames(ssFiles) = 0 then
      exit;

    Result := WindowsMove(Handle,
                          ssFiles,
                          sDestFolder,
                          ieioRenameOnCollision in fFileOperationOptions,
                          ieioShowConfirmation in fFileOperationOptions,
                          ieioShowProgress in fFileOperationOptions,
                          ieioVerboseErrors in fFileOperationOptions);

    if fAutoRefresh = False then
      CheckIfFilesDeletedFromFolder(ssFiles);
  finally
    ssFiles.Free;
  end;
end;

{!!
<FS>TImageEnFolderMView.CopySelectedFilesToFolder

<FM>Declaration<FC>
function CopySelectedFilesToFolder(const sDestFolder : string) : Boolean;

<FM>Description<FN>
Copies the selected files from the current folder to <FC>sDestFolder<FN>

Result is False if the action failed or was aborted.

Progress, confirmation and other options are controlled by <A TImageEnFolderMView.FileOperationOptions>.

!!}
function TImageEnFolderMView.CopySelectedFilesToFolder(const sDestFolder : string) : Boolean;
var
  ssFiles: TStringList;
begin                
  Result := False;
  if fFolder = '' then
    exit;

  ssFiles := TStringList.create;
  try
    if SelectedFilenames(ssFiles) = 0 then
      exit;

    Result := WindowsCopy(Handle,
                          ssFiles,
                          sDestFolder,
                          ieioRenameOnCollision in fFileOperationOptions,
                          ieioShowConfirmation in fFileOperationOptions,
                          ieioShowProgress in fFileOperationOptions,
                          ieioVerboseErrors in fFileOperationOptions);

  finally
    ssFiles.Free;
  end;
end;

{!!
<FS>TImageEnFolderMView.DeleteSelectedFilesFromFolder

<FM>Declaration<FC>
function DeleteSelectedFilesFromFolder : Boolean;

<FM>Description<FN>
Deletes the selected files from the current folder. Do not confuse this method with <A TImageEnMView.DeleteSelectedImages> the files are deleted from the system.

Result is False if the action failed or was aborted.

If <FC>ieioSendToRecycleBin<FN> is specified for <A TImageEnFolderMView.FileOperationOptions> the files are moved to the Recycle Bin (though this will depend on the user's settings in Windows). Otherwise they are permanently deleted.
       
<FM>Example<FC>
// Send selected file to Recycle Bin  
IEFolderMView1.FileOperationOptions := FileOperationOptions + [ieioSendToRecycleBin];
IEFolderMView1.DeleteSelectedFilesFromFolder;

// Permanently delete the selected file with confirmation
IEFolderMView1.FileOperationOptions := FileOperationOptions - [ieioSendToRecycleBin] + [ieioShowConfirmation];
IEFolderMView1.DeleteSelectedFilesFromFolder;
!!}
function TImageEnFolderMView.DeleteSelectedFilesFromFolder : Boolean;
begin
  Result := DeleteSelectedFilesFromFolderEx(False);
end;


function TImageEnFolderMView.DeleteSelectedFilesFromFolderEx(bForcePermanentDelete : Boolean) : Boolean;
var
  ssFiles: TStringList;
  WasFileOperationOptions: TIEFileOperationOptions;
begin    
  Result := False;
  if fFolder = '' then
    exit;

  WasFileOperationOptions := FileOperationOptions;
  if bForcePermanentDelete then
    FileOperationOptions := FileOperationOptions - [ieioSendToRecycleBin] + [ieioShowConfirmation];

  ssFiles := TStringList.create;
  try
    if SelectedFilenames(ssFiles) = 0 then
      exit;

    Result := WindowsErase(Handle,
                           ssFiles,
                           ieioSendToRecycleBin in fFileOperationOptions,
                           ieioShowConfirmation in fFileOperationOptions,
                           ieioShowProgress in fFileOperationOptions,
                           ieioVerboseErrors in fFileOperationOptions);

    if fAutoRefresh = False then
      CheckIfFilesDeletedFromFolder(ssFiles);

  finally
    ssFiles.Free;
    if bForcePermanentDelete then
      FileOperationOptions := WasFileOperationOptions;
  end;
end;


{!!
<FS>TImageEnFolderMView.CutSelectedFilesToClipboard

<FM>Declaration<FC>
function CutSelectedFilesToClipboard : Integer;

<FM>Description<FN>
Cuts all filenames of selected files to the clipboard. These files can be pasted in Windows Explorer or using <A TImageEnFolderMView.PasteFilesFromClipboard>

Result is the count of files added to the clipboard.

Note:
- This is best used in combination with <A TImageEnFolderMView.AutoRefresh>
- An exception will be raised if an error occurs
- TImageEnFolderMView does not change the visual style of cut files
!!}
function TImageEnFolderMView.CutSelectedFilesToClipboard : Integer;
var
  ssFilenames : TStringList;
begin
  Result := 0;        
  ssFilenames := TStringList.create;
  try                                  
    if SelectedFilenames(ssFilenames) = 0 then
      exit;
    if iexWindowsFunctions.CutFilesToClipboard(Handle, ssFilenames) then
      Result := ssFilenames.Count;
  finally
    ssFilenames.free;
  end;
end;


{!!
<FS>TImageEnFolderMView.CopySelectedFilesToClipboard

<FM>Declaration<FC>
function CopySelectedFilesToClipboard : Integer;

<FM>Description<FN>
Copies all filenames of selected files to the clipboard. These files can be pasted in Windows Explorer or using <A TImageEnFolderMView.PasteFilesFromClipboard>

Result is the count of files added to the clipboard.

Note: An exception will be raised if an error occurs
!!}
function TImageEnFolderMView.CopySelectedFilesToClipboard : Integer;
var
  ssFilenames : TStringList;
begin
  Result := 0;             
  ssFilenames := TStringList.create;
  try                                  
    if SelectedFilenames(ssFilenames) = 0 then
      exit;
    if iexWindowsFunctions.CopyFilesToClipboard(Handle, ssFilenames) then
      Result := ssFilenames.Count;
  finally
    ssFilenames.free;
  end;
end;



{!!
<FS>TImageEnFolderMView.PasteFilesFromClipboard

<FM>Declaration<FC>
function PasteFilesFromClipboard : Boolean;

<FM>Description<FN>
Copies any files found on the clipboard to the current folder. Files can be copied using Windows Explorer or <A TImageEnFolderMView.CopySelectedFilesToClipboard>.

Result is False if the action failed or was aborted.

Notes:
- You do NOT need to call <A TImageEnFolderMView.CanPasteFilesFromClipboard> before pasting, but <A TImageEnFolderMView.CanPasteFilesFromClipboard> is useful to enable your Paste controls
- Progress, confirmation and other options are controlled by <A TImageEnFolderMView.FileOperationOptions>.

!!}
function TImageEnFolderMView.PasteFilesFromClipboard : Boolean;
var
  ssFilenames : TStringList;
  bMoveFiles : Boolean;
begin
  ssFilenames := TStringList.create;  
  try
    iexWindowsFunctions.PasteFilesFromClipboard(Handle, ssFilenames, bMoveFiles);

    if bMoveFiles then
    begin
      // PASTE FROM CUT
      Result := MoveFilesToCurrentFolder(ssFilenames);

      // Clear from clipboard
      if OpenClipboard(Handle) then
      begin
        EmptyClipboard();
        CloseClipboard;
      end;
    end
    else
    begin
      // PASTE FROM COPY
      Result := CopyFilesToCurrentFolder(ssFilenames);
    end;
  finally
    ssFilenames.free;
  end;
end;

{!!
<FS>TImageEnFolderMView.CanPasteFilesFromClipboard

<FM>Declaration<FC>
function CanPasteFilesFromClipboard : Boolean;

<FM>Description<FN>
Returns true if there are files on the clipboard that can be pasted into the control.

See also: <A TImageEnFolderMView.PasteFilesFromClipboard>

<FM>Example<FC>
// Enable our Paste button
btnPaste.Enabled := IEFolderMView1.CanPasteFilesFromClipboard;

!!}
function TImageEnFolderMView.CanPasteFilesFromClipboard : Boolean;
begin
  Result := iexWindowsFunctions.CanPasteFilesFromClipboard(Handle);
end;

procedure TImageEnFolderMView.DropFiles(Sender: TObject; ssFiles : TStrings; dwEffect: Integer);
begin
  case dwEffect of
    DROPEFFECT_MOVE : MoveFilesToCurrentFolder(ssFiles);
    DROPEFFECT_COPY : CopyFilesToCurrentFolder(ssFiles);
  end;
end;

// Check if any of the specified files now exist in the current folder. Append them if they do
procedure TImageEnFolderMView.CheckIfFilesInFolder(ssFilenames : TStrings);
var
  I: Integer;
  sFilename: string;
begin
  for I := 0 to ssFilenames.Count - 1 do
  begin
    sFilename := IncludeTrailingBackslash(GetFirstFolder) + ExtractFilename(ssFilenames[i]);
    if (FileExists(sFilename) or DirectoryExists(sFilename)) and
       (FilenameToIndex(sFilename) = -1) then
      AppendFile(sFilename, True, True);
  end;
end;

// Check if any of the specified files no longer exist in the current folder. Remove them if they don't
procedure TImageEnFolderMView.CheckIfFilesDeletedFromFolder(ssFilenames : TStrings);
var
  I: Integer;
  sFilename: string;
begin
  for I := 0 to ssFilenames.Count - 1 do
  begin
    sFilename := ssFilenames[i];
    if (FileExists(sFilename) or DirectoryExists(sFilename)) = False then
      RemoveFile(sFilename);
  end;
end;

procedure TImageEnFolderMView.GetSaveSnapshotParameters(Sender: TObject; Stream: TStream; Version : Byte);
begin
  // ExclusionMask
  IESaveStringToStream(Stream, AnsiString(fExclusionMask));

  // FileTypes
  Stream.Write(fFileTypes, SizeOf(TIEFolderFileTypes));
                
  // FileTypesMask
  IESaveStringToStream(Stream, AnsiString(fFileTypesMask));
  
  // Folder
  IESaveStringToStream(Stream, AnsiString(fFolder));

  // SortAscending
  Stream.Write(fSortAscending, SizeOf(Boolean));

  // SortCaseSensitive
  Stream.Write(fSortCaseSensitive, SizeOf(Boolean));

  // SortOrder
  Stream.Write(fSortOrder, SizeOf(TIEImageEnMViewSortBy));

  // ShowFolders
  Stream.Write(fShowFolders, SizeOf(Boolean));

  // ShowHiddenFiles
  Stream.Write(fShowHiddenFiles, SizeOf(Boolean));
end;

procedure TImageEnFolderMView.SaveSnapshot(Stream: TStream; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False);
begin
  SaveSnapshotEx(Stream, SaveCache, Compressed, SaveParams, GetSaveSnapshotParameters);
end;

procedure TImageEnFolderMView.SaveSnapshot(FileName: WideString; SaveCache: Boolean = True; Compressed: Boolean = False; SaveParams: Boolean = False);
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmCreate);
  try
    SaveSnapshotEx(fs, SaveCache, Compressed, SaveParams, GetSaveSnapshotParameters);
  finally
    fs.Free;
  end;
end;

procedure TImageEnFolderMView.SetLoadSnapshotParameters(Sender: TObject; Stream: TStream; Version : Byte);
var
  s: AnsiString;
begin
  if Version >= 7 then
  begin
    // ExclusionMask
    IELoadStringFromStream(Stream, s);
    fExclusionMask := string(s);

    // FileTypes
    Stream.Read(fFileTypes, SizeOf(TIEFolderFileTypes));
               
    // FileTypesMask
    IELoadStringFromStream(Stream, s);
    fFileTypesMask := string(s);
  
    // Folder
    IELoadStringFromStream(Stream, s);
    fFolder := string(s);

    // SortAscending
    Stream.Read(fSortAscending, SizeOf(Boolean));

    // SortCaseSensitive
    Stream.Read(fSortCaseSensitive, SizeOf(Boolean));

    // SortOrder
    Stream.Read(fSortOrder, SizeOf(TIEImageEnMViewSortBy));

    // ShowFolders
    Stream.Read(fShowFolders, SizeOf(Boolean));
                 
    // ShowHiddenFiles
    Stream.Read(fShowHiddenFiles, SizeOf(Boolean));
  end;
end;

function TImageEnFolderMView.LoadSnapshot(Stream: TStream): Boolean;
begin
  result := LoadSnapshotEx(Stream, SetLoadSnapshotParameters);
end;

function TImageEnFolderMView.LoadSnapshot(FileName: WideString): Boolean; 
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := LoadSnapshotEx(fs, SetLoadSnapshotParameters);
  finally
    fs.Free;
  end;
end;


{$ELSE} // {$ifdef IEINCLUDEMULTIVIEW}

interface
implementation

{$ENDIF} 

end.
