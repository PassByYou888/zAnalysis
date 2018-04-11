(*
File version 1040
Doc revision 1004
*)

// This unit contains only documentation that is not part of class, methods or properties.
// It is not linked or included in packages.

unit iedocum;
interface
implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
INDEX
<AC><FT>ImageEn 5.2.0
<FS>Imaging Components

<FN>Copyright (c) 1998-2014 by Carlotta Calandra. All rights reserved.
Copyright (c) 2011-2014 by Xequte Software.

<L http://www.imageen.com>www.imageen.com</L>

This software comes without express or implied warranty.
In no case shall the author be liable for any damage or unwanted behavior of any computer hardware and/or software.
Author grants you the right to include the compiled component in your application, whether COMMERCIAL, SHAREWARE or FREEWARE.
ImageEn may not be included in any commercial, shareware or freeware "libraries" or "components".

THE SOFTWARE IS NOT INTENDED FOR USE IN THE OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION, COMMUNICATIONS SYSTEMS, OR AIR TRAFFIC CONTROL OR SIMILAR ACTIVITIES IN WHICH CASE THE FAILURE OF THE SOFTWARE COULD LEAD TO DEATH, PERSONAL INJURY, OR SEVERE PHYSICAL OR ENVIRONMENTAL DAMAGE.
No Rental. Customer may not rent or lease the SOFTWARE to someone else.
The SOFTWARE is protected by copyright laws and international treaty provisions.
Accordingly, Customer is required to treat the SOFTWARE like any other copyrighted material, except as otherwise allowed pursuant to this LICENSE and that it may make one copy of the SOFTWARE solely for backup or archive purposes.

ImageEn is not certified as a commercial medical device (FDA or CE-1) for primary diagnostic imaging.



<FB>C R E D I T S<FN>


Bill Miller for for demos, suggestions and bug fixes

Nitin Chandra
<L mailto://nitinchandra@airtelbroadband.in>nitinchandra@airtelbroadband.in</L>
<L mailto://nitinchandra@yahoo.com>nitinchandra@yahoo.com</L>

Sebastian Zierer
<L mailto://mail@zierer.info>mail@zierer.info</L>
<L http://www.songbeamer.com>www.songbeamer.com</L>

<FB>Redeye Removal Algorithm<FN>
"Redeye removal" algorithm developed by Valentim Batista
<L mailto://timsara@softhome.net>timsara@softhome.net</L>

<FB>Extra Transitions<FN>
Based on "TCustomPicShow" by Kambiz R. Khojasteh
<L mailto://kambiz@delphiarea.com>kambiz@delphiarea.com</L>
                 
<FB>Folder Monitoring<FN>
Based on “Directory Watch” by Iztok Kacin.
<L http://www.cromis.net/blog/downloads/directory-watch/>www.cromis.net</L>

<FB>Little cms<FN>
Copyright (C) 1998-2004 Marti Maria

<FB>JPEG<FN>
This software is based in part on the work of the Independent JPEG Group

<FB>PNG<FN>
PNG Development Group
Copyright (c) 1995, 1996 Guy Eric Schalnat, Group 42, Inc.
Copyright (c) 1996, 1997 Andreas Dilger
Copyright (c) 1998, 1999 Glenn Randers-Pehrson

<FB>JPEG2000<FN>
This software is based in part on JasPer library
Copyright (c) 1999-2000, Image Power, Inc. and the University of British - Columbia, Canada.
Copyright (c) 2001-2002 Michael David Adams.

<FB>IEVision is based on<FN>
OpenCV (Open Source Computer Vision, <L http://opencv.willowgarage.com>opencv.willowgarage.com</L>)
Lapack (Linear Algebra PACKage, <L http://www.netlib.org/lapack>www.netlib.org/lapack</L>)
Boost C++ libraries (<L http://www.boost.org>www.boost.org</L>)
Tesseract (<L http://code.google.com/p/tesseract-ocr>code.google.com/p/tesseract-ocr</L>)
ZBar Copyright 2007-2009 (c) Jeff Brown - spadix@users.sourceforge.net

<FB>Some improvements in _IEGetHistogram, _CopyDIB2BitmapEx and _IEGetHistogram<FN>
Michal Smiechowski, DSA Polska Sp. z o.o.

!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>What's New

<FM>VERSION 5.2.0

<FM>New Features<FN>
- Media Foundation video capture: now can automatically rotate images
- Added: Experimental version of VCL Theme support
- Added: GetExifThumbnail property to TOpenImageEnImageDialog to disable thumbnail retrieval via EXIF
- Added: Now TImageEnProc.Random supports black/white images
- Added: OnAddImage event for TImageEnFolderMView so addition of certain files can be skipped
- Added: Glyphs in dialogs can be disabled using IEGlobalSettings().UseButtonGlyphsInDialogs
- Added: Icon caching in TImageEnMView for better performance
- Added: TImageEnFolderMView can now display content of multiple folders
- Added: AppendImage and InsertImage functions of TImageEnView support remote images (HTTP and FTP)
- Added: Checkboxes for thumbnails can now be displayed only when hovering over the mouse
- Added: now TIFF reader supports RGB48 tiled images
- Added: TIFF, now CMYK writing supports alpha channel
- Added: Auto calculates font size for ruler of TImageEnVect
- Added: TImageEnMView supports gestures
- Improved DICOM reading speed and memory requirements
- Added: now DICOM decoder can read multiple sequence items (subtags)
- Now Twain selection dialog is modal (cannot lose focus)
- Added: TImageEnProc.ConvertTo can reduce number of colors using Floyd-Steinberg dithering algorithm
- Added: now calling TImageEnIO.Acquire or TImageEnMIO.Acquire with ShowSettingsOnly = True, will return True if user presses Ok or Save
- Added: now LayersMerge and LayersMergeTo supports Cropping
- Added: now TIFF reader supports RGB color-mapped images with PlanarConfiguration = 2
- Added: Replaced EXIF helper unit, iexEXIFRoutines.pas and IPTC helper unit, iexIPTCRoutines.pas, with much improved iexMetaHelpers.pas
- Added: now TImageEnDBVect can save annotations inside TIFFs without stream headers (readable by other apps)
- Added: optimized CMYK to RGB conversion speed
- Added: Now supports the most recent official list of nearly 5,000 Dicom tags
- Added: now TIEDirectShow.Run() returns a result code (success, generic error, device busy)
- Added: Mouse wheel effect can now be controlled for TImageEnMView using MouseWheelParams property
- Added: TImageEnProc.ConvertTo overload with several dithering types and palette options
- Added: four resampling filters from Windows WIC, NearestNeighbor, Linear, Cubic and Fant
- Added: IO and MIO LoadFromFile methods now include parameter to support unknown formats
- Added: TImageEnMView provides better visual indication when disabled

<FM>Bug Fixes<FN>
- Fixed: TImageEnVect, highlighted-box is not painted in CopyObjectsToBack
- Fixed: New objects in TImageEnVect may not be initialized with valid values
- Fixed: TImageEnMView.OnDestroyImage may not be called
- Fixed: memory leak in TImageEnMView loading not valid 256x256 icons
- Fixed: TImageEnVect.CopyObjectsToBack cuts rotated text
- Fixed: error loading some ZIP encoded TIFFs
- Fixed: Nested callback functions not working in 64bit
- Fixed: TImageEnVect, when ObjTextAutoSize = True, iekTEXT could not be displayed correctly
- Added: Some integers casts were not valid for 64bit
- Fixed: corruption of EXIF Maker Notes
- Fixed: DICOM reader can correctly detect root images
- Fixed: DICOM/JPEG2000 9..15 bit gray scale images were not read correctly

<FM>Changes to Classes<FN>
- Added: TIEMediaFoundationSourceReader.VideoProcessor property
- Added: TIEMediaFoundationVideoProcessor class
- Added: TIEMediaFoundationSourceReader.SamplesBufferSize property
- Fixed: Resampling using Maintain Aspect Ratio may create invalid sized image
- Added: TImageEnProc.SelCutToClip, CutAlpha parameter
- Added: New parameter for <A TImageEnMView.OnPlayFrame> to allow skipping play of certain frames
- Added: TImageEnMView.OnImageEnGesture event
- Added: TImageEnMView.Gestures property
- Added: TIPDialogParams supports Horz and Vert flipping
- Added: TImageEnMView loading multi files on demand supports unknown extensions
- Added: TIOParamsVals.DICOM_RescaleIntercept property
- Added: TIOParamsVals.DICOM_RescaleSlope property
- Added: TIEImageEnGlobalSettings.ReleaseTwainResources property
- Added: TIEImageEnGlobalSettings.ModelessSelectTwainSource property
- Added: TIEImageEnGlobalSettings.IsInsideTwain property
- Added: TIEDitherMethod ieDithering value
- Added: IEExtToFileFormat now supports WMV and MPEG files
- Added: TImageEnView.LayersMerge overload (to merge a list of layers)
- Added: TIELayer.GetLayerMask method
- Added: TIELayer.GetIndex method
- Added: TImageEnMView.MultiSelectedImagesList property
- Added: TIEBitmap.DrawToCanvasWithAlpha method
- Changed: TImageEnView.Assign will copy all layers and vect objects
- Changed: TImageEnView.Assign also supports TIEBitmap as a source
- Added: TImageEnMView.SelectedFontColor
- Removed: TImageEnMView.HighlightColor
- Removed: TImageEnMView.HighlightTextColor
- Added: All TImageEnMIO.SaveTo* methods include OnlySelected parameter
- Added: Helper method, GetFileExtensionsOfType
- Added: ifTIFFwAnnot to TDataFieldImageFormat
- Added: now TImageEnView.Assign accepts TIEBitmap as parameter
- Added: TImageEnProc.CheckLegacyBitmap method
- Added: TResampleFilter, new resampling filters rfWICNearestNeighbor, rfWICLinear, rfWICCubic, rfWICFant
- Added: TImageEnView.VisibleBitmapRect to return the visible area of the image
- Added: TIEImageEnGestureEvent.Handled parameter
- Removed: TImageEnMView.SelectionStyle

<FM>Changes to Method Parameters<FN>
- TIEDICOMTags.AddTag and TIEDICOMTags.GetTagChilds

<FM>Compatibility Issues<FN>
- Checkbox properties of TImageEnMView have changed. See <A TImageEnMView.Checkboxes> and <A TImageEnMView.CheckboxPos>
- Parameters have changed for <A TImageEnMView.OnPlayFrame>
- Now when calling TImageEnView.Assign and the source is a TImageEnView it will copy all layers. If it is a TImageEnVect it will also copy all objects
- TImageEnMView.HighlightColor has been removed, use TImageEnMView.ThumbnailsBackgroundSelected (Note: Default of HighlightColor was clHighlight, whereas ThumbnailsBackgroundSelected defaults to clBtnFace)
- TImageEnMView.HighlightTextColor has been removed, use TImageEnMView.SelectedFontColor  (Note: Default of HighlightColor was clHighlightText, whereas ThumbnailsBackgroundSelected defaults to clNone)
- Applications calling TIEDICOMTags.AddTag and TIEDICOMTags.GetTagChilds must work with TObjectList as child object
- EXIF helper unit, iexEXIFRoutines.pas, and IPTC helper unit, iexIPTCRoutines.pas, are no longer included, by default. Either use the new iexMetaHelpers.pas or the access the old units in \Source\Legacy\
- Dicom helper unit, iexDicomRoutines is no longer included, by default. Either use the updated array and relevant functions in ieDicom or the access the old units in \Source\Legacy\
- The TImageEnMView.DisplayImageRect method has been removed, set the TImageEnView.VisibleBitmapRect property instead
- TImageEnMView.SelectionStyle has been removed. Use ThumbnailsBackgroundSelected instead
- No longer use integers to specify channels for TImageEnProc.ShiftChannel, use iecRed for 2, iecGreen for 1 and iecBlue for 0

View complete history on <L http://www.imageen.com/info/HistoryFull.html>www.imageen.com</L>

!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS><AC>License agreement

<FN>
All users of the ImageEn/IEvolution/ImageEn ActiveX/IEVision components should agree with the following Licence agreement statements:

1. LEGAL NOTICE
This License Agreement constitutes a valid and binding agreement between you and author for the use of the software named ImageEn/IEvolution/ImageEn ActiveX/IEVision.
You are required to read this Agreement and agree to all of its terms and conditions in order to download and use the software.

2. LICENSE & USAGE
You are granted a License to use this Software product without royalties.
Modify, Translate or Create derivative components/libraries from the Software are prohibited!
Author grants you the right to include the compiled component in your application, whether COMMERCIAL, SHAREWARE or FREEWARE.
The Software may not be included in any commercial, shareware or freeware "libraries" or "components".
THE SOFTWARE IS NOT INTENDED FOR USE IN THE OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION, COMMUNICATIONS SYSTEMS,
OR AIR TRAFFIC CONTROL OR SIMILAR ACTIVITIES IN WHICH CASE THE FAILURE OF THE SOFTWARE COULD LEAD TO DEATH, PERSONAL
INJURY, OR SEVERE PHYSICAL OR ENVIRONMENTAL DAMAGE.

3. DISCLAIMER OF WARRANTY
This software is provided "as is" without express or implied warranties, including warranties of merchantability and
fitness for a particular purpose or non infringement. Author shall not be liable under any theory or any damages
suffered by you or any user of the software. In no event will the author be liable for any loss of information, damage to
computer or monitor, any incidental, special, indirect or similar damages, or consequential damages (including damages for
loss of business profits, business interruption, loss of business information and the like) arising out of the use of or
inability to use the software or its documentation even if the author has been advised and warned of the possibility of
such damages.
In no event does Author warrant that the Software is error free or that Customer will be able to operate the
Software without problems or interruptions.
This warranty does not apply if the sofware has been altered, except by Author, has not been installed, operated,
repaired, or maintained in accordance with instructions supplied by Author, has been subjected to abnormal physical
or electrical stress, misuse, negligence, or accident, or is used in ultra-hazardous activities.

4. COPYRIGHT
ImageEn/IEvolution/ImageEn ActiveX/IEVision is protected by copyright laws and international treaty provisions.
You acknowledge that no title to the intellectual property of ImageEn/IEvolution/ImageEn ActiveX/IEVision is transferred to you.
You further acknowledge that title and full ownership rights to ImageEn/IEvolution/ImageEn ActiveX/IEVision will remain the exclusive property
of Author and you will not acquire any rights to ImageEn/IEvolution/ImageEn ActiveX/IEVision except as expressly set forth in this license.
No Rental. Customer may not rent or lease the SOFTWARE to someone else.
All title and copyrights in and to the SOFTWARE (including but not limited to all images, photographs, animations, video,
audio, music, text, and other information incorporated into the SOFTWARE), the accompanying printed materials, and any
copies of the SOFTWARE, are owned by Author.
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
CONTENTS
Introduction
 ImageEn=MAININDEX
 Features=Features
 Common Usage Scenarios=Common Usage Scenarios
 ImageEn Hierarchy=ImageEn Hierarchy
 What's New=What's New
 Helper Functions=Helper functions
 Global Settings=TIEImageEnGlobalSettings
 Color Management System=Color Management System
 64bit Support and DLLs=64bit Support and DLLs
 Supported RAW Camera Formats=List of supported Camera RAW formats
 Language Support=Language support
 Demos=Demos
 Manual Installation=Installation
 FAQ=FAQ
 License Agreement=License agreement
Display
 TImageEnView Component=TImageEnView
 TImageEnVect Component=TImageEnVect
 TImageEnMView Component=TImageEnMView
 TImageEnFolderMView Component=TImageEnFolderMView
Image Modification
 TImageEnProc Component=TImageEnProc
 TIEICC Class=TIEICC
 Lossless Jpeg Transformations= Lossless Jpeg Transformations
Input/Output
 TImageEnIO Component=TImageEnIO
 TImageEnMIO Component=TImageEnMIO
 TOpenImageEnDialog Component=TOpenImageEnDialog
 TSaveImageEnDialog Component=TSaveImageEnDialog
 TIOParamsVals Class=TIOParamsVals
 TIEHashStream Class=TIEHashStream
 TIEResourceExtractor Class=TIEResourceExtractor
 TIETIFFHandler Class=TIETIFFHandler
 TIETagsHandler Class=TIETagsHandler
 TIEWICReader Class=TIEWICReader
 TIEWICWriter Class=TIEWICWriter
 TIERFBClient Class=TIERFBClient
 Custom File Format Support=Custom file format support
 Multipage Tiff/Gif Functions=Multipage Tiff-Gif functions
Capture
 TIEAcquireParams Class=TIEAcquireParams
 TIETwainParams Class=TIETwainParams
 TIEWia Class=TIEWia
 TIEMediaFoundationSourceReader Class=TIEMediaFoundationSourceReader
 TIEDirectShow Class=TIEDirectShow
 TIEMediaReader Class=TIEMediaReader
 TImageEnVideoView Component=TImageEnVideoView
 TImageEnVideoCap Component=TImageEnVideoCap
Database
 TImageEnDBView Component=TImageEnDBView
 TImageEnDBVect Component=TImageEnDBVect
Actions
 TImageEnView Actions=TImageEnView Actions
 TImageEnVect Actions=TImageEnVect Actions
 TImageEnMView Actions=TImageEnMView Actions
 TImageEnFolderMView Actions=TImageEnFolderMView Actions
Other Components
 THSVBox Component=THSVBox
 TRulerBox Component=TRulerBox
 TIEGradientBar Component=TIEGradientBar
 THistogramBox Component=THistogramBox
Other Classes
 TIEBitmap Class=TIEBitmap
 TIEMask Class=TIEMask
 TIEAnimation Class=TIEAnimation
 TIEHorizontalFlow Class=TIEHorizontalFlow
 TIECircularFlow Class=TIECircularFlow
 TIEImageList Class=TIEImageList
 TIESlippyMap Class=TIESlippyMap
Vision classes
 Introduction=IEVision
 What's New=IEVision What's New
 Distribution=Distributing IEVision
 Helper Functions=IEVision Helper Functions
 TIEVisionLibrary Class=TIEVisionLibrary
 TIEVisionImage Class=TIEVisionImage
 TIEVisionMath Class=TIEVisionMath
 TIEVisionOCR Class=TIEVisionOCR
 TIEVisionObjectsFinder Class=TIEVisionObjectsFinder
 TIEVisionObjectTracker Class=TIEVisionObjectTracker
 TIEVisionDrawing Class=TIEVisionDrawing
 TIEVisionBarCodeScanner Class=TIEVisionBarCodeScanner
 TIEVisionPeopleDetector Class=TIEVisionPeopleDetector
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Features

<FM>File Operations<FN>
  • Asynchronous loading and saving
  • Lossless JPEG rotation and cropping
  • Support for Color Management Systems to process ICC profiles
  • Loading and saving of digital camera (EXIF) fields in JPEG, TIFF, RAW and HD Photo files (without modifying the original image)
  • Loading and saving of EXIF GPS fields
  • Loading and saving of Adobe XMP fields from JPEG, TIFF, HD Photo and PSD/PSB files
  • Raw camera support including access to internal thumbnails and other data fields
  • Loading and saving of IPTC data (most commonly used by Photoshop) from JPEG and TIFF files (without modifying the original image)
  • Quick reading of image properties (dimensions, color depth, etc) without loading image
  • Image load and save dialogs which include a preview and relevant save settings
  • Support for alpha channel in GIF, TIFF, PNG, ICO, CUR, TGA, PSD/PSB files
  • Load images directly from the internet (using http/ftp protocol)
  • Encryption and decryption using a 128 bit key
  • Support for other formats such as PDF, PS, JBIG, etc, via <L IEAutoLoadIOPlugins>plug-ins</L>

<FM>Image Display<FN>
  • Images can be automatically displayed "To-Fit" or with real time zoom. Six quality settings are available to enhance its display (from fastest to best quality)
  • Images from digital cameras that support EXIF rotation data can be automatically displayed with the correct orientation
  • 180 stunning image transition and Pan-Zoom effects
  • Many selection types: rectangle, ellipse, polygon and "magic wand" (to instantly select a colored area) and other options (including selection intensity and feathering)
  • Other mouse interaction options include mouse wheel support and click-dragging to navigate and zoom the image
  • Multilevel undo and redo
  • Support for alpha channel (transparency) and multiple layers (with 37 layer blend modes). All layers can be moved, resized and rotated (programmatically or by the user)
  • Display and navigate images using Coverflow-style animation

<FM>Image Editing and Processing<FN>
  • Image resizing with 11 quality filters including Triangle, Hermite, Bell, BSpline, Lanczos3, Mitchell, FastLinear, Bilinear and Bicubic
  • Many color adjustment facilities including contrast, HSL, HSV channel separation, RGB, histogram equalization, Fast Fourier Transformation (FFT), gamma correction, temperature and noise removal. All can be applied programmatically or using the built-in dialog
  • Image effects including custom filters, bump map, lens, wave, morphing, Gaussian and motion blurring and sharpening. All can be applied programmatically or using the built-in dialog
  • Conversion to gray scale and negative
  • Conversion of color ranges
  • Image cropping, auto-cropping, flipping and rotation with anti-alias
  • Edge and skew detection
  • Red-eye removal
  • Soft shadow and inner shadow effects
  • Supports a wide range of native pixel formats: 1 bit, 8 bit paletted, 8 bit grayscale, 16 bit grayscale, 24 bit RGB, 32 bit float point, 24 bit CMYK, 48 bit RGB, CIELab
 
<FM>Image and Video Acquisition<FN>
  • Acquire images from Twain (modal and modeless) and WIA compatible scanners and cameras
  • Capture from screen
  • Visual and non-visual components for video capture (supporting freeze frames and real-time processing). Supports all installed codecs and video capture cards
  • Video capture and saving of multimedia using DirectShow
 
<FM>Thumbnails<FN>
Powerful thumbnail components that displays a grid of images (e.g. read from a folder or database table):

  • Many style and other customizations, including wallpaper
  • Supports multiple selection
  • Display all frames/pages of: AVI, GIF, TIFF or video files (using Directshow)
  • Very memory efficient and fast with multi-threading background image loading, caching and optional use of embedded thumbnails

<FM>Vectorial Object Component<FN>
Vectorial Editing component that allows you to add and manipulate objects on a background image:

  • Width object support: lines, boxes, circles, ellipses, bitmaps, text (including curved, multi-line and formatted), rulers, polylines, polygons, angles and arrows
  • Many object options including transparency, soft-shadow and anti-alias
  • Save/Load to compressed IEV format and import from AutoCAD DXF files
  • Undo and redo support
  • Measurement of lines, perimeters, areas and angles
  • Full clipboard support
       
<FM>IEVision Advanced Features Plug-In<FN>
<L IEVision>IEVision</L> is an optional plugin for ImageEn which adds advanced features:

  • Face detection (and other parts of the body, including "eye", "eye glasses", "full body", "lower body", "upper body", "face profile", "eye pair", "mouth", "nose")
  • Face and objects tracking
  • Basic OCR
  • Bar code reading (all common formats, including QR Code)
  • Inpainting/Image patching (to hide blemishes and copy content)

<FM>Other Features<FN>
  • Printing of single images and sheets of multiple images, including print preview support
  • Data-aware versions of image and thumbnail components to automatically display files stored as blob or path references in a database table
  • One-click selection of languages for all dialogs with support for: English, Italian, German, Spanish, French, Portuguese, Greek, Russian, Dutch, Swedish, Polish, Japanese, Czech, Finnish, Farsi, Chinese, Danish, Turkish and Hungarian
  • Also works with the standard TImage component and TPicture class

<FM>Supported Formats<FN>
<TABLE>
<R> <H>Format</H> <H>Notes</H> <H>Load</H> <H>Save</H> </R>
<R> <C><FB>JPEG<FN></C> <C>Supports 1/2, 1/4 and 1/8 sub-sizes for fast preview</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>JPEG2000<FN></C> <C></C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>GIF<FN></C> <C>Including editing and display of animated GIFs</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>PNG<FN></C> <C></C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>BMP<FN></C> <C>Compressed and uncompressed</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>TIFF<FN></C> <C>Editing and display of single and multipage TIFF. Also supports FAX, G3F and G3N sub-formats</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>BigTIFF<FN></C> <C>Single and multipage BigTIFF. Also supports FAX, G3F and G3N sub-formats</C> <C>Yes</C> <C>No</C> </R>
<R> <C><FB>PCX<FN></C> <C>Including DCX (Multipage PCX) format</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>Raw Camera Formats<FN></C> <C>Including Digital Negative Format (*.dng), Canon (*.cr2, *.crw), Kodak (*.dcr), Minolta (*.mrw), Nikon (*.nef), Olympus (*.orf), Pentax (*.pef), Fuji (*.raf), Leica (*.raw), Sony (*.srf) and Sigma (*.x3f)</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>Icons (ICO)<FN></C> <C>With multiple resolution and color depth support</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>DICOM (Medical Imaging)<FN></C> <C>Single and multipage. Can compress with RLE, jpeg2000 and lossy-jpeg</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>Adobe Photoshop (PSD/PSB)<FN></C> <C>With multiple layer and large document support</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>WMP<FN></C> <C>Also known as Microsoft HD Photo</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>PostScript (PS and EPS)<FN></C> <C>Single and multipage</C> <C> -</C> <C>Yes</C> </R>
<R> <C><FB>Adobe PDF<FN></C> <C>Single and multipage images</C> <C> -</C> <C>Yes</C> </R>
<R> <C><FB>Metafiles (WMF and EMF)<FN></C> <C></C> <C>Yes</C> <C> -</C> </R>
<R> <C><FB>Cursors (CUR)<FN></C> <C></C> <C>Yes</C> <C> -</C> </R>
<R> <C><FB>AVI<FN></C> <C>Including retrieval and modification of frames</C> <C>Yes</C> <C>Yes</C> </R>
<R> <C><FB>Media formats (MPEG, WMV, etc)<FN></C> <C>Via the Directshow API</C> <C>Yes</C> <C>Yes</C> </R>
</TABLE>

!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Installation<FN>

Generally you should use the ImageEn installer to handle the compilation and installation of the ImageEn packages.  However you can also do it manually as follows...

Note: Before installing ImageEn, remove old versions of it (remove all pkie*.bpl files):
1. In Delphi select Tools -> Options, Library tab.
2. Go through the list of Library paths and check for any that contain ImageEn packages (pkie*.bpl files)
3. Delete any ImageEn packages that you find


COMPILING THE IMAGEEN PACKAGES:

For each version of Delphi/C++ Builder you should have four BPL packages named DPKIECTRL*.BPL, DPKIEDB*.BPL, PKIECTRL*.BPL and PKIEDB*.BPL where * indicates the version, e.g. DPKIECTRL16.BPL for Delphi XE2.

If these were not installed then you can manually compile them as follows:

1. Select Tools -> Options, Library and ensure the ImageEn source files folder is listed in your "Library path"

2. Open and compile PKIECTRL*.DPK, PKIEDB*.DPK, DPKIECTRL*.DPK and DPKIEDB*.DPK (where * is the version number for your Delphi/C++ builder product)


MANUALLY INSTALLING THE IMAGEEN PACKAGES:

If you did not install the ImageEn packages into Delphi/C++ Builder using the installer then you can do it manually as follows:

1. Copy PKIECTRL*.BPL and PKIEDB*.BPL to your Windows "System" folder or Delphi "Bin" folder (where * is the version number for your Delphi/C++ builder product)

Note: The "System" folder is generally "C:\Windows\System32" on a 32bit system or "C:\Windows\SysWOW64\" on a 64bit one.  The Delphi “Bin” folder will be similar to: "C:\Program Files\Embarcadero\Rad Studio\7.0\Bin"

2. Select Component -> Install packages, and press Add button. Then select DPKIECTRL*.BPL and DPKIEDB*.BPL (the latter is for database components).

3. Select Tools -> Options, Library and ensure that the ImageEn source folder is in your "Library path".
!!}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Language support<FN>

To use Resourcestrings English must be enabled using "$DEFINE IESUPPORTENGLISH" or "$DEFINE IEUSESTRINGRESOURCEONLY" in "ie.inc" and <L Installation>recompile your packages</L>.
All other languages can be be disabled in "ie.inc" so the language array is a little bit smaller.

You can specify the language of ImageEn's dialogs and components using IEGlobalSettings().<A TIEImageEnGlobalSettings.MsgLanguage>.

Email support@xequte.com to add/improve translation for your language.

!!}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Demos

<FM>Annotation Demos<FN>
<TABLE>
<R> <C>Add Markers</C> <C><FC>\Demo\Annotations\Marker\<FN></C> <C>Add information markers to an image</C> </R>
<R> <C>Add Spots</C> <C><FC>\Demo\Annotations\Spots\<FN></C> <C>Add and edit circular objects on an image</C> </R>
<R> <C>Curved Text</C> <C><FC>\Demo\Annotations\CurvedText\<FN></C> <C>Draw text that is curved, rotated or with shadow effects</C> </R>
<R> <C>Image Annotations</C> <C><FC>\Demo\Annotations\Annotations\<FN></C> <C>Add text, highlighting and other objects to an image</C> </R>
<R> <C>Magic Selection to Polygon</C> <C><FC>\Demo\Annotations\MagicToPolygon\<FN></C> <C>Convert an irregular selection to a polygon</C> </R>
<R> <C>Formatted Text Annotations</C> <C><FC>\Demo\Annotations\AdvancedText\<FN></C> <C>Burn formatted text into an image</C> </R>
<R> <C>Text Arrows</C> <C><FC>\Demo\Annotations\TextArrow\<FN></C> <C>Add arrows with text to an image</C> </R>
<R> <C>Vectorial Editor</C> <C><FC>\Demo\Annotations\Vectorial\<FN></C> <C>A full demo of <A TImageEnVect> capabilities for creating and editing vector based images</C> </R>
<R> <C>Vectorial with User Data</C> <C><FC> \Demo\Annotations\UserDataVect\<FN></C> <C>Store user data with objects in a <A TImageEnVect></C> </R>
</TABLE>

<FM>Database Demos<FN>
<TABLE>
<R> <C>Database Demo</C> <C><FC>\Demo\Database\DBDemo\<FN></C> <C>Display of an image stored within a database blob field of the active record using <A TImageEnDBView></C> </R>
<R> <C>Database Path Demo</C> <C><FC>\Demo\Database\DBPath\<FN></C> <C>Display of an image referenced by path within a database field of the active record using <A TImageEnDBView></C> </R>
<R> <C>Database Thumbnail Viewer</C> <C><FC>\Demo\Database\DBMView\<FN></C> <C>Displaying thumbnails of all images stored within the blob field of a table using <A TImageEnMView></C> </R>
<R> <C>Database Vectorial Demo</C> <C><FC>\Demo\Database\DBDemoVect\<FN></C> <C>Display of vector files stored within a database blob field using <A TImageEnDBVect></C> </R>
<R> <C>Database Vectorial Stream</C> <C><FC>\Demo\Database\ManualVect\<FN></C> <C>Streamed display of vector files stored within a database blob field using <A TImageEnVect></C> </R>
<R> <C>DBCtrlGrid Demo</C> <C><FC>\Demo\Database\DBCtrl\<FN></C> <C>Display of multiple images stored within a database blob field using <A TImageEnDBView> and TDBCtrlGrid</C> </R>
<R> <C>DBCtrlGrid Path Demo</C> <C><FC>\Demo\Database\DBCtrlPath\<FN></C> <C>Display of multiple images referenced by path within a database field using <A TImageEnDBView> and TDBCtrlGrid</C> </R>
</TABLE>

<FM>Display Demos<FN>
<TABLE>
<R> <C>"Coverflow" Effect</C> <C><FC>\Demo\Display\MviewFlow\<FN></C> <C>Demo of "Coverflow" style effect using a <A TImageEnMView></C> </R>
<R> <C>"Coverflow" Effect (Manual)</C> <C><FC>\Demo\Display\ManualFlow\<FN></C> <C>Demo of "Coverflow" style effect (output to an <A TImageEnView>)</C> </R>
<R> <C>DirectShow Video Player</C> <C><FC>\Demo\Display\VideoPlayer\<FN></C> <C>Video player using DirectShow</C> </R>
<R> <C>Display Adjustment</C> <C><FC>\Demo\Display\DisplayAdjust\<FN></C> <C>Adjust contrast, color and brightness of the displayed image</C> </R>
<R> <C>External Bitmap</C> <C><FC>\Demo\Display\ExternalBitmap\<FN></C> <C>Use of an external bitmap as the source display for <A TImageEnView></C> </R>
<R> <C>Image Comparison</C> <C><FC>\Demo\Display\ImageComp\<FN></C> <C>Compare the content of two image side-by-side</C> </R>
<R> <C>Magnifier 1</C> <C><FC>\Demo\Display\Magnify\<FN></C> <C>Magnify a portion of an image using a second <A TImageEnView></C> </R>
<R> <C>Magnifier 2</C> <C><FC>\Demo\Display\Magnify2\<FN></C> <C>Magnify a portion of an image using a layer</C> </R>
<R> <C>Navigator 1</C> <C><FC>\Demo\Display\Navigator\<FN></C> <C>Use of <A TImageEnView.SetNavigator> to show the currently viewable portion of the image</C> </R>
<R> <C>Navigator 2</C> <C><FC>\Demo\Display\Navigator2\<FN></C> <C>Use of <A TImageEnView.SetNavigator> to show the currently viewable portion of the image</C> </R>
<R> <C>Pan Zoom Effects</C> <C><FC>\Demo\Display\PanZoomEffects\<FN></C> <C>Adding interest to static image with Pan-Zoom ("Ken Burns") effects</C> </R>
<R> <C>Project Draw Effect</C> <C><FC>\Demo\Display\ProjectDraw\<FN></C> <C>Use of <A TImageEnProc.ProjectDraw> for advanced image display effects</C> </R>
<R> <C>Soft-Pan Navigation</C> <C><FC>\Demo\Display\SoftPan\<FN></C> <C>Navigate an image with the cursor (using <A TImageEnView.MouseInteract> = miMovingScroll)</C> </R>
<R> <C>Transition Effects</C> <C><FC>\Demo\Display\Transitions\<FN></C> <C>Use of transition effects to smooth the changing of images during a slideshow</C> </R>
<R> <C>VMR DVD Player</C> <C><FC>\Demo\Display\VMR_DVD\<FN></C> <C>Playback of DVD using DirectShow VMR</C> </R>
<R> <C>VMR Video Player</C> <C><FC>\Demo\Display\VMR_Video\<FN></C> <C>Playback of multimedia using DirectShow VMR</C> </R>
</TABLE>

<FM>Drag and Drop Demos<FN>
<TABLE>
<R> <C>Dragging from TImageEnMView</C> <C><FC>\Demo\DragDrop\TImageEnMView_FileList_2\<FN></C> <C>Extracting images from multiple-image files (such as TIFF, GIF, etc.) using drag and drop</C> </R>
<R> <C>Dragging to TImageEnMView</C> <C><FC>\Demo\DragDrop\TImageEnMView_FileList_1\<FN></C> <C>Adding images to a <A TImageEnMView> using drag and drop</C> </R>
<R> <C>TImageEnMView Drag-Drop</C> <C><FC>\Demo\DragDrop\MultiView2\<FN></C> <C>Dragging of images within a <A TImageEnMView></C> </R>
<R> <C>TImageEnMView Drag-Drop 2</C> <C><FC>\Demo\DragDrop\TImageEnMView_DragDrop\<FN></C> <C>Dragging of images between multiple <A TImageEnMView></C> </R>
</TABLE>

<FM>Full Applications<FN>
<TABLE>
<R> <C>Air Photo SE</C> <C><FC>\Demo\FullApps\AirSE\<FN></C> <C>Full application for working with very large (e.g. 15,000 x 15,000 pixel) aerial photos (with multi processor support)</C> </R>
<R> <C>Batch Converter</C> <C><FC>\Demo\FullApps\ResizeConvert\<FN></C> <C>Batching editing and conversion application</C> </R>
<R> <C>Icon Editor</C> <C><FC>\Demo\FullApps\PlainIconEditor\<FN></C> <C>Icon editing application</C> </R>
<R> <C>Painting Application</C> <C><FC>\Demo\FullApps\ImageEnPainter\<FN></C> <C>Paint style editing application</C> </R>
<R> <C>Photo Editor</C> <C><FC>\Demo\FullApps\ConvertTo\<FN></C> <C>MDI image editing application</C> </R>
<R> <C>PhotoEn</C> <C><FC>\Demo\FullApps\PhotoEn\<FN></C> <C>MDI image editing application</C> </R>
<R> <C>PhotoEn v2</C> <C><FC>\Demo\FullApps\PhotoEn2\<FN></C> <C>MDI image editing application with support for transparency (Alpha channel)</C> </R>
<R> <C>Resource Editor</C> <C><FC>\Demo\FullApps\ResourceExtractor\<FN></C> <C>Application for accessing and editing resources</C> </R>
</TABLE>

<FM>IEVision<FN>
Note: Demos require the <L http://www.imageen.com/ievision/>IEVision.dll Plug-in</L>
<TABLE>
<R> <C>Barcode Reading</C> <C><FC>\Demo\IEVision\Barcode\<FN></C> <C>Reading of all common barcode types, including QR code</C> </R>
<R> <C>Detect Barcodes in a Video</C> <C><FC>\Demo\IEVision\BarcodeCam\<FN></C> <C>Automatically detect barcodes in a live video stream</C> </R>
<R> <C>Face Detection</C> <C><FC>\Demo\IEVision\FaceDetection\<FN></C> <C>Detect and track faces, eyes, mouths, etc., in a video stream</C> </R>
<R> <C>Face Detection (Low Level)</C> <C><FC>\Demo\IEVision\FaceDetection_LowLevel\<FN></C> <C>Detect and track faces, eyes, mouths, etc., in a video stream (low level method)</C> </R>
<R> <C>Find Faces in Photo</C> <C><FC>\Demo\IEVision\GetFaces\<FN></C> <C>Detect faces in an image</C> </R>
<R> <C>Find People in Photo</C> <C><FC>\Demo\IEVision\GetPeople\<FN></C> <C>Detect people in an image</C> </R>
<R> <C>Inpaint Brush</C> <C><FC>\Demo\IEVision\Inpaint_Brush\<FN></C> <C>Demo of IEVision's inpainting method to patch area of an image</C> </R>
<R> <C>Inpaint Selection</C> <C><FC>\Demo\IEVision\Inpaint_Selection\<FN></C> <C>Demo of IEVision's inpainting method to patch a selected area of an image</C> </R>
<R> <C>OCR</C> <C><FC>\Demo\IEVision\OCR\<FN></C> <C>Recognize text in an image</C> </R>
<R> <C>OCR with Layout</C> <C><FC>\Demo\IEVision\OCRWithLayout\<FN></C> <C>Recognize text and layout in an image</C> </R>
<R> <C>Track Objects</C> <C><FC>\Demo\IEVision\TrackObjects\<FN></C> <C>Click to track an object in a video stream</C> </R>
<R> <C>Track Objects (Low Level)</C> <C><FC>\Demo\IEVision\TrackObjects_LowLevel\<FN></C> <C>Click to track an object in a video stream (low level method)</C> </R>
</TABLE>

<FM>Image Acquisition Demos<FN>
<TABLE>
<R> <C>Acquire from Any Source</C> <C><FC>\Demo\ImageAcquisition\AllAcquire\<FN></C> <C>Acquire via Twain, WIA or connected DCIM devices</C> </R>
<R> <C>Take Photo with WIA</C> <C><FC>\Demo\ImageAcquisition\TakePhoto\<FN></C> <C>Take a snapshot from a WIA device</C> </R>
<R> <C>Twain Scanner</C> <C><FC>\Demo\ImageAcquisition\Twain\<FN></C> <C>Acquisition and configuration of a Twain scanner</C> </R>
<R> <C>Twain Settings Storage</C> <C><FC>\Demo\ImageAcquisition\TwainStore\<FN></C> <C>Loading and saving of setting from a Twain device</C> </R>
<R> <C>WIA Acquisition</C> <C><FC>\Demo\ImageAcquisition\CameraGetImages\<FN></C> <C>Retrieval of images from a camera using WIA</C> </R>
<R> <C>WIA Scanner</C> <C><FC>\Demo\ImageAcquisition\WIAScanner\<FN></C> <C>Acquisition of images from cameras and scanners using WIA</C> </R>
</TABLE>

<FM>Image Analysis Demos<FN>
<TABLE>
<R> <C>Automatic orientation</C> <C><FC>\Demo\ImageAnalysis\Orientator\<FN></C> <C>Using <A TImageEnProc.SkewDetection> to automatically rotate poorly scanned images</C> </R>
<R> <C>Color Density Analysis</C> <C><FC>\Demo\ImageAnalysis\DensityAnalysis\<FN></C> <C>Calculate the pixel density within an image (<A TImageEnProc.CalcDensityHistogram>)</C> </R>
<R> <C>Color Histogram</C> <C><FC>\Demo\ImageAnalysis\Histogram\<FN></C> <C>Display histograms of the color density within an image</C> </R>
<R> <C>Fourier Transformation</C> <C><FC>\Demo\ImageAnalysis\FFT\<FN></C> <C>Create a Fourier Transformation for an image (<A TImageEnProc.FTCreateImage>)</C> </R>
<R> <C>Get Palette</C> <C><FC>\Demo\ImageAnalysis\Palette\<FN></C> <C>Display the color palette of an image</C> </R>
<R> <C>Image Similarity</C> <C><FC>\Demo\ImageAnalysis\Compare\<FN></C> <C>Use an algorithm to determine the similarity of two images</C> </R>
<R> <C>Measurement</C> <C><FC>\Demo\ImageAnalysis\MeasureIt\<FN></C> <C>Accurate measurement within images (<A TImageEnVect.SetScaleFromSelectionLen>)</C> </R>
<R> <C>Separate Objects</C> <C><FC>\Demo\ImageAnalysis\SeparateObjects\<FN></C> <C>Detect and separate all objects in an image (<A TImageEnProc.SeparateObjects>)</C> </R>
<R> <C>Show Image Differences</C> <C><FC>\Demo\ImageAnalysis\ImagesDiff\<FN></C> <C>Calculate and display the differences between two images (<A TImageEnProc.CompareWith>)</C> </R>
</TABLE>

<FM>Image Editing Demos<FN>
<TABLE>
<R> <C>Add Border</C> <C><FC>\Demo\ImageEditing\AddBorder\<FN></C> <C>Add a border to an image</C> </R>
<R> <C>Add Picture Frame</C> <C><FC>\Demo\ImageEditing\PictureFrames\<FN></C> <C>Using masks to add border effects to images</C> </R>
<R> <C>Animated GIF Creator</C> <C><FC>\Demo\ImageEditing\AnimatedGIF\<FN></C> <C>Using ImageEn to create and animate multiple frame GIF files</C> </R>
<R> <C>Automatic Enhancement</C> <C><FC> \Demo\ImageEditing\AutoEnhance\ <FN></C> <C>Various algorithms to automatically adjust the color of an image</C> </R>
<R> <C>Brush Effects</C> <C><FC>\Demo\ImageEditing\Brush\<FN></C> <C>Using brush and paint effects on an image</C> </R>
<R> <C>Copy Transparent Selection</C> <C><FC>\Demo\ImageEditing\CopySel\<FN></C> <C>Copying and pasting transparent selections</C> </R>
<R> <C>Dithering</C> <C><FC>\Demo\ImageEditing\Dithering\<FN></C> <C>Reduction of colors in an image and dithering methods</C> </R>
<R> <C>Image Resizing</C> <C><FC>\Demo\ImageEditing\Resize\<FN></C> <C>Resizing an image to keep only a portion of it (Note: does not demonstrate stretching/scaling)</C> </R>
<R> <C>Layer Editing</C> <C><FC>\Demo\ImageEditing\Layers\<FN></C> <C>Use of layers when editing an image</C> </R>
<R> <C>Layer Rotation</C> <C><FC>\Demo\ImageEditing\RotateLayers\<FN></C> <C>Free rotation and resizing of layers of a <A TImageEnView></C> </R>
<R> <C>Layers and Text</C> <C><FC>\Demo\ImageEditing\TextOut\<FN></C> <C> Working with text and image layers and transparency </C> </R>
<R> <C>Lens Effect</C> <C><FC>\Demo\ImageEditing\Lens\<FN></C> <C>Demo of the lens effect (<A TImageEnProc.Lens>)</C> </R>
<R> <C>Lossless JPEG Cropping</C> <C><FC>\Demo\ImageEditing\LosslessCrop\<FN></C> <C>Perform a lossless crop of a JPEG (i.e. no quality loss due to re-saving)</C> </R>
<R> <C>Radial Effect</C> <C><FC>\Demo\ImageEditing\Radial\<FN></C> <C>Using <A TImageEnProc.RadialStretch> to correct barrel or pincushion distortion</C> </R>
<R> <C>Remove Red Eyes</C> <C><FC>\Demo\ImageEditing\RedEye\<FN></C> <C>Removing the "Red Eye" effect from flash photos</C> </R>
<R> <C>Replace Parts</C> <C><FC>\Demo\ImageEditing\ReplaceParts\<FN></C> <C>Working with selections in <A TImageEnView></C> </R>
<R> <C>Select and Crop</C> <C><FC>\Demo\ImageEditing\SelectAndCrop\<FN></C> <C>How to select and crop an image regardless of zoom level</C> </R>
<R> <C>Set Transparency</C> <C><FC>\Demo\ImageEditing\MakeTransparent\<FN></C> <C>Set the alpha/transparency in an image by color</C> </R>
<R> <C>Soft Selections</C> <C><FC>\Demo\ImageEditing\SoftSelections\<FN></C> <C>Demo of feathering a selection (to give it a soft edge)</C> </R>
<R> <C>Soft Shadow</C> <C><FC>\Demo\ImageEditing\SoftShadow\<FN></C> <C>Adding a soft shadow to an image</C> </R>
<R> <C>Threaded Processing</C> <C><FC>\Demo\ImageEditing\ThreadedProcessing\<FN></C> <C>Applying image effects in a background thread</C> </R>
<R> <C>Undo/Redo</C> <C><FC>\Demo\ImageEditing\UndoRedo\<FN></C> <C>Using Undo and Redo to step back though image changes</C> </R>
</TABLE>

<FM>Loading/Saving Demos<FN>
<TABLE>
<R> <C>Camera Raw Files</C> <C><FC>\Demo\InputOutput\CameraRaw\<FN></C> <C>Loading digital camera raw files using the DCRAW plug-in</C> </R>
<R> <C>Custom File Format</C> <C><FC>\Demo\InputOutput\FileFormatPlugins\ <FN></C> <C>Creating and supporting your own image format</C> </R>
<R> <C>Dicom Viewer</C> <C><FC>\Demo\InputOutput\Dicom\<FN></C> <C>Read and animated images within a Dicom file</C> </R>
<R> <C>EXIF Editor</C> <C><FC>\Demo\InputOutput\EXIF\<FN></C> <C>View and edit EXIF fields from a digital camera file</C> </R>
<R> <C>IPTC Editor</C> <C><FC>\Demo\InputOutput\IPTC\<FN></C> <C>View and edit IPTC fields from a JPEG or TIFF image</C> </R>
<R> <C>PDF Builder</C> <C><FC>\Demo\InputOutput\PDFBuilder\<FN></C> <C>Create PDF and PS files from a selection of images</C> </R>
<R> <C>Preload Images</C> <C><FC>\Demo\InputOutput\Preload\<FN></C> <C>Loading images in the background and displaying as required</C> </R>
<R> <C>Print Selection</C> <C><FC>\Demo\InputOutput\PrintSelected\<FN></C> <C>Print only the selected area of an image</C> </R>
<R> <C>Printing Demo</C> <C><FC>\Demo\InputOutput\PrintProjects\<FN></C> <C>Printing and print preview demo</C> </R>
<R> <C>Resource Loader</C> <C><FC>\Demo\InputOutput\ResourceLoader\<FN></C> <C>Load images from resources</C> </R>
<R> <C>TIFF Editor</C> <C><FC>\Demo\InputOutput\TiffHandler\<FN></C> <C>Edit pages and tags of TIFF files</C> </R>
<R> <C>True Raw Format</C> <C><FC>\Demo\InputOutput\RealRAW\<FN></C> <C>Loading and saving images in a true "Raw" format (Note: this is not the same as digital camera raw format)</C> </R>
<R> <C>Video Frame Viewer</C> <C><FC>\Demo\InputOutput\LargeVideos\<FN></C> <C>Load frames from a video or multipage file into a <A TImageEnMView></C> </R>
</TABLE>

<FM>Multiple Image Demos<FN>
<TABLE>
<R> <C>Create Thumbnail Frames</C> <C><FC>\Demo\Multi\CreateTransitionFrames\<FN></C> <C>Inserting transition frames into a <A TImageEnMView> or saving them to file</C> </R>
<R> <C>Custom Thumbnail Background</C> <C><FC>\Demo\Multi\CustomThumbs\<FN></C> <C>Show thumbnails for images in a folder with custom draw background</C> </R>
<R> <C>Draw Control on Thumbnail</C> <C><FC>\Demo\Multi\CustomThumbs2\<FN></C> <C>Custom drawing of checkboxes onto thumbnails</C> </R>
<R> <C>Frame Rotation</C> <C><FC>\Demo\Multi\Mview_Rotate\<FN></C> <C>Rotate a frame within a <A TImageEnMView></C> </R>
<R> <C>ImageEnFolderMView Demo</C> <C><FC>\Demo\Multi\FolderMView\<FN></C> <C>Using <A TImageEnFolderMView> to display file folders</C> </R>
<R> <C>Print Multiview Frame</C> <C><FC>\Demo\Multi\Multiview_PrintFrame\<FN></C> <C>Use <A TImageEnMView> to view frames and animate multiple-image files such as AVI, TIFF and GIF</C> </R>
<R> <C>Thumbnail Viewer</C> <C><FC>\Demo\Multi\Thumbnails\<FN></C> <C>Using <A TImageEnMView> to show thumbnails for images in a folder</C> </R>
<R> <C>Thumbnails with Checkboxes</C> <C><FC>\Demo\Multi\Checkboxes\<FN></C> <C>Using checkboxes to select thumbnails</C> </R>
<R> <C>Thumbnails with Style</C> <C><FC>\Demo\Multi\Thumbnails2\<FN></C> <C>Show thumbnails with preset styles</C> </R>
<R> <C>View All Frames</C> <C><FC>\Demo\Multi\Multiview\<FN></C> <C>Display of frames/images within multiple image sources such as AVI, TIFF, GIF, etc. using <A TImageEnMView></C> </R>
<R> <C>View Frame</C> <C><FC>\Demo\Multi\Multiview2\<FN></C> <C>Use a <A TImageEnView> to view the selected frame of a <A TImageEnMView></C> </R>
</TABLE>

<FM>Video Capture Demos<FN>
<TABLE>
<R> <C>Delayed DirectShow Capture</C> <C><FC>\Demo\VideoCapture\DirectShow5\<FN></C> <C>Delayed display of captured video using DirectShow</C> </R>
<R> <C>DirectShow Capture to AVI</C> <C><FC>\Demo\VideoCapture\DirectShow4\<FN></C> <C>Capture of video to AVI file using DirectShow</C> </R>
<R> <C>DirectShow Frame Capture</C> <C><FC>\Demo\VideoCapture\DirectShow3\<FN></C> <C>Capture of frames (to <A TImageEnMView>) using DirectShow</C> </R>
<R> <C>DirectShow to JPEG Stream</C> <C><FC>\Demo\VideoCapture\DirectShow6\<FN></C> <C>Capture of video to JPEG stream using DirectShow</C> </R>
<R> <C>DirectShow Video Capture 1</C> <C><FC>\Demo\VideoCapture\DirectShow1\<FN></C> <C>Video capture using DirectShow</C> </R>
<R> <C>DirectShow Video Capture 2</C> <C><FC>\Demo\VideoCapture\DirectShow2\<FN></C> <C>Video capture using DirectShow</C> </R>
<R> <C>IP Camera Capture</C> <C><FC>\Demo\VideoCapture\CaptFromIPCamera\<FN></C> <C>Capturing from an IP camera (which send a stream of JPEG images)</C> </R>
<R> <C>MMF Camera Grabbing</C> <C><FC>\Demo\VideoCapture\MediaFoundationCam\<FN></C> <C>Grabbing video from a camera using Microsoft Media Foundation</C> </R>
<R> <C>MMF File Grabbing</C> <C><FC>\Demo\VideoCapture\MediaFoundationFile\<FN></C> <C>Grabbing video from a file using Microsoft Media Foundation</C> </R>
<R> <C>MMF Web Grabbing</C> <C><FC>\Demo\VideoCapture\MediaFoundationURL\<FN></C> <C>Grabbing video from the web using Microsoft Media Foundation</C> </R>
<R> <C>Motion Detector</C> <C><FC>\Demo\VideoCapture\MotionDetector\<FN></C> <C>Motion detection using DirectShow</C> </R>
<R> <C>Multiple Videos</C> <C><FC>\Demo\VideoCapture\MView_VideoCapture\<FN></C> <C>Using a <A TImageEnMView> to display multiple video feeds</C> </R>
<R> <C>Record Desktop to AVI</C> <C><FC>\Demo\VideoCapture\DesktopToAvi\<FN></C> <C>Record desktop activity to an AVI file</C> </R>
<R> <C>VFW Video Capture 1 (Deprecated)</C> <C><FC>\Demo\VideoCapture\VideoCapture1\<FN></C> <C>Video capture (using older "Video for Windows" interface)</C> </R>
<R> <C>VFW Video Capture 2 (Deprecated)</C> <C><FC>\Demo\VideoCapture\VideoCapture2\<FN></C> <C>Video capture (using older "Video for Windows" interface)</C> </R>
<R> <C>VFW Video Capture 3 (Deprecated)</C> <C><FC>\Demo\VideoCapture\VideoCapture3\<FN></C> <C>Video capture (using older "Video for Windows" interface)</C> </R>
<R> <C>Video Effects</C> <C><FC>\Demo\VideoCapture\VideoEffects\<FN></C> <C>Adding effects to captured video with DirectShow</C> </R>
<R> <C>VMR to AVI</C> <C><FC>\Demo\VideoCapture\VMR_VideoCapture\<FN></C> <C>Capture of video to AVI file using DirectShow VMR</C> </R>
<R> <C>VMR Video Capture</C> <C><FC>\Demo\VideoCapture\VMR_Camera\<FN></C> <C>Video capture using Directshow VMR</C> </R>
<R> <C>VNC Viewer 1 (RFB)</C> <C><FC>\Demo\VideoCapture\RFB_VNCViewer1\<FN></C> <C>VNC Viewer (using RFB protocol)</C> </R>
<R> <C>VNC Viewer 2 (RFB)</C> <C><FC>\Demo\VideoCapture\RFB_VNCViewer2\<FN></C> <C>VNC Viewer (using RFB protocol) supporting multiple sources</C> </R>
</TABLE>

<FM>Other Demos<FN>
<TABLE>
<R> <C>ImageEnView Actions</C> <C><FC>\Demo\Other\Actions\<FN></C> <C>Image application built using only <L TImageEnView Actions>ImageEnView actions</L>    </C> </R>
<R> <C>ImageEnMView Actions</C> <C><FC>\Demo\Other\Actions_MView\<FN></C> <C>Multiple Image and thumbnail application built using only <L TImageEnMView Actions>ImageEnMView actions</L>    </C> </R>
<R> <C>ImageEnVect Actions</C> <C><FC>\Demo\Other\Actions_Vect\<FN></C> <C>Vector image application built using only <L TImageEnVect Actions>ImageEnVect actions</L>    </C> </R>
<R> <C>Burn Copyright</C> <C><FC>\Demo\Other\Copyright\<FN></C> <C>Adding text and borders to an image</L>    </C> </R>
<R> <C>Image Encryption</C> <C><FC>\Demo\Other\Encrypt\<FN></C> <C>Encryption and decryption of images using 128bit TEA algorithm</C> </R>
<R> <C>Image Geo-Mapping</C> <C><FC>\Demo\Other\GeoMaps\<FN></C> <C>Use the GPS data in photos to show their location on a map</C> </R>
<R> <C>Mouse Wheel Params</C> <C><FC>\Demo\Other\MouseWheel\<FN></C> <C>The effect of the TImageEnView and TImageEnMView MouseWheelParams properties</C> </R>
<R> <C>Photo Morphing</C> <C><FC>\Demo\Other\Morphing\<FN></C> <C>Morphing of two images</C> </R>
<R> <C>Pixel Viewer</C> <C><FC>\Demo\Other\PixelView\<FN></C> <C>Display the color of the current pixel under the cursor</C> </R>
<R> <C>Ruler</C> <C><FC>\Demo\Other\RulerBox\<FN></C> <C>Use of <A TRulerBox> to show current cursor position</C> </R>
<R> <C>Select Image Cells</C> <C><FC>\Demo\Other\CellsAndGrid\<FN></C> <C>Divide an image into a grid and allow selection of individual cells</C> </R>
</TABLE>

!!}




////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>IPTC items compatible with Adobe PhotoShop<FN>

<TABLE>
<R> <H>Field</H> <H>Record Number</H> <H>DataSet</H> <H>Max Length (Chars)</H> <H>Notes</H> </R>
<R> <C><FC>Object name<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Title (5)</C> <C>64</C> <C></C> </R>
<R> <C><FC>Edit status<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Edit_Status (7)</C> <C>7</C> <C></C> </R>
<R> <C><FC>Urgency<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Urgency (10)</C> <C>1</C> <C>1 numeric character</C> </R>
<R> <C><FC>Category<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Category (15)</C> <C>3</C> <C></C> </R>
<R> <C><FC>Supplemental Category<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Category_2 (20)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Fixture Identifier<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Fixture_Identifier (22)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Keywords<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Keywords (25)</C> <C>64 per instance</C> <C>Multiple instances of field</C> </R>
<R> <C><FC>Release Date<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Release_Date (30)</C> <C></C> <C>CCYYMMDD date format</C> </R>
<R> <C><FC>Release Time<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Release_Time (35)</C> <C></C> <C>HHMMSS±HHMM time format</C> </R>
<R> <C><FC>Special Instructions<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Instructions (40)</C> <C>256</C> <C></C> </R>
<R> <C><FC>Reference Service<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Reference_Service (45)</C> <C>10</C> <C></C> </R>
<R> <C><FC>Reference Date<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Reference_Date (47)</C> <C></C> <C>CCYYMMDD date format</C> </R>
<R> <C><FC>Reference Number<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Reference_Number (50)</C> <C>8</C> <C></C> </R>
<R> <C><FC>Date Created<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Date_Created (55)</C> <C></C> <C>CCYYMMDD date format</C> </R>
<R> <C><FC>Time Created<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Time_Created (60)</C> <C></C> <C>HHMMSS±HHMM time format</C> </R>
<R> <C><FC>Originating Program<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Originating_Program (65)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Program Version<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Program_Version (70)</C> <C>10</C> <C></C> </R>
<R> <C><FC>Object Cycle<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Object_Cycle (75)</C> <C>1</C> <C>1 character: 'a' is morning, 'b' is evening and 'c' is both</C> </R>
<R> <C><FC>By-line<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Byline_1 (80)</C> <C>32</C> <C></C> </R>
<R> <C><FC>By-line Title<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Byline_2 (85)</C> <C>32</C> <C></C> </R>
<R> <C><FC>City<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_City (90)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Province/State<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_State_Province (95)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Country/Primary Location Code<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Country_Code (100)</C> <C>3</C> <C>3 characters, see ISO 3166</C> </R>
<R> <C><FC>Country/Primary Location Name<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Country (101)</C> <C>64</C> <C></C> </R>
<R> <C><FC>Original Transmission Reference<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Transmission_Reference (103)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Credit<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Credit (110)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Source<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Source (115)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Copyright Notice<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Copyright_Notice (116)</C> <C>128</C> <C></C> </R>
<R> <C><FC>Caption/Abstract<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Caption (120)</C> <C>2000</C> <C>max 2000 characters, carriage-returns, linefeeds and spaces are permitted</C> </R>
<R> <C><FC>Writer/Editor<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Writer (122)</C> <C>32</C> <C></C> </R>
<R> <C><FC>Image Type<FN></C> <C>PhotoShop_IPTC_Records (2)</C> <C>IPTC_PS_Image_Type (130)</C> <C>2</C> <C>2 characters, see IIMV4 specifications</C> </R>
</TABLE>

For a list of all IPTC items, see the IPTC - NAA Information Interchange Model Version 4 (October 1997). Download the pdf manual from www.iptc.org
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Helper functions

<FM>File Formats<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A FindFileFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A FindStreamFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEExtToFileFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEFileIsOfFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEFileFormatGetInfo></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEFileFormatGetInfo2></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEFilenameInExtensions></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEFileExtInExtensions></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEFindNumberWithKnownFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEGetFileFramesCount></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEIsInternalFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IsKnownFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IsKnownSaveFormat></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A GetAllSupportedFileExtensions></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A GetFileExtensionsOfType></C> </R>
</TABLE>

<FM>External Formats (Plug-Ins)<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IEAddExtIOPlugin></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEAutoLoadIOPlugins></C> </R>
<R> <C_IMG_CLASS> <C><A TIEMiscPluginsImageMagick></C> </R>
</TABLE>

<FM>GIF<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A CheckAniGIF></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A DeleteGifIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A EnumGifIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEOptimizeGIF></C> </R>
</TABLE>

<FM>TIFF<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A DeleteTIFFImGroup></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A DeleteTIFFIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A EnumTIFFIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A EnumTIFFStream></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ExtractTIFFImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ExtractTIFFImageStream></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A InsertTIFFImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A InsertTIFFImageStream></C> </R>
</TABLE>

<FM>ICO<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A EnumICOIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEWriteICOImages></C> </R>
</TABLE>

<FM>DCX<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A DeleteDCXIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A EnumDCXIm></C> </R>
</TABLE>

<FM>JPEG<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A JpegLosslessTransform></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A JpegLosslessTransform2></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A JpegLosslessTransformStream></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IECalcJpegFileQuality></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IECalcJpegStreamQuality></C> </R>
</TABLE>

<FM>Register ImageEn Formats in VCL<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IERegisterFormats></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEUnregisterFormats></C> </R>
</TABLE>

<FM>AVI<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IEAVISelectCodec></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEAVIGetCodecs></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEAVIGetCodecsDescription></C> </R>
</TABLE>

<FM>Layers<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IELayersMerge></C> </R>
</TABLE>

<FM>Utilities<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IEAlphaToOpacity></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEGetCoresCount></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEOpacityToAlpha></C> </R>
</TABLE>

<FM>Helper Function Units<FN>
<TABLE2>
<R> <C_IMG_UNIT> <C><A iexHelperFunctions>: TImageEnIO, TIEBitmap, TBitmap and File Functions</C> </R>
<R> <C_IMG_UNIT> <C><A iexMetaHelpers>: EXIF, IPTC and Dicom functions</C> </R>
<R> <C_IMG_UNIT> <C><A iexWindowsFunctions>: File and Folder Management Functions</C> </R>
<R> <C_IMG_UNIT> <C><A iexRegistryFunctions>: Saving/Loading settings from the registry</C> </R>
<R> <C_IMG_UNIT> <C><A iexCanvasUtils>: TCanvas functions</C> </R>
</TABLE>

!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Lossless Jpeg Transformations<FN>

Rotate, flip or crop a JPEG image without recompression so that its quality is maintained.

<FI>Note: Defined in ImageEnIO unit<FN>

<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A JpegLosslessTransform></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A JpegLosslessTransform2></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A JpegLosslessTransformStream></C> </R>
</TABLE>
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>Action Classes

<FM>A set of actions is available for rapid UI development<FN>
<L TImageEnView Actions>ImageEnView Actions</L>
<L TImageEnMView Actions>ImageEnMView Actions</L>
<L TImageEnFolderMView Actions>ImageEnFolderMView Actions</L>
<L TImageEnVect Actions>ImageEnVect Actions</L>
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



{!!
<FS>Multipage Tiff-Gif functions

<FM>Defined in ImageEnIO unit<FN>
  <FI>GIF file format<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A CheckAniGif></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A DeleteGifIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A EnumGifIm></C> </R>
</TABLE>

  <FI>TIFF file format<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A DeleteTIFFIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A EnumTIFFIm></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A EnumTIFFStream></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ExtractTIFFImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ExtractTIFFImageStream></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A InsertTIFFImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A InsertTIFFImageStream></C> </R>
</TABLE>
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Color Management System<FN>

ImageEn includes a Color Management System (CMS) which enables the rendering of an image with its original colors.

To enable the CMS use:<FC>

IEGlobalSettings().EnableCMS := True;

<A TIEImageEnGlobalSettings.EnableCMS> <FN>public setting is defined in the unit <FC>iesettings<FN>.


If you want to use LCMS instead of Windows CMS you need to recompile ImageEn (with source code). Make sure that following line inside ie.inc is enabled:

<FC>(*$define IEINCLUDECMS*)<FN>



When the CMS is enabled loading of images with a color profile will be slower.

If you don't define <FC>IEINCLUDECMS<FN> in ie.inc then only the Windows CMS will be used. Note that Windows CMS is quicker than LCMS.

See also: <A TIEICC>

ImageEn includes parts of Little cms by Marti Maria


COPYRIGHT NOTICE OF LCMS:

Little cms
Copyright (C) 1998-2004 Marti Maria

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>64bit Support and DLLs

<FM>64BIT APPLICATIONS: IELIB64.DLL<FN>
ImageEn can be used to create 64bit applications with Delphi/CPB XE2 and XE3. It requires the use of a library, ielib64.dll, which you will need to ship with your application.

Ideally ielib64.dll should be placed in the same folder as your EXE, but it can also be located on the system path.

<FM>32BIT APPLICATIONS: IELIB32.DLL<FN>
32 bit applications do NOT require the shipping of a DLL as all necessary code is compiled into the EXE.

ielib32.dll is an OPTIONAL library for ImageEn applications. It provides no functionality other than that included in the native ImageEn libraries, however due to optimized compilation you will find file loading is approximately 30% faster with the DLL.

To use ielib32.dll you will need to edit ie.inc and enable the following defines:

IEUSEDLLJPEGLIB
IEUSEDLLPNGLIB
IEUSEDLLJPEG2000LIB
IEUSEDLLRAWLIB

You will then need to recompile your packages. Naturally you will need to ship ielib32.dll with your application. Ideally this should be placed in the same folder as your EXE, but it can also be located on the system path.

Notes:
 - Delphi/BCB 5 and 6 are not supported
 - You can check the availability of requisite DLL's by calling <A IELibAvailable>.
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Custom file format support

<FM>Description<FN>
<A IEFileFormatAdd>, <A IEFileFormatRemove>, <A IEFileFormatGetInfo> or <A IEFileFormatGetInfo2> allow applications to register custom file format readers/writers.

The only function you need to add custom file format is <A IEFileFormatAdd>.
You must allocate and fill a <A TIEFileFormatInfo> structure to the above function.

<FM>Demo<FN>
InputOutput\FileFormatPlugins

!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>WIA device properties

<FM>Device Information Property Constants

<FM>WIA_DIP_BAUDRATE<FN>
The baud rate of serial devices.

<FM>WIA_DIP_DEV_DESC<FN>
A description of the device.

<FM>WIA_DIP_DEV_ID<FN>
A device ID that is unique per physical device. This ID is composed of the Still Image class name followed by the device index. This ID is generated by the operating system.

<FM>WIA_DIP_DEV_NAME<FN>
A user-readable device name.

<FM>WIA_DIP_DEV_TYPE<FN>
An STI device type constant. Use the GET_STI_DEVICE_TYPE macro to get the device type. Currently, device types are defined as follows.
<TABLE>
<R> <H>Type</H> <H>Definition</H> </R>
<R> <C><FC>stiDeviceTypeDefault<FN></C> <C>When enumerating devices, any device that matches</C> </R>
<R> <C><FC>stiDeviceTypeScanner<FN></C> <C>Device that is represented as a scanner (see the WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES to determine if the scanner is flatbed or sheet-fed)</C> </R>
<R> <C><FC>stiDeviceTypeDigitalCamera<FN></C> <C>Device that is represented as a camera</C> </R>
<R> <C><FC>stiDeviceTypeStreamingVideo<FN></C> <C>Device that is represented as streaming video</C> </R>
</TABLE>

<FM>WIA_DIP_DRIVER_VERSION<FN>
The DLL version number of the driver's USD file.

<FM>WIA_DIP_HW_CONFIG<FN>
The "hardwareConfig" entry in the driver's INF file.

<FM>WIA_DIP_PORT_NAME<FN>
Name of the port through which the device is connected.

<FM>WIA_DIP_REMOTE_DEV_ID<FN>
The ID of the remote device.

<FM>WIA_DIP_SERVER_NAME<FN>
The name of the server where the WIA server for this device is running.

<FM>WIA_DIP_STI_GEN_CAPABILITIES<FN>
The driver's "STI_USD_CAPS.dwGenericCaps" value. This value contains the driver's "DeviceType" and "DeviceSubType" values.

<FM>WIA_DIP_UI_CLSID<FN>
The class ID of the UI component.

<FM>WIA_DIP_VEND_DESC<FN>
The device manufactures name.

<FM>WIA_DIP_WIA_VERSION<FN>
The version of WIA.


<FM>Common Device Property Constants

<FM>WIA_DPA_CONNECT_STATUS<FN>
Device connect status. This is intended for serial devices or other non-Plug and Play devices where the operating system cannot determine if the device is connected. There are only two defined values for this property, as follows.

<TABLE>
<R> <H>Connect Status</H> <H>Definition</H> </R>
<R> <C><FC>WIA_DEVICE_NOT_CONNECTED<<FN></C> <C>Device is not connected</C> </R>
<R> <C><FC>WIA_DEVICE_CONNECTED<FN></C> <C>Device is connected and operational</C> </R>
</TABLE>

<FM>WIA_DPA_FIRMWARE_VERSION<FN>
Firmware version number. The value for the firmware version number is up to the device. For example, "4.2.3".


<FM>Scanner Device Property Constants<FN>

<FM>WIA_DPS_DITHER_PATTERN_DATA<FN>
Reserved for future use.

<FM>WIA_DPS_DITHER_SELECT<FN>
Reserved for future use.

<FM>WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES<FN>
This property reports the document handling capabilities of a scanner. This property is mandatory for sheet-fed, scroll-fed, and hand-held scanners, and for scanners that have an automatic document feeder. If this property is not present, the application can assume that the scanner does not have an automatic document feeder. The document handling capability flags include the following:
WIA_FEED	The scanner has an automatic document handler installed.
WIA_FLAT	The scanner has a flatbed platen.
WIA_DETECT_FLAT	The scanner can detect a document on the flatbed platen.
WIA_DETECT_SCAN	The scanner can detect a document in the feeder only by scanning.
WIA_DETECT_FEED	The scanner can detect a document in the feeder.
WIA_DETECT_DUP	The scanner can detect a duplex scan request from the user.
WIA_DETECT_FEED_AVAIL	The scanner can tell when the automatic document feeder is installed.
WIA_DETECT_DUP_AVAIL	The scanner can tell when the duplexer is installed.
WIA_DUP	The scanner has a duplexer.

<FM>WIA_DPS_DOCUMENT_HANDLING_CAPACITY<FN>
This property reports the maximum number of pages that the document handler input tray can accept. Do not use this property. Use the WIA_DPS_PAGES property instead.

<FM>WIA_DPS_DOCUMENT_HANDLING_SELECT<FN>
On scanners with an automatic document feeder, this property selects the document-handling mode. This property is mandatory for scanners with an automatic document feeder. The mode flags include the following:
WIA_FEEDER	Scan from the document feeder.
WIA_FLATBED	Scan from flatbed platen.
WIA_DUPLEX	Place feeder in duplex mode.
WIA_AUTO_ADVANCE	Enables automatic feeding of the next document after a scan.
WIA_FRONT_FIRST	Scan the front (top) of a document first when doing duplex scanning.
WIA_BACK_FIRST	Scan the back (bottom) of a document first when doing duplex scanning.
WIA_FRONT_ONLY	Scan only the front (top) of a document.
WIA_BACK_ONLY	Scan only the back (bottom) of a document.
WIA_NEXT_PAGE	Scan the next page of the document.
WIA_PREFEED	Enable pre-feed mode. Pre-position next document while scanning.

Because this property is read/write, the driver implements two values for this property: a valid value and a current value. The bits in the valid value indicate values the driver and device support.
For example, if the scanner supports allowing the application to select feeder mode, the FEEDER bit in the valid value for this property is set. The application selects feeder mode by setting the FEEDER bit in the current value.
Scanners have a device item associated with each scan head in the scanner. These items, together with the "Document Handling Select" property, completely specify the source of a scan. The following examples are provided to clarify the use of the "Document Handling Select" property on different scanner hardware configurations:
- For a flatbed or hand held scanner with no automatic document handling, this property would not be implemented.
- If a duplex scanner does not support programmable (front/back) page order, either FRONT_FIRST or BACK_FIRST must still be available as a flag so that applications can detect the page order. An error should be generated if an application tries to change the order.
- A non-duplex scanner with automatic document handling must support either the FRONT_ONLY or BACK_ONLY flag so that applications can detect the page orientation. An error should be generated if an application tries to change the page orientation.

<FM>WIA_DPS_DOCUMENT_HANDLING_STATUS<FN>
This property reports the real-time document handling status of a scanner. This property is mandatory for sheet-fed, scroll-fed, and hand-held scanners, and for scanners that have an automatic document feeder. The status flags include the following:
WIA_FEED_READY	The document handler has a page ready to feed.
WIA_FLAT_READY	The scanner has a document on the flatbed platen.
WIA_DUP_READY	The scanner has detected a duplex request from the user.
WIA_FLAT_COVER_UP	The flat bed cover is up.
WIA_PATH_COVER_UP	The paper path cover is up.
WIA_PAPER_JAM	There is a document jam in the paper path.

<FM>WIA_DPS_ENDORSER_CHARACTERS<FN>
Returns a string that contains all of the valid characters that are printed by the endorser. This property is optional.

<FM>WIA_DPS_ENDORSER_STRING<FN>
Specifies a string that is printed onto the document when it is scanned. The endorser string supports using tokens that are replaced with values by the system. This property is optional. The following tokens are supported:
$DATE$	The date in the form YYYY/MM/DD.
$DAY$	The day in the form DD.
$MONTH$	The month of the year in the form MM.
$PAGE_COUNT$	The number of pages transferred.
$TIME$	The time of day in the form HH:MM:SS.
$YEAR$	The year in the form YYYY.

<FM>WIA_DPS_FILTER_SELECT<FN>
Reserved for future use.

<FM>WIA_DPS_HORIZONTAL_BED_REGISTRATION<FN>
Indicates how the sheet is positioned horizontally on the platen of a flatbed scanner. It is used to predict where a document is placed across the platen. It takes one of the following constants:
WIA_LEFT_JUSTIFIED	The sheet is positioned to the left with respect to the platen.
WIA_CENTERED	The sheet is centered on the platen.
WIA_RIGHT_JUSTIFIED	The sheet is positioned to the right with respect to the platen.

On scanners that support more than one scan head, this property is relative to the top scan head. This property is mandatory for scanners that have a platen.

<FM>WIA_DPS_HORIZONTAL_BED_SIZE<FN>
Specifies the maximum width, in thousandths of an inch, that is scanned in the horizontal (X) axis from the platen of a flatbed scanner at the current resolution. This property also applies to automatic document feeders that move sheets to the platen of a flatbed scanner for scanning. This property is mandatory for scanners that have a platen.. Other scanner types will instead implement the WIA_DPS_HORIZONTAL_SHEET_FEED_SIZE property.

<FM>WIA_DPS_HORIZONTAL_SHEET_FEED_SIZE<FN>
Specifies the maximum width, in thousandths of an inch, that is scanned in the horizontal (X) axis from a handheld or sheet feed scanner at the current resolution. This property also applies to automatic document feeders that scan without moving sheets to the platen of a flatbed scanner. This property is mandatory for sheet-fed, scroll-fed, and hand-held scanners.

<FM>WIA_DPS_MAX_SCAN_TIME<FN>
This property reports the maximum time, in milliseconds, to scan a page with the current property settings. Applications use this value for detecting hung device error conditions. This property is required for all scanners.

<FM>WIA_DPS_MIN_HORIZONTAL_SHEET_FEED_SIZE<FN>
Specifies the minimum width, in thousandths of an inch, that is scanned in the horizontal (X) axis from a handheld or sheet feed scanner at the current resolution. This property also applies to automatic document feeders that scan without moving sheets to the platen of a flatbed scanner. This property is mandatory for sheet-fed, scroll-fed, and hand-held scanners.

<FM>WIA_DPS_MIN_VERTICAL_SHEET_FEED_SIZE<FN>
Specifies the maximum width, in thousandths of an inch, that is scanned in the vertical (Y) axis from a handheld or sheet feed scanner at the current resolution. This property also applies to automatic document feeders that scan without moving sheets to the platen of a flatbed scanner. This property is mandatory for sheet-fed, scroll-fed, and hand-held scanners.

<FM>WIA_DPS_OPTICAL_XRES<FN>
Horizontal Optical Resolution. Highest supported horizontal optical resolution in DPI. This property is a single value. This is not a list of all resolutions that can be generated by the device. Rather, this is the resolution of the device's optics. This property is required for all scanners.

<FM>WIA_DPS_OPTICAL_YRES<FN>
Vertical Optical Resolution. Highest supported vertical optical resolution in DPI. This property is a single value. This is not a list of all resolutions that are generated by the device. Rather, this is the resolution of the device's optics. This property is required for all scanners.

<FM>WIA_DPS_PAGE_HEIGHT<FN>
Indicates the maximum height, in thousandths of an inch, of the page to be scanned This property is optional.

<FM>WIA_DPS_PAGE_WIDTH<FN>
Indicates the maximum width, in thousandths of an inch, of the page to be scanned. This property is optional.

<FM>WIA_DPS_PAGES<FN>
Specifies the number of pages to scan. The valid values for this property are represented as a range of values. For scanners with an automatic document feeder, the maximum value of the range represents the document feeder's capacity. If WIA_DPS_DOCUMENT_HANDLING_CAPACITY is used, the maximum value in this range must be the same as the value of WIA_DPS_DOCUMENT_HANDLING_CAPACITY. This property is mandatory for all scanners with an automatic document feeder, and for all sheet-fed, scroll-fed, and hand-held scanners.

<FM>WIA_DPS_PREVIEW<FN>
This property indicates whether or not the device supports preview scans. For example, sheet-fed and hand-held scanners cannot preview. The application should set this property when it does a preview scan so that the device can be optimized for previewing. This property is optional for all scanners..

<FM>WIA_DPS_SCAN_AHEAD_PAGES<FN>
Specifies the number of pages that are scanned ahead to local scanner or driver memory on scanners that support scan ahead. A value of zero disables scan ahead. The following negative value constant is supported:
WIA_SCAN_AHEAD_ALL	Scan ahead as far as possible.

Doing normal data transfers on the buffered scan head item retrieves the buffered pages. Device and item properties may not be changed during a scan-ahead operation. Scanners that support only WIA_SCAN_AHEAD_ALL would report only this value as min, max, and nominal in the valid values for one property. This property is optional.

<FM>WIA_DPS_SHEET_FEEDER_REGISTRATION<FN>
Indicates how the sheet is positioned horizontally on the scanning head of a hand-held or sheet-fed scanner. It is used to predict where a document is placed across the scan head. It takes one of the following constants:
WIA_LEFT_JUSTIFIED	The sheet is positioned to the left with respect to the scanning head.
WIA_CENTERED	The sheet is centered on the scanning head.
WIA_RIGHT_JUSTIFIED	The sheet is positioned to the right with respect to the scanning head.

On scanners that support more than one scan head, this property is relative to the top scan head. This property is mandatory for sheet-fed, scroll-fed, and hand-held scanners.

<FM>WIA_DPS_SHOW_PREVIEW_CONTROL<FN>
Indicates whether the user interface displays a preview control for the scanner. This property has one of the following constant values:
WIA_SHOW_PREVIEW_CONTROL	The scanner supports displaying a preview control.
WIA_DONT_SHOW_PREVIEW_CONTROL	The scanner does not support displaying a preview control.

<FM>WIA_DPS_TRANSPARENCY<FN>
Indicates the capabilities and status of an alternate light source/path (transparency adapter) in the scanner for scanning transparencies or slides. This property is mandatory for scanners that have a transparency attachment. The following flags are defined:
WIA_LIGHT_SOURCE_PRESENT_DETECT	The scanner detects when the transparency adapter is present.
WIA_LIGHT_SOURCE_PRESENT	A transparency adapter is present.
WIA_LIGHT_SOURCE_DETECT_READY	The scanner detects when media is placed on the transparency scanning bed.
WIA_LIGHT_SOURCE_READY	The user selected the transparency adapter at the scanner.

<FM>WIA_DPS_TRANSPARENCY_SELECT<FN>
Used by the application to select the alternate light source/path or transparency adapter. This property is mandatory for scanners that have a transparency attachment. The following flags are defined:
WIA_LIGHT_SOURCE_SELECT	Turns on the transparency adapter.

<FM>WIA_DPS_VERTICAL_BED_REGISTRATION<FN>
Indicates how the sheet is positioned vertically on the platen of a flatbed scanner. It is used to predict where a document is placed on the platen. It takes one of the following constants:
WIA_TOP_JUSTIFIED	The sheet is positioned to the top with respect to the platen.
WIA_CENTERED	The sheet is centered on the platen.
WIA_BOTTOM_JUSTIFIED	The sheet is positioned to the bottom with respect to the platen.

On scanners that support more than one scan head, this property is relative to the top scan head. This property is mandatory for scanners that have a platen.

<FM>WIA_DPS_VERTICAL_BED_SIZE<FN>
Specifies the maximum height, in thousandths of an inch, that is scanned in the vertical (Y) axis from the platen of a flatbed scanner at the current resolution. This property also applies to automatic document feeders, that move sheets to the platen of a flatbed scanner for scanning. This property is mandatory for scanners that have a platen. Other scanner types will instead implement the WIA_DPS_VERTICAL_SHEET_FEED_SIZE property.

<FM>WIA_DPS_VERTICAL_SHEET_FEED_SIZE<FN>
Specifies the maximum height, in thousandths of an inch, that is scanned in the vertical (Y) axis from a handheld or sheet feed scanner at the current resolution. This property also applies to automatic document feeders that scan without moving sheets to the platen of a flatbed scanner. This property is mandatory for sheet-fed scanners. Scroll-fed and hand-held scanners should not implement this property.

<FM>WIA_DPS_WARM_UP_TIME<FN>
Indicates to the application how long, in milliseconds, it takes the scanner and/or lamp to warm up before scanning commences. The application can display a progress indicator to the user based on this value. If the device requires no warm-up, the value should be zero. This property is required for all scanners.


<FM>Camera Device Property Constants<FN>

<FM>WIA_DPC_ARTIST<FN>
The name of the photographer.

<FM>WIA_DPC_BATTERY_STATUS<FN>
Camera battery life remaining, represented as a value from 100 (full charge) to 0.

<FM>WIA_DPC_BURST_INTERVAL<FN>
The time, in milliseconds, between image captures during a burst operation.

<FM>WIA_DPC_BURST_NUMBER<FN>
The number of images the device attempts to capture during a burst operation.

<FM>WIA_DPC_CAPTURE_DELAY<FN>
The time, in milliseconds, between the firing of the capture trigger and the actual initiation of the data capture. For no pre-capture delay, set this property to zero.
This property is not intended to be used to describe the time between frames for single-initiation multiple captures such as burst or time-lapse.

<FM>WIA_DPC_CAPTURE_MODE<FN>
Sets the image-capturing mode:
WIA_CAPTUREMODE_NORMAL	Normal mode for the camera.
WIA_CAPTUREMODE_BURST	Capture more than one image in quick succession as defined by the values of WIA_DPC_BURST_NUMBER and WIA_DPC_BURST_INTERVAL.
WIA_CAPTURMODE_TIMELAPSE	Capture more than one image in succession as defined by WIA_DPC_TIMELAPSE_NUMBER and WIA_DPC_TIMELAPSE_INTERVAL.

<FM>WIA_DPC_COMPRESSION_SETTING<FN>
A numeric value that specifies relative image quality. This property is device-specific.

<FM>WIA_DPC_CONTRAST<FN>
The perceived contrast of captured images. This property can use either an enumeration or a range.

<FM>WIA_DPC_COPYRIGHT_INFO<FN>
Copyright information for the image.

<FM>WIA_DPC_DIGITAL_ZOOM<FN>
The effective zoom ratio of the digital camera's acquired image scaled by a factor of 10. No digital zoom (1X) corresponds to a value of 10, which is the standard scene size captured by the camera. A value of 20 corresponds to a 2X zoom where 1/4 of the standard scene size is captured by the camera.

<FM>WIA_DPC_DIMENSION<FN>
Reserved, do not use.

<FM>WIA_DPC_EFFECT_MODE<FN>
Specifies the special image acquisition mode of the camera.

Effect Modes:
WIA_EFFECTMODE_STANDARD	The standard mode for the camera.
WIA_EFECTMODE_BW	Capture a grayscale image.
WIA_EFFECTMODE_SEPIA	Capture a sepia image.

<FM>WIA_DPC_EXPOSURE_COMP<FN>
Exposure compensation when using automatic exposure mode. Valid values are from -200 to 200 in increments of 50. These values represent the range -2.0 to +2.0.

<FM>WIA_DPC_EXPOSURE_INDEX<FN>
Specifies the film speed for digital cameras that can emulate ISO (ASA/DIN) speed settings. Typically, a device supports discrete enumerated values, but continuous control over a range is possible.
A value of 0xFFFF corresponds to Automatic ISO setting.

<FM>WIA_DPC_EXPOSURE_METERING_MODE<FN>
Specifies the mode the camera uses to automatically adjust the exposure setting.

Exposure Metering Modes:
WIA_EXPOSUREMETERING_AVERAGE	Set the exposure based on an average of the entire scene.
WIA_EXPOSUREMETERING_CENTERWEIGHT	Set the exposure based on a center-weighted average.
WIA_EXPOSUREMETERING_MULTISPOT	Set the exposure based on a multi-spot pattern.
WIA_EXPOSUREMETERING_CENTERSPOT	Set the exposure based on a center spot.

<FM>WIA_DPC_EXPOSURE_MODE<FN>
Specifies the camera exposure mode:
WIA_EXPOSURE_MODE_MANUAL 	The shutter speed and aperture are set by the user.
WIA_EXPOSUREMODE_AUTO	The shutter speed and aperture are automatically set by the camera.
WIA_EXPOSUREMODE_APERTURE_PRIORITY	The aperture is set by the user, and the camera automatically sets the shutter speed.
WIA_EXPOSUREMODE_SHUTTER_PRIORITY	The shutter speed is set by the user, and the camera automatically sets the aperture.
WIA_EXPOSUREMODE_PROGRAM_CREATIVE	The shutter speed and aperture are automatically set by the camera, optimized for still subject matter.
WIA_EXPOSUREMODE_PROGRAM_ACTION	The shutter speed and aperture are automatically set by the camera, optimized for scenes containing fast motion.
WIA_EXPOSUREMODE_PORTRAIT	The shutter speed and aperture are automatically set by the camera, optimized for portrait photography.

<FM>WIA_DPC_EXPOSURE_TIME<FN>
Manual exposure time. Units are milliseconds. The camera may represent the range of manual exposure settings it supports through the property attributes. Manual exposure time is only used when the camera is in manual exposure mode.

<FM>WIA_DPC_FLASH_MODE<FN>
Camera flash mode. Flash modes include the following:
WIA_FLASH_MODE_AUTO	Camera determines whether to flash.
WIA_FLASH_MODE_FILL	Camera flashes, regardless of light conditions.
WIA_FLASH_MODE_OFF	Camera will not flash.
WIA_FLASH_MODE_REDEYE_AUTO	Camera determines whether to flash, using red eye reduction.
WIA_FLASH_MODE_REDEYE_FILL	Camera flashes, regardless of light conditions, using red eye reduction.
WIA_FLASH_MODE_EXTERNALSYNC	Camera syncs external flash units.

<FM>WIA_DPC_FNUMBER<FN>
Manual F-number setting for use when the camera is in F-number exposure mode. Exposure mode is represented as camera F-number x 100. For example, Number 5.0 = 500.

<FM>WIA_DPC_FOCAL_LENGTH<FN>
The 35mm equivalent focal length, in 100ths of a millimeter.

<FM>WIA_DPC_FOCUS_DISTANCE<FN>
The distance, in millimeters, between the image-capturing plane of the digital camera and the point of focus.
A value of $FFFF corresponds to a setting greater than 655 meters.

<FM>WIA_DPC_FOCUS_MANUAL_DIST<FN>
Reserved, do not use.

<FM>WIA_DPC_FOCUS_METERING<FN>
Reserved, do not use.

<FM>WIA_DPC_FOCUS_METERING_MODE<FN>
Specifies the mode the camera uses to automatically adjust the focus.

Focus Metering Modes:
WIA_FOCUSMETERING_CENTERSPOT	Adjust the focus based on a center spot.
WIA_FOCUSMETERING_MULTISPOT	Adjust the focus based on a multi-spot pattern.

<FM>WIA_DPC_FOCUS_MODE<FN>
Specifies the focus mode of the digital camera:
WIA_FOCUSMODE_MANUAL	The focus is set by the user.
WIA_FOCUSMODE_AUTO	The focus is automatically set by the camera.
WIA_FOCUSMODE_MACROAUTO	The focus is automatically set by the camera to a macro (short-range) setting.

<FM>WIA_DPC_PAN_POSITION<FN>
Specifies the pan position for aiming the camera.

<FM>WIA_DPC_PICT_HEIGHT<FN>
The height to use for newly captured images. The list of valid values for this property has a one-to-one correspondence to the list of valid values for the WIA_DPC_PICT_WIDTH property.

<FM>WIA_DPC_PICT_WIDTH<FN>
The width to use for newly captured images. The list of valid values for this property has a one-to-one correspondence to the list of valid values for the WIA_DPC_PICT_HEIGHT property.

<FM>WIA_DPC_PICTURES_REMAINING<FN>
Number of pictures the device may still capture based on the current device capture settings.

<FM>WIA_DPC_PICTURES_TAKEN<FN>
Number of pictures taken. This property is mandatory for all cameras.

<FM>WIA_DPC_POWER_MODE<FN>
This property is used to determine whether the device is operating on battery or line power.

Power Modes:
WIA_POWERMODE_LINE	AC or other power adapter.
WIA_POWERMODE_BATTERY	Running off batteries.

<FM>WIA_DPC_RGB_GAIN<FN>
A null-terminated Unicode string that represents the red, green, and blue gain applied to image data, respectively. For example, "4:25:50" (null-terminated) represents a red gain of 4, a green gain of 25, and a blue gain of 50.

<FM>WIA_DPC_SHARPNESS<FN>
The perceived sharpness of captured images. This property can use either an enumeration or a range.

<FM>WIA_DPC_THUMB_HEIGHT<FN>
The height, in pixels, of thumbnails created for captured images. The list of valid values for this property have a one-to-one correspondence to the list of valid values for the WIA_DPC_THUMB_WIDTH property.

<FM>WIA_DPC_THUMB_WIDTH<FN>
The width, in pixels, of thumbnails created for captured images. The list of valid values for this property have a one-to-one correspondence to the list of valid values for the WIA_DPC_THUMB_HEIGHT property.

<FM>WIA_DPC_TILT_POSITION<FN>
Specifies the tilt position for aiming the camera.

<FM>WIA_DPC_TIMELAPSE_INTERVAL<FN>
The time, in milliseconds, between image captures in a time-lapse capture operation.

<FM>WIA_DPC_TIMELAPSE_NUMBER<FN>
The number of images the device attempts to capture during a time-lapse capture.

<FM>WIA_DPC_TIMER_MODE<FN>
Automatic picture timer mode.

<FM>WIA_DPC_TIMER_VALUE<FN>
Automatic picture timer time setting in milliseconds. This value is only used when taking a picture through computer control and when the timer mode is on.

<FM>WIA_DPC_UPLOAD_URL<FN>
A URL to which to upload images from a digital camera.

<FM>WIA_DPC_WHITE_BALANCE<FN>
Specifies how the digital camera weights color channels.

White Balance values:
WIA_WHITEBALANCE_MANUAL	White balance is set directly with the WIA_DPC_RGBGAIN property.
WIA_WHITEBALANCE_AUTO	The camera uses an automatic mechanism to set the white balance.
WIA_WHITEBALANCE_ONEPUSH_AUTO	The camera determines the white balance setting when a user presses the capture button while pointing the camera at a white surface.
WIA_WHITEBALANCE_DAYLIGHT	The camera sets the white balance to a value appropriate for use in daylight conditions.
WIA_WHITEBALANCE_FLORESCENT	The camera sets the white balance to a value appropriate for use with a fluorescent light source.
WIA_WHITEBALANCE_TUNGSTEN	The camera sets the white balance to a value appropriate for use with a tungsten light source.
WIA_WHITEBALANCE_FLASH	The camera sets the white balance to a value appropriate for use with an electronic flash.

<FM>WIA_DPC_ZOOM_POSITION<FN>
Reserved, do not use

!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>WIA item properties

<FM>Common WIA Item Property Constants<FN>

<FM>WIA_IPA_ACCESS_RIGHTS<FN>
This flag controls access to the item as well as whether the item is deleted. This property is read only for scanners and read/write for cameras, depending on whether the camera supports changing the access rights

Access Right values:
WIA_ITEM_READ 	Item has read-only access.
WIA_ITEM_WRITE	Item has write-only access.
WIA_ITEM_CAN_BE_DELETED	Item has delete-only access.
WIA_ITEM_RD	Item has read and delete access.
WIA_ITEM_RWD	Item has read, write, and delete access.

<FM>WIA_IPA_APP_COLOR_MAPPING<FN>
By default, it is the responsibility of the driver to map images from the user-selected profile to SRGB. If an application uses the ICM system to handle color profiles, it sets this property to instruct the driver to stop mapping automatically. The image is then in the color space specified by the WIA_IPA_COLOR_PROFILE property.

<FM>WIA_IPA_BITS_PER_CHANNEL<FN>
Bits per color channel. 1 for 1 bit per pixel black and white; 8 for 8-bit-per-pixel RGB. This property is read/write for scanners that support different bits-per-pixel settings. Otherwise it is read only.

<FM>WIA_IPA_BYTES_PER_LINE<FN>
Bytes per line of scan data. Some scanners may pad scan lines with extra bytes to make the scan line a fixed number of DWORDS. This is a read-only property and is set by the device, based on the scan settings.

<FM>WIA_IPA_CHANNELS_PER_PIXEL<FN>
Number of color channels per pixel. Typical values are 1 for black and white data; 3 for RGB.

<FM>WIA_IPA_COLOR_PROFILE<FN>
This property specifies the International Color Consortium (ICC) color profile for the images produced by the item.
When the WIA_IPA_APP_COLOR_MAPPING property is set, the current value of this property indicates the color profile for images produced by the WIA item. If the color mapping property is not set, the driver will map images from the current value of this property to sRGB.
This property should not be set by the application or driver. It is managed by WIA based on user input. The valid values setting lists the profiles the user has associated with the device.

<FM>WIA_IPA_COMPRESSION<FN>
Specifies the compression within the given data format. Useful for data formats like TIFF and DIB that have optional compressed modes.
This property is read/write for devices that support more than one compression type. For all other devices, it is read only.

Compression Types:
WIA_COMPRESSION_NONE	No compression.
WIA_COMPRESSION_BI_RLE4	DIB 4 bit run length encoding.
WIA_COMPRESSION_BI_RLE8	DIB 8 bit run length encoding.
WIA_COMPRESSION_G3	TIFF CCITT G3
WIA_COMPRESSION_G4	TIFF CCITT G4
WIA_COMPRESSION_JPEG	JPEG encoding.

<FM>WIA_IPA_DATATYPE<FN>
This property indicates the image type of the item. This property is read-only for cameras and read/write for scanners.

Data Types:
WIA_DATA_THRESHOLD	One bit-per-pixel black-and-white threshold. Data over the current value of WIA_IPS_THRESHOLD is converted to white; data under this value is converted to black.
WIA_DATA_DITHER	Scan data is dithered using the currently selected dither pattern.
WIA_DATA_GRAYSCALE	Scan data represents intensity. The palette is a fixed, equally-spaced gray scale with a depth specified by WIA_IPA_DEPTH property.
WIA_DATA_COLOR	Scan data is red, green, blue (RGB) color. The full color format is described using the following WIA properties:
WIA_IPA_CHANNELS_PER_PIXEL
WIA_IPA_BITS_PER_CHANNEL
WIA_IPA_PLANAR
WIA_IPA_PIXELS_PER_LINE
WIA_IPA_BYTES_PER_LINE
WIA_IPA_NUMBER_OF_LINES
WIA_DATA_COLOR_THRESHOLD	Same as WIA_DATA_COLOR except that the threshold value is used when scanning the data. Color values over the WIA_IPS_THRESHOLD value are converted to full brightness; colors under this value are converted to black.
WIA_DATA_COLOR_DITHER	Same as WIA_DATA_COLOR except that the data is dithered using the currently selected dither pattern.

<FM>WIA_IPA_DEPTH<FN>
Requested number of bits per pixel. On devices that allow for multiple color-space resolutions per data type, this property is a list of values (a WIA_PROP_LIST) specifying the color-space resolutions. On devices that allow only a single color-space resolution per data type, this is a read-only property that tracks the WIA_IPA_DATATYPE set above, otherwise it is read/write. This property is always read-only for cameras.

<FM>WIA_IPA_FILENAME_EXTENSION<FN>
This property specifies the preferred filename extension for a particular file format. The driver updates this property to reflect the current value of the WIA_IPA_FORMAT property.
This property is recommended for drivers that support standard formats. However, it is required for drivers that implement custom-defined formats. It is needed to allow the application to know the correct filename extension to use during file transfer of private formats.
For example, if WIA_IPA_FORMAT is WiaImgFmt_JPEG, then WIA_IPA_FILENAME_EXTENSION should be "jpg", or if WIA_IPA_FORMAT is WiaImgFmt_BMP, then WIA_IPA_FILENAME_EXTENSION should be "bmp". Notice that the file name extension does not include the ".".

<FM>WIA_IPA_FORMAT<FN>
This property controls the data format returned to the application.
This property is read/write for devices that support more than one format. Otherwise, it is read only.

Format Types:
WiaImgFmt_MEMORYBMP	Windows bitmap without a BITMAPFILEHEADER header.
WiaImgFmt_BMP	Windows bitmap with a BITMAPFILEHEADER header.
WiaImgFmt_EMF	Extended Windows metafile.
WiaImgFmt_WMF	Windows metafile.
WiaImgFmt_JPEG	JPEG compressed format.
WiaImgFmt_PNG	W3C PNG format.
WiaImgFmt_GIF	GIF image format.
WiaImgFmt_TIFF	Tag Image File Format.
WiaImgFmt_EXIF	Exchangeable File Format.
WiaImgFmt_PHOTOCD	Eastman Kodak file format.
WiaImgFmt_FLASHPIX	FlashPix format.
WiaImgFmt_ICO	Microsoft icon file format.
WiaImgFmt_CIFF	Camera Image File format.
WiaImgFmt_PICT	Apple file format.
WiaImgFmt_JPEG2K	JPEG 2000 compressed format.
WiaImgFmt_JPEG2KX	JPEG 2000 compressed format.
WiaImgFmt_RTF	Rich Text File format.
WiaImgFmt_XML	XML file.
WiaImgFmt_TXT	Text file.
WiaImgFmt_MPG	MPG video format.
WiaImgFmt_AVI	AVI video format.
WiaImgFmt_ASF	ASF video format.
WiaImgFmt_SCRIPT	Script file.
WiaImgFmt_EXEC	Executable file.
WiaImgFmt_UNICODE16	UNICODE 16-bit encoding.
WiaImgFmt_DPOF	DPOF printing format.
WiaAudFmt_WAV	WAV audio format.
WiaAudFmt_MP3	MP3 audio format.
WiaAudFmt_AIFF	AIFF audio format.
WiaAudFmt_WMA	WMA audio format.
WiaImgFmt_HTML	HTML format.
WiaImgFmt_RAWRGB	Raw RGB format.

The valid values for this property match the values returned by drvGetWiaFormatInfo based on the setting of WIA_IPA_TYMED. When WIA_IPA_TYMED is set to TYMED_CALLBACK, the valid values for this property include at least IMGFMT_MEMORYBMP. When WIA_IPA_TYMED is set to TYMED_FILE, the valid values for this property include at least IMGFMT_BMP.
When an application uses WiaImgFmt_RAWRGB, the accuracy of the image transfer depends on the following properties:
- WIA_IPA_DATATYPE
- WIA_IPA_CHANNELS_PER_PIXEL
- WIA_IPA_BITS_PER_CHANNEL
- WIA_IPA_DEPTH
- WIA_IPA_PIXELS_PER_LINE
- WIA_IPA_NUMBER_OF_LINES
- WIA_IPS_XRES
- WIA_IPS_YRES
For WiaImgFmt_RAWRGB valid values for these properties are the following:
WIA_IPA_DATATYPE - set to WIA_DATA_COLOR
WIA_IPA_DEPTH - set to 24 or higher.
The data transferred for RAWRGB should be uncompressed, in RGB byte order, DWORD aligned, and transferred with no image header.
Supporting the WiaImgFmt_RAWRGB format GUID allows TWAIN applications to transfer images higher than 32 bit using the memory transfer method.

<FM>WIA_IPA_FULL_ITEM_NAME<FN>
Complete item name, that includes hierarchy. This property is supplied by WIA. The full hierarchy includes the Plug and Play device index followed by the root item name, ROOT, followed by the item name. For example, a picture at the highest level in a camera could be named:
0001\Root\Image0001

<FM>WIA_IPA_GAMMA_CURVES<FN>
Gamma Correction curves for each color channel. Format is TBD. This property is optional.

<FM>WIA_IPA_ICM_PROFILE_NAME<FN>
The name of the color management device profile for the scanner.

<FM>WIA_IPA_ITEM_FLAGS<FN>
Flags that specify the item type and accessibility. This property is supplied by WIA. For information about these flags

<FM>WIA_IPA_ITEM_NAME<FN>
Name of the item object, for example, "\Image0001". This property is supplied by WIA.

<FM>WIA_IPA_ITEM_SIZE<FN>
Total data transfer size including data and data-specific header information, if the size is known. If the size is not known, this property must be set to zero.

<FM>WIA_IPA_MIN_BUFFER_SIZE<FN>
This property is used to specify the minimum buffer size used in data transfers. If the data transfer is done via callback, it may be as small as 64K, but if the transfer is to file, then it is the number of bytes needed to transfer a page of data at a time.

<FM>WIA_IPA_NUMBER_OF_LINES<FN>
Number of lines in the final image. This is a read-only property and is set by the device, based on the scan settings.

<FM>WIA_IPA_PIXELS_PER_LINE<FN>
Number of pixels per line in the current image. This is a read-only property and is set by the device based on the scan settings.

<FM>WIA_IPA_PLANAR<FN>
Describes whether the data is returned in a planar or packed-pixel format.

<FM>WIA_IPA_PREFERRED_FORMAT<FN>
The preferred data format the driver or device supplies to the application.

Format Types:
WiaImgFmt_MEMORYBMP	Windows bitmap without a header file.
WiaImgFmt_BMP	Windows bitmap with header file.
WiaImgFmt_EMF	Extended Windows metafile.
WiaImgFmt_WMF	Windows metafile.
WiaImgFmt_JPEG	JPEG compressed format.
WiaImgFmt_PNG	W3C PNG format.
WiaImgFmt_GIF	GIF image format.
WiaImgFmt_TIFF	Tag Image File Format.
WiaImgFmt_EXIF	Exchangeable File Format.
WiaImgFmt_PHOTOCD	Eastman Kodak file format.
WiaImgFmt_FLASHPIX	FlashPix format.
WiaImgFmt_ICO	Microsoft icon file format.

<FM>WIA_IPA_PROP_STREAM_COMPAT_ID<FN>
This property specifies a CLSID that represents a set of device property values. If a device driver implements this feature, applications use this property to determine if the device supports a set of values. This property is optional.

<FM>WIA_IPA_REGION_TYPE<FN>
This property is used to specify the type of region the image analysis has created. This property is optional. The following region types are supported:

Region Types:
WIA_TEXT_ITEM	Region contains text.
WIA_PHOTO_ITEM	Region contains a photo.
WIA_DRAWING_ITEM	Region contains a line drawing.
WIA_LOGO_ITEM	Region contains a logo.
WIA_HANDWRITING_ITEM	Region contains handwriting.
WIA_CHART_ITEM	Region contains a chart or graph.
WIA_TABLE_ITEM	Region contains tabular data.

<FM>WIA_IPA_SUPPRESS_PROPERTY_PAGE<FN>
Specifies whether to suppress the property pages for items on this device. This property takes one of the following constant values:

WIA_PROPPAGE_CAMERA_ITEM_GENERAL	Suppress the general item property page for a camera.
WIA_PROPPAGE_SCANNER_ITEM_GENERAL	Suppress the general item property page for a scanner.

<FM>WIA_IPA_TYMED<FN>
Specifies data transfer type:

WIA_TYMED_CALLBACK	Use callback based data transfer.
WIA_TYMED_MULTIPAGE_CALLBACK	Use callback based data transfer for multi-page or finished file scans.
WIA_TYMED_FILE	Use file based data transfer.
WIA_TYMED_MULTIPAGE_FILE	Use file based data transfer for multi-page or finished file scans.



<FM>Scanner WIA Item Property Constants<FN>


<FM>WIA_IPS_BRIGHTNESS<FN>
The image brightness values available within the scanner.
It is up to the scanner driver to supply a range of valid values for this property. Applications query the device for the valid values. Smaller values make the image darker. Larger values make the image brighter. This property is mandatory for all scanners.

<FM>WIA_IPS_CONTRAST<FN>
The image contrast values available within the scanner. It is up to the scanner driver to supply a range of valid values for this property. Applications query the device for the valid values. Smaller values make the image darker. Larger values make the image brighter. This property is mandatory for all scanners.

<FM>WIA_IPS_CUR_INTENT<FN>
Allows a driver to preset item properties based on the intended use of the image of an application. To provide an intent, use an intended image type flag and an intended size/quality flag OR'd together. Note that an image cannot be both grayscale and color. This property is mandatory for all scanners.

Intended image type flags:
WIA_INTENT_NONE	Default value. Do not preset any properties.
WIA_INTENT_IMAGE_TYPE_COLOR	Preset properties for color content.
WIA_INTENT_IMAGE_TYPE_GRAYSCALE	Preset properties for grayscale content.
WIA_INTENT_IMAGE_TYPE_TEXT	Preset properties for text content.
WIA_INTENT_IMAGE_TYPE_MASK	Mask for all of the image type flags.

Intended image size/quality flags:
WIA_INTENT_MINIMIZE_SIZE	Preset properties to minimize image size.
WIA_INTENT_MAXIMIZE_QUALITY	Preset properties to maximize image quality.
WIA_INTENT_SIZE_MASK	Mask for all of the size/quality flags.

<FM>WIA_IPS_INVERT<FN>
Specifies a photo-negative scan. This property is optional.

<FM>WIA_IPS_MIRROR<FN>
If a scanner supports the detection of document mirroring, this property is implemented with read-only access. If a scanner supports mirroring during scanning, this property is implemented with read/write access. The mirroring flags include the following:

WIA_MIRRORED	The image data is in mirrored orientation, or requests the device to mirror-invert the image.

<FM>WIA_IPS_ORIENTATION<FN>
If a scanner can support the detection of document orientation, this property is implemented. The orientation flags include the following:
WIA_PORTRAIT	The image data is in portrait orientation(0 degrees rotation).
WIA_LANDSCAPE	The image data is in landscape orientation (the page is rotated 90 degrees counter-clockwise).
WIA_ROT180	The image data is in an upside-down orientation (180 degrees rotation).
WIA_ROT270	The image data is in an upside-down landscape orientation (the page is rotated 270 degrees counter-clockwise).

<FM>WIA_IPS_PHOTOMETRIC_INTERP<FN>
Photometric interpretation. The default color interpretation of image data.
WIA_PHOTO_WHITE_1 : white is 1, black is 0.
WIA_PHOTO_WHITE_0 : white is 0, black is 1.
For devices or drivers that support selectable photometric interpretation, the access for this property is read/write. For all others, it is read only. This property is mandatory for all scanners.

<FM>WIA_IPS_ROTATION<FN>
If a scanner supports internal rotation during scanning, this property is implemented. The rotation flags include the following:
WIA_PORTRAIT	The driver or device will not rotate the image (0 degrees counter-clockwise).
WIA_LANDSCAPE	The driver or device rotates the image 90 degrees counter-clockwise.
WIA_ROT180	The driver or device rotates the image 180 degrees counter-clockwise.
WIA_ROT270	The driver or device rotates the image 270 degrees counter-clockwise.

<FM>WIA_IPS_THRESHOLD<FN>
Specifies the dividing line between black and white. This is the value the scanner uses to threshold, when the WIA_IPA_DATATYPE property is set to WIA_DATA_THRESHOLD. Higher positive values represent lighter pixel values.
The device driver specifies a range of valid values for this property. Applications query the device for the range of valid values. This property is optional.

<FM>WIA_IPS_WARM_UP_TIME<FN>
Indicates to the application how long, in milliseconds, it takes the scanner and/or lamp to warm up before scanning commences. The application displays a progress indicator to the user based on this value. If the device requires no warm-up, the value should be zero. This property is optional.

<FM>WIA_IPS_XEXTENT - Horizontal Extent<FN>
The width of the scan in pixels. This property is mandatory for all scanners.

<FM>WIA_IPS_XPOS - Horizontal Start Position<FN>
The horizontal start position in pixels of the image to be scanned. This property is mandatory for all scanners.

<FM>WIA_IPS_XRES - Horizontal Resolution<FN>
Specifies the current horizontal (X) axis resolution, in dots per inch (DPI).For scanners that do not support the independent setting of horizontal and vertical resolutions, the vertical resolution tracks the Horizontal Resolution property. To find out if the scanner supports independent resolutions, query the Vertical Resolution property.
The following properties are dependent on the Horizontal Resolution property for the independent resolution case:
- Horizontal Start Position
- Horizontal Extent
- Pixels per Line
- Bytes Per Line
- File Size

In addition, the following properties are also reliant in the dependent resolution case:
- Vertical Start Position
- Vertical Extent
- Number of Lines

This property is either a range or a list. This property is mandatory for all scanners.

<FM>WIA_IPS_YEXTENT - Vertical Extent<FN>
The height of the scan in lines. This property is mandatory for all scanners.

<FM>WIA_IPS_YPOS - Vertical Start Position<FN>
The vertical start position in pixels of the image to be scanned. This property is mandatory for all scanners.

<FM>WIA_IPS_YRES - Vertical Resolution<FN>
For scanners that support the independent setting of horizontal and vertical resolution, this property specifies the current vertical (Y) axis resolution, in DPI. It is implemented as read/write. For scanners that do not support independent setting of horizontal and vertical resolutions, this property is implemented as read-only and tracks the WIA_IPS_XRES property. This property is mandatory for all scanners.
The following properties are dependent on the Vertical Resolution property:
- Vertical Start Position
- Vertical Extent
- Number of Lines
- File Size



<FM>Camera WIA Item Property Constants<FN>


<FM>WIA_IPC_AUDIO_AVAILABLE<FN>
The camera item has audio data available.

<FM>WIA_IPC_AUDIO_DATA<FN>
Camera Audio data.

<FM>WIA_IPC_AUDIO_DATA_FORMAT<FN>
Audio formats for WIA are TBD.

<FM>WIA_IPC_NUM_PICT_PER_ROW<FN>
The number of pictures in a horizontal row for this camera.

<FM>WIA_IPC_SEQUENCE<FN>
Indicates the relative temporal position of this image in a multi-image time sequence. The numbers are not required to be sequential or start at any particular value. The values increase with time.

<FM>WIA_IPC_THUMB_HEIGHT<FN>
Specifies the height of the thumbnail in pixels.

<FM>WIA_IPC_THUMB_WIDTH<FN>
Specifies the width of the thumbnail in pixels.

<FM>WIA_IPC_THUMBNAIL<FN>
The thumbnail data. This is a fixed format consisting of raw, 24 bit, DWORD aligned, Blue-Green-Red color order, DIB bits with no DIB header.

<FM>WIA_IPC_TIMEDELAY<FN>
Indicates the amount of time, in milliseconds, that transpired between the capture of the current image and the previous image.

!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEICC Supported Color Formats<FN>

  TYPE_GRAY_8
  TYPE_GRAY_8_REV
  TYPE_GRAY_16
  TYPE_GRAY_16_REV
  TYPE_GRAY_16_SE
  TYPE_GRAYA_8
  TYPE_GRAYA_16
  TYPE_GRAYA_16_SE
  TYPE_GRAYA_8_PLANAR
  TYPE_GRAYA_16_PLANAR
  TYPE_RGB_8
  TYPE_RGB_8_PLANAR
  TYPE_BGR_8
  TYPE_BGR_8_PLANAR
  TYPE_RGB_16
  TYPE_RGB_16_PLANAR
  TYPE_RGB_16_SE
  TYPE_BGR_16
  TYPE_BGR_16_PLANAR
  TYPE_BGR_16_SE
  TYPE_RGBA_8
  TYPE_RGBA_8_PLANAR
  TYPE_RGBA_16
  TYPE_RGBA_16_PLANAR
  TYPE_RGBA_16_SE
  TYPE_ARGB_8
  TYPE_ARGB_16
  TYPE_ABGR_8
  TYPE_ABGR_16
  TYPE_ABGR_16_PLANAR
  TYPE_ABGR_16_SE
  TYPE_BGRA_8
  TYPE_BGRA_16
  TYPE_BGRA_16_SE
  TYPE_CMY_8
  TYPE_CMY_8_PLANAR
  TYPE_CMY_16
  TYPE_CMY_16_PLANAR
  TYPE_CMY_16_SE
  TYPE_CMYK_8
  TYPE_CMYK_8_REV
  TYPE_YUVK_8
  TYPE_CMYK_8_PLANAR
  TYPE_CMYK_16
  TYPE_CMYK_16_REV
  TYPE_YUVK_16
  TYPE_CMYK_16_PLANAR
  TYPE_CMYK_16_SE
  TYPE_KYMC_8
  TYPE_KYMC_16
  TYPE_KYMC_16_SE
  TYPE_KCMY_8
  TYPE_KCMY_8_REV
  TYPE_KCMY_16
  TYPE_KCMY_16_REV
  TYPE_KCMY_16_SE
  TYPE_CMYK5_8
  TYPE_CMYK5_16
  TYPE_CMYK5_16_SE
  TYPE_KYMC5_8
  TYPE_KYMC5_16
  TYPE_KYMC5_16_SE
  TYPE_CMYKcm_8
  TYPE_CMYKcm_8_PLANAR
  TYPE_CMYKcm_16
  TYPE_CMYKcm_16_PLANAR
  TYPE_CMYKcm_16_SE
  TYPE_CMYK7_8
  TYPE_CMYK7_16
  TYPE_CMYK7_16_SE
  TYPE_KYMC7_8
  TYPE_KYMC7_16
  TYPE_KYMC7_16_SE
  TYPE_CMYK8_8
  TYPE_CMYK8_16
  TYPE_CMYK8_16_SE
  TYPE_KYMC8_8
  TYPE_KYMC8_16
  TYPE_KYMC8_16_SE
  TYPE_CMYK9_8
  TYPE_CMYK9_16
  TYPE_CMYK9_16_SE
  TYPE_KYMC9_8
  TYPE_KYMC9_16
  TYPE_KYMC9_16_SE
  TYPE_CMYK10_8
  TYPE_CMYK10_16
  TYPE_CMYK10_16_SE
  TYPE_KYMC10_8
  TYPE_KYMC10_16
  TYPE_KYMC10_16_SE
  TYPE_CMYK11_8
  TYPE_CMYK11_16
  TYPE_CMYK11_16_SE
  TYPE_KYMC11_8
  TYPE_KYMC11_16
  TYPE_KYMC11_16_SE
  TYPE_CMYK12_8
  TYPE_CMYK12_16
  TYPE_CMYK12_16_SE
  TYPE_KYMC12_8
  TYPE_KYMC12_16
  TYPE_KYMC12_16_SE
  TYPE_XYZ_16
  TYPE_Lab_8
  TYPE_ALab_8
  TYPE_Lab_16
  TYPE_Yxy_16
  TYPE_YCbCr_8
  TYPE_YCbCr_8_PLANAR
  TYPE_YCbCr_16
  TYPE_YCbCr_16_PLANAR
  TYPE_YCbCr_16_SE
  TYPE_YUV_8
  TYPE_YUV_8_PLANAR
  TYPE_YUV_16
  TYPE_YUV_16_PLANAR
  TYPE_YUV_16_SE
  TYPE_HLS_8
  TYPE_HLS_8_PLANAR
  TYPE_HLS_16
  TYPE_HLS_16_PLANAR
  TYPE_HLS_16_SE
  TYPE_HSV_8
  TYPE_HSV_8_PLANAR
  TYPE_HSV_16
  TYPE_HSV_16_PLANAR
  TYPE_HSV_16_SE
  TYPE_XYZ_DBL
  TYPE_Lab_DBL
  TYPE_GRAY_DBL
  TYPE_RGB_DBL
  TYPE_CMYK_DBL
!!}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>FAQ

<FN>
• <L #1>After updating ImageEn I get the following error on start-up: "The procedure entry point ... could not be located in the dynamic link library PKIECtrl15.bpl"</L>
• <L #2>How can I prevent the scrollwheel from causing the image to zoom? I want it to remain the orginal size at all times?</L>
• <L #3>I was wondering if it was possible to convert, say, a GIF image to a TIFF image without a visual component. In other words, how could I convert among image formats in a console application?</L>
• <L #5>Can I modify brightness?</L>
• <L #6>Can I modify scanner resolution?</L>
• <L #7>Is there any way to save a CMYK TIFF using ImageEn?</L>
• <L #8>I want to resample a normal picture but I get the compiler error: TResampleFilter.... is not defined?</L>
• <L #9>Is it possible to de/activate duplex on a scanner?</L>
• <L #10>I'm trying to draw some text onto the raw video frame using the event OnVideoFrameRaw. How can I get a canvas from the parameters passed to this event?</L>
• <L #11>Can your component take a series of single page TIFF files and create a Multi-Page file from them?</L>
• <L #12>How do I change the current page of a TIFF image in ImageEn?</L>
• <L #14>Is it possible to convert a multipage-image to a different encoding? G3 and G4 specifically?</L>
• <L #15>How do I get the transparent PNGs to work inside your vector view control?</L>
• <L #16>I would like users to be able to resize and move vectorial objects when they are displayed on the screen. How can I do that?</L>
• <L #17>Using TImageEnMView, how do I load images as they are displayed?</L>
• <L #19>Is it possible to save the objects (Lines, Ellipses, etc) added in TImageEnVect with the image? I.e. burn them into the JPEG file?</L>
• <L #20>How do i load or modify the IPTC info without loading the actual image?</L>
• <L #22>How I do save a JPEG without EXIF and other metadata?</L>
• <L #23>How can I check if the TImageEnView is empty (i.e. no bitmap is loaded)?</L>
• <L #24>When a user selects a page of a multipage tiff image displayed in a TImageEnMView, I want the image to be displayed in a TImageEnView.</L>
• <L #25>How do I assign the image in one ImageEnView to another ImageEnView?</L>
• <L #26>What is the correct way to load a Resource Image at runtime into an TImageEnView component in C++?</L>
• <L #27>How do I read custom TIFF tags?</L>
• <L #28>Why are my JPEGs so big?</L>
• <L #29>In ImageEnVect, how do I save an image with many objects? And how can I load the objects if I've saved them?</L>
• <L #30>How do I store/retrieve pages of a multiple image file in a blob field with TImagenDBView?</L>
• <L #32>How to load the embedded jpeg from camera RAW images?</L>
• <L #33>How do I terminate a polyline without double-clicking?</L>
• <L #34>Why am I unable to read recent Camera RAW formats?</L>
• <L #35>Measuring line length using miLineLen (or miArea) does not work. The mouse pointer always shows 0.00 pixels. Why is that?</L>
• <L #36>Why do I get "Floating point overflow" when printing?</L>
• <L #37>Why do I see horizontal/vertical bands when loading images using TImageEnMView?</L>


<# 1><FM>After updating ImageEn I get the following error on start-up: "The procedure entry point ... could not be located in the dynamic link library PKIECtrl15.bpl"<FN>

This means that a package - either one of ImageEn's or one of your own - is trying to access PKIECtrl15.bpl, but is failing because PKIECtrl15.bpl has been recompiled more recently. The error may be followed by another one that advised you which package is giving you the error: "Xyz package could not be loaded. Would you like to load it next time you run Delphi...".

If you know which package has caused the problem, open it DPK file and recompile it.

Otherwise, search your system for any PKIE*.* and DPKIE*.* files and remove them (it might help to review the library paths under Tools > Options, Library). Then reinstall ImageEn. 

If you still get the error, then you might have a package that links to ImageEn. Go through each of your packages and recompile any that use ImageEn.


<# 2><FM>How can I prevent the scrollwheel from causing the image to zoom? I want it to remain the original size at all times?<FN>

Just write:

<FC>ImageEnView1.MouseWheelParams.Action := iemwNone;<FN>

other values are iemwVScroll and iemwZoom (default).


<# 3><FM>I was wondering if it was possible to convert, say, a GIF image to a TIFF image without a visual component. In other words, how could I convert among image formats in a console application?<FN>

There are several ways. The simplest is:
<FC>
var
  ie: TImageEnView;
begin
  ie := TImageEnView.Create(nil);
  ie.IO.LoadFromFile('in.gif');
  ie.IO.SaveToFile('out.gif');
  ie.free;
end;
<FN>

<FM>You can also create a TIEBitmap (or TBitmap) object that contains the image. This is the code:<FN>

<FC>
var
  bmp: TIEBitmap;
  io: TImageEnIO;
Begin
  bmp := TIEBitmap.Create;
  io := TImageEnIO.CreateFromBitmap(bmp);
  io.LoadFromFile('in.gif');
  io.SaveToFile('out.gif');
  io.free;
  bmp.free;
End;
<FN>

<# 5><FM>Can I modify brightness?<FN>

You can change brightness (luminosity) using several methods.

Using IntensityRGBall method:

  <FC>ImageEnView1.Proc.IntensityRGBall(20, 20, 20); // increment luminosity of 20 (the fastest)<FN>

Using HSLvar method:

  <FC>ImageEnView1.Proc.HSLvar(0, 0, 20); // increment luminosity of 20 (slow but more accurate)<FN>

Using HSVvar method:

  <FC>ImageEnView1.Proc.HSVvar(0, 0, 20); // increment luminosity of 20 (slow but more accurate)<FN>


<# 6><FM>Can I modify scanner resolution?<FN>

To change scan resolution to 300 on your scanner use:
<FC>
ImageEnView1.IO.TwainParams.XResolution.CurrentValue := 300;
ImageEnView1.IO.TwainParams.YResolution.CurrentValue := 300;
ImageEnView1.IO.Acquire;
<FN>

<# 7><FM>Is there any way to save a CMYK TIFF using ImageEn?<FN>

To save TIFF with CMYK write:
<FC>
ImageEnView1.IO.Params.TIFF_PhotometInterpret := ioTIFF_CMYK;

ImageEnView1.IO.SaveToFile('xxx.tif');
<FN>

<# 8><FM>I want to resample a normal picture but I get the compiler error: TResampleFilter.... is not defined<FN>

You must add the "hyiedefs" unit to your "uses" clause:
<FC>
uses hyiedefs;
<FN>

<# 9><FM>Is it possible to de/activate duplex on a scanner?<FN>

Write <FC>ImageEnView1.IO.TwainParams.DuplexEnabled := True<FN> (or False to disable).


<# 10><FM>I'm trying to draw some text onto the raw video frame using the event OnVideoFrameRaw. How can I get a canvas from the parameters passed to this event?<FN>

Instead of using OnVideoFrameRaw you must use OnVideoFrame which returns a Bitmap object.

From the Bitmap object you can obtain the Canvas with:

Bitmap.Canvas


<# 11><FM>Can your component take a series of single page TIFF files and create a Multi-Page file from them?<FN>

Yes, load each page using:

  <FC>ImageEnView1.IO.LoadFromFile('page1.tif');<FN>

Then save the page with:
<FC>
  ImageEnView1.IO.Params.TIFF_ImageIndex := 0; // increment this value for each page

  ImageEnView1.IO.InsertToFileTIFF('multipage.tif');
<FN>
Otherwise you can use TImageEnMView and TImageEnMIO component. See the "multi" demo for more detail.


<# 12><FM>How do I change the current page of a TIFF image in ImageEn?<FN>

There are two ways to load several pages from a TIFF:

1) Load a page at the time, using TImageEnView, for example:
<FC>
ImageEnView1.IO.Params.TIFF_ImageIndex := page_number; // where page_number is the page to load and starts from 0 (first page)

ImageEnView1.IO.LoadFromFile('mytiff.tiff');
<FN>
To know how many pages there are use:

<FC>page_count := EnumTIFFIm('mytiff.tiff');<FN>


2) load all pages using a TImageEnMView:

<FC>ImageEnMView1.MIO.LoadFromFile('mytiff.tiff');<FN>


<# 14><FM>Is it possible to convert a multipage-image to a different encoding? G3 and G4 specifically?<FN>

Using TImageEnMView you must change the compression property for all pages:
<FC>
ImageEnMView1.MIO.LoadFromFile('original.tif');

// change compression for the first page
ImageEnMView1.MIO.Params[0].TIFF_Compression := ioTIFF_G4FAX;

// change compression for the other pages
ImageEnMView1.MIO.DuplicateCompressionInfo;

// now save
ImageEnMView1.Mio.SaveToFile('output.tif');
<FN>


<# 15><FM>How do I get the transparent PNGs to work inside your vector view control?<FN>

To load the alpha channel from PNG (and others) you must use SetObjBitmapFromFile method:

<FC>ImageEnVect1.SetObjBitmapFromFile(hobj, 'test.png');<FN>


<# 16><FM>I would like users to be able to resize and move vectorial objects when they are displayed on the screen. How can I do that?<FN>

To enable users to modify (move, resize) objects just set:

<FC>ImageEnVect1.MouseInteractVt := [miObjectSelect];<FN>

The users can then click objects to select them and resize or move them.


<# 17><FM>Using TImageEnMView, how do I load images as they are displayed?<FN>

There are several ways to display images "on demand":

1)  If you have a directory where are all files just write:
<FC>ImageEnMView1.FillFromDirectory('c:\myimages');<FN>

2) When you add a new image just set its ImageFileName[] index, and ImageEn will automatically load the specified file when needed. For example:
<FC>
idx := ImageEnMView1.AppendImage;
ImageEnMView1.ImageFileName[idx] := 'first.jpg';
<FN>
3) When you add a new image just set the ImageID[] property. You must create an array of filenames where images are to be loaded. For example:
<FC>
var
  files: array [0..1] of string;
begin
  files[0] := 'first.jpg';
  files[1] := 'second.jpg';
  ImageEnMView1.ImageID[ ImageEnMView1.AppendImage ] := 0;
  ImageEnMView1.ImageID[ ImageEnMView1.AppendImage ] := 1;
end;
<FN>
You must also implement the OnImageIDRequest event, e.g.:
<FC>
procedure TForm1.OnImageIDRequest(Sender: TObject; ID: integer; var Bitmap: TBitmap);
var
  io: TImageEnIO;
begin
  io := TImageEnIO.Create(self);
  io.AttachedBitmap := bmp; // bmp is a TBitmap object, defined at class level (must exists after the OnImageIDRequest exits)
  io.LoadFromFile( files[ID] );
  io.free;
  Bitmap := bmp;
end;
<FN>
4) If the images are frames of a media file (like AVI, MPEG, etc..) you can write:
<FC>
ImageEnMView1.LoadFromFileOnDemand('film.mpeg');
<FN>


<# 19><FM>Is it possible to save the objects (Lines, Ellipses, etc) added in TImageEnVect with the image? I.e. burn them into the JPEG file?<FN>

Use <A TImageEnVect.CopyObjectsToBack> the method, then save the background image (IO.SaveToFile).


<# 20><FM>How do i load or modify the IPTC info without loading the actual image?<FN>

The IPTC info is automatically loaded when you load the JPEG image using the LoadFromFile method. After this you have in ImageEnView.IO.Params.IPTC_Info object all IPTC informations loaded. To read the caption you can write:
<FC>
ImageEnView.IO.LoadFromFile('image.jpg');
Idx := ImageEnView.IO. Params.IPTC_Info.IndexOf(2, 120);
Caption := ImageEnView.IO.Params.IPTC_Info.StringItem[idx];
<FN>
To modify the caption:
<FC>
ImageEnView.IO.Params.IPTC_Info.StringItem[idx] := 'new caption';
ImageEnView.IO.SaveToFile('image2.jpg');
<FN>
To modify IPTC info without loading the image use ParamsFromFile and InjectJpegIPTC methods, as follows:

<FC>ImageEnView.IO.ParamsFromFile('one.jpg');<FN>

...After modifying the IPTC info save it

<FC>ImageEnView.IO.InjectJpegIPTC('two.jpg');<FN>


<# 22><FM>How I do save a JPEG without EXIF and other metadata?<FN>

Call Params.ResetInfo. For example:
<FC>
ImageEnView.IO.LoadFromFile('input.jpg');
ImageEnView.IO.Params.ResetInfo;
ImageEnView.IO.SaveToFile('output.jpg');
<FN>

<# 23><FM>How can I check if the TImageEnView is empty (i.e. no bitmap is loaded)?<FN>

To empty the component use the "Blank" method. To check if it is empty use IsEmpty or IsEmpty2:
<FC>
If ImageEnView1.IsEmpty then
  ...
<FN>

<# 24><FM>When a user selects a page of a multipage tiff image displayed in a TImageEnMView, I want the image to be displayed in a TImageEnView.<FN>

To handle image selection use the OnImageSelect event. To transfer the currently selected image use the Assign method:
<FC>
procedure TForm1.ImageEnMView1ImageSelect(Sender: TObject; idx: Integer);
begin
  ImageEnView1.Assign( ImageEnMView1.Bitmap );
end;
<FN>

<# 25><FM>How do I assign the image in one ImageEnView to another ImageEnView?<FN>

Just use Assign method:

<FC>ImageEnView1.Assign( ImageEnView2 );<FN>

or

<FC>ImageEnView1.Assign( ImageEnView1.Bitmap ); // this doesn't copy DPI or other metadata, but just the image<FN>


<# 26><FM>What is the correct way to load a Resource Image at runtime into an TImageEnView component in C++?<FN>

Here is a sample code:
<FC>
    TResourceStream *ResourceImage;
    // Load from resource the About image ( a JPEG file).
    ResourceImage = new TResourceStream((int)HInstance, "ABOUTBITMAP", RT_RCDATA);
    MainForm->ImageAbout->IO->LoadFromStreamJpeg(ResourceImage);
    delete ResourceImage;
<FN>

Here is a single line text file named "resource.rc" with the sentence:

ABOUTBITMAP RCDATA "about.jpg"

Just add the Resource file to the project and compile.


<# 27><FM>How do I read custom TIFF tags?<FN>

This example shows how to read EXIF tags saved by Canon cameras:
<FC>
var
  ms: TMemoryStream;
  tagReader1, tagReader2, tagReader3: TIETifTagsReader;
  i: integer;
  // some Canon tags
  m_nMacroMode, m_nLenghtTimer, m_Quality: integer;
  m_ImageType: string;
begin
  with imageenvect1 do begin

    IO.LoadFromFile('Capture_00006.JPG');
    with IO.Params.JPEG_MarkerList do begin
      i := IndexOf( JPEG_APP1 );
      if i>=0 then
      begin
        // there are EXIF info
        ms := TMemoryStream.Create;
        ms.Write( MarkerData[i][6], MarkerLength[i] );  // bypass first 4 bytes (must contain 'Exif')
        ms.Position := 0;

        tagReader1 := TIETifTagsReader.CreateFromStream( ms, 0 );    // read TIFF's IFD

        tagReader2 := TIETifTagsReader.CreateFromIFD( tagReader1, 34665 );    // read IFD in tag 34665 (SubEXIF)

        tagReader3 := TIETifTagsReader.CreateFromIFD( tagReader2, $927c );    // read IFD in tag $927C (MarkerData - Canon IFD data)

        // read Canon EXIF tags
        m_nMacroMode := tagReader3.GetIntegerIndexed(1, 1);
        m_nLenghtTimer := tagReader3.GetIntegerIndexed(1, 2);
        m_Quality := tagReader3.GetIntegerIndexed(1, 3);
        m_ImageType := tagReader3.GetString(6);

        tagReader3.Free;
        tagReader2.Free;
        tagReader1.Free;

        ms.Free;
      end;
    end;
  end;
end;
<FN>

<# 28><FM>Why are my JPEGs so big?<FN>

Jpeg is a file format with variable compression rate. The property that regulates the compression (and the quality) is JPEG_Quality. So you should set this property before saving. For example:
<FC>
ImageEnView.IO.LoadFromFile('input.jpg');
ImageEnView.IO.Params.JPEG_Quality := 70;
ImageEnView.IO.SaveToFile('output.jpg');
<FN>
The default is 80, while other software uses 70.

To estimate the value used to create your file, use:
<FC>Quality := IECalcJpegFileQuality('input.jpg');<FN>

So you could write:
<FC>
ImageEnView.IO.LoadFromFile('input.jpg');
ImageEnView.IO.Params.JPEG_Quality := IECalcJpegFileQuality('input.jpg');
ImageEnView.IO.SaveToFile('output.jpg');
<FN>
If the file is still too large it may contain metatags (text info). To remove them call:

<FC>ImageEnView.IO.Params.ResetInfo;<FN>

..before saving.


<# 29><FM>In ImageEnVect, how do I save an image with many objects? And how can I load the objects if I've saved them?<FN>

Your options are as follows:

1) To save only objects:

<FC>ImageEnVect1.SaveToFileIEV('file.iev');<FN>

To load:

<FC>ImageEnVect1.LoadfromFileIEV('file.iev');<FN>


2) To save the image with the objects objects:

<FC>ImageEnVect1.SaveToFileAll('file.all');<FN>

To load:

<FC>ImageEnVect1.LoadFromFileAll('file.all');<FN>


3) To save the image and objects as a standard tiff using Imaging Annotations (readable by Windows Preview):
<FC>
ImageEnVect1.IO.SaveToFile('file.tif');
ImageEnVect1.SaveObjectsToTIFF('file.tif');
<FN>
To load:
<FC>
ImageEnVect1.IO.LoadFromFile('file.tif');
ImageEnVect1.LoadObjectsFromTIFF('file.tif');
<FN>

4)
To save the image and objects as a jpeg or other format which does not support Imaing Annotations (i.e. burn the objects into the image):
<FC>
ImageEnVect1.CopyObjectsToBack;
ImageEnVect1.IO.SaveToFile('file.jpg');
<FN>

<# 30><FM>How do I store/retrieve pages of a multiple image file in a blob field with TImagenDBView?<FN>

<A TImageEnDBView> cannot store/retrieve multiple pages in a blob. The best solution is to put a TImageEnMView component on the form and use it to load/save multiple pages as TIFFs in blobs using streams.

To store the content of TImageEnMView inside a blob write:
<FC>
var 
  tempStream : TMemoryStream;

tempStream := TMemoryStream.Create;
ImageEnMView1.MIO.SaveToStreamTIFF( tempStream );
BlobField.LoadFromStream( tempStream );
tempStream.free;
<FN>
To retrieve from a blob:
<FC>
tempStream := TMemoryStream.Create;
BlobField.SaveToStream( tempStream );
tempStream.Position := 0;
ImageEnMView1.MIO.LoadFromStreamTIFF( tempStream );
tempStream.free;
<FN>
If you use a TDataSet inherited data set you can create a blob stream without using an intermediate TMemoryStream. This will speed up the operation:
<FC>
BlobStream := myDataSet.CreateBlobStream(field, bmWrite);
ImageEnMView1.MIO.SaveToStreamTIFF( BlobStream );
BlobStream.Free;
<FN>
And...
<FC>
BlobStream := myDataSet.CreateBlobStream(field, bmRead);
ImageEnMView1.MiO.LoadFromStreamTIFF( BlobStream );
BlobStream.Free;
<FN>

Finally, when saving you can set compression info. For example, for black/white images you could write:
<FC>
ImageEnMView1.MIO.Params[0].TIFF_Compression := ioTIFF_G4FAX;
ImageEnMView1.MIO.DuplicateCompressionInfo;
<FN>And then save.


<# 32><FM>How to load the embedded jpeg from camera RAW images?<FN>

Unfortunately Camera RAW formats aren't documented, so support for all formats is not possible. However you can load the embedded jpegs from NEF, CR2 and DNG raw formats:

<FN>DNG:<FC>
ImageEnView1.IO.Params.TIFF_SubIndex := 1;
ImageEnView1.IO.LoadFromFileTIFF('input.dng');

<FN>CR2:<FC>
ImageEnView1.IO.Params.ImageIndex := 0;
ImageEnView1.IO.LoadFromFileTIFF('input.cr2');

<FN>NEF:<FC>
ImageEnView1.IO.Params.ImageIndex := 1;
ImageEnView1.IO.LoadFromFileTIFF('input.nef');

<FN>CRW:<FC>
ImageEnView1.IO.LoadJpegFromFileCRW('input.crw');


<# 33><FM>How do I terminate a polyline without double-clicking?<FN>

Just set:

ImageEnVect.PolylineEndingMode := ieemMouseUp;



<# 34><FM>Why am I unable to read recent Camera RAW formats?<FN>

In order to read recent Camera RAW files you should use the external dcraw plugin which is available from the Registered Users Download Page.



<# 35><FM>Measuring line length using miLineLen (or miArea) does not work. The mouse pointer always shows 0.00 pixels. Why is that?<FN>

miLineLen measures the perimeter of current selection. So a selection must be present. The same is for miArea which measures the area of current selection.

To measure the length of a line write instead:
<FN>ImageEnVect.MouseInteractVt := [miDragLen];<FN>



<# 36>Why do I get "Floating point overflow" when printing?</L>

This can happen on shared printers due to a bug in the VCL or printer drivers.

Try <L http://docwiki.embarcadero.com/CodeExamples/XE3/en/Set8087CW_%28Delphi%29>disabling FPU exceptions</L> by executing this before your printing code:

Set8087CW($133F);



<# 37>Why do I see horizontal/vertical bands when loading images using TImageEnMView?</L>

By default TImageEnMView loads thumbnails from the EXIF data when available to speed up display. Sometime these thumbnails contain black bands (horizontal or vertical bands).

To disable EXIF thumbnail loading set:

TImageEnMView.EnableLoadEXIFThumbnails := false;

!!}





////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Common Usage Scenarios<FN>

<IMG help_images\IEView_Component.gif>
See also: <A TImageEnView>, <A TImageEnIO> and <A TImageEnProc>

<IMG help_images\IEFolderMView_Component.gif>
See also: <A TImageEnFolderMView>, <A TImageEnMIO> and <A TImageEnProc>

<IMG help_images\IEMView_Thumbnails.gif>
See also: <A TImageEnMView>, <A TImageEnMIO> and <A TImageEnProc>

<IMG help_images\IEMView_Multiframe.gif>
See also: <A TImageEnMView>, <A TImageEnMIO> and <A TImageEnProc>

<IMG help_images\IEMView_Capture.gif>
See also: <A TImageEnMView>, <A TImageEnMIO> and <A TIEAcquireParams>
                                                                  
<IMG help_images\IEMediaFoundation_Grabbing.gif>
See also: <A TImageEnView>, <A TImageEnIO> and <A TIEMediaFoundationSourceReader>

<IMG help_images\IEView_Capture.gif>
See also: <A TImageEnView>, <A TImageEnIO> and <A TIEDirectShow>

<IMG help_images\IEVect_Component.gif>
See also: <A TImageEnVect>, <A TImageEnIO> and <A TImageEnProc>

!!}



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>ImageEn Hierarchy
<FN>

TObject

  • <A TIEScrollBarParams>

  • <A TIEMouseWheelParams>

  • <A TIEList>

      • <A TIEIntegerList>

      • <A TIEDoubleList>

  • <A TIEMarkerList>

  • <A TIEIPTCInfoList>

  • <A TIEImagingObject>

  • <A TIEImagingAnnot>

  • <A TIEMask>

  • <A TIEBaseBitmap>

      • <A TIEBitmap>

      • <A TIEDibBitmap>

  • <A TIETIFTagsReader>

  • <A TIETagsHandler>

  • <A TIETIFFHandler>

  • <A TIEICC>

  • <A TIEDICOMTags>

  • <A TIEDirectShow>

  • <A TIEMediaFoundationSourceReader>

  • <A TIEMediaReader>

  • <A TIEWia>

  • <A TIEWICReader>

  • <A TIEWICWriter>


    • TComponent

      • <A TImageEnMIO>

      • <A TImageEnIO> (encapsulates <A TIEDirectShow>, <A TIEWia>, <A TIEMediaFoundationSourceReader>)

      • <A TImageEnProc>

      • <A TImageEnVideoCap>

      • <A TIECommonDialog>

          • <A TOpenImageEnDialog>

              • <A TSaveImageEnDialog>

        • TCustomControl

          • <A TIEView>

              • <A TImageEnMView> (encapsulates <A TImageEnMIO>, <A TImageEnProc>)

              • <A TImageEnView> (encapsulates <A TImageEnIO>, <A TImageEnProc>)

                  • <A TImageEnDBView>

                      • <A TImageEnVect>

                      • <A TImageEnDBVect>

                      • <A TImageEnVideoView>

                  • <A TImageEn> alias of <A TImageEnView>

          • <A THistogramBox>

          • <A THSVBox>

          • <A TIEGradientBar>

          • <A TRulerBox>

            • TForm

              • <A TImageEnPaletteDialog>

!!}



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>Twain language constants
<FN>

  TWLG_DAN      =   0; // Danish
  TWLG_DUT      =   1; // Dutch
  TWLG_ENG      =   2; // International English
  TWLG_FCF      =   3; // French Canadian
  TWLG_FIN      =   4; // Finnish
  TWLG_FRN      =   5; // French
  TWLG_GER      =   6; // German
  TWLG_ICE      =   7; // Icelandic
  TWLG_ITN      =   8; // Italian
  TWLG_NOR      =   9; // Norwegian
  TWLG_POR      =  10; // Portuguese
  TWLG_SPA      =  11; // Spanish
  TWLG_SWE      =  12; // Swedish
  TWLG_USA      =  13; // U.S. English

  TWLG_USERLOCALE         =  $FFFF;
  TWLG_AFRIKAANS          =  14;
  TWLG_ALBANIA            =  15;
  TWLG_ARABIC             =  16;
  TWLG_ARABIC_ALGERIA     =  17;
  TWLG_ARABIC_BAHRAIN     =  18;
  TWLG_ARABIC_EGYPT       =  19;
  TWLG_ARABIC_IRAQ        =  20;
  TWLG_ARABIC_JORDAN      =  21;
  TWLG_ARABIC_KUWAIT      =  22;
  TWLG_ARABIC_LEBANON     =  23;
  TWLG_ARABIC_LIBYA       =  24;
  TWLG_ARABIC_MOROCCO     =  25;
  TWLG_ARABIC_OMAN        =  26;
  TWLG_ARABIC_QATAR       =  27;
  TWLG_ARABIC_SAUDIARABIA =  28;
  TWLG_ARABIC_SYRIA       =  29;
  TWLG_ARABIC_TUNISIA     =  30;
  TWLG_ARABIC_UAE         =  31;  // United Arabic Emirates
  TWLG_ARABIC_YEMEN       =  32;
  TWLG_BASQUE             =  33;
  TWLG_BYELORUSSIAN       =  34;
  TWLG_BULGARIAN          =  35;
  TWLG_CATALAN            =  36;
  TWLG_CHINESE            =  37;
  TWLG_CHINESE_HONGKONG   =  38;
  TWLG_CHINESE_PRC        =  39; // People's Republic of China
  TWLG_CHINESE_SINGAPORE  =  40;
  TWLG_CHINESE_SIMPLIFIED =  41;
  TWLG_CHINESE_TAIWAN     =  42;
  TWLG_CHINESE_TRADITIONAL=  43;
  TWLG_CROATIA            =  44;
  TWLG_CZECH              =  45;
  TWLG_DANISH             =  TWLG_DAN;
  TWLG_DUTCH              =  TWLG_DUT;
  TWLG_DUTCH_BELGIAN      =  46;
  TWLG_ENGLISH            =  TWLG_ENG;
  TWLG_ENGLISH_AUSTRALIAN =  47;
  TWLG_ENGLISH_CANADIAN   =  48;
  TWLG_ENGLISH_IRELAND    =   49;
  TWLG_ENGLISH_NEWZEALAND =  50;
  TWLG_ENGLISH_SOUTHAFRICA=  51;
  TWLG_ENGLISH_UK           =  52;
  TWLG_ENGLISH_USA          =  TWLG_USA;
  TWLG_ESTONIAN             =  53;
  TWLG_FAEROESE             =  54;
  TWLG_FARSI                =  55;
  TWLG_FINNISH              =  TWLG_FIN;
  TWLG_FRENCH               =  TWLG_FRN;
  TWLG_FRENCH_BELGIAN       =  56;
  TWLG_FRENCH_CANADIAN      =  TWLG_FCF;
  TWLG_FRENCH_LUXEMBOURG    =  57;
  TWLG_FRENCH_SWISS         =  58;
  TWLG_GERMAN               =  TWLG_GER;
  TWLG_GERMAN_AUSTRIAN      =  59;
  TWLG_GERMAN_LUXEMBOURG    =  60;
  TWLG_GERMAN_LIECHTENSTEIN =  61;
  TWLG_GERMAN_SWISS         =  62;
  TWLG_GREEK                =  63;
  TWLG_HEBREW               =  64;
  TWLG_HUNGARIAN            =  65;
  TWLG_ICELANDIC            =  TWLG_ICE;
  TWLG_INDONESIAN           =  66;
  TWLG_ITALIAN              =  TWLG_ITN;
  TWLG_ITALIAN_SWISS        =  67;
  TWLG_JAPANESE             =  68;
  TWLG_KOREAN               =  69;
  TWLG_KOREAN_JOHAB         =  70;
  TWLG_LATVIAN              =  71;
  TWLG_LITHUANIAN           =  72;
  TWLG_NORWEGIAN            =  TWLG_NOR;
  TWLG_NORWEGIAN_BOKMAL     =  73;
  TWLG_NORWEGIAN_NYNORSK    =  74;
  TWLG_POLISH               =  75;
  TWLG_PORTUGUESE           =  TWLG_POR;
  TWLG_PORTUGUESE_BRAZIL    =  76;
  TWLG_ROMANIAN             =  77;
  TWLG_RUSSIAN              =  78;
  TWLG_SERBIAN_LATIN        =  79;
  TWLG_SLOVAK               =  80;
  TWLG_SLOVENIAN            =  81;
  TWLG_SPANISH              =  TWLG_SPA;
  TWLG_SPANISH_MEXICAN      =  82;
  TWLG_SPANISH_MODERN       =  83;
  TWLG_SWEDISH              =  TWLG_SWE;
  TWLG_THAI                 =  84;
  TWLG_TURKISH              =  85;
  TWLG_UKRANIAN             =  86;
  TWLG_ASSAMESE             =   87;
  TWLG_BENGALI              =   88;
  TWLG_BIHARI               =   89;
  TWLG_BODO                 =   90;
  TWLG_DOGRI                =   91;
  TWLG_GUJARATI             =   92;
  TWLG_HARYANVI             =   93;
  TWLG_HINDI                =   94;
  TWLG_KANNADA              =   95;
  TWLG_KASHMIRI             =   96;
  TWLG_MALAYALAM            =   97;
  TWLG_MARATHI              =   98;
  TWLG_MARWARI              =   99;
  TWLG_MEGHALAYAN           =   100;
  TWLG_MIZO                 =   101;
  TWLG_NAGA                 =   102;
  TWLG_ORISSI               =   103;
  TWLG_PUNJABI              =   104;
  TWLG_PUSHTU               =   105;
  TWLG_SERBIAN_CYRILLIC     =   106;
  TWLG_SIKKIMI              =   107;
  TWLG_SWEDISH_FINLAND      =   108;
  TWLG_TAMIL                =   109;
  TWLG_TELUGU               =   110;
  TWLG_TRIPURI              =   111;
  TWLG_URDU                 =   112;
  TWLG_VIETNAMESE           =   113;


!!}


{!!
<FS>Twain country constants
<FN>

  TWCY_AFGHANISTAN    =  1001;
  TWCY_ALGERIA        =  213;
  TWCY_AMERICANSAMOA  =  684;
  TWCY_ANDORRA        =  033;
  TWCY_ANGOLA         =  1002;
  TWCY_ANGUILLA       =  8090;
  TWCY_ANTIGUA        =  8091;
  TWCY_ARGENTINA      =   54;
  TWCY_ARUBA          =  297;
  TWCY_ASCENSIONI     =  247;
  TWCY_AUSTRALIA      =   61;
  TWCY_AUSTRIA        =   43;
  TWCY_BAHAMAS        =  8092;
  TWCY_BAHRAIN        =  973;
  TWCY_BANGLADESH     =  880;
  TWCY_BARBADOS       =  8093;
  TWCY_BELGIUM        =   32;
  TWCY_BELIZE         =  501;
  TWCY_BENIN          =  229;
  TWCY_BERMUDA        =  8094;
  TWCY_BHUTAN         =  1003;
  TWCY_BOLIVIA        =  591;
  TWCY_BOTSWANA       =  267;
  TWCY_BRITAIN        =   6;
  TWCY_BRITVIRGINIS   =  8095;
  TWCY_BRAZIL         =   55;
  TWCY_BRUNEI         =  673;
  TWCY_BULGARIA       =  359;
  TWCY_BURKINAFASO    =  1004;
  TWCY_BURMA          =  1005;
  TWCY_BURUNDI        =  1006;
  TWCY_CAMAROON       =  237;
  TWCY_CANADA         =   2;
  TWCY_CAPEVERDEIS    =  238;
  TWCY_CAYMANIS       =  8096;
  TWCY_CENTRALAFREP   =  1007;
  TWCY_CHAD           =  1008;
  TWCY_CHILE          =   56;
  TWCY_CHINA          =   86;
  TWCY_CHRISTMASIS    =  1009;
  TWCY_COCOSIS        =  1009;
  TWCY_COLOMBIA       =   57;
  TWCY_COMOROS        =  1010;
  TWCY_CONGO          =  1011;
  TWCY_COOKIS         =  1012;
  TWCY_COSTARICA      =  506 ;
  TWCY_CUBA           =  005;
  TWCY_CYPRUS         =  357;
  TWCY_CZECHOSLOVAKIA =  42;
  TWCY_DENMARK        =   45;
  TWCY_DJIBOUTI       =  1013;
  TWCY_DOMINICA       =  8097;
  TWCY_DOMINCANREP    =  8098;
  TWCY_EASTERIS       =  1014;
  TWCY_ECUADOR        =  593;
  TWCY_EGYPT          =   20;
  TWCY_ELSALVADOR     =  503;
  TWCY_EQGUINEA       =  1015;
  TWCY_ETHIOPIA       =  251;
  TWCY_FALKLANDIS     =  1016;
  TWCY_FAEROEIS       =  298;
  TWCY_FIJIISLANDS    =  679;
  TWCY_FINLAND        =  358;
  TWCY_FRANCE         =   33;
  TWCY_FRANTILLES     =  596;
  TWCY_FRGUIANA       =  594;
  TWCY_FRPOLYNEISA    =  689;
  TWCY_FUTANAIS       =  1043;
  TWCY_GABON          =  241;
  TWCY_GAMBIA         =  220;
  TWCY_GERMANY        =   49;
  TWCY_GHANA          =  233;
  TWCY_GIBRALTER      =  350;
  TWCY_GREECE         =   30;
  TWCY_GREENLAND      =  299;
  TWCY_GRENADA        =  8099;
  TWCY_GRENEDINES     =  8015;
  TWCY_GUADELOUPE     =  590;
  TWCY_GUAM           =  671;
  TWCY_GUANTANAMOBAY  =  5399;
  TWCY_GUATEMALA      =  502;
  TWCY_GUINEA         =  224;
  TWCY_GUINEABISSAU   =  1017;
  TWCY_GUYANA         =  592;
  TWCY_HAITI          =  509;
  TWCY_HONDURAS       =  504;
  TWCY_HONGKONG       =  852 ;
  TWCY_HUNGARY        =   36;
  TWCY_ICELAND        =  354;
  TWCY_INDIA          =   91;
  TWCY_INDONESIA      =   62;
  TWCY_IRAN           =   98;
  TWCY_IRAQ           =  964;
  TWCY_IRELAND        =  353;
  TWCY_ISRAEL         =  972;
  TWCY_ITALY          =   39;
  TWCY_IVORYCOAST     =  225 ;
  TWCY_JAMAICA        =  8010;
  TWCY_JAPAN          =   81;
  TWCY_JORDAN         =  962;
  TWCY_KENYA          =  254;
  TWCY_KIRIBATI       =  1018;
  TWCY_KOREA          =   82;
  TWCY_KUWAIT         =  965;
  TWCY_LAOS           =  1019;
  TWCY_LEBANON        =  1020;
  TWCY_LIBERIA        =  231;
  TWCY_LIBYA          =  218;
  TWCY_LIECHTENSTEIN  =   41;
  TWCY_LUXENBOURG     =  352;
  TWCY_MACAO          =  853;
  TWCY_MADAGASCAR     =  1021;
  TWCY_MALAWI         =  265;
  TWCY_MALAYSIA       =   60;
  TWCY_MALDIVES       =  960;
  TWCY_MALI           =  1022;
  TWCY_MALTA          =  356;
  TWCY_MARSHALLIS     =  692;
  TWCY_MAURITANIA     =  1023;
  TWCY_MAURITIUS      =  230;
  TWCY_MEXICO         =   3;
  TWCY_MICRONESIA     =  691;
  TWCY_MIQUELON       =  508;
  TWCY_MONACO         =   33;
  TWCY_MONGOLIA       =  1024;
  TWCY_MONTSERRAT     =  8011;
  TWCY_MOROCCO        =  212;
  TWCY_MOZAMBIQUE     =  1025;
  TWCY_NAMIBIA        =  264;
  TWCY_NAURU          =  1026;
  TWCY_NEPAL          =  977;
  TWCY_NETHERLANDS    =   31;
  TWCY_NETHANTILLES   =  599;
  TWCY_NEVIS          =  8012;
  TWCY_NEWCALEDONIA   =  687;
  TWCY_NEWZEALAND     =   64;
  TWCY_NICARAGUA      =  505;
  TWCY_NIGER          =  227;
  TWCY_NIGERIA        =  234;
  TWCY_NIUE           =  1027;
  TWCY_NORFOLKI       =  1028;
  TWCY_NORWAY         =   47;
  TWCY_OMAN           =  968;
  TWCY_PAKISTAN       =   92;
  TWCY_PALAU          =  1029;
  TWCY_PANAMA         =  507;
  TWCY_PARAGUAY       =  595;
  TWCY_PERU           =   51;
  TWCY_PHILLIPPINES   =   63;
  TWCY_PITCAIRNIS     =  1030;
  TWCY_PNEWGUINEA     =  675;
  TWCY_POLAND         =   48;
  TWCY_PORTUGAL       =  351;
  TWCY_QATAR          =  974;
  TWCY_REUNIONI       =  1031;
  TWCY_ROMANIA        =   40;
  TWCY_RWANDA         =  250;
  TWCY_SAIPAN         =  670;
  TWCY_SANMARINO      =   39;
  TWCY_SAOTOME        =  1033;
  TWCY_SAUDIARABIA    =  966;
  TWCY_SENEGAL        =  221;
  TWCY_SEYCHELLESIS   =  1034;
  TWCY_SIERRALEONE    =  1035;
  TWCY_SINGAPORE      =   65;
  TWCY_SOLOMONIS      =  1036;
  TWCY_SOMALI         =  1037;
  TWCY_SOUTHAFRICA    =  27 ;
  TWCY_SPAIN          =   34;
  TWCY_SRILANKA       =   94;
  TWCY_STHELENA       =  1032;
  TWCY_STKITTS        =  8013;
  TWCY_STLUCIA        =  8014;
  TWCY_STPIERRE       =  508;
  TWCY_STVINCENT      =  8015;
  TWCY_SUDAN          =  1038;
  TWCY_SURINAME       =  597;
  TWCY_SWAZILAND      =  268;
  TWCY_SWEDEN         =   46;
  TWCY_SWITZERLAND    =   41;
  TWCY_SYRIA          =  1039;
  TWCY_TAIWAN         =  886;
  TWCY_TANZANIA       =  255;
  TWCY_THAILAND       =   66;
  TWCY_TOBAGO         =  8016;
  TWCY_TOGO           =  228;
  TWCY_TONGAIS        =  676;
  TWCY_TRINIDAD       =  8016;
  TWCY_TUNISIA        =  216;
  TWCY_TURKEY         =   90;
  TWCY_TURKSCAICOS    =  8017;
  TWCY_TUVALU         =  1040;
  TWCY_UGANDA         =  256;
  TWCY_USSR           =   7;
  TWCY_UAEMIRATES     =  971;
  TWCY_UNITEDKINGDOM  =   44;
  TWCY_USA            =   1;
  TWCY_URUGUAY        =  598;
  TWCY_VANUATU        =  1041;
  TWCY_VATICANCITY    =   39;
  TWCY_VENEZUELA      =   58;
  TWCY_WAKE           =  1042;
  TWCY_WALLISIS       =  1043;
  TWCY_WESTERNSAHARA  =  1044;
  TWCY_WESTERNSAMOA   =  1045;
  TWCY_YEMEN          =  1046;
  TWCY_YUGOSLAVIA     =   38;
  TWCY_ZAIRE          =  243;
  TWCY_ZAMBIA         =  260;
  TWCY_ZIMBABWE       =  263;
  TWCY_ALBANIA        =  355;
  TWCY_ARMENIA        =  374;
  TWCY_AZERBAIJAN     =  994;
  TWCY_BELARUS        =  375;
  TWCY_BOSNIAHERZGO   =  387;
  TWCY_CAMBODIA       =  855;
  TWCY_CROATIA        =  385;
  TWCY_CZECHREPUBLIC  =  420;
  TWCY_DIEGOGARCIA    =  246;
  TWCY_ERITREA        =  291;
  TWCY_ESTONIA        =  372;
  TWCY_GEORGIA        =  995;
  TWCY_LATVIA         =  371;
  TWCY_LESOTHO        =  266;
  TWCY_LITHUANIA      =  370;
  TWCY_MACEDONIA      =  389;
  TWCY_MAYOTTEIS      =  269;
  TWCY_MOLDOVA        =  373;
  TWCY_MYANMAR        =  95 ;
  TWCY_NORTHKOREA     =  850;
  TWCY_PUERTORICO     =  787;
  TWCY_RUSSIA         =  7 ;
  TWCY_SERBIA         =  381;
  TWCY_SLOVAKIA       =  421;
  TWCY_SLOVENIA       =  386;
  TWCY_SOUTHKOREA     =  82 ;
  TWCY_UKRAINE        =  380;
  TWCY_USVIRGINIS     =  340;
  TWCY_VIETNAM        =  84 ;
!!}



{!!
<FS>IEVision

<FM>Description<FN>
IEVision is an optional plugin for ImageEn which adds advanced vision features:

• Face detection (and other parts of the body, including "eye", "eye glasses", "full body", "lower body", "upper body", "face profile", "eye pair", "mouth", "nose")
• Face and objects tracking
• Basic OCR
• Bar code reading (EAN-13/UPC-A, UPC-E, EAN-8, Code 128, Code 39, Interleaved 2 of 5 and QR Code)
• Inpainting/Image patching (to hide blemishes and copy content)
• Several new image processing algorithms

In order to use IEVision you must copy the <FC>ievision.dll<FN> library to the same folder as your executable or in any location in the system path.
You can check if ievision.dll is accessible by ImageEn calling <A IEVisionAvailable> function. For example:

<FC>
if not IEVisionAvailable() then
begin
  ShowMessage('This application requires the ievision.dll plugin');
  Application.Terminate;
  exit;
end;
<FN>

The ievision unit exports all interfaces and helper functions, plus the <A IEVisionLib> public field which encapsulates the factory class <A TIEVisionLibrary>.

It is possible to encapsulate an ImageEn bitmap (<A TIEBitmap>) inside a IEVision bitmap (<A TIEVisionImage>), but it is necessary to change the TIEBitmap origin. For example:
<FC>
ImageEnView1.IEBitmap.Origin := ieboTOPLEFT;
image := IEVisionLib.createImage(ImageEnView1.IEBitmap.Width, ImageEnView1.IEBitmap.Height,
                                 ievUINT8, 3, ImageEnView1.IEBitmap.Rowlen,
                                 ImageEnView1.IEBitmap.ScanLine[0]);
<FN>
This way you can use all IEVision functions sharing the results to a TImageEnView embedded image. It is also possible to create an IEVision image from scratch and then copy it to TIEBitmap or TImageEnView.

An alternative way is to use <A TIEBitmap.GetIEVisionImage> which sets the bitmap origin and pixelformat for you:

<FC>
image := ImageEnView1.IEBitmap.GetIEVisionImage();
<FN>


All IEVision objects are implemented as COM objects, so you don't need to handle object destruction (it is done automatically at runtime).
For example, if you create an IEVision image with:
<FC>image := IEVisionLib.createImage(1000, 1000, ievUINT8, 3);<FN>
You don't need to free or dispose it.

IEVision is not part of ImageEn and is <L http://www.imageen.com/order/#IEVision>sold separately</L>.

<FM>Demos<FN>
IEVision\FaceDetection
IEVision\FaceDetection_LowLevel
IEVision\GetFaces
IEVision\Inpaint_Brush
IEVision\Inpaint_Selection
IEVision\OCR
IEVision\OCRwithLayout
IEVision\TrackObjects
IEVision\TrackObjects_LowLevel
IEVision\BarCode
IEVision\BarCodeCam

!!}


{!!
<FS>IEVision Helper Functions

<FM>IEVision Availability<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionAvailable></C> </R>
</TABLE>

<FM>Simple Record Constructors<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionSize></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionRect></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionPoint></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionScalar></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionFloatPair></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionTermCriteria></C> </R>
</TABLE>

!!}


{!!
<FS>Distributing IEVision

<FN>To take advantage of IEVision's functionality you will need to ship ievision.dll (or ievision64.dll) with your application.

ievision.dll should be installed in one of the following locations:

 • The same folder as your application EXE (RECOMMENDED)
 • The Windows System32 folder
 • A path specified in the Windows %PATH% environment variable

Distribution is royalty-free if you have purchased an ImageEn and IEVision license.
!!}


{!!
<FS>IEVision What's New

<FM>Version 2.0.0<FN>
- added bar code reading support (EAN-13/UPC-A, UPC-E, EAN-8, Code 128, Code 39, Interleaved 2 of 5 and QR Code)
- added TIEVisionBarCodeSymbol class
- added TIEVisionBarCodeScanner class
- added TIEVisionLibrary.createBarCodeScanner method
- added TIEVisionLibrary.getLibraryInfo method


<FM>Version 1.0.4<FN>
- updated OCR library (now supports more languages with enhanced accuracy)
- OCR: English is no more embedded
- OCR: createOCR accepts only language code instead of file name
- OCR: now supports multithreading
- OCR: removed TIEVisionOCR.setAccuracy method
- OCR: TIEVisionOCR.getRegions, added onlyText parameter to get only text regions
- OCR: TIEVisionOCRWordBox, added getConfidence method
- OCR: TIEVisionOCRWordBox, added isBold method
- OCR: TIEVisionOCRWordBox, added isItalic method
- OCR: TIEVisionOCRWordBox, added isUnderlined method
- OCR: TIEVisionOCRWordBox, added isMonospace method
- OCR: TIEVisionOCRWordBox, added isSerif method
- OCR: TIEVisionOCRWordBox, added isSmallCaps method
- OCR: TIEVisionOCRWordBox, added getPointSize method
- OCR: TIEVisionOCRWordBox, added getLanguage method
- OCR: TIEVisionOCRWordBox, added getScriptDirection method
- OCR: now supported loading and detecting multiple languages (on the same TIEVisionOCR object)

!!}




end.
