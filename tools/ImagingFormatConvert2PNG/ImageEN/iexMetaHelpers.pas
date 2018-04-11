{------------------------------------------------------------------------------}
{                                                                              }
{  Helper functions for working with EXIF, IPTC, Dicom meta data               }               
{                                                                              }
{  Nigel Cross                                                                 }
{  Xequte Software                                                             }
{  nigel@xequte.com                                                            }
{  http://www.xequte.com                                                       }
{                                                                              }
{  © Xequte Software 2009-2014                                                 }
{                                                                              }
{  TListView enhancements by William Miller, Adirondack Software & Graphics    }
{------------------------------------------------------------------------------}

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
File version 1030
*)

// STRING GRID SUPPORT
// Define to include methods for loading and saving content to/from TStringGrids
{$DEFINE USE_STRINGGRIDS}

// LIST VIEW SUPPORT
// Define to include methods for loading and saving content to/from TListViews
{$DEFINE USE_LISTVIEW}


unit iexMetaHelpers;

{$I ie.inc}

interface

{$ifdef IEHASUNICODEWARNS}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$endif}


uses
{$IFDEF USE_STRINGGRIDS}
  grids,
{$ENDIF}
{$IFDEF USE_LISTVIEW}
  ComCtrls,
{$ENDIF}
  ImageEnIO, dialogs, Sysutils, math, Graphics, Classes;

type

{!!
<FS>TIEDicomInclude

<FM>Declaration<FC>
TIEDicomInclude = set of (diDeprecated, diProprietary, diChildTags, diUnknown);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>diDeprecated<FN></C> <C>Include tags that have been retired in the NEMA standard</C> </R>
<R> <C><FC>diProprietary<FN></C> <C>Include tags that are not in the NEMA standard (vendor specific tags)</C> </R>
<R> <C><FC>diChildTags<FN></C> <C>Include subordinate tags (children of parent tags)</C> </R>
<R> <C><FC>diUnknown<FN></C> <C>Include tags even if they do not have a description</C> </R>
</TABLE>
!!}
  TIEDicomInclude = set of (diDeprecated, diProprietary, diChildTags, diUnknown);

  {$IFDEF Delphi2005orNewer}
{!!
<FS>TIOParamsValsHelper

<FM>Declaration<FC>
TIOParamsValsHelper = class helper for TIOParamsVals

<FM>Description<FN>
<A iexMetaHelpers>
!!}
  // TIOParamsValsHelper Helper Functions
  TIOParamsValsHelper = class helper for TIOParamsVals
  private
    function GetEXIF_ApertureValue_Str : string;
    function GetEXIF_Camera_Str : string;
    function GetEXIF_ExposureTime_Str : string;
    function GetEXIF_FNumber_Str : string;
    function GetEXIF_MaxApertureValue_Str : string;
    function GetEXIF_ShutterSpeedValue_Str : string;
    function GetEXIF_XResolution_Str : string;
    function GetEXIF_YResolution_Str : string;
    function GetEXIF_AsStr(Index: Integer): string;
    function GetEXIF_FieldDescription(Index: Integer): string;
    function GetEXIF_CanWriteEXIFData : Boolean;

    procedure SetEXIF_ApertureValue_Str(const value : string);
    procedure SetEXIF_Camera_Str(const value : string);
    procedure SetEXIF_ExposureTime_Str(value : string);
    procedure SetEXIF_FNumber_Str(value : string);
    procedure SetEXIF_MaxApertureValue_Str(const value : string);
    procedure SetEXIF_ShutterSpeedValue_Str(const value : string);
    procedure SetEXIF_XResolution_Str(value : string);
    procedure SetEXIF_YResolution_Str(value : string);
    procedure SetEXIF_AsStr(Index: Integer; value : string);
  public
    property EXIF_AsStr [index : Integer]: string read GetEXIF_AsStr write SetEXIF_AsStr;
    property EXIF_FieldDescription [index : Integer]: string read GetEXIF_FieldDescription;
    property EXIF_CanWriteEXIFData : Boolean read GetEXIF_CanWriteEXIFData;

    property EXIF_ApertureValue_Str : string read GetEXIF_ApertureValue_Str write SetEXIF_ApertureValue_Str;
    property EXIF_Camera_Str : string read GetEXIF_Camera_Str write SetEXIF_Camera_Str;
    property EXIF_ExposureTime_Str : string read GetEXIF_ExposureTime_Str write SetEXIF_ExposureTime_Str;
    property EXIF_FNumber_Str : string read GetEXIF_FNumber_Str write SetEXIF_FNumber_Str;
    property EXIF_MaxApertureValue_Str : string read GetEXIF_MaxApertureValue_Str write SetEXIF_MaxApertureValue_Str;
    property EXIF_ShutterSpeedValue_Str : string read GetEXIF_ShutterSpeedValue_Str write SetEXIF_ShutterSpeedValue_Str;
    property EXIF_XResolution_Str : string read GetEXIF_XResolution_Str write SetEXIF_XResolution_Str;
    property EXIF_YResolution_Str : string read GetEXIF_YResolution_Str write SetEXIF_YResolution_Str;

    function EXIF_WriteToStrings(Dest : TStrings) : Boolean;
    function IPTC_WriteToStrings(Dest : TStrings) : Boolean;
    function DICOM_WriteToStrings(Dest : TStrings; TagInclude: TIEDicomInclude) : Boolean;
  end;
  {$ENDIF}
         

{$IFDEF USE_STRINGGRIDS}
  {$IFDEF Delphi2005orNewer}
{!!
<FS>TStringGridHelper

<FM>Declaration<FC>
TStringGridHelper = class helper for TStringGrid

<FM>Description<FN>
<A iexMetaHelpers>
!!}
  // TStringGridHelper Helper Functions
  TStringGridHelper = class helper for TStringGrid
  private

  public
    // EXIF
    procedure NewGridForEXIF(bReadOnly : Boolean = False);
    function ReadGridFromExif(IOParams : TIOParamsVals; bReadOnly : Boolean = False) : Boolean; overload;
    function ReadGridFromExif(const sFilename : string; bReadOnly : Boolean = False) : Boolean; overload;
    function WriteGridToExif(IOParams : TIOParamsVals) : Boolean; overload;
    function WriteGridToExif(const sFilename : string) : Boolean; overload;

    // IPTC
    procedure NewGridForIPTC(bReadOnly : Boolean = False);
    function ReadGridFromIPTC(IOParams : TIOParamsVals; bReadOnly : Boolean = False) : Boolean; overload;
    function ReadGridFromIPTC(const sFilename : string; bReadOnly : Boolean = False) : Boolean; overload;
    function WriteGridToIPTC(IOParams : TIOParamsVals) : Boolean; overload;
    function WriteGridToIPTC(const sFilename : string) : Boolean; overload;

    // DICOM
    function ReadGridFromDicom(IOParams : TIOParamsVals; TagInclude: TIEDicomInclude) : Boolean; overload;
    function ReadGridFromDicom(const sFilename : string; TagInclude: TIEDicomInclude) : Boolean; overload;

    // GENERAL
    procedure InitializeGrid(bFixLayout : Boolean = True);
    procedure ClearGridFields;
  end;
  {$ENDIF}
{$ENDIF}


{$IFDEF USE_LISTVIEW}
  {$IFDEF Delphi2005orNewer}
{!!
<FS>TListViewHelper

<FM>Declaration<FC>
TListViewHelper = class helper for TListView

<FM>Description<FN>
<A iexMetaHelpers>
!!}
  // TListViewHelper Helper Functions
  TListViewHelper = class helper for TListView
  private

  public
    // EXIF
    procedure NewListForExif();
    function ReadListFromExif(IOParams : TIOParamsVals) : Boolean; overload;
    function ReadListFromExif(const sFilename : string) : Boolean; overload;
    function WriteListToExif(IOParams : TIOParamsVals) : Boolean; overload;
    function WriteListToExif(const sFilename : string) : Boolean; overload;

    // IPTC
    procedure NewListForIPTC();
    function ReadListFromIPTC(IOParams : TIOParamsVals) : Boolean; overload;
    function ReadListFromIPTC(const sFilename : string) : Boolean; overload;
    function WriteListToIPTC(IOParams : TIOParamsVals) : Boolean; overload;
    function WriteListToIPTC(const sFilename : string) : Boolean; overload;

    // DICOM
    function ReadListFromDicom(IOParams : TIOParamsVals; TagInclude: TIEDicomInclude) : Boolean; overload;
    function ReadListFromDicom(const sFilename : string; TagInclude: TIEDicomInclude) : Boolean; overload;

    // GENERAL
    procedure InitializeList(bFixLayout : Boolean = True);
    procedure ClearListFields;
  end;
  {$ENDIF}
{$ENDIF}

  // GENERAL EXIF METHODS
  function ExifCompatibleFile(const sFilename: string): boolean;
  {$IFDEF Delphi2005orNewer}
  function ReadExifCameraFieldsFromFile(const sFilename : string;
                                        out sCameraModel : string;
                                        out sExposureTime : string;
                                        out sFlashMode : string
                                        ) : boolean;
  {$ENDIF}
  function ReadExifGPSFieldsFromFile(const sFilename : string;
                                     out GPSLatitude: Double;
                                     out GPSLongitude: Double
                                     ) : boolean; overload;
  function ReadExifGPSFieldsFromFile(const sFilename : string;
                                     out sGPSLatitude: string;
                                     out sGPSLongitude: string
                                     ) : boolean; overload;

  // EXIF TAG METHODS
  procedure GetExifTagList(ssDest: TStrings);
  function ContainsExifTag(const sText: string): Boolean;
  {$IFDEF Delphi2005orNewer}
  function ReplaceExifTags(const sText: string; EXIFSource : TIOParamsVals): string; overload;
  function ReplaceExifTags(const sText: string; const sEXIFSourceFilename : string): string; overload;
  {$ENDIF}

  // GENERAL IPTC METHODS
  function IPTCCompatibleFile(sFilename: string): boolean;
  function ReadIPTCDescriptionAndKeywordsFromFile(const sFilename: string;
                                                  out sDescription: string;
                                                  ssKeywords: TStrings = nil) : Boolean;
  procedure WriteIPTCDescriptionAndKeywordsToFile(const sFilename: string;
                                                  sDescription: string;
                                                  ssKeywords: TStrings = nil);

{!!
<FS>EXIF Consts for EXIF_AsStr<FN>

<FM>Declaration<FC>
}
const
  _EXIF_UserComment           = 0;
  _EXIF_ImageDescription      = 1;
  _EXIF_CameraMake            = 2;
  _EXIF_CameraModel           = 3;
  _EXIF_XResolution           = 4;
  _EXIF_YResolution           = 5;
  _EXIF_DateTime              = 6;
  _EXIF_DateTimeOriginal      = 7;
  _EXIF_DateTimeDigitized     = 8;
  _EXIF_Copyright             = 9;
  _EXIF_Orientation           = 10;
  _EXIF_ExposureTime          = 11;
  _EXIF_FNumber               = 12;
  _EXIF_ExposureProgram       = 13;
  _EXIF_ISOSpeedRatings       = 14;
  _EXIF_ShutterSpeedValue     = 15;
  _EXIF_ApertureValue         = 16;
  _EXIF_BrightnessValue       = 17;
  _EXIF_ExposureBiasValue     = 18;
  _EXIF_MaxApertureValue      = 19;
  _EXIF_SubjectDistance       = 20;
  _EXIF_MeteringMode          = 21;
  _EXIF_LightSource           = 22;
  _EXIF_Flash                 = 23;
  _EXIF_FocalLength           = 24;
  _EXIF_FlashPixVersion       = 25;
  _EXIF_ColorSpace            = 26;
  _EXIF_ExifImageWidth        = 27;
  _EXIF_ExifImageHeight       = 28;
  _EXIF_RelatedSoundFile      = 29;
  _EXIF_FocalPlaneXResolution = 30;
  _EXIF_FocalPlaneYResolution = 31;
  _EXIF_ExposureIndex         = 32;
  _EXIF_SensingMethod         = 33;
  _EXIF_FileSource            = 34;
  _EXIF_SceneType             = 35;
  _EXIF_YCbCrPositioning      = 36;
  _EXIF_ExposureMode          = 37;
  _EXIF_WhiteBalance          = 38;
  _EXIF_DigitalZoomRatio      = 39;
  _EXIF_FocalLengthIn35mmFilm = 40;
  _EXIF_SceneCaptureType      = 41;
  _EXIF_GainControl           = 42;
  _EXIF_Contrast              = 43;
  _EXIF_Saturation            = 44;
  _EXIF_Sharpness             = 45;
  _EXIF_SubjectDistanceRange  = 46;
  _EXIF_GPSLatitude           = 47;
  _EXIF_GPSLongitude          = 48;
  _EXIF_GPSAltitude           = 49;
  _EXIF_GPSImageDirection     = 50;
  _EXIF_GPSTrack              = 51;
  _EXIF_GPSSpeed              = 52;
  _EXIF_GPSDateAndTime        = 53;
  _EXIF_GPSSatellites         = 54;
  _EXIF_GPSVersionID          = 55;
  _EXIF_Artist                = 56;
  _EXIF_XPTitle               = 57;
  _EXIF_XPComment             = 58;
  _EXIF_XPAuthor              = 59;
  _EXIF_XPKeywords            = 60;
  _EXIF_XPSubject             = 61;
  _EXIF_XPRating              = 62;
  _EXIF_InteropVersion        = 63;

  _EXIF_Tag_Count             = 64;
{!!}

  {  SKIPPED :

  Skipped the following fields :

    EXIF_Bitmap
    EXIF_ResolutionUnit
    EXIF_SubsecTime
    EXIF_SubsecTimeOriginal
    EXIF_SubsecTimeDigitized
    EXIF_WhitePoint
    EXIF_PrimaryChromaticities
    EXIF_YCbCrCoefficients
    EXIF_ReferenceBlackWhite
    EXIF_CompressedBitsPerPixel
    EXIF_FocalPlaneResolutionUnit
    EXIF_ExifVersion
    EXIF_Software
    EXIF_MakerNote
    EXIF_ImageUniqueID
    EXIF_GPSStatus
    EXIF_GPSMapDatum
    EXIF_GPSMeasureMode
    EXIF_GPSDOP
    EXIF_GPSDestLatitudeRef
    EXIF_GPSDestLatitudeDegrees
    EXIF_GPSDestLatitudeMinutes
    EXIF_GPSDestLatitudeSeconds
    EXIF_GPSDestLongitudeRef
    EXIF_GPSDestLongitudeDegrees
    EXIF_GPSDestLongitudeMinutes
    EXIF_GPSDestLongitudeSeconds
    EXIF_GPSDestBearingRef
    EXIF_GPSDestBearing
    EXIF_GPSDestDistanceRef
    EXIF_GPSDestDistance
    EXIF_InteropIndex
  }
  Example_File_Only = 'Example File';
  SKIP_DESCRIPTION = 'SKIP_DESCRIPTION';

  Maintain_File_Dates_On_Meta_Write : Boolean = False; // Set to true if updating the EXIF data should not modify the file date


implementation

uses
  Windows, hyieutils, hyiedefs, iedicom;

type
  TIEMetaType = (iemEXIF, iemIPTC, iemDicom);

  TExifTag = record
    Desc    : string;     // Description
    VarType : Integer;    // Type of field
    Edit    : boolean;    // Write support available for field
  end;

resourcestring

  // EXIF Data
  s_UserComment             = 'User Comment';
  s_Description             = 'Description';
  s_CameraMake              = 'Camera Make';
  s_CameraModel             = 'Camera Model';
  s_Orientation             = 'Camera Orientation';
  s_Inch                    = 'inch';
  s_CM                      = 'cm';
  s_PerInch                 = ' / inch';
  s_PerCM                   = ' / cm';
  s_XResolution             = 'Horizontal Resolution';
  s_Yresolution             = 'Vertical Resolution';
  s_DateTime                = 'Date and Time';
  s_DateTimeOriginal        = 'Original Date and Time';
  s_DateTimeDigitized       = 'Digitized Date and Time';
  s_Copyright               = 'Copyright';
  s_ExposureTime            = 'Exposure Time';
  s_FNumber                 = 'F-Stop';
  s_ExposureProgram         = 'Exposure Program';
  s_ManualControl           = 'Manual';
  s_ProgramNormal           = 'Normal';
  s_AperturePriority        = 'Aperture Priority';
  s_ShutterPriority         = 'Shutter Priority';
  s_CreativeProgram         = 'Creative Program';
  s_ActionProgram           = 'Action Program';
  s_Portraitmode            = 'Portrait Mode';
  s_LandscapeMode           = 'Landscape Mode';
  s_CompressedBitsPerPixel  = 'Compression Ratio';
  s_ShutterSpeedValue       = 'Shutter Speed';
  s_ApertureValue           = 'Aperture Value';
  s_ISOSpeedRatings         = 'ISO Speed Rating';
  s_BrightnessValue         = 'Brightness';
  s_ExposureBiasValue       = 'Exposure Compensation';
  s_MaxApertureValue        = 'Max Aperture Value';
  s_SubjectDistance         = 'Subject Distance';
  s_MeteringMode            = 'Metering Mode';
  s_average                 = 'Average';
  s_CenterWeightedAverage   = 'Center-weighted Average';
  s_Spot                    = 'Spot';
  s_MultiSpot               = 'Multi-Spot';
  s_MultiSegment            = 'Multi-Segment';
  s_partial                 = 'Partial';
  s_Lighting                = 'Lighting';
  s_daylight                = 'Daylight';
  s_fluorescent             = 'Flourescent';
  s_tungsten                = 'Tungsten';
  s_flash                   = 'Flash';
  s_standardLightA          = 'Standard Light A';
  s_standardLightB          = 'Standard Light B';
  s_standardLightC          = 'Standard Light C';
  s_D55                     = 'D55';
  s_D65                     = 'D65';
  s_D75                     = 'D75';
  s_FlashDidNotFire         = 'Not Used';
  s_flashFired              = 'Fired';
  s_flashFiredNoStrobeLight = 'Fired, No Strobe Return';
  s_flashFiredStrobeLight   = 'Fired, with Strobe Return';
  s_FocalLength             = 'Focal Length';
  s_FlashPixVersion         = 'FlashPix Version';
  s_ColorSpace              = 'Color Space';
  s_RGB                     = 'RGB';
  s_Uncalibrated            = 'Uncalibrated';
  s_WhiteBalance            = 'White Balance';
  s_ExifImageWidth          = 'Image Width';
  s_ExifImageHeight         = 'Image Height';
  s_RelatedSoundFile        = 'Sound File';
  s_FocalPlaneXResolution   = 'Focal Plane Horz. Resolution';
  s_FocalPlaneYResolution   = 'Focal Plane Vert. Resolution';
  s_ExposureIndex           = 'Exposure Index';
  s_SensingMethod           = 'Sensing Method';
  s_OneChipColorAreaSensor  = 'Single Chip Color Area';
  s_UnknownMethod           = 'Unknown Method';
  s_FileSource              = 'File Source';
  s_DigitalStillCamera      = 'Digital Still Camera';
  s_UnknownDevice           = 'Unknown Device';
  s_SceneType               = 'Scene Type';
  s_DirectlyPhotographed    = 'Directly Photographed';
  s_YcbCrPositioning        = 'Chroma Sample Point';
  s_Centered                = 'Centered';
  s_DataPoint               = 'Data Point';
  s_Seconds                 = 'Seconds';
  s_Second                  = 'Second';
  s_EXIFOrientation1        = 'Orientated Correctly';
  s_EXIFOrientation2        = 'Horizontally Flipped';
  s_EXIFOrientation3        = 'Offset by 180°';
  s_EXIFOrientation4        = 'Vertically Flipped';
  s_EXIFOrientation5        = 'Flipped Horiz. and Offset 90° CCW';
  s_EXIFOrientation6        = 'Offset by 90° CCW';
  s_EXIFOrientation7        = 'Flipped Horiz. and Offset 90° CW';
  s_EXIFOrientation8        = 'Offset by 90° Clockwise';

  s_ExposureMode            = 'Exposure Mode';
  s_DigitalZoomRatio        = 'Digital Zoom Ratio';
  s_FocalLengthIn35mmFilm   = 'Focal Length in 35mm Film';
  s_SceneCaptureType        = 'Scene Capture Type';
  s_GainControl             = 'Gain Control';
  s_Contrast                = 'Contrast';
  s_Saturation              = 'Saturation';
  s_Sharpness               = 'Sharpness';
  s_SubjectDistanceRange    = 'Subject Distance';
  s_GPSLatitude             = 'GPS Latitude'; 
  s_GPSLongitude            = 'GPS Longitude';
  s_GPSAltitude             = 'GPS Altitude';
  s_GPSImageDirection       = 'GPS Image Direction';
  s_GPSTrack                = 'GPS Movement Direction';
  s_GPSSpeed                = 'GPS Movement Speed';
  s_GPSDateAndTime          = 'GPS Date and Time';
  s_GPSSatellites           = 'GPS Satellites';
  s_GPSVersionID            = 'GPS Version';

  s_AutoExposure            = 'Auto exposure';
  s_ManualExposure          = 'Manual exposure';
  s_AutoBracket             = 'Auto bracket';

  s_Autowhitebalance        = 'Auto white balance';
  s_Manualwhitebalance      = 'Manual white balance';

  s_Standard                = 'Standard';
  s_Landscape               = 'Landscape';
  s_Portrait                = 'Portrait';
  s_NightScene              = 'Night scene';

  s_None                    = 'None';
  s_LowGainup               = 'Low gain up';
  s_HighGainup              = 'High gain up';
  s_LowGaindown             = 'Low gain down';
  s_HighGaindown            = 'High gain down';

  s_Normal                  = 'Normal';
  s_Soft                    = 'Soft';
  s_Hard                    = 'Hard';

  s_LowSaturation           = 'Low saturation';
  s_HighSaturation          = 'High saturation';

  s_Macro                   = 'Macro';
  s_CloseView               = 'Close view';
  s_DistantView             = 'Distant view';

  s_Artist                  = 'Artist';
  s_XPTitle                 = 'Title (Windows)';
  s_XPComment               = 'Comment (Windows)';
  s_XPAuthor                = 'Author (Windows)';
  s_XPKeywords              = 'Keywords (Windows)';
  s_XPSubject               = 'Subject (Windows)';
  s_XPRating                = 'Rating (Windows)';

  s_InteropVersion          = 'Interoperability Version';

const
  _vString  = 1;
  _vInteger = 2;
  _vDouble  = 3;

  Digital_Raw_Camera_Formats = '*.ARW;*.CRW;*.CR2;*.DNG;*.NEF;*.RAW;*.PEF;*.RAF;*.X3F;*.BAY;*.ORF;*.MRW;*.SRF;*.DCR;';
  EXIF_COMPATIBLE_EXTENSIONS = '*.TIF;*.TIFF;*.JPE;*.JPG;*.JPEG;' + Digital_Raw_Camera_Formats;

  ExifTags : array [0.._EXIF_Tag_Count - 1] of TExifTag = (
          (Desc :  s_UserComment           ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_UserComment
          (Desc :  s_Description           ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_ImageDescription
          (Desc :  s_CameraMake            ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_Make
          (Desc :  s_CameraModel           ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_Model
          (Desc :  s_XResolution           ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_XResolution
          (Desc :  s_YResolution           ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_YResolution
          (Desc :  s_DateTime              ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_DateTime
          (Desc :  s_DateTimeOriginal      ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_DateTimeOriginal
          (Desc :  s_DateTimeDigitized     ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_DateTimeDigitized
          (Desc :  s_Copyright             ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_Copyright
          (Desc :  s_Orientation           ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_Orientation
          (Desc :  s_ExposureTime          ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_ExposureTime
          (Desc :  s_FNumber               ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_FNumber
          (Desc :  s_ExposureProgram       ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_ExposureProgram
          (Desc :  s_ISOSpeedRatings       ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_ISOSpeedRatings
          (Desc :  s_ShutterSpeedValue     ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_ShutterSpeedValue
          (Desc :  s_ApertureValue         ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_ApertureValue
          (Desc :  s_BrightnessValue       ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_BrightnessValue
          (Desc :  s_ExposureBiasValue     ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_ExposureBiasValue
          (Desc :  s_MaxApertureValue      ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_MaxApertureValue
          (Desc :  s_SubjectDistance       ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_SubjectDistance
          (Desc :  s_MeteringMode          ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_MeteringMode
          (Desc :  s_Lighting              ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_LightSource
          (Desc :  s_Flash                 ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_Flash
          (Desc :  s_FocalLength           ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_FocalLength
          (Desc :  s_FlashPixVersion       ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_FlashPixVersion
          (Desc :  s_ColorSpace            ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_ColorSpace
          (Desc :  s_ExifImageWidth        ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_ExifImageWidth
          (Desc :  s_ExifImageHeight       ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_ExifImageHeight
          (Desc :  s_RelatedSoundFile      ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_RelatedSoundFile
          (Desc :  s_FocalPlaneXResolution ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_FocalPlaneXResolution
          (Desc :  s_FocalPlaneYResolution ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_FocalPlaneYResolution
          (Desc :  s_ExposureIndex         ;  VarType :  _vDouble;   Edit :  TRUE   ),  // _EXIF_ExposureIndex
          (Desc :  s_SensingMethod         ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_SensingMethod
          (Desc :  s_FileSource            ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_FileSource
          (Desc :  s_SceneType             ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_SceneType
          (Desc :  s_YCbCrPositioning      ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_YCbCrPositioning
          (Desc :  s_ExposureMode          ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_ExposureMode
          (Desc :  s_WhiteBalance          ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_WhiteBalance
          (Desc :  s_DigitalZoomRatio      ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_DigitalZoomRatio
          (Desc :  s_FocalLengthIn35mmFilm ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_FocalLengthIn35mmFilm
          (Desc :  s_SceneCaptureType      ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_SceneCaptureType
          (Desc :  s_GainControl           ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_GainControl
          (Desc :  s_Contrast              ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_Contrast
          (Desc :  s_Saturation            ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_Saturation
          (Desc :  s_Sharpness             ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_Sharpness
          (Desc :  s_SubjectDistanceRange  ;  VarType :  _vInteger;  Edit :  FALSE  ),  // _EXIF_SubjectDistanceRange
          (Desc :  s_GPSLatitude           ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_GPSLatitude
          (Desc :  s_GPSLongitude          ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_GPSLongitude
          (Desc :  s_GPSAltitude           ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_GPSAltitude
          (Desc :  s_GPSImageDirection     ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_GPSImageDirection
          (Desc :  s_GPSTrack              ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_GPSTrack
          (Desc :  s_GPSSpeed              ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_GPSSpeed
          (Desc :  s_GPSDateAndTime        ;  VarType :  _vDouble;   Edit :  FALSE  ),  // _EXIF_GPSDateAndTime
          (Desc :  s_GPSSatellites         ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_GPSSatellites
          (Desc :  s_GPSVersionID          ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_GPSVersionID
          (Desc :  s_Artist                ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_Artist
          (Desc :  s_XPTitle               ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_XPTitle
          (Desc :  s_XPComment             ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_XPComment
          (Desc :  s_XPAuthor              ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_XPAuthor
          (Desc :  s_XPKeywords            ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_XPKeywords
          (Desc :  s_XPSubject             ;  VarType :  _vString;   Edit :  TRUE   ),  // _EXIF_XPSubject
          (Desc :  s_XPRating              ;  VarType :  _vInteger;  Edit :  TRUE   ),  // _EXIF_XPRating
          (Desc :  s_InteropVersion        ;  VarType :  _vString;   Edit :  TRUE   )   // _EXIF_InteropVersion
          );

  // Exif Tags are a representation of the exif field that a user can put in a text string to be replaced
  // dymanically by the actual value read from the file, e.g. %EXIF-User-Comment%
  Exif_Tag_Prefix = '%EXIF-';
  Exif_Tag_Suffix = '%';



resourcestring
  s_Title                         =  'Title';    // Object name
  s_Caption                       =  'Caption';  // Caption/Abstract
  s_Keywords                      =  'Keywords';
  s_SpecialInstructions           =  'Special Instructions';
  s_DateCreated                   =  'Date Created (YYYYMMDD)';
  s_TimeCreated                   =  'Time Created (HHMMSS±HHMM)';
  s_Byline1                       =  'By-line 1';
  s_Byline2                       =  'By-line 2';
  s_City                          =  'City';
  s_s_StateProvince               =  'State/Province';
  s_CountryPrimaryLocationCode    =  'Country/Primary Location Code';
  s_CountryPrimaryLocationName    =  'Country/Primary Location Name';
  s_OriginalTransmissionReference =  'Original Transmission Reference';
  s_Credit                        =  'Credit';
  s_Source                        =  'Source';
  s_WriterEditor                  =  'Writer/Editor';
  s_Editstatus                    =  'Edit status';
  s_Urgency                       =  'Urgency';
  s_Category                      =  'Category';
  s_SupplementalCategory          =  'Supplemental Category';
  s_FixtureIdentifier             =  'Fixture Identifier';
  s_ReleaseDate                   =  'Release Date (YYYYMMDD)';
  s_ReleaseTime                   =  'Release Time (HHMMSS±HHMM)';
  s_ReferenceService              =  'Reference Service';
  s_ReferenceDate                 =  'Reference Date (YYYYMMDD)';
  s_ReferenceNumber               =  'Reference Number';
  s_OriginatingProgram            =  'Originating Program';
  s_ProgramVersion                =  'Program Version';
  s_ObjectCycle                   =  'Object Cycle (a=AM, b=PM, c=both)';
  s_CopyrightNotice               =  'Copyright Notice';
  s_ImageType                     =  'Image Type';

type
  TIPTCTag = record
    Rec  : integer;  // Record Number
    DSet : integer;  // Data Set
    Desc : string;   // Field Description
  end;

const
  IPTC_COMPATIBLE_EXTENSIONS = '*.TIF;*.TIFF;*.JPE;*.JPG;*.JPEG;';

  IPTCTags : array [0..30] of TIPTCTag = (
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Title;                  Desc :   s_Title                         ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Caption;                Desc :   s_Caption                       ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Keywords;               Desc :   s_Keywords                      ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Instructions;           Desc :   s_SpecialInstructions           ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Date_Created;           Desc :   s_DateCreated                   ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Time_Created;           Desc :   s_TimeCreated                   ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Byline_1;               Desc :   s_Byline1                       ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Byline_2;               Desc :   s_Byline2                       ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_City;                   Desc :   s_City                          ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_State_Province;         Desc :   s_s_StateProvince               ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Country_Code;           Desc :   s_CountryPrimaryLocationCode    ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Country;                Desc :   s_CountryPrimaryLocationName    ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Transmission_Reference; Desc :   s_OriginalTransmissionReference ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Credit;                 Desc :   s_Credit                        ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Source;                 Desc :   s_Source                        ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Writer;                 Desc :   s_WriterEditor                  ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Edit_Status;            Desc :   s_Editstatus                    ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Urgency;                Desc :   s_Urgency                       ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Category;               Desc :   s_Category                      ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Category_2;             Desc :   s_SupplementalCategory          ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Fixture_Identifier;     Desc :   s_FixtureIdentifier             ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Release_Date;           Desc :   s_ReleaseDate                   ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Release_Time;           Desc :   s_ReleaseTime                   ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Reference_Service;      Desc :   s_ReferenceService              ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Reference_Date;         Desc :   s_ReferenceDate                 ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Reference_Number;       Desc :   s_ReferenceNumber               ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Originating_Program;    Desc :   s_OriginatingProgram            ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Program_Version;        Desc :   s_ProgramVersion                ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Object_Cycle;           Desc :   s_ObjectCycle                   ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Copyright_Notice;       Desc :   s_CopyrightNotice               ),
    (Rec : PhotoShop_IPTC_Records;    DSet : IPTC_PS_Image_Type;             Desc :   s_ImageType                     )
    );
        
resourcestring
  s_Unknown = 'Unknown';

// Return a double value as a fraction string
function DoubleToFraction(dValue: double): string;
begin
  if (dValue = 0) or (dValue = -1) then
    result := ''
  else
    result := '1/' + FloatToStr(dValue);
end;

// Convert a fraction string to a double
function FractionToDouble(dValue: string): double;
begin
  if pos('1/', dValue) <> 1 then
    raise EConvertError.create('Not a fraction');    

  delete(dValue, 1, 2);
  result := StrToFloat(dValue);
end;

// Return a float as string, if the value is not 0 or -1
function FloatToStrOrNull(dValue: double; sAppend: string): string;
begin
  if (dValue = 0) or (dValue = -1) then
    result := ''
  else
    result := FloatToStr(dValue) + sAppend;
end;

// Remove sStrip from the start of the string value
function StripPrefix(const Value: string; const sStrip: string): string;
begin
  result := value;
  if pos(uppercase(sStrip), uppercase(result)) = 1 then
    Delete(result, 1, length(sStrip));
end;

// Remove sStrip from the end of the string value
function StripSuffix(const Value: string; const sStrip: string): string;
begin
  result := value;
  if pos(uppercase(sStrip), uppercase(result)) = (length(result) - length(sStrip)) + 1 then
    SetLength(result, length(result) - length(sStrip));
end;

// Convert an apex value to a string
function ApexToStr(dBase: double;
                   dValue: double;
                   const sPrepend: string): string;
begin
  try
    if (dBase = 0) or (dValue = -1) then
      raise EConvertError.create('Invalid value');

    dValue := Power(dBase, dValue);

    if (sPrepend = '1/') and
       (dValue > 0) and
       (dvalue <= 1) then
      // cope with values such as 0.17
      result := IEFloatToFormatString(dValue, 1, True)
    else
      result := sPrepend + IEFloatToFormatString(dValue, 1, True);
  except
    result := '';
  end;
end;

// Convert a string to an apex value
function StrToApex(dBase: double;
                   Value: string;
                   sPrepended: string
                   ): double;
var
  dValue: double;                   
begin
  if (sPrepended = '1/') and
     (pos('1/', Value) = 0) then
  begin
    dValue := 1 / StrToFloat(Value);
  end
  else
  begin
    Value := StripPrefix(Value, sPrepended);
    dValue := StrToFloat(Value);
  end;

  Result :=  LogN(dBase, dValue);
end;

// Remove the null terminator from a string
function RemoveNull(sValue: string): string;
begin
  result := trim(svalue);
  if (result <> '') and
    (result[length(result)] = #0) then
    SetLength(result, length(result) - 1);
  result := trim(result);
end;

// Convert EXIF_Flash to a descriptive string
function FlashModeToString(iFlash: Integer): string;
begin
  Case iFlash of
    0: result := s_FlashDidNotFire;
    1: result := s_flashFired;
    5: result := s_flashFiredNoStrobeLight;
    7: result := s_flashFiredStrobeLight;
  else
    result := '';
  end;
end;

// Return any text found before SeekStr
function GetTextBeforeChar(const Value, SeekStr: string; bNullIfNotFound: boolean = false): string;
var
  iPos: integer;
begin
  result := Value;
  iPos := pos(uppercase(SeekStr), uppercase(Value));
  if iPos > 0 then
    delete(Result, iPos, (length(Result) - iPos) + 1)
  else 
  if bNullIfNotFound then
    result := '';
end;
                  
// Return any text found after SeekStr
function GetTextAfterChar(const Value, SeekStr: string; bNullIfNotFound: boolean = false): string;
var
  ipos: integer;
begin
  result := Value;
  iPos := pos(uppercase(SeekStr), uppercase(Value));
  if ipos > 0 then
    Delete(Result, 1, iPos + (length(SeekStr) - 1))
  else
  if bNullIfNotFound then
    result := '';
end;


{$IFDEF Delphi2005orNewer}

function _ReadFromEXIF_Params(ADest: TObject; IOParams : TIOParamsVals) : Boolean;
var
  i: integer;
begin
  if ADest is TStrings then
    TStrings(ADest).Clear;
  result := IOParams.EXIF_HasEXIFData;
  if result = False then
    exit;

  for i := low(ExifTags) to high(ExifTags) do
  begin
    {$IFDEF USE_STRINGGRIDS}
    if ADest is TStringGrid then
      TStringGrid(ADest).Cells[1, i + TStringGrid(ADest).FixedRows] := IOParams.EXIF_AsStr[ i ]
    else
    {$endif}
    {$IFDEF USE_LISTVIEW}
    if ADest is TListView then
      TListView(ADest).Items.Item[i].SubItems[0] := IOParams.EXIF_AsStr[ i ]
    else
    {$endif}
    if ADest is TStrings then
      TStrings(ADest).Add( ExifTags[ i ].Desc + ': ' + IOParams.EXIF_AsStr[ i ] )
    else
      raise Exception.create('Invalid');
  end;
end;

function _ReadFromIPTC_Params(ADest: TObject; IOParams : TIOParamsVals) : Boolean;
var
  i: integer;
  sValue: string;
begin
  result := false;
  if ADest is TStrings then
    TStrings(ADest).Clear;

  // set properties to properties grid
  for i := low(IPTCTags) to high(IPTCTags) do
  begin
    sValue := IOParams.ReadIPTCField(IPTCTags[i].Rec, IPTCTags[i].DSet);

    {$IFDEF USE_STRINGGRIDS}
    if ADest is TStringGrid then
      TStringGrid(ADest).Cells[1, i + TStringGrid(ADest).FixedRows] := sValue
    else
    {$endif}
    {$IFDEF USE_LISTVIEW}
    if ADest is TListView then
      TListView(ADest).Items[i].SubItems[0] := sValue
    else
    {$endif}
    if ADest is TStrings then
      TStrings(ADest).Add( IPTCTags[i].Desc + ': ' + sValue )
    else
      raise Exception.create('Invalid');

    if sValue <> '' then
      result := true;
  end;
end;

function _ReadFromDicom_Params(ADest: TObject; IOParams : TIOParamsVals; TagInclude: TIEDicomInclude) : Boolean;
var
  iRows: Integer;

  procedure _ParseTags(tags: TIEDicomTags);
  var
    i, j: integer;
    tag: PIEDicomTag;
    sDescription: string;
    sValue: string;
    aListItem: TListItem;
    aTagSource: TIEDicomTagSource;
    bAllowTag: Boolean;
  begin
    for i := 0 to tags.Count - 1 do
    begin
      // GET DATA
      tag := tags.GetTag(i);
      sDescription := tags.GetTagDescription(i, aTagSource);

      bAllowTag := True;
      if (aTagSource = dsDeprecated) and ((diDeprecated in TagInclude) = False) then
        bAllowTag := False
      else
      if (aTagSource = dsProprietary) and ((diProprietary in TagInclude) = False) then
        bAllowTag := False;

      if (sDescription = '') then
      begin
        if (diUnknown in TagInclude) = False then
          bAllowTag := False
        else
          sDescription := s_Unknown;
      end;

      sValue := Trim( tags.GetTagString(i) );

      // ADD TO OUR CONTROL
      if bAllowTag and (sValue <> '') then
      begin             
        result := true;

        {$IFDEF USE_STRINGGRIDS}
        if ADest is TStringGrid then
        begin                
          inc(iRows);
          if TStringGrid(ADest).RowCount < TStringGrid(ADest).FixedRows + iRows then
            TStringGrid(ADest).RowCount := TStringGrid(ADest).FixedRows + iRows;

          TStringGrid(ADest).Cells[0, iRows + TStringGrid(ADest).FixedRows - 1] := sDescription;
          TStringGrid(ADest).Cells[1, iRows + TStringGrid(ADest).FixedRows - 1] := sValue;
        end
        else
        {$endif}
        {$IFDEF USE_LISTVIEW}
        if ADest is TListView then
        begin
          aListItem := TListView(ADest).Items.Add;
          aListItem.Caption := sDescription;
          aListItem.SubItems.Add(sValue);
        end
        else
        {$endif}
        if ADest is TStrings then
        begin
          TStrings(ADest).Add( sDescription + ': ' + sValue )
        end
        else
          raise Exception.create('Invalid');
      end;

      // PROCESS ANY CHILDREN
      if (diChildTags in TagInclude) and assigned(tag.Children) then
      begin
        // tag.Children is a TObjectList object, where each item is a TIEDicomTags object to pass recursively into ShowRawTags
        for j := 0 to tag.Children.Count - 1 do
          _ParseTags(tag.Children[j] as TIEDicomTags);
      end;
    end;
  end;

begin   
  result := False;
  if ADest is TStrings then
  begin
    TStrings(ADest).Clear;
    TStrings(ADest).BeginUpdate;
  end;
  try
    iRows := 0;
    _ParseTags(IOParams.DICOM_Tags);
  finally
    if ADest is TStrings then
      TStrings(ADest).EndUpdate;
  end;
end;

function _ReadFromMeta_File(ADest: TObject; AMetaType : TIEMetaType; const sFilename : string; DicomTagInclude: TIEDicomInclude = []) : Boolean;
var
  aImageEnIO: TImageEnIO;
begin
  Result := False;
  aImageEnIO := TImageEnIO.create(nil);
  try
    aImageEnIO.ParamsFromFile(sFilename);

    case AMetaType of
      iemEXIF  : Result := _ReadFromEXIF_Params(ADest, aImageEnIO.Params);
      iemIPTC  : Result := _ReadFromIPTC_Params(ADest, aImageEnIO.Params);
      iemDicom : Result := _ReadFromDicom_Params(ADest, aImageEnIO.Params, DicomTagInclude);
      else raise Exception.create('Invalid');
    end;
    
  finally
    aImageEnIO.Free;
  end;
end;

function _WriteToExif_Params(ADest: TObject; IOParams : TIOParamsVals) : Boolean;
var
  sNewValue :  string;
  i : integer;
  bExifHasData : boolean;
  bExifHasChanged: Boolean;
begin
  bExifHasData := false;
  bExifHasChanged := False;

  for i := low(ExifTags) to high(ExifTags) do
  begin
    {$IFDEF USE_STRINGGRIDS}
    if ADest is TStringGrid then
      sNewValue := TStringGrid(ADest).Cells[1, i + TStringGrid(ADest).FixedRows]
    else
    {$endif}
    {$IFDEF USE_LISTVIEW}
    if ADest is TListView then
      sNewValue := TListView(ADest).Items.Item[i].SubItems[0]
    else
    {$endif}
      raise Exception.create('Invalid');

    if sNewValue <> '' then
      bExifHasData := True;

    // Has the data for that field changed?
    if sNewValue <> IOParams.EXIF_AsStr[ i ] then
    begin
      bExifHasChanged := True;
      IOParams.EXIF_AsStr[ i ] := sNewValue;
    end;
  end;
  IOParams.EXIF_HasEXIFData := bExifHasData;

  Result := bExifHasChanged;
end;

function _WriteToIPTC_Params(ADest: TObject; IOParams : TIOParamsVals) : Boolean;
var
  i: integer;
  sOldValue, sNewValue: string;
begin
  Result := False;
  for i := low(IPTCTags) to high(IPTCTags) do
  begin
    {$IFDEF USE_STRINGGRIDS}
    if ADest is TStringGrid then
      sNewValue := TStringGrid(ADest).Cells[1, i + TStringGrid(ADest).FixedRows]
    else
    {$endif}
    {$IFDEF USE_LISTVIEW}
    if ADest is TListView then
      sNewValue := TListView(ADest).Items.Item[i].SubItems[0]
    else
    {$endif}
      raise Exception.create('Invalid');

    sOldValue := IOParams.ReadIPTCField(IPTCTags[i].Rec, IPTCTags[i].DSet);

    if sNewValue <> sOldValue then
    begin     
      Result := True;
      IOParams.WriteIPTCField(IPTCTags[i].Rec, IPTCTags[i].DSet, sNewValue);
    end;
  end;
end;


function _WriteToMeta_File(ADest: TObject; AMetaType : TIEMetaType; const sFilename : string) : Boolean;
var
  aImageEnIO: TImageEnIO;
  dFileDate : TDateTime;
  bHasChanged: Boolean;
begin
  Result := False;

  aImageEnIO := TImageEnIO.create(nil);
  try
    if FileExists(sFilename) = False then
      raise EIEException.create('Cannot find file to update: ' + sFilename);

    // Get existing meta data
    // NOTE: There is no InjectTiffIPTC
    if (AMetaType = iemIPTC) and
       (IEFilenameInExtensions(sFilename, '*.jpeg;*.jpg;*.jpe;') = False) then
      aImageEnIO.LoadFromFile(sFilename)
    else
      aImageEnIO.ParamsFromFile(sFilename);

    case AMetaType of
      iemEXIF : bHasChanged := _WriteToExif_Params(ADest, aImageEnIO.Params);
      iemIPTC : bHasChanged := _WriteToIPTC_Params(ADest, aImageEnIO.Params);
      else raise Exception.create('Invalid');
    end;

    if bHasChanged then
    begin
      Result := True;

      {$WARNINGS OFF} // FileAge is deprecated
      dFileDate := 0;
      if Maintain_File_Dates_On_Meta_Write then
        dFileDate := FileDateToDateTime(FileAge(sFileName));
      {$WARNINGS ON}

      if aImageEnIO.Params.FileType = ioJPEG then
        case AMetaType of
          iemEXIF : aImageEnIO.InjectJpegEXIF(sFileName);
          iemIPTC : aImageEnIO.InjectJpegIPTC(sFileName);
        end
      else
      if aImageEnIO.Params.FileType = ioTIFF then
        case AMetaType of
          iemEXIF : aImageEnIO.InjectTiffEXIF(sFileName);
          iemIPTC : aImageEnIO.SaveToFile(sFileName);
        end
      else
        raise EIEException.create('File format does not support this meta data type');

      if Maintain_File_Dates_On_Meta_Write and (dFileDate <> 0) then
        IEFileSetDate(sFileName, dFileDate);
    end;

  finally
    aImageEnIO.Free;
  end;
end;

{$endif}

{ TStringGridHelper }

{$IFDEF Delphi2005orNewer}
{$IFDEF USE_STRINGGRIDS}

{!!
<FS>TStringGridHelper.InitializeGrid

<FM>Declaration<FC>
procedure InitializeGrid(bFixLayout : Boolean = True);

<FM>Description<FN>
Set up a TStringGrid for display or editing of EXIF, IPTC and Dicom meta data. If <FC>bFixLayout<FN> is true then it will automatically add columns and a header row to your grid.

<FM>Example<FC>
// Display a grid allowing user to add EXIF data
procedure TExifForm.FormCreate(Sender: TObject);
begin
  // Fix grid layout
  MyExifStringGrid.InitializeGrid;

  // Add fields to grid
  MyExifStringGrid.NewGridForExif;
end;
!!}
procedure TStringGridHelper.InitializeGrid(bFixLayout : Boolean = True);
var
  c, r: integer;
begin
  if bFixLayout then
  begin
    FixedRows := 1;
    RowCount  := FixedRows + 1;
    FixedCols := 1;
    ColCount  := FixedCols + 1;
    ColWidths[ 0 ] := Width div 3;       // 1/3 the width
    ColWidths[ 1 ] := MulDiv(Width, 3, 2);   // 2/3 the width
  end;

  // Clear content
  for r := 0 to RowCount - 1 do
    for c := 0 to ColCount - 1 do
      Cells[c, r] := '';

  // header row
  if FixedRows > 0 then
  begin
    Cells[0, 0] := iemsg(IEMSG_NAME);
    Cells[1, 0] := iemsg(IEMSG_VALUE);
  end;
end;

procedure _NewGrid(AStringGrid: TStringGrid; AMetaType : TIEMetaType; bReadOnly : Boolean);
var
  i: integer;
  sDescription: string;
  iRowMax: Integer;
begin
  with AStringGrid do
  begin
    case AMetaType of
      iemEXIF  : iRowMax := High(ExifTags);
      iemIPTC  : iRowMax := High(IPTCTags);
      iemDicom : iRowMax := -1; // Filled later
      else raise Exception.create('Invalid');
    end;

    // add a row for each EXIF plus the header (if there is a fixed row)
    if iRowMax > 0 then
      RowCount := iRowMax + 1 + FixedRows
    else                             
      RowCount := FixedRows + 1;

    InitializeGrid(False);

    // fill the fields with the EXIF property names
    for i := 0 to RowCount - 2 do
    begin
      case AMetaType of
        iemEXIF  : sDescription := ExifTags[i].Desc;
        iemIPTC  : sDescription := IPTCTags[i].Desc;
        iemDicom : sDescription := ''; // Filled later
        else raise Exception.create('Invalid');
      end;
      Cells[0, i + FixedRows] := sDescription;
      Cells[1, i + FixedRows] := '';
    end;

    if bReadOnly then
      Options := Options - [goEditing]
    else
      Options := Options + [goEditing];
  end;
end;


{!!
<FS>TStringGridHelper.NewGridForExif

<FM>Declaration<FC>
procedure NewGridForExif(bReadOnly : Boolean = False);

<FM>Description<FN>
Set up a TStringGrid for specifying values for EXIF fields (e.g. to a file without EXIF fields presently).
Two columns will be added with the left one containing property descriptions (using <A TIOParamsValsHelper.EXIF_FieldDescription>) and the right blank for field entry. If the grid has a fixed row it will be given a heading of "Name" and "Value".
If <FC>bReadOnly<FN> is true the right-column will not be editable.

Note: You may wish to call <A TStringGridHelper.InitializeGrid> before <FC>NewGridForExif<FN> to automatically assign the optimal layout to the grid.

<FM>Example<FC>
// Display a grid allowing user to add EXIF data
procedure TExifForm.FormCreate(Sender: TObject);
begin
  // Fix grid layout
  MyExifStringGrid.InitializeGrid;

  // Add fields to grid
  MyExifStringGrid.NewGridForExif;
end;
!!}
procedure TStringGridHelper.NewGridForExif(bReadOnly : Boolean = False);
begin
  _NewGrid(Self, iemEXIF, bReadOnly);
end;

{!!
<FS>TStringGridHelper.NewGridForIPTC

<FM>Declaration<FC>
procedure NewGridForIPTC(bReadOnly : Boolean = False);

<FM>Description<FN>
Set up a TStringGrid for specifying values for IPTC fields (e.g. to a file without IPTC fields presently).
Two columns will be added with the left one containing property descriptions and the right blank for field entry. If the grid has a fixed row it will be given a heading of "Name" and "Value".
If <FC>bReadOnly<FN> is true the right-column will not be editable.

Note: You may wish to call <A TStringGridHelper.InitializeGrid> before <FC>NewGridForIPTC<FN> to automatically assign the optimal layout to the grid.

<FM>Example<FC>
// Display a grid allowing user to add IPTC data
procedure TIPTCForm.FormCreate(Sender: TObject);
begin
  // Fix grid layout
  MyIPTCStringGrid.InitializeGrid;

  // Add fields to grid
  MyIPTCtringGrid.NewGridForIPTC;
end;
!!}
// Formerly InitializeIPTCStringGrid
procedure TStringGridHelper.NewGridForIPTC(bReadOnly : Boolean = False);
begin
  _NewGrid(Self, iemIPTC, bReadOnly);
end;


{!!
<FS>TStringGridHelper.ReadGridFromExif

<FM>Declaration<FC>
function ReadGridFromExif(IOParams : <A TIOParamsVals>; bReadOnly : Boolean = False) : Boolean; overload;
function ReadGridFromExif(const sFilename : string; bReadOnly : Boolean = False) : Boolean; overload;

<FM>Description<FN>
Fill a TStringGrid with the EXIF properties of the current image, or one loaded from file.
Two columns will be added with the left one containing property descriptions (using <A TIOParamsValsHelper.EXIF_FieldDescription>) and the right one containing properties values (using <A TIOParamsValsHelper.EXIF_AsStr>). If the grid has a fixed row it will be given a heading of "Name" and "Value".
If <FC>bReadOnly<FN> is true the right-column will not be editable.
Result is true if the file has any EXIF fields.

Note: You may wish to call <A TStringGridHelper.InitializeGrid> before <FC>ReadGridFromExif<FN> to automatically assign the optimal layout to the grid.

<FM>Examples<FC>
// Display the EXIF properties of the current image
MyExifStringGrid.ReadGridFromExif( ImageEnView1.IO.Params );

// Read-only display of EXIF properties of a file
MyExifStringGrid.ReadGridFromExif( 'C:\MyImage.jpeg', True );
!!}
function TStringGridHelper.ReadGridFromExif(IOParams : TIOParamsVals; bReadOnly : Boolean = False) : Boolean;
begin
  NewGridForExif(bReadOnly);
  Result := _ReadFromEXIF_Params(Self, IOParams);
end;

function TStringGridHelper.ReadGridFromExif(const sFilename : string; bReadOnly : Boolean = False) : Boolean;
begin
  NewGridForExif(bReadOnly);
  Result := _ReadFromMeta_File(Self, iemEXIF, sFilename);
end;


{!!
<FS>TStringGridHelper.ReadGridFromIPTC

<FM>Declaration<FC>
function ReadGridFromIPTC(IOParams : <A TIOParamsVals>; bReadOnly : Boolean = False) : Boolean; overload;
function ReadGridFromIPTC(const sFilename : string; bReadOnly : Boolean = False) : Boolean; overload;

<FM>Description<FN>
Fill a TStringGrid with the IPTC properties of the current image, or one loaded from file.
Two columns will be added with the left one containing property descriptions and the right one containing properties values. If the grid has a fixed row it will be given a heading of "Name" and "Value".
If <FC>bReadOnly<FN> is true the right-column will not be editable.
Result is true if the file has any IPTC fields.

Note: You may wish to call <A TStringGridHelper.InitializeGrid> before <FC>ReadGridFromIPTC<FN> to automatically assign the optimal layout to the grid.

<FM>Examples<FC>
// Display the IPTC properties of the current image
MyIPTCStringGrid.ReadGridFromIPTC( ImageEnView1.IO.Params );

// Read-only display of IPTC properties of a file
MyIPTCStringGrid.ReadGridFromIPTC( 'C:\MyImage.jpeg', True );
!!}
// Formerly LoadIPTCFields
function TStringGridHelper.ReadGridFromIPTC(IOParams : TIOParamsVals; bReadOnly : Boolean = False) : Boolean;
begin
  NewGridForIPTC(bReadOnly);
  Result := _ReadFromIPTC_Params(Self, IOParams);
end;

function TStringGridHelper.ReadGridFromIPTC(const sFilename : string; bReadOnly : Boolean = False) : Boolean;
begin
  NewGridForIPTC(bReadOnly);
  Result := _ReadFromMeta_File(Self, iemIPTC, sFilename);
end;

{!!
<FS>TStringGridHelper.ReadGridFromDicom

<FM>Declaration<FC>
function ReadGridFromDicom(IOParams : <A TIOParamsVals>; TagInclude: <A TIEDicomInclude>) : Boolean; overload;
function ReadGridFromDicom(const sFilename : string; TagInclude: <A TIEDicomInclude>) : Boolean; overload;

<FM>Description<FN>
Fill a TStringGrid with the Dicom properties of the current image, or one loaded from file.
Two columns will be added with the left one containing property descriptions (using <A TIEDicomTags.GetTagDescription>) and the right one containing Dicom values (using <A TIEDicomTags.GetTagString>). If the grid has a fixed row it will be given a heading of "Name" and "Value".
<FC>TagInclude<FN> allows you to filter the types of tags returned.
Result is true if the file has any Dicom fields.

Note: You may wish to call <A TStringGridHelper.InitializeGrid> before <FC>ReadGridFromDicom<FN> to automatically assign the optimal layout to the grid.

<FM>Examples<FC>
// Display the Dicom properties of the current image (all types)
MyDicomGridView.ReadGridFromDicom( ImageEnView1.IO.Params, [ diProprietary, diDeprecated, diChildTags, diUnknown ] );

// Display of Dicom properties of a file (excluding deprecated and unknown tags)
MyDicomGridView.ReadGridFromDicom( 'C:\MyImage.jpeg', [ diProprietary, diChildTags ] );
!!}
function TStringGridHelper.ReadGridFromDicom(IOParams : TIOParamsVals; TagInclude: TIEDicomInclude) : Boolean;
begin
  _NewGrid(Self, iemDicom, True);
  Result := _ReadFromDicom_Params(Self, IOParams, TagInclude);
end;

function TStringGridHelper.ReadGridFromDicom(const sFilename : string; TagInclude: TIEDicomInclude) : Boolean;
begin
  _NewGrid(Self, iemDicom, True);
  Result := _ReadFromMeta_File(Self, iemDicom, sFilename, TagInclude);
end;

{!!
<FS>TStringGridHelper.WriteGridToExif

<FM>Declaration<FC>
function WriteGridToExif(IOParams : <A TIOParamsVals>) : Boolean; overload;
function WriteGridToExif(const sFilename : string) : Boolean; overload;

<FM>Description<FN>
Write the changes made by a user to EXIF values in a TStringGrid (which was filled using <A TStringGridHelper.NewGridForExif> or <A TStringGridHelper.ReadGridFromExif>) to the current image or an image file.
Result is true if any changes were made to the EXIF properties (i.e. a result of false means the TStringGrid EXIF content matches the file).

Notes:
- For first overload, only the <A TIOParamsVals> is updated, the changes are not saved to file.
- For the second overload, the file must already exist! If no changes were made to the EXIF data then the file is not modified

<FM>Examples<FC>
// Write changes to EXIF properties of the current image, and ask if the user wants to save changes if any fields were modified
if MyExifStringGrid.WriteGridToExif( ImageEnView1.IO.Params ) then
  PromptToSaveFile;

// write EXIF changes to a file
MyExifStringGrid.WriteGridToExif( 'D:\MyImage.jpeg' );
!!}
function TStringGridHelper.WriteGridToExif(IOParams : TIOParamsVals) : Boolean;
begin
  Result := _WriteToExif_Params(Self, IOParams);
end;

function TStringGridHelper.WriteGridToExif(const sFilename : string) : Boolean;
begin
  Result := _WriteToMeta_File(Self, iemEXIF, sFilename);
end;


{!!
<FS>TStringGridHelper.WriteGridToIPTC

<FM>Declaration<FC>
function WriteGridToIPTC(IOParams : <A TIOParamsVals>) : Boolean; overload;
function WriteGridToIPTC(const sFilename : string) : Boolean; overload;

<FM>Description<FN>
Write the changes made by a user to IPTC values in a TStringGrid (which was filled using <A TStringGridHelper.NewGridForIPTC> or <A TStringGridHelper.ReadGridFromIPTC>) to the current image or an image file.
Result is true if any changes were made to the IPTC properties (i.e. a result of false means the TStringGrid IPTC content matches the file).

Notes:
- For first overload, only the <A TIOParamsVals> is updated, the changes are not saved to file.
- For the second overload, the file must already exist! If no changes were made to the IPTC data then the file is not modified

<FM>Examples<FC>
// Write changes to IPTC properties of the current image, and ask if the user wants to save changes if any fields were modified
if MyIPTCStringGrid.WriteGridToIPTC( ImageEnView1.IO.Params ) then
  PromptToSaveFile;

// write IPTC changes to a file
MyIPTCStringGrid.WriteGridToIPTC( 'D:\MyImage.jpeg' );
!!}
// Formerly SaveIPTCFields
function TStringGridHelper.WriteGridToIPTC(IOParams : TIOParamsVals) : Boolean;
begin
  Result := _WriteToIPTC_Params(Self, IOParams);
end;

function TStringGridHelper.WriteGridToIPTC(const sFilename : string) : Boolean;
begin
  Result := _WriteToMeta_File(Self, iemIPTC, sFilename);
end;


{!!
<FS>TStringGridHelper.ClearGridFields

<FM>Declaration<FC>
procedure ClearGridFields;

<FM>Description<FN>
Clears all values in a TStringGrid containing EXIF or IPTC properties, which was filled using one of the following:
- <A TStringGridHelper.NewGridForExif>
- <A TStringGridHelper.NewGridForIPTC>
- <A TStringGridHelper.ReadGridFromExif>
- <A TStringGridHelper.ReadGridFromIPTC>

Note: Only clears the grid, not EXIF or IPTC properties of a file

<FM>Examples<FC>
procedure TMainForm.btnResetGridClick(Sender: TObject);
begin
  // Clear all the specified values for our grid
  MyExifStringGrid.ClearGridFields;
end;
!!}
procedure TStringGridHelper.ClearGridFields;
var
  i: integer;
begin
  for i := FixedRows to RowCount - 1 do
    Cells[1, i] := '';
end;

{$endif}
{$endif}


{ TListViewHelper }

{$IFDEF Delphi2005orNewer}
{$IFDEF USE_LISTVIEW}


{!!
<FS>TListViewHelper.InitializeList

<FM>Declaration<FC>
procedure InitializeList(bFixLayout : Boolean = True);

<FM>Description<FN>
Set up a TListView for display or editing of EXIF, IPTC and Dicom meta data. If <FC>bFixLayout<FN> is true then it will automatically adjust the properties for best display.

<FM>Example<FC>
// Display a TListView allowing user to add EXIF data
procedure TEXIFForm.FormCreate(Sender: TObject);
begin
  // Fix List layout
  MyExifListView.InitializeList;

  // Add fields to List
  MyExifListView.NewListForExif;
end;
!!}
procedure TListViewHelper.InitializeList(bFixLayout : Boolean = True);
const
  Buffer_px = 4;
var
  aListColumn: TListColumn;
begin
  Clear;

  if bFixLayout then
  begin
    ViewStyle := vsReport;
    Columns.Clear;
  end;
         
  // Clear content
  Clear;

  // Setup the columns
  if Columns.count < 2 then
  begin
    aListColumn := Columns.Add;
    aListColumn.Caption := iemsg(IEMSG_NAME);
    aListColumn.Width := Width div 3;      // 1/3 width
    aListColumn := Columns.Add;
    aListColumn.Caption := iemsg(IEMSG_VALUE);
    aListColumn.Width := MulDiv(Width, 3, 2) - GetSystemMetrics(SM_CYVSCROLL) - Buffer_px;  // 2/3 width less Scrollbar width
  end;
end;


procedure _NewList(aListView: TListView; AMetaType : TIEMetaType; bReadOnly : Boolean);
var
  i: integer;
  aListItem: TListItem;
  iRowMax: Integer;
  sDescription: string;
begin
  with aListView do
  begin
    InitializeList( False );

    case AMetaType of
      iemEXIF  : iRowMax := High(ExifTags);
      iemIPTC  : iRowMax := High(IPTCTags);
      iemDicom : iRowMax := -1; // Handle at time of fill
      else raise Exception.create('Invalid');
    end;

     // fill the fields with the property names
    for i := 0 to iRowMax do
    begin
      case AMetaType of
        iemEXIF : sDescription := ExifTags[i].Desc;
        iemIPTC : sDescription := IPTCTags[i].Desc;
        else raise Exception.create('Invalid');
      end;
      aListItem := Items.Add;
      aListItem.Caption := sDescription;
      aListItem.SubItems.Add('')
    end;

    ReadOnly := bReadOnly;
  end;
end;

{!!
<FS>TListViewHelper.NewListForExif

<FM>Declaration<FC>
procedure NewListForExif;

<FM>Description<FN>
Set up a TListView for specifying values for EXIF fields (e.g. to a file without EXIF fields presently).
Two columns will be added with the left one containing property descriptions (using <A TIOParamsValsHelper.EXIF_FieldDescription>) and the right blank for field entry. The headings will read "Name" and "Value".

Notes:
- A standard TListView does not permit editing of subitems, so either download a TListView descendent that allows editing, or search for code snippets for "Edit Subitems TListView"
- You may wish to call <A TListViewHelper.InitializeList> before <FC>NewListForExif<FN> to automatically assign the optimal layout to the TListView

<FM>Example<FC>
// Display a TListView allowing user to add EXIF data
procedure TExifForm.FormCreate(Sender: TObject);
begin
  // Fix List layout
  MyExifListView.InitializeList;

  // Add fields to List
  MyExifListView.NewListForExif;
end;
!!}
procedure TListViewHelper.NewListForExif();
begin
  _NewList(Self, iemEXIF, True);
end;

{!!
<FS>TListViewHelper.NewListForIPTC

<FM>Declaration<FC>
procedure NewListForIPTC;

<FM>Description<FN>
Set up a TListView for specifying values for IPTC fields (e.g. to a file without IPTC fields presently).
Two columns will be added with the left one containing property descriptions and the right blank for field entry. The headings will read "Name" and "Value".

Notes:
- A standard TListView does not permit editing of subitems, so either download a TListView descendent that allows editing, or search for code snippets for "Edit Subitems TListView"
- Call <A TListViewHelper.InitializeList> before <FC>NewListForIPTC<FN> to automatically assign the optimal layout to the TListView

<FM>Example<FC>
// Display a TListView allowing user to add IPTC data
procedure TIPTCForm.FormCreate(Sender: TObject);
begin
  // Fix List layout
  MyIPTCListView.InitializeList;

  // Add fields to List
  MyIPTCListView.NewListForIPTC;
end;
!!}
// Formerly InitializeIPTCListView
procedure TListViewHelper.NewListForIPTC();
begin
  _NewList(Self, iemIPTC, True);
end;

{!!
<FS>TListViewHelper.ReadListFromExif

<FM>Declaration<FC>
function ReadListFromExif(IOParams : <A TIOParamsVals>) : Boolean; overload;
function ReadListFromExif(const sFilename : string) : Boolean; overload;

<FM>Description<FN>
Fill a TListView with the EXIF properties of the current image, or one loaded from file.
Result is true if the file has any EXIF fields.

Note: You may wish to call <A TListViewHelper.InitializeList> before <FC>ReadListFromExif<FN> to automatically assign the optimal layout to the TListView

<FM>Examples<FC>
// Display the EXIF properties of the current image
MyExifListView.ReadListFromExif( ImageEnView1.IO.Params );

// Read-only display of EXIF properties of a file
MyExifListView.ReadListFromExif( 'C:\MyImage.jpeg', True );
!!}
function TListViewHelper.ReadListFromExif(IOParams : TIOParamsVals) : Boolean;
begin
  NewListForExif();
  Result := _ReadFromEXIF_Params(Self, IOParams);
end;

function TListViewHelper.ReadListFromExif(const sFilename : string) : Boolean;
begin
  NewListForExif();
  Result := _ReadFromMeta_File(Self, iemEXIF, sFilename);
end;


{!!
<FS>TListViewHelper.ReadListFromIPTC

<FM>Declaration<FC>
function ReadListFromIPTC(IOParams : <A TIOParamsVals>) : Boolean; overload;
function ReadListFromIPTC(const sFilename : string) : Boolean; overload;

<FM>Description<FN>
Fill a TListView with the IPTC properties of the current image, or one loaded from file.
Result is true if the file has any IPTC fields specified.

Note: You may wish to call <A TListViewHelper.InitializeList> before <FC>ReadListFromIPTC<FN> to automatically assign the optimal layout to the TListView

<FM>Examples<FC>
// Display the IPTC properties of the current image
MyIPTCListView.ReadListFromIPTC( ImageEnView1.IO.Params );

// Read-only display of IPTC properties of a file
MyIPTCListView.ReadListFromIPTC( 'C:\MyImage.jpeg', True );
!!}
// Formerly LoadIPTCFields
function TListViewHelper.ReadListFromIPTC(IOParams : TIOParamsVals) : Boolean;
begin
  NewListForIPTC();
  Result := _ReadFromIPTC_Params(Self, IOParams);
end;

function TListViewHelper.ReadListFromIPTC(const sFilename : string) : Boolean;
begin
  NewListForIPTC();
  Result := _ReadFromMeta_File(Self, iemIPTC, sFilename);
end;


{!!
<FS>TListViewHelper.ReadListFromDicom

<FM>Declaration<FC>
function ReadListFromDicom(IOParams : <A TIOParamsVals>; TagInclude: <A TIEDicomInclude>) : Boolean; overload;
function ReadListFromDicom(const sFilename : string; TagInclude: <A TIEDicomInclude>) : Boolean; overload;

<FM>Description<FN>
Fill a TListView with the Dicom properties of the current image, or one loaded from file. <FC>TagInclude<FN> allows you to filter the types of tags returned.
Result is true if the file has any Dicom fields specified.

Note: You may wish to call <A TListViewHelper.InitializeList> before <FC>ReadListFromDicom<FN> to automatically assign the optimal layout to the TListView

<FM>Examples<FC>
// Display the Dicom properties of the current image (all types)
MyDicomListView.ReadListFromDicom( ImageEnView1.IO.Params, [ diProprietary, diDeprecated, diChildTags, diUnknown ] );

// Display of Dicom properties of a file (excluding deprecated and unknown tags)
MyDicomListView.ReadListFromDicom( 'C:\MyImage.jpeg', [ diProprietary, diChildTags ] );
!!}
function TListViewHelper.ReadListFromDicom(IOParams : TIOParamsVals; TagInclude: TIEDicomInclude) : Boolean;
begin
  _NewList(Self, iemDicom, True);
  Result := _ReadFromDicom_Params(Self, IOParams, TagInclude);
end;

function TListViewHelper.ReadListFromDicom(const sFilename : string; TagInclude: TIEDicomInclude) : Boolean;
begin
  _NewList(Self, iemDicom, True);
  Result := _ReadFromMeta_File(Self, iemDicom, sFilename, TagInclude);
end;


{!!
<FS>TListViewHelper.WriteListToExif

<FM>Declaration<FC>
function WriteListToExif(IOParams : <A TIOParamsVals>) : Boolean; overload;
function WriteListToExif(const sFilename : string) : Boolean; overload;

<FM>Description<FN>
Write the changes made by a user to EXIF values in a TListView (which was initialized using <A TListViewHelper.NewListForExif> or <A TListViewHelper.ReadListFromExif>) to the current image or an image file.
Result is true if any changes were made to the EXIF properties (i.e. a result of false means the TListView EXIF content matches the file).

NOTE: A standard TListView does not permit editing of subitems, so either download a TListView descendent that allows editing, or search for code snippets for "Edit Subitems TListView"

Other Notes:
- For first overload, only the <A TIOParamsVals> is updated, the changes are not saved to file.
- For the second overload, the file must already exist! If no changes were made to the EXIF data then the file is not modified

<FM>Examples<FC>
// Write changes to EXIF properties of the current image, and ask if the user wants to save changes if any fields were modified
if MyExifListView.WriteListToExif( ImageEnView1.IO.Params ) then
  PromptToSaveFile;

// write EXIF changes to a file
MyExifListView.WriteListToExif( 'D:\MyImage.jpeg' );
!!}
function TListViewHelper.WriteListToExif(IOParams : TIOParamsVals) : Boolean;
begin
  Result := _WriteToEXIF_Params(Self, IOParams);
end;

function TListViewHelper.WriteListToExif(const sFilename : string) : Boolean;
begin
  Result := _WriteToMeta_File(Self, iemEXIF, sFilename);
end;


{!!
<FS>TListViewHelper.WriteListToIPTC

<FM>Declaration<FC>
function WriteListToIPTC(IOParams : <A TIOParamsVals>) : Boolean; overload;
function WriteListToIPTC(const sFilename : string) : Boolean; overload;

<FM>Description<FN>
Write the changes made by a user to IPTC values in a TListView (which was initialized using <A TListViewHelper.NewListForIPTC> or <A TListViewHelper.ReadListFromIPTC>) to the current image or an image file.
Result is true if any changes were made to the IPTC properties (i.e. a result of false means the TListView IPTC content matches the file).

NOTE: A standard TListView does not permit editing of subitems, so either download a TListView descendent that allows editing, or search for code snippets for "Edit Subitems TListView"

Other Notes:
- For first overload, only the <A TIOParamsVals> is updated, the changes are not saved to file.
- For the second overload, the file must already exist! If no changes were made to the IPTC data then the file is not modified

<FM>Examples<FC>
// Write changes to IPTC properties of the current image, and ask if the user wants to save changes if any fields were modified
if MyIPTCListView.WriteListToIPTC( ImageEnView1.IO.Params ) then
  PromptToSaveFile;

// write IPTC changes to a file
MyIPTCListView.WriteListToIPTC( 'D:\MyImage.jpeg' );
!!}
// Formerly SaveIPTCFields
function TListViewHelper.WriteListToIPTC(IOParams : TIOParamsVals) : Boolean;
begin
  Result := _WriteToIPTC_Params(Self, IOParams);
end;

function TListViewHelper.WriteListToIPTC(const sFilename : string) : Boolean;
begin
  Result := _WriteToMeta_File(Self, iemIPTC, sFilename); 
end;


{!!
<FS>TListViewHelper.ClearListFields

<FM>Declaration<FC>
procedure ClearListFields;

<FM>Description<FN>
Clears all values in a TListView containing EXIF or IPTC properties, which was initialized using:
- <A TListViewHelper.NewListForExif>
- <A TListViewHelper.NewListForIPTC>
- <A TListViewHelper.ReadListFromExif>
- <A TListViewHelper.ReadListFromIPTC>

Note: Only clears the ListView, not EXIF/IPTC properties of a file

<FM>Examples<FC>
procedure TMainForm.btnResetListClick(Sender: TObject);
begin
  // Clear all the specified values for our ListView
  MyExifListView.ClearEXIFFields;
end;
!!}
procedure TListViewHelper.ClearListFields;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items.Item[i].SubItems[0] := '';
end;

{$endif}
{$endif}



{ TIOParamsValsHelper }

{$IFDEF Delphi2005orNewer}
                                   
{!!
<FS>TIOParamsValsHelper.EXIF_Camera_Str

<FM>Declaration<FC>
property EXIF_Camera_Str : string; (read/write)

<FM>Description<FN>
Returns a concatenation of <A TIOParamsVals.EXIF_Make> and <A TIOParamsVals.EXIF_Model>.
!!}
function TIOParamsValsHelper.GetEXIF_Camera_Str : string;
var
  sMake, sModel: string;
begin
  // EXIF_Make and EXIF_Model combined
  sMake  := RemoveNull(EXIF_Make);
  sModel := RemoveNull(EXIF_Model);

  // sometimes EXIF_Model already includes the make
  if (sMake <> '') and (pos(sMake, sModel) > 0) then
    sMake := '';

  Result := Trim(sMake + ' ' + sModel);
end;
   
{!!
<FS>TIOParamsValsHelper.EXIF_XResolution_Str

<FM>Declaration<FC>
property EXIF_XResolution_Str : string; (read/write)

<FM>Description<FN>
A string formatted version of <A TIOParamsVals.EXIF_XResolution>. E.g. if EXIF_XResolution is 72 then EXIF_XResolution_Str returns '1/72'
!!}
function TIOParamsValsHelper.GetEXIF_XResolution_Str : string;
begin
  // Display/Print resolution of image. Default value is 1/72inch, but it has no mean because personal computer doesn't use this value to display/print out
  result := DoubleToFraction(EXIF_Xresolution);
end;

{!!
<FS>TIOParamsValsHelper.EXIF_YResolution_Str

<FM>Declaration<FC>
property EXIF_YResolution_Str : string; (read/write)

<FM>Description<FN>
A string formatted version of <A TIOParamsVals.EXIF_YResolution>. E.g. if EXIF_YResolution is 72 then EXIF_YResolution_Str returns '1/72'
!!}
function TIOParamsValsHelper.GetEXIF_YResolution_Str : string;
begin
  // Display/Print resolution of image. Default value is 1/72inch, but it has no mean because personal computer doesn't use this value to display/print out
  result := DoubleToFraction(EXIF_Yresolution);
end;

{!!
<FS>TIOParamsValsHelper.EXIF_ExposureTime_Str

<FM>Declaration<FC>
property EXIF_ExposureTime_Str : string; (read/write)

<FM>Description<FN>
A string formatted version of <A TIOParamsVals.EXIF_ExposureTime>. E.g. if EXIF_ExposureTime is 4 then EXIF_ExposureTime_Str returns '4' (seconds). If EXIF_ExposureTime is 0.25 then it returns '1/4' (of a second).
!!}
function TIOParamsValsHelper.GetEXIF_ExposureTime_Str : string;
var
  iExposureTime: double;
begin
  // Exposure time (reciprocal of shutter speed). Unit is seconds
  iExposureTime := EXIF_ExposureTime;

  if iExposureTime > 1 then
    // then it will be something like 4 i.e. 4 seconds
    result := inttostr(round(iExposureTime))
  else
  if (iExposureTime > 0) then
    // then it is an integer second value, e.g. 0.25, which translates to 1/4 second
    result := DoubleToFraction(round(1 / iExposureTime));
end;

{!!
<FS>TIOParamsValsHelper.EXIF_FNumber_Str

<FM>Declaration<FC>
property EXIF_FNumber_Str : string; (read/write)

<FM>Description<FN>
A string formatted version of <A TIOParamsVals.EXIF_FNumber>. <FC>EXIF_FNumber_Str<FN> simply prefixes an 'F' for valid values, e.g. if <FC>EXIF_FNumber<FN> is 5.6, this property would return 'F5.6'
!!}
function TIOParamsValsHelper.GetEXIF_FNumber_Str : string;
begin
  // The actual F-number(F-stop) of lens when the image was taken
  result := '';
  if (EXIF_Fnumber <> 0) and (EXIF_Fnumber <> -1) then
    result := 'F' + FloatToStrOrNull(EXIF_FNumber, '');
end;

{!!
<FS>TIOParamsValsHelper.EXIF_ShutterSpeedValue_Str

<FM>Declaration<FC>
property EXIF_ShutterSpeedValue_Str : string; (read/write)

<FM>Description<FN>
A string formatted version of <A TIOParamsVals.EXIF_ShutterSpeedValue>. Returns the value as a formatted fraction or number, e.g. '1/4' of a second or '2' (seconds).
!!}
function TIOParamsValsHelper.GetEXIF_ShutterSpeedValue_Str : string;
begin
  // Shutter speed by APEX value. To convert this value to ordinary 'Shutter Speed'; calculate this value's power of 2, then reciprocal. For example, if the ShutterSpeedValue is '4', shutter speed is 1/(24)=1/16 second
  result := ApexToStr(2, EXIF_ShutterSpeedValue, '1/');
end;

{!!
<FS>TIOParamsValsHelper.EXIF_ApertureValue_Str

<FM>Declaration<FC>
property EXIF_ApertureValue_Str : string; (read/write)

<FM>Description<FN>
A string formatted version of <A TIOParamsVals.EXIF_ApertureValue>. <FC>EXIF_ApertureValue<FN> is an Apex value which can be difficult to calculate. This property returns a human readible format, e.g. if <FC>EXIF_ApertureValue<FN> is 5, then 'F5.6' will be returned.
!!}
function TIOParamsValsHelper.GetEXIF_ApertureValue_Str : string;
begin
  // The actual aperture value of lens when the image was taken. Unit is APEX. To convert this value to
  // ordinary F-number (F-stop), calculate this value's power of root 2 (=1.4142). For example, if the
  // ApertureValue is '5', F-number is 1.41425 = F5.6.
  result := ApexToStr(Sqrt(2), EXIF_ApertureValue, 'F');
  if Result = 'F0' then
    result := '';
end;

{!!
<FS>TIOParamsValsHelper.EXIF_MaxApertureValue_Str

<FM>Declaration<FC>
property EXIF_MaxApertureValue_Str : string; (read/write)

<FM>Description<FN>
A string formatted version of <A TIOParamsVals.EXIF_MaxApertureValue>. <FC>EXIF_MaxApertureValue<FN> is an Apex value which can be difficult to calculate. This property returns a human readible format, e.g. if <FC>EXIF_MaxApertureValue<FN> is 5, then 'F5.6' will be returned.
!!}
function TIOParamsValsHelper.GetEXIF_MaxApertureValue_Str : string;
begin
  // Maximum aperture value of lens. You can convert to F-number by calculating power of root 2 (same process of ApertureValue)
  result := ApexToStr(Sqrt(2), EXIF_MaxApertureValue, 'F');
  if Result = 'F0' then
    result := '';
end;

{!!
<FS>TIOParamsValsHelper.EXIF_AsStr

<FM>Declaration<FC>
property EXIF_AsStr [index : Integer]: string; (read/write)

<FM>Description<FN>
Provides an alternative way to access EXIF properties in batch situations. Each property is available as a human-readible string value, and in most cases write access is also supported.

Index is a value between 0 and <FC>_EXIF_Tag_Count<FN> - 1 and can be referred to by <L EXIF Consts for EXIF_AsStr>constants</L>.

<TABLE>
<R> <H>Const</H> <H>Description</H> <H>DataSet</H> <H>Writeable?</H> <H>Output</H> </R>
<R> <C><FC>_EXIF_UserComment<FN></C> <C>User Comment</C> <C>YES</C> <C><A TIOParamsVals.EXIF_UserComment></C> </R>
<R> <C><FC>_EXIF_ImageDescription<FN></C> <C>Description</C> <C>YES</C> <C><A TIOParamsVals.EXIF_ImageDescription></C> </R>
<R> <C><FC>_EXIF_Make<FN></C> <C>Camera Make</C> <C>YES</C> <C><A TIOParamsVals.EXIF_Make></C> </R>
<R> <C><FC>_EXIF_Model<FN></C> <C>Camera Model</C> <C>YES</C> <C><A TIOParamsVals.EXIF_Model></C> </R>
<R> <C><FC>_EXIF_XResolution<FN></C> <C>Horizontal Resolution</C> <C>YES</C> <C><A TIOParamsValsHelper.EXIF_YResolution_Str> with <L TIOParamsVals.EXIF_ResolutionUnit>unit</L></C> </R>
<R> <C><FC>_EXIF_YResolution<FN></C> <C>Vertical Resolution</C> <C>YES</C> <C><A TIOParamsValsHelper.EXIF_XResolution_Str> with <L TIOParamsVals.EXIF_ResolutionUnit>unit</L></C> </R>
<R> <C><FC>_EXIF_DateTime<FN></C> <C>Date and Time</C> <C>YES</C> <C>DateTimeToStr(<A TIOParamsVals.EXIF_DateTime2>)</C> </R>
<R> <C><FC>_EXIF_DateTimeOriginal<FN></C> <C>Original Date and Time</C> <C>YES</C> <C>DateTimeToStr(<A TIOParamsVals.EXIF_DateTimeOriginal2>)</C> </R>
<R> <C><FC>_EXIF_DateTimeDigitized<FN></C> <C>Digitized Date and Time</C> <C>YES</C> <C>DateTimeToStr(<A TIOParamsVals.EXIF_DateTimeDigitized2>)</C> </R>
<R> <C><FC>_EXIF_Copyright<FN></C> <C>Copyright</C> <C>YES</C> <C><A TIOParamsVals.EXIF_Copyright></C> </R>
<R> <C><FC>_EXIF_Orientation<FN></C> <C>Orientation</C> <C>YES</C> <C>Text description of <A TIOParamsVals.EXIF_Orientation></C> </R>
<R> <C><FC>_EXIF_ExposureTime<FN></C> <C>Exposure Time</C> <C>YES</C> <C><A TIOParamsValsHelper.EXIF_ExposureTime_Str> seconds</C> </R>
<R> <C><FC>_EXIF_FNumber<FN></C> <C>F-Stop</C> <C>YES</C> <C><A TIOParamsValsHelper.EXIF_Fnumber_Str></C> </R>
<R> <C><FC>_EXIF_ExposureProgram<FN></C> <C>Exposure Program</C> <C>YES</C> <C>Text description of <A TIOParamsVals.EXIF_ExposureProgram></C> </R>
<R> <C><FC>_EXIF_ISOSpeedRatings<FN></C> <C>ISO Speed Rating</C> <C>YES</C> <C><A TIOParamsVals.EXIF_ISOSpeedRatings></C> </R>
<R> <C><FC>_EXIF_ShutterSpeedValue<FN></C> <C>Shutter Speed</C> <C>YES</C> <C><A TIOParamsValsHelper.EXIF_ShutterSpeedValue_Str></C> </R>
<R> <C><FC>_EXIF_ApertureValue<FN></C> <C>Aperture Value</C> <C>YES</C> <C><A TIOParamsValsHelper.EXIF_ApertureValue_Str></C> </R>
<R> <C><FC>_EXIF_BrightnessValue<FN></C> <C>Brightness</C> <C>YES</C> <C>FloatToStr(<A TIOParamsVals.EXIF_BrightnessValue>)</C> </R>
<R> <C><FC>_EXIF_ExposureBiasValue<FN></C> <C>Exposure Compensation</C> <C>YES</C> <C>FloatToStr(<A TIOParamsVals.EXIF_ExposureBiasValue>)</C> </R>
<R> <C><FC>_EXIF_MaxApertureValue<FN></C> <C>Max Aperture Value</C> <C>YES</C> <C><A TIOParamsValsHelper.EXIF_MaxApertureValue_Str></C> </R>
<R> <C><FC>_EXIF_SubjectDistance<FN></C> <C>Subject Distance</C> <C>YES</C> <C>FloatToStr(<A TIOParamsVals.EXIF_SubjectDistance>) m</C> </R>
<R> <C><FC>_EXIF_MeteringMode<FN></C> <C>Metering Mode</C> <C>YES</C> <C>Text description of <A TIOParamsVals.EXIF_MeteringMode></C> </R>
<R> <C><FC>_EXIF_LightSource<FN></C> <C>Lighting</C> <C>YES</C> <C>Text description of <A TIOParamsVals.EXIF_LightSource></C> </R>
<R> <C><FC>_EXIF_Flash<FN></C> <C>Flash</C> <C>YES</C> <C>Text description of <A TIOParamsVals.EXIF_Flash></C> </R>
<R> <C><FC>_EXIF_FocalLength<FN></C> <C>Focal Length</C> <C>YES</C> <C>FloatToStr(<A TIOParamsVals.EXIF_FocalLength>) mm</C> </R>
<R> <C><FC>_EXIF_FlashPixVersion<FN></C> <C>FlashPix Version</C> <C>YES</C> <C><A TIOParamsVals.EXIF_FlashPixVersion></C> </R>
<R> <C><FC>_EXIF_ColorSpace<FN></C> <C>Color Space</C> <C>YES</C> <C>Text description of <A TIOParamsVals.EXIF_ColorSpace></C> </R>
<R> <C><FC>_EXIF_ExifImageWidth<FN></C> <C>Image Width</C> <C>YES</C> <C><A TIOParamsVals.EXIF_EXIFImageWidth></C> </R>
<R> <C><FC>_EXIF_ExifImageHeight<FN></C> <C>Image Height</C> <C>YES</C> <C><A TIOParamsVals.EXIF_EXIFImageHeight></C> </R>
<R> <C><FC>_EXIF_RelatedSoundFile<FN></C> <C>Sound File</C> <C>YES</C> <C><A TIOParamsVals.EXIF_RelatedSoundFile></C> </R>
<R> <C><FC>_EXIF_FocalPlaneXResolution<FN></C> <C>Focal Plane Horz. Resolution</C> <C>YES</C> <C>FloatToStr(<A TIOParamsVals.EXIF_FocalPlaneXResolution>) with <L TIOParamsVals.EXIF_FocalPlaneResolutionUnit>unit</L></C> </R>
<R> <C><FC>_EXIF_FocalPlaneYResolution<FN></C> <C>Focal Plane Vert. Resolution</C> <C>YES</C> <C>FloatToStr(<A TIOParamsVals.EXIF_FocalPlaneYResolution>) with <L TIOParamsVals.EXIF_FocalPlaneResolutionUnit>unit</L></C> </R>
<R> <C><FC>_EXIF_ExposureIndex<FN></C> <C>Exposure Index</C> <C>YES</C> <C>FloatToStr(<A TIOParamsVals.EXIF_ExposureIndex>)</C> </R>
<R> <C><FC>_EXIF_SensingMethod<FN></C> <C>Sensing Method</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_SensingMethod></C> </R>
<R> <C><FC>_EXIF_FileSource<FN></C> <C>File Source</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_FileSource></C> </R>
<R> <C><FC>_EXIF_SceneType<FN></C> <C>Scene Type</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_SceneType></C> </R>
<R> <C><FC>_EXIF_YCbCrPositioning<FN></C> <C>Data Point</C> <C>YES</C> <C>Text description of <A TIOParamsVals.EXIF_YCbCrPositioning></C> </R>
<R> <C><FC>_EXIF_ExposureMode<FN></C> <C>Exposure Mode</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_ExposureMode></C> </R>
<R> <C><FC>_EXIF_WhiteBalance<FN></C> <C>White Balance</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_WhiteBalance></C> </R>
<R> <C><FC>_EXIF_DigitalZoomRatio<FN></C> <C>Digital Zoom Ratio</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_DigitalZoomRatio></C> </R>
<R> <C><FC>_EXIF_FocalLengthIn35mmFilm<FN></C> <C>Focal Length in 35mm Film</C> <C>no</C> <C>IntToStr(<A TIOParamsVals.EXIF_FocalLengthIn35mmFilm>) mm</C> </R>
<R> <C><FC>_EXIF_SceneCaptureType  <FN></C> <C>Scene Capture Type</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_SceneCaptureType></C> </R>
<R> <C><FC>_EXIF_GainControl<FN></C> <C>Gain Control</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_GainControl></C> </R>
<R> <C><FC>_EXIF_Contrast<FN></C> <C>Contrast</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_Contrast></C> </R>
<R> <C><FC>_EXIF_Saturation<FN></C> <C>Saturation</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_Saturation></C> </R>
<R> <C><FC>_EXIF_Sharpness<FN></C> <C>Sharpness</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_Sharpness></C> </R>
<R> <C><FC>_EXIF_SubjectDistanceRange<FN></C> <C>Subject Distance</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_SubjectDistanceRange></C> </R>
<R> <C><FC>_EXIF_GPSLatitude<FN></C> <C>GPS Latitude</C> <C>no</C> <C><A TIOParamsVals.EXIF_GPSLatitude_Str></C> </R>
<R> <C><FC>_EXIF_GPSLongitude<FN></C> <C>GPS Longitude</C> <C>no</C> <C><A TIOParamsVals.EXIF_GPSLongitude_Str></C> </R>
<R> <C><FC>_EXIF_GPSAltitude<FN></C> <C>GPS Altitude</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_GPSAltitude> and <A TIOParamsVals.EXIF_GPSAltitudeRef></C> </R>
<R> <C><FC>_EXIF_GPSImageDirection<FN></C> <C>GPS Image Direction</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_GPSImgDirection> and <A TIOParamsVals.EXIF_GPSImgDirectionRef></C> </R>
<R> <C><FC>_EXIF_GPSTrack<FN></C> <C>GPS Movement Direction</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_GPSTrack> and <A TIOParamsVals.EXIF_GPSTrackRef></C> </R>
<R> <C><FC>_EXIF_GPSSpeed<FN></C> <C>GPS Movement Speed</C> <C>no</C> <C>Text description of <A TIOParamsVals.EXIF_GPSSpeed> and <A TIOParamsVals.EXIF_GPSSpeedRef></C> </R>
<R> <C><FC>_EXIF_GPSDateAndTime<FN></C> <C>GPS Date and Time</C> <C>no</C> <C><A TIOParamsVals.EXIF_GPSDateStamp></C> </R>
<R> <C><FC>_EXIF_GPSSatellites<FN></C> <C>GPS Satellites</C> <C>YES</C> <C><A TIOParamsVals.EXIF_GPSSatellites></C> </R>
<R> <C><FC>_EXIF_GPSVersionID<FN></C> <C>GPS Version</C> <C>YES</C> <C><A TIOParamsVals.EXIF_GPSVersionID></C> </R>
<R> <C><FC>_EXIF_Artist<FN></C> <C>Artist</C> <C>YES</C> <C><A TIOParamsVals.EXIF_Artist></C> </R>
<R> <C><FC>_EXIF_XPTitle<FN></C> <C>Title (Windows)</C> <C>YES</C> <C><A TIOParamsVals.EXIF_XPTitle></C> </R>
<R> <C><FC>_EXIF_XPComment<FN></C> <C>Comment (Windows)</C> <C>YES</C> <C><A TIOParamsVals.EXIF_XPComment></C> </R>
<R> <C><FC>_EXIF_XPAuthor<FN></C> <C>Author (Windows)</C> <C>YES</C> <C><A TIOParamsVals.EXIF_XPAuthor></C> </R>
<R> <C><FC>_EXIF_XPKeywords<FN></C> <C>Keywords (Windows)</C> <C>YES</C> <C><A TIOParamsVals.EXIF_XPKeywords></C> </R>
<R> <C><FC>_EXIF_XPSubject<FN></C> <C>Subject (Windows)</C> <C>YES</C> <C><A TIOParamsVals.EXIF_XPSubject></C> </R>
<R> <C><FC>_EXIF_XPRating<FN></C> <C>Rating (Windows)</C> <C>YES</C> <C>IntToStr(<A TIOParamsVals.EXIF_XPRating>)</C> </R>
<R> <C><FC>_EXIF_InteropVersion<FN></C> <C>Interoperability Version</C> <C>YES</C> <C><A TIOParamsVals.EXIF_InteropVersion></C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIOParamsValsHelper.EXIF_FieldDescription>

<FM>Examples<FC>
// Output all fields of current image to a memo (same as <A TIOParamsValsHelper.EXIF_WriteToStrings>)
for i := 0 to _EXIF_Tag_Count - 1 do
  memo1.Lines.Add( ImageEnView1.IO.Params.EXIF_FieldDescription[ i ] + ': ' + ImageEnView1.IO.Params.EXIF_AsStr[ i ] );

// Set EXIF_ShutterSpeedValue to 1/4 a second
ImageEnView1.IO.Params.EXIF_AsStr[ _EXIF_ShutterSpeedValue ] := '1/4';
!!}
// get a string representation of the exif data specified by the field index
function TIOParamsValsHelper.GetEXIF_AsStr(Index: Integer): string;
var
  sFPUnit: string; // used in retrieval of EXIF data (resolution unit)
  sResUnit: string; // used in retrieval of EXIF data (resolution unit)
begin
  result := '';
  try
    case Index of

      _EXIF_UserComment: result := RemoveNull(EXIF_UserComment);
      _EXIF_ImageDescription: result := RemoveNull(EXIF_ImageDescription);
      _EXIF_CameraMake : Result := EXIF_Make;
      _EXIF_CameraModel: Result := EXIF_Model;

      {
      Unit of XResolution/YResolution:
      1 means no-unit
      2 means inch
      3 means centimeter
      Default value is 2 (inch)
      }
      _EXIF_XResolution: { EXIF_XResolution and EXIF_ResolutionUnit }
        begin
          case EXIF_ResolutionUnit of
            2: sResUnit := s_Inch;
            3: sResUnit := s_CM;
          else
            sResUnit := '';
          end;
          result := EXIF_XResolution_Str + ' ' + sResUnit;
        end;

      {
      Unit of XResolution/YResolution:
      1 means no-unit
      2 means inch
      3 means centimeter
      Default value is 2 (inch)
      }
      _EXIF_YResolution: { EXIF_YResolution and EXIF_ResolutionUnit }
        begin
          case EXIF_ResolutionUnit of
            2: sResUnit := s_Inch;
            3: sResUnit := s_CM;
          else
            sResUnit := '';
          end;

          result := EXIF_YResolution_Str + ' ' + sResUnit;
        end;

      _EXIF_DateTime: result := DateTimeToStr(EXIF_DateTime2);
      _EXIF_DateTimeOriginal: result := DateTimeToStr(EXIF_DateTimeOriginal2);
      _EXIF_DateTimeDigitized: result := DateTimeToStr(EXIF_DateTimeDigitized2);

      _EXIF_Copyright: result := RemoveNull(EXIF_Copyright);

      _EXIF_Orientation:
        case EXIF_Orientation of
          1: result := s_EXIFOrientation1;
          2: result := s_EXIFOrientation2;
          3: result := s_EXIFOrientation3;
          4: result := s_EXIFOrientation4;
          5: result := s_EXIFOrientation5;
          6: result := s_EXIFOrientation6;
          7: result := s_EXIFOrientation7;
          8: result := s_EXIFOrientation8;
        end;

      _EXIF_ExposureTime:
        begin
          result := EXIF_ExposureTime_Str;
          if result <> '' then
          begin
            if Pos('1/', result) > 0 then
              result := result + ' ' + s_Second
            else
              result := result + ' ' + s_Seconds
          end;
        end;

      _EXIF_FNumber: Result := EXIF_FNumber_Str;

      {
      EXIF_ExposureProgram:
      1 means manual control
      2 program normal
      3 aperture priority
      4 shutter priority
      5 program creative (slow program)
      6 program action(high-speed program)
      7 portrait mode
      8 landscape mode.
      }
      _EXIF_ExposureProgram:
        case EXIF_ExposureProgram of
          1: result := s_ManualControl;
          2: result := s_ProgramNormal;
          3: result := s_AperturePriority;
          4: result := s_ShutterPriority;
          5: result := s_CreativeProgram;
          6: result := s_ActionProgram;
          7: result := s_Portraitmode;
          8: result := s_LandscapeMode;
        end;

      // CCD sensitivity equivalent to Ag-Hr film speedrate.
      _EXIF_ISOSpeedRatings:
        if (EXIF_ISOSpeedRatings[0] <> 0) and (EXIF_ISOSpeedRatings[0] <> -1) then
          result := inttostr(EXIF_ISOSpeedRatings[0]);

      _EXIF_ShutterSpeedValue: result := EXIF_ShutterSpeedValue_Str;
      _EXIF_ApertureValue: result := EXIF_ApertureValue_Str;

      {
      Brightness of taken subject, unit is APEX. To calculate Exposure(Ev) from BrigtnessValue(Bv), you must add SensitivityValue(Sv).
      Ev=Bv+Sv   Sv=log2(ISOSpeedRating/3.125)
      ISO100 : Sv=5, ISO200 : Sv=6, ISO400 : Sv=7, ISO125 : Sv=5.32.
      }
      _EXIF_BrightnessValue: Result := FloatToStrOrNull(EXIF_BrightnessValue, '');

      _EXIF_ExposureBiasValue: result := FloatToStrOrNull(EXIF_ExposureBiasValue, '');
      _EXIF_MaxApertureValue: result := EXIF_MaxApertureValue_Str;

      // Distance to focus point, unit is meter.
      _EXIF_SubjectDistance:
        if EXIF_SubjectDistance > 0 then
          result := FloatToStrOrNull(EXIF_SubjectDistance, ' m');

      {
      Exposure metering method:
      0 means unknown
      1 average
      2 center weighted average
      3 spot
      4 multi-spot
      5 multi-segment
      6 partial
      255 other.
      }
      _EXIF_MeteringMode:
        case EXIF_MeteringMode of
          1: result := s_average;
          2: result := s_CenterWeightedAverage;
          3: result := s_Spot;
          4: result := s_MultiSpot;
          5: result := s_MultiSegment;
          6: result := s_partial;
        end;

      {
      Light source, actually this means white balance setting:
      0 means unknown
      1 daylight
      2 fluorescent
      3 tungsten
      10 flash
      17 standard light A
      18 standard light B
      19 standard light C
      20 D55
      21 D65
      22 D75
      255 other.
      }
      _EXIF_LightSource:
        case EXIF_LightSource of
          1: result := s_daylight;
          2: result := s_fluorescent;
          3: result := s_tungsten;
          10: result := s_flash;
          17: result := s_standardLightA;
          18: result := s_standardLightB;
          19: result := s_standardLightC;
          20: result := s_D55;
          21: result := s_D65;
          22: result := s_D75;
        end;

      {
      EXIF_Flash:
      0 means flash did not fire
      1 flash fired
      5 flash fired but strobe return light not detected
      7 flash fired and strobe return light detected
      }
      _EXIF_Flash: result := FlashModeToString(EXIF_Flash);

      // Focal length of lens used to take image. Unit is millimeter.
      _EXIF_FocalLength:
        if EXIF_FocalLength > 0 then
          result := FloatToStrOrNull(EXIF_FocalLength, ' mm');

      // Stores FlashPix version. If the image data is based on FlashPix formar Ver.1.0, value is "0100".
      _EXIF_FlashPixVersion: result := RemoveNull(EXIF_FlashPixVersion);

      // Defines Color Space. DCF image must use sRGB color space so value is always '1'. If the picture uses the other color space, value is '65535' : Uncalibrated.
      _EXIF_ColorSpace:
        case EXIF_ColorSpace of
          1: result := s_RGB;
          65535: result := s_Uncalibrated;
        end;

      _EXIF_ExifImageWidth  : if EXIF_ExifImageWidth > 0 then
                                result := IntToStr(EXIF_ExifImageWidth);
      _EXIF_ExifImageHeight : if EXIF_ExifImageHeight > 0 then
                                result := IntToStr(EXIF_ExifImageHeight);

      // If this digicam can record audio data with image, shows name of audio data.
      _EXIF_RelatedSoundFile: result := RemoveNull(EXIF_RelatedSoundFile);

      {
      Unit of FocalPlaneXResoluton/FocalPlaneYResolution:
      1 means no-unit
      2 inch
      3 centimeter.
      Note : Some of Fujifilm's digicam(e.g.FX2700, FX2900, Finepix4700Z/40i etc) uses value '3' so it must be 'centimeter', but it seems that they use a '8.3mm?'(1/3in.?) to their ResolutionUnit. Fuji's BUG? Finepix4900Z has been changed to use value '2' but it doesn't match to actual value also.
      }
      _EXIF_FocalPlaneXResolution:
        begin
          case EXIF_FocalPlaneResolutionUnit of
            2: sFPUnit := s_PerInch;
            3: sFPUnit := s_PerCM;
          else
            sFPUnit := '';
          end;

          // Pixel density at CCD's position. If you have MegaPixel digicam and take a picture by lower resolution(e.g.VGA mode), this value is re-sampled by picture resolution. In such case, FocalPlaneResolution is not same as CCD's actual resolution.
          if EXIF_FocalPlaneXResolution > 50 then
            result := IntToStr(Trunc(EXIF_FocalPlaneXResolution)) + sFPUnit
          else
          if EXIF_FocalPlaneXResolution > 0 then
            result := IEFloatToFormatString(EXIF_FocalPlaneXResolution, 2, true) + sFPUnit;
        end;

      _EXIF_FocalPlaneYResolution:
        begin
          case EXIF_FocalPlaneResolutionUnit of
            2: sFPUnit := s_PerInch;
            3: sFPUnit := s_PerCM;
          else
            sFPUnit := '';
          end;

          // Pixel density at CCD's position. If you have MegaPixel digicam and take a picture by lower resolution(e.g.VGA mode), this value is re-sampled by picture resolution. In such case, FocalPlaneResolution is not same as CCD's actual resolution.
          if EXIF_FocalPlaneYResolution > 50 then
            result := IntToStr(Trunc(EXIF_FocalPlaneYResolution)) + sFPUnit
          else
          if EXIF_FocalPlaneYResolution > 0 then
            result := IEFloatToFormatString(EXIF_FocalPlaneYResolution, 2, true) + sFPUnit;
        end;

      // EXIF_ExposureIndex: Same as ISOSpeedRatings(0x8827) but data type is unsigned rational. Only Kodak's digicam uses this tag instead of ISOSpeedRating.
      _EXIF_ExposureIndex: result := FloatToStrOrNull(EXIF_ExposureIndex, '');

      // Image sensor unit: '2' means 1 chip color area sensor, most of all digicam use this type.
      _EXIF_SensingMethod:
        if EXIF_SensingMethod = 2 then
          result := s_OneChipColorAreaSensor;

      // Image source: Value '0x03' means the image source is digital still camera.
      _EXIF_FileSource:
        if EXIF_FileSource = $03 then
          result := s_DigitalStillCamera;

      // Type of scene: Value '0x01' means that the image was directly photographed.
      _EXIF_SceneType:
        if EXIF_SceneType = $01 then
          result := s_DirectlyPhotographed;

      {
      When image format is YCbCr and uses 'Subsampling' (cropping of chroma data, all the digicam do that), defines the chroma sample point of subsampling pixel array.
      1 means the center of pixel array
      2 means the datum point.
      }
      _EXIF_YCbCrPositioning:
        case EXIF_YcbCrPositioning of
          1: result := s_Centered;
          2: result := s_DataPoint;
        end;

      {
      The exposure mode set when the image was shot. In auto-bracketing mode, the camera shoots a series of frames of the same scene at different exposure settings.
      0 = Auto exposure
      1 = Manual exposure
      2 = Auto bracket
      }
      _EXIF_ExposureMode:
        case EXIF_ExposureMode of
          0: s_Autoexposure;
          1: s_Manualexposure;
          2: s_Autobracket;
        end;

      {
      The white balance mode set when the image was shot.
      0 = Auto white balance
      1 = Manual white balance
      }
      _EXIF_WhiteBalance:
        case EXIF_WhiteBalance of
          0: s_Autowhitebalance;
          1: s_Manualwhitebalance;
        end;

      // The digital zoom ratio when the image was shot. If the numerator of the recorded value is 0, this indicates that digital zoom was not used.
      _EXIF_DigitalZoomRatio:
        if (EXIF_DigitalZoomRatio = 0) or (EXIF_DigitalZoomRatio = -1) then
          result := s_None
        else
          result := FloatToStrOrNull(EXIF_DigitalZoomRatio, '');

      // The equivalent focal length assuming a 35mm film camera, in mm. A value of 0 means the focal length is unknown. Note that this tag differs from the FocalLength tag.
      _EXIF_FocalLengthIn35mmFilm:
        if EXIF_FocalLengthIn35mmFilm > 0 then
          result := IntToStr(EXIF_FocalLengthIn35mmFilm) + ' mm';

      {
      The type of scene that was shot. It can also be used to record the mode in which the image was shot. Note that this differs from the scene type (SceneType) tag.
      0 = Standard
      1 = Landscape
      2 = Portrait
      3 = Night scene
      }
      _EXIF_SceneCaptureType:
        case EXIF_SceneCaptureType of
          0: s_Standard;
          1: s_Landscape; 
          2: s_Portrait;
          3: s_Nightscene;
        end;

      {
      The degree of overall image gain adjustment.
      0 = None
      1 = Low gain up
      2 = High gain up
      3 = Low gain down
      4 = High gain down
      }
      _EXIF_GainControl:
        case EXIF_GainControl of
          0: s_None;
          1: s_LowGainup;
          2: s_HighGainup; 
          3: s_LowGaindown;
          4: s_HighGaindown;
        end;

      {
      The direction of contrast processing applied by the camera when the image was shot.
      0 = Normal
      1 = Soft
      2 = Hard
      }
      _EXIF_Contrast:
        case EXIF_Contrast of
          0: s_Normal;
          1: s_Soft;
          2: s_Hard;
        end;

      {
      The direction of saturation processing applied by the camera when the image was shot.
      0 = Normal
      1 = Low saturation
      2 = High saturation
      }
      _EXIF_Saturation:
        case EXIF_Saturation of
          0: s_Normal;
          1: s_Lowsaturation; 
          2: s_Highsaturation;
        end;

      {
      The direction of sharpness processing applied by the camera when the image was shot.
      0 = Normal
      1 = Soft
      2 = Hard
      }
      _EXIF_Sharpness:
        case EXIF_Sharpness of
          0: s_Normal;
          1: s_Soft;
          2: s_Hard;
        end;

      {
      The distance to the subject:
      0 = unknown
      1 = Macro
      2 = Close view
      3 = Distant view
      }
      _EXIF_SubjectDistanceRange:
        case EXIF_SubjectDistanceRange of
          1: s_Macro;
          2: s_Closeview;
          3: s_Distantview;
        end;

      _EXIF_GPSLatitude  : Result := EXIF_GPSLatitude_Str;
      _EXIF_GPSLongitude : Result := EXIF_GPSLongitude_Str;

      // The altitude based on the reference in Ref. The reference unit is meters.
      _EXIF_GPSAltitude:
        if (EXIF_GPSAltitude <> 0) or (EXIF_GPSAltitudeRef <> '') then
        begin
          result := FloatToStrOrNull(EXIF_GPSAltitude, ' m');

          if RemoveNull(EXIF_GPSAltitudeRef) = '1' then
            Result := Result + ' (Below sea-level)';
        end;

      _EXIF_GPSDateAndTime:
        if (EXIF_GPSDateStamp <> '') then
        begin
          result := Format('%s %s : %s.%s', [RemoveNull(EXIF_GPSDateStamp),
            FloatToStr(EXIF_GPSTimeStampHour),
              FloatToStr(EXIF_GPSTimeStampMinute),
              FloatToStr(EXIF_GPSTimeStampSecond)]);
        end;

      _EXIF_GPSImageDirection:
        {
        EXIF_GPSImgDirection :  The direction of the image when it was captured. The range of values is from 0.00 to 359.99.
        EXIF_GPSImgDirectionRef :  The reference for giving the direction of the image when it is captured. 'T' denotes true direction and 'M' is magnetic direction.
        }
        if (EXIF_GPSImgDirection <> 0) or (RemoveNull(EXIF_GPSImgDirectionRef) <> '') then
        begin
          result := FloatToStrOrNull(EXIF_GPSImgDirection, ' degrees');
          if uppercase(RemoveNull(EXIF_GPSImgDirectionRef)) = 'T' then
            Result := Result + ' (True)'
          else
          if uppercase(RemoveNull(EXIF_GPSImgDirectionRef)) = 'M' then
            Result := Result + ' (Magnetic)';
        end;

      _EXIF_GPSSpeed:
        {
        EXIF_GPSSpeed :  speed of GPS receiver movement
        EXIF_GPSSpeedRef :  unit used to express the GPS receiver speed of movement. 'K' 'M' and 'N' represents kilometers per hour, miles per hour, and knots.
        }
        if (EXIF_GPSSpeed <> 0) or (RemoveNull(EXIF_GPSSpeedRef) <> '') then
        begin
          if uppercase(RemoveNull(EXIF_GPSSpeedRef)) = 'K' then
            Result := FloatToStr(EXIF_GPSSpeed) + ' km/h'
          else
          if uppercase(RemoveNull(EXIF_GPSSpeedRef)) = 'M' then
            Result := FloatToStr(EXIF_GPSSpeed) + ' mph'
          else
          if uppercase(RemoveNull(EXIF_GPSSpeedRef)) = 'N' then
            Result := FloatToStr(EXIF_GPSSpeed) + ' knots'
          else
            Result := FloatToStr(EXIF_GPSSpeed) + ' ' + RemoveNull(EXIF_GPSSpeedRef);
        end;

      _EXIF_GPSTrack:
        {
        EXIF_GPSTrack :  direction of GPS receiver movement. The range of values is from 0.00 to 359.99.
        EXIF_GPSTrackRef :  The reference for giving the direction of the direction of GPS receiver movement. 'T' denotes true direction and 'M' is magnetic direction.
        }
        if (EXIF_GPSTrack <> 0) or (RemoveNull(EXIF_GPSTrackRef) <> '') then
        begin
          result := FloatToStrOrNull(EXIF_GPSTrack, ' degrees');
          if uppercase(RemoveNull(EXIF_GPSTrackRef)) = 'T' then
            Result := Result + ' (True)'
          else
          if uppercase(RemoveNull(EXIF_GPSTrackRef)) = 'M' then
            Result := Result + ' (Magnetic)';
        end;

      _EXIF_GPSSatellites: result := RemoveNull(EXIF_GPSSatellites);
      _EXIF_GPSVersionID: result := RemoveNull(EXIF_GPSVersionID);

      _EXIF_Artist: result := RemoveNull(EXIF_Artist);

      _EXIF_XPTitle: result := WideCharToString(PWideChar(EXIF_XPTitle));
      _EXIF_XPComment: result := WideCharToString(PWideChar(EXIF_XPComment));
      _EXIF_XPAuthor: result := WideCharToString(PWideChar(EXIF_XPAuthor));
      _EXIF_XPKeywords: result := WideCharToString(PWideChar(EXIF_XPKeywords));
      _EXIF_XPSubject: result := WideCharToString(PWideChar(EXIF_XPSubject));
      _EXIF_XPRating:
        begin
          {
           Windows XP Rating
           Allowed values from 0 up to 5.  -1 means not available.
          }
          if EXIF_XPRating < 0 then
            Result := ''
          else
            Result := IntToStr(EXIF_XPRating);
        end;

      _EXIF_InteropVersion : result := RemoveNull(EXIF_InteropVersion);
    end;

  except
    // ERROR
  end;
end;


{!!
<FS>TIOParamsValsHelper.EXIF_FieldDescription

<FM>Declaration<FC>
property EXIF_FieldDescription [index : Integer]: string; (read-only)

<FM>Description<FN>
Returns a description of an EXIF field specified by the <L EXIF Consts for EXIF_AsStr>constants</L> used by <A TIOParamsValsHelper.EXIF_AsStr>.

Index is a value between 0 and <FC>_EXIF_Tag_Count<FN> - 1.

<TABLE>
<R> <H>Const</H> <H>Description</H> </R>
<R> <C><FC>_EXIF_UserComment<FN></C> <C>User Comment</C> </R>
<R> <C><FC>_EXIF_ImageDescription<FN></C> <C>Description</C> </R>
<R> <C><FC>_EXIF_Make<FN></C> <C>Camera Make</C> </R>
<R> <C><FC>_EXIF_Model<FN></C> <C>Camera Model</C> </R>
<R> <C><FC>_EXIF_XResolution<FN></C> <C>Horizontal Resolution</C> </R>
<R> <C><FC>_EXIF_YResolution<FN></C> <C>Vertical Resolution</C> </R>
<R> <C><FC>_EXIF_DateTime<FN></C> <C>Date and Time</C> </R>
<R> <C><FC>_EXIF_DateTimeOriginal<FN></C> <C>Original Date and Time</C> </R>
<R> <C><FC>_EXIF_DateTimeDigitized<FN></C> <C>Digitized Date and Time</C> </R>
<R> <C><FC>_EXIF_Copyright<FN></C> <C>Copyright</C> </R>
<R> <C><FC>_EXIF_Orientation<FN></C> <C>Orientation</C> </R>
<R> <C><FC>_EXIF_ExposureTime<FN></C> <C>Exposure Time</C> </R>
<R> <C><FC>_EXIF_FNumber<FN></C> <C>F-Stop</C> </R>
<R> <C><FC>_EXIF_ExposureProgram<FN></C> <C>Exposure Program</C> </R>
<R> <C><FC>_EXIF_ISOSpeedRatings<FN></C> <C>ISO Speed Rating</C> </R>
<R> <C><FC>_EXIF_ShutterSpeedValue<FN></C> <C>Shutter Speed</C> </R>
<R> <C><FC>_EXIF_ApertureValue<FN></C> <C>Aperture Value</C> </R>
<R> <C><FC>_EXIF_BrightnessValue<FN></C> <C>Brightness</C> </R>
<R> <C><FC>_EXIF_ExposureBiasValue<FN></C> <C>Exposure Compensation</C> </R>
<R> <C><FC>_EXIF_MaxApertureValue<FN></C> <C>Max Aperture Value</C> </R>
<R> <C><FC>_EXIF_SubjectDistance<FN></C> <C>Subject Distance</C> </R>
<R> <C><FC>_EXIF_MeteringMode<FN></C> <C>Metering Mode</C> </R>
<R> <C><FC>_EXIF_LightSource<FN></C> <C>Lighting</C> </R>
<R> <C><FC>_EXIF_Flash<FN></C> <C>Flash</C> </R>
<R> <C><FC>_EXIF_FocalLength<FN></C> <C>Focal Length</C> </R>
<R> <C><FC>_EXIF_FlashPixVersion<FN></C> <C>FlashPix Version</C> </R>
<R> <C><FC>_EXIF_ColorSpace<FN></C> <C>Color Space</C> </R>
<R> <C><FC>_EXIF_ExifImageWidth<FN></C> <C>Image Width</C> </R>
<R> <C><FC>_EXIF_ExifImageHeight<FN></C> <C>Image Height</C> </R>
<R> <C><FC>_EXIF_RelatedSoundFile<FN></C> <C>Sound File</C> </R>
<R> <C><FC>_EXIF_FocalPlaneXResolution<FN></C> <C>Focal Plane Horz. Resolution</C> </R>
<R> <C><FC>_EXIF_FocalPlaneYResolution<FN></C> <C>Focal Plane Vert. Resolution</C> </R>
<R> <C><FC>_EXIF_ExposureIndex<FN></C> <C>Exposure Index</C> </R>
<R> <C><FC>_EXIF_SensingMethod<FN></C> <C>Sensing Method</C> </R>
<R> <C><FC>_EXIF_FileSource<FN></C> <C>File Source</C> </R>
<R> <C><FC>_EXIF_SceneType<FN></C> <C>Scene Type</C> </R>
<R> <C><FC>_EXIF_YCbCrPositioning<FN></C> <C>Data Point</C> </R>
<R> <C><FC>_EXIF_ExposureMode<FN></C> <C>Exposure Mode</C> </R>
<R> <C><FC>_EXIF_WhiteBalance<FN></C> <C>White Balance</C> </R>
<R> <C><FC>_EXIF_DigitalZoomRatio     <FN></C> <C>Digital Zoom Ratio</C> </R>
<R> <C><FC>_EXIF_FocalLengthIn35mmFilm<FN></C> <C>Focal Length in 35mm Film</C> </R>
<R> <C><FC>_EXIF_SceneCaptureType  <FN></C> <C>Scene Capture Type</C> </R>
<R> <C><FC>_EXIF_GainControl<FN></C> <C>Gain Control</C> </R>
<R> <C><FC>_EXIF_Contrast  <FN></C> <C>Contrast</C> </R>
<R> <C><FC>_EXIF_Saturation<FN></C> <C>Saturation</C> </R>
<R> <C><FC>_EXIF_Sharpness<FN></C> <C>Sharpness</C> </R>
<R> <C><FC>_EXIF_SubjectDistanceRange<FN></C> <C>Subject Distance</C> </R>
<R> <C><FC>_EXIF_GPSLatitude<FN></C> <C>GPS Latitude</C> </R>
<R> <C><FC>_EXIF_GPSLongitude<FN></C> <C>GPS Longitude</C> </R>
<R> <C><FC>_EXIF_GPSAltitude<FN></C> <C>GPS Altitude</C> </R>
<R> <C><FC>_EXIF_GPSImageDirection<FN></C> <C>GPS Image Direction</C> </R>
<R> <C><FC>_EXIF_GPSTrack<FN></C> <C>GPS Movement Direction</C> </R>
<R> <C><FC>_EXIF_GPSSpeed<FN></C> <C>GPS Movement Speed</C> </R>
<R> <C><FC>_EXIF_GPSDateAndTime<FN></C> <C>GPS Date and Time</C> </R>
<R> <C><FC>_EXIF_GPSSatellites<FN></C> <C>GPS Satellites</C> </R>
<R> <C><FC>_EXIF_GPSVersionID<FN></C> <C>GPS Version</C> </R>
<R> <C><FC>_EXIF_Artist<FN></C> <C>Artist</C> </R>
<R> <C><FC>_EXIF_XPTitle<FN></C> <C>Title (Windows)</C> </R>
<R> <C><FC>_EXIF_XPComment<FN></C> <C>Comment (Windows)</C> </R>
<R> <C><FC>_EXIF_XPAuthor<FN></C> <C>Author (Windows)</C> </R>
<R> <C><FC>_EXIF_XPKeywords<FN></C> <C>Keywords (Windows)</C> </R>
<R> <C><FC>_EXIF_XPSubject<FN></C> <C>Subject (Windows)</C> </R>
<R> <C><FC>_EXIF_XPRating<FN></C> <C>Rating (Windows)</C> </R>
<R> <C><FC>_EXIF_InteropVersion<FN></C> <C>Interoperability Version</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIOParamsValsHelper.EXIF_AsStr>

<FM>Example<FC>
// Output all fields of current image to a memo (same as <A TIOParamsValsHelper.EXIF_WriteToStrings>)
for i := 0 to _EXIF_Tag_Count - 1 do
  memo1.Lines.Add( ImageEnView1.IO.Params.EXIF_FieldDescription[ i ] + ': ' + ImageEnView1.IO.Params.EXIF_AsStr[ i ] );
!!}
function TIOParamsValsHelper.GetEXIF_FieldDescription(Index: Integer): string;
begin
  Result := ExifTags[ Index ].Desc;
end;

{!!
<FS>TIOParamsValsHelper.EXIF_CanWriteEXIFData

<FM>Declaration<FC>
property EXIF_CanWriteEXIFData : Boolean; (read-only)

<FM>Description<FN>
Returns true if the current image is of a type that supports EXIF writing, i.e. TIFF or JPEG

Note: This is only based on the current format. Naturally you could add EXIF data to an image that was loaded as a Bitmap and then save it to JPEG.  
!!}
function TIOParamsValsHelper.GetEXIF_CanWriteEXIFData : Boolean;
begin
  Result := FileType in [ioJPEG, ioTIFF];
end;


procedure TIOParamsValsHelper.SetEXIF_Camera_Str(const value: string);
begin
  // EXIF_Make and EXIF_Model combined
  if GetEXIF_Camera_Str <> Value then
  begin
    EXIF_Make := GetTextBeforeChar(Value, ' ', False);
    EXIF_Model := GetTextAfterChar(Value, ' ', True);
  end;
end;

procedure TIOParamsValsHelper.SetEXIF_XResolution_Str(value: string);
begin
  // Display/Print resolution of image. Default value is 1/72inch, but it has no mean because personal computer doesn't use this value to display/print out.
  Value := StripSuffix(value, ' ' + s_Inch);
  Value := StripSuffix(value, ' ' + s_CM);

  EXIF_XResolution := FractionToDouble(value);
end;

procedure TIOParamsValsHelper.SetEXIF_YResolution_Str(value: string);
begin      
  // Display/Print resolution of image. Default value is 1/72inch, but it has no mean because personal computer doesn't use this value to display/print out.
  Value := StripSuffix(value, ' ' + s_Inch);
  Value := StripSuffix(value, ' ' + s_CM);

  EXIF_YResolution := FractionToDouble(value);
end;

procedure TIOParamsValsHelper.SetEXIF_ExposureTime_Str(value: string);
begin
  // Exposure time (reciprocal of shutter speed). Unit is seconds
  Value := StripSuffix(value, ' ' + s_Seconds);
  Value := StripSuffix(value, ' ' + s_Second);
  if pos('1/', value) > 0 then
    EXIF_ExposureTime := 1 / FractionToDouble(value)
  else
    EXIF_ExposureTime := StrToInt(value);
end;

procedure TIOParamsValsHelper.SetEXIF_FNumber_Str(value: string);
begin                     
  // The actual F-number(F-stop) of lens when the image was taken
  if uppercase(value)[1] = 'F' then
    delete(value, 1, 1);

  EXIF_FNumber := StrToFloat(value);
end;

procedure TIOParamsValsHelper.SetEXIF_ShutterSpeedValue_Str(const value: string);
begin
  // Shutter speed by APEX value. To convert this value to ordinary 'Shutter Speed'; calculate this value's power of 2, then reciprocal. For example, if the ShutterSpeedValue is '4', shutter speed is 1/(24)=1/16 second
  EXIF_ShutterSpeedValue := StrToApex(2, value, '1/');
end;

procedure TIOParamsValsHelper.SetEXIF_ApertureValue_Str(const value: string);
begin
  // The actual aperture value of lens when the image was taken. Unit is APEX. To convert this value to
  // ordinary F-number (F-stop), calculate this value's power of root 2 (=1.4142). For example, if the
  // ApertureValue is '5', F-number is 1.41425 = F5.6.
  EXIF_ApertureValue := StrToApex(Sqrt(2), value, 'F');
end;

procedure TIOParamsValsHelper.SetEXIF_MaxApertureValue_Str(const value: string);
begin
  // Maximum aperture value of lens. You can convert to F-number by calculating power of root 2 (same process of ApertureValue)
  EXIF_MaxApertureValue := StrToApex(Sqrt(2), value, 'F');
end;

// exif data specified by the field index with the value in the specified string
procedure TIOParamsValsHelper.SetEXIF_AsStr(Index: Integer; value : string);
begin
  value := Trim(value);
  try
    case Index of

      _EXIF_UserComment: EXIF_UserComment := value;
      _EXIF_ImageDescription: EXIF_ImageDescription := value;
      _EXIF_CameraMake  : EXIF_Make := Value;
      _EXIF_CameraModel : EXIF_Model := Value;

      {
      Unit of XResolution/YResolution:
      1 means no-unit
      2 means inch
      3 means centimeter
      Default value is 2 (inch)
      }
      _EXIF_XResolution:
        begin
          if pos(uppercase(s_Inch), uppercase(value)) > 0 then
            EXIF_ResolutionUnit := 2
          else
          if pos(uppercase(s_CM), uppercase(value)) > 0 then
            EXIF_ResolutionUnit := 3;

          EXIF_XResolution_Str := value;
        end;

      {                  
      Unit of XResolution/YResolution:
      1 means no-unit
      2 means inch
      3 means centimeter
      Default value is 2 (inch)
      }
      _EXIF_YResolution:
        begin
          if pos(uppercase(s_Inch), uppercase(value)) > 0 then
            EXIF_ResolutionUnit := 2
          else
          if pos(uppercase(s_CM), uppercase(value)) > 0 then
            EXIF_ResolutionUnit := 3;

          Value := StripSuffix(value, s_Inch);
          Value := StripSuffix(value, s_CM);

          EXIF_YResolution_Str := value;
        end;

      _EXIF_DateTime: EXIF_DateTime2 := StrToDateTime(value);
      _EXIF_DateTimeOriginal: EXIF_DateTimeOriginal2 := StrToDateTime(value);
      _EXIF_DateTimeDigitized: EXIF_DateTimeDigitized2 := StrToDateTime(value);

      _EXIF_Copyright: EXIF_Copyright := value;

      // The orientation of the camera relative to the scene, when the image was captured
      _EXIF_Orientation:
        begin
          if SameText(value, s_EXIFOrientation1) then
            EXIF_Orientation := 1
          else
          if SameText(value, s_EXIFOrientation2) then
            EXIF_Orientation := 2
          else
          if SameText(value, s_EXIFOrientation3) then
            EXIF_Orientation := 3
          else
          if SameText(value, s_EXIFOrientation4) then
            EXIF_Orientation := 4
          else
          if SameText(value, s_EXIFOrientation5) then
            EXIF_Orientation := 5
          else
          if SameText(value, s_EXIFOrientation6) then
            EXIF_Orientation := 6
          else
          if SameText(value, s_EXIFOrientation7) then
            EXIF_Orientation := 7
          else
          if SameText(value, s_EXIFOrientation8) then
            EXIF_Orientation := 8;
        end;

      _EXIF_ExposureTime: EXIF_ExposureTime_Str := Value;
      _EXIF_FNumber: EXIF_FNumber_Str := value;

      {
      Exposure program that the camera used when image was taken:
      1 means manual control
      2 program normal
      3 aperture priority
      4 shutter priority
      5 program creative (slow program)
      6 program action(high-speed program)
      7 portrait mode
      8 landscape mode.
      }
      _EXIF_ExposureProgram:
        begin    
          if SameText(value, s_ManualControl) then
            EXIF_ExposureProgram := 1
          else
          if SameText(value, s_ProgramNormal) then
            EXIF_ExposureProgram := 2
          else
          if SameText(value, s_AperturePriority) then
            EXIF_ExposureProgram := 3
          else
          if SameText(value, s_ShutterPriority) then
            EXIF_ExposureProgram := 4
          else
          if SameText(value, s_CreativeProgram) then
            EXIF_ExposureProgram := 5
          else
          if SameText(value, s_ActionProgram) then
            EXIF_ExposureProgram := 6
          else
          if SameText(value, s_Portraitmode) then
            EXIF_ExposureProgram := 7
          else
          if SameText(value, s_LandscapeMode) then
            EXIF_ExposureProgram := 8;
        end;

      // EXIF_ISOSpeedRatings: CCD sensitivity equivalent to Ag-Hr film speedrate.
      _EXIF_ISOSpeedRatings: EXIF_ISOSpeedRatings[0] := StrToInt(value);
      _EXIF_ShutterSpeedValue: EXIF_ShutterSpeedValue_Str := value;
      _EXIF_ApertureValue: EXIF_ApertureValue_Str := value;

      {
      Brightness of taken subject, unit is APEX. To calculate Exposure(Ev) from BrigtnessValue(Bv), you must add SensitivityValue(Sv):
      Ev=Bv+Sv   Sv=log2(ISOSpeedRating/3.125)
      ISO100 : Sv=5, ISO200 : Sv=6, ISO400 : Sv=7, ISO125 : Sv=5.32.
      }
      _EXIF_BrightnessValue: if Value <> '' then
                               EXIF_BrightnessValue := StrToFloat(value);
      _EXIF_ExposureBiasValue: EXIF_ExposureBiasValue := StrToFloat(value);
      _EXIF_MaxApertureValue: EXIF_MaxApertureValue_Str := value;

      // Distance to focus point, unit is meter.
      _EXIF_SubjectDistance:
        begin
          value := StripSuffix(value, ' m');
          EXIF_SubjectDistance := StrToFloat(value);
        end;

      {
      Exposure metering method:
      0 means unknown
      1 average
      2 center weighted average
      3 spot
      4 multi-spot
      5 multi-segment
      6 partial
      255 other.
      }
      _EXIF_MeteringMode:
        begin
          if SameText(value, s_average) then
            EXIF_MeteringMode := 1
          else
          if SameText(value, s_CenterWeightedAverage) then
            EXIF_MeteringMode := 2
          else
          if SameText(value, s_Spot) then
            EXIF_MeteringMode := 3
          else
          if SameText(value, s_MultiSpot) then
            EXIF_MeteringMode := 4
          else
          if SameText(value, s_MultiSegment) then
            EXIF_MeteringMode := 5
          else
          if SameText(value, s_partial) then
            EXIF_MeteringMode := 6;
        end;

      {
      Light source, actually this means white balance setting:
      0 means unknown
      1 daylight
      2 fluorescent
      3 tungsten
      10 flash
      17 standard light A
      18 standard light B
      19 standard light C
      20 D55
      21 D65
      22 D75
      255 other.
      }
      _EXIF_LightSource:
        begin
          if SameText(value, s_daylight) then
            EXIF_LightSource := 1
          else
          if SameText(value, s_fluorescent) then
            EXIF_LightSource := 2
          else
          if SameText(value, s_tungsten) then
            EXIF_LightSource := 3
          else
          if SameText(value, s_flash) then
            EXIF_LightSource := 10
          else
          if SameText(value, s_standardLightA) then
            EXIF_LightSource := 17
          else
          if SameText(value, s_standardLightB) then
            EXIF_LightSource := 18
          else
          if SameText(value, s_standardLightC) then
            EXIF_LightSource := 19
          else
          if SameText(value, s_D55) then
            EXIF_LightSource := 20
          else
          if SameText(value, s_D65) then
            EXIF_LightSource := 21
          else
          if SameText(value, s_D75) then
            EXIF_LightSource := 22;
        end;

      {
      EXIF_Flash:
      0 means flash did not fire
      1 flash fired
      5 flash fired but strobe return light not detected
      7 flash fired and strobe return light detected
      }
      _EXIF_Flash:
        begin
          if SameText(value, s_FlashDidNotFire) then
            EXIF_Flash := 0
          else
          if SameText(value, s_flashFired) then
            EXIF_Flash := 1
          else
          if SameText(value, s_flashFiredNoStrobeLight) then
            EXIF_Flash := 5
          else
          if SameText(value, s_flashFiredStrobeLight) then
            EXIF_Flash := 7;
        end;

      // Focal length of lens used to take image. Unit is millimeter.
      _EXIF_FocalLength:
        begin
          value := StripSuffix(value, ' mm');
          EXIF_FocalLength := StrToFloat(value);
        end;

      _EXIF_FlashPixVersion: EXIF_FlashPixVersion := Value;

      // Defines Color Space: DCF image must use sRGB color space so value is always '1'. If the picture uses the other color space, value is '65535' : Uncalibrated.
      _EXIF_ColorSpace:
        begin
          if SameText(value, s_RGB) then
            EXIF_ColorSpace := 1
          else
          if SameText(value, s_Uncalibrated) then
            EXIF_ColorSpace := 65535;
        end;

      _EXIF_ExifImageWidth: EXIF_ExifImageWidth := StrToInt(value);
      _EXIF_ExifImageHeight: EXIF_ExifImageHeight := StrToInt(value);

      // If this digicam can record audio data with image, shows name of audio data.
      _EXIF_RelatedSoundFile: EXIF_RelatedSoundFile := value;

      {
      Unit of FocalPlaneXResoluton/FocalPlaneYResolution:
      1 means no-unit
      2 inch
      3 centimeter.
      Note : Some of Fujifilm's digicam(e.g.FX2700, FX2900, Finepix4700Z/40i etc) uses value '3' so it must be 'centimeter', but it seems that they use a '8.3mm?'(1/3in.?) to their ResolutionUnit. Fuji's BUG? Finepix4900Z has been changed to use value '2' but it doesn't match to actual value also.
      }
      _EXIF_FocalPlaneXResolution:
        begin
          if pos(uppercase(s_Inch), uppercase(value)) > 0 then
            EXIF_FocalPlaneResolutionUnit := 2
          else
          if pos(uppercase(s_CM), uppercase(value)) > 0 then
            EXIF_FocalPlaneResolutionUnit := 3;

          Value := StripSuffix(value, s_PerInch);
          Value := StripSuffix(value, s_PerCM);

          EXIF_FocalPlaneXResolution := StrToFloat(value);
        end;

      _EXIF_FocalPlaneYResolution:
        begin
          if pos(uppercase(s_Inch), uppercase(value)) > 0 then
            EXIF_FocalPlaneResolutionUnit := 2
          else
          if pos(uppercase(s_CM), uppercase(value)) > 0 then
            EXIF_FocalPlaneResolutionUnit := 3;

          Value := StripSuffix(value, s_PerInch);
          Value := StripSuffix(value, s_PerCM);

          EXIF_FocalPlaneYResolution := StrToFloat(value);
        end;

      // EXIF_ExposureIndex: Same as ISOSpeedRatings(0x8827) but data type is unsigned rational. Only Kodak's digicam uses this tag instead of ISOSpeedRating.
      _EXIF_ExposureIndex: EXIF_ExposureIndex := StrToFloat(value);

      {
      When image format is YCbCr and uses 'Subsampling' (cropping of chroma data, all the digicam do that), defines the chroma sample point of subsampling pixel array.
      1 means the center of pixel array
      2 means the datum point.
      }
      _EXIF_YCbCrPositioning:
        begin
          if SameText(value, s_Centered) then
            EXIF_YcbCrPositioning := 1
          else
          if SameText(value, s_DataPoint) then
            EXIF_YcbCrPositioning := 2;
        end;

      _EXIF_GPSSatellites: EXIF_GPSSatellites := Value;
      _EXIF_GPSVersionID: EXIF_GPSVersionID := Value;

      _EXIF_Artist: EXIF_Artist := Value;

      _EXIF_XPTitle: EXIF_XPTitle := Value;
      _EXIF_XPComment: EXIF_XPComment := Value;
      _EXIF_XPAuthor: EXIF_XPAuthor := Value;
      _EXIF_XPKeywords: EXIF_XPKeywords := Value;
      _EXIF_XPSubject: EXIF_XPSubject := Value;
      _EXIF_XPRating: EXIF_XPRating := StrToIntDef(Value, -1);

      _EXIF_InteropVersion : EXIF_InteropVersion := AnsiString(Value);

      {
      TO-DO: ADD WRITE SUPPORT :

      _EXIF_SensingMethod
      _EXIF_FileSource
      _EXIF_SceneType
      _EXIF_ExposureMode
      _EXIF_WhiteBalance
      _EXIF_DigitalZoomRatio
      _EXIF_FocalLengthIn35mmFilm
      _EXIF_SceneCaptureType
      _EXIF_GainControl
      _EXIF_Contrast
      _EXIF_Saturation
      _EXIF_Sharpness
      _EXIF_SubjectDistanceRange
      _EXIF_GPSLatitude
      _EXIF_GPSLongitude
      _EXIF_GPSAltitude
      _EXIF_GPSImageDirection
      _EXIF_GPSTrack
      _EXIF_GPSSpeed
      _EXIF_GPSDateAndTime
       }
    end;

  except
    // ERROR
  end;
end;



{!!
<FS>TIOParamsValsHelper.EXIF_WriteToStrings

<FM>Declaration<FC>
function TIOParamsValsHelper.EXIF_WriteToStrings(Dest : TStrings) : Boolean;

<FM>Description<FN>
Fills a TStrings object with the EXIF properties of the current image in the format:
Description: Value

Result is true if the image has any EXIF fields.

<FM>Example<FC>
// Output the EXIF properties of the current image to a TMemo
ImageEnView1.IO.EXIF_WriteToStrings( memo1.Lines );
!!}
function TIOParamsValsHelper.EXIF_WriteToStrings(Dest : TStrings) : Boolean;
begin
  Result := _ReadFromEXIF_Params(Dest, Self);
end;

{!!
<FS>TIOParamsValsHelper.IPTC_WriteToStrings

<FM>Declaration<FC>
function TIOParamsValsHelper.IPTC_WriteToStrings(Dest : TStrings) : Boolean;

<FM>Description<FN>
Fills a TStrings object with the IPTC properties of the current image in the format:
Description: Value

Result is true if the image has any IPTC fields.

<FM>Example<FC>
// Output the IPTC properties of the current image to a TMemo
ImageEnView1.IO.IPTC_WriteToStrings( memo1.Lines );
!!}
function TIOParamsValsHelper.IPTC_WriteToStrings(Dest : TStrings) : Boolean;
begin
  Result := _ReadFromIPTC_Params(Dest, Self);
end;

{!!
<FS>TIOParamsValsHelper.DICOM_WriteToStrings

<FM>Declaration<FC>
function TIOParamsValsHelper.DICOM_WriteToStrings(Dest : TStrings; TagInclude: <A TIEDicomInclude>) : Boolean;

<FM>Description<FN>
Fills a TStrings object with the Dicom properties of the current image in the format:
Description: Value

<FC>TagInclude<FN> allows you to filter the types of tags returned.
Result is true if the image has any Dicom fields.

<FM>Examples<FC>
// Output the Dicom properties of the current image to a TMemo (all types)
ImageEnView1.IO.DICOM_WriteToStrings( memo1.Lines, [ diProprietary, diDeprecated, diChildTags, diUnknown ] );

// Export the dicom list of the current image to a file (exclude deprecated and unknown tags)
aStringList := TStringList.create;
ImageEnView1.IO.DICOM_WriteToStrings( aStringList, [ diProprietary, diChildTags ] );
aStringList.SaveToFile('D:\DicomList.txt');
aStringList.Free;
!!}
function TIOParamsValsHelper.DICOM_WriteToStrings(Dest : TStrings; TagInclude: TIEDicomInclude) : Boolean;
begin
  Result := _ReadFromDicom_Params(Dest, Self, TagInclude);
end;

{$endif}


// GENERAL EXIF METHODS

{!!
<FS>ExifCompatibleFile

<FM>Declaration<FC>
function ExifCompatibleFile(const sFilename: string): boolean;

<FM>Description<FN>
Returns true if the specified filename has an extension of a file type that supports EXIF reading (JPEG, TIFF or Camera Raw).

<FM>Examples<FC>
// Enable button if the selected file may contain exif fields
btnShowExif.Enabled := ExifCompatibleFile( MyFileListBox.Filename );
!!}
// return true if the specified filename can support writing of EXIF Fields
function ExifCompatibleFile(const sFilename: string): boolean;
begin
  Result := IEFilenameInExtensions(sFilename, EXIF_COMPATIBLE_EXTENSIONS);
end;

{!!
<FS>ReadExifCameraFieldsFromFile

<FM>Declaration<FC>
function ReadExifCameraFieldsFromFile(const sFilename : string;
                                      out sCameraModel : string;
                                      out sExposureTime : string;
                                      out sFlashMode : string
                                      ) : boolean;

<FM>Description<FN>
Retrieves camera related EXIF data from a file. The following properties are returned:
- <A TIOParamsValsHelper.EXIF_Camera_Str>
- <A TIOParamsValsHelper.EXIF_ExposureTime_Str>
- <A TIOParamsVals.EXIF_Flash>

Returns false if <FC>sFilename<FN> is not a file type that supports EXIF.

<FM>Examples<FC>
// Show Camera data for selected file
if ReadExifCameraFieldsFromFile( MyFileListBox.Filename, sCameraModel, sExposureTime, sFlashMode ) then
begin
  lblCameraModel .Caption := sCameraModel;
  lblExposureTime.Caption := sExposureTime;
  lblFlashMode   .Caption := sFlashMode;
end
else
begin
  lblCameraModel .Caption := '';
  lblExposureTime.Caption := '';
  lblFlashMode   .Caption := '';
end;
!!}
// Formerly GetUsefulExifFields
{$IFDEF Delphi2005orNewer}
function ReadExifCameraFieldsFromFile(const sFilename : string;
                                      out sCameraModel : string;
                                      out sExposureTime : string;
                                      out sFlashMode : string
                                      ) : boolean;
var
  aImageEnIO: TImageEnIO;
begin
  result := false;
  if ExifCompatibleFile(sFilename) then
  try
    aImageEnIO := TImageEnIO.create(nil);
    try
      aImageEnIO.ParamsFromFile(sFilename);
      if aImageEnIO.params.EXIF_HasEXIFData then
      begin
        sCameraModel  := aImageEnIO.Params.EXIF_Camera_Str;
        sExposureTime := aImageEnIO.Params.EXIF_ExposureTime_Str;
        sFlashMode := FlashModeToString(aImageEnIO.Params.EXIF_Flash);

        result := (sCameraModel <> '') or (sExposureTime <> '');
      end;

    finally
      aImageEnIO.Free;
    end;

  except
    // ERROR
  end;
end;
{$ENDIF}


function _ReadExifGPSFieldsFromFile(const sFilename : string;
                                   out GPSLatitude: Double;
                                   out GPSLongitude: Double;
                                   out sGPSLatitude: string;
                                   out sGPSLongitude: string
                                   ) : boolean;
var
  aImageEnIO: TImageEnIO;
begin
  result := false;
  GPSLatitude := 0;
  GPSLongitude := 0;
  sGPSLatitude  := '';
  sGPSLongitude := '';

  if ExifCompatibleFile(sFilename) then
  try
    aImageEnIO := TImageEnIO.create(nil);
    try
      aImageEnIO.ParamsFromFile(sFilename);
      if aImageEnIO.params.EXIF_HasEXIFData then
      begin
        GPSLatitude  := aImageEnIO.Params.EXIF_GPSLatitude;
        GPSLongitude := aImageEnIO.Params.EXIF_GPSLongitude;
        Result := (GPSLatitude <> 0) or (GPSLongitude <> 0);
        if Result then
        begin
          sGPSLatitude  := aImageEnIO.Params.EXIF_GPSLatitude_Str;
          sGPSLongitude := aImageEnIO.Params.EXIF_GPSLongitude_Str;
        end;
      end;
    finally
      aImageEnIO.Free;
    end;

  except
    // ERROR
  end;
end;

{!!
<FS>ReadExifGPSFieldsFromFile

<FM>Declaration<FC>
function ReadExifGPSFieldsFromFile(const sFilename : string;
                                   out GPSLatitude: Double;
                                   out GPSLongitude: Double
                                   ) : boolean; overload;

function ReadExifGPSFieldsFromFile(const sFilename : string;
                                   out sGPSLatitude: string;
                                   out sGPSLongitude: string
                                   ) : boolean; overload;

<FM>Description<FN>
Retrieves GPS location data from a file. The following properties are returned:
- <A TIOParamsVals.EXIF_GPSLatitude>
- <A TIOParamsVals.EXIF_GPSLongitude>

Returns false if <FC>sFilename<FN> does not contain any GPS data.

<FM>Examples<FC>
// Show location on map for selected file
if ReadExifGPSFieldsFromFile( MyFileListBox.Filename, GPSLatitude, GPSLongitude ) then
  Map.NavigateTo( GPSLatitude, GPSLongitude );

// Show GPS location for selected file
if ReadExifGPSFieldsFromFile( MyFileListBox.Filename, sGPSLatitude, sGPSLongitude ) then
begin
  lblGPSLatitude .Caption := sGPSLatitude;
  lblGPSLongitude.Caption := sGPSLongitude;
end
else
begin
  lblGPSLatitude .Caption := '';
  lblGPSLongitude.Caption := '';
end;
!!}
// Formerly ExifGPSData
function ReadExifGPSFieldsFromFile(const sFilename : string;
                                   out GPSLatitude: Double;
                                   out GPSLongitude: Double
                                   ) : boolean;
var
  sGPSLatitude: string;
  sGPSLongitude: string;
begin
  Result := _ReadExifGPSFieldsFromFile( sFilename, GPSLatitude, GPSLongitude, sGPSLatitude, sGPSLongitude);
end;

function ReadExifGPSFieldsFromFile(const sFilename : string;
                                   out sGPSLatitude: string;
                                   out sGPSLongitude: string
                                   ) : boolean;
var
  GPSLatitude: Double;
  GPSLongitude: Double;
begin
  Result := _ReadExifGPSFieldsFromFile( sFilename, GPSLatitude, GPSLongitude, sGPSLatitude, sGPSLongitude);
end;


// EXIF TAG ROUTINES

function ExifFieldToTag(const sExifField : string) : string;
begin
  Result := Exif_Tag_Prefix + StringReplace(sExifField, ' ', '-', [rfReplaceAll]) + Exif_Tag_Suffix;
end;


{!!
<FS>ContainsExifTag

<FM>Declaration<FC>
function ContainsExifTag(const sText: string): Boolean;

<FM>Description<FN>
Returns true is sText contains an exif tag, such as %EXIF-User-Comment%

<FB>EXIF TAGS<FN>
Exif tags allow creation of generic text blocks where the data within the text is replaced with EXIF data (in a similar way to a mail merge). For example, a user may insert the exif tag, %EXIF-User-Comment%, you can then call <A ReplaceExifTags> to replace the tag with the actually value of the <A TIOParamsVals.EXIF_UserComment> field.

BEFORE:
Description: %EXIF-User-Comment%
Camera: %EXIF-Camera-Make%
Shutter Speed: %EXIF-Shutter-Speed%

AFTER (Example):
Description: Eiffel Tower
Camera: Nikon
Shutter Speed: 1/200 second

<FM>Examples<FC>
// Replace the exif tags in memo with the actual values of the current image
if ContainsExifTag( Memo1.Text ) then
  Memo1.Text := ReplaceExifTags( Memo1.Text, ImageEnView1.IO.Params );
!!}
function ContainsExifTag(const sText: string): Boolean;
begin
  Result := Pos(uppercase(Exif_Tag_Prefix), uppercase(sText)) > 0;
end;


{!!
<FS>GetExifTagList

<FM>Declaration<FC>
procedure GetExifTagList(ssDest: TStrings);

<FM>Description<FN>
Fill <FC>ssDest<FN> with all available exif tags, such as %EXIF-User-Comment%

<FB>EXIF TAGS<FN>
Exif tags allow creation of generic text blocks where the data within the text is replaced with EXIF data (in a similar way to a mail merge). For example, a user may insert the exif tag, %EXIF-User-Comment%, you can then call <A ReplaceExifTags> to replace the tag with the actually value of the <A TIOParamsVals.EXIF_UserComment> field.

BEFORE:
Description: %EXIF-User-Comment%
Camera: %EXIF-Camera-Make%
Shutter Speed: %EXIF-Shutter-Speed%

AFTER (Example):
Description: Eiffel Tower
Camera: Nikon
Shutter Speed: 1/200 second

<FM>Examples<FC>
// Fill our tag selector with all available Exif tags
GetExifTagList( lbxExifTagList.Items );
!!}
procedure GetExifTagList(ssDest: TStrings);
var
  i: integer;
begin
  // fill the strings with the EXIF tag names
  for i := Low(ExifTags) to High(ExifTags) do
    ssDest.Add( ExifFieldToTag(ExifTags[ i ].Desc) );
end;

{$IFDEF Delphi2005orNewer}
function _ReplaceExifTags(const sText: string; EXIFSource : TIOParamsVals; bValid, bSample : Boolean): string;
var
  i: Integer;
  sValue: string;
begin
  // NOTE: May be slow with large text blocks!
  for i := Low(ExifTags) to high(ExifTags) do
  begin 
    if bSample then
      sValue := 'Sample ' + ExifTags[ i ].Desc
    else
    if bValid then
      sValue := EXIFSource.EXIF_AsStr[ i ]
    else
      sValue := '';

    Result := StringReplace(result, ExifFieldToTag(ExifTags[ i ].Desc), sValue, [rfReplaceAll, rfIgnoreCase]);
  end;
end;
{$ENDIF}

{!!
<FS>ReplaceExifTags

<FM>Declaration<FC>
function ReplaceExifTags(const sText: string; EXIFSource : TIOParamsVals): string; overload;
function ReplaceExifTags(const sText: string; const sEXIFSourceFilename : string): string; overload;

<FM>Description<FN>
Replace all Exif tags found in <FC>sText<FN> with the actual EXIF properties of the current image, or an image loaded from file.

Note: Specify sEXIFSourceFilename as <FC>Example_File_Only<FN> to show example properties to the user (e.g. instead of reading the <A TIOParamsVals.EXIF_UserComment> field from file, it will insert 'Sample User Comment')

<FB>EXIF TAGS<FN>
Exif tags allow creation of generic text blocks where the data within the text is replaced with EXIF data (in a similar way to a mail merge). For example, a user may insert the exif tag, %EXIF-User-Comment%, you can then call <A ReplaceExifTags> to replace the tag with the actually value of the <A TIOParamsVals.EXIF_UserComment> field.

BEFORE:
Description: %EXIF-User-Comment%
Camera: %EXIF-Camera-Make%
Shutter Speed: %EXIF-Shutter-Speed%

AFTER (Example):
Description: Eiffel Tower
Camera: Nikon
Shutter Speed: 1/200 second

<FM>Examples<FC>
// Replace the exif tags in memo with the actual values of the current image
if ContainsExifTag( Memo1.Text ) then
  Memo1.Text := ReplaceExifTags( Memo1.Text, ImageEnView1.IO.Params );

// Show sample output
memSample.Text := ReplaceExifTags( 'Camera: %EXIF-Camera-Make%'#13#10'Shutter Speed: %EXIF-Shutter-Speed%', Example_File_Only );
(*
Which outputs:
Camera: Sample Camera Make
Shutter Speed: Sample Shutter Speed
*)
!!}
{$IFDEF Delphi2005orNewer}
function ReplaceExifTags(const sText: string; EXIFSource : TIOParamsVals): string;
var
  bValid: Boolean;
begin
  Result := sText;

  // Anything to do?
  if ContainsExifTag(sText) then
  begin
    bValid := EXIFSource.EXIF_HasEXIFData;
    Result := _ReplaceExifTags(sText, EXIFSource, bValid, False);
  end;
end;
{$ENDIF}

{$IFDEF Delphi2005orNewer}
function ReplaceExifTags(const sText: string; const sEXIFSourceFilename : string): string;
var
  bValid: Boolean;
  aImageEnIO: TImageEnIO;
  bSampleOnly: Boolean;
begin
  Result := sText;

  // Anything to do?
  if ContainsExifTag(sText) = False then
    Exit;

  bSampleOnly := SameText(sEXIFSourceFilename, Example_File_Only);
  aImageEnIO := TImageEnIO.create(nil);
  try
    aImageEnIO.ParamsFromFile(sEXIFSourceFilename);

    bValid := False;
    if EXIFCompatibleFile(sEXIFSourceFilename) then
    try
      AImageEnIO.ParamsFromFile(sEXIFSourceFilename);
      bValid := AImageEnIO.Params.EXIF_HasEXIFData;
    except
      // ERROR
    end;

    Result := _ReplaceExifTags(sText, AImageEnIO.Params, bValid, bSampleOnly);

  finally
    aImageEnIO.Free;
  end;
end;
{$ENDIF}


// GENERAL IPTC METHODS

{!!
<FS>IPTCCompatibleFile

<FM>Declaration<FC>
function IPTCCompatibleFile(const sFilename: string): boolean;

<FM>Description<FN>
Returns true if the specified filename has an extension of a file type that supports IPTC reading (i.e. JPEG or TIFF).

<FM>Examples<FC>
// Enable button if the selected file may contain IPTC
btnShowIPTC.Enabled := IPTCCompatibleFile( MyFileListBox.Filename );
!!}
function IPTCCompatibleFile(sFilename : string) : boolean;
begin
  result := IEFilenameInExtensions(sFilename, IPTC_COMPATIBLE_EXTENSIONS);
end;


{!!
<FS>ReadIPTCDescriptionAndKeywordsFromFile

<FM>Declaration<FC>
function ReadIPTCDescriptionAndKeywordsFromFile(const sFilename: string;
                                                out sDescription: string;
                                                ssKeywords: TStrings = nil) : Boolean;

<FM>Description<FN>
Retrieves the description and keywords from the IPTC data of a file. The following records are returned:
- PhotoShop_IPTC_Records/IPTC_PS_Caption
- PhotoShop_IPTC_Records/IPTC_PS_Keywords

Returns false if the file does not contain an IPTC description or keywords.

Note: ssKeywords can be nil

<FM>Example<FC>
// Show keyword and description of selected file
if ReadIPTCCameraFieldsFromFile( MyFileListBox.Filename, sDescription, ssKeywords ) then
begin
  lblDescription .Caption := sDescription;
  lblKeywords    .Caption := ssKeywords.CommaText;
end
else
begin
  lblDescription .Caption := '';
  lblKeywords    .Caption := '';
end;
!!}
function ReadIPTCDescriptionAndKeywordsFromFile(const sFilename: string;
                                                out sDescription: string;
                                                ssKeywords: TStrings = nil) : Boolean;
var
  aImageEnIO: TImageEnIO;
begin
  result := false;
  sDescription := '';

  if IPTCCompatibleFile(sFilename) then
  try
    aImageEnIO := TImageEnIO.create(nil);
    try
      aImageEnIO.ParamsFromFile(sFilename);

      // DESCRIPTION FIELD
      sDescription := aImageEnIO.Params.ReadIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Caption);
      if sDescription <> '' then
        Result := True;

      // KEYWORD FIELD (ssKeywords can be nil)
      if assigned(ssKeywords) then
      begin
        // get the keywords
        aImageEnIO.Params.ReadIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Caption, ssKeywords);
        if ssKeywords.Text <> '' then
          Result := True;
      end;

    finally
      aImageEnIO.Free;
    end;

  except
    // ERROR
  end;
end;


{!!
<FS>WriteIPTCDescriptionAndKeywordsToFile

<FM>Declaration<FC>
procedure WriteIPTCDescriptionAndKeywordsToFile(const sFilename: string;
                                                sDescription: string;
                                                ssKeywords: TStrings = nil);

<FM>Description<FN>
Saves a description and keywords to the IPTC data of a file. The following records are used:
- PhotoShop_IPTC_Records/IPTC_PS_Caption
- PhotoShop_IPTC_Records/IPTC_PS_Keywords

Notes:
- Specify the <FC>SKIP_DESCRIPTION<FN> const to avoid updating the description
- Pass ssKeywords as nil to avoid updating the keywords

<FM>Examples<FC>
// Save description and keywords to a file
WriteIPTCDescriptionAndKeywordsToFile( 'D:\MyImage.jpeg', edtDescription.Text, lbxKeywords.Items );

// Save description to a file
WriteIPTCDescriptionAndKeywordsToFile( 'D:\MyImage.jpeg', edtDescription.Text, nil );

// Save keywords to a file
WriteIPTCDescriptionAndKeywordsToFile( 'D:\MyImage.jpeg', SKIP_DESCRIPTION, lbxKeywords.Items );
!!}
// Formerly WriteIPTCDescriptionAndKeywords
procedure WriteIPTCDescriptionAndKeywordsToFile(const sFilename: string;
                                                sDescription: string;
                                                ssKeywords: TStrings = nil);
var
  aImageEnIO: TImageEnIO;
  dFileDate : TDateTime;
  bCanUpdateJPEG: Boolean;
begin
  aImageEnIO := TImageEnIO.create(nil);
  try
    if FileExists(sFilename) = False then
      raise EIEException.create('Cannot find file to update: ' + sFilename);

    bCanUpdateJPEG := IEFilenameInExtensions(sFilename, '*.jpeg;*.jpg;*.jpe;');
    if bCanUpdateJPEG then
      aImageEnIO.ParamsFromFile(sFilename)
    else
      aImageEnIO.LoadFromFile(sFilename);

    // DESCRIPTION
    if sDescription <> SKIP_DESCRIPTION then
      aImageEnIO.Params.WriteIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Caption, sDescription);

    // KEYWORDS
    if Assigned(ssKeywords) then
      aImageEnIO.Params.WriteIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Keywords, ssKeywords);

    {$WARNINGS OFF} // FileAge is deprecated
    dFileDate := 0;
    if Maintain_File_Dates_On_Meta_Write then
      dFileDate := FileDateToDateTime(FileAge(sFileName));
    {$WARNINGS ON}

    // Can write IPTC data?
    if (aImageEnIO.Params.FileType in [ioJPEG, ioTIFF]) = False then
      raise EIEException.create('File format does not support IPTC');

    if bCanUpdateJPEG then
      aImageEnIO.InjectJpegIPTC(sFileName)
    else
      aImageEnIO.SaveToFile(sFilename);

    if Maintain_File_Dates_On_Meta_Write and (dFileDate <> 0) then
      IEFileSetDate(sFileName, dFileDate);

  finally
    aImageEnIO.Free;
  end;
end;



{!!
<FS>iexMetaHelpers

<FN>iexMetaHelpers.pas provides helper functions for working with the EXIF and IPTC meta data in compatible files (e.g. JPEG and TIFF files). It includes an array of all common EXIF fields and provides methods for reading and writing fields by ID, as well as displaying and saving data in a TStringGrid or TListView. Also supports DICOM meta data.

Simply add iexMetaHelpers to your uses clause to access the new methods

<FM>IOPARAMVALS EXIF HELPER FUNCTIONS<FN>
Provide a string formatted access (human-readable) to some <L TIOParamsVals>EXIF properties</L> (that are complex to interpret):
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_AsStr></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_CanWriteEXIFData></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_FieldDescription></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_ApertureValue_Str></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_Camera_Str></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_ExposureTime_Str></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_FNumber_Str></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_MaxApertureValue_Str></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_ShutterSpeedValue_Str></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_XResolution_Str></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_YResolution_Str></C> </R>
</TABLE>

<FM>OTHER IOPARAMVALS HELPER FUNCTIONS<FN>
Provide a string formatted access (human-readable) to some <L TIOParamsVals>EXIF properties</L> (that are complex to interpret):
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.EXIF_WriteToStrings></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.IPTC_WriteToStrings></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsValsHelper.Dicom_WriteToStrings></C> </R>
</TABLE>

<FM>STRINGGRID HELPER FUNCTIONS<FN>
Helper functions to output EXIF and IPTC fields to a TStringGrid:
<TABLE2>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.NewGridForExif></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.ReadGridFromExif></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.WriteGridToExif></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.NewGridForIPTC></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.ReadGridFromIPTC></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.WriteGridToIPTC></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.ReadGridFromDicom></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.InitializeGrid></C> </R>
<R> <C_IMG_METHOD> <C><A TStringGridHelper.ClearGridFields></C> </R>
</TABLE>

<FM>LISTVIEW HELPER FUNCTIONS<FN>
Helper functions to output EXIF and IPTC fields to a TListView:
<TABLE2>
<R> <C_IMG_METHOD> <C><A TListViewHelper.NewListForExif></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.ReadListFromExif></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.WriteListToExif></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.NewListForIPTC></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.ReadListFromIPTC></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.WriteListToIPTC></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.ReadListFromDicom></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.InitializeList></C> </R>
<R> <C_IMG_METHOD> <C><A TListViewHelper.ClearListFields></C> </R>
</TABLE>

<FM>GENERAL EXIF METHODS<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A EXIFCompatibleFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ReadEXIFCameraFieldsFromFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ReadEXIFGPSFieldsFromFile></C> </R>
</TABLE>

<FM>GENERAL IPTC METHODS<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IPTCCompatibleFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ReadIPTCDescriptionAndKeywordsFromFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A WriteIPTCDescriptionAndKeywordsToFile></C> </R>
</TABLE>

<FM>EXIF TAG METHODS<FN>
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A ContainsExifTag></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A GetExifTagList></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ReplaceExifTags></C> </R>
</TABLE>

* Note: Delphi/C++ 2005 or newer is required to use helper classes
!!}

end.

