{------------------------------------------------------------------------------}
{                                                                              }
{  ImageEn Helper functions                                                    }
{  Requires: Delphi 2005 or newer                                              }
{                                                                              }
{  Nigel Cross                                                                 }
{  Xequte Software                                                             }
{  nigel@xequte.com                                                            }
{  http://www.xequte.com                                                       }
{                                                                              }
{  © Xequte Software 2011-2014                                                 }
{                                                                              }
{  4/4/2013                                                                    }
{  - Added TIEBitmapHelper = class helper for TIEBitmap along with help        }
{  William Miller, Adirondack Software & Graphics                              }
{                                                                              }
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
File version 1010
*)

unit iexHelperFunctions;

interface   

{$I ie.inc}


// Enable to include TImageEnIO helpers for loading and saving to blob fields
{.$Define IE_DB_Helpers}

uses
  Windows, Graphics, Classes, 
  {$ifdef IEHASTYPES} Types, {$endif}
  {$ifdef IE_DB_Helpers} Db, DbTables, {$endif}
  ImageEnProc, hyiedefs, ImageEnIO, hyieutils;

    

type
{!!
<FS>TLightOrigin

<FM>Declaration<FC>
}
  TLightOrigin = (_loTopLeft, _loTopRight, _loBottomLeft, _loBottomRight);
{!!}

  {$IFDEF Delphi2005orNewer}
  // TImageEnIO Helper Functions
  TImageEnIOHelper = class helper for TImageEnIO
  private
    function ImageSize : TPoint;
    function IsEmpty : Boolean;   
    function _LoadFromFileEx(const sFilename: string;
                             iMaxX, iMaxY: integer;
                             bAutoAdjustOrientation: Boolean = False;
                             iImageIndex : Integer = -1;
                             bUseFileExt : Boolean = true): Boolean;
  public
    function Reload : Boolean;

    function LoadFromFileEx(const sFilename: string;
                            bAutoAdjustOrientation: Boolean;
                            iImageIndex : Integer = -1): Boolean;

    function LoadFromFileFast(const sFilename: string;
                              iMaxX, iMaxY: integer;
                              bAutoAdjustOrientation: Boolean = False
                              ): Boolean;

    function LoadFromFileJPEGFast(const sFilename: string;
                                  iMaxX, iMaxY: integer;
                                  var iFastScaleUsed: Integer;
                                  bAutoAdjustOrientation: Boolean = False
                                  ): Boolean;

    function LoadFromFileAutoEx(const sFilename: string;
                                bAutoAdjustOrientation: Boolean = False;
                                iImageIndex : Integer = -1): Boolean;

    {$IFDEF IEINCLUDERAWFORMATS}
    function LoadFromFileRawFast(const sFilename: string; iMaxX, iMaxY: integer): Boolean;
    {$ENDIF}

    function SaveToFileEx(const sFilename : string;
                          iJpegQuality: integer = 0): Boolean;

    function CreatePDFFromFileList(const sDestFilename : string;
                                   ssFileList : TStrings;
                                   const aPaperSize : TIOPDFPaperSize;
                                   const aCompression : TIOPDFCompression;
                                   const sTitle : string;
                                   const sAuthor : string;
                                   const bFailOnUnsupportedImage : Boolean = True;
                                   const bRaiseExceptions : Boolean = False
                                   ) : Boolean;

    function CreatePSFromFileList(const sDestFilename : string;
                                  ssFileList : TStrings;
                                  const aPaperSize : TIOPDFPaperSize;
                                  const aCompression : TIOPSCompression;
                                  const sTitle : string = '';
                                  const bFailOnUnsupportedImage : Boolean = True;
                                  const bRaiseExceptions : Boolean = False
                                  ) : Boolean;

    {$IFDEF IE_DB_Helpers}
    function LoadFromBlob(aField: TBlobField;
                          bAutoAdjustOrientation: Boolean = False;
                          iImageIndex : Integer = -1
                          ): Boolean;

    function LoadFromBlobFast(aField: TBlobField;
                              iMaxX, iMaxY: integer;
                              bAutoAdjustOrientation: Boolean = False;
                              iImageIndex : Integer = -1
                              ): Boolean;

    function SaveToBlob(aField : TBlobField;
                        aFileType : TIOFileType;
                        iJpegQuality: integer = 0): Boolean;
    {$ENDIF}

  end;
  {$ENDIF}


  {$IFDEF Delphi2005orNewer}
  // TBitmap Helper Functions
  TBitmapHelper = class helper for TBitmap
  private
    procedure ConvertTo24Bit;
  public                 
    procedure IEInitialize(iWidth, iHeight: Integer; ABackgroundColor : TColor = clNone);
    procedure IERotate(Angle: double; AntiAliasMode: TIEAntialiasMode = ierFast; BackgroundColor: TColor = -1);
    procedure IEFlip(Direction: TFlipDir);
    procedure IEResample(iNewWidth, iNewHeight: integer; QualityFilter: TResampleFilter = rfNone; bMaintainAspectRatio : Boolean = False);
    function IELoadFromFile(const sFilename : string): Boolean;
    function IELoadFromFileFast(const  sFilename: string;
                                iMaxX, iMaxY: integer;
                                bAutoAdjustOrientation: Boolean = False
                                ): Boolean;
    function IESaveToFile(const sFilename : string; iJPEGQuality : Integer): Boolean; 
    function LoadFromURL(const URL            : WideString;
                         const sProxyAddress  : WideString = '';
                         const sProxyUser     : WideString = '';
                         const sProxyPassword : WideString = ''
                         ): Boolean;


    function IELoadAsThumbnail(const sFilename: string;
                               iMaxX, iMaxY: integer;
                               bCanStretch: Boolean;
                               bAutoAdjustOrientation: Boolean = False;
                               QualityFilter : TResampleFilter = rfLanczos3;

                               bAddBorder: Boolean = False;
                               cBorderColor: TColor = clBlack;

                               bAddShadow: Boolean = False;
                               iBlurRadius : Integer = 4;
                               iShadowOffset : Integer = 4;
                               cShadowColor: TColor = clBlack;
                               cBGColor: TColor = clWhite
                               ) : Boolean;

    procedure IEConvertToThumbnail(iMaxX, iMaxY: integer;
                                   bCanStretch: Boolean;
                                   QualityFilter : TResampleFilter = rfLanczos3;

                                   bAddBorder: Boolean = False;
                                   cBorderColor: TColor = clBlack;

                                   bAddShadow: Boolean = False;
                                   iBlurRadius : Integer = 4;
                                   iShadowOffset : Integer = 4;
                                   cShadowColor: TColor = clBlack;
                                   cBGColor: TColor = clWhite);

    procedure IEAddSoftShadow(iBlurRadius: integer;
                              iShadowOffset : Integer;
                              cBGColor: TColor;
                              LightOrigin: TLightOrigin = _loTopLeft;
                              cShadowColor: TColor = clblack);

    procedure PrintImage(PrtCanvas: TCanvas = nil;
                         MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1;
                         VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER;
                         Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0;
                         GammaCorrection: double = 1;
                         SubsampleFilter: TResampleFilter = rfFastLinear);

  end;
  {$ENDIF}

  {$IFDEF Delphi2005orNewer}
  // TIEBitmap Helper Functions
  TIEBitmapHelper = class helper for TIEBitmap
  private
    procedure ConvertTo24Bit;
  public
    procedure IEInitialize(iWidth, iHeight: Integer; ABackgroundColor: TColor); overload;
    procedure IEInitialize(iWidth, iHeight: Integer; ABackgroundColor: TColor; ATransparent: Boolean); overload;
    function IELoadFromFile(const sFilename: string): Boolean;
    function IELoadFromFileFast(const sFilename: string; iMaxX, iMaxY: integer; bAutoAdjustOrientation: Boolean = False): Boolean;
    function IESaveToFile(const sFilename: string; iJPEGQuality: Integer): Boolean;
    function LoadFromURL(const URL            : WideString;
                         const sProxyAddress  : WideString = '';
                         const sProxyUser     : WideString = '';
                         const sProxyPassword : WideString = ''
                         ): Boolean;

    function IELoadAsThumbnail(const sFilename: string;
                               iMaxX, iMaxY: integer;
                               bCanStretch: Boolean;
                               bAutoAdjustOrientation: Boolean = False;
                               QualityFilter: TResampleFilter = rfLanczos3;

                               bAddBorder: Boolean = False;
                               cBorderColor: TColor = clBlack;

                               bAddShadow: Boolean = False;
                               iBlurRadius: Integer = 4;
                               iShadowOffset: Integer = 4;
                               cShadowColor: TColor = clBlack;
                               cBGColor: TColor = clWhite
                               ): Boolean;

    procedure IEConvertToThumbnail(iMaxX, iMaxY: integer;
                                   bCanStretch: Boolean;
                                   QualityFilter: TResampleFilter = rfLanczos3;

                                   bAddBorder: Boolean = False;
                                   cBorderColor: TColor = clBlack;

                                   bAddShadow: Boolean = False;
                                   iBlurRadius: Integer = 4;
                                   iShadowOffset: Integer = 4;
                                   cShadowColor: TColor = clBlack;
                                   cBGColor: TColor = clWhite);

    procedure IEAddSoftShadow(iBlurRadius: integer;
                              iShadowOffset: Integer;
                              cBGColor: TColor;
                              LightOrigin: TLightOrigin = _loTopLeft;
                              cShadowColor: TColor = clblack);


    procedure PrintImage(PrtCanvas: TCanvas = nil;
                         MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1;
                         VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER;
                         Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0;
                         GammaCorrection: double = 1;
                         SubsampleFilter: TResampleFilter = rfFastLinear);

  end;
  {$ENDIF}


  // FILE FUNCTIONS
                                                            
  {$IFDEF Delphi2005orNewer}
  function IEResampleImageFile(const sInFilename, sOutFilename: string;
                               iJpegQuality: Integer;
                               iMaxX: Integer;
                               iMaxY: Integer;
                               bCanStretch: Boolean = False;
                               QualityFilter: TResampleFilter = rfLanczos3;
                               bAutoAdjustOrientation: Boolean = False
                               ): Boolean;
  {$ENDIF}


  {$IFDEF Delphi2005orNewer}
  function IEConvertImageFile(const sInFilename, sOutFilename: string;
                              iJpegQuality: Integer;
                              bAutoAdjustOrientation: Boolean = False
                              ): Boolean;
  {$ENDIF}


  {$IFDEF Delphi2005orNewer}
  function IECreateThumbnailFromFile(const sInFilename: string;
                                     const sOutFilename: string;
                                     iMaxX, iMaxY: integer;
                                     bCanStretch: Boolean;
                                     iJPEGQuality: integer;
                                     bAutoAdjustOrientation: Boolean = False;
                                     QualityFilter : TResampleFilter = rfLanczos3;

                                     bAddBorder: Boolean = False;
                                     cBorderColor: TColor = clBlack;

                                     bAddShadow: Boolean = False;
                                     iBlurRadius : Integer = 4;
                                     iShadowOffset : Integer = 4;
                                     cShadowColor: TColor = clBlack;
                                     cBGColor: TColor = clWhite
                                     ): Boolean;    
  {$ENDIF}

  function GetFileDetails(const sFilename: string;
                          out iFileSizeBytes: Int64;
                          out dtCreationDate: TDateTime;
                          out dtModifiedDate: TDateTime
                          ): Boolean;

  function GetImageDetails(const sFilename: string;
                           out iWidth: Integer;
                           out iHeight: Integer;
                           out iBitsPerPixel: Integer
                           ): Boolean; overload;
  function GetImageDetails(const sFilename: string): TPoint; overload;

  function GetExifOrFileCreationDate(const sFilename: string;
                                     bReturnCreateDate: Boolean = true  {if true then the create date is returned if there is no exif date}
                                     ): TDateTime;

  function JPEGLosslessRotateFile(const sInFilename : WideString; const sOutFilename : WideString; iRotateAngle : integer) : Boolean; overload;
  function JPEGLosslessRotateFile(const sFilename : WideString; iRotateAngle : integer) : Boolean; overload;

  function JPEGLosslessFlipFile(const sInFilename : WideString; const sOutFilename : WideString; Direction: TFlipDir) : Boolean; overload;
  function JPEGLosslessFlipFile(const sFilename : WideString; Direction: TFlipDir) : Boolean; overload;

  {$IFDEF Delphi2005orNewer}
  function IERotateImageFile(const sFilename : string;
                             iJpegQuality : integer;
                             iRotateAngle : integer;
                             AntiAliasMode: TIEAntialiasMode = ierFast;
                             bCanUseLossless: Boolean = true;      // if true then a lossless rotate is used for JPEG images
                             cBackgroundColor: TColor = clWhite): Boolean; overload;
  function IERotateImageFile(const sInFilename, sOutFilename : string;
                             iJpegQuality : integer;
                             iRotateAngle : integer;
                             AntiAliasMode: TIEAntialiasMode = ierFast;
                             bCanUseLossless: Boolean = true;      // if true then a lossless rotate is used for JPEG images
                             cBackgroundColor: TColor = clWhite): Boolean; overload;    
  function IEFlipImageFile(const sFilename : string;
                           iJpegQuality : integer;
                           Direction: TFlipDir;
                           bCanUseLossless: Boolean = true      // if true then a lossless rotate is used for JPEG images
                           ): Boolean; overload;
  function IEFlipImageFile(const sInFilename, sOutFilename : string;
                           iJpegQuality : integer;
                           Direction: TFlipDir;
                           bCanUseLossless: Boolean = true      // if true then a lossless rotate is used for JPEG images
                           ): Boolean; overload;
  {$ENDIF}

  function IEReadCorrectOrientationOfImageFile(const sFilename : string; bConservativeChecking : Boolean = True) : Integer;
  function IEAutomaticallyRotateImageFile(const sFilename : string; bConservativeChecking : Boolean = True) : Integer;    
  function BitsPerPixelToStr(iBitsPerPixel: Integer) : string;
  function AngleToImageEnRotateAngle(iAngle: Integer): integer;
  function ShowTempHourglass: IUnknown;



implementation

uses
  SysUtils, ImageEnView, Controls, Math, Forms;



{$IFDEF Delphi2005orNewer}
{ TImageEnIOHelper }

function TImageEnIOHelper.ImageSize: TPoint;
begin
  if Assigned(IEBitmap) then
    result := Point(IEBitmap.width, IEBitmap.height)
  else
  if Assigned(Bitmap) then
    result := Point(Bitmap.width, Bitmap.height)
  else
    result := Point(0, 0);
end;

function TImageEnIOHelper.IsEmpty: Boolean;
begin
  Result := (ImageSize.X <= 2) or (ImageSize.Y <= 2);
end;


function TImageEnIOHelper._LoadFromFileEx(const sFilename: string;
                                          iMaxX, iMaxY: integer;
                                          bAutoAdjustOrientation: Boolean = False;
                                          iImageIndex : Integer = -1;
                                          bUseFileExt : Boolean = true): Boolean;
var
  bFastLoad: Boolean;
  iFastScaleUsed : Integer;
begin
  bFastLoad := (iMaxX > 0) and (iMaxY > 0);
  try
    {$IFDEF IEINCLUDERAWFORMATS}
    if bFastLoad and (IEFileIsOfFormat(sFilename, ioRaw) or IEFileIsOfFormat(sFilename, ioRawDLLPlugIn)) then
    begin
      Result := LoadFromFileRawFast(sFilename, iMaxX, iMaxY);
    end
    else
    {$ENDIF}
    if bFastLoad and IEFileIsOfFormat(sFilename, ioJPEG) then
    begin
      Result := LoadFromFileJPEGFast(sFilename, iMaxX, iMaxY, iFastScaleUsed, bAutoAdjustOrientation);
    end
    else
    begin
      // reset any fast loading variables
      Params.JPEG_Scale := ioJPEG_FullSize;      
      Params.JPEG_DCTMethod := ioJPEG_ISLOW;
      Params.EnableAdjustOrientation := bAutoAdjustOrientation;

      Params.RAW_HalfSize := false;
      Params.RAW_GetExifThumbnail := false;

      if iImageIndex <> -1 then      
        Params.ImageIndex := iImageIndex;

      if bUseFileExt then  
        LoadFromFile(sFilename)
      else
        LoadFromFileAuto(sFilename);
      Result := not (Aborting or IsEmpty);
    end;

  except
    result := false;
  end;
end;



{!!
<FS>TImageEnIO.LoadFromFileFast

<FM>Declaration<FC>
function LoadFromFileFast(const sFilename: string; iMaxX, iMaxY: integer; bAutoAdjustOrientation: Boolean = False): Boolean;

<FM>Description<FN>
Calls <A TImageEnIO.LoadFromFile> and allows you to specify a maximum size that you require an image so that it can be loaded as fast as possible, by using:
- <A TIOParamsVals.JPEG_Scale> for JPEGs
- <A TIOParamsVals.RAW_GetExifThumbnail> and <A TIOParamsVals.RAW_HalfSize> for Camera Raw files

You can also set <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images

Returns True if loading was successful, or False on error

<FM>Example<FC>
// Load the image specified in an open dialog as fast as possible, but bigger than the displaying TImageEnView
ImageEnView1.AutoShrink := True;
ImageEnView1.IO.LoadFromFileFast(OpenPictureDialog1.FileName, ImageEnView1.Width, ImageEnView1.Height)
!!}
function TImageEnIOHelper.LoadFromFileFast(const sFilename: string;
                                           iMaxX, iMaxY: integer;
                                           bAutoAdjustOrientation: Boolean = False): Boolean;
begin
  Result := _LoadFromFileEx(sFilename, iMaxX, iMaxY, bAutoAdjustOrientation);
end;


{!!
<FS>TImageEnIO.LoadFromFileEx

<FM>Declaration<FC>
function LoadFromFileEx(const sFilename: string; bAutoAdjustOrientation: Boolean; iImageIndex : Integer = -1): Boolean;

<FM>Description<FN>
Calls <A TImageEnIO.LoadFromFile>, optionally setting <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images and <A TIOParamsVals.ImageIndex> to display a specific image.
Returns True if loading was successful, or False on error

<FM>Example<FC>
// Load the third page of MyImage.tiff
ImageEnView1.IO.LoadFromFileEx('D:\MyImage.tiff', False, 2);
!!}
function TImageEnIOHelper.LoadFromFileEx(const sFilename: string;
                                         bAutoAdjustOrientation: Boolean;
                                         iImageIndex : Integer = -1): Boolean;
begin
  Result := _LoadFromFileEx(sFilename, -1, -1, bAutoAdjustOrientation, iImageIndex);
end;



{!!
<FS>TImageEnIO.LoadFromFileAutoEx

<FM>Declaration<FC>
function LoadFromFileAutoEx(const sFilename: string; bAutoAdjustOrientation: Boolean; iImageIndex : Integer = -1): Boolean;

<FM>Description<FN>
A modified version of <A TImageEnIO.LoadFromFileEx> that copes with image files with a incorrect extension (e.g. a GIF file named MyImage.jpg).

This function will attempt to load the file using its extension (using <A TImageEnIO.LoadFromFile>) but on failure will examine the message content to determine its type (i.e. falling back to <A TImageEnIO.LoadFromFileAuto>).

Optionally it will set <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images and <A TIOParamsVals.ImageIndex> to display a specific image.
Returns True if loading was successful, or False on error (i.e. the file could not be loaded even by examining its content).

<FM>Example<FC>
// Load MyImage.tiff (even if it is a JPEG or GIF)
ImageEnView1.IO.LoadFromFileAutoEx('D:\MyImage.tiff');
!!}
function TImageEnIOHelper.LoadFromFileAutoEx(const sFilename: string;
                                             bAutoAdjustOrientation: Boolean = False;
                                             iImageIndex : Integer = -1): Boolean;
begin
  Result := _LoadFromFileEx(sFilename, -1, -1, bAutoAdjustOrientation, iImageIndex);

  // Try loading without extension
  if not Result then
    Result := _LoadFromFileEx(sFilename, -1, -1, bAutoAdjustOrientation, iImageIndex, False);
end;

{!!
<FS>TImageEnIO.LoadFromFileJPEGFast

<FM>Declaration<FC>
function LoadFromFileJPEGFast(const sFilename: string;
                              iMaxX, iMaxY: integer;
                              var iFastScaleUsed: Integer;
                              bAutoAdjustOrientation: Boolean = False
                              ): Boolean;

<FM>Description<FN>
Calls <A TImageEnIO.LoadFromFileJpeg> to load a JPEG image and allows you to specify a maximum size that you require an image so that it can be loaded as fast as possible, by using <A TIOParamsVals.JPEG_Scale>.
You can also set <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images
Returns True if loading was successful, or False on error. iFastScaleUsed is set with the scaling that was used, i.e. 1 = loaded full size, 2 = loaded at half size, etc.

<FM>Example<FC>       
// Load the JPEG specified in an open dialog as fast as possible, but bigger than the displaying TImageEnView
ImageEnView1.AutoShrink := True;
ImageEnView1.IO.LoadFromFileJpegFast(OpenPictureDialog1.FileName, ImageEnView1.Width, ImageEnView1.Height, iFastScaleUsed)
!!}
function TImageEnIOHelper.LoadFromFileJPEGFast(const sFilename: string;
                                               iMaxX, iMaxY: integer;
                                               var iFastScaleUsed: Integer;
                                               bAutoAdjustOrientation: Boolean = False
                                               ): Boolean;
begin
  Result := True;
  try
    iFastScaleUsed := 1;

    if (iMaxX < 1) or (iMaxY < 1) then
      Params.JPEG_Scale := ioJPEG_FullSize
    else
    begin
      // Fast Loading
      Params.Width  := iMaxX;
      Params.Height := iMaxY;
      Params.JPEG_Scale := ioJPEG_AUTOCALC;

      // Is it a thumbnail?
      if iMaxX + iMaxY < 500 then
        Params.JPEG_DCTMethod := ioJPEG_IFAST;
    end;
    Params.EnableAdjustOrientation := bAutoAdjustOrientation;

    LoadFromFileJpeg(sFilename);

    if Aborting or IsEmpty then
      raise EIEException.create('Load Error');

    iFastScaleUsed := Params.Jpeg_Scale_Used;
  except
    result := false;
  end;
end;


{!!
<FS>TImageEnIO.Reload

<FM>Declaration<FC>
function Reload(const sFilename : Boolean;

<FM>Description<FN>
Reload the image.

<FM>Example<FC>
ImageEnView1.IO.Reload;
!!}
function TImageEnIOHelper.Reload : Boolean;
begin
  Result := True;
  try            
    if Params.FileName = '' then    
      raise EIEException.create('Nothing to Load');

    LoadFromFile(Params.FileName);

    if Aborting or IsEmpty then
      raise EIEException.create('Load Error');
  except
    result := false;
  end;
end;


{!!
<FS>TImageEnIO.LoadFromFileRawFast

<FM>Declaration<FC>
function LoadFromFileRawFast(const sFilename: string; iMaxX, iMaxY: integer): Boolean;

<FM>Description<FN>
Calls <A TImageEnIO.LoadFromFile> to load a camera Raw image and allows you to specify a maximum size that you require an image so that it can be loaded as fast as possible, by using <A TIOParamsVals.RAW_GetExifThumbnail> and <A TIOParamsVals.RAW_HalfSize> for Camera Raw files.
Returns True if loading was successful, or False on error

<FM>Example<FC>  
// Load the image specified in an open dialog as fast as possible, but bigger than the displaying TImageEnView
ImageEnView1.AutoShrink := True;
ImageEnView1.IO.LoadFromFileRawFast(OpenPictureDialog1.FileName, ImageEnView1.Width, ImageEnView1.Height)
!!}
{$IFDEF IEINCLUDERAWFORMATS}
function TImageEnIOHelper.LoadFromFileRawFast(const sFilename: string; iMaxX, iMaxY: integer): Boolean;
const
  // SOME APPROXIMATE VALUES TO OPTIMIZE RAW LOADING
  SMALLEST_RAW_THUMBNAIL_WIDTH  = 150;
  SMALLEST_RAW_THUMBNAIL_HEIGHT = 100;
  SMALLEST_RAW_IMAGE_WIDTH  = 1800;
  SMALLEST_RAW_IMAGE_HEIGHT = 1400;
begin
  try
    Params.RAW_HalfSize := (iMaxX <= SMALLEST_RAW_IMAGE_WIDTH) and (iMaxY <= SMALLEST_RAW_IMAGE_HEIGHT);
    Params.RAW_GetExifThumbnail := (iMaxX <= SMALLEST_RAW_THUMBNAIL_WIDTH) and (iMaxY <= SMALLEST_RAW_THUMBNAIL_HEIGHT);

    if Params.RAW_GetExifThumbnail then
      LoadFromFileRaw(sFilename)  // INTERNAL IERAW
    else
      LoadFromFile(sFilename);    // PLUG IN (if available)

    result := not (Aborting or IsEmpty);
  except
    on E:Exception do
      result := false;
  end;
end;
{$ENDIF}



{!!
<FS>TImageEnIO.SaveToFileEx

<FM>Declaration<FC>
function SaveToFileEx(const sFilename : string;
                      iJpegQuality: integer = 0): Boolean;

<FM>Description<FN>
Calls <A TImageEnIO.SaveToFile>, optionally setting <A TIOParamsVals.JPEG_Quality>.
Returns True if saving was successful, or False on error

<FM>Example<FC>
// Save the image to MyImage.jpeg at 90% quality
ImageEnView1.IO.SaveToFileEx('D:\MyImage.jpeg', 90)
!!}
function TImageEnIOHelper.SaveToFileEx(const sFilename: string;
                                       iJpegQuality: integer = 0): Boolean;
const
  Reset_Jpeg_Orientation_On_Save = False;
begin
  try
    if iJpegQuality > 0 then
      Params.JPEG_Quality := iJpegQuality;

    if Reset_Jpeg_Orientation_On_Save then
      Params.EXIF_Orientation := _exoCorrectOrientation;

    SavetoFile(sFilename);

    Result := not Aborting;
  except
    result := false;
  end;
end;


{!!
<FS>TImageEnIO.CreatePDFFromFileList

<FM>Declaration<FC>
function CreatePDFFromFileList(const sDestFilename : string;
                               ssFileList : TStrings;
                               const aPaperSize : <A TIOPDFPaperSize>;
                               const aCompression : <A TIOPDFCompression>;
                               const sTitle : string;
                               const sAuthor : string;
                               const bFailOnUnsupportedImage : Boolean = True;
                               const bRaiseExceptions : Boolean = False
                               ) : Boolean;

<FM>Description<FN>
Generates a PDF file from all images specified in a TStrings list, by combining the functions of <A TImageEnIO.CreatePDFFile>, <A TImageEnIO.SaveToPDF> and <A TImageEnIO.ClosePDFFile>.
Result is true unless an error is encountered.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>sDestFilename<FN></C> <C>The .PDF filename to save the file to</C> </R>
<R> <C><FC>ssFileList<FN></C> <C>A list of all images to add to the PDF file</C> </R>
<R> <C><FC>aPaperSize<FN></C> <C>The <L TIOPDFPaperSize>size</L> at which to create the PDF file</C> </R>
<R> <C><FC>aCompression<FN></C> <C>The level of <L TIOPDFCompression>compression</L> to use</C> </R>
<R> <C><FC>sTitle<FN></C> <C>The title for the PDF document (appears in "Properties")</C> </R>      
<R> <C><FC>sAuthor<FN></C> <C>The author for the PDF document (appears in "Properties")</C> </R>
<R> <C><FC>bFailOnUnsupportedImage<FN></C> <C>When true this procedure will fail if unsupported images (or other file types) are found in ssFileList. If false, unsupported file types are skipped</C> </R>
<R> <C><FC>bRaiseExceptions<FN></C> <C>When true this procedure will generate an exception if it fails. If false, you will need to check the result to detect failure</C> </R>
</TABLE>

<FM>Example<FC>
CreatePDFFromFileList('D:\MyNewPDF.pdf',
                      ssMyImageList,
                      iepA4,
                      ioPDF_JPEG,
                      'My Cool PDF',
                      'Mr Developer');
!!}
function TImageEnIOHelper.CreatePDFFromFileList(const sDestFilename : string;
                                                ssFileList : TStrings;
                                                const aPaperSize : TIOPDFPaperSize;
                                                const aCompression : TIOPDFCompression;
                                                const sTitle : string;
                                                const sAuthor : string;
                                                const bFailOnUnsupportedImage : Boolean = True;
                                                const bRaiseExceptions : Boolean = False
                                                ) : Boolean;
var
  I: Integer;
  sCurrFile: string;
begin
  Result := True;
  try
    Params.PDF_PaperSize := aPaperSize;
    Params.PDF_Title     := AnsiString(sTitle);
    Params.PDF_Author    := AnsiString(sAuthor);
    CreatePDFFile(sDestFilename);

    for I := 0 to ssFileList.Count - 1 do
    begin
      sCurrFile := ssFileList[I];
      if (bFailOnUnsupportedImage = False) or IsKnownFormat(sCurrFile) then
      begin
        LoadFromFile(sCurrFile);
        Params.PDF_Compression := aCompression;
        SaveToPDF;
      end;
    end;

    ClosePDFFile;
  Except
    Result := False;
  end;
end;


{!!
<FS>TImageEnIO.CreatePSFromFileList

<FM>Declaration<FC>
function CreatePSFromFileList(const sDestFilename : string;
                              ssFileList : TStrings;
                              const aPaperSize : <A TIOPDFPaperSize>;
                              const aCompression : <A TIOPSCompression>;
                              const sTitle : string = '';
                              const bFailOnUnsupportedImage : Boolean = True;
                              const bRaiseExceptions : Boolean = False
                              ) : Boolean;

<FM>Description<FN>
Generates a PS file from all images specified in a TStrings list, by combining the functions of <A TImageEnIO.CreatePSFile>, <A TImageEnIO.SaveToPS> and <A TImageEnIO.ClosePSFile>.
Result is true unless an error is encountered.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>sDestFilename<FN></C> <C>The .PS filename to save the file to</C> </R>
<R> <C><FC>ssFileList<FN></C> <C>A list of all images to add to the PS file</C> </R>
<R> <C><FC>aPaperSize<FN></C> <C>The <L TIOPDFPaperSize>size</L> at which to create the PS file</C> </R>
<R> <C><FC>aCompression<FN></C> <C>The level of <L TIOPSCompression>compression</L> to use</C> </R>
<R> <C><FC>sTitle<FN></C> <C>The title for the PS document</C> </R>
<R> <C><FC>bFailOnUnsupportedImage<FN></C> <C>When true this procedure will fail if unsupported images (or other file types) are found in ssFileList. If false, unsupported file types are skipped</C> </R>
<R> <C><FC>bRaiseExceptions<FN></C> <C>When true this procedure will generate an exception if it fails. If false, you will need to check the result to detect failure</C> </R>
</TABLE>

<FM>Example<FC>
CreatePSFromFileList('D:\MyNewPS.PS',
                      ssMyImageList,
                      iepA4,
                      ioPS_JPEG);
!!}
function TImageEnIOHelper.CreatePSFromFileList(const sDestFilename : string;
                                               ssFileList : TStrings;
                                               const aPaperSize : TIOPDFPaperSize;
                                               const aCompression : TIOPSCompression;
                                               const sTitle : string = '';
                                               const bFailOnUnsupportedImage : Boolean = True;
                                               const bRaiseExceptions : Boolean = False
                                               ) : Boolean;
var
  I: Integer;
  sCurrFile: string;
begin
  Result := True;
  try
    Params.PS_PaperSize := aPaperSize;
    Params.PS_Title     := AnsiString(sTitle);
    CreatePSFile(sDestFilename);

    for I := 0 to ssFileList.Count - 1 do
    begin
      sCurrFile := ssFileList[I];
      if (bFailOnUnsupportedImage = False) or IsKnownFormat(sCurrFile) then
      begin
        LoadFromFile(sCurrFile);
        Params.PS_Compression := aCompression;
        SaveToPS;
      end;
    end;

    ClosePSFile;
  Except
    Result := False;
  end;
end;


                       
{!!
<FS>TImageEnIO.LoadFromBlob

<FM>Declaration<FC>
function LoadFromBlob(aField: TBlobField;
                      bAutoAdjustOrientation: Boolean = False;
                      iImageIndex : Integer = -1
                      ): Boolean;

<FM>Description<FN>
Uses streams to load an image from the blob field of a database table, optionally setting <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images and <A TIOParamsVals.ImageIndex> to display a specific image.
Returns True if loading was successful, or False on error

Note: You must define IE_DB_Helpers in iexHelperFunctions.pas to use this method

<FM>Example<FC>
// Load the image from the MyTableImageBlob field at the current position in the database
ImageEnView1.IO.LoadFromBlob(MyTableImageBlob);
!!}
{$IFDEF IE_DB_Helpers}
function TImageEnIOHelper.LoadFromBlob(aField: TBlobField;
                                       bAutoAdjustOrientation: Boolean = False;
                                       iImageIndex : Integer = -1
                                       ): Boolean;
begin
  Result := LoadFromBlobFast( aField, 0, 0, bAutoAdjustOrientation, iImageIndex);
end;
{$ENDIF}

{!!
<FS>TImageEnIO.LoadFromBlobFast

<FM>Declaration<FC>
function LoadFromBlobFast(aField: TBlobField;
                          iMaxX, iMaxY: integer;
                          bAutoAdjustOrientation: Boolean = False;
                          iImageIndex : Integer = -1
                          ): Boolean;

<FM>Description<FN>
Uses streams to load an image from the blob field of a database table and allows you to specify a maximum size that you require an image so that it can be loaded as fast as possible, by using:
- <A TIOParamsVals.JPEG_Scale> for JPEGs
- <A TIOParamsVals.RAW_GetExifThumbnail> and <A TIOParamsVals.RAW_HalfSize> for Camera Raw files

You can also set <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images

Returns True if loading was successful, or False on error

Note: You must define IE_DB_Helpers in iexHelperFunctions.pas to use this method

<FM>Example<FC>         
// Load the image from the MyTableImageBlob field as fast as possible, but bigger than the displaying TImageEnView
ImageEnView1.AutoShrink := True;
ImageEnView1.IO.LoadFromBlobFast(MyTableImageBlob, ImageEnView1.Width, ImageEnView1.Height)
!!}                                                                     
{$IFDEF IE_DB_Helpers}
function TImageEnIOHelper.LoadFromBlobFast(aField: TBlobField;
                                           iMaxX, iMaxY: integer;
                                           bAutoAdjustOrientation: Boolean = False;
                                           iImageIndex : Integer = -1
                                           ): Boolean;
const
  // SOME APPROXIMATE VALUES TO OPTIMIZE RAW LOADING
  SMALLEST_RAW_THUMBNAIL_WIDTH  = 150;
  SMALLEST_RAW_THUMBNAIL_HEIGHT = 100;
  SMALLEST_RAW_IMAGE_WIDTH  = 1800;
  SMALLEST_RAW_IMAGE_HEIGHT = 1400;
var
  aMemStream: TMemoryStream;
  aBlobStream: TBlobStream;
  bFastLoad: Boolean;
begin
  Result := True;
  try
    // SET PARAMS
    bFastLoad := (iMaxX > 0) and (iMaxY > 0);
    if bFastLoad then
    begin
      {$IFDEF IEINCLUDERAWFORMATS}
      // RAW PARAMS
      Params.RAW_HalfSize := (iMaxX <= SMALLEST_RAW_IMAGE_WIDTH) and (iMaxY <= SMALLEST_RAW_IMAGE_HEIGHT);
      Params.RAW_GetExifThumbnail := (iMaxX <= SMALLEST_RAW_THUMBNAIL_WIDTH) and (iMaxY <= SMALLEST_RAW_THUMBNAIL_HEIGHT);
      {$ENDIF}

      // JPEG PARAMS
      Params.Width  := iMaxX;
      Params.Height := iMaxY;
      Params.JPEG_Scale := ioJPEG_AUTOCALC;

      // Is it a thumbnail?
      if iMaxX + iMaxY < 500 then
        Params.JPEG_DCTMethod := ioJPEG_IFAST;
    end
    else
    begin
      // reset any fast loading variables
      Params.RAW_HalfSize := false;
      Params.RAW_GetExifThumbnail := false;

      Params.JPEG_Scale := ioJPEG_fullsize;
      Params.JPEG_DCTMethod := ioJPEG_ISLOW;
    end;
      
    Params.EnableAdjustOrientation := bAutoAdjustOrientation;
    if iImageIndex <> -1 then
      Params.ImageIndex := iImageIndex;

    aMemStream := TMemoryStream.create;
    try
      aBlobStream := TBlobStream.create(aField, bmRead);
      try
        aMemStream.LoadFromStream(aBlobStream);
        aMemStream.Position := 0;
      finally
        aBlobStream.free;
      end;

      LoadFromStream(aMemStream);
    finally
      aMemStream.free;
    end;

  except
    result := false;
  end;
end;
{$ENDIF}


{!!
<FS>TImageEnIO.SaveToBlob

<FM>Declaration<FC>
function SaveToBlob(aField : TBlobField;
                    aFileType : TIOFileType;
                    iJpegQuality: integer = 0): Boolean;

<FM>Description<FN>
Uses streams to set a blob field with an image, optionally setting <A TIOParamsVals.JPEG_Quality>. You must specify a <L TIOFileType>file type</L>, e.g. ioJPEG.
Returns True if saving was successful, or False on error

Note: You must define IE_DB_Helpers in iexHelperFunctions.pas to use this method

<FM>Example<FC>
// Update the current image in the table with that displayed in our TImageEnView
MyDBTable.Edit;
ImageEnView1.IO.SaveToFileEx(MyDBTableImageField, ioJPEG, 90)
MyDBTableEditDate.AsDateTime := Now;
MyDBTable.Post;
!!}                      
{$IFDEF IE_DB_Helpers}
function TImageEnIOHelper.SaveToBlob(aField : TBlobField;
                                     aFileType : TIOFileType;
                                     iJpegQuality: integer = 0): Boolean;
const
  Reset_Jpeg_Orientation_On_Save = False;
var
  aMemStream: TMemoryStream;
  aBlobStream: TBlobStream;
begin
  Result := False;

  if aFileType <> ioUnknown then
  try
    if iJpegQuality > 0 then
      Params.JPEG_Quality := iJpegQuality;

    if Reset_Jpeg_Orientation_On_Save then
      Params.EXIF_Orientation := _exoCorrectOrientation;

    aMemStream := TMemoryStream.create;
    try
      SaveToStream(aMemStream, aFileType);
      aMemStream.Position := 0;

      aBlobStream := TBlobStream.create(aField, bmWrite);
      try
        aMemStream.SaveToStream(aBlobStream);
      finally
        aBlobStream.Free;
      end;

      result := true;
    finally
      aMemStream.free;
    end;
  except
    result := False;
  end;
end;
{$ENDIF}

{$ENDIF}

           

{$IFDEF Delphi2005orNewer}
{ TBitmapHelper }

// Provide link to TBitmap references
{!!
<FS>TBitmap

<FM>Description<FN>
Standard Windows TBitmap class. See Delphi documentation for more detail.

ImageEn provides a similar class for holding images in memory, <A TIEBitmap>. 
!!}


{!!
<FS>TBitmap.IEInitialize

<FM>Declaration<FC>
procedure IEInitialize(iWidth, iHeight: Integer; ABackgroundColor : TColor = clNone);

<FM>Description<FN>
Resizes a bitmap and optionally fills it with the specified color.

<FM>Example<FC>
// Make a bitmap of screen size and black background
MyBitmap.IEInitialize(Screen.Width, Screen.Height, clBlack);
!!}
procedure TBitmapHelper.IEInitialize(iWidth, iHeight: Integer; ABackgroundColor : TColor = clNone);
begin
  Width  := iWidth;
  Height := iHeight;

  if ABackgroundColor <> clNone then
  begin
    Canvas.Brush.Color := ABackgroundColor;
    Canvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

{!!
<FS>TBitmap.IERotate

<FM>Declaration<FC>
procedure IERotate(Angle: double; AntiAliasMode: <A TIEAntialiasMode> = ierFast; BackgroundColor: TColor = -1);

<FM>Description<FN>
Calls <A TImageEnProc.Rotate> to rotate a TBitmap.

<FM>Example<FC>
// Rotate the image 90 deg. counter-clockwise
MyBitmap.IERotate(90);
          
// Rotate the image 45 deg. clockwise and fill new areas with black
MyBitmap.IERotate(315, ierFast, clBlack);
!!}
procedure TBitmapHelper.IERotate(Angle: double; AntiAliasMode: TIEAntialiasMode = ierFast; BackgroundColor: TColor = -1);
var
  AImageEnProc: TImageEnProc;
begin
  AImageEnProc := TImageEnProc.CreateFromBitmap(Self); // This uses the overloaded (TBitmap) parameter
  try
    AImageEnProc.Rotate(Angle, AntialiasMode, BackgroundColor);
  finally
    AImageEnProc.Free;
  end;
end;


{!!
<FS>TBitmap.IEFlip

<FM>Declaration<FC>
procedure IEFlip(Direction: <A TFlipDir>);

<FM>Description<FN>
Calls <A TImageEnProc.Flip> to flip a TBitmap.

<FM>Example<FC>
// Flip the bitmap image horizontally
MyBitmap.IEFlip(fdHorizontal);
!!}
procedure TBitmapHelper.IEFlip(Direction: TFlipDir);
var
  AImageEnProc: TImageEnProc;
begin
  AImageEnProc := TImageEnProc.CreateFromBitmap(Self); // This uses the overloaded (TBitmap) parameter
  try
    AImageEnProc.Flip(Direction);
  finally
    AImageEnProc.Free;
  end;
end;


{!!
<FS>TBitmap.IEResample

<FM>Declaration<FC>
procedure IEResample(iNewWidth, iNewHeight: integer; QualityFilter: TResampleFilter = rfNone; bMaintainAspectRatio : Boolean = False);

<FM>Description<FN>
Calls <A TImageEnProc.Resample> to resize a TBitmap and its content.

<FM>Example<FC>
// Resize the bitmap to screen dimensions (and good quality)
MyBitmap.IEResample(Screen.Width, Screen.Height, rfLanczos3);
!!}
procedure TBitmapHelper.IEResample(iNewWidth, iNewHeight: integer; QualityFilter: TResampleFilter = rfNone; bMaintainAspectRatio : Boolean = False);
var
  AImageEnProc: TImageEnProc;
begin
  if (Width = iNewWidth) and (Height = iNewHeight) then
    exit;

  AImageEnProc := TImageEnProc.CreateFromBitmap(Self); // This uses the overloaded (TBitmap) parameter
  try
    AImageEnProc.Resample(iNewWidth, iNewHeight, QualityFilter, bMaintainAspectRatio);
  finally
    AImageEnProc.Free;
  end;
end;


{!!
<FS>TBitmap.IELoadFromFile

<FM>Declaration<FC>
function IELoadFromFile(const sFilename : string): Boolean;

<FM>Description<FN>
Allows a TBitmap to load any format supported by ImageEn.
Returns True if loading was successful, or False on error

<FM>Example<FC>
// Load MyImage.jpeg into a bitmap
MyBitmap.IELoadFromFile('D:\MyImage.jpeg')
!!}
function TBitmapHelper.IELoadFromFile(const sFilename: string): Boolean;
begin
  Result := IELoadFromFileFast(sFilename, -1, -1);
end;      



{!!
<FS>TBitmap.IELoadFromFileFast

<FM>Declaration<FC>
function IELoadFromFileFast(const  sFilename: string;
                            iMaxX, iMaxY: integer;
                            bAutoAdjustOrientation: Boolean = False
                            ): Boolean;

<FM>Description<FN>
Allows you to load an image into a TBitmap using the fastest method by specifying the maximum size it is required. It uses:
- <A TIOParamsVals.JPEG_Scale> for JPEGs
- <A TIOParamsVals.RAW_GetExifThumbnail> and <A TIOParamsVals.RAW_HalfSize> for Camera Raw files

You can also set <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images.

Returns True if loading was successful, or False on error.

<FM>Example<FC>
// Load the specified image as fast as possible, but bigger than screen size
MyBitmap.IELoadFromFileFast('D:\MyImage.jpeg', Screen.Width, Screen.Height)
!!}
function TBitmapHelper.IELoadFromFileFast(const  sFilename: string;
                                          iMaxX, iMaxY: integer;
                                          bAutoAdjustOrientation: Boolean = False
                                          ): Boolean;
var
  AImageEnIO: TImageEnIO;
begin
  Result := True;
  try
    if IEFilenameInExtensions(sFilename, '.bmp') then
    begin
      LoadFromFile(sFilename);
    end
    else
    begin
      AImageEnIO := TImageEnIO.CreateFromBitmap(Self);
      // Loaded image may have transparency (e.g. WMF files), so match background color to existing background of images (i.e. honour IEInitialize)
      if (Width > 0) and (Height > 0) then
        AImageEnIO.Background := Canvas.Pixels[0, 0];
      Result := AImageEnIO.LoadFromFileFast(sFilename, iMaxX, iMaxY, bAutoAdjustOrientation);
      AImageEnIO.Free;
    end;
  except
    result := False;
  end;
end;



{!!
<FS>TBitmap.LoadFromURL

<FM>Declaration<FC>
function LoadFromURL(const URL            : WideString;
                     const sProxyAddress  : WideString = '';
                     const sProxyUser     : WideString = '';
                     const sProxyPassword : WideString = ''
                     ): Boolean;

<FM>Description<FN>
Load an image from the internet using the HTTP or FTP protocol.

Note: See documentation for <L TImageEnIO.LoadFromURL>TImageEnIO.LoadFromURL</L>

<FM>Example<FC>
MyBitmapLoadFromURL('http://www.imageen.com/image.jpg');
!!}
function TBitmapHelper.LoadFromURL(const URL            : WideString;
                                   const sProxyAddress  : WideString = '';
                                   const sProxyUser     : WideString = '';
                                   const sProxyPassword : WideString = ''
                                   ): Boolean;

var
  AImageEnIO: TImageEnIO;
begin
  try
    AImageEnIO := TImageEnIO.CreateFromBitmap(Self);

    AImageEnIO.ProxyAddress  := sProxyAddress;
    AImageEnIO.ProxyUser     := sProxyUser;
    AImageEnIO.ProxyPassword := sProxyPassword;

    Result := AImageEnIO.LoadFromURL(URL);
    AImageEnIO.Free;
  except
    result := False;
  end;
end;

{!!
<FS>TBitmap.IESaveToFile

<FM>Declaration<FC>
function IESaveToFile(const sFilename : string; iJPEGQuality : Integer): Boolean;

<FM>Description<FN>
Allows a TBitmap to save to any format supported by ImageEn. If you are saving to JPEG you can also specify the <A TIOParamsVals.JPEG_Quality>.
Returns True if saving was successful, or False on error

<FM>Example<FC>
// Save the image at 90% quality
MyBitmap.IESaveToFile(SavePictureDialog1.FileName, 90)
!!}
function TBitmapHelper.IESaveToFile(const sFilename: string; iJPEGQuality : Integer): Boolean;
var
  AImageEnIO: TImageEnIO;
begin
  result := True;
  try
    if IEFilenameInExtensions(sFilename, '.bmp') then
    begin
      SaveToFile(sFilename);
    end
    else
    begin
      if PixelFormat = pfDevice then
        PixelFormat := pf24bit;
      AImageEnIO := TImageEnIO.CreateFromBitmap(Self);
      Result := AImageEnIO.SaveToFileEx(sFilename, iJPEGQuality);
      AImageEnIO.Free;
    end;
  except
    Result := False;
  end;
end;



// CalcSoftShadowWidth: Use IESoftShadowSize

{!!
<FS>TBitmap.IELoadAsThumbnail

<FM>Declaration<FC>
function IELoadAsThumbnail(const sFilename: string;
                           iMaxX, iMaxY: integer;
                           bCanStretch: Boolean;
                           bAutoAdjustOrientation: Boolean = False;
                           QualityFilter : <A TResampleFilter> = rfLanczos3;

                           bAddBorder: Boolean = False;
                           cBorderColor: TColor = clBlack;

                           bAddShadow: Boolean = False;
                           iBlurRadius : Integer = 4;
                           iShadowOffset : Integer = 4;
                           cShadowColor: TColor = clBlack;
                           cBGColor: TColor = clWhite
                           ) : Boolean;

<FM>Description<FN>
Loads an image of any format into a TBitmap reducing it to the specified size (while maintaining the aspect ratio, thus one of the dimensions is likely to be less than the specified value).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>iMaxX, iMaxY<FN></C> <C>The maximum size of the new image (as the aspect ratio is maintained, one of the dimensions is likely to end upless than the specified value)</C> </R>
<R> <C><FC>bCanStretch<FN></C> <C>Set to false to avoid images smaller than iMaxX x iMaxY from being made larger</C> </R>
<R> <C><FC>bAutoAdjustOrientation<FN></C> <C>Sets <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images</C> </R>
<R> <C><FC>QualityFilter<FN></C> <C>Specify the quality that is used for rescaling the image</C> </R>
<R> <C><FC>bAddBorder<FN></C> <C>Set to true to add a 1 pixel border to the thumbnail</C> </R>
<R> <C><FC>cBorderColor<FN></C> <C>The color of the added border</C> </R>
<R> <C><FC>bAddShadow<FN></C> <C>Add a solid or soft shadow to the image</C> </R>
<R> <C><FC>iBlurRadius<FN></C> <C>Set to 0 to add a solid shadow or any other value for the width of the <L TImageEnProc.AddSoftShadow>Soft Shadow</L></C> </R>
<R> <C><FC>iShadowOffset<FN></C> <C>The <L TImageEnProc.AddSoftShadow>offset</L> of the shadow from the image</C> </R>
<R> <C><FC>cShadowColor<FN></C> <C>The shadow color</C> </R>
<R> <C><FC>cBGColor<FN></C> <C>The color of the image behind the shadow</C> </R>
</TABLE>

<FM>Example<FC>
// Load an image at the size 160x120
MyBitmap.IELoadAsThumbnail('D:\MyImage.jpeg', 160, 120, False);
!!}
function TBitmapHelper.IELoadAsThumbnail(const sFilename: string;
                                         iMaxX, iMaxY: integer;
                                         bCanStretch: Boolean;
                                         bAutoAdjustOrientation: Boolean = False;
                                         QualityFilter : TResampleFilter = rfLanczos3;

                                         bAddBorder: Boolean = False;
                                         cBorderColor: TColor = clBlack;

                                         bAddShadow: Boolean = False;
                                         iBlurRadius : Integer = 4;
                                         iShadowOffset : Integer = 4;
                                         cShadowColor: TColor = clBlack;
                                         cBGColor: TColor = clWhite
                                         ) : Boolean;
begin
  try
    Result := IELoadFromFileFast(sFilename, iMaxX, iMaxY, bAutoAdjustOrientation);
    if Result then
      IEConvertToThumbnail(iMaxX, iMaxY, bCanStretch, QualityFilter,
                           bAddBorder, cBorderColor,
                           bAddShadow, iBlurRadius, iShadowOffset, cShadowColor, cBGColor);
  except
    Result := False;
  end;
end;



procedure TBitmapHelper.ConvertTo24Bit;
var
  AImageEnProc: TImageEnProc;
begin
  AImageEnProc := TImageEnProc.CreateFromBitmap(Self);
  try
  AImageEnProc.ConvertTo24Bit;
  finally
    AImageEnProc.Free;
  end;
end;




{!!
<FS>TBitmap.IEConvertToThumbnail

<FM>Declaration<FC>

procedure IEConvertToThumbnail(iMaxX, iMaxY: integer;
                               bCanStretch: Boolean;
                               QualityFilter : <A TResampleFilter> = rfLanczos3;

                               bAddBorder: Boolean = False;
                               cBorderColor: TColor = clBlack;

                               bAddShadow: Boolean = False;
                               iBlurRadius : Integer = 4;
                               iShadowOffset : Integer = 4;
                               cShadowColor: TColor = clBlack;
                               cBGColor: TColor = clWhite);

<FM>Description<FN>
Resize an image in a TBitmap to the specified size (while maintaining the aspect ratio, thus one of the dimensions is likely to be less than the specified value).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>iMaxX, iMaxY<FN></C> <C>The maximum size of the new image (as the aspect ratio is maintained, one of the dimensions is likely to end upless than the specified value)</C> </R>
<R> <C><FC>bCanStretch<FN></C> <C>Set to false to avoid images smaller than iMaxX x iMaxY from being made larger</C> </R>
<R> <C><FC>bAutoAdjustOrientation<FN></C> <C>Sets <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images</C> </R>
<R> <C><FC>QualityFilter<FN></C> <C>Specify the quality that is used for rescaling the image</C> </R>
<R> <C><FC>bAddBorder<FN></C> <C>Set to true to add a 1 pixel border to the thumbnail</C> </R>
<R> <C><FC>cBorderColor<FN></C> <C>The color of the added border</C> </R>
<R> <C><FC>bAddShadow<FN></C> <C>Add a solid or soft shadow to the image</C> </R>
<R> <C><FC>iBlurRadius<FN></C> <C>Set to 0 to add a solid shadow or any other value for the width of the <L TImageEnProc.AddSoftShadow>Soft Shadow</L></C> </R>
<R> <C><FC>iShadowOffset<FN></C> <C>The <L TImageEnProc.AddSoftShadow>offset</L> of the shadow from the image</C> </R>
<R> <C><FC>cShadowColor<FN></C> <C>The shadow color</C> </R>
<R> <C><FC>cBGColor<FN></C> <C>The color of the image behind the shadow</C> </R>
</TABLE>

<FM>Example<FC>            
// Resize the current bitmap image to 160x120 with a black border
MyBitmap.IEConvertToThumbnail(160, 120, True, rfFastLinear, True, clBlack);
!!}
procedure TBitmapHelper.IEConvertToThumbnail(iMaxX, iMaxY: integer;
                                             bCanStretch: Boolean;
                                             QualityFilter : TResampleFilter = rfLanczos3;

                                             bAddBorder: Boolean = False;
                                             cBorderColor: TColor = clBlack;

                                             bAddShadow: Boolean = False;
                                             iBlurRadius : Integer = 4;
                                             iShadowOffset : Integer = 4;
                                             cShadowColor: TColor = clBlack;
                                             cBGColor: TColor = clWhite);
var
  ASize: tpoint;
  bUseSoftShadow : Boolean;
begin
  bUseSoftShadow := iBlurRadius > 0;

  // if a shadow is to be added remove the size of the shadow from the bitmap dimension
  if bAddShadow and bUseSoftShadow then
  begin
    Dec(iMaxX, IESoftShadowSize(iBlurRadius, iShadowOffset, iShadowOffset));
    Dec(iMaxY, IESoftShadowSize(iBlurRadius, iShadowOffset, iShadowOffset));
  end
  else
    if bAddShadow and (bUseSoftShadow = false) then
  begin
    Dec(iMaxX, iShadowOffset);
    Dec(iMaxY, iShadowOffset);
  end;
  
  if baddshadow or bAddBorder then
    ConvertTo24Bit;

  if PixelFormat = pfCustom then
    PixelFormat := pf24bit;

  // RESIZE
  ASize := GetImageSizeWithinArea(Width, Height, iMaxX, iMaxY);
  if bCanStretch or (Width > ASize.X) then
    IEResample(ASize.X, ASize.Y, QualityFilter, false);


  // BORDER
  if bAddBorder then
  begin
    Canvas.pen.color := cBorderColor;
    Canvas.Polyline([Point(0, 0),
                     Point(Width - 1, 0),
                     Point(Width - 1, Height - 1),
                     Point(0, Height - 1),
                     Point(0, 0)]);
  end;

  // SHADOW
  if bAddShadow then
  begin
    if bUseSoftShadow then
      IEAddSoftShadow(iBlurRadius, iShadowOffset, cBGColor, _loTopLeft, cShadowColor)
    else
    begin
       // add back the pixels we removed
      width  := width  + iShadowOffset;
      Height := Height + iShadowOffset;

       // Draw the shadowm
      Canvas.Brush.color := cShadowColor;

       // along the right
      Canvas.FillRect(Rect(Width - iShadowOffset, 0, Width, Height));

       // along the bottom
      Canvas.FillRect(Rect(0, Height - iShadowOffset, Width, Height));

       // now add the background colour
      Canvas.Brush.color := cBGColor;

       // at the top right
      Canvas.FillRect(Rect(Width - iShadowOffset, 0, Width, iShadowOffset));

       // at the bottom left
      Canvas.FillRect(Rect(0, Height - iShadowOffset, iShadowOffset, Height));
    end;
  end;
end;



{!!
<FS>TBitmap.IEAddSoftShadow

<FM>Declaration<FC>
procedure IEAddSoftShadow(iBlurRadius: integer;
                          iShadowOffset : Integer;
                          cBGColor: TColor;
                          LightOrigin: <A TLightOrigin> = _loTopLeft;
                          cShadowColor: TColor = clblack);


<FM>Description<FN>
Calls <A TImageEnProc.AddSoftShadow> to add a soft shadow to an image in a TBitmap

<FM>Example<FC>
// Add a soft shadow to the bitmap image
MyBitmap.IEAddSoftShadow(3, 3, clWhite);
!!}
procedure TBitmapHelper.IEAddSoftShadow(iBlurRadius: integer;
                                        iShadowOffset : Integer;
                                        cBGColor: TColor;
                                        LightOrigin: TLightOrigin = _loTopLeft;
                                        cShadowColor: TColor = clblack);
var
  ie: TImageEnView;
  iOffSetX, iOffSetY: integer;
begin
  ie := TImageEnView.Create(nil);
  try
    ie.background := cBGColor;
    ie.Bitmap.assign(Self);
    ie.update;
    iOffSetX := iShadowOffset;
    iOffSetY := iShadowOffset;
    if LightOrigin in [_loTopRight, _loBottomRight] then
      iOffSetX := -iOffSetX;
    if LightOrigin in [_loBottomLeft, _loBottomRight] then
      iOffSetY := -iOffSetY;

    ie.Proc.AddSoftShadow(iBlurRadius, iOffSetX, iOffSetY, TRUE, cShadowColor);
    ie.RemoveAlphaChannel(True);
    Self.assign(ie.bitmap);
  finally
    ie.Free;
  end;
end;





{!!
<FS>TBitmap.PrintImage

<FM>Declaration<FC>
procedure PrintImage(PrtCanvas: TCanvas = nil; MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1; VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER; Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0; GammaCorrection: double = 1; SubsampleFilter: <A TResampleFilter> = rfFastLinear);

<FM>Description<FN>
Print the current bitmap by specifying margins, vertical position, horizontal position and size.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>PrtCanvas<FN></C> <C>The canvas to bring to. Generally the application will pass this as Printer.Canvas</C> </R>
<R> <C><FC>MarginLeft<FN></C> <C>Left page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>MarginTop<FN></C> <C>Top page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>MarginRight<FN></C> <C>Right page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>MarginBottom<FN></C> <C>Bottom page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>VerticalPos<FN></C> <C>How the image is vertically aligned on the page</C> </R>
<R> <C><FC>HorizontalPos<FN></C> <C>How the image horizontally aligned on the page</C> </R>
<R> <C><FC>Size<FN></C> <C>The size to print the image</C> </R>
<R> <C><FC>SpecWidth<FN></C> <C>The absolute width of the image in inches (Valid only if Size = iesSPECIFIEDSIZE)</C> </R>
<R> <C><FC>SpecHeight<FN></C> <C>The absolute height of the image in inches (Valid only if Size = iesSPECIFIEDSIZE)</C> </R>
<R> <C><FC>GammaCorrection<FN></C> <C>The gamma correction value (Specify 1.0 to disable gamma correction)</C> </R>
<R> <C><FC>SubsampleFilter<FN></C> <C>The filter that is used to enhance quality if hte image needs to be resampled for printing</C> </R>
</TABLE>


<FM>Example<FC>
// Print the image in the center of the page at the original size
Printer.BeginDoc;
ABitmap.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesNORMAL, 0, 0, 1);
Printer.EndDoc;

// Print the image in the center of the page stretched to page dimensions (respecting the proportions)
Printer.BeginDoc;
ABitmap.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE, 0, 0, 1);
Printer.EndDoc;

!!}

procedure TBitmapHelper.PrintImage(PrtCanvas: TCanvas = nil;
                                   MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1;
                                   VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER;
                                   Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0;
                                   GammaCorrection: double = 1;
                                   SubsampleFilter: TResampleFilter = rfFastLinear);
var
  AImageEnIO: TImageEnIO;
begin
  AImageEnIO := TImageEnIO.CreateFromBitmap(Self);
  try
    AImageEnIO.PrintingFilterOnSubsampling := SubsampleFilter;
    AImageEnIO.PrintImage(PrtCanvas, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
  finally
    AImageEnIO.Free;
  end;
end;


{$ENDIF}




{$IFDEF Delphi2005orNewer}
{ TIEBitmapHelper }


{!!
<FS>TIEBitmap.IEInitialize

<FM>Declaration<FC>
procedure IEInitialize(iWidth, iHeight: Integer; ABackgroundColor : TColor = clNone); overload;
procedure IEInitialize(iWidth, iHeight: Integer; ABackgroundColor: TColor; ATransparent: Boolean); overload;

<FM>Description<FN>
Resizes a TIEBitmap and optionally fills it with the specified color.

<FM>Example<FC>
// Resize the IEBitmap to screen size with a black background
MyIEBitmap.IEInitialize(Screen.Width, Screen.Height, clBlack);

// Resize the IEBitmap to screen size with transparency
MyIEBitmap.IEInitialize(Screen.Width, Screen.Height, clBlack, True);
!!}
procedure TIEBitmapHelper.IEInitialize(iWidth, iHeight: Integer; ABackgroundColor: TColor);
begin
  Width := iWidth;
  Height := iHeight;

  if ABackgroundColor <> clNone then
  begin
    Canvas.Brush.Color := ABackgroundColor;
    Canvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TIEBitmapHelper.IEInitialize(iWidth, iHeight: Integer; ABackgroundColor: TColor; ATransparent: Boolean);
var
  ie: TImageEnView;
begin
  ie := TImageEnView.Create(nil);
  try
    ie.Background := ABackgroundColor;
    ie.IEBitmap.Assign(Self);
    ie.IEBitmap.Width := iWidth;
    ie.IEBitmap.Height := iHeight;
    ie.Update;
    ie.IEBitmap.AlphaChannel.Fill(0);
    Self.Assign(ie.IEbitmap);
  finally
    ie.Free;
  end;
end;



{!!
<FS>TIEBitmap.IELoadFromFile

<FM>Declaration<FC>
function IELoadFromFile(const sFilename : string): Boolean;

<FM>Description<FN>
Allows a TIEBitmap to load any format supported by ImageEn.
Returns True if loading was successful, or False on error

Note: There is no advantage to using this instead of <A TIEBitmap.Read>

<FM>Example<FC>       
// Load the image specified in an open dialog
ImageEnView1.Bitmap.IELoadFromFile(OpenPictureDialog1.FileName)
!!}
function TIEBitmapHelper.IELoadFromFile(const sFilename: string): Boolean;
begin
  Result := IELoadFromFileFast(sFilename, -1, -1);
end;



{!!
<FS>TIEBitmap.IELoadFromFileFast

<FM>Declaration<FC>
function IELoadFromFileFast(const  sFilename: string;
                            iMaxX, iMaxY: integer;
                            bAutoAdjustOrientation: Boolean = False
                            ): Boolean;

<FM>Description<FN>
Allows you to load an image into a TIEBitmap using the fastest method by specifying the maximum size it is required. It uses:
- <A TIOParamsVals.JPEG_Scale> for JPEGs
- <A TIOParamsVals.RAW_GetExifThumbnail> and <A TIOParamsVals.RAW_HalfSize> for Camera Raw files

You can also set <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images.

Returns True if loading was successful, or False on error.

<FM>Example<FC>                   
// Load the specified image as fast as possible, but bigger than the screen dimensions
MyIEBitmap.IELoadFromFileFast('D:\MyImage.jpeg', Screen.Width, Screen.Height)
!!}
function TIEBitmapHelper.IELoadFromFileFast(const sFilename: string;
  iMaxX, iMaxY: integer;
  bAutoAdjustOrientation: Boolean = False
  ): Boolean;
var
  AImageEnIO: TImageEnIO;
begin
  try
    if (IEFilenameInExtensions(sFilename, '*.jpeg;*.jpg;*.jpe;') or
        IEFileIsOfFormat(sFilename, ioRaw) or
        IEFileIsOfFormat(sFilename, ioRawDLLPlugIn)) = False then
    begin
      Result := Read(sFilename);
    end
    else
    begin
      AImageEnIO := TImageEnIO.CreateFromBitmap(Self);
      Result := AImageEnIO.LoadFromFileFast(sFilename, iMaxX, iMaxY, bAutoAdjustOrientation);
      AImageEnIO.Free;
    end;
  except
    result := False;
  end;
end;


{!!
<FS>TIEBitmap.LoadFromURL

<FM>Declaration<FC>
function LoadFromURL(const URL            : WideString;
                     const sProxyAddress  : WideString = '';
                     const sProxyUser     : WideString = '';
                     const sProxyPassword : WideString = ''
                     ): Boolean;

<FM>Description<FN>
Load an image from the internet using the HTTP or FTP protocol.

Note: See documentation for <L TImageEnIO.LoadFromURL>TImageEnIO.LoadFromURL</L>

<FM>Example<FC>
MyBitmapLoadFromURL('http://www.imageen.com/image.jpg');
!!}
function TIEBitmapHelper.LoadFromURL(const URL            : WideString;
                                     const sProxyAddress  : WideString = '';
                                     const sProxyUser     : WideString = '';
                                     const sProxyPassword : WideString = ''
                                     ): Boolean;
var
  AImageEnIO: TImageEnIO;
begin
  try
    AImageEnIO := TImageEnIO.CreateFromBitmap(Self);

    AImageEnIO.ProxyAddress  := sProxyAddress;
    AImageEnIO.ProxyUser     := sProxyUser;
    AImageEnIO.ProxyPassword := sProxyPassword;

    Result := AImageEnIO.LoadFromURL(URL);
    AImageEnIO.Free;
  except
    result := False;
  end;
end;

{!!
<FS>TIEBitmap.IESaveToFile

<FM>Declaration<FC>
function IESaveToFile(const sFilename : string; iJPEGQuality : Integer): Boolean;

<FM>Description<FN>
Allows a TIEBitmap to save to any format supported by ImageEn. If you are saving to JPEG you can also specify the <A TIOParamsVals.JPEG_Quality>.
Returns True if saving was successful, or False on error

<FM>Example<FC>    
// Save the image at 90% quality
MyIEBitmap.IESaveToFile(SavePictureDialog1.FileName, 90)
!!}
function TIEBitmapHelper.IESaveToFile(const sFilename: string; iJPEGQuality: Integer): Boolean;
var
  AImageEnIO: TImageEnIO;
begin
  try
    if IEFilenameInExtensions(sFilename, '*.jpeg;*.jpg;*.jpe;') = False then
    begin
      Result := Write(sFilename);
    end
    else
    begin
      AImageEnIO := TImageEnIO.CreateFromBitmap(Self);
      Result := AImageEnIO.SaveToFileEx(sFilename, iJPEGQuality);
      AImageEnIO.Free;
    end;
  except
    Result := False;
  end;
end;




{!!
<FS>TIEBitmap.IELoadAsThumbnail

<FM>Declaration<FC>
function IELoadAsThumbnail(const sFilename: string;
                           iMaxX, iMaxY: integer;
                           bCanStretch: Boolean;
                           bAutoAdjustOrientation: Boolean = False;
                           QualityFilter : <A TResampleFilter> = rfLanczos3;

                           bAddBorder: Boolean = False;
                           cBorderColor: TColor = clBlack;

                           bAddShadow: Boolean = False;
                           iBlurRadius : Integer = 4;
                           iShadowOffset : Integer = 4;
                           cShadowColor: TColor = clBlack;
                           cBGColor: TColor = clWhite
                           ) : Boolean;

<FM>Description<FN>
Loads an image of any format into a TIEBitmap reducing it to the specified size (while maintaining the aspect ratio, thus one of the dimensions is likely to be less than the specified value).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>iMaxX, iMaxY<FN></C> <C>The maximum size of the new image (as the aspect ratio is maintained, one of the dimensions is likely to end upless than the specified value)</C> </R>
<R> <C><FC>bCanStretch<FN></C> <C>Set to false to avoid images smaller than iMaxX x iMaxY from being made larger</C> </R>
<R> <C><FC>bAutoAdjustOrientation<FN></C> <C>Sets <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images</C> </R>
<R> <C><FC>QualityFilter<FN></C> <C>Specify the quality that is used for rescaling the image</C> </R>
<R> <C><FC>bAddBorder<FN></C> <C>Set to true to add a 1 pixel border to the thumbnail</C> </R>
<R> <C><FC>cBorderColor<FN></C> <C>The color of the added border</C> </R>
<R> <C><FC>bAddShadow<FN></C> <C>Add a solid or soft shadow to the image</C> </R>
<R> <C><FC>iBlurRadius<FN></C> <C>Set to 0 to add a solid shadow or any other value for the width of the <L TImageEnProc.AddSoftShadow>Soft Shadow</L></C> </R>
<R> <C><FC>iShadowOffset<FN></C> <C>The <L TImageEnProc.AddSoftShadow>offset</L> of the shadow from the image</C> </R>
<R> <C><FC>cShadowColor<FN></C> <C>The shadow color</C> </R>
<R> <C><FC>cBGColor<FN></C> <C>The color of the image behind the shadow</C> </R>
</TABLE>

<FM>Example<FC>                 
// Load an image at the size 160x120
MyIEBitmap.IELoadAsThumbnail('D:\MyImage.jpeg', 160, 120, False);
!!}
function TIEBitmapHelper.IELoadAsThumbnail(const sFilename: string;
                                           iMaxX, iMaxY: integer;
                                           bCanStretch: Boolean;
                                           bAutoAdjustOrientation: Boolean = False;
                                           QualityFilter: TResampleFilter = rfLanczos3;

                                           bAddBorder: Boolean = False;
                                           cBorderColor: TColor = clBlack;

                                           bAddShadow: Boolean = False;
                                           iBlurRadius: Integer = 4;
                                           iShadowOffset: Integer = 4;
                                           cShadowColor: TColor = clBlack;
                                           cBGColor: TColor = clWhite
                                           ): Boolean;
begin
  try
    Result := IELoadFromFileFast(sFilename, iMaxX, iMaxY, bAutoAdjustOrientation);
    if Result then
      IEConvertToThumbnail(iMaxX, iMaxY, bCanStretch, QualityFilter,
        bAddBorder, cBorderColor,
        bAddShadow, iBlurRadius, iShadowOffset, cShadowColor, cBGColor);
  except
    Result := False;
  end;
end;

procedure TIEBitmapHelper.ConvertTo24Bit;
var
  AImageEnProc: TImageEnProc;
begin
  AImageEnProc := TImageEnProc.CreateFromBitmap(Self);
  try
    AImageEnProc.ConvertTo24Bit;
  finally
    AImageEnProc.Free;
  end;
end;


{!!
<FS>TIEBitmap.IEConvertToThumbnail

<FM>Declaration<FC>

procedure IEConvertToThumbnail(iMaxX, iMaxY: integer;
                               bCanStretch: Boolean;
                               QualityFilter : <A TResampleFilter> = rfLanczos3;

                               bAddBorder: Boolean = False;
                               cBorderColor: TColor = clBlack;

                               bAddShadow: Boolean = False;
                               iBlurRadius : Integer = 4;
                               iShadowOffset : Integer = 4;
                               cShadowColor: TColor = clBlack;
                               cBGColor: TColor = clWhite);

<FM>Description<FN>
Resize an image in a TIEBitmap to the specified size (while maintaining the aspect ratio, thus one of the dimensions is likely to be less than the specified value).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>iMaxX, iMaxY<FN></C> <C>The maximum size of the new image (as the aspect ratio is maintained, one of the dimensions is likely to end upless than the specified value)</C> </R>
<R> <C><FC>bCanStretch<FN></C> <C>Set to false to avoid images smaller than iMaxX x iMaxY from being made larger</C> </R>
<R> <C><FC>bAutoAdjustOrientation<FN></C> <C>Sets <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images</C> </R>
<R> <C><FC>QualityFilter<FN></C> <C>Specify the quality that is used for rescaling the image</C> </R>
<R> <C><FC>bAddBorder<FN></C> <C>Set to true to add a 1 pixel border to the thumbnail</C> </R>
<R> <C><FC>cBorderColor<FN></C> <C>The color of the added border</C> </R>
<R> <C><FC>bAddShadow<FN></C> <C>Add a solid or soft shadow to the image</C> </R>
<R> <C><FC>iBlurRadius<FN></C> <C>Set to 0 to add a solid shadow or any other value for the width of the <L TImageEnProc.AddSoftShadow>Soft Shadow</L></C> </R>
<R> <C><FC>iShadowOffset<FN></C> <C>The <L TImageEnProc.AddSoftShadow>offset</L> of the shadow from the image</C> </R>
<R> <C><FC>cShadowColor<FN></C> <C>The shadow color</C> </R>
<R> <C><FC>cBGColor<FN></C> <C>The color of the image behind the shadow</C> </R>
</TABLE>

<FM>Example<FC>
// Resize the IEBitmap to 160x120 with a black border
MyIEBitmap.IEConvertToThumbnail(160, 120, True, rfFastLinear, True, clBlack);
!!}
procedure TIEBitmapHelper.IEConvertToThumbnail(iMaxX, iMaxY: integer;
                                               bCanStretch: Boolean;
                                               QualityFilter: TResampleFilter = rfLanczos3;

                                               bAddBorder: Boolean = False;
                                               cBorderColor: TColor = clBlack;

                                               bAddShadow: Boolean = False;
                                               iBlurRadius: Integer = 4;
                                               iShadowOffset: Integer = 4;
                                               cShadowColor: TColor = clBlack;
                                               cBGColor: TColor = clWhite);
var
  ASize: tpoint;
  bUseSoftShadow: Boolean;
begin
  bUseSoftShadow := iBlurRadius > 0;

  // if a shadow is to be added remove the size of the shadow from the bitmap dimension
  if bAddShadow and bUseSoftShadow then
  begin
    Dec(iMaxX, IESoftShadowSize(iBlurRadius, iShadowOffset, iShadowOffset));
    Dec(iMaxY, IESoftShadowSize(iBlurRadius, iShadowOffset, iShadowOffset));
  end
  else
  if bAddShadow and (bUseSoftShadow = false) then
  begin
    Dec(iMaxX, iShadowOffset);
    Dec(iMaxY, iShadowOffset);
  end;

  if baddshadow or bAddBorder then
    ConvertTo24Bit;

  // RESIZE
  ASize := GetImageSizeWithinArea(Width, Height, iMaxX, iMaxY);
  if bCanStretch or (Width > ASize.X) then
    Resample(ASize.X, ASize.Y, QualityFilter, false);

  // BORDER
  if bAddBorder then
  begin
    Canvas.pen.color := cBorderColor;
    Canvas.Polyline([Point(0, 0),
      Point(Width - 1, 0),
        Point(Width - 1, Height - 1),
        Point(0, Height - 1),
        Point(0, 0)]);
  end;

  // SHADOW
  if bAddShadow then
  begin
    if bUseSoftShadow then
      IEAddSoftShadow(iBlurRadius, iShadowOffset, cBGColor, _loTopLeft, cShadowColor)
    else
    begin
      // add back the pixels we removed
      width := width + iShadowOffset;
      Height := Height + iShadowOffset;

      // Draw the shadowm
      Canvas.Brush.color := cShadowColor;

      // along the right
      Canvas.FillRect(Rect(Width - iShadowOffset, 0, Width, Height));

      // along the bottom
      Canvas.FillRect(Rect(0, Height - iShadowOffset, Width, Height));

      // now add the background colour
      Canvas.Brush.color := cBGColor;

      // at the top right
      Canvas.FillRect(Rect(Width - iShadowOffset, 0, Width, iShadowOffset));

      // at the bottom left
      Canvas.FillRect(Rect(0, Height - iShadowOffset, iShadowOffset, Height));
    end;
  end;
end;



{!!
<FS>TIEBitmap.IEAddSoftShadow

<FM>Declaration<FC>
procedure IEAddSoftShadow(iBlurRadius: integer;
                          iShadowOffset : Integer;
                          cBGColor: TColor;
                          LightOrigin: <A TLightOrigin> = _loTopLeft;
                          cShadowColor: TColor = clblack);

<FM>Description<FN>
Calls <A TImageEnProc.AddSoftShadow> to add a soft shadow to an image in a TIEBitmap

<FM>Example<FC>            
// Add a soft shadow to the bitmap image
MyIEBitmap.IEAddSoftShadow(3, 3, clWhite);
!!}
procedure TIEBitmapHelper.IEAddSoftShadow(iBlurRadius: integer;
                                          iShadowOffset: Integer;
                                          cBGColor: TColor;
                                          LightOrigin: TLightOrigin = _loTopLeft;
                                          cShadowColor: TColor = clblack);
var
  ie: TImageEnView;
  iOffSetX, iOffSetY: integer;
begin
  ie := TImageEnView.Create(nil);
  try
    ie.background := cBGColor;
    ie.IEBitmap.assign(Self);
    ie.update;
    iOffSetX := iShadowOffset;
    iOffSetY := iShadowOffset;
    if LightOrigin in [_loTopRight, _loBottomRight] then
      iOffSetX := -iOffSetX;
    if LightOrigin in [_loBottomLeft, _loBottomRight] then
      iOffSetY := -iOffSetY;

    ie.Proc.AddSoftShadow(iBlurRadius, iOffSetX, iOffSetY, TRUE, cShadowColor);
    ie.RemoveAlphaChannel(True);
    Self.assign(ie.IEbitmap);
  finally
    ie.Free;
  end;
end;






{!!
<FS>TIEBitmap.PrintImage

<FM>Declaration<FC>
procedure PrintImage(PrtCanvas: TCanvas = nil; MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1; VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER; Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0; GammaCorrection: double = 1; SubsampleFilter: <A TResampleFilter> = rfFastLinear);

<FM>Description<FN>
Print the current bitmap by specifying margins, vertical position, horizontal position and size.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>PrtCanvas<FN></C> <C>The canvas to bring to. Generally the application will pass this as Printer.Canvas</C> </R>
<R> <C><FC>MarginLeft<FN></C> <C>Left page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>MarginTop<FN></C> <C>Top page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>MarginRight<FN></C> <C>Right page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>MarginBottom<FN></C> <C>Bottom page margin in inches (Specify zero for no margin)</C> </R>
<R> <C><FC>VerticalPos<FN></C> <C>How the image is vertically aligned on the page</C> </R>
<R> <C><FC>HorizontalPos<FN></C> <C>How the image horizontally aligned on the page</C> </R>
<R> <C><FC>Size<FN></C> <C>The size to print the image</C> </R>
<R> <C><FC>SpecWidth<FN></C> <C>The absolute width of the image in inches (Valid only if Size = iesSPECIFIEDSIZE)</C> </R>
<R> <C><FC>SpecHeight<FN></C> <C>The absolute height of the image in inches (Valid only if Size = iesSPECIFIEDSIZE)</C> </R>
<R> <C><FC>GammaCorrection<FN></C> <C>The gamma correction value (Specify 1.0 to disable gamma correction)</C> </R>
<R> <C><FC>SubsampleFilter<FN></C> <C>The filter that is used to enhance quality if hte image needs to be resampled for printing</C> </R>
</TABLE>


<FM>Example<FC>
// Print the image in the center of the page at the original size
Printer.BeginDoc;
ABitmap.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesNORMAL, 0, 0, 1);
Printer.EndDoc;

// Print the image in the center of the page stretched to page dimensions (respecting the proportions)
Printer.BeginDoc;
ABitmap.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE, 0, 0, 1);
Printer.EndDoc;

!!}

procedure TIEBitmapHelper.PrintImage(PrtCanvas: TCanvas = nil;
                                     MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1;
                                     VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER;
                                     Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0;
                                     GammaCorrection: double = 1;
                                     SubsampleFilter: TResampleFilter = rfFastLinear);
var
  AImageEnIO: TImageEnIO;
begin
  AImageEnIO := TImageEnIO.CreateFromBitmap(Self);
  try
    AImageEnIO.PrintingFilterOnSubsampling := SubsampleFilter;
    AImageEnIO.PrintImage(PrtCanvas, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
  finally
    AImageEnIO.Free;
  end;
end;
{$ENDIF}




// FILE FUNCTIONS

{!!
<FS>IEResampleImageFile

<FM>Declaration<FC>
function IEResampleImageFile(const sInFilename, sOutFilename: string;
                             iJpegQuality: Integer;
                             iMaxX: Integer;
                             iMaxY: Integer;
                             bCanStretch: Boolean = False;
                             QualityFilter: <A TResampleFilter> = rfLanczos3;
                             bAutoAdjustOrientation: Boolean = False
                             ): Boolean;

<FM>Description<FN>
Loads an image of any format, resizes it and saves it to file (of any format).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>iJPEGQuality<FN></C> <C>Specify the <A TIOParamsVals.JPEG_Quality> if sOutFilename is a JPEG</C> </R>
<R> <C><FC>iMaxX, iMaxY<FN></C> <C>The maximum size of the new image (as the aspect ratio is maintained, one of the dimensions is likely to end upless than the specified value)</C> </R>
<R> <C><FC>bCanStretch<FN></C> <C>Set to false to avoid images smaller than iMaxX x iMaxY from being made larger</C> </R>
<R> <C><FC>QualityFilter<FN></C> <C>Specify the quality that is used for rescaling the image</C> </R>
<R> <C><FC>bAutoAdjustOrientation<FN></C> <C>Sets <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images</C> </R>
</TABLE>

<FM>Example<FC>
// Resize MyImage.jpeg to the screen dimensions and save to MyImage_Screen.jpeg
IEResampleImageFile('D:\MyImage.jpeg', 'D:\MyImage_Screen.jpeg', 90, Screen.Width, Screen.Height, False);
!!}
{$IFDEF Delphi2005orNewer}
function IEResampleImageFile(const sInFilename, sOutFilename: string;
                             iJpegQuality: Integer;
                             iMaxX: Integer;
                             iMaxY: Integer;
                             bCanStretch: Boolean = False;
                             QualityFilter: TResampleFilter = rfLanczos3;
                             bAutoAdjustOrientation: Boolean = False
                             ): Boolean;
var
  ABitmap : TBitmap;   
  io: TImageEnIO;
  ASize: TPoint;          
  SaveCursor: TCursor;
begin
  result := true;
  saveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    ABitmap := TBitmap.create;
    io := TImageEnIO.CreateFromBitmap(ABitmap);                               
    try
      // Load full size for best quality (and avoid size discrepency if we are auto-rotating)
      if io.LoadFromFileFast(sInFilename, -1, -1, bAutoAdjustOrientation) = False then
        raise EIEException.create('Load error');

      if bCanStretch or (ABitmap.width > iMaxX) or (ABitmap.height > iMaxY) then
      begin
        // Resize the image
        ASize := GetImageSizeWithinArea(ABitmap.width, ABitmap.height, iMaxX, iMaxY);
        ABitmap.IEResample(ASize.X, ASize.Y, QualityFilter);
      end;

      if not io.SaveToFileEx(sOutFilename, iJpegQuality) then
        raise EIEException.create('Save Error');

    finally
      io.Free;
      ABitmap.free;     
      Screen.Cursor := saveCursor;
    end;
  except
    result := false;
  end;
end;    
{$ENDIF}


{!!
<FS>IEConvertImageFile

<FM>Declaration<FC>
function IEConvertImageFile(const sInFilename, sOutFilename: string;
                            iJpegQuality: Integer;
                            bAutoAdjustOrientation: Boolean = False
                            ): Boolean;

<FM>Description<FN>
Change the format of a file (e.g. from BMP to JPEG). Specify the <A TIOParamsVals.JPEG_Quality> if sOutFilename is a JPEG. Returns False if an error was encountered.

<FM>Example<FC>
// Convert MyImage.jpeg to a BMP file
IEConvertImageFile('D:\MyImage.jpeg', 'D:\MyImage.bmp', 90);
!!}
{$IFDEF Delphi2005orNewer}
function IEConvertImageFile(const sInFilename, sOutFilename: string;
                            iJpegQuality: Integer;
                            bAutoAdjustOrientation: Boolean = False
                            ): Boolean;
var
  ABitmap: TIEBitmap;
  io: TImageEnIO;
  SaveCursor: TCursor;
begin
  Result := True;
  if sInFilename = sOutFilename then
    exit;

  saveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    ABitmap := TIEBitmap.create;
    io := TImageEnIO.CreateFromBitmap(ABitmap);
    try
      if io.LoadFromFileFast(sInFilename, -1, -1, bAutoAdjustOrientation) = False then
        raise EIEException.create('Load error');

      if bAutoAdjustOrientation then
        io.params.EXIF_Orientation := _exoCorrectOrientation;

      if io.SaveToFileEx(sOutFilename, iJpegQuality) = False then
        raise EIEException.create('Save error');

    finally
      io.Free;
      ABitmap.free;
      Screen.Cursor := saveCursor;
    end;
  except
    result := false;
  end;
end;
{$ENDIF}


{!!
<FS>IECreateThumbnailFromFile

<FM>Declaration<FC>
function IECreateThumbnailFromFile(const sInFilename: string;
                                   const sOutFilename: string;
                                   iMaxX, iMaxY: integer;
                                   bCanStretch: Boolean;
                                   iJPEGQuality: integer;
                                   bAutoAdjustOrientation: Boolean = False;
                                   QualityFilter : <A TResampleFilter> = rfLanczos3;

                                   bAddBorder: Boolean = False;
                                   cBorderColor: TColor = clBlack;

                                   bAddShadow: Boolean = False;
                                   iBlurRadius : Integer = 4;
                                   iShadowOffset : Integer = 4;
                                   cShadowColor: TColor = clBlack;
                                   cBGColor: TColor = clWhite
                                   ): Boolean;

<FM>Description<FN>
Creates a thumbnail from an image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>iMaxX, iMaxY<FN></C> <C>The maximum size of the new image (as the aspect ratio is maintained, one of the dimensions is likely to end upless than the specified value)</C> </R>
<R> <C><FC>bCanStretch<FN></C> <C>Set to false to avoid images smaller than iMaxX x iMaxY from being made larger</C> </R>
<R> <C><FC>bAutoAdjustOrientation<FN></C> <C>Sets <A TIOParamsVals.EnableAdjustOrientation> to automatically re-orient JPEG camera images</C> </R>
<R> <C><FC>QualityFilter<FN></C> <C>Specify the quality that is used for rescaling the image</C> </R>
<R> <C><FC>bAddBorder<FN></C> <C>Set to true to add a 1 pixel border to the thumbnail</C> </R>
<R> <C><FC>cBorderColor<FN></C> <C>The color of the added border</C> </R>
<R> <C><FC>bAddShadow<FN></C> <C>Add a solid or soft shadow to the image</C> </R>
<R> <C><FC>iBlurRadius<FN></C> <C>Set to 0 to add a solid shadow or any other value for the width of the <L TImageEnProc.AddSoftShadow>Soft Shadow</L></C> </R>
<R> <C><FC>iShadowOffset<FN></C> <C>The <L TImageEnProc.AddSoftShadow>offset</L> of the shadow from the image</C> </R>
<R> <C><FC>cShadowColor<FN></C> <C>The shadow color</C> </R>
<R> <C><FC>cBGColor<FN></C> <C>The color of the image behind the shadow</C> </R>
</TABLE>

<FM>Example<FC>
// Create a thumbnail of MyImage.jpeg at size 160x120 and save to MyImage_Thumb.jpeg
IECreateThumbnailFromFile('D:\MyImage.jpeg', 'D:\MyImage_Thumb.jpeg', 160, 120, False, 75)
!!}
{$IFDEF Delphi2005orNewer}
function IECreateThumbnailFromFile(const sInFilename: string;
                                   const sOutFilename: string;
                                   iMaxX, iMaxY: integer;
                                   bCanStretch: Boolean;
                                   iJPEGQuality: integer;
                                   bAutoAdjustOrientation: Boolean = False;
                                   QualityFilter : TResampleFilter = rfLanczos3;

                                   bAddBorder: Boolean = False;
                                   cBorderColor: TColor = clBlack;

                                   bAddShadow: Boolean = False;
                                   iBlurRadius : Integer = 4;
                                   iShadowOffset : Integer = 4;
                                   cShadowColor: TColor = clBlack;
                                   cBGColor: TColor = clWhite
                                   ): Boolean;
var
  ABitmap: TBitmap;                    
  SaveCursor: TCursor;
begin
  result := True;
  saveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    ABitmap := TBitmap.create;
    try
      if ABitmap.IELoadAsThumbnail(sInFilename,
                                   iMaxX, iMaxY, bCanStretch,
                                   bAutoAdjustOrientation, QualityFilter,
                                   bAddBorder, cBorderColor,
                                   bAddShadow, iBlurRadius, iShadowOffset, cShadowColor, cBGColor) = False then
        raise EIEException.create('Thumb creation error');

      if not ABitmap.IESaveToFile(sOutFilename, iJpegQuality) then
        raise EIEException.create('Save Error');
    finally
      ABitmap.free;
      Screen.Cursor := saveCursor;
    end;
  except
    result := false;
  end;
end;
{$ENDIF}


{!!
<FS>GetFileDetails

<FM>Declaration<FC>
function GetFileDetails(const sFilename: string;
                        out iFileSize: Int64;
                        out dtCreationDate: TDateTime;
                        out dtModifiedDate: TDateTime
                        ): Boolean;


<FM>Description<FN>
Uses FindFirstFile to find the size and dates of a file.

<FM>Example<FC>
// Get file details for MyImage.jpeg
GetFileDetails('D:\MyImage.jpeg', iFileSize, aCreationDate, aModifiedDate);
!!}
function GetFileDetails(const sFilename: string;
                        out iFileSizeBytes: Int64;
                        out dtCreationDate: TDateTime;
                        out dtModifiedDate: TDateTime
                        ): Boolean;
begin
  result := IEGetFileDetails(sFilename, iFileSizeBytes, dtCreationDate, dtModifiedDate);
end;




{!!
<FS>GetImageDetails

<FM>Declaration<FC>
function GetImageDetails(const sFilename: string;
                         out iWidth: Integer;
                         out iHeight: Integer;
                         out iBitsPerPixel: Integer
                         ): Boolean; overload;
function GetImageDetails(const sFilename: string): TPoint; overload;


<FM>Description<FN>
Return the size and color depth of an image. Result is false if a load error occurs.
The second overload only returns only the width and height (which will be -1,-1 if a load error is encountered).

Note: Color depth is TImageEnIO.Params.BitsPerSample * TImageEnIO.Params.SamplesPerPixel.

See also: <A BitsPerPixelToStr>

<FM>Example<FC>
if GetImageDetails('D:\MyImage.jpeg', iWidth, iHeight, iBitsPerPixel) then
begin
  lblWidth.Caption := IntToStr(iWidth);
  lblHeight.Caption := IntToStr(iHeight);
  lblColorDepth.Caption := BitsPerPixelToStr(iBitsPerPixel);
end;
!!}

function GetImageDetails(const sFilename: string;
                         out iWidth: Integer;
                         out iHeight: Integer;
                         out iBitsPerPixel: Integer
                         ): Boolean;
var
  io: TImageEnIO;
begin
  result  := False;
  iWidth  := 0;
  iHeight := 0;
  iBitsPerPixel := 0;
  try
    io := TImageEnIO.Create(nil);
    try
      io.ParamsFromFile(sFilename);
      if (io.Aborting = False) and (io.params.Width > 0) then
      begin             
        result  := True;
        iWidth  := io.params.Width;
        iHeight := io.params.Height;
        iBitsPerPixel :=  io.Params.BitsPerSample * io.Params.SamplesPerPixel;
      end; 
    finally
      io.free;
    end;
  except
    // LOAD ERROR
  end;
end;


function GetImageDetails(const sFilename: string): TPoint;
var
  iWidth: Integer;
  iHeight: Integer;
  iBitsPerPixel: Integer;
begin
  Result.x := -1;
  Result.y := -1;
  if GetImageDetails(sFilename, iWidth, iHeight, iBitsPerPixel) then
  begin
    Result.x := iWidth;
    Result.y := iHeight;
  end;
end;



{!!
<FS>GetExifOrFileCreationDate

<FM>Declaration<FC>
function GetExifOrFileCreationDate(const sFilename: string;
                                   bReturnCreateDate: Boolean = true
                                   ): TDateTime;

<FM>Description<FN>
Calls <A TIOParamsVals.EXIF_DateTimeOriginal> to retrieve the creation date specified in a camera image. If the image does not contain a date, you can optionally return the Windows File Creation Date.

<FM>Example<FC>       
// Get digital camera date for for MyImage.jpeg
aCreateDate := GetExifOrFileCreationDate('D:\MyImage.jpeg', True);
!!}
function GetExifOrFileCreationDate(const sFilename: string;
                                   bReturnCreateDate: Boolean = true  {if true then the create date is returned if there is no exif date}
                                   ): TDateTime;
var
  iFileSizeBytes: Int64;
  dtModifiedDate: TDateTime;
  io: TImageEnIO;
begin
  result := 0;
  try
    if IEFileIsOfFormat(sFilename, ioJPEG) then
    try
      io := TImageEnIO.Create(nil);
      try
        io.ParamsFromFile(sFilename);
        if (io.Aborting = False) and (io.params.EXIF_HasEXIFData) then
          Result := EXIFDateToDateTime(string(io.params.EXIF_DateTimeOriginal));
      finally
        io.free;
      end;
    except
      // LOAD ERROR
    end;

    if (Result < 1) and bReturnCreateDate then
      GetFileDetails(sFilename, iFileSizeBytes, Result, dtModifiedDate);

  except
    // GetFileDateTime Error
  end;
end;


{!!
<FS>JPEGLosslessRotateFile

<FM>Declaration<FC>
function JPEGLosslessRotateFile(const sInFilename : WideString; const sOutFilename : WideString; iRotateAngle : integer) : Boolean; overload;
function JPEGLosslessRotateFile(const sFilename : WideString; iRotateAngle : integer) : Boolean; overload;

<FM>Description<FN>
Calls <A JpegLosslessTransform> or <A JpegLosslessTransform2> to losslessly rotate a JPEG image, but allows you to specify a rotation angle that matches the <A TImageEnProc.Rotate> Angle parameter.

Note: All comments and markers are copied and the EXIF orientation and thumbnail are updated. All values other than -90, 90, 180, -180, 270 and -270 are ignored

<FM>Example<FC>
// Rotate MyImage.jpeg 90 deg. counter-clockwise and save to MyImage_ROT.jpeg
JPEGLosslessRotateFile('D:\MyImage.jpeg', 'D:\MyImage_ROT.jpeg', 90);
!!}
function JPEGLosslessRotateFile(const sInFilename : WideString; const sOutFilename : WideString; iRotateAngle : integer) : Boolean;
const
  Update_EXIF_On_Transform = True;  // write updated orientation detail and rotate thumbnail
var
  SaveCursor: TCursor;
  RotType: TIEJpegTransform;
begin
  // Note: ImageEn Rotation is the opposite of JpegLosslessTransform
  case iRotateAngle of
    270, -90  : RotType := jtRotate90;
    180, -180 : RotType := jtRotate180;
    90,  -270 : RotType := jtRotate270;
  else                              {90}
     raise EIEException.create('Unsupported lossless rotation angle');
  end;

  saveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if (sOutFilename = '') or SameText(sInFilename, sOutFilename) then
      result := JpegLosslessTransform2(sInFilename, RotType, false, jcCopyAll, rect(0, 0, 0, 0), Update_EXIF_On_Transform)
    else
      result := JpegLosslessTransform(sInFilename, sOutFilename, RotType, false, jcCopyAll, rect(0, 0, 0, 0), Update_EXIF_On_Transform);
  finally
    Screen.Cursor := saveCursor;
  end;
end;


function JPEGLosslessRotateFile(const sFilename : WideString; iRotateAngle : integer) : Boolean;
begin
  Result := JPEGLosslessRotateFile(sFilename, '', iRotateAngle);
end;




{!!
<FS>JPEGLosslessFlipFile

<FM>Declaration<FC>
function JPEGLosslessFlipFile(const sInFilename : WideString; const sOutFilename : WideString; Direction: TFlipDir) : Boolean; overload;
function JPEGLosslessFlipFile(const sFilename : WideString; Direction: TFlipDir) : Boolean; overload;

<FM>Description<FN>
Calls <A JpegLosslessTransform> or <A JpegLosslessTransform2> to losslessly flip a JPEG image.

Note: All comments and markers are copied and the EXIF orientation and thumbnail are updated.

<FM>Example<FC>
// Flip MyImage.jpeg horizontally
JPEGLosslessRotateFile('D:\MyImage.jpeg', fdHorizontal);
!!}
function JPEGLosslessFlipFile(const sInFilename : WideString; const sOutFilename : WideString; Direction: TFlipDir) : Boolean;
const
  Update_EXIF_On_Transform = True;  // write updated orientation detail and rotate thumbnail
var
  SaveCursor: TCursor;
  RotType: TIEJpegTransform;
begin
  saveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if Direction = fdVertical then
      RotType := jtVertFlip
    else
      RotType := jtHorizFlip;
  
    if (sOutFilename = '') or SameText(sInFilename, sOutFilename) then
      result := JpegLosslessTransform2(sInFilename, RotType, false, jcCopyAll, rect(0, 0, 0, 0), Update_EXIF_On_Transform)
    else
      result := JpegLosslessTransform(sInFilename, sOutFilename, RotType, false, jcCopyAll, rect(0, 0, 0, 0), Update_EXIF_On_Transform);
  finally
    Screen.Cursor := saveCursor;
  end;
end;


function JPEGLosslessFlipFile(const sFilename : WideString; Direction: TFlipDir) : Boolean;
begin
  Result := JPEGLosslessFlipFile(sFilename, '', Direction);
end;




{!!
<FS>IERotateImageFile

<FM>Declaration<FC>
function IERotateImageFile(const sFilename : string;
                           iJpegQuality : integer;
                           iRotateAngle : integer;
                           AntiAliasMode: TIEAntialiasMode = ierFast;
                           bCanUseLossless: Boolean = true;     
                           cBackgroundColor: TColor = clWhite): Boolean; overload;
function IERotateImageFile(const sInFilename, sOutFilename : string;
                           iJpegQuality : integer;
                           iRotateAngle : integer;
                           AntiAliasMode: TIEAntialiasMode = ierFast;
                           bCanUseLossless: Boolean = true;
                           cBackgroundColor: TColor = clWhite): Boolean; overload;

<FM>Description<FN>
Calls <A TIEBitmap.Rotate> to rotate an image file and resave it (optionally to an alternative file). if bCanUseLossless is specified then <A JpegLosslessTransform2> is used where possible for JPEG files.

See also: <A AngleToImageEnRotateAngle>

<FM>Example<FC>
// This will result in a lossless rotation
IERotateImageFile('D:\MyImage.jpg', 90, ierFast, True);
                                                                
// This will be a lossy rotation
IERotateImageFile('D:\MyImage.jpg', 45, ierFast, True, clBlack);
!!}
{$IFDEF Delphi2005orNewer}
function IERotateImageFile(const sInFilename, sOutFilename : string;
                           iJpegQuality : integer;
                           iRotateAngle : integer;
                           AntiAliasMode: TIEAntialiasMode = ierFast;
                           bCanUseLossless: Boolean = true;      // if true then a lossless Flip is used for JPEG images
                           cBackgroundColor: TColor = clWhite): Boolean;
var
  ABitmap: TIEBitmap;
  io: TImageEnIO;
  saveCursor : TCursor;
begin
  result := True;

  if iRotateAngle = 0 then
  begin
    // Result will not be valid (exist) if we we are outputting a different file
    Result := SameText(sInFilename, sOutFilename);
    exit;
  end;

  // can we rotate it losslessly?
  if bCanUseLossless and
     IEFileIsOfFormat(sInFilename, ioJPEG) and
     IEFileIsOfFormat(sOutFilename, ioJPEG) and
     (iRotateAngle mod 90 = 0 {i.e. =90, 180 or 270}) then
  begin
    // rotate the image losslessly
    Result := JPEGLosslessRotateFile(sInFilename, sOutFilename, iRotateAngle);
    exit;
  end;

  try
    saveCursor := Screen.Cursor;
    ABitmap := TIEBitmap.create;
    io := TImageEnIO.CreateFromBitmap(ABitmap);
    try
      Screen.Cursor := crHourGlass;
      if io.LoadFromFileEx(sInFilename, False) = False then
        raise EIEException.create('Load Error');

      ABitmap.Rotate(iRotateAngle, AntialiasMode, cBackgroundColor);

      if io.SaveToFileEx(sOutFilename, iJpegQuality) = False then
        raise EIEException.create('Save Error');
    finally
      io.free;
      ABitmap.free;
      Screen.Cursor := saveCursor;
    end;
  except
    result := false;
  end;
end;

function IERotateImageFile(const sFilename  : string;   
                           iJpegQuality : integer;
                           iRotateAngle : integer;
                           AntiAliasMode: TIEAntialiasMode = ierFast;
                           bCanUseLossless: Boolean = true;      // if true then a lossless Flip is used for JPEG images
                           cBackgroundColor: TColor = clWhite): Boolean;
begin
  Result := IERotateImageFile(sFilename, sFilename, iJpegQuality, iRotateAngle, AntiAliasMode, bCanUseLossless, cBackgroundColor);
end;

{$ENDIF}




{!!
<FS>IEFlipImageFile

<FM>Declaration<FC>
function IEFlipImageFile(const sFilename : string;
                         iJpegQuality : integer;
                         Direction: TFlipDir;
                         bCanUseLossless: Boolean = true 
                         ): Boolean; overload;
function IEFlipImageFile(const sInFilename, sOutFilename : string;
                         iJpegQuality : integer;
                         Direction: TFlipDir;
                         bCanUseLossless: Boolean = true 
                         ): Boolean; overload;

<FM>Description<FN>
Calls <A TIEBitmap.Flip> to Flip an image file and resave it (optionally to an alternative file). if bCanUseLossless is specified then <A JpegLosslessTransform2> is used where possible for JPEG files.

<FM>Example<FC>
// Flip an image horizontally and save to a JPEG
IEFlipImageFile('D:\Source.bmp', 'D:\MyImage.jpg', 90, fdHorizontal);

// Flip an image vertically. This will be lossless as source and destination are JPEG
IEFlipImageFile('D:\MyJPEG.jpg', 90, fdVertical);
!!}
{$IFDEF Delphi2005orNewer}
function IEFlipImageFile(const sInFilename, sOutFilename : string;   
                         iJpegQuality : integer;
                         Direction: TFlipDir;
                         bCanUseLossless: Boolean = true 
                         ): Boolean;
var
  ABitmap: TIEBitmap;
  io: TImageEnIO;
  saveCursor : TCursor;
begin
  result := True;

  // can we Flip it losslessly?
  if bCanUseLossless and
     IEFileIsOfFormat(sInFilename, ioJPEG) and
     IEFileIsOfFormat(sOutFilename, ioJPEG) then
  begin
    // Flip the image losslessly
    Result := JPEGLosslessFlipFile(sInFilename, sOutFilename, Direction);
    exit;
  end;

  try
    saveCursor := Screen.Cursor;
    ABitmap := TIEBitmap.create;
    io := TImageEnIO.CreateFromBitmap(ABitmap);
    try
      Screen.Cursor := crHourGlass;
      if io.LoadFromFileEx(sInFilename, False) = False then
        raise EIEException.create('Load Error');

      ABitmap.Flip(Direction);

      if io.SaveToFileEx(sOutFilename, iJpegQuality) = False then
        raise EIEException.create('Save Error');
    finally
      io.free;
      ABitmap.free;
      Screen.Cursor := saveCursor;
    end;
  except
    result := false;
  end;
end;

function IEFlipImageFile(const sFilename  : string;
                         iJpegQuality : integer;
                         Direction: TFlipDir;
                         bCanUseLossless: Boolean = true 
                         ): Boolean; overload;
begin
  Result := IEFlipImageFile(sFilename, sFilename, iJpegQuality, Direction, bCanUseLossless);
end;
{$ENDIF}



{!!
<FS>IEReadCorrectOrientationOfImageFile

<FM>Declaration<FC>
function IEReadCorrectOrientationOfImageFile(const sFilename : string; bConservativeChecking : Boolean = True) : Integer;

<FM>Description<FN>
Upmarket digital cameras will automatically store the correct orientation of an image when the camera is rotated to take a portrait photo. This function calls <A TIOParamsVals.EXIF_Orientation> to retrieve the desired orientation for a JPEG camera image.
If bConservativeChecking is true it performs some further checking to ensure the returned value is accurate (e.g. to avoid problems where the photo has already been rotated in another program).

<FM>Example<FC>
iBestOrientation := IEReadCorrectOrientationOfImageFile('D:\MyImage.jpeg', True);
!!}

// checks the EXIF orientation flag of the image to see whether it needs rotating
{
For a file returns:
-1 : Error
 0 : Is not a JPEG
_exoCorrectOrientation : Doesn't need rotating
Or: _exoNeeds90RotateCW, _exoNeeds180Rotate, _exoNeeds270RotateCW

Here is an explanation of EXIF Rotate Values:

  1        2       3      4         5            6           7          8

888888  888888      88  88      8888888888  88                  88  8888888888
88          88      88  88      88  88      88  88          88  88      88  88
8888      8888    8888  8888    88          8888888888  8888888888          88
88          88      88  88
88          88  888888  888888
}

function IEReadCorrectOrientationOfImageFile(const sFilename : string; bConservativeChecking : Boolean = True) : Integer;
var
  io: TImageEnIO;
  SaveCursor: TCursor;
  bIsPortraitImage: Boolean;
  bSizeMatchesFields: Boolean;
begin
  Result := 0;

  // if it is not a JPEG then it can't be processed
  if IEFileIsOfFormat(sFilename, ioJPEG) = False then
    exit;

  try
    saveCursor := Screen.Cursor;
    io := TImageEnIO.Create(nil);
    try
      Screen.Cursor := crHourGlass;
      io.ParamsFromFile(sFilename);
      if io.Aborting then
        raise EIEException.create('Load Error');

      if io.params.EXIF_HasEXIFData then
      begin
        Result := io.params.EXIF_Orientation;

        // check to see if hte image dimensions match those in the EXIF data
        if (bConservativeChecking) and (Result <> _exoCorrectOrientation) then
        begin
          bIsPortraitImage := io.params.Height > io.params.Width;
          bSizeMatchesFields := (io.params.EXIF_EXIFImageWidth = io.params.width) and (io.params.EXIF_EXIFImageHeight = io.params.Height);

          if (Result in [_exoNeeds90RotateCW, _exoNeeds270RotateCW]) and bIsPortraitImage then
            // camera photos are already landscape so it looks like this one is already rotated
            result := _exoCorrectOrientation
          else
          if bSizeMatchesFields = False then
            result := _exoCorrectOrientation;
        end;

        // The following are not used by cameras so just report as correct
        if result in [_exoNeedsHorizontalFlip, _exoNeedsVerticalFlip, _exoNeedsHorzAndVertFlip, _exoNeedsFlipHorzAnd90Rotate] then
          result := _exoCorrectOrientation;
       end;
    finally
      io.free;
      Screen.Cursor := saveCursor;
    end;
  except
    Result := -1;
  end;
end;



{!!
<FS>IEAutomaticallyRotateImageFile

<FM>Declaration<FC>
function IEAutomaticallyRotateImageFile(const sFilename : string) : Integer;

<FM>Description<FN>
Upmarket digital cameras will automatically store the correct orientation of an image when the camera is rotated to take a portrait photo. This function calls <A TIOParamsVals.EXIF_Orientation> to retrieve the desired orientation for a JPEG camera image and then calls <A JpegLosslessTransform2> to losslessly rotate it.
If bConservativeChecking is true it performs some further checking to ensure the orientation data is accurate (e.g. to avoid problems where the photo has already been rotated in another program).

<FM>Example<FC>
// Rotate MyImage.jpeg if it is not correctly oriented
IEAutomaticallyRotateImageFile('D:\MyImage.jpeg', True);
!!}
function IEAutomaticallyRotateImageFile(const sFilename : string; bConservativeChecking : Boolean = True) : Integer;
var
  SaveCursor: TCursor;
  RotType: TIEJpegTransform;
begin
  saveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    Result := IEReadCorrectOrientationOfImageFile(sFilename, bConservativeChecking);

    // Needs rotation?
    if Result in [_exoNeeds90RotateCW, _exoNeeds180Rotate, _exoNeeds270RotateCW] then
    begin   
      // Note: ImageEn Rotation is the opposite of JpegLosslessTransform
      case Result of
        _exoNeeds90RotateCW  : RotType := jtRotate90;
        _exoNeeds180Rotate   : RotType := jtRotate180;
        _exoNeeds270RotateCW : RotType := jtRotate270;
        else exit; // avoid compiler warning
      end;

      if JpegLosslessTransform2(sFilename, RotType, false, jcCopyAll, rect(0, 0, 0, 0), True) = False then
        Result := -1;
    end;
  finally
    Screen.Cursor := saveCursor;
  end;
end;


{!!
<FS>BitsPerPixelToStr

<FM>Declaration<FC>
function BitsPerPixelToStr(iBitsPerPixel: Integer) : string;

<FM>Description<FN>
Provides a suitable textual representation of a Bits per Sample value (i.e. BitsPerSample * SamplesPerPixel). Example outputs: Monochrome, 128 colors, 24 bit color, etc.

<FM>Example<FC>
// Display the color depth of the currently displayed image
lblColorDepth.Caption := BitsPerPixelToStr(ImageEnView1.IO.Params.BitsPerSample * ImageEnView1.IO.Params.SamplesPerPixel);
!!}
function BitsPerPixelToStr(iBitsPerPixel: Integer) : string;
var
  iColors: Integer;
begin    
  if iBitsPerPixel < 1 then
    result := ''
  else
  if iBitsPerPixel = 1 then
    result := 'Monochrome'
  else
  if iBitsPerPixel <= 8 then
  begin
    iColors := Trunc(Power(2, iBitsPerPixel));
    result := format('%d colors', [iColors])
  end
  else
  if iBitsPerPixel = 12 then
    result := '4,096 colors'
  else
  if (iBitsPerPixel = 15) or
     (iBitsPerPixel = 16) then
    result := '65,536 colors' // Note: Generally true, but there are exceptions
  else {24, 32, etc}
    result := format('%d bit color', [iBitsPerPixel]);
end;

{!!
<FS>AngleToImageEnRotateAngle

<FM>Declaration<FC>
function AngleToImageEnRotateAngle(iAngle: Integer): integer;

<FM>Description<FN>
ImageEn's <L TImageEnProc.Rotate>rotation parameter</L> requires a value specified negative or positive degrees counter-clockwise. This method converts a standard positive clockwise value to an ImageEn value.

<FM>Example<FC>
// Rotate image 90 deg. clockwise
ImageEnView1.Proc.Rotate( AngleToImageEnRotateAngle( 90 ) );
!!}
function AngleToImageEnRotateAngle(iAngle: Integer): integer;
begin
  result := abs(360 - iAngle);
end;


type
  HourglassHandler = class(TInterfacedObject)
  private
    fOldCursor: TCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ HourglassHandler }

constructor HourglassHandler.Create;
begin
  FOldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

destructor HourglassHandler.Destroy;
begin
  Screen.Cursor := fOldCursor;
  inherited;
end;


{!!
<FS>ShowTempHourglass

<FM>Declaration<FC>
function ShowTempHourglass: IUnknown;

<FM>Description<FN>
Change the cursor to an hourglass during the current procedure (i.e. will revert to the previous cursor once we go out of scope)

<FM>Example<FC>
Load an image when a button is clicked. Show hourglass during loading
procedure TMain.Button1Click(Sender: TObject);
begin
  ShowTempHourglass;
  ImageEnView1.IO.LoadFromFile('C:\MyImage.jpg');
end;
!!}
function ShowTempHourglass: IUnknown;
begin
  result := HourglassHandler.Create
end;
                                                    
{!!
<FS>iexHelperFunctions

<FN>iexHelperFunctions.pas provides helper functions for <A TImageEnIO>, TBitmap and <A TIEBitmap>, plus a variety of file functions. These methods provide quicker access to some ImageEn functionality for common tasks.

Simply add iexHelperFunctions to your uses clause to access the new methods

<FM>IMAGEENIO HELPER FUNCTIONS<FN>
Adds shortcut methods to the <A TImageEnIO> class:
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CreatePDFFromFileList></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CreatePSFromFileList></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromBlob></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromBlobFast></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileAutoEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileFast></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileJPEGFast></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileRawFast></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.Reload></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToBlob></C> </R>    
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileEx></C> </R>
</TABLE>

<FM>BITMAP HELPER FUNCTIONS<FN>
Adds more functionality to the TBitmap class:
<TABLE2>
<R> <C_IMG_METHOD> <C><A TBitmap.IEInitialize></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IELoadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IELoadFromFileFast></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.LoadFromURL></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IESaveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IERotate></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IEFlip></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IEResample></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IELoadAsThumbnail></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IEConvertToThumbnail></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.IEAddSoftShadow></C> </R>
<R> <C_IMG_METHOD> <C><A TBitmap.PrintImage></C> </R>
</TABLE>

<FM>IEBITMAP HELPER FUNCTIONS<FN>
Adds more functionality to the <A TIEBitmap> class:
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEBitmap.IEInitialize></C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.IELoadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.IELoadFromFileFast></C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.LoadFromURL></C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.IESaveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.IELoadAsThumbnail></C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.IEConvertToThumbnail></C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.IEAddSoftShadow> </C> </R>
<R> <C_IMG_METHOD> <C><A TIEBitmap.PrintImage></C> </R>
</TABLE>

<FM>FILE FUNCTIONS<FN>
Perform image manipulation functions without direct use of ImageEn components:
<TABLE2>
<R> <C_IMG_GLOBMETHOD> <C><A IEResampleImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEConvertImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IECreateThumbnailFromFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A GetFileDetails></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A GetImageDetails></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A GetExifOrFileCreationDate></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A JPEGLosslessRotateFile></C> </R>   
<R> <C_IMG_GLOBMETHOD> <C><A JPEGLosslessFlipFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IERotateImageFile></C> </R>   
<R> <C_IMG_GLOBMETHOD> <C><A IEFlipImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEReadCorrectOrientationOfImageFile></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEAutomaticallyRotateImageFile></C> </R>
</TABLE>

<FM>OTHER FUNCTIONS<FN>
Other functions that may be useful with ImageEn:
<TABLE2>                                         
<R> <C_IMG_GLOBMETHOD> <C><A BitsPerPixelToStr></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A AngleToImageEnRotateAngle></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A ShowTempHourglass></C> </R>
</TABLE>

* Note: Delphi/C++ 2005 or newer is required to use helper classes

!!}

end.
