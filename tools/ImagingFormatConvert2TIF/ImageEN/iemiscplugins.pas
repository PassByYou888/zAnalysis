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
File version 1001
*)


unit iemiscplugins;

{$R-}
{$Q-}
{$J+}
{$Z4}

{$I ie.inc}


interface

uses Windows, Messages, Classes, Graphics, SysUtils, hyiedefs, imageenio, hyieutils, iesettings;


{$IFDEF IEINCLUDEMISCPLUGINS}


///////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMiscPluginsImageMagick

const iomscFITS   = ioMiscDLLPlugIns + 1;
const iomscPCD    = ioMiscDLLPlugIns + 2;
const iomscPCL    = ioMiscDLLPlugIns + 3;
const iomscPDF    = ioMiscDLLPlugIns + 4;
const iomscBIE    = ioMiscDLLPlugIns + 5;
const iomscCIN    = ioMiscDLLPlugIns + 6;
const iomscDDS    = ioMiscDLLPlugIns + 7;
const iomscDFONT  = ioMiscDLLPlugIns + 8;
const iomscDPX    = ioMiscDLLPlugIns + 9;
const iomscJBIG   = ioMiscDLLPlugIns + 10;
const iomscJNG    = ioMiscDLLPlugIns + 11;
const iomscM2V    = ioMiscDLLPlugIns + 12;
const iomscMAT    = ioMiscDLLPlugIns + 13;
const iomscMIFF   = ioMiscDLLPlugIns + 14;
const iomscMNG    = ioMiscDLLPlugIns + 15;
const iomscMP4    = ioMiscDLLPlugIns + 16;
const iomscMSVG   = ioMiscDLLPlugIns + 17;
const iomscPES    = ioMiscDLLPlugIns + 18;
const iomscPS     = ioMiscDLLPlugIns + 19;
const iomscPSB    = ioMiscDLLPlugIns + 20;
const iomscPTIF   = ioMiscDLLPlugIns + 21;
const iomscSVG    = ioMiscDLLPlugIns + 22;
const iomscTIFF64 = ioMiscDLLPlugIns + 23;
const iomscXCF    = ioMiscDLLPlugIns + 24;
const iomscXPS    = ioMiscDLLPlugIns + 25;

const Min_ImageMagick_Type = iomscFITS;
const Max_ImageMagick_Type = iomscXPS;

const IEIMAGEMAGICK_WAND_DLL_FILENAME   = 'CORE_RL_wand_.dll';
const IEIMAGEMAGICK_MAGICK_DLL_FILENAME = 'CORE_RL_magick_.dll';

type TIEImageMagick_MagickWand        = pointer;
type TIEImageMagick_MagickBooleanType = longbool;
type TIEImageMagick_size_t            = {$ifdef WIN64} uint64; {$else} dword; {$endif}
type TIEImageMagick_ssize_t           = {$ifdef WIN64} int64; {$else} integer; {$endif}
type TIEImageMagick_MagickOffsetType  = int64;
type TIEImageMagick_MagickSizeType    = uint64;
type TIEImageMagick_MagicInfo         = pointer;
type TIEImageMagick_ExceptionInfo     = pointer;
type TIEImageMagick_ExceptionType     = integer;
type TIEImageMagick_PixelWand         = pointer;

type TIEImageMagick_StorageType = (
                                    imck_UndefinedPixel,
                                    imck_CharPixel,
                                    imck_DoublePixel,
                                    imck_FloatPixel,
                                    imck_IntegerPixel,
                                    imck_LongPixel,
                                    imck_QuantumPixel,
                                    imck_ShortPixel
                                  );
type TIEImageMagick_ChannelType = (
                                    imck_UndefinedChannel    = 0,
                                    imck_RedChannel          = $0001,
                                    imck_GrayChannel         = $0001,
                                    imck_CyanChannel         = $0001,
                                    imck_GreenChannel        = $0002,
                                    imck_MagentaChannel      = $0002,
                                    imck_BlueChannel         = $0004,
                                    imck_YellowChannel       = $0004,
                                    imck_AlphaChannel        = $0008,
                                    imck_OpacityChannel      = $0008,
                                    imck_MatteChannel        = $0008,
                                    imck_BlackChannel        = $0020,
                                    imck_IndexChannel        = $0020,
                                    imck_CompositeChannels   = $002F,
                                    imck_AllChannels         = $7ffffff,
                                    imck_TrueAlphaChannel    = $0040,
                                    imck_RGBChannels         = $0080,
                                    imck_GrayChannels        = $0080,
                                    imck_SyncChannels        = $0100,
                                    imck_DefaultChannels     = $7FFFFF7
                                  );
type TIEImageMagick_ColorspaceType = (
                                    imck_UndefinedColorspace,
                                    imck_RGBColorspace,
                                    imck_GRAYColorspace,
                                    imck_TransparentColorspace,
                                    imck_OHTAColorspace,
                                    imck_LabColorspace,
                                    imck_XYZColorspace,
                                    imck_YCbCrColorspace,
                                    imck_YCCColorspace,
                                    imck_YIQColorspace,
                                    imck_YPbPrColorspace,
                                    imck_YUVColorspace,
                                    imck_CMYKColorspace,
                                    imck_sRGBColorspace,
                                    imck_HSBColorspace,
                                    imck_HSLColorspace,
                                    imck_HWBColorspace,
                                    imck_Rec601LumaColorspace,
                                    imck_Rec601YCbCrColorspace,
                                    imck_Rec709LumaColorspace,
                                    imck_Rec709YCbCrColorspace,
                                    imck_LogColorspace,
                                    imck_CMYColorspace,
                                    imck_LuvColorspace,
                                    imck_HCLColorspace,
                                    imck_LCHColorspace,
                                    imck_LMSColorspace,
                                    imck_LCHabColorspace,
                                    imck_LCHuvColorspace,
                                    imck_scRGBColorspace,
                                    imck_HSIColorspace,
                                    imck_HSVColorspace,
                                    imck_HCLpColorspace,
                                    imck_YDbDrColorspace
                                  );
type TIEImageMagick_ResolutionType = (
                                    imck_UndefinedResolution,
                                    imck_PixelsPerInchResolution,
                                    imck_PixelsPerCentimeterResolution
                                    );
type TIEImageMagick_ImageType = (
                                    imck_UndefinedType,
                                    imck_BilevelType,
                                    imck_GrayscaleType,
                                    imck_GrayscaleMatteType,
                                    imck_PaletteType,
                                    imck_PaletteMatteType,
                                    imck_TrueColorType,
                                    imck_TrueColorMatteType,
                                    imck_ColorSeparationType,
                                    imck_ColorSeparationMatteType,
                                    imck_OptimizeType,
                                    imck_PaletteBilevelMatteType
                                    );

type TIEImageMagick_MagickProgressMonitor = function(text: PAnsiChar; offset: TIEImageMagick_MagickOffsetType; span: TIEImageMagick_MagickSizeType; client_data: pointer): TIEImageMagick_MagickBooleanType; cdecl;


procedure TIEMiscPluginsImageMagick_ReadImageStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
procedure TIEMiscPluginsImageMagick_WriteImageStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
function TIEMiscPluginsImageMagick_TryImageStream(Stream: TStream; TryingFormat: TIOFileType): boolean;
function TIEMiscPluginsImageMagick_PDFFrameCount(Stream: TStream): integer;


{!!
<FS>TIEMiscPluginsImageMagick

<FM>Declaration<FC>
TIEMiscPluginsImageMagick = class;

<FM>Description<FN>
TIEMiscPluginsImageMagick is a wrapper around ImageMagick library, allowing ImageEn to load many formats like PDF, PS, PCD and many others.
To setup ImageMagick wrapper just call, one time, the <A TIEMiscPluginsImageMagick.RegisterPlugin> class method.

You can download ImageMagick from:
<L http://www.imagemagick.org/script/binary-releases.php#windows>http://www.imagemagick.org/script/binary-releases.php#windows</L>

In order to load PDF, PS, PCL and  you must install the Ghostscript library:
<L http://www.ghostscript.com/download/>http://www.ghostscript.com/download/</L>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMiscPluginsImageMagick.IsAvailable></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMiscPluginsImageMagick.RegisterPlugin></C> </R>
</TABLE>

<FM>Example<FC>
// rasterizes page number 5 of mybook.pdf at 200dpi
TIEMiscPluginsImageMagick.RegisterPlugin(); // call only one time!
ImageEnView1.IO.Params.ImageIndex := 5;
ImageEnView1.IO.Params.Dict.Insert('PDF:Density', 200);
ImageEnView1.IO.LoadFromFile('mybook.pdf');
!!}
type TIEMiscPluginsImageMagick = class

  public

    class function IsAvailable(): boolean;
    class procedure Initialize(reinitialize: boolean = true);
    class procedure Finalize();
    class procedure RegisterPlugin();

  private
  
    class function HandleWand(Handle: THandle = 0; Setup: boolean = false): THandle;
    class function HandleMagick(Handle: THandle = 0; Setup: boolean = false): THandle;
    class procedure CheckCallWand(var exec: pointer; name: AnsiString);
    class procedure CheckCallMagick(var exec: pointer; name: AnsiString);

  public
  
    // MagickWand API wrappers
    class procedure MagickWandGenesis();
    class procedure MagickWandTerminus();
    class function NewMagickWand(): TIEImageMagick_MagickWand;
    class function DestroyMagickWand(wand: TIEImageMagick_MagickWand): TIEImageMagick_MagickWand;
    class function MagickReadImage(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType;
    class function MagickReadImageBlob(wand: TIEImageMagick_MagickWand; blob: pointer; length: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType;
    class function MagickPingImageBlob(wand: TIEImageMagick_MagickWand; blob: pointer; length: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType;
    class function MagickPingImage(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType;
    class procedure MagickResetIterator(wand: TIEImageMagick_MagickWand);
    class function MagickNextImage(wand: TIEImageMagick_MagickWand): TIEImageMagick_MagickBooleanType;
    class function MagickSetIteratorIndex(wand: TIEImageMagick_MagickWand; index: TIEImageMagick_ssize_t): TIEImageMagick_MagickBooleanType;
    class procedure MagickSetLastIterator(wand: TIEImageMagick_MagickWand);
    class function MagickGetIteratorIndex(wand: TIEImageMagick_MagickWand): TIEImageMagick_ssize_t;
    class function MagickExportImagePixels(wand: TIEImageMagick_MagickWand; x: TIEImageMagick_ssize_t; y: TIEImageMagick_ssize_t; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; map: PAnsiChar; storage: TIEImageMagick_StorageType; pixels: pointer): TIEImageMagick_MagickBooleanType;
    class function MagickGetImageWidth(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t;
    class function MagickGetImageHeight(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t;
    class function MagickGetImageChannelDepth(wand: TIEImageMagick_MagickWand; channel: TIEImageMagick_ChannelType): TIEImageMagick_size_t;
    class function MagickGetImageColorspace(wand: TIEImageMagick_MagickWand): TIEImageMagick_ColorspaceType;
    class function MagickSetImageColorspace(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType;
    class function MagickTransformImageColorspace(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType;
    class function MagickGetImageDepth(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t;
    class function MagickGetImageResolution(wand: TIEImageMagick_MagickWand; var x: double; var y: double): TIEImageMagick_MagickBooleanType;
    class function MagickGetImageUnits(wand: TIEImageMagick_MagickWand): TIEImageMagick_ResolutionType;
    class function MagickGetImageFormat(wand: TIEImageMagick_MagickWand): PAnsiChar;
    class function MagickGetType(wand: TIEImageMagick_MagickWand): TIEImageMagick_ImageType;
    class function MagickSetProgressMonitor(wand: TIEImageMagick_MagickWand; progress_monitor: TIEImageMagick_MagickProgressMonitor; client_data: pointer): TIEImageMagick_MagickProgressMonitor;
    class function MagickSetImageProperty(wand: TIEImageMagick_MagickWand; prop: PAnsiChar; value: PAnsiChar): TIEImageMagick_MagickBooleanType;
    class function MagickSetOption(wand: TIEImageMagick_MagickWand; key: PAnsiChar; value: PAnsiChar): TIEImageMagick_MagickBooleanType;
    class function MagickGetException(wand: TIEImageMagick_MagickWand; var severity: TIEImageMagick_ExceptionType): PAnsiChar;
    class function MagickRelinquishMemory(resource: pointer): pointer;
    class function MagickSetColorspace(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType;
    class function MagickSetDepth(wand: TIEImageMagick_MagickWand; depth: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType;
    class function MagickSetFormat(wand: TIEImageMagick_MagickWand; format: PAnsiChar): TIEImageMagick_MagickBooleanType;
    class function MagickSetResolution(wand: TIEImageMagick_MagickWand; x_resolution: double; y_resolution: double): TIEImageMagick_MagickBooleanType;
    class function MagickSetImageUnits(wand: TIEImageMagick_MagickWand; units: TIEImageMagick_ResolutionType): TIEImageMagick_MagickBooleanType;
    class function MagickGetImageBlob(wand: TIEImageMagick_MagickWand; var length: TIEImageMagick_size_t): pbyte;
    class function MagickWriteImage(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType;
    class function MagickWriteImages(wand: TIEImageMagick_MagickWand; filename: PAnsiChar; adjoin: TIEImageMagick_MagickBooleanType): TIEImageMagick_MagickBooleanType;
    class function MagickImportImagePixels(wand: TIEImageMagick_MagickWand; x: TIEImageMagick_ssize_t; y: TIEImageMagick_ssize_t; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; map: PAnsiChar; storage: TIEImageMagick_StorageType; pixels: pointer): TIEImageMagick_MagickBooleanType;
    class function MagickNewImage(wand: TIEImageMagick_MagickWand; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; background: TIEImageMagick_PixelWand): TIEImageMagick_MagickBooleanType;
    class function NewPixelWand(): TIEImageMagick_PixelWand;
    class function DestroyPixelWand(pixelWand: TIEImageMagick_PixelWand): TIEImageMagick_PixelWand;

    // MagickCore API wrappers
    class function GetMagicInfo(magic: pointer; length: TIEImageMagick_size_t; exception: TIEImageMagick_ExceptionInfo): TIEImageMagick_MagicInfo;
    class function GetMagicName(magic_info: TIEImageMagick_MagicInfo): PAnsiChar;
    class function AcquireExceptionInfo(): TIEImageMagick_ExceptionInfo;
    class function DestroyExceptionInfo(e: TIEImageMagick_ExceptionInfo): TIEImageMagick_ExceptionInfo;
    //class function GetAuthenticPixels(image: TIEImageMagick_Image; x: TIEImageMagick_ssize_t; y: TIEImageMagick_ssize_t; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; exception: TIEImageMagick_ExceptionInfo): TIEImageMagick_PixelPacket;
end;

// TIEMiscPluginsImageMagick
///////////////////////////////////////////////////////////////////////////////////////////////////////




implementation
uses imageenproc;


var
  IEMiscPluginsCS: TRTLCriticalSection;


///////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMiscPluginsImageMagick


type TIEMiscPluginsImageMagick_Format = record
  typ: TIOFileType;
  des: string;
  ext: string;
  sue: string;
  wrt: boolean;
end;

const
  ImageMagickFormats: array [0..24] of TIEMiscPluginsImageMagick_Format = (
    (typ: iomscFITS;   des: 'Flexible Image Transport System';                        ext: 'FITS;FTS';                 sue: 'fits';   wrt: true),
    (typ: iomscPCD;    des: 'Photo CD';                                               ext: 'PCD;PCDS';                 sue: 'pcd';    wrt: true),
    (typ: iomscPCL;    des: 'Printer Control Language';                               ext: 'PCL';                      sue: 'pcl';    wrt: true),
    (typ: iomscPDF;    des: 'Portable Document Format';                               ext: 'PDF;PDFA;EPDF';            sue: 'pdf';    wrt: true),
    (typ: iomscBIE;    des: 'Joint Bi-level Image experts Group interchange format';  ext: 'BIE';                      sue: 'bie';    wrt: true),
    (typ: iomscCIN;    des: 'Cineon Image File';                                      ext: 'CIN';                      sue: 'cin';    wrt: true),
    (typ: iomscDDS;    des: 'Microsoft DirectDraw Surface';                           ext: 'DDS';                      sue: 'dds';    wrt: true),
    (typ: iomscDFONT;  des: 'Multi-face font package';                                ext: 'DFONT';                    sue: 'dfont';  wrt: false),
    (typ: iomscDPX;    des: 'SMPTE 268M-2003 (DPX 2.0)';                              ext: 'DPX';                      sue: 'dpx';    wrt: true),
    (typ: iomscJBIG;   des: 'Joint Bi-level Image experts Group interchange format';  ext: 'JBIG;JBG';                 sue: 'jbig';   wrt: true),
    (typ: iomscJNG;    des: 'JPEG Network Graphics';                                  ext: 'JNG';                      sue: 'jng';    wrt: true),
    (typ: iomscM2V;    des: 'MPEG Video Stream';                                      ext: 'M2V;MOV;MPEG;MPG';         sue: 'm2v';    wrt: true),
    (typ: iomscMAT;    des: 'MATLAB level 5 image format';                            ext: 'MAT';                      sue: 'mat';    wrt: true),
    (typ: iomscMIFF;   des: 'Magick Image File Format';                               ext: 'MIFF';                     sue: 'miff';   wrt: true),
    (typ: iomscMNG;    des: 'Multiple-image Network Graphics';                        ext: 'MNG';                      sue: 'mng';    wrt: true),
    (typ: iomscMP4;    des: 'MPEG-4 Video';                                           ext: 'MP4;M4V';                  sue: 'mp4';    wrt: true),
    (typ: iomscMSVG;   des: 'ImageMagick''s own SVG internal renderer';               ext: 'MSVG';                     sue: 'msvg';   wrt: true),
    (typ: iomscPES;    des: 'Embrid Embroidery Format';                               ext: 'PES';                      sue: 'pes';    wrt: false),
    (typ: iomscPS;     des: 'PostScript';                                             ext: 'PS;EPS;EPSF;EPSI;EPI;EPT'; sue: 'ps';     wrt: true),
    (typ: iomscPSB;    des: 'Adobe Large Document Format';                            ext: 'PSB';                      sue: 'psb';    wrt: true),
    (typ: iomscPTIF;   des: 'Pyramid encoded TIFF';                                   ext: 'PTIF';                     sue: 'ptif';   wrt: true),
    (typ: iomscSVG;    des: 'Scalable Vector Graphics';                               ext: 'SVG;SVGZ';                 sue: 'svg';    wrt: true),
    (typ: iomscTIFF64; des: 'Tagged Image File Format (64-bit)';                      ext: 'TIFF64';                   sue: 'tiff64'; wrt: true),
    (typ: iomscXCF;    des: 'GIMP image';                                             ext: 'XCF';                      sue: 'xcf';    wrt: false),
    (typ: iomscXPS;    des: 'Microsoft XML Paper Specification';                      ext: 'XPS';                      sue: 'xps';    wrt: false)
  );


{!!
<FS>TIEMiscPluginsImageMagick.RegisterPlugin

<FM>Declaration<FC>
class procedure RegisterPlugin();

<FM>Description<FN>
Registers ImageMagick file formats into ImageEn.
Applications should call this method one time only (maybe at the start of the application).

<FM>Example<FC>
// rasterizes page number 5 of mybook.pdf at 200dpi
TIEMiscPluginsImageMagick.RegisterPlugin(); // call only one time!
ImageEnView1.IO.Params.ImageIndex := 5;
ImageEnView1.IO.Params.Dict.Insert('PDF:Density', 200);
ImageEnView1.IO.LoadFromFile('mybook.pdf');
!!}
class procedure TIEMiscPluginsImageMagick.RegisterPlugin();
var
  i: integer;
begin
  for i := 0 to high(ImageMagickFormats) do
    IEFileFormatAdd( TIEFileFormatInfo.Create(ImageMagickFormats[i].typ,
                                              ImageMagickFormats[i].des,
                                              ImageMagickFormats[i].ext,
                                              ImageMagickFormats[i].sue,
                                              false,
                                              [],
                                              @TIEMiscPluginsImageMagick_ReadImageStream,
                                              @TIEMiscPluginsImageMagick_WriteImageStream,
                                              @TIEMiscPluginsImageMagick_TryImageStream) );
  // remove native ImageEn formats
  IEFileFormatRemove(ioPDF);
  IEFileFormatRemove(ioPS);
end;


// Warning: in future "identify" command may change the SPATTERN!
// At the moment this is the fastest way to know pages count using imagemagick/ghostscript
function TIEMiscPluginsImageMagick_PDFFrameCount(Stream: TStream): integer;
const
  SPATTERN: AnsiString = 'number of pages in the file: ';
var
  filename: AnsiString;
  dosout: AnsiString;
  p: integer;
begin
  result := 0;
  if TIEMiscPluginsImageMagick.IsAvailable() and (Stream is TIEWideFileStream) then
  begin
    filename := AnsiString((Stream as TIEWideFileStream).FileName);
    dosout := IEGetDosOutput('identify ' + filename + '[999999999]');
    p := IEPos(SPATTERN, dosout);
    if p > 0 then
    begin
      dosout := IECopy(dosout, p + length(SPATTERN), length(dosout));
      p := IEPos(#$D#$A, dosout);
      result := IEStrToIntDef(IECopy(dosout, 1, p - 1), 0);
    end;
  end;
end;


// note: may be executed inside another thread. So Progress method should not update UI (to avoid locks!)
function TIEMiscPluginsImageMagick_MagickProgressMonitor(text: PAnsiChar; offset: TIEImageMagick_MagickOffsetType; span: TIEImageMagick_MagickSizeType; client_data: pointer): TIEImageMagick_MagickBooleanType; cdecl;
var
  pr: ^TProgressRec;
  per: integer;
begin
  pr := client_data;
  if ((span < 100) or (offset = span - 1) or (offset mod (span div 100) = 0)) and assigned(pr) and assigned(pr^.fOnProgress) then
  begin
    if GetCurrentThreadId() = System.MainThreadID then
    begin
      per := trunc(100 * integer(offset) / integer(span) / pr^.tot + 100 / pr^.tot * pr^.val);
      pr^.fOnProgress(pr^.Sender, per);
    end;
    result := not pr^.Aborting^;
  end
  else
    result := true;
end;


// todo:
//  - more reading options (other than PDF:Density)
procedure TIEMiscPluginsImageMagick_ReadImageStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
var
  mwand: TIEImageMagick_MagickWand;
  mstream: TMemoryStream;
  filename: AnsiString;
  height, width: integer;
  i, j: integer;
  r: TIEImageMagick_MagickBooleanType;
  dpix, dpiy: double;
  imdepth: TIEImageMagick_size_t;
  imcolorspc: TIEImageMagick_ColorspaceType;
  pixfrmt: TIEPixelFormat;
  outfrmt: AnsiString;
  outstor: TIEImageMagick_StorageType;
  bytebuf: array of byte;
  px: pointer;
begin
  if TIEMiscPluginsImageMagick.IsAvailable() then
  begin
    Progress.tot := 2;  // number of steps
    mwand := TIEMiscPluginsImageMagick.NewMagickWand();
    mstream := nil;
    try
      if assigned(Progress.fOnProgress) then
        TIEMiscPluginsImageMagick.MagickSetProgressMonitor(mwand, @TIEMiscPluginsImageMagick_MagickProgressMonitor, @Progress);

      if Stream is TIEWideFileStream then
      begin
        filename := AnsiString((Stream as TIEWideFileStream).FileName);
      end
      else
      begin
        mstream := TMemoryStream.Create();
        mstream.LoadFromStream(Stream);
      end;
      Progress.val := 0;  // step one (0) - load

      if IOParams.Dict.HasKey('PDF:Density') then
        TIEMiscPluginsImageMagick.MagickSetOption(mwand, 'density', PAnsiChar(AnsiString(IOParams.Dict.GetString('PDF:Density'))));

      // it is too much expensive calculate pages count (think to PDF and PS). For PDF one may use TIEMiscPluginsImageMagick_PDFFrameCount or IEPDFFrameCount instead
      IOParams.ImageCount := 1;
      filename := filename + '[' + AnsiString(IntToStr(IOParams.ImageIndex)) + ']';

      r := false;
      if Preview then
      begin
        if filename <> '' then
          r := TIEMiscPluginsImageMagick.MagickPingImage(mwand, PAnsiChar(filename))
        else
        if mstream <> nil then
          r := TIEMiscPluginsImageMagick.MagickPingImageBlob(mwand, mstream.Memory, mstream.Size);
      end
      else
      begin
        if filename <> '' then
          r := TIEMiscPluginsImageMagick.MagickReadImage(mwand, PAnsiChar(filename))
        else
        if mstream <> nil then
          r := TIEMiscPluginsImageMagick.MagickReadImageBlob(mwand, mstream.Memory, mstream.Size);
      end;

      if r then
      begin

        // resolution
        TIEMiscPluginsImageMagick.MagickGetImageResolution(mwand, dpix, dpiy);
        case TIEMiscPluginsImageMagick.MagickGetImageUnits(mwand) of
          imck_UndefinedResolution,
          imck_PixelsPerInchResolution:
            begin
              IOParams.DpiX := trunc(dpix);
              IOParams.DpiY := trunc(dpiy);
            end;
          imck_PixelsPerCentimeterResolution:
            begin
              IOParams.DpiX := trunc(dpix / CM_per_Inch);
              IOParams.DpiY := trunc(dpiy / CM_per_Inch);
            end;
        end;
        if IOParams.DpiX = 0 then
          IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
        if IOParams.DpiY = 0 then
          IOParams.DpiY := IEGlobalSettings().DefaultDPIY;

        // decode image
        Progress.val := 1;  // step two (1) - convert color space

        imdepth := TIEMiscPluginsImageMagick.MagickGetImageDepth(mwand);
        imcolorspc := TIEMiscPluginsImageMagick.MagickGetImageColorspace(mwand);

        case imcolorspc of
          imck_GRAYColorspace:
            begin
              if imdepth = 1 then
              begin
                pixfrmt := ie1g;
                outfrmt := 'I';
                outstor := imck_CharPixel;
                IOParams.BitsPerSample := 1;
                IOParams.SamplesPerPixel := 1;
              end
              else
              if imdepth = 8 then
              begin
                pixfrmt := ie8g;
                outfrmt := 'I';
                outstor := imck_CharPixel;
                IOParams.BitsPerSample := 8;
                IOParams.SamplesPerPixel := 1;
              end
              else
              if (imdepth = 16) and (IOParams.IsNativePixelFormat) then
              begin
                pixfrmt := ie16g;
                outfrmt := 'I';
                outstor := imck_ShortPixel;
                IOParams.BitsPerSample := 16;
                IOParams.SamplesPerPixel := 1;
              end
              else
              begin
                pixfrmt := ie8g;
                outfrmt := 'I';
                outstor := imck_CharPixel;
                IOParams.BitsPerSample := 8;
                IOParams.SamplesPerPixel := 1;
              end;
            end;
          else
            begin
              TIEMiscPluginsImageMagick.MagickTransformImageColorspace(mwand, imck_sRGBColorspace);
              pixfrmt := ie24RGB;
              outfrmt := 'BGR';
              outstor := imck_CharPixel;
              IOParams.BitsPerSample := 8;
              IOParams.SamplesPerPixel := 3;
            end;
        end;

        width  := TIEMiscPluginsImageMagick.MagickGetImageWidth(mwand);
        height := TIEMiscPluginsImageMagick.MagickGetImageHeight(mwand);
        Bitmap.Allocate(width, height, pixfrmt);
        IOParams.Width          := width;
        IOParams.OriginalWidth  := width;
        IOParams.Height         := height;
        IOParams.OriginalHeight := height;

        IOParams.FreeColorMap();

        if not Preview then
        begin
          if pixfrmt = ie1g then
          begin
            // special case, packet 1 bit channels
            SetLength(bytebuf, width);
            for i := 0 to height - 1 do
            begin
              TIEMiscPluginsImageMagick.MagickExportImagePixels(mwand, 0, i, width, 1, PAnsiChar(outfrmt), outstor, bytebuf);
              px := Bitmap.ScanLine[i];
              for j := 0 to width - 1 do
                _SetPixelbw(px, j, bytebuf[j]);
            end;
          end
          else
          begin
            // other cases (non packet channels)
            for i := 0 to height - 1 do
              TIEMiscPluginsImageMagick.MagickExportImagePixels(mwand, 0, i, width, 1, PAnsiChar(outfrmt), outstor, Bitmap.ScanLine[i]);
          end;
        end;
      end
      else
        Progress.Aborting^ := true;
    finally
      mstream.Free();
      TIEMiscPluginsImageMagick.DestroyMagickWand(mwand);
    end;
  end
  else
    Progress.Aborting^ := true;
end;


// todo:
//  - implement other pixel formats.
//  - multipages
//  - more writing options
// known bugs: resulting PCD has wrong colors
procedure TIEMiscPluginsImageMagick_WriteImageStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  mwand: TIEImageMagick_MagickWand;
  bkgrnd: TIEImageMagick_PixelWand;
  i: integer;
  frmt: AnsiString;
  imageBlob: pbyte;
  imageBlobLen: TIEImageMagick_size_t;
begin
  if TIEMiscPluginsImageMagick.IsAvailable() then
  begin
    mwand := TIEMiscPluginsImageMagick.NewMagickWand();
    bkgrnd := TIEMiscPluginsImageMagick.NewPixelWand();
    try
      TIEMiscPluginsImageMagick.MagickNewImage(mwand, Bitmap.Width, Bitmap.Height, bkgrnd);

      TIEMiscPluginsImageMagick.MagickSetDepth(mwand, 8);
      frmt := AnsiString(Uppercase(IEFileFormatGetExt(IOParams.FileType, 0)));
      TIEMiscPluginsImageMagick.MagickSetImageUnits(mwand, imck_PixelsPerInchResolution);
      TIEMiscPluginsImageMagick.MagickSetResolution(mwand, IOParams.DpiX, IOParams.DpiY);
      TIEMiscPluginsImageMagick.MagickSetColorspace(mwand, imck_sRGBColorspace);
      TIEMiscPluginsImageMagick.MagickSetFormat(mwand, PAnsiChar(frmt));

      for i := 0 to Bitmap.Height - 1 do
        TIEMiscPluginsImageMagick.MagickImportImagePixels(mwand, 0, i, Bitmap.Width, 1, 'BGR', imck_CharPixel, Bitmap.ScanLine[i]);

      imageBlob := TIEMiscPluginsImageMagick.MagickGetImageBlob(mwand, imageBlobLen);
      Stream.Write(imageBlob^, imageBlobLen);
      TIEMiscPluginsImageMagick.MagickRelinquishMemory(imageBlob);
    finally
      TIEMiscPluginsImageMagick.DestroyPixelWand(bkgrnd);
      TIEMiscPluginsImageMagick.DestroyMagickWand(mwand);
    end;
  end;
end;


function TIEMiscPluginsImageMagick_TryImageStream(Stream: TStream; TryingFormat: TIOFileType): boolean;
const
  BUFSIZE = 8192; // file head to analyse
var
  lpos: int64;
  frmt: string;
  i: integer;
  buf: array of byte;
  ex: TIEImageMagick_ExceptionInfo;
  info: TIEImageMagick_MagicInfo;
begin
  result := false;
  if TIEMiscPluginsImageMagick.IsAvailable() then
  begin
    lpos := Stream.Position;
    try
      SetLength(buf, BUFSIZE);
      Stream.Read(buf[0], BUFSIZE);
      ex := TIEMiscPluginsImageMagick.AcquireExceptionInfo();
      info := TIEMiscPluginsImageMagick.GetMagicInfo(@buf[0], BUFSIZE, ex);
      if info <> nil then
        frmt := string(AnsiString(TIEMiscPluginsImageMagick.GetMagicName(info)))
      else
        frmt := '';
      TIEMiscPluginsImageMagick.DestroyExceptionInfo(ex);
      if frmt <> '' then
        for i := 0 to high(ImageMagickFormats) do
          if (TryingFormat = ImageMagickFormats[i].typ) and (pos(';' + frmt + ';', ';' + ImageMagickFormats[i].ext + ';') > 0) then
          begin
            result := true;
            break;
          end;
    finally
      Stream.Position := lpos;
    end;
  end;
end;


class procedure TIEMiscPluginsImageMagick.CheckCallWand(var exec: pointer; name: AnsiString);
begin
  if exec = nil then
    exec := GetProcAddress(HandleWand(), PAnsiChar(name));
end;


class procedure TIEMiscPluginsImageMagick.CheckCallMagick(var exec: pointer; name: AnsiString);
begin
  if exec = nil then
    exec := GetProcAddress(HandleMagick(), PAnsiChar(name));
end;


class function TIEMiscPluginsImageMagick.HandleWand(Handle: THandle; Setup: boolean): THandle;
const
  LibHandle: THandle = 0;
begin
  if Setup then
    LibHandle := Handle;
  result := LibHandle;
end;


class function TIEMiscPluginsImageMagick.HandleMagick(Handle: THandle; Setup: boolean): THandle;
const
  LibHandle: THandle = 0;
begin
  if Setup then
    LibHandle := Handle;
  result := LibHandle;
end;


{!!
<FS>TIEMiscPluginsImageMagick.IsAvailable

<FM>Declaration<FC>
class function IsAvailable(): boolean;

<FM>Description<FN>
Returns True if the ImageMagick library is present and successfully loaded.
!!}
class function TIEMiscPluginsImageMagick.IsAvailable(): boolean;
begin
  if (HandleWand() = 0) or (HandleMagick() = 0) then
    Initialize(false);
  result := (HandleWand() <> 0) and (HandleMagick() <> 0);
end;


class procedure TIEMiscPluginsImageMagick.Initialize(reinitialize: boolean = true);
  procedure TryLoad();
  begin
    if HandleWand() = 0 then
    begin
      HandleWand(LoadLibraryW(PWideChar(WideString(IEIMAGEMAGICK_WAND_DLL_FILENAME))), true);
      if (HandleWand() = 0) and IsLibrary then // if this is a DLL try to load from dll path
        HandleWand(LoadLibraryW(PWideChar( ExtractFilePath(GetModuleName(HInstance)) + WideString(IEIMAGEMAGICK_WAND_DLL_FILENAME) )), true);
      if (HandleWand() <> 0) then
        MagickWandGenesis();
    end;
    if HandleMagick() = 0 then
    begin
      HandleMagick(LoadLibraryW(PWideChar(WideString(IEIMAGEMAGICK_MAGICK_DLL_FILENAME))), true);
      if (HandleMagick() = 0) and IsLibrary then // if this is a DLL try to load from dll path
        HandleMagick(LoadLibraryW(PWideChar( ExtractFilePath(GetModuleName(HInstance)) + WideString(IEIMAGEMAGICK_MAGICK_DLL_FILENAME) )), true);
    end;
  end;
begin
  EnterCriticalSection(IEMiscPluginsCS);
  try
    if reinitialize then
      Finalize();
    TryLoad();
  finally
    LeaveCriticalSection(IEMiscPluginsCS);
  end;
end;


class procedure TIEMiscPluginsImageMagick.Finalize();
begin
  if HandleMagick() <> 0 then
  begin
    FreeLibrary(HandleMagick());
    HandleMagick(0, true);
  end;
  if HandleWand() <> 0 then
  begin
    MagickWandTerminus();
    FreeLibrary(HandleWand());
    HandleWand(0, true);
  end;
end;


class procedure TIEMiscPluginsImageMagick.MagickWandGenesis();
const
  exec: procedure(); cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickWandGenesis');
  exec();
end;


class procedure TIEMiscPluginsImageMagick.MagickWandTerminus();
const
  exec: procedure(); cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickWandTerminus');
  exec();
end;


class function TIEMiscPluginsImageMagick.NewMagickWand(): TIEImageMagick_MagickWand;
const
  exec: function(): TIEImageMagick_MagickWand; cdecl = nil;
begin
  CheckCallWand(@exec, 'NewMagickWand');
  result := exec();
end;


class function TIEMiscPluginsImageMagick.DestroyMagickWand(wand: TIEImageMagick_MagickWand): TIEImageMagick_MagickWand;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_MagickWand; cdecl = nil;
begin
  CheckCallWand(@exec, 'DestroyMagickWand');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickReadImage(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickReadImage');
  result := exec(wand, filename);
end;


class function TIEMiscPluginsImageMagick.MagickReadImageBlob(wand: TIEImageMagick_MagickWand; blob: pointer; length: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; blob: pointer; length: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickReadImageBlob');
  result := exec(wand, blob, length);
end;


class function TIEMiscPluginsImageMagick.MagickPingImageBlob(wand: TIEImageMagick_MagickWand; blob: pointer; length: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; blob: pointer; length: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickPingImageBlob');
  result := exec(wand, blob, length);
end;


class function TIEMiscPluginsImageMagick.MagickPingImage(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickPingImage');
  result := exec(wand, filename);
end;


class procedure TIEMiscPluginsImageMagick.MagickResetIterator(wand: TIEImageMagick_MagickWand);
const
  exec: procedure(wand: TIEImageMagick_MagickWand); cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickResetIterator');
  exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickNextImage(wand: TIEImageMagick_MagickWand): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickNextImage');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickSetIteratorIndex(wand: TIEImageMagick_MagickWand; index: TIEImageMagick_ssize_t): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; index: TIEImageMagick_ssize_t): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetIteratorIndex');
  result := exec(wand, index);
end;


class procedure TIEMiscPluginsImageMagick.MagickSetLastIterator(wand: TIEImageMagick_MagickWand);
const
  exec: procedure(wand: TIEImageMagick_MagickWand); cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetLastIterator');
  exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickGetIteratorIndex(wand: TIEImageMagick_MagickWand): TIEImageMagick_ssize_t;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_ssize_t; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetIteratorIndex');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickWriteImages(wand: TIEImageMagick_MagickWand; filename: PAnsiChar; adjoin: TIEImageMagick_MagickBooleanType): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; filename: PAnsiChar; adjoin: TIEImageMagick_MagickBooleanType): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickWriteImages');
  result := exec(wand, filename, adjoin);
end;


class function TIEMiscPluginsImageMagick.MagickExportImagePixels(wand: TIEImageMagick_MagickWand; x: TIEImageMagick_ssize_t; y: TIEImageMagick_ssize_t; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; map: PAnsiChar; storage: TIEImageMagick_StorageType; pixels: pointer): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; x: TIEImageMagick_ssize_t; y: TIEImageMagick_ssize_t; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; map: PAnsiChar; storage: TIEImageMagick_StorageType; pixels: pointer): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickExportImagePixels');
  result := exec(wand, x, y, columns, rows, map, storage, pixels);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageWidth(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageWidth');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageHeight(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageHeight');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageChannelDepth(wand: TIEImageMagick_MagickWand; channel: TIEImageMagick_ChannelType): TIEImageMagick_size_t;
const
  exec: function(wand: TIEImageMagick_MagickWand; channel: TIEImageMagick_ChannelType): TIEImageMagick_size_t; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageChannelDepth');
  result := exec(wand, channel);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageColorspace(wand: TIEImageMagick_MagickWand): TIEImageMagick_ColorspaceType;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_ColorspaceType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageColorspace');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickSetImageColorspace(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetImageColorspace');
  result := exec(wand, colorspace);
end;


class function TIEMiscPluginsImageMagick.MagickTransformImageColorspace(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickTransformImageColorspace');
  result := exec(wand, colorspace);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageDepth(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_size_t; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageDepth');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageResolution(wand: TIEImageMagick_MagickWand; var x: double; var y: double): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; var x: double; var y: double): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageResolution');
  result := exec(wand, x, y);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageUnits(wand: TIEImageMagick_MagickWand): TIEImageMagick_ResolutionType;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_ResolutionType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageUnits');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageFormat(wand: TIEImageMagick_MagickWand): PAnsiChar;
const
  exec: function(wand: TIEImageMagick_MagickWand): PAnsiChar; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageFormat');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickGetType(wand: TIEImageMagick_MagickWand): TIEImageMagick_ImageType;
const
  exec: function(wand: TIEImageMagick_MagickWand): TIEImageMagick_ImageType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetType');
  result := exec(wand);
end;


class function TIEMiscPluginsImageMagick.MagickSetProgressMonitor(wand: TIEImageMagick_MagickWand; progress_monitor: TIEImageMagick_MagickProgressMonitor; client_data: pointer): TIEImageMagick_MagickProgressMonitor;
const
  exec: function(wand: TIEImageMagick_MagickWand; progress_monitor: TIEImageMagick_MagickProgressMonitor; client_data: pointer): TIEImageMagick_MagickProgressMonitor; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetProgressMonitor');
  result := exec(wand, progress_monitor, client_data);
end;


class function TIEMiscPluginsImageMagick.GetMagicInfo(magic: pointer; length: TIEImageMagick_size_t; exception: TIEImageMagick_ExceptionInfo): TIEImageMagick_MagicInfo;
const
  exec: function(magic: pointer; length: TIEImageMagick_size_t; exception: TIEImageMagick_ExceptionInfo): TIEImageMagick_MagicInfo; cdecl = nil;
begin
  CheckCallMagick(@exec, 'GetMagicInfo');
  result := exec(magic, length, exception);
end;


class function TIEMiscPluginsImageMagick.GetMagicName(magic_info: TIEImageMagick_MagicInfo): PAnsiChar;
const
  exec: function(magic_info: TIEImageMagick_MagicInfo): PAnsiChar; cdecl = nil;
begin
  CheckCallMagick(@exec, 'GetMagicName');
  result := exec(magic_info);
end;


class function TIEMiscPluginsImageMagick.AcquireExceptionInfo(): TIEImageMagick_ExceptionInfo;
const
  exec: function(): TIEImageMagick_ExceptionInfo; cdecl = nil;
begin
  CheckCallMagick(@exec, 'AcquireExceptionInfo');
  result := exec();
end;


class function TIEMiscPluginsImageMagick.DestroyExceptionInfo(e: TIEImageMagick_ExceptionInfo): TIEImageMagick_ExceptionInfo;
const
  exec: function(e: TIEImageMagick_ExceptionInfo): TIEImageMagick_ExceptionInfo; cdecl = nil;
begin
  CheckCallMagick(@exec, 'DestroyExceptionInfo');
  result := exec(e);
end;


class function TIEMiscPluginsImageMagick.MagickSetImageProperty(wand: TIEImageMagick_MagickWand; prop: PAnsiChar; value: PAnsiChar): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; prop: PAnsiChar; value: PAnsiChar): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetImageProperty');
  result := exec(wand, prop, value);
end;


class function TIEMiscPluginsImageMagick.MagickSetOption(wand: TIEImageMagick_MagickWand; key: PAnsiChar; value: PAnsiChar): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; key: PAnsiChar; value: PAnsiChar): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetOption');
  result := exec(wand, key, value);
end;


class function TIEMiscPluginsImageMagick.MagickGetException(wand: TIEImageMagick_MagickWand; var severity: TIEImageMagick_ExceptionType): PAnsiChar;
const
  exec: function(wand: TIEImageMagick_MagickWand; var severity: TIEImageMagick_ExceptionType): PAnsiChar; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetException');
  result := exec(wand, severity);
end;


class function TIEMiscPluginsImageMagick.MagickRelinquishMemory(resource: pointer): pointer;
const
  exec: function(resource: pointer): pointer; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickRelinquishMemory');
  result := exec(resource);
end;


class function TIEMiscPluginsImageMagick.MagickSetColorspace(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; colorspace: TIEImageMagick_ColorspaceType): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetColorspace');
  result := exec(wand, colorspace);
end;


class function TIEMiscPluginsImageMagick.MagickSetDepth(wand: TIEImageMagick_MagickWand; depth: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; depth: TIEImageMagick_size_t): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetDepth');
  result := exec(wand, depth);
end;


class function TIEMiscPluginsImageMagick.MagickSetFormat(wand: TIEImageMagick_MagickWand; format: PAnsiChar): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; format: PAnsiChar): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetFormat');
  result := exec(wand, format);
end;


class function TIEMiscPluginsImageMagick.MagickSetResolution(wand: TIEImageMagick_MagickWand; x_resolution: double; y_resolution: double): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; x_resolution: double; y_resolution: double): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetResolution');
  result := exec(wand, x_resolution, y_resolution);
end;


class function TIEMiscPluginsImageMagick.MagickSetImageUnits(wand: TIEImageMagick_MagickWand; units: TIEImageMagick_ResolutionType): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; units: TIEImageMagick_ResolutionType): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickSetImageUnits');
  result := exec(wand, units);
end;


class function TIEMiscPluginsImageMagick.MagickGetImageBlob(wand: TIEImageMagick_MagickWand; var length: TIEImageMagick_size_t): pbyte;
const
  exec: function(wand: TIEImageMagick_MagickWand; var length: TIEImageMagick_size_t): pbyte; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickGetImageBlob');
  result := exec(wand, length);
end;


class function TIEMiscPluginsImageMagick.MagickWriteImage(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; filename: PAnsiChar): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickWriteImage');
  result := exec(wand, filename);
end;


class function TIEMiscPluginsImageMagick.MagickImportImagePixels(wand: TIEImageMagick_MagickWand; x: TIEImageMagick_ssize_t; y: TIEImageMagick_ssize_t; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; map: PAnsiChar; storage: TIEImageMagick_StorageType; pixels: pointer): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; x: TIEImageMagick_ssize_t; y: TIEImageMagick_ssize_t; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; map: PAnsiChar; storage: TIEImageMagick_StorageType; pixels: pointer): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickImportImagePixels');
  result := exec(wand, x, y, columns, rows, map, storage, pixels);
end;


class function TIEMiscPluginsImageMagick.MagickNewImage(wand: TIEImageMagick_MagickWand; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; background: TIEImageMagick_PixelWand): TIEImageMagick_MagickBooleanType;
const
  exec: function(wand: TIEImageMagick_MagickWand; columns: TIEImageMagick_size_t; rows: TIEImageMagick_size_t; background: TIEImageMagick_PixelWand): TIEImageMagick_MagickBooleanType; cdecl = nil;
begin
  CheckCallWand(@exec, 'MagickNewImage');
  result := exec(wand, columns, rows, background);
end;


class function TIEMiscPluginsImageMagick.NewPixelWand(): TIEImageMagick_PixelWand;
const
  exec: function(): TIEImageMagick_PixelWand; cdecl = nil;
begin
  CheckCallWand(@exec, 'NewPixelWand');
  result := exec();
end;

class function TIEMiscPluginsImageMagick.DestroyPixelWand(pixelWand: TIEImageMagick_PixelWand): TIEImageMagick_PixelWand;
const
  exec: function(pixelWand: TIEImageMagick_PixelWand): TIEImageMagick_PixelWand; cdecl = nil;
begin
  CheckCallWand(@exec, 'DestroyPixelWand');
  result := exec(pixelWand);
end;



// TIEMiscPluginsImageMagick
///////////////////////////////////////////////////////////////////////////////////////////////////////





initialization
  InitializeCriticalSection(IEMiscPluginsCS);

finalization
  DeleteCriticalSection(IEMiscPluginsCS);


{$ELSE}  // not IEINCLUDEMISCPLUGINS

implementation

{$ENDIF}

end.
